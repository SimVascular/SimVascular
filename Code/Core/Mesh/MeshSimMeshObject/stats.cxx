/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including 
 * without limitation the rights to use, copy, modify, merge, publish, 
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*-------------------------------------------------------------------------
  Function  :
    Write timing statistics (if given) and quality statistics for mesh.
-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "SimVascular.h"
#include "MeshSim.h"

#define MY_MESHSIM_VERTEX_ORDERING 1

const double dhd145 = -0.819152044;
const double dhd160 = -0.93969262;
const double dhd170 = -0.984807753;

typedef double dArray[3];  /* for hp compiler compatibility */

// meshsim internal function?
//void R_coord(pRegion rgn,dArray *xyz);

#ifndef M_PI
#define M_PI		3.14159265358979323846
#endif
 
double scorecXYZ_aspectRatio2(dArray *xyz);
void scorecXYZ_dihedral(dArray *xyz,double *sml,double *big);
double scorecXYZ_edgLenRatio2(dArray *xyz);
double scorecXYZ_rbyR2(dArray *xyz);
int scorecXYZ_shape(dArray *xyz,double *shape);

int M_writeSTS(pMesh mesh, char *fname, char *mname) {
  pRegion reg;
  int MinAngRegion, MaxAngRegion, rbyRRegion, eRatioRegion, aspRatioRegion, wrstShpReg;
  void *temp = 0;
  char *tmpstr;
  int i, j, elem_num, maxlen=0, len;
  double tottime=0,smlang,bigang,worstMinAng=-1.,worstMaxAng=1.;
  double rbyR2, worst_rbyR2 = 1.0, eRatio2, worst_eRatio2 = 1.0;
  double shape, worst_shape = 1.0, rxyz[10][3];
  double aspRatio2, worstAspRatio2 = 1.0,num,percent,cumul;
  int    aspratio[7],dhdang[4];
  int    aspratio_g[7],dhdang_g[4];
  FILE *fp;
  int PImytid;
  int PInumtids;
  int np, nv, ne, nf, nr;
  int np_g, nv_g, ne_g, nf_g, nr_g;
  double worstMinAng_g;
  double worstMaxAng_g;
  double worst_rbyR2_g;
  double worst_eRatio2_g;
  double worstAspRatio2_g;
  double worst_shape_g;
  double num_g;
  static char *dhdtxt[4] = {
    "    <= 145 :","]145..160] :","]160..170] :","    > 170  :" };
  static char *asptxt[7] = {
    "      < 2  :","[  2..  4[ :","[  4..  6[ :","[  6.. 10[ :",
    "[ 10.. 20[ :","[ 20.. 40[ :","     > 40  :" };

  nr= M_numRegions(mesh);
  nr_g= nr;

   if (nr_g == 0) {
     /* for 2D meshes only */
     return CV_ERROR;
   }

  /* for 3D meshes only */

   if (!(fp = fopen(fname,"w"))) {
     fprintf(stderr,"Could not open statistics file in M_writeSTS");
     return CV_ERROR;
   }

   fprintf(fp," Model Name:\t%s\n\n",mname);

   /* write out timing info??
   for (i=0; i<ntimes; i++)
     if ((int)strlen(timetxt[i]) > maxlen)
       maxlen = strlen(timetxt[i]);
   maxlen += 5;
   tmpstr = (char *)SC_malloc(maxlen*sizeof(char));
   for (i=0; i<ntimes; i++) {
     len = strlen(timetxt[i]);
     for (j=0; j<(maxlen-1); j++)
       tmpstr[j] = (j < len)?timetxt[i][j]:' ';
     tmpstr[maxlen-1] = '\0';
     fprintf(fp,"%s %10.3f\n",tmpstr,times[i]);
   }
   fprintf(fp,"\n");
   SC_free(tmpstr);
   */

  /* write out mesh information */

   //np= M_numPoints(mesh);
  nv= M_numVertices(mesh);
  np= nv;
  ne= M_numEdges(mesh);
  nf= M_numFaces(mesh);
  nr= M_numRegions(mesh);

  np_g= np;
  nv_g= nv;
  ne_g= ne;
  nf_g= nf;
  nr_g= nr;

  fprintf(fp,"\n Number of nodes     :   %20d\n",np_g);
  fprintf(fp," Number of vertices  :   %20d\n",nv_g);
  fprintf(fp," Number of edges     :   %20d\n",ne_g);
  fprintf(fp," Number of faces     :   %20d\n",nf_g);
  fprintf(fp," Number of elements  :   %20d\n\n",nr_g);

  for (i=0; i<4; i++)
    aspratio[i]=dhdang[i] = 0;
  aspratio[4]=aspratio[5]=aspratio[6]=0;
  elem_num = 0;
  RIter myRiter;
  myRiter = M_regionIter(mesh);
  while (reg=RIter_next(myRiter)) {
    elem_num++;
    pPList regionVerts = R_vertices(reg,MY_MESHSIM_VERTEX_ORDERING);
    for (int j = 0; j < 4; j++) {
        pVertex vertex = (pVertex)PList_item(regionVerts,j);
        V_coord(vertex,rxyz[j]);
    }

    /* SCOREC shape measure */
    if (!scorecXYZ_shape(rxyz, &shape)) 
      shape = 0.0;
    if (worst_shape > shape) {
      worst_shape = shape;
      wrstShpReg = elem_num;
    }

    /* R_dihedral actually return cosine of  dihedral angle */
    scorecXYZ_dihedral(rxyz,&smlang,&bigang);
    if (worstMinAng < smlang) {
      worstMinAng = smlang;
      MinAngRegion = elem_num;
    }
    if (worstMaxAng > bigang) {
      worstMaxAng = bigang;
      MaxAngRegion = elem_num;
    }
    /* get a simplify histogram of dihedral angles */
    if (bigang<dhd170)
      dhdang[3]+=1;
    else if (bigang<dhd160)
      dhdang[2]+=1;
    else if (bigang<dhd145)
      dhdang[1]+=1;
    else dhdang[0]+=1;

    if (worst_rbyR2 > (rbyR2 = scorecXYZ_rbyR2(rxyz))) {
      worst_rbyR2 = rbyR2;
      rbyRRegion = elem_num;
    }
    if (worst_eRatio2 < (eRatio2 = scorecXYZ_edgLenRatio2(rxyz))) {
      worst_eRatio2 = eRatio2;
      eRatioRegion = elem_num;
    }
    if (worstAspRatio2 < (aspRatio2 = scorecXYZ_aspectRatio2(rxyz))) {
      worstAspRatio2 = aspRatio2;
      aspRatioRegion = elem_num;
    }
    /* get a simplify histogram of aspect ratios */
    if (aspRatio2<4.)
      aspratio[0]+=1;
    else if (aspRatio2<16.)
      aspratio[1]+=1;
    else if (aspRatio2<36.)
      aspratio[2]+=1;
    else if (aspRatio2<100.)
      aspratio[3]+=1;
    else if (aspRatio2<400.)
      aspratio[4]+=1;
    else if (aspRatio2<1600.)
      aspratio[5]+=1;
    else aspratio[6]+=1;
  }
  RIter_delete(myRiter);

  /* print out numerical results for 3D meshes */
  worstMinAng_g= worstMinAng;
  fprintf(fp," Smallest dihedral angle:\tElement = %10d   %20.5f\n",
          MinAngRegion,180.0*acos(worstMinAng_g)/M_PI);
  worstMaxAng_g= worstMaxAng;
  fprintf(fp," Largest dihedral angle:\tElement = %10d   %20.5f\n",
          MaxAngRegion, 180.0*acos(worstMaxAng_g)/M_PI);
  worst_rbyR2_g= worst_rbyR2;
  fprintf(fp," Worst r/R ratio:\t\tElement = %10d   %20.5f\n",
          rbyRRegion,sqrt(worst_rbyR2_g));
  worst_eRatio2_g= worst_eRatio2;
  fprintf(fp," Worst Edge Length Ratio:\tElement = %10d   %20.5f\n",
          eRatioRegion, sqrt(worst_eRatio2_g));
  worstAspRatio2_g= worstAspRatio2;
  fprintf(fp," Worst Aspect Ratio:\t\tElement = %10d   %20.5f\n",
          aspRatioRegion, sqrt(worstAspRatio2_g));
  worst_shape_g= worst_shape;
  fprintf(fp," Worst Shape:\t\t\tElement = %10d   %20.5e\n",
          wrstShpReg, 108*worst_shape_g);

  /* write out simplified histograms */
   fprintf(fp,"\nDihedral angles:  # regions \tpercent. \t  cumul.\n");

  num_g = (double)nr_g/100.;

  dhdang_g[0]= dhdang[0];
  dhdang_g[1]= dhdang[1];
  dhdang_g[2]= dhdang[2];
  dhdang_g[3]= dhdang[3];

  cumul=0.;
  for (i=0; i<4; i++) {
    percent = dhdang_g[i]/num_g;
    cumul+=percent;

     fprintf(fp,"%s %13d\t%6.2f %%\t%7.2f %%\n",
            dhdtxt[i],dhdang_g[i],percent,cumul);
  }

  fprintf(fp,"\nAspect ratios:    # regions \tpercent. \t  cumul.\n");
  num_g = (double)nr_g/100.;

  aspratio_g[0]= aspratio[0];
  aspratio_g[1]= aspratio[1];
  aspratio_g[2]= aspratio[2];
  aspratio_g[3]= aspratio[3];
  aspratio_g[4]= aspratio[4];
  aspratio_g[5]= aspratio[5];
  aspratio_g[6]= aspratio[6];

  cumul=0.;
  for (i=0; i<7; i++) {
    percent= aspratio_g[i]/num_g;
    cumul+=percent;

     fprintf(fp,"%s %13d\t%6.2f %%\t%7.2f %%\n",
            asptxt[i],aspratio_g[i],percent,cumul);

  }

  fprintf(fp,"\n\t\tTotal time (in secs):%15.3f\n",tottime);

  /* close file */
   fclose(fp);

   return CV_OK;

}

