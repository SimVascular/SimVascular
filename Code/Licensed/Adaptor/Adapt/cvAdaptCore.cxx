/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution. 
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior 
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *=========================================================================*/

#include "cvAdaptCore.h"

long cvAdaptCore::eigen (double pos[3][3], double e[3][3], double v[3], int checkOrthogonality)
{  
  // characteristic polynomial of T : 
  // solve x^3 + (I[2]/I[3])*x^2 + (I[1]/I[3])*x + (I[0]/I[3])*a3 = 0
  // I1 : first invariant , trace(T)
  // I2 : second invariant , 1/2 (I1^2 -trace(T^2))
  // I3 : third invariant , det T

  double I[4];
  I[3] = 1.0;
  I[2] = - trace(pos);
  I[1] = 0.5 * (I[2]*I[2] - trace2(pos));
  I[0] = - det(pos);

  // printf (" %lf x^3 +  %lf x^2 + %lf x + %lf = 0\n",
  // I[3],I[2],I[1],I[0]);

  // solve x^3 + (I[2]/I[3])*x^2 + (I[1]/I[3])*x + (I[0]/I[3])*a3 = 0
  // solve x^3 + a1 x^2 + a2 x + a3 = 0
  long nbEigen = FindCubicRoots (I,v);
    
  std::sort(v,v+3, simvascular_adaptor_greater_abs() );
  // printf ("nbEigen = %d %12.5E %12.5E %12.5E\n",nbEigen,v[0],v[1],v[2]);
    
  double result[12];
  int nb_vec=0;

  while(1)
    {
      double a[9] = {pos[0][0]-v[nb_vec],pos[0][1],pos[0][2],
		     pos[1][0],pos[1][1]-v[nb_vec],pos[1][2],
		     pos[2][0],pos[2][1],pos[2][2]-v[nb_vec]};
	
      double eps = 1.e-3;
      int nb = 0;
      while (1)
	{
	  nb = NullSpace (a,result,eps,3);
	  if (nb != 0)break;
	  eps *= 2.0;
	}
      int kk=0;
      for (int i=nb_vec;i<nb+nb_vec;i++)
	{
	  e[i][0] = result[0+kk*3];
	  e[i][1] = result[1+kk*3];
	  e[i][2] = result[2+kk*3];
	  //normVt (e[i], e[i]);
          NormVector(&e[i][0],&e[i][1],&e[i][2]);
	  // printf("%d: %f (%f, %f, %f)\n",i,v[nb_vec],e[i][0],e[i][1],e[i][2]);
	  kk++;
	  if (i == 2 && checkOrthogonality) {
	    int factor;
	    if( !checkUnitaryOthoganal(e,factor) )
	      {
		  
		printf (" %lf x^3 +  %lf x^2 + %lf x + %lf = 0\n",I[3],I[2],I[1],I[0]);
		printf ("nbEigen = %d %12.5E %12.5E %12.5E\n",nbEigen,v[0],v[1],v[2]);
		for(int jj=0; jj<3; jj++ )
		  printf("%d: %f (%f, %f, %f)\n",jj,v[jj],e[jj][0],e[jj][1],e[jj][2]);
		printf("nb=%d nb_vec=%d nbEigen=%d\n",nb,nb_vec,nbEigen);
		printf("WARNING: not orthoganal (eigen)\n\n");
	      }

	    // // changing the orientation of thrid vector
	    // // such that it follows right hand rule
	    // if(factor==-1) {
	    // for(int icomp=0;icomp<3;icomp++) {
	    //   e[3][icomp]=factor*e[3][icomp];
	    // }
	    // // adaptSimLog<<"Changing orientation for third eigen-vector"<<endl;
	    // }

	    return nbEigen;
	  }
	}
      nb_vec += nb;
      if (nb_vec == 3)
	return nbEigen;
      if( nb_vec > 3 )
	throw;
      if (nb > 3)
	throw;
    }
}
  
int cvAdaptCore::checkUnitaryOthoganal(double e[3][3], int &factor)
{
  int i;
  double dot, n[3];  
  double tol=1e-14;
  double cosalpha, alpha;
    
  for( i=0; i<3; i++ ) {
    dot=Dot(e[i][0],e[i][1],e[i][2],e[i][0],e[i][1],e[i][2]);
    if( dot < tol ) 
      { printf("the %d vector in zero length\n",i); return 0; }
    if( ABS(dot - 1.) > tol )
      { printf("the %d vector not unitary. lenthSq=%f\n",i,dot); return 0; }
  }
  dot=Dot(e[0][0],e[0][1],e[0][2],e[1][0],e[1][1],e[1][2]);
  cosalpha=dot/sqrt(Dot(e[0][0],e[0][1],e[0][2],e[0][0],e[0][1],e[0][2])*
                    Dot(e[1][0],e[1][1],e[1][2],e[1][0],e[1][1],e[1][2]));
  alpha = 57.295718*acos(cosalpha);
  if( alpha > 95 && alpha<85 ) {
    printf("first two base vectors not orthognal.  %f\n",alpha);
    return 0;
  }
  Cross(e[0][0],e[0][1],e[0][2],
        e[1][0],e[1][1],e[1][2],
        &n[0],&n[1],&n[2]);
  dot=Dot(e[2][0],e[2][1],e[2][2],n[0],n[1],n[2]);

  if(dot<0.)
    factor=-1;

  cosalpha=dot/sqrt(Dot(e[2][0],e[2][1],e[2][2],e[2][0],e[2][1],e[2][2])*
		    Dot(n[0],n[1],n[2],n[0],n[1],n[2]));

  alpha = 57.295718*acos(cosalpha);
  if( alpha < 175 && alpha>5 ) {
    printf("third base vector not orthognal to first two.  %f\n", alpha);
    return 0;
  }
  return 1;
}
  
double cvAdaptCore::trace (double pos[3][3])
{
  return pos[0][0] + pos[1][1] + pos[2][2];
}

double cvAdaptCore::trace2 (double pos[3][3])
{
  double a00 =  pos[0][0] * pos[0][0] + 
    pos[1][0] * pos[0][1] + 
    pos[2][0] * pos[0][2]; 
  double a11 =  pos[1][0] * pos[0][1] + 
    pos[1][1] * pos[1][1] + 
    pos[1][2] * pos[2][1]; 
  double a22 =  pos[2][0] * pos[0][2] + 
    pos[2][1] * pos[1][2] + 
    pos[2][2] * pos[2][2];

  return a00 + a11 + a22;
}

double cvAdaptCore::det (double pos[3][3])
{
  return pos[0][0] * (pos[1][1] * pos[2][2] - pos[1][2] * pos[2][1]) -
    pos[0][1] * (pos[1][0] * pos[2][2] - pos[1][2] * pos[2][0]) +
    pos[0][2] * (pos[1][0] * pos[2][1] - pos[1][1] * pos[2][0]);
}

// solve x^2 + b x + c = 0
// x[2] is always set to be zero
long cvAdaptCore::FindQuadraticRoots(const double b, const double c, double x[3])
{
  //    printf("Quadratic roots\n");
  x[2]=0.0;
  double delt=b*b-4.*c;
  if( delt >=0 ) {
    delt=sqrt(delt);
    x[0]=(-b+delt)/2.0;
    x[1]=(-b-delt)/2.0;
    return 3;
  }
    
  printf("Imaginary roots, impossible, delt=%f\n",delt);
  return 1;
}
  
// solve x^3 + a1 x^2 + a2 x + a3 = 0
long cvAdaptCore::FindCubicRoots(const double coeff[4], double x[3])
{
  double a1 = coeff[2] / coeff[3];
  double a2 = coeff[1] / coeff[3];
  double a3 = coeff[0] / coeff[3];
    
  if( ABS(a3)<1.0e-8 ) 
    return FindQuadraticRoots(a1,a2,x);
    
  double Q = (a1 * a1 - 3 * a2) / 9.;
  double R = (2. * a1 * a1 * a1 - 9. * a1 * a2 + 27. * a3) / 54.;
  double Qcubed = Q * Q * Q;
  double d = Qcubed - R * R;
    
  //    printf ("d = %22.15e Q = %12.5E R = %12.5E Qcubed %12.5E\n",d,Q,R,Qcubed);

  /// three roots, 2 equal 
  if(Qcubed == 0.0 || fabs ( Qcubed - R * R ) < 1.e-8 * (fabs ( Qcubed) + fabs( R * R)) )
    {
      double theta;
      if (Qcubed <= 0.0)theta = acos(1.0);
      else if (R / sqrt(Qcubed) > 1.0)theta = acos(1.0); 
      else if (R / sqrt(Qcubed) < -1.0)theta = acos(-1.0); 
      else theta = acos(R / sqrt(Qcubed));
      double sqrtQ = sqrt(Q);
      //      printf("sqrtQ = %12.5E teta=%12.5E a1=%12.5E\n",sqrt(Q),theta,a1);
      x[0] = -2 * sqrtQ * cos( theta           / 3) - a1 / 3;
      x[1] = -2 * sqrtQ * cos((theta + 2 * M_PI) / 3) - a1 / 3;
      x[2] = -2 * sqrtQ * cos((theta + 4 * M_PI) / 3) - a1 / 3;
      return (3);
    }

  /* Three real roots */
  if (d >= 0.0) {
    double theta = acos(R / sqrt(Qcubed));
    double sqrtQ = sqrt(Q);
    x[0] = -2 * sqrtQ * cos( theta           / 3) - a1 / 3;
    x[1] = -2 * sqrtQ * cos((theta + 2 * M_PI) / 3) - a1 / 3;
    x[2] = -2 * sqrtQ * cos((theta + 4 * M_PI) / 3) - a1 / 3;
    return (3);
  }
    
  /* One real root */
  else {
    printf("IMPOSSIBLE !!!\n");

    double e = pow(sqrt(-d) + fabs(R), 1. / 3.);
    if (R > 0)
      e = -e;
    x[0] = (e + Q / e) - a1 / 3.;
    return (1);
  }
}

  
long cvAdaptCore::NullSpace(const double *a, double *result, double eps, long n)
{
  int r[MAXN], c[MAXN];
  register long i, j, k;
  int jj, kk, t;
  double max, temp;
  int ec;
    
  for (i = 0; i < n; i++)
    r[i] = c[i] = -1;                 /* Reset row and column pivot indices */
    
  // copy the input matrix if not in place
  if (result != a) 
    for (i = 0; i < n*n; i++)  
      result[i] = a[i];
  // rest of algorithm is in place wrt result[]
    
  for (i = 0; i < n; i++) {
    /* Find the biggest element in the remaining submatrix
     * for the next full pivot.
     */
    max = 0.0;
    for (k = 0; k < n; k++) {
      if (r[k] < 0) {
	for (j = 0; j < n; j++) {
	  if ((c[j] < 0) && ((temp = fabs(R(k, j))) > max)) {
	    kk = k;
	    jj = j;
	    max = temp;
	  }
	}
      }
    }
    if (max < eps)
      break;          /* Consider this and all subsequent pivots to be zero */

    c[jj] = kk;                                       /* The row */
    r[kk] = jj;                                       /* and column of the next pivot */
      
    temp = 1.0 / R(kk, jj);
    R(kk, jj) = 1.0;
    for (j = 0; j < n; j++)           /* Should this be for j != jj ? */
      R(kk, j) *= temp;               /* Row equilibration */
      
    for (k = 0; k < n; k++) { /* Row elimination */
      if (k == kk)
	continue;                     /* Don't do a thing to the pivot row */
      temp = R(k, jj);
      R(k, jj) = 0.0;
      for (j = 0; j < n; j++) {
	R(k, j) -= temp * R(kk, j);   /* Subtract row kk from row k */
	if (fabs(R(k, j)) < eps)
	  R(k, j) = 0.0;      /* Flush to zero if too small */
      }
    }
  }
    
  /* Sort into a truncated triangular matrix */
  for (j = 0; j < n; j++) {           /* For all columns... */
    while ((c[j] >= 0) && (j != c[j])) {
      for (k = 0; k < n; k++) {
	if (r[k] < 0) {
	  /* Aha! a null column vector */
	  temp = R(k, j);     /* Get it on top */
	  R(k, j) = R(k, c[j]);
	  R(k, c[j]) = temp;
	}
      }
      t = c[j];                /* Twiddle until pivots are on the diagonal */
      c[j] = c[t];
      c[t] = t;
    }
  }
    
  /* Copy the null space vectors into the top of the A matrix */
  ec = 0;
  for (k = 0; k < n; k++) {
    if (r[k] < 0) {
      R(k, k) = 1.0;                  /* Set the pivot equal to 1 */
      if (ec != k) {
	for (j = 0; j < n; j++) {
	  R(ec, j) = R(k, j);
	}
      }
      ec++;
    }
  }
  /* The first  ec  rows of the matrix  a  are the vectors which are
   * orthogonal to the columns of the matrix  a.
   */
  return (ec);
}


// called in callback = phastaTransfer
double* cvAdaptCore::InterpolateSolution( pRegion region, 
                     double xi[3], 
                     int ndof,
                     pMeshDataId modes ) {

/* This routine takes the region and the location of the vertex in */
/* parametric coordinates, polynomial order, and the attached solution*/
/* and returns the solution interpolated at the specified location. Higher*/
/* order mode coefficients are also evaluated    */
  int exitCode=0;
  double* vcc;
  double* ecc;
  double* fcc;
  double sfval;
  double* q = new double[ndof];
  double r=xi[0];
  double s=xi[1];
  double t=xi[2];
  double L[4] = { r, s, t, 1-r-s-t};
  
  for (int i=0; i < ndof; i++) q[i] = 0.0;
  
  pVertex vrts[4];
  // pEdge edgs[6];
  // pFace facs[4];
  
  pPList rverts = R_vertices(region,1);
  for(int iVert=0; iVert<PList_size(rverts); iVert++) {
      vrts[iVert] = (pVertex)PList_item(rverts,iVert);
  }
  PList_delete(rverts);
  
  // loop over the vertices of (parent) region
  for (int k=0; k<4; k++) {
    
    // solution(( pEntity )vrts[k], phasta_solution, &vcc ); 
    /* get the solution at the vertex */

    if(!EN_getDataPtr( (pEntity)vrts[k], phasta_solution, (void**)&vcc )) {
      V_info(vrts[k]);
      printf("\nerror in InterpolateSolution: wanted to retrieve "
	     "a solution on a vertex that is empty\n");
      exit(-1);
    }

    /*evaluate the shape function at the given location xi*/
    sfval = Entity_shapeFunction(( pEntity )vrts[k],
				 ( pEntity )region, 1, 0, L);
    for (int i=0; i < ndof; i++){
      /*sigma(N_a(xi)*y_a)*/
      q[i] += vcc[i]*sfval;
    }
  }
  
  // get the edge and face coefficients
  // not implemented at the moment

  return q;
}


void  cvAdaptCore::display_region( pRegion region ){

  double xyz[3];
  pPList vertices = R_vertices( region, 1 );
  adaptSimLog << "-------------------"<< endl ;
  for( int i=0; i<PList_size( vertices ); i++) {
      V_coord( (pVertex)PList_item( vertices, i ), xyz );
      adaptSimLog << xyz[0] <<" "<< xyz[1]<<" "<<xyz[2]<< endl;
  }
  adaptSimLog << "-------------------"<< endl ;
}


int cvAdaptCore::inverseMap( pRegion region, 
            double* qpt,
            double* pt ) {

    // This is the version of Inverse map, which uses the algorithm 
    // by ken from Mesh2Mesh  (MTMHO3)
    // This thing basically, does a linear solution and then tries to
    // get a Iterative Newton correction to it.

    // First to setup the constants of the forward transformation
    //                   x = Ax* xi
    //                   y = Ay* xi
    //                   z = Az* xi
    // Ax,Ay,Az have 8 terms each and can be obtained using the 
    // solution of an 8x8 system,( which is what I am going to do)!!

    double** A;
    double** AA;
    static double M[4][4] ={ {1, 1, 0, 0 }, 
                             {1, 0, 1, 0 },
                             {1, 0, 0, 1 },
                             {1, 0, 0, 0 } };
			   
			   
			   
			   
    int eight = 8;
    int four  = 4;
    static double Mtemp [16];
    double x = qpt[0];
    double y = qpt[1];
    double z = qpt[2];
    double xel[8],yel[8],zel[8];
    double xisol[3];
    double xyz[3];
    int indx[4];
    double fnumber;
    pPList verts = R_vertices( region ,1);

    A = new double* [3];
    AA = new double*[3];
    for(int i =0; i< 3;i++) A[i] = new double [4];
    for(int i =0; i< 3;i++) AA[i] = new double [4];  


    //creating the LHS
    int k=0;
    for(int i =0; i<4; i++)
        for(int j=0; j<4; j++)
            Mtemp[k++]= M[i][j]; 
  
    
  // LU decompsing the coeff matrix
    ludcmp( Mtemp, &four, &four, indx, &fnumber);
  
    // Creating the RHS
    for(int i=0; i< 4; i++){
        V_coord( ( pVertex ) PList_item( verts, i ), xyz );
        xel[i] = xyz[0];
        yel[i] = xyz[1];
        zel[i] = xyz[2];
    }

    PList_delete(verts);

    for(int i=0; i<4;i++){
        A[0][i]=xel[i];
        A[1][i]=yel[i];
        A[2][i]=zel[i];
    }

    // Now back substituting to get back the correct set of constants
    // for this element.
  
    lubksb( Mtemp, &four, &four, indx, A[0] );
    lubksb( Mtemp, &four, &four, indx, A[1] );
    lubksb( Mtemp, &four, &four, indx, A[2] );


  // Now we have Ax, Ay and Az (where A is the inverse of matrix Mtemp). 
  // Next, we try to get xi, zeta, eta for a given x, y, z.
  // Ax contains the alpha_x in the form of A[0][0] = alpha_x0, 
  // A[0][1] = alpha_x1, and so on. A[1][0] = alpha_y0, 
  // A[1][1] = alpha_y1, and so on. A[2][0] = alpha_z0, 
  // A[2][1] = alpha_z1, and so on. 

  // But first, overwrite the alphas with the solution solved for by
  // paper and pencil.

    AA[0][0] = xel[3];
    AA[0][1] = ( xel[0] - xel[3] );
    AA[0][2] = ( xel[1] - xel[3] );
    AA[0][3] = ( xel[2] - xel[3] );

    AA[1][0] = yel[3];
    AA[1][1] = ( yel[0] - yel[3] );
    AA[1][2] = ( yel[1] - yel[3] );
    AA[1][3] = ( yel[2] - yel[3] );

    AA[2][0] = zel[3];
    AA[2][1] = ( zel[0] - zel[3] );
    AA[2][2] = ( zel[1] - zel[3] );
    AA[2][3] = ( zel[2] - zel[3] );


    int indx2[3];
    int three=3;

    double MS[9];
    k =0;
    for(int i =0;i<3;i++){
        for(int j=1; j<4;j++){
            MS[k++]= A[i][j];
        }
    }

    double xl[3],dxl[3];
    xisol[0] =  x - A[0][0];
    xisol[1] =  y - A[1][0];
    xisol[2] =  z - A[2][0];
    xl[0] = x;
    xl[1] = y;
    xl[2] = z;
    // LU decompsing the coeff matrix and solving for xisol

    ludcmp( MS, &three, &three, indx2, &fnumber);
    lubksb( MS, &three, &three, indx2, xisol);

    double tol = 1.0e-6;
    int truth =1;                        
  
    for( int i=0; i<3 ; i++) {         
        if ( xisol[i] > 1.0+tol || xisol[i] < 0.0-tol ) {
            truth = 0; 
        } 
    }
    double l4 = 1 - xisol[0] - xisol[1] - xisol[2];
    if ( l4 > 1.0+tol || l4 < 0.0-tol ) truth = 0;
  
    if (truth){                          
        pt[0] = xisol[0];                 
        pt[1] = xisol[1];                 
        pt[2] = xisol[2];  
    }                                    

    for(int i =0; i< 3;i++) delete A[i] ;
    for(int i =0; i< 3;i++) delete AA[i] ;
    delete [] A;
    delete [] AA;


    return truth;
}


void cvAdaptCore::ModifyHessiansAtBdry(pMesh mesh) {
  pVertex v;
  VIter vIter=M_vertexIter(mesh);
  while(v = VIter_next(vIter)) {
    
    // bdry correction:
    // take values of (any/closest) OFF-bdry vertex as the bdry vertex value
    // all verts classified on model :face, edge or vertex
    if(V_whatInType(v) != 3) {      
      int numEdges = V_numEdges(v);
      pEdge edge;
      bool finished=false;
      pVertex otherVertex;
      for(int i=0; i<numEdges  ;i++){
	edge = V_edge(v,i);
	
	// if the edge is in interior
	// (later distance could be added) 
	if(E_whatInType(edge) == 3){
	  otherVertex = E_otherVertex(edge,v);
	  finished=true;
	  break;
	}
      }

      if(finished) {
	
	double *nodalHessianOther;
	if(!EN_getDataPtr((pEntity)otherVertex, nodalhessianID,(void**)&nodalHessianOther)){
	  
	  adaptSimLog<<"\nerror in :V_Hessian no data attached to OTHER vertex\n";
	  V_info(otherVertex);
	  exit(0);
	}
	
	double *nodalHessianActual;
	if(!EN_getDataPtr((pEntity)v, nodalhessianID,(void**)&nodalHessianActual)){
	  
	  adaptSimLog<<"\nerror in :V_Hessian no data attached to ACTUAL vertex\n";
	  exit(0);
	}
	
	delete [] nodalHessianActual;
	double* nodalHessianModified=new double[6];
	
	for(int iHess=0;iHess<6;iHess++) {
	  nodalHessianModified[iHess]=nodalHessianOther[iHess];
	}
	
	EN_deleteData((pEntity)v, nodalhessianID);
	
	EN_attachDataPtr( (pEntity)v, nodalhessianID, (void *)
			  nodalHessianModified );
      }
    }
    // one comment is appropriate:
    // if the bdry vertex does NOT have any edges
    // which are interior (very pathologigal mesh anyway)
    // the hessian on that node is kept as it is 
    // (mesh should not have such pathological case)
  }
  VIter_delete(vIter); 
}
  

// Modify mesh-metric to take special care
// like in parallel plates (i.e, w=f(y)) 
// if a user wants different hmax in x-direction
// other examples can be situations where
// user don't want to have one mesh edge 
// connecting two geometric model edges (like no dofs)
void cvAdaptCore::ModifyMetric(pVertex vertex, double dir[3][3], double* h){
  double tol = 1.e-2; 
  double* nodalSolution;

  pVertex solVertex; // take solution from this vertex  
  if(V_whatInType(vertex) == 3) { // if the vertex is in interior
    solVertex = vertex;
  }
  else { // if the vertex is on model bdry.
    bool noOtherVertex;
    int numEdges = V_numEdges(vertex);
    for(int i=0; i<numEdges; i++) {
      pEdge edge = V_edge(vertex,i);
      pVertex otherVertex = E_otherVertex(edge,vertex);
      if(V_whatInType(otherVertex) == 3) {
	solVertex = otherVertex;
	noOtherVertex = false;
      }
    }
    if(noOtherVertex) { // vertex is on bdry. with no interior vertex around
      adaptSimLog<<"No interior node : V_info(vertex)"<<endl;
      exit(0);
    }
  }
 
  EN_getDataPtr((pEntity)solVertex,phasta_solution,(void**)&nodalSolution);

  double hmaxOther = 0.2; // user should specify this
  for (int jRow=0; jRow<3; jRow++) {
    double dot = 0.;
    for(int iDir=0; iDir<3; iDir++) {
      dot +=nodalSolution[iDir+1]*dir[jRow][iDir];
    }
    // parallel plates (i.e, w=f(y)) (only z-velocity)
    // currently velocity vector & metric is used to find the other direction
    // hmaxOther plays role in x- and y-direction (perpendicular to velocity)
    if(dot*dot < tol*tol ) // criteria to decide other hmax direction 
      if(h[jRow] > 0.2) h[jRow] = hmaxOther;
  }
}
  


void cvAdaptCore::SmoothErrorIndicators(pMesh mesh, int option)
{
  pVertex v;
  VIter vIter=M_vertexIter(mesh);
  int vCount=0;

  double* averageNodalValue = new double[M_numVertices(mesh)];

  while(v = VIter_next(vIter)) {

    // get each vert's surrounding nodes 
    // if they are off-bdry
    int numEdges = V_numEdges(v);
    pEdge edge;
	
    averageNodalValue[vCount] = 0.0;

    // can be different from V_numEdges
    // will include the actual vertex, too
    int numSurroundingVerts=0;
        
    // exclude non-interor vertex from contributing to
    // its own average
    if(V_whatInType(v)== 3){
      // first the "central" vertex
      double *nodalEIs;
      if(!EN_getDataPtr((pEntity)v, errorIndicatorID,(void**)&nodalEIs)){
	adaptSimLog<<"\nerror in SmoothErrorIndicators: no data attached to  vertex\n";
	V_info(v);
	exit(0);
      }
      averageNodalValue[vCount] = averageNodalValue[vCount] + getErrorValue(nodalEIs,option);
      numSurroundingVerts++;
    }
        
    // now for the surrounding vertices
    for(int i=0; i<numEdges  ;i++){
      
      edge = V_edge(v,i);
      pVertex otherVertex;
      otherVertex = E_otherVertex(edge,v);
      
      // if neighbor vert is NOT on bdry
      if(V_whatInType(otherVertex)== 3){//is in interior
                
	double *nodalEIs;
	if(!EN_getDataPtr((pEntity)otherVertex,errorIndicatorID,(void**)&nodalEIs)){
	  adaptSimLog<<"\nerror in SmoothErrorIndicators: no data attached to OTHER vertex\n";
	  V_info(otherVertex);
	  exit(0);
	}
	// add values up
	averageNodalValue[vCount] = averageNodalValue[vCount] + getErrorValue(nodalEIs,option);
	numSurroundingVerts++;
      }
    }//for( ..i<numEdges...)
    if(!numSurroundingVerts){
      adaptSimLog<<"\nerror in SmoothErrorIndicators: there is a boundary vertex whose\n"
	  <<"neighbors are exclusively classsified on modelfaces/edges/vertices\n"
	  <<"and NOT in the interior\n";
      
      adaptSimLog<<"For the following vertex : "<<endl;
      V_info(v);
      exit(0);
    }
    
    averageNodalValue[vCount] = averageNodalValue[vCount]/numSurroundingVerts;
    vCount++;

    }// while v = vIter
    VIter_reset(vIter); 

    vCount=0;
    while(v = VIter_next(vIter)) {
    
      // delete the old nodal EI values
      double* oldNodalEIs;
      if(!EN_getDataPtr((pEntity)v,errorIndicatorID,(void**)&oldNodalEIs)){
	adaptSimLog<<"\nerror in SmoothErrorIndicators: no data attached to OTHER vertex\n";
	V_info(v);
	exit(0);
      }
      delete [] oldNodalEIs;

      EN_deleteData((pEntity)v,errorIndicatorID);
    
      double* averageEI = new double;
      *averageEI = averageNodalValue[vCount];

      EN_attachDataPtr( (pEntity)v, errorIndicatorID, (void *)
			averageEI );

      vCount++;
    }
    VIter_delete(vIter); 

    delete [] averageNodalValue;

}


// simple average over a patch surrounding the vertex    
void cvAdaptCore::SmoothHessians(pMesh mesh) {

  pVertex v;
  VIter vIter=M_vertexIter(mesh);
  int vCount=0;
  
  // keep the vals in memory before finally setting them
  double** averageHessian;
  averageHessian = new double*[M_numVertices(mesh)];  
  
  while(v = VIter_next(vIter)) {
    if (EN_isBLEntity(v)) continue;
    // get each vert's surrounding nodes 
    // if they are off-bdry
    int numEdges = V_numEdges(v);
    pEdge edge;
    
    averageHessian[vCount] = new double[6];
    
    for(int iHess=0;iHess<6;iHess++) {
      averageHessian[vCount][iHess]=0.0;
    }
    // can be different from V_numEdges
    // will include the actual vertex, too
    int numSurroundingVerts=0;
    
    // exclude non-interor vertex from contributing to
    // its own average
    if(V_whatInType(v)== 3) {
      // first the "central" vertex
      double *nodalHessian;
      if(!EN_getDataPtr((pEntity)v, nodalhessianID,(void**)&nodalHessian)){
	
	adaptSimLog<<"\nerror in SmoothHessians: no data attached to  vertex\n";
	V_info(v);
	exit(0);
      }
      for(int iHess=0; iHess<6; iHess++) {
	averageHessian[vCount][iHess]= averageHessian[vCount][iHess] + nodalHessian[iHess];
      }
      numSurroundingVerts++;
    }//if(V_whatInType(v)== 3) 
    
    // now for the surrounding vertices
    for(int i=0; i<numEdges ; i++){
      edge = V_edge(v,i);
      pVertex otherVertex;
      otherVertex = E_otherVertex(edge,v);
      
      // if neighbor vert is NOT on bdry
      if(V_whatInType(otherVertex)== 3){//is in interior
	
	double *nodalHessianOther;
	if(!EN_getDataPtr((pEntity)otherVertex, nodalhessianID,(void**)&nodalHessianOther)){
	  
	  adaptSimLog<<"\nerror in SmoothHessians: no data attached to OTHER vertex\n";
	  V_info(otherVertex);
	  exit(0);
	}
	// add values up
	for(int iHess=0;iHess<6;iHess++) {
	  averageHessian[vCount][iHess] = averageHessian[vCount][iHess] + nodalHessianOther[iHess];
	}
	numSurroundingVerts++;
      }
    }//for( ..i<numEdges...)

    if(!numSurroundingVerts){
      adaptSimLog<<"\nerror in SmoothHessians: there is a boundary vertex whose\n"
	  <<"neighbors are exclusively classsified on modelfaces/edges/vertices\n"
	  <<"and NOT in the interior\n";
      
      adaptSimLog<<"For the following vertex : "<<endl;
      V_info(v);
      exit(0);
    }

//      if(!numSurroundingVerts) {   
//        double *nodalHessianSame;
//        if(!EN_getDataPtr((pEntity)v, nodalhessianID,(void**)&nodalHessianSame)){
	
//  	adaptSimLog<<"\nerror in SmoothHessians: no data attached to vertex\n";
//  	V_info(v);
//  	exit(0);
//        } 
//        // add values up
//        for(int iHess=0;iHess<6;iHess++) {
//  	averageHessian[vCount][iHess] = averageHessian[vCount][iHess] + nodalHessianSame[iHess];
//        }
//        numSurroundingVerts=1;
//      }
    
    for(int iHess=0;iHess<6;iHess++) {
      averageHessian[vCount][iHess] = averageHessian[vCount][iHess]/numSurroundingVerts;
    }
    vCount++;
  }// while v = vIter
  VIter_reset(vIter); 
  
  vCount=0;
  while(v = VIter_next(vIter)) {
    if (EN_isBLEntity(v)) continue;
    // delete the old hessian data
    double* oldHessian;
    if(!EN_getDataPtr((pEntity)v, nodalhessianID,(void**)&oldHessian)){
      
      adaptSimLog<<"\nerror in SmoothHessians: no data attached to OTHER vertex\n";
      V_info(v);
      exit(0);
    }
    delete [] oldHessian;
    
    EN_deleteData((pEntity)v, nodalhessianID);
    
#ifdef DEBUG
    //         adaptSimLog<<"\nin SmoothHessians: going to attach\n";
    //         for(int i=0; i<6; i++){
    //             adaptSimLog<<averageHessian[vCount][i]<<" ";
    //         }
    //         adaptSimLog<<"\n";
#endif//DEBUG
    
    EN_attachDataPtr( (pEntity)v, nodalhessianID, (void *)
		      averageHessian[vCount] );
    
    vCount++;
  }
  VIter_delete(vIter);
}
  
void cvAdaptCore::V_AnalyticHessian (pVertex v, double H[3][3], int option)
{

  for(int i=0; i<3; i++) {
    for(int j=0; j<3; j++) {
      H[i][j]=0.0;
    }
  }
  
  if(V_whatInType(v)!=3) {
    double temp_H[3][3];        
    int numSurroundingVerts=0;
    int numEdges = V_numEdges(v);

    pEdge edge;
    for(int i=0; i<numEdges ; i++) {      
      edge = V_edge(v,i);
      pVertex otherVertex;
      otherVertex = E_otherVertex(edge,v);
      
      // if neighbor vert is NOT on bdry
      if( V_whatInType(otherVertex)==3 ){//is in interior
	V_AnalyticHessian(otherVertex,temp_H,option);
	numSurroundingVerts++;
	for(int i=0; i<3; i++) {
	  for(int j=0; j<3; j++) {
	    H[i][j]+=temp_H[i][j];
	  }
	}
      }
    }//for( ..i<numEdges...)


    if(!numSurroundingVerts){
      adaptSimLog<<"\nerror in V_AnalyticHessian: there is a boundary vertex whose\n"
	  <<"neighbors are exclusively classsified on model faces/edges/vertices\n"
	  <<"and NOT in the interior\n";
      
      adaptSimLog<<"For the following vertex : "<<endl;
      V_info(v);
      exit(0);
    }

    for(int i=0; i<3; i++) {
      for(int j=0; j<3; j++) {
	H[i][j]=H[i][j]/numSurroundingVerts;
      }
    }

  }
  else {
    // can provide different analytic hessian 
    // to compare it with numerical one for different cases
    switch(option) {
    // case 1 not allowed as numerical hessian is used then (in sizefield.cc)
    case 2: {
      // this is generated by maple 
      // this case corresponds to 1_over_7 (turbulent) profile
      // for a straight cylindrical pipe
      double xyz[3];
      V_coord(v,xyz);
      
      double x;
      double y;
      double eps=1.0e-10;
      double t20;
      double t6;
      double ddf_xy;
      double t7;
      double t4;
      double t3;
      double t2;
      double t1;
      double t16;
      double t17;
      double t15;
      double t14;
      double t12;
      double t11;
      double t10;
      double t9;
      double t8;
      double t29;
      double t25;
      double t24;
      double t21;
      double t32;
      double t30;
      double t31;
      double t37;
      double t36;
      double t40;
      double t42;
      double t41;
      double t45;
      double t48;
      double t57;
      double t74;
      double t56;
      x = xyz[0];
      y = xyz[1];
      t1 = x * x;
      t2 = y * y;
      t3 = t1 + t2;
      t4 = sqrt(t3);
      t6 = 0.250e2 + eps - 0.250e2 * t4;
      t7 = pow(t6, -0.20e1);
      t8 = 0.1e1 +eps - t4;
      t9 = pow(t8, -0.2857142857e0);
      t10 = t7 + t9;
      t11 = t10 * t10;
      t12 = sqrt(t10);
      t14 = 0.10e1 / t12 / t11;
      t15 = pow(t6, -0.30e1);
      t16 = 0.10e1 / t4;
      t17 = t15 * t16;
      t20 = pow(t8, -0.1285714286e1);
      t21 = t20 * t16;
      t24 = 0.5000e2 * t17 * x + 0.2857142857e0 * t21 * x;
      t25 = t24 * t24;
      t29 = 0.10e1 / t12 / t10;
      t30 = pow(t6, -0.40e1);
      t31 = 0.10e1 / t3;
      t32 = t30 * t31;
      t36 = 0.10e1 / t4 / t3;
      t37 = t15 * t36;
      t40 = 0.5000e2 * t17;
      t41 = pow(t8, -0.2285714286e1);
      t42 = t41 * t31;
      t45 = t20 * t36;
      t48 = 0.2857142857e0 * t21;
      t56 = 0.5000e2 * t17 * y + 0.2857142857e0 * t21 * y;
      t57 = t56 * t56;
      t74 = y * x;
      ddf_xy = 0.3e1 / 0.4e1 * t14 * t24 * t56 - t29 * (0.37500000e4 * t32 * t74 - 0.5000e2 * t37 * t74 + 0.3673469388e0 * t42 * t74 - 0.2857142857e0 * t45 * t74) / 0.2e1;
      H[0][0] = 0.3e1 / 0.4e1 * t14 * t25 - t29 * (0.37500000e4 * t32 * t1 - 0.5000e2 * t37 * t1 + t40 + 0.3673469388e0 * t42 * t1 - 0.2857142857e0 * t45 * t1 + t48) / 0.2e1;
      H[0][1] = ddf_xy;
      H[0][2] = 0.0e0;
      H[1][0] = ddf_xy;
      H[1][1] = 0.3e1 / 0.4e1 * t14 * t57 - t29 * (0.37500000e4 * t32 * t2 - 0.5000e2 * t37 * t2 + t40 + 0.3673469388e0 * t42 * t2 - 0.2857142857e0 * t45 * t2 + t48) / 0.2e1;
      H[1][2] = 0.0e0;
      H[2][0] = 0.0e0;
      H[2][1] = 0.0e0;
      H[2][2] = 0.0e0;
    }
    break;
    default : {
      adaptSimLog<<"\nSpecify a correct option (V_AnalyticHessian)"<<endl;
      exit(-1); 
    }
    break;
    }
  }
}


// hessian returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
// called in setSizeFieldUsingHessians (sizefield.cc)
void cvAdaptCore::V_Hessian(pVertex v, double T[3][3])
{
  double* nodalHessian;
  
  if(EN_getDataPtr((pEntity)v, nodalhessianID,(void**)&nodalHessian) == NULL){
    adaptSimLog<<"\nerror in :V_Hessian no data attached to vertex\n";
    exit(0);
  }
  
  T[0][0]=nodalHessian[0];
  T[0][1]=T[1][0]=nodalHessian[1];
  T[0][2]=T[2][0]=nodalHessian[2];
  T[1][1]=nodalHessian[3];
  T[1][2]=T[2][1]=nodalHessian[4];
  T[2][2]=nodalHessian[5];  
}


void cvAdaptCore::V_getHessians(double *hessiansFromPhasta, pMesh mesh, 
	      int nvar, int option, double *hessians) {

  int nshg=M_numVertices(mesh);
  
  switch (option) {
  case 0: { // for speed
    // from each component hessains get speed hessians
    // not a good way (can have trouble when speed is zero)
  }
  break;
  // For cases 1, 2 & 3 : 27 entries (for each node)
  // u_xx, v_xx, w_xx, u_yx, v_yx, w_yx, u_zx, v_zx, w_zx
  // u_yy, v_yy, w_yy, u_zy, v_zy, w_zy, u_zz, v_zz, w_zz
  // u_x, v_x, w_x, u_y, v_y, w_y, u_z, v_z, w_z
  case 1: { // for u
    for(int i=0; i<nshg; i++)
      for(int j=0; j<6; j++)
	hessians[i*6+j] = hessiansFromPhasta[i*27+3*j];
  }
  break;
  case 2: { // for v
    for(int i=0; i<nshg; i++)
      for(int j=0; j<6; j++)
	hessians[i*6+j] = hessiansFromPhasta[i*27+3*j+1];
  }
  break;
  case 3: { // for w
    for(int i=0; i<nshg; i++)
      for(int j=0; j<6; j++)
	hessians[i*6+j] = hessiansFromPhasta[i*27+3*j+2];
  }
  break;
  case 4: { // for speed OR average speed
    // 9 entries (for each node) : 
    // U_xx, U_yx, U_zx, U_yy, U_zy, U_zz, U_x, U_y, U_z
    for(int i=0; i<nshg; i++)
      for(int j=0; j<6; j++)
	hessians[i*6+j] = hessiansFromPhasta[i*9+j];
  }
  break;
  case 5: { // for temperature    
    // 9 entries (for each node) :
    // T_xx, T_yx, T_zx, T_yy, T_zy, T_zz, T_x, T_y, T_z
    for(int i=0; i<nshg; i++)
      for(int j=0; j<6; j++)
	hessians[i*6+j] = hessiansFromPhasta[i*9+j];    
  }
  break;
  default: {
    adaptSimLog<<"Specify a correct option (V_getHessians.cc)"<<endl;
    exit(1);
  }
  break; 
  }
}


// attaches array to mesh entities
// `dataID' is the MeshDataId
// `nVar' is the no. of variables at each dof
// e.g., `nVar'=5 (for flow problems) or 27 (for hessians)
// `poly' is the polynomial order 
// this routine attaches  "valueArray"
// the incoming "valueArray" which contains data
// for ALL vertices (general dofs) is split into 
// local entity-level arrays to handle the memory
// during local mesh modifications 
void cvAdaptCore::attachArray( double *valueArray, 
		  pMesh mesh, 
		  pMeshDataId dataID,
		  int nVar, 
		  int poly ) {
  int count,i;
  int nem = (poly > 1) ? (poly - 1) : 0;
  int nfm = (poly > 3) ? ((poly-3)*(poly-2)/2.0) : 0;
  int nrm = (poly > 5) ? ((poly-4)*(poly-5)*(poly-3)/6.0) : 0;
  if(poly==3) nfm =1;

  /* attach the vertex coefficients */
  count = 0;
  pVertex vertex;
  VIter vIter = M_vertexIter( mesh );
  while (vertex = VIter_next( vIter ) ) {
    double *data = new double[nVar];
    for (i=0; i < nVar; i++) data[i] = valueArray[count++];
    EN_attachDataPtr( (pEntity)vertex, dataID, (void *) data );
  }
  VIter_delete( vIter );

  /* attach the edge coefficients */
  if (nem > 0){
    pEdge edge;
    EIter eIter = M_edgeIter( mesh );
    while (edge = EIter_next( eIter ) ) {
      double* data = new double[nVar*nem];
      for (i=0; i < nVar*nem; i++) data[i] = valueArray[count++];
      EN_attachDataPtr( (pEntity)edge, dataID, (void *) data );
    }
    EIter_delete( eIter );
  }
  
  /* attach face coefficients */
  if (nfm > 0){
    pFace face;
    FIter fIter = M_faceIter( mesh );
    while (face = FIter_next( fIter ) ) {
      double* data = new double[nVar*nfm];
      for ( i=0; i < nVar*nfm; i++ ) data[i] = valueArray[count++];
      EN_attachDataPtr( (pEntity)face, dataID, (void *) data );
    }
    FIter_delete( fIter );
  }

  /* attach region coefficients */
  if (nrm > 0) {
     adaptSimLog << " No code to attach Region Modes " << endl;
     exit ( -1 );
  }

}
// get data (previously attached) from mesh
// `dataID' is the MeshDataId
// `nVar' is the no. of variables at each dof
// e.g., `nVar'=5 (for flow problems) or 27 (for hessians)
// `poly' is the polynomial order (ONLY order 1 is supported as of now) 
// this routine gets attached data array from mesh   
// in restart-writable format 
// memory is allocated within the function
// user has to delete the memory
void cvAdaptCore::getAttachedArray( double *&valueArray,
                       pMesh mesh,
                       pMeshDataId dataID,
                       int nVar,
                       int poly)
{


  int i;
  if(poly!=1) {
      adaptSimLog << "\nError in getAttachedData() [in attachData.cc]" << endl;
      adaptSimLog << "Polynomial order [" << poly << "] NOT supported" << endl;
      exit(-1);
  }

  int nshg = M_numVertices(mesh);
  valueArray = new double[nshg*nVar];

  /* attach the vertex coefficients */
  int vCount = 0;
  pVertex vertex;
  VIter vIter = M_vertexIter( mesh );
  while (vertex = VIter_next( vIter ) ) {
    double *data;
    if(!EN_getDataPtr( (pEntity)vertex, dataID, (void **)&data )) {
        adaptSimLog << "\nError in getAttachedData() [in attachData.cc]" << endl;
        adaptSimLog << "Data not attached to vertex [" << EN_id((pEntity)vertex) << "]" << endl;
        exit(0);
    }
    for (i=0; i < nVar; i++) valueArray[vCount+i*nshg] = data[i];
    vCount++;

  }
  VIter_delete( vIter );
}



// cleans data attached to mesh entities
// `dataID' is the MeshDataId
// `en_type' is the entity type on which data is atached
// e.g., 0 (for vertex), 3 (for regions), 4 (for all)
// can use polynomial order to delete data on entities
// i.e., for different modes, instead user should call routine
// for different entities depending on poly. order/mode
// with this attached data doesn't have to be solution
void cvAdaptCore::cleanAttachedData(pMesh mesh,
		       pMeshDataId dataID,
		       int en_type,
		       int array) {
  switch(en_type) {
  case 0:
    {      
      pVertex vtx;
      VIter vt_iter=M_vertexIter(mesh);
      while(vtx = VIter_next(vt_iter)) {
	void *data;
	EN_getDataPtr((pEntity)vtx,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)vtx,dataID);
      }
      VIter_delete(vt_iter);
    }
    break;
  case 1:
    {
      pEdge edge;
      EIter eg_iter=M_edgeIter(mesh);
      while(edge = EIter_next(eg_iter)) {
	void *data;
	EN_getDataPtr((pEntity)edge,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)edge,dataID);
      }
      EIter_delete(eg_iter);
    }
    break;
  case 2:
    {
      pFace face;
      FIter face_iter=M_faceIter(mesh);
      while(face = FIter_next(face_iter)) {
	void *data;
	EN_getDataPtr((pEntity)face,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)face,dataID);
      }
      FIter_delete(face_iter);
    }
    break;
  case 3:
    {
      pRegion rg;
      RIter rg_iter=M_regionIter(mesh);
      while(rg = RIter_next(rg_iter)) {
	void *data;
	EN_getDataPtr((pEntity)rg,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)rg,dataID);
      }
      RIter_delete(rg_iter);
    }
    break;
  case 4://all
    {
      pVertex vtx;
      VIter vt_iter=M_vertexIter(mesh);
      while(vtx = VIter_next(vt_iter)) {
	void *data;
	EN_getDataPtr((pEntity)vtx,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)vtx,dataID);
      }
      VIter_delete(vt_iter);
      
      pEdge edge;
      EIter eg_iter=M_edgeIter(mesh);
      while(edge = EIter_next(eg_iter)) {
	void *data;
	EN_getDataPtr((pEntity)edge,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)edge,dataID);
      }
      EIter_delete(eg_iter);

      pFace face;
      FIter face_iter=M_faceIter(mesh);
      while(face = FIter_next(face_iter)) {
	void *data;
	EN_getDataPtr((pEntity)face,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)face,dataID);
      }
      FIter_delete(face_iter);

      pRegion rg;
      RIter rg_iter=M_regionIter(mesh);
      while(rg = RIter_next(rg_iter)) {
	void *data;
	EN_getDataPtr((pEntity)rg,dataID,(void**)&data);

	if(array)	
	  delete [] data;
	else
	  delete data;

	EN_deleteData((pEntity)rg,dataID);
      }
      RIter_delete(rg_iter);
    }
    break;
  default :
    adaptSimLog<< "Check the en_type in cleanAttachedData(...)"<<endl;
    break;
  }
}


// build the linear system
void cvAdaptCore::buildSystem(pRegion region, double* eltMatrix)
{

  pPList regionVerts;
  regionVerts = R_vertices(region, 0);
  double X1[3];
  double X2[3];
  double X3[3];
  double X4[3];
  
  V_coord((pVertex)PList_item(regionVerts,0), X1);
  V_coord((pVertex) PList_item(regionVerts,1), X2);
  V_coord((pVertex) PList_item(regionVerts,2), X3);
  V_coord((pVertex) PList_item(regionVerts,3), X4);
  
  PList_delete(regionVerts);
  
  ///////////////////////////////////////////////////////////////////////
  // matrix used for reconstruction of the polynomial: only geometric  //
  // facts are needed                                                 //
  // The right hand side is given by the field values                  //
  ///////////////////////////////////////////////////////////////////////
  
  // coordinates
  double x1 = X1[0];
  double x2 = X2[0];
  double x3 = X3[0];
  double x4 = X4[0];
  double y1 = X1[1]; 
  double y2 = X2[1];
  double y3 = X3[1];
  double y4 = X4[1];
  double z1 = X1[2];
  double z2 = X2[2];
  double z3 = X3[2];
  double z4 = X4[2];
    
  /////////////////////////////////////////////////////////////////////////
  // we are starting with the zeroth order polynomial                    //
  // that is, m(1,1) = 1 , the coefficient in front of the constant term //
  /////////////////////////////////////////////////////////////////////////
  // first row: field at node 1 
  eltMatrix[0] = 1;  // const

  eltMatrix[1] = x1; // x

  eltMatrix[2] = y1; // y

  eltMatrix[3] = z1; // z
  // second row: field at node 2 
  eltMatrix[4] = 1;  // const

  eltMatrix[5] = x2; // x

  eltMatrix[6] = y2; // y

  eltMatrix[7] = z2; // z

  // third row: field at node 3 
  eltMatrix[8] = 1;   // const

  eltMatrix[9] = x3;  // x

  eltMatrix[10] = y3; // y

  eltMatrix[11] = z3; // z

  // forth row: field at node 4 
  eltMatrix[12] = 1;  // const

  eltMatrix[13] = x4; // x

  eltMatrix[14] = y4; // y

  eltMatrix[15] = z4; // z

}
   

// arguments are :
// mesh mod. type, look at file MeshSimAdapt.h
// data containing mesh entities created, deleted or reshaped
// userData used to set a pointer to be passed in callback function
void cvAdaptCore::phastaTransfer( MeshModType mtype, pMeshChanges mco, void *userData)  {

    CBcounter++;

    pPList fresh = NULL;
    pPList old = NULL;
    pRegion  parent_region=0;
    pRegion  rgn;
    pEntity ent;
    int numFoundOldSolution;
    int count = 0;
    int counter = 0;

    pPList newVertices = NULL;

    pPList verticesO = NULL;
    pPList verticesN = NULL;

    pVertex v = NULL;
    pVertex deleteVertex = NULL;
    int i,j,k,deletedVerts;

    double xyz[3], xietazeta[3];
    double* nodalSolution;
    double coords[3];
//     FILE  *logfile; 
    char stringName[255];
    double* averageSolution;
    double* avSol;
    double* solution;
    
//     sprintf(stringName, "adaptLog.%d",PMU_rank+1);
    //logfile=fopen(stringName,"a");
//    printf("\n entering callback fct\n");

    pPList freshEnts = MCO_createdEntities(mco); 
    pPList oldEnts  = MCO_deletedEntities(mco);
    pPList reshapedEnts = MCO_reshapedEntities(mco);

    int oldEntsSize = PList_size(oldEnts);
    int freshEntsSize = PList_size(freshEnts);
    int reshapedEntsSize = PList_size(reshapedEnts);

#ifdef DEBUG_MESHSIM_MSA_CALLBACK

    //MeshModType_TRef  Edge, Face or Region refinement. Mesh is inconsistent because the old entities are still attached to the mesh.  
    //MeshModType_VMov  Vertex motion.  
    //MeshModType_ESplit  Edge split.  
    //MeshModType_EColapse  Edge collapse.  
    //MeshModType_ESwap  Edge swap.  
    //MeshModType_ESplitClps  Edge split followed be collapse of an edge connected to the new vertex.  
    //MeshModType_FSwap  Face swap.  
    //MeshModType_RColapse  Region collapse.  
    //MeshModType_CavRetri  A portion of the mesh is removed and retriangulated.  
    //MeshModType_General  Other mesh modification.    

    fprintf(stdout,"CBcounter: %i\n",CBcounter);
    if (mtype == MeshModType_TRef) {
      fprintf(stdout,"mtype: Edge, Face or Region refinement. Mesh is inconsistent.\n");
    } else if (mtype == MeshModType_VMov) {
      fprintf(stdout,"mtype: Vertex motion.\n"); 
    } else if (mtype == MeshModType_ESplit) {
      fprintf(stdout,"mtype: Edge split.\n");
    } else if (mtype == MeshModType_EColapse) {
      fprintf(stdout,"mtype: Edge collapse.\n");
    } else if (mtype == MeshModType_ESwap) {
      fprintf(stdout,"mtype: Edge swap.\n");  
    } else if (mtype == MeshModType_ESplitClps) {
      fprintf(stdout,"mtype: Edge split followed be collapse of an edge connected to the new vertex.\n");  
    } else if (mtype == MeshModType_FSwap) {
      fprintf(stdout,"mtype: Face swap.\n");
    } else if (mtype == MeshModType_RColapse) {
      fprintf(stdout,"mtype: Region collapse.\n");  
    } else if (mtype == MeshModType_CavRetri) {
      fprintf(stdout,"mtype: A portion of the mesh is removed and retriangulated.\n"); 
    } else if (mtype == MeshModType_General) {
      fprintf(stdout,"mtype: Other mesh modification.\n");
    }

    fprintf(stdout,"num deleted entities: %i\n",oldEntsSize);
    fprintf(stdout,"num created entities: %i\n",freshEntsSize);
    fprintf(stdout,"num reshaped entities: %i\n",reshapedEntsSize);

#endif

    // this is done as callback function is invoked for
    // some mesh mod. operations which are not successful
    // this must be fixed with newer versions of Simmetrix libs.
    if(!freshEntsSize) {
      PList_delete(freshEnts);
      PList_delete(oldEnts);
      PList_delete(reshapedEnts);

      // do not count this call
      CBcounter--;

      return;
    }

    // old - holds old regions (to be deleted)
    old = PList_new();
    // freash - holds new regions (created)
    fresh = PList_new();
    // oldVertices - hold old vertices (to be deleted)
    pPList oldVertices = PList_new();
    
    int numOldRegions = 0;

    for(i=0; i<oldEntsSize; i++){
        ent = (pEntity)PList_item(oldEnts,i);
        int en_type = EN_type(ent);
        if(en_type== Tvertex) {
            PList_append(oldVertices,ent);
        }
        else if(en_type == Tregion) {
            PList_append(old,ent);
            numOldRegions++;
        }
    }

    int numFreshRegions = 0;

    for(i=0; i<freshEntsSize; i++){
        ent = (pEntity)PList_item(freshEnts,i);
        int en_type = EN_type(ent);
        if(en_type == Tregion) {
            PList_append(fresh,ent);
            numFreshRegions++;
        }
    }

#ifdef DEBUG_MESHSIM_MSA_CALLBACK
    fprintf(stdout,"num deleted regions: %i\n",numOldRegions);
    fprintf(stdout,"num created regions: %i\n",numFreshRegions);
#endif

    PList_delete(freshEnts);
    PList_delete(oldEnts);
    PList_delete(reshapedEnts);

    numFoundOldSolution = 0;
    
    ////////////////////////////////////////////////////////////////////////////////////////
    // more than 1 region in old
    ////////////////////////////////////////////////////////////////////////////////////////
    if ( PList_size( old ) > 1 ) {
        
        count = PList_size( old );
        
        // get all vertices that have some solution on them and build average
        // set this average to all NEW vertices of the NEW regions

        // looks like a memory leak to me!  (NMW)
        averageSolution= (double *)malloc(sizeof(double)*numVars );
        for( i =0; i <numVars ; i++ ){
        averageSolution[i]=0.0;
        }

/*
 *  REMOVED CODE (NMW - 2015-03-09) since it seems to cause errors
 *  with meshsim-9, but doesn't appear to actually be needed!
 *       
        for( i =0; i < count ; i++ ){
             
            verticesO = R_vertices((pRegion)PList_item(old , i ),1);

            for( j =0; j < PList_size(verticesO) ; j++ ){

                v = (pVertex)PList_item(verticesO , j );

                if( EN_getDataPtr( ( pEntity )v, phasta_solution, (void
                                                                    **)&nodalSolution) != 0){
                    for( k =0; k <numVars ; k++ ){
                        averageSolution[k] += nodalSolution[k];
                    }
                    numFoundOldSolution++;
//                    printf("\n found solution on old vertex \n" );
                }
            }

#ifdef DEBUG_MESHSIM_MSA_CALLBACK
	   // print out a debugging list of ints attached to mesh?
	   int dummy_node_id = 0;
           fprintf(stdout,"old node ids: ");
           for( j =0; j < PList_size(verticesO) ; j++ ){
	        dummy_node_id = -1;
                pVertex vertex = (pVertex)PList_item(verticesO , j );
                EN_getDataInt( (pEntity)vertex, pseudoNodeID_, &dummy_node_id );
                fprintf(stdout," %i",dummy_node_id);
	    }
	    fprintf(stdout,"\n");
#endif

	    PList_delete(verticesO);

            counter++;
        }
        if(numFoundOldSolution != 0){

          for( i =0; i <numVars ; i++ ){
              averageSolution[i] = averageSolution[i]/numFoundOldSolution;
          }

        } else {

	  //            printf("\n warning: all vertices in old region list have NO solution \n" );
          for( i =0; i <numVars ; i++ ){
              averageSolution[i] = 0.0;
          }

        }

#ifdef DEBUG_MESHSIM_MSA_CALLBACK
        fprintf(stdout,"num soln found: %i\n",numFoundOldSolution);
#endif

        // assign the average to ALL new vertices
        for( i =0; i < PList_size( fresh ) ; i++ ){

//            printf("\n got vertices of a new region \n" );

            verticesN = R_vertices((pRegion)PList_item(fresh , i ),1);

#ifdef DEBUG_MESHSIM_MSA_CALLBACK
	   // print out a debugging list of ints attached to mesh?
	   int dummy_node_id = 0;
           fprintf(stdout,"new node ids: ");
           for( j =0; j < PList_size(verticesN) ; j++ ){
	        dummy_node_id = -1;
                pVertex vertex = (pVertex)PList_item(verticesN , j );
                EN_getDataInt( (pEntity)vertex, pseudoNodeID_, &dummy_node_id );
                fprintf(stdout," %i",dummy_node_id);
	   }
	   fprintf(stdout,"\n");
	   int has_soln_already = 0;
           fprintf(stdout,"has soln already: ");
           for( j =0; j < PList_size(verticesN) ; j++ ){
	        has_soln_already = 0;
                pVertex vertex = (pVertex)PList_item(verticesN , j );
                if (EN_getDataPtr( (pEntity)vertex, phasta_solution, NULL )) {
		  has_soln_already = 1;
		} 
                fprintf(stdout," %i",has_soln_already);
                if (!has_soln_already) {
		  fprintf(stdout,"no solution, but needed???\n");
                  exit(-1);
		}
	    }
	    fprintf(stdout,"\n");
#endif

            for( j =0; j < PList_size(verticesN  ) ; j++ ){

                v =  (pVertex)PList_item(verticesN , j );

#ifdef DEBUG_MESHSIM_MSA_CALLBACK
		if (EN_getDataInt((pEntity)v, pseudoNodeID_, NULL)) {
		  // EN_modifyDataInt( (pEntity)v, pseudoNodeID_, 999999 );
		} else {
                  exit(-1);
                  EN_attachDataInt( (pEntity)v, pseudoNodeID_, 999999 );
		}
#endif

                if(! EN_getDataPtr( ( pEntity )v, phasta_solution, (void
                                                                    **)&nodalSolution)){
                
                    // provide memory for new solution
		    // avSol= (double *)malloc(sizeof(double)*numVars );
		    avSol = new double[numVars];
                    for( i =0; i <numVars ; i++ ){
                        avSol[i]=averageSolution[i];
                    }
//                    printf("\n found vertex in new regions that got assigned a sol \n" );
                    EN_attachDataPtr( (pEntity)v, phasta_solution, (void *)
                                      avSol ); 
                }
            }

	    PList_delete(verticesN);
        }
        free(averageSolution);

*
*
*/

        
    }// PList_size(old) > 1 
    ////////////////////////////////////////////////////////////////////////////////////////
    // exactly 1 region in old list ==> it is  a split operation
    ////////////////////////////////////////////////////////////////////////////////////////
    else if( PList_size( old ) == 1){
//        printf("\n  1 region in old, split type is %d\n",mtype );
        
        parent_region = (pRegion)PList_item( old, 0 );
        
        // split operation
        // retrieve new vertices
        // get all vertices on old and new element(s)
        verticesO = R_vertices(parent_region,1);
        
        newVertices = PList_new();

        for( i =0; i < PList_size( fresh ) ; i++ ){

            verticesN = R_vertices((pRegion)PList_item(fresh , i ),1);

            for( j =0; j < PList_size(verticesN  ) ; j++ ){
                
                if(!PList_inList(newVertices,PList_item(verticesN , j ))){
                     PList_append(newVertices,PList_item(verticesN , j ));
                }
            }

	    PList_delete(verticesN);
        }         
        // find the vertices which are not in the old list (-> a new vertex)
        for( i =0; i < PList_size(newVertices  ) ; i++ ){
            
            v =  (pVertex)PList_item(newVertices , i );
            V_coord(v,xyz);
             
            if(! PList_inList(verticesO,v)){

#ifdef DEBUG_MESHSIM_MSA_CALLBACK                
		if (EN_getDataInt((pEntity)v, pseudoNodeID_, NULL)) {
		  // EN_modifyDataInt( (pEntity)v, pseudoNodeID_, 999999 );
		} else {
                  EN_attachDataInt( (pEntity)v, pseudoNodeID_, 666666 );
		}
#endif

                //if there is not a solution attached already
                if(! EN_getDataPtr( ( pEntity )v, phasta_solution, (void
                                                                    **)&nodalSolution)){


                    // do a solution interpolation for that vertex
                    // calculate the parametric co-ordinates of the current vertex in the
                    // space of the parent region. ( also use these to make sure that the
                    // vertex lies inside the given region )
                    // inversemap implemented in InverseMap.cc
//                      if(! R_isValidElement(parent_region)){

//                          adaptSimLog<<"Warning in callback: found an invalid element by using R_isValidElement\n";
//                          exit(0);
//                      }
                    if ( !inverseMap( parent_region, xyz, xietazeta ) ) {	
                        printf("Warning in CB: Could not locate point inside the parent region: \n");
                        //R_info(parent_region);
                        //V_info(v);
                        CBinvalidRegionCount++;
                        exit( -1 );
                    }
                    else {	// inverse Map IS valid 
                                // be invalid
                        // interpolate the solution from the original region onto the finer one 
                        // use appropriate shapeFuns             
                        
                        // check if all verts of parent_region have data
                        // attached
                        int valid=1;
                        double* foo;
                        pPList parentVerts = R_vertices(parent_region,1);
                        for(int nu=0;nu< PList_size( parentVerts ) ; nu++ ){

                            pVertex vx = (pVertex)PList_item(parentVerts , nu );

                            if(! EN_getDataPtr( ( pEntity )vx, phasta_solution, (void
                                                                    **)&foo)){
                                
                                valid=0;
                            }
                        }
                        if(valid){
                            solution = InterpolateSolution( parent_region,
                                                            xietazeta,
                                                            numVars,
                                                            modes);
                            
                            // attach it to the vertex (if not done yet)
                            // otherwise do nothing
                            EN_attachDataPtr( (pEntity)v, phasta_solution, (void *)
                                              solution );
                            
//                    printf("\n info: assigning a solution val to a new vertex: \n" );
/*                      V_coord( v ,coords ); */
/*                      for(j=0; j<3; j++){ */
/*                          //fprintf(logfile,"%f\n",coords[j]); */
/*                      } */
                            //fprintf(logfile,"\n");
                        }//if(valid
                    }//else
                }//if(! EN_getDataPtr(  
                else{
                    // a solution already exists at that vertex: do nothing
                }
                
            }// if(! PList_inList(v
        }//for(...
       

        PList_delete(verticesO);
        PList_delete(newVertices);
    }
    else{//  size of old regions list =0
//        printf("\n  in callback.c: size of old regions list =0 \n" );
    }


    PList_delete(fresh);
    PList_delete(old);


    //
    // not sure we should even be doing this?  NMW 2015-03-09
    //

    for( i =0; i < PList_size(oldVertices) ; i++ ){
        ent = (pEntity)PList_item(oldVertices,i);
        double* data;
        if(!EN_getDataPtr( ( pEntity )ent, phasta_solution, (void
							     **)&data)) {
	  printf("\nError: solution NOT attached to an old vertex\n");
	  exit(1);

	} else {
        
          delete [] data;
	  EN_deleteData((pEntity)ent,phasta_solution);

	}
    }
    PList_delete(oldVertices);
    
}



// reconstruct the element gradient
void cvAdaptCore::elementGradient(pRegion region, double* elementGradient)
{
  pVertex v;
  double* nodalData;
  
  // build the linear system
  double matrix[16];
  buildSystem(region,matrix);
  double fieldVector[4];
  
  int four = 4;
  int indx[4];
  double fnumber;
  
  pPList regionVerts;
  regionVerts = R_vertices(region, 0);
  
  // get the field vals
  for (int i=0; i<PList_size(regionVerts); i++) {
    v = (pVertex)PList_item(regionVerts,i);
    if(EN_getDataPtr((pEntity)v, errorIndicatorID,
		     (void**)&nodalData)==NULL){
      adaptSimLog<<"\nerror in elementGradient: no data attached to vertex\n";
      exit(0);
    }
    fieldVector[i]=nodalData[4];// speed component
  }
  PList_delete(regionVerts);    
  
  ludcmp(matrix , &four, &four, indx, &fnumber);
  
  // fieldVector is going to be overridden and will
  // contain the solution
  lubksb(matrix , &four, &four, indx, fieldVector );
  
  for(int i=0; i<3; i++) {
    // the poly's structure is
    // a0 + a1*x + a2*y + a3*z
    elementGradient[i] = fieldVector[i+1];
    
#if  ( defined  DEBUG )
    //        adaptSimLog<<"\ngradient["<<i<<"] "<<elementGradient[i]<<"\n";
#endif
  }
 
}


// reconstruct the element hessian : 6-component (symmetric)
void cvAdaptCore::elementHessian(pRegion region, double* elemHessian)
{
  pVertex v;
  double* nodalGradient;
  
  // build the linear system
  double matrix[16];
  buildSystem(region,matrix);
  
  double  fieldVectorX[4];
  double fieldVectorY[4];
  double fieldVectorZ[4];
  int four = 4;
  int indx[4];
  double fnumber;
  
  pPList regionVerts;
  regionVerts = R_vertices(region, 0);
  
  // get the field vals
  for (int i=0; i<PList_size(regionVerts); i++) {
    v = (pVertex)PList_item(regionVerts,i);
    if(EN_getDataPtr((pEntity)v, nodalgradientID,
		     (void**)&nodalGradient)==NULL){
      adaptSimLog<<"\nerror in elementHessian: no data attached to vertex\n";
      exit(0);
    }
    fieldVectorX[i]= nodalGradient[0];
    fieldVectorY[i]= nodalGradient[1];
    fieldVectorZ[i]= nodalGradient[2];
  }
  PList_delete(regionVerts);    
  
  ludcmp(matrix , &four, &four, indx, &fnumber);
  
  // fieldVector is going to be overridden and will
  // contain the solution
  lubksb(matrix , &four, &four, indx, fieldVectorX );
  lubksb(matrix , &four, &four, indx, fieldVectorY );
  lubksb(matrix , &four, &four, indx, fieldVectorZ );

  // each poly is 
  // a0 + a1*x + a2*y + a3*z  
  elemHessian[0] = fieldVectorX[1];// xx
  elemHessian[1] = fieldVectorX[2];// xy
  elemHessian[2] = fieldVectorX[3];// xz
  elemHessian[3] = fieldVectorY[2];// yy
  elemHessian[4] = fieldVectorY[3];// yz
  elemHessian[5] = fieldVectorZ[3];// zz
  
#if  ( defined  DEBUG )
  //     for(int i=0; i<6; i++) {
  //         adaptSimLog<<"\nhessian["<<i<<"] "<<elemHessian[i]<<"\n";
  //     }
#endif
 
}
   


// just take the value from any adjacent vertex
void cvAdaptCore::fix4SolutionTransfer(pMesh mesh)
{

    VIter vit=M_vertexIter(mesh);
    pVertex vertex;
    double* nodalData;
    while(vertex=VIter_next(vit)) { 

        // if the node has NO solution, i.e. it is a NEW node
      if(!EN_getDataPtr((pEntity)vertex, phasta_solution, NULL)) {

            int foundVertex=0;
            // need to find a neighbor for this one
            int nedges = V_numEdges( vertex);
            for(int j=0; j<nedges;j++){
                
                pEdge edge = V_edge(vertex,j);
                
                pVertex vex = E_otherVertex(edge,vertex);

                if(EN_getDataPtr((pEntity)vex, phasta_solution ,
		     (void**)&nodalData)){

                    double* sol = new double[numVars];
                    
                    for(int k=0;k<numVars;k++){
                        sol[k] = nodalData[k];
                    }
                    EN_attachDataPtr( (pEntity)vertex, phasta_solution, (void *)
		      sol );
                    foundVertex=1;
                    break;
                }
            }
            if(!foundVertex){
                adaptSimLog<<"\nerror in fix4SolutionTransfer:"
                    <<"all surrounding vertices have no solution\n";
                exit(0);
            }
        }
    }
    VIter_delete(vit);
}                
                


// recover gradient at a vertex using patch of elements around it
void cvAdaptCore::gradientsFromPatch(pMesh mesh)
{
  VIter vIter;
  vIter = M_vertexIter(mesh);
  pVertex vertex;
  
  double elemGradient[3];
  
  double patchVolume;
  
  // loop over vertices and get patch of elements
  while(vertex = VIter_next(vIter)) {
    pPList elementPatch;
    elementPatch = V_regions(vertex);
    patchVolume = 0;

    double* patchGradient = new double[3];
    for(int i=0; i<3; i++){
      patchGradient[i]= 0.0  ;
    }

    // loop over elements and recover gradient at vertex   
    for (int i=0; i<PList_size(elementPatch); i++) {
      patchVolume+=R_volume((pRegion) PList_item(elementPatch,i));
      
      // compute element grad. for each element in the patch at vertex
      elementGradient( (pRegion)PList_item(elementPatch,i), elemGradient);
      
      // use element grad. (each component) to recover gradient at vertex
      for(int j=0; j<3; j++) {
	patchGradient[j]= patchGradient[j] + 
	  elemGradient[j]* R_volume((pRegion) PList_item(elementPatch,i));
      }
      
    }//loop over element patch ends
    
    for(int i=0; i<3; i++) {
      patchGradient[i]= patchGradient[i] / patchVolume ;
    }

    // attach the recovered gradient to the vertex
    EN_attachDataPtr( (pEntity)vertex, nodalgradientID, (void *)
		      patchGradient );
    
    PList_delete(elementPatch);
  }
  VIter_delete(vIter);

}

      
// recover hessian using a patch (from gradients)
// hessian returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
void cvAdaptCore::hessiansFromPatch(pMesh mesh)
{
  VIter vIter;
  vIter = M_vertexIter(mesh);
  pVertex vertex;
  
  double elemHessian[6];
  double patchVolume;
  
  // loop over vertices and get patch of elements
  while(vertex = VIter_next(vIter)) {
    pPList elementPatch;
    elementPatch = V_regions(vertex);
    patchVolume=0;

    double* patchHessian = new double[6];
    for(int i=0; i<6; i++) {
      patchHessian[i]=0.0;
    }
    
    // loop over elements and recover hessian at vertex
    for (int i=0; i < PList_size(elementPatch); i++) {
      patchVolume+=R_volume((pRegion) PList_item(elementPatch,i));

      // compute element hessian (each component) to recover hessian at vertex
      elementHessian( (pRegion)PList_item(elementPatch,i), elemHessian);
      
      // use element hessian (each component) to recover hessian at vertex
      for(int j=0; j<6; j++) {
	patchHessian[j]= patchHessian[j] + 
	  elemHessian[j]* R_volume((pRegion) PList_item(elementPatch,i));
      }
      
    }//loop over element patch ends
    
    for(int i=0; i<6; i++) {
      patchHessian[i]= patchHessian[i] / patchVolume ;
    }

    // attach the recovered hessian to the vertex    
    EN_attachDataPtr( (pEntity)vertex, nodalhessianID, (void *)
		      patchHessian );

    PList_delete(elementPatch);
  }//while (vertex = VIter_next(vIter))      
  VIter_delete(vIter);

#if  ( defined  DEBUG )
  //    writeRestartHessians(mesh);
#endif//DEBUG      
}


// hessian  returned : 6-component (symmetric)
// u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
// the nodal data later can be retrieved via
// nodalHessianID
void cvAdaptCore::hessiansFromSolution(pMesh mesh,int stepNumber)
{  
  // compute the hessain field from the solution

  nodalgradientID  =  MD_newMeshDataId( "gradient");
  nodalhessianID  =  MD_newMeshDataId( "hessian");
  
  // recover gradients using a patch
  // attaches gradient to vertices
  // gradient attached via nodalgradientID
  gradientsFromPatch(mesh);
  
  // recover hessians using a patch (from gradients)
  // attaches hessian to vertices
  // hessian attached via  nodalhessianID
  // hessian  attached : 6-component (symmetric)
  // u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
  hessiansFromPatch(mesh);

  // no need to call ModifyHessiansAtBdry 
  // as it is embedded in SmoothHessians
  // ModifyHessiansAtBdry(mesh);
  SmoothHessians(mesh);
}


// this routine tags/marks the mesh entities for refinement (i.e., tag driven)
// as of now only tags the edges (later, may introduce other choices)
// tags entities for refinement which have error values greater than threshold
// as of now do not use hmin and hmax
// can introduce one more factor to compute threshold for coarsening
int cvAdaptCore::applyMarkingStrategy(pMesh mesh, pMSAdapt simAdapter,
		     double factor, double hmin, double hmax,
		     double &totalError, double &maxError, double &minError,
		     double &threshold, int option) 
{
  threshold = getErrorThreshold(mesh,factor,totalError,
				maxError,minError,option);

  pEdge edge;    
  double *tagged = new double;
  pMeshDataId tagged_edges = MD_newMeshDataId("tagged edges");

  pVertex vertex;
  VIter vIter = M_vertexIter(mesh);
  while(vertex = VIter_next(vIter)) {
    double *nodalValues;
    if(EN_getDataPtr((pEntity)vertex,errorIndicatorID,(void**)&nodalValues) == NULL) {
      adaptSimLog<<"\nerror in applyMarkingStrategy(...) : no solution attached to vertex"<<endl;
      exit(0);
    }
    
    double errorValue = getErrorValue(nodalValues,option);

    // If the error is above threshold
    // tag/mark all the edges of vertex for refinement
    // threshold is fraction of maximum error 
    if(errorValue > threshold) {
      int numEdges = V_numEdges(vertex);
      for (int i=0; i < numEdges; i++) {
	edge = V_edge(vertex,i);
	MSA_setRefineLevel(simAdapter,(pEntity)edge,1);
	if(EN_getDataPtr((pEntity)edge,tagged_edges,(void**)&tagged) == NULL) {
	  EN_attachDataPtr((pEntity)edge,tagged_edges,(void *)tagged);
	}
      }
    }
  }
  VIter_delete(vIter);

  int edgesTagged = 0;
  EIter eIter = M_edgeIter(mesh);
  while(edge = EIter_next(eIter)) {
    if(EN_getDataPtr((pEntity)edge,tagged_edges,(void**)&tagged) != NULL) {
      edgesTagged++;
      EN_deleteData((pEntity)edge,tagged_edges);
    }
  }
  EIter_delete(eIter);

  delete tagged;
  MD_deleteMeshDataId(tagged_edges);

  return edgesTagged;
}
  

double cvAdaptCore::getErrorThreshold(pMesh mesh, double factor,
		  double &totalError, 
		  double &maxError, double &minError,
		  int option)
{
  maxError = 0.;
  minError = 1.e15;
  totalError = 0.;
  
  // std::ofstream errorLog("error.log");
  
  pVertex vertex;
  VIter vIter = M_vertexIter(mesh);
  while(vertex = VIter_next(vIter)) {
    double *nodalValues;
    if(EN_getDataPtr((pEntity)vertex,errorIndicatorID,(void**)&nodalValues) == NULL) {
      adaptSimLog<<"\nerror in getErrorThreshold(...) : no error data attached to vertex"<<endl;
      exit(0);
    }

    double errorValue = getErrorValue(nodalValues,option);

    if(errorValue > maxError) {
      maxError = errorValue;
    }

    if(errorValue < minError) {
      minError = errorValue;
    }

    totalError+=errorValue;

    // errorLog<<errorValue<<endl;
  }
  VIter_delete(vIter);

  // errorLog.close();

  totalError = sqrt(totalError);

  return factor*maxError;
} 

// option is to decide how to compute the error value
// (i.e., use 3 EI for flow problem or use 1 EI for scalar problem)
double cvAdaptCore::getErrorValue(double *nodalValues, int option) {

  double errorValue = 0.;

  switch(option) {
  case 0:
    {
      double weight = 1.;
      errorValue = weight*nodalValues[0];  
    }
    break;
  case 1:
    {
      double weight[3]={1.,1.,1.};
      for (int i=0; i<3; i++) {
	errorValue += weight[i]*nodalValues[i];
      }
    }
    break;
  default :
    adaptSimLog<<"\nSpecify correct `option' to compute error value in getErrorValue(...)"<<endl;
    break;
  }
  return errorValue;
}


// to read parameters from a phasta file (filename)
// parameters correspond to nshg & nvar, i.e., size of field-array
// these parameters are used as reference values 
// (sometimes needed before reading the field-array)
void cvAdaptCore::readParametersFromFile(char *filename,
			    char *fieldName,
			    int &nshg, 
			    int &numVars) {

  // read file (i.e., restart, error etc.)
  // fileDescriptor
  int restart;
  // format of the file
  char* iformat = "binary";

  openfile_( filename, "read",  &restart );

  // contains: nshg,numVars,lstep
  int iarray[4];
  // don't know what is this for
  // think it loops over token `<>' 
  // inside phastaIO.cc in readHeader(...)
  int isize = 3;
  
  readheader_( &restart, fieldName, iarray,
	       &isize, "double", iformat );

  // nshg * numVars
  nshg=iarray[0];
  numVars=iarray[1];

  closefile_(&restart, "read");
}

// to read array from a phasta file (filename)
// memory is allocated HERE for 'valueArray'
// `fieldName' tells which block to read like solution, error etc.
void cvAdaptCore::readArrayFromFile( char *filename,
			char *fieldName,
			double *&valueArray) {
  // read file (i.e., restart, error etc.)
  // fileDescriptor
  int restart;
  // format of the file
  char* iformat = "binary";

  cvsolverIO* reader = new cvsolverIO();

  if ( reader->openFile( filename, "read" ) != CVSOLVER_IO_OK ) {
    fprintf(stderr,"error opening file (%s).\n",filename);
    exit(-1);
  }

  // contains: nshg,numVars,lstep
  int iarray[4];
  // don't know what is this for
  // think it loops over token `<>' 
  // inside phastaIO.cc in readHeader(...)
  int isize = 3;
  
  if ( reader->readHeader( fieldName, iarray, 
                           isize, "double", iformat ) != CVSOLVER_IO_OK ) {
    fprintf(stderr,"error reading field (%s).\n",fieldName);
    exit(-1);
  }

  // nshg * numVars
  int nshg=iarray[0];
  int numVars=iarray[1];
  isize = iarray[0]*iarray[1];

  double* q = new double[nshg*numVars];

  if ( reader->readDataBlock( fieldName, q, isize,
		      "double" , iformat ) != CVSOLVER_IO_OK ) {
    fprintf(stderr,"error opening file (%s).\n",filename);
    exit(-1);
  }


  valueArray = new double[nshg*numVars];

  fprintf(stdout,"nshg numVars: %i %i\n",nshg,numVars);

  for(int i = 0; i< nshg; i++){
    for( int j=0; j< numVars; j++){
      valueArray[i*numVars+j] = q[j*nshg+i];
    }
  }
  
  delete [] q;

  if ( reader->closeFile() != CVSOLVER_IO_OK ) {
    fprintf(stderr,"error closing file (%s).\n",filename);
    exit(-1);
  }

}

// to write array to a phasta file (filename)
// NOTE: array should be transposed!!!
// `fieldName' tells in which block to write like solution, error etc.
// `outputFormat' tells in which format to write, i.e., binary/ascii
// `mode' : "write", "appeand" etc.
void cvAdaptCore::writeArrayToFile( char *filename,
		       char *fieldName,
		       char *outputFormat,
		       char *mode,
		       int nshg,
		       int numVars,
		       int stepNumber,
		       double *valueArray) {
 
  int restart;
  char fname[256];
  int iarray[10];
  int size, nitems;
  
  openfile_( filename, mode, &restart );

  writestring_( &restart,"# PHASTA Input File Version 2.0\n");
  writestring_( &restart, "# Byte Order Magic Number : 362436 \n");

  fname[0]='\0';
  sprintf(fname,"# Output generated by phAdapt version: 0 \n");
  writestring_( &restart, fname );

  time_t timenow = time ( &timenow);
  fname[0]='\0';
  sprintf(fname,"# %s\n", ctime( &timenow ));
  writestring_( &restart, fname );

  size = 1;
  nitems = 1;
  iarray[0] = 1;
  int magic_number = 362436;
  int* mptr = &magic_number;

  writeheader_( &restart, "byteorder magic number ",
		(void*)iarray, &nitems, &size, "integer", outputFormat );

  writedatablock_( &restart, "byteorder magic number ",
		   (void*)mptr, &nitems, "integer", outputFormat );          

  bzero( (void*)fname, 256 );
  sprintf(fname,"number of modes : < 0 > %d\n", nshg);
  writestring_( &restart, fname );
    
  bzero( (void*)fname, 256 );
  sprintf(fname,"number of variables : < 0 > %d\n", numVars);
  writestring_( &restart, fname );
    
  size =  nshg*numVars;
  nitems = 3; // length of array
  iarray[0] = nshg;
  iarray[1] = numVars;
  iarray[2] = stepNumber;

  writeheader_( &restart, fieldName,
		( void* )iarray, &nitems, &size,"double", outputFormat );

  nitems = nshg*numVars; // length of array
  writedatablock_( &restart, fieldName,
		   ( void* )(valueArray), &nitems, "double", outputFormat );

  closefile_( &restart, mode);
}


void cvAdaptCore::setSizeFieldUsingHessians(pMesh mesh,
			       pMSAdapt simAdapter,
			       double factor,
			       double hmax,
			       double hmin,
			       int option,
                               double sphere[5])
{
  int nshg=M_numVertices(mesh);  // only true for linear elements
  double tol=1.e-12;
  
  double T[3][3];
  double maxEigenval=0;
  int i,j;
  
  pVertex vertex;
  double eloc;  	  // local error at a vertex
  double etot=0.;	  // total error for all vertices
  double emean; 	  // emean = etot / nv
  double elocmax=0.;	  // max local error
  double elocmin=1.e20;   // min local error
  int bdryNumNodes = 0;

  // struct Hessian contains decomposed values
  // mesh sizes and directional information
  Hessian *hess = new Hessian[nshg];

  VIter vit=M_vertexIter(mesh);
  i=0;
  while(vertex=VIter_next(vit)) { 
    if (EN_isBLEntity(vertex)) continue;
    double xyz[3];
    V_coord(vertex,xyz);

    // get hessian of appropriate variable at a `vertex'
    if(option==1) 
      V_Hessian(vertex,T);
    else
      V_AnalyticHessian(vertex,T,option);
 
    // `T' is the actual Hessian 
    // of any solution variable (e.g., T, u, v, w etc.)
    // `hess' has the information related to Mesh-Metric (eventually)
    // used for mapping the ellipsoid into a unit sphere 
    
    // compute eigen values and eigen vectors
//      int nbEigen = eigen(T,hess[i].dir,hess[i].h);

//      adaptSimLog<<"eigenvals/eigenVecs (eigen-Li)\n";
//      for(int j=0; j< 3; j++){

//          adaptSimLog<<hess[i].h[j]<<"\n";
//          adaptSimLog<<hess[i].dir[0][j]<<" "<<hess[i].dir[1][j]<<" "<<hess[i].dir[2][j]<<"\n";
//      }
        


//      if(nbEigen == 1) {
//        printf(" Vertex Number : %d\n",i);
//        printf(" xyz : %f %f %f \n",xyz[0],xyz[1],xyz[2]);
//        printf("       %f %f %f %f %f %f\n\n",T[0][0], 
//               T[0][1],T[0][2],T[1][1],T[1][2],T[2][2]);
//      }
//      adaptSimLog<<"Hessian\n";
    
//      for(int j=0; j< 3; j++){
//              adaptSimLog<<T[j][0]<<" "<<T[j][1]<<" "<<T[j][2]<<"\n";
//      }

    double eigenVals[3];
    double e[3];
    

    // copy T into temporary buffer
    double Tfoo[3][3];
    for(int j=0; j< 3; j++){
        for(int k=0; k< 3; k++){       
            
            Tfoo[j][k]=T[j][k];
        }
    } 

    int three = 3;
    double z[3][3];
    mytred(&three,&three,Tfoo,eigenVals,e,z);
//    adaptSimLog<<"eigenvals/eigenVecs (tred)\n";

    int ierr;
    tql2(&three,&three,eigenVals,e,z,&ierr);

    for(int j=0; j< 3; j++){
        hess[i].h[j]=eigenVals[j];
        for(int k=0; k< 3; k++){   
            hess[i].dir[j][k]=z[j][k];
        }
    }

//      for(int j=0; j< 3; j++){

//          adaptSimLog<<hess[i].h[j]<<"\n";
//          adaptSimLog<<hess[i].dir[0][j]<<" "<<hess[i].dir[1][j]<<" "<<hess[i].dir[2][j]<<"\n";
//      }
        
    
           

    hess[i].h[0] = ABS(hess[i].h[0]);
    hess[i].h[1] = ABS(hess[i].h[1]);
    hess[i].h[2] = ABS(hess[i].h[2]);
    
    if( MAX(hess[i].h[0],MAX(hess[i].h[1],hess[i].h[2])) < tol ) {
      printf("Warning: zero maximum eigenvalue for node %d !!!\n",i);
      printf("       %f %f %f\n", hess[i].h[0],
             hess[i].h[1],hess[i].h[2]);

      adaptSimLog<<"Error: zero maximum eigenvalue !!!"<<endl;
      adaptSimLog<<hess[i].h[0]<<" "<<hess[i].h[1]<<" "<<hess[i].h[2]<<endl;
      adaptSimLog<<T[0][0]<<" "<<T[0][1]<<" "<<T[0][2]<<" ";
      adaptSimLog<<T[1][1]<<" "<<T[1][2]<<" "<<T[2][2]<<endl;
      // exit(1);
      continue;
    }

    // estimate relative interpolation error
    // needed for scaling metric field (mesh size field)
    // to get an idea refer Appendix A in Li's thesis
    eloc=maxLocalError(vertex,T);
    etot += eloc;
    if( eloc>elocmax )  elocmax=eloc;
    if( eloc<elocmin )  elocmin=eloc;

    ++i;
  }
  printf("Info: Reading hessian... done...\n");

  emean =  etot / nshg;
  printf("\n Info on relative interpolation error: ");
  printf("   total: %f\n",etot);
  printf("   mean:  %f\n",emean);
  printf("   max local: %f\n",elocmax);
  printf("   min local: %f\n",elocmin);

  eloc = emean*factor;

  printf("\n towards uniform local error distribution of %f\n", eloc);
  printf("   with max edge length=%f; min edge length=%f\n\n",hmax,hmin);

  adaptSimLog<<"MeshSim   Library build ID : "<<SimMeshing_buildID()<<endl;
  adaptSimLog<<"MeshTools Library build ID : "<<SimMeshTools_buildID()<<endl;
  adaptSimLog<<endl;

  adaptSimLog<<"Strategy chosen is anisotropic adaptation, i.e., size-field driven"<<endl;
  if(option==1)
    adaptSimLog<<"Using numerical/computed hessian... done..."<<endl;
  else
    adaptSimLog<<"Using analytic hessian... done..."<<endl;
  adaptSimLog<<"Info on relative interpolation error :"<<endl;
  adaptSimLog<<"total : "<<etot<<endl;
  adaptSimLog<<"mean : "<<emean<<endl;
  adaptSimLog<<"factor : "<<factor<<endl;
  adaptSimLog<<"min. local : "<<elocmin<<endl;
  adaptSimLog<<"max. local : "<<elocmax<<endl;
  adaptSimLog<<"towards uniform local error distribution of "<<eloc<<endl;
  adaptSimLog<<"with min. edge length : "<<hmin<<endl;
  adaptSimLog<<"with max. edge length : "<<hmax<<endl;

  int foundHmin = 0;
  int foundHmax = 0;
  int hminCount = 0;
  int hmaxCount = 0;
  int bothHminHmaxCount = 0;
  int insideSphereCount = 0;

  double tol2, tol3;
  // scale the hessian
  // double errorSqRoot = hmin*sqrt(maxEigenval);
  VIter_reset(vit);
  i=0;
  while ( vertex=VIter_next(vit)) {
    // skip vertex if it's in boundary layer
    if (EN_isBLEntity(vertex)) {
      bdryNumNodes++;
      continue;
    }
    tol2 = 0.01*hmax;
    tol3 = 0.01*hmin;
    foundHmin = 0;
    foundHmax = 0;
    for( j=0; j<3; j++ ) {
      if( hess[i].h[j] < tol )
        hess[i].h[j] = hmax;
      else {
        hess[i].h[j] = sqrt(eloc/hess[i].h[j]);
        if( hess[i].h[j] > hmax )
          hess[i].h[j] = hmax;
        if( hess[i].h[j] < hmin )
          hess[i].h[j] = hmin;
      }
    }

    for(j=0; j<3; j++) {
      if(ABS(hess[i].h[j]-hmax) <= tol2)
	foundHmax = 1;
      if(ABS(hess[i].h[j]-hmin) <= tol3)
	foundHmin = 1;
    }
    if(foundHmin)
      hminCount++;
    if(foundHmax)
      hmaxCount++;
    if(foundHmin && foundHmax)
      bothHminHmaxCount++;

    if ( sphere[0] > 0 ) {

      double vxyz[3];
      V_coord(vertex, vxyz);
      // check if inside of sphere radius
      double r = sqrt ((vxyz[0] - sphere[1])*(vxyz[0] - sphere[1]) + 
                       (vxyz[1] - sphere[2])*(vxyz[1] - sphere[2]) +
                       (vxyz[2] - sphere[3])*(vxyz[2] - sphere[3]));

      if (r < sphere[0]) {

        hess[i].h[0] = sphere[4];
        hess[i].h[1] = sphere[4];
        hess[i].h[2] = sphere[4];

        insideSphereCount++;

      }

    }

    // Modify mesh-metric to take special care
    // like in parallel plates (i.e, w=f(y)) 
    // if a user wants different hmax in x-direction
    // other examples can be situations where
    // user don't want to have one mesh edge 
    // connecting two geometric model edges (like no dofs)
    // ModifyMetric(vertex,hess[i].dir,hess[i].h);

    // set the data in MeshSim format
    for (int jRow=0; jRow<3; jRow++) {
      for(int iDir=0; iDir<3; iDir++) {
        hess[i].dir[jRow][iDir]=hess[i].dir[jRow][iDir]*hess[i].h[jRow];
      }
    }

    MSA_setAnisoVertexSize(simAdapter, 
        		   vertex,
        		   hess[i].dir);
//      adaptSimLog<<"SIZE node "<<i<<"\n";
//      for(int j=0; j< 3; j++)
//         adaptSimLog<<hess[i].dir[j][0]<<" "<<hess[i].dir[j][1]<<" "<<hess[i].dir[j][2]<<"\n";
//      adaptSimLog<<"\n";
       

    ++i;
  }
  VIter_delete(vit);

  adaptSimLog<<"Nodes with hmin into effect : "<<hminCount<<endl;
  adaptSimLog<<"Nodes with hmax into effect : "<<hmaxCount<<endl;
  adaptSimLog<<"Nodes with both hmin/hmax into effect : "<<bothHminHmaxCount<<endl;
  adaptSimLog<<"Nodes within sphere : "<<insideSphereCount<<endl;
  adaptSimLog<<"Nodes ignored in boundary layer : "<<bdryNumNodes<<endl<<endl;

  writeMEDITSizeField(hess,mesh);

  if(option==1) {
    cleanAttachedData(mesh,nodalgradientID,0);
    cleanAttachedData(mesh,nodalhessianID,0);
    
    MD_deleteMeshDataId(nodalgradientID);
    MD_deleteMeshDataId(nodalhessianID);
  }

  delete [] hess;
}

// max relative interpolation error at a vertex
double cvAdaptCore::maxLocalError(pVertex vertex, double H[3][3])
{
  pEdge edge;
  double locE;
  double maxLocE=0;

  for(int i=0; i<V_numEdges(vertex); i++ ) {
    edge=V_edge(vertex,i);
    locE=E_error(edge,H);
    if( locE > maxLocE )
      maxLocE=locE;
  }

  return maxLocE;
}

// relative interpolation error along an edge
double cvAdaptCore::E_error(pEdge edge, double H[3][3])
{
  double xyz[2][3], vec[3];
  double locE=0;
  int i,j;

  V_coord(E_vertex(edge,0),xyz[0]);
  V_coord(E_vertex(edge,1),xyz[1]);

  //  diffVt(xyz[0],xyz[1],vec);
  // do we need to normalize this vector???
  vec[0] = xyz[1][0] - xyz[0][0];
  vec[1] = xyz[1][1] - xyz[0][1];
  vec[2] = xyz[1][2] - xyz[0][2];

  for( i=0; i<3; i++ )
    for( j=0; j<3; j++ ) 
      locE += H[i][j]*vec[i]*vec[j];

  return ABS(locE);
}


void cvAdaptCore::setIsotropicSizeField(pMesh mesh,
			   pMSAdapt simAdapter,
			   double factor,
			   double hmax, 
			   double hmin,
			   int option)
{
  // for 3D problems
  int dim = 3;
  // assuming linear basis
  int poly_order = 1;

  double totalError = 0.;
  double threshold  = 0.;
  double sumOfError = 0.;

  pVertex vertex;
  VIter vIter = M_vertexIter(mesh);
  while(vertex = VIter_next(vIter)) {
    double *nodalValue;
    if(EN_getDataPtr((pEntity)vertex,errorIndicatorID,(void**)&nodalValue) == NULL) {
      adaptSimLog<<"\nerror in setIsotropicSizeField(...) : no error data attached to vertex"<<endl;
      exit(0);
    }

    // eta_domain^2 = sum_k (eta_k^2)
    totalError += *nodalValue;
    // eta_sum = sum_k (eta_k)
    sumOfError += sqrt(*nodalValue);
  }

  totalError = sqrt(totalError);

  std::ofstream sizes("isoSize.dat");  
  std::ofstream adaptFactorFile("adaptFactor.dat");

  VIter_reset(vIter);  
  while(vertex = VIter_next(vIter)) {
    double *nodalValue;
    if(EN_getDataPtr((pEntity)vertex,errorIndicatorID,(void**)&nodalValue) == NULL) {
      adaptSimLog<<"\nerror in setIsotropicSizeField(...) : no error data attached to vertex"<<endl;
      exit(0);
    }

    double oldSize = 0.;
    pEdge edge;
    int numEdges = V_numEdges(vertex);
    // old size at a location (vertex)
    // is mean value of the lengths 
    // of edges around (seems ok for isotropic meshes)
    // (can have different choices)
    for (int i=0; i < numEdges; i++) {
      edge = V_edge(vertex,i);
      oldSize += E_length(edge);
    }
    oldSize = oldSize/numEdges;

    threshold = factor*totalError;
    // based on mesh optimal criteria and error convergence rate
    // implemented for linear cases
    // refer L.-Y. Li, Comm. Num. Meth. Eng.,
    // Vol. 11, 857-868 & 911-915 (1995)
    double adaptFactor = threshold/(pow(*nodalValue,0.25)*sqrt(sumOfError));

    double MaxRefineFactor = hmin;
    double MaxCoarsenFactor = hmax;

    // these factors set cut-off levels for adaptation
    // can set such that no coarsening (or refinement) occurs
    if (MaxRefineFactor > 1.e-10  && adaptFactor < 1.0/MaxRefineFactor) 
      adaptFactor = 1.0/MaxRefineFactor;

    if (MaxCoarsenFactor > 1.e-10 && adaptFactor > MaxCoarsenFactor) 
      adaptFactor = MaxCoarsenFactor;

    double newSize = adaptFactor*oldSize;

    MSA_setVertexSize(simAdapter, 
		      vertex,
		      newSize);

    sizes<<newSize<<"\n";
    adaptFactorFile<<adaptFactor<<"\n";
  }
  VIter_delete(vIter);

  sizes.close();
  adaptFactorFile.close();

  adaptSimLog<<"Info. on adaptation parameters are: "<<endl;
  adaptSimLog<<"Total Error      : "<<totalError<<endl;
  adaptSimLog<<"factor           : "<<factor<<endl;
  adaptSimLog<<"Threshold        : "<<threshold<<endl;
  adaptSimLog<<"sumOfError       : "<<sumOfError<<endl;
  adaptSimLog<<"MaxCoarsenFactor : "<<hmax<<endl;
  adaptSimLog<<"MaxRefineFactor  : "<<hmin<<"\n"<<endl;

  std::ofstream adaptSimLog("adaptor.log");

  adaptSimLog<<"Strategy chosen is size-field driven for isotropic adaptation"<<endl;  
  adaptSimLog<<"Info. on adaptation parameters are: "<<endl;
  adaptSimLog<<"Total Error      : "<<totalError<<endl;
  adaptSimLog<<"factor           : "<<factor<<endl;  
  adaptSimLog<<"Threshold        : "<<threshold<<endl;
  adaptSimLog<<"sumOfError       : "<<sumOfError<<endl;
  adaptSimLog<<"MaxCoarsenFactor : "<<hmax<<endl;
  adaptSimLog<<"MaxRefineFactor  : "<<hmin<<endl;
  adaptSimLog.close();
}


void cvAdaptCore::setManualSizeField(pMesh mesh,
		   pMSAdapt simAdapter, 
        	   int strategy) {
  double dir[3][3];
  char option[28];
  
  switch(strategy) {
  case -1:
    {
      sprintf(option,"constant");

      dir[0][0]=0.1;//x-direction
      dir[1][1]=0.1;//y-direction
      dir[2][2]=6.0;//z-direction
    
      dir[0][1]=dir[0][2]=0.;
      dir[1][0]=dir[1][2]=0.;
      dir[2][0]=dir[2][1]=0.;

      pVertex vertex;
      VIter vit=M_vertexIter(mesh);
      while(vertex=VIter_next(vit)) {   
	MSA_setAnisoVertexSize(simAdapter, 
			       vertex,
			       dir);
      }
      VIter_delete(vit);
    }
    break;
  case -2:
    {
      sprintf(option,"cylindrical");

      double norm,tol=1.e-8;
    
      double sizeR=0.1;
      double sizeTheta=0.1;
      double sizeZ=1.0;

      pVertex vertex;
      VIter vit=M_vertexIter(mesh);
      while(vertex=VIter_next(vit)) {  
	double xyz[3];
	V_coord(vertex,xyz);
	norm=sqrt(xyz[0]*xyz[0]+xyz[1]*xyz[1]);
	if( norm>tol ) 
	  {
	    dir[0][0]=sizeR*(xyz[0]/norm);
	    dir[0][1]=sizeR*(xyz[1]/norm);
	    dir[0][2]=0.;
	    dir[1][0]=-sizeTheta*(xyz[1]/norm);
	    dir[1][1]=sizeTheta*(xyz[0]/norm);
	    dir[1][2]=0.;
	    dir[2][0]=0.;
	    dir[2][1]=0.;
	    dir[2][2]=sizeZ;
	  }
	else
	  {
	    dir[0][0]=sizeR;
	    dir[0][1]=0.;
	    dir[0][2]=0.;
	    dir[1][0]=0.;
	    dir[1][1]=sizeTheta;
	    dir[1][2]=0.;
	    dir[2][0]=0.;
	    dir[2][1]=0.;
	    dir[2][2]=sizeZ;
	  }

	MSA_setAnisoVertexSize(simAdapter, 
			       vertex,
			       dir);
      }
      VIter_delete(vit);
    }
    break;
  default :
    adaptSimLog<<"check strategy [in setManualSizefield(...)]"<<endl;
    exit(-1);
    break; 
  }

  std::ofstream adaptSimLog("adaptor.log");
  adaptSimLog<<"Strategy chosen for adaptation is size-field driven"<<endl;
  adaptSimLog<<"Mesh size-field is set manually"<<endl;
  adaptSimLog<<"Size-field option : "<<option<<endl;
  adaptSimLog.close();
}


// tag the entities to be refinement (for isotropic refinement)
// factor is used to evaluate the threshold for refinement
// as of now do not use hmin and hmax 
void cvAdaptCore::tagEntitiesForRefinement(pMesh mesh,
			      pMSAdapt simAdapter,
			      double factor,
			      double hmax, 
			      double hmin,
			      int option)
{
  double totalError = 0.;
  double maxError = 0.;
  double minError = 0.;
  double threshold = 0.;

  int edgesTagged = applyMarkingStrategy(mesh,simAdapter,factor,hmax,hmin,
					 totalError,maxError,minError,
					 threshold,option);

  adaptSimLog<<" Info. on adaptation parameters are: "<<endl;
  adaptSimLog<<" Total Error : "<<totalError<<endl;
  adaptSimLog<<" Max. Error  : "<<maxError<<endl;
  adaptSimLog<<" Min. Error  : "<<minError<<endl;
  adaptSimLog<<" Threshold   : "<<threshold<<endl;
  adaptSimLog<<" factor : "<<factor<<endl;
  adaptSimLog<<" hmax   : "<<hmax<<endl;
  adaptSimLog<<" hmin   : "<<hmin<<endl;
  adaptSimLog<<"\nNumber of edges tagged to be refined : "<<edgesTagged<<"\n"<<endl;

  ofstream adaptSimLog("adaptor.log");
  adaptSimLog<<"Strategy chosen for adaptation is tag driven"<<endl;
  adaptSimLog<<"(i.e., isotropic refinement)"<<endl;
  adaptSimLog<<"Info. on adaptation parameters are: "<<endl;
  adaptSimLog<<"Total Error : "<<totalError<<endl;
  adaptSimLog<<"Max. Error  : "<<maxError<<endl;
  adaptSimLog<<"Min. Error  : "<<minError<<endl;
  adaptSimLog<<"Threshold   : "<<threshold<<endl;
  adaptSimLog<<"factor : "<<factor<<endl;
  adaptSimLog<<"hmax   : "<<hmax<<endl;
  adaptSimLog<<"hmin   : "<<hmin<<endl;
  adaptSimLog<<"\nNumber of edges tagged to be refined : "<<edgesTagged<<endl;
  adaptSimLog.close();
}


void cvAdaptCore::writeMEDITSolution(pMesh mesh)
{
  ofstream fout;
  fout.open("sizefield-sol.bb");
  
  fout<<"3 1 "<<M_numVertices(mesh)<<" 2\n";

  double *nodalData;
  pVertex vertex;
  VIter vIter = M_vertexIter( mesh );
  while (vertex = VIter_next( vIter ) ) {
    if(EN_getDataPtr((pEntity)vertex, errorIndicatorID,
		     (void**)&nodalData) == NULL) {
      adaptSimLog<<"\nerror in writeMEDITSolution : no data attached to vertex\n";
      exit(0);
    }
    fout<<nodalData[4]<<endl;
  }
  VIter_delete(vIter);
  fout.close();
}

////////////////////////////////////////////
// write in MEDIT format                  //
// for visualization of mesh-metric field //
////////////////////////////////////////////
void cvAdaptCore::writeMEDITSizeField(Hessian* hess, pMesh mesh)
{
    pVertex vertex;
    VIter vIter = M_vertexIter( mesh );

    int counter =0;
    
    ofstream fout;
    fout.open("sizefield.bb");

    // file header 
    fout<<"3 6 "<<M_numVertices(mesh)<<" 2\n";

    while ( vertex = VIter_next( vIter ) ) {

      int factor=1;
      double n[3];
      Cross((hess[counter].dir[0])[0],
            (hess[counter].dir[0])[1],
            (hess[counter].dir[0])[2],
	    (hess[counter].dir[1])[0],
	    (hess[counter].dir[1])[1],
	    (hess[counter].dir[1])[2],
            &n[0],&n[1],&n[2]);

      if( Dot((hess[counter].dir[2])[0],
              (hess[counter].dir[2])[1],
              (hess[counter].dir[2])[2],
	      n[0],n[1],n[2]) <0. )
	factor=-1;

      double h[3];
      h[0]=hess[counter].h[0];
      h[1]=hess[counter].h[1];
      h[2]=hess[counter].h[2];
      
      double xx  = hess[counter].dir[0][0]*hess[counter].dir[1][0];
      xx=xx/(h[0]*h[1]);
      double xy  = hess[counter].dir[0][0]*hess[counter].dir[1][1];
      xy=xy/(h[0]*h[1]);
      double xz  = hess[counter].dir[0][0]*hess[counter].dir[1][2];
      xz=xz/(h[0]*h[1]);
      double yx  = hess[counter].dir[0][1]*hess[counter].dir[1][0];
      yx=yx/(h[0]*h[1]);
      double zx  = hess[counter].dir[0][2]*hess[counter].dir[1][0];
      zx=zx/(h[0]*h[1]);
      double oyx = hess[counter].dir[1][1]*factor*hess[counter].dir[2][0];
      oyx=oyx/(h[1]*h[2]);

      double xxy = xx*factor*hess[counter].dir[2][1];
      xxy=xxy/(h[2]);
      double xxz = xx*factor*hess[counter].dir[2][2];
      xxz=xxz/(h[2]);
      double xyx = xy*factor*hess[counter].dir[2][0];
      xyx=xyx/(h[2]);
      double xyy = xy*factor*hess[counter].dir[2][1];
      xyy=xyy/(h[2]);
      double xyz = xy*factor*hess[counter].dir[2][2];
      xyz=xyz/(h[2]);
      double xzx = xz*factor*hess[counter].dir[2][0];
      xzx=xzx/(h[2]);
      double xzy = xz*factor*hess[counter].dir[2][1];
      xzy=xzy/(h[2]);

      double yxx = yx*factor*hess[counter].dir[2][0];
      yxx=yxx/(h[2]);
      double yxy = yx*factor*hess[counter].dir[2][1];  
      yxy=yxy/(h[2]);
      double yxz = yx*factor*hess[counter].dir[2][2];
      yxz=yxz/(h[2]);
      double yzx = hess[counter].dir[0][1]*hess[counter].dir[1][2]*factor*hess[counter].dir[2][0];
      yzx=yzx/(h[0]*h[1]*h[2]);
      double yyx = hess[counter].dir[0][1]*oyx;
      yyx=yyx/(h[0]);

      double zxx = zx*factor*hess[counter].dir[2][0];
      zxx=zxx/(h[2]);
      double zxy = zx*factor*hess[counter].dir[2][1];
      zxy=zxy/(h[2]);
      double zyx = hess[counter].dir[0][2]*oyx;
      zyx=zyx/(h[0]);

      double vp[3];
      for(int i=0;i<3;i++) {
	vp[i]=1./(hess[counter].h[i]*hess[counter].h[i]);
      }
      
      double metric[6];

      metric[0]=  vp[0]*(xyz-xzy)+vp[1]*(zxy-yxz)+vp[2]*(yzx-zyx);
      
      metric[1]= -vp[0]*(xxz-xzx)-vp[1]*(zxx-xxz)-vp[2]*(xzx-zxx);

      metric[2]=  vp[0]*(xxy-xyx)+vp[1]*(yxx-xxy)+vp[2]*(xyx-yxx);

      metric[3]= -vp[0]*(yxz-yzx)-vp[1]*(zyx-xyz)-vp[2]*(xzy-zxy);

      metric[4]=  vp[0]*(yxy-yyx)+vp[1]*(yyx-xyy)+vp[2]*(xyy-yxy);

      metric[5]=  vp[0]*(zxy-zyx)+vp[1]*(yzx-xzy)+vp[2]*(xyz-yxz);

      fout<<metric[0]<<" ";
      fout<<metric[1]<<" "<<metric[3]<<" ";
      fout<<metric[2]<<" "<<metric[4]<<" "<<metric[5]<<"\n";
      
      counter++;
    }
    VIter_delete(vIter); 

    fout.close();
}


/////////////////////////////////
// write out the restart files //
// which contain the hessians  //
/////////////////////////////////
void cvAdaptCore::writeRestartHessians(pMesh mesh )
{
  double* nodalHessian;
  pVertex vertex;
  VIter vIter = M_vertexIter( mesh );
  // variables written out
  int nshg_fine = M_numVertices(mesh);
  double* q = new double [ nshg_fine * 6 ];    
  int vcounter=0;
  
  while ( vertex = VIter_next( vIter ) ) {
    if(EN_getDataPtr( (pEntity)vertex, nodalhessianID,
		      (void**)&nodalHessian)  != NULL) {
      int j=0;
      for( int i=0; i<6; i++ ) {	
	// store the hessians into q vector
	q[vcounter+j] = nodalHessian[i]; 
	
	j=j+nshg_fine;
      }
      vcounter = vcounter+1;  
      
    }
    // there is no hessian on this vertex
    else{
      adaptSimLog<<"\nerror in writeRestartHessians: encountered\n"
	  <<"a vertex that carries no hessian\n";
      exit(0);
    }    
  }
  VIter_delete(vIter); 
  
  // write the files
  char fname[255];
  char* oformat ="binary";

  int magic_number = 362436;
  int* mptr = &magic_number;
  int  frest;
  int iarray[10];
  int size, nitems;
  char filename[255];
  
  openfile_( "restart.1111.1", "write", &frest );
  
  writestring_( &frest,"# PHASTA Input File Version 2.0\n");
  writestring_( &frest, "# Byte Order Magic Number : 362436 \n");
  sprintf(fname,"# Output generated by phAdapt version: %s \n");
  
  writestring_( &frest, fname );
 
  time_t timenow = time ( &timenow);

  sprintf(fname,"# %s\n", ctime( &timenow ));
  
  writestring_( &frest, fname );
  
  int one=1;
  
  size = 1;
  nitems = 1;// length of iarray
  iarray[ 0 ] = 1;
  
  writeheader_( &frest, "byteorder magic number ",
                  (void*)iarray, &nitems, &size, "integer", oformat );

  writedatablock_( &frest, "byteorder magic number ",
                     (void*)mptr, &nitems, "integer", oformat );
  
  /////////////////////////
  // writing the restart //
  /////////////////////////

  bzero( (void*)fname, 255 );
  sprintf(fname,"number of modes : < 0 > %d\n", nshg_fine);
  writestring_( &frest, fname );
  
  bzero( (void*)fname, 255 );
  sprintf(fname,"number of variables : < 0 > %d\n", 6);
  writestring_( &frest, fname );
  
  size =  6*nshg_fine;
  nitems = 3; // length of iarray
  iarray[0] = nshg_fine;
  iarray[1] = 6;
  iarray[2] = 1111; // stepNumber;
  
  writeheader_( &frest, "solution ",
		( void* )iarray, &nitems, &size,"double", oformat );
  
  nitems = 6*nshg_fine;
  writedatablock_( &frest, "solution ",
		   ( void* )(q), &nitems, "double", oformat );
  
  closefile_( &frest, "write" );
  // finished writing the restart //
}


void cvAdaptCore::writeSmoothEIs(pMesh mesh) {

  int nshg = M_numVertices(mesh);
  double *smoothEIs=new double[nshg];

  pVertex v;
  VIter vIter=M_vertexIter(mesh);
  int vCount=0;

  while(v = VIter_next(vIter)) {
    double *nodalEIs;
    if(!EN_getDataPtr((pEntity)v, errorIndicatorID,(void**)&nodalEIs)) {
      adaptSimLog<<"\nerror in writeSmoothEIs: no data attached to  vertex\n";
      V_info(v);
      exit(0);
    }
    smoothEIs[vCount]=*nodalEIs;
    vCount++;
  }
  VIter_delete(vIter);

  writeArrayToFile("restart.99.1","SmoothEIs","binary","write",nshg,1,99,smoothEIs);

  delete [] smoothEIs;

}


void cvAdaptCore::attachSolution(char* solfile, 
                    pMesh mesh, 
                    map<pEntity, double *>& data,
		            int ndof, 
                    int P ) {
    int count,i;
    double *soln;
    int nem = (P > 1) ? (P - 1) : 0;
    int nfm = (P > 3) ? ((P-3)*(P-2)/2) : 0;
    int nrm = (P > 5) ? ((P-4)*(P-5)*(P-3)/6) : 0;
  
    int nshg = M_numVertices(mesh) + nem * M_numEdges(mesh)
        + nfm * M_numFaces(mesh) + nrm * M_numRegions(mesh);

    double* q_ji = new double[nshg*ndof];
    double* q = new double[nshg*ndof];
  
    // read solution file

    restart( solfile, q_ji, nshg, ndof );

    // transpose the solution

    for(i = 0; i< nshg; i++)
        for( int j=0; j< ndof; j++)
            q[i*ndof+j] = q_ji[j*nshg+i];

    delete [] q_ji; 
  
    /* attach the vertex coefficients */
    count = 0;
    pVertex vertex;
    VIter vIter = M_vertexIter( mesh );
    while (vertex = VIter_next( vIter ) ) {
        soln = new double[ndof];
        for (i=0; i < ndof; i++) soln[i] = q[count++];
        data[(pEntity)vertex]=soln;
    }
    VIter_delete( vIter );
  
    /* attach the edge coefficients */
    if (nem > 0){
        pEdge edge;
        EIter eIter = M_edgeIter( mesh );
        while (edge = EIter_next( eIter ) ) {
            soln = new double[ndof*nem];
            for (i=0; i < ndof*nem; i++) soln[i] = q[count++];
            data[(pEntity)edge]=soln;
        }
        EIter_delete( eIter );
    }
  
    /* attach face coefficients */
    if (nfm > 0){
        pFace face;
        FIter fIter = M_faceIter( mesh );
        while (face = FIter_next( fIter ) ) {
            soln = new double[ndof*nfm];
            for (i=0; i < ndof*nfm; i++) soln[i] = q[count++];
            data[(pEntity)face]=soln;
        }
        FIter_delete( fIter );
    }
    /* attach region coefficients */
    if (nrm > 0){
        adaptSimLog << " No code to attach Region Modes " << endl;
        exit ( -1 );
    }

    delete [] q;
}
void cvAdaptCore::attachSoln( char *solfile, 
                 pMesh mesh, 
                 pMeshDataId phSol,
                 int ndof, 
                 int P ) {
    int count,i;
    double *soln;
    int nem = (P > 1) ? (P - 1) : 0;
    int nfm = (P > 3) ? ((P-3)*(P-2)/2.0) : 0;
    int nrm = (P > 5) ? ((P-4)*(P-5)*(P-3)/6.0) : 0;
  
    int nshg = M_numVertices(mesh) + nem * M_numEdges(mesh)
        + nfm * M_numFaces(mesh) + nrm * M_numRegions(mesh);

    double* q_ji = new double[nshg*ndof];
    double* q = new double[nshg*ndof];
  
    // read solution file

    restart( solfile, q_ji, nshg, ndof );

    // transpose the solution

    for(i = 0; i< nshg; i++)
        for( int j=0; j< ndof; j++)
            q[i*ndof+j] = q_ji[j*nshg+i];

    delete [] q_ji;

    /* attach the vertex coefficients */
    count = 0;
    pVertex vertex;
    VIter vIter = M_vertexIter( mesh );
    while (vertex = VIter_next( vIter ) ) {
        soln = new double[ndof];
        for (i=0; i < ndof; i++) soln[i] = q[count++];
        EN_attachDataPtr( (pEntity)vertex, phSol, (void *) soln );
    }
    VIter_delete( vIter );

    /* attach the edge coefficients */
    if (nem > 0){
        pEdge edge;
        EIter eIter = M_edgeIter( mesh );
        while (edge = EIter_next( eIter ) ) {
            soln = new double[ndof*nem];
            for (i=0; i < ndof*nem; i++) soln[i] = q[count++];
            EN_attachDataPtr( (pEntity)edge, phSol, (void *) soln );
        }
        EIter_delete( eIter );
    }
  
    /* attach face coefficients */
    if (nfm > 0){
        pFace face;
        FIter fIter = M_faceIter( mesh );
        while (face = FIter_next( fIter ) ) {
            soln = new double[ndof*nfm];
            for ( i=0; i < ndof*nfm; i++ ) soln[i] = q[count++];
            EN_attachDataPtr( (pEntity)face, phSol, (void *) soln );
        }
        FIter_delete( fIter );
    }
    /* attach region coefficients */
    if (nrm > 0) {
        adaptSimLog << " No code to attach Region Modes " << endl;
        exit ( -1 );
    }

    delete[] q;
}

int cvAdaptCore::attachVPSoln( char *solfile, 
              pMesh mesh, 
              pMeshDataId phSol,
              pMeshDataId modes,
              int ndof ) {

    /* still need to update this function to handle uniform p */
    double* soln;
    int refmap = -1;
    openfile_( "geombc.dat.1", "read", &refmap );
    if ( refmap == -1 ) {
        adaptSimLog << "file not there " << endl;
        exit(0);
    }
    int ifour = 4;
    int iarray[4]={-1,-1,-1,-1}; /* size, nshg, nedgemodes, nfacemodes */

    readheader_( &refmap, "higher order refinement mapping", iarray, &ifour,
                 "integer", "binary");

    int size = iarray[0];
    int nshg = iarray[1];
    int emodes= iarray[2];
    int fmodes= iarray[3];

    int* rmap = new int [ size ];
    double* q_ji = new double[nshg*ndof];
    double* q = new double[nshg*ndof];

    readdatablock_( &refmap, "higher order refinement mapping", rmap,
                    &size, "integer", "binary");
  
    closefile_( &refmap, "read");

    // read solution file

    int stepno = restart( solfile, q_ji, 0, 0 );

    // transpose the solution
    // q_ji = [ndof][nshg]
    // q    = [nshg][ndof]

    for(int i = 0; i< nshg; i++)
        for( int j=0; j< ndof; j++)
            q[i*ndof+j] = q_ji[j*nshg+i];

    delete [] q_ji;

    /* attach the vertex coefficients */
    pVertex vertex;
    VIter vIter = M_vertexIter( mesh );
    while (vertex = VIter_next( vIter ) ) {
        soln = new double[ndof];
        for (int i=0; i < ndof; i++) 
            soln[i] = q[ndof*(EN_id( (pEntity)vertex)) + i ];
        EN_attachDataPtr( (pEntity)vertex, phSol, (void *) soln );
    }
    VIter_delete( vIter );

    int* ptr = rmap;
    int nem;
    pEdge edge;
    int ecounter =0;
    EIter eIter = M_edgeIter( mesh );
    while ( (edge = EIter_next( eIter )) && (ecounter < emodes )) {
        if ( EN_id( (pEntity)edge ) == *(ptr) ) {
            ptr++;
            nem = *(ptr++);
            if ( nem > 0 ) {
                ecounter += nem;
                soln = new double[ndof*nem];
                for(int i=0; i < nem ; i++ ) {
                    for (int j=0; j < ndof; j++) 
                        soln[i*ndof+j] = q[(*ptr)*ndof + j];
                    ptr++;
                }
                EN_attachDataPtr( (pEntity)edge, phSol, (void *) soln );
                EN_attachDataInt( (pEntity)edge, modes, nem );
            }
        }
    }
    EIter_delete( eIter );

    pFace face ;
    int fcounter=0;
    FIter fIter = M_faceIter( mesh );
    while( ( face = FIter_next( fIter ) ) && ( fcounter < fmodes )) {
        if ( EN_id( (pEntity)face ) == *(ptr) ) {
            ptr++;
            nem = *(ptr++);
            if ( nem > 0 ) {
                if ( nem > 1 )  {
                    adaptSimLog << "face cannot have 2 modes at the moment" << endl;
                    adaptSimLog << "please check  :" << __FILE__ << endl;
                    exit(0);
                }
                ecounter += nem;
                soln = new double[ndof*nem];
                for(int i=0; i < nem ; i++ ) {
                    for (int j=0; j < ndof; j++) 
                        soln[i*ndof+j] = q[(*ptr)*ndof + j];
                    ptr++;
                }
                EN_attachDataPtr( (pEntity)face, phSol, (void *) soln );
                EN_attachDataInt( (pEntity)face, modes, nem );
            }
        }
    }
    
    FIter_delete( fIter );

    delete[] q;
    delete[] rmap;

    return stepno;
}

void cvAdaptCore::solution( pEntity ent,
          pMeshDataId phSol,
	      double** sol ) {
    *sol=0;
    EN_getDataPtr( ent, phSol, (void **)sol );
}

void cvAdaptCore::numberofmodes( pEntity ent,
               pMeshDataId modes,
	           int* sol ) {
    *sol = 0;
    EN_getDataInt( ent, modes, sol );
}


int cvAdaptCore::restart( char filename[], 
         double* q,
         int nshg, 
         int nvr ) {

	int restart;
  	openfile_( filename , "read",  &restart );

	int iarray[4];
	int isize = 3;

	readheader_( &restart, "solution", iarray,
                 &isize, "double", iformat );
    
	isize = iarray[0]*iarray[1];
    int lstep = iarray[2];
    double* qlocal = new double [ isize ];
    
    readdatablock_( &restart, "solution", qlocal, &isize,
                    "double" , iformat );

    // copy the needed part into the array passed in 

	if ( nshg > iarray[0] ) {
        adaptSimLog << "reading only " << iarray[0] << "modes from restart" << endl;
        adaptSimLog << nshg << " modes were requested " << endl;
        nshg = iarray[0];
    }
    if ( nvr  > iarray[1] ) {
        adaptSimLog << "reading only " << iarray[1] << "vars from restart" << endl;
        adaptSimLog << nvr << " modes were requested " << endl;
        nvr  = iarray[1];
    }
    // we are not transposing here, just copying the needed chunk
    for(int i=0; i < nvr; i++)
        for(int j=0; j < nshg; j++)
            q[i*nshg+j] = qlocal[i*iarray[0]+j];
    
    delete [] qlocal;
    return lstep;
}


void cvAdaptCore::check(pMesh mesh)
{
  int nv=0, ne=0, nf=0, nr=0, unknown=0;
  
  VIter vit=M_vertexIter(mesh);
  pVertex vertex;
  while( vertex=VIter_next(vit) ) {
    switch( V_whatInType(vertex) ) {
    case Tvertex: nv++; break;
    case Tedge: ne++; break;
    case Tface: nf++; break;
    case Tregion: nr++; break;
    default:
      unknown++;
    }
  }
  
  printf("# of vertices on gv: %d\n",nv);
  printf("# of vertices on ge: %d\n",ne);
  printf("# of vertices on gf: %d\n",nf);
  printf("# of vertices on gr: %d\n",nr);
  printf("# of vertices of unknown classification: %d\n",unknown);
}

