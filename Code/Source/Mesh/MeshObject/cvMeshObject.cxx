/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
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
 *
 *=========================================================================*/

#include "SimVascular.h" 

#include "cvMeshObject.h"
#include "cv_misc_utils.h"
#include <string.h>

// -------------
// cvMeshObject
// -------------

cvMeshObject::cvMeshObject()
  : cvRepositoryData( MESH_T )
{
 
}


// --------------
// ~cvMeshObject
// --------------

cvMeshObject::~cvMeshObject()
{
  ;
}


// Caller should deallocate the returned string.

char *cvMeshObject::GetKernelName( cvMeshObject::KernelType kernel )
{
  char *result;

  result = new char[100];
  result[0]='\0';
  switch (kernel) {
  case KERNEL_MESHSIM:
    strcpy( result, "MeshSim" );
    break;
  case KERNEL_GMSH:
    strcpy( result, "GMsh" );
    break;
  case KERNEL_TETGEN:
    strcpy( result, "TetGen" );
    break;
  default:
    strcpy( result, "Invalid kernel name; must be one of "
	    "{ MeshSim, GMsh,TetGen }" );
    return NULL;
    break;
  }

  return result;
}

cvMeshObject::KernelType cvMeshObject::GetKernelType( const char* kernel_name )
{
  if (strcmp(kernel_name, "MeshSim") == 0)
    return cvMeshObject::KERNEL_MESHSIM;
  else if (strcmp(kernel_name, "GMsh") == 0)
    return cvMeshObject::KERNEL_GMSH;
  else if (strcmp(kernel_name, "TetGen") == 0)
    return cvMeshObject::KERNEL_TETGEN;

  return cvMeshObject::KERNEL_INVALID;
}


int cvMeshObject::openOutputFile(char* filename) {
  fp_ = NULL;
  // open the output file
  #ifdef USE_ZLIB
  char filenamegz[MAXPATHLEN];
  filenamegz[0]='\0';
  sprintf (filenamegz, "%s.gz", filename);
  fp_ = gzopen (filenamegz, "wb");
  if (fp_ == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filenamegz);
      return CV_ERROR;
  }
  #else
  fp_ = gzopen (filename, "wb");
  if (fp_ == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filename);
      return CV_ERROR;
  }
  #endif
  return CV_OK;
}

int cvMeshObject::closeOutputFile() {
  gzclose(fp_);
  return CV_OK;
}

/*---------------------------------------------------------------------*
 *                                                                     *
 *             ****  WriteSpectrumSolverElements ****                  *
 *                                                                     *
 *---------------------------------------------------------------------*/

int cvMeshObject::WriteSpectrumSolverElements (char *filename) {

  // this code assumes there is only 1 material region.

  // open the output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  initRegionTraversal();
  while (getNextRegion() == 1) {
    initElementTraversal();
    while (getNextElement() == 1) {
      // spectrum only supports linear tets
        gzprintf(fp_,"%d %d %d %d %d\n",curElemID_, connID_[0],connID_[1],connID_[2],connID_[3]);
    }
  }

  return closeOutputFile();

}

/*---------------------------------------------------------------------*
 *                                                                     *
 *                 ****  WriteSpectrumSolverNodes  ****                *
 *                                                                     *
 *---------------------------------------------------------------------*/

int cvMeshObject::WriteSpectrumSolverNodes (char *filename) {

  // This method outputs all of the nodes to a file.

  // open the output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  initNodeTraversal();

  while (getNextNode() == 1) {
    gzprintf(fp_, "  %d  %f  %f  %f \n", nodeID_, nodeX_, nodeY_, nodeZ_);
  }

  return closeOutputFile();

}


/*---------------------------------------------------------------------*
 *                                                                     *
 *                 ****  WriteSpectrumVisData  ****                    *
 *                                                                     *
 *---------------------------------------------------------------------*/

int cvMeshObject::WriteSpectrumVisData (char *filename) {

  // open the output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  char s[80];
  sprintf (s, "region_%d", 1);
  gzprintf(fp_, "  region  \"%s\" \n", s);

  gzprintf(fp_, "    time step %d \n", 1);
  gzprintf(fp_, "    time %g \n", 1.0);

  gzprintf(fp_, "    analysis results \"%s\" \n", "pressure");
  gzprintf(fp_, "      number of data %d \n", numLinearNodes_);
  gzprintf(fp_, "      type \"%s\" \n", "nodal");
  gzprintf(fp_, "      order \"%s\" \n", "scalar");
  gzprintf(fp_, "      length %d \n", 1);
  gzprintf(fp_, "      data \n");

  initNodeTraversal();

  while (getNextNode() == 1) {
    gzprintf(fp_, "  1.0\n", 1.0);
  }

  gzprintf(fp_, "      end data \n");
  gzprintf(fp_, "    end analysis results \n");

  gzprintf(fp_, "    analysis results \"velocity\" \n");
  gzprintf(fp_, "      number of data %d \n",numLinearNodes_);
  gzprintf(fp_, "      type \"nodal\"\n");
  gzprintf(fp_, "      order \"vector\"\n");
  gzprintf(fp_, "      number of components 3\n");
  gzprintf(fp_, "      components\n");
  gzprintf(fp_, "      \"x\"\n");
  gzprintf(fp_, "      \"y\"\n");
  gzprintf(fp_, "      \"z\"\n");
  gzprintf(fp_, "      end components\n");
  gzprintf(fp_, "      length 3\n");
  gzprintf(fp_, "      data\n");

  initNodeTraversal();

  while (getNextNode() == 1) {
    gzprintf(fp_, "  0.0 0.0 1.0\n");
  }

  gzprintf(fp_, "      end data \n");
  gzprintf(fp_, "    end analysis results \n");

  //gzprintf(fp_, "  end region \n\n");
  
  return closeOutputFile();

}


/*---------------------------------------------------------------------*
 *                                                                     *
 *                 **** WriteSpectrumVisMesh ****                      *
 *                                                                     *
 *---------------------------------------------------------------------*/

int cvMeshObject::WriteSpectrumVisMesh (char *filename) {

  // this code assumes there is only 1 material region.

  // open the output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // output the nodes
  char s[80];
  gzprintf(fp_, "problem  \"%s\"  \n", "scorec mesh");
  gzprintf(fp_, "  time information \n");
  gzprintf(fp_, "    number of time steps %d \n", 1);
  gzprintf(fp_, "    time steps \n");
  gzprintf(fp_, "    %d  %g \n", 1, 1.0);
  gzprintf(fp_, "    end time steps \n");
  gzprintf(fp_, "  end time information \n\n");

  sprintf (s, "region_%d", 1);
  gzprintf(fp_, "  region  \"%s\" \n", s);
  gzprintf(fp_, "    nature \"%s\" \n", "solid");

  gzprintf(fp_, "    number of nodal coordinates %d \n", numLinearNodes_);
  gzprintf(fp_, "    nodal coordinates \n");

  initNodeTraversal();
  while (getNextNode() == 1) {
    gzprintf(fp_, "  %d  %f  %f  %f \n", nodeID_, nodeX_, nodeY_, nodeZ_);
  }
  gzprintf(fp_, "    end node coordinates \n");

  // output the elements

  gzprintf(fp_, "    element set \"%s\"  \n", "eset_1");
  gzprintf(fp_, "      material id 0  \n");
  gzprintf(fp_, "      nodes per element %d \n", 4);
  gzprintf(fp_, "      topology \"%s\" \n", "tet");
  gzprintf(fp_, "      number of elements %d \n", numElements_);
  gzprintf(fp_, "      connectivity \n" );


  initRegionTraversal();
  while (getNextRegion() == 1) {
    initElementTraversal();
    while (getNextElement() == 1) {
      // spectrum only supports linear tets
        gzprintf(fp_,"%d %d %d %d %d\n",curElemID_, connID_[0],connID_[1],connID_[2],connID_[3]);
    }
  }

  gzprintf(fp_, "      end connectivity \n");
  gzprintf(fp_, "    end element set \n");
  gzprintf(fp_, "  end region \n\n");
  gzprintf(fp_, "end problem  \n");

  return closeOutputFile();

}


/*---------------------------------------------------------------------*
 *                                                                     *
 *                     ****  WriteProphlexInputDeck  ****              *
 *                                                                     *
 *---------------------------------------------------------------------*/

int cvMeshObject::WriteProphlexInputDeck (char *filename) {

  // 
  // NOTE:  This method has never been tested with quadratic elements
  //
 
  // open the output file 
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // output the nodes

  gzprintf(fp_,"BEGIN NODES\n");
  gzprintf(fp_,"NUMBERED\n"); 

  initNodeTraversal();
  while (getNextNode() == 1) {
    gzprintf(fp_, "  %d  %f  %f  %f \n", nodeID_, nodeX_, nodeY_, nodeZ_);
  }

  gzprintf(fp_,"END\n\n");

  // output the elements in each region
  gzprintf(fp_,"BEGIN ELEMENTS NUMBERED\n");
  if (quadElem_ == 0) {
    gzprintf(fp_,"TET4\n");
  } else {
    gzprintf(fp_,"TET10\n");
  }

  initRegionTraversal();
  while (getNextRegion() == 1) {
    gzprintf(fp_,"ELEMENTSET region_%i\n",curMdlRegID_);
    initElementTraversal();
    if (quadElem_ == 0) {
      while (getNextElement() == 1) {
        gzprintf(fp_,"%d %d %d %d %d\n",curElemID_, connID_[0],connID_[1],connID_[2],connID_[3]);
      }
    } else {
      while (getNextElement() == 1) {
        gzprintf(fp_,"%d %d %d %d %d %d %d %d %d %d %d\n",curElemID_, 
                                       connID_[0],connID_[1],connID_[2],connID_[3],
                                       connID_[4],connID_[5],connID_[6],connID_[7],
                                       connID_[8],connID_[9]);
      }            
    }
  }
  gzprintf(fp_,"END\n\n");

  // Write out a few helper lines if you want to view the mesh in prophlex (phlex-statics)
  gzprintf(fp_,"#  Uncomment the following lines below (i.e. remove # sign) to view mesh in prohlex-statics\n");

  // write out fake materials
  gzprintf(fp_,"#  BEGIN MATERIALS\n");
  int i;
  for (i=0; i < numModelRegions_ ; i++) {
    gzprintf(fp_,"#  IsoHook fakemat_%i { E = 1.0 nu = 0.1 die = 1.0 } \n",regionID_[i]);
  }
  gzprintf(fp_,"#  END\n\n");
  
  // write out fake matassigns
  gzprintf(fp_,"#  BEGIN MATASSIGN\n");
  for (i=0; i < numModelRegions_ ; i++) {
    gzprintf(fp_,"#  fakemat_%i { region_%i } \n",regionID_[i],regionID_[i]);
  }
  gzprintf(fp_,"#  END\n\n");  
  
  return closeOutputFile();

}


/*---------------------------------------------------------------------*
 *                                                                     *
 *                     ****  WriteAbaqusInputDeck  ****              *
 *                                                                     *
 *---------------------------------------------------------------------*/

int cvMeshObject::WriteAbaqusInputDeck (char *filename) {

  // 
  // NOTE:  This method has never been tested with linear elements
  //
 
  // open the output file 
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // output the required heading
  gzprintf(fp_,"*heading\n");
  gzprintf(fp_,"auto created from scorec for %s\n",filename); 

  // output the nodes
  gzprintf(fp_,"*node\n");

  initNodeTraversal();
  while (getNextNode() == 1) {
    gzprintf(fp_, "%d ,  %f , %f , %f \n", nodeID_, nodeX_, nodeY_, nodeZ_);
  }

  // output the elements in each region
  initRegionTraversal();
  while (getNextRegion() == 1) {
    if (quadElem_ == 0) {
      gzprintf(fp_,"*element,type=C3D4,elset=region_%i\n",curMdlRegID_);
    } else {
      gzprintf(fp_,"*element,type=C3D10,elset=region_%i\n",curMdlRegID_);    
    }
 
    initElementTraversal();
    if (quadElem_ == 0) {
      while (getNextElement() == 1) {
        gzprintf(fp_,"%d , %d , %d , %d , %d\n",curElemID_, connID_[0],connID_[1],connID_[2],connID_[3]);
      }
    } else {
      while (getNextElement() == 1) {
          gzprintf(fp_,"%d , %d , %d , %d , %d ,\n",curElemID_, connID_[0],connID_[1],
                   connID_[2],connID_[3]);
          gzprintf(fp_,"%d , %d , %d , %d , %d , %d\n",connID_[4], connID_[5], connID_[6],
                                                      connID_[7], connID_[8], connID_[9]);
      }            
    }
  }

  // Write out a few helper lines if you want to view the mesh  
  gzprintf(fp_,"**  Uncomment the following lines below for fake material properties\n");

  // write out fake materials
  for (int i=0; i < numModelRegions_ ; i++) {
    gzprintf(fp_,"** *material, name=mat%i\n",regionID_[i]);
    gzprintf(fp_,"** *elastic, type=isotropic\n");
    gzprintf(fp_,"** 130e9,0.23\n");
    gzprintf(fp_,"** *expansion\n");
    gzprintf(fp_,"** 4e-05,\n");
    gzprintf(fp_,"** *solid section, elset=region_%i,material=mat%i\n",regionID_[i],regionID_[i]);
  }
   
  return closeOutputFile();

}


/*------------------------------------------------------------*
 *                                                            *
 *                     ****  WriteMeshVrml  ****              *
 *                                                            *
 *------------------------------------------------------------*/

int cvMeshObject::WriteMeshVrml (char *filename) {

  // open output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // Calculate bounding box for mesh and scale to unit cube
  double maxpoint = 0;
  initNodeTraversal();
  while (getNextNode() == 1) {
    if (fabs(nodeX_) > maxpoint) {
        maxpoint = fabs(nodeX_);
    } else if (fabs(nodeY_) > maxpoint) {
        maxpoint = fabs(nodeY_);
    } else if (fabs(nodeZ_) > maxpoint) {
        maxpoint = fabs(nodeZ_);
    }
  }

  // output the header
  gzprintf(fp_,"#VRML V2.0 utf8\n");

  double invertMax = 1.0/maxpoint;

  // loop over all of the material regions, making each a separate object
  initRegionTraversal();
  int i = 0;
  while (getNextRegion() == 1) {
    initElementTraversal();
    double colorme = 1.0*i/numModelRegions_;
    gzprintf(fp_,"DEF region_%i Transform {\n",curMdlRegID_);
    gzprintf(fp_,"  center 0 0 0\n");
    gzprintf(fp_,"  scale %f %f %f\n",invertMax,invertMax,invertMax);
    gzprintf(fp_,"  children [\n");
    gzprintf(fp_,"    Shape {\n");
    gzprintf(fp_,"      appearance Appearance {\n");
    gzprintf(fp_,"	material DEF region_%iMaterial Material {\n",curMdlRegID_);
    gzprintf(fp_,"	  emissiveColor %f %f %f\n",colorme,colorme,1.0);
    gzprintf(fp_,"	  shininess    0.8\n");
    gzprintf(fp_,"   	}\n");
    gzprintf(fp_,"      }\n");

    // since in general we don't have the ability to display wireframes
    // (e.g. in CosmoPlayer), we will output faces as line segments instead
    gzprintf(fp_,"      geometry IndexedLineSet {\n");

    if (i == 0) {
    gzprintf(fp_,"	  coord DEF all_nodes Coordinate {\n");
    gzprintf(fp_,"	    point [\n");
      initNodeTraversal();
      while(getNextNode() == 1) {
    gzprintf(fp_,"            %f   %f   %f,  \n", nodeX_, nodeY_, nodeZ_);
      }
    gzprintf(fp_,"          ]\n");
    gzprintf(fp_,"        } \n");
    } else {
    gzprintf(fp_,"        coord USE all_nodes\n");    
    }
    gzprintf(fp_,"        coordIndex [\n");
    while (getNextElement() == 1) {
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[0]-1,connID_[2]-1,connID_[1]-1,connID_[0]-1);
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[0]-1,connID_[1]-1,connID_[3]-1,connID_[0]-1);
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[1]-1,connID_[2]-1,connID_[3]-1,connID_[1]-1);
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[0]-1,connID_[3]-1,connID_[2]-1,connID_[0]-1);
    }
    gzprintf(fp_,"          -1,\n");
    
    gzprintf(fp_,"        ]\n");
    gzprintf(fp_,"      }\n"); 
    gzprintf(fp_,"    }\n");
    gzprintf(fp_,"  ]\n");
    gzprintf(fp_,"}\n");

    i++;
  }

  return closeOutputFile();

}


/*------------------------------------------------------------*
 *                                                            *
 *              ****  WriteSurfaceMeshVrml  ****              *
 *                                                            *
 *------------------------------------------------------------*/

int cvMeshObject::WriteSurfaceMeshVrml (char *filename) {

  // open output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // Calculate bounding box for mesh and scale to unit cube
  double maxpoint = 0;
  initNodeTraversal();
  while (getNextNode() == 1) {
    if (fabs(nodeX_) > maxpoint) {
        maxpoint = fabs(nodeX_);
    } else if (fabs(nodeY_) > maxpoint) {
        maxpoint = fabs(nodeY_);
    } else if (fabs(nodeZ_) > maxpoint) {
        maxpoint = fabs(nodeZ_);
    }
  }

  // output the header
  gzprintf(fp_,"#VRML V2.0 utf8\n");

  double invertMax = 1.0/maxpoint;

  // loop over all of the material regions, making each a separate object
  initRegionTraversal();
  int i = 0;
  while (getNextRegion() == 1) {
    initElementTraversal();
    double colorme = 1.0*i/numModelRegions_;
    gzprintf(fp_,"DEF region_%i Transform {\n",curMdlRegID_);
    gzprintf(fp_,"  center 0 0 0\n");
    gzprintf(fp_,"  scale %f %f %f\n",invertMax,invertMax,invertMax);
    gzprintf(fp_,"  children [\n");
    gzprintf(fp_,"    Shape {\n");
    gzprintf(fp_,"      appearance Appearance {\n");
    gzprintf(fp_,"	material DEF region_%iMaterial Material {\n",curMdlRegID_);
    gzprintf(fp_,"	  emissiveColor %f %f %f\n",colorme,colorme,1.0);
    gzprintf(fp_,"	  shininess    0.8\n");
    gzprintf(fp_,"   	}\n");
    gzprintf(fp_,"      }\n");

    // since in general we don't have the ability to display wireframes
    // (e.g. in CosmoPlayer), we will output faces as line segments instead
    gzprintf(fp_,"      geometry IndexedLineSet {\n");

    if (i == 0) {
    gzprintf(fp_,"	  coord DEF all_nodes Coordinate {\n");
    gzprintf(fp_,"	    point [\n");
      initNodeTraversal();
      while(getNextNode() == 1) {
    gzprintf(fp_,"            %f   %f   %f,  \n", nodeX_, nodeY_, nodeZ_);
      }
    gzprintf(fp_,"          ]\n");
    gzprintf(fp_,"        } \n");
    } else {
    gzprintf(fp_,"        coord USE all_nodes\n");    
    }
    gzprintf(fp_,"        coordIndex [\n");
    // only output the element face if its on an interface
    while (getNextElement() == 1) {
      getNeighborMdlRegIds();
      if ( (curElemNe_[0][0] != curElemNe_[0][1]) || (curElemNe_[0][0] == -1 && curElemNe_[0][1] == -1) ) 
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[0]-1,connID_[2]-1,connID_[1]-1,connID_[0]-1);
      if ( (curElemNe_[1][0] != curElemNe_[1][1]) || (curElemNe_[1][0] == -1 && curElemNe_[1][1] == -1) ) 
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[0]-1,connID_[1]-1,connID_[3]-1,connID_[0]-1);
      if ( (curElemNe_[2][0] != curElemNe_[2][1]) || (curElemNe_[2][0] == -1 && curElemNe_[2][1] == -1) ) 
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[1]-1,connID_[2]-1,connID_[3]-1,connID_[1]-1);
      if ( (curElemNe_[3][0] != curElemNe_[3][1]) || (curElemNe_[3][0] == -1 && curElemNe_[3][1] == -1) ) 
    gzprintf(fp_,"          %d, %d, %d, %d, -1,\n",connID_[0]-1,connID_[3]-1,connID_[2]-1,connID_[0]-1);
    }
    gzprintf(fp_, "         -1,\n");
    
    gzprintf(fp_,"        ]\n");
    gzprintf(fp_,"      }\n"); 
    gzprintf(fp_,"    }\n");
    gzprintf(fp_,"  ]\n");
    gzprintf(fp_,"}\n");

    i++;
  }

  return closeOutputFile();

}


/*------------------------------------------------------------*
 *                                                            *
 *              ****  WriteHypermeshAscii  ****               *
 *                                                            *
 *------------------------------------------------------------*/

int cvMeshObject::WriteHypermeshAscii (char *filename) {

  // open the output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // output the nodes 
  gzprintf(fp_,"HyperMesh Input Deck generated by Geodesic Meshing Layer\n");
  gzprintf(fp_,"*filetype(ASCII)\n");
  gzprintf(fp_,"*version(2.10)\n");

  // output the nodes
  initNodeTraversal();
  while (getNextNode()) { 
    gzprintf(fp_, "*node(%i,%lf,%lf,%lf,0,0,0)\n", nodeID_, nodeX_, nodeY_, nodeZ_);
  }
 
  // output the elements

  int color = 1;

  initRegionTraversal();
  while (getNextRegion()) {
    gzprintf(fp_,"*component(%i,\"region%i\",0,%i)\n",curMdlRegID_,curMdlRegID_,color++);
    initElementTraversal();
    while (getNextElement()) {
      if (quadElem_ == 0) {
       gzprintf(fp_,"*tetra4(%i,1,%i,%i,%i,%i)\n",curElemID_,
                connID_[0],connID_[1],connID_[2],connID_[3]);
      } else {
      gzprintf(fp_,"*tetra10(%i,1,%i,%i,%i,%i,%i,%i,%i,%i,%i,%i)\n",curElemID_,
         connID_[0],connID_[1],connID_[2],connID_[3],connID_[4],
         connID_[5],connID_[6],connID_[7],connID_[8],connID_[9]);
      }
    }
  }
  
  return closeOutputFile();

} // end of WriteHypermeshAscii


/*------------------------------------------------------------*
 *                                                            *
 *              ****  WriteSurfaceMeshOogl  ****              *
 *                                                            *
 *------------------------------------------------------------*/

int cvMeshObject::WriteSurfaceMeshOogl (char *filename) {

  int i;

  // open output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

  // Calculate bounding box for mesh and scale to unit cube
  double maxpoint = 0;
  initNodeTraversal();
  while (getNextNode() == 1) {
    if (fabs(nodeX_) > maxpoint) {
        maxpoint = fabs(nodeX_);
    } else if (fabs(nodeY_) > maxpoint) {
        maxpoint = fabs(nodeY_);
    } else if (fabs(nodeZ_) > maxpoint) {
        maxpoint = fabs(nodeZ_);
    }
  }

  // first we have to count the entities before we write the file

  // loop over all of the material regions, making each a separate object
  initRegionTraversal();
  int num_faces = 0;
  while (getNextRegion() == 1) {
    initElementTraversal();
    // only output the element face if its on an interface
    while (getNextElement() == 1) {
      getNeighborMdlRegIds();
      if ( (curElemNe_[0][0] != curElemNe_[0][1]) || (curElemNe_[0][0] == -1 && curElemNe_[0][1] == -1) ) 
          num_faces++;
      if ( (curElemNe_[1][0] != curElemNe_[1][1]) || (curElemNe_[1][0] == -1 && curElemNe_[1][1] == -1) ) 
          num_faces++;
      if ( (curElemNe_[2][0] != curElemNe_[2][1]) || (curElemNe_[2][0] == -1 && curElemNe_[2][1] == -1) ) 
          num_faces++;
      if ( (curElemNe_[3][0] != curElemNe_[3][1]) || (curElemNe_[3][0] == -1 && curElemNe_[3][1] == -1) ) 
          num_faces++;
    }
  }

  // output header information
  gzprintf(fp_,"VECT\n");
 
  // number of polylines, number of vertexes, number of colors
  // NOTE:  even though the code below was original written to output only 1 color line per
  // region, this does not appear to work in JGV.  Therefore we explicitly write out the color
  // for each face.
  gzprintf(fp_,"%i %i %i\n",num_faces,4*num_faces,num_faces);

  // number of vertexes in each polyline
  for (i = 0; i < num_faces; i++) {
    gzprintf(fp_,"4 ");
  }
  gzprintf(fp_,"\n");

  // output color info
  for (i = 0; i < num_faces; i++) {
    gzprintf(fp_,"1 ");
  }
  gzprintf(fp_,"\n");

  // loop over all of the material regions, making each a separate object
  initRegionTraversal();
  int *faces_in_region = new int[numModelRegions_+1];
  i = 0;
  while (getNextRegion() == 1) {
    initElementTraversal();
    faces_in_region[i] = 0; 
    // only output the element face if its on an interface
    while (getNextElement() == 1) {
      getNeighborMdlRegIds();
      if ( (curElemNe_[0][0] != curElemNe_[0][1]) || (curElemNe_[0][0] == -1 && curElemNe_[0][1] == -1) ) { 
          faces_in_region[i]++;
          if (outputOoglFaceNodes(connID_[0],connID_[2],connID_[1],connID_[0],maxpoint) == CV_ERROR) {
              closeOutputFile();
              delete faces_in_region;
              return CV_ERROR;
          }
      }
      if ( (curElemNe_[1][0] != curElemNe_[1][1]) || (curElemNe_[1][0] == -1 && curElemNe_[1][1] == -1) ) {
          faces_in_region[i]++;
          if (outputOoglFaceNodes(connID_[0],connID_[1],connID_[3],connID_[0],maxpoint) == CV_ERROR) {
              closeOutputFile();
              delete faces_in_region;
              return CV_ERROR;
          }
      }
      if ( (curElemNe_[2][0] != curElemNe_[2][1]) || (curElemNe_[2][0] == -1 && curElemNe_[2][1] == -1) ) {
          faces_in_region[i]++;
          if (outputOoglFaceNodes(connID_[1],connID_[2],connID_[3],connID_[1],maxpoint) == CV_ERROR) {
              closeOutputFile();
              delete faces_in_region;
              return CV_ERROR;
          }
      }
      if ( (curElemNe_[3][0] != curElemNe_[3][1]) || (curElemNe_[3][0] == -1 && curElemNe_[3][1] == -1) ) {
          faces_in_region[i]++;
          if (outputOoglFaceNodes(connID_[0],connID_[3],connID_[2],connID_[0],maxpoint) == CV_ERROR) {
              closeOutputFile();
              delete faces_in_region;
              return CV_ERROR;
          }
      }
    }
    i++;
  }

  // output color info
  for (i = 0; i < numModelRegions_; i++) {
    // Calculate color based on region number
    double colorme = 1.0*i/numModelRegions_;
    for (int j = 0; j < faces_in_region[i]; j++) {
       gzprintf(fp_,"%f %f %f %f\n",colorme,colorme,1.0,0);
    }
  } 
  gzprintf(fp_,"\n");

  delete faces_in_region;
  return closeOutputFile();

}

int cvMeshObject::outputOoglFaceNodes(int n1,int n2,int n3,int n4,double maxpoint) {
  if (GetNodeCoords(n1) == CV_ERROR) return CV_ERROR;
  gzprintf(fp_,"%lf %lf %lf\n",nodeX_/maxpoint,nodeY_/maxpoint,nodeZ_/maxpoint);
  if (GetNodeCoords(n2) == CV_ERROR) return CV_ERROR;
  gzprintf(fp_,"%lf %lf %lf\n",nodeX_/maxpoint,nodeY_/maxpoint,nodeZ_/maxpoint);
  if (GetNodeCoords(n3) == CV_ERROR) return CV_ERROR;
  gzprintf(fp_,"%lf %lf %lf\n",nodeX_/maxpoint,nodeY_/maxpoint,nodeZ_/maxpoint);
  if (GetNodeCoords(n4) == CV_ERROR) return CV_ERROR;
  gzprintf(fp_,"%lf %lf %lf\n\n",nodeX_/maxpoint,nodeY_/maxpoint,nodeZ_/maxpoint);
  return CV_OK;
}

