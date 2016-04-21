/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
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
 */

#include "SimVascular.h" 
#include "cvMeshSimSolidModel.h"
#include "cv_sys_geom.h"

#include "SimError.h"

// -----------------
// cvMeshSimSolidModel
// -----------------

cvMeshSimSolidModel::cvMeshSimSolidModel()
  : cvSolidModel( SM_KT_MESHSIMSOLID )
{
  geom_ = NULL;
  quadElem_ = 0;
  progress_ = Progress_new();
  Progress_setDefaultCallback(progress_);
}


// ------------------
// ~cvMeshSimSolidModel
// ------------------

cvMeshSimSolidModel::~cvMeshSimSolidModel()
{
  if ( geom_ != NULL ) {
    GM_release(geom_);
  }
  if (progress_ != NULL) Progress_delete(progress_);
}


// -----
// Clear
// -----

void cvMeshSimSolidModel::Clear()
{
  if ( geom_ != NULL ) {
    GM_release(geom_);
  }
}


// -----
// Print
// -----

void cvMeshSimSolidModel::Print() const
{
  if ( geom_ != NULL ) {
    int numEdges = GM_numEdges(geom_);
    int numFaces = GM_numFaces(geom_);
    int numRegions = GM_numRegions(geom_);
    cout<<"num edges: "<<numEdges<<endl;    
    cout<<"num faces: "<<numFaces<<endl;
    cout<<"num regions: "<<numRegions<<endl;
  }
}


// ----------
// GetFaceIds
// ----------

int cvMeshSimSolidModel::GetFaceIds (int *numFaces, int **faceIds) {

  if ( geom_ == NULL) {
    return CV_ERROR;
  }

  this->Print();
  
  int num = 0;

  GFIter myFiter = GM_faceIter(geom_);
  pGFace myface; 
  while (myface = GFIter_next(myFiter)) {
    num++;
  }  
  GFIter_delete(myFiter);

  *numFaces = num;

  if (num == 0) return CV_ERROR;

  (*faceIds) = new int [num];

  myFiter = GM_faceIter(geom_);
  int j = 0;
  while (myface = GFIter_next(myFiter)) {
    (*faceIds)[j++] = GEN_tag(myface);
  }  
  GFIter_delete(myFiter);

  return CV_OK;

}


int cvMeshSimSolidModel::FindNodesOnElementFace (pFace face, int* nodes, double *xyz) const {

  return 0;
  
  int i;
  int num_nodes = 0;

  int pos = 0;

  pPList vert_list = F_vertices (face,MY_MESHSIM_VERTEX_ORDERING);
  int numElemFaceVerts = PList_size (vert_list);
  
  // write out "linear" nodes
  for (i = 0; i < numElemFaceVerts; i++) {
    pVertex vertex = (pVertex)PList_item (vert_list, i);
    pPoint point = V_point (vertex);
    xyz[pos++]=P_x(point);
    xyz[pos++]=P_y(point);
    xyz[pos++]=P_z(point);
    nodes[i] = P_id(point);
  }

  num_nodes = numElemFaceVerts;

  // quad nodes
  if (quadElem_ == 1) {
    pPList edge_list = F_edges(face, 1, NULL);
    int numEdges = PList_size (edge_list);

    for (i = 0; i < numEdges; i++) {
      pEdge xedge = (pEdge)PList_item (edge_list, i);
      if (E_numPoints(xedge) != 1) {
        fprintf(stderr,"ERROR:  no interior point!\n");
        exit(-1);
      }       
      nodes[numElemFaceVerts+i] = P_id(E_point(xedge,0)); 
      xyz[pos++]=P_x(E_point(xedge,0));
      xyz[pos++]=P_y(E_point(xedge,0));
      xyz[pos++]=P_z(E_point(xedge,0));    
    }

    num_nodes+=numEdges;
  }

  return num_nodes;
  
}


cvPolyData* cvMeshSimSolidModel::GetFacePolyData(int faceID, int useMaxDist, double max_dist) const {

  // useMaxDist and max_dist are ignored!

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.

  int n;
  vtkPolyData *mycvPolyData;
  cvPolyData *result; 

  int foundIt = 0;

  GFIter myGFiter = GM_faceIter(geom_);
  pGFace mygface; 
  while (mygface = GFIter_next(myGFiter)) {
    if (faceID == GEN_tag(mygface)) {
      foundIt++;
      break;
    }
  }  
  GFIter_delete(myGFiter);

  if (!foundIt) {
    fprintf(stdout,"Error:  face id (%i) not found in model!\n",faceID);
    return NULL;
  }

/////// NEED TO FIX STUFF HERE!
  pMesh mesh = NULL;
  return NULL;
  //pMesh mesh = DM_getMesh(geom_);
/////// PROBABLY OKAY FROM HERE DOWN!
  
  // vtk requires single precision
  vtkFloatingPointType p[3];

  // create the vtk object
  vtkPoints* myPoints = vtkPoints::New();
  myPoints->Allocate(1000,1000);
  mycvPolyData = vtkPolyData::New();
  mycvPolyData->Allocate(1000,1000);
  mycvPolyData->SetPoints(myPoints);

  vtkIdList* PointIds = vtkIdList::New();
  PointIds->Allocate(10,10);  

  // track the actual mesh node numbers in a scalar array
  vtkFloatArray *myScalars = vtkFloatArray::New();
  myScalars->Allocate(1000,1000);

  // node numbering in polydata object
  int poly_nodes = 0;

  int num_faces = M_numFaces (mesh);

  FIter myFiter = M_faceIter(mesh);
  pFace myface = NULL;
  while (myface = FIter_next(myFiter)) {

        //if (!(loopfaces % 10000)) fprintf(stdout,"face %i\n",loopfaces);
  
        // Get the model tag for the face
        int facetag=0;
        pGEntity modelface = F_whatIn(myface);
        if (modelface != NULL) {
           facetag=GEN_tag(F_whatIn(myface));
        }

        //  only output element face if it is on the model face
        if (facetag == faceID) {
          int nodes[20];
          double xyz[60];
          int num_nodes = FindNodesOnElementFace(myface,nodes,xyz);

          int pos = 0;
          for (n=0;n < num_nodes; n++) {
            // vtk requires single precision
            vtkFloatingPointType p[3];
            p[0]=xyz[pos++];p[1]=xyz[pos++];p[2]=xyz[pos++];
            myPoints->InsertNextPoint(p);
            myScalars->InsertNextTuple1(nodes[n]);
          }

          PointIds->Reset();
          for (n = 0; n < num_nodes; n++) {
            PointIds->InsertNextId(poly_nodes++);
          }
          mycvPolyData->InsertNextCell(VTK_POLYGON,PointIds);
        }


  }
  FIter_delete(myFiter);

  // attach the scalars to the polydata
  mycvPolyData->GetPointData()->SetScalars(myScalars);

  PointIds->Delete();
  cvPolyData *tmpresult = new cvPolyData( mycvPolyData );
  mycvPolyData->Delete();  // cvPolyData uses ref-counting

  // delete points and scalars here?
  myPoints->Delete();
  myScalars->Delete();

  // delete duplicate points
  result = sys_geom_MergePts( tmpresult );

  delete tmpresult;

  return result;
  
}



// -----
// Check
// -----

void cvMeshSimSolidModel::Check( int *nerr) const
{
  if ( geom_ != NULL ) {
  }
}


// ----------
// ReadNative
// ----------

typedef struct meshsim_vertex_info {
  pGVertex ptr;
  double xyz[3];
} meshsim_vertex_info;

typedef struct meshsim_line_info {
  int id;
  int is_null;
  double pt0[3];
  double pt1[3];
  int vertex0_id;
  int vertex1_id;
  pCurve ptr;
  } meshsim_line_info;

typedef struct meshsim_plane_info {
  int id;
  int is_null;
  double location[3];
  double axis[3];
  double ref_direction[3];
  double pt0[3];
  double pt1[3];
  int sense;  // is this used?  might not be set?
  pSurface ptr;
} meshsim_plane_info;

typedef struct meshsim_edge_info {
  int edge_id;
  int vertex0_id;
  int vertex1_id;
  int curve_id;
  int curve_sense;
  pGEdge ptr;
  } meshsim_edge_info;

typedef struct meshsim_face_info {
  int face_id;
  int surf_id;
  int sense;
  pGFace ptr;
  } meshsim_face_info;

typedef struct meshsim_region_info {
  int region_id;
  pGRegion ptr;
  } meshsim_region_info;

#include <stdio.h>
#include <map>

int cvMeshSimSolidModel::ReadNative( char *filename )
{

  int i;
  
  fprintf(stdout,"inside of ReadNative!\n");
  fflush(stdout);
  
  if (geom_ != NULL) {
    return CV_ERROR;
  }

  pGImporter importer = GImporter_new();

  //
  // read the geometry
  //
  
  FILE* fp = NULL;
  if ( (fp = fopen(filename,"r")) == NULL) {
    fprintf(stdout,"ERROR opening file (%s)\n",filename);
    return CV_ERROR;
  }

  //
  //  vertices
  //
  
  std::map <int,meshsim_vertex_info> vertices;

  int number_of_vertices  = 0;
  char line[1024];
  line[0]='\0';
  if (fgets(line,1024,fp) == NULL) {
    fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
    return CV_ERROR;
  }
  sscanf(line,"number_of_vertices %i",&number_of_vertices);
  fprintf(stdout,"number_of_vertices %i\n",number_of_vertices);
  fflush(stdout);

  meshsim_vertex_info v;
  int vid = 0;
  pGVertex pv;
  for (i = 0;i < number_of_vertices;i++) {
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }
    sscanf(line,"vertex_point %i %lf %lf %lf",&vid,&v.xyz[0],
	    &v.xyz[1],&v.xyz[2]);
    pv = NULL;
    pv = GImporter_createVertex(importer,v.xyz);
    v.ptr = pv;
    vertices[vid] = v; 
  }

  std::map<int,meshsim_vertex_info>::iterator iv;
  
  for (iv=vertices.begin();iv !=vertices.end(); ++iv) {
    int tmpid = iv->first;
    meshsim_vertex_info tmpv = iv->second;
    fprintf(stdout,"v[%i] ptr: %p xyz: %lf %lf %lf\n",tmpid,
	    tmpv.ptr,
	    tmpv.xyz[0],
	    tmpv.xyz[1],
	    tmpv.xyz[2]);
  }
  fflush(stdout);
  
  //
  // read lines
  //

  int number_of_lines = 0;
  
  line[0]='\0';
  if (fgets(line,1024,fp) == NULL) {
    fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
    return CV_ERROR;
  }
  sscanf(line,"number_of_lines %i",&number_of_lines);

  fprintf(stdout,"number_of_lines %i\n",number_of_lines);
  fflush(stdout);
 
  std::map <int,meshsim_line_info> lines;

  for (i = 0;i < number_of_lines;i++) {
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }
    meshsim_line_info myline;
    sscanf(line,"line %i %i %i %lf %lf %lf %lf %lf %lf\n",
	    &myline.id,&myline.vertex0_id,&myline.vertex1_id,
	    &myline.pt0[0],&myline.pt0[1],&myline.pt0[2],
	    &myline.pt1[0],&myline.pt1[1],&myline.pt1[2]);
    pCurve curve_ptr = NULL;
    curve_ptr = SCurve_createLine(myline.pt0,myline.pt1);
    myline.ptr = curve_ptr;
    lines[myline.id] = myline;
  }

  std::map<int,meshsim_line_info>::iterator il;
  
  for (il=lines.begin();il !=lines.end(); ++il) {
    int lid = il->first;
    meshsim_line_info tmpl = il->second;
    fprintf(stdout,"line %i %p %i %i %lf %lf %lf %lf %lf %lf\n",
 	    tmpl.id,tmpl.ptr,
	    tmpl.vertex0_id,tmpl.vertex1_id,
	    tmpl.pt0[0],tmpl.pt0[1],tmpl.pt0[2],
	    tmpl.pt1[0],tmpl.pt1[1],tmpl.pt1[2]);
  }

  //
  // read planes
  //

  int number_of_planes = 0;
  
  line[0]='\0';
  if (fgets(line,1024,fp) == NULL) {
    fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
    return CV_ERROR;
  }
  sscanf(line,"number_of_planes %i",&number_of_planes);

  fprintf(stdout,"number_of_planes %i\n",number_of_planes);
  fflush(stdout);
 
  std::map <int,meshsim_plane_info> planes;

  for (i = 0;i < number_of_planes;i++) {
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }
    meshsim_plane_info myplane;
    sscanf(line,"plane %i %lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
	    &myplane.id,
	    &myplane.location[0],&myplane.location[1],&myplane.location[2],
	    &myplane.pt0[0],&myplane.pt0[1],&myplane.pt0[2],
	    &myplane.pt1[0],&myplane.pt1[1],&myplane.pt1[2]);
    pSurface plane_ptr = NULL;
    plane_ptr = SSurface_createPlane(myplane.location,myplane.pt0,myplane.pt1);
    myplane.ptr = plane_ptr;
    planes[myplane.id] = myplane;
  }

  std::map<int,meshsim_plane_info>::iterator ip;
  
  for (ip=planes.begin();ip !=planes.end(); ++ip) {
    int pid = ip->first;
    meshsim_plane_info tmpp = ip->second;
    fprintf(stdout,"plane %i %p %lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
	    pid,tmpp.ptr,
	    tmpp.location[0],tmpp.location[1],tmpp.location[2],
	    tmpp.pt0[0],tmpp.pt0[1],tmpp.pt0[2],
	    tmpp.pt1[0],tmpp.pt1[1],tmpp.pt1[2]);
    fflush(stdout);
  }

  //
  // read edges
  //

  int number_of_edges = 0;
  
  line[0]='\0';
  if (fgets(line,1024,fp) == NULL) {
    fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
    return CV_ERROR;
  }
  sscanf(line,"number_of_edges %i",&number_of_edges);

  fprintf(stdout,"number_of_edges %i\n",number_of_edges);
  fflush(stdout);
 
  std::map <int,meshsim_edge_info> edges;

  for (i = 0;i < number_of_edges;i++) {
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }
    
    meshsim_edge_info myedge;
    int eid = 0;
    int vertex0_id = 0;
    int vertex1_id = 0;
    int curve_id = 0;
    int edge_dir = 0;
    
    sscanf(line,"edge %i %i %i %i %i\n",
	   &eid,&vertex0_id,&vertex1_id,&curve_id,&edge_dir);

    pGVertex edgeV0 = vertices[vertex0_id].ptr;
    pGVertex edgeV1 = vertices[vertex1_id].ptr;
    pCurve  edgecurve = lines[curve_id].ptr;
    pGEdge edge_ptr = NULL;
    fprintf(stdout,"createEdge %p %p %p %i\n",edgeV0,edgeV1,edgecurve,edge_dir);
    edge_ptr = GImporter_createEdge(importer,edgeV0,edgeV1,edgecurve,edge_dir);
    myedge.ptr = edge_ptr;
    myedge.vertex0_id = vertex0_id;
    myedge.vertex1_id = vertex1_id;
    myedge.curve_id = curve_id;
    myedge.curve_sense = edge_dir;
    edges[eid] = myedge;
  }

  std::map<int,meshsim_edge_info>::iterator ie;
  
  for (ie=edges.begin();ie !=edges.end(); ++ie) {
    int eid2 = ie->first;
    meshsim_edge_info tmpe = ie->second;
    fprintf(stdout,"edge %i %p %i %i %i %i\n",
	    eid2,tmpe.ptr,
	    tmpe.vertex0_id,tmpe.vertex1_id,
	    tmpe.curve_id,tmpe.curve_sense);
    fflush(stdout);
  }

  //
  // read faces
  //

  int number_of_faces = 0;
  
  line[0]='\0';
  if (fgets(line,1024,fp) == NULL) {
    fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
    return CV_ERROR;
  }
  sscanf(line,"number_of_faces %i",&number_of_faces);

  fprintf(stdout,"number_of_faces %i\n",number_of_faces);
  fflush(stdout);

  std::map <int,meshsim_face_info> faces;

  for (i = 0;i < number_of_faces;i++) {
    
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }

    int fid = 0;
    sscanf(line,"face %i\n",&fid);
     
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }

    meshsim_face_info myface;
    
    int face_number_of_edges = 0;
    sscanf(line,"number_of_edges %i\n",&face_number_of_edges);
    
    pGEdge* face_edges = new pGEdge[face_number_of_edges];
    int* face_edge_senses = new int[face_number_of_edges];

    int j;
    
    for (j = 0; j < face_number_of_edges; j++) {
      line[0]='\0';
      if (fgets(line,1024,fp) == NULL) {
        fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
        return CV_ERROR;
      }
      int face_edge_id = 0;
      int face_edge_dir = 0;
      
      sscanf(line,"edge %i %i\n",&face_edge_id,&face_edge_dir);
      face_edges[j] = edges[face_edge_id].ptr;
      face_edge_senses[j] = face_edge_dir;
    }
   
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }

    int number_of_loops = 0;
    sscanf(line,"number_of_loops %i\n",&number_of_loops);

    int* loopIndex = new int[number_of_loops];
    for (j = 0; j < number_of_loops; j++) {
      line[0]='\0';
      if (fgets(line,1024,fp) == NULL) {
        fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
        return CV_ERROR;
      }
      int loop_j = 0;
      int loop_index = 0;
      sscanf(line,"loopIndex %i %i\n",&loop_j,&loop_index);
      loopIndex[j] = loop_index;
    }
    
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }
    int sid = 0;
    sscanf(line,"face_surface_id %i\n",&sid);
    pSurface sid_ptr = NULL;
    sid_ptr = planes[sid].ptr;

    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }
    int face_orientation = 0;
    sscanf(line,"face_orientation %i\n",&face_orientation);

    pGFace face_ptr = NULL;
    fprintf(stdout,"creaeFace %i %p %p %i %p %p %i\n",face_number_of_edges,
				    face_edges,
				    face_edge_senses,
				    number_of_loops,
				    loopIndex,
				    sid_ptr,
				    face_orientation);
    fflush(stdout);
    face_ptr = GImporter_createFace(importer,
				    face_number_of_edges,
				    face_edges,
				    face_edge_senses,
				    number_of_loops,
				    loopIndex,
				    sid_ptr,
				    face_orientation);
    myface.ptr = face_ptr;
    myface.surf_id = sid;
    myface.sense = face_orientation;
    faces[fid] = myface;
  }

  std::map<int,meshsim_face_info>::iterator iface;
  
  for (iface=faces.begin();iface !=faces.end(); ++iface) {
    int fid2 = iface->first;
    meshsim_face_info tmpf = iface->second;
    fprintf(stdout,"face %i %p %i %i\n",
	    fid2,tmpf.ptr,
	    tmpf.surf_id,tmpf.sense);
    fflush(stdout);
  }

  //
  // read regions
  //

  int number_of_regions = 0;
  
  line[0]='\0';
  if (fgets(line,1024,fp) == NULL) {
    fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
    return CV_ERROR;
  }
  sscanf(line,"number_of_regions %i",&number_of_regions);

  fprintf(stdout,"number_of_regions %i\n",number_of_regions);
  fflush(stdout);

  std::map <int,meshsim_region_info> regions;

  for (i = 0;i < number_of_regions;i++) {
    
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }

    int rid = 0;
    sscanf(line,"create_region %i\n",&rid);
     
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }

    meshsim_region_info myregion;
    
    int region_number_of_faces = 0;
    sscanf(line,"number_of_faces %i\n",&region_number_of_faces);
    fprintf(stdout,"numebr_of_faces %i\n",region_number_of_faces);
    fflush(stdout);
    
    pGFace* region_faces = new pGFace[region_number_of_faces];
    int* region_face_senses = new int[region_number_of_faces];

    int j;
    
    for (j = 0; j < region_number_of_faces; j++) {
      line[0]='\0';
      if (fgets(line,1024,fp) == NULL) {
        fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
        return CV_ERROR;
      }
      int region_face_id = 0;
      int region_face_dir = 0;
      
      sscanf(line,"face %i %i\n",&region_face_id,&region_face_dir);
      region_faces[j] = faces[region_face_id].ptr;
      region_face_senses[j] = region_face_dir;
    }
   
    line[0]='\0';
    if (fgets(line,1024,fp) == NULL) {
      fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
      return CV_ERROR;
    }

    int number_of_shells = 0;
    sscanf(line,"number_of_shells %i\n",&number_of_shells);
    fprintf(stdout,"numebr_of_shells %i\n",number_of_shells);
    fflush(stdout);
 
    int* shellIndex = new int[number_of_shells];
    for (j = 0; j < number_of_shells; j++) {
      line[0]='\0';
      if (fgets(line,1024,fp) == NULL) {
        fprintf(stdout,"ERROR reading line from file (%s)\n",filename);
        return CV_ERROR;
      }
      int shell_j = 0;
      int shell_index = 0;
      sscanf(line,"shellIndex %i %i\n",&shell_j,&shell_index);
      shellIndex[j] = shell_index;
    }
 
    pGRegion region_ptr = NULL;
    fprintf(stdout,"creaeRegion %i %p %p %i %p\n",region_number_of_faces,
				    region_faces,
				    region_face_senses,
				    number_of_shells,
				    shellIndex);
    fflush(stdout);
    region_ptr = GImporter_createRegion(importer,
				    region_number_of_faces,
				    region_faces,
				    region_face_senses,
				    number_of_shells,
				    shellIndex);
    myregion.ptr = region_ptr;
    regions[rid] = myregion;
  }

  std::map<int,meshsim_region_info>::iterator iregion;
  
  for (iregion=regions.begin();iregion !=regions.end(); ++iregion) {
    int rid2 = iregion->first;
    meshsim_region_info tmpf = iregion->second;
    fprintf(stdout,"region %i %p\n",
	    rid2,tmpf.ptr);
    fflush(stdout);
  }

  geom_ = NULL;

  geom_ = GImporter_complete(importer);

  if (geom_ == NULL) {
    return CV_ERROR;
  }

  pPList errorList = PList_new();
  int isvalid = GM_isValid(geom_,1,errorList);
  fprintf(stdout,"solid model check: %i\n",isvalid);
  if (!isvalid) {
    fprintf(stdout,"number of errors: %i\n",PList_size(errorList));
    fflush(stdout);
    void *iter = 0; // must initialize to 0
    pSimError myerror;
    for (i = 0; i < PList_size(errorList);i++) {
      pSimError myerror = (pSimError)(PList_item(errorList,i));
      // process each item in list
      fprintf(stdout,"error code: %i\n",SimError_code(myerror));
      fflush(stdout);
    }
  }
  
  fflush(stdout);
  // memory leak for now...
  //GImporter_delete(importer);


   
  fflush(stdout);
  
  fclose(fp);

  // read geomsim geometric model
  //pNativeModel nmodel = NULL;
  //geom_ = GM_load(filename, nmodel, progress_);

  return CV_OK;

}


// -----------
// WriteNative
// -----------

int cvMeshSimSolidModel::WriteNative( int file_version, char *filename ) const
{
  if ( geom_ == NULL ) return CV_ERROR;

  GM_write(geom_,filename,file_version,progress_);

  return CV_OK;

}


/* -------------- */
/* GetVtkPolyData */
/* -------------- */

int MeshSimSolidUtils_GetVtkPolyData( pGModel model, int useMaxDist, double max_dist, vtkPolyData **out )
{

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.

  vtkPolyData *mycvPolyData;

  int i,j;
  
  pACase mcase = MS_newMeshCase(model);
  pModelItem mdomain = GM_domain(model);

  pMesh mesh = NULL;
  return CV_ERROR;
  ////// NEED TO FIX THIS!
  /*
  if (useMaxDist != 0) {

      MS_setupMesh(mesh);

      mesh = M_new(0,model);  

      double target_size = max_dist;
      MS_setMeshSize(mcase,mdomain,1,target_size,NULL);
  
      // initialize the surface mesher
      MS_initSurfaceMesher(mcase,0);
      pProcess sp = MS_newSurfaceMesher(mcase,mesh,0);
      Process_execute(sp);
      Process_delete(sp);
      
  }
  */
  ////// PROBABLY OKAY FROM HERE DOWN!!
  
  int num_verts = M_numVertices (mesh);
  fprintf(stdout,"num_verts: %i\n",num_verts);
  // only want the linear tets, surfaces, and nodes

  // vtk requires single precision
  vtkFloatingPointType p[3];

  // create a list of the nodes used in the linear elements

  // Add node numbers to the points
  // NOTE: node numbers start at 1
  VIter myViter = M_vertexIter(mesh);
  for (i = 0; i < num_verts; i++){
    P_setID (V_point (VIter_next(myViter)), i+1);
  }
  VIter_delete(myViter);

  vtkPoints* myPoints = vtkPoints::New();
  myPoints->SetNumberOfPoints(num_verts);
  myViter = M_vertexIter(mesh);
  for (i = 0; i < num_verts; i++) {
    pPoint point = V_point (VIter_next(myViter));
    int nodenumber = P_id (point);
    //fprintf(stdout,"nodenumber: %i\n",nodenumber);  
    double x = P_x (point);
    double y = P_y (point);
    double z = P_z (point);  
    p[0] = x; p[1] = y; p[2] = z;
    myPoints->InsertPoint(nodenumber-1,p);
  } // i 
  VIter_delete(myViter);

  mycvPolyData = vtkPolyData::New();
  mycvPolyData->Allocate(1000,1000);
  mycvPolyData->SetPoints(myPoints);

  // if a surface mesh, insert facets
  vtkIdList* PointIds = vtkIdList::New();
  PointIds->Allocate(10,10);  
 
  // insert the triangles

  int num_faces = M_numFaces (mesh);
  fprintf(stdout,"num_faces: %i\n",num_faces);

  FIter myFiter = M_faceIter(mesh);
  pFace myface; 
  while (myface = FIter_next(myFiter)) {
      pPList vert_list = F_vertices (myface,MY_MESHSIM_VERTEX_ORDERING);
      for (j = 0; j < PList_size (vert_list); j++) {
        int nodenum = P_id (V_point ((pVertex)PList_item (vert_list, j)));
        PointIds->InsertNextId(nodenum-1);
      }      
      mycvPolyData->InsertNextCell(VTK_POLYGON,PointIds);
      PList_delete(vert_list);  
      PointIds->Reset();
  }  
  FIter_delete(myFiter);

  PointIds->Delete();

  *out = mycvPolyData;

  return CV_OK;

}


// -----------
// GetPolyData
// -----------

cvPolyData *cvMeshSimSolidModel::GetPolyData(int useMaxDist, double max_dist) const
{

  // max_dist is equivalent to global max size

  vtkPolyData *bound;
  cvPolyData *result;
 
  if ( geom_ == NULL ) return NULL;
  
  if ( MeshSimSolidUtils_GetVtkPolyData( geom_, useMaxDist, max_dist, &bound ) != CV_OK ) {
    return NULL;
  }
 
  result = new cvPolyData( bound );
  bound->Delete();  // cvPolyData uses ref-counting

  return result;
}
