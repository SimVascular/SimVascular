//
// Author: Bryn Lloyd, blloyd at vision.ee.ethz.ch
// Date: March 2009
//
//


#include "vtkSurfaceBooleanOperations.h"

extern "C" {
#include "glib.h"
#include "gts.h"
}
#include <vtkPolyData.h>
#include <vtkCellArray.h>
#include <vtkTriangleFilter.h>
#include <vtkCleanPolyData.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkObjectFactory.h>
#include <vtkInformationVector.h>
#include <vtkInformation.h>

#include <vtkSmartPointer.h>
#define vtkNew(type,name) \
  vtkSmartPointer<type> name = vtkSmartPointer<type>::New()

using namespace std;

vtkCxxRevisionMacro(vtkSurfaceBooleanOperations, "$Revision: 0.0 $")
;
vtkStandardNewMacro(vtkSurfaceBooleanOperations)
;

vtkSurfaceBooleanOperations::vtkSurfaceBooleanOperations() {
  this->SetNumberOfInputPorts(1);
  this->Loud = false;
  this->Mode = BOOLEAN_UNION;

}

vtkSurfaceBooleanOperations::~vtkSurfaceBooleanOperations() {
}

int vtkSurfaceBooleanOperations::FillInputPortInformation(
  int vtkNotUsed(port), vtkInformation* info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkPolyData");
  info->Set(vtkAlgorithm::INPUT_IS_REPEATABLE(), 1);
  return 1;
}

void vtkSurfaceBooleanOperations::SetMode(int mode) {
  this->Mode = mode;
  this->Modified();
}

void vtkSurfaceBooleanOperations::SetModeToUnion() {
  this->Mode = BOOLEAN_UNION;
  
  this->Modified();
}

void vtkSurfaceBooleanOperations::SetModeToIntersection() {
  this->Mode = BOOLEAN_INTERSECTION;

  this->Modified();
}

void vtkSurfaceBooleanOperations::SetModeToDifference() {
  this->Mode = BOOLEAN_DIFFERENCE;

  this->Modified();
}

void vtk2gts (vtkPolyData * input, GtsSurface * output)
{
  GtsVertex ** vertices;

  g_return_if_fail(output != NULL);
  if (g_hash_table_size(output->faces) != 0)
    g_warning("overwriting surface");
  input->BuildLinks();

  vertices = (GtsVertex **)
    g_malloc(input->GetNumberOfPoints() * sizeof(GtsVertex *));
  for (vtkIdType i = 0; i < input->GetNumberOfPoints(); ++i) {
    vtkFloatingPointType x[3];
    input->GetPoint(i, x);
    vertices[i] = gts_vertex_new(output->vertex_class, x[0], x[1], x[2]);
  }
  for (vtkIdType i = 0; i < input->GetNumberOfCells(); ++i) {
    vtkIdType npts, * pts;
    input->GetCellPoints(i, npts, pts);
    if (npts != 3) {
      g_warning("skipping no triangle cell");
      continue;
    }
    GtsEdge * newedges[3];
    for (gint j = 0; j < 3; ++j) {
      vtkIdType v1 = pts[j], v2 = pts[(j+1) % 3];
      newedges[j] = gts_edge_new(output->edge_class,
                                 vertices[v1], vertices[v2]);
      if (gts_segment_is_duplicate(GTS_SEGMENT(newedges[j]))) {
        GtsSegment * tmp = gts_segment_is_duplicate(GTS_SEGMENT(newedges[j]));
        gts_object_destroy(GTS_OBJECT(newedges[j]));
        newedges[j] = GTS_EDGE(tmp);
        g_return_if_fail(gts_segment_is_duplicate(GTS_SEGMENT(newedges[j])) ==
                         NULL);
      }
    }
    gts_surface_add_face(output,
                         gts_face_new(output->face_class,
                                      newedges[0], newedges[1], newedges[2]));
  }
  g_free(vertices);
}

static void gts2vtk_insert_point (GtsPoint * p, vtkPoints * points)
{
  points->InsertNextPoint(p->x, p->y, p->z);
  GTS_OBJECT(p)->reserved = GUINT_TO_POINTER(points->GetNumberOfPoints() - 1);
}
static void gts2vtk_insert_triangle (GtsTriangle * t, vtkCellArray * polys)
{
  GtsVertex * v1, * v2, * v3;
  gts_triangle_vertices(t, &v1, &v2, &v3);
  polys->InsertNextCell(3);
  polys->InsertCellPoint(GPOINTER_TO_UINT(GTS_OBJECT(v1)->reserved));
  polys->InsertCellPoint(GPOINTER_TO_UINT(GTS_OBJECT(v2)->reserved));
  polys->InsertCellPoint(GPOINTER_TO_UINT(GTS_OBJECT(v3)->reserved));
}
void gts2vtk (GtsSurface * input, vtkPolyData * output)
{
  vtkNew(vtkPoints,points);
  vtkNew(vtkCellArray,polys);
  gts_surface_foreach_vertex(input, (GtsFunc) &gts2vtk_insert_point, points);
  gts_surface_foreach_face(input, (GtsFunc) &gts2vtk_insert_triangle, polys);
  gts_surface_foreach_vertex(input, (GtsFunc) gts_object_reset_reserved, NULL);
  output->SetPoints(points);
  output->SetPolys(polys);
}

int vtkSurfaceBooleanOperations::RequestData(vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector, vtkInformationVector *outputVector) {
  if (Loud) cerr << "vtkSurfaceBooleanOperations::RequestData()" << endl;

  if(this->GetNumberOfInputConnections(0)!=2) {
        cerr << "vtkSurfaceBooleanOperations can only accept two input connections" << endl;
        cerr << " number of connections " << this->GetNumberOfInputConnections(0) << endl;
    return 0;
  }

  // get the info objects
  vtkInformation *inInfo1 = inputVector[0]->GetInformationObject(0);
  vtkInformation *inInfo2 = inputVector[0]->GetInformationObject(1);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  // get the input and output
  vtkPolyData *input1 = vtkPolyData::SafeDownCast(inInfo1->Get(
      vtkDataObject::DATA_OBJECT()));
  vtkPolyData *input2 = vtkPolyData::SafeDownCast(inInfo2->Get(
      vtkDataObject::DATA_OBJECT()));
  vtkPolyData *output = vtkPolyData::SafeDownCast(outInfo->Get(
      vtkDataObject::DATA_OBJECT()));

  vtkNew(vtkTriangleFilter, triangle1);
  triangle1->SetInputDataObject(input1);
  triangle1->Update();
  vtkNew(vtkCleanPolyData,clean1);
  clean1->SetInputDataObject(triangle1->GetOutput());
  clean1->Update();
  vtkNew(vtkDataSetSurfaceFilter,flip1);
  flip1->SetInputDataObject(clean1->GetOutput());
  flip1->SetPieceInvariant(1);
  flip1->Update();

  vtkNew(vtkTriangleFilter, triangle2);
  triangle2->SetInputDataObject(input2);
  triangle2->Update();
  vtkNew(vtkCleanPolyData,clean2);
  clean2->SetInputDataObject(triangle2->GetOutput());
  clean2->Update();
  vtkNew(vtkDataSetSurfaceFilter,flip2);
  flip2->SetInputDataObject(clean2->GetOutput());
  flip2->SetPieceInvariant(1);
  flip2->Update();

  GtsSurface * surface1 = gts_surface_new(gts_surface_class(), gts_face_class(),
                                   gts_edge_class(), gts_vertex_class());
  GtsSurface * surface2 = gts_surface_new(gts_surface_class(), gts_face_class(),
                                   gts_edge_class(), gts_vertex_class());

  vtk2gts(flip1->GetOutput(), surface1);
  vtk2gts(flip2->GetOutput(), surface2);

  GNode *tree1 = gts_bb_tree_surface(surface1);
  GNode *tree2 = gts_bb_tree_surface(surface2);

  GtsSurfaceInter* inter = gts_surface_inter_new(gts_surface_inter_class(),
    surface1,
    surface2,
    tree1,
    tree2,
    !gts_surface_is_closed(surface1),
    !gts_surface_is_closed(surface2)
  );

  GtsSurface *surface = gts_surface_new(gts_surface_class(),
                                gts_face_class(),
                                gts_edge_class(),
                                gts_vertex_class());

  if(this->Mode == BOOLEAN_UNION) {
    gts_surface_inter_boolean(inter, surface, GTS_1_OUT_2);
    gts_surface_inter_boolean(inter, surface, GTS_2_OUT_1);
  } else if(this->Mode == BOOLEAN_INTERSECTION) {
    gts_surface_inter_boolean (inter, surface, GTS_1_IN_2);
    gts_surface_inter_boolean (inter, surface, GTS_2_IN_1);
  } else if(this->Mode == BOOLEAN_DIFFERENCE) {
    gts_surface_inter_boolean (inter, surface, GTS_1_OUT_2);
    gts_surface_inter_boolean (inter, surface, GTS_2_IN_1);
    gts_surface_foreach_face (inter->s2, (GtsFunc) gts_triangle_revert, NULL);
    gts_surface_foreach_face (surface2, (GtsFunc) gts_triangle_revert, NULL);
  }

  gts2vtk(surface, output);

  return 1;
}

void vtkSurfaceBooleanOperations::PrintSelf(ostream& os, vtkIndent indent) {
  this->Superclass::PrintSelf(os, indent);
}

