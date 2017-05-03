#ifndef SVMESHLEGACYIO_H
#define SVMESHLEGACYIO_H

#include <svMeshExports.h>

#include "svMesh.h"

#include "mitkDataNode.h"

#include <QString>

class SVMESH_EXPORT svMeshLegacyIO
{
public:

  svMeshLegacyIO(){}
  virtual ~svMeshLegacyIO(){}

  static bool WriteFiles(mitk::DataNode::Pointer meshNode, svModelElement* modelElement, QString meshDir);

  static bool WriteFiles(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, svModelElement* modelElement, QString meshDir);


};

#endif // SVMESHLEGACYIO_H
