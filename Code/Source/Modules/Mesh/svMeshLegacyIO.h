#ifndef SVMESHLEGACYIO_H
#define SVMESHLEGACYIO_H

#include <svMeshExports.h>

#include "mitkDataNode.h"
#include "mitkDataStorage.h"
#include <QString>

class SVMESH_EXPORT svMeshLegacyIO
{
public:

  svMeshLegacyIO(){}
  virtual ~svMeshLegacyIO(){}

  static void WriteFiles(mitk::DataNode::Pointer node, QString meshDir);

};

#endif // SVMESHLEGACYIO_H
