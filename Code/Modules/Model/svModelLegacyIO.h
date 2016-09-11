#ifndef SVMODELLEGACYIO_H
#define SVMODELLEGACYIO_H

#include <svModelExports.h>

#include "mitkDataNode.h"
#include "mitkDataStorage.h"
#include <QString>

class SVMODEL_EXPORT svModelLegacyIO
{
public:

  svModelLegacyIO(){}
  virtual ~svModelLegacyIO(){}

  static mitk::DataNode::Pointer ReadFile(QString filePath) ;

  static std::vector<mitk::DataNode::Pointer> ReadFiles(QString modelDir);

  static void WriteFile(mitk::DataNode::Pointer node, QString filePath);

  static void WriteFiles(mitk::DataStorage::SetOfObjects::ConstPointer modelNodes, QString modelDir);

};

#endif // SVMODELLEGACYIO_H
