#ifndef SVPATHLEGACYIO_H
#define SVPATHLEGACYIO_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "mitkDataNode.h"
#include "mitkDataStorage.h"
#include <QString>

class SVPATH_EXPORT svPathLegacyIO
{
public:

  svPathLegacyIO(){}
  virtual ~svPathLegacyIO(){}

  static std::vector<mitk::DataNode::Pointer> ReadFile(QString filePath) ;

  static void WriteFile(mitk::DataStorage::SetOfObjects::ConstPointer rs, QString filePath);

};

#endif // SVPATHLEGACYIO_H
