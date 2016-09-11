#ifndef SVSEGMENTATIONLEGACYIO_H
#define SVSEGMENTATIONLEGACYIO_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "mitkDataNode.h"
#include "mitkDataStorage.h"
#include "mitkImage.h"
#include <QString>


class SVSEGMENTATION_EXPORT svSegmentationLegacyIO
{
public:

  svSegmentationLegacyIO(){}
  virtual ~svSegmentationLegacyIO(){}

  static mitk::DataNode::Pointer ReadContourGroupFile(QString filePath) ;

  static std::vector<mitk::DataNode::Pointer> ReadFiles(QString segDir);

  static void WriteContourGroupFile(mitk::DataNode::Pointer node, QString filePath);

  static void WriteSeg3DFile(mitk::DataNode::Pointer node, QString filePath);

  static void WriteTclFile(mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup, mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D, QString filePath);

  static void WriteFiles(mitk::DataStorage::SetOfObjects::ConstPointer contoruGroupNodes, mitk::DataStorage::SetOfObjects::ConstPointer seg3DNodes, QString segDir);


};

#endif // SVSEGMENTATIONLEGACYIO_H
