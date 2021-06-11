/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_SEGMENTATIONLEGACYIO_H
#define SV4GUI_SEGMENTATIONLEGACYIO_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>
#include "sv4gui_ContourGroup.h"

#include "mitkDataNode.h"
#include "mitkDataStorage.h"
#include "mitkImage.h"
#include <QString>


class SV4GUIMODULESEGMENTATION_EXPORT sv4guiSegmentationLegacyIO
{
public:

  sv4guiSegmentationLegacyIO(){}
  virtual ~sv4guiSegmentationLegacyIO(){}

  static mitk::DataNode::Pointer ReadContourGroupFile(QString filePath) ;

  static sv4guiContourGroup::Pointer CreateGroupFromFile(const std::string& fileName);

  static std::vector<mitk::DataNode::Pointer> ReadFiles(QString segDir);

  static void WriteContourGroupFile(mitk::DataNode::Pointer node, QString filePath);

  static void WriteSeg3DFile(mitk::DataNode::Pointer node, QString filePath);

  static void WriteTclFile(mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup, mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D, QString filePath);

  static void WriteFiles(mitk::DataStorage::SetOfObjects::ConstPointer contoruGroupNodes, mitk::DataStorage::SetOfObjects::ConstPointer seg3DNodes, QString segDir);


};

#endif // SV4GUI_SEGMENTATIONLEGACYIO_H
