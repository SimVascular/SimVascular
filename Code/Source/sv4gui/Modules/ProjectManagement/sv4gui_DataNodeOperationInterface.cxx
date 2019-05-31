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

#include <regex>

#include "sv4gui_DataNodeOperationInterface.h"

#include "sv4gui_ProjectManager.h"
#include "sv4gui_DataNodeOperation.h"

// Messege defining valid Data Node names.
//
// This text is used to display a warning message in a popup window 
// when an invalid name is given.
//
std::string sv4guiDataNodeOperationInterface::ValidDataNodeNameMsg = 
  "must start with an alphanumeric character followed by alphanumeric characters, underscores, or single dashes and dots.";

//---------------------
// IsValidDataNodeName
//---------------------
// Check that a Data Node name is valid.
//
// This function is called when a user creates a new tool instance and specfies a name. For example, 
// right clicking on the 'Paths' Data Node and selecting 'Create Path'. Names with spaces and certain 
// non-alphanumeric characters can cause problems when the names are used as file names. 
//
// A valid name 
//    1) Starts with a letter, number or _.
//    2) Has no spaces.
//    3) Can use underscores (_) or single dot (.), dash (-) as separators. 
//    4) Does not start or end with a dot or dash.
//
// [Note] Using '\\w' in the regex permits underscores at the start and end 
// of names, as well as repeated underscores.
//
bool sv4guiDataNodeOperationInterface::IsValidDataNodeName(const std::string& name) 
{
  if (name.size() == 0) { 
      return false;
  }
  std::regex regex("^\\w(\\-?\\.?\\w)*$");
  return regex_match(name, regex);
}

sv4guiDataNodeOperationInterface::sv4guiDataNodeOperationInterface()
{
}

sv4guiDataNodeOperationInterface::~sv4guiDataNodeOperationInterface()
{
}

void sv4guiDataNodeOperationInterface::ExecuteOperation(mitk::Operation* op)
{
    sv4guiDataNodeOperation* operation = dynamic_cast<sv4guiDataNodeOperation*>(op);

    if ( operation==NULL )
    {
        MITK_ERROR << "No valid sv4guiDataNodeOperation" << std::endl;
        return;
    }

    mitk::DataStorage::Pointer dataStorage=operation->GetDataStorage();
    mitk::DataNode::Pointer dataNode=operation->GetDataNode();
    mitk::DataNode::Pointer parentNode=operation->GetParentNode();

    switch (operation->GetOperationType())
    {

    case sv4guiDataNodeOperation::OpADDDATANODE:
    {
        sv4guiProjectManager::AddDataNode(dataStorage,dataNode,parentNode);
    }
        break;

    case sv4guiDataNodeOperation::OpREMOVEDATANODE:
    {
        sv4guiProjectManager::RemoveDataNode(dataStorage,dataNode,parentNode);
    }
        break;

    default:
//        itkWarningMacro("sv4guiDataNodeOperationInterface could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}
