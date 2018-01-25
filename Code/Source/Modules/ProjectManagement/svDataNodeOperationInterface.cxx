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

#include "svDataNodeOperationInterface.h"

#include "svProjectManager.h"
#include "svDataNodeOperation.h"

svDataNodeOperationInterface::svDataNodeOperationInterface()
{
}

svDataNodeOperationInterface::~svDataNodeOperationInterface()
{
}

void svDataNodeOperationInterface::ExecuteOperation(mitk::Operation* op)
{
    svDataNodeOperation* operation = dynamic_cast<svDataNodeOperation*>(op);

    if ( operation==NULL )
    {
        MITK_ERROR << "No valid svDataNodeOperation" << std::endl;
        return;
    }

    mitk::DataStorage::Pointer dataStorage=operation->GetDataStorage();
    mitk::DataNode::Pointer dataNode=operation->GetDataNode();
    mitk::DataNode::Pointer parentNode=operation->GetParentNode();

    switch (operation->GetOperationType())
    {

    case svDataNodeOperation::OpADDDATANODE:
    {
        svProjectManager::AddDataNode(dataStorage,dataNode,parentNode);
    }
        break;

    case svDataNodeOperation::OpREMOVEDATANODE:
    {
        svProjectManager::RemoveDataNode(dataStorage,dataNode,parentNode);
    }
        break;

    default:
//        itkWarningMacro("svDataNodeOperationInterface could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}
