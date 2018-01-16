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

#include "svModelObjectFactory.h"

#include "svModelVtkMapper2D.h"
#include "svModelVtkMapper3D.h"

#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"
#include "mitkCoreObjectFactory.h"

svModelObjectFactory::svModelObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "svModelObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

svModelObjectFactory::~svModelObjectFactory()
{
}

mitk::Mapper::Pointer svModelObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<svModel*>(node->GetData())!=NULL )
    {
        newMapper = svModelVtkMapper2D::New();
        newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svModel*>(node->GetData())!=NULL )
    {
        newMapper = svModelVtkMapper3D::New();
        newMapper->SetDataNode(node);
    }
  }
  return newMapper;
}

void svModelObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

//  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svModel*>(node->GetData())!=NULL )
  {
      svModelVtkMapper2D::SetDefaultProperties(node);
      svModelVtkMapper3D::SetDefaultProperties(node);
  }
}

const char* svModelObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType svModelObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType svModelObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void svModelObjectFactory::CreateFileExtensionsMap()
{
}

const char* svModelObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void svModelObjectFactory::RegisterIOFactories()
{
}

RegistersvModelObjectFactory::RegistersvModelObjectFactory()
    : m_Factory( svModelObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_ModelIO=new svModelIO();
}

RegistersvModelObjectFactory::~RegistersvModelObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_ModelIO;
}

//static RegistersvModelObjectFactory registersvModelObjectFactory;
