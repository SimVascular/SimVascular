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

#include "svSegmentationObjectFactory.h"

#include "svContourModelVtkMapper2D.h"
#include "svContourGroupVtkMapper2D.h"
#include "svContourGroupVtkMapper3D.h"
#include "svMitkSeg3DVtkMapper3D.h"

#include "mitkCoreObjectFactory.h"
#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"

svSegmentationObjectFactory::svSegmentationObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "svSegmentationObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

svSegmentationObjectFactory::~svSegmentationObjectFactory()
{
}

mitk::Mapper::Pointer svSegmentationObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<svContourModel*>(node->GetData())!=NULL )
    {
      newMapper = svContourModelVtkMapper2D::New();
      newMapper->SetDataNode(node);
    }
    else if( dynamic_cast<svContourGroup*>(node->GetData())!=NULL )
    {
      newMapper = svContourGroupVtkMapper2D::New();
      newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svContourGroup*>(node->GetData())!=NULL )
    {
      newMapper = svContourGroupVtkMapper3D::New();
      newMapper->SetDataNode(node);
    }
    else if( dynamic_cast<svMitkSeg3D*>(node->GetData())!=NULL )
    {
      newMapper = svMitkSeg3DVtkMapper3D::New();
      newMapper->SetDataNode(node);
    }
  }
  return newMapper;
}

void svSegmentationObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svContourModel*>(node->GetData())!=NULL )
  {
    svContourModelVtkMapper2D::SetDefaultProperties(node);
  }
  if( dynamic_cast<svContourGroup*>(node->GetData())!=NULL )
  {
      svContourGroupVtkMapper2D::SetDefaultProperties(node);
      svContourGroupVtkMapper3D::SetDefaultProperties(node);
  }
  if( dynamic_cast<svMitkSeg3D*>(node->GetData())!=NULL )
  {
    svMitkSeg3DVtkMapper3D::SetDefaultProperties(node);
  }
}

const char* svSegmentationObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType svSegmentationObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType svSegmentationObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void svSegmentationObjectFactory::CreateFileExtensionsMap()
{
}

const char* svSegmentationObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void svSegmentationObjectFactory::RegisterIOFactories()
{
}

RegistersvSegmentationObjectFactory::RegistersvSegmentationObjectFactory()
    : m_Factory( svSegmentationObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_ContourGroupIO=new svContourGroupIO();
    m_Seg3DIO=new svMitkSeg3DIO();
}

RegistersvSegmentationObjectFactory::~RegistersvSegmentationObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_ContourGroupIO;
    delete m_Seg3DIO;
}

//static RegistersvSegmentationObjectFactory registersvSegmentationObjectFactory;
