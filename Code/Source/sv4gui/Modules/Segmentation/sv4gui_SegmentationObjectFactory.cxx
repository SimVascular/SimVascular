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

#include "sv4gui_SegmentationObjectFactory.h"

#include "sv4gui_ContourModelVtkMapper2D.h"
#include "sv4gui_ContourGroupVtkMapper2D.h"
#include "sv4gui_ContourGroupVtkMapper3D.h"
#include "sv4gui_MitkSeg3DVtkMapper3D.h"

#include "mitkCoreObjectFactory.h"
#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"

sv4guiSegmentationObjectFactory::sv4guiSegmentationObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "sv4guiSegmentationObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

sv4guiSegmentationObjectFactory::~sv4guiSegmentationObjectFactory()
{
}

mitk::Mapper::Pointer sv4guiSegmentationObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<sv4guiContourModel*>(node->GetData())!=NULL )
    {
      newMapper = sv4guiContourModelVtkMapper2D::New();
      newMapper->SetDataNode(node);
    }
    else if( dynamic_cast<sv4guiContourGroup*>(node->GetData())!=NULL )
    {
      newMapper = sv4guiContourGroupVtkMapper2D::New();
      newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<sv4guiContourGroup*>(node->GetData())!=NULL )
    {
      newMapper = sv4guiContourGroupVtkMapper3D::New();
      newMapper->SetDataNode(node);
    }
    else if( dynamic_cast<sv4guiMitkSeg3D*>(node->GetData())!=NULL )
    {
      newMapper = sv4guiMitkSeg3DVtkMapper3D::New();
      newMapper->SetDataNode(node);
    }
  }
  return newMapper;
}

void sv4guiSegmentationObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<sv4guiContourModel*>(node->GetData())!=NULL )
  {
    sv4guiContourModelVtkMapper2D::SetDefaultProperties(node);
  }
  if( dynamic_cast<sv4guiContourGroup*>(node->GetData())!=NULL )
  {
      sv4guiContourGroupVtkMapper2D::SetDefaultProperties(node);
      sv4guiContourGroupVtkMapper3D::SetDefaultProperties(node);
  }
  if( dynamic_cast<sv4guiMitkSeg3D*>(node->GetData())!=NULL )
  {
    sv4guiMitkSeg3DVtkMapper3D::SetDefaultProperties(node);
  }
}

const char* sv4guiSegmentationObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiSegmentationObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiSegmentationObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void sv4guiSegmentationObjectFactory::CreateFileExtensionsMap()
{
}

const char* sv4guiSegmentationObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void sv4guiSegmentationObjectFactory::RegisterIOFactories()
{
}

Registersv4guiSegmentationObjectFactory::Registersv4guiSegmentationObjectFactory()
    : m_Factory( sv4guiSegmentationObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_ContourGroupIO=new sv4guiContourGroupIO();
    m_Seg3DIO=new sv4guiMitkSeg3DIO();
}

Registersv4guiSegmentationObjectFactory::~Registersv4guiSegmentationObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_ContourGroupIO;
    delete m_Seg3DIO;
}

//static Registersv4guiSegmentationObjectFactory registersv4guiSegmentationObjectFactory;
