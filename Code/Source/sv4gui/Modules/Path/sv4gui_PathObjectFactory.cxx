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

#include "sv4gui_PathObjectFactory.h"

#include "sv4gui_Path.h"
#include "sv4gui_PathVtkMapper2D.h"
#include "sv4gui_PathVtkMapper3D.h"

#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"
#include "mitkCoreObjectFactory.h"

sv4guiPathObjectFactory::sv4guiPathObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "sv4guiPathObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

sv4guiPathObjectFactory::~sv4guiPathObjectFactory()
{
}

mitk::Mapper::Pointer sv4guiPathObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=nullptr;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<sv4guiPath*>(node->GetData())!=nullptr )
    {
      newMapper = sv4guiPathVtkMapper2D::New();
      newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<sv4guiPath*>(node->GetData())!=nullptr )
    {
      newMapper = sv4guiPathVtkMapper3D::New();
      newMapper->SetDataNode(node);
    }

  }
  return newMapper;
}

void sv4guiPathObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==nullptr)
    return;

  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==nullptr)
    return;

  if( dynamic_cast<sv4guiPath*>(node->GetData())!=nullptr )
  {
    sv4guiPathVtkMapper2D::SetDefaultProperties(node);
    sv4guiPathVtkMapper3D::SetDefaultProperties(node);
  }

}

std::string sv4guiPathObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiPathObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiPathObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void sv4guiPathObjectFactory::CreateFileExtensionsMap()
{
}

std::string sv4guiPathObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void sv4guiPathObjectFactory::RegisterIOFactories()
{
}

Registersv4guiPathObjectFactory::Registersv4guiPathObjectFactory()
    : m_Factory( sv4guiPathObjectFactory::New() )
  {
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_sv4guiPathIO=new sv4guiPathIO();
  }

Registersv4guiPathObjectFactory::~Registersv4guiPathObjectFactory()
{
  mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
  delete m_sv4guiPathIO;
}

//static Registersv4guiPathObjectFactory registersv4guiPathObjectFactory;
