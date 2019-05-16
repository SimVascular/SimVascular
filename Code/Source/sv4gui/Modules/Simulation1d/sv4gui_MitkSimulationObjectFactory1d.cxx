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

#include "sv4gui_MitkSimulationObjectFactory1d.h"

#include "sv4gui_MitkSimJob1d.h"

#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"
#include "mitkCoreObjectFactory.h"

//-------------------------------------
// sv4guiMitkSimulationObjectFactory1d
//-------------------------------------
//
sv4guiMitkSimulationObjectFactory1d::sv4guiMitkSimulationObjectFactory1d() : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone) {
    MITK_DEBUG << "sv4guiMitkSimulationObjectFactory1d c'tor" << std::endl;
    alreadyDone = true;
  }
}

sv4guiMitkSimulationObjectFactory1d::~sv4guiMitkSimulationObjectFactory1d()
{
}

//--------------
// CreateMapper
//--------------
//
mitk::Mapper::Pointer sv4guiMitkSimulationObjectFactory1d::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper = NULL;

  if ( id == mitk::BaseRenderer::Standard2D ) {
    if( dynamic_cast<sv4guiMitkSimJob1d*>(node->GetData())!=NULL ) {
    }
  } else if ( id == mitk::BaseRenderer::Standard3D ) {
    if( dynamic_cast<sv4guiMitkSimJob1d*>(node->GetData())!=NULL ) {
    }
  }
  return newMapper;
}

//----------------------
// SetDefaultProperties
//----------------------
//
void sv4guiMitkSimulationObjectFactory1d::SetDefaultProperties(mitk::DataNode* node)
{
  if (node==NULL) {
    return;
  }

  if(node->GetData() ==NULL) {
    return;
  }

  if( dynamic_cast<sv4guiMitkSimJob1d*>(node->GetData())!=NULL ) {
  }
}

//-------------------
// GetFileExtensions
//-------------------
//
const char* sv4guiMitkSimulationObjectFactory1d::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiMitkSimulationObjectFactory1d::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiMitkSimulationObjectFactory1d::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void sv4guiMitkSimulationObjectFactory1d::CreateFileExtensionsMap()
{
}

const char* sv4guiMitkSimulationObjectFactory1d::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void sv4guiMitkSimulationObjectFactory1d::RegisterIOFactories()
{
}

//---------------------------------------------
// Registersv4guiMitkSimulationObjectFactory1d
//---------------------------------------------
// Register the sv4guiMitkSimJobIO1d for reading and writing
// job xml files.
//
// [DaveP] How the m_MitkSimJobIO object is used is a mystery. 
//
Registersv4guiMitkSimulationObjectFactory1d::Registersv4guiMitkSimulationObjectFactory1d()
    : m_Factory( sv4guiMitkSimulationObjectFactory1d::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_MitkSimJobIO = new sv4guiMitkSimJobIO1d();
}

Registersv4guiMitkSimulationObjectFactory1d::~Registersv4guiMitkSimulationObjectFactory1d()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_MitkSimJobIO;
}

//static Registersv4guiMitkSimulationObjectFactory1d registersv4guiMitkSimulationObjectFactory1d;
