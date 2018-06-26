#include "sv4gui_MitksvFSIObjectFactory.h"

#include "sv4gui_MitksvFSIJob.h"

#include <mitkProperties.h>
#include <mitkBaseRenderer.h>
#include <mitkDataNode.h>
#include <mitkCoreObjectFactory.h>

sv4guiMitksvFSIObjectFactory::sv4guiMitksvFSIObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "sv4guiMitksvFSIObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

sv4guiMitksvFSIObjectFactory::~sv4guiMitksvFSIObjectFactory()
{
}

mitk::Mapper::Pointer sv4guiMitksvFSIObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<sv4guiMitksvFSIJob*>(node->GetData())!=NULL )
    {
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<sv4guiMitksvFSIJob*>(node->GetData())!=NULL )
    {
    }
  }
  return newMapper;
}

void sv4guiMitksvFSIObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<sv4guiMitksvFSIJob*>(node->GetData())!=NULL )
  {
  }
}

const char* sv4guiMitksvFSIObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiMitksvFSIObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType sv4guiMitksvFSIObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void sv4guiMitksvFSIObjectFactory::CreateFileExtensionsMap()
{
}

const char* sv4guiMitksvFSIObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void sv4guiMitksvFSIObjectFactory::RegisterIOFactories()
{
}

Registersv4guiMitksvFSIObjectFactory::Registersv4guiMitksvFSIObjectFactory()
    : m_Factory( sv4guiMitksvFSIObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_MitkSimJobIO=new sv4guiMitksvFSIJobIO();
}

Registersv4guiMitksvFSIObjectFactory::~Registersv4guiMitksvFSIObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_MitkSimJobIO;
}

static Registersv4guiMitksvFSIObjectFactory registersv4guiMitksvFSIObjectFactory;
