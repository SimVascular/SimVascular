#include "svMitkSimulationObjectFactory.h"

#include "svMitkSimJob.h"

#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"
#include "mitkCoreObjectFactory.h"

svMitkSimulationObjectFactory::svMitkSimulationObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "svMitkSimulationObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

svMitkSimulationObjectFactory::~svMitkSimulationObjectFactory()
{
}

mitk::Mapper::Pointer svMitkSimulationObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<svMitkSimJob*>(node->GetData())!=NULL )
    {
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svMitkSimJob*>(node->GetData())!=NULL )
    {
    }
  }
  return newMapper;
}

void svMitkSimulationObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svMitkSimJob*>(node->GetData())!=NULL )
  {
  }
}

const char* svMitkSimulationObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType svMitkSimulationObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType svMitkSimulationObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void svMitkSimulationObjectFactory::CreateFileExtensionsMap()
{
}

const char* svMitkSimulationObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void svMitkSimulationObjectFactory::RegisterIOFactories()
{
}

RegistersvMitkSimulationObjectFactory::RegistersvMitkSimulationObjectFactory()
    : m_Factory( svMitkSimulationObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_MitkSimJobIO=new svMitkSimJobIO();
}

RegistersvMitkSimulationObjectFactory::~RegistersvMitkSimulationObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_MitkSimJobIO;
}

//static RegistersvMitkSimulationObjectFactory registersvMitkSimulationObjectFactory;
