#include "svMitkMeshObjectFactory.h"

#include "svMitkMeshMapper2D.h"
#include "svMitkMeshMapper3D.h"

#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"
#include "mitkCoreObjectFactory.h"

svMitkMeshObjectFactory::svMitkMeshObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "svMitkMeshObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

svMitkMeshObjectFactory::~svMitkMeshObjectFactory()
{
}

mitk::Mapper::Pointer svMitkMeshObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<svMitkMesh*>(node->GetData())!=NULL )
    {
        newMapper = svMitkMeshMapper2D::New();
        newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svMitkMesh*>(node->GetData())!=NULL )
    {
        newMapper = svMitkMeshMapper3D::New();
        newMapper->SetDataNode(node);
    }
  }
  return newMapper;
}

void svMitkMeshObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svMitkMesh*>(node->GetData())!=NULL )
  {
      svMitkMeshMapper2D::SetDefaultProperties(node);
      svMitkMeshMapper3D::SetDefaultProperties(node);
  }
}

const char* svMitkMeshObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType svMitkMeshObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType svMitkMeshObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void svMitkMeshObjectFactory::CreateFileExtensionsMap()
{
}

const char* svMitkMeshObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void svMitkMeshObjectFactory::RegisterIOFactories()
{
}

RegistersvMitkMeshObjectFactory::RegistersvMitkMeshObjectFactory()
    : m_Factory( svMitkMeshObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_MitkMeshIO=new svMitkMeshIO();
}

RegistersvMitkMeshObjectFactory::~RegistersvMitkMeshObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_MitkMeshIO;
}

//static RegistersvMitkMeshObjectFactory registersvMitkMeshObjectFactory;
