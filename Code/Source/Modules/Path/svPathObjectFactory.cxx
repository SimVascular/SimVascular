#include "svPathObjectFactory.h"

#include "svPath.h"
#include "svPathVtkMapper2D.h"
#include "svPathVtkMapper3D.h"

#include "mitkProperties.h"
#include "mitkBaseRenderer.h"
#include "mitkDataNode.h"
#include "mitkCoreObjectFactory.h"

svPathObjectFactory::svPathObjectFactory()
  : mitk::CoreObjectFactoryBase()
{
  static bool alreadyDone = false;
  if (!alreadyDone)
  {
    MITK_DEBUG << "svPathObjectFactory c'tor" << std::endl;

    alreadyDone = true;
  }
}

svPathObjectFactory::~svPathObjectFactory()
{
}

mitk::Mapper::Pointer svPathObjectFactory::CreateMapper(mitk::DataNode* node, MapperSlotId id)
{
  mitk::Mapper::Pointer newMapper=NULL;

  if ( id == mitk::BaseRenderer::Standard2D )
  {
    if( dynamic_cast<svPath*>(node->GetData())!=NULL )
    {
      newMapper = svPathVtkMapper2D::New();
      newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svPath*>(node->GetData())!=NULL )
    {
      newMapper = svPathVtkMapper3D::New();
      newMapper->SetDataNode(node);
    }

  }
  return newMapper;
}

void svPathObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svPath*>(node->GetData())!=NULL )
  {
    svPathVtkMapper2D::SetDefaultProperties(node);
    svPathVtkMapper3D::SetDefaultProperties(node);
  }

}

const char* svPathObjectFactory::GetFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_FileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

mitk::CoreObjectFactoryBase::MultimapType svPathObjectFactory::GetFileExtensionsMap()
{
  return m_FileExtensionsMap;
}

mitk::CoreObjectFactoryBase::MultimapType svPathObjectFactory::GetSaveFileExtensionsMap()
{
  return m_SaveFileExtensionsMap;
}

void svPathObjectFactory::CreateFileExtensionsMap()
{
}

const char* svPathObjectFactory::GetSaveFileExtensions()
{
  std::string fileExtension;
  this->CreateFileExtensions(m_SaveFileExtensionsMap, fileExtension);
  return fileExtension.c_str();
}

void svPathObjectFactory::RegisterIOFactories()
{
}

RegistersvPathObjectFactory::RegistersvPathObjectFactory()
    : m_Factory( svPathObjectFactory::New() )
  {
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_svPathIO=new svPathIO();
  }

RegistersvPathObjectFactory::~RegistersvPathObjectFactory()
{
  mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
  delete m_svPathIO;
}

//static RegistersvPathObjectFactory registersvPathObjectFactory;
