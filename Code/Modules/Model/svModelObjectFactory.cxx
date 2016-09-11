#include "svModelObjectFactory.h"

#include "svModel.h"
#include "svModelIO.h"

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
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svModel*>(node->GetData())!=NULL )
    {
    }
  }
  return newMapper;
}

void svModelObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svModel*>(node->GetData())!=NULL )
  {
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

struct RegistersvModelObjectFactory{
  RegistersvModelObjectFactory()
    : m_Factory( svModelObjectFactory::New() )
  {
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_ModelIO=new svModelIO();
  }

  ~RegistersvModelObjectFactory()
  {
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_ModelIO;
  }

  svModelObjectFactory::Pointer m_Factory;
  svModelIO* m_ModelIO;
};

static RegistersvModelObjectFactory registersvModelObjectFactory;
