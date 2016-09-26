#include "svSegmentationObjectFactory.h"

#include "svContourModelVtkMapper2D.h"
#include "svContourGroupVtkMapper2D.h"
#include "svContourGroupVtkMapper3D.h"

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
}

RegistersvSegmentationObjectFactory::~RegistersvSegmentationObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_ContourGroupIO;
}

//static RegistersvSegmentationObjectFactory registersvSegmentationObjectFactory;
