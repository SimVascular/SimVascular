#include "svModelObjectFactory.h"

#include "svModelVtkMapper2D.h"
#include "svModelVtkMapper3D.h"

#include "simvascular_options.h"

#ifdef SV_USE_OpenCASCADE
#include "cv_globals.h"
#include <TDocStd_Document.hxx>
#include <XCAFDoc_DocumentTool.hxx>
#include <XCAFApp_Application.hxx>
#endif

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
        newMapper = svModelVtkMapper2D::New();
        newMapper->SetDataNode(node);
    }
  }
  else if ( id == mitk::BaseRenderer::Standard3D )
  {
    if( dynamic_cast<svModel*>(node->GetData())!=NULL )
    {
        newMapper = svModelVtkMapper3D::New();
        newMapper->SetDataNode(node);
    }
  }
  return newMapper;
}

void svModelObjectFactory::SetDefaultProperties(mitk::DataNode* node)
{

  if(node==NULL)
    return;

//  mitk::DataNode::Pointer nodePointer = node;

  if(node->GetData() ==NULL)
    return;

  if( dynamic_cast<svModel*>(node->GetData())!=NULL )
  {
      svModelVtkMapper2D::SetDefaultProperties(node);
      svModelVtkMapper3D::SetDefaultProperties(node);
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

RegistersvModelObjectFactory::RegistersvModelObjectFactory()
    : m_Factory( svModelObjectFactory::New() )
{
    mitk::CoreObjectFactory::GetInstance()->RegisterExtraFactory( m_Factory );
    m_ModelIO=new svModelIO();

#ifdef SV_USE_OpenCASCADE
    Handle(XCAFApp_Application) OCCTManager = static_cast<XCAFApp_Application*>(gOCCTManager);
    //gOCCTManager = new AppStd_Application;
    OCCTManager = XCAFApp_Application::GetApplication();
    //if ( gOCCTManager == NULL ) {
    //  fprintf( stderr, "error allocating gOCCTManager\n" );
    //  return TCL_ERROR;
    //}
    Handle(TDocStd_Document) doc;
    //gOCCTManager->NewDocument("Standard",doc);
    OCCTManager->NewDocument("MDTV-XCAF",doc);
    if ( !XCAFDoc_DocumentTool::IsXCAFDocument(doc))
    {
      MITK_ERROR<<"OCCT XDE is not setup correctly, file i/o and register of solid will not work correctly";
    }
#endif

}

RegistersvModelObjectFactory::~RegistersvModelObjectFactory()
{
    mitk::CoreObjectFactory::GetInstance()->UnRegisterExtraFactory( m_Factory );
    delete m_ModelIO;
}

//static RegistersvModelObjectFactory registersvModelObjectFactory;
