#include "svApplication.h"

#include <vtkNew.h>
#include <vtksys/SystemTools.hxx>

#include <mitkStandaloneDataStorage.h>

svApplication::svApplication(int &_argc, char **_argv):Superclass(_argc, _argv)
{

    this->Environment = QProcessEnvironment::systemEnvironment();

//    this->svHome = this->discoverSVHomeDirectory();
//    this->setEnvironmentVariable("SV_HOME", this->svHome);

    mPythonManager=new svPythonManager();
    mPythonManager->initPythonQtAll();
//    this->setPythonEnvironmentVariables();

    mPythonManager->setSystemExitExceptionHandlerEnabled(true);
    this->connect(mPythonManager, SIGNAL(systemExitExceptionRaised(int)), this, SLOT(terminate(int)));

    mStandaloneDataStorage=mitk::StandaloneDataStorage::New();
    mDataStorage=static_cast<mitk::DataStorage::Pointer>(mStandaloneDataStorage);

    mRenderWindowPart=new svRenderWindowPart;
}


//-----------------------------------------------------------------------------
svApplication::~svApplication()
{
}

//-----------------------------------------------------------------------------
svApplication* svApplication::application()
{
  svApplication* app = qobject_cast<svApplication*>(QApplication::instance());
  return app;
}

void svApplication::setEnvironmentVariable(const QString& key, const QString& value)
{

  this->Environment.insert(key, value);
  // Since QProcessEnvironment can't be used to update the environment of the
  // current process, let's use 'putenv()'.
  // See http://doc.qt.nokia.com/4.6/qprocessenvironment.html#details
  vtksys::SystemTools::PutEnv(QString("%1=%2").arg(key).arg(value).toLatin1().constData());

  this->setPythonOsEnviron(key, value);

}

void svApplication::setPythonOsEnviron(const QString& key, const QString& value)
{
  if(!this->mPythonManager->isPythonInitialized())
    {
    return;
    }
  this->mPythonManager->executeString(
        QString("import os; os.environ['%1']='%2'; del os").arg(key).arg(value));
}

void svApplication::setPythonEnvironmentVariables()
{
//  // Set PYTHONHOME if not already done
//  if (this->Environment.value("PYTHONHOME").isEmpty())
//    {
//    if (!isInstalled())
//      {
//      // TODO
//      }
//    else
//      {
//      svApplication * app = svApplication::application();
//      this->setEnvironmentVariable("PYTHONHOME", app->svHome() + "/lib/Python");
//      }
//    }

//  // Set PYTHONPATH if not already done
//  if (this->Environment.value("PYTHONPATH").isEmpty())
//    {
//    this->setEnvironmentVariable(
//          "PYTHONPATH", svPythonManager().pythonPaths().join(":"));
//    }
}

svPythonManager* svApplication::pythonManager() const
{
    return mPythonManager;
}

//void setPythonManager (svPythonManager* pythonManager){

//}

mitk::DataStorage::Pointer svApplication::dataStorage() const
{
	return mDataStorage;
}

mitk::StandaloneDataStorage::Pointer svApplication::standaloneDataStorage()
{
    return mStandaloneDataStorage;
}


svQmitkDataManager* svApplication::dataManager() const
{
	return mDataManager;

}

void svApplication::setDataManager(svQmitkDataManager* dataManager)
{
	mDataManager=dataManager;
}

QmitkStdMultiWidget* svApplication::displayWidget() const
{
	return mDisplayWidget;
}

void svApplication::setDisplayWidget(QmitkStdMultiWidget* displayWidget)
{
	mDisplayWidget=displayWidget;
}

svRenderWindowPart* svApplication::renderWindowPart() const
{
	return mRenderWindowPart;
}

void svApplication::setExtensionManager(svExtensionManager* extensionManager){
    mExtensionManager=extensionManager;
}

svExtensionManager* svApplication::extensionManager()
{
    return mExtensionManager;
}

void svApplication::terminate(int returnCode)
{
  this->exit(returnCode);
}
