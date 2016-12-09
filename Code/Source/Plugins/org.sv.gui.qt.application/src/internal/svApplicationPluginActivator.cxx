#include "svApplicationPluginActivator.h"

//#include <QtWidgetsExtRegisterClasses.h>

//#include "QmitkAboutHandler.h"
//#include "QmitkAppInstancesPreferencePage.h"
//#include "QmitkExternalProgramsPreferencePage.h"
//#include "QmitkInputDevicesPrefPage.h"

//#include "QmitkModuleView.h"

//#include <mitkIDataStorageService.h>
//#include <mitkSceneIO.h>
//#include <mitkProgressBar.h>
//#include <mitkRenderingManager.h>
//#include <mitkIOUtil.h>

//#include <mitkBaseApplication.h>

//#include <berryPlatformUI.h>
//#include <berryIPreferencesService.h>
//#include <berryPlatform.h>

//#include <Poco/Util/OptionProcessor.h>

//#include <QProcess>
//#include <QMainWindow>

#include <mitkVersion.h>
#include <mitkLogMacros.h>

#include <service/cm/ctkConfigurationAdmin.h>
#include <service/cm/ctkConfiguration.h>

ctkPluginContext* svApplicationPluginActivator::_context = nullptr;

svApplicationPluginActivator* svApplicationPluginActivator::inst = nullptr;

svApplicationPluginActivator::svApplicationPluginActivator()
{
  inst = this;
}

svApplicationPluginActivator::~svApplicationPluginActivator()
{
}

svApplicationPluginActivator* svApplicationPluginActivator::GetDefault()
{
  return inst;
}

void svApplicationPluginActivator::start(ctkPluginContext* context)
{
    berry::AbstractUICTKPlugin::start(context);

    this->_context = context;

    //  QtWidgetsExtRegisterClasses();

    //  BERRY_REGISTER_EXTENSION_CLASS(QmitkAboutHandler, context)
    //  BERRY_REGISTER_EXTENSION_CLASS(QmitkAppInstancesPreferencePage, context)
    //  BERRY_REGISTER_EXTENSION_CLASS(QmitkExternalProgramsPreferencePage, context)
    //  BERRY_REGISTER_EXTENSION_CLASS(QmitkInputDevicesPrefPage, context)

    //  BERRY_REGISTER_EXTENSION_CLASS(QmitkModuleView, context)

    //  if (qApp->metaObject()->indexOfSignal("messageReceived(QByteArray)") > -1)
    //  {
    //    connect(qApp, SIGNAL(messageReceived(QByteArray)), this, SLOT(handleIPCMessage(QByteArray)));
    //  }

    //  // This is a potentially long running operation.
    //  loadDataFromDisk(berry::Platform::GetApplicationArgs(), true);

    ctkServiceReference cmRef = context->getServiceReference<ctkConfigurationAdmin>();
    ctkConfigurationAdmin* configAdmin = nullptr;
    if (cmRef)
    {
      configAdmin = context->getService<ctkConfigurationAdmin>(cmRef);
    }

    // Use the CTK Configuration Admin service to configure the BlueBerry help system
    if (configAdmin)
    {
      ctkConfigurationPtr conf = configAdmin->getConfiguration("org.blueberry.services.help", QString());
      ctkDictionary helpProps;
      helpProps.insert("homePage", "qthelp://org.mitk.gui.qt.extapplication/bundle/index.html");
      conf->update(helpProps);
      context->ungetService(cmRef);
    }
    else
    {
      MITK_WARN << "Configuration Admin service unavailable, cannot set home page url.";
    }
}

void svApplicationPluginActivator::stop(ctkPluginContext* context)
{
    Q_UNUSED(context)

    this->_context = nullptr;
}

ctkPluginContext* svApplicationPluginActivator::getContext()
{
    return _context;
}

//void svApplicationPluginActivator::loadDataFromDisk(const QStringList &arguments, bool globalReinit)
//{
//  if (!arguments.empty())
//  {
//    ctkServiceReference serviceRef = _context->getServiceReference<mitk::IDataStorageService>();
//    if (serviceRef)
//    {
//       mitk::IDataStorageService* dataStorageService = _context->getService<mitk::IDataStorageService>(serviceRef);
//       mitk::DataStorage::Pointer dataStorage = dataStorageService->GetDefaultDataStorage()->GetDataStorage();

//       int argumentsAdded = 0;
//       for (int i = 0; i < arguments.size(); ++i)
//       {
//         if (arguments[i].right(5) == ".mitk")
//         {
//           mitk::SceneIO::Pointer sceneIO = mitk::SceneIO::New();

//           bool clearDataStorageFirst(false);
//           mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
//           dataStorage = sceneIO->LoadScene( arguments[i].toLocal8Bit().constData(), dataStorage, clearDataStorageFirst );
//           mitk::ProgressBar::GetInstance()->Progress(2);
//           argumentsAdded++;
//         }
//         else
//         {
//           try
//           {
//             const std::string path(arguments[i].toStdString());
//             mitk::IOUtil::Load(path, *dataStorage);
//             argumentsAdded++;
//           }
//           catch(...)
//           {
//             MITK_WARN << "Failed to load command line argument: " << arguments[i].toStdString();
//           }
//         }
//       } // end for each command line argument

//       if (argumentsAdded > 0 && globalReinit)
//       {
//         // calculate bounding geometry
//         mitk::RenderingManager::GetInstance()->InitializeViews(dataStorage->ComputeBoundingGeometry3D());
//       }
//    }
//    else
//    {
//      MITK_ERROR << "A service reference for mitk::IDataStorageService does not exist";
//    }
//  }
//}

//void svApplicationPluginActivator::startNewInstance(const QStringList &args, const QStringList& files)
//{
//  QStringList newArgs(args);
//#ifdef Q_OS_UNIX
//  newArgs << QString("--") + mitk::BaseApplication::ARG_NEWINSTANCE;
//#else
//  newArgs << QString("/") + mitk::BaseApplication::ARG_NEWINSTANCE;
//#endif
//  newArgs << files;
//  QProcess::startDetached(qApp->applicationFilePath(), newArgs);
//}

//void svApplicationPluginActivator::handleIPCMessage(const QByteArray& msg)
//{
//  QDataStream ds(msg);
//  QString msgType;
//  ds >> msgType;

//  // we only handle messages containing command line arguments
//  if (msgType != "$cmdLineArgs") return;

//  // activate the current workbench window
//  berry::IWorkbenchWindow::Pointer window =
//      berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

//  QMainWindow* mainWindow =
//   static_cast<QMainWindow*> (window->GetShell()->GetControl());

//  mainWindow->setWindowState(mainWindow->windowState() & ~Qt::WindowMinimized);
//  mainWindow->raise();
//  mainWindow->activateWindow();

//  // Get the preferences for the instantiation behavior
//  berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
//  berry::IPreferences::Pointer prefs = prefService->GetSystemPreferences()->Node("/General");
//  bool newInstanceAlways = prefs->GetBool("newInstance.always", false);
//  bool newInstanceScene = prefs->GetBool("newInstance.scene", true);

//  QStringList args;
//  ds >> args;

//  QStringList fileArgs;
//  QStringList sceneArgs;

//  foreach (QString arg, args)
//  {
//    if (arg.endsWith(".mitk"))
//    {
//      sceneArgs << arg;
//    }
//    else
//    {
//      fileArgs << arg;
//    }
//  }

//  if (newInstanceAlways)
//  {
//    if (newInstanceScene)
//    {
//      startNewInstance(args, fileArgs);

//      foreach(QString sceneFile, sceneArgs)
//      {
//        startNewInstance(args, QStringList(sceneFile));
//      }
//    }
//    else
//    {
//      fileArgs.append(sceneArgs);
//      startNewInstance(args, fileArgs);
//    }
//  }
//  else
//  {
//    loadDataFromDisk(fileArgs, false);
//    if (newInstanceScene)
//    {
//      foreach(QString sceneFile, sceneArgs)
//      {
//        startNewInstance(args, QStringList(sceneFile));
//      }
//    }
//    else
//    {
//      loadDataFromDisk(sceneArgs, false);
//    }
//  }

//}
