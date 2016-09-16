// Qt includes
#include <QBitArray>
#include <QSettings>

// CTK includes
#include <ctkVTKPythonQtWrapperFactory.h>

// PythonQt includes
#include <PythonQt.h>
#ifndef SV_NO_PYTHONQT_ALL
  #include <PythonQt_QtAll.h>
#endif

// VTK includes
#include <vtkPythonUtil.h>
#include <vtkVersion.h>

#include "svPythonManager.h"
#include "svApplication.h"
#include "svScriptUtil.h"

#ifndef SV_NO_HARDCODED_PYTHON_PATHS
  #include "svConfigure.h"
#endif

svPythonManager::svPythonManager(QObject* _parent)
    :Superclass(_parent)
{

    this->Factory = 0;
    int flags = this->initializationFlags();
    flags &= ~(PythonQt::IgnoreSiteModule); // Clear bit
    this->setInitializationFlags(flags);
    mainContext(); // Initialize python
    #ifndef SV_NO_PYTHONQT_ALL
      PythonQt_QtAll::init();
    #else
      PythonQt::init();
    #endif
//    this->initialize();
}

svPythonManager::~svPythonManager()
{
    if (this->Factory)
    {
        delete this->Factory;
        this->Factory = 0;
    }
}

void svPythonManager::initPythonQtAll()
{
#ifndef SV_NO_PYTHONQT_ALL
    PythonQt_QtAll::init();
#endif
}

QStringList svPythonManager::pythonPaths()
{
    QStringList paths;
    paths << Superclass::pythonPaths();

//    // Add here python path specific to the Installed tree, for sv, vtk, ctk, qt, python
//    paths << app->svHome() + "/" SV_BIN_DIR "/" + app->intDir();
//    paths << app->svHome() + "/" SV_BIN_DIR "/Python";

//    paths << QSettings().value("Python/AdditionalPythonPaths").toStringList();
//    paths << app->svHome() + "/" SV_LIB_DIR;

//#if defined(Q_WS_WIN)
//    QString pythonLibSubDirectory("/Lib");
//    paths << app->svHome() + "/lib/Python" + pythonLibSubDirectory;
//    paths << app->svHome() + "/lib/Python" + pythonLibSubDirectory + "/lib-dynload";
//    paths << app->svHome() + "/lib/Python" + pythonLibSubDirectory + "/lib-tk";
//#elif defined(Q_WS_X11) || defined(Q_WS_MAC)
//    // On unix-like system, setting PYTHONHOME is enough to have the following path automatically
//    // appended to PYTHONPATH: ../lib/pythonX.Y.zip, ../lib/pythonX.Y/,
//    // and ../lib/pythonX.Y/{lib-tk, lib-old, lib-dynload}
//    // See http://docs.python.org/c-api/intro.html#embedding-python
//    QString pythonLibSubDirectory("/lib/python" SV_PYTHON_VERSION_DOT);
//#endif
//    paths << app->svHome() + "/lib/Python" + pythonLibSubDirectory + "/site-packages";

#ifndef SV_NO_HARDCODED_PYTHON_PATHS
    // Add here python path specific to the BUILD tree, for vtk, ctk(including ctk and qt), simpleitk
    paths << VTK_DIR "/lib";
    paths << VTK_DIR "/Wrapping/Python";

    paths << CTK_DIR "/CTK-build/bin";
    paths << CTK_DIR "/CTK-build/bin/Python";

    paths << SimpleITK_DIR "/lib";
    paths << SimpleITK_DIR "/Wrapping";
#endif

    return paths;
}

void svPythonManager::preInitialization()
{
  Superclass::preInitialization();
  this->Factory = new ctkVTKPythonQtWrapperFactory();
  this->addWrapperFactory(this->Factory);

  svApplication* app = svApplication::application();
  if (app)
    {
    // Add object to python interpreter context
    this->addObjectToPythonMain("_svApplicationInstance", app);
    }
}

void svPythonManager::addVTKObjectToPythonMain(const QString& name, vtkObject * object)
{
  // Split name using '.'
  QStringList moduleNameList = name.split('.', QString::SkipEmptyParts);

  // Remove the last part
  QString attributeName = moduleNameList.takeLast();

  bool success = svScriptUtil::setModuleAttribute(
        moduleNameList.join("."),
        attributeName,
        vtkPythonUtil::GetObjectFromPointer(object));
  if (!success)
    {
    qCritical() << "svPythonManager::addVTKObjectToPythonMain - "
                   "Failed to add VTK object:" << name;
    }
}

//  for loadable modules
void svPythonManager::appendPythonPaths(const QStringList& paths)
{
  foreach(const QString& path, paths)
    {
    this->executeString(QString("import sys; sys.path.append('%1'); del sys").arg(path));
    }
}


void svPythonManager::executeInitializationScripts()
{
//  for
//  import vtk, qt, ctk
//  import sv
//  from sv.util import *
//  this->executeFile(app->svHome() + "/bin/Python/sv/sv_init.py");
}
