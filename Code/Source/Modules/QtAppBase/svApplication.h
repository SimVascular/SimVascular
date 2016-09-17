#ifndef SVAPPLICATION_H
#define SVAPPLICATION_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svPythonManager.h"
#include "svQmitkDataManager.h"
#include "svRenderWindowPart.h"
#include "svExtensionManager.h"

#include <mitkDataStorage.h>
#include <mitkStandaloneDataStorage.h>
#include <QmitkStdMultiWidget.h>

// Qt includes
#include <QApplication>
#include <QMetaType>
#include <QVariant>
#include <QProcessEnvironment>

// CTK includes
//#include <ctkVTKObject.h>

//class svPythonManager;

class SVQTAPPBASE_EXPORT svApplication : public QApplication
{
    Q_OBJECT
//    QVTK_OBJECT

//    Q_PROPERTY(QString svHome READ svHome CONSTANT)

public:

    typedef QApplication Superclass;
    svApplication(int &argc, char **argv);
    ~svApplication();

    QProcessEnvironment  Environment;

    void setPythonEnvironmentVariables();
    void setPythonOsEnviron(const QString& key, const QString& value);
    void setEnvironmentVariable(const QString& key, const QString& value);

    static svApplication* application();

//    svPythonManager* mPythonManager;
    svPythonManager* pythonManager() const;
//    void setPythonManager (svPythonManager* pythonManager);

    mitk::DataStorage::Pointer dataStorage() const;
    mitk::StandaloneDataStorage::Pointer standaloneDataStorage();
    svQmitkDataManager* dataManager() const;
    void setDataManager(svQmitkDataManager* dataManager);

    QmitkStdMultiWidget* displayWidget() const;
    void setDisplayWidget(QmitkStdMultiWidget* displayWidget);

    svRenderWindowPart* renderWindowPart() const;

    svExtensionManager* extensionManager();
    void setExtensionManager(svExtensionManager* extensionManager);

protected:

    svPythonManager* mPythonManager;
    mitk::DataStorage::Pointer mDataStorage;
    mitk::StandaloneDataStorage::Pointer mStandaloneDataStorage;
    svQmitkDataManager* mDataManager;
    QmitkStdMultiWidget* mDisplayWidget;

    svRenderWindowPart* mRenderWindowPart;
    svExtensionManager* mExtensionManager;


protected slots:
    void terminate(int exitCode = EXIT_SUCCESS);



};

#endif // SVAPPLICATION_H
