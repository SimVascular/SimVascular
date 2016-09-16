#ifndef SVPYTHONMANAGER_H
#define SVPYTHONMANAGER_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

# include <ctkAbstractPythonManager.h>

//class ctkVTKPythonQtWrapperFactory;
#include <ctkVTKPythonQtWrapperFactory.h>
class PythonQtObjectPtr;
class vtkObject;

class SVQTAPPBASE_EXPORT svPythonManager : public ctkAbstractPythonManager
{
    Q_OBJECT

public:
    typedef ctkAbstractPythonManager Superclass;
    svPythonManager(QObject* parent=0);
    ~svPythonManager();

    void addVTKObjectToPythonMain(const QString& name, vtkObject * object);

    void appendPythonPath(const QString& path);

    void appendPythonPaths(const QStringList& path);

    void initPythonQtAll();


protected:
    QStringList pythonPaths();

    void preInitialization();

    void executeInitializationScripts();

    ctkVTKPythonQtWrapperFactory* Factory;
};

#endif // SVPYTHONMANAGER_H
