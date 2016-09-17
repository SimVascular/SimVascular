#include "svScriptUtil.h"

// PythonQt includes
#include <PythonQt.h>

bool svScriptUtil::setModuleAttribute(const QString& moduleName,
                                              const QString& attributeName,
                                              PyObject* attributeValue)
{
  if (!attributeValue)
    {
    return false;
    }

  // Import module
  PyObject * module = PythonQt::self()->getMainModule();
  if (!moduleName.isEmpty())
    {
    module = PyImport_ImportModule(moduleName.toLatin1());
    if (!module)
      {
      PythonQt::self()->handleError();
      return false;
      }
    }

  // Add the object to the imported module
  int ret = PyObject_SetAttrString(module, attributeName.toLatin1(), attributeValue);
  if (ret != 0)
    {
    PythonQt::self()->handleError();
    return false;
    }
  return true;
}

svScriptUtil::svScriptUtil()
{

}

svScriptUtil::~svScriptUtil()
{

}

