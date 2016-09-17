#ifndef SVSCRIPTUTIL_H
#define SVSCRIPTUTIL_H

#include "SimVascular.h"
#include <svQtAppBaseExports.h>

// Qt includes
#include <QString>

// PythonQt includes
#include <PythonQtObjectPtr.h>


class  SVQTAPPBASE_EXPORT svScriptUtil
{
public:
    typedef svScriptUtil Self;

    /// \brief Set the value of the attribute named \a attributeName, for module
    /// named \a moduleName, to the value \a attributeValue.
    ///
    /// If \a moduleName is empty, attribute will be set for module `__main__`.
    ///
    /// If \a moduleName is a dotted name, attribute will be set the last module.
    static bool setModuleAttribute(const QString& moduleName,
                                   const QString& attributeName,
                                   PyObject* attributeValue);


    svScriptUtil();
    ~svScriptUtil();
};

#endif // SVSCRIPTUTIL_H
