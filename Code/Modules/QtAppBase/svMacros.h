#ifndef SVMACROS_H
#define SVMACROS_H

#include "SimVascular.h"

#include "svExtensionRegistry.h"

class QString;

#define SV_REGISTER_EXTENSION_CLASS(_ClassType)                         \
{                                                                          \
   QString typeName = _ClassType::staticMetaObject.className();            \
   ::registerExtensionType<_ClassType>(typeName.toLatin1().data()); \
}

#endif // SVMACROS_H
