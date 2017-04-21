#ifndef SVMODELELEMENTFACTORY_H
#define SVMODELELEMENTFACTORY_H

#include <svModelExports.h>

#include "svModelElement.h";
#include <map>

class SVMODEL_EXPORT svModelElementFactory
{

public:

    typedef svModelElement* (*ModelElementCreationFunction)();

    static void RegisterCreationFunction(std::string type, ModelElementCreationFunction function);

    static svModelElement* CreateModelElement(std::string type);

    static std::vector<std::string> GetAvailableTypes();

    static bool IsTypeAvailable(std::string type);

  protected:

    static std::map<std::string, ModelElementCreationFunction> m_FunctionMap;

  };


#endif // SVMODELELEMENTFACTORY_H
