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

    static void RegisterFileExtensions(std::string type, std::vector<std::string> fileExtensions);

    static std::vector<std::string> GetFileExtensions(std::string type);

    static std::string GetType(std::string fileExtension);

  protected:

    static std::map<std::string, ModelElementCreationFunction> m_FunctionMap;

    static std::map<std::string, std::vector<std::string>> m_FileExtensionMap;
  };


#endif // SVMODELELEMENTFACTORY_H
