#ifndef SVMESHFACTORY_H
#define SVMESHFACTORY_H

#include <svMeshExports.h>

#include "svMesh.h";
#include "svMeshAdaptor.h"
#include <map>

class SVMESH_EXPORT svMeshFactory
{

public:

    typedef svMesh* (*MeshCreationFunction)();

    typedef svMeshAdaptor* (*AdaptorCreationFunction)();

    static void RegisterCreationFunction(std::string type, MeshCreationFunction function);

    static void RegisterAdaptorFunction(std::string type, AdaptorCreationFunction function);

    static svMesh* CreateMesh(std::string type);

    static svMeshAdaptor* CreateAdaptor(std::string type);

    static std::vector<std::string> GetAvailableTypes();

    static bool IsTypeAvailable(std::string type);

    static void RegisterFileExtensions(std::string type, std::vector<std::string> fileExtensions);

    static std::vector<std::string> GetFileExtensions(std::string type);

    static std::string GetType(std::string fileExtension);

  protected:

    static std::map<std::string, MeshCreationFunction> m_FunctionMap;

    static std::map<std::string, std::vector<std::string>> m_FileExtensionMap;

    static std::map<std::string, AdaptorCreationFunction> m_AdaptorFunctionMap;
  };


#endif // SVMESHFACTORY_H
