#include "svMeshFactory.h"

std::map<std::string, svMeshFactory::MeshCreationFunction> svMeshFactory::m_FunctionMap;

std::map<std::string, std::vector<std::string>> svMeshFactory::m_FileExtensionMap;

std::map<std::string, svMeshFactory::AdaptorCreationFunction> svMeshFactory::m_AdaptorFunctionMap;

void svMeshFactory::RegisterCreationFunction(std::string type, MeshCreationFunction function)
{
    auto search=m_FunctionMap.find(type);
    if(search==m_FunctionMap.end())
        m_FunctionMap[type]=function;
}

void svMeshFactory::RegisterAdaptorFunction(std::string type, AdaptorCreationFunction function)
{
    auto search=m_AdaptorFunctionMap.find(type);
    if(search==m_AdaptorFunctionMap.end())
        m_AdaptorFunctionMap[type]=function;
}

svMesh* svMeshFactory::CreateMesh(std::string type)
{
    svMesh* mesh=NULL;

    auto search=m_FunctionMap.find(type);
    if(search!=m_FunctionMap.end())
        mesh=m_FunctionMap[type]();

    return mesh;
}

svMeshAdaptor* svMeshFactory::CreateAdaptor(std::string type)
{
    svMeshAdaptor* adaptor=NULL;

    auto search=m_AdaptorFunctionMap.find(type);
    if(search!=m_AdaptorFunctionMap.end())
        adaptor=m_AdaptorFunctionMap[type]();

    return adaptor;
}

std::vector<std::string> svMeshFactory::GetAvailableTypes()
{
    std::vector<std::string> types;
    auto it=m_FunctionMap.begin();
    while(it!=m_FunctionMap.end())
    {
        if(it->first!="")
            types.push_back(it->first);

        it++;
    }

    return types;
}

bool svMeshFactory::IsTypeAvailable(std::string type)
{
    auto search=m_FunctionMap.find(type);
    if(search!=m_FunctionMap.end())
        return true;
    else
        return false;
}

void svMeshFactory::RegisterFileExtensions(std::string type, std::vector<std::string> fileExtensions)
{
    auto search=m_FileExtensionMap.find(type);
    if(search==m_FileExtensionMap.end())
        m_FileExtensionMap[type]=fileExtensions;
}

std::vector<std::string> svMeshFactory::GetFileExtensions(std::string type)
{
    std::vector<std::string> exts;

    auto search=m_FileExtensionMap.find(type);
    if(search!=m_FileExtensionMap.end())
        exts=m_FileExtensionMap[type];

    return exts;
}

std::string svMeshFactory::GetType(std::string fileExtension)
{
    auto it=m_FileExtensionMap.begin();
    while(it!=m_FileExtensionMap.end())
    {
        if(it->first!="")
        {
            auto exts=it->second;
            for(int i=0;i<exts.size();i++)
                if(fileExtension==exts[i])
                    return it->first;
       }

        it++;
    }

    return "";
}
