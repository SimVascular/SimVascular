#include "svModelElementFactory.h"

std::map<std::string, svModelElementFactory::ModelElementCreationFunction> svModelElementFactory::m_FunctionMap;

std::map<std::string, std::string> svModelElementFactory::m_FileExtensionMap;

void svModelElementFactory::RegisterCreationFunction(std::string type, ModelElementCreationFunction function)
{
    auto search=m_FunctionMap.find(type);
    if(search==m_FunctionMap.end())
        m_FunctionMap[type]=function;
}

svModelElement* svModelElementFactory::CreateModelElement(std::string type)
{
    svModelElement* me=NULL;

    auto search=m_FunctionMap.find(type);
    if(search!=m_FunctionMap.end())
        me=m_FunctionMap[type]();

    return me;
}

std::vector<std::string> svModelElementFactory::GetAvailableTypes()
{
    std::vector<std::string> types;
    auto it=m_FunctionMap.begin();
    while(it!=m_FunctionMap.end())
    {
        if(it->first!="")
            types.push_back(it->first);
    }

    return types;
}

bool svModelElementFactory::IsTypeAvailable(std::string type)
{
    auto search=m_FunctionMap.find(type);
    if(search!=m_FunctionMap.end())
        return true;
    else
        return false;
}

void svModelElementFactory::RegisterFileExtension(std::string type, std::string fileExtension)
{
    auto search=m_FileExtensionMap.find(type);
    if(search==m_FileExtensionMap.end())
        m_FileExtensionMap[type]=fileExtension;
}

std::string svModelElementFactory::GetFileExtension(std::string type)
{
    std::string extension="";

    auto search=m_FileExtensionMap.find(type);
    if(search!=m_FileExtensionMap.end())
        extension=m_FileExtensionMap[type];

    return extension;
}
