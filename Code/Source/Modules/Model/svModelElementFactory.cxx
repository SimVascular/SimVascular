#include "svModelElementFactory.h"

std::map<std::string, svModelElementFactory::ModelElementCreationFunction> svModelElementFactory::m_FunctionMap;

std::map<std::string, std::vector<std::string>> svModelElementFactory::m_FileExtensionMap;

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

        it++;
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

void svModelElementFactory::RegisterFileExtensions(std::string type, std::vector<std::string> fileExtensions)
{
    auto search=m_FileExtensionMap.find(type);
    if(search==m_FileExtensionMap.end())
        m_FileExtensionMap[type]=fileExtensions;
}

std::vector<std::string> svModelElementFactory::GetFileExtensions(std::string type)
{
    std::vector<std::string> exts;

    auto search=m_FileExtensionMap.find(type);
    if(search!=m_FileExtensionMap.end())
        exts=m_FileExtensionMap[type];

    return exts;
}

std::string svModelElementFactory::GetType(std::string fileExtension)
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
