/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_MeshFactory.h"

std::map<std::string, sv4guiMeshFactory::MeshCreationFunction> sv4guiMeshFactory::m_FunctionMap;

std::map<std::string, std::vector<std::string>> sv4guiMeshFactory::m_FileExtensionMap;

std::map<std::string, sv4guiMeshFactory::AdaptorCreationFunction> sv4guiMeshFactory::m_AdaptorFunctionMap;

void sv4guiMeshFactory::RegisterCreationFunction(std::string type, MeshCreationFunction function)
{
    auto search=m_FunctionMap.find(type);
    if(search==m_FunctionMap.end())
        m_FunctionMap[type]=function;
}

void sv4guiMeshFactory::RegisterAdaptorFunction(std::string type, AdaptorCreationFunction function)
{
    auto search=m_AdaptorFunctionMap.find(type);
    if(search==m_AdaptorFunctionMap.end())
        m_AdaptorFunctionMap[type]=function;
}

sv4guiMesh* sv4guiMeshFactory::CreateMesh(std::string type)
{
    sv4guiMesh* mesh=NULL;

    auto search=m_FunctionMap.find(type);
    if(search!=m_FunctionMap.end())
        mesh=m_FunctionMap[type]();

    return mesh;
}

sv4guiMeshAdaptor* sv4guiMeshFactory::CreateAdaptor(std::string type)
{
    sv4guiMeshAdaptor* adaptor=NULL;

    auto search=m_AdaptorFunctionMap.find(type);
    if(search!=m_AdaptorFunctionMap.end())
        adaptor=m_AdaptorFunctionMap[type]();

    return adaptor;
}

std::vector<std::string> sv4guiMeshFactory::GetAvailableTypes()
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

bool sv4guiMeshFactory::IsTypeAvailable(std::string type)
{
    auto search=m_FunctionMap.find(type);
    if(search!=m_FunctionMap.end())
        return true;
    else
        return false;
}

void sv4guiMeshFactory::RegisterFileExtensions(std::string type, std::vector<std::string> fileExtensions)
{
    auto search=m_FileExtensionMap.find(type);
    if(search==m_FileExtensionMap.end())
        m_FileExtensionMap[type]=fileExtensions;
}

std::vector<std::string> sv4guiMeshFactory::GetFileExtensions(std::string type)
{
    std::vector<std::string> exts;

    auto search=m_FileExtensionMap.find(type);
    if(search!=m_FileExtensionMap.end())
        exts=m_FileExtensionMap[type];

    return exts;
}

std::string sv4guiMeshFactory::GetType(std::string fileExtension)
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
