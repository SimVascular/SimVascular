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

#include "sv4gui_MitkMeshIO.h"

#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshFactory.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include "sv4gui_ContourGroupIO.h"
auto set_string_from_attribute = &sv4guiContourGroupIO::set_string_from_attribute;

#include <tinyxml2.h>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLUnstructuredGridWriter.h>

#include <vtkErrorCode.h>

#include <fstream>

static mitk::CustomMimeType Createsv4guiMeshMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svmesh");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("msh");
    mimeType.SetComment("SimVascular Mesh");

    return mimeType;
}

sv4guiMitkMeshIO::sv4guiMitkMeshIO()
    : mitk::AbstractFileIO(sv4guiMitkMesh::GetStaticNameOfClass(), Createsv4guiMeshMimeType(), "SimVascular Mesh")
//    , m_ReadMeshData(false)
{
    this->RegisterService();
//    m_Singleton=this;
}

//void sv4guiMitkMeshIO::SetReadMeshData(bool read)
//{
//    m_ReadMeshData=read;
//}

//sv4guiMitkMeshIO* sv4guiMitkMeshIO::m_Singleton = nullptr;

//sv4guiMitkMeshIO* sv4guiMitkMeshIO::GetSingleton()
//{
//    return m_Singleton;
//}

std::vector<mitk::BaseData::Pointer> sv4guiMitkMeshIO::Read()
{
    std::string fileName=GetInputLocation();
    sv4guiMitkMesh::Pointer mitkMesh=ReadFromFile(fileName,false,false);

    std::vector<mitk::BaseData::Pointer> result;
    if (mitkMesh.IsNull())
    {
        return result;
    }

    result.push_back(mitkMesh.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkMeshIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiMitkMeshIO::Write()
{
    ValidateOutputLocation();
    std::string fileName = GetOutputLocation();
    //const sv4guiMitkMesh* mitkMesh = dynamic_cast<const sv4guiMitkMesh*>(this->GetInput());
    auto mitkMesh = dynamic_cast<const sv4guiMitkMesh*>(this->GetInput());

    if (!mitkMesh) {
       return;
    }

    WriteGroupToFile(const_cast<sv4guiMitkMesh*>(mitkMesh), fileName);
}

//------------------
// WriteGroupToFile
//------------------
// Write a mesh group to a .msh file.
//
void sv4guiMitkMeshIO::WriteGroupToFile(sv4guiMitkMesh* mitkMesh, std::string& fileName)
{
    tinyxml2::XMLDocument document;
    auto  decl = document.NewDeclaration();
    document.LinkEndChild( decl );

    auto mmElement =document.NewElement("mitk_mesh");
    mmElement->SetAttribute("type", mitkMesh->GetType().c_str());
    mmElement->SetAttribute("model_name", mitkMesh->GetModelName().c_str());
    mmElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(mmElement);

    for(int t=0;t<mitkMesh->GetTimeSize();t++)
    {
        auto  timestepElement =document.NewElement("timestep");
        timestepElement->SetAttribute("id",t);
        mmElement->LinkEndChild(timestepElement);

        sv4guiMesh* mesh=mitkMesh->GetMesh(t);

        if(!mesh) continue;

        auto meshElement =document.NewElement("mesh");
        timestepElement->LinkEndChild(meshElement);
        meshElement->SetAttribute("type",mesh->GetType().c_str());

        auto chElement=document.NewElement("command_history");
        meshElement->LinkEndChild(chElement);

        std::vector<std::string> cmdHistory=mesh->GetCommandHistory();
        for(int i=0;i<cmdHistory.size();i++)
        {
            auto cmdElement=document.NewElement("command");
            chElement->LinkEndChild(cmdElement);
            cmdElement->SetAttribute("content", cmdHistory[i].c_str());
        }

        std::string surfaceFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
        if(!mesh->WriteSurfaceFile(surfaceFileName))
            mitkThrow() << "Error in writing surface mesh to file: " << surfaceFileName;

        std::string volumeFileName=fileName.substr(0,fileName.find_last_of("."))+".vtu";
        if(!mesh->WriteVolumeFile(volumeFileName))
            mitkThrow() << "Error in writing surface mesh to file: " << surfaceFileName;

    }

    if (document.SaveFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        mitkThrow() << "Could not write Mesh parameter file to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkMeshIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiMitkMesh* input = dynamic_cast<const sv4guiMitkMesh*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiMitkMeshIO* sv4guiMitkMeshIO::IOClone() const
{
    return new sv4guiMitkMeshIO(*this);
}

sv4guiMitkMesh::Pointer sv4guiMitkMeshIO::ReadFromFile(std::string fileName, bool readSurfaceMesh, bool readVolumeMesh)
{
    //std::string msg("sv4guiMitkMeshIO::ReadFromFile");
    //std::cout << msg << "=================== sv4guiMitkMeshIO::ReadFromFile =========" << std::endl;
    //std::cout << msg << "fileName: " << fileName << std::endl;

    tinyxml2::XMLDocument document;

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
//        mitkThrow() << "Could not open/read/parse " << fileName;
        MITK_ERROR << "Could not open/read/parse " << fileName;
        return nullptr;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    auto mmElement = document.FirstChildElement("mitk_mesh");

    if(!mmElement){
        MITK_ERROR << "No Mesh data in "<< fileName;
//        mitkThrow() << "No Mesh data in "<< fileName;
        return nullptr;
    }

    sv4guiMitkMesh::Pointer mitkMesh = sv4guiMitkMesh::New();
    std::string meshType="";
    std::string modelName="";

    set_string_from_attribute(mmElement,"type",meshType);
    set_string_from_attribute(mmElement,"model_name",modelName);
    mitkMesh->SetType(meshType);
    mitkMesh->SetModelName(modelName);

    int timestep=-1;
    for( auto timestepElement = mmElement->FirstChildElement("timestep");
         timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr)
            continue;

        //        timestepElement->QueryIntAttribute("id",&timestep);
        timestep++;
        mitkMesh->Expand(timestep+1);

        auto meshElement = timestepElement->FirstChildElement("mesh");
//        if(meshElement != nullptr && m_ReadMeshData)
        if(meshElement != nullptr)
        {
            std::string type;
            set_string_from_attribute(meshElement, "type", type);

            sv4guiMesh* mesh=sv4guiMeshFactory::CreateMesh(type);
            if(mesh==nullptr)
            {
                MITK_ERROR << "No mesh constructor for "<< type;
        //        mitkThrow() << "No mesh constructor for "<< type;
                return nullptr;
            }

            auto chElement = meshElement->FirstChildElement("command_history");
            if(chElement != nullptr)
            {
                std::vector<std::string> cmdHistory;
                for( auto cmdElement = chElement->FirstChildElement("command");
                     cmdElement != nullptr;
                     cmdElement =cmdElement->NextSiblingElement("command") )
                {
                    if (cmdElement == nullptr)
                        continue;

                    std::string cmd="";
                    set_string_from_attribute(cmdElement, "content", cmd);
                    cmdHistory.push_back(cmd);
                }
                mesh->SetCommandHistory(cmdHistory);
            }

            if(readSurfaceMesh)
            {
                std::string surfaceFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
                mesh->ReadSurfaceFile(surfaceFileName);
            }

            if(readVolumeMesh)
            {
                std::string volumeFileName=fileName.substr(0,fileName.find_last_of("."))+".vtu";
                mesh->ReadVolumeFile(volumeFileName);
            }

            mitkMesh->SetMesh(mesh,timestep);
        } //mesh

    }//timestep

    return mitkMesh;
}
