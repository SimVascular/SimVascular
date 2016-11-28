#include "svMitkMeshIO.h"

#include "svMitkMesh.h"
#include "svMeshTetGen.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLUnstructuredGridWriter.h>

#include <vtkErrorCode.h>

#include <fstream>

static mitk::CustomMimeType CreatesvMeshMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svmesh");
    mimeType.SetCategory("Mesh");
    mimeType.AddExtension("msh");
    mimeType.SetComment("SimVascular Mesh");

    return mimeType;
}

svMitkMeshIO::svMitkMeshIO()
    : mitk::AbstractFileIO(svMitkMesh::GetStaticNameOfClass(), CreatesvMeshMimeType(), "SimVascular Mesh")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> svMitkMeshIO::Read()
{
    TiXmlDocument document;

    std::string fileName=GetInputLocation();

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        std::vector<mitk::BaseData::Pointer> empty;
        return empty;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* mmElement = document.FirstChildElement("mitk_mesh");

    if(!mmElement){
        //        MITK_ERROR << "No Mesh data in "<< fileName;
        mitkThrow() << "No Mesh data in "<< fileName;
    }

    svMitkMesh::Pointer mitkMesh = svMitkMesh::New();
    std::string meshType="";
    std::string modelName="";
    mmElement->QueryStringAttribute("type",&meshType);
    mmElement->QueryStringAttribute("model_name",&modelName);
    mitkMesh->SetType(meshType);
    mitkMesh->SetModelName(modelName);

    int timestep=-1;
    for( TiXmlElement* timestepElement = mmElement->FirstChildElement("timestep");
         timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr)
            continue;

        //        timestepElement->QueryIntAttribute("id",&timestep);
        timestep++;
        mitkMesh->Expand(timestep+1);

        TiXmlElement* meshElement = timestepElement->FirstChildElement("mesh");
        if(meshElement != nullptr)
        {
            std::string type;
            meshElement->QueryStringAttribute("type", &type);

            svMesh* mesh=NULL;

            if(type=="TetGen")
            {
                mesh=new svMeshTetGen();
            }
//            else if(type=="MeshSim")
//            {
//                mesh=new svMeshMeshSim();
//            }

            TiXmlElement* chElement = meshElement->FirstChildElement("command_history");
            if(chElement != nullptr)
            {
                std::vector<std::string> cmdHistory;
                for( TiXmlElement* cmdElement = chElement->FirstChildElement("command");
                     cmdElement != nullptr;
                     cmdElement =cmdElement->NextSiblingElement("command") )
                {
                    if (cmdElement == nullptr)
                        continue;

                    std::string cmd="";
                    cmdElement->QueryStringAttribute("content", &cmd);

                    cmdHistory.push_back(cmd);
                }
                mesh->SetCommandHistory(cmdHistory);
            }

            std::string surfaceFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
            std::ifstream surfaceFile(surfaceFileName);
            if (surfaceFile) {
                vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();

                reader->SetFileName(surfaceFileName.c_str());
                reader->Update();
                vtkSmartPointer<vtkPolyData> surfaceMesh=reader->GetOutput();

                mesh->SetSurfaceMesh(surfaceMesh);
            }

            std::string volumeFileName=fileName.substr(0,fileName.find_last_of("."))+".vtu";
            std::ifstream volumeFile(volumeFileName);
            if (volumeFile) {
                vtkSmartPointer<vtkXMLUnstructuredGridReader> reader = vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();

                reader->SetFileName(volumeFileName.c_str());
                reader->Update();
                vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=reader->GetOutput();

                mesh->SetVolumeMesh(volumeMesh);
            }

            mitkMesh->SetMesh(mesh,timestep);
        } //mesh

    }//timestep

    std::vector<mitk::BaseData::Pointer> result;
    result.push_back(mitkMesh.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel svMitkMeshIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void svMitkMeshIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const svMitkMesh* mitkMesh = dynamic_cast<const svMitkMesh*>(this->GetInput());
    if(!mitkMesh) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  version = new TiXmlElement("format");
    version->SetAttribute("version",  "1.0" );
    document.LinkEndChild(version);

    auto  mmElement = new TiXmlElement("mitk_mesh");
    mmElement->SetAttribute("type", mitkMesh->GetType());
    mmElement->SetAttribute("model_name", mitkMesh->GetModelName());
    document.LinkEndChild(mmElement);

    for(int t=0;t<mitkMesh->GetTimeSize();t++)
    {
        auto  timestepElement = new TiXmlElement("timestep");
        timestepElement->SetAttribute("id",t);
        mmElement->LinkEndChild(timestepElement);

        svMesh* mesh=mitkMesh->GetMesh(t);

        if(!mesh) continue;

        auto meshElement = new TiXmlElement("mesh");
        timestepElement->LinkEndChild(meshElement);
        meshElement->SetAttribute("type",mesh->GetType());

        auto chElement= new TiXmlElement("command_history");
        meshElement->LinkEndChild(chElement);

        std::vector<std::string> cmdHistory=mesh->GetCommandHistory();
        for(int i=0;i<cmdHistory.size();i++)
        {
            auto cmdElement=new TiXmlElement("command");
            chElement->LinkEndChild(cmdElement);
            cmdElement->SetAttribute("content", cmdHistory[i]);
        }

        //Output actual mesh data file
        if(mesh->GetType()=="TetGen")
        {
            if(mesh->GetSurfaceMesh())
            {
                std::string surfaceFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
                vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
                writer->SetFileName(surfaceFileName.c_str());
                writer->SetInputData(mesh->GetSurfaceMesh());
                if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
                {
                    mitkThrow() << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
                }
            }

            if(mesh->GetVolumeMesh())
            {
                std::string volumeFileName=fileName.substr(0,fileName.find_last_of("."))+".vtu";
                vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
                writer->SetFileName(volumeFileName.c_str());
                writer->SetInputData(mesh->GetVolumeMesh());
                if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
                {
                    mitkThrow() << "vtkXMLUnstructuredGridWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
                }
            }
        }

    }

    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel svMitkMeshIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const svMitkMesh* input = dynamic_cast<const svMitkMesh*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

svMitkMeshIO* svMitkMeshIO::IOClone() const
{
    return new svMitkMeshIO(*this);
}

