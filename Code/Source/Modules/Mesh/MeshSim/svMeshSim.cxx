#include "svMeshSim.h"

#include "svStringUtils.h"
#include "svModelUtils.h"

#include <vtkXMLPolyDataWriter.h>
#include <vtkSmartPointer.h>
#include <vtkErrorCode.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLUnstructuredGridReader.h>

#include <iostream>
#include <fstream>

svMeshSim::svMeshSim()
    : m_cvMeshSimMesh(NULL)
{
    m_Type="MeshSim";
}

svMeshSim::svMeshSim(const svMeshSim &other)
    : svMesh(other)
    , m_cvMeshSimMesh(other.m_cvMeshSimMesh)
{
}

svMeshSim::~svMeshSim()
{
    if(m_cvMeshSimMesh!=NULL)
        delete m_cvMeshSimMesh;
}

svMeshSim* svMeshSim::Clone()
{
    return new svMeshSim(*this);
}

void svMeshSim::InitNewMesher()
{
    if(m_cvMeshSimMesh!=NULL)
        delete m_cvMeshSimMesh;

    m_cvMeshSimMesh=new cvMeshSimMeshObject(NULL);
}

bool svMeshSim::SetModelElement(svModelElement* modelElement)
{
    if(!svMesh::SetModelElement(modelElement))
        return false;

    if(m_cvMeshSimMesh==NULL)
        return false;

    if(modelElement==NULL||modelElement->GetInnerSolid()==NULL)
        return false;

    std::string modelType=modelElement->GetType();
    char *mtype = const_cast<char *>(modelType.c_str());
    SolidModel_KernelT kernel= SolidModel_KernelT_StrToEnum( mtype);

    m_cvMeshSimMesh->SetSolidModelKernel(kernel);

    if(m_cvMeshSimMesh->LoadModel(modelElement->GetInnerSolid())!=SV_OK)
        return false;

    return true;
}

bool svMeshSim::Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg)
{
    if(m_cvMeshSimMesh==NULL)
    {
        msg="No mesher created";
        return false;
    }

    if(flag=="")
    {
        msg="Empty Command";
        return true;
    }

    if(option)
    {
        if(flag=="LocalEdgeSize" || flag=="LocalCurvature" || flag=="LocalCurvatureMin" )
        {
            if(m_ModelElement==NULL)
            {
                msg="Model not assigned to the mesher";
                return false;
            }
            std::string idtf=m_ModelElement->GetFaceIdentifierFromInnerSolid(strValues[0]);
            int ident=std::stoi(idtf);

            if(ident<0)
            {
                msg="Identifier not found for face: "+strValues[0];
                return false;
            }
            values[0]=ident;
        }
        char *mflag = const_cast<char *>(flag.c_str());
        if(m_cvMeshSimMesh->SetMeshOptions(mflag, 10, values)!=SV_OK)
        {
            msg="Failed in setting mesh options";
            return false;
        }
    }
    else if(flag=="boundaryLayer")
    {
        if(m_ModelElement==NULL)
        {
            msg="Model not assigned to the mesher";
            return false;
        }
        std::string idtf=m_ModelElement->GetFaceIdentifierFromInnerSolid(strValues[0]);
        int ident=std::stoi(idtf);

        if(ident<0)
        {
            msg="Identifier not found for face: "+strValues[0];
            return false;
        }
        values[0]=ident;

        double H[10]={0};
        for(int i=0;i<values[4];i++)
            H[i]=values[5+i];

        if(m_cvMeshSimMesh->SetBoundaryLayer(values[1], values[0], values[2], values[3], H)!=SV_OK)
        {
            msg="Failed in boudnary layer meshing";
            return false;
        }
    }
    else if(flag=="sphereRefinement")
    {
        double center[3]={values[2],values[3],values[4]};
        if(m_cvMeshSimMesh->SetSphereRefinement(values[0],values[1],center)!=SV_OK)
        {
            msg="Failed in sphere refinement";
            return false;
        }
    }
    else if(flag=="generateMesh")
    {
        if(m_cvMeshSimMesh->GenerateMesh()!=SV_OK)
        {
            msg="Failed in generating mesh";
            return false;
        }
    }
    else if(flag=="writeMesh")
    {
        cvUnstructuredGrid* cvug=m_cvMeshSimMesh->GetUnstructuredGrid();
        if(cvug)
            m_VolumeMesh=cvug->GetVtkUnstructuredGrid();

        cvPolyData* cvpd=m_cvMeshSimMesh->GetPolyData();
        if(cvpd)
            m_SurfaceMesh=cvpd->GetVtkPolyData();

        delete m_cvMeshSimMesh;
        m_cvMeshSimMesh=NULL;
    }
    else if(flag=="logon")
    {
        m_cvMeshSimMesh->Logon(strValues[0].c_str());
    }
    else if(flag=="logoff")
    {
        m_cvMeshSimMesh->Logoff();
    }
    else if(flag=="writeStats")
    {
        m_cvMeshSimMesh->WriteStats(const_cast<char*>(strValues[0].c_str()));
    }
    else
    {
        msg="Unknown command";
        return false;
    }

    msg="Command executed";
    return true;
}

bool svMeshSim::ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg)
{
    option=false;

    std::string originalCmd=cmd;
    cmd=svStringUtils_trim(cmd);

    if(cmd=="")
    {
        flag="";
        msg="Empty command";
        return true;
    }

    std::vector<std::string> params=svStringUtils_split(cmd,' ');

    if(svStringUtils_lower(params[0])=="option")
    {
        params.erase(params.begin());
    }

    int paramSize=params.size();
    if(paramSize==0)
    {
        msg="Command not complete";
        return false;
    }

    params[0]=svStringUtils_lower(params[0]);

    try{
        if(paramSize==2 && params[0]=="logon")
        {
            flag="logon";
            strValues[0]=params[1];
        }
        else if(params[0]=="logoff")
        {
            flag="logoff";
        }
        else if(params[0]=="newmesh")
        {
            flag="newMesh";
        }
        else if(paramSize==2 && (params[0]=="surfacemesh" || params[0]=="surface"))
        {
            flag="SurfaceMeshFlag";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="surface" && params[1]=="optimization")
        {
            flag="SurfaceOptimization";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="surface" && params[1]=="smoothing")
        {
            flag="SurfaceSmoothing";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==2 && (params[0]=="volumemesh" || params[0]=="volume"))
        {
            flag="VolumeMeshFlag";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="volume" && params[1]=="optimization")
        {
            flag="VolumeOptimization";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="volume" && params[1]=="smoothing")
        {
            flag="VolumeSmoothing";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && (params[0]=="globaledgesize" || params[0]=="gsize"))
        {
            flag="GlobalEdgeSize";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && (params[0]=="globalcurvature" || params[0]=="gcurv"))
        {
            flag="GlobalCurvature";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && (params[0]=="globalcurvaturemin" || params[0]=="gmincurv"))
        {
            flag="GlobalCurvatureMin";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==4 && (params[0]=="size" || params[0]=="localedgesize" || params[0]=="localsize"))
        {
            flag="LocalEdgeSize";
            strValues[0]=params[1];
            //values[0] will be set at Execute
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            option=true;
        }
        else if(paramSize==4 && (params[0]=="curv" || params[0]=="localcurvature" ))
        {
            flag="LocalCurvature";
            strValues[0]=params[1];
            //values[0] will be set at Execute
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            option=true;
        }
        else if(paramSize==4 && (params[0]=="mincurv" || params[0]=="localcurvaturemin" ))
        {
            flag="LocalCurvatureMin";
            strValues[0]=params[1];
            //values[0] will be set at Execute
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            option=true;
        }
        else if(paramSize>=5 && params[0]=="boundarylayer")
        {
            flag="boundaryLayer";
            strValues[0]=params[1];
            //values[0] will be set at Execute
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            values[3]=std::stod(params[4]);
            int count=paramSize-5;
            values[4]=count;
            for(int i=0;i<count;i++)
                values[5+i]=std::stod(params[5+i]);
        }
        else if(paramSize==6 && params[0]=="sphererefinement")
        {
            flag="sphereRefinement";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            values[3]=std::stod(params[4]);
            values[4]=std::stod(params[5]);
        }
        else if(params[0]=="generatemesh")
        {
            flag="generateMesh";
        }
        else if(params[0]=="writemesh")
        {
            flag="writeMesh";
        }
        else if(params[0]=="writestats")
        {
            flag="writeStats";
        }
        else
        {
            msg="Unknown command: "+originalCmd;
            return false;
        }

    }catch (...){

        msg="Command values not right: "+originalCmd;
        return false;
    }

    msg="Command parsed";
    return true;

}

cvMeshSimMeshObject* svMeshSim::GetMesher()
{
    return m_cvMeshSimMesh;
}

svMesh* svMeshSim::CreateMesh()
{
    return new svMeshSim();
}

bool svMeshSim::WriteSurfaceFile(std::string filePath)
{
    if(m_SurfaceMesh)
    {
        vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
        writer->SetFileName(filePath.c_str());
        writer->SetInputData(m_SurfaceMesh);
        if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
        {
            std::cerr << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode())<<std::endl;
            return false;
        }
    }

    return true;
}

bool svMeshSim::WriteVolumeFile(std::string filePath)
{
    if(m_VolumeMesh)
    {
        vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
        writer->SetFileName(filePath.c_str());
        writer->SetInputData(m_VolumeMesh);
        if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
        {
            std::cerr << "vtkXMLUnstructuredGridWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode())<<std::endl;
            return false;
        }
    }

    return true;
}

bool svMeshSim::ReadSurfaceFile(std::string filePath)
{
    m_SurfaceMesh=CreateSurfaceMeshFromFile(filePath);

    return true;
}

bool svMeshSim::ReadVolumeFile(std::string filePath)
{
    m_VolumeMesh=CreateVolumeMeshFromFile(filePath);

    return true;
}

vtkSmartPointer<vtkPolyData> svMeshSim::CreateSurfaceMeshFromFile(std::string filePath)
{
    vtkSmartPointer<vtkPolyData> surfaceMesh=NULL;
    std::ifstream surfaceFile(filePath);
    if (surfaceFile) {
        vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();

        reader->SetFileName(filePath.c_str());
        reader->Update();
        surfaceMesh=reader->GetOutput();
    }

    return surfaceMesh;
}

vtkSmartPointer<vtkUnstructuredGrid> svMeshSim::CreateVolumeMeshFromFile(std::string filePath)
{
    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=NULL;
    std::ifstream volumeFile(filePath);
    if (volumeFile) {
        vtkSmartPointer<vtkXMLUnstructuredGridReader> reader = vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();

        reader->SetFileName(filePath.c_str());
        reader->Update();
        volumeMesh=reader->GetOutput();
    }

    return volumeMesh;
}

//bool svMeshSim::WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh
//                                     , vtkSmartPointer<vtkUnstructuredGrid> volumeMesh
//                                     , svModelElement* modelElement
//                                     , std::string meshDir)
//{

//}

//bool svMeshSim::WriteMeshComplete(std::string meshDir)
//{

//}
