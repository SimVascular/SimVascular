#include "svMeshSim.h"

#include "svStringUtils.h"
#include "svModelUtils.h"
#include "svMeshSimAdaptor.h"

//#include "cvSolidModel.h"
#include <vtkXMLPolyDataWriter.h>
#include <vtkSmartPointer.h>
#include <vtkErrorCode.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLUnstructuredGridReader.h>

#include <iostream>
#include <fstream>

svMeshSim::svMeshSim()
    : m_cvTetGetMesh(NULL)
{
    m_Type="TetGen";
    //m_cvTetGetMesh=new cvTetGenMeshObject(NULL);
}

svMeshSim::svMeshSim(const svMeshSim &other)
    : svMesh(other)
    , m_cvTetGetMesh(other.m_cvTetGetMesh)
{
}

svMeshSim::~svMeshSim()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;
}

svMeshSim* svMeshSim::Clone()
{
    return new svMeshSim(*this);
}

void svMeshSim::InitNewMesher()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;

    m_cvTetGetMesh=new cvTetGenMeshObject(NULL);
}

bool svMeshSim::SetModelElement(svModelElement* modelElement)
{
    if(!svMesh::SetModelElement(modelElement))
        return false;

    if(m_cvTetGetMesh==NULL)
        return false;

    if(modelElement==NULL||modelElement->GetWholeVtkPolyData()==NULL)
        return false;

    std::string modelType=modelElement->GetType();
    char *mtype = const_cast<char *>(modelType.c_str());
    SolidModel_KernelT kernel= SolidModel_KernelT_StrToEnum( mtype);

    m_cvTetGetMesh->SetSolidModelKernel(kernel);

    vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->SetInputData(modelElement->GetWholeVtkPolyData());
    cleaner->Update();

    vtkSmartPointer<vtkPolyData> vtkpd=vtkSmartPointer<vtkPolyData>::New();
    vtkpd->DeepCopy(cleaner->GetOutput());
    vtkpd->BuildLinks();

    if(m_cvTetGetMesh->LoadModel(vtkpd)!=SV_OK)
        return false;

//    //set walls
//    //m_cvTetGetMesh->SetMeshOptions("MeshWallFirst",0,NULL) //not nessary, because in "SetWalls" it'll be set at 1 again.
//    std::vector<int> wallFaceIDs=modelElement->GetWallFaceIDs();
//    if(m_cvTetGetMesh->SetWalls(wallFaceIDs.size(),&wallFaceIDs[0])!=SV_OK)
//        return false;

    return true;
}

bool svMeshSim::Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg)
{
    if(m_cvTetGetMesh==NULL)
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
        if(flag=="LocalEdgeSize")
        {
            if(m_ModelElement==NULL)
            {
                msg="Model not assigned to the mesher";
                return false;
            }
            int faceID=m_ModelElement->GetFaceID(strValues[0]);
            if(faceID<0)
            {
                msg="ID not found for face: "+strValues[0];
                return false;
            }
            double egdeSize=values[0];
            values[0]=faceID;
            values[1]=egdeSize;
        }
        char *mflag = const_cast<char *>(flag.c_str());
        if(m_cvTetGetMesh->SetMeshOptions(mflag, 10, values)!=SV_OK)
        {
            msg="Failed in setting walls";
            return false;
        }
    }
    else if(flag=="setWalls")
    {
        if(m_ModelElement==NULL)
        {
            msg="Model not assigned to the mesher";
            return false;
        }

        std::vector<int> wallFaceIDs=m_ModelElement->GetWallFaceIDs();
        if(m_cvTetGetMesh->SetWalls(wallFaceIDs.size(),&wallFaceIDs[0])!=SV_OK)
        {
            msg="Failed ot set walls";
            return false;
        }

    }
    else if(flag=="useCenterlineRadius")
    {
        cvPolyData* solid=m_cvTetGetMesh->GetSolid();
        if(solid==NULL)
        {
            msg="No mesher model";
            return false;
        }
        vtkPolyData* centerlines=svModelUtils::CreateCenterlines(solid->GetVtkPolyData());
        vtkPolyData* distance=svModelUtils::CalculateDistanceToCenterlines(centerlines, solid->GetVtkPolyData());
        if(distance==NULL)
        {
            msg="Failed in calculating distance to centerlines";
            return false;
        }

        //delete centerlines;
        //delete solid;

        m_cvTetGetMesh->SetVtkPolyDataObject(distance);
    }
    else if(flag=="functionBasedMeshing")
    {
        char *strv = const_cast<char *>(strValues[0].c_str());
        if(m_cvTetGetMesh->SetSizeFunctionBasedMesh(values[0],strv)!=SV_OK)
        {
            msg="Failed in function based meshing, such as radius based";
            return false;
        }
    }
    else if(flag=="boundaryLayer")
    {
        double H[2]={values[1],values[2]};
        if(m_cvTetGetMesh->SetBoundaryLayer(0, 0, 0, values[0], H)!=SV_OK)
        {
            msg="Failed in boudnary layer meshing";
            return false;
        }
    }
    else if(flag=="sphereRefinement")
    {
        double center[3]={values[2],values[3],values[4]};
        if(m_cvTetGetMesh->SetSphereRefinement(values[0],values[1],center)!=SV_OK)
        {
            msg="Failed in sphere refinement";
            return false;
        }
    }
    else if(flag=="generateMesh")
    {
        if(m_cvTetGetMesh->GenerateMesh()!=SV_OK)
        {
            msg="Failed in generating mesh";
            return false;
        }
    }
    else if(flag=="getBoundaries")
    {
        if(m_cvTetGetMesh->GetBoundaryFaces(50)!=SV_OK)
        {
            msg="Failed in getting boundary after boundary layer meshing";
            return false;
        }
    }
    else if(flag=="writeMesh")
    {
        if(m_cvTetGetMesh->WriteMesh(NULL,0)==SV_OK)
        {
            vtkPolyData* surfaceMesh=m_cvTetGetMesh->GetPolyData()->GetVtkPolyData();
            vtkUnstructuredGrid* volumeMesh=m_cvTetGetMesh->GetUnstructuredGrid()->GetVtkUnstructuredGrid();
            if(surfaceMesh==NULL)
            {
                delete m_cvTetGetMesh;
                m_cvTetGetMesh=NULL;
                msg="Empty mesh created";
                return false;
            }
            m_SurfaceMesh=vtkSmartPointer<vtkPolyData>::New();
            m_SurfaceMesh->DeepCopy(surfaceMesh);
            m_VolumeMesh=vtkSmartPointer<vtkUnstructuredGrid>::New();
            m_VolumeMesh->DeepCopy(volumeMesh);
            delete m_cvTetGetMesh;
            m_cvTetGetMesh=NULL;
        }
        else
        {
            delete m_cvTetGetMesh;
            m_cvTetGetMesh=NULL;
            msg="Failed in writing meshing";
            return false;
        }
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

        if(paramSize==2 && (params[0]=="surfacemesh" || params[0]=="surface"))
        {
            flag="SurfaceMeshFlag";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==2 && (params[0]=="volumemesh" || params[0]=="volume"))
        {
            flag="VolumeMeshFlag";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==2 && params[0]=="usemmg")
        {
            flag="UseMMG";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(params[0]=="setwalls")
        {
            flag="setWalls";
        }
        else if(paramSize==2 && (params[0]=="globaledgesize" || params[0]=="gsize" || params[0]=="a"))
        {
            flag="GlobalEdgeSize";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(params[0]=="usecenterlineradius")
        {
            flag="useCenterlineRadius";
        }
        else if(paramSize==3 && params[0]=="functionbasedmeshing")
        {
            flag="functionBasedMeshing";
            values[0]=std::stod(params[1]);
            strValues[0]=params[2];
        }
        else if(paramSize==4 && params[0]=="boundarylayer")
        {
            flag="boundaryLayer";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
        }
        else if(paramSize==3 && (params[0]=="localedgesize" || params[0]=="localsize"))
        {
            flag="LocalEdgeSize";
            values[0]=std::stod(params[2]);
            strValues[0]=params[1];
            option=true;
        }
        else if(paramSize==4 && params[0]=="localsize")
        {
            flag="LocalEdgeSize";
            values[0]=std::stod(params[3]);
            strValues[0]=params[1];
            option=true;
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
        else if(paramSize==2 && params[0]=="optimization")
        {
            flag="Optimization";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==2 && params[0]=="qualityratio")
        {
            flag="QualityRatio";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(params[0]=="nobisect")
        {
            flag="NoBisect";
            option=true;
        }
        else if(params[0]=="generatemesh")
        {
            flag="generateMesh";
        }
        else if(params[0]=="getboundaries")
        {
            flag="getBoundaries";
        }
        else if(params[0]=="writemesh")
        {
            flag="writeMesh";
        }
        else if(paramSize==2 && params[0]=="epsilon")
        {
            flag="Epsilon";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(params[0]=="nomerge")
        {
            flag="NoMerge";
            option=true;
        }
        else if(params[0]=="diagnose")
        {
            flag="Diagnose";
            option=true;
        }
        else if(params[0]=="check")
        {
            flag="Check";
            option=true;
        }
        else if(params[0]=="quiet")
        {
            flag="Quiet";
            option=true;
        }
        else if(params[0]=="verbose")
        {
            flag="Verbose";
            option=true;
        }
        else if(params[0]=="meshwallfirst")
        {
            flag="MeshWallFirst";
            option=true;
        }
        else if(paramSize==2 && params[0]=="hausd")
        {
            flag="Hausd";
            values[0]=std::stod(params[1]);
            option=true;
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

cvTetGenMeshObject* svMeshSim::GetMesher()
{
    return m_cvTetGetMesh;
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
