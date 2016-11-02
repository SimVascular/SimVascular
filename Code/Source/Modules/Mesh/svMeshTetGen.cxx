#include "svMeshTetGen.h"

#include "svStringUtils.h"

#include "cvSolidModel.h"

#include <iostream>
using namespace std;

svMeshTetGen::svMeshTetGen()
    : m_cvTetGetMesh(NULL)
{
    m_Type="TetGen";
    //m_cvTetGetMesh=new cvTetGenMeshObject(NULL);
}

svMeshTetGen::svMeshTetGen(const svMeshTetGen &other)
    : svMesh(other)
    , m_cvTetGetMesh(other.m_cvTetGetMesh)
{
}

svMeshTetGen::~svMeshTetGen()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;
}

svMeshTetGen* svMeshTetGen::Clone()
{
    return new svMeshTetGen(*this);
}

void svMeshTetGen::InitNewMesher()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;

    m_cvTetGetMesh=new cvTetGenMeshObject(NULL);
}

bool svMeshTetGen::SetModelElement(svModelElement* modelElement)
{
    if(!svMesh::SetModelElement(modelElement))
        return false;

    if(modelElement==NULL||modelElement->GetWholeVtkPolyData()==NULL)
        return false;

    std::string modelType=modelElement->GetType();

    SolidModel_KernelT kernel= SolidModel_KernelT_StrToEnum( modelType.c_str() );

    m_cvTetGetMesh->SetSolidModelKernel(kernel);

    if(m_cvTetGetMesh->LoadModel(modelElement->GetWholeVtkPolyData())!=CV_OK)
        return false;

    //set walls
    //m_cvTetGetMesh->SetMeshOptions("MeshWallFirst",0,NULL) //not nessary, because in "SetWalls" it'll be set at 1 again.
    std::vector<int> wallFaceIDs=modelElement->GetWallFaceIDs();
    if(m_cvTetGetMesh->SetWalls(wallFaceIDs.size(),&wallFaceIDs[0])!=CV_OK)
        return false;

    return true;
}

bool svMeshTetGen::Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg)
{
    if(flag=="")
    {
        msg="Empty Command";
        return true;
    }

    if(option)
    {
        if(m_cvTetGetMesh->SetMeshOptions(flag.c_str(), 10, values)!=CV_OK)
        {
            msg="Failed in setting options with "+flag;
            return false;
        }
    }
    else if(flag=="functionBasedMeshing")
    {
        if(m_cvTetGetMesh->SetSizeFunctionBasedMesh(values[0],strValues[0].c_str())!=CV_OK)
        {
            msg="Failed in function based meshing, such as radius based";
            return false;
        }
    }
    else if(flag=="boundaryLayer")
    {
        double H[2]={values[1],values[2]};
        if(m_cvTetGetMesh->SetBoundaryLayer(0, 0, 0, values[0], H)!=CV_OK)
        {
            msg="Failed in boudnary layer meshing";
            return false;
        }
    }
    else if(flag=="sphereRefinement")
    {
        double center[3]={values[2],values[3],values[4]};
        if(m_cvTetGetMesh->SetSphereRefinement(values[0],values[1],center)!=CV_OK)
        {
            msg="Failed in sphere refinement";
            return false;
        }
    }
    else if(flag=="generateMesh")
    {
        if(m_cvTetGetMesh->GenerateMesh()!=CV_OK)
        {
            msg="Failed in generating mesh";
            return false;
        }
    }
    else if(flag=="getBoundaries")
    {
        if(m_cvTetGetMesh->GetBoundaryFaces(50)!=CV_OK)
        {
            msg="Failed in getting boundary after boundary layer meshing";
            return false;
        }
    }
    else if(flag=="writeMesh")
    {
        if(m_cvTetGetMesh->WriteMesh(NULL,0)!=CV_OK)
        {
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

bool svMeshTetGen::ParseCommandInternal(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg)
{
    return ParseCommand(cmd, flag, values, strValues, option, msg);
}

bool svMeshTetGen::ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg)
{
    option=false;

    std::string originalCmd=cmd;
    sv::trim(cmd);
    if(cmd=="")
    {
        flag="";
        msg="Empty command";
        return true;
    }

    sv::lower(cmd);

    std::vector<std::string> params=sv::split(cmd);
    int paramSize=params.size();

    try{

        if(paramSize==2 && params[0]=="surfacemesh")
        {
            flag="SurfaceMeshFlag";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="surface")
        {
            flag="SurfaceMeshFlag";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==2 && params[0]=="volumemesh")
        {
            flag="VolumeMeshFlag";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="volume")
        {
            flag="VolumeMeshFlag";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==2 && params[0]=="usemmg")
        {
            flag="UseMMG";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="usemmg")
        {
            flag="UseMMG";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==2 && params[0]=="globaledgesize")
        {
            flag="GlobalEdgeSize";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="globaledgesize")
        {
            flag="GlobalEdgeSize";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="gsize")
        {
            flag="GlobalEdgeSize";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="a")
        {
            flag="GlobalEdgeSize";
            values[0]=std::stod(params[2]);
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
        else if(paramSize==3 && params[0]=="localedgesize")
        {
            flag="LocalEdgeSize";
            values[0]=std::stod(params[2]);
            strValues[0]=params[1];
            option=true;
        }
        else if(paramSize==3 && params[0]=="localsize")
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
        else if(paramSize==3 && params[0]=="option" && params[1]=="optimization")
        {
            flag="Optimization";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(paramSize==2 && params[0]=="qualityratio")
        {
            flag="QualityRatio";
            values[0]=std::stod(params[1]);
            option=true;
        }
        else if(paramSize==3 && params[0]=="option" && params[1]=="qualityratio")
        {
            flag="QualityRatio";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(params[0]=="nobisect")
        {
            flag="NoBisect";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="nobisect")
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
        else if(paramSize==3 && params[0]=="option" && params[1]=="epsilon")
        {
            flag="Epsilon";
            values[0]=std::stod(params[2]);
            option=true;
        }
        else if(params[0]=="nomerge")
        {
            flag="NoMerge";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="nomerge")
        {
            flag="NoMerge";
            option=true;
        }
        else if(params[0]=="diagnose")
        {
            flag="Diagnose";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="diagnose")
        {
            flag="Diagnose";
            option=true;
        }
        else if(params[0]=="check")
        {
            flag="Check";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="check")
        {
            flag="Check";
            option=true;
        }
        else if(params[0]=="quiet")
        {
            flag="Quiet";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="quiet")
        {
            flag="Quiet";
            option=true;
        }
        else if(params[0]=="verbose")
        {
            flag="Verbose";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="verbose")
        {
            flag="Verbose";
            option=true;
        }
        else if(params[0]=="meshwallfirst")
        {
            flag="MeshWallFirst";
            option=true;
        }
        else if(paramSize==2 && params[0]=="option" && params[1]=="meshwallfirst")
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
        else if(paramSize==3 && params[0]=="option" && params[1]=="hausd")
        {
            flag="Hausd";
            values[0]=std::stod(params[2]);
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

cvTetGenMeshObject* svMeshTetGen::GetMesher()
{
    return m_cvTetGetMesh;
}
