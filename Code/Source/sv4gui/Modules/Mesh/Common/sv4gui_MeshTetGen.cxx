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

#include "sv4gui_MeshTetGen.h"

#include "sv4gui_StringUtils.h"
#include "sv4gui_ModelUtils.h"

#include <iostream>
#include <fstream>

sv4guiMeshTetGen::sv4guiMeshTetGen()
    : m_cvTetGenMesh(NULL)
{
    m_Type="TetGen";
    //m_cvTetGenMesh=new cvTetGenMeshObject(NULL);
}

sv4guiMeshTetGen::sv4guiMeshTetGen(const sv4guiMeshTetGen &other)
    : sv4guiMesh(other)
//    , m_cvTetGenMesh(other.m_cvTetGenMesh)
{
}

sv4guiMeshTetGen::~sv4guiMeshTetGen()
{
    if(m_cvTetGenMesh!=NULL)
        delete m_cvTetGenMesh;
}

sv4guiMeshTetGen* sv4guiMeshTetGen::Clone()
{
    return new sv4guiMeshTetGen(*this);
}

void sv4guiMeshTetGen::InitNewMesher()
{
    if(m_cvTetGenMesh!=NULL)
        delete m_cvTetGenMesh;

    m_cvTetGenMesh=new cvTetGenMeshObject(NULL);
}

bool sv4guiMeshTetGen::SetModelElement(sv4guiModelElement* modelElement)
{
    if(!sv4guiMesh::SetModelElement(modelElement))
        return false;

    if(m_cvTetGenMesh==NULL)
        return false;

    if(modelElement==NULL||modelElement->GetWholeVtkPolyData()==NULL)
        return false;

    std::string modelType=modelElement->GetType();
    char *mtype = const_cast<char *>(modelType.c_str());
    SolidModel_KernelT kernel= SolidModel_KernelT_StrToEnum( mtype);

    m_cvTetGenMesh->SetSolidModelKernel(kernel);

    vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->SetInputData(modelElement->GetWholeVtkPolyData());
    cleaner->Update();

    vtkSmartPointer<vtkPolyData> vtkpd=vtkSmartPointer<vtkPolyData>::New();
    vtkpd->DeepCopy(cleaner->GetOutput());
    vtkpd->BuildLinks();

    if(m_cvTetGenMesh->LoadModel(vtkpd)!=SV_OK)
        return false;

//    //set walls
//    //m_cvTetGenMesh->SetMeshOptions("MeshWallFirst",0,NULL) //not nessary, because in "SetWalls" it'll be set at 1 again.
//    std::vector<int> wallFaceIDs=modelElement->GetWallFaceIDs();
//    if(m_cvTetGenMesh->SetWalls(wallFaceIDs.size(),&wallFaceIDs[0])!=SV_OK)
//        return false;

    return true;
}

// ----------
//  Execute  
// ----------
/**
 * @brief Execute a mesh command.
 * @param[in] flag The command name. 
 * @param[in] values The command numeric parameter values. 
 * @param[in] strValues The command string parameter values. 
 * @param[in] option True if command is an option.
 * @param[out] msg The string describing the details of command success or failure. 
 * @retval true if the command was successfully parsed. 
 * @retval false if the command was not successfully parsed. 
 * @todo The command processing code needs to be redone, creating a single place where command
 *        names are defined, remove of all this string comparison. 
 */

bool sv4guiMeshTetGen::Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg)
{
    if(m_cvTetGenMesh==NULL)
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
        if(m_cvTetGenMesh->SetMeshOptions(mflag, 10, values)!=SV_OK)
        {
            msg="Failed in setting mesh options";
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
        if(m_cvTetGenMesh->SetWalls(wallFaceIDs.size(),&wallFaceIDs[0])!=SV_OK)
        {
            msg="Failed ot set walls";
            return false;
        }

    }
    else if(flag=="useCenterlineRadius")
    {
        cvPolyData* solid=m_cvTetGenMesh->GetSolid();
        if(solid==NULL)
        {
            msg="No mesher model";
            return false;
        }
        vtkPolyData* centerlines=sv4guiModelUtils::CreateCenterlines(solid->GetVtkPolyData());
        vtkPolyData* distance=sv4guiModelUtils::CalculateDistanceToCenterlines(centerlines, solid->GetVtkPolyData());
        if(distance==NULL)
        {
            msg="Failed in calculating distance to centerlines";
            return false;
        }

        //delete centerlines;
        //delete solid;

        m_cvTetGenMesh->SetVtkPolyDataObject(distance);
    }
    else if(flag=="functionBasedMeshing")
    {
        char *strv = const_cast<char *>(strValues[0].c_str());
        if(m_cvTetGenMesh->SetSizeFunctionBasedMesh(values[0],strv)!=SV_OK)
        {
            msg="Failed in function based meshing, such as radius based";
            return false;
        }
    }
    else if(flag=="boundaryLayer")
    {
      double H[3]={values[1],values[2],values[3]};
      if(m_cvTetGenMesh->SetBoundaryLayer(0, 0, 0, values[0], H)!=SV_OK)
      {
          msg="Failed in boudnary layer meshing";
          return false;
      }
    }
    else if(flag=="sphereRefinement")
    {
        double center[3]={values[2],values[3],values[4]};
        if(m_cvTetGenMesh->SetSphereRefinement(values[0],values[1],center)!=SV_OK)
        {
            msg="Failed in sphere refinement";
            return false;
        }
    }
    else if(flag=="AllowMultipleRegions")
    {
        bool value = (int(values[0]) == 1);
        m_cvTetGenMesh->SetAllowMultipleRegions(value);
    }
    else if(flag=="generateMesh")
    {
        if(m_cvTetGenMesh->GenerateMesh()!=SV_OK)
        {
            msg="Failed in generating mesh";
            return false;
        }

        vtkPolyData* surfaceMesh=m_cvTetGenMesh->GetPolyData()->GetVtkPolyData();
        vtkUnstructuredGrid* volumeMesh = NULL;
        if (m_cvTetGenMesh->GetUnstructuredGrid() != NULL)
          volumeMesh = m_cvTetGenMesh->GetUnstructuredGrid()->GetVtkUnstructuredGrid();

        if(surfaceMesh==NULL)
        {
            delete m_cvTetGenMesh;
            m_cvTetGenMesh=NULL;
            msg="Empty mesh created";
            return false;
        }
        m_SurfaceMesh=vtkSmartPointer<vtkPolyData>::New();
        m_SurfaceMesh->DeepCopy(surfaceMesh);
        if (volumeMesh!=NULL)
        {
          m_VolumeMesh=vtkSmartPointer<vtkUnstructuredGrid>::New();
          m_VolumeMesh->DeepCopy(volumeMesh);
        }
    }
    else if(flag=="writeMesh")
    {
      // No action
      delete m_cvTetGenMesh;//Get all data;ok to delete inner mesh
      m_cvTetGenMesh=NULL;
    }
    else
    {
        msg="Unknown command";
        return false;
    }

    msg="Command executed";
    return true;
}

// --------------
//  ParseCommand 
// --------------
/**
 * @brief Extract mesh commands from a string. 
 * @param[in] cmd The space-separated command. 
 * @param[out] flag The command name. 
 * @param[out] values The command numeric parameter values. 
 * @param[out] values The command string parameter values. 
 * @param[out] option Set to true if command is an option.
 * @param[out] msg The string describing the details of command success or failure. 
 * @retval true if the command was successfully parsed. 
 * @retval false if the command was not successfully parsed. 
 * @todo The command processing code needs to be redone, creating a single place where command
 *        names are defined, remove of all this string comparison. 
 */

bool sv4guiMeshTetGen::ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg)
{
    option=false;
    std::string originalCmd=cmd;
    cmd=sv4guiStringUtils_trim(cmd);

    if(cmd=="")
    {
        flag="";
        msg="Empty command";
        return true;
    }

    std::vector<std::string> params=sv4guiStringUtils_split(cmd,' ');

    if(sv4guiStringUtils_lower(params[0])=="option")
    {
        params.erase(params.begin());
    }

    int paramSize=params.size();
    if(paramSize==0)
    {
        msg="Command not complete";
        return false;
    }

    params[0]=sv4guiStringUtils_lower(params[0]);

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
        else if(paramSize==5 && params[0]=="boundarylayer")
        {
            flag="boundaryLayer";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            values[3]=std::stod(params[4]);
        }
        else if(params[0]=="newregionboundarylayer")
        {
          flag="NewRegionBoundaryLayer";
          option=true;
        }
        else if(paramSize==2 && params[0]=="boundarylayerdirection")
        {
          flag="BoundaryLayerDirection";
          values[0]=std::stod(params[1]);
          option=true;
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
        else if(paramSize==2 && params[0]=="mindihedral")
        {
            flag="MinDihedral";
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
        else if(paramSize==4 && params[0]=="addhole")
        {
            flag="AddHole";
            values[0]=std::stod(params[1]);
            values[1]=std::stod(params[2]);
            values[2]=std::stod(params[3]);
            option=true;
        }
        else if(paramSize==5 && params[0]=="addsubdomain")
        {
          flag="AddSubDomain";
          values[0]=std::stod(params[1]);
          values[1]=std::stod(params[2]);
          values[2]=std::stod(params[3]);
          values[3]=std::stod(params[4]);
          option=true;
        }
        else if(paramSize==2 && params[0]=="allowmultipleregions")
        {
          flag="AllowMultipleRegions";
          values[0]=std::stod(params[1]);
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

cvTetGenMeshObject* sv4guiMeshTetGen::GetMesher()
{
    return m_cvTetGenMesh;
}

sv4guiMesh* sv4guiMeshTetGen::CreateMesh()
{
    return new sv4guiMeshTetGen();
}

//bool sv4guiMeshTetGen::WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh
//                                     , vtkSmartPointer<vtkUnstructuredGrid> volumeMesh
//                                     , sv4guiModelElement* modelElement
//                                     , std::string meshDir)
//{

//}

//bool sv4guiMeshTetGen::WriteMeshComplete(std::string meshDir)
//{

//}
