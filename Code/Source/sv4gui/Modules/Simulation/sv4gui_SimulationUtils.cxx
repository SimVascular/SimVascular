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

#include "sv4gui_SimulationUtils.h"

#include "sv4gui_StringUtils.h"
#include "sv_integrate_surface.h"

#include <sstream>
#include <iostream>
#include <string>
#include <fstream>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkPointData.h>
#include <vtkDoubleArray.h>
#include <vtkAbstractArray.h>

std::string sv4guiSimulationUtils::CreatePreSolverFileContent(sv4guiSimJob* job, std::string outputDir)
{
    std::stringstream ss;
    std::map<std::string,int> IDs;

    if(outputDir!="")
        outputDir=outputDir+"/";

    ss << "mesh_and_adjncy_vtu mesh-complete/mesh-complete.mesh.vtu\n";

    //set ids for caps
    //================================================================
    int id=1;
    //    IDs["mesh-complete.exterior"]=id;
    ss << "set_surface_id_vtp mesh-complete/mesh-complete.exterior.vtp 1\n";

    std::string setIDPrefix="set_surface_id_vtp mesh-complete/mesh-surfaces/";
    auto capProps=job->GetCapProps();
    int velocityCapNumber=0;
    int pressureCapNumber=0;
    auto it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="")
        {
            auto props=it->second;
            if(props["BC Type"]=="Prescribed Velocities"){
                velocityCapNumber++;
            }
            else
            {
                pressureCapNumber++;
            }

            id++;
            IDs[it->first]=id;
            ss << setIDPrefix << it->first <<".vtp " << id <<"\n";
        }
        it++;
    }
    job->SetIDs(IDs);
    job->SetVelocityCapNumber(velocityCapNumber);
    job->SetPressureCapNumber(pressureCapNumber);

    //set bct for prescribed velocity cap
    //=================================================================
    auto basicProps=job->GetBasicProps();
    if(velocityCapNumber>0)
    {
        ss << "fluid_density " << basicProps["Fluid Density"] <<"\n";
        ss << "fluid_viscosity " << basicProps["Fluid Viscosity"] <<"\n";

        ss << "initial_pressure " << basicProps["Initial Pressure"] <<"\n";
        ss << "initial_velocity " << basicProps["Initial Velocities"] <<"\n";

        it = capProps.begin();
        while(it != capProps.end())
        {
            if(it->first!=""&&it->second["BC Type"]=="Prescribed Velocities")
            {
                ss << "prescribed_velocities_vtp mesh-complete/mesh-surfaces/" << it->first <<".vtp\n";

                auto props=it->second;
                ss << "bct_analytical_shape " << props["Analytic Shape"] <<"\n";
                //                if(props["Period"]=="")
                //                {
                //                    ss << "bct_period " << basicProps["Period"] <<"\n";
                //                }
                //                else
                //                {
                //                    ss << "bct_period " << props["Period"] <<"\n";
                //                }
                ss << "bct_period " << props["Period"] <<"\n";
                ss << "bct_point_number " << props["Point Number"] <<"\n";
                ss << "bct_fourier_mode_number " << props["Fourier Modes"] <<"\n";
                if(props["Flip Normal"]=="True")
                {
                    ss << "bct_flip\n";
                }
                ss << "bct_create mesh-complete/mesh-surfaces/" << it->first << ".vtp " << it->first <<".flow\n";
            }
            it++;
        }

        if(velocityCapNumber>1)
        {
            ss << "bct_merge_on\n";
        }

        ss << "bct_write_dat " << outputDir << "bct.dat\n";
        ss << "bct_write_vtp " << outputDir << "bct.vtp\n";
    }

    //set prescribed pressure cap
    //====================================================================
    it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="" && it->second["BC Type"]!="" && it->second["BC Type"]!="Prescribed Velocities")
        {
            ss << "pressure_vtp mesh-complete/mesh-surfaces/" << it->first << ".vtp " << it->second["Pressure"] << "\n";
        }
        it++;
    }

    //set wall properties
    //====================================================================
    bool deformable=false;
    auto wallProps=job->GetWallProps();
    if(wallProps["Type"]=="rigid")
    {
        ss << "noslip_vtp mesh-complete/walls_combined.vtp\n";
    }
    else if(wallProps["Type"]=="deformable")
    {
        deformable=true;
        ss << "deformable_wall_vtp mesh-complete/walls_combined.vtp\n";
        ss << "deformable_thickness " << wallProps["Thickness"] << "\n";
        ss << "deformable_E " << wallProps["Elastic Modulus"] << "\n";
        ss << "deformable_nu " << wallProps["Poisson Ratio"] << "\n";
        ss << "deformable_kcons " << wallProps["Shear Constant"] << "\n";
        ss << "deformable_pressure " << wallProps["Pressure"] << "\n";
        ss << "deformable_solve_displacements\n";
        ss << "wall_displacements_write_vtp " << outputDir <<"displacement.vtp" << "\n";
    }
    else if(wallProps["Type"]=="variable")
    {
        deformable=true;
        ss << "deformable_wall_vtp mesh-complete/walls_combined.vtp\n";

        auto varProps=job->GetVarProps();
        it = varProps.begin();
        while(it != varProps.end())
        {
            if(it->first!="")
            {
                auto props=it->second;
                if(props["Thickness"]!="")
                {
                    ss << "set_surface_thickness_vtp mesh-complete/mesh-surfaces/" << it->first << ".vtp " << props["Thickness"] << "\n";
                }

            }
            it++;
        }
        ss << "solve_varwall_thickness\n";

        it = varProps.begin();
        while(it != varProps.end())
        {
            if(it->first!="")
            {
                auto props=it->second;
                if(props["Elastic Modulus"]!="")
                {
                    ss << "set_surface_E_vtp mesh-complete/mesh-surfaces/" << it->first << ".vtp " << props["Elastic Modulus"] << "\n";
                }

            }
            it++;
        }
        ss << "solve_varwall_E\n";

        ss << "varwallprop_write_vtp " << outputDir <<"varwallprop.vtp" << "\n";

        ss << "deformable_nu " << wallProps["Poisson Ratio"] << "\n";
        ss << "deformable_kcons " << wallProps["Shear Constant"] << "\n";
        ss << "deformable_pressure " << wallProps["Pressure"] << "\n";
        ss << "deformable_solve_varwall_displacements\n";
        ss << "wall_displacements_write_vtp " << outputDir <<"displacement.vtp" << "\n";
    }

    ss << "write_geombc " << outputDir <<"geombc.dat.1" << "\n";

    if(basicProps["IC File"]=="")
        ss << "write_restart " << outputDir <<"restart.0.1" << "\n";

    else if(deformable)
    {
        ss << "append_displacements " << outputDir <<"restart.0.1" << "\n";
    }

    //    ss << "write_numstart " << outputDir <<"numstart.dat" << "\n";

    return ss.str();
}

std::string sv4guiSimulationUtils::CreateRCRTFileContent(sv4guiSimJob* job)
{
    std::stringstream ss;
    //    auto basicProps=job->GetBasicProps();

    auto capProps=job->GetCapProps();
    auto it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="")
        {
            auto props=it->second;
            if(props["BC Type"]=="RCR")
            {
                auto values=sv4guiStringUtils_split(props["Values"],' ');
                if(values.size()==3)
                {
                    ss << "2\n";
                    ss << values[0] <<"\n";
                    ss << values[1] <<"\n";
                    ss << values[2] <<"\n";
                    ss << "0.0 " << props["Pressure"] <<"\n";
                    //                    ss << basicProps["Period"] << " " << props["Pressure"] <<"\n";
                    ss << "1.0 " << props["Pressure"] <<"\n";
                }
            }
        }
        it++;
    }

    if(ss.str()=="")
        return "";
    else
        return "2\n"+ss.str();
}

std::string sv4guiSimulationUtils::CreateCORTFileContent(sv4guiSimJob* job)
{
    std::stringstream ss;
    int maxStepNumber=0;

    auto capProps=job->GetCapProps();
    auto it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="")
        {
            auto props=it->second;
            if(props["BC Type"]=="Coronary")
            {
                auto values=sv4guiStringUtils_split(props["Values"],' ');
                if(values.size()==5)
                {
                    double pressurePeriod=std::stod(props["Pressure Period"]);
                    double pressureScaling=std::stod(props["Pressure Scaling"]);

                    auto timeStepVec=sv4guiStringUtils_split(props["Timed Pressure"],'\n');
                    int timeStepNumber=timeStepVec.size();
                    if(timeStepNumber>maxStepNumber)
                        maxStepNumber=timeStepNumber;

                    auto lastVec=sv4guiStringUtils_split(timeStepVec.back(),' ');
                    double orignalPeriod=std::stod(lastVec[0]);

                    bool scaleTime=false;
                    double timeFactor=1;
                    if(pressurePeriod>0 && pressurePeriod!=orignalPeriod)
                    {
                        scaleTime=true;
                        timeFactor=pressurePeriod/orignalPeriod;
                    }

                    bool scalePressure=false;
                    double pressureFactor=1;
                    if(pressureScaling!=0 && pressureScaling!=1)
                    {
                        scalePressure=true;
                        pressureFactor=pressureScaling;
                    }

                    double Ra=std::stod(values[0]);
                    double Ca=std::stod(values[1]);
                    double Ram=std::stod(values[2]);
                    double Cim=std::stod(values[3]);
                    double Rv=std::stod(values[4]);

                    double q0=Ra+Ram+Rv;
                    double q1=Ra*Ca*(Ram+Rv)+Cim*(Ra+Ram)*Rv;
                    double q2=Ca*Cim*Ra*Ram*Rv;

                    double p0=1;
                    double p1=Ram*Ca+Rv*(Ca+Cim);
                    double p2=Ca*Cim*Ram*Rv;

                    double b0=0;
                    double b1=Cim*Rv;
                    double b2=0;

                    ss << timeStepNumber <<"\n";
                    ss << q0 <<"\n";
                    ss << q1 <<"\n";
                    ss << q2 <<"\n";
                    ss << p0 <<"\n";
                    ss << p1 <<"\n";
                    ss << p2 <<"\n";
                    ss << b0 <<"\n";
                    ss << b1 <<"\n";
                    ss << b2 <<"\n";
                    ss << "0.0\n";
                    ss << "100.0\n";

                    for(int i=0;i<timeStepVec.size();i++)
                    {
                        auto lineVec=sv4guiStringUtils_split(timeStepVec[i],' ');
                        std::string timeStr=lineVec[0];
                        std::string pressureStr=lineVec[1];

                        if(scaleTime)
                            timeStr=std::to_string(std::stod(timeStr)*timeFactor);

                        if(scalePressure)
                            pressureStr=std::to_string(std::stod(pressureStr)*pressureFactor);

                        ss << timeStr << " " << pressureStr<<"\n";
                    }
                }
            }
        }
        it++;
    }

    if(ss.str()=="")
        return "";
    else
    {
        std::stringstream newss;
        newss<<maxStepNumber << "\n" <<ss.str();
        return newss.str();
    }
}

std::string sv4guiSimulationUtils::CreateFlowSolverFileContent(sv4guiSimJob* job)
{
    std::stringstream ss;
    std::map<std::string,int> IDs=job->GetIDs();

    auto basicProps=job->GetBasicProps();
    auto solverProps=job->GetSolverProps();

    //Fluid Properties
    //======================================================
    ss << "Density: " << basicProps["Fluid Density"] <<"\n";
    ss << "Viscosity: " << basicProps["Fluid Viscosity"] <<"\n";
    ss << "\n";

    //Time Steps
    //======================================================
    ss << "Number of Timesteps: " << solverProps["Number of Timesteps"] <<"\n";
    ss << "Time Step Size: " << solverProps["Time Step Size"] <<"\n";
    ss << "\n";

    //Output Control
    //======================================================
    ss << "Number of Timesteps between Restarts: " << solverProps["Number of Timesteps between Restarts"] <<"\n";
    if(solverProps["Output Surface Stress"]=="True")
    {
        ss << "Number of Force Surfaces: 1\n";
        ss << "Surface ID's for Force Calculation: 1\n";
        ss << "Force Calculation Method: " << solverProps["Force Calculation Method"] <<"\n";
    }
    ss << "Print Average Solution: " << solverProps["Print Average Solution"] <<"\n";
    ss << "Print Error Indicators: " << solverProps["Print Error Indicators"] <<"\n";
    ss << "\n";

    //BCT Prescribed Velocities
    //======================================================
    if(job->GetVelocityCapNumber()>0)
        ss << "Time Varying Boundary Conditions From File: True\n";
    else
        ss << "Time Varying Boundary Conditions From File: False\n";

    ss << "\n";

    //Step Construction
    //=====================================================
    int stepNumber=std::stoi(solverProps["Step Construction"]);
    if(stepNumber>0)
    {
        ss << "Step Construction:";
        for(int i=0;i<stepNumber;i++)
            ss << " 0 1";

        ss << "\n";
    }
    ss << "\n";

    //BC: Resistance
    //===================================================
    auto capProps=job->GetCapProps();
    std::vector<int> RIDs;
    std::vector<std::string> RValues;
    auto it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="" && it->second["BC Type"]=="Resistance")
        {
            RIDs.push_back(IDs[it->first]);
            RValues.push_back(it->second["Values"]);
        }
        it++;
    }
    if(RIDs.size()>0)
    {
        ss << "Number of Resistance Surfaces: " << RIDs.size() <<"\n";

        ss << "List of Resistance Surfaces:";
        for(int i=0;i<RIDs.size();i++)
        {
            ss << " " << RIDs[i];
        }
        ss << "\n";

        ss << "Resistance Values:";
        for(int i=0;i<RValues.size();i++)
        {
            ss << " " << RValues[i];
        }
        ss << "\n";

        ss << "\n";
    }

    //BC: RCR
    //===================================================
    std::vector<int> RCRIDs;
    it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="" && it->second["BC Type"]=="RCR")
        {
            RCRIDs.push_back(IDs[it->first]);
        }
        it++;
    }
    if(RCRIDs.size()>0)
    {
        ss << "Number of RCR Surfaces: " << RCRIDs.size() <<"\n";

        ss << "List of RCR Surfaces:";
        for(int i=0;i<RCRIDs.size();i++)
        {
            ss << " " << RCRIDs[i];
        }
        ss << "\n";

        ss << "RCR Values From File: True\n";

        ss << "\n";
    }

    //BC: Impedance
    //===================================================
    std::vector<int> ImpIDs;
    it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="" && it->second["BC Type"]=="Impedance")
        {
            ImpIDs.push_back(IDs[it->first]);
        }
        it++;
    }
    if(ImpIDs.size()>0)
    {
        ss << "Number of Impedance Surfaces: " << ImpIDs.size() <<"\n";

        ss << "List of Impedance Surfaces:";
        for(int i=0;i<ImpIDs.size();i++)
        {
            ss << " " << ImpIDs[i];
        }
        ss << "\n";

        ss << "Impedance From File: True\n";

        ss << "\n";
    }

    //BC: Coronary
    //===================================================
    std::vector<int> CorIDs;
    it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="" && it->second["BC Type"]=="Coronary")
        {
            CorIDs.push_back(IDs[it->first]);
        }
        it++;
    }
    if(CorIDs.size()>0)
    {
        ss << "Number of COR Surfaces: " << CorIDs.size() <<"\n";

        ss << "List of COR Surfaces:";
        for(int i=0;i<CorIDs.size();i++)
        {
            ss << " " << CorIDs[i];
        }
        ss << "\n";

        ss << "COR Values From File: True\n";

        ss << "\n";
    }

    //BC: Closedloop
    //===================================================
    //to do

    //Deformable
    //==================================================
    auto wallProps=job->GetWallProps();
    if(wallProps["Type"]=="deformable")
    {
        ss << "Deformable Wall: True\n";
        ss << "Thickness of Vessel Wall: " << wallProps["Thickness"] << "\n";
        ss << "Young Mod of Vessel Wall: " << wallProps["Elastic Modulus"] << "\n";
        ss << "Density of Vessel Wall: " << wallProps["Density"] <<"\n";
        ss << "Poisson Ratio of Vessel Wall: " << wallProps["Poisson Ratio"] << "\n";
        ss << "Shear Constant of Vessel Wall: " << wallProps["Shear Constant"] << "\n";
    }
    else if(wallProps["Type"]=="variable")
    {
        ss << "Deformable Wall: True\n";
        ss << "Variable Wall Thickness and Young Mod: True\n";
        ss << "Density of Vessel Wall: " << wallProps["Density"] <<"\n";
        ss << "Poisson Ratio of Vessel Wall: " << wallProps["Poisson Ratio"] << "\n";
        ss << "Shear Constant of Vessel Wall: " << wallProps["Shear Constant"] << "\n";
    }

    //Advanced
    //======================================================

    //BC: coupling
    //==================================================
    ss << "Pressure Coupling: " << solverProps["Pressure Coupling"] <<"\n";
    ss << "Number of Coupled Surfaces: " << job->GetPressureCapNumber() <<"\n";
    ss << "\n";

    //BC: Backflow Control
    //===================================================
    ss << "Backflow Stabilization Coefficient: " << solverProps["Backflow Stabilization Coefficient"] <<"\n";


    //Non-linear Iteration Control
    //--------------------------------------------------
    ss << "Residual Control: " << solverProps["Residual Control"] <<"\n";
    ss << "Residual Criteria: " << solverProps["Residual Criteria"] <<"\n";
    ss << "Minimum Required Iterations: " << solverProps["Minimum Required Iterations"] <<"\n";
    //Linear Solver
    //--------------------------------------------------
    ss << "svLS Type: " << solverProps["svLS Type"] <<"\n";
    ss << "Number of Krylov Vectors per GMRES Sweep: " << solverProps["Number of Krylov Vectors per GMRES Sweep"] <<"\n";
    ss << "Number of Solves per Left-hand-side Formation: " << solverProps["Number of Solves per Left-hand-side Formation"] <<"\n";
    ss << "Tolerance on Momentum Equations: " << solverProps["Tolerance on Momentum Equations"] <<"\n";
    ss << "Tolerance on Continuity Equations: " << solverProps["Tolerance on Continuity Equations"] <<"\n";
    ss << "Tolerance on svLS NS Solver: " << solverProps["Tolerance on svLS NS Solver"] <<"\n";
    ss << "Maximum Number of Iterations for svLS NS Solver: " << solverProps["Maximum Number of Iterations for svLS NS Solver"] <<"\n";
    ss << "Maximum Number of Iterations for svLS Momentum Loop: " << solverProps["Maximum Number of Iterations for svLS Momentum Loop"] <<"\n";
    ss << "Maximum Number of Iterations for svLS Continuity Loop: " << solverProps["Maximum Number of Iterations for svLS Continuity Loop"] <<"\n";
    //Discretization Control
    //----------------------------------------------------
    ss << "Time Integration Rule: " << solverProps["Time Integration Rule"] <<"\n";
    ss << "Time Integration Rho Infinity: " << solverProps["Time Integration Rho Infinity"] <<"\n";
    ss << "Flow Advection Form: " << solverProps["Flow Advection Form"] <<"\n";
    ss << "Quadrature Rule on Interior: " << solverProps["Quadrature Rule on Interior"] <<"\n";
    ss << "Quadrature Rule on Boundary: " << solverProps["Quadrature Rule on Boundary"] <<"\n";


    return ss.str();
}

bool sv4guiSimulationUtils::CreateFlowFiles(std::string outFlowFilePath, std::string outPressureFlePath
                                        , std::string outAverageFilePath, std::string outAverageUnitsFilePath
                                        , std::vector<std::string> vtxFilePaths, bool useComboFile
                                        , std::string meshFaceDir, std::vector<std::string> meshFaceFileNames
                                        , std::string unit, bool skipWalls)
{
    std::map<std::string,vtkSmartPointer<vtkPolyData>> vtpMap;

    for(int i=0;i<meshFaceFileNames.size();++i)
    {
        std::string filePath=meshFaceDir+"/"+meshFaceFileNames[i];
        std::string faceName=meshFaceFileNames[i].substr(0,meshFaceFileNames[i].find_last_of('.'));

        if(skipWalls && faceName.substr(0,4)=="wall")
            continue;

        std::ifstream faceFile(filePath);
        if(!faceFile)
            continue;

        vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
        reader->SetFileName(filePath.c_str());
        reader->Update();
        vtkSmartPointer<vtkPolyData> facevtp=reader->GetOutput();
        vtkSmartPointer<vtkDataArray> array=facevtp->GetPointData()->GetArray("GlobalNodeID");
        if(array==NULL)
            facevtp->GetPointData()->GetScalars()->SetName("GlobalNodeID");

        facevtp->GetPointData()->SetActiveScalars("GlobalNodeID");

        vtpMap[faceName]=facevtp;
    }

    std::map<std::string,vtkSmartPointer<vtkPolyData>> simVtps;
    std::map<std::string,vtkSmartPointer<vtkUnstructuredGrid>> simUgs;

    for(int i=0;i<vtxFilePaths.size();i++)
    {
        std::string vtxFilePath=vtxFilePaths[i];

        vtkSmartPointer<vtkPolyData> simvtp=NULL;
        vtkSmartPointer<vtkUnstructuredGrid> simvtu=NULL;

        if(vtxFilePath.substr(vtxFilePath.find_last_of('.'),4)==".vtp")
        {
            vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
            reader->SetFileName(vtxFilePath.c_str());
            reader->Update();
            simvtp=reader->GetOutput();
        }
        else if(vtxFilePath.substr(vtxFilePath.find_last_of('.'),4)==".vtu")
        {
            vtkSmartPointer<vtkXMLUnstructuredGridReader> reader = vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();
            reader->SetFileName(vtxFilePath.c_str());
            reader->Update();
            simvtu=reader->GetOutput();
        }

        if(useComboFile)
        {
            if(simvtp!=NULL)
                simVtps["combo"]=simvtp;

            if(simvtu!=NULL)
                simUgs["combo"]=simvtu;
        }
        else
        {
            std::string str=vtxFilePath.substr(vtxFilePath.find_last_of('_')+1);
            std::string step=str.substr(0,str.find_last_of('.'));
            if(simvtp!=NULL)
                simVtps[step]=simvtp;

            if(simvtu!=NULL)
                simUgs[step]=simvtu;
        }

    }

    if(simVtps.size()==0 && simUgs.size()==0)
        return false;

    std::map<std::string,std::map<std::string, double>> pressureMap;
    std::map<std::string,std::map<std::string, double>> flowrateMap;
    std::map<std::string,std::map<std::string, double>> areaMap;

    auto it=vtpMap.begin();
    while(it!=vtpMap.end())
    {
        for(auto step_simvtp:simVtps)
        {
            std::string step=step_simvtp.first;
            vtkSmartPointer<vtkPolyData> simvtp=step_simvtp.second;

            if(simvtp!=NULL)
                VtpExtractSingleFace(step,simvtp, it->second);
        }

        for(auto step_simug:simUgs)
        {
            std::string step=step_simug.first;
            vtkSmartPointer<vtkUnstructuredGrid> simug=step_simug.second;

            if(simug!=NULL)
                VtuExtractSingleFace(step,simug, it->second);
        }

        std::map<std::string, double> pmap;
        std::map<std::string, double> qmap;
        std::map<std::string, double> amap;

        VtpIntegrateFace(it->second,pmap,qmap,amap);

        pressureMap[it->first]=pmap;
        flowrateMap[it->first]=qmap;
        areaMap[it->first]=amap;

        it++;
    }

    ofstream pressurefs(outPressureFlePath.c_str());
    ofstream flowfs(outFlowFilePath.c_str());
    ofstream averagefs(outAverageFilePath.c_str());
    ofstream averageunitsfs(outAverageUnitsFilePath.c_str());

    pressurefs<<std::fixed<<"step\t";
    flowfs<<std::fixed<<"step\t";

    for(auto name_vtp:vtpMap)
    {
        pressurefs<<name_vtp.first<<"\t";
        flowfs<<name_vtp.first<<"\t";
    }

    pressurefs<<"\n";
    flowfs<<"\n";

    std::vector<std::string> steps;
    auto pmap=pressureMap.begin()->second;
    for(auto step:pmap)
        steps.push_back(step.first);

    for(int i=0;i<steps.size();++i)
    {
        pressurefs<<steps[i]<<"\t";
        for(auto name_pdata:pressureMap)
            pressurefs<<name_pdata.second[steps[i]]<<"\t";

        pressurefs<<"\n";
    }
    pressurefs.close();

    steps.clear();
    auto qmap=flowrateMap.begin()->second;
    for(auto step:qmap)
        steps.push_back(step.first);

    for(int i=0;i<steps.size();++i)
    {
        flowfs<<steps[i]<<"\t";
        for(auto name_fdata:flowrateMap)
            flowfs<<name_fdata.second[steps[i]]<<"\t";

        flowfs<<"\n";
    }
    flowfs.close();

    averagefs<<std::fixed<<"Face\t"<<"Pavg\t"<<"Qavg\t"<<"Pmin\t"<<"Pmin_Time\t"<<"Pmax\t"<<"Pmax_Time\t"<<"Qmin\t"<<"Qmin_Time\t"<<"Qmax\t"<<"Qmax_Time\n";
    averageunitsfs<<std::fixed<<"Face\t"<<"Pavg\t"<<"Qavg\t"<<"Pmin\t"<<"Pmin_Time\t"<<"Pmax\t"<<"Pmax_Time\t"<<"Qmin\t"<<"Qmin_Time\t"<<"Qmax\t"<<"Qmax_Time\n";
    for(auto name_vtp:vtpMap)
    {
        std::string faceName=name_vtp.first;
        pmap=pressureMap[faceName];
        qmap=flowrateMap[faceName];

        std::string step0=pmap.begin()->first;
        double pmin=pmap[step0];
        double pmax=pmap[step0];
        std::string pminStep=step0;
        std::string pmaxStep=step0;
        double pavg=0;

        step0=qmap.begin()->first;
        double qmin=qmap[step0];
        double qmax=qmap[step0];
        std::string qminStep=step0;
        std::string qmaxStep=step0;
        double qavg=0;

        for(auto step_data:pmap)
        {
            std::string step=step_data.first;
            double p=pmap[step];
            double q=qmap[step];

            if(p<pmin)
            {
                pmin=p;
                pminStep=step;
            }
            if(p>pmax)
            {
                pmax=p;
                pmaxStep=step;
            }
            if(q<qmin)
            {
                qmin=q;
                qminStep=step;
            }
            if(q>qmax)
            {
                qmax=q;
                qmaxStep=step;
            }
            pavg+=p;
            qavg+=q;
        }

        pavg/=pmap.size();
        qavg/=pmap.size();

        averagefs<<faceName<<"\t"<<pavg<<"\t"<<qavg<<"\t"
                <<pmin<<"\t"<<pminStep<<"\t"<<pmax<<"\t"<<pmaxStep<<"\t"
               <<qmin<<"\t"<<qminStep<<"\t"<<qmax<<"\t"<<qmaxStep<<"\n";

        double pscaling=1;
        double qscaling=1;

        if(unit=="mm")
        {
            pscaling=760.0/101325.0;
            qscaling=60.0/1000.0/1000.0;
        }
        else if(unit=="cm")
        {
            pscaling=76.0/101325.0;
            qscaling=60.0/1000.0;
        }
        else if(unit=="m")
        {
            pscaling=760.0/101325.0;
            qscaling=60.0*1000.0;
        }
        averageunitsfs<<faceName<<"\t"<<pavg*pscaling<<"\t"<<qavg*qscaling<<"\t"
                     <<pmin*pscaling<<"\t"<<pminStep<<"\t"<<pmax*pscaling<<"\t"<<pmaxStep<<"\t"
                    <<qmin*qscaling<<"\t"<<qminStep<<"\t"<<qmax*qscaling<<"\t"<<qmaxStep<<"\n";

    }

    averagefs.close();
    averageunitsfs.close();

    return true;
}

void sv4guiSimulationUtils::VtpExtractSingleFace(std::string step, vtkSmartPointer<vtkPolyData> simvtp,vtkSmartPointer<vtkPolyData> facevtp)
{
    int faceNumPoint=facevtp->GetNumberOfPoints();
    vtkDataArray* faceNodeIDs=facevtp->GetPointData()->GetArray("GlobalNodeID");

    vtkPointData* pointData=simvtp->GetPointData();

    std::vector<std::string> pressureNames, velocityNames;

    for(int i=0;i<pointData->GetNumberOfArrays();++i)
    {
        std::string name(pointData->GetAbstractArray(i)->GetName());

        if(step=="combo")
        {
            if(name=="pressure_avg" || name=="pressure_avg_mmHg")
                continue;

            if(name.substr(0,9)=="pressure_")
                pressureNames.push_back(name);
            else if(name.substr(0,9)=="velocity_")
                velocityNames.push_back(name);
        }
        else
        {
            if(name=="pressure")
                pressureNames.push_back(name);
            else if(name=="velocity")
                velocityNames.push_back(name);
        }
    }

    std::map<int,int> mapGlobal2Local;
    vtkDataArray* simvtpGlobalIDs=pointData->GetArray("GlobalNodeID");
    for(int i=0;i<simvtp->GetNumberOfPoints();++i)
    {
        int nodeID=simvtpGlobalIDs->GetTuple1(i);
        mapGlobal2Local[nodeID]=i;
    }

    for(int i=0;i<pressureNames.size();++i)
    {
        vtkSmartPointer<vtkDoubleArray> parray=vtkSmartPointer<vtkDoubleArray>::New();
        parray->SetNumberOfComponents(1);
        parray->Allocate(100,100);

        std::string pn=pressureNames[i];
        if(step!="combo")
            pn=pn+"_"+step;

        parray->SetName(pn.c_str());

        vtkDataArray* array=pointData->GetArray(pressureNames[i].c_str());
        for(int j=0;j<faceNumPoint;++j)
        {
            int gID=faceNodeIDs->GetTuple1(j);
            int lID=mapGlobal2Local[gID];
            double p=array->GetTuple1(lID);
            parray->InsertNextTuple1(p);
        }

        facevtp->GetPointData()->AddArray(parray);
    }

    for(int i=0;i<velocityNames.size();++i)
    {
        vtkSmartPointer<vtkDoubleArray> varray=vtkSmartPointer<vtkDoubleArray>::New();
        varray->SetNumberOfComponents(3);
        varray->Allocate(100,100);

        std::string vn=velocityNames[i];
        if(step!="combo")
            vn=vn+"_"+step;

        varray->SetName(vn.c_str());

        vtkDataArray* array=pointData->GetArray(velocityNames[i].c_str());
        for(int j=0;j<faceNumPoint;++j)
        {
            int gID=faceNodeIDs->GetTuple1(j);
            int lID=mapGlobal2Local[gID];
            double* v=array->GetTuple3(lID);
            varray->InsertNextTuple3(v[0],v[1],v[2]);
        }

        facevtp->GetPointData()->AddArray(varray);
    }

}

void sv4guiSimulationUtils::VtuExtractSingleFace(std::string step, vtkSmartPointer<vtkUnstructuredGrid> simug,vtkSmartPointer<vtkPolyData> facevtp)
{
    int faceNumPoint=facevtp->GetNumberOfPoints();
    vtkDataArray* faceNodeIDs=facevtp->GetPointData()->GetArray("GlobalNodeID");

    vtkPointData* pointData=simug->GetPointData();

    std::vector<std::string> pressureNames, velocityNames;

    for(int i=0;i<pointData->GetNumberOfArrays();++i)
    {
        std::string name(pointData->GetAbstractArray(i)->GetName());

        if(step=="combo")
        {
            if(name=="pressure_avg" || name=="pressure_avg_mmHg")
                continue;

            if(name.substr(0,9)=="pressure_")
                pressureNames.push_back(name);
            else if(name.substr(0,9)=="velocity_")
                velocityNames.push_back(name);
        }
        else
        {
            if(name=="pressure")
                pressureNames.push_back(name);
            else if(name=="velocity")
                velocityNames.push_back(name);
        }
    }

    std::map<int,int> mapGlobal2Local;
    vtkDataArray* simugGlobalIDs=pointData->GetArray("GlobalNodeID");
    for(int i=0;i<simug->GetNumberOfPoints();++i)
    {
        int nodeID=simugGlobalIDs->GetTuple1(i);
        mapGlobal2Local[nodeID]=i;
    }

    for(int i=0;i<pressureNames.size();++i)
    {
        vtkSmartPointer<vtkDoubleArray> parray=vtkSmartPointer<vtkDoubleArray>::New();
        parray->SetNumberOfComponents(1);
        parray->Allocate(100,100);

        std::string pn=pressureNames[i];
        if(step!="combo")
            pn=pn+"_"+step;

        parray->SetName(pn.c_str());

        vtkDataArray* array=pointData->GetArray(pressureNames[i].c_str());
        for(int j=0;j<faceNumPoint;++j)
        {
            int gID=faceNodeIDs->GetTuple1(j);
            int lID=mapGlobal2Local[gID];
            double p=array->GetTuple1(lID);
            parray->InsertNextTuple1(p);
        }

        facevtp->GetPointData()->AddArray(parray);
    }

    for(int i=0;i<velocityNames.size();++i)
    {
        vtkSmartPointer<vtkDoubleArray> varray=vtkSmartPointer<vtkDoubleArray>::New();
        varray->SetNumberOfComponents(3);
        varray->Allocate(100,100);

        std::string vn=velocityNames[i];
        if(step!="combo")
            vn=vn+"_"+step;

        varray->SetName(vn.c_str());

        vtkDataArray* array=pointData->GetArray(velocityNames[i].c_str());
        for(int j=0;j<faceNumPoint;++j)
        {
            int gID=faceNodeIDs->GetTuple1(j);
            int lID=mapGlobal2Local[gID];
            double* v=array->GetTuple3(lID);
            varray->InsertNextTuple3(v[0],v[1],v[2]);
        }

        facevtp->GetPointData()->AddArray(varray);
    }
}

void sv4guiSimulationUtils::VtpIntegrateFace(vtkSmartPointer<vtkPolyData> facevtp, std::map<std::string, double>& pmap
                                         , std::map<std::string, double>& qmap, std::map<std::string, double>& amap)
{
    vtkPointData* pointData=facevtp->GetPointData();

    std::vector<std::string> pressureNames, velocityNames;

    for(int i=0;i<pointData->GetNumberOfArrays();++i)
    {
        std::string name(pointData->GetAbstractArray(i)->GetName());

        if(name.substr(0,9)=="pressure_")
            pressureNames.push_back(name);
        else if(name.substr(0,9)=="velocity_")
            velocityNames.push_back(name);
    }

    for(int i=0;i<pressureNames.size();i++)
    {
        pointData->SetActiveScalars(pressureNames[i].c_str());
        double force=0,area=0;

        sys_geom_IntegrateSurface2(facevtp,0,&force,&area);

        amap[pressureNames[i].substr(9)]=area;
        pmap[pressureNames[i].substr(9)]=force/area;
    }

    for(int i=0;i<velocityNames.size();i++)
    {
        pointData->SetActiveVectors(velocityNames[i].c_str());
        double flowrate=0,area=0;

        sys_geom_IntegrateSurface2(facevtp,1,&flowrate,&area);

        qmap[pressureNames[i].substr(9)]=flowrate;
    }

}

