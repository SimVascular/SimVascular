#include "svSimulationUtils.h"

#include "svStringUtils.h"

#include <sstream>
#include <iostream>
#include <string>

std::string svSimulationUtils::CreatePreSolverFileContent(svSimJob* job, std::string outputDir)
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

        it = capProps.begin();
        while(it != capProps.end())
        {
            if(it->first!=""&&it->second["BC Type"]=="Prescribed Velocities")
            {
                ss << "prescribed_velocities_vtp mesh-complete/mesh-surfaces/" << it->first <<".vtp\n";

                auto props=it->second;
                ss << "bct_analytical_shape " << props["Analytic Shape"] <<"\n";
                if(props["Period"]=="")
                {
                    ss << "bct_period " << basicProps["Period"] <<"\n";
                }
                else
                {
                    ss << "bct_period " << props["Period"] <<"\n";
                }
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
    ss << "write_restart " << outputDir <<"restart.0.1" << "\n";
    if(deformable)
    {
        ss << "append_displacements " << outputDir <<"restart.0.1" << "\n";
    }

//    ss << "write_numstart " << outputDir <<"numstart.dat" << "\n";

    return ss.str();
}

std::string svSimulationUtils::CreateRCRTFileContent(svSimJob* job)
{
    std::stringstream ss;
    auto basicProps=job->GetBasicProps();

//    ss << "2\n";

    auto capProps=job->GetCapProps();
    auto it = capProps.begin();
    while(it != capProps.end())
    {
        if(it->first!="")
        {
            auto props=it->second;
            if(props["BC Type"]=="RCR")
            {
	      auto values=svStringUtils_split(props["Values"],' ');
                if(values.size()==3)
                {
                    ss << "2\n";
                    ss << values[0] <<"\n";
                    ss << values[1] <<"\n";
                    ss << values[2] <<"\n";
                    ss << "0.0 " << props["Pressure"] <<"\n";
                    ss << basicProps["Period"] << props["Pressure"] <<"\n";
                }
            }
        }
        it++;
    }

    if(ss.str()=="")
        return "";
    else
        return "2\n"+ss.str();;

//    return ss.str();
}

std::string svSimulationUtils::CreateFlowSolverFileContent(svSimJob* job)
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
