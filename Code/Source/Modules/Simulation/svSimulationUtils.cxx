#include "svSimulationUtils.h"

#include "svStringUtils.h"

#include <sstream>
#include <iostream>
#include <string>

std::string svSimulationUtils::CreatePreSolverFileContent(svSimJob* job, std::string outputDir)
{
    std::stringstream ss;
    std::map<std::string,int> IDs;

    ss << "mesh_and_adjncy_vtu mesh-complete/mesh-complete.mesh.vtu\n";

    //set ids
    //================================================================
    int id=1;
    IDs["mesh-complete.exterior"]=id;
    std::string setIDPrefix="set_surface_id_vtp mesh-complete/mesh-surfaces/";
    auto inletProps=job->GetInletProps();
    int inletNumber=0;
    auto it = inletProps.begin();
    while(it != inletProps.end())
    {
        if(it->first!="")
        {
            inletNumber++;

            id++;
            IDs[it->first]=id;
            ss << setIDPrefix << it->first <<".vtp " << id <<"\n";
        }
        it++;
    }
    job->SetIDs(IDs);
    job->SetPrescribedCapNumber(inletNumber);

    auto outletProps=job->GetOutletProps();
    it = outletProps.begin();
    while(it != outletProps.end())
    {
        if(it->first!="")
        {
            id++;
            IDs[it->first]=id;
            ss << setIDPrefix << it->first <<".vtp " << id <<"\n";
        }
        it++;
    }

    //set bct for inlets
    //=================================================================
    auto basicProps=job->GetBasicProps();
    if(inletNumber>0)
    {
        ss << "fluid_density " << basicProps["Fluid Density"] <<"\n";
        ss << "fluid_viscosity " << basicProps["Fluid Viscosity"] <<"\n";
    }

    it = inletProps.begin();
    while(it != inletProps.end())
    {
        if(it->first!="")
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
            if(props["Flip Normal"]=="true")
            {
                ss << "bct_flip\n";
            }
            ss << "bct_create mesh-complete/mesh-surfaces/" << it->first << ".vtp " << it->first <<".flow\n";
        }
        it++;
    }
    if(inletNumber>1)
    {
        ss << "bct_merge_on\n";
    }

    ss << "bct_write_dat " << outputDir << "/bct.dat\n";
    ss << "bct_write_vtp " << outputDir << "/bct.vtp\n";

    //set outlets
    //====================================================================
    it = outletProps.begin();
    while(it != outletProps.end())
    {
        if(it->first!="")
        {
            auto props=it->second;
            ss << "pressure_vtp mesh-complete/mesh-surfaces/" << it->first << ".vtp " << props["Pressure"] << "\n";
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
        ss << "wall_displacements_write_vtp " << outputDir <<"/displacement.vtp" << "\n";
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

        ss << "varwallprop_write_vtp " << outputDir <<"/varwallprop.vtp" << "\n";

        ss << "deformable_nu " << wallProps["Poisson Ratio"] << "\n";
        ss << "deformable_kcons " << wallProps["Shear Constant"] << "\n";
        ss << "deformable_pressure " << wallProps["Pressure"] << "\n";
        ss << "deformable_solve_varwall_displacements\n";
        ss << "wall_displacements_write_vtp " << outputDir <<"/displacement.vtp" << "\n";
    }

    ss << "write_geombc " << outputDir <<"/geombc.dat.1" << "\n";
    ss << "write_restart " << outputDir <<"/restart.0.1" << "\n";
    if(deformable)
    {
        ss << "append_displacements " << outputDir <<"/restart.0.1" << "\n";
    }

    ss << "write_numstart " << outputDir <<"/numstart.dat" << "\n";

    return ss.str();
}

std::string svSimulationUtils::CreateRCRTFileContent(svSimJob* job)
{
    std::stringstream ss;
    auto basicProps=job->GetBasicProps();

    ss << "2\n";

    auto outletProps=job->GetOutletProps();
    auto it = outletProps.begin();
    while(it != outletProps.end())
    {
        if(it->first!="")
        {
            auto props=it->second;
            if(props["Type"]=="RCR")
            {
                auto values=sv::split(props["Values"]);
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

    return ss.str();
}

std::string svSimulationUtils::CreateFlowSolverFileContent(svSimJob* job)
{
    std::stringstream ss;
    std::map<std::string,int> IDs=job->GetIDs();

    auto basicProps=job->GetBasicProps();
    auto solverProps=job->GetSolverProps();

    //Time Steps
    //======================================================
    ss << "Number of Timesteps: " << solverProps["Number of Timesteps"] <<"\n";
    ss << "Time Step Size: " << solverProps["Time Step Size"] <<"\n";

    //Output Control
    //======================================================
    ss << "Number of Timesteps between Restarts: " << solverProps["Number of Timesteps between Restarts"] <<"\n";
    ss << "Print Average Solution: " << solverProps["Output Average Solution"] <<"\n";
    ss << "Print Error Indicators: " << solverProps["Output Error Indicators"] <<"\n";
    if(solverProps["Output Surface Stress"]=="True")
    {
        ss << "Number of Force Surfaces: 1\n";
        ss << "Surface ID's for Force Calculation: 1\n";
        ss << "Force Calculation Method: " << solverProps["Surface Stress Calculation Method"] <<"\n";
    }

    //Fluid Properties
    //======================================================
    ss << "Density: " << basicProps["Fluid Density"] <<"\n";
    ss << "Viscosity: " << basicProps["Fluid Viscosity"] <<"\n";

    //BCT Prescribed Velocities
    //======================================================
    if(job->GetPrescribedCapNumber()>0)
        ss << "Time Varying Boundary Conditions From File: True\n";
    else
        ss << "Time Varying Boundary Conditions From File: False\n";

    //Step Construction
    //=====================================================
    int stepNumber=std::stoi(solverProps["Step Construction"]);
    std::string stepStr=solverProps["Step Construction"];
    if(stepNumber>0)
    {
        ss << "Step Construction:";
        for(int i=0;i<stepNumber;i++)
            ss << " 0 1";

        ss << "\n";
    }

    //BC: R, RCR, COR, IMP, CLOSELOOP, BACKFLOW
    //==================================================
    ss << "Pressure Coupling: " << solverProps["Pressure Coupling"] <<"\n";
//    ss << "Number of Coupled Surfaces: " <<    <<"\n";


    //Deformable
    //==================================================
    ss << "Density of Vessel Wall: " << solverProps["Density of Vessel Wall"] <<"\n";

    //Advanced
    //======================================================
    //Non-linear Iteration Control
    //--------------------------------------------------
    ss << "Residual Control: " << solverProps["Residual Control"] <<"\n";
    ss << "Residual Criteria: " << solverProps["Residual Criteria"] <<"\n";
    ss << "Minimum Required Iterations: " << solverProps["Minimum Required Iterations"] <<"\n";
    //Linear Solver
    //--------------------------------------------------
    ss << "svLS Type: " << solverProps["Linear Solver Type"] <<"\n";
    ss << "Number of Krylov Vectors per GMRES Sweep: " << solverProps["Number of Krylov Vectors per GMRES Sweep"] <<"\n";
    ss << "Number of Solves per Left-hand-side Formation: " << solverProps["Number of Solves per Left-hand-side Formation"] <<"\n";
    ss << "Tolerance on Momentum Equations: " << solverProps["Tolerance on Momentum Equations"] <<"\n";
    ss << "Tolerance on Continuity Equations: " << solverProps["Tolerance on Continuity Equations"] <<"\n";
    ss << "Tolerance on svLS NS Solver: " << solverProps["Tolerance on svLS NS Solver"] <<"\n";
    ss << "Maximum Number of Iterations for svLS NS Solver: " << solverProps["Maximum Number of Iterations for svLS NS Solver"] <<"\n";
    ss << "Maximum Number of Iterations for svLS Momentum Loop: " << solverProps["Maximum Number of Iterations for svLS Momentum Loop"] <<"\n";
    ss << "Maximum Number of Iterations for svLS Continuity Loop: " << solverProps["Maximum Number of Iterations for svLS Continuity Loop"] <<"\n";
    //Discretiztion Control
    //----------------------------------------------------
    ss << "Time Integration Rule: " << solverProps["Time Integration Rule"] <<"\n";
    ss << "Time Integration Rho Infinity: " << solverProps["Time Integration Rho Infinity"] <<"\n";
    ss << "Flow Advection Form: " << solverProps["Flow Advection Form"] <<"\n";
    ss << "Quadrature Rule on Interior: " << solverProps["Quadrature Rule on Interior"] <<"\n";
    ss << "Quadrature Rule on Boundary: " << solverProps["Quadrature Rule on Boundary"] <<"\n";


    return ss.str();
}
