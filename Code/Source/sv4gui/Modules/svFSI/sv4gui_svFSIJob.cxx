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

// The sv4guisvFSIJob methods defined here just write out simulation
// parameters to a file that is read in by the FSI solver/
//

#include "sv4gui_svFSIJob.h"

#include <QFile>
#include <QTextStream>

//----------------
// sv4guisvFSIJob
//----------------
//
sv4guisvFSIJob::sv4guisvFSIJob() : nsd(3), timeSteps(1000), stepSize("1e-3"), continuePrevious(false), 
    restartFileName("results"), restartInc(10), startSavingStep(1), 
    saveAvgResult(true), rhoInf(0.2), stopFileName("STOP_SIM"), verbose(true), warn(true), debug(false), 
    remeshing(false)
{
    vtkSaveResults = false;
    vtkFileName = "results";
    vtkInc = 10;
}

sv4guisvFSIJob::sv4guisvFSIJob(const sv4guisvFSIJob &other)
{
    *this=other;
}

sv4guisvFSIJob::~sv4guisvFSIJob()
{
}

sv4guisvFSIJob* sv4guisvFSIJob::Clone()
{
    return new sv4guisvFSIJob(*this);
}

//-----------
// WriteFile
//-----------
// Write simulation parameters to a plain text file.
//
bool sv4guisvFSIJob::WriteFile(std::string filePath)
{
    std::cout << "writing svFSI input file\n";

    if ( (m_Domains.size() <= 0) || (m_Domains.size() > 2)) {
        return false;
    }

    QFile file(QString::fromStdString(filePath));
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        return false;
    }

    QTextStream out(&file);

    QString tabS="   ";
    QString endL="\n";
    QString formatVersion="#Format Version 1.0";

    out << formatVersion << endL;
    out << "# General simulation parameters" << endL;
    out << "# -----------------------------------------------------" << endL;
    out << "Number of spatial dimensions: " << nsd << endL;
    out << "Number of time steps: " << timeSteps << endL;
    out << "Time step size: " << QString::fromStdString(stepSize) << endL;
    out << "Continue previous simulation: " << continuePrevious << endL;

    // [DaveP] old format
    // out << "Save results in a folder: " << saveInFoder << endL;
    // out << "Name prefix of saved files: " << QString::fromStdString(resultPrefix) << endL;
    // out << "Increment in saving files: " << resultInc << endL;

    // Restart 
    out << "Restart file name: " << QString::fromStdString(restartFileName) << endL;
    out << "Increment in saving restart files: " << restartInc << endL;
    out << "Start saving after time step: " << startSavingStep << endL;
    out << "Save averaged results: " << saveAvgResult << endL;

    // vtk output
    out << "Save results to VTK format: " << vtkSaveResults << endL;
    out << "Name prefix of saved VTK files: " << QString::fromStdString(vtkFileName) << endL;
    out << "Increment in saving VTK files: " << vtkInc << endL;

    // Advanced 
    out << "Spectral radius of infinite time step: " << rhoInf << endL;

    out << "Searched file name to trigger stop: " << QString::fromStdString(stopFileName) << endL;
    out << "Simulation requires remeshing: " << (remeshing?"T":"F") << endL;
    out << "Verbose: " << verbose << endL;
    out << "Warning: " << warn << endL;
    out << "Debug: " << debug << endL << endL;

    out << "# Domains" << endL;
    out << "#------------------------------------------------------" << endL;

    // Make sure the first domain is fluid if there are two domains.

    std::vector<sv4guisvFSIDomain> domains;
    if (m_Domains.size()==1){
      for(auto& pair : m_Domains) {
          domains.push_back(pair.second);
      }

    } else if (m_Domains.size()==2){
      domains.resize(2);
      for (auto& pair : m_Domains) {
          if (pair.second.type=="fluid") {
              domains[0]=pair.second;
          } else {
              domains[1]=pair.second;
          }
      }
    }

    // Add meshes.
    //
    int domainID=0;
    for (auto& domain : domains) {
        out << "Add mesh: "<< QString::fromStdString(domain.name) <<" {" << endL;
        out << tabS << "Mesh file path: " << QString::fromStdString(domain.folderName) << "/" 
            << QString::fromStdString(domain.fileName)<< endL;
        domainID++;

        for ( auto& faceName : domain.faceNames ) {
            out << tabS << "Add face: " << QString::fromStdString(faceName) << " {" << endL;
            out << tabS << tabS << "Face file path: " << QString::fromStdString(domain.folderName)
                << "/" << QString::fromStdString(domain.faceFolderName) << "/" 
                << QString::fromStdString(faceName+".vtp") << endL;
            out << tabS << "}" << endL;
        }

        out << tabS << "Domain: " << domainID << endL;
        out << "}" << endL;
    }
    out << endL;

    // Add projection.
    //
    for (sv4guisvFSIeqClass& eq: m_Eqs) {
        if(eq.physName=="FSI") {
            for( auto& fbc : eq.faceBCs ) {
                sv4guisvFSIbcClass& iBc=fbc.second;
                if(iBc.bcType=="Projection") {
                    out << "Add projection: " << iBc.faceName << " { Project from face: " 
                        << iBc.projectionFaceName << " }"<< endL;
                }
            }
        }
    }
    out << endL;

    // Add equations.
    //
    out << "# Equations" << endL;
    out << "#------------------------------------------------------" << endL;

    for (sv4guisvFSIeqClass& eq: m_Eqs) {
        out << "Add equation: " << eq.physName << " {" << endL;
        out << tabS << "Coupled: " << eq.coupled << endL;
        out << tabS << "Min iterations: "  <<  eq.minItr << endL;
        out << tabS << "Max iterations: "  <<  eq.maxItr << endL;
        out << tabS << "Tolerance: "  <<  eq.tol << endL;

        // [DaveP] old format
        // out << tabS << "Residual dB reduction: "  << eq.dBr << endL;

        if (eq.physName == "fluid") {
            out << tabS << "Backflow stabilization coefficient: "  << eq.backflowStab << endL;
        }

        out << endL;

        // Write FSI parameters.
        //
        if (eq.getPhysName() == "FSI") {

            if (eq.remesher != "None") {
                out << tabS << "Remesher: "<< eq.remesher <<" { " << endL;
                for (auto& pair : m_Domains) {
                    std::string domainName=pair.first;
                    sv4guisvFSIDomain& domain=pair.second;
                    out << tabS << tabS << "Max edge size: " << QString::fromStdString(domainName) 
                        << " { val: " << domain.edgeSize <<" }" << endL;
                }

                out << tabS << tabS << "Min dihedral angle: " << eq.rmMinAngle <<endL;
                out << tabS << tabS << "Max radius ratio: " << eq.rmMaxRadiusRatio << endL;
                out << tabS << tabS << "Remesh frequency: " << eq.rmFrequency << endL;
                out << tabS << tabS << "Frequency for copying data: "  << eq.rmCopyFrequency << endL;
                out << tabS << "}" << endL << endL;
            }

            // [TODO:DaveP] Is Domain 1 always a fluid?
            out << tabS << "Domain: 1 { " << endL;
            out << tabS << tabS << "Equation: fluid" << endL;
            out << tabS << tabS << "Density: " << eq.getPropValue(0) << endL;
            out << tabS << tabS << "Viscosity: Constant {Value: " << eq.getPropValue(1) << "}" << endL;
            out << tabS << tabS << "Backflow stabilization coefficient: "  << eq.backflowStab << endL;
            out << tabS << "}" << endL;
            out << endL;

            out << tabS << "Domain: 2 { " << endL;
            out << tabS << tabS << "Equation: struct" << endL;
            out << tabS << tabS << "Constitutive model: " << eq.constitutiveModel << endL;
            out << tabS << tabS << "Density: " << eq.getPropValue(2) << endL;
            out << tabS << tabS << "Elasticity modulus: " << eq.getPropValue(3) << endL;
            out << tabS << tabS << "Poisson ratio: " << eq.getPropValue(4) << endL;
            out << tabS << "}" << endL;
        } else {
            for ( int i=0 ; i < eq.getPropCount() ; i++ ) {
                out << tabS << eq.getPropName(i) << ": " << eq.getPropValue(i) << endL  ;
            }

            if (eq.getPhysName()=="struct"){
                out << tabS << "Constitutive model: " << eq.constitutiveModel << endL  ;
            }
        }
        out << endL;

        // Linear solver. 
        //
        if (eq.physName != "mesh") {
            out << tabS << "LS type: " << eq.lsType << " {" << endL;
            if ((eq.lsPreconditioner != "") && (eq.lsPreconditioner != "Default")) {
                out << tabS << tabS << "Preconditioner: " << eq.lsPreconditioner << endL;
            }
            out << tabS << tabS << "Max iterations: " << eq.lsMaxItr << endL;
            out << tabS << tabS << "Tolerance: " << eq.lsTol << endL;

            if(eq.lsType=="NS") {
                out << tabS << tabS << "NS-GM max iterations: " << eq.lsNSGMMaxItr << endL;
                out << tabS << tabS << "NS-GM tolerance: " << eq.lsNSGMTol << endL;
                out << tabS << tabS << "NS-CG max iterations: " << eq.lsNSCGMaxItr << endL;
                out << tabS << tabS << "NS-CG tolerance: " << eq.lsNSCGTol << endL;
            }
            out << tabS << tabS << "Absolute tolerance: " << eq.lsAbsoluteTol << endL;
            out << tabS << tabS << "Krylov space dimension: " << eq.lsKrylovDim << endL;
            out << tabS << "}" << endL << endL;
        }

        out << tabS << "Output: Spatial {" << endL;
        foreach ( QString outName , eq.getOutputNames() ) {
            out << tabS << tabS << outName << ": t" << endL;
        }
        out << tabS << "}" << endL << endL;

        // Boundary conditions.
        //
        for( auto& fbc : eq.faceBCs ) {
            sv4guisvFSIbcClass& iBc=fbc.second;

            if (iBc.bcType=="Projection") {
                continue;
            }

            out << tabS << "Add BC: "  <<  iBc.faceName << " {" << endL;
            if ( iBc.bcGrp != "" ) {
                out << tabS << tabS << "Type: " << iBc.bcGrp << endL;
            } else {
                return false;
            }
            out << tabS << tabS << "Time dependence: " << iBc.bcType << endL;
            if ( iBc.bcType == "Steady" ) {
                out << tabS << tabS << "Value: " << iBc.g << endL;
            } else if ( iBc.bcType == "Unsteady" ) {
                out << tabS << tabS << "Temporal values file path: " << iBc.gtFile << endL;
            } else if ( iBc.bcType == "Resistance" ) {
                    out << tabS << tabS << "Value: " << iBc.r << endL;
            } else if ( iBc.bcType == "Coupled" ) {
                // Noting special is required
            } else if ( iBc.bcType == "General" ) {
                out << tabS << tabS << "Temporal and spatial values file path: " << iBc.gmFile << endL;
            }

            out << tabS << tabS << "Profile: " << iBc.profile << endL;
            if ( iBc.profile == "User_defined" ) {
                out << tabS << tabS << "Spatial profile file path: " << iBc.gxFile << endL;
            }

            if (iBc.zperm) {
                out << tabS << tabS << "Zero out perimeter: " << iBc.zperm << endL;
            }

            if (iBc.flux) {
                out << tabS << tabS << "Impose flux: " << iBc.flux << endL;
            }

            if (iBc.imposeIntegral) {
                out << tabS << tabS << "Impose on state variable integral: " << iBc.imposeIntegral << endL;
            }

            if (iBc.effectiveDirection.trimmed()!="") {
                QStringList list=iBc.effectiveDirection.trimmed().split(QRegExp("[(),{}-\\s+]"),QString::SkipEmptyParts);
                out << tabS << tabS << "Effective direction: (" << list[0] << ", " << list[1] <<", " << list[2] << ")" << endL;
            }

            out << tabS << "}" << endL << endL;
        }
        out << "}" << endL;
        out << endL;
    }

    file.close();

    return true;
}

