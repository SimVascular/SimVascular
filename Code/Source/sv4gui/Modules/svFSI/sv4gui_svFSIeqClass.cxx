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

// The methods defined here are used to set/get data for the various
// equations (fluid, structure, FSI, etc.) supported in the FSI solver.

#include "sv4gui_svFSIeqClass.h"

// Define the linear solver preconditionsers.
//
// These names must match those in READFILES.f:READLS().
//
std::string sv4guisvFSILinearSolverPreconditioner::FSILS = "fsils" ;
std::string sv4guisvFSILinearSolverPreconditioner::ROW_COLUMN_SCALING = "row-column-scaling";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_DIAGONAL = "trilinos-diagonal";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_BLOCK_JACOBI = "trilinos-blockjacobi";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_ILU = "trilinos-ilu";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_ILUT = "trilinos-ilut";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_IC = "trilinos-ic";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_ICT = "trilinos-ict";
std::string sv4guisvFSILinearSolverPreconditioner::TRILINOS_ML = "trilinos-ml";

std::vector<std::string> sv4guisvFSILinearSolverPreconditioner::list = {
  sv4guisvFSILinearSolverPreconditioner::FSILS,
  sv4guisvFSILinearSolverPreconditioner::ROW_COLUMN_SCALING,
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_BLOCK_JACOBI,
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_DIAGONAL, 
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_IC,
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_ICT,
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_ILU,
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_ILUT,
  sv4guisvFSILinearSolverPreconditioner::TRILINOS_ML,
};
            

//--------------------
// sv4guisvFSIeqClass
//--------------------
// Set class member data depending on 'eq' which is the type of
// physics (e.g. fluid) the equation represents.
//
// Warning: There is a dependancy between the names in propNames[] 
// and those in the GUI. 
//
sv4guisvFSIeqClass::sv4guisvFSIeqClass(const QString& eq)
{
    for (int i=0; i < maxOutput ; i++ ) {
        isOutsputed[i] = false;
    }

    for (int i=0; i < maxProp ; i++ ) {
        propVal[i] = 0.0;
    }

    if ( eq == "none") {
        return;

    } else if ( eq == "Incomp. fluid" ||  eq == "fluid") {
        outputNames << "Velocity" << "Acceleration" << "Pressure" << "WSS" << "Vorticity";
        fullName = "Incomp. fluid";
        physName = "fluid";

        isOutsputed[0]=true;
        isOutsputed[2]=true;

        propNames << "Density" << "Viscosity";
        propVal[0] = 1.0;
        propVal[1] = 0.04;

    } else if ( eq == "Structure" || eq == "struct") {
        outputNames << "Displacement" << "Velocity" << "Acceleration";
        fullName = "Structure";
        physName = "struct";

        isOutsputed[0]=true;
        isOutsputed[1]=true;

        propNames << "Density" << "Elasticity modulus" << "Poisson ratio";
        propVal[0] = 1.0;
        propVal[1] = 100000;
        propVal[2] = 0.4;

    } else if ( eq == "FSI" ) {
        outputNames << "Displacement" << "Velocity" << "Acceleration" << "Pressure" << "WSS" << "Vorticity";
        fullName = "FSI";
        physName = "FSI";

        isOutsputed[0]=true;
        isOutsputed[1]=true;
        isOutsputed[3]=true;

        propNames << "Fluid density" << "Viscosity" << "Solid density" << "Elasticity modulus" << "Poisson ratio";
        propVal[0] = 1.0;
        propVal[1] = 0.04;
        propVal[2] = 1.0;
        propVal[3] = 100000;
        propVal[4] = 0.4;

    } else if ( eq == "Mesh motion" || eq == "mesh") {
        outputNames << "Displacement" << "Velocity" << "Acceleration";
        fullName = "Mesh motion";
        physName = "mesh";

        isOutsputed[0]=true;

        propNames << "Poisson ratio";
        propVal[0] = 0.3;

    } else if ( eq == "Transport" || eq == "heatF") {
        outputNames << "Temperature";
        fullName = "Transport";
        physName = "heatF";
        isOutsputed[0]=true;
        propNames << "Conductivity";

    } else if ( eq == "Linear elasticity" || eq == "lElas") {
        outputNames << "Displacement" << "Velocity" << "Acceleration";
        fullName = "Linear elasticity";
        physName = "lElas";
        isOutsputed[0]=true;
        isOutsputed[1]=true;

        propNames << "Density" << "Elasticity modulus" << "Poisson ratio";
        propVal[0] = 1.0;
        propVal[1] = 100000;
        propVal[2] = 0.4;

    } else if ( eq == "Heat (Laplace)" || eq == "heatS" ) {
        outputNames << "Temperature";
        fullName = "Heat (Laplace)";
        physName = "heatS";
        isOutsputed[0]=true;
        propNames << "Conductivity";
    } else if ( eq == "Particles (BBO)" || eq == "BBO") {
        outputNames << "Velocity" << "Acceleration";
        fullName = "Particles (BBO)";
        physName = "BBO";
        isOutsputed[0]=true;
        isOutsputed[1]=true;
        propNames << "Particle density" << "Particle diameter" << "Force";
    } else if ( eq == "Electromagnetic" || eq == "elcMag") {
        outputNames << "Potential";
        fullName = "Electromagnetic";
        physName = "elcMag";
        isOutsputed[0]=true;
        propNames << "Force" << "Contact distance";
    }

    constitutiveModel="stVK";

    coupled = true;
    maxItr = 10;
    minItr = 1;
    tol = "1e-4";
    dBr = -20.0;

    backflowStab=0.3;

    //lsType="NS";//NS, GMRES, CG, BICG
    lsType="GMRES";
    lsMaxItr=10;
    lsTol="1e-4";
    lsNSGMMaxItr=5;
    lsNSGMTol="1e-4";
    lsNSCGMaxItr=500;
    lsNSCGTol="1e-4";
    lsKrylovDim=50;
    lsAbsoluteTol="1e-4";
    lsPreconditioner="Default";

    remesher="None";
    rmMinAngle=10.0;
    rmMaxRadiusRatio=1.3;
    rmFrequency=300;
    rmCopyFrequency=1;
}

sv4guisvFSIeqClass::~sv4guisvFSIeqClass()
{
    outputNames.clear();
}

//--------------
// setPropValue
//--------------
// Set the value of a property by its name.
//
void sv4guisvFSIeqClass::setPropValue( const double value, const QString propName)
{
    int indx = -1;

    for (int i=0 ; i < propNames.length() ; i++) {
        if ( propName == propNames.at(i) ) {
            indx=i;
        }
    }

    if(indx!=-1) {
        propVal[indx]=value;
    }
}


int sv4guisvFSIeqClass::searchOutput(const QString &outputName) const
{
    for (int i=0 ; i < outputNames.length() ; i++)
        if ( outputName == outputNames.at(i) )
            return i;

    return -1;
}

const QStringList sv4guisvFSIeqClass::getOutputNames() const
{
    QStringList res;
    for (int i=0 ; i < outputNames.length() ; i++) {
        if ( isOutsputed[i] ) res.append(outputNames.at(i));
    }
    return res;
}

const QStringList sv4guisvFSIeqClass::getOutputCandidates() const
{
    QStringList res;
    for (int i=0 ; i < outputNames.length() ; i++) {
        if ( !isOutsputed[i] ) res.append(outputNames.at(i));
    }
    return res;
}

void sv4guisvFSIeqClass::setOutputs(const QStringList& outputNameList) {
    for ( int i=0 ; i < maxOutput ; i++ ) isOutsputed[i] = false;
    for ( int i=0 ; i < outputNameList.length() ; i++ ) setOutput(outputNameList.at(i),true);
}
