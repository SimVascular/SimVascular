//  Copyright, 2017
//  Hongzhi Lan

//     Copyright, 2013
//     Mahdi Esmaily Moghadam

//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//--------------------------------------------------------------------

#include "sv4gui_eqClass.h"

eqClass::eqClass(const QString& eq)
{
    for (int i=0; i < maxOutput ; i++ ) isOutsputed[i] = false;
    for (int i=0; i < maxProp ; i++ ) propVal[i] = 0.0;
    if ( eq == "none") {
        return;
    } else if ( eq == "Incomp. fluid" ||  eq == "fluid") {
        outputNames << "Displacement" << "Velocity" << "Acceleration" << "Pressure" << "WSS" << "Vorticity";
        fullName = "Incomp. fluid";
        physName = "fluid";
        isOutsputed[1]=true;
        isOutsputed[3]=true;
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

    lsType="NS";//NS, GMRES, CG, BICG
    lsMaxItr=10;
    lsTol="1e-4";
    lsNSGMMaxItr=5;
    lsNSGMTol="1e-4";
    lsNSCGMaxItr=500;
    lsNSCGTol="1e-4";
    lsKrylovDim=50;
    lsAbsoluteTol="1e-12";
    lsPreconditioner="";

    remesher="None";
    rmMinAngle=10.0;
    rmMaxRadiusRatio=1.3;
    rmFrequency=300;
    rmCopyFrequency=1;
}

eqClass::~eqClass()
{
    outputNames.clear();
}

void eqClass::setPropValue( const double value, const QString propName)
{
    int indx=-1;
    for (int i=0 ; i < propNames.length() ; i++)
        if ( propName == propNames.at(i) )
            indx=i;

    if(indx!=-1)
        propVal[indx]=value;
}


int eqClass::searchOutput(const QString &outputName) const
{
    for (int i=0 ; i < outputNames.length() ; i++)
        if ( outputName == outputNames.at(i) )
            return i;

    return -1;
}

const QStringList eqClass::getOutputNames() const
{
    QStringList res;
    for (int i=0 ; i < outputNames.length() ; i++) {
        if ( isOutsputed[i] ) res.append(outputNames.at(i));
    }
    return res;
}

const QStringList eqClass::getOutputCandidates() const
{
    QStringList res;
    for (int i=0 ; i < outputNames.length() ; i++) {
        if ( !isOutsputed[i] ) res.append(outputNames.at(i));
    }
    return res;
}

void eqClass::setOutputs(const QStringList& outputNameList) {
    for ( int i=0 ; i < maxOutput ; i++ ) isOutsputed[i] = false;
    for ( int i=0 ; i < outputNameList.length() ; i++ ) setOutput(outputNameList.at(i),true);
}
