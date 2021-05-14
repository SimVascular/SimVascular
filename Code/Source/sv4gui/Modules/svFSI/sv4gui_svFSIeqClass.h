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

#ifndef SV4GUI_SVFSIEQCLASS_H
#define SV4GUI_SVFSIEQCLASS_H

#include "sv4guiModulesvFSIExports.h"
#include "sv4gui_svFSIbcClass.h"

#define maxOutput 10
#define maxProp 10
#include <map>
#include <QStringList>
#include <vector>

class SV4GUIMODULESVFSI_EXPORT sv4guisvFSILinearSolverPreconditioner
{
  public:
    static std::string FSILS;
    static std::string ROW_COLUMN_SCALING;
    static std::string TRILINOS_DIAGONAL;
    static std::string TRILINOS_BLOCK_JACOBI;
    static std::string TRILINOS_ILU;
    static std::string TRILINOS_ILUT;
    static std::string TRILINOS_IC;
    static std::string TRILINOS_ICT;
    static std::string TRILINOS_ML;
    static std::vector<std::string> list;
};

//--------------------
// sv4guisvFSIeqClass
//--------------------
// The sv4guisvFSIeqClass is used to store properties for different simulation
// physics: fluid, solid, FSI, etc.
//
// Equation specific properties are store in
//
//    QStringList propNames;
//
//    double propVal[maxProp];
//
// Warning: Property names and values are referenced using an interger index into
// propNames[] and propVal[] so values stored at a given index must match.
// propNames[] sets the GUI widget label name in the Physics/Properties panel.
//
// See sv4guisvFSIView::SelectEquation().
//
class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIeqClass
{
  public:
    sv4guisvFSIeqClass(const QString& eq="none");
    sv4guisvFSIeqClass(const sv4guisvFSIeqClass& iEq) { *this = iEq; }
    ~sv4guisvFSIeqClass();

    // get and set functions for few common data between equations
    void setCoupled(const bool& coupledIn) { coupled = coupledIn; }
    void setMaxItr(const int& maxItrIn) { maxItr = maxItrIn; }
    void setMinItr(const int& minItrIn) { minItr = minItrIn; }
    void setTol(const QString& tolIn) { tol = tolIn; }
    void setdBr(const double& dBrIn) { dBr = dBrIn; }

    bool getCoupled() const { return coupled; }
    int getMaxItr() const { return maxItr; }
    int getMinItr() const { return minItr; }
    QString getTol() const { return tol; }
    double getdBr() const { return dBr; }
    QString getPhysName() const { return physName; }
    QString getDomainName() const { return domainName; }

    // output related functions
    bool getOutput(const QString& outputName ) const
    {
        int indx=searchOutput(outputName);
        if(indx>-1)
            return isOutsputed[indx];
        else
            return false;
    }

    void setOutput(const QString& outputName, const bool flag)
    {
        int indx=searchOutput(outputName);
        if(indx>-1)
            isOutsputed[indx] = flag;
    }
    void setOutputs( const QStringList& outputNameList );
    const QStringList getOutputNames() const;
    const QStringList getOutputCandidates() const;

    // properties related functions
    int getPropCount() const { return propNames.length(); }
    const QString getPropName(const int i ) const { return propNames.at(i); }
    double getPropValue( const int i ) const { return propVal[i]; }
    void setPropValue( const double val, const int i ) { propVal[i] = val; }
    void setPropValue( const double value, const QString propName);

    // Common properties
    bool coupled;
    int maxItr;
    int minItr;
    QString tol;
    double dBr;
    QString fullName;
    QString physName;

    // Outputs
    QStringList outputNames;
    bool isOutsputed[maxOutput];

    // Equation specific properties
    QStringList propNames;
    double propVal[maxProp];

    QString constitutiveModel;

    double backflowStab; //for fluid

    //linear solver
    QString lsType;
    int lsMaxItr;
    QString lsTol;
    int lsNSGMMaxItr;
    QString lsNSGMTol;
    int lsNSCGMaxItr;
    QString lsNSCGTol;
    int lsKrylovDim;
    QString lsAbsoluteTol;
    QString lsPreconditioner;

    //remesher
    QString remesher;//Tetgen, Meshsim
    std::map<std::string, double> rmMaxEdgeSizes; //domain name, size
    double rmMinAngle;
    double rmMaxRadiusRatio;
    int rmFrequency;
    int rmCopyFrequency;

    //    QList<sv4guisvFSIbcClass> bcs;
    std::map<std::string, sv4guisvFSIbcClass> faceBCs;

    QString domainName;

    QString domainName2; //for FSI

    int searchOutput(const QString& outputName) const;

};

#endif // SV4GUI_SVFSIEQCLASS_H
