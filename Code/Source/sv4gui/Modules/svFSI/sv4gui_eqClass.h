//  Copyright, 2017
//  Hongzhi Lan

//     Copyright, 2013
//     Mahdi Esmaily Moghadam

//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//--------------------------------------------------------------------

#ifndef EQCLASS_H
#define EQCLASS_H
#define maxOutput 10
#define maxProp 10
#include <map>
#include <QStringList>

class bcClass {
public:
    QString faceName;

    QString bcGrp;
    QString bcType;
    QString tDep;
    QString profile;
    int eDrn;
    int cplBCPtr;
    int faIn;
    double r;
    double g;
    QString gxFile;
    QString gmFile;
    QString gtFile;
    bool zperm;
    bool flux;

    QString projectionFaceName;

    bool imposeIntegral;
    QString effectiveDirection;

    bcClass()
    {
        faceName="";
        bcGrp = "NA";
        bcType = "Steady";
        profile = "Flat";
        eDrn = 0;
        cplBCPtr = -1;
        faIn = -1;
        r = 0.0;
        g = 0.0;
        gmFile = "";
        gtFile = "";
        gxFile = "";
        zperm = true;
        flux = false;
        projectionFaceName="";
        imposeIntegral=false;
        effectiveDirection="";
    }

    ~bcClass() {}
};

class eqClass
{
public:

    // constructor and destructor
    eqClass(const QString& eq="none");
    eqClass(const eqClass& iEq) { *this = iEq; }
    ~eqClass();

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

    //common properties
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
    //equation specific properties
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

    //    QList<bcClass> bcs;
    std::map<std::string, bcClass> faceBCs;

    QString domainName;

    QString domainName2; //for FSI

    int searchOutput(const QString& outputName) const;

};

#endif // EQCLASS_H
