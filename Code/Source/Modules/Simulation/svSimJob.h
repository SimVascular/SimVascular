#ifndef SVSIMJOB_H
#define SVSIMJOB_H

#include <svSimulationExports.h>

#include <map>
#include <sstream>
#include <iostream>
#include <string>

class SVSIMULATION_EXPORT svSimJob
{

public:

    svSimJob();

    svSimJob(const svSimJob &other);

    virtual ~svSimJob();

    virtual svSimJob* Clone();

//    std::string GetStatus();

//    void SetStatus(std::string status);

    void SetBasicProps(std::map<std::string,std::string> basicProps);
    std::map<std::string,std::string> GetBasicProps();
    void SetBasicProp(const std::string& key, std::string value);
    std::string GetBasicProp(const std::string& key);

    void SetInletProps(std::map<std::string,std::map<std::string,std::string>> inletProps);
    std::map<std::string,std::map<std::string,std::string>> GetInletProps();
    void SetInletProp(const std::string& inletName, const std::string& key, std::string value);
    std::string GetInletProp(const std::string& inletName, const std::string& key);

    void SetOutletProps(std::map<std::string,std::map<std::string,std::string>> outletProps);
    std::map<std::string,std::map<std::string,std::string>> GetOutletProps();
    void SetOutletProp(const std::string& outletName, const std::string& key, std::string value);
    std::string GetOutletProp(const std::string& outletName, const std::string& key);

    void SetWallProps(std::map<std::string,std::string> wallProps);
    std::map<std::string,std::string> GetWallProps();
    void SetWallProp(const std::string& key, std::string value);
    std::string GetWallProp(const std::string& key);

    void SetVarProps(std::map<std::string,std::map<std::string,std::string>> varProps);
    std::map<std::string,std::map<std::string,std::string>> GetVarProps();
    void SetVarProp(const std::string& faceName, const std::string& key, std::string value);
    std::string GetVarProp(const std::string& faceName, const std::string& key);

    void SetSolverProps(std::map<std::string,std::string> solverProps);
    std::map<std::string,std::string> GetSolverProps();
    void SetSolverProp(const std::string& key, std::string value);
    std::string GetSolverProp(const std::string& key);

    void SetRunProps(std::map<std::string,std::string> runProps);
    std::map<std::string,std::string> GetRunProps();
    void SetRunProp(const std::string& key, std::string value);
    std::string GetRunProp(const std::string& key);

    void SetIDs(std::map<std::string,int> IDs);
    std::map<std::string,int> GetIDs();

    void SetPrescribedCapNumber(int number);
    int GetPrescribedCapNumber();

  protected:

//    std::string m_Status;
    std::map<std::string,std::string> m_BasicProps;
    std::map<std::string,std::map<std::string,std::string>> m_InletProps;
    std::map<std::string,std::map<std::string,std::string>> m_OutletProps;
    std::map<std::string,std::string> m_WallProps;
    std::map<std::string,std::map<std::string,std::string>> m_VarProps;
    std::map<std::string,std::string> m_SolverProps;
    std::map<std::string,std::string> m_RunProps;

    std::map<std::string,int> m_IDs;

    int m_PrescribedCapNumber; //for caps with prescribed velosities

  };


#endif // SVSIMJOB_H
