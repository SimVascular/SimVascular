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

    void SetBasicProps(std::map<std::string,std::string> basicProps);
    std::map<std::string,std::string> GetBasicProps();
    void SetBasicProp(const std::string& key, std::string value);
    std::string GetBasicProp(const std::string& key);

    void SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps);
    std::map<std::string,std::map<std::string,std::string> > GetCapProps();
    void SetCapProp(const std::string& capName, const std::string& key, std::string value);
    std::string GetCapProp(const std::string& capName, const std::string& key);

    void SetWallProps(std::map<std::string,std::string> wallProps);
    std::map<std::string,std::string> GetWallProps();
    void SetWallProp(const std::string& key, std::string value);
    std::string GetWallProp(const std::string& key);

    void SetVarProps(std::map<std::string,std::map<std::string,std::string> > varProps);
    std::map<std::string,std::map<std::string,std::string> > GetVarProps();
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

    void SetVelocityCapNumber(int number);
    int GetVelocityCapNumber();

    void SetPressureCapNumber(int number);
    int GetPressureCapNumber();

  protected:

    std::map<std::string,std::string> m_BasicProps;
    std::map<std::string,std::map<std::string,std::string> > m_CapProps;
    std::map<std::string,std::string> m_WallProps;
    std::map<std::string,std::map<std::string,std::string> > m_VarProps;
    std::map<std::string,std::string> m_SolverProps;
    std::map<std::string,std::string> m_RunProps;

    std::map<std::string,int> m_IDs;

    int m_VelocityCapNumber; //for caps with prescribed velosities
    int m_PressureCapNumber; //for caps with prescribed velosities

  };


#endif // SVSIMJOB_H
