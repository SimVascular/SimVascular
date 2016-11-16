#ifndef SVSIMJOB_H
#define SVSIMJOB_H

#include <svSimulationExports.h>

#include <svModelElement.h>

#include <vtkSmartPointer.h>

#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>

#include <map>

class SVSIMULATION_EXPORT svSimJob
{

public:

    svSimJob();

    svSimJob(const svSimJob &other);

    virtual ~svSimJob();

    virtual svSimJob* Clone();

//    std::string GetStatus();

//    void SetStatus(std::string status);

    std::map<std::string,std::string>& GetBasicProps();
    void SetBasicProp(const std::string& key, std::string value);
    std::string GetBasicProp(const std::string& key);

    std::map<std::string,std::map<std::string,std::string>>& GetInletProps();
    void SetInletProp(const std::string& inletName, const std::string& key, std::string value);
    std::string GetInletProp(const std::string& inletName, const std::string& key);

    std::map<std::string,std::string>& GetOutletProps();
    void SetOutletProp(const std::string& key, std::string value);
    std::string GetOutletProp(const std::string& key);

    std::map<std::string,std::string>& GetWallProps();
    void SetWallProp(const std::string& key, std::string value);
    std::string GetWallProp(const std::string& key);

    std::map<std::string,std::string>& GetSolverProps();
    void SetSolverProp(const std::string& key, std::string value);
    std::string GetSolverProp(const std::string& key);

    std::map<std::string,std::string>& GetRunProps();
    void SetRunProp(const std::string& key, std::string value);
    std::string GetRunProp(const std::string& key);

  protected:

//    std::string m_Status;
    std::map<std::string,std::string> m_BasicProps;
    std::map<std::string,std::map<std::string,std::string>> m_InletProps;
    std::map<std::string,std::string> m_OutletProps;
    std::map<std::string,std::string> m_WallProps;
    std::map<std::string,std::string> m_SolverProps;
    std::map<std::string,std::string> m_RunProps;

  };


#endif // SVSIMJOB_H
