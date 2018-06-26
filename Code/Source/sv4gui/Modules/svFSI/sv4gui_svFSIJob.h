#ifndef sv4guisvFSIJOB_H
#define sv4guisvFSIJOB_H

#include <svFSIExports.h>

#include "sv4gui_eqClass.h"

#include <map>
#include <vector>
#include <sstream>
#include <iostream>
#include <string>

struct SVFSI_EXPORT svDomain
{
    std::string name;
    std::string folderName;
    std::string fileName;
    std::string type; //fluid, or solid for FSI
    int id;

    std::string faceFolderName;
    std::vector<std::string> faceNames;

    double edgeSize; //max edge size for remesher

    svDomain()
        : name("")
        , folderName("")
        , fileName("")
        , type("fluid")
        , faceFolderName("mesh-surfaces")
        , id(0)
        , edgeSize(2.6)
    {

    }

};

class SVFSI_EXPORT sv4guisvFSIJob
{

public:

    sv4guisvFSIJob();

    sv4guisvFSIJob(const sv4guisvFSIJob &other);

    virtual ~sv4guisvFSIJob();

    virtual sv4guisvFSIJob* Clone();

  public:

    int nsd;
    int timeSteps;
    std::string stepSize;
    bool continuePrevious;
    bool saveInFoder;
    int restartInc;
    std::string resultPrefix;
    int resultInc;
    int startSavingStep;
    bool saveAvgResult;
    double rhoInf;
    std::string stopFileName;
    bool verbose;
    bool warn;
    bool debug;

    bool remeshing;

    std::map<std::string,svDomain> m_Domains;

    std::vector<eqClass> m_Eqs;

    bool WriteFile(std::string filePath);

//    bool ReadFile(std::string filePath);

  };


#endif // sv4guisvFSIJOB_H
