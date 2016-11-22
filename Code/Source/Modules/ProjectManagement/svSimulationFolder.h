#ifndef SVSIMULATIONFOLDER_H
#define SVSIMULATIONFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svSimulationFolder : public svDataFolder
{
public:

    mitkClassMacro(svSimulationFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svSimulationFolder(){}
    svSimulationFolder(const svSimulationFolder &other) : svDataFolder(other) { }
    virtual ~svSimulationFolder(){}

};


#endif // SVSIMULATIONFOLDER_H
