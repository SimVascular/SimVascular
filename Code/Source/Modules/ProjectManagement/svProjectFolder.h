#ifndef SVPROJECTFOLDER_H
#define SVPROJECTFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svProjectFolder : public svDataFolder
{
public:

    mitkClassMacro(svProjectFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svProjectFolder(){}
    svProjectFolder(const svProjectFolder &other) : svDataFolder(other) { }
    virtual ~svProjectFolder(){}

};


#endif // SVPROJECTFOLDER_H
