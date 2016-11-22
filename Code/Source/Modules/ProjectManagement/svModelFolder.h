#ifndef SVMODELFOLDER_H
#define SVMODELFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svModelFolder : public svDataFolder
{
public:

    mitkClassMacro(svModelFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svModelFolder(){}
    svModelFolder(const svModelFolder &other) : svDataFolder(other) { }
    virtual ~svModelFolder(){}

};


#endif // SVMODELFOLDER_H
