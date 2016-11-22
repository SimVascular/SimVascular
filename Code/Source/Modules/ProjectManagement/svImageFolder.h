#ifndef SVIMAGEFOLDER_H
#define SVIMAGEFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svImageFolder : public svDataFolder
{
public:

    mitkClassMacro(svImageFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svImageFolder(){}
    svImageFolder(const svImageFolder &other) : svDataFolder(other) { }
    virtual ~svImageFolder(){}

};


#endif // SVIMAGEFOLDER_H
