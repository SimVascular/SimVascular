#ifndef SVPROJECTFOLDER_H
#define SVPROJECTFOLDER_H

#include "svDataFolder.h"

#include "mitkBaseData.h"
#include "mitkDataNode.h"
#include "mitkDataStorage.h"

class svProjectFolder : public svDataFolder
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
