#ifndef SVMESHFOLDER_H
#define SVMESHFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svMeshFolder : public svDataFolder
{
public:

    mitkClassMacro(svMeshFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svMeshFolder(){}
    svMeshFolder(const svMeshFolder &other) : svDataFolder(other) { }
    virtual ~svMeshFolder(){}

};


#endif // SVMESHFOLDER_H
