#ifndef SVPATHFOLDER_H
#define SVPATHFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svPathFolder : public svDataFolder
{
public:

    mitkClassMacro(svPathFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svPathFolder(){}
    svPathFolder(const svPathFolder &other) : svDataFolder(other) { }
    virtual ~svPathFolder(){}

};


#endif // SVPATHFOLDER_H
