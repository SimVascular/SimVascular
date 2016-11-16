#ifndef SVSEGMENTATIONFOLDER_H
#define SVSEGMENTATIONFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "svDataFolder.h"

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svSegmentationFolder : public svDataFolder
{
public:

    mitkClassMacro(svSegmentationFolder, svDataFolder);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

protected:

    mitkCloneMacro(Self);

    svSegmentationFolder(){}
    svSegmentationFolder(const svSegmentationFolder &other) : svDataFolder(other) { }
    virtual ~svSegmentationFolder(){}

};


#endif // SVSEGMENTATIONFOLDER_H
