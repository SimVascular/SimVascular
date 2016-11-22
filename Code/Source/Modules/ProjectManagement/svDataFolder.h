#ifndef SVDATAFOLDER_H
#define SVDATAFOLDER_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "mitkBaseData.h"

class SVPROJECTMANAGEMENT_EXPORT svDataFolder : public mitk::BaseData
{
public:

    mitkClassMacro(svDataFolder, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

//    virtual void Expand( unsigned int timeSteps ) override;
//    virtual void ExecuteOperation(mitk::Operation* operation) override;
//    virtual bool IsEmptyTimeStep(unsigned int t) const override;
//    virtual void UpdateOutputInformation() override;

    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

//    bool isActive();
//    void SetActive(bool active);

protected:

    mitkCloneMacro(Self);

    svDataFolder();
    svDataFolder(const svDataFolder &other);
    virtual ~svDataFolder();

//    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
//    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

//    bool m_CalculateBoundingBox;

//    bool m_Active;

  };


#endif // SVDATAFOLDER_H
