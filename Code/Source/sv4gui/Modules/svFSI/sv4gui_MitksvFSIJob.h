#ifndef sv4guiMitksvFSIJOB_H
#define sv4guiMitksvFSIJOB_H

#include <svFSIExports.h>

#include "sv4gui_svFSIJob.h"

#include <mitkBaseData.h>

class SVFSI_EXPORT sv4guiMitksvFSIJob : public mitk::BaseData
{
public:

    mitkClassMacro(sv4guiMitksvFSIJob, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps = 1 ) override;
//    virtual void ExecuteOperation(mitk::Operation *operation) override;
    virtual bool IsEmptyTimeStep(unsigned int t) const override;
    virtual unsigned int GetTimeSize() const;
    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    sv4guisvFSIJob* GetSimJob(unsigned int t=0) const;

    void SetSimJob(sv4guisvFSIJob* job, unsigned int t=0);

    void SetMeshName(std::string meshName);

    std::string GetMeshName() const;

    void SetModelName(std::string modelName);

    std::string GetModelName() const;

    std::string GetStatus() const;

    void SetStatus(std::string status);

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

    int GetProcessNumber() const {return m_ProcessNumber;}
    void SetProcessNumber(int number) {m_ProcessNumber=number;}

  protected:

    mitkCloneMacro(Self);

    sv4guiMitksvFSIJob();
    sv4guiMitksvFSIJob(const sv4guiMitksvFSIJob &other);
    virtual ~sv4guiMitksvFSIJob();

//    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

    std::vector<sv4guisvFSIJob*> m_JobSet;

    bool m_CalculateBoundingBox;

    std::string m_MeshName;

    std::string m_ModelName;

    std::string m_Status;

    bool m_DataModified;

    int m_ProcessNumber;
};

//itkEventMacro( sv4guiMitksvFSIJobEvent, itk::AnyEvent );

#endif // sv4guiMitksvFSIJOB_H
