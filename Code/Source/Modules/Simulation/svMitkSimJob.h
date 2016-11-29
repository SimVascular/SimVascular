#ifndef SVMITKSIMJOB_H
#define SVMITKSIMJOB_H

#include <svSimulationExports.h>

#include "svSimJob.h"

#include "mitkBaseData.h"

class SVSIMULATION_EXPORT svMitkSimJob : public mitk::BaseData
{
public:

    mitkClassMacro(svMitkSimJob, mitk::BaseData);
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

    svSimJob* GetSimJob(unsigned int t=0) const;

    void SetSimJob(svSimJob* job, unsigned int t=0);

    void SetMeshName(std::string meshName);

    std::string GetMeshName() const;

    void SetModelName(std::string modelName);

    std::string GetModelName() const;

    std::string GetStatus() const;

    void SetStatus(std::string status);

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

  protected:

    mitkCloneMacro(Self);

    svMitkSimJob();
    svMitkSimJob(const svMitkSimJob &other);
    virtual ~svMitkSimJob();

//    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

//    svSimJob* m_Job;
    std::vector<svSimJob*> m_JobSet;

    bool m_CalculateBoundingBox;

    std::string m_MeshName;

    std::string m_ModelName;

    std::string m_Status;

    bool m_DataModified;
};

itkEventMacro( svMitkSimJobEvent, itk::AnyEvent );

#endif // SVMITKSIMJOB_H
