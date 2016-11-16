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

    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    svSimJob* GetSimJob() const;

    void SetSimJob(svSimJob* job);

    void SetMeshName(std::string meshName);

    std::string GetMeshName() const;

    void SetModelName(std::string modelName);

    std::string GetModelName() const;

  protected:

    mitkCloneMacro(Self);

    svMitkSimJob();
    svMitkSimJob(const svMitkSimJob &other);
    virtual ~svMitkSimJob();

//    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

    svSimJob* m_Job;

//    bool m_CalculateBoundingBox;

    std::string m_MeshName;

    std::string m_ModelName;

};

itkEventMacro( svMitkSimJobEvent, itk::AnyEvent );

#endif // SVMITKSIMJOB_H
