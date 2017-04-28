#ifndef SVMODEL_H
#define SVMODEL_H

#include <svModelExports.h>

#include "svModelElement.h"
#include "svModelOperation.h"

#include "mitkBaseData.h"

class SVMODEL_EXPORT svModel : public mitk::BaseData
{
public:

    mitkClassMacro(svModel, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps = 1 ) override;

    virtual void ExecuteOperation(mitk::Operation *operation) override;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    virtual unsigned int GetTimeSize() const;

    svModelElement* GetModelElement(unsigned int t=0) const;

    void SetModelElement(svModelElement* modelElement, unsigned int t=0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    void SetType(std::string type);

    std::string GetType() const;

    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

  protected:

    mitkCloneMacro(Self);

    svModel();
    svModel(const svModel &other);
    virtual ~svModel();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

    std::vector< svModelElement* > m_ModelElementSet;

    bool m_CalculateBoundingBox;

    std::string m_Type;

    bool m_DataModified;
};

itkEventMacro( svModelEvent, itk::AnyEvent );

itkEventMacro( svModelExtendTimeRangeEvent, svModelEvent );
itkEventMacro( svModelSetEvent, svModelEvent );
itkEventMacro( svModelSetVtkPolyDataEvent, svModelEvent );

#endif // SVMODEL_H
