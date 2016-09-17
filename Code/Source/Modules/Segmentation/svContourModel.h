#ifndef SVCONTOURMODEL_H
#define SVCONTOURMODEL_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContour.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"

class SVSEGMENTATION_EXPORT svContourModel : public mitk::BaseData
{
public:

    mitkClassMacro(svContourModel, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps ) override;

    virtual void ExecuteOperation(mitk::Operation* operation) override;

    virtual unsigned int GetTimeSize() const;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    void InsertControlPoint(int index, mitk::Point3D point, unsigned int t = 0 );

    void RemoveControlPoint(int index, unsigned int t = 0);

    void SetControlPoint(int index, mitk::Point3D point, unsigned int t = 0);

    void SetControlPointSelectedIndex(int index, unsigned int t = 0);

    void DeselectControlPoint(unsigned int t = 0);

    int GetControlPointSelectedIndex(unsigned int t = 0);

    void SetContour(svContour* contour, unsigned int t = 0);

    svContour* GetContour(unsigned int t = 0) const;

    void SetContourSelected(bool selected = true, unsigned int t = 0);

    void ControlPointsChanged(unsigned int t = 0);

    void ContoursChanged(unsigned int t = 0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

  protected:

    mitkCloneMacro(Self);

    svContourModel();
    svContourModel(const svContourModel &other);
    virtual ~svContourModel();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

    std::vector< svContour* > m_ContourSet;

    bool m_CalculateBoundingBox;

  };

//bool Equal( const svContourModel* leftHandSide, const svContourModel* rightHandSide, mitk::ScalarType eps, bool verbose );
//bool Equal( const svContourModel& leftHandSide, const svContourModel& rightHandSide, mitk::ScalarType eps, bool verbose );

itkEventMacro( svContourModelEvent, itk::AnyEvent );

itkEventMacro( svContourModelExtendTimeRangeEvent, svContourModelEvent );
itkEventMacro( svContourModelSetEvent, svContourModelEvent );
itkEventMacro( svContourModelPointEvent, svContourModelEvent );

itkEventMacro( svContourModelPointMoveEvent, svContourModelPointEvent );
itkEventMacro( svContourModelSizeChangeEvent, svContourModelPointEvent );
itkEventMacro( svContourModelPointInsertEvent, svContourModelSizeChangeEvent );
itkEventMacro( svContourModelPointRemoveEvent, svContourModelSizeChangeEvent );

#endif // SVCONTOURMODEL_H
