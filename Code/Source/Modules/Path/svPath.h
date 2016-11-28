#ifndef SVPATH_H
#define SVPATH_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "svPathElement.h"
#include "svPathOperation.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"
#include "mitkDataNode.h"
#include "mitkDataStorage.h"

class SVPATH_EXPORT svPath : public mitk::BaseData
{
public:

    mitkClassMacro(svPath, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps ) override;

    virtual void ExecuteOperation(mitk::Operation* operation) override;

    virtual unsigned int GetTimeSize() const;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    virtual int GetSize( unsigned int t = 0 ) const;

    svPathElement* GetPathElement(unsigned int t = 0) const;

    void SetPathElement(svPathElement* pathElement, unsigned int t = 0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    int GetPathID() const;

    void SetPathID(int pathID);

//    std::string GetPathName();

//    void SetPathName(std::string pathName);

    //get the max path ID of all the path nodes.
    static int GetMaxPathID(mitk::DataStorage::SetOfObjects::ConstPointer rs);

    void SetSpacing(double spacing);

    double GetSpacing() const;

    void SetMethod(svPathElement::CalculationMethod method = svPathElement::CONSTANT_TOTAL_NUMBER );

    svPathElement::CalculationMethod GetMethod() const;

    void SetCalculationNumber(int number);

    mitk::Point3D GetNewControlPoint();

    int GetCalculationNumber() const;

    svPathOperation::PathOperationType GetOperationType();

    //virtual methods, that need to be implemented
    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    bool IsDataModified();
    void SetDataModified(bool modified = true);

  protected:

    mitkCloneMacro(Self);

    svPath();
    svPath(const svPath &other);
    virtual ~svPath();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

    std::vector< svPathElement* > m_PathElementSet;

    bool m_CalculateBoundingBox;

    int m_PathID;

//    std::string m_PathName;

    double m_Spacing;

    svPathElement::CalculationMethod m_Method;

    int m_CalculationNumber;

    mitk::Point3D m_NewControlPoint;

    svPathOperation::PathOperationType m_OperationType;

    bool m_DataModified;
  };

SVPATH_EXPORT bool Equal( const svPath* leftHandSide, const svPath* rightHandSide, mitk::ScalarType eps, bool verbose );
SVPATH_EXPORT bool Equal( const svPath& leftHandSide, const svPath& rightHandSide, mitk::ScalarType eps, bool verbose );

itkEventMacro( svPathEvent, itk::AnyEvent );

itkEventMacro( svPathExtendTimeRangeEvent, svPathEvent );
itkEventMacro( svPathSetEvent, svPathEvent );
itkEventMacro( svPathPointEvent, svPathEvent );

itkEventMacro( svPathPointSelectEvent, svPathPointEvent );
itkEventMacro( svPathPointMoveEvent, svPathPointEvent );
itkEventMacro( svPathSizeChangeEvent, svPathPointEvent );
itkEventMacro( svPathPointInsertEvent, svPathSizeChangeEvent );
itkEventMacro( svPathPointRemoveEvent, svPathSizeChangeEvent );


#endif // SVPATH_H
