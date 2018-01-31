/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

#include <map>
#include <sstream>
#include <iostream>
#include <string>

class SVPATH_EXPORT svPath : public mitk::BaseData
{
public:

    enum AddingMode {SMART=0, BEGINNING=1, END=2, BEFORE=3, AFTER=4};

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

    double GetResliceSize() const {return m_ResliceSize;}

    void SetResliceSize(double size) {m_ResliceSize=size;}

    svPathOperation::PathOperationType GetOperationType();

    //virtual methods, that need to be implemented
    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    bool IsDataModified();
    void SetDataModified(bool modified = true);

    AddingMode GetAddingMode() { return m_AddingMode;}
    void SetAddingMode(AddingMode mode) { m_AddingMode=mode;}

    void SetProp(const std::string& key, std::string value);
    std::string GetProp(const std::string& key) const;
    std::map<std::string,std::string> GetProps() {return m_Props;}

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

    double m_ResliceSize;

    AddingMode m_AddingMode;

    std::map<std::string,std::string> m_Props;
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
