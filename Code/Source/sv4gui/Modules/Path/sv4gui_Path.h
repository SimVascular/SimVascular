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

#ifndef SV4GUI_PATH_H
#define SV4GUI_PATH_H

#include "SimVascular.h"

#include <sv4guiModulePathExports.h>

#include "sv3_PathElement.h"
#include "sv3_PathGroup.h"
#include "sv4gui_PathElement.h"
#include "sv4gui_PathOperation.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"
#include "mitkDataNode.h"
#include "mitkDataStorage.h"

#include <map>
#include <sstream>
#include <iostream>
#include <string>

class SV4GUIMODULEPATH_EXPORT sv4guiPath : public mitk::BaseData, public sv3::PathGroup
{
public:

    enum AddingMode {SMART=0, BEGINNING=1, END=2, BEFORE=3, AFTER=4};

    mitkClassMacro(sv4guiPath, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps ) override;

    virtual void ExecuteOperation(mitk::Operation* operation) override;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    sv4guiPathElement* GetPathElement(unsigned int t = 0) const;

    void SetPathElement(sv4guiPathElement* pathElement, unsigned int t = 0);
    
//    std::string GetPathName();

//    void SetPathName(std::string pathName);

    //get the max path ID of all the path nodes.
    static int GetMaxPathID(mitk::DataStorage::SetOfObjects::ConstPointer rs);

    mitk::Point3D GetNewControlPoint();

    double GetResliceSize() const {return m_ResliceSize;}

    void SetResliceSize(double size) {m_ResliceSize=size;}

    sv4guiPathOperation::PathOperationType GetOperationType();

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

    sv4guiPath();
    sv4guiPath(const sv4guiPath &other);
    virtual ~sv4guiPath();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

    mitk::Point3D m_NewControlPoint;

    sv4guiPathOperation::PathOperationType m_OperationType;

    bool m_DataModified;

    double m_ResliceSize;

    AddingMode m_AddingMode;

    std::map<std::string,std::string> m_Props;
  };

SV4GUIMODULEPATH_EXPORT bool Equal( const sv4guiPath* leftHandSide, const sv4guiPath* rightHandSide, mitk::ScalarType eps, bool verbose );
SV4GUIMODULEPATH_EXPORT bool Equal( const sv4guiPath& leftHandSide, const sv4guiPath& rightHandSide, mitk::ScalarType eps, bool verbose );

itkEventMacro( sv4guiPathEvent, itk::AnyEvent );

itkEventMacro( sv4guiPathExtendTimeRangeEvent, sv4guiPathEvent );
itkEventMacro( sv4guiPathSetEvent, sv4guiPathEvent );
itkEventMacro( sv4guiPathPointEvent, sv4guiPathEvent );

itkEventMacro( sv4guiPathPointSelectEvent, sv4guiPathPointEvent );
itkEventMacro( sv4guiPathPointMoveEvent, sv4guiPathPointEvent );
itkEventMacro( sv4guiPathSizeChangeEvent, sv4guiPathPointEvent );
itkEventMacro( sv4guiPathPointInsertEvent, sv4guiPathSizeChangeEvent );
itkEventMacro( sv4guiPathPointRemoveEvent, sv4guiPathSizeChangeEvent );


#endif // SV4GUI_PATH_H
