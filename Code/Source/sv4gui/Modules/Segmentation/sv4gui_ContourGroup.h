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

#ifndef SV4GUI_CONTOURGROUP_H
#define SV4GUI_CONTOURGROUP_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_Contour.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"

#include <map>
#include <sstream>
#include <iostream>
#include <string>

struct SV4GUIMODULESEGMENTATION_EXPORT svLoftingParam
{
    std::string method;

    //Spline Lofting
    int numOutPtsInSegs;//sampleDefault
    //std::vector<int> overrides;
    int samplePerSegment;
    int useLinearSampleAlongLength;
    int linearMuliplier;
    int useFFT;
    int numModes;

    int addCaps;
    //int noInterOut;
    int vecFlag;

    //int numSegs=0;
    int numOutPtsAlongLength;//=samplePerSegment*numSegs
    int numPtsInLinearSampleAlongLength;//=linearMuliplier*numOutPtsAlongLength
    int splineType;

    int numSuperPts;//the number of points of the contour with the maximum point number

    double bias;
    double tension;
    double continuity;

    //Nurbs Lofting
    int uDegree;
    int vDegree;
    std::string uKnotSpanType;
    std::string vKnotSpanType;
    std::string uParametricSpanType;
    std::string vParametricSpanType;

    svLoftingParam()
        : method("nurbs")
        , numOutPtsInSegs(60)
        , samplePerSegment(12)
        , useLinearSampleAlongLength(1)
        , linearMuliplier(10)
        , useFFT(0)
        , numModes(20)
        , addCaps(0)
        //, noInterOut(1)
        , vecFlag(0)
        , numOutPtsAlongLength(0)
        , numPtsInLinearSampleAlongLength(0)
        , splineType(0)
        , numSuperPts(0)
        , bias(0)
        , tension(0)
        , continuity(0)
        , uDegree(2)
        , vDegree(2)
        , uKnotSpanType("derivative")
        , vKnotSpanType("average")
        , uParametricSpanType("centripetal")
        , vParametricSpanType("chord")

    {}

    svLoftingParam(const svLoftingParam &other)
        : method(other.method)
        , numOutPtsInSegs(other.numOutPtsInSegs)
        , samplePerSegment(other.samplePerSegment)
        , useLinearSampleAlongLength(other.useLinearSampleAlongLength)
        , linearMuliplier(other.linearMuliplier)
        , useFFT(other.useFFT)
        , numModes(other.numModes)
        , addCaps(other.addCaps)
        //, noInterOut(other.noInterOut)
        , vecFlag(other.vecFlag)
        , numOutPtsAlongLength(other.numOutPtsAlongLength)
        , numPtsInLinearSampleAlongLength(other.numPtsInLinearSampleAlongLength)
        , splineType(other.splineType)
        , numSuperPts(other.numSuperPts)
        , bias(other.bias)
        , tension(other.tension)
        , continuity(other.continuity)
        , uDegree(other.uDegree)
        , vDegree(other.vDegree)
        , uKnotSpanType(other.uKnotSpanType)
        , vKnotSpanType(other.vKnotSpanType)
        , uParametricSpanType(other.uParametricSpanType)
        , vParametricSpanType(other.vParametricSpanType)
    {}


};

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourGroup : public mitk::BaseData
{
public:

    mitkClassMacro(sv4guiContourGroup, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps ) override;

    virtual void ExecuteOperation(mitk::Operation* operation) override;

    virtual unsigned int GetTimeSize() const;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    virtual int GetSize( unsigned int t = 0 ) const;

    virtual sv4guiContour* GetContour(int contourIndex, unsigned int t = 0) const;

    void InsertControlPoint(int contourIndex, int index, mitk::Point3D point, unsigned int t = 0 );

    void RemoveControlPoint(int contourIndex, int index, unsigned int t = 0);

    void SetControlPoint(int contourIndex, int index, mitk::Point3D point, unsigned int t = 0);

    void SetControlPointSelectedIndex(int contourIndex, int index, unsigned int t = 0);

    void DeselectControlPoint(unsigned int t = 0);

    int GetControlPointSelectedIndex(int contourIndex, unsigned int t = 0);

    void InsertContour(int contourIndex, sv4guiContour* contour, unsigned int t = 0);

    void RemoveContour(int contourIndex, unsigned int t = 0);

    void RemoveInvalidContours(unsigned int t = 0);

    void SetContour(int contourIndex, sv4guiContour* contour, unsigned int t = 0);

    bool IsContourSelected(int contourIndex, unsigned int t = 0);

    void SetContourSelected(int contourIndex, bool selected = true, unsigned int t = 0 );

    int GetSelectedContourIndex(unsigned int t = 0);

    void DeselectContours(unsigned int t = 0);

    void ContourControlPointsChanged(unsigned t = 0);

    void ContoursChanged(unsigned t = 0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    sv4guiContour* GetUnplacedContour(unsigned int t = 0);

    sv4guiContour* GetContourOnPlane(const mitk::PlaneGeometry *planeGeometry, double precisionFactor=0.1, unsigned int t = 0);

    int GetUnplacedContourIndex(unsigned int t = 0);

    int SearchContourByPlane(const mitk::PlaneGeometry *planeGeometry, double precisionFactor=0.1, unsigned int t = 0);

    int GetInsertingContourIndexByPathPosPoint(mitk::Point3D posPoint, unsigned int t = 0);

    std::vector<sv4guiPathElement::sv4guiPathPoint>  GetContourPathPoints(unsigned int t = 0);

    std::vector<mitk::Point3D> GetContourPathPosPoints(unsigned int t = 0);

    int GetContourIndexByPathPosPoint(mitk::Point3D posPoint, unsigned int t = 0);

    int GetInsertingContourIndexByTagIndex(int tagIndex, unsigned int t = 0);

    //virtual methods, that need to be implemented
    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    //    int GetGroupID();
    //    void SetGroupID(int groupID);

        //get the max group ID of all the path nodes.
    //    static int GetMaxGroupID(mitk::DataStorage::SetOfObjects::ConstPointer rs);

    std::string GetPathName() const;
    void SetPathName(std::string name);

    int GetPathID() const;
    void SetPathID(int id);

    std::vector<sv4guiPathElement::sv4guiPathPoint>  GetPathPoints(unsigned int t =0);

    std::vector<mitk::Point3D> GetPathPosPoints(unsigned int t =0);

    int GetCurrentIndexOn2DView();

    void SetCurrentIndexOn2DView(int index);

    std::vector<sv4guiContour*> GetContourSet(unsigned int t = 0);

    std::vector<sv4guiContour*> GetValidContourSet(unsigned int t = 0);//contour points > 1

    svLoftingParam* GetLoftingParam() const {return m_LoftingParam;}

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

    double GetResliceSize() const {return m_ResliceSize;}

    void SetResliceSize(double size) {m_ResliceSize=size;}

    void SetProp(const std::string& key, std::string value);
    std::string GetProp(const std::string& key) const;
    std::map<std::string,std::string> GetProps() {return m_Props;}

  protected:

    mitkCloneMacro(Self);

    sv4guiContourGroup();
    sv4guiContourGroup(const sv4guiContourGroup &other);
    virtual ~sv4guiContourGroup();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

    std::vector< std::vector<sv4guiContour*> > m_ContourSets;

    bool m_CalculateBoundingBox;

    int m_GroupID;

//    std::string m_ContourGroupName;

    int m_PathID;

    std::string m_PathName;

    int m_CurrentIndexOn2DView;

    //lofting parameters as public
    svLoftingParam *m_LoftingParam;

    bool m_DataModified;

    double m_ResliceSize;

    std::map<std::string,std::string> m_Props;
  };

itkEventMacro( sv4guiContourGroupAllEvent, itk::AnyEvent );

itkEventMacro( sv4guiContourGroupEvent, sv4guiContourGroupAllEvent );
itkEventMacro( sv4guiContourEvent, sv4guiContourGroupAllEvent );

itkEventMacro( sv4guiContourSelectEvent, sv4guiContourGroupEvent );
itkEventMacro( sv4guiContourGroupChangeEvent, sv4guiContourGroupEvent );
itkEventMacro( sv4guiContourSetEvent, sv4guiContourGroupChangeEvent );
itkEventMacro( sv4guiContourGroupSizeChangeEvent, sv4guiContourGroupChangeEvent );
itkEventMacro( sv4guiContourInsertEvent, sv4guiContourGroupSizeChangeEvent );
itkEventMacro( sv4guiContourRemoveEvent, sv4guiContourGroupSizeChangeEvent );
itkEventMacro( sv4guiContourGroupExtendTimeRangeEvent, sv4guiContourGroupChangeEvent );

itkEventMacro( sv4guiContourPointSelectEvent, sv4guiContourEvent );
itkEventMacro( sv4guiContourChangeEvent, sv4guiContourEvent );
itkEventMacro( sv4guiContourPointMoveEvent, sv4guiContourChangeEvent );
itkEventMacro( sv4guiContourSizeChangeEvent, sv4guiContourChangeEvent );
itkEventMacro( sv4guiContourPointInsertEvent, sv4guiContourSizeChangeEvent );
itkEventMacro( sv4guiContourPointRemoveEvent, sv4guiContourSizeChangeEvent );

#endif // SV4GUI_CONTOURGROUP_H
