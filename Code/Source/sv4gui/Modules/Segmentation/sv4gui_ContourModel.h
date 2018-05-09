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

#ifndef SV4GUI_CONTOURMODEL_H
#define SV4GUI_CONTOURMODEL_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_Contour.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourModel : public mitk::BaseData
{
public:

    mitkClassMacro(sv4guiContourModel, mitk::BaseData);
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

    void SetContour(sv4guiContour* contour, unsigned int t = 0);

    sv4guiContour* GetContour(unsigned int t = 0) const;

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

    sv4guiContourModel();
    sv4guiContourModel(const sv4guiContourModel &other);
    virtual ~sv4guiContourModel();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

    std::vector< sv4guiContour* > m_ContourSet;

    bool m_CalculateBoundingBox;

  };

//bool Equal( const sv4guiContourModel* leftHandSide, const sv4guiContourModel* rightHandSide, mitk::ScalarType eps, bool verbose );
//bool Equal( const sv4guiContourModel& leftHandSide, const sv4guiContourModel& rightHandSide, mitk::ScalarType eps, bool verbose );

itkEventMacro( sv4guiContourModelEvent, itk::AnyEvent );

itkEventMacro( sv4guiContourModelExtendTimeRangeEvent, sv4guiContourModelEvent );
itkEventMacro( sv4guiContourModelSetEvent, sv4guiContourModelEvent );
itkEventMacro( sv4guiContourModelPointEvent, sv4guiContourModelEvent );

itkEventMacro( sv4guiContourModelPointMoveEvent, sv4guiContourModelPointEvent );
itkEventMacro( sv4guiContourModelSizeChangeEvent, sv4guiContourModelPointEvent );
itkEventMacro( sv4guiContourModelPointInsertEvent, sv4guiContourModelSizeChangeEvent );
itkEventMacro( sv4guiContourModelPointRemoveEvent, sv4guiContourModelSizeChangeEvent );

#endif // SV4GUI_CONTOURMODEL_H
