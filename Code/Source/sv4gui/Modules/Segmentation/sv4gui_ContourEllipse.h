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

#ifndef SV4GUI_CONTOURELLIPSE_H
#define SV4GUI_CONTOURELLIPSE_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_Contour.h"

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourEllipse : public sv4guiContour
{

public:

    sv4guiContourEllipse();

    sv4guiContourEllipse(const sv4guiContourEllipse &other);

    virtual ~sv4guiContourEllipse();

    virtual sv4guiContourEllipse* Clone() override;

    virtual std::string GetClassName() override;

    virtual void SetControlPoint(int index, mitk::Point3D point) override;

    virtual void CreateContourPoints() override;

    bool AsCircle();

    void SetAsCircle(bool asCircle);

    void AssignCenterScalingPoints() override;

    virtual void PlaceControlPoints(mitk::Point3D point) override;

    static sv4guiContour* CreateByFitting(sv4guiContour* contour);

  protected:

    bool m_TreatAsCircle;

  };


#endif // SV4GUI_CONTOURELLIPSE_H
