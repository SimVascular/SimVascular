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

#ifndef SV4GUI_CONTOUROPERATION_H
#define SV4GUI_CONTOUROPERATION_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "mitkOperation.h"
#include "sv4gui_Contour.h"

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourOperation : public mitk::Operation
{
public:

    enum ContourOperationType {OpINSERTCONTROLPOINT, OpREMOVECONTROLPOINT, OpMOVECONTROLPOINT, OpINSERTCONTOUR, OpREMOVECONTOUR, OpSETCONTOUR};

    sv4guiContourOperation(mitk::OperationType operationType, mitk::Point3D point, int coutourIndex, int index);

    sv4guiContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int coutourIndex, int index);

    sv4guiContourOperation(mitk::OperationType operationType, sv4guiContour* contour, int coutourIndex);

    sv4guiContourOperation(mitk::OperationType operationType, unsigned int timeStep, sv4guiContour* contour, int coutourIndex);

    sv4guiContourOperation(mitk::OperationType operationType, mitk::Point3D point, int index);

    sv4guiContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index);

    sv4guiContourOperation(mitk::OperationType operationType, sv4guiContour* contour);

    sv4guiContourOperation(mitk::OperationType operationType, unsigned int timeStep, sv4guiContour* contour);

    virtual ~sv4guiContourOperation();

    mitk::Point3D GetPoint();

    sv4guiContour* GetContour();

    int GetContourIndex();

    int GetIndex();

    unsigned int GetTimeStep() const;

private:

    mitk::Point3D m_Point;

    sv4guiContour* m_Contour;

    int m_ContourIndex;

    int m_Index;

    unsigned int m_TimeStep;

};

#endif // SV4GUI_CONTOUROPERATION_H
