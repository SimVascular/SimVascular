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

#ifndef SV4GUI_PATHOPERATION_H
#define SV4GUI_PATHOPERATION_H

#include "SimVascular.h"

#include <sv4guiModulePathExports.h>

#include "mitkOperation.h"
#include "sv4gui_PathElement.h"

class SV4GUIMODULEPATH_EXPORT sv4guiPathOperation : public mitk::Operation
{
public:

    enum PathOperationType {OpINSERTCONTROLPOINT, OpREMOVECONTROLPOINT, OpMOVECONTROLPOINT, OpSELECTCONTROLPOINT, OpDESELECTALL, OpINSERTPATHELEMENT, OpREMOVEPATHELEMENT, OpSETPATHELEMENT};

    sv4guiPathOperation(mitk::OperationType operationType);

    sv4guiPathOperation(mitk::OperationType operationType, unsigned int timeStep);

    sv4guiPathOperation(mitk::OperationType operationType, mitk::Point3D point, int index);

    sv4guiPathOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index);

//    sv4guiPathOperation(mitk::OperationType operationType, int index, bool selected);

    sv4guiPathOperation(mitk::OperationType operationType, unsigned int timeStep, int index, bool selected);

    sv4guiPathOperation(mitk::OperationType operationType, sv4guiPathElement* pathElement);

    sv4guiPathOperation(mitk::OperationType operationType, unsigned int timeStep, sv4guiPathElement* pathElement);

    virtual ~sv4guiPathOperation();

    mitk::Point3D GetPoint();

    sv4guiPathElement* GetPathElement();

    int GetIndex();

    unsigned int GetTimeStep() const;

    bool GetSelected();

private:

    mitk::Point3D m_Point;

    sv4guiPathElement* m_PathElement;

    int m_Index;

    unsigned int m_TimeStep;

    bool m_Selected;

};

#endif // SV4GUI_PATHOPERATION_H
