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

#include "svContourOperation.h"

svContourOperation::svContourOperation(mitk::OperationType operationType, mitk::Point3D point, int contourIndex, int index)
    : mitk::Operation(operationType)
    , m_Point(point)
    , m_ContourIndex(contourIndex)
    , m_Index(index)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int contourIndex, int index)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Point(point)
    , m_ContourIndex(contourIndex)
    , m_Index(index)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, svContour* contour, int contourIndex)
    : mitk::Operation(operationType)
    , m_Contour(contour)
    , m_ContourIndex(contourIndex)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, svContour* contour, int contourIndex)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Contour(contour)
    , m_ContourIndex(contourIndex)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, mitk::Point3D point, int index)
    : mitk::Operation(operationType)
    , m_Point(point)
    , m_Index(index)
{
   }

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Point(point)
    , m_Index(index)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, svContour* contour)
    : mitk::Operation(operationType)
    , m_Contour(contour)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, svContour* contour)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Contour(contour)
{
}

svContourOperation::~svContourOperation()
{
}

mitk::Point3D svContourOperation::GetPoint()
{
    return m_Point;
}

svContour* svContourOperation::GetContour()
{
    return m_Contour;
}

int svContourOperation::GetContourIndex()
{
    return m_ContourIndex;
}

int svContourOperation::GetIndex()
{
    return m_Index;
}

unsigned int svContourOperation::GetTimeStep() const
{
    return m_TimeStep;
}
