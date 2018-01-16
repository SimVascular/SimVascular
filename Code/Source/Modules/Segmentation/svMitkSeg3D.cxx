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

#include "svMitkSeg3D.h"
#include "svMitkSeg3DOperation.h"

svMitkSeg3D::svMitkSeg3D()
    : mitk::svSurface()
    , m_Seg3D(NULL)
    , m_DataModified(false)
{
}

svMitkSeg3D::svMitkSeg3D(const svMitkSeg3D &other)
    : mitk::svSurface(other)
    , m_DataModified(true)
{
    m_Seg3D=new svSeg3D(*(other.m_Seg3D),false);
    m_Seg3D->SetVtkPolyData(GetVtkPolyData());
}

svMitkSeg3D::~svMitkSeg3D()
{
    if(m_Seg3D)
        delete m_Seg3D;
}

bool svMitkSeg3D::IsEmptyTimeStep(unsigned int t) const
{
    return !IsInitialized();
}

void svMitkSeg3D::SetSeg3D(svSeg3D* seg3D)
{
    m_Seg3D=seg3D;

    SetVtkPolyData(seg3D->GetVtkPolyData());
    CalculateBoundingBox();
    Modified();

    this->InvokeEvent( svMitkSeg3DSetEvent() );
}

void svMitkSeg3D::ExecuteOperation( mitk::Operation* operation )
{

    svMitkSeg3DOperation* seg3DOperation = dynamic_cast<svMitkSeg3DOperation*>(operation);

    if(seg3DOperation==NULL)
    {
        MITK_ERROR << "No valid Operation for svMitkSeg3D" << std::endl;
        return;
    }

    svSeg3D* seg3D=seg3DOperation->GetSeg3D();

    if(seg3DOperation->GetOperationType()==svMitkSeg3DOperation::OpSETSEG3D)
    {
        m_DataModified=true;
        SetSeg3D(seg3D);
    }

    // mitk::OperationEndEvent endevent(operation);
    // ((const itk::Object*)this)->InvokeEvent(endevent);

}

