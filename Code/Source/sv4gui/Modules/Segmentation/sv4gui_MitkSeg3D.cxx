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

#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_MitkSeg3DOperation.h"

sv4guiMitkSeg3D::sv4guiMitkSeg3D()
    : mitk::sv4guiSurface()
    , m_Seg3D(NULL)
    , m_DataModified(false)
{
}

sv4guiMitkSeg3D::sv4guiMitkSeg3D(const sv4guiMitkSeg3D &other)
    : mitk::sv4guiSurface(other)
    , m_DataModified(true)
{
    m_Seg3D=new sv4guiSeg3D(*(other.m_Seg3D),false);
    m_Seg3D->SetVtkPolyData(GetVtkPolyData());
}

sv4guiMitkSeg3D::~sv4guiMitkSeg3D()
{
    if(m_Seg3D)
        delete m_Seg3D;
}

bool sv4guiMitkSeg3D::IsEmptyTimeStep(unsigned int t) const
{
    return !IsInitialized();
}

void sv4guiMitkSeg3D::SetSeg3D(sv4guiSeg3D* seg3D)
{
    m_Seg3D=seg3D;

    SetVtkPolyData(seg3D->GetVtkPolyData());
    CalculateBoundingBox();
    Modified();

    this->InvokeEvent( sv4guiMitkSeg3DSetEvent() );
}

void sv4guiMitkSeg3D::ExecuteOperation( mitk::Operation* operation )
{

    sv4guiMitkSeg3DOperation* seg3DOperation = dynamic_cast<sv4guiMitkSeg3DOperation*>(operation);

    if(seg3DOperation==NULL)
    {
        MITK_ERROR << "No valid Operation for sv4guiMitkSeg3D" << std::endl;
        return;
    }

    sv4guiSeg3D* seg3D=seg3DOperation->GetSeg3D();

    if(seg3DOperation->GetOperationType()==sv4guiMitkSeg3DOperation::OpSETSEG3D)
    {
        m_DataModified=true;
        SetSeg3D(seg3D);
    }

    // mitk::OperationEndEvent endevent(operation);
    // ((const itk::Object*)this)->InvokeEvent(endevent);

}

