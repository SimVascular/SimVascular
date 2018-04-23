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

#include "sv4gui_Seg3D.h"

sv4guiSeg3D::sv4guiSeg3D()
    : m_Vpd(NULL)
{
}

sv4guiSeg3D::sv4guiSeg3D(const sv4guiSeg3D &other, bool copyVpd)
    : m_Param(other.m_Param)
    , m_Vpd(NULL)
{
    if(copyVpd && other.m_Vpd!=NULL)
    {
        m_Vpd=vtkSmartPointer<vtkPolyData>::New();
        m_Vpd->DeepCopy(other.m_Vpd);
    }
}

sv4guiSeg3D::~sv4guiSeg3D()
{
}

sv4guiSeg3D* sv4guiSeg3D::Clone()
{
    return new sv4guiSeg3D(*this);
}

sv4guiSeg3DParam& sv4guiSeg3D::GetParam()
{
    return m_Param;
}

sv4guiSeg3DParam& sv4guiSeg3D::GetInnerParam()
{
    return m_InnerParam;
}

void sv4guiSeg3D::SetParam(sv4guiSeg3DParam param, bool copyToInner)
{
    m_Param=param;
    if(copyToInner)
        m_InnerParam=param;
}
