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

#ifndef SV4GUI_MITKSEG3D_H
#define SV4GUI_MITKSEG3D_H

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_Surface.h"
#include <sv4gui_Seg3D.h>

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiMitkSeg3D : public mitk::sv4guiSurface
{
public:

    mitkClassMacro(sv4guiMitkSeg3D, mitk::sv4guiSurface);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual bool IsEmptyTimeStep(unsigned int t) const override;
    virtual void ExecuteOperation(mitk::Operation *operation) override;

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

    sv4guiSeg3D* GetSeg3D() const {return m_Seg3D;}

    void SetSeg3D(sv4guiSeg3D* seg3D);

  protected:

    mitkCloneMacro(Self);

    sv4guiMitkSeg3D();
    sv4guiMitkSeg3D(const sv4guiMitkSeg3D &other);
    virtual ~sv4guiMitkSeg3D();

    bool m_DataModified;

    sv4guiSeg3D* m_Seg3D;

};

itkEventMacro( sv4guiMitkSeg3DEvent, itk::AnyEvent );

itkEventMacro( sv4guiMitkSeg3DSetEvent, sv4guiMitkSeg3DEvent );

#endif // SV4GUI_MITKSEG3D_H
