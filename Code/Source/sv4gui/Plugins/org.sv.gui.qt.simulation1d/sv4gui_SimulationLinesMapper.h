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

#ifndef SV4GUISIMULATIONLINES_MAPPER_H
#define SV4GUISIMULATIONLINES_MAPPER_H

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkAssembly.h>
#include <vtkPropAssembly.h>
#if VTK_MAJOR_VERSION == 6
    #include <vtkPainterPolyDataMapper.h>
#else
    #include <vtkOpenGLPolyDataMapper.h>
#endif
#include <vtkActor.h>
#include <vtkSmartPointer.h>
#include <vtkActor.h>
#include <string>

class sv4guiSimulationLinesMapper : public mitk::VtkMapper
{
public:

    mitkClassMacro(sv4guiSimulationLinesMapper, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {
    public:
        vtkSmartPointer<vtkAssembly> m_PropAssembly;
        LocalStorage() {
            m_PropAssembly = vtkSmartPointer<vtkAssembly>::New();
        }
        ~LocalStorage() { }
    };

    virtual vtkProp *GetVtkProp(mitk::BaseRenderer *renderer) override;

    void SetColor(const float red, const float green, const float blue);

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    std::string mode;

    bool m_needsUpdate = true;
    bool m_box = false;
    double m_seedRadius = 0.5;
    bool m_NewMesh = true;
    float m_Color[3];

protected:

    sv4guiSimulationLinesMapper();

    virtual ~sv4guiSimulationLinesMapper();
    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;
    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;
};

#endif /* SV4GUIPURKINJENETWORK_NETWORK_MAPPER_H */
