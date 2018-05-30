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

#ifndef SV4GUI_MITKSEG3DVTKMAPPER3D_H
#define SV4GUI_MITKSEG3DVTKMAPPER3D_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_MitkSeg3D.h"

#include "sv4gui_SurfaceVtkMapper3D.h"

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiMitkSeg3DVtkMapper3D : public mitk::sv4guiSurfaceVtkMapper3D
{
public:

    mitkClassMacro( sv4guiMitkSeg3DVtkMapper3D, mitk::sv4guiSurfaceVtkMapper3D );
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const sv4guiMitkSeg3D* GetInput() override;

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

protected:

    sv4guiMitkSeg3DVtkMapper3D();

    virtual ~sv4guiMitkSeg3DVtkMapper3D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

public:

    class svLocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        vtkSmartPointer<vtkPropAssembly> m_Assembly;

        vtkSmartPointer<vtkActor> m_SelectedSeedActor;
#if VTK_MAJOR_VERSION == 6
        vtkSmartPointer<vtkPainterPolyDataMapper> m_SelectedSeedVtkPolyDataMapper;
#else
        vtkSmartPointer<vtkOpenGLPolyDataMapper> m_SelectedSeedVtkPolyDataMapper;
#endif

        vtkSmartPointer<vtkActor> m_SeedActor;
#if VTK_MAJOR_VERSION == 6
        vtkSmartPointer<vtkPainterPolyDataMapper> m_SeedVtkPolyDataMapper;
#else
        vtkSmartPointer<vtkOpenGLPolyDataMapper> m_SeedVtkPolyDataMapper;
#endif

        vtkSmartPointer<vtkActor> m_EndSeedActor;
#if VTK_MAJOR_VERSION == 6
        vtkSmartPointer<vtkPainterPolyDataMapper> m_EndSeedVtkPolyDataMapper;
#else
        vtkSmartPointer<vtkOpenGLPolyDataMapper> m_EndSeedVtkPolyDataMapper;
#endif

//        itk::TimeStamp m_LastUpdateTime;

        svLocalStorage()
        {
#if VTK_MAJOR_VERSION == 6
            m_SelectedSeedVtkPolyDataMapper=vtkSmartPointer<vtkPainterPolyDataMapper>::New();
#else
            m_SelectedSeedVtkPolyDataMapper=vtkSmartPointer<vtkOpenGLPolyDataMapper>::New();
#endif
            m_SelectedSeedActor=vtkSmartPointer<vtkActor>::New();
            m_SelectedSeedActor->SetMapper(m_SelectedSeedVtkPolyDataMapper);

#if VTK_MAJOR_VERSION == 6
            m_SeedVtkPolyDataMapper=vtkSmartPointer<vtkPainterPolyDataMapper>::New();
#else
            m_SeedVtkPolyDataMapper=vtkSmartPointer<vtkOpenGLPolyDataMapper>::New();
#endif
            m_SeedActor=vtkSmartPointer<vtkActor>::New();
            m_SeedActor->SetMapper(m_SeedVtkPolyDataMapper);

#if VTK_MAJOR_VERSION == 6
            m_EndSeedVtkPolyDataMapper=vtkSmartPointer<vtkPainterPolyDataMapper>::New();
#else
            m_EndSeedVtkPolyDataMapper=vtkSmartPointer<vtkOpenGLPolyDataMapper>::New();
#endif
            m_EndSeedActor=vtkSmartPointer<vtkActor>::New();
            m_EndSeedActor->SetMapper(m_EndSeedVtkPolyDataMapper);

            m_Assembly=vtkSmartPointer<vtkPropAssembly>::New();
        }

        ~svLocalStorage()
        {
        }
    };

    mitk::LocalStorageHandler<svLocalStorage> m_LSHandler;

    bool m_AlreadyAddParts;

};

#endif //SV4GUI_MITKSEG3DVTKMAPPER3D_H
