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

#ifndef SV4GUI_IMAGE_PATHS_MAPPER_H
#define SV4GUI_IMAGE_PATHS_MAPPER_H

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

class sv4guiImagePathsMapper : public mitk::VtkMapper
{
  public:
    mitkClassMacro(sv4guiImagePathsMapper, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    class LocalStorage : public mitk::Mapper::BaseLocalStorage {
    public:
        vtkSmartPointer<vtkAssembly> m_PropAssembly;
        LocalStorage() {
            m_PropAssembly = vtkSmartPointer<vtkAssembly>::New();
        }
        ~LocalStorage() { }
    };

    virtual vtkProp *GetVtkProp(mitk::BaseRenderer *renderer) override;

    void SetColor(const float red, const float green, const float blue);
    void SetUpdate(bool update);

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    double m_Color[3];

  protected:

    sv4guiImagePathsMapper();

    virtual ~sv4guiImagePathsMapper();
    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;
    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

  private:
    vtkSmartPointer<vtkActor> AddControlPointMarker(double x, double y, double z, double radius);
    bool needsUpdate_;

};

#endif 
