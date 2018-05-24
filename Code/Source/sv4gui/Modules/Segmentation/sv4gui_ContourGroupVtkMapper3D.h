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

#ifndef SV4GUI_CONTOURGROUPVTKMAPPER3D_H
#define SV4GUI_CONTOURGROUPVTKMAPPER3D_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_ContourGroup.h"

#include "mitkCommon.h"

#include "mitkBaseRenderer.h"
#include "mitkVtkMapper.h"

#include <vtkSmartPointer.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkAssembly.h>
#include <vtkPropAssembly.h>
#include <vtkProp.h>
#include <vtkPolyData.h>
#include <vtkTubeFilter.h>

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourGroupVtkMapper3D : public mitk::VtkMapper
{

public:

    mitkClassMacro( sv4guiContourGroupVtkMapper3D,mitk::VtkMapper );
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    const sv4guiContourGroup* GetInput(void);

//    virtual void Update(mitk::BaseRenderer * renderer) override;

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    class SV4GUIMODULESEGMENTATION_EXPORT LocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        vtkSmartPointer<vtkPropAssembly> m_Assembly;

//        mitk::ContourModelToSurfaceFilter::Pointer m_contourToPolyData;

        itk::TimeStamp m_LastUpdateTime;

        LocalStorage();

        ~LocalStorage()
        {
        }
    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    LocalStorage* GetLocalStorage(mitk::BaseRenderer* renderer);

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);


protected:
    sv4guiContourGroupVtkMapper3D();
    virtual ~sv4guiContourGroupVtkMapper3D();

    void GenerateDataForRenderer( mitk::BaseRenderer *renderer ) override;

    void ResetMapper( mitk::BaseRenderer* renderer ) override;

//    virtual vtkSmartPointer<vtkPolyData> CreateVtkPolyDataFromContour(sv4guiContour* contour);

//    virtual void ApplyContourProperties(mitk::BaseRenderer* renderer);
};

#endif //SV4GUI_CONTOURGROUPVTKMAPPER3D_H
