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

/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/

#ifndef SV4GUI_SURFACEVTKMAPPER3D_H
#define SV4GUI_SURFACEVTKMAPPER3D_H

#include <MitkCoreExports.h>
#include "sv4gui_Surface.h"
#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkActor.h>
#if VTK_MAJOR_VERSION == 6
#include <vtkPainterPolyDataMapper.h>
#else
#include <vtkOpenGLPolyDataMapper.h>
#endif
#include <vtkPolyDataMapper.h>
#include <vtkPolyDataNormals.h>
#include <vtkPlaneCollection.h>
#include <vtkDepthSortPolyData.h>
#include <vtkSmartPointer.h>

namespace mitk {
  /**
  * @brief Vtk-based mapper for Surfaces.
  *
  * The mapper renders a surface in 3D. The actor is adapted according to the geometry in
  * the base class in mitk::VtkMapper::UpdateVtkTransform().
  *

  * Properties that can be set for surfaces and influence the surfaceVTKMapper3D are:
  *
  *   - \b "Backface Culling": True enables backface culling, which means only front-facing polygons will be visualized. False/disabled by default.
  *   - \b "color": (ColorProperty) Diffuse color of the surface object (this property will be read when material.diffuseColor is not defined)
  *   - \b "Opacity": (FloatProperty) Opacity of the surface object
  *   - \b "material.ambientColor": (ColorProperty) Ambient color  of the surface object
  *   - \b "material.ambientCoefficient": (  FloatProperty) Ambient coefficient of the surface object
  *   - \b "material.diffuseColor": ( ColorProperty) Diffuse color of the surface object
  *   - \b "material.diffuseCoefficient": (FloatProperty) Diffuse coefficient of the surface object
  *   - \b "material.specularColor": (ColorProperty) Specular Color of the surface object
  *   - \b "material.specularCoefficient": (FloatProperty) Specular coefficient of the surface object
  *   - \b "material.specularPower": (FloatProperty) Specular power of the surface object
  *   - \b "material.interpolation": (VtkInterpolationProperty) Interpolation
  *   - \b "material.representation": (VtkRepresentationProperty*) Representation
  *   - \b "material.wireframeLineWidth": (FloatProperty) Width in pixels of the lines drawn.
  *   - \b "material.pointSize": (FloatProperty) Size in pixels of the points drawn.
  *   - \b "scalar visibility": (BoolProperty) If the scarlars of the surface are visible
  *   - \b "Surface.TransferFunction (TransferFunctionProperty) Set a transferfunction for coloring the surface
  *   - \b "LookupTable (LookupTableProperty) LookupTable

  * Properties to look for are:
  *
  *   - \b "scalar visibility": if set to on, scalars assigned to the data are shown
  *        Turn this on if using a lookup table.
  *   - \b "ScalarsRangeMinimum": Optional. Can be used to store the scalar min, e.g.
  *         for the level window settings.
  *   - \b "ScalarsRangeMaximum": Optional. See above.
  *
  * There might be still some other, deprecated properties. These will not be documented anymore.
  * Please check the source if you really need them.
  *
  * @ingroup Mapper
  */

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiSurfaceVtkMapper3D : public VtkMapper
{
public:

  mitkClassMacro(sv4guiSurfaceVtkMapper3D, VtkMapper);

  itkFactorylessNewMacro(Self)
  itkCloneMacro(Self)

  itkSetMacro(GenerateNormals, bool);

  itkGetMacro(GenerateNormals, bool);

  virtual const mitk::sv4guiSurface* GetInput();

  virtual vtkProp *GetVtkProp(mitk::BaseRenderer *renderer) override;

  virtual void ApplyAllProperties(mitk::BaseRenderer* renderer, vtkActor* actor);

  static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

protected:
  sv4guiSurfaceVtkMapper3D();

  virtual ~sv4guiSurfaceVtkMapper3D();

  virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

  virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

  /** Checks whether the specified property is a ClippingProperty and if yes,
   * adds it to m_ClippingPlaneCollection (internal method). */
  virtual void CheckForClippingProperty( mitk::BaseRenderer* renderer, mitk::BaseProperty *property );

  bool m_GenerateNormals;

public:

  class LocalStorage : public mitk::Mapper::BaseLocalStorage
  {
    public:

      vtkSmartPointer<vtkActor> m_Actor;
#if VTK_MAJOR_VERSION == 6
      vtkSmartPointer<vtkPainterPolyDataMapper> m_VtkPolyDataMapper;
#else
      vtkSmartPointer<vtkOpenGLPolyDataMapper> m_VtkPolyDataMapper;
#endif
      vtkSmartPointer<vtkPolyDataNormals> m_VtkPolyDataNormals;
      vtkSmartPointer<vtkPlaneCollection> m_ClippingPlaneCollection;
      vtkSmartPointer<vtkDepthSortPolyData> m_DepthSort;
      itk::TimeStamp m_ShaderTimestampUpdate;

      LocalStorage()
      {
#if VTK_MAJOR_VERSION == 6
        m_VtkPolyDataMapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
#else
        m_VtkPolyDataMapper = vtkSmartPointer<vtkOpenGLPolyDataMapper>::New();
#endif
        m_VtkPolyDataNormals = vtkSmartPointer<vtkPolyDataNormals>::New();
        m_Actor = vtkSmartPointer<vtkActor>::New();
        m_ClippingPlaneCollection = vtkSmartPointer<vtkPlaneCollection>::New();

        m_Actor->SetMapper(m_VtkPolyDataMapper);

        m_DepthSort = vtkSmartPointer<vtkDepthSortPolyData>::New();
      }

      ~LocalStorage()
      {
      }
  };

  mitk::LocalStorageHandler<LocalStorage> m_LSH;

  static void ApplyMitkPropertiesToVtkProperty(mitk::DataNode *node, vtkProperty* property, mitk::BaseRenderer* renderer);
  static void SetDefaultPropertiesForVtkProperty(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite);
};
} // namespace mitk

#endif /* SV4GUI_SURFACEVTKMAPPER3D_H */

