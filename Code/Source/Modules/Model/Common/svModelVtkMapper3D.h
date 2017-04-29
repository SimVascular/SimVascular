
#ifndef SVMODELVTKMAPPER3D_H
#define SVMODELVTKMAPPER3D_H

#include <svModelExports.h>

#include "svModel.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkAssembly.h>
#include <vtkPropAssembly.h>
#include <vtkPainterPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkPlaneCollection.h>
#include <vtkSmartPointer.h>

/* Properties that can be set for surfaces and influence the svModelVtkMapper3D are:
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

class SVMODEL_EXPORT svModelVtkMapper3D : public mitk::VtkMapper
{
public:

    mitkClassMacro(svModelVtkMapper3D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const svModel* GetInput();

    virtual vtkProp *GetVtkProp(mitk::BaseRenderer *renderer) override;

    virtual void ApplyAllProperties(mitk::BaseRenderer* renderer, vtkSmartPointer<vtkPainterPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor);

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    vtkSmartPointer<vtkActor> GetWholeSurfaceActor(mitk::BaseRenderer* renderer);
    std::vector<vtkSmartPointer<vtkActor>> GetFaceActors(mitk::BaseRenderer* renderer);

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {
    public:

//        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;
        vtkSmartPointer<vtkAssembly> m_PropAssembly;
        vtkSmartPointer<vtkPlaneCollection> m_ClippingPlaneCollection;

//        vtkSmartPointer<vtkActor> m_Actor;
//        vtkSmartPointer<vtkPainterPolyDataMapper> m_VtkPolyDataMapper;
//        vtkSmartPointer<vtkPolyDataNormals> m_VtkPolyDataNormals;

//        vtkSmartPointer<vtkDepthSortPolyData> m_DepthSort;

        vtkSmartPointer<vtkActor> m_Actor;

        std::vector<vtkSmartPointer<vtkActor>> m_FaceActors;

        LocalStorage()
        {
            m_PropAssembly = vtkSmartPointer<vtkAssembly>::New();
            m_ClippingPlaneCollection = vtkSmartPointer<vtkPlaneCollection>::New();

//            m_VtkPolyDataMapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
//            m_VtkPolyDataNormals = vtkSmartPointer<vtkPolyDataNormals>::New();
//            m_Actor = vtkSmartPointer<vtkActor>::New();
//            m_Actor->SetMapper(m_VtkPolyDataMapper);

//            m_DepthSort = vtkSmartPointer<vtkDepthSortPolyData>::New();
        }

        ~LocalStorage()
        {
        }
    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    static void ApplyMitkPropertiesToVtkProperty(mitk::DataNode *node, vtkProperty* property, mitk::BaseRenderer* renderer);
    static void SetDefaultPropertiesForVtkProperty(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite);

protected:
    svModelVtkMapper3D();

    virtual ~svModelVtkMapper3D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    virtual void CheckForClippingProperty( mitk::BaseRenderer* renderer, mitk::BaseProperty *property );

};


#endif /* SVMODELVTKMAPPER3D_H */
