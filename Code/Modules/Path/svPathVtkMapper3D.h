#ifndef SVPATHVTKMAPPER3D_H
#define SVPATHVTKMAPPER3D_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "svPath.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include <vtkSmartPointer.h>

class vtkActor;
class vtkPropAssembly;
class vtkAppendPolyData;
class vtkPolyData;
class vtkTubeFilter;
class vtkPolyDataMapper;

class SVPATH_EXPORT svPathVtkMapper3D : public mitk::VtkMapper
{
public:
    mitkClassMacro(svPathVtkMapper3D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const svPath* GetInput();

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;
    virtual void UpdateVtkTransform(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    void ReleaseGraphicsResources(mitk::BaseRenderer* renderer) override;

    mitk::LocalStorageHandler<BaseLocalStorage> m_LSH;

protected:
    svPathVtkMapper3D();

    virtual ~svPathVtkMapper3D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;
    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;
    virtual void ApplyAllProperties(mitk::BaseRenderer* renderer);
    virtual void CreateSpline();
    virtual void CreateVTKRenderObjects();

    vtkSmartPointer<vtkAppendPolyData> m_vtkSelectedPoints;
    vtkSmartPointer<vtkAppendPolyData> m_vtkUnselectedPoints;
    vtkSmartPointer<vtkAppendPolyData> m_vtkSplinePoints;

    vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
    vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;
    vtkSmartPointer<vtkPolyDataMapper> m_VtkSplinePointsPolyDataMapper;
    vtkSmartPointer<vtkPolyDataMapper> m_VtkSplinePolyDataMapper;

    vtkSmartPointer<vtkActor> m_SelectedActor;
    vtkSmartPointer<vtkActor> m_UnselectedActor;
    vtkSmartPointer<vtkActor> m_SplinePointsActor;
    vtkSmartPointer<vtkActor> m_SplineActor;

    vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

    //variables to check if an update of the vtk objects is needed
    mitk::ScalarType m_PointSize;          // "point size" property
    mitk::ScalarType m_SplinePointSize;    // "spline point size" property
    mitk::ScalarType m_SplineRadius;       // "spline size" property

};


#endif // SVPATHVTKMAPPER3D_H
