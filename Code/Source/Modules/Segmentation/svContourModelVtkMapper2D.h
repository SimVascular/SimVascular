#ifndef SVCONTOURMODELVTKMAPPER2D_H
#define SVCONTOURMODELVTKMAPPER2D_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourModel.h"
#include "svContour.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

//VTK
#include <vtkSmartPointer.h>
class vtkActor;
class vtkPropAssembly;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkGlyphSource2D;
class vtkGlyph3D;
class vtkFloatArray;
class vtkCellArray;

class SVSEGMENTATION_EXPORT svContourModelVtkMapper2D : public mitk::VtkMapper
{
public:

    mitkClassMacro(svContourModelVtkMapper2D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    void ReleaseGraphicsResources(mitk::BaseRenderer* renderer) override;

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        LocalStorage();

        ~LocalStorage();

        // points
        vtkSmartPointer<vtkPoints> m_UnselectedPoints;
        vtkSmartPointer<vtkPoints> m_SelectedPoints;

        // scales
        vtkSmartPointer<vtkFloatArray> m_UnselectedScales;
        vtkSmartPointer<vtkFloatArray> m_SelectedScales;

        // glyph source (provides different shapes for the points)
        vtkSmartPointer<vtkGlyphSource2D> m_UnselectedGlyphSource2D;
        vtkSmartPointer<vtkGlyphSource2D> m_SelectedGlyphSource2D;

        // glyph
        vtkSmartPointer<vtkGlyph3D> m_UnselectedGlyph3D;
        vtkSmartPointer<vtkGlyph3D> m_SelectedGlyph3D;

        // polydata
        vtkSmartPointer<vtkPolyData> m_VtkUnselectedPointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSelectedPointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkContourPolyData;

        // actor
        vtkSmartPointer<vtkActor> m_UnselectedActor;
        vtkSmartPointer<vtkActor> m_SelectedActor;
        vtkSmartPointer<vtkActor> m_ContourActor;

        // mappers
        vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkContourPolyDataMapper;

        // propassembly
        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

protected:

    svContourModelVtkMapper2D();

    virtual ~svContourModelVtkMapper2D();

    virtual void CreateVTKRenderObjects(mitk::BaseRenderer* renderer);

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    virtual void FindContourOnCurrentSlice(mitk::BaseRenderer* renderer, unsigned int t = 0) ;

    svContour* m_Contour;//available contour on the renderer plane

    // member variables holding the current value of the properties used in this mapper
    //  bool m_ShowContour;             // "show contour" property
    bool m_ShowPoints;              // "show points" property
    int m_LineWidth;                // "line width" property
    int m_PointLineWidth;           // "point line width" property
    int m_Point2DSize;              // "point 2D size" property
    float m_DistanceToPlane;        // "point 2D distance to plane" property


};

#endif // SVCONTOURMODELVTKMAPPER2D_H
