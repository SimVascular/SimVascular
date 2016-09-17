#ifndef SVPATHVTKMAPPER2D_H
#define SVPATHVTKMAPPER2D_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "svPath.h"

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

class SVPATH_EXPORT svPathVtkMapper2D : public mitk::VtkMapper
{
public:
    mitkClassMacro(svPathVtkMapper2D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const svPath* GetInput() const;

    static bool makePerpendicularVector2D(const mitk::Vector2D& in, mitk::Vector2D& out);

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    class SVPATH_EXPORT LocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        LocalStorage();

        ~LocalStorage();

        // points
        vtkSmartPointer<vtkPoints> m_UnselectedPoints;
        vtkSmartPointer<vtkPoints> m_SelectedPoints;
        //    vtkSmartPointer<vtkPoints> m_ContourPoints;
        vtkSmartPointer<vtkPoints> m_SplinePoints;
        vtkSmartPointer<vtkPoints> m_SplineConnectingPoints;

        // scales
        vtkSmartPointer<vtkFloatArray> m_UnselectedScales;
        vtkSmartPointer<vtkFloatArray> m_SelectedScales;
        vtkSmartPointer<vtkFloatArray> m_SplinePointsScales;

        // distances
        //    vtkSmartPointer<vtkFloatArray> m_DistancesBetweenPoints;

        // lines
        //    vtkSmartPointer<vtkCellArray> m_ContourLines;
        vtkSmartPointer<vtkCellArray> m_SplineConnectingLines;

        // glyph source (provides different shapes for the points)
        vtkSmartPointer<vtkGlyphSource2D> m_UnselectedGlyphSource2D;
        vtkSmartPointer<vtkGlyphSource2D> m_SelectedGlyphSource2D;
        vtkSmartPointer<vtkGlyphSource2D> m_SplineGlyphSource2D;

        // glyph
        vtkSmartPointer<vtkGlyph3D> m_UnselectedGlyph3D;
        vtkSmartPointer<vtkGlyph3D> m_SelectedGlyph3D;
        vtkSmartPointer<vtkGlyph3D> m_SplineGlyph3D;

        // polydata
        vtkSmartPointer<vtkPolyData> m_VtkUnselectedPointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSelectedPointsPolyData;
        //    vtkSmartPointer<vtkPolyData> m_VtkContourPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSplinePointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSplineConnectingLinesPolyData;

        // actor
        vtkSmartPointer<vtkActor> m_UnselectedActor;
        vtkSmartPointer<vtkActor> m_SelectedActor;
        //    vtkSmartPointer<vtkActor> m_ContourActor;
        vtkSmartPointer<vtkActor> m_SplinePointsActor;
        vtkSmartPointer<vtkActor> m_SplineConnectingLinesActor;
        //    vtkSmartPointer<vtkTextActor> m_VtkTextActor;

        //    std::vector < vtkSmartPointer<vtkTextActor> > m_VtkTextLabelActors;
        //    std::vector < vtkSmartPointer<vtkTextActor> > m_VtkTextDistanceActors;
        //    std::vector < vtkSmartPointer<vtkTextActor> > m_VtkTextAngleActors;

        // mappers
        vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
        //    vtkSmartPointer<vtkPolyDataMapper> m_VtkContourPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSplinePointsPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSplineConnectingLinesPolyDataMapper;

        // propassembly
        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

protected:

    svPathVtkMapper2D();

    virtual ~svPathVtkMapper2D();

    virtual void CreateVTKRenderObjects(mitk::BaseRenderer* renderer);

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    // member variables holding the current value of the properties used in this mapper
    //  bool m_ShowContour;             // "show contour" property
    bool m_ShowSpline;             // "show spline" property
    bool m_ShowSplinePoints;             // "show spline points" property
    //  bool m_CloseContour;            // "close contour" property
    bool m_ShowPoints;              // "show points" property
    //  bool m_ShowDistances;           // "show distances" property
    //  int m_DistancesDecimalDigits;   // "distance decimal digits" property
    //  bool m_ShowAngles;              // "show angles" property
    //  bool m_ShowDistantLines;        // "show distant lines" property
    int m_LineWidth;                // "line width" property
    int m_PointLineWidth;           // "point line width" property
    int m_Point2DSize;              // "point 2D size" property, in display units
    int m_SplinePoint2DSize;        // "spline point 2D size" property, in display units
    //  int m_IDShapeProperty;          // ID for mitkPointSetShape Enumeration Property "Pointset.2D.shape"
    //  bool m_FillShape;               // "Pointset.2D.fill shape" property
    float m_DistanceToPlane;        // "point 2D distance to plane" property


};

#endif // SVPATHVTKMAPPER2D_H
