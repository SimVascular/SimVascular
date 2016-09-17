#include "svContour.h"
#include "svMath3.h"
#include "svSegmentationUtils.h"

#include <vtkPoints.h>
#include <vtkCellArray.h>

svContour::svContour()
    : m_Type("Contour"),
      m_Method(""),
      m_Placed( false ),
      m_Closed( true ),
      m_Finished( true ),
      m_ControlPointSelectedIndex( -2 ),
      m_PlaneGeometry( nullptr ),
      m_PreviewControlPointVisible( false ),
      m_Extendable( false ),
      m_Selected(false),
      m_MinControlPointNumber(2),
      m_MaxControlPointNumber(2)
 {
    for (int i=0;i<5;i++)
    {
        m_ControlPointNonRemovableIndices[i]=-2;
    }
    m_ControlPointNonRemovableIndices[0]=0;
    m_ControlPointNonRemovableIndices[1]=1;


    m_SubdivisionType=CONSTANT_TOTAL_NUMBER;
    m_SubdivisionNumber=0;
    m_SubdivisionSpacing=0.0;

}

svContour::svContour(const svContour &other)
    : m_Type(other.m_Type)
    , m_Method(other.m_Method)
    , m_Placed(other.m_Placed)
    , m_Closed(other.m_Closed)
    , m_Finished(other.m_Finished)
    , m_MinControlPointNumber(other.m_MinControlPointNumber)
    , m_MaxControlPointNumber(other.m_MaxControlPointNumber)
    , m_SubdivisionType(other.m_SubdivisionType)
    , m_SubdivisionNumber(other.m_SubdivisionNumber)
    , m_SubdivisionSpacing(other.m_SubdivisionSpacing)
    , m_InitiallyPlaced(other.m_InitiallyPlaced)
    , m_ControlPoints(other.m_ControlPoints)
    , m_ContourPoints(other.m_ContourPoints)
    , m_Extendable(other.m_Extendable)
{
//    SetPlaneGeometry(other.m_PlaneGeometry);
    SetPathPoint(other.m_PathPoint);

    for(int i=0;i<5;i++){
        m_ControlPointNonRemovableIndices[i]=other.m_ControlPointNonRemovableIndices[i];
    }

}

svContour::~svContour()
{
    //    if(m_PlaneGeometry)
    //    {
    //        delete m_PlaneGeometry;
    //    }

}

svContour* svContour::Clone()
{
    return new svContour(*this);
}

std::string svContour::GetClassName()
{
    return "svContour";
}

std::string svContour::GetType()
{
    return m_Type;
}

void svContour::SetType(std::string type)
{
    m_Type=type;
}

std::string svContour::GetMethod()
{
    return m_Method;
}

void svContour::SetMethod(std::string method)
{
    m_Method=method;
}

int svContour::GetContourID()
{
    return m_ContourID;
}

void svContour::SetContourID(int contourID)
{
    m_ContourID=contourID;
}

bool svContour::IsSelected()
{
    return m_Selected;
}

void svContour::SetSelected(bool selected)
{
    m_Selected=selected;
}

bool svContour::IsHovering()
{
    return m_Hovering;
}

void svContour::SetHovering(bool hovering)
{
    m_Hovering=hovering;
}

bool svContour::IsPlaced()
{
    return m_Placed;
}

void svContour::SetPlaced(bool placed)
{
    m_Placed=placed;
}

bool svContour::IsExtendable()
{
    return m_Extendable;
}

void svContour::SetExtendable(bool extendable)
{
    m_Extendable=extendable;
}

svContour::ShapeType svContour::GetShape()
{
    return m_Shape;
}

bool svContour::IsClosed()
{
    return m_Closed;
}

void svContour::SetClosed(bool closed)
{
    if(m_Closed!=closed)
    {
        m_Closed=closed;
        CreateContour();
    }
}

bool svContour::IsFinished()
{
    return m_Finished;
}

void svContour::SetFinished(bool finished)
{
    m_Finished=finished;
}

int svContour::GetSubdivisionNumber()
{
    return m_SubdivisionNumber;
}

void svContour::SetSubdivisionNumber(int number)
{
    if(m_SubdivisionNumber!=number)
    {
        m_SubdivisionNumber=number;
        CreateContour();
    }
}

svContour::SubdivisionType svContour::GetSubdivisionType()
{
    return m_SubdivisionType;
}

void svContour::SetSubdivisionType(SubdivisionType subdivType)
{
    if(m_SubdivisionType!=subdivType)
    {
        m_SubdivisionType=subdivType;
        CreateContour();
    }
}

double svContour::GetSubdivisionSpacing()
{
    return m_SubdivisionSpacing;
}

void svContour::SetSubdivisionSpacing(double spacing)
{
    if(m_SubdivisionSpacing!=spacing)
    {
        m_SubdivisionSpacing=spacing;
        CreateContour();
    }
}

void svContour::SetPlaneGeometry(mitk::PlaneGeometry* planeGeometry)
{
//    if(m_PlaneGeometry)
//    {
//        m_PlaneGeometry->Delete();
//    }

//    if(planeGeometry)
//    {
//        m_PlaneGeometry=dynamic_cast<mitk::PlaneGeometry*>(planeGeometry->Clone().GetPointer());
//    }
//    else
//    {
//        m_PlaneGeometry=NULL;
//    }

//    m_PlaneGeometry=planeGeometry;

    if(planeGeometry!=NULL)
    {
        m_PlaneGeometry = planeGeometry->Clone();
    }else{
        m_PlaneGeometry = NULL;
    }

}

mitk::PlaneGeometry* svContour::GetPlaneGeometry()
{
    return m_PlaneGeometry;
}

int svContour::GetControlPointNumber()
{
    return m_ControlPoints.size();
}

int svContour::GetMinControlPointNumber()
{
    return m_MinControlPointNumber;
}

int svContour::GetMaxControlPointNumber()
{
    return m_MaxControlPointNumber;
}

void svContour::SetMinControlPointNumber(int number)
{
    m_MinControlPointNumber=number;
}

void svContour::SetMaxControlPointNumber(int number)
{
    m_MaxControlPointNumber=number;
}

mitk::Point3D svContour::GetControlPoint(int index){
    mitk::Point3D point;
    point.Fill(0);

    if(index==-1)
    {
        point=m_ControlPoints[m_ControlPoints.size()-1];
    }
    else if(index>-1 && index<m_ControlPoints.size())
    {
        point=m_ControlPoints[index];
    }

    return point;
}

void svContour::InsertControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size();

    if(index>-1 && index<=m_ControlPoints.size())
    {
        m_ControlPoints.insert(m_ControlPoints.begin()+index,point);
        m_ControlPointSelectedIndex=index;
        ControlPointsChanged();
    }

}

void svContour::RemoveControlPoint(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPoints.erase(m_ControlPoints.begin()+index);
        ControlPointsChanged();
    }

}

void svContour::SetControlPoint(int index, mitk::Point3D point)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        if(index==0)
        {
            mitk::Vector3D dirVec=point-m_ControlPoints[index];
            Shift(dirVec);
        }
        else if(index==1)
        {
            Scale(m_ControlPoints[0], m_ControlPoints[index], point);
        }

    }

}

//void svContour::SetActualControlPoint(int index, mitk::Point3D point)
//{
//    m_ControlPoints[index]=point;
//}

void svContour::SetControlPointSelectedIndex(int index)
{
    if(index==-1) index=m_ControlPoints.size()-1;

    if(index>-1 && index<m_ControlPoints.size())
    {
        m_ControlPointSelectedIndex=index;
    }else{
        m_ControlPointSelectedIndex=-2;
    }

}

void svContour::DeselectControlPoint()
{
    m_ControlPointSelectedIndex=-2;
}

int svContour::GetControlPointSelectedIndex()
{
    return m_ControlPointSelectedIndex;
}

void svContour::ClearControlPoints()
{
    m_ControlPoints.clear();
}

void svContour::PlaceContour(mitk::Point3D point)
{
    PlaceControlPoints(point);
    ControlPointsChanged();
}

void svContour::PlaceControlPoints(mitk::Point3D point)
{
    for ( unsigned int i = 0; i < GetMinControlPointNumber(); ++i )
    {
      m_ControlPoints.push_back( point );
    }

    m_Placed = true;
    m_ControlPointSelectedIndex = 1;
}

void svContour::SetControlPoints(std::vector<mitk::Point3D> controlPoints, bool updateContour)
{
    m_ControlPoints=controlPoints;
    if(updateContour)
        ControlPointsChanged();
}

bool svContour::IsControlPointRemovable(int index)
{
    for(int i=0;i<5;i++)
    {
        if(m_ControlPointNonRemovableIndices[i]==index)
            return false;
    }

    return true;
}

void svContour::SetPreviewControlPoint(mitk::Point3D point )
{
    m_PreviewControlPoint = point;
    m_PreviewControlPointVisible = true;
}

void svContour::HidePreviewControlPoint()
{
    m_PreviewControlPointVisible = false;
}

bool svContour::IsPreviewControlPointVisible()
{
    return m_PreviewControlPointVisible;
}

mitk::Point3D svContour::GetPreviewControlPoint()
{
    return m_PreviewControlPoint;
}

void svContour::ClearContourPoints()
{
    m_ContourPoints.clear();
}

void svContour::CreateContour()
{
    if(m_ControlPoints.size()<1)
    {
        return;
    }

    m_ContourPoints.clear();
    CreateContourPoints();
    ContourPointsChanged();
}

void svContour::ControlPointsChanged(){
    CreateContour();
    //    this->Modified();
}

void svContour::SetContourPoints(std::vector<mitk::Point3D> contourPoints, bool update)
{
    m_ContourPoints=contourPoints;
    if(update)
    ContourPointsChanged();
}

int svContour::GetContourPointNumber()
{
    return m_ContourPoints.size();
}

mitk::Point3D svContour::GetContourPoint(int index)
{
    mitk::Point3D point;
    point.Fill(0);

    if(index==-1)
        index=m_ContourPoints.size()-1;

    if(index>-1 && index<m_ContourPoints.size())
    {
        point=m_ContourPoints[index];
    }
    return point;
}

void svContour::ContourPointsChanged()
{
    CreateCenterScalingPoints();
    AssignCenterScalingPoints();
}

void svContour::AssignCenterScalingPoints()
{
    if(m_ControlPoints.size()==0)
    {
        m_ControlPoints.push_back(m_CenterPoint);
        m_ControlPoints.push_back(m_ScalingPoint);
    }
    else if(m_ControlPoints.size()==1)
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints.push_back(m_ScalingPoint);
    }
    else
    {
        m_ControlPoints[0]=m_CenterPoint;
        m_ControlPoints[1]=m_ScalingPoint;
    }
}

void svContour::CreateCenterScalingPoints()
{
    double Sx=0,Sy=0,A=0;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        mitk::Point2D point1,point2;
        m_PlaneGeometry->Map(m_ContourPoints[i], point1);
        if(i==m_ContourPoints.size()-1)
            m_PlaneGeometry->Map(m_ContourPoints[0], point2);
        else
            m_PlaneGeometry->Map(m_ContourPoints[i+1], point2);

        Sx+=(point1[0]+point2[0])*(point1[0]*point2[1]-point2[0]*point1[1]);
        Sy+=(point1[1]+point2[1])*(point1[0]*point2[1]-point2[0]*point1[1]);
        A+=(point1[0]*point2[1]-point2[0]*point1[1]);
    }

    mitk::Point2D center;
    if(A!=0)
    {
        center[0]=Sx/A/3;
        center[1]=Sy/A/3;
    }
    else
    {
        Sx=0;
        Sy=0;
        for(int i=0;i<m_ContourPoints.size();i++)
        {
            mitk::Point2D point;
            m_PlaneGeometry->Map(m_ContourPoints[i], point);
            Sx+=point[0];
            Sy+=point[1];
        }
        center[0]=Sx/m_ContourPoints.size();
        center[1]=Sy/m_ContourPoints.size();
    }

    double minDis=0;
    bool firstTime=true;

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        mitk::Point2D point;
        m_PlaneGeometry->Map(m_ContourPoints[i], point);
        double dis=point.EuclideanDistanceTo(center);
        if(firstTime)
        {
            minDis=dis;
            firstTime=false;
        }
        else if(dis<minDis)
        {
            minDis=dis;
        }
    }

    mitk::Point2D scalingPoint;
    scalingPoint[0]=center[0]+minDis/2;
    scalingPoint[1]=center[1];

    m_PlaneGeometry->Map(center,m_CenterPoint);
    m_PlaneGeometry->Map(scalingPoint,m_ScalingPoint);

}

//mitk::Point3D svContour::GetCenterPoint()
//{
//    return m_CenterPoint;
//}

svContour* svContour::CreateSmoothedContour(int fourierNumber)
{
    if(m_ContourPoints.size()<3)
        return this->Clone();

    svContour* contour=new svContour();
    contour->SetPathPoint(m_PathPoint);
//    contour->SetPlaneGeometry(m_PlaneGeometry);
    std::string method=m_Method;
    int idx=method.find("Smoothed");
    if(idx<0)
        method=method+" + Smoothed";

    contour->SetMethod(method);
    contour->SetPlaced(true);
    contour->SetClosed(m_Closed);

    int pointNumber=m_ContourPoints.size();

    int smoothedPointNumber;

    if((2*pointNumber)<fourierNumber)
        smoothedPointNumber=3*fourierNumber;
    else
        smoothedPointNumber=pointNumber;

    std::vector<mitk::Point3D> smoothedContourPoints=svMath3::CreateSmoothedCurve(m_ContourPoints,m_Closed,fourierNumber,0,smoothedPointNumber);

    contour->SetContourPoints(smoothedContourPoints);

    return contour;
}

vtkSmartPointer<vtkPolyData> svContour::CreateVtkPolyDataFromContour(bool includingAllLines)
{
    vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();

    int pointNumber=m_ContourPoints.size();

    for (int i=0; i<=pointNumber; i++)
    {
        if(i<pointNumber)
        {
            mitk::Point3D point = GetContourPoint(i);
            points->InsertPoint(i,point[0],point[1],point[2]);
        }

        if(i>0&&i<pointNumber){
            vtkIdType cell[2] = {i-1,i};
            lines->InsertNextCell(2,cell);
        }else if(i==pointNumber&&m_Closed){
            vtkIdType cell[2] = {i-1,0};
            lines->InsertNextCell(2,cell);
        }

    }

    if(includingAllLines&&m_Closed&&m_Finished&&m_ControlPoints.size()>1)
    {
        mitk::Point3D point = GetControlPoint(0);
        points->InsertPoint(pointNumber,point[0],point[1],point[2]);
        point = GetControlPoint(1);
        points->InsertPoint(pointNumber+1,point[0],point[1],point[2]);
        vtkIdType cell[2] = {pointNumber,pointNumber+1};
        lines->InsertNextCell(2,cell);
    }

    vtkSmartPointer<vtkPolyData> polyData = vtkSmartPointer<vtkPolyData>::New();
    polyData->SetPoints(points);
    polyData->SetLines(lines);

    return polyData;
}

int svContour::SearchControlPointByContourPoint( int contourPointIndex )
{
    return contourPointIndex;
}

void svContour::Shift(mitk::Vector3D dirVec){

    for(int i=0;i<m_ControlPoints.size();i++)
    {
        m_ControlPoints[i]=m_ControlPoints[i]+dirVec;
    }

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        m_ContourPoints[i]=m_ContourPoints[i]+dirVec;
    }
}

void svContour::Scale(double scalingFactor)
{
    Scale(scalingFactor, m_ControlPoints[0]);
}

void svContour::Scale(double scalingFactor, mitk::Point3D referencePoint)
{
    mitk::Vector3D dirVec;

    for(int i=0;i<m_ControlPoints.size();i++)
    {
        dirVec=m_ControlPoints[i]-referencePoint;
        m_ControlPoints[i]=referencePoint+scalingFactor*dirVec;
    }

    for(int i=0;i<m_ContourPoints.size();i++)
    {
        dirVec=m_ContourPoints[i]-referencePoint;
        m_ContourPoints[i]=referencePoint+scalingFactor*dirVec;
    }
}

void svContour::Scale(mitk::Point3D referencePoint, mitk::Point3D oldPoint, mitk::Point3D newPoint)
{
    double dis1=oldPoint.EuclideanDistanceTo(referencePoint);
    double dis2=newPoint.EuclideanDistanceTo(referencePoint);
    double scalingFactor;
    if(dis1==0||dis1==dis2){
        return;
    }else{
        scalingFactor=dis2/dis1;
        Scale(scalingFactor,referencePoint);
        m_ControlPoints[1]=newPoint;//make scaling point is exactly same as new point;

    }

}


void svContour::CalculateBoundingBox(double *bounds)
{
    for(int i=0;i<m_ControlPoints.size();i++)
    {
        double x=m_ControlPoints[i][0];
        double y=m_ControlPoints[i][1];
        double z=m_ControlPoints[i][2];

        if(i==0){
            bounds[0]=x;
            bounds[1]=x;
            bounds[2]=y;
            bounds[3]=y;
            bounds[4]=z;
            bounds[5]=z;
        }else{
            if(x<bounds[0]) bounds[0]=x;
            if(x>bounds[1]) bounds[1]=x;
            if(y<bounds[2]) bounds[2]=y;
            if(y>bounds[3]) bounds[3]=y;
            if(z<bounds[4]) bounds[4]=z;
            if(z>bounds[5]) bounds[5]=z;
        }
    }

    for (int i = 0; i < m_ContourPoints.size(); i++) {
        double x=m_ContourPoints[i][0];
        double y=m_ContourPoints[i][1];
        double z=m_ContourPoints[i][2];
        if(x<bounds[0]) bounds[0]=x;
        if(x>bounds[1]) bounds[1]=x;
        if(y<bounds[2]) bounds[2]=y;
        if(y>bounds[3]) bounds[3]=y;
        if(z<bounds[4]) bounds[4]=z;
        if(z>bounds[5]) bounds[5]=z;
    }

}

bool svContour::IsOnPlane(const mitk::PlaneGeometry* planeGeometry, double precisionFactor)
{
    if(m_PlaneGeometry.IsNull() || planeGeometry==NULL) return false;

//    double contourThickness = m_PlaneGeometry->GetExtentInMM( 2 )*precisionFactor;
//    if(m_PlaneGeometry->IsParallel(planeGeometry)
//            && m_PlaneGeometry->DistanceFromPlane(planeGeometry)<contourThickness)
//        return true;
//    else
//        return false;

    double contourThickness = planeGeometry->GetExtentInMM( 2 )*precisionFactor;

    double ang=m_PlaneGeometry->Angle(planeGeometry);
    double dis=std::abs(m_PlaneGeometry->SignedDistance(planeGeometry->GetOrigin()));
    if( (ang<0.02||ang>3.12) && dis<contourThickness)
        return true;
    else
        return false;
}

svPathElement::svPathPoint svContour::GetPathPoint()
{
    return m_PathPoint;
}

void svContour::SetPathPoint(svPathElement::svPathPoint pathPoint)
{
    m_PathPoint=pathPoint;

    mitk::Vector3D spacing;
    spacing.Fill(0.1);

    m_PlaneGeometry=svSegmentationUtils::CreatePlaneGeometry(pathPoint,spacing, 1.0);
}

int svContour::GetPathPosID(){
    return m_PathPoint.id;
}

mitk::Point3D svContour::GetPathPosPoint()
{
    return m_PathPoint.pos;
}

vtkImageData* svContour::GetVtkImageSlice()
{
    return m_VtkImageSlice;
}

void svContour::SetVtkImageSlice(vtkImageData* slice)
{
    m_VtkImageSlice=slice;
}

mitk::Point3D svContour::GetCenterPoint()
{
    return m_ControlPoints[0];
}
