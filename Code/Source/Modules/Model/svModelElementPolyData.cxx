#include "svModelElementPolyData.h"

#include "svMath3.h"
#include "svModelUtils.h"

#include "cv_polydatasolid_utils.h"
#include "cv_VMTK_utils.h"

#include <vtkFillHolesFilter.h>
#include <vtkPolyDataConnectivityFilter.h>
#include <vtkQuadricDecimation.h>
#include <vtkSmoothPolyDataFilter.h>
#include <vtkButterflySubdivisionFilter.h>
#include <vtkWindowedSincPolyDataFilter.h>
#include <vtkDensifyPolyData.h>

#include <iostream>
using namespace std;

svModelElementPolyData::svModelElementPolyData()
{
    m_Type="PolyData";
    m_BlendParam=new svBlendParam();
}

svModelElementPolyData::svModelElementPolyData(const svModelElementPolyData &other)
    : svModelElement(other)
    , m_SelectedCellIDs(other.m_SelectedCellIDs)
{
    m_BlendParam=new svBlendParam(*(other.m_BlendParam));
}

svModelElementPolyData::~svModelElementPolyData()
{
}

svModelElementPolyData* svModelElementPolyData::Clone()
{
    return new svModelElementPolyData(*this);
}

vtkSmartPointer<vtkPolyData> svModelElementPolyData::CreateFaceVtkPolyData(int id)
{
    vtkPolyData* facepd=NULL;

    if(m_WholeVtkPolyData)
    {
        facepd = vtkPolyData::New();
//        PlyDtaUtils_GetFacePolyData(m_SolidModel, &id, facepd);
        PlyDtaUtils_GetFacePolyData(m_WholeVtkPolyData.GetPointer(), &id, facepd);
    }

    vtkSmartPointer<vtkPolyData> fpd
      = vtkSmartPointer<vtkPolyData>::Take(facepd);

    return fpd;
}

vtkSmartPointer<vtkPolyData> svModelElementPolyData::CreateWholeVtkPolyData()
{
    return m_WholeVtkPolyData;
}

//vtkPolyData* svModelElementPolyData::GetSolidModel() const
//{
//    return m_SolidModel;
//}

//void svModelElementPolyData::SetSolidModel(vtkPolyData* solidModel)
//{
//    m_SolidModel=solidModel;
//    m_WholeVtkPolyData=solidModel;
//}

svModelElementPolyData::svBlendParam* svModelElementPolyData::GetBlendParam()
{
    return m_BlendParam;
}

void svModelElementPolyData::AssignBlendParam(svModelElementPolyData::svBlendParam* param)
{
    m_BlendParam->numblenditers=param->numblenditers;
    m_BlendParam->numsubblenditers=param->numsubblenditers;
    m_BlendParam->numsubdivisioniters=param->numsubdivisioniters;
    m_BlendParam->numcgsmoothiters=param->numcgsmoothiters;
    m_BlendParam->numlapsmoothiters=param->numlapsmoothiters;
    m_BlendParam->targetdecimation=param->targetdecimation;
}

//bool svModelElementPolyData::DeleteFaces(std::vector<int> faceIDs)
//{
//    if(m_WholeVtkPolyData==NULL)
//        return false;

//    std::string arrayname="ModelFaceID";
//    bool existing=false;

//    if(m_WholeVtkPolyData->GetCellData()->HasArray(arrayname.c_str()))
//        existing=true;

//    if(!existing)
//        return false;

//    for(int i=0;i<faceIDs.size();i++)
//    {
//        vtkSmartPointer<vtkIntArray> boundaryRegions = vtkSmartPointer<vtkIntArray>::New();
//        boundaryRegions = vtkIntArray::SafeDownCast(m_WholeVtkPolyData->GetCellData()-> GetScalars("ModelFaceID"));

//        m_WholeVtkPolyData->BuildLinks();

//        for (vtkIdType cellId=0; cellId< m_WholeVtkPolyData->GetNumberOfCells(); cellId++)
//        {
//          if (boundaryRegions->GetValue(cellId) == faceIDs[i])
//          {
//            m_WholeVtkPolyData->DeleteCell(cellId);
//          }
//        }

//        m_WholeVtkPolyData->RemoveDeletedCells();

//        RemoveFace(faceIDs[i]);

//        RemoveFaceFromBlendParamRadii(faceIDs[i]);

//    }

//    m_SelectedCellIDs.clear();

//    return true;
//}

bool svModelElementPolyData::DeleteFaces(std::vector<int> faceIDs)
{
    if(!svModelUtils::DeleteRegions(m_WholeVtkPolyData, faceIDs))
        return false;

    for(int i=0;i<faceIDs.size();i++)
    {
        RemoveFace(faceIDs[i]);

        RemoveFaceFromBlendParamRadii(faceIDs[i]);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::CombineFaces(std::vector<int> faceIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    std::string arrayname="ModelFaceID";
    bool existing=false;

    if(m_WholeVtkPolyData->GetCellData()->HasArray(arrayname.c_str()))
        existing=true;

    if(!existing)
        return false;

    int targetID=0;
    int loseID=0;

    for(int i=0;i<faceIDs.size();i++)
    {
        if(i==0)
        {
            targetID=faceIDs[i];
            continue;
        }

        loseID=faceIDs[i];

        vtkSmartPointer<vtkIntArray> boundaryRegions = vtkSmartPointer<vtkIntArray>::New();
        boundaryRegions = vtkIntArray::SafeDownCast(m_WholeVtkPolyData->GetCellData()-> GetScalars("ModelFaceID"));

        m_WholeVtkPolyData->BuildLinks();

        for (vtkIdType cellId=0; cellId< m_WholeVtkPolyData->GetNumberOfCells(); cellId++)
        {
          if (boundaryRegions->GetValue(cellId) == loseID)
          {
            boundaryRegions->SetValue(cellId,targetID);
          }
        }

        m_WholeVtkPolyData->GetCellData()->RemoveArray("ModelFaceID");
        boundaryRegions->SetName("ModelFaceID");
        m_WholeVtkPolyData->GetCellData()->AddArray(boundaryRegions);
        m_WholeVtkPolyData->GetCellData()->SetActiveScalars("ModelFaceID");

        RemoveFace(loseID);

        ReplaceFaceIDForBlendParamRadii(targetID, loseID);

    }

    svFace* face=GetFace(targetID);
    face->vpd=CreateFaceVtkPolyData(targetID);

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::RemeshFaces(std::vector<int> faceIDs, double size)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkIdList> excluded = vtkSmartPointer<vtkIdList>::New();
    for(int i=0;i<m_Faces.size();i++)
    {
        bool found=false;
        for(int j=0;j<faceIDs.size();j++)
        {
            if(m_Faces[i] && m_Faces[i]->id==faceIDs[j])
            {
                found=true;
                break;
            }

        }
        if(!found)
        {
            excluded->InsertNextId(m_Faces[i]->id);
        }
    }

    int meshcaps = 1;
    int preserveedges = 0;
    //     int triangleoutput = 1;
    double collapseanglethreshold = 0;
    double trianglesplitfactor = 0;
    int useSizeFunction = 0;
    std::string markerListName = "ModelFaceID";

    if (VMTKUtils_SurfaceRemeshing(m_WholeVtkPolyData,size,meshcaps,preserveedges,
                                   trianglesplitfactor,collapseanglethreshold,excluded,
                                   markerListName,useSizeFunction,NULL) != CV_OK)
    {
        fprintf(stderr,"Issue while remeshing surface\n");
        return false;
    }

    //update all faces; some excluded faces may be remeshed
    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::FillHolesWithIDs()
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    int maxFaceID=GetMaxFaceID();
    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::FillHolesWithIDs(m_WholeVtkPolyData, maxFaceID, 2);
    if(newvpd==NULL)
        return false;

    int *newFaceIDs;
    int numFaces = 0;
    if (PlyDtaUtils_GetFaceIds(newvpd,&numFaces,&newFaceIDs) != CV_OK)
    {
      fprintf(stderr,"Could not get face ids\n");
      return false;
    }

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<numFaces;i++)
    {
        int newID=newFaceIDs[i];
        if(newID>maxFaceID){
            svFace* face=new svFace;
            face->id=newID;
            face->name="noname_"+std::to_string(newID);;
            face->vpd=CreateFaceVtkPolyData(newID);
            m_Faces.push_back(face);
        }

    }

    m_SelectedCellIDs.clear();

    return true;

}

bool svModelElementPolyData::ExtractFaces(double angle)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    int numFaces = 0;
    if (PlyDtaUtils_GetBoundaryFaces(m_WholeVtkPolyData,angle,&numFaces) != CV_OK)
    {
        fprintf(stderr,"Could not extract faces\n");
        return false;
    }

    int *faceIDs;
    if (PlyDtaUtils_GetFaceIds(m_WholeVtkPolyData,&numFaces,&faceIDs) != CV_OK)
    {
        fprintf(stderr,"Could not get face ids\n");
        return false;
    }

    m_Faces.clear();

    for(int i=0;i<numFaces;i++)
    {
        svFace* face=new svFace;
        face->id=faceIDs[i];
        face->name="noname_"+std::to_string(faceIDs[i]);;
        face->vpd=CreateFaceVtkPolyData(faceIDs[i]);
        m_Faces.push_back(face);
    }

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::FillHoles()
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkFillHolesFilter> filler=vtkSmartPointer<vtkFillHolesFilter>::New();
    filler->SetHoleSize(filler->GetHoleSizeMaxValue());
    filler->SetInputDataObject(m_WholeVtkPolyData);
    filler->Update();

    vtkSmartPointer<vtkPolyData> newvpd=filler->GetOutput();
    if(newvpd==NULL)
        return false;

    if(newvpd->GetNumberOfCells()==0)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::SelectLargestConnectedRegion()
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkCleanPolyData> merge = vtkSmartPointer<vtkCleanPolyData>::New();
    merge->SetTolerance(svMath3::GetMachineEpsilon());
    merge->SetInputDataObject(m_WholeVtkPolyData);
    merge->Update();

    vtkSmartPointer<vtkPolyData> mergedpd=merge->GetOutput();

    vtkSmartPointer<vtkPolyDataConnectivityFilter> connfilt=vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();
    connfilt->SetInputDataObject(mergedpd);
    connfilt->SetExtractionModeToLargestRegion();
    connfilt->Update();

    vtkSmartPointer<vtkPolyData> newvpd=connfilt->GetOutput();
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::Decimate(double targetRate)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkQuadricDecimation> decimator=vtkSmartPointer<vtkQuadricDecimation>::New();
    decimator->SetTargetReduction(targetRate);
    decimator->SetInputDataObject(m_WholeVtkPolyData);
    decimator->Update();

    vtkSmartPointer<vtkPolyData> newvpd=decimator->GetOutput();
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::LaplacianSmooth(int numIters, double relaxFactor)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkSmoothPolyDataFilter> smoother=vtkSmartPointer<vtkSmoothPolyDataFilter>::New();
    smoother->SetInputDataObject(m_WholeVtkPolyData);
    smoother->SetRelaxationFactor(relaxFactor);
    smoother->SetNumberOfIterations(numIters);
    //smoother->SetFeatureAngle(30.0);
    smoother->FeatureEdgeSmoothingOff();
    smoother->BoundarySmoothingOff();
    smoother->Update();

    vtkSmartPointer<vtkPolyData> newvpd=smoother->GetOutput();
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::ButterflySubdivide(int numDivs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkButterflySubdivisionFilter> butt=vtkSmartPointer<vtkButterflySubdivisionFilter>::New();
    butt->SetInputDataObject(m_WholeVtkPolyData);
    butt->SetNumberOfSubdivisions(numDivs);
    butt->Update();

    vtkSmartPointer<vtkPolyData> newvpd=butt->GetOutput();
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::WindowSincSmooth(int numIters, double band)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkWindowedSincPolyDataFilter> smoother=vtkSmartPointer<vtkWindowedSincPolyDataFilter>::New();
    smoother->SetInputDataObject(m_WholeVtkPolyData);
    smoother->SetPassBand(band);
    smoother->SetNumberOfIterations(numIters);
    //smoother->SetFeatureAngle(30.0);
    smoother->FeatureEdgeSmoothingOff();
    smoother->BoundarySmoothingOff();
    smoother->Update();

    vtkSmartPointer<vtkPolyData> newvpd=smoother->GetOutput();
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::Densify(int numDivs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkDensifyPolyData> densy=vtkSmartPointer<vtkDensifyPolyData>::New();
    densy->SetInputDataObject(m_WholeVtkPolyData);
    densy->SetNumberOfSubdivisions(numDivs);
    densy->Update();

    vtkSmartPointer<vtkPolyData> newvpd=densy->GetOutput();
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

std::vector<int> svModelElementPolyData::GetSelectedCellIDs()
{
    return m_SelectedCellIDs;
}

void svModelElementPolyData::ClearCellSelection()
{
    m_SelectedCellIDs.clear();
}

bool svModelElementPolyData::SelectCell(int cellID, bool select)
{
    if(cellID==-1)
        return false;

    int foundIndex=-1;
    bool toUpdate=false;

    for(int i=m_SelectedCellIDs.size()-1;i>-1;i--)
    {
        if(m_SelectedCellIDs[i]==cellID)
        {
            foundIndex=i;
            break;
        }
    }

    if(select)
    {
        if(foundIndex==-1)
        {
            m_SelectedCellIDs.push_back(cellID);
            toUpdate=true;
        }
    }
    else
    {
        if(foundIndex!=-1)
        {
            m_SelectedCellIDs.erase(m_SelectedCellIDs.begin()+foundIndex);
            toUpdate=true;
        }
    }

    return toUpdate;
}

bool svModelElementPolyData::DeleteCells(std::vector<int> cellIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    m_WholeVtkPolyData->BuildLinks();

    for (int i=0; i<cellIDs.size();i++)
    {
        m_WholeVtkPolyData->DeleteCell(cellIDs[i]);
    }

    m_WholeVtkPolyData->RemoveDeletedCells();

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::MarkCells(std::vector<int> cellIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::MarkCells(m_WholeVtkPolyData, cellIDs);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool svModelElementPolyData::MarkCellsBySphere(double radius, double center[3])
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::MarkCellsBySphere(m_WholeVtkPolyData, radius, center);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool svModelElementPolyData::MarkCellsByFaces(std::vector<int> faceIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::MarkCellsByFaces(m_WholeVtkPolyData, faceIDs);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool svModelElementPolyData::DecimateLocal(double targetRate)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::DecimateLocal(m_WholeVtkPolyData, targetRate);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::LaplacianSmoothLocal(int numIters, double relaxFactor)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::LaplacianSmoothLocal(m_WholeVtkPolyData, numIters, relaxFactor);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

//    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::ConstrainSmoothLocal(int numIters, double constrainFactor, int numCGSolves)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::ConstrainSmoothLocal(m_WholeVtkPolyData, numIters, constrainFactor, numCGSolves);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

//    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::LinearSubdivideLocal(int numDivs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::LinearSubdivideLocal(m_WholeVtkPolyData, numDivs);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::CutByPlane(double origin[3], double point1[3], double point2[3], bool above )
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::CutByPlane(m_WholeVtkPolyData, origin, point1, point2, above);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}

bool svModelElementPolyData::CutByBox(vtkSmartPointer<vtkPlanes> boxPlanes, bool inside)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=svModelUtils::CutByBox(m_WholeVtkPolyData, boxPlanes, inside);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}


void svModelElementPolyData::RemoveActiveCells()
{
    if(m_WholeVtkPolyData==NULL)
        return;

    m_WholeVtkPolyData->GetCellData()->RemoveArray("ActiveCells");
}
