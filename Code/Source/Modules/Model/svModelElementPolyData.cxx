#include "svModelElementPolyData.h"

#include "svModelUtils.h"

#include "cv_polydatasolid_utils.h"
#include "cv_VMTK_utils.h"

#include <vtkFillHolesFilter.h>

#include <iostream>
using namespace std;

svModelElementPolyData::svModelElementPolyData()
{
    m_Type="PolyData";
    m_BlendParam=new svBlendParam();
}

svModelElementPolyData::svModelElementPolyData(const svModelElementPolyData &other)
    : svModelElement(other)
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

bool svModelElementPolyData::DeleteFaces(std::vector<int> faceIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    std::string arrayname="ModelFaceID";
    bool existing=false;

    if(m_WholeVtkPolyData->GetCellData()->HasArray(arrayname.c_str()))
        existing=true;

    if(!existing)
        return false;

    for(int i=0;i<faceIDs.size();i++)
    {
        vtkSmartPointer<vtkIntArray> boundaryRegions = vtkSmartPointer<vtkIntArray>::New();
        boundaryRegions = vtkIntArray::SafeDownCast(m_WholeVtkPolyData->GetCellData()-> GetScalars("ModelFaceID"));

        m_WholeVtkPolyData->BuildLinks();

        for (vtkIdType cellId=0; cellId< m_WholeVtkPolyData->GetNumberOfCells(); cellId++)
        {
          if (boundaryRegions->GetValue(cellId) == faceIDs[i])
          {
            m_WholeVtkPolyData->DeleteCell(cellId);
          }
        }

        m_WholeVtkPolyData->RemoveDeletedCells();

        RemoveFace(faceIDs[i]);

        RemoveFaceFromBlendParamRadii(faceIDs[i]);

    }

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

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    return true;
}
