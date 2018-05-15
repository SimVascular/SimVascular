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

#include "sv4gui_ModelElementPolyData.h"

#include "sv4gui_Math3.h"
#include "sv4gui_ModelUtils.h"

#include "sv_PolyDataSolid.h"
#include "sv_polydatasolid_utils.h"
#ifdef SV_USE_VMTK
  #include "sv_vmtk_utils.h"
#endif
#ifdef SV_USE_MMG
  #include "sv_mmg_mesh_utils.h"
#endif

#include <vtkCleanPolyData.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>
#include <vtkFillHolesFilter.h>
#include <vtkPolyDataConnectivityFilter.h>
#include <vtkQuadricDecimation.h>
#include <vtkSmoothPolyDataFilter.h>
#include <vtkButterflySubdivisionFilter.h>
#include <vtkWindowedSincPolyDataFilter.h>
#include <vtkDensifyPolyData.h>

#include <iostream>

sv4guiModelElementPolyData::sv4guiModelElementPolyData()
{
    m_Type="PolyData";
    std::vector<std::string> exts={"vtp","vtk","vtu","stl","ply"};
    m_FileExtensions=exts;
}

sv4guiModelElementPolyData::sv4guiModelElementPolyData(const sv4guiModelElementPolyData &other)
    : sv4guiModelElement(other)
    , m_SelectedCellIDs(other.m_SelectedCellIDs)
{
}

sv4guiModelElementPolyData::~sv4guiModelElementPolyData()
{
}

sv4guiModelElementPolyData* sv4guiModelElementPolyData::Clone()
{
    return new sv4guiModelElementPolyData(*this);
}

vtkSmartPointer<vtkPolyData> sv4guiModelElementPolyData::CreateFaceVtkPolyData(int id)
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

vtkSmartPointer<vtkPolyData> sv4guiModelElementPolyData::CreateWholeVtkPolyData()
{
    return m_WholeVtkPolyData;
}

std::vector<int> sv4guiModelElementPolyData::GetFaceIDsFromInnerSolid()
{
    std::vector<int> ids;

    if(m_WholeVtkPolyData)
    {
        if (VtkUtils_PDCheckArrayName(m_WholeVtkPolyData,1,"ModelFaceID") == SV_OK)
        {
            int *faceIds=NULL;
            int numBoundaryRegions=0;
            int result = PlyDtaUtils_GetFaceIds( m_WholeVtkPolyData, &numBoundaryRegions, &faceIds);
            if(result==SV_OK)
            {
                for(int i=0;i<numBoundaryRegions;++i)
                    ids.push_back(faceIds[i]);
            }
            delete [] faceIds;
        }
    }

    return ids;
}

bool sv4guiModelElementPolyData::DeleteFaces(std::vector<int> faceIDs)
{
    if(!sv4guiModelUtils::DeleteRegions(m_WholeVtkPolyData, faceIDs))
        return false;

    for(int i=0;i<faceIDs.size();i++)
    {
        RemoveFace(faceIDs[i]);

        RemoveFaceFromBlendParamRadii(faceIDs[i]);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool sv4guiModelElementPolyData::CombineFaces(std::vector<int> faceIDs)
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

bool sv4guiModelElementPolyData::RemeshFaces(std::vector<int> faceIDs, double size)
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

#ifdef SV_USE_MMG
    if (excluded->GetNumberOfIds() == m_Faces.size())
    {
      double hausd = 0.01;
      double angle = 45.0;
      double hgrad = 1.1;
      vtkDoubleArray *meshSizingFunction = NULL;
      int useSizingFunction = 0;
      int numAddedRefines = 0;

      if ( MMGUtils_SurfaceRemeshing( m_WholeVtkPolyData, size, size, hausd, angle, hgrad,
        useSizingFunction, meshSizingFunction, numAddedRefines) != SV_OK ) {
          fprintf(stderr,"Issue while remeshing surface\n");
          return false;
        }
      }
    else
    {
#endif

      int meshcaps = 1;
      int preserveedges = 0;
      //     int triangleoutput = 1;
      double collapseanglethreshold = 0;
      double trianglesplitfactor = 0;
      int useSizeFunction = 0;
      std::string markerListName = "ModelFaceID";

      if (VMTKUtils_SurfaceRemeshing(m_WholeVtkPolyData,size,meshcaps,preserveedges,
                                     trianglesplitfactor,collapseanglethreshold,excluded,
                                     markerListName,useSizeFunction,NULL) != SV_OK)
      {
          fprintf(stderr,"Issue while remeshing surface\n");
          return false;
      }

#ifdef SV_USE_MMG
    }
#endif

    //update all faces; some excluded faces may be remeshed
    for(int i=0;i<m_Faces.size();i++)
    {
        m_Faces[i]->vpd=CreateFaceVtkPolyData(m_Faces[i]->id);
    }

    m_SelectedCellIDs.clear();

    return true;
}

bool sv4guiModelElementPolyData::FillHolesWithIDs()
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    int maxFaceID=GetMaxFaceID();
    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::FillHolesWithIDs(m_WholeVtkPolyData, maxFaceID, 2);
    if(newvpd==NULL)
        return false;

    int *newFaceIDs;
    int numFaces = 0;
    if (PlyDtaUtils_GetFaceIds(newvpd,&numFaces,&newFaceIDs) != SV_OK)
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

bool sv4guiModelElementPolyData::ExtractFaces(double angle)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    int numFaces = 0;
    if (PlyDtaUtils_GetBoundaryFaces(m_WholeVtkPolyData,angle,&numFaces) != SV_OK)
    {
        fprintf(stderr,"Could not extract faces\n");
        return false;
    }

    int *faceIDs;
    if (PlyDtaUtils_GetFaceIds(m_WholeVtkPolyData,&numFaces,&faceIDs) != SV_OK)
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

bool sv4guiModelElementPolyData::FillHoles()
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

bool sv4guiModelElementPolyData::SelectLargestConnectedRegion()
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkCleanPolyData> merge = vtkSmartPointer<vtkCleanPolyData>::New();
    merge->SetTolerance(sv4guiMath3::GetMachineEpsilon());
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

bool sv4guiModelElementPolyData::RemeshG(double hmax, double hmin)
{
#ifdef SV_USE_MMG
    if(m_WholeVtkPolyData==NULL)
    {
      return false;
    }

    double hausd = 0.01;
    double angle = 45.0;
    double hgrad = 1.1;
    vtkDoubleArray *meshSizingFunction = NULL;
    int useSizingFunction = 0;
    int numAddedRefines = 0;

  if ( MMGUtils_SurfaceRemeshing( m_WholeVtkPolyData, hmin, hmax, hausd, angle, hgrad,
	  useSizingFunction, meshSizingFunction, numAddedRefines) != SV_OK ) {
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
#else

    fprintf(stderr,"MMG module does not exist, cannot remesh\n");
    return false;
#endif
}

bool sv4guiModelElementPolyData::Decimate(double targetRate)
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

bool sv4guiModelElementPolyData::LaplacianSmooth(int numIters, double relaxFactor)
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

bool sv4guiModelElementPolyData::ButterflySubdivide(int numDivs)
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

bool sv4guiModelElementPolyData::WindowSincSmooth(int numIters, double band)
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

bool sv4guiModelElementPolyData::Densify(int numDivs)
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

std::vector<int> sv4guiModelElementPolyData::GetSelectedCellIDs()
{
    return m_SelectedCellIDs;
}

void sv4guiModelElementPolyData::ClearCellSelection()
{
    m_SelectedCellIDs.clear();
}

bool sv4guiModelElementPolyData::SelectCell(int cellID, bool select)
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

bool sv4guiModelElementPolyData::DeleteCells(std::vector<int> cellIDs)
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

bool sv4guiModelElementPolyData::MarkCells(std::vector<int> cellIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::MarkCells(m_WholeVtkPolyData, cellIDs);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool sv4guiModelElementPolyData::MarkCellsBySphere(double radius, double center[3])
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::MarkCellsBySphere(m_WholeVtkPolyData, radius, center);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool sv4guiModelElementPolyData::MarkCellsByFaces(std::vector<int> faceIDs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::MarkCellsByFaces(m_WholeVtkPolyData, faceIDs);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool sv4guiModelElementPolyData::MarkCellsByFaceJunctions(std::vector<int> faceIDs, double radius)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::MarkCellsByFaceJunctions(m_WholeVtkPolyData, faceIDs, radius);

    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    return true;
}

bool sv4guiModelElementPolyData::DecimateLocal(double targetRate)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::DecimateLocal(m_WholeVtkPolyData, targetRate);
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

bool sv4guiModelElementPolyData::LaplacianSmoothLocal(int numIters, double relaxFactor)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::LaplacianSmoothLocal(m_WholeVtkPolyData, numIters, relaxFactor);
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

bool sv4guiModelElementPolyData::ConstrainSmoothLocal(int numIters, double constrainFactor, int numCGSolves)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::ConstrainSmoothLocal(m_WholeVtkPolyData, numIters, constrainFactor, numCGSolves);
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

bool sv4guiModelElementPolyData::LinearSubdivideLocal(int numDivs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::LinearSubdivideLocal(m_WholeVtkPolyData, numDivs);
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

bool sv4guiModelElementPolyData::LoopSubdivideLocal(int numDivs)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::LoopSubdivideLocal(m_WholeVtkPolyData, numDivs);
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

bool sv4guiModelElementPolyData::CutByPlane(double origin[3], double point1[3], double point2[3], bool above )
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::CutByPlane(m_WholeVtkPolyData, origin, point1, point2, above);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}

bool sv4guiModelElementPolyData::CutByBox(vtkSmartPointer<vtkPlanes> boxPlanes, bool inside)
{
    if(m_WholeVtkPolyData==NULL)
        return false;

    vtkSmartPointer<vtkPolyData> newvpd=sv4guiModelUtils::CutByBox(m_WholeVtkPolyData, boxPlanes, inside);
    if(newvpd==NULL)
        return false;

    m_WholeVtkPolyData=newvpd;

    m_Faces.clear();

    m_BlendRadii.clear();

    m_SelectedCellIDs.clear();

    return true;
}


void sv4guiModelElementPolyData::RemoveActiveCells()
{
    if(m_WholeVtkPolyData==NULL)
        return;

    m_WholeVtkPolyData->GetCellData()->RemoveArray("ActiveCells");
}

sv4guiModelElement* sv4guiModelElementPolyData::CreateModelElement()
{
    return new sv4guiModelElementPolyData();
}

sv4guiModelElement* sv4guiModelElementPolyData::CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                , int numSamplingPts
                                , svLoftingParam *param
                                , int* stats
                                , double maxDist
                                , int noInterOut
                                , double tol
                                , unsigned int t)
{
    return sv4guiModelUtils::CreateModelElementPolyData(segNodes,numSamplingPts,stats,param,t,noInterOut,tol);
}

sv4guiModelElement* sv4guiModelElementPolyData::CreateModelElementByBlend(std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii
                                                  , sv4guiModelElement::svBlendParam* param)
{
    return sv4guiModelUtils::CreateModelElementPolyDataByBlend(this,blendRadii,param);
}

bool sv4guiModelElementPolyData::ReadFile(std::string filePath)
{
    vtkSmartPointer<vtkPolyData> pd=vtkSmartPointer<vtkPolyData>::New();
    if(PlyDtaUtils_ReadNative(const_cast<char*>(filePath.c_str()), pd) != SV_OK)
        return false;

    vtkSmartPointer<vtkCleanPolyData> cleaner =vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->SetInputData(pd);
    cleaner->Update();

    vtkSmartPointer<vtkPolyData> cleanpd=cleaner->GetOutput();
    cleanpd->BuildLinks();

    m_WholeVtkPolyData=cleanpd;

    return true;
}

bool sv4guiModelElementPolyData::WriteFile(std::string filePath)
{
    if(m_WholeVtkPolyData)
    {
        if (PlyDtaUtils_WriteNative(m_WholeVtkPolyData, 0, const_cast<char*>(filePath.c_str()) ) != SV_OK)
            return false;
    }

    return true;
}

