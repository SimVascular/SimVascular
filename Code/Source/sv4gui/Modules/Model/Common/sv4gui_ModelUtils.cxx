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

#include "sv4gui_ModelUtils.h"

#include "sv4gui_ModelElementPolyData.h"
#include "sv3_PathElement.h"
#include "sv4gui_PathElement.h"
#include "sv4gui_Math3.h"
#include "sv4gui_MitkSeg3D.h"

#include "SimVascular.h"
#include "sv_sys_geom.h"
#include "sv_vmtk_utils.h"
#include "sv_PolyData.h"
#include "sv_polydatasolid_utils.h"

#include <vtkCellType.h>
#include <vtkFillHolesFilter.h>
#include <vtkPlaneSource.h>
#include <vtkClipPolyData.h>
#include <vtkImplicitDataSet.h>
#include <vtkThreshold.h>
#include <vtkDataSetSurfaceFilter.h>
#include "vtkSVGlobals.h"
#include "vtkSVNURBSSurface.h"
#include <vtkCenterOfMass.h>

#include "vtkXMLPolyDataWriter.h"

vtkPolyData* sv4guiModelUtils::CreatePolyData(std::vector<sv4guiContourGroup*> groups, std::vector<vtkPolyData*> vtps, int numSamplingPts, svLoftingParam *param, unsigned int t, int noInterOut, double tol)
{
    int groupNumber=groups.size();
    int vtpNumber=vtps.size();
    cvPolyData **srcs=new cvPolyData* [groupNumber+vtpNumber];
    for(int i=0;i<groupNumber;i++)
    {
      sv4guiContourGroup* group=groups[i];
      vtkPolyData *vtkpd = CreateLoftSurface(group,numSamplingPts,1,param,t);
      if (vtkpd == NULL)
      {
        for (int j=0; j< i-1; j++)
          delete srcs[j];
        delete [] srcs;
        return NULL;
      }
      srcs[i]=new cvPolyData(vtkpd);
      vtkpd->Delete();
    }

    for(int i=0;i<vtpNumber;i++)
    {
        if(vtps[i]==NULL)
        {
            for(int j=0;j<i+groupNumber-1;j++)
                delete srcs[j];
            delete [] srcs;
            return NULL;
        }
        vtkPolyData* newvtp=vtkPolyData::New();
        newvtp->DeepCopy(vtps[i]);

        if(!newvtp->GetCellData()->HasArray("CapID"))
        {
            vtkIntArray* capArray=vtkIntArray::New();
            capArray->SetName("CapID");
            for(int i=0;i<newvtp->GetNumberOfCells();i++)
                capArray->InsertNextValue(-1);

            newvtp->GetCellData()->AddArray(capArray);
        }

        cvPolyData* cvpd=new cvPolyData(newvtp);
        srcs[i+groupNumber]=cvpd;
        newvtp->Delete();
    }

    cvPolyData *dst=NULL;

    int status=sys_geom_all_union(srcs, groupNumber+vtpNumber, noInterOut, tol, &dst);

    for(int i=0;i<groupNumber+vtpNumber;i++)
    {
        delete srcs[i];
    }
    delete [] srcs;

    if(status!=SV_OK)
        return NULL;
    else
        return dst->GetVtkPolyData();
}

sv4guiModelElementPolyData* sv4guiModelUtils::CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, int stats[], svLoftingParam *param, unsigned int t, int noInterOut, double tol)
{
    std::vector<sv4guiContourGroup*> groups;
    std::vector<vtkPolyData*> vtps;
    std::vector<std::string> segNames;

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*>(segNode->GetData());
        if(group!=NULL)
        {
            groups.push_back(group);
            segNames.push_back(segNode->GetName());
        }
    }

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        sv4guiMitkSeg3D* seg3D=dynamic_cast<sv4guiMitkSeg3D*>(segNode->GetData());
        if(seg3D && seg3D->GetVtkPolyData())
        {
            vtps.push_back(seg3D->GetVtkPolyData());
            segNames.push_back(segNode->GetName());
        }
    }

    vtkPolyData* solidvpd=CreatePolyData(groups,vtps,numSamplingPts,param,t,noInterOut,tol);
    if(solidvpd==NULL) return NULL;

    cvPolyData *src=new cvPolyData(solidvpd);
    cvPolyData *dst = NULL;

    if(stats&&sys_geom_checksurface(src,stats,tol)!=SV_OK)
    {
      solidvpd->Delete();
      return NULL;
    }

    int *doublecaps;
    int numfaces=0;

    if (sys_geom_set_ids_for_caps(src, &dst,  &doublecaps,&numfaces) != SV_OK)
    {
      solidvpd->Delete();
      return NULL;
    }

    vtkSmartPointer<vtkPolyData> forClean =
      vtkSmartPointer<vtkPolyData>::New();
    forClean->DeepCopy(dst->GetVtkPolyData());
    vtkSmartPointer<vtkPolyData> nowClean =
      vtkSmartPointer<vtkPolyData>::New();
    nowClean = sv4guiModelUtils::OrientVtkPolyData(forClean);

    solidvpd->DeepCopy(nowClean);;

    int numSeg=segNames.size();
    int numCap2=0;
    for(int i=numSeg-1;i>-1;i--)
    {
        if(doublecaps[i]!=0)
        {
            numCap2=doublecaps[i];
            break;
        }
    }

    std::string *allNames=new std::string[2*numSeg+numCap2];

    for(int i=0;i<numSeg;i++)
    {
        allNames[i]="wall_"+segNames[i];
        allNames[numSeg+i]="cap_"+segNames[i];
        if(doublecaps[i]!=0)
            allNames[2*numSeg+doublecaps[i]-1]="cap_"+segNames[i]+"_2";
    }

    std::vector<sv4guiModelElement::svFace*> faces;

    for(int i=0;i<2*numSeg+numCap2;i++)
    {
        vtkPolyData *facepd = vtkPolyData::New();
        int faceid=i+1;
        PlyDtaUtils_GetFacePolyData(solidvpd, &faceid, facepd);

        if(facepd==NULL||facepd->GetNumberOfPoints()==0)
            continue;

        sv4guiModelElement::svFace* face =new sv4guiModelElement::svFace;
        face->id=faceid;
        face->name=allNames[i];
        face->vpd=facepd;

        if(face->name.substr(0,5)=="wall_")
            face->type="wall";
        else if(face->name.substr(0,4)=="cap_")
            face->type="cap";

        faces.push_back(face);
    }

    delete[] allNames;

    sv4guiModelElementPolyData* modelElement=new sv4guiModelElementPolyData();
    modelElement->SetSegNames(segNames);
    modelElement->SetFaces(faces);
    modelElement->SetWholeVtkPolyData(solidvpd);
    modelElement->SetNumSampling(numSamplingPts);


    bool ok = false;
    if(modelElement->MarkCellsByFaces(modelElement->GetCapFaceIDs()))
    {
      int numDivs = 1;
      ok=modelElement->LinearSubdivideLocal(numDivs);
    }
    if(!ok)
    {
      MITK_ERROR << "Failed to subdivide caps of created PolyData";
      return NULL;
    }

    return modelElement;
}

vtkPolyData* sv4guiModelUtils::CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, sv4guiModelElement::svBlendParam* param)
{
    if(vpdsrc==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(vpdsrc);
    cvPolyData *dst = NULL;

    int vals[2];
    vals[0]=faceID1;
    vals[1]=faceID2;

    if ( sys_geom_set_array_for_local_op_face_blend(src,&dst, "ModelFaceID", vals, 2, radius, "ActiveCells", 1)
         != SV_OK )
    {
        MITK_ERROR << "poly blend (using radius) error ";
        return NULL;
    }

    cvPolyData *dst2 = NULL;

    if ( sys_geom_local_blend( dst, &dst2, param->numblenditers,
                               param->numsubblenditers, param->numsubdivisioniters,
                               param->numcgsmoothiters, param->numlapsmoothiters,
                               param->targetdecimation,
                               NULL, "ActiveCells")

         != SV_OK )
    {
        MITK_ERROR << "poly blend error ";
        return NULL;
    }

    vtkPolyData* vpd=dst2->GetVtkPolyData();
    vpd->GetCellData()->RemoveArray("ActiveCells");

    return vpd;

}

sv4guiModelElementPolyData* sv4guiModelUtils::CreateModelElementPolyDataByBlend(sv4guiModelElementPolyData* mepdsrc, std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii, sv4guiModelElement::svBlendParam* param)
{

    vtkSmartPointer<vtkPolyData> oldVpd=mepdsrc->GetWholeVtkPolyData();
    if(oldVpd==NULL) return NULL;

    vtkSmartPointer<vtkPolyData> lastVpd=oldVpd;

    for(int i=0;i<blendRadii.size();i++)
    {
        int faceID1=0;
        int faceID2=0;
        double radius=0.0;
        if(blendRadii[i] && blendRadii[i]->radius>0)
        {
            faceID1=blendRadii[i]->faceID1;
            faceID2=blendRadii[i]->faceID2;
            radius=blendRadii[i]->radius;

        }

        lastVpd=sv4guiModelUtils::CreatePolyDataByBlend(lastVpd, faceID1, faceID2, radius, param);

        if(lastVpd==NULL) return NULL;

    }

    sv4guiModelElementPolyData* mepddst =mepdsrc->Clone();
    mepddst->SetWholeVtkPolyData(lastVpd);
    std::vector<sv4guiModelElement::svFace*> faces=mepddst->GetFaces();
    for(int i=0;i<faces.size();i++)
    {
        faces[i]->vpd=mepddst->CreateFaceVtkPolyData(faces[i]->id);
    }

    mepddst->AssignBlendParam(param);

    mepddst->AddBlendRadii(blendRadii);

    return mepddst;
}

vtkPolyData* sv4guiModelUtils::CreateLoftSurface(sv4guiContourGroup* contourGroup, int numSamplingPts, int addCaps, svLoftingParam* param, unsigned int t)
{

    svLoftingParam* usedParam= contourGroup->GetLoftingParam();
    if(param!=NULL) usedParam=param;

    std::vector<sv4guiContour*> contourSet=contourGroup->GetValidContourSet(t);

    return CreateLoftSurface(contourSet,numSamplingPts,usedParam,addCaps);
}

vtkPolyData* sv4guiModelUtils::CreateLoftSurface(std::vector<sv4guiContour*> contourSet, int numSamplingPts, svLoftingParam* param, int addCaps)
{
    int contourNumber=contourSet.size();
    if (contourNumber < 2)
      return NULL;

    if(param==NULL)
        return NULL;

    param->numOutPtsAlongLength=param->samplePerSegment*contourNumber;
    param->numPtsInLinearSampleAlongLength=param->linearMuliplier*param->numOutPtsAlongLength;

    param->numSuperPts=0;
    for(int i=0;i<contourNumber;i++)
    {
        int pointNunumber=contourSet[i]->GetContourPointNumber();
        if(pointNunumber>param->numSuperPts)
            param->numSuperPts=pointNunumber;
    }

    if(param->numOutPtsInSegs>param->numSuperPts)
        param->numSuperPts=param->numOutPtsInSegs;

    int newNumSamplingPts=param->numOutPtsInSegs;

    if(numSamplingPts>3)
    {
        newNumSamplingPts=numSamplingPts;
        if(numSamplingPts>param->numSuperPts)
            param->numSuperPts=numSamplingPts;
    }

    std::vector<cvPolyData*> superSampledContours;
    for(int i=0;i<contourNumber;i++)
    {
        vtkPolyData* vtkpd=vtkPolyData::New();

        vtkpd->DeepCopy(contourSet[i]->CreateVtkPolyDataFromContour(false));
        cvPolyData* cvpd=new cvPolyData(vtkpd);
        vtkpd->Delete();
        cvPolyData* cvpd2=sys_geom_sampleLoop(cvpd,param->numSuperPts);
        if(cvpd2==NULL)
        {
            MITK_ERROR << "Supersampling error ";
            return NULL;
        }
        superSampledContours.push_back(cvpd2);
    }

    std::vector<cvPolyData*> alignedContours;
    for(int i=0;i<contourNumber;i++)
    {
        if(i==0)
        {
            alignedContours.push_back(superSampledContours[0]);
        }
        else
        {
            cvPolyData* cvpd3;
            if(param->vecFlag==1)
                cvpd3=sys_geom_Align(alignedContours[i-1],superSampledContours[i]);
            else
                cvpd3=sys_geom_AlignByDist(alignedContours[i-1],superSampledContours[i]);

            if(cvpd3==NULL)
            {
                MITK_ERROR << "aligning error ";
                // Clean up
                for (int i=0; i<contourNumber; i++)
                  delete superSampledContours[i];

                return NULL;
            }

            alignedContours.push_back(cvpd3);
        }
    }

    cvPolyData **sampledContours=new cvPolyData*[contourNumber];
    for(int i=0;i<contourNumber;i++)
    {
        cvPolyData * cvpd4=sys_geom_sampleLoop(alignedContours[i],newNumSamplingPts);
        if(cvpd4==NULL)
        {
            MITK_ERROR << "sampling error ";
            for (int j=0; j<i; j++)
              delete sampledContours[j];
            delete [] sampledContours;
            return NULL;
        }
        sampledContours[i]=cvpd4;
    }

    cvPolyData *dst;
    vtkPolyData* outpd=NULL;

    if (param->method=="spline")
    {
      if ( sys_geom_loft_solid(sampledContours, contourNumber,param->useLinearSampleAlongLength,param->useFFT,
                               param->numOutPtsAlongLength,newNumSamplingPts,
                               param->numPtsInLinearSampleAlongLength,param->numModes,param->splineType,param->bias,param->tension,param->continuity,
                               &dst )
           != SV_OK )
      {
          MITK_ERROR << "poly manipulation error ";
          outpd=NULL;
      }
      else
      {

          if(addCaps==1)
              outpd=CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
          else
              outpd=CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
      }
    }
    else if (param->method=="nurbs")
    {
      // Degrees of surface
      int uDegree = param->uDegree;
      int vDegree = param->vDegree;

      // Override to maximum possible degree if too large a degree for given number of inputs!
      if (uDegree >= contourNumber)
        uDegree = contourNumber-1;
      if (vDegree >= newNumSamplingPts)
        vDegree = newNumSamplingPts-1;

      // Set to average knot span and chord length if just two inputs
      if (contourNumber == 2)
      {
        param->uKnotSpanType = "average";
        param->uParametricSpanType = "chord";
      }
      if (newNumSamplingPts == 2)
      {
        param->uKnotSpanType = "average";
        param->uParametricSpanType = "chord";
      }

      // Output spacing function of given input points
      double uSpacing = 1.0/param->numOutPtsAlongLength;
      double vSpacing = 1.0/newNumSamplingPts;

      // span types
      const char *uKnotSpanType       = param->uKnotSpanType.c_str();
      const char *vKnotSpanType       = param->vKnotSpanType.c_str();
      const char *uParametricSpanType = param->uParametricSpanType.c_str();
      const char *vParametricSpanType = param->vParametricSpanType.c_str();
      vtkNew(vtkSVNURBSSurface, NURBSSurface);

      if ( sys_geom_loft_solid_with_nurbs(sampledContours, contourNumber,
                                          uDegree, vDegree, uSpacing,
                                          vSpacing, uKnotSpanType,
                                          vKnotSpanType,
                                          uParametricSpanType,
                                          vParametricSpanType,
                                          NURBSSurface,
                                          &dst )
           != SV_OK )
      {
          MITK_ERROR << "poly manipulation error ";
          outpd=NULL;
      }
      else
      {
          if (PlyDtaUtils_CheckLoftSurface(dst->GetVtkPolyData()) != SV_OK)
          {
            MITK_ERROR << "Error lofting surface";
            outpd=NULL;
          }
          else
          {
            if(addCaps==1)
                outpd=CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
            else
                outpd=CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
          }

      }
    }

    // Clean up
    for (int i=0; i<contourNumber; i++)
    {
      delete superSampledContours[i];
      delete sampledContours[i];
    }
    delete [] sampledContours;

    if(dst!=NULL) delete dst;

    return outpd;

}

vtkPolyData* sv4guiModelUtils::CreateOrientOpenPolySolidVessel(vtkPolyData* inpd)
{
    int originalCellNumber=inpd->GetNumberOfCells();

    vtkPolyData* tmppd=FillHoles(inpd);

    vtkSmartPointer<vtkPolyDataNormals> nrmls = vtkSmartPointer<vtkPolyDataNormals>::New();
    nrmls->SplittingOff();
    nrmls->ConsistencyOn();
    nrmls->AutoOrientNormalsOn();
    nrmls->ComputeCellNormalsOn();
    nrmls->ComputePointNormalsOff();
    nrmls->SetInputData(tmppd);
    nrmls->Update();

    vtkPolyData* outpd=vtkPolyData::New();
    outpd->DeepCopy(nrmls->GetOutput());
    //    int VTK_TRIANGLE=5;
    for(int i=outpd->GetNumberOfCells()-1;i>=originalCellNumber;i--)
    {
        if(outpd->GetCellType(i)==VTK_TRIANGLE)
            outpd->DeleteCell(i);
    }
    outpd->RemoveDeletedCells();

    tmppd->Delete();

    return outpd;
}

vtkPolyData* sv4guiModelUtils::FillHoles(vtkPolyData* inpd)
{
    vtkSmartPointer<vtkFillHolesFilter> filler = vtkSmartPointer<vtkFillHolesFilter>::New();
    filler->SetHoleSize(filler->GetHoleSizeMaxValue());
    filler->SetInputData(inpd);
    filler->Update();

    return Orient(filler->GetOutput());
}

vtkPolyData* sv4guiModelUtils::Orient(vtkPolyData* inpd)
{
    vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->PointMergingOn();
    cleaner->ConvertLinesToPointsOff();
    cleaner->ConvertPolysToLinesOff();
    cleaner->SetInputData(inpd);
    cleaner->Update();

    vtkSmartPointer<vtkPolyDataNormals> orienter = vtkSmartPointer<vtkPolyDataNormals>::New();
    orienter->SetInputData(cleaner->GetOutput());
    orienter->AutoOrientNormalsOn();
    orienter->ComputePointNormalsOff();
    orienter->FlipNormalsOn();
    orienter->SplittingOff();
    orienter->ComputeCellNormalsOn();
    orienter->ConsistencyOn();
    orienter->NonManifoldTraversalOff();
    orienter->Update();

    vtkPolyData* outpd=vtkPolyData::New();
    outpd->DeepCopy(orienter->GetOutput());

    return outpd;
}

vtkPolyData* sv4guiModelUtils::CreateOrientClosedPolySolidVessel(vtkPolyData* inpd)
{
    int fillID=0;
    int fillType=0;

    vtkPolyData* tmppd=FillHolesWithIDs(inpd,fillID,fillType);

    vtkSmartPointer<vtkPolyDataNormals> nrmls = vtkSmartPointer<vtkPolyDataNormals>::New();
    nrmls->SplittingOff();
    nrmls->ConsistencyOn();
    nrmls->AutoOrientNormalsOn();
    nrmls->ComputeCellNormalsOn();
    nrmls->ComputePointNormalsOff();
    nrmls->SetInputData(tmppd);
    nrmls->Update();

    vtkPolyData* outpd=vtkPolyData::New();
    outpd->DeepCopy(nrmls->GetOutput());

    tmppd->Delete();

    return outpd;
}

vtkPolyData* sv4guiModelUtils::FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType)
{
    cvPolyData* cvpd=new cvPolyData(inpd);
    int numFilled=0;
    cvPolyData* tmpcvpd;
    if(sys_geom_cap_with_ids(cvpd,&tmpcvpd,fillID,numFilled,fillType)!=SV_OK)
        return NULL;

    if(tmpcvpd==NULL)
        return NULL;

    vtkPolyData* outpd=Orient(tmpcvpd->GetVtkPolyData());

    delete tmpcvpd;

    return outpd;
}

bool sv4guiModelUtils::CheckArrayName(vtkDataSet *object,int datatype,std::string arrayname )
{
    vtkIdType i;
    int numArrays;

    if (datatype == 0)
    {
        numArrays = object->GetPointData()->GetNumberOfArrays();
        for (i=0;i<numArrays;i++)
        {
            if (strcmp(object->GetPointData()->GetArrayName(i),arrayname.c_str())==0)
            {
                return true;
            }
        }

        //    if(object->GetPointData()->HasArray(arrayname.c_str()))
        //        return true;

    }
    else
    {
        numArrays = object->GetCellData()->GetNumberOfArrays();
        for (i=0;i<numArrays;i++)
        {
            if (strcmp(object->GetCellData()->GetArrayName(i),arrayname.c_str())==0)
            {
                return true;
            }
        }

        //    if(object->GetCellData()->HasArray(arrayname.c_str()))
        //        return true;
    }

    return false;
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::OrientVtkPolyData(vtkSmartPointer<vtkPolyData> inpd)
{
    vtkSmartPointer<vtkCleanPolyData> cleaner=vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->PointMergingOn();
    cleaner->ConvertLinesToPointsOff();
    cleaner->ConvertPolysToLinesOff();
    cleaner->SetInputDataObject(inpd);
    cleaner->Update();

    vtkSmartPointer<vtkPolyDataNormals> orienter=vtkSmartPointer<vtkPolyDataNormals>::New();
    orienter->SetInputDataObject(cleaner->GetOutput());
    orienter->AutoOrientNormalsOn();
    orienter->ComputePointNormalsOff();
    orienter->SplittingOff();
    orienter->ComputeCellNormalsOn();
    orienter->ConsistencyOn();
    orienter->NonManifoldTraversalOff();
    orienter->Update();

    return orienter->GetOutput();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::MarkCells(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> cellIDs)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    int* cellIDArray = &cellIDs[0];

    if ( sys_geom_set_array_for_local_op_cells(src, &dst, cellIDArray, cellIDs.size(), "ActiveCells", 1) != SV_OK )
    {
        MITK_ERROR << "poly marking cells (by cell ids) error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}


vtkSmartPointer<vtkPolyData> sv4guiModelUtils::MarkCellsBySphere(vtkSmartPointer<vtkPolyData> inpd, double radius, double center[3])
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    if ( sys_geom_set_array_for_local_op_sphere(src, &dst, radius, center, "ActiveCells", 1) != SV_OK )
    {
        MITK_ERROR << "poly marking cells (by sphere) error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::MarkCellsByFaces(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> faceIDs)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    int* faceIDArray = &faceIDs[0];

    if ( sys_geom_set_array_for_local_op_face(src, &dst, "ModelFaceID", faceIDArray, faceIDs.size(), "ActiveCells", 1) != SV_OK )
    {
        MITK_ERROR << "poly marking cells (by face ids) error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::MarkCellsByFaceJunctions(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> faceIDs, double radius)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    int* faceIDArray = &faceIDs[0];

    if ( sys_geom_set_array_for_local_op_face_blend(src, &dst, "ModelFaceID", faceIDArray, faceIDs.size(), radius, "ActiveCells", 1) != SV_OK )
    {
        MITK_ERROR << "poly marking cells (by face functions) error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::DecimateLocal(vtkSmartPointer<vtkPolyData> inpd, double targetRate)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    if ( sys_geom_local_quadric_decimation(src, &dst, targetRate, NULL, "ActiveCells") != SV_OK )
    {
        MITK_ERROR << "poly local decimation error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::LaplacianSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double relaxFactor)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    if ( sys_geom_local_laplacian_smooth(src, &dst, numIters, relaxFactor, NULL, "ActiveCells") != SV_OK )
    {
        MITK_ERROR << "poly local Laplacian smooth error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::ConstrainSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double constrainFactor, int numCGSolves)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    if ( sys_geom_local_constrain_smooth(src, &dst, numIters, constrainFactor, numCGSolves, NULL, "ActiveCells") != SV_OK )
    {
        MITK_ERROR << "poly local constrain smooth error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::LinearSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    if ( sys_geom_local_linear_subdivision(src, &dst, numDivs, NULL, "ActiveCells") != SV_OK )
    {
        MITK_ERROR << "poly local linear subdivision error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::LoopSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(inpd);
    cvPolyData *dst = NULL;

    if ( sys_geom_local_loop_subdivision(src, &dst, numDivs, NULL, "ActiveCells") != SV_OK )
    {
        MITK_ERROR << "poly local loop subdivision error ";
        return NULL;
    }

    return dst->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::CutByPlane(vtkSmartPointer<vtkPolyData> inpd, double origin[3], double point1[3], double point2[3], bool above )
{
    if(inpd==NULL)
        return NULL;

    vtkSmartPointer<vtkPlaneSource> plane= vtkSmartPointer<vtkPlaneSource>::New();
    plane->SetOrigin(origin);
    plane->SetPoint1(point1);
    plane->SetPoint2(point2);
    plane->Update();

    double* nrm=plane->GetNormal();
    nrm[0]=-nrm[0];
    nrm[1]=-nrm[1];
    nrm[2]=-nrm[2];

    vtkSmartPointer<vtkPlane> impPlane=vtkSmartPointer<vtkPlane>::New();
    impPlane->SetOrigin(origin);
    impPlane->SetNormal(nrm);

    vtkSmartPointer<vtkClipPolyData> clipper=vtkSmartPointer<vtkClipPolyData>::New();
    clipper->SetInputData(inpd);
    clipper->GenerateClippedOutputOn();
    clipper->SetClipFunction(impPlane);
    clipper->Update();

    vtkSmartPointer<vtkFillHolesFilter> triangulator=vtkSmartPointer<vtkFillHolesFilter>::New();
    if(above)
    {
        triangulator->SetInputData(clipper->GetOutput());
    }
    else
    {
        triangulator->SetInputData(clipper->GetClippedOutput());
    }
    triangulator->Update();

    return triangulator->GetOutput();
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::CutByBox(vtkSmartPointer<vtkPolyData> inpd, vtkSmartPointer<vtkPlanes> boxPlanes, bool inside)
{
    if(inpd==NULL)
        return NULL;

    if(boxPlanes==NULL)
        return NULL;

    vtkSmartPointer<vtkClipPolyData> clipper=vtkSmartPointer<vtkClipPolyData>::New();
    clipper->SetInputData(inpd);
    clipper->GenerateClippedOutputOn();
    clipper->SetClipFunction(boxPlanes);
    clipper->Update();

    vtkSmartPointer<vtkTriangleFilter> triangulator=vtkSmartPointer<vtkTriangleFilter>::New();
    if(inside)
    {
        triangulator->SetInputData(clipper->GetOutput());
    }
    else
    {
        triangulator->SetInputData(clipper->GetClippedOutput());
    }
    triangulator->Update();

    return triangulator->GetOutput();
}

//---------------
// DeleteRegions
//---------------
// Delete the cells from a vtkPolyData with ModelFaceID given in 'regionIDs'.
//
bool sv4guiModelUtils::DeleteRegions(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> regionIDs)
{
    if (inpd == NULL) {
        return false;
    }

    std::string arrayname="ModelFaceID";
    bool existing=false;

    if(inpd->GetCellData()->HasArray(arrayname.c_str())) {
        existing=true;
    }

    if(!existing) {
        return false;
    }

    for (int i = 0; i < regionIDs.size(); i++) {
        auto boundaryRegions = vtkSmartPointer<vtkIntArray>::New();
        boundaryRegions = vtkIntArray::SafeDownCast(inpd->GetCellData()-> GetScalars("ModelFaceID"));
        inpd->BuildLinks();

        for (vtkIdType cellId = 0; cellId < inpd->GetNumberOfCells(); cellId++) {
            if (boundaryRegions->GetValue(cellId) == regionIDs[i]) {
                inpd->DeleteCell(cellId);
            }
        }

        inpd->RemoveDeletedCells();
    }

    return true;
}

//-------------------
// CreateCenterlines
//-------------------
// Compute the centerlines for the given model.
//
// Note: Model caps are removed and replaced with new geometry. It is not clear why this 
// is done, perhaps to produce a better geometry for centerline computation; the new caps 
// are defined with a single node in the center of the model face.
//
vtkPolyData* 
sv4guiModelUtils::CreateCenterlines(sv4guiModelElement* modelElement, vtkIdList *sourceCapIds, bool getSections)
{
    if(modelElement==NULL || modelElement->GetWholeVtkPolyData()==NULL) {
        return NULL;
    }

    // Copy model twice (?)
    auto inpd = vtkSmartPointer<vtkPolyData>::New();
    inpd->DeepCopy(modelElement->GetWholeVtkPolyData());
    auto fullpd = vtkSmartPointer<vtkPolyData>::New();
    fullpd->DeepCopy(modelElement->GetWholeVtkPolyData());

    // Remove the cells defining the model caps.
    if (!DeleteRegions(inpd, modelElement->GetCapFaceIDs())) {
        return NULL;
    }

    cvPolyData *src = new cvPolyData(inpd);
    auto cleaned = sys_geom_Clean(src);
    delete src;

    // Recap the model surface.
    //
    cvPolyData *capped  = NULL;
    int numCapCenterIds;
    int *capCenterIds = NULL;
    int capUsingCenter = 1;     // Cap using a point in the cap center.

    if (sys_geom_cap_for_centerlines(cleaned, &capped, &numCapCenterIds, &capCenterIds, capUsingCenter ) != SV_OK) {
      delete cleaned;
      if (capped != NULL) {
        delete capped;
      }
      return NULL;
    }

    if (numCapCenterIds < 2) {
      delete cleaned;
      if (capped != NULL) {
        delete capped;
      }
      return NULL;
    }
    delete cleaned;

    bool capIdsGiven = false;
    if ((sourceCapIds != NULL) && (sourceCapIds->GetNumberOfIds() > 0)) {
        capIdsGiven = true;
    }

    // Define the source and target cap node IDs used to compute the centerline.
    //
    auto sourcePtIds = vtkSmartPointer<vtkIdList>::New();
    auto targetPtIds = vtkSmartPointer<vtkIdList>::New();

    if (!capIdsGiven) {
      sourcePtIds->InsertNextId(capCenterIds[0]);
      for (int i = 1; i < numCapCenterIds; i++) {
        targetPtIds->InsertNextId(capCenterIds[i]);
      }

    // Get the node IDs closest to the cap centers. 
    //
    // capFaceId's are added to a map to produce a sorted list. 
    //
    } else {
      auto locator = vtkSmartPointer<vtkCellLocator>::New();
      locator->SetDataSet(fullpd);
      locator->BuildLocator();
      auto genericCell = vtkSmartPointer<vtkGenericCell>::New();
      std::map<int,int> facePtIdMap;

      for (int i = 0;  i < numCapCenterIds; i++) {
        int ptId = capCenterIds[i];
        double capPt[3];
        capped->GetVtkPolyData()->GetPoint(ptId, capPt);

        int subId;
        double closestPt[3];
        vtkIdType closestCellId;
        double distance;
        locator->FindClosestPoint(capPt, closestPt, genericCell, closestCellId, subId, distance);

        int capFaceId = fullpd->GetCellData()->GetArray("ModelFaceID")->GetTuple1(closestCellId);
        facePtIdMap[capFaceId] = ptId;
      }

      // Add point IDs to the source and target lists.
      for (auto face : facePtIdMap) { 
        int capFaceId = face.first;
        int ptId = face.second;
        if (sourceCapIds->IsId(capFaceId) != -1) {
          sourcePtIds->InsertNextId(ptId);
        } else {
          targetPtIds->InsertNextId(ptId);
        }
      }
    }

    delete [] capCenterIds;

    vtkPolyData* centerlines;

    if (getSections) {
      centerlines = CreateCenterlineSections(capped->GetVtkPolyData(), sourcePtIds, targetPtIds);
    } else {
      centerlines = CreateCenterlines(capped->GetVtkPolyData(), sourcePtIds, targetPtIds);
    }

    delete capped;
    return centerlines;
}

vtkPolyData* sv4guiModelUtils::CreateCenterlines(vtkPolyData* inpd)
{
  // If given just a polydata, assume it is a wall, cap and get source and
  // target points and then send to centerline extraction

  // Cap the solid to get centerline ids
  cvPolyData *src = new cvPolyData(inpd);
  cvPolyData *cleaned = NULL;
  cvPolyData *capped  = NULL;
  int numCapCenterIds;
  int *capCenterIds=NULL;

  cleaned = sys_geom_Clean(src);

  if ( sys_geom_cap_for_centerlines(cleaned, &capped, &numCapCenterIds, &capCenterIds, 1 ) != SV_OK)
  {
    delete cleaned;
    if (capped != NULL)
      delete capped;
    return NULL;
  }
  if (numCapCenterIds < 2)
  {
    delete cleaned;
    if (capped != NULL)
      delete capped;
    return NULL;
  }
  delete cleaned;

  vtkSmartPointer<vtkIdList> sourcePtIds = vtkSmartPointer<vtkIdList>::New();
  sourcePtIds->InsertNextId(capCenterIds[0]);
  vtkSmartPointer<vtkIdList> targetPtIds = vtkSmartPointer<vtkIdList>::New();
  for (int i=1; i<numCapCenterIds; i++)
    targetPtIds->InsertNextId(capCenterIds[i]);

  delete [] capCenterIds;
  // capped and got ids

  return CreateCenterlines(capped->GetVtkPolyData(), sourcePtIds, targetPtIds);

}


vtkPolyData* sv4guiModelUtils::CreateCenterlines(vtkPolyData* inpd,
                                             vtkIdList *sourcePtIds,
                                             vtkIdList *targetPtIds)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src = new cvPolyData(inpd);
    cvPolyData *tempCenterlines = NULL;
    cvPolyData *voronoi = NULL;

    int numSourcePts = sourcePtIds->GetNumberOfIds();
    int *sources=new int[numSourcePts];
    for (int i=0; i<numSourcePts; i++)
      sources[i]=sourcePtIds->GetId(i);

    int numTargetPts = targetPtIds->GetNumberOfIds();
    int *targets=new int[numTargetPts];
    for (int i=0; i<numTargetPts; i++)
      targets[i]=targetPtIds->GetId(i);

    if ( sys_geom_centerlines(src, sources, numSourcePts, targets, numTargetPts, &tempCenterlines, &voronoi) != SV_OK )
    {
        delete src;
        delete [] sources;
        delete [] targets;
        return NULL;
    }
    delete src;
    delete voronoi;
    delete [] sources;
    delete [] targets;

    cvPolyData *centerlines=NULL;
    if ( sys_geom_separatecenterlines(tempCenterlines, &centerlines) != SV_OK )
    {
        delete tempCenterlines;
        return NULL;
    }
    delete tempCenterlines;

    return centerlines->GetVtkPolyData();
}

vtkPolyData* sv4guiModelUtils::CreateCenterlineSections(vtkPolyData* inpd,
                                                        vtkIdList *sourcePtIds,
                                                        vtkIdList *targetPtIds)
{
    if(inpd==NULL)
        return NULL;

    cvPolyData *src = new cvPolyData(inpd);
    cvPolyData *tempCenterlines = NULL;
    cvPolyData *voronoi = NULL;

    int numSourcePts = sourcePtIds->GetNumberOfIds();
    int *sources=new int[numSourcePts];
    for (int i=0; i<numSourcePts; i++)
      sources[i]=sourcePtIds->GetId(i);

    int numTargetPts = targetPtIds->GetNumberOfIds();
    int *targets=new int[numTargetPts];
    for (int i=0; i<numTargetPts; i++)
      targets[i]=targetPtIds->GetId(i);

    if ( sys_geom_centerlines(src, sources, numSourcePts, targets, numTargetPts, &tempCenterlines, &voronoi) != SV_OK )
    {
        delete src;
        delete [] sources;
        delete [] targets;
        return NULL;
    }
    delete voronoi;
    delete [] sources;
    delete [] targets;

    cvPolyData *centerlines=NULL;
    cvPolyData *surf_grouped=NULL;
    cvPolyData *sections=NULL;
    if ( sys_geom_centerlinesections(tempCenterlines, src, &centerlines, &surf_grouped, &sections) != SV_OK )
    {
        delete src;
        delete centerlines;
        delete surf_grouped;
        delete sections;
        return NULL;
    }
    delete src;
    delete surf_grouped;
    delete sections;

    return centerlines->GetVtkPolyData();
}

vtkPolyData* sv4guiModelUtils::MergeCenterlines(vtkPolyData* centerlinesPD)
{
    if(centerlinesPD==NULL)
        return NULL;

    cvPolyData *centerlines =new cvPolyData(centerlinesPD);

    cvPolyData *merged_centerlines=NULL;
    int mergeblanked = 1;
    if (sys_geom_mergecenterlines(centerlines, mergeblanked, &merged_centerlines) != SV_OK )
    {
      delete centerlines;
      return NULL;
    }
    delete centerlines;

    return merged_centerlines->GetVtkPolyData();
}

vtkPolyData* sv4guiModelUtils::CalculateDistanceToCenterlines(vtkPolyData* centerlines, vtkPolyData* original)
{
    if(centerlines==NULL || original==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(original);
    cvPolyData *lines=new cvPolyData(centerlines);
    cvPolyData *distance = NULL;
    if ( sys_geom_distancetocenterlines(src, lines, &distance) != SV_OK )
    {
        return NULL;
    }

    return distance->GetVtkPolyData();
}

std::vector<sv4guiPathElement::sv4guiPathPoint> sv4guiModelUtils::ConvertToPathPoints(std::vector<mitk::Point3D> posPoints)
{
    std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints;

    for(int i=0;i<posPoints.size()-1;i++)
    {
        sv4guiPathElement::sv4guiPathPoint pathPoint;
        pathPoint.pos=posPoints[i];
        pathPoint.tangent=posPoints[i+1]-posPoints[i];
        pathPoint.tangent.Normalize();
        pathPoint.rotation=sv4guiMath3::GetPerpendicularNormalVector(pathPoint.tangent);

        pathPoints.push_back(pathPoint);
    }

    sv4guiPathElement::sv4guiPathPoint lastPathPoint=pathPoints.back();
    lastPathPoint.pos=posPoints.back();
    pathPoints.push_back(lastPathPoint);

    return pathPoints;
}

vtkSmartPointer<vtkPolyData> sv4guiModelUtils::GetThresholdRegion(vtkSmartPointer<vtkPolyData> pd, vtkDataObject::FieldAssociations dataType, std::string arrayName, double minValue, double maxValue )
{
    vtkSmartPointer<vtkThreshold> thresholder=vtkSmartPointer<vtkThreshold>::New();
    thresholder->SetInputData(pd);
    thresholder->SetInputArrayToProcess(0, 0, 0, dataType, arrayName.c_str());
    thresholder->ThresholdBetween(minValue, maxValue);
    thresholder->Update();

    vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer=vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
    surfacer->SetInputData(thresholder->GetOutput());
    surfacer->Update();

    return surfacer->GetOutput();
}

std::vector<sv4guiPathElement*> sv4guiModelUtils::CreatePathElements(sv4guiModelElement* modelElement,
                                                             vtkSmartPointer<vtkPolyData> centerlinesPD)
{
    std::vector<sv4guiPathElement*> pathElements;

    if(centerlinesPD==NULL || !centerlinesPD->GetCellData()->HasArray("CenterlineIds"))
        return pathElements;

    int numCenterlines=centerlinesPD->GetCellData()->GetArray("CenterlineIds")->GetRange()[1]+1;

    for(int i=0;i<numCenterlines;i++)
    {
        vtkSmartPointer<vtkPolyData> polyline=GetThresholdRegion(centerlinesPD,vtkDataObject::FIELD_ASSOCIATION_CELLS, "CenterlineIds", i, i);

        vtkSmartPointer<vtkDataArray> groupArray=polyline->GetCellData()->GetArray("GroupIds");
        int lowerValue=groupArray->GetRange()[0];
        int upperValue=groupArray->GetRange()[1];

        vtkSmartPointer<vtkPolyData> centerline=GetThresholdRegion(polyline,vtkDataObject::FIELD_ASSOCIATION_CELLS,"GroupIds",lowerValue,upperValue);
        std::vector<mitk::Point3D> posPoints;
        for(int j=0;j<centerline->GetNumberOfPoints();j++)
        {
            mitk::Point3D point;
            point[0]=centerline->GetPoint(j)[0];
            point[1]=centerline->GetPoint(j)[1];
            point[2]=centerline->GetPoint(j)[2];

            posPoints.push_back(point);
        }

        sv4guiPathElement* pe=new sv4guiPathElement();
        pe->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
        pe->SetCalculationNumber(centerline->GetNumberOfPoints());
        //pe->SetControlPoints(controlPoints,false);
        pe->SetPathPoints(ConvertToPathPoints(posPoints));

        pathElements.push_back(pe);
    }

    return pathElements;
}

double sv4guiModelUtils::CalculateVpdArea(vtkPolyData* vpd)
{
    if(vpd==NULL)
        return 0;

    double area=0;
    cvPolyData *src=new cvPolyData(vpd);

    if ( sys_geom_SurfArea(src, &area) != SV_OK )
    {
        return 0;
    }

    return area;
}

bool sv4guiModelUtils::CheckPolyDataSurface(vtkPolyData* pd, std::string &msg)
{
    if(pd==NULL)
    {
      msg = "Polydata is empty\n";
      return SV_ERROR;
    }

    pd->BuildLinks();

    int numPolys  = pd->GetNumberOfCells();
    int numPoints = pd->GetNumberOfPoints();

    bool valid=true;
    int numNotTriangles=0;
    int numOpenEdges=0;
    int numNonManifoldEdges=0;
    for (int i=0; i<numPolys; i++)
    {
      vtkIdType npts, *pts;
      pd->GetCellPoints(i, npts, pts);
      if (npts != 3)
      {
        valid = false;
        numNotTriangles++;
      }
      for (int j=0; j<npts; j++)
      {
        vtkIdType p0, p1;
        p0 = pts[j];
        p1 = pts[(j+1)%npts];

        vtkNew(vtkIdList, edgeNeighbor);
        pd->GetCellEdgeNeighbors(i, p0, p1, edgeNeighbor);

        if (edgeNeighbor->GetNumberOfIds() > 1)
          numNonManifoldEdges++;
        else if (edgeNeighbor->GetNumberOfIds() == 0)
          numOpenEdges++;
      }
    }

    msg = "";
    if (!valid)
    {
      msg = msg + "Surface contains non-triangular elements!\n";
      msg = msg + "  Number of non-triangular elements: "+ std::to_string(numNotTriangles) + "\n";
    }
    msg = msg +  "  Number of elements: " + std::to_string(numPolys) + "\n";
    msg = msg +  "  Number of points: " + std::to_string(numPoints) + "\n";
    msg = msg +  "  Number of  non-manifold edges: " + std::to_string(numNonManifoldEdges) + "\n";
    msg = msg +  "  Number of  open edges: " + std::to_string(numOpenEdges) + "\n";

    return valid;
}

bool sv4guiModelUtils::TriangulateSurface(vtkPolyData* pd)
{
  vtkNew(vtkTriangleFilter, triangulator);
  triangulator->SetInputData(pd);
  triangulator->Update();

  pd->DeepCopy(triangulator->GetOutput());

  return SV_OK;
}
