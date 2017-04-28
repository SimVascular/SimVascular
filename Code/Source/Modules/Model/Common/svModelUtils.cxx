#include "svModelUtils.h"

#include "svModelElementPolyData.h"
#include "svPathElement.h"
#include "svMath3.h"

#include "SimVascular.h"
#include "cv_sys_geom.h"
#include "cvPolyData.h"
#include "cv_polydatasolid_utils.h"

#include "mitkSurface.h"

#include <vtkCellType.h>
#include <vtkFillHolesFilter.h>
#include <vtkPlaneSource.h>
#include <vtkClipPolyData.h>
#include <vtkImplicitDataSet.h>
#include <vtkThreshold.h>
#include <vtkDataSetSurfaceFilter.h>
#include "vtkSVGlobals.h"
#include "vtkSVNURBSSurface.h"

vtkPolyData* svModelUtils::CreatePolyData(std::vector<svContourGroup*> groups, std::vector<vtkPolyData*> vtps, int numSamplingPts, svModelElement::svNURBSLoftParam *nurbsParam, unsigned int t, int noInterOut, double tol)
{
    int groupNumber=groups.size();
    int vtpNumber=vtps.size();
    cvPolyData **srcs=new cvPolyData* [groupNumber+vtpNumber];
    for(int i=0;i<groupNumber;i++)
    {
      svContourGroup* group=groups[i];
      vtkPolyData *vtkpd = CreateLoftSurface(group,numSamplingPts,nurbsParam,1,t);
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

svModelElementPolyData* svModelUtils::CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, int stats[], svModelElement::svNURBSLoftParam *nurbsParam, unsigned int t, int noInterOut, double tol)
{
    std::vector<svContourGroup*> groups;
    std::vector<vtkPolyData*> vtps;
    std::vector<std::string> segNames;

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        svContourGroup* group = dynamic_cast<svContourGroup*>(segNode->GetData());
        if(group!=NULL)
        {
            groups.push_back(group);
            segNames.push_back(segNode->GetName());
        }
    }

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        mitk::Surface* surface=dynamic_cast<mitk::Surface*>(segNode->GetData());
        if(surface && surface->GetVtkPolyData())
        {
            vtps.push_back(surface->GetVtkPolyData());
            segNames.push_back(segNode->GetName());
        }
    }

    vtkPolyData* solidvpd=CreatePolyData(groups,vtps,numSamplingPts,nurbsParam,t,noInterOut,tol);
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
    nowClean = svModelUtils::OrientVtkPolyData(forClean);

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

    std::vector<svModelElement::svFace*> faces;

    for(int i=0;i<2*numSeg+numCap2;i++)
    {
        vtkPolyData *facepd = vtkPolyData::New();
        int faceid=i+1;
        PlyDtaUtils_GetFacePolyData(solidvpd, &faceid, facepd);

        if(facepd==NULL||facepd->GetNumberOfPoints()==0)
            continue;

        svModelElement::svFace* face =new svModelElement::svFace;
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

    svModelElementPolyData* modelElement=new svModelElementPolyData();
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

vtkPolyData* svModelUtils::CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svModelElement::svBlendParam* param)
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

svModelElementPolyData* svModelUtils::CreateModelElementPolyDataByBlend(svModelElementPolyData* mepdsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii, svModelElement::svBlendParam* param)
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

        lastVpd=svModelUtils::CreatePolyDataByBlend(lastVpd, faceID1, faceID2, radius, param);

        if(lastVpd==NULL) return NULL;

    }

    svModelElementPolyData* mepddst =mepdsrc->Clone();
    mepddst->SetWholeVtkPolyData(lastVpd);
    std::vector<svModelElement::svFace*> faces=mepddst->GetFaces();
    for(int i=0;i<faces.size();i++)
    {
        faces[i]->vpd=mepddst->CreateFaceVtkPolyData(faces[i]->id);
    }

    mepddst->AssignBlendParam(param);
    delete param;

    mepddst->AddBlendRadii(blendRadii);

    return mepddst;
}

vtkPolyData* svModelUtils::CreateLoftSurface(svContourGroup* contourGroup, int numSamplingPts, svModelElement::svNURBSLoftParam *nurbsParam, int addCaps, unsigned int t,  svContourGroup::svLoftingParam* param)
{

    svContourGroup::svLoftingParam* usedParam= contourGroup->GetLoftingParam();
    if(param!=NULL) usedParam=param;

    std::vector<svContour*> contourSet=contourGroup->GetValidContourSet(t);

    return CreateLoftSurface(contourSet,numSamplingPts,nurbsParam,usedParam,addCaps);
}

vtkPolyData* svModelUtils::CreateLoftSurface(std::vector<svContour*> contourSet, int numSamplingPts, svModelElement::svNURBSLoftParam *nurbsParam, svContourGroup::svLoftingParam* param, int addCaps)
{
    int contourNumber=contourSet.size();
    if (contourNumber < 2)
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
    vtkPolyData* outpd;

    if (nurbsParam==NULL || nurbsParam->advancedLofting == 0)
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
    else
    {
      // Degrees of surface
      int uDegree = nurbsParam->uDegree;
      int vDegree = nurbsParam->vDegree;

      // Override to maximum possible degree if too large a degree for given number of inputs!
      if (uDegree >= contourNumber)
        uDegree = contourNumber-1;
      if (vDegree >= newNumSamplingPts)
        vDegree = newNumSamplingPts-1;

      // Set to average knot span and chord length if just two inputs
      if (contourNumber == 2)
      {
        nurbsParam->uKnotSpanType = "average";
        nurbsParam->uParametricSpanType = "chord";
      }
      if (newNumSamplingPts == 2)
      {
        nurbsParam->uKnotSpanType = "average";
        nurbsParam->uParametricSpanType = "chord";
      }

      // Output spacing function of given input points
      double uSpacing = 1.0/param->numOutPtsAlongLength;
      double vSpacing = 1.0/newNumSamplingPts;

      // span types
      const char *uKnotSpanType       = nurbsParam->uKnotSpanType.c_str();
      const char *vKnotSpanType       = nurbsParam->vKnotSpanType.c_str();
      const char *uParametricSpanType = nurbsParam->uParametricSpanType.c_str();
      const char *vParametricSpanType = nurbsParam->vParametricSpanType.c_str();
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

          if(addCaps==1)
              outpd=CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
          else
              outpd=CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
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

vtkPolyData* svModelUtils::CreateOrientOpenPolySolidVessel(vtkPolyData* inpd)
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

vtkPolyData* svModelUtils::FillHoles(vtkPolyData* inpd)
{
    vtkSmartPointer<vtkFillHolesFilter> filler = vtkSmartPointer<vtkFillHolesFilter>::New();
    filler->SetHoleSize(filler->GetHoleSizeMaxValue());
    filler->SetInputData(inpd);
    filler->Update();

    return Orient(filler->GetOutput());
}

vtkPolyData* svModelUtils::Orient(vtkPolyData* inpd)
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

vtkPolyData* svModelUtils::CreateOrientClosedPolySolidVessel(vtkPolyData* inpd)
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

vtkPolyData* svModelUtils::FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType)
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

bool svModelUtils::CheckArrayName(vtkDataSet *object,int datatype,std::string arrayname )
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

vtkSmartPointer<vtkPolyData> svModelUtils::OrientVtkPolyData(vtkSmartPointer<vtkPolyData> inpd)
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

vtkSmartPointer<vtkPolyData> svModelUtils::MarkCells(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> cellIDs)
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


vtkSmartPointer<vtkPolyData> svModelUtils::MarkCellsBySphere(vtkSmartPointer<vtkPolyData> inpd, double radius, double center[3])
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

vtkSmartPointer<vtkPolyData> svModelUtils::MarkCellsByFaces(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> faceIDs)
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

vtkSmartPointer<vtkPolyData> svModelUtils::DecimateLocal(vtkSmartPointer<vtkPolyData> inpd, double targetRate)
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

vtkSmartPointer<vtkPolyData> svModelUtils::LaplacianSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double relaxFactor)
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

vtkSmartPointer<vtkPolyData> svModelUtils::ConstrainSmoothLocal(vtkSmartPointer<vtkPolyData> inpd, int numIters, double constrainFactor, int numCGSolves)
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

vtkSmartPointer<vtkPolyData> svModelUtils::LinearSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs)
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

vtkSmartPointer<vtkPolyData> svModelUtils::LoopSubdivideLocal(vtkSmartPointer<vtkPolyData> inpd, int numDivs)
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

vtkSmartPointer<vtkPolyData> svModelUtils::CutByPlane(vtkSmartPointer<vtkPolyData> inpd, double origin[3], double point1[3], double point2[3], bool above )
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

vtkSmartPointer<vtkPolyData> svModelUtils::CutByBox(vtkSmartPointer<vtkPolyData> inpd, vtkSmartPointer<vtkPlanes> boxPlanes, bool inside)
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

bool svModelUtils::DeleteRegions(vtkSmartPointer<vtkPolyData> inpd, std::vector<int> regionIDs)
{
    if(inpd==NULL)
        return false;

    std::string arrayname="ModelFaceID";
    bool existing=false;

    if(inpd->GetCellData()->HasArray(arrayname.c_str()))
        existing=true;

    if(!existing)
        return false;

    for(int i=0;i<regionIDs.size();i++)
    {
        vtkSmartPointer<vtkIntArray> boundaryRegions = vtkSmartPointer<vtkIntArray>::New();
        boundaryRegions = vtkIntArray::SafeDownCast(inpd->GetCellData()-> GetScalars("ModelFaceID"));

        inpd->BuildLinks();

        for (vtkIdType cellId=0; cellId< inpd->GetNumberOfCells(); cellId++)
        {
            if (boundaryRegions->GetValue(cellId) == regionIDs[i])
            {
                inpd->DeleteCell(cellId);
            }
        }

        inpd->RemoveDeletedCells();
    }

    return true;
}

vtkPolyData* svModelUtils::CreateCenterlines(svModelElement* modelElement)
{
    if(modelElement==NULL || modelElement->GetWholeVtkPolyData()==NULL)
        return NULL;

    vtkSmartPointer<vtkPolyData> inpd=vtkSmartPointer<vtkPolyData>::New();
    inpd->DeepCopy(modelElement->GetWholeVtkPolyData());
    if(!DeleteRegions(inpd,modelElement->GetCapFaceIDs()))
    {
        return NULL;
    }

    vtkPolyData* centerlines=CreateCenterlines(inpd);

    return centerlines;
}

vtkPolyData* svModelUtils::CreateCenterlines(vtkPolyData* vpd)
{
    if(vpd==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(vpd);
    cvPolyData *cleaned = NULL;
    cvPolyData *capped  = NULL;
    int numCapCenterIDs;
    int *capCenterIDs=NULL;

    cleaned = sys_geom_Clean(src);
    delete src;

    if ( sys_geom_cap(cleaned, &capped, &numCapCenterIDs, &capCenterIDs, 1 ) != SV_OK || numCapCenterIDs<2)
    {
        //        delete capped;
        delete cleaned;
        if (capped != NULL)
          delete capped;
        return NULL;
    }
    delete cleaned;

    cvPolyData *tempCenterlines = NULL;
    cvPolyData *voronoi = NULL;

    int *sources=new int[1];
    sources[0]=capCenterIDs[0];

    int *targets=new int[numCapCenterIDs-1];
    for(int i=1;i<numCapCenterIDs;i++)
        targets[i-1]= capCenterIDs[i];

    if ( sys_geom_centerlines(capped, sources, 1, targets, numCapCenterIDs-1, &tempCenterlines, &voronoi) != SV_OK )
    {
        delete capped;
        return NULL;
    }
    delete capped;

    cvPolyData *temp2Centerlines=NULL;
    if ( sys_geom_separatecenterlines(tempCenterlines, &temp2Centerlines) != SV_OK )
    {
        delete tempCenterlines;
        return NULL;
    }
    delete tempCenterlines;

    cvPolyData *centerlines=NULL;
    int mergeblanked = 1;
    if (sys_geom_mergecenterlines(temp2Centerlines, mergeblanked, &centerlines) != SV_OK )
    {
      delete temp2Centerlines;
      return NULL;
    }
    delete temp2Centerlines;

    return centerlines->GetVtkPolyData();
}

vtkPolyData* svModelUtils::CalculateDistanceToCenterlines(vtkPolyData* centerlines, vtkPolyData* original)
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

std::vector<svPathElement::svPathPoint> svModelUtils::ConvertToPathPoints(std::vector<mitk::Point3D> posPoints)
{
    std::vector<svPathElement::svPathPoint> pathPoints;

    for(int i=0;i<posPoints.size()-1;i++)
    {
        svPathElement::svPathPoint pathPoint;
        pathPoint.pos=posPoints[i];
        pathPoint.tangent=posPoints[i+1]-posPoints[i];
        pathPoint.tangent.Normalize();
        pathPoint.rotation=svMath3::GetPerpendicularNormalVector(pathPoint.tangent);

        pathPoints.push_back(pathPoint);
    }

    svPathElement::svPathPoint lastPathPoint=pathPoints.back();
    lastPathPoint.pos=posPoints.back();
    pathPoints.push_back(lastPathPoint);

    return pathPoints;
}

vtkSmartPointer<vtkPolyData> svModelUtils::GetThresholdRegion(vtkSmartPointer<vtkPolyData> pd, vtkDataObject::FieldAssociations dataType, std::string arrayName, double minValue, double maxValue )
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

std::vector<svPathElement*> svModelUtils::CreatePathElements(svModelElement* modelElement)
{
    std::vector<svPathElement*> pathElements;

    vtkSmartPointer<vtkPolyData> centerlinesPD=CreateCenterlines(modelElement);

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

        svPathElement* pe=new svPathElement();
        pe->SetMethod(svPathElement::CONSTANT_TOTAL_NUMBER);
        pe->SetCalculationNumber(centerline->GetNumberOfPoints());
        //pe->SetControlPoints(controlPoints,false);
        pe->SetPathPoints(ConvertToPathPoints(posPoints));

        pathElements.push_back(pe);
    }

    return pathElements;
}

double svModelUtils::CalculateVpdArea(vtkPolyData* vpd)
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
