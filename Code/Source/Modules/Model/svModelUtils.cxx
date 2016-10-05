#include "svModelUtils.h"

#include "svModelElementPolyData.h"

#include "SimVascular.h"
#include "cv_sys_geom.h"
#include "cvPolyData.h"
#include "cv_polydatasolid_utils.h"

#include <vtkCellType.h>
#include <vtkFillHolesFilter.h>

vtkPolyData* svModelUtils::CreatePolyData(std::vector<svContourGroup*> segs, unsigned int t, int noInterOut, double tol)
{
    int groupNumber=segs.size();
    cvPolyData **srcs=new cvPolyData* [groupNumber];
    for(int i=0;i<groupNumber;i++)
    {
        svContourGroup* group=segs[i];
        cvPolyData* cvpd=new cvPolyData(CreateLoftSurface(group,1,t));
        srcs[i]=cvpd;
    }

    cvPolyData *dst=NULL;

    int status=sys_geom_all_union(srcs, groupNumber, noInterOut, tol, &dst);
    if(status!=CV_OK) return NULL;

    for(int i=0;i<groupNumber;i++)
    {
        delete srcs[i];
    }
    delete [] srcs;

    return dst->GetVtkPolyData();
}

static svModelElement* svModelUtils::CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, unsigned int t = 0, int noInterOut = 1, double tol = 1e-6)
{
    std::vector<svContourGroup*> segs;
    std::vector<std::string> segNames;

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        svContourGroup* group = dynamic_cast<svContourGroup*>(segNode->GetData());
        if(group!=NULL)
        {
            segs.push_back(group);
            segNames.push_back(segNode->GetName());
        }
    }

    vtkPolyData* solidvpd=CreatePolyData(segs,t,noInterOut,tol);
    if(solidvpd==NULL) return NULL;

    int *doublecaps;
    int numfaces=0;

    cvPolyData *src=new cvPolyData(solidvpd);
    cvPolyData *dst = NULL;

    sys_geom_set_ids_for_caps(src, &dst,  &doublecaps,&numfaces);

    solidvpd=dst->GetVtkPolyData();

    int totalNumFaces=0;
    for(int i=0;i<numfaces;i++)
    {
        totalNumFaces=totalNumFaces+doublecaps[i]+2;
    }
    std::string *allNames=new std::string[totalNumFaces];

    for(int i=0;i<numfaces;i++)
    {
        allNames[i]="wall_"+segNames[i];
        allNames[i+numfaces]="cap_"+segNames[i];
        if(doublecaps[i]!=0)
            allNames[2*numfaces]="cap_"+segNames[i]+"_2";
    }

//    int numBoundaryRegions;
//    int* faceIds=NULL;
//    PlyDtaUtils_GetFaceIds( solidvpd, &numBoundaryRegions, &faceIds);

//    cout<<numBoundaryRegions<<endl;
//    cout<<faceIds[0]<<","<<faceIds[1]<<","<<faceIds[2]<<","<<faceIds[3]<<","<<faceIds[4]<<endl;

    std::vector<svModelElement::svFace*> faces;

    for(int i=0;i<totalNumFaces;i++)
    {
          vtkPolyData *facepd = vtkPolyData::New();
          int faceid=i+1;
          PlyDtaUtils_GetFacePolyData(solidvpd, &faceid, facepd);

          svModelElement::svFace* face =new svModelElement::svFace;
          face->id=faceid;
          face->name=allNames[i];
          face->vpd=facepd;

          faces.push_back(face);
    }

    svModelElementPolyData* modelElement=new svModelElementPolyData();
    modelElement->SetSegNames(segNames);
    modelElement->SetFaces(faces);
    modelElement->SetWholeVtkPolyData(solidvpd);

    return modelElement;
}

vtkPolyData* svModelUtils::CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svModelUtils::svBlendParamPolyData* param)
{
    if(vpdsrc==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(vpdsrc);
    cvPolyData *dst = NULL;

    int vals[2];
    vals[0]=faceID1;
    vals[1]=faceID2;

    if ( sys_geom_set_array_for_local_op_face_blend(src,&dst, "ModelFaceID", vals, 2, radius, "ActiveCells", 1)
         != CV_OK )
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

         != CV_OK )
    {
        MITK_ERROR << "poly blend error ";
        return NULL;
    }

    vtkPolyData* vpd=dst2->GetVtkPolyData();
    vpd->GetCellData()->RemoveArray("ActiveCells");

    return vpd;

}


svModelElement* svModelUtils::CreateModelElementPolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svBlendParamPolyData* param)
{

}

vtkPolyData* svModelUtils::CreateLoftSurface(svContourGroup* contourGroup, int addCaps, unsigned int t,  svContourGroup::svLoftingParam* param)
{

    svContourGroup::svLoftingParam* usedParam= contourGroup->GetLoftingParam();
    if(param!=NULL) usedParam=param;

    std::vector<svContour*> contourSet=contourGroup->GetContourSet(t);

    return CreateLoftSurface(contourSet,usedParam,addCaps);
}

vtkPolyData* svModelUtils::CreateLoftSurface(std::vector<svContour*> contourSet, svContourGroup::svLoftingParam* param, int addCaps)
{
    int contourNumber=contourSet.size();

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

    std::vector<cvPolyData*> superSampledContours;
    for(int i=0;i<contourNumber;i++)
    {
        vtkPolyData* vtkpd=vtkPolyData::New();

        vtkpd->DeepCopy(contourSet[i]->CreateVtkPolyDataFromContour(false));
        cvPolyData* cvpd=new cvPolyData(vtkpd);
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
                return NULL;
            }

            alignedContours.push_back(cvpd3);
        }
    }

    cvPolyData **sampledContours=new cvPolyData*[contourNumber];
    for(int i=0;i<contourNumber;i++)
    {
        cvPolyData * cvpd4=sys_geom_sampleLoop(alignedContours[i],param->numOutPtsInSegs);
        if(cvpd4==NULL)
        {
            MITK_ERROR << "sampling error ";
            return NULL;
        }
        sampledContours[i]=cvpd4;
    }

    cvPolyData *dst;
    vtkPolyData* outpd;

    if ( sys_geom_loft_solid(sampledContours, contourNumber,param->useLinearSampleAlongLength,param->useFFT,
                             param->numOutPtsAlongLength,param->numOutPtsInSegs,
                             param->numPtsInLinearSampleAlongLength,param->numModes,param->splineType,param->bias,param->tension,param->continuity,
                             &dst )
         != CV_OK )
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
    orienter->ComputePointNormalsOn();
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
    sys_geom_cap_with_ids(cvpd,&tmpcvpd,fillID,numFilled,fillType);

    vtkPolyData* outpd=Orient(tmpcvpd->GetVtkPolyData());

    delete tmpcvpd;

    return outpd;
}
