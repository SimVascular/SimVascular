#include "svModelUtils.h"

#include "svModelElementPolyData.h"
#include "svPathElement.h"
#include "svMath3.h"

#include "SimVascular.h"
#include "cv_sys_geom.h"
#include "cvPolyData.h"
#include "cv_polydatasolid_utils.h"

#include <vtkCellType.h>
#include <vtkFillHolesFilter.h>
#include <vtkPlaneSource.h>
#include <vtkClipPolyData.h>
#include <vtkImplicitDataSet.h>
#include <vtkThreshold.h>
#include <vtkDataSetSurfaceFilter.h>

vtkPolyData* svModelUtils::CreatePolyData(std::vector<svContourGroup*> segs, int numSamplingPts, unsigned int t, int noInterOut, double tol)
{
    int groupNumber=segs.size();
    cvPolyData **srcs=new cvPolyData* [groupNumber];
    for(int i=0;i<groupNumber;i++)
    {
        svContourGroup* group=segs[i];
        cvPolyData* cvpd=new cvPolyData(CreateLoftSurface(group,numSamplingPts,1,t));
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

svModelElementPolyData* svModelUtils::CreateModelElementPolyData(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, int stats[], unsigned int t, int noInterOut, double tol)
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

    vtkPolyData* solidvpd=CreatePolyData(segs,numSamplingPts,t,noInterOut,tol);
    if(solidvpd==NULL) return NULL;

    cvPolyData *src=new cvPolyData(solidvpd);
    cvPolyData *dst = NULL;

    if(sys_geom_checksurface(src,stats,tol)!=CV_OK)
        return NULL;

    int *doublecaps;
    int numfaces=0;

    sys_geom_set_ids_for_caps(src, &dst,  &doublecaps,&numfaces);

    solidvpd=dst->GetVtkPolyData();

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

    return modelElement;
}

vtkPolyData* svModelUtils::CreatePolyDataByBlend(vtkPolyData* vpdsrc, int faceID1, int faceID2, double radius, svModelElementPolyData::svBlendParam* param)
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

svModelElementPolyData* svModelUtils::CreateModelElementPolyDataByBlend(svModelElementPolyData* mepdsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii, svModelElementPolyData::svBlendParam* param)
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

vtkPolyData* svModelUtils::CreateLoftSurface(svContourGroup* contourGroup, int numSamplingPts, int addCaps, unsigned int t,  svContourGroup::svLoftingParam* param)
{

    svContourGroup::svLoftingParam* usedParam= contourGroup->GetLoftingParam();
    if(param!=NULL) usedParam=param;

    std::vector<svContour*> contourSet=contourGroup->GetValidContourSet(t);

    return CreateLoftSurface(contourSet,numSamplingPts,usedParam,addCaps);
}

vtkPolyData* svModelUtils::CreateLoftSurface(std::vector<svContour*> contourSet, int numSamplingPts, svContourGroup::svLoftingParam* param, int addCaps)
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

    int newNumSamplingPts=param->numOutPtsInSegs;

    if(numSamplingPts>0)
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
        cvPolyData * cvpd4=sys_geom_sampleLoop(alignedContours[i],newNumSamplingPts);
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
                             param->numOutPtsAlongLength,newNumSamplingPts,
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
    if(sys_geom_cap_with_ids(cvpd,&tmpcvpd,fillID,numFilled,fillType)!=CV_OK)
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
    orienter->ComputePointNormalsOn();
    orienter->FlipNormalsOn();
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

    if ( sys_geom_set_array_for_local_op_cells(src, &dst, cellIDArray, cellIDs.size(), "ActiveCells", 1) != CV_OK )
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

    if ( sys_geom_set_array_for_local_op_sphere(src, &dst, radius, center, "ActiveCells", 1) != CV_OK )
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

    if ( sys_geom_set_array_for_local_op_face(src, &dst, "ModelFaceID", faceIDArray, faceIDs.size(), "ActiveCells", 1) != CV_OK )
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

    if ( sys_geom_local_quadric_decimation(src, &dst, targetRate, NULL, "ActiveCells") != CV_OK )
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

    if ( sys_geom_local_laplacian_smooth(src, &dst, numIters, relaxFactor, NULL, "ActiveCells") != CV_OK )
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

    if ( sys_geom_local_constrain_smooth(src, &dst, numIters, constrainFactor, numCGSolves, NULL, "ActiveCells") != CV_OK )
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

    if ( sys_geom_local_linear_subdivision(src, &dst, numDivs, NULL, "ActiveCells") != CV_OK )
    {
        MITK_ERROR << "poly local linear subdivision error ";
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
    cvPolyData *capped = NULL;
    int numCapCenterIDs;
    int *capCenterIDs=NULL;

    if ( sys_geom_cap(src, &capped, &numCapCenterIDs, &capCenterIDs, 1 ) != CV_OK || numCapCenterIDs<2)
    {
        //        delete capped;
        return NULL;
    }

    cvPolyData *tempCenterlines = NULL;
    cvPolyData *voronoi = NULL;

    int *sources=new int[1];
    sources[0]=capCenterIDs[0];

    int *targets=new int[numCapCenterIDs-1];
    for(int i=1;i<numCapCenterIDs;i++)
        targets[i-1]= capCenterIDs[i];

    if ( sys_geom_centerlines(capped, sources, 1, targets, numCapCenterIDs-1, &tempCenterlines, &voronoi) != CV_OK )
    {
        return NULL;
    }

    cvPolyData *centerlines=NULL;
    if ( sys_geom_separatecenterlines(tempCenterlines, &centerlines) != CV_OK )
    {
        return NULL;
    }

    return centerlines->GetVtkPolyData();
}

vtkPolyData* svModelUtils::CalculateDistanceToCenterlines(vtkPolyData* centerlines, vtkPolyData* original)
{
    if(centerlines==NULL || original==NULL)
        return NULL;

    cvPolyData *src=new cvPolyData(original);
    cvPolyData *lines=new cvPolyData(centerlines);
    cvPolyData *distance = NULL;
    if ( sys_geom_distancetocenterlines(src, lines, &distance) != CV_OK )
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

    if ( sys_geom_SurfArea(src, &area) != CV_OK )
    {
        return 0;
    }

    return area;
}

#ifdef SV_USE_OpenCASCADE

cvOCCTSolidModel* svModelUtils::CreateLoftSurfaceOCCT(std::vector<svContour*> contourSet, std::string groupName, int numSamplingPts, int vecFlag, int addCaps)
{
    int contourNumber=contourSet.size();

    if(contourNumber==0 || numSamplingPts==0)
        return NULL;

    int numSuperPts=0;
    for(int i=0;i<contourNumber;i++)
    {
        int pointNunumber=contourSet[i]->GetContourPointNumber();
        if(pointNunumber>numSuperPts)
            numSuperPts=pointNunumber;
    }

    if(numSamplingPts>numSuperPts)
        numSuperPts=numSamplingPts;

    int newNumSamplingPts=numSamplingPts;

    std::vector<cvPolyData*> superSampledContours;
    for(int i=0;i<contourNumber;i++)
    {
        vtkPolyData* vtkpd=vtkPolyData::New();

        vtkpd->DeepCopy(contourSet[i]->CreateVtkPolyDataFromContour(false));
        cvPolyData* cvpd=new cvPolyData(vtkpd);
        cvPolyData* cvpd2=sys_geom_sampleLoop(cvpd,numSuperPts);

        //        delete cvpd;

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
            if(vecFlag==1)
                cvpd3=sys_geom_Align(alignedContours[i-1],superSampledContours[i]);
            else
                cvpd3=sys_geom_AlignByDist(alignedContours[i-1],superSampledContours[i]);


            //            delete superSampledContours[i];

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
        cvPolyData * cvpd4=sys_geom_sampleLoop(alignedContours[i],newNumSamplingPts);

        //        delete alignedContours[i];

        if(cvpd4==NULL)
        {
            MITK_ERROR << "sampling error ";
            return NULL;
        }
        sampledContours[i]=cvpd4;
    }

//    cvOCCTSolidModel **curveList=new cvOCCTSolidModel*[contourNumber];
    cvSolidModel **curveList=new cvSolidModel*[contourNumber];
    int closed=1;
    for(int i=0;i<contourNumber;i++)
    {
        cvOCCTSolidModel* curve=new cvOCCTSolidModel();

        int status=curve->MakeInterpCurveLoop(sampledContours[i],closed);

        //        delete sampledContours[i];

        if ( status != CV_OK )
        {
            //            delete curve;
            MITK_ERROR << "error in curve loop construction ";
            return NULL;
        }

        curveList[i]=curve;
    }

    cvOCCTSolidModel* surfFinal=NULL;

    cvOCCTSolidModel* surf=new cvOCCTSolidModel();
    int continuity=2;
    int partype=0;
    int smoothing=0;
    double w1=1.0,w2=1.0,w3=1.0;
    if ( surf->MakeLoftedSurf(curveList,contourNumber,"dummy_name",continuity,partype,w1,w2,w3,smoothing) != CV_OK )
    {
        MITK_ERROR << "error in lofting surface. ";
        return NULL;
    }
    //delete curveList;
    surfFinal=surf;

    if(addCaps)
    {
        cvOCCTSolidModel* surfCapped=new cvOCCTSolidModel();
        if ( surfCapped->CapSurfToSolid(surf) != CV_OK )
        {
            MITK_ERROR << "error in cap / bound operation ";
            return NULL;
        }
        //delete surf
        surfFinal=surfCapped;
    }

    int numFaces;
    int *faces;
    if(surfFinal->GetFaceIds( &numFaces, &faces) != CV_OK )
    {
        MITK_ERROR << "GetFaceIds: error on object";
        return NULL;
    }

    for(int i=0;i<numFaces;i++)
    {
        char* gn=const_cast<char*>(groupName.c_str());

        surfFinal->SetFaceAttribute("parent",faces[i],gn);
    }

    return surfFinal;
}

svModelElementOCCT* svModelUtils::CreateModelElementOCCT(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, double maxDist, unsigned int t)
{
    std::vector<cvOCCTSolidModel*> loftedSolids;
    std::vector<std::string> segNames;

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        svContourGroup* group = dynamic_cast<svContourGroup*>(segNode->GetData());
        if(group!=NULL)
        {
            std::vector<svContour*> contourSet=group->GetValidContourSet(t);
            std::string groupName=segNode->GetName();
            segNames.push_back(groupName);

            cvOCCTSolidModel* solid=CreateLoftSurfaceOCCT(contourSet,groupName,numSamplingPts,0,1);
            loftedSolids.push_back(solid);
        }
    }

    cvOCCTSolidModel* unionSolid=loftedSolids[0];

    cvOCCTSolidModel* previousUnionSolid=NULL;
    //    SolidModel_SimplifyT smp = SM_Simplify_All;
    for(int i=1;i<loftedSolids.size();i++)
    {
        previousUnionSolid=unionSolid;
        unionSolid=new cvOCCTSolidModel();
        unionSolid->Union(loftedSolids[i],previousUnionSolid);
        //        delete previousUnionSolid;
        //        delete loftedSolids[i];
    }


    //setup face names
    int numFaces;
    int *ids;
    int status=unionSolid->GetFaceIds( &numFaces, &ids);
    if(status != CV_OK )
    {
        //        delete unionSolid;
        MITK_ERROR << "GetFaceIds: error on object";
        return NULL;
    }

    std::vector<int> faceIDs;
    std::vector<std::string> faceNames;
    for(int i=0;i<numFaces;i++)
    {
        faceIDs.push_back(ids[i]);
        char *value=NULL;
        char *parent=NULL;
        unionSolid->GetFaceAttribute("gdscName",ids[i],&value);
        std::string type(value);
        unionSolid->GetFaceAttribute("parent",ids[i],&parent);
        std::string groupName(parent);
        faceNames.push_back(type+"_"+groupName);
    }

    for(int i=0;i<faceNames.size()-1;i++)
    {
        int idx=1;
        for(int j=i+1;j<faceNames.size();j++)
        {
            if(faceNames[i]==faceNames[j])
            {
                idx++;
                std::stringstream ss;
                ss << idx;
                std::string idxStr = ss.str();
                faceNames[j]=faceNames[j]+"_"+idxStr;
            }
        }
    }

    for(int i=0;i<numFaces;i++)
    {
        char* fn=const_cast<char*>(faceNames[i].c_str());

        unionSolid->SetFaceAttribute("gdscName",ids[i],fn);
    }

    cvPolyData* cvwholevpd=unionSolid->GetPolyData(1,maxDist);
    if(cvwholevpd==NULL || cvwholevpd->GetVtkPolyData()==NULL)
        return NULL;

    std::vector<svModelElement::svFace*> faces;

    for(int i=0;i<numFaces;i++)
    {
        cvPolyData* cvfacevpd=unionSolid->GetFacePolyData(ids[i],1,maxDist);
        if(cvfacevpd==NULL || cvfacevpd->GetVtkPolyData()==NULL)
            return NULL;

        svModelElement::svFace* face =new svModelElement::svFace;
        face->id=ids[i];
        face->name=faceNames[i];
        face->vpd=cvfacevpd->GetVtkPolyData();

        if(face->name.substr(0,5)=="wall_")
            face->type="wall";
        else if(face->name.substr(0,4)=="cap_")
            face->type="cap";

        faces.push_back(face);
    }

    svModelElementOCCT* modelElement=new svModelElementOCCT();
    modelElement->SetSegNames(segNames);
    modelElement->SetFaces(faces);
    modelElement->SetWholeVtkPolyData(cvwholevpd->GetVtkPolyData());
    modelElement->SetNumSampling(numSamplingPts);
    modelElement->SetOCCTSolid(unionSolid);
    modelElement->SetMaxDist(maxDist);

    return modelElement;
}

svModelElementOCCT* svModelUtils::CreateModelElementOCCTByBlend(svModelElementOCCT* meocctsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii)
{
    if(meocctsrc==NULL || meocctsrc->GetOCCTSolid()==NULL)
        return NULL;

    svModelElementOCCT* meocctdst =meocctsrc->Clone();

    cvOCCTSolidModel* occtSolid=meocctdst->GetOCCTSolid();

    for(int i=0;i<blendRadii.size();i++)
    {
        if(blendRadii[i] && blendRadii[i]->radius>0)
        {
            int faceID1=meocctdst->GetFaceIDFromInnerSolid(blendRadii[i]->faceName1);
            int faceID2=meocctdst->GetFaceIDFromInnerSolid(blendRadii[i]->faceName2);
            double radius=blendRadii[i]->radius;

            if(occtSolid->CreateEdgeBlend(faceID1,faceID2,radius,0)!=CV_OK)
            {
                delete meocctdst;
                MITK_ERROR << "OpenCASCADE model blending failed";
//                return NULL;
            }
        }
    }

    int numFaces;
    int *ids;
    int status=occtSolid->GetFaceIds( &numFaces, &ids);
    if(status != CV_OK )
    {
        delete meocctdst;
        MITK_ERROR << "OpenCASCADE model GetFaceIds: error on object";
        return NULL;
    }

    vtkSmartPointer<vtkPolyData> wholevpd=meocctdst->CreateWholeVtkPolyData();
    if(wholevpd==NULL)
    {
        delete meocctdst;
        return NULL;
    }

    meocctdst->SetWholeVtkPolyData(meocctdst->CreateWholeVtkPolyData());


    std::vector<svModelElement::svFace*> oldFaces=meocctsrc->GetFaces();
    std::vector<svModelElement::svFace*> faces;
    for(int i=0;i<numFaces;i++)
    {
        vtkSmartPointer<vtkPolyData> facevpd=meocctdst->CreateFaceVtkPolyData(ids[i]);
        if(facevpd==NULL)
        {
            delete meocctdst;
            return NULL;
        }

        svModelElement::svFace* face =new svModelElement::svFace;
        char *value;
        occtSolid->GetFaceAttribute("gdscName",ids[i],&value);
        std::string name(value);
        face->id=ids[i];
        face->name=name;
        face->vpd=facevpd;

        for(int j=0;j<oldFaces.size();j++)
        {
            if(face->name==oldFaces[j]->name)
            {
                face->type=oldFaces[j]->type;
                break;
            }
        }

        if(face->type=="")
        {
            if(face->name.substr(0,5)=="wall_")
                face->type="wall";
            else if(face->name.substr(0,4)=="cap_")
                face->type="cap";
        }

        faces.push_back(face);
    }

    meocctdst->SetFaces(faces);
    meocctdst->AddBlendRadii(blendRadii);

    return meocctdst;
}

#endif



