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

#include "sv4gui_ModelUtilsOCCT.h"

#include "SimVascular.h"
#include "sv_sys_geom.h"
#include "sv_PolyData.h"
#include "sv_polydatasolid_utils.h"

#include "vtkSVGlobals.h"

cvOCCTSolidModel* sv4guiModelUtilsOCCT::CreateLoftSurfaceOCCT(std::vector<sv4guiContour*> contourSet, std::string groupName, int numSamplingPts, svLoftingParam *param, int vecFlag, int addCaps)
{
    int contourNumber=contourSet.size();

    if(contourNumber==0 || numSamplingPts==0)
        return NULL;

    if(param==NULL)
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
        vtkpd->Delete();
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
            for (int j=0; j<i; j++)
              delete sampledContours[j];
            delete [] sampledContours;
            return NULL;
        }
        sampledContours[i]=cvpd4;
    }

    cvSolidModel **curveList=new cvSolidModel*[contourNumber];
    int closed=1;
    for(int i=0;i<contourNumber;i++)
    {
        cvOCCTSolidModel* curve=new cvOCCTSolidModel();

        int status=curve->MakeInterpCurveLoop(sampledContours[i],closed);

        //        delete sampledContours[i];

        if ( status != SV_OK )
        {
            //            delete curve;
            MITK_ERROR << "error in curve loop construction ";
            for (int j=0; j<i; j++)
              delete curveList[j];
            delete [] curveList;
            for (int j=0; j<contourNumber; j++)
              delete sampledContours[j];
            delete [] sampledContours;
            return NULL;
        }

        curveList[i]=curve;
    }

    cvOCCTSolidModel* surfFinal=NULL;

    cvOCCTSolidModel* surf=new cvOCCTSolidModel();
    if(param->method=="spline")
    {
      int continuity=2;
      int partype=0;
      int smoothing=0;
      double w1=1.0,w2=1.0,w3=1.0;
      if ( surf->MakeLoftedSurf(curveList,contourNumber,"dummy_name",continuity,partype,w1,w2,w3,smoothing) != SV_OK )
      {
          MITK_ERROR << "error in lofting surface. ";
          for (int j=0; j<contourNumber; j++)
          {
            delete curveList[j];
            delete sampledContours[j];
          }
          delete [] curveList;
          delete [] sampledContours;
          delete surf;
          return NULL;
      }
      //delete curveList;
    }
    else if (param->method=="nurbs")
    {
      // Set degrees
      int uDegree = param->uDegree;
      int vDegree = param->vDegree;

      // Need to return if contourNumber is too low
      if (contourNumber < 3)
      {
        MITK_ERROR << "Not enough segmentations provided in group. Need at least 3";
        return NULL;
      }

      // Override to maximum possible degree if too large a degree for given number of inputs!
      if (uDegree > contourNumber)
        uDegree = contourNumber;
      if (vDegree > newNumSamplingPts)
        vDegree = newNumSamplingPts;

      // OpenCASCADE only handles surfaces of deg >= 3 currently
      if (uDegree < 3)
        uDegree = 3;
      if (vDegree < 3)
        vDegree = 3;

      // Output spacing super larger because we don't actually use the polydata
      double uSpacing = 0.8;
      double vSpacing = 0.8;

      // span types
      const char *uKnotSpanType       = param->uKnotSpanType.c_str();
      const char *vKnotSpanType       = param->vKnotSpanType.c_str();
      const char *uParametricSpanType = param->uParametricSpanType.c_str();
      const char *vParametricSpanType = param->vParametricSpanType.c_str();
      vtkNew(vtkSVNURBSSurface, NURBSSurface);

      cvPolyData *dst;
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
          for (int j=0; j<contourNumber; j++)
          {
            delete curveList[j];
            delete sampledContours[j];
          }
          delete [] curveList;
          delete sampledContours;
          delete surf;
          return NULL;
      }

      // Get multiplicities, then convert everything to double arrays
      vtkNew(vtkDoubleArray, USingleKnotArray);
      vtkNew(vtkIntArray,    UMultArray);
      NURBSSurface->GetUMultiplicity(UMultArray, USingleKnotArray);
      vtkNew(vtkDoubleArray, VSingleKnotArray);
      vtkNew(vtkIntArray,    VMultArray);
      NURBSSurface->GetVMultiplicity(VMultArray, VSingleKnotArray);
      vtkSVControlGrid *controlPointGrid = NURBSSurface->GetControlPointGrid();

      // Get all information needed by creation of bspline surface
      int dims[3];
      controlPointGrid->GetDimensions(dims);
      int Xlen1 = dims[0];
      int Xlen2 = dims[1];
      double **Xarr = new double*[Xlen1];
      double **Yarr = new double*[Xlen1];
      double **Zarr = new double*[Xlen1];
      for (int i=0; i<Xlen1; i++)
      {
        Xarr[i] = new double[Xlen2];
        Yarr[i] = new double[Xlen2];
        Zarr[i] = new double[Xlen2];
        for (int j=0; j<Xlen2; j++)
        {
          double pt[3];
          double w;
          controlPointGrid->GetControlPoint(i, j, 0, pt, w);
          Xarr[i][j] = pt[0];
          Yarr[i][j] = pt[1];
          Zarr[i][j] = pt[2];
        }
      }

      // Get mult information as arrays
      int uKlen = USingleKnotArray->GetNumberOfTuples();
      double *uKarr = new double[uKlen];
      for (int i=0; i<uKlen; i++)
        uKarr[i] = USingleKnotArray->GetTuple1(i);

      int vKlen = VSingleKnotArray->GetNumberOfTuples();
      double *vKarr = new double[vKlen];
      for (int i=0; i<vKlen; i++)
        vKarr[i] = VSingleKnotArray->GetTuple1(i);

      int uMlen = UMultArray->GetNumberOfTuples();
      double *uMarr = new double[uMlen];
      for (int i=0; i<uMlen; i++)
        uMarr[i] = UMultArray->GetTuple1(i);

      int vMlen = VMultArray->GetNumberOfTuples();
      double *vMarr = new double[vMlen];
      for (int i=0; i<vMlen; i++)
        vMarr[i] = VMultArray->GetTuple1(i);

      // Flipping order!
      if (surf->CreateBSplineSurface(Xarr, Yarr, Zarr,
                                     Xlen1, Xlen2,
                                     vKarr, vKlen,
                                     uKarr, uKlen,
                                     vMarr, vMlen,
                                     uMarr, uMlen,
                                     vDegree, uDegree) != SV_OK )
      {
          MITK_ERROR << "poly manipulation error ";
          for (int j=0; j<contourNumber; j++)
          {
            delete curveList[j];
            delete sampledContours[j];
          }
          delete [] curveList;
          delete sampledContours;
          delete surf;
          //Clean up
          for (int i=0;i<Xlen1;i++)
          {
            delete [] Xarr[i];
            delete [] Yarr[i];
            delete [] Zarr[i];
          }
          delete [] Xarr;
          delete [] Yarr;
          delete [] Zarr;

          delete [] uKarr;
          delete [] vKarr;
          delete [] uMarr;
          delete [] vMarr;
          return NULL;
      }
      //Clean up
      for (int i=0;i<Xlen1;i++)
      {
        delete [] Xarr[i];
        delete [] Yarr[i];
        delete [] Zarr[i];
      }
      delete [] Xarr;
      delete [] Yarr;
      delete [] Zarr;

      delete [] uKarr;
      delete [] vKarr;
      delete [] uMarr;
      delete [] vMarr;

    }
    surfFinal=surf;

    for (int j=0; j<contourNumber; j++)
    {
      delete curveList[j];
      delete sampledContours[j];
    }
    delete [] curveList;
    delete sampledContours;

    if(addCaps)
    {
        cvOCCTSolidModel* surfCapped=new cvOCCTSolidModel();
        if ( surfCapped->CapSurfToSolid(surf) != SV_OK )
        {
            MITK_ERROR << "error in cap / bound operation ";
            delete surf;
            return NULL;
        }
        //delete surf
        surfFinal=surfCapped;
    }

    int numFaces;
    int *faces;
    if(surfFinal->GetFaceIds( &numFaces, &faces) != SV_OK )
    {
        MITK_ERROR << "GetFaceIds: error on object";
        delete surf;
        return NULL;
    }

    for(int i=0;i<numFaces;i++)
    {
        char* gn=const_cast<char*>(groupName.c_str());

        surfFinal->SetFaceAttribute("parent",faces[i],gn);
    }

    return surfFinal;
}

sv4guiModelElementOCCT* sv4guiModelUtilsOCCT::CreateModelElementOCCT(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts,svLoftingParam *param, double maxDist, unsigned int t)
{
    std::vector<cvOCCTSolidModel*> loftedSolids;
    std::vector<std::string> segNames;

    for(int i=0;i<segNodes.size();i++)
    {
        mitk::DataNode::Pointer segNode=segNodes[i];
        sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*>(segNode->GetData());
        if(group!=NULL)
        {
            std::vector<sv4guiContour*> contourSet=group->GetValidContourSet(t);
            std::string groupName=segNode->GetName();
            segNames.push_back(groupName);

            svLoftingParam* usedParam= group->GetLoftingParam();
            if(param!=NULL) usedParam=param;

            cvOCCTSolidModel* solid=CreateLoftSurfaceOCCT(contourSet,groupName,numSamplingPts,usedParam,0,1);
            loftedSolids.push_back(solid);
            if (solid == NULL)
              return NULL;
        }
    }

    if(loftedSolids.size()==0)
        return NULL;

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
    if(status != SV_OK )
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

    std::vector<sv4guiModelElement::svFace*> faces;

    for(int i=0;i<numFaces;i++)
    {
        cvPolyData* cvfacevpd=unionSolid->GetFacePolyData(ids[i],1,maxDist);
        if(cvfacevpd==NULL || cvfacevpd->GetVtkPolyData()==NULL)
            return NULL;

        sv4guiModelElement::svFace* face =new sv4guiModelElement::svFace;
        face->id=ids[i];
        face->name=faceNames[i];
        face->vpd=cvfacevpd->GetVtkPolyData();

        if(face->name.substr(0,5)=="wall_")
            face->type="wall";
        else if(face->name.substr(0,4)=="cap_")
            face->type="cap";

        faces.push_back(face);
    }

    sv4guiModelElementOCCT* modelElement=new sv4guiModelElementOCCT();
    modelElement->SetSegNames(segNames);
    modelElement->SetFaces(faces);
    modelElement->SetWholeVtkPolyData(cvwholevpd->GetVtkPolyData());
    modelElement->SetNumSampling(numSamplingPts);
    modelElement->SetInnerSolid(unionSolid);
    modelElement->SetMaxDist(maxDist);

    return modelElement;
}

sv4guiModelElementOCCT* sv4guiModelUtilsOCCT::CreateModelElementOCCTByBlend(sv4guiModelElementOCCT* meocctsrc, std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii)
{
    if(meocctsrc==NULL || meocctsrc->GetInnerSolid()==NULL)
        return NULL;

    sv4guiModelElementOCCT* meocctdst =meocctsrc->Clone();

    cvSolidModel* occtSolid=meocctdst->GetInnerSolid();

    for(int i=0;i<blendRadii.size();i++)
    {
        if(blendRadii[i] && blendRadii[i]->radius>0)
        {
            int faceID1=meocctdst->GetFaceIDFromInnerSolid(blendRadii[i]->faceName1);
            int faceID2=meocctdst->GetFaceIDFromInnerSolid(blendRadii[i]->faceName2);
            double radius=blendRadii[i]->radius;

            if(occtSolid->CreateEdgeBlend(faceID1,faceID2,radius,0)!=SV_OK)
            {
                delete meocctdst;
                MITK_ERROR << "OpenCASCADE model blending failed";
                return nullptr;
            }
        }
    }

    int numFaces;
    int *ids;
    int status=occtSolid->GetFaceIds( &numFaces, &ids);
    if(status != SV_OK )
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

    meocctdst->SetWholeVtkPolyData(wholevpd);

    std::vector<sv4guiModelElement::svFace*> oldFaces=meocctsrc->GetFaces();
    std::vector<sv4guiModelElement::svFace*> faces;
    for(int i=0;i<numFaces;i++)
    {
        vtkSmartPointer<vtkPolyData> facevpd=meocctdst->CreateFaceVtkPolyData(ids[i]);
        if(facevpd==NULL)
        {
            delete meocctdst;
            return NULL;
        }

        sv4guiModelElement::svFace* face =new sv4guiModelElement::svFace;
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


