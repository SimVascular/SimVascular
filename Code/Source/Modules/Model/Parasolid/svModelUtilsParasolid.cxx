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

#include "svModelUtilsParasolid.h"

#include "SimVascular.h"
#include "cv_sys_geom.h"
#include "cvPolyData.h"
#include "cv_polydatasolid_utils.h"

cvParasolidSolidModel* svModelUtilsParasolid::CreateLoftSurfaceParasolid(std::vector<svContour*> contourSet, std::string groupName, int numSamplingPts, int vecFlag, int addCaps)
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
            return NULL;
        }
        sampledContours[i]=cvpd4;
    }

    cvSolidModel **curveList=new cvSolidModel*[contourNumber];
    int closed=1;
    for(int i=0;i<contourNumber;i++)
    {
        cvParasolidSolidModel* curve=new cvParasolidSolidModel();

        int status=curve->MakeInterpCurveLoop(sampledContours[i],closed);

        //        delete sampledContours[i];

        if ( status != SV_OK )
        {
            //            delete curve;
            MITK_ERROR << "error in curve loop construction ";
            return NULL;
        }

        curveList[i]=curve;
    }

    cvParasolidSolidModel* surfFinal=NULL;

    cvParasolidSolidModel* surf=new cvParasolidSolidModel();
    int continuity=0;
    int partype=0;
    int smoothing=0;
    double w1=0.4,w2=0.2,w3=0.4;
    if ( surf->MakeLoftedSurf(curveList,contourNumber,"dummy_name",continuity,partype,w1,w2,w3,smoothing) != SV_OK )
    {
        MITK_ERROR << "error in lofting surface. ";
        return NULL;
    }
    //delete curveList;
    surfFinal=surf;

    if(addCaps)
    {
        cvParasolidSolidModel* surfCapped=new cvParasolidSolidModel();
        if ( surfCapped->CapSurfToSolid(surf) != SV_OK )
        {
            MITK_ERROR << "error in cap / bound operation ";
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
        return NULL;
    }

    for(int i=0;i<numFaces;i++)
    {
        char *value=NULL;
        surfFinal->GetFaceAttribute("gdscName",faces[i],&value);
        std::string name(value);

        char* gn;

        if(name!="" && name!="inflow" && name!="inlet")
            gn=const_cast<char*>(groupName.c_str());
        else
        {
            gn=const_cast<char*>(("wall_"+groupName).c_str());
        }

        surfFinal->SetFaceAttribute("gdscName",faces[i],gn);
    }

    return surfFinal;
}

svModelElementParasolid* svModelUtilsParasolid::CreateModelElementParasolid(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, double maxDist, unsigned int t)
{
    std::vector<cvParasolidSolidModel*> loftedSolids;
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

            cvParasolidSolidModel* solid=CreateLoftSurfaceParasolid(contourSet,groupName,numSamplingPts,0,1);
            loftedSolids.push_back(solid);
        }
    }

    if(loftedSolids.size()==0)
        return NULL;

    cvParasolidSolidModel* unionSolid=loftedSolids[0];

    cvParasolidSolidModel* previousUnionSolid=NULL;
    SolidModel_SimplifyT smp = SM_Simplify_All;
    for(int i=1;i<loftedSolids.size();i++)
    {
        previousUnionSolid=unionSolid;
        unionSolid=new cvParasolidSolidModel();
        unionSolid->Union(loftedSolids[i],previousUnionSolid,smp);
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
        unionSolid->GetFaceAttribute("gdscName",ids[i],&value);
        std::string name(value);
        faceNames.push_back(name);
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
        else
            face->type="cap";

        faces.push_back(face);
    }

    svModelElementParasolid* modelElement=new svModelElementParasolid();
    modelElement->SetSegNames(segNames);
    modelElement->SetFaces(faces);
    modelElement->SetWholeVtkPolyData(cvwholevpd->GetVtkPolyData());
    modelElement->SetNumSampling(numSamplingPts);
    modelElement->SetInnerSolid(unionSolid);
    modelElement->SetMaxDist(maxDist);

    return modelElement;
}

svModelElementParasolid* svModelUtilsParasolid::CreateModelElementParasolidByBlend(svModelElementParasolid* mepssrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii)
{
    if(mepssrc==NULL || mepssrc->GetInnerSolid()==NULL)
        return NULL;

    svModelElementParasolid* mepsdst =mepssrc->Clone();

    cvSolidModel* parasolid=mepsdst->GetInnerSolid();

    for(int i=0;i<blendRadii.size();i++)
    {
        if(blendRadii[i] && blendRadii[i]->radius>0)
        {
            std::string nameA=blendRadii[i]->faceName1;
            std::string nameB=blendRadii[i]->faceName2;

            int faceID1=mepsdst->GetFaceIDFromInnerSolid(nameA);
            int faceID2=mepsdst->GetFaceIDFromInnerSolid(nameB);
            double radius=blendRadii[i]->radius;

            int numFacesOld;
            int *idsOld;
            parasolid->GetFaceIds( &numFacesOld, &idsOld);

            if(parasolid->CreateEdgeBlend(faceID1,faceID2,radius,0)!=SV_OK)
            {
                delete mepsdst;
                MITK_ERROR << "Parasolid model blending failed";
                return NULL;
            }

            bool wallblended=false;
            std::string name="blend";

            if(nameA.substr(0,5)=="wall_")
            {
                wallblended=true;
                nameA=nameA.substr(5);
            }

            if(nameB.substr(0,5)=="wall_")
            {
                wallblended=true;
                nameB=nameB.substr(5);
            }

            if(wallblended)
                name="wall_blend";

            int tagger=0;
            int numFacesNew;
            int *idsNew;
            parasolid->GetFaceIds( &numFacesNew, &idsNew);

            for(int j=0;j<numFacesNew;j++)
            {
                bool found=false;

                for(int k=0;k<numFacesOld;k++)
                {
                    if(idsNew[j]==idsOld[k])
                    {
                        found=true;
                        break;
                    }
                }

                if(!found)
                {
                    name=name+"_"+nameA+"_"+nameB;
                    if(tagger>0)
                        name=name+"_tag"+std::to_string(tagger);

                    tagger++;

                    char* fn=const_cast<char*>(name.c_str());
                    parasolid->SetFaceAttribute("gdscName",idsNew[j],fn);
                }

            }

        }
    }

    int numFaces;
    int *ids;
    int status=parasolid->GetFaceIds( &numFaces, &ids);
    if(status != SV_OK )
    {
        delete mepsdst;
        MITK_ERROR << "Parasolid model GetFaceIds: error on object";
        return NULL;
    }

    vtkSmartPointer<vtkPolyData> wholevpd=mepsdst->CreateWholeVtkPolyData();
    if(wholevpd==NULL)
    {
        delete mepsdst;
        return NULL;
    }

    mepsdst->SetWholeVtkPolyData(wholevpd);

    std::vector<svModelElement::svFace*> oldFaces=mepssrc->GetFaces();
    std::vector<svModelElement::svFace*> faces;
    for(int i=0;i<numFaces;i++)
    {
        vtkSmartPointer<vtkPolyData> facevpd=mepsdst->CreateFaceVtkPolyData(ids[i]);
        if(facevpd==NULL)
        {
            delete mepsdst;
            return NULL;
        }

        svModelElement::svFace* face =new svModelElement::svFace;
        char *value;
        parasolid->GetFaceAttribute("gdscName",ids[i],&value);
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
            else
                face->type="cap";
        }

        faces.push_back(face);
    }

    mepsdst->SetFaces(faces);
    mepsdst->AddBlendRadii(blendRadii);

    return mepsdst;
}
