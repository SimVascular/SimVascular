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

#include "sv4gui_ModelElementAnalytic.h"

#include "sv_sys_geom.h"

sv4guiModelElementAnalytic::sv4guiModelElementAnalytic()
    : m_MaxDist(0)
{
}

sv4guiModelElementAnalytic::sv4guiModelElementAnalytic(const sv4guiModelElementAnalytic &other)
    : sv4guiModelElement(other)
    , m_MaxDist(other.m_MaxDist)
{
}

sv4guiModelElementAnalytic::~sv4guiModelElementAnalytic()
{
}

sv4guiModelElementAnalytic* sv4guiModelElementAnalytic::Clone()
{
    return new sv4guiModelElementAnalytic(*this);
}

vtkSmartPointer<vtkPolyData> sv4guiModelElementAnalytic::CreateFaceVtkPolyData(int id)
{
    if(m_InnerSolid==NULL)
        return NULL;

    cvPolyData* cvfacevpd=m_InnerSolid->GetFacePolyData(id,1,m_MaxDist);
    if(cvfacevpd==NULL)
        return NULL;

    return cvfacevpd->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> sv4guiModelElementAnalytic::CreateWholeVtkPolyData()
{
    if(m_InnerSolid==NULL)
        return NULL;

    cvPolyData* cvwholevpd=m_InnerSolid->GetPolyData(1,m_MaxDist);
    if(cvwholevpd==NULL)
        return NULL;

    return cvwholevpd->GetVtkPolyData();
}

double sv4guiModelElementAnalytic::GetMaxDist()
{
    return m_MaxDist;
}

void sv4guiModelElementAnalytic::SetMaxDist(double maxDist)
{
    m_MaxDist=maxDist;
}

void sv4guiModelElementAnalytic::AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii)
{
    for(int i=0;i<moreBlendRadii.size();i++)
    {
        svBlendParamRadius*  newParamRadius=moreBlendRadii[i];
        svBlendParamRadius* existingParamRadius=GetBlendParamRadius(newParamRadius->faceName1, newParamRadius->faceName2);
        if(existingParamRadius)
        {
            existingParamRadius->radius=newParamRadius->radius;
            delete newParamRadius;
        }
        else
        {
            m_BlendRadii.push_back(newParamRadius);
        }
    }

    //update faceids since ids in m_InnerSolid changed after blending
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i])
        {
            int faceID1=GetFaceIDFromInnerSolid(m_BlendRadii[i]->faceName1);
            int faceID2=GetFaceIDFromInnerSolid(m_BlendRadii[i]->faceName2);
            m_BlendRadii[i]->faceID1=faceID1;
            m_BlendRadii[i]->faceID2=faceID2;
        }
    }

}

sv4guiModelElementPolyData* sv4guiModelElementAnalytic::ConverToPolyDataModel()
{
    sv4guiModelElementPolyData* mepd=new sv4guiModelElementPolyData();
    mepd->SetSegNames(GetSegNames());

    vtkSmartPointer<vtkPolyData> wholevpd=NULL;
    if(GetWholeVtkPolyData())
    {
        wholevpd=vtkSmartPointer<vtkPolyData>::New();
        wholevpd->DeepCopy(GetWholeVtkPolyData());
    }

    if(wholevpd==NULL)
        return NULL;

    cvPolyData* src=new cvPolyData(wholevpd);

    std::vector<sv4guiModelElement::svFace*> oldFaces=GetFaces();
    std::vector<sv4guiModelElement::svFace*> faces;
    int numFaces=oldFaces.size();
    int* ids=new int[numFaces];
    cvPolyData **facevpds=new cvPolyData*[numFaces];

    for(int i=0;i<numFaces;i++)
    {
        ids[i]=oldFaces[i]->id;
        facevpds[i]=new cvPolyData(oldFaces[i]->vpd);

        sv4guiModelElement::svFace* face=new sv4guiModelElement::svFace(*(oldFaces[i]),false);

        faces.push_back(face);
    }

    cvPolyData *dst=NULL;
    if ( sys_geom_assign_ids_based_on_faces(src,facevpds,numFaces,ids,&dst ) != SV_OK ) {
        if(dst!=NULL)
            delete dst;

        delete [] ids;
        return NULL;
    }

    mepd->SetWholeVtkPolyData(dst->GetVtkPolyData());

    for(int i=0;i<numFaces;i++)
    {
        faces[i]->vpd=mepd->CreateFaceVtkPolyData(faces[i]->id);
    }

    mepd->SetFaces(faces);

    return mepd;
}
