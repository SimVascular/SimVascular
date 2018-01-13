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

#include "svModelElement.h"
#include "svModelUtils.h"

svModelElement::svModelElement()
    : m_Type("")
    , m_WholeVtkPolyData(NULL)
    , m_NumSampling(0)
    , m_InnerSolid(NULL)
    , m_UseUniform(0)
{
    m_BlendParam=new svBlendParam();
    m_LoftParam=new svLoftingParam;
}

svModelElement::svModelElement(const svModelElement &other)
    : m_Type(other.m_Type)
    , m_SegNames(other.m_SegNames)
    , m_NumSampling(other.m_NumSampling)
    , m_FileExtensions(other.m_FileExtensions)
    , m_InnerSolid(NULL)
{
    int faceNum=other.m_Faces.size();
    m_Faces.resize(faceNum);

    for(int i=0;i<faceNum;i++)
    {
        m_Faces[i]=new svFace(*(other.m_Faces[i]),true);
    }

    m_WholeVtkPolyData=NULL;
    if(other.m_WholeVtkPolyData)
    {
        m_WholeVtkPolyData=vtkSmartPointer<vtkPolyData>::New();
        m_WholeVtkPolyData->DeepCopy(other.m_WholeVtkPolyData);
    }

    for(int i=0;i<other.m_BlendRadii.size();i++)
    {
        m_BlendRadii.push_back(new svBlendParamRadius(*(other.m_BlendRadii[i])));
    }

    m_BlendParam=new svBlendParam(*(other.m_BlendParam));

    if(other.m_InnerSolid)
        m_InnerSolid=other.m_InnerSolid->Copy();

    m_LoftParam=new svLoftingParam(*(other.m_LoftParam));
}

svModelElement::~svModelElement()
{
    int faceNum=m_Faces.size();
    for(int i=0;i<faceNum;i++)
    {
        if(m_Faces[i])
        {
            delete m_Faces[i];
        }
    }

    if(m_InnerSolid)
        delete m_InnerSolid;

    if(m_BlendParam)
        delete m_BlendParam;

    if(m_LoftParam)
        delete m_LoftParam;

}

svModelElement* svModelElement::Clone()
{
    return new svModelElement(*this);
}

std::string svModelElement::GetType() const
{
    return m_Type;
}

std::vector<std::string> svModelElement::GetSegNames() const
{
    return m_SegNames;
}

void svModelElement::SetSegNames(std::vector<std::string> segNames)
{
    m_SegNames=segNames;
}

bool svModelElement::HasSeg(std::string segName)
{
    for(int i=0;i<m_SegNames.size();i++)
    {
        if(segName==m_SegNames[i])
            return true;
    }
    return false;
}

std::vector<svModelElement::svFace*> svModelElement::GetFaces() const
{
    return m_Faces;
}

std::vector<std::string> svModelElement::GetFaceNames() const
{
    std::vector<std::string> names;
    std::vector<svModelElement::svFace*> faces=GetFaces();

    for(int i=0;i<faces.size();i++)
        if(faces[i])
            names.push_back(faces[i]->name);

    return names;
}

void svModelElement::SetFaces(std::vector<svModelElement::svFace*> faces)
{
    m_Faces=faces;
}

svModelElement::svFace* svModelElement::GetFace(int id) const
{
    int idx=GetFaceIndex(id);
    if(idx<0)
        return NULL;
    else
        return m_Faces[idx];
}

svModelElement::svFace* svModelElement::GetFace(std::string name) const
{
    return GetFace(GetFaceID(name));
}

int svModelElement::GetFaceIndex(int id) const
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->id==id)
            return i;
    }

    return -1;
}

std::string svModelElement::GetFaceName(int id) const
{
    svFace* face=GetFace(id);
    if(face)
        return face->name;
    else
        return "";
}

void svModelElement::SetFaceName(std::string name, int id)
{
    int index=GetFaceIndex(id);
    if(index>-1)
    {
        m_Faces[index]->name=name;
        if(m_InnerSolid)
        {
            char* nc=const_cast<char*>(name.c_str());
            m_InnerSolid->SetFaceAttribute("gdscName",id,nc);
        }
    }
}

vtkSmartPointer<vtkPolyData> svModelElement::GetWholeVtkPolyData() const
{
    return m_WholeVtkPolyData;
}

void svModelElement::SetWholeVtkPolyData(vtkSmartPointer<vtkPolyData> wvpd)
{
    m_WholeVtkPolyData=wvpd;
}

//int svModelElement::GetSelectedFaceIndex()
//{
//    return m_SelectedFaceIndex;
//}

void svModelElement::SelectFaceByIndex(int idx, bool select)
{
    if(idx>-1&&idx<m_Faces.size())
    {
        if(m_Faces[idx])
            m_Faces[idx]->selected=select;
    }

}

void svModelElement::ClearFaceSelection()
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i])
            m_Faces[i]->selected=false;
    }

}

void svModelElement::SelectFace(int id)
{
    svFace* face=GetFace(id);
    if(face)
        face->selected=true;
}

void svModelElement::SelectFace(std::string name)
{
    svFace* face=GetFace(name);
    if(face)
        face->selected=true;
}

int svModelElement::GetFaceID(std::string name) const
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->name==name)
            return m_Faces[i]->id;
    }

    return -1;
}

int svModelElement::GetMaxFaceID() const
{
    int maxID=0;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->id>maxID)
            maxID=m_Faces[i]->id;
    }

    return maxID;
}

int svModelElement::GetFaceNumber() const
{
    return m_Faces.size();
}

bool svModelElement::IsFaceSelected(std::string name)
{
    return GetFace(name)&&GetFace(name)->selected;
}

bool svModelElement::IsFaceSelected(int id)
{
    return GetFace(id)&&GetFace(id)->selected;
}

std::vector<int> svModelElement::GetAllFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i])
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> svModelElement::GetSelectedFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->selected)
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> svModelElement::GetWallFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->type=="wall")
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> svModelElement::GetCapFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&(m_Faces[i]->type=="cap"||m_Faces[i]->type=="inlet"||m_Faces[i]->type=="outlet"))
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> svModelElement::GetInletFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&& m_Faces[i]->type=="inlet")
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> svModelElement::GetOutletFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&(m_Faces[i]->type=="cap" || m_Faces[i]->type=="outlet"))
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

double svModelElement::GetFaceArea(int id)
{
    svFace* face=GetFace(id);
    if(face==NULL)
        return 0;

    return svModelUtils::CalculateVpdArea(face->vpd);
}

double svModelElement::GetMinFaceArea()
{
    double minArea=0;

    std::vector<int> faceIDs=GetAllFaceIDs();
    for(int i=0;i<faceIDs.size();i++)
    {
        if(i==0)
            minArea=GetFaceArea(faceIDs[i]);
        else
        {
            double area=GetFaceArea(faceIDs[i]);
            if(area<minArea)
                minArea=area;
        }
    }

    return minArea;
}

void svModelElement::CalculateBoundingBox(double *bounds)
{
    bounds[0]=0;
    bounds[1]=0;
    bounds[2]=0;
    bounds[3]=0;
    bounds[4]=0;
    bounds[5]=0;

    if (m_WholeVtkPolyData != nullptr && m_WholeVtkPolyData->GetNumberOfPoints() > 0)
    {
      m_WholeVtkPolyData->ComputeBounds();
      m_WholeVtkPolyData->GetBounds(bounds);
    }

}

std::vector<svModelElement::svBlendParamRadius*> svModelElement::GetBlendRadii()
{
    return m_BlendRadii;
}

void svModelElement::SetBlendRadii(std::vector<svModelElement::svBlendParamRadius*> blendRadii)
{
    m_BlendRadii=blendRadii;
}

void svModelElement::AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii)
{
    for(int i=0;i<moreBlendRadii.size();i++)
    {
        svBlendParamRadius*  newParamRadius=moreBlendRadii[i];
        svBlendParamRadius* existingParamRadius=GetBlendParamRadius(newParamRadius->faceID1, newParamRadius->faceID2);
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
}

svModelElement::svBlendParamRadius* svModelElement::GetBlendParamRadius(int faceID1, int faceID2)
{
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i] && m_BlendRadii[i]->faceID1==faceID1 &&  m_BlendRadii[i]->faceID2==faceID2)
            return m_BlendRadii[i];
    }

    return NULL;
}

svModelElement::svBlendParamRadius* svModelElement::GetBlendParamRadius(std::string faceName1, std::string faceName2)
{
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i] && m_BlendRadii[i]->faceName1==faceName1 &&  m_BlendRadii[i]->faceName2==faceName2)
            return m_BlendRadii[i];
    }

    return NULL;
}

void svModelElement::RemoveFace(int faceID)
{
    int idx=GetFaceIndex(faceID);

    if(idx>-1)
        m_Faces.erase(m_Faces.begin()+idx);
}

void svModelElement::RemoveFaceFromBlendParamRadii(int faceID)
{

    for(int i=m_BlendRadii.size()-1;i>-1;i--)
    {
        if( m_BlendRadii[i] && (m_BlendRadii[i]->faceID1==faceID || m_BlendRadii[i]->faceID2==faceID) )
            m_BlendRadii.erase(m_BlendRadii.begin()+i);
    }

}

void svModelElement::ReplaceFaceIDForBlendParamRadii(int targetID, int loseID)
{
    //replace
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i] )
        {
            if(m_BlendRadii[i]->faceID1==loseID)
                m_BlendRadii[i]->faceID1=targetID;

            if(m_BlendRadii[i]->faceID2==loseID)
                m_BlendRadii[i]->faceID2=targetID;
        }
    }

    //remove invalid ones, in which faceID1==faceID2
    for(int i=m_BlendRadii.size()-1;i>-1;i--)
    {
        if(m_BlendRadii[i] && m_BlendRadii[i]->faceID1==m_BlendRadii[i]->faceID2)
        {
            m_BlendRadii.erase(m_BlendRadii.begin()+i);
        }
    }

    //remove duplicate ones
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        for(int j=m_BlendRadii.size()-1;j>i;j--)
        {
            if(m_BlendRadii[i] && m_BlendRadii[j]
                    && m_BlendRadii[i]->faceID1==m_BlendRadii[j]->faceID1
                    && m_BlendRadii[i]->faceID2==m_BlendRadii[j]->faceID2)
            {
                m_BlendRadii.erase(m_BlendRadii.begin()+j);
            }
        }
    }

}

void svModelElement::SetNumSampling(int num)
{
    m_NumSampling=num;
}

int svModelElement::GetNumSampling()
{
    return m_NumSampling;
}

svModelElement::svBlendParam* svModelElement::GetBlendParam()
{
    return m_BlendParam;
}

void svModelElement::AssignBlendParam(svModelElement::svBlendParam* param)
{
    m_BlendParam->numblenditers=param->numblenditers;
    m_BlendParam->numsubblenditers=param->numsubblenditers;
    m_BlendParam->numsubdivisioniters=param->numsubdivisioniters;
    m_BlendParam->numcgsmoothiters=param->numcgsmoothiters;
    m_BlendParam->numlapsmoothiters=param->numlapsmoothiters;
    m_BlendParam->targetdecimation=param->targetdecimation;
}

cvSolidModel* svModelElement::GetInnerSolid()
{
    return m_InnerSolid;
}

void svModelElement::SetInnerSolid(cvSolidModel* innerSolid)
{
    m_InnerSolid=innerSolid;
}

int svModelElement::GetFaceIDFromInnerSolid(std::string faceName)
{
    int id=-1;

    if(m_InnerSolid==NULL)
        return id;

    int numFaces;
    int *ids;
    int status=m_InnerSolid->GetFaceIds( &numFaces, &ids);
    if(status!=SV_OK)
        return id;

    for(int i=0;i<numFaces;i++)
    {
        char *value;
        m_InnerSolid->GetFaceAttribute("gdscName",ids[i],&value);
        std::string name(value);
        if(name==faceName)
            return ids[i];
    }

    return id;
}

int svModelElement::GetFaceIdentifierFromInnerSolid(std::string faceName)
{
    return GetFaceIDFromInnerSolid(faceName);
}

int svModelElement::GetFaceIdentifierFromInnerSolid(int faceID)
{
    return faceID;
}

std::vector<int> svModelElement::GetFaceIDsFromInnerSolid()
{
    std::vector<int> faceIDs;
    if(m_InnerSolid)
    {
        int numFaces;
        int *ids;
        int status=m_InnerSolid->GetFaceIds( &numFaces, &ids);
        for(int i=0;i<numFaces;i++)
            faceIDs.push_back(ids[i]);
    }

    return faceIDs;
}

std::string svModelElement::GetFaceNameFromInnerSolid(int faceID)
{
    std::string faceName="";
    if(m_InnerSolid)
    {
        char *value;
        m_InnerSolid->GetFaceAttribute("gdscName",faceID,&value);
        std::string name(value);
        faceName=name;
    }

    return faceName;
}
