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

#include "sv4gui_ModelElement.h"
#include "sv4gui_ModelUtils.h"

sv4guiModelElement::sv4guiModelElement()
    : m_Type("")
    , m_WholeVtkPolyData(NULL)
    , m_NumSampling(0)
    , m_InnerSolid(NULL)
    , m_UseUniform(0)
{
    m_BlendParam=new svBlendParam();
    m_LoftParam=new svLoftingParam;
}

sv4guiModelElement::sv4guiModelElement(const sv4guiModelElement &other)
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

sv4guiModelElement::~sv4guiModelElement()
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

sv4guiModelElement* sv4guiModelElement::Clone()
{
    return new sv4guiModelElement(*this);
}

std::string sv4guiModelElement::GetType() const
{
    return m_Type;
}

std::vector<std::string> sv4guiModelElement::GetSegNames() const
{
    return m_SegNames;
}

void sv4guiModelElement::SetSegNames(std::vector<std::string> segNames)
{
    m_SegNames=segNames;
}

bool sv4guiModelElement::HasSeg(std::string segName)
{
    for(int i=0;i<m_SegNames.size();i++)
    {
        if(segName==m_SegNames[i])
            return true;
    }
    return false;
}

std::vector<sv4guiModelElement::svFace*> sv4guiModelElement::GetFaces() const
{
    return m_Faces;
}

std::vector<std::string> sv4guiModelElement::GetFaceNames() const
{
    std::vector<std::string> names;
    std::vector<sv4guiModelElement::svFace*> faces=GetFaces();

    for(int i=0;i<faces.size();i++)
        if(faces[i])
            names.push_back(faces[i]->name);

    return names;
}

void sv4guiModelElement::SetFaces(std::vector<sv4guiModelElement::svFace*> faces)
{
    m_Faces=faces;
}

sv4guiModelElement::svFace* sv4guiModelElement::GetFace(int id) const
{
    int idx=GetFaceIndex(id);
    if(idx<0)
        return NULL;
    else
        return m_Faces[idx];
}

sv4guiModelElement::svFace* sv4guiModelElement::GetFace(std::string name) const
{
    return GetFace(GetFaceID(name));
}

int sv4guiModelElement::GetFaceIndex(int id) const
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->id==id)
            return i;
    }

    return -1;
}

std::string sv4guiModelElement::GetFaceName(int id) const
{
    svFace* face=GetFace(id);
    if(face)
        return face->name;
    else
        return "";
}

void sv4guiModelElement::SetFaceName(std::string name, int id)
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

vtkSmartPointer<vtkPolyData> sv4guiModelElement::GetWholeVtkPolyData() const
{
    return m_WholeVtkPolyData;
}

void sv4guiModelElement::SetWholeVtkPolyData(vtkSmartPointer<vtkPolyData> wvpd)
{
    m_WholeVtkPolyData=wvpd;
}

//int sv4guiModelElement::GetSelectedFaceIndex()
//{
//    return m_SelectedFaceIndex;
//}

void sv4guiModelElement::SelectFaceByIndex(int idx, bool select)
{
    if(idx>-1&&idx<m_Faces.size())
    {
        if(m_Faces[idx])
            m_Faces[idx]->selected=select;
    }

}

void sv4guiModelElement::ClearFaceSelection()
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i])
            m_Faces[i]->selected=false;
    }

}

void sv4guiModelElement::SelectFace(int id)
{
    svFace* face=GetFace(id);
    if(face)
        face->selected=true;
}

void sv4guiModelElement::SelectFace(std::string name)
{
    svFace* face=GetFace(name);
    if(face)
        face->selected=true;
}

int sv4guiModelElement::GetFaceID(std::string name) const
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->name==name)
            return m_Faces[i]->id;
    }

    return -1;
}

//------------------
// GetFaceNameIDMap
//------------------
// Get a map between face names and IDs.
//
std::map<std::string,int>
sv4guiModelElement::GetFaceNameIDMap() const
{
    std::map<std::string,int> faceIDMap;
    for (auto const& face : m_Faces) {
        faceIDMap[face->name] = face->id;
    }
    return faceIDMap;
}


int sv4guiModelElement::GetMaxFaceID() const
{
    int maxID=0;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->id>maxID)
            maxID=m_Faces[i]->id;
    }

    return maxID;
}

int sv4guiModelElement::GetFaceNumber() const
{
    return m_Faces.size();
}

bool sv4guiModelElement::IsFaceSelected(std::string name)
{
    return GetFace(name)&&GetFace(name)->selected;
}

bool sv4guiModelElement::IsFaceSelected(int id)
{
    return GetFace(id)&&GetFace(id)->selected;
}

std::vector<int> sv4guiModelElement::GetAllFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i])
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> sv4guiModelElement::GetSelectedFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->selected)
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> sv4guiModelElement::GetWallFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&m_Faces[i]->type=="wall")
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> sv4guiModelElement::GetCapFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&(m_Faces[i]->type=="cap"||m_Faces[i]->type=="inlet"||m_Faces[i]->type=="outlet"))
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> sv4guiModelElement::GetInletFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&& m_Faces[i]->type=="inlet")
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

std::vector<int> sv4guiModelElement::GetOutletFaceIDs()
{
    std::vector<int> ids;
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]&&(m_Faces[i]->type=="cap" || m_Faces[i]->type=="outlet"))
           ids.push_back(m_Faces[i]->id);
    }

    return ids;
}

double sv4guiModelElement::GetFaceArea(int id)
{
    svFace* face=GetFace(id);
    if(face==NULL)
        return 0;

    return sv4guiModelUtils::CalculateVpdArea(face->vpd);
}

double sv4guiModelElement::GetMinFaceArea()
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

void sv4guiModelElement::CalculateBoundingBox(double *bounds)
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

std::vector<sv4guiModelElement::svBlendParamRadius*> sv4guiModelElement::GetBlendRadii()
{
    return m_BlendRadii;
}

void sv4guiModelElement::SetBlendRadii(std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii)
{
    m_BlendRadii=blendRadii;
}

void sv4guiModelElement::AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii)
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

sv4guiModelElement::svBlendParamRadius* sv4guiModelElement::GetBlendParamRadius(int faceID1, int faceID2)
{
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i] && m_BlendRadii[i]->faceID1==faceID1 &&  m_BlendRadii[i]->faceID2==faceID2)
            return m_BlendRadii[i];
    }

    return NULL;
}

sv4guiModelElement::svBlendParamRadius* sv4guiModelElement::GetBlendParamRadius(std::string faceName1, std::string faceName2)
{
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i] && m_BlendRadii[i]->faceName1==faceName1 &&  m_BlendRadii[i]->faceName2==faceName2)
            return m_BlendRadii[i];
    }

    return NULL;
}

void sv4guiModelElement::RemoveFace(int faceID)
{
    int idx=GetFaceIndex(faceID);

    if(idx>-1)
        m_Faces.erase(m_Faces.begin()+idx);
}

void sv4guiModelElement::RemoveFaceFromBlendParamRadii(int faceID)
{

    for(int i=m_BlendRadii.size()-1;i>-1;i--)
    {
        if( m_BlendRadii[i] && (m_BlendRadii[i]->faceID1==faceID || m_BlendRadii[i]->faceID2==faceID) )
            m_BlendRadii.erase(m_BlendRadii.begin()+i);
    }

}

void sv4guiModelElement::ReplaceFaceIDForBlendParamRadii(int targetID, int loseID)
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

void sv4guiModelElement::SetNumSampling(int num)
{
    m_NumSampling=num;
}

int sv4guiModelElement::GetNumSampling()
{
    return m_NumSampling;
}

sv4guiModelElement::svBlendParam* sv4guiModelElement::GetBlendParam()
{
    return m_BlendParam;
}

void sv4guiModelElement::AssignBlendParam(sv4guiModelElement::svBlendParam* param)
{
    m_BlendParam->numblenditers=param->numblenditers;
    m_BlendParam->numsubblenditers=param->numsubblenditers;
    m_BlendParam->numsubdivisioniters=param->numsubdivisioniters;
    m_BlendParam->numcgsmoothiters=param->numcgsmoothiters;
    m_BlendParam->numlapsmoothiters=param->numlapsmoothiters;
    m_BlendParam->targetdecimation=param->targetdecimation;
}

cvSolidModel* sv4guiModelElement::GetInnerSolid()
{
    return m_InnerSolid;
}

void sv4guiModelElement::SetInnerSolid(cvSolidModel* innerSolid)
{
    m_InnerSolid=innerSolid;
}

int sv4guiModelElement::GetFaceIDFromInnerSolid(std::string faceName)
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

int sv4guiModelElement::GetFaceIdentifierFromInnerSolid(std::string faceName)
{
    return GetFaceIDFromInnerSolid(faceName);
}

int sv4guiModelElement::GetFaceIdentifierFromInnerSolid(int faceID)
{
    return faceID;
}

std::vector<int> sv4guiModelElement::GetFaceIDsFromInnerSolid()
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

std::string sv4guiModelElement::GetFaceNameFromInnerSolid(int faceID)
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
