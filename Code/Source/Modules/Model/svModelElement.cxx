#include "svModelElement.h"

svModelElement::svModelElement()
    : m_Type("")
    , m_WholeVtkPolyData(NULL)
{
}

svModelElement::svModelElement(const svModelElement &other)
    : m_Type(other.m_Type)
    , m_SegNames(other.m_SegNames)
{
    int faceNum=other.m_Faces.size();
    m_Faces.resize(faceNum);

    for(int i=0;i<faceNum;i++)
    {
        svFace* face=new svFace;
        face->id=other.m_Faces[i]->id;
        face->name=other.m_Faces[i]->name;
        face->opacity=other.m_Faces[i]->opacity;
        face->visible=other.m_Faces[i]->visible;
        face->color[0]=other.m_Faces[i]->color[0];
        face->color[1]=other.m_Faces[i]->color[1];
        face->color[2]=other.m_Faces[i]->color[2];
        face->isWall=other.m_Faces[i]->isWall;
        vtkPolyData* vpd=NULL;
        if(other.m_Faces[i]->vpd)
        {
            vpd=vtkPolyData::New();
            vpd->DeepCopy(other.m_Faces[i]->vpd);
        }
        face->vpd=vpd;

        m_Faces[i]=face;
    }

    m_WholeVtkPolyData=NULL;
    if(other.m_WholeVtkPolyData)
    {
        m_WholeVtkPolyData=vtkPolyData::New();
        m_WholeVtkPolyData->DeepCopy(other.m_WholeVtkPolyData);
    }
}

svModelElement::~svModelElement()
{
    int faceNum=m_Faces.size();
    for(int i=0;i<faceNum;i++)
    {
        if(m_Faces[i])
        {

        if(m_Faces[i]->vpd)
        {
            m_Faces[i]->vpd->Delete();
        }

        delete m_Faces[i];

        }

    }

    if(m_WholeVtkPolyData)
    {
        m_WholeVtkPolyData->Delete();
    }
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

void svModelElement::SetFaces(std::vector<svModelElement::svFace*> faces)
{
    m_Faces=faces;
}

svModelElement::svFace* GetFace(int id) const
{
    int idx=GetFaceIndex(id);
    if(idx<0)
        return NULL;
    else
        return m_Faces[indx];
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
    int index=GetFaceIndex(id);
    if(index<0)
        return "";
    else if(!m_Faces[index])
        return "";
    else
        return m_Faces[index]->name;
}

void svModelElement::SetFaceName(std::string name, int id)
{
    int index=GetFaceIndex(id);
    if(index>-1)
        m_Faces[index]->name=name;
}

vtkPolyData* svModelElement::GetWholeVtkPolyData() const
{
    return m_WholeVtkPolyData;
}

void svModelElement::SetWholePolyData(vtkPolyData* wvpd)
{
    m_WholeVtkPolyData=wvpd;
}

//int svModelElement::GetSelectedFaceIndex()
//{
//    return m_SelectedFaceIndex;
//}

void svModelElement::SetSelectedFaceIndex(int idx)
{
    if(idx>-1&&idx<m_Faces.size())
    {
        if(m_Faces[idx])
            m_Faces[idx]->selected=true;
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
