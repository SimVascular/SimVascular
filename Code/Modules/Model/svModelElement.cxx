#include "svModelElement.h"

svModelElement::svModelElement()
    : m_Type("PolyData")
    , m_VtkPolyDataModel(NULL)
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
        vtkPolyData* vpd=NULL;
        if(other.m_Faces[i]->vpd)
        {
            vpd=vtkPolyData::New();
            vpd->DeepCopy(other.m_Faces[i]->vpd);
        }
        face->vpd=vpd;

        m_Faces[i]=face;
    }

    m_VtkPolyDataModel=NULL;
    if(other.m_VtkPolyDataModel)
    {
        m_VtkPolyDataModel=vtkPolyData::New();
        m_VtkPolyDataModel->DeepCopy(other.m_VtkPolyDataModel);
    }
}

svModelElement::~svModelElement()
{
    int faceNum=m_Faces.size();
    for(int i=0;i<faceNum;i++)
    {
        if(m_Faces[i]->vpd)
        {
            m_Faces[i]->vpd->Delete();
        }
    }

    if(m_VtkPolyDataModel)
    {
        m_VtkPolyDataModel->Delete();
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

int svModelElement::GetFaceIndex(int id) const
{
    for(int i=0;i<m_Faces.size();i++)
    {
        if(m_Faces[i]->id==id)
            return i;
    }

    return -1;
}

std::string svModelElement::GetFaceName(int id) const
{
    int index=GetFaceIndex(id);
    if(index<0)
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

vtkPolyData* svModelElement::GetVtkPolyDataModel() const
{
    return m_VtkPolyDataModel;
}

void svModelElement::SetVtkPolyDataModel(vtkPolyData* vpdModel)
{
    m_VtkPolyDataModel=vpdModel;
}


