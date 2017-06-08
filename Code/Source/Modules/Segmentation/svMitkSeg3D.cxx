#include "svMitkSeg3D.h"

svMitkSeg3D::svMitkSeg3D()
    : mitk::Surface()
    , m_Method("")
    , m_DataModified(false)
    // , m_CalculateBoundingBox(true)

{
}

svMitkSeg3D::svMitkSeg3D(const svMitkSeg3D &other)
    : mitk::Surface(other)
    , m_Method(other.m_Method)
    , m_DataModified(true)
    , m_SeedMap(other.m_SeedMap)
    // , m_CalculateBoundingBox(true)
{
}

svMitkSeg3D::~svMitkSeg3D()
{
}

void svMitkSeg3D::ExecuteOperation( mitk::Operation* operation )
{
    Superclass::ExecuteOperation(operation);
    m_DataModified=true;

    this->InvokeEvent( svMitkSeg3DSetEvent() );

    // mitk::OperationEndEvent endevent(operation);
    // ((const itk::Object*)this)->InvokeEvent(endevent);

}

void svMitkSeg3D::SetMethod(std::string method)
{
    m_Method=method;
}

std::string svMitkSeg3D::GetMethod() const
{
    return m_Method;
}

std::map<int,svSeed>& svMitkSeg3D::GetSeedMap()
{
    return m_SeedMap;
}

int svMitkSeg3D::AddSeed(svSeed seed)
{
    int newID=seed.id;

    if(newID<0)
    {
        int idmax=0;
        for(auto s:m_SeedMap)
        {
            if(s.first>idmax)
                idmax=s.first;
        }

        newID=idmax+1;

        seed.id=newID;
    }

    m_SeedMap[newID]=seed;

    return newID;
}

bool svMitkSeg3D::RemoveSeed(int id)
{
    m_SeedMap.erase(id);
}
