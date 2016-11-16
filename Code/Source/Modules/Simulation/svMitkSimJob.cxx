#include "svMitkSimJob.h"

svMitkSimJob::svMitkSimJob()
    : m_Job(NULL)
    , m_MeshName("")
    , m_ModelName("")
{
    this->InitializeEmpty();
}

svMitkSimJob::svMitkSimJob(const svMitkSimJob &other)
    : mitk::BaseData(other)
    , m_MeshName(other.m_MeshName)
    , m_ModelName(other.m_ModelName)
{
    if(other.m_Job)
        m_Job=other.m_Job->Clone();
    else
        m_Job=NULL;
}

svMitkSimJob::~svMitkSimJob()
{
    this->ClearData();
}

void svMitkSimJob::ClearData()
{
    if(m_Job)
        delete m_Job;

    Superclass::ClearData();
}

void svMitkSimJob::InitializeEmpty()
{
//    Superclass::InitializeTimeGeometry(1);
//    m_Initialized = true;
}

void svMitkSimJob::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svMitkSimJob::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svMitkSimJob::VerifyRequestedRegion()
{
    return false;
}

void svMitkSimJob::SetRequestedRegion(const DataObject * )
{
}

svSimJob* svMitkSimJob::GetSimJob() const
{
    return m_Job;
}

void svMitkSimJob::SetSimJob(svSimJob* job)
{
    m_Job=job;
}

void svMitkSimJob::SetMeshName(std::string meshName)
{
    m_MeshName=meshName;
}

std::string svMitkSimJob::GetMeshName() const
{
    return m_MeshName;
}

void svMitkSimJob::SetModelName(std::string modelName)
{
    m_ModelName=modelName;
}

std::string svMitkSimJob::GetModelName() const
{
    return m_ModelName;
}
