#include "svMitkSimJob.h"

svMitkSimJob::svMitkSimJob()
    : m_CalculateBoundingBox(true)
//    , m_Job(NULL)
    , m_MeshName("")
    , m_ModelName("")
    , m_Status("No Data Files")
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

svMitkSimJob::svMitkSimJob(const svMitkSimJob &other)
    : mitk::BaseData(other)
    , m_MeshName(other.m_MeshName)
    , m_ModelName(other.m_ModelName)
    , m_JobSet(other.GetTimeSize())
    , m_DataModified(true)
{
    for (std::size_t t = 0; t < other.m_JobSet.size(); ++t)
    {
        if(other.m_JobSet[t])
            m_JobSet.push_back(other.m_JobSet[t]->Clone());
        else
            m_JobSet.push_back(NULL);
    }
}

svMitkSimJob::~svMitkSimJob()
{
    this->ClearData();
}

void svMitkSimJob::ClearData()
{
    for(int t=0;t<m_JobSet.size();t++)
        delete m_JobSet[t];

    m_JobSet.clear();
    Superclass::ClearData();
}

void svMitkSimJob::InitializeEmpty()
{
    if (!m_JobSet.empty())
      this->ClearData();

    m_JobSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

void svMitkSimJob::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_JobSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_JobSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( svMitkSimJobEvent() );
    }

}

unsigned int svMitkSimJob::GetTimeSize() const
{
    return m_JobSet.size();
}

bool svMitkSimJob::IsEmptyTimeStep(unsigned int t) const
{
    return false;
}

void svMitkSimJob::CalculateBoundingBox(double *bounds,unsigned int t)
{
}

void svMitkSimJob::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_JobSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_JobSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_JobSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
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
    return true;
}

void svMitkSimJob::SetRequestedRegion(const DataObject * )
{
}

svSimJob* svMitkSimJob::GetSimJob(unsigned int t) const
{
    if ( t < m_JobSet.size() )
    {
        return m_JobSet[t];
    }
    else
    {
        return NULL;
    }
}

void svMitkSimJob::SetSimJob(svSimJob* job, unsigned int t)
{
    if(t<m_JobSet.size())
    {
        m_JobSet[t]=job;

        m_CalculateBoundingBox = true;
        m_DataModified=true;

        this->UpdateOutputInformation();
        this->InvokeEvent( svMitkSimJobEvent() );
    }
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

void svMitkSimJob::SetStatus(std::string status)
{
    m_Status=status;
}

std::string svMitkSimJob::GetStatus() const
{
    return m_Status;
}
