#include "svMitkMesh.h"
//#include "svMeshTetGen.h"

svMitkMesh::svMitkMesh()
    : m_CalculateBoundingBox(true)
    , m_Type("")
    , m_ModelName("")
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

svMitkMesh::svMitkMesh(const svMitkMesh &other)
    : mitk::BaseData(other)
    , m_Type(other.m_Type)
    , m_ModelName(other.m_ModelName)
    , m_MeshSet(other.GetTimeSize())
    , m_DataModified(true)
{
    for (std::size_t t = 0; t < other.m_MeshSet.size(); ++t)
    {
        if(other.m_MeshSet[t])
            m_MeshSet.push_back(other.m_MeshSet[t]->Clone());
        else
            m_MeshSet.push_back(NULL);
    }
}

svMitkMesh::~svMitkMesh()
{
    this->ClearData();
}

void svMitkMesh::ClearData()
{
    for(int t=0;t<m_MeshSet.size();t++)
    {
        if(m_MeshSet[t])
            delete m_MeshSet[t];
    }
    m_MeshSet.clear();
    Superclass::ClearData();
}

void svMitkMesh::InitializeEmpty()
{
    if (!m_MeshSet.empty())
        this->ClearData();

    m_MeshSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool svMitkMesh::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetMesh(t) == NULL);

//    if(!IsInitialized())
//        return false;

//    return GetMesh(t) == NULL || GetMesh(t)->GetSurfaceMesh() == NULL || (
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfLines() == 0 &&
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfPolys() == 0 &&
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfStrips() == 0 &&
//                GetMesh(t)->GetSurfaceMesh()->GetNumberOfVerts() == 0
//                );

    return false;
}

void svMitkMesh::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_MeshSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_MeshSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( svMitkMeshExtendTimeRangeEvent() );
    }

}

unsigned int svMitkMesh::GetTimeSize() const
{
    return m_MeshSet.size();
}

svMesh* svMitkMesh::GetMesh(unsigned int t) const
{
    if ( t < m_MeshSet.size() )
    {
        return m_MeshSet[t];
    }
    else
    {
        return NULL;
    }
}

void svMitkMesh::SetMesh(svMesh* mesh, unsigned int t)
{
    if(t<m_MeshSet.size())
    {
        m_MeshSet[t]=mesh;

        m_CalculateBoundingBox = true;

        this->Modified();
        this->UpdateOutputInformation();
        this->InvokeEvent( svMitkMeshSetEvent() );
    }
}

void svMitkMesh::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    svMitkMeshOperation* meshOperation = dynamic_cast<svMitkMeshOperation*>(operation);

    if ( meshOperation )
    {
        timeStep = meshOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Mesh Operation for svMitkMesh" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of svMitkMesh time bounds" << std::endl;
        return;
    }

    //svMesh* originalMesh=m_MeshSet[timeStep];
    svMesh* newMesh=meshOperation->GetMesh();

    switch (operation->GetOperationType())
    {

    case svMitkMeshOperation::OpSETMESH:
    {
        SetMesh(newMesh,timeStep);
        m_DataModified=true;
    }
        break;

    default:
        itkWarningMacro("svMitkMesh could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);
}

void svMitkMesh::CalculateBoundingBox(double *bounds,unsigned int t)
{
    svMesh* mesh=GetMesh(t);
    if(mesh)
    {
        mesh->CalculateBoundingBox(bounds);
    }
}

void svMitkMesh::SetType(std::string type)
{
    m_Type=type;
}

std::string svMitkMesh::GetType() const
{
    return m_Type;
}

std::string svMitkMesh::GetModelName() const
{
    return m_ModelName;
}

void svMitkMesh::SetModelName(std::string name)
{
    m_ModelName=name;
}

void svMitkMesh::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_MeshSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_MeshSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_MeshSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void svMitkMesh::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svMitkMesh::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svMitkMesh::VerifyRequestedRegion()
{
    return true;
}

void svMitkMesh::SetRequestedRegion(const DataObject * )
{
}

void svMitkMesh::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_MeshSet.size() << "\n";

    for ( unsigned int t = 0 ; t < m_MeshSet.size() ; ++t )
    {
        os << indent << "Timestep " << t << ": \n";
        itk::Indent nextIndent = indent.GetNextIndent();

        if(m_MeshSet[t])
        {
            os << nextIndent << "Mesh Type: " << m_MeshSet[t]->GetType() << "\n";
                os << nextIndent << "Number of cells: " << m_MeshSet[t]->GetVolumeMesh()->GetNumberOfCells() << "\n";
                os << nextIndent << "Number of points: " << m_MeshSet[t]->GetVolumeMesh()->GetNumberOfPoints() << "\n";
        }else
        {
            os << nextIndent << "No Mesh! \n";
        }
    }
}

