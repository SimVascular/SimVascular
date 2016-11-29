#include "svModel.h"
#include "svModelElementPolyData.h"

svModel::svModel()
    : m_CalculateBoundingBox(true)
    , m_Type("")
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

svModel::svModel(const svModel &other)
    : mitk::BaseData(other)
    , m_Type(other.m_Type)
    , m_ModelElementSet(other.GetTimeSize())
    , m_DataModified(true)
{
    for (std::size_t t = 0; t < other.m_ModelElementSet.size(); ++t)
    {
        if(other.m_ModelElementSet[t])
            m_ModelElementSet.push_back(other.m_ModelElementSet[t]->Clone());
        else
            m_ModelElementSet.push_back(NULL);
    }
}

svModel::~svModel()
{
    this->ClearData();
}

void svModel::ClearData()
{
    for(int t=0;t<m_ModelElementSet.size();t++)
        delete m_ModelElementSet[t];

    m_ModelElementSet.clear();
    Superclass::ClearData();
}

void svModel::InitializeEmpty()
{
    if (!m_ModelElementSet.empty())
      this->ClearData();

    m_ModelElementSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

bool svModel::IsEmptyTimeStep(unsigned int t) const
{
//    return IsInitialized() && (GetModelElement(t) == NULL);

//    if(!IsInitialized())
//        return false;

//    return GetModelElement(t) == NULL || GetModelElement(t)->GetWholeVtkPolyData() == NULL || (
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfLines() == 0 &&
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfPolys() == 0 &&
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfStrips() == 0 &&
//                GetModelElement(t)->GetWholeVtkPolyData()->GetNumberOfVerts() == 0
//                );
    return false;
}

void svModel::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_ModelElementSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_ModelElementSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( svModelExtendTimeRangeEvent() );
    }

}

unsigned int svModel::GetTimeSize() const
{
    return m_ModelElementSet.size();
}

svModelElement* svModel::GetModelElement(unsigned int t) const
{
    if ( t < m_ModelElementSet.size() )
    {
        return m_ModelElementSet[t];
    }
    else
    {
        return NULL;
    }
}

void svModel::SetModelElement(svModelElement* modelElement, unsigned int t)
{
    if(t<m_ModelElementSet.size())
    {
        m_ModelElementSet[t]=modelElement;

        m_CalculateBoundingBox = true;

        this->Modified();
        this->UpdateOutputInformation();
        this->InvokeEvent( svModelSetEvent() );
    }
}

void svModel::ExecuteOperation( mitk::Operation* operation )
{
    int timeStep = -1;

    svModelOperation* modelOperation = dynamic_cast<svModelOperation*>(operation);

    if ( modelOperation )
    {
        timeStep = modelOperation->GetTimeStep();

    }else{
        MITK_ERROR << "No valid Model Operation for svModel" << std::endl;
        return;
    }

    if ( timeStep < 0 )
    {
        MITK_ERROR << "Time step (" << timeStep << ") outside of svModel time bounds" << std::endl;
        return;
    }

    //svModelElement* originalModelElement=m_ModelElementSet[timeStep];

    svModelElement* newModelElement=modelOperation->GetModelElement();
    vtkSmartPointer<vtkPolyData> newVpd=modelOperation->GetVtkPolyData();

    switch (operation->GetOperationType())
    {

    case svModelOperation::OpSETMODELELEMENT:
    {
        SetModelElement(newModelElement,timeStep);
        m_DataModified=true;
    }
        break;

    case svModelOperation::OpSETVTKPOLYDATA:
    {
        svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(GetModelElement(timeStep));
        if(modelElement==NULL) return;

        modelElement->SetWholeVtkPolyData(newVpd);

        m_CalculateBoundingBox = true;
        m_DataModified=true;
        this->Modified();
        this->UpdateOutputInformation();
        this->InvokeEvent( svModelSetVtkPolyDataEvent() );
    }
        break;

    default:
        itkWarningMacro("svModel could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}


void svModel::CalculateBoundingBox(double *bounds,unsigned int t)
{
    svModelElement* modelElement=GetModelElement(t);
    if(modelElement)
    {
        modelElement->CalculateBoundingBox(bounds);
    }
}

void svModel::SetType(std::string type)
{
    m_Type=type;
}

std::string svModel::GetType() const
{
    return m_Type;
}

void svModel::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_ModelElementSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_ModelElementSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_ModelElementSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void svModel::SetRequestedRegionToLargestPossibleRegion()
{
}

bool svModel::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool svModel::VerifyRequestedRegion()
{
    return true;
}

void svModel::SetRequestedRegion(const DataObject * )
{
}

void svModel::PrintSelf( std::ostream& os, itk::Indent indent ) const
{
    Superclass::PrintSelf(os, indent);

    os << indent << "Number timesteps: " << m_ModelElementSet.size() << "\n";

    for ( unsigned int t = 0 ; t < m_ModelElementSet.size() ; ++t )
    {
        os << indent << "Timestep " << t << ": \n";
        itk::Indent nextIndent = indent.GetNextIndent();

        if(m_ModelElementSet[t])
        {
            os << nextIndent << "Model Type: " << m_ModelElementSet[t]->GetType() << "\n";
            if(m_ModelElementSet[t]->GetType()=="PolyData" && m_ModelElementSet[t]->GetWholeVtkPolyData())
            {
                os << nextIndent << "Number of cells: " << m_ModelElementSet[t]->GetWholeVtkPolyData()->GetNumberOfCells() << "\n";
                os << nextIndent << "Number of points: " << m_ModelElementSet[t]->GetWholeVtkPolyData()->GetNumberOfPoints() << "\n";
            }
        }else
        {
            os << nextIndent << "No Model! \n";
        }
    }
}

