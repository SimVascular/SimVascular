#include "svModel.h"
#include "svModelElementPolyData.h"

svModel::svModel()
{
    this->InitializeEmpty();
}

svModel::svModel(const svModel &other)
    : mitk::Surface(other)
    , m_ModelElementSet(other.m_ModelElementSet.size())
{
    for (std::size_t t = 0; t < other.m_ModelElementSet.size(); ++t)
    {
        m_ModelElementSet.push_back(other.m_ModelElementSet[t]->Clone());
    }
}

svModel::~svModel()
{
    this->ClearData();
}

void svModel::Expand(unsigned int timeSteps)
{
    Superclass::Expand(timeSteps);

    if (timeSteps > m_ModelElementSet.size())
    {
        m_ModelElementSet.resize(timeSteps);

        this->InvokeEvent( svModelExtendTimeRangeEvent() );
    }
}

//virtual bool svModel::IsEmptyTimeStep(unsigned int t) const;

void svModel::ClearData()
{
//    for(int t=0;t<m_ModelElementSet.size();t++)
//        delete m_ModelElementSet[t];

    m_ModelElementSet.clear();
    Superclass::ClearData();
}

void svModel::InitializeEmpty()
{
    Superclass::InitializeEmpty();

    m_ModelElementSet.resize( 1 );
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
        vtkPolyData* vpd=NULL;
        if(modelElement)
        {
            vpd=modelElement->GetVtkPolyDataModel();
        }
        SetVtkPolyData(vpd,t);

        Modified();
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
    vtkPolyData* newVpd=modelOperation->GetVtkPolyData();

    switch (operation->GetOperationType())
    {

    case svModelOperation::OpSETMODELELEMENT:
    {
        SetModelElement(newModelElement,timeStep);
    }
        break;

    case svModelOperation::OpSETVTKPOLYDATA:
    {
        svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(GetModelElement(timeStep));
        if(modelElement==NULL) return;

        modelElement->SetSolidModel(newVpd);
        SetModelElement(modelElement,timeStep);

        Modified();
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
