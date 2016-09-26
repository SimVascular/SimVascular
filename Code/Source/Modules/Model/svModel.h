#ifndef SVMODEL_H
#define SVMODEL_H

#include <svModelExports.h>

#include "svModelElement.h"
#include "svModelOperation.h"

#include <mitkSurface.h>

class SVMODEL_EXPORT svModel : public mitk::Surface
{
public:

    mitkClassMacro(svModel, mitk::Surface);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps = 1 ) override;

    virtual void ExecuteOperation(mitk::Operation *operation) override;

//    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    virtual unsigned int GetTimeSize() const;

//    virtual vtkPolyData* GetVtkPolyData(unsigned int t=0) const override;

//    virtual void SetVtkPolyData (vtkPolyData *polydata, unsigned int t=0) override;

//    std::vector<svModelElement::svFace*> GetFaces(unsigned int t=0) const;

    svModelElement* GetModelElement(unsigned int t=0) const;

    void SetModelElement(svModelElement* modelElement, unsigned int t=0);

  protected:

    mitkCloneMacro(Self);

    svModel();
    svModel(const svModel &other);
    virtual ~svModel();

    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

    std::vector< svModelElement* > m_ModelElementSet;

};

itkEventMacro( svModelEvent, itk::AnyEvent );

itkEventMacro( svModelExtendTimeRangeEvent, svModelEvent );
itkEventMacro( svModelSetEvent, svModelEvent );
itkEventMacro( svModelSetVtkPolyDataEvent, svModelEvent );

#endif // SVMODEL_H
