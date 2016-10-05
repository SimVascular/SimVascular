
#ifndef SVMODELVTKMAPPER2D_H
#define SVMODELVTKMAPPER2D_H

#include <svModelExports.h>

#include "svModel.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkSmartPointer.h>
#include <vtkPropAssembly.h>

class SVMODEL_EXPORT svModelVtkMapper2D : public mitk::VtkMapper
{
public:
    mitkClassMacro(svModelVtkMapper2D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const svModel* GetInput() const;

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {
    public:

        itk::TimeStamp m_LastUpdateTime;

        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

        LocalStorage();

        ~LocalStorage();
    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    void UpdateVtkTransform(mitk::BaseRenderer* /*renderer*/) override
    {
    }

protected:

    svModelVtkMapper2D();

    virtual ~svModelVtkMapper2D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    void ApplyMapperProperties(vtkSmartPointer<vtkPolyDataMapper> mapper, mitk::BaseRenderer* renderer);

//    void Update(mitk::BaseRenderer* renderer) override;
};

#endif /* SVMODELVTKMAPPER2D_H */
