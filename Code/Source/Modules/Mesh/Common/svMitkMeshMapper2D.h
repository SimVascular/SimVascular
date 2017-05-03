
#ifndef SVMITKMESHMAPPER2D_H
#define SVMITKMESHMAPPER2D_H

#include <svMeshExports.h>

#include "svMitkMesh.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkSmartPointer.h>
#include <vtkPropAssembly.h>

class SVMESH_EXPORT svMitkMeshMapper2D : public mitk::VtkMapper
{
public:
    mitkClassMacro(svMitkMeshMapper2D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const svMitkMesh* GetInput() const;

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

    svMitkMeshMapper2D();

    virtual ~svMitkMeshMapper2D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    void ApplyMapperProperties(vtkSmartPointer<vtkPolyDataMapper> mapper, mitk::BaseRenderer* renderer);

//    void Update(mitk::BaseRenderer* renderer) override;
};

#endif /* SVMITKMESHMAPPER2D_H */
