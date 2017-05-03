
#ifndef SVMITKMESHMAPPER3D_H
#define SVMITKMESHMAPPER3D_H

#include <svMeshExports.h>

#include "svMitkMesh.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkAssembly.h>
#include <vtkPropAssembly.h>
#include <vtkPainterPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkPlaneCollection.h>
#include <vtkSmartPointer.h>

class SVMESH_EXPORT svMitkMeshMapper3D : public mitk::VtkMapper
{
public:

    mitkClassMacro(svMitkMeshMapper3D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {
    public:

        vtkSmartPointer<vtkAssembly> m_PropAssembly;
        vtkSmartPointer<vtkPlaneCollection> m_ClippingPlaneCollection;

        vtkSmartPointer<vtkActor> m_Actor;

        LocalStorage()
        {
            m_PropAssembly = vtkSmartPointer<vtkAssembly>::New();
            m_ClippingPlaneCollection = vtkSmartPointer<vtkPlaneCollection>::New();
        }

        ~LocalStorage()
        {
        }
    };

    virtual const svMitkMesh* GetInput();

    virtual vtkProp *GetVtkProp(mitk::BaseRenderer *renderer) override;

    static void ApplyAllProperties(mitk::DataNode *node, mitk::BaseRenderer* renderer, vtkSmartPointer<vtkPainterPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor, mitk::LocalStorageHandler<LocalStorage>* handler, bool clipping = true);

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    static void ApplyMitkPropertiesToVtkProperty(mitk::DataNode *node, vtkProperty* property, mitk::BaseRenderer* renderer);
    static void SetDefaultPropertiesForVtkProperty(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite);

protected:
    svMitkMeshMapper3D();

    virtual ~svMitkMeshMapper3D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    static void CheckForClippingProperty( mitk::BaseRenderer* renderer, mitk::BaseProperty *property, mitk::LocalStorageHandler<LocalStorage>* handler );

};

#endif /* SVMITKMESHMAPPER3D_H */
