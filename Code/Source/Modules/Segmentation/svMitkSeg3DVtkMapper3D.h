#ifndef SVMITKSEG3DVTKMAPPER3D_H
#define SVMITKSEG3DVTKMAPPER3D_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svMitkSeg3D.h"

#include "svSurfaceVtkMapper3D.h"

class SVSEGMENTATION_EXPORT svMitkSeg3DVtkMapper3D : public mitk::svSurfaceVtkMapper3D
{
public:

    mitkClassMacro( svMitkSeg3DVtkMapper3D, mitk::svSurfaceVtkMapper3D );
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const svMitkSeg3D* GetInput() override;

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

protected:

    svMitkSeg3DVtkMapper3D();

    virtual ~svMitkSeg3DVtkMapper3D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

public:

    class svLocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        vtkSmartPointer<vtkPropAssembly> m_Assembly;

        vtkSmartPointer<vtkActor> m_SelectedSeedActor;
        vtkSmartPointer<vtkPainterPolyDataMapper> m_SelectedSeedVtkPolyDataMapper;

        vtkSmartPointer<vtkActor> m_SeedActor;
        vtkSmartPointer<vtkPainterPolyDataMapper> m_SeedVtkPolyDataMapper;

        vtkSmartPointer<vtkActor> m_EndSeedActor;
        vtkSmartPointer<vtkPainterPolyDataMapper> m_EndSeedVtkPolyDataMapper;

//        itk::TimeStamp m_LastUpdateTime;

        svLocalStorage()
        {
            m_SelectedSeedVtkPolyDataMapper=vtkSmartPointer<vtkPainterPolyDataMapper>::New();
            m_SelectedSeedActor=vtkSmartPointer<vtkActor>::New();
            m_SelectedSeedActor->SetMapper(m_SelectedSeedVtkPolyDataMapper);

            m_SeedVtkPolyDataMapper=vtkSmartPointer<vtkPainterPolyDataMapper>::New();
            m_SeedActor=vtkSmartPointer<vtkActor>::New();
            m_SeedActor->SetMapper(m_SeedVtkPolyDataMapper);

            m_EndSeedVtkPolyDataMapper=vtkSmartPointer<vtkPainterPolyDataMapper>::New();
            m_EndSeedActor=vtkSmartPointer<vtkActor>::New();
            m_EndSeedActor->SetMapper(m_EndSeedVtkPolyDataMapper);

            m_Assembly=vtkSmartPointer<vtkPropAssembly>::New();
        }

        ~svLocalStorage()
        {
        }
    };

    mitk::LocalStorageHandler<svLocalStorage> m_LSHandler;

    bool m_AlreadyAddParts;

};

#endif //SVMITKSEG3DVTKMAPPER3D_H
