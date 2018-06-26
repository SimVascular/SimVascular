#ifndef sv4guiImageSEEDMAPPER_H
#define sv4guiImageSEEDMAPPER_H

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkAssembly.h>
#include <vtkPropAssembly.h>
#include <vtkPainterPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkSmartPointer.h>
#include <vtkActor.h>
#include <string>
class sv4guiImageSeedMapper : public mitk::VtkMapper
{
public:

    mitkClassMacro(sv4guiImageSeedMapper, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {
    public:

        vtkSmartPointer<vtkAssembly> m_PropAssembly;

        LocalStorage()
        {
            m_PropAssembly = vtkSmartPointer<vtkAssembly>::New();
        }

        ~LocalStorage()
        {
        }
    };

    virtual vtkProp *GetVtkProp(mitk::BaseRenderer *renderer) override;

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    std::string mode;

    bool m_needsUpdate = true;

    bool m_box = false;

    double m_seedRadius = 0.5;

protected:
    sv4guiImageSeedMapper();

    virtual ~sv4guiImageSeedMapper();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    vtkSmartPointer<vtkActor> createSeedActor(int x, int y, int z, int color);

    vtkSmartPointer<vtkActor> createCubeActor(int x1, int y1, int z1,
    int x2, int y2, int z2 );
};

#endif /* sv4guiImageSEEDMAPPER_H */
