#ifndef SVCONTOURGROUPVTKMAPPER3D_H
#define SVCONTOURGROUPVTKMAPPER3D_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourGroup.h"

#include "mitkCommon.h"

#include "mitkBaseRenderer.h"
#include "mitkVtkMapper.h"

#include <vtkSmartPointer.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkAssembly.h>
#include <vtkPropAssembly.h>
#include <vtkProp.h>
#include <vtkPolyData.h>
#include <vtkTubeFilter.h>

class SVSEGMENTATION_EXPORT svContourGroupVtkMapper3D : public mitk::VtkMapper
{

public:

    mitkClassMacro( svContourGroupVtkMapper3D,mitk::VtkMapper );
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    const svContourGroup* GetInput(void);

//    virtual void Update(mitk::BaseRenderer * renderer) override;

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    class SVSEGMENTATION_EXPORT LocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        vtkSmartPointer<vtkPropAssembly> m_Assembly;

//        mitk::ContourModelToSurfaceFilter::Pointer m_contourToPolyData;

        itk::TimeStamp m_LastUpdateTime;

        LocalStorage();

        ~LocalStorage()
        {
        }
    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    LocalStorage* GetLocalStorage(mitk::BaseRenderer* renderer);

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);


protected:
    svContourGroupVtkMapper3D();
    virtual ~svContourGroupVtkMapper3D();

    void GenerateDataForRenderer( mitk::BaseRenderer *renderer ) override;

    void ResetMapper( mitk::BaseRenderer* renderer ) override;

//    virtual vtkSmartPointer<vtkPolyData> CreateVtkPolyDataFromContour(svContour* contour);

//    virtual void ApplyContourProperties(mitk::BaseRenderer* renderer);
};

#endif //SVCONTOURGROUPVTKMAPPER3D_H
