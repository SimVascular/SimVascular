#ifndef SVMODELELEMENTPOLYDATA_H
#define SVMODELELEMENTPOLYDATA_H

#include <svModelExports.h>

#include "svModelElement.h"

#include <mitkDataNode.h>

//#include <vtkSmartPointer.h>

class SVMODEL_EXPORT svModelElementPolyData : public svModelElement
{

public:

    svModelElementPolyData();

    svModelElementPolyData(const svModelElementPolyData &other);

    virtual ~svModelElementPolyData();

    virtual svModelElementPolyData* Clone() override;

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) override;

//    vtkSmartPointer<vtkPolyData> GetSolidModel() const;

//    void SetSolidModel(vtkSmartPointer<vtkPolyData> solidModel);

  protected:

//    vtkSmartPointer<vtkPolyData> m_SolidModel;

  };


#endif // SVMODELELEMENTPOLYDATA_H
