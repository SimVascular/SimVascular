#ifndef SVMODELELEMENT_H
#define SVMODELELEMENT_H

#include <svModelExports.h>

#include "svModelElement.h"

#include <mitkDataNode.h>



class SVMODEL_EXPORT svModelElementPolyData : public svModelElement
{

public:

    svModelElementPolyData();

    svModelElementPolyData(const svModelElementPolyData &other);

    virtual ~svModelElementPolyData();

    virtual svModelElementPolyData* Clone() override;

    virtual vtkPolyData* CreateFaceVtkPolyData(int id) override;

    vtkPolyData* GetSolidModel() const;

    void SetSolidModel(vtkPolyData* solidModel);

  protected:

    vtkPolyData* m_SolidModel;

  };


#endif // SVMODELELEMENT_H
