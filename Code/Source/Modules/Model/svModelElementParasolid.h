#ifndef SVMODELELEMENTPARASOLID_H
#define SVMODELELEMENTPARASOLID_H

#include <svModelExports.h>

#include "svModelElement.h"
#include "svModelElementPolyData.h"

#include "cvParasolidSolidModel.h"

class SVMODEL_EXPORT svModelElementParasolid : public svModelElement
{
public:

    svModelElementParasolid();

    svModelElementParasolid(const svModelElementParasolid &other);

    virtual ~svModelElementParasolid();

    virtual svModelElementParasolid* Clone() override;

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) override;

    virtual vtkSmartPointer<vtkPolyData> CreateWholeVtkPolyData() override;

    virtual void AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii) override;

    virtual void SetFaceName(std::string name, int id) override;

    int GetFaceIDFromInnerSolid(std::string faceName);

    double GetMaxDist();

    void SetMaxDist(double maxDist);

    svModelElementPolyData* ConverToPolyDataModel();

    cvParasolidSolidModel* GetInnerSolid();

    void SetInnerSolid(cvParasolidSolidModel* innerSolid);

protected:

    cvParasolidSolidModel* m_InnerSolid;

    double m_MaxDist;

};


#endif // SVMODELELEMENTPARASOLID_H
