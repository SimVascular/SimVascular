#ifndef SVMODELELEMENTANALYTIC_H
#define SVMODELELEMENTANALYTIC_H

#include <svModelExports.h>

#include "svModelElement.h"
#include "svModelElementPolyData.h"

class SVMODEL_EXPORT svModelElementAnalytic : public svModelElement
{
public:

    svModelElementAnalytic();

    svModelElementAnalytic(const svModelElementAnalytic &other);

    virtual ~svModelElementAnalytic();

    virtual svModelElementAnalytic* Clone() override;

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) override;

    virtual vtkSmartPointer<vtkPolyData> CreateWholeVtkPolyData() override;

    virtual void AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii) override;

//    virtual void SetFaceName(std::string name, int id) override;

//    virtual int GetFaceIDFromInnerSolid(std::string faceName) override;

//    virtual std::vector<int> GetFaceIDsFromInnerSolid() override;

//    virtual std::string GetFaceNameFromInnerSolid(int id) override;

    virtual double GetMaxDist();

    virtual void SetMaxDist(double maxDist);

    virtual svModelElementPolyData* ConverToPolyDataModel();

//    virtual cvSolidModel* GetInnerSolid();

//    virtual void SetInnerSolid(cvSolidModel* innerSolid);

protected:

    double m_MaxDist;

};

#endif // SVMODELELEMENTANALYTIC_H
