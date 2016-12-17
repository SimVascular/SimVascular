#ifndef SVMODELELEMENTOCCT_H
#define SVMODELELEMENTOCCT_H

#include <svModelExports.h>

#include "svModelElement.h"
#include "svModelElementPolyData.h"

#include "cvOCCTSolidModel.h"

class SVMODEL_EXPORT svModelElementOCCT : public svModelElement
{
public:

    svModelElementOCCT();

    svModelElementOCCT(const svModelElementOCCT &other);

    virtual ~svModelElementOCCT();

    virtual svModelElementOCCT* Clone() override;

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) override;

    virtual vtkSmartPointer<vtkPolyData> CreateWholeVtkPolyData() override;

    virtual void AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii) override;

    virtual void SetFaceName(std::string name, int id) override;

    int GetFaceIDFromInnerSolid(std::string faceName);

    double GetMaxDist();

    void SetMaxDist(double maxDist);

    svModelElementPolyData* ConverToPolyDataModel();

    cvOCCTSolidModel* GetOCCTSolid();

    void SetOCCTSolid(cvOCCTSolidModel* occtSolid);

protected:

    cvOCCTSolidModel* m_OCCTSolid;

    double m_MaxDist;

};


#endif // SVMODELELEMENTOCCT_H
