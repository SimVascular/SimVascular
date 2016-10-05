#ifndef SVMODELELEMENTPOLYDATA_H
#define SVMODELELEMENTPOLYDATA_H

#include <svModelExports.h>

#include "svModelElement.h"

#include <mitkDataNode.h>

//#include <vtkSmartPointer.h>

class SVMODEL_EXPORT svModelElementPolyData : public svModelElement
{
public:

    struct svBlendParamRadius
    {
        int id1;
        int id2;
        double radius;

        svBlendParamRadius()
            : id1(0)
            , id2(0)
            , radius(0.0)
        {
        }

        svBlendParamRadius(const svBlendParamRadius &other)
            : id1(other.id1)
            , id2(other.id2)
            , radius(other.radius)
        {
        }
    };

    struct svBlendParam
    {
        int numblenditers;
        int numsubblenditers;
        int numsubdivisioniters;
        int numcgsmoothiters;
        int numlapsmoothiters;
        double targetdecimation;

        svBlendParam()
            : numblenditers(2)
            , numsubblenditers(3)
            , numsubdivisioniters(1)
            , numcgsmoothiters(2)
            , numlapsmoothiters(50)
            , targetdecimation(0.01)
        {
        }

        svBlendParam(const svBlendParam &other)
            : numblenditers(other.numblenditers)
            , numsubblenditers(other.numsubblenditers)
            , numsubdivisioniters(other.numsubdivisioniters)
            , numcgsmoothiters(other.numcgsmoothiters)
            , numlapsmoothiters(other.numlapsmoothiters)
            , targetdecimation(other.targetdecimation)
        {}
    };

    svModelElementPolyData();

    svModelElementPolyData(const svModelElementPolyData &other);

    virtual ~svModelElementPolyData();

    virtual svModelElementPolyData* Clone() override;

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) override;

//    vtkSmartPointer<vtkPolyData> GetSolidModel() const;

//    void SetSolidModel(vtkSmartPointer<vtkPolyData> solidModel);

    svBlendParam* GetBlendParam();

    std::vector<svBlendParamRadius*> GetBlendRadius();

    void SetBlendRadius(std::vector<svBlendParamRadius*> blendRadius);

  protected:

//    vtkSmartPointer<vtkPolyData> m_SolidModel;

    svBlendParam* m_BlendParam;

    std::vector<svBlendParamRadius*> m_BlendRadius;


  };


#endif // SVMODELELEMENTPOLYDATA_H
