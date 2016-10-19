#ifndef SVMODELELEMENTPOLYDATA_H
#define SVMODELELEMENTPOLYDATA_H

#include <svModelExports.h>

#include "svModelElement.h"

#include <mitkDataNode.h>

//#include <vtkSmartPointer.h>

class SVMODEL_EXPORT svModelElementPolyData : public svModelElement
{
public:

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


    void AssignBlendParam(svBlendParam* param);

    bool DeleteFaces(std::vector<int> faceIDs);

    bool CombineFaces(std::vector<int> faceIDs);

    bool RemeshFaces(std::vector<int> faceIDs, double size);

    bool FillHolesWithIDs();


  protected:

//    vtkSmartPointer<vtkPolyData> m_SolidModel;

    svBlendParam* m_BlendParam;

  };


#endif // SVMODELELEMENTPOLYDATA_H
