#ifndef SVMODELELEMENTPOLYDATA_H
#define SVMODELELEMENTPOLYDATA_H

#include <svModelExports.h>

#include "svModelElement.h"

#include <mitkDataNode.h>

#include <vtkSmartPointer.h>
#include <vtkPlanes.h>

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

    virtual vtkSmartPointer<vtkPolyData> CreateWholeVtkPolyData() override;

//    vtkSmartPointer<vtkPolyData> GetSolidModel() const;

//    void SetSolidModel(vtkSmartPointer<vtkPolyData> solidModel);

    svBlendParam* GetBlendParam();


    void AssignBlendParam(svBlendParam* param);

    bool DeleteFaces(std::vector<int> faceIDs);

    bool CombineFaces(std::vector<int> faceIDs);

    bool RemeshFaces(std::vector<int> faceIDs, double size);

    bool FillHolesWithIDs();

    bool ExtractFaces(double angle);

    bool FillHoles();

    bool SelectLargestConnectedRegion();

    bool Decimate(double targetRate);

    bool LaplacianSmooth(int numIters, double relaxFactor);

    bool ButterflySubdivide(int numDivs);

    bool WindowSincSmooth(int numIters, double band);

    bool Densify(int numDivs);

    std::vector<int> GetSelectedCellIDs();

    void ClearCellSelection();

    bool SelectCell(int cellID, bool select=true);

    bool DeleteCells(std::vector<int> cellIDs);

    bool MarkCells(std::vector<int> cellIDs);

    bool MarkCellsBySphere(double radius, double center[3]);

    bool MarkCellsByFaces(std::vector<int> faceIDs);

    bool DecimateLocal(double targetRate);//mark cells befor calling it

    bool LaplacianSmoothLocal(int numIters, double relaxFactor);//mark cells befor calling it

    bool ConstrainSmoothLocal(int numIters, double constrainFactor, int numCGSolves = 30);//mark cells befor calling it

    bool LinearSubdivideLocal(int numDivs);//mark cells befor calling it

    bool CutByPlane(double origin[3], double point1[3], double point2[3], bool above );

    bool CutByBox(vtkSmartPointer<vtkPlanes> boxPlanes, bool inside);

    void RemoveActiveCells();

  protected:

//    vtkSmartPointer<vtkPolyData> m_SolidModel;

    svBlendParam* m_BlendParam;

    std::vector<int> m_SelectedCellIDs;

  };


#endif // SVMODELELEMENTPOLYDATA_H
