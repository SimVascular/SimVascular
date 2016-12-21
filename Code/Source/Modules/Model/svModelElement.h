#ifndef SVMODELELEMENT_H
#define SVMODELELEMENT_H

#include <svModelExports.h>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>

class SVMODEL_EXPORT svModelElement
{

public:

    struct svFace
    {
        int id;
        std::string name;
        std::string type;//wall, cap, inlet, or empty if unknown

        vtkSmartPointer<vtkPolyData> vpd;

        bool selected;

        bool visible;
        float opacity;
        float color[3];

        svFace()
            : id(0)
            , name("")
            , type("")
            , vpd(NULL)
            , selected(false)
            , visible(true)
            , opacity(1.0f)
        {
            color[0]=1.0f;
            color[1]=1.0f;
            color[2]=1.0f;
        }

        svFace(const svFace &other, bool copyData = true)
            : id(other.id)
            , name(other.name)
            , type(other.type)
            , selected(other.selected)
            , visible(other.visible)
            , opacity(other.opacity)
        {
            color[0]=other.color[0];
            color[1]=other.color[1];
            color[2]=other.color[2];

            if(copyData)
            {
                vtkSmartPointer<vtkPolyData> othervpd=NULL;
                if(other.vpd)
                {
                    othervpd=vtkSmartPointer<vtkPolyData>::New();
                    othervpd->DeepCopy(other.vpd);
                }
                vpd=othervpd;
            }
            else
                vpd=NULL;
        }

    };

    struct svBlendParamRadius
    {
        int faceID1;
        int faceID2;
        double radius;
        std::string faceName1;
        std::string faceName2;

        svBlendParamRadius()
            : faceID1(0)
            , faceID2(0)
            , radius(0.0)
            , faceName1("")
            , faceName2("")
        {
        }

        svBlendParamRadius(int id1, int id2, double r)
            : faceID1(id1)
            , faceID2(id2)
            , faceName1("")
            , faceName2("")
            , radius(r)
        {
        }

        svBlendParamRadius(int id1, int id2, std::string name1, std::string name2, double r)
            : faceID1(id1)
            , faceID2(id2)
            , faceName1(name1)
            , faceName2(name2)
            , radius(r)
        {
        }

        svBlendParamRadius(const svBlendParamRadius &other)
            : faceID1(other.faceID1)
            , faceID2(other.faceID2)
            , faceName1(other.faceName1)
            , faceName2(other.faceName2)
            , radius(other.radius)
        {
        }
    };

    svModelElement();

    svModelElement(const svModelElement &other);

    virtual ~svModelElement();

    virtual svModelElement* Clone();

    std::string GetType() const;

    void SetType(std::string type);

    std::vector<std::string> GetSegNames() const;

    void SetSegNames(std::vector<std::string> segNames);

    bool HasSeg(std::string segName);

    std::vector<svFace*> GetFaces() const;

    void SetFaces(std::vector<svFace*> faces);

    svFace* GetFace(int id) const;

    svFace* GetFace(std::string name) const;

    int GetFaceIndex(int id) const;

    int GetFaceID(std::string name) const;

    int GetMaxFaceID() const;

    int GetFaceNumber() const;

    std::string GetFaceName(int id) const;

    virtual void SetFaceName(std::string name, int id);

    vtkSmartPointer<vtkPolyData> GetWholeVtkPolyData() const;

    void SetWholeVtkPolyData(vtkSmartPointer<vtkPolyData> wvpd);

    virtual vtkSmartPointer<vtkPolyData> CreateFaceVtkPolyData(int id) {return NULL;}

    virtual vtkSmartPointer<vtkPolyData> CreateWholeVtkPolyData() {return NULL;}

//    int GetSelectedFaceIndex();

    void SelectFaceByIndex(int idx, bool select = true);

    void ClearFaceSelection();

    void SelectFace(int id);
    void SelectFace(std::string name);
    bool IsFaceSelected(std::string name);
    bool IsFaceSelected(int id);

    std::vector<int> GetAllFaceIDs();

    std::vector<int> GetSelectedFaceIDs();

    std::vector<int> GetWallFaceIDs();

    std::vector<int> GetCapFaceIDs();

    std::vector<int> GetInletFaceIDs();

    std::vector<int> GetOutletFaceIDs();

    double GetFaceArea(int id);

    double GetMinFaceArea();

    std::vector<svBlendParamRadius*> GetBlendRadii();

    void SetBlendRadii(std::vector<svBlendParamRadius*> blendRadii);

    svBlendParamRadius* GetBlendParamRadius(int faceID1, int faceID2);

    svBlendParamRadius* GetBlendParamRadius(std::string faceName1, std::string faceName2);

    virtual void AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii);

    void RemoveFace(int faceID);
    void RemoveFaceFromBlendParamRadii(int faceID);

    void ReplaceFaceIDForBlendParamRadii(int targetID, int loseID);

    void CalculateBoundingBox(double *bounds);

    void SetNumSampling(int num);

    int GetNumSampling();

  protected:

    std::string m_Type;

    std::vector<std::string> m_SegNames;

    std::vector<svFace*> m_Faces;

    vtkSmartPointer<vtkPolyData> m_WholeVtkPolyData;

    std::vector<svBlendParamRadius*> m_BlendRadii;

    int m_NumSampling;

  };


#endif // SVMODELELEMENT_H
