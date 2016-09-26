#ifndef SVMODELELEMENT_H
#define SVMODELELEMENT_H

#include <svModelExports.h>

#include <mitkDataNode.h>

#include "vtkPolyData.h"
//#include "vtkSmartPointer.h"

class SVMODEL_EXPORT svModelElement
{

public:

    struct svFace
    {
        int id;
        std::string name;
        vtkPolyData* vpd;

        mitk::DataNode* node;

//        float opacity;
//        bool visible;
//        double color[3];



//        svFace()
//            : id(0)
//            , name("")
//            , vpd(NULL)
//            , opacity(1.0f)
//            , visible(true)
//        {
//            color[0]=1.0;
//            color[1]=1.0;
//            color[2]=1.0;
//        }

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

    int GetFaceIndex(int id) const;

    std::string GetFaceName(int id) const;

    void SetFaceName(std::string name, int id);



//    std::vector<std::string> GetFaceNames();

//    void SetFaceNames(std::vector<std::string> faceNames);

//    std::string GetFaceName(int id);

//    void SetFaceName(std::string faceName, int id);

//    std::vector<vtkPolyData*> GetVtkPolyDataFaces();

//    void SetVtkPolyDataFaces(std::vector<vtkPolyData*> vpdFaces);

    vtkPolyData* GetVtkPolyDataModel() const;

    void SetVtkPolyDataModel(vtkPolyData* vpdModel);

    int GetSelectedFaceIndex();

    void SetSelectedFaceIndex(int idx);


  protected:

    std::string m_Type;

    std::vector<std::string> m_SegNames;

//    std::vector<std::string> m_FaceNames;

//    std::vector<vtkPolyData*> m_VtkPolyDataFaces;

    std::vector<svFace*> m_Faces;

    vtkPolyData* m_VtkPolyDataModel;

    int m_SelectedFaceIndex;

  };


#endif // SVMODELELEMENT_H
