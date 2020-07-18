/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_MODELELEMENT_H
#define SV4GUI_MODELELEMENT_H

#include <sv4guiModuleModelExports.h>

#define __CVMISC_UTILS_H //avoid including sv_misc_utils.h from sv_SolidModel.h
#include "sv_SolidModel.h"

#include "sv4gui_ContourGroup.h"

#include <mitkDataNode.h>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>
#include <map>

class SV4GUIMODULEMODEL_EXPORT sv4guiModelElement
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

    sv4guiModelElement();

    sv4guiModelElement(const sv4guiModelElement &other);

    virtual ~sv4guiModelElement();

    virtual sv4guiModelElement* Clone();

    std::string GetType() const;

    void SetType(std::string type);

    std::vector<std::string> GetSegNames() const;

    void SetSegNames(std::vector<std::string> segNames);

    bool HasSeg(std::string segName);

    std::vector<svFace*> GetFaces() const;

    std::vector<std::string> GetFaceNames() const;

    void SetFaces(std::vector<svFace*> faces);

    svFace* GetFace(int id) const;

    svFace* GetFace(std::string name) const;

    int GetFaceIndex(int id) const;

    int GetFaceID(std::string name) const;

    int GetMaxFaceID() const;

    std::map<std::string,int> GetFaceNameIDMap() const;

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

    virtual sv4guiModelElement* CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                               , int numSamplingPts
                                               , svLoftingParam *param
                                               , int* stats = NULL
                                               , double maxDist = 1.0
                                               , int noInterOut = 1
                                               , double tol = 1e-6
                                               , unsigned int t = 0) {return NULL;}

    virtual sv4guiModelElement* CreateModelElementByBlend(std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii
                                                      , sv4guiModelElement::svBlendParam* param) {return NULL;}

    virtual bool ReadFile(std::string filePath){return false;}

    virtual bool WriteFile(std::string filePath){return false;}

    svBlendParam* GetBlendParam();

    void AssignBlendParam(svBlendParam* param);

    std::vector<std::string> GetFileExtensions(){return m_FileExtensions;}

    virtual int GetFaceIDFromInnerSolid(std::string faceName);

    virtual int GetFaceIdentifierFromInnerSolid(std::string faceName);

    virtual int GetFaceIdentifierFromInnerSolid(int faceID);

    virtual std::vector<int> GetFaceIDsFromInnerSolid();

    virtual std::string GetFaceNameFromInnerSolid(int faceID);

    virtual cvSolidModel* GetInnerSolid();

    virtual void SetInnerSolid(cvSolidModel* innerSolid);

    int IfUseUniform() {return m_UseUniform;}
    void SetUseUniform(int flag) { m_UseUniform=flag;}

    svLoftingParam* GetLoftingParam(){return m_LoftParam;}

    void SetLoftingParam(svLoftingParam* param) {
        if(m_LoftParam) delete m_LoftParam;
        m_LoftParam=param;
    }

  protected:

    std::string m_Type;

    std::vector<std::string> m_SegNames;

    std::vector<svFace*> m_Faces;

    vtkSmartPointer<vtkPolyData> m_WholeVtkPolyData;

    std::vector<svBlendParamRadius*> m_BlendRadii;

    int m_NumSampling;

    svBlendParam* m_BlendParam;

    std::vector<std::string> m_FileExtensions;

    cvSolidModel* m_InnerSolid;

    int m_UseUniform;

    svLoftingParam* m_LoftParam;
  };


#endif // SV4GUI_MODELELEMENT_H
