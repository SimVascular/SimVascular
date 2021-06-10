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

#include "sv4gui_ModelIO.h"
#include "sv4gui_Model.h"
#include "sv4gui_ModelElementAnalytic.h"
#include "sv4gui_ModelElementFactory.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <vtkCleanPolyData.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>

static mitk::CustomMimeType Createsv4guiModelMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svmodel");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("mdl");
    mimeType.SetComment("SimVascular Model");

    return mimeType;
}

sv4guiModelIO::sv4guiModelIO()
    : mitk::AbstractFileIO(sv4guiModel::GetStaticNameOfClass(), Createsv4guiModelMimeType(), "SimVascular Model")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiModelIO::Read()
{
    std::string fileName=GetInputLocation();

    return ReadFile(fileName);
}

std::vector<mitk::BaseData::Pointer> sv4guiModelIO::ReadFile(std::string fileName)
{
    auto group = CreateGroupFromFile(fileName);
    std::vector<mitk::BaseData::Pointer> result { group.GetPointer() };
    return result;
}

//---------------------
// CreateGroupFromFile
//---------------------
// Create a sv4guiModel group from a file.
//
sv4guiModel::Pointer sv4guiModelIO::CreateGroupFromFile(std::string fileName)
{
    TiXmlDocument document;

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
//        return result;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* modelElement = document.FirstChildElement("model");

    if(!modelElement){
        mitkThrow() << "No Model data in "<< fileName;
//        return result;
    }

    sv4guiModel::Pointer model = sv4guiModel::New();
    std::string modelType="";
    modelElement->QueryStringAttribute("type",&modelType);
    model->SetType(modelType);

    int timestep=-1;
    for( TiXmlElement* timestepElement = modelElement->FirstChildElement("timestep");
         timestepElement != nullptr;
         timestepElement = timestepElement->NextSiblingElement("timestep") )
    {
        if (timestepElement == nullptr)
            continue;

        //        timestepElement->QueryIntAttribute("id",&timestep);
        timestep++;
        model->Expand(timestep+1);

        TiXmlElement* meElement = timestepElement->FirstChildElement("model_element");
        if(meElement != nullptr)
        {
            std::string type="";
            meElement->QueryStringAttribute("type", &type);
            if(type=="")
            {
                mitkThrow() << "No type info available when trying to load the model ";
//                return result;
            }

            sv4guiModelElement* me=sv4guiModelElementFactory::CreateModelElement(type);
            if(me==NULL)
            {
                mitkThrow() << "No model constructor available for model type: "<< type;
//                return result;
            }

            std::string fileExtension="";
            auto exts=me->GetFileExtensions();
            if(exts.size()>0)
                fileExtension=exts[0];
            if(fileExtension=="")
            {
                mitkThrow() << "No file extension available for model type: "<< type;
//                return result;
            }

            std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+"." +fileExtension;

            if(!me->ReadFile(dataFileName))
            {
                delete me;
                mitkThrow() << "Failed in reading: "<< dataFileName;
            }

            sv4guiModelElementAnalytic* meAnalytic=dynamic_cast<sv4guiModelElementAnalytic*>(me);
            if(meAnalytic)
            {
                double maxDist=0;
                if(meElement->QueryDoubleAttribute("max_dist", &maxDist)==TIXML_SUCCESS)
                {
                    meAnalytic->SetMaxDist(maxDist);
                }
                meAnalytic->SetWholeVtkPolyData(meAnalytic->CreateWholeVtkPolyData());
            }

            int numSampling=0;
            meElement->QueryIntAttribute("num_sampling", &numSampling);
            me->SetNumSampling(numSampling);

            int useUniform=me->IfUseUniform();
            meElement->QueryIntAttribute("use_uniform", &useUniform);
            me->SetUseUniform(useUniform);

            svLoftingParam* param=me->GetLoftingParam();
            if(useUniform && param )
            {
                meElement->QueryStringAttribute("method", &param->method);

                meElement->QueryIntAttribute("sampling", &param->numOutPtsInSegs);
                meElement->QueryIntAttribute("sample_per_seg",&param->samplePerSegment);
                meElement->QueryIntAttribute("use_linear_sample",&param->useLinearSampleAlongLength);
                meElement->QueryIntAttribute("linear_multiplier",&param->linearMuliplier);
                meElement->QueryIntAttribute("use_fft",&param->useFFT);
                meElement->QueryIntAttribute("num_modes",&param->numModes);

                meElement->QueryIntAttribute("u_degree",&param->uDegree);
                meElement->QueryIntAttribute("v_degree",&param->vDegree);
                meElement->QueryStringAttribute("u_knot_type",&param->uKnotSpanType);
                meElement->QueryStringAttribute("v_knot_type",&param->vKnotSpanType);
                meElement->QueryStringAttribute("u_parametric_type",&param->uParametricSpanType);
                meElement->QueryStringAttribute("v_parametric_type",&param->vParametricSpanType);
            }

            TiXmlElement* facesElement = meElement->FirstChildElement("faces");
            if(facesElement!=nullptr)
            {
                std::vector<sv4guiModelElement::svFace*> faces;
                for( TiXmlElement* faceElement = facesElement->FirstChildElement("face");
                     faceElement != nullptr;
                     faceElement =faceElement->NextSiblingElement("face") )
                {
                    if (faceElement == nullptr)
                        continue;

                    int id;
                    std::string name="";
                    std::string facetype="";
                    std::string isVisible="true";
                    float opacity=1.0f;
                    float color1=1.0f;
                    float color2=1.0f;
                    float color3=1.0f;

                    faceElement->QueryIntAttribute("id", &id);
                    faceElement->QueryStringAttribute("name", &name);
                    faceElement->QueryStringAttribute("type", &facetype);
                    faceElement->QueryStringAttribute("visible", &isVisible);
                    faceElement->QueryFloatAttribute("opacity", &opacity);
                    faceElement->QueryFloatAttribute("color1", &color1);
                    faceElement->QueryFloatAttribute("color2", &color2);
                    faceElement->QueryFloatAttribute("color3", &color3);

                    vtkSmartPointer<vtkPolyData> facepd=NULL;
                    //face id not exists in parasolid, so skip this and set later.
                    if(type!="Parasolid")
                        facepd=me->CreateFaceVtkPolyData(id);
                    sv4guiModelElement::svFace* face=new sv4guiModelElement::svFace;
                    face->id=id;
                    face->name=name;
                    face->type=facetype;
                    face->visible=(isVisible=="true"?true:false);
                    face->opacity=opacity;
                    face->color[0]=color1;
                    face->color[1]=color2;
                    face->color[2]=color3;

                    face->vpd=facepd;

                    faces.push_back(face);
                }
                me->SetFaces(faces);
            }

            TiXmlElement* segsElement = meElement->FirstChildElement("segmentations");
            if(segsElement!=nullptr)
            {
                std::vector<std::string> segNames;
                for( TiXmlElement* segElement = segsElement->FirstChildElement("seg");
                     segElement != nullptr;
                     segElement = segElement->NextSiblingElement("seg") )
                {
                    if (segElement == nullptr)
                        continue;

                    std::string name;
                    segElement->QueryStringAttribute("name", &name);
                    segNames.push_back(name);
                }
                me->SetSegNames(segNames);
            }

            TiXmlElement* blendRadiiElement = meElement->FirstChildElement("blend_radii");
            if(blendRadiiElement!=nullptr)
            {
                std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii;
                for( TiXmlElement* radiusElement = blendRadiiElement->FirstChildElement("face_pair");
                     radiusElement != nullptr;
                     radiusElement =blendRadiiElement->NextSiblingElement("face_pair") )
                {
                    if (radiusElement == nullptr)
                        continue;

                    int faceID1=0;
                    int faceID2=0;
                    double radius=0;

                    radiusElement->QueryIntAttribute("face_id1", &faceID1);
                    radiusElement->QueryIntAttribute("face_id2", &faceID2);
                    radiusElement->QueryDoubleAttribute("radius", &radius);

//                    blendRadii.push_back(new sv4guiModelElement::svBlendParamRadius(faceID1,faceID2,radius));
                    blendRadii.push_back(new sv4guiModelElement::svBlendParamRadius(faceID1,faceID2,me->GetFaceName(faceID1),me->GetFaceName(faceID2),radius));

                }
                me->SetBlendRadii(blendRadii);
            }

            if(type=="PolyData")
            {
                TiXmlElement* blendElement = meElement->FirstChildElement("blend_param");
                if(blendElement!=nullptr)
                {
                    sv4guiModelElement::svBlendParam* param=me->GetBlendParam();
                    blendElement->QueryIntAttribute("blend_iters", &(param->numblenditers));
                    blendElement->QueryIntAttribute("sub_blend_iter", &(param->numsubblenditers));
                    blendElement->QueryIntAttribute("cstr_smooth_iter", &(param->numcgsmoothiters));
                    blendElement->QueryIntAttribute("lap_smooth_iter", &(param->numlapsmoothiters));
                    blendElement->QueryIntAttribute("subdivision_iters", &(param->numsubdivisioniters));
                    blendElement->QueryDoubleAttribute("decimation", &(param->targetdecimation));
                }
            }

            //update face ids (face ids change when loading parasolid file and are become different from info in .mdl file)
            if(type=="Parasolid" && meAnalytic)
            {
                std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii=meAnalytic->GetBlendRadii();
                //set face names
                for(int i=0;i<blendRadii.size();i++)
                {
                    if(blendRadii[i])
                    {
                        blendRadii[i]->faceName1=meAnalytic->GetFaceName(blendRadii[i]->faceID1);
                        blendRadii[i]->faceName2=meAnalytic->GetFaceName(blendRadii[i]->faceID2);
                    }
                }

                //update face id and vpd
                std::vector<sv4guiModelElement::svFace*> faces=meAnalytic->GetFaces();
                for(int i=0;i<faces.size();i++)
                {
                    faces[i]->id=meAnalytic->GetFaceIDFromInnerSolid(faces[i]->name);
                    faces[i]->vpd=meAnalytic->CreateFaceVtkPolyData(faces[i]->id);
                }

                //update face id
                for(int i=0;i<blendRadii.size();i++)
                {
                    if(blendRadii[i])
                    {
                        int faceID1=meAnalytic->GetFaceID(blendRadii[i]->faceName1);
                        int faceID2=meAnalytic->GetFaceID(blendRadii[i]->faceName2);
                        blendRadii[i]->faceID1=faceID1;
                        blendRadii[i]->faceID2=faceID2;
                    }
                }
            }

            model->SetModelElement(me,timestep);
        } //model element

    }//timestep

    return model;
}

mitk::IFileIO::ConfidenceLevel sv4guiModelIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiModelIO::Write()
{
    ValidateOutputLocation();
    std::string fileName = GetOutputLocation();
    const sv4guiModel* model = dynamic_cast<const sv4guiModel*>(this->GetInput());

    if (!model) {
      return;
    }

    WriteGroupToFile(const_cast<sv4guiModel*>(model), fileName);
}

//------------------
// WriteGroupToFile
//------------------
// Write a model group to a .mdl file.
//
// This method can be called from the Python API. 
//
void sv4guiModelIO::WriteGroupToFile(sv4guiModel* model, std::string& fileName)
{
    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  modelElement = new TiXmlElement("model");
    modelElement->SetAttribute("type", model->GetType());
    modelElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(modelElement);

    for(int t=0;t<model->GetTimeSize();t++)
    {
        auto  timestepElement = new TiXmlElement("timestep");
        timestepElement->SetAttribute("id",t);
        modelElement->LinkEndChild(timestepElement);

        sv4guiModelElement* me=model->GetModelElement(t);

        if(!me) continue;

        auto meElement = new TiXmlElement("model_element");
        timestepElement->LinkEndChild(meElement);
        meElement->SetAttribute("type",me->GetType());
        meElement->SetAttribute("num_sampling", me->GetNumSampling());
        meElement->SetAttribute("use_uniform", me->IfUseUniform());

        if(me->IfUseUniform())
        {
            svLoftingParam* param=me->GetLoftingParam();
            if(param)
            {
                meElement->SetAttribute("method",param->method);

                meElement->SetAttribute("sampling",param->numOutPtsInSegs);
                meElement->SetAttribute("sample_per_seg",param->samplePerSegment);
                meElement->SetAttribute("use_linear_sample",param->useLinearSampleAlongLength);
                meElement->SetAttribute("linear_multiplier",param->linearMuliplier);
                meElement->SetAttribute("use_fft",param->useFFT);
                meElement->SetAttribute("num_modes",param->numModes);

                meElement->SetAttribute("u_degree",param->uDegree);
                meElement->SetAttribute("v_degree",param->vDegree);
                meElement->SetAttribute("u_knot_type",param->uKnotSpanType);
                meElement->SetAttribute("v_knot_type",param->vKnotSpanType);
                meElement->SetAttribute("u_parametric_type",param->uParametricSpanType);
                meElement->SetAttribute("v_parametric_type",param->vParametricSpanType);
            }
        }




        auto segsElement= new TiXmlElement("segmentations");
        meElement->LinkEndChild(segsElement);

        std::vector<std::string> segNames=me->GetSegNames();
        for(int i=0;i<segNames.size();i++)
        {
            auto segElement=new TiXmlElement("seg");
            segsElement->LinkEndChild(segElement);
            segElement->SetAttribute("name", segNames[i]);
        }

        auto facesElement= new TiXmlElement("faces");
        meElement->LinkEndChild(facesElement);

        std::vector<sv4guiModelElement::svFace*> faces=me->GetFaces();
        for(int i=0;i<faces.size();i++)
        {
            if(faces[i])
            {
                auto faceElement=new TiXmlElement("face");
                facesElement->LinkEndChild(faceElement);
                faceElement->SetAttribute("id", faces[i]->id);
                faceElement->SetAttribute("name", faces[i]->name);
                faceElement->SetAttribute("type", faces[i]->type);
                faceElement->SetAttribute("visible", faces[i]->visible?"true":"false");
                faceElement->SetDoubleAttribute("opacity", faces[i]->opacity);
                faceElement->SetDoubleAttribute("color1", faces[i]->color[0]);
                faceElement->SetDoubleAttribute("color2", faces[i]->color[1]);
                faceElement->SetDoubleAttribute("color3", faces[i]->color[2]);
            }
        }

        //radii for blending
        auto blendRadiiElement= new TiXmlElement("blend_radii");
        meElement->LinkEndChild(blendRadiiElement);
        std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii=me->GetBlendRadii();
        for(int i=0;i<blendRadii.size();i++)
        {
            if(blendRadii[i])
            {
                auto radiusElement=new TiXmlElement("face_pair");
                blendRadiiElement->LinkEndChild(radiusElement);
                radiusElement->SetAttribute("face_id1", blendRadii[i]->faceID1);
                radiusElement->SetAttribute("face_id2", blendRadii[i]->faceID2);
                radiusElement->SetDoubleAttribute("radius", blendRadii[i]->radius);
            }
        }

        if(me->GetType()=="PolyData")
        {
            // for PolyData
            auto blendElement= new TiXmlElement("blend_param");
            meElement->LinkEndChild(blendElement);
            sv4guiModelElement::svBlendParam* param=me->GetBlendParam();

            blendElement->SetAttribute("blend_iters", param->numblenditers);
            blendElement->SetAttribute("sub_blend_iters", param->numsubblenditers);
            blendElement->SetAttribute("cstr_smooth_iters", param->numcgsmoothiters);
            blendElement->SetAttribute("lap_smooth_iters", param->numlapsmoothiters);
            blendElement->SetAttribute("subdivision_iters", param->numsubdivisioniters);
            blendElement->SetDoubleAttribute("decimation", param->targetdecimation);
        }

        sv4guiModelElementAnalytic* meAnalytic=dynamic_cast<sv4guiModelElementAnalytic*>(me);
        if(meAnalytic)
        {
            meElement->SetDoubleAttribute("max_dist", meAnalytic->GetMaxDist());
        }

        //Output actual model data file
        std::string fileExtension="";
        auto exts=me->GetFileExtensions();
        if(exts.size()>0)
            fileExtension=exts[0];

        if(fileExtension=="")
        {
            mitkThrow() << "No file extension available for model type: "<< me->GetType();
        }

        std::string dataFileName=fileName.substr(0,fileName.find_last_of("."));
        if(me->GetType()!="Parasolid")
            dataFileName=dataFileName+"." +fileExtension;

        if(!me->WriteFile(dataFileName))
            mitkThrow() << "Failed to write model to " << dataFileName;
    }

    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel sv4guiModelIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiModel* input = dynamic_cast<const sv4guiModel*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiModelIO* sv4guiModelIO::IOClone() const
{
    return new sv4guiModelIO(*this);
}

