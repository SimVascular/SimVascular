#include "svModelIO.h"
#include "svModel.h"
#include "svModelElementPolyData.h"

#include "simvascular_options.h"

#ifdef SV_USE_OpenCASCADE
#include "svModelElementOCCT.h"
#endif

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>

static mitk::CustomMimeType CreatesvModelMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svmodel");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("mdl");
    mimeType.SetComment("SimVascular Model");

    return mimeType;
}

svModelIO::svModelIO()
    : mitk::AbstractFileIO(svModel::GetStaticNameOfClass(), CreatesvModelMimeType(), "SimVascular Model")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> svModelIO::Read()
{
    std::vector<mitk::BaseData::Pointer> result;

    TiXmlDocument document;

    std::string fileName=GetInputLocation();

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        return result;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* modelElement = document.FirstChildElement("model");

    if(!modelElement){
        //        MITK_ERROR << "No Model data in "<< fileName;
        mitkThrow() << "No Model data in "<< fileName;
        return result;
    }

    svModel::Pointer model = svModel::New();
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
                return result;
            }

            svModelElement* me=NULL;

            if(type=="PolyData")
            {
                me=new svModelElementPolyData();

                svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(me);

                std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
                vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();

                reader->SetFileName(dataFileName.c_str());
                reader->Update();
                vtkSmartPointer<vtkPolyData> pd=reader->GetOutput();

                mepd->SetWholeVtkPolyData(pd);

            }

#ifdef SV_USE_OpenCASCADE
            if(type=="OpenCASCADE")
            {
                me=new svModelElementOCCT();
                svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(me);
                std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".brep";
                cvOCCTSolidModel* occtSolid=new cvOCCTSolidModel();
                char* df=const_cast<char*>(dataFileName.c_str());
                occtSolid->ReadNative(df);
                meocct->SetOCCTSolid(occtSolid);

                double maxDist=0;
                if(meElement->QueryDoubleAttribute("max_dist", &maxDist)==TIXML_SUCCESS)
                {
                    meocct->SetMaxDist(maxDist);
                }

                meocct->SetWholeVtkPolyData(meocct->CreateWholeVtkPolyData());
            }
#endif

            if(me==NULL)
            {
                mitkThrow() << "No support in reading file of "<< type;
                return result;
            }

            int numSampling=0;
            meElement->QueryIntAttribute("num_sampling", &numSampling);
            me->SetNumSampling(numSampling);

            TiXmlElement* facesElement = meElement->FirstChildElement("faces");
            if(facesElement!=nullptr)
            {
                std::vector<svModelElement::svFace*> faces;
                for( TiXmlElement* faceElement = facesElement->FirstChildElement("face");
                     faceElement != nullptr;
                     faceElement =faceElement->NextSiblingElement("face") )
                {
                    if (faceElement == nullptr)
                        continue;

                    int id;
                    std::string name="";
                    std::string type="";
                    std::string isVisible="true";
                    float opacity=1.0f;
                    float color1=1.0f;
                    float color2=1.0f;
                    float color3=1.0f;

                    faceElement->QueryIntAttribute("id", &id);
                    faceElement->QueryStringAttribute("name", &name);
                    faceElement->QueryStringAttribute("type", &type);
                    faceElement->QueryStringAttribute("visible", &isVisible);
                    faceElement->QueryFloatAttribute("opacity", &opacity);
                    faceElement->QueryFloatAttribute("color1", &color1);
                    faceElement->QueryFloatAttribute("color2", &color2);
                    faceElement->QueryFloatAttribute("color3", &color3);

                    vtkSmartPointer<vtkPolyData> facepd=me->CreateFaceVtkPolyData(id);
                    svModelElement::svFace* face=new svModelElement::svFace;
                    face->id=id;
                    face->name=name;
                    face->type=type;
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
                std::vector<svModelElement::svBlendParamRadius*> blendRadii;
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

//                    blendRadii.push_back(new svModelElement::svBlendParamRadius(faceID1,faceID2,radius));
                    blendRadii.push_back(new svModelElement::svBlendParamRadius(faceID1,faceID2,me->GetFaceName(faceID1),me->GetFaceName(faceID2),radius));

                }
                me->SetBlendRadii(blendRadii);
            }

            if(type=="PolyData")
            {
                svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(me);
                if(mepd)
                {
                    TiXmlElement* blendElement = meElement->FirstChildElement("blend_param");
                    if(blendElement!=nullptr)
                    {
                        svModelElementPolyData::svBlendParam* param=mepd->GetBlendParam();
                        blendElement->QueryIntAttribute("blend_iters", &(param->numblenditers));
                        blendElement->QueryIntAttribute("sub_blend_iter", &(param->numsubblenditers));
                        blendElement->QueryIntAttribute("cstr_smooth_iter", &(param->numcgsmoothiters));
                        blendElement->QueryIntAttribute("lap_smooth_iter", &(param->numlapsmoothiters));
                        blendElement->QueryIntAttribute("subdivision_iters", &(param->numsubdivisioniters));
                        blendElement->QueryDoubleAttribute("decimation", &(param->targetdecimation));
                    }
                }
            }

            model->SetModelElement(me,timestep);
        } //model element

    }//timestep

    result.push_back(model.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel svModelIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void svModelIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const svModel* model = dynamic_cast<const svModel*>(this->GetInput());
    if(!model) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  version = new TiXmlElement("format");
    version->SetAttribute("version",  "1.0" );
    document.LinkEndChild(version);

    auto  modelElement = new TiXmlElement("model");
    modelElement->SetAttribute("type", model->GetType());
    document.LinkEndChild(modelElement);

    for(int t=0;t<model->GetTimeSize();t++)
    {
        auto  timestepElement = new TiXmlElement("timestep");
        timestepElement->SetAttribute("id",t);
        modelElement->LinkEndChild(timestepElement);

        svModelElement* me=model->GetModelElement(t);

        if(!me) continue;

        auto meElement = new TiXmlElement("model_element");
        timestepElement->LinkEndChild(meElement);
        meElement->SetAttribute("type",me->GetType());
        meElement->SetAttribute("num_sampling", me->GetNumSampling());

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

        std::vector<svModelElement::svFace*> faces=me->GetFaces();
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
        std::vector<svModelElement::svBlendParamRadius*> blendRadii=me->GetBlendRadii();
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
            svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(me);
            if(mepd)
            {
                auto blendElement= new TiXmlElement("blend_param");
                meElement->LinkEndChild(blendElement);
                svModelElementPolyData::svBlendParam* param=mepd->GetBlendParam();

                blendElement->SetAttribute("blend_iters", param->numblenditers);
                blendElement->SetAttribute("sub_blend_iters", param->numsubblenditers);
                blendElement->SetAttribute("cstr_smooth_iters", param->numcgsmoothiters);
                blendElement->SetAttribute("lap_smooth_iters", param->numlapsmoothiters);
                blendElement->SetAttribute("subdivision_iters", param->numsubdivisioniters);
                blendElement->SetDoubleAttribute("decimation", param->targetdecimation);
            }
        }

#ifdef SV_USE_OpenCASCADE
        if(me->GetType()=="OpenCASCADE")
        {
            //for OpenCASCADE
            svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(me);
            if(meocct)
            {
                meElement->SetDoubleAttribute("max_dist", meocct->GetMaxDist());
            }
        }
#endif

        //Output actual model data file
        if(me->GetType()=="PolyData")
        {
            std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";

            svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(me);

            if(mepd&&mepd->GetWholeVtkPolyData())
            {
                vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
                writer->SetFileName(dataFileName.c_str());
                writer->SetInputData(mepd->GetWholeVtkPolyData());
                if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
                {
                    mitkThrow() << "PolyData model writing error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
                }
            }
        }

#ifdef SV_USE_OpenCASCADE
        if(me->GetType()=="OpenCASCADE")
        {
            std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".brep";

            svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(me);

            if(meocct&&meocct->GetOCCTSolid())
            {
               char* df=const_cast<char*>(dataFileName.c_str());
                if (meocct->GetOCCTSolid()->WriteNative(0,df) != CV_OK )
                {
                    mitkThrow() << "OpenCASCADE model writing error: ";
                }
            }
        }
#endif

    }

    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;

    }
}

mitk::IFileIO::ConfidenceLevel svModelIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const svModel* input = dynamic_cast<const svModel*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

svModelIO* svModelIO::IOClone() const
{
    return new svModelIO(*this);
}

