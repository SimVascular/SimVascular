#include "svModelIO.h"
#include "svModel.h"
#include "svModelElementPolyData.h"

//#include "cv_polydatasolid_utils.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkErrorCode.h>

static mitk::CustomMimeType CreatesvModelMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svmodel");
    mimeType.SetCategory("Model");
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
    TiXmlDocument document;

    std::string fileName=GetInputLocation();

    if (!document.LoadFile(fileName))
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        std::vector<mitk::BaseData::Pointer> empty;
        return empty;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    TiXmlElement* modelElement = document.FirstChildElement("model");

    if(!modelElement){
        //        MITK_ERROR << "No Model data in "<< fileName;
        mitkThrow() << "No Model data in "<< fileName;
    }

    svModel::Pointer model = svModel::New();

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
            std::string type;
            meElement->QueryStringAttribute("type", &type);

            svModelElement* me;

            if(type=="PolyData")
            {
                me=new svModelElementPolyData();

                svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(me);

                std::string dataFileName=fileName.substr(0,fileName.find_last_of("."))+".vtp";
                vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();

                reader->SetFileName(dataFileName.c_str());
                reader->Update();
                vtkSmartPointer<vtkPolyData> pd=reader->GetOutput();
//                vtkSmartPointer<vtkPolyData> vpdModel=NULL;
//                if(pd!=NULL)
//                {
//                    vpdModel=vtkSmartPointer<vtkPolyData>::New();
//                    vpdModel->DeepCopy(pd);
//                }
                mepd->SetWholeVtkPolyData(pd);

            }
//            else if(type=="ParaSolid")
//            {
//                me=new svModelElementParaSolid();
//            }

            TiXmlElement* facesElement = meElement->FirstChildElement("faces");
            std::vector<svModelElement::svFace*> faces;
            for( TiXmlElement* faceElement = facesElement->FirstChildElement("face");
                 faceElement != nullptr;
                 faceElement =faceElement->NextSiblingElement("face") )
            {
                if (faceElement == nullptr)
                    continue;

                int id;
                std::string name;

                faceElement->QueryIntAttribute("id", &id);
                faceElement->QueryStringAttribute("name", &name);
                //                    vtkPolyData *facepd = vtkPolyData::New();
                //                    PlyDtaUtils_GetFacePolyData(me->GetWholeVtkPolyDatal(), &id, facepd);
                vtkSmartPointer<vtkPolyData> facepd=me->CreateFaceVtkPolyData(id);
                svModelElement::svFace* face=new svModelElement::svFace;
                face->id=id;
                face->name=name;
                face->vpd=facepd;

                faces.push_back(face);
            }
            me->SetFaces(faces);

            TiXmlElement* segsElement = meElement->FirstChildElement("segmentations");
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


            //            if(type=="PolyData")
            //            {
            //                TiXmlElement* facesElement = meElement->FirstChildElement("faces");
            //                std::vector<svModelElement::svFace*> faces;
            //                for( TiXmlElement* faceElement = facesElement->FirstChildElement("face");
            //                     faceElement != nullptr;
            //                     faceElement =faceElement->NextSiblingElement("face") )
            //                {
            //                    if (faceElement == nullptr)
            //                        continue;

            //                    int id;
            //                    std::string name;

            //                    faceElement->QueryIntAttribute("id", &id);
            //                    faceElement->QueryStringAttribute("name", &name);
            //                    vtkPolyData *facepd = vtkPolyData::New();
            //                    PlyDtaUtils_GetFacePolyData(me->GetSolidModel(), &id, facepd);
            //                    svModelElement::svFace* face=new svModelElement::svFace;
            //                    face->id=id;
            //                    face->name=name;
            //                    face->vpd=facepd;

            //                    faces.push_back(face);
            //                }
            //                me->SetFaces(faces);
            //            }

            model->SetModelElement(me,timestep);
        } //model element

    }//timestep

    std::vector<mitk::BaseData::Pointer> result;
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
            auto faceElement=new TiXmlElement("face");
            facesElement->LinkEndChild(faceElement);
            faceElement->SetAttribute("id", faces[i]->id);
            faceElement->SetAttribute("name", faces[i]->name);
        }

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
                    mitkThrow() << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
                }
            }
        }

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

