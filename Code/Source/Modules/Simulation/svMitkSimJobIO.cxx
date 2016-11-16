#include "svMitkSimJobIO.h"

#include "svMitkSimJob.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <fstream>

static mitk::CustomMimeType CreatesvModelMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svjob");
    mimeType.SetCategory("Simulation");
    mimeType.AddExtension("sjb");
    mimeType.SetComment("SimVascular Job");

    return mimeType;
}

svMitkSimJobIO::svMitkSimJobIO()
    : mitk::AbstractFileIO(svMitkSimJob::GetStaticNameOfClass(), CreatesvModelMimeType(), "SimVascular Job")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> svMitkSimJobIO::Read()
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

    TiXmlElement* mjElement = document.FirstChildElement("mitk_job");

    if(!mjElement){
        //        MITK_ERROR << "No job data in "<< fileName;
        mitkThrow() << "No job data in "<< fileName;
        return result;
    }

    svMitkSimJob::Pointer mitkSimJob = svMitkSimJob::New();
    std::string modelName="";
    std::string meshName="";
    mjElement->QueryStringAttribute("model_name",&modelName);
    mjElement->QueryStringAttribute("mesh_name",&meshName);
    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetMeshName(meshName);

    TiXmlElement* jobElement = mjElement->FirstChildElement("job");
    if(jobElement != nullptr)
    {
        svSimJob* job=new svSimJob();

        TiXmlElement* bpElement = jobElement->FirstChildElement("basic_props");
        if(bpElement != nullptr)
        {
            for( TiXmlElement* element = bpElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                element->QueryStringAttribute("key", &key);
                element->QueryStringAttribute("value", &value);

                job->GetBasicProps()[key]=value;
            }
        }

        TiXmlElement* ipElement = jobElement->FirstChildElement("inlet_props");
        if(ipElement != nullptr)
        {
            for( TiXmlElement* ielement = ipElement->FirstChildElement("inlet");
                 ielement != nullptr;
                 ielement =ielement->NextSiblingElement("inlet") )
            {
                if (ielement == nullptr)
                    continue;

                std::string name="";
                ielement->QueryStringAttribute("name", &name);

                for( TiXmlElement* element = ielement->FirstChildElement("prop");
                     element != nullptr;
                     element =element->NextSiblingElement("prop") )
                {
                    if (element == nullptr)
                        continue;

                    std::string key="";
                    std::string value="";
                    element->QueryStringAttribute("key", &key);
                    element->QueryStringAttribute("value", &value);

                    job->GetInletProps()[name][key]=value;
                }

            }
        }

        TiXmlElement* opElement = jobElement->FirstChildElement("outlet_props");
        if(opElement != nullptr)
        {
            for( TiXmlElement* element = opElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                element->QueryStringAttribute("key", &key);
                element->QueryStringAttribute("value", &value);

                job->GetOutletProps()[key]=value;
            }
        }

        TiXmlElement* wpElement = jobElement->FirstChildElement("wall_props");
        if(wpElement != nullptr)
        {
            for( TiXmlElement* element = wpElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                element->QueryStringAttribute("key", &key);
                element->QueryStringAttribute("value", &value);

                job->GetWallProps()[key]=value;
            }
        }

        TiXmlElement* spElement = jobElement->FirstChildElement("solver_props");
        if(spElement != nullptr)
        {
            for( TiXmlElement* element = spElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                element->QueryStringAttribute("key", &key);
                element->QueryStringAttribute("value", &value);

                job->GetSolverProps()[key]=value;
            }
        }

        TiXmlElement* rpElement = jobElement->FirstChildElement("run_props");
        if(rpElement != nullptr)
        {
            for( TiXmlElement* element = rpElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                element->QueryStringAttribute("key", &key);
                element->QueryStringAttribute("value", &value);

                job->GetRunProps()[key]=value;
            }
        }

        mitkSimJob->SetSimJob(job);
    } //job

    result.push_back(mitkSimJob.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel svMitkSimJobIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void svMitkSimJobIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const svMitkSimJob* mitkSimJob = dynamic_cast<const svMitkSimJob*>(this->GetInput());
    if(!mitkSimJob) return;

    TiXmlDocument document;
    auto  decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
    document.LinkEndChild( decl );

    auto  version = new TiXmlElement("format");
    version->SetAttribute("version",  "1.0" );
    document.LinkEndChild(version);

    auto  mjElement = new TiXmlElement("mitk_job");
    mjElement->SetAttribute("model_name", mitkSimJob->GetModelName());
    mjElement->SetAttribute("mesh_name", mitkSimJob->GetMeshName());
    document.LinkEndChild(mjElement);

    svSimJob* job=mitkSimJob->GetSimJob();

    if(job)
    {
        auto jobElement = new TiXmlElement("job");
        mjElement->LinkEndChild(jobElement);

        auto bpElement = new TiXmlElement("basic_props");
        jobElement->LinkEndChild(bpElement);
        std::map<std::string,std::string> basicProps=job->GetBasicProps();
        std::map<std::string, std::string>::iterator it = basicProps.begin();
        while(it != basicProps.end())
        {
            auto element = new TiXmlElement("prop");
            bpElement->LinkEndChild(element);
            element->SetAttribute("key", it->first);
            element->SetAttribute("value", it->second);
            it++;
        }

        auto ipElement = new TiXmlElement("inlet_props");
        jobElement->LinkEndChild(ipElement);
        std::map<std::string, std::map<std::string, std::string>> inletProps=job->GetInletProps();
        std::map<std::string, std::map<std::string, std::string>>::iterator itit = inletProps.begin();
        while(itit != inletProps.end())
        {
            auto ielement = new TiXmlElement("inlet");
            ipElement->LinkEndChild(ielement);

            ielement->SetAttribute("name", itit->first);

            std::map<std::string, std::string> props=itit->second;

            it = props.begin();
            while(it != props.end())
            {
                auto element = new TiXmlElement("prop");
                ielement->LinkEndChild(element);
                element->SetAttribute("key", it->first);
                element->SetAttribute("value", it->second);
                it++;
            }

            itit++;
        }

        auto opElement = new TiXmlElement("outlet_props");
        jobElement->LinkEndChild(opElement);
        std::map<std::string,std::string> outletProps=job->GetOutletProps();
        it = outletProps.begin();
        while(it != outletProps.end())
        {
            auto element = new TiXmlElement("prop");
            opElement->LinkEndChild(element);
            element->SetAttribute("key", it->first);
            element->SetAttribute("value", it->second);
            it++;
        }

        auto wpElement = new TiXmlElement("wall_props");
        jobElement->LinkEndChild(wpElement);
        std::map<std::string,std::string> wallProps=job->GetWallProps();
        it = wallProps.begin();
        while(it != wallProps.end())
        {
            auto element = new TiXmlElement("prop");
            wpElement->LinkEndChild(element);
            element->SetAttribute("key", it->first);
            element->SetAttribute("value", it->second);
            it++;
        }

        auto spElement = new TiXmlElement("solver_props");
        jobElement->LinkEndChild(spElement);
        std::map<std::string,std::string> solverProps=job->GetSolverProps();
        it = solverProps.begin();
        while(it != solverProps.end())
        {
            auto element = new TiXmlElement("prop");
            spElement->LinkEndChild(element);
            element->SetAttribute("key", it->first);
            element->SetAttribute("value", it->second);
            it++;
        }

        auto rpElement = new TiXmlElement("run_props");
        jobElement->LinkEndChild(rpElement);
        std::map<std::string,std::string> runProps=job->GetRunProps();
        it = runProps.begin();
        while(it != runProps.end())
        {
            auto element = new TiXmlElement("prop");
            rpElement->LinkEndChild(element);
            element->SetAttribute("key", it->first);
            element->SetAttribute("value", it->second);
            it++;
        }

    }

    if (document.SaveFile(fileName) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;
    }
}

mitk::IFileIO::ConfidenceLevel svMitkSimJobIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const svMitkSimJob* input = dynamic_cast<const svMitkSimJob*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

svMitkSimJobIO* svMitkSimJobIO::IOClone() const
{
    return new svMitkSimJobIO(*this);
}

