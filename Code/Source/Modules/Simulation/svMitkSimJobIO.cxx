#include "svMitkSimJobIO.h"

#include "svMitkSimJob.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml.h>

#include <fstream>

static mitk::CustomMimeType CreatesvSimJobMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svjob");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("sjb");
    mimeType.SetComment("SimVascular Job");

    return mimeType;
}

svMitkSimJobIO::svMitkSimJobIO()
    : mitk::AbstractFileIO(svMitkSimJob::GetStaticNameOfClass(), CreatesvSimJobMimeType(), "SimVascular Job")
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
    std::string status="";
    mjElement->QueryStringAttribute("model_name",&modelName);
    mjElement->QueryStringAttribute("mesh_name",&meshName);
    mjElement->QueryStringAttribute("status",&status);
    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetMeshName(meshName);
    mitkSimJob->SetStatus(status);

    TiXmlElement* jobElement = mjElement->FirstChildElement("job");
    if(jobElement != nullptr)
    {
        svSimJob* job=new svSimJob();

        TiXmlElement* bpElement = jobElement->FirstChildElement("basic_props");
        if(bpElement != nullptr)
        {
            std::map<std::string,std::string> basicProps;
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

                basicProps[key]=value;
            }
            job->SetBasicProps(basicProps);
        }

        TiXmlElement* cpElement = jobElement->FirstChildElement("cap_props");
        if(cpElement != nullptr)
        {
            std::map<std::string,std::map<std::string,std::string> > capProps;
            for( TiXmlElement* celement = cpElement->FirstChildElement("cap");
                 celement != nullptr;
                 celement =celement->NextSiblingElement("cap") )
            {
                if (celement == nullptr)
                    continue;

                std::string name="";
                celement->QueryStringAttribute("name", &name);

                for( TiXmlElement* element = celement->FirstChildElement("prop");
                     element != nullptr;
                     element =element->NextSiblingElement("prop") )
                {
                    if (element == nullptr)
                        continue;

                    std::string key="";
                    std::string value="";
                    element->QueryStringAttribute("key", &key);
                    element->QueryStringAttribute("value", &value);

                    capProps[name][key]=value;
                }

            }
            job->SetCapProps(capProps);
        }

        TiXmlElement* wpElement = jobElement->FirstChildElement("wall_props");
        if(wpElement != nullptr)
        {
            std::map<std::string,std::string> wallProps;
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

                wallProps[key]=value;
            }
            job->SetWallProps(wallProps);
        }

        TiXmlElement* vpElement = jobElement->FirstChildElement("var_props");
        if(vpElement != nullptr)
        {
            std::map<std::string,std::map<std::string,std::string> > varProps;
            for( TiXmlElement* felement = vpElement->FirstChildElement("face");
                 felement != nullptr;
                 felement =felement->NextSiblingElement("face") )
            {
                if (felement == nullptr)
                    continue;

                std::string name="";
                felement->QueryStringAttribute("name", &name);

                for( TiXmlElement* element = felement->FirstChildElement("prop");
                     element != nullptr;
                     element =element->NextSiblingElement("prop") )
                {
                    if (element == nullptr)
                        continue;

                    std::string key="";
                    std::string value="";
                    element->QueryStringAttribute("key", &key);
                    element->QueryStringAttribute("value", &value);

                    varProps[name][key]=value;
                }

            }
            job->SetVarProps(varProps);
        }

        TiXmlElement* spElement = jobElement->FirstChildElement("solver_props");
        if(spElement != nullptr)
        {
            std::map<std::string,std::string> solverProps;
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

                solverProps[key]=value;
            }
            job->SetSolverProps(solverProps);
        }

        TiXmlElement* rpElement = jobElement->FirstChildElement("run_props");
        if(rpElement != nullptr)
        {
            std::map<std::string,std::string> runProps;
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

                runProps[key]=value;
            }
            job->SetRunProps(runProps);
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
    mjElement->SetAttribute("status", mitkSimJob->GetStatus());
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

        auto cpElement = new TiXmlElement("cap_props");
        jobElement->LinkEndChild(cpElement);
        std::map<std::string, std::map<std::string, std::string> > capProps=job->GetCapProps();
        auto itit = capProps.begin();
        while(itit != capProps.end())
        {
            auto celement = new TiXmlElement("cap");
            cpElement->LinkEndChild(celement);

            celement->SetAttribute("name", itit->first);

            std::map<std::string, std::string> props=itit->second;

            it = props.begin();
            while(it != props.end())
            {
                auto element = new TiXmlElement("prop");
                celement->LinkEndChild(element);
                element->SetAttribute("key", it->first);
                element->SetAttribute("value", it->second);
                it++;
            }

            itit++;
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

        auto vpElement = new TiXmlElement("var_props");
        jobElement->LinkEndChild(vpElement);
        std::map<std::string, std::map<std::string, std::string> > varProps=job->GetVarProps();
        itit = varProps.begin();
        while(itit != varProps.end())
        {
            auto felement = new TiXmlElement("face");
            vpElement->LinkEndChild(felement);

            felement->SetAttribute("name", itit->first);

            std::map<std::string, std::string> props=itit->second;

            it = props.begin();
            while(it != props.end())
            {
                auto element = new TiXmlElement("prop");
                felement->LinkEndChild(element);
                element->SetAttribute("key", it->first);
                element->SetAttribute("value", it->second);
                it++;
            }

            itit++;
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

