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

#include "sv4gui_MitkSimJobIO.h"

#include "sv4gui_MitkSimJob.h"

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <tinyxml2.h>

#include "sv4gui_ContourGroupIO.h"
auto set_string_from_attribute = &sv4guiContourGroupIO::set_string_from_attribute;

#include <fstream>

static mitk::CustomMimeType Createsv4guiSimJobMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".svjob");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("sjb");
    mimeType.SetComment("SimVascular Job");

    return mimeType;
}

sv4guiMitkSimJobIO::sv4guiMitkSimJobIO()
    : mitk::AbstractFileIO(sv4guiMitkSimJob::GetStaticNameOfClass(), Createsv4guiSimJobMimeType(), "SimVascular Job")
{
    this->RegisterService();
}

std::vector<mitk::BaseData::Pointer> sv4guiMitkSimJobIO::Read()
{
    std::vector<mitk::BaseData::Pointer> result;

    tinyxml2::XMLDocument document;

    std::string fileName=GetInputLocation();

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS)
    {
        mitkThrow() << "Could not open/read/parse " << fileName;
        //        MITK_ERROR << "Could not open/read/parse " << fileName;
        return result;
    }

    //    TiXmlElement* version = document.FirstChildElement("format");

    auto mjElement = document.FirstChildElement("mitk_job");

    if(!mjElement){
        //        MITK_ERROR << "No job data in "<< fileName;
        mitkThrow() << "No job data in "<< fileName;
        return result;
    }

    sv4guiMitkSimJob::Pointer mitkSimJob = sv4guiMitkSimJob::New();
    std::string modelName="";
    std::string meshName="";
    std::string status="";
    set_string_from_attribute(mjElement,"model_name",modelName);
    set_string_from_attribute(mjElement,"mesh_name",meshName);
    set_string_from_attribute(mjElement,"status",status);
    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetMeshName(meshName);
    mitkSimJob->SetStatus(status);

    auto jobElement = mjElement->FirstChildElement("job");
    if(jobElement != nullptr)
    {
        sv4guiSimJob* job=new sv4guiSimJob();

        auto bpElement = jobElement->FirstChildElement("basic_props");
        if(bpElement != nullptr)
        {
            std::map<std::string,std::string> basicProps;
            for( auto element = bpElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                set_string_from_attribute(element, "key", key);
                set_string_from_attribute(element, "value", value);

                basicProps[key]=value;
            }
            job->SetBasicProps(basicProps);
        }

        auto cpElement = jobElement->FirstChildElement("cap_props");
        if(cpElement != nullptr)
        {
            std::map<std::string,std::map<std::string,std::string> > capProps;
            for( auto celement = cpElement->FirstChildElement("cap");
                 celement != nullptr;
                 celement =celement->NextSiblingElement("cap") )
            {
                if (celement == nullptr)
                    continue;

                std::string name="";
                set_string_from_attribute(celement, "name", name);

                for( auto element = celement->FirstChildElement("prop");
                     element != nullptr;
                     element =element->NextSiblingElement("prop") )
                {
                    if (element == nullptr)
                        continue;

                    std::string key="";
                    std::string value="";
                    set_string_from_attribute(element, "key", key);
                    set_string_from_attribute(element, "value", value);

                    capProps[name][key]=value;
                }

            }
            job->SetCapProps(capProps);
        }

        auto wpElement = jobElement->FirstChildElement("wall_props");
        if(wpElement != nullptr)
        {
            std::map<std::string,std::string> wallProps;
            for( auto element = wpElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                set_string_from_attribute(element, "key", key);
                set_string_from_attribute(element, "value", value);

                wallProps[key]=value;
            }
            job->SetWallProps(wallProps);
        }

        auto vpElement = jobElement->FirstChildElement("var_props");
        if(vpElement != nullptr)
        {
            std::map<std::string,std::map<std::string,std::string> > varProps;
            for( auto felement = vpElement->FirstChildElement("face");
                 felement != nullptr;
                 felement =felement->NextSiblingElement("face") )
            {
                if (felement == nullptr)
                    continue;

                std::string name="";
                set_string_from_attribute(felement, "name", name);

                for( auto element = felement->FirstChildElement("prop");
                     element != nullptr;
                     element =element->NextSiblingElement("prop") )
                {
                    if (element == nullptr)
                        continue;

                    std::string key="";
                    std::string value="";
                    set_string_from_attribute(element, "key", key);
                    set_string_from_attribute(element, "value", value);

                    varProps[name][key]=value;
                }

            }
            job->SetVarProps(varProps);
        }

        auto spElement = jobElement->FirstChildElement("solver_props");
        if(spElement != nullptr)
        {
            std::map<std::string,std::string> solverProps;
            for( auto element = spElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                set_string_from_attribute(element, "key", key);
                set_string_from_attribute(element, "value", value);

                solverProps[key]=value;
            }
            job->SetSolverProps(solverProps);
        }

        auto rpElement = jobElement->FirstChildElement("run_props");
        if(rpElement != nullptr)
        {
            std::map<std::string,std::string> runProps;
            for( auto element = rpElement->FirstChildElement("prop");
                 element != nullptr;
                 element =element->NextSiblingElement("prop") )
            {
                if (element == nullptr)
                    continue;

                std::string key="";
                std::string value="";
                set_string_from_attribute(element, "key", key);
                set_string_from_attribute(element, "value", value);

                runProps[key]=value;
            }
            job->SetRunProps(runProps);
        }

        mitkSimJob->SetSimJob(job);
    } //job

    result.push_back(mitkSimJob.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkSimJobIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

void sv4guiMitkSimJobIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const sv4guiMitkSimJob* mitkSimJob = dynamic_cast<const sv4guiMitkSimJob*>(this->GetInput());
    if(!mitkSimJob) return;

    tinyxml2::XMLDocument document;
    auto  decl = document.NewDeclaration();
    document.LinkEndChild( decl );

    auto mjElement = document.NewElement("mitk_job");
    mjElement->SetAttribute("model_name", mitkSimJob->GetModelName().c_str());
    mjElement->SetAttribute("mesh_name", mitkSimJob->GetMeshName().c_str());
    mjElement->SetAttribute("status", mitkSimJob->GetStatus().c_str());
    mjElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(mjElement);

    sv4guiSimJob* job=mitkSimJob->GetSimJob();

    if(job)
    {
        auto jobElement = document.NewElement("job");
        mjElement->LinkEndChild(jobElement);

        auto bpElement = document.NewElement("basic_props");
        jobElement->LinkEndChild(bpElement);
        std::map<std::string,std::string> basicProps=job->GetBasicProps();
        std::map<std::string, std::string>::iterator it = basicProps.begin();
        while(it != basicProps.end())
        {
            auto element = document.NewElement("prop");
            bpElement->LinkEndChild(element);
            element->SetAttribute("key", it->first.c_str());
            element->SetAttribute("value", it->second.c_str());
            it++;
        }

        auto cpElement = document.NewElement("cap_props");
        jobElement->LinkEndChild(cpElement);
        std::map<std::string, std::map<std::string, std::string> > capProps=job->GetCapProps();
        auto itit = capProps.begin();
        while(itit != capProps.end())
        {
            auto celement = document.NewElement("cap");
            cpElement->LinkEndChild(celement);

            celement->SetAttribute("name", itit->first.c_str());
            std::map<std::string, std::string> props=itit->second;

            it = props.begin();
            while(it != props.end())
            {
                auto element = document.NewElement("prop");
                celement->LinkEndChild(element);
                element->SetAttribute("key", it->first.c_str());
                element->SetAttribute("value", it->second.c_str());
                it++;
            }

            itit++;
        }

        auto wpElement = document.NewElement("wall_props");
        jobElement->LinkEndChild(wpElement);
        std::map<std::string,std::string> wallProps=job->GetWallProps();
        it = wallProps.begin();
        while(it != wallProps.end())
        {
            auto element = document.NewElement("prop");
            wpElement->LinkEndChild(element);
            element->SetAttribute("key", it->first.c_str());
            element->SetAttribute("value", it->second.c_str());
            it++;
        }

        auto vpElement = document.NewElement("var_props");
        jobElement->LinkEndChild(vpElement);
        std::map<std::string, std::map<std::string, std::string> > varProps=job->GetVarProps();
        itit = varProps.begin();
        while(itit != varProps.end())
        {
            auto felement = document.NewElement("face");
            vpElement->LinkEndChild(felement);

            felement->SetAttribute("name", itit->first.c_str());

            std::map<std::string, std::string> props=itit->second;

            it = props.begin();
            while(it != props.end())
            {
                auto element = document.NewElement("prop");
                felement->LinkEndChild(element);
                element->SetAttribute("key", it->first.c_str());
                element->SetAttribute("value", it->second.c_str());
                it++;
            }

            itit++;
        }

        auto spElement = document.NewElement("solver_props");
        jobElement->LinkEndChild(spElement);
        std::map<std::string,std::string> solverProps=job->GetSolverProps();
        it = solverProps.begin();
        while(it != solverProps.end())
        {
            auto element = document.NewElement("prop");
            spElement->LinkEndChild(element);
            element->SetAttribute("key", it->first.c_str());
            element->SetAttribute("value", it->second.c_str());
            it++;
        }

        auto rpElement = document.NewElement("run_props");
        jobElement->LinkEndChild(rpElement);
        std::map<std::string,std::string> runProps=job->GetRunProps();
        it = runProps.begin();
        while(it != runProps.end())
        {
            auto element = document.NewElement("prop");
            rpElement->LinkEndChild(element);
            element->SetAttribute("key", it->first.c_str());
            element->SetAttribute("value", it->second.c_str());
            it++;
        }

    }

    if (document.SaveFile(fileName.c_str()) == false)
    {
        mitkThrow() << "Could not write model to " << fileName;
    }
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkSimJobIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiMitkSimJob* input = dynamic_cast<const sv4guiMitkSimJob*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiMitkSimJobIO* sv4guiMitkSimJobIO::IOClone() const
{
    return new sv4guiMitkSimJobIO(*this);
}

