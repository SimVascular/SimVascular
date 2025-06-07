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

#include "sv4gui_MitkROMSimJobIO.h"

#include "sv4gui_MitkROMSimJob.h"

#include "sv4gui_ContourGroupIO.h"
auto set_string_from_attribute = &sv4guiContourGroupIO::set_string_from_attribute;

#include <mitkCustomMimeType.h>
#include <mitkIOMimeTypes.h>

#include <fstream>

static mitk::CustomMimeType Createsv4guiROMSimJobMimeType()
{
    mitk::CustomMimeType mimeType(mitk::IOMimeTypes::DEFAULT_BASE_NAME() + ".sv1djob");
    mimeType.SetCategory("SimVascular Files");
    mimeType.AddExtension("romsimjob");
    mimeType.SetComment("SimVascular Reduced-Order Job");

    return mimeType;
}

sv4guiMitkROMSimJobIO::sv4guiMitkROMSimJobIO()
    : mitk::AbstractFileIO(sv4guiMitkROMSimJob::GetStaticNameOfClass(), Createsv4guiROMSimJobMimeType(), "SimVascular Reduced-Order Job")
{
    this->RegisterService();
}

//------
// Read
//------
// Read in a job .romsimjob xml file.
//
std::vector<mitk::BaseData::Pointer> sv4guiMitkROMSimJobIO::Read()
{
    std::vector<mitk::BaseData::Pointer> result;

    tinyxml2::XMLDocument document;
    std::string fileName=GetInputLocation();

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS) {
        mitkThrow() << "Could not open/read/parse " << fileName;
        return result;
    }

    auto mjElement = document.FirstChildElement("mitk_job");

    if(!mjElement){
        mitkThrow() << "No job data in "<< fileName;
        return result;
    }

    sv4guiMitkROMSimJob::Pointer mitkSimJob = sv4guiMitkROMSimJob::New();
    std::string modelName = "";
    std::string modelOrder = "";
    std::string meshName = "";
    std::string status = "";

    set_string_from_attribute(mjElement, "model_name",modelName);
    set_string_from_attribute(mjElement, "model_order", modelOrder);
    set_string_from_attribute(mjElement, "mesh_name",meshName);
    set_string_from_attribute(mjElement, "status",status);

    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetModelOrder(modelOrder);
    mitkSimJob->SetMeshName(meshName);
    mitkSimJob->SetStatus(status);

    auto jobElement = mjElement->FirstChildElement("job");

    if(jobElement != nullptr) {
        sv4guiROMSimJob* job = new sv4guiROMSimJob();

        auto basicProps = GetProps(jobElement, "basic_props");
        job->SetBasicProps(basicProps);

        auto convProps = GetProps(jobElement, "convert_results_props");
        job->SetConvertResultsProps(convProps);

        auto meshProps = GetProps(jobElement, "mesh_props");
        job->SetMeshProps(meshProps);

        auto modelProps = GetProps(jobElement, "model_props");
        job->SetModelProps(modelProps);

        auto capProps = GetMapProps(jobElement, "cap_props", "cap");
        job->SetCapProps(capProps);

        auto wallProps = GetProps(jobElement, "wall_props");
        job->SetWallProps(wallProps);

        auto varProps = GetMapProps(jobElement, "var_props", "face");
        job->SetVarProps(varProps);

        auto solverProps = GetProps(jobElement, "solver_props");
        job->SetSolverProps(solverProps);

        auto runProps = GetProps(jobElement, "run_props");
        job->SetRunProps(runProps);

        mitkSimJob->SetSimJob(job);
    } //job

    result.push_back(mitkSimJob.GetPointer());
    return result;
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkROMSimJobIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

//----------
// GetProps
//----------
// Get job properties from an xml element.
//
std::map<std::string,std::string> sv4guiMitkROMSimJobIO::GetProps(tinyxml2::XMLElement* jobElement, const std::string& propName)
{
    std::map<std::string,std::string> props;
    auto * element = jobElement->FirstChildElement(propName.c_str());
    if(element == nullptr) {
        return props;
    }

    element = element->FirstChildElement("prop");

    while (element != nullptr) {
        std::string key="";
        std::string value="";
        set_string_from_attribute(element, "key", key);
        set_string_from_attribute(element, "value", value);
        props[key]=value;
        element = element->NextSiblingElement("prop");
    }

    return props;
}

//-------------
// GetMapProps
//-------------
// Get job properties from an xml element.
//
// The properties here are a two-level hierarchy of elements.
//
std::map<std::string,std::map<std::string,std::string> > 
sv4guiMitkROMSimJobIO::GetMapProps(tinyxml2::XMLElement* jobElement, const std::string& propName1, const std::string& propName2)
{
  std::map<std::string,std::map<std::string,std::string> > props;
  auto element = jobElement->FirstChildElement(propName1.c_str());

  for( auto element1 = element->FirstChildElement(propName2.c_str()); element1 != nullptr; 
    element1 = element1->NextSiblingElement(propName2.c_str()) ) {
      if (element1 == nullptr) {
          continue;
      }

      std::string name="";
      set_string_from_attribute(element1, "name", name);

      for( auto element2 = element1->FirstChildElement("prop"); element2 != nullptr;
        element2 =element2->NextSiblingElement("prop") ) {
          if (element2 == nullptr) {
              continue;
          }

          std::string key="";
          std::string value="";
          set_string_from_attribute(element2, "key", key);
          set_string_from_attribute(element2, "value", value);
          props[name][key]=value;
      }
  }

  return props;
}

//-------
// Write
//-------
// Write a job .romsjob xml file.
//
void sv4guiMitkROMSimJobIO::Write()
{
    ValidateOutputLocation();

    std::string fileName=GetOutputLocation();

    const sv4guiMitkROMSimJob* mitkSimJob = dynamic_cast<const sv4guiMitkROMSimJob*>(this->GetInput());
    if(!mitkSimJob) return;

    tinyxml2::XMLDocument document;
    auto decl = document.NewDeclaration();
    document.LinkEndChild( decl );

    auto mjElement = document.NewElement("mitk_job");
    mjElement->SetAttribute("model_name", mitkSimJob->GetModelName().c_str());
    mjElement->SetAttribute("model_order", mitkSimJob->GetModelOrder().c_str());
    mjElement->SetAttribute("mesh_name", mitkSimJob->GetMeshName().c_str());
    mjElement->SetAttribute("status", mitkSimJob->GetStatus().c_str());
    mjElement->SetAttribute("version",  "1.0" );
    document.LinkEndChild(mjElement);

    sv4guiROMSimJob* job=mitkSimJob->GetSimJob();

    if (job) {
        auto jobElement = document.NewElement("job");
        mjElement->LinkEndChild(jobElement);

        auto modelElement = document.NewElement("model_props");
        jobElement->LinkEndChild(modelElement);
        std::map<std::string,std::string> modelProps = job->GetModelProps();
        for (auto const& prop : modelProps) {
            auto element = document.NewElement("prop");
            modelElement->LinkEndChild(element);
            element->SetAttribute("key", prop.first.c_str());
            element->SetAttribute("value", prop.second.c_str());
        }
        //std::map<std::string, std::string>::iterator it = basicProps.begin();

        //while(it != basicProps.end()) {
            //auto element = document.NewElement("prop");
            //modelElement->LinkEndChild(element);
            //element->SetAttribute("key", it->first);
            //element->SetAttribute("value", it->second);
            //it++;
        //}

        auto bpElement = document.NewElement("basic_props");
        jobElement->LinkEndChild(bpElement);
        std::map<std::string,std::string> basicProps=job->GetBasicProps();
        std::map<std::string, std::string>::iterator it = basicProps.begin();

        while (it != basicProps.end()) {
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
        while(itit != capProps.end()) {
            auto celement = document.NewElement("cap");
            cpElement->LinkEndChild(celement);
            celement->SetAttribute("name", itit->first.c_str());
            std::map<std::string, std::string> props=itit->second;
            it = props.begin();
            while(it != props.end()) {
                auto element = document.NewElement("prop");
                celement->LinkEndChild(element);
                element->SetAttribute("key", it->first.c_str());
                element->SetAttribute("value", it->second.c_str());
                it++;
            }
            itit++;
        }

        auto convElement = document.NewElement("convert_results_props");
        jobElement->LinkEndChild(convElement);
        std::map<std::string,std::string> convProps = job->GetConvertResultsProps();
        it = convProps.begin();
        while (it != convProps.end()) {
            auto element = document.NewElement("prop");
            convElement->LinkEndChild(element);
            element->SetAttribute("key", it->first.c_str());
            element->SetAttribute("value", it->second.c_str());
            it++;
        }

        auto msElement = document.NewElement("mesh_props");
        jobElement->LinkEndChild(msElement);
        std::map<std::string,std::string> meshProps = job->GetMeshProps();
        it = meshProps.begin();
        while(it != meshProps.end()) {
            auto element = document.NewElement("prop");
            msElement->LinkEndChild(element);
            element->SetAttribute("key", it->first.c_str());
            element->SetAttribute("value", it->second.c_str());
            it++;
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

mitk::IFileIO::ConfidenceLevel sv4guiMitkROMSimJobIO::GetWriterConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetWriterConfidenceLevel() == mitk::IFileIO::Unsupported) return mitk::IFileIO::Unsupported;
    const sv4guiMitkROMSimJob* input = dynamic_cast<const sv4guiMitkROMSimJob*>(this->GetInput());
    if (input)
    {
        return Supported;
    }else{
        return Unsupported;
    }
}

sv4guiMitkROMSimJobIO* sv4guiMitkROMSimJobIO::IOClone() const
{
    return new sv4guiMitkROMSimJobIO(*this);
}

