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
#include "sv4gui_SimJob.h"

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

//------
// Read
//------
// Read in a Simulations .sjb XML file.
//
// Values are read in from the XML file and stored in std::maps defined
// in a sv4guiSimJob object. 
//
std::vector<mitk::BaseData::Pointer> sv4guiMitkSimJobIO::Read()
{
    #define n_debug_Read
    #ifdef debug_Read
    std::string msg("[sv4guiMitkSimJobIO::Read] ");
    std::cout << msg << std::endl;
    std::cout << msg << "========== Read =========" << std::endl;
    #endif

    std::vector<mitk::BaseData::Pointer> result;

    tinyxml2::XMLDocument document;

    std::string fileName = GetInputLocation();
    #ifdef debug_Read
    std::cout << msg << "fileName: " <<  fileName << std::endl;
    #endif

    if (document.LoadFile(fileName.c_str()) != tinyxml2::XML_SUCCESS) {
        mitkThrow() << "Could not open/read/parse " << fileName;
        return result;
    }

    auto mjElement = document.FirstChildElement("mitk_job");

    if (!mjElement) {
        mitkThrow() << "No job data in "<< fileName;
        return result;
    }

    sv4guiMitkSimJob::Pointer mitkSimJob = sv4guiMitkSimJob::New();
    std::string modelName = "";
    std::string meshName = "";
    std::string status = "";

    set_string_from_attribute(mjElement,"model_name",modelName);
    set_string_from_attribute(mjElement,"mesh_name",meshName);
    set_string_from_attribute(mjElement,"status",status);

    mitkSimJob->SetModelName(modelName);
    mitkSimJob->SetMeshName(meshName);
    mitkSimJob->SetStatus(status);

    auto jobElement = mjElement->FirstChildElement("job");

    if (jobElement != nullptr) {
        sv4guiSimJob* job = new sv4guiSimJob();

        ReadProperties(jobElement, job->basic_props, "basic_props");  

        ReadProperties(jobElement, job->zerod_interface_props, "zerod_interface_props");  

        ReadCapProperties(jobElement, job->cap_props, "cap_props");

        ReadProperties(jobElement, job->wall_props, "wall_props");  

        ReadProperties(jobElement, job->cmm_props, "cmm_props");  

        ReadProperties(jobElement, job->solver_time_props, "solver_time_props");  

        ReadProperties(jobElement, job->solver_output_props, "solver_output_props");  

        ReadProperties(jobElement, job->nonlinear_solver_props, "nonlinear_solver_props");  

        ReadProperties(jobElement, job->linear_solver_props, "linear_solver_props");  

        ReadProperties(jobElement, job->run_props, "run_props");  

        mitkSimJob->SetSimJob(job);
    } 

    result.push_back(mitkSimJob.GetPointer());
    return result;
}

//----------------
// ReadProperties
//----------------
// Read job properties form an .sjb xml file and store them 
// in the sv4guiSimJobProperties object.
//
void sv4guiMitkSimJobIO::ReadProperties(tinyxml2::XMLElement* job_element, sv4guiSimJobProperties& properties, 
    const std::string& section_name)
{
  auto section_element = job_element->FirstChildElement(section_name.c_str());

  if (section_element == nullptr) {
    return;
  }

  std::map<std::string,std::string> props;

  for (auto element = section_element->FirstChildElement("prop"); element != nullptr;
      element =element->NextSiblingElement("prop") ) {

    if (element == nullptr) {
      continue;
    }

    std::string key = "";
    std::string value = "";
    set_string_from_attribute(element, "key", key);
    set_string_from_attribute(element, "value", value);

    props[key] = value;
    }

  properties.SetAll(props);
}

//-------------------
// ReadCapProperties
//-------------------
// Read in cap properties from an .sjb xml file.
//
void sv4guiMitkSimJobIO::ReadCapProperties(tinyxml2::XMLElement* job_element, sv4guiSimJobCapProperties& properties, 
    const std::string& section_name)
{
  auto section_element = job_element->FirstChildElement(section_name.c_str());

  if (section_element == nullptr) {
    return;
  }

  std::map<std::string,std::map<std::string,std::string> > capProps;

  for (auto celement = section_element->FirstChildElement("cap"); celement != nullptr;
       celement = celement->NextSiblingElement("cap") ) {
    if (celement == nullptr) {
      continue;
    }

    std::string name="";
    set_string_from_attribute(celement, "name", name);

    for (auto element = celement->FirstChildElement("prop"); element != nullptr;
         element =element->NextSiblingElement("prop") ) {
      if (element == nullptr) {
        continue;
      }

      std::string key="";
      std::string value="";
      set_string_from_attribute(element, "key", key);
      set_string_from_attribute(element, "value", value);

      capProps[name][key]=value;
    }
  }

  properties.SetAll(capProps);
}

mitk::IFileIO::ConfidenceLevel sv4guiMitkSimJobIO::GetReaderConfidenceLevel() const
{
    if (mitk::AbstractFileIO::GetReaderConfidenceLevel() == mitk::IFileIO::Unsupported)
    {
        return mitk::IFileIO::Unsupported;
    }
    return Supported;
}

//-------
// Write
//-------
// Write a Simulations .sjb XML file.
//
// Values written to an XML file from std::maps defined 
// in a sv4guiSimJob object. 
//
void sv4guiMitkSimJobIO::Write()
{
  #define n_debug_Write
  #ifdef debug_Write
  std::string dmsg("[sv4guiMitkSimJobIO::Write] ");
  std::cout << dmsg << std::endl;
  std::cout << dmsg << "========== Write ==========" << std::endl;
  #endif

  ValidateOutputLocation();
  std::string fileName = GetOutputLocation();
  #ifdef debug_Write
  std::cout << dmsg << "fileName: " << fileName << std::endl;
  #endif

  const sv4guiMitkSimJob* mitkSimJob = dynamic_cast<const sv4guiMitkSimJob*>(this->GetInput());

  if (!mitkSimJob) {
    return;
  }

  tinyxml2::XMLDocument document;
  auto decl = document.NewDeclaration();
  document.LinkEndChild( decl );

  auto mjElement = document.NewElement("mitk_job");
  mjElement->SetAttribute("model_name", mitkSimJob->GetModelName().c_str());
  mjElement->SetAttribute("mesh_name", mitkSimJob->GetMeshName().c_str());
  mjElement->SetAttribute("status", mitkSimJob->GetStatus().c_str());
  mjElement->SetAttribute("version",  "1.0" );
  document.LinkEndChild(mjElement);

  sv4guiSimJob* job = mitkSimJob->GetSimJob();
  #ifdef debug_Write
  std::cout << dmsg << "job: " << job << std::endl;
  #endif

  if (job) {
    auto jobElement = document.NewElement("job");
    mjElement->LinkEndChild(jobElement);

    WriteProperties(document, jobElement, job->basic_props, "basic_props"); 

    WriteProperties(document, jobElement, job->zerod_interface_props, "zerod_interface_props"); 

    WriteCapProperties(document, jobElement, job->cap_props);

    WriteProperties(document, jobElement, job->wall_props, "wall_props"); 

    WriteProperties(document, jobElement, job->cmm_props, "cmm_props"); 

    WriteProperties(document, jobElement, job->solver_output_props, "solver_output_props"); 

    WriteProperties(document, jobElement, job->solver_time_props, "solver_time_props"); 

    WriteProperties(document, jobElement, job->nonlinear_solver_props, "nonlinear_solver_props"); 

    WriteProperties(document, jobElement, job->linear_solver_props, "linear_solver_props"); 

    WriteProperties(document, jobElement, job->run_props, "run_props"); 
  }

  #ifdef debug_Write
  std::cout << dmsg << "document.SaveFile ... " << std::endl;
  #endif

  if (document.SaveFile(fileName.c_str()) != tinyxml2::XML_SUCCESS) {
    mitkThrow() << "Could not write CFD Simulation parameters to file '" << fileName << "'.";
  }
}

//-----------------
// WriteProperties
//-----------------
// Write Basic properties data
//
void sv4guiMitkSimJobIO::WriteProperties(tinyxml2::XMLDocument& document, tinyxml2::XMLElement* job_element, 
    sv4guiSimJobProperties& properties, const std::string& section_name)
{
  auto section_element = document.NewElement(section_name.c_str());
  job_element->LinkEndChild(section_element);
  auto props_data = properties.GetAll();
  std::map<std::string, std::string>::iterator it = props_data.begin();

  while(it != props_data.end()) {
    auto element = document.NewElement("prop");
    section_element->LinkEndChild(element);
    element->SetAttribute("key", it->first.c_str());
    element->SetAttribute("value", it->second.c_str());
    it++;
  }
}

//--------------------
// WriteCapProperties
//--------------------
// Write Inlet and Outlet BCs data.
//
void sv4guiMitkSimJobIO::WriteCapProperties(tinyxml2::XMLDocument& document, tinyxml2::XMLElement* job_element, 
    sv4guiSimJobCapProperties& properties)
{
  #define n_debug_WriteCapProperties
  #ifdef debug_WriteCapProperties
  std::string dmsg("[sv4guiMitkSimJobIO::WriteCapProperties] ");
  std::cout << dmsg << std::endl;
  std::cout << dmsg << "========== WriteCapProperties ==========" << std::endl;
  #endif

  ValidateOutputLocation();
  auto cpElement = document.NewElement("cap_props");
  job_element->LinkEndChild(cpElement);
  auto capProps = properties.GetAll();
  auto itit = capProps.begin();

  while(itit != capProps.end()) {
    auto celement = document.NewElement("cap");
    cpElement->LinkEndChild(celement);

    celement->SetAttribute("name", itit->first.c_str());
    std::map<std::string, std::string> props=itit->second;
    auto it = props.begin();

    while (it != props.end()) {
      auto element = document.NewElement("prop");
      celement->LinkEndChild(element);
      element->SetAttribute("key", it->first.c_str());
      element->SetAttribute("value", it->second.c_str());
      #ifdef debug_WriteCapProperties
      std::cout << dmsg << "prop: " << it->first << std::endl;
      #endif
      it++;
    }

  itit++;
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

