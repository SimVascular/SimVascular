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

#ifndef SV4GUI_SIM_XML_WRITER_H
#define SV4GUI_SIM_XML_WRITER_H 

#include "sv4gui_SimJob.h"

#include <fstream>
#include <string>
#include <vector>

#include "tinyxml2.h"

using GuiProperties = std::map<std::string,std::string>;

//------------------------------
// Sv4GuiSimXmlWriterParameters
//------------------------------
// The Sv4GuiSimXmlWriterParameters class stores default values 
// for the svMultiPhysics XML paramters that are not set (yet)
// from the GUI.
//
class Sv4GuiSimXmlWriterParameters
{
  public:
    bool Coupled = true;
    bool Impose_flux = true;
    char* Add_mesh_name = "fluid_mesh";
    char* Mesh_file_path = "mesh-complete/mesh-complete.mesh.vtu";
    std::string Face_file_path = "mesh-complete/mesh-surfaces";
    char* Preconditioner = "fsils";
    char* Name_prefix_of_saved_VTK_files = "result";

    // Map between GUI and svMultiphysics profile names.
    std::map<std::string,std::string> profile_names;

};

//--------------------
// Sv4GuiSimXmlWriter
//--------------------
// The Sv4GuiSimXmlWriter class is used to write svMultiPhysics XML input file.
//
class Sv4GuiSimXmlWriter
{
  static const char path_sep_ = 
  #ifdef _WIN32
  '\\';
  #else
  '/';
  #endif

  public:
    Sv4GuiSimXmlWriter();
    ~Sv4GuiSimXmlWriter();

    void create_document(sv4guiSimJob* job, const std::map<std::string,std::string>& faces_name_type,
        const std::string& output_dir, const std::string& file_name);

  private:
    
    // The XML document to create.
    tinyxml2::XMLDocument doc_;

    std::string output_dir_;
    std::map<std::string,std::string> faces_name_type_;  

    // If true then write cmm simulation parameters.
    bool cmm_simulation_enabled_ = false;

    // If true then write parameters for cmm prestress or inflate initialization simulation.
    bool cmm_simulation_initialization_ = false;

    // If true then write parameters for a cmm prestress initialization simulation,
    // else write parameters for a inflate initialization simulation.
    bool cmm_prestress_simulation_ = false;

    std::string cmm_wall_name_ = "";

    Sv4GuiSimXmlWriterParameters parameters;

    // The root element of the document.
    tinyxml2::XMLElement* root_;

    template <typename T>
    tinyxml2::XMLElement* add_child(tinyxml2::XMLElement* parent, const std::string& name, T value);

    tinyxml2::XMLElement* add_sub_child(tinyxml2::XMLElement* parent, const std::string& name);

    void add_equation(sv4guiSimJob* job);

    void add_equation_bcs(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

    void add_mesh(sv4guiSimJob* job);

    void add_lpm_bc(GuiProperties& props, sv4guiSimJob* job, tinyxml2::XMLElement* boundary_condition);

    void add_rcr_bc(GuiProperties& props, sv4guiSimJob* job, tinyxml2::XMLElement* boundary_condition);

    void add_resistance_bc(GuiProperties& props, sv4guiSimJob* job, tinyxml2::XMLElement* xml_boundary_condition);

    void add_velocity_bc(GuiProperties& props, sv4guiSimJob* job, const std::string& face_name,
        tinyxml2::XMLElement* xml_boundary_condition);

    void add_wall_bc(sv4guiSimJob* job, tinyxml2::XMLElement* boundary_condition);

    void add_equation_nl_solver(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

    void add_equation_output(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

    void add_equation_solver(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

    void add_general(sv4guiSimJob* job);

    void add_cmm_init_mesh(sv4guiSimJob* job);
    void add_cmm_equation(sv4guiSimJob* job);
    void add_cmm_equation_wall_bc(sv4guiSimJob* job, tinyxml2::XMLElement* equation);
    void add_cmm_equation_bf(sv4guiSimJob* job, tinyxml2::XMLElement* equation);
    void add_cmm_simulation(sv4guiSimJob* job);
    void add_cmm_wall_properties(sv4guiSimJob* job, tinyxml2::XMLElement* equation);
    bool cmm_simulation_enabled(sv4guiSimJob* job);

};


#endif 
