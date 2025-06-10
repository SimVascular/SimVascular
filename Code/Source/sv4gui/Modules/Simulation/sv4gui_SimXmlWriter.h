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
// for the svMultiPhysics XML paramters.
//
class Sv4GuiSimXmlWriterParameters
{
  public:
    bool Coupled = true;
    int Min_iterations = 1;
    int Max_iterations = 10;
    double Tolerance = 1e-6;
    bool Impose_flux = true;
    char* Add_mesh_name = "fluid_mesh";
    char* Mesh_file_path = "mesh-complete/mesh-complete.mesh.vtu";
    std::string Face_file_path = "mesh-complete/mesh-surfaces";

    // Map between GUI and svMultiphysics profile names.
    std::map<std::string,std::string> profile_names;

    char* Add_BC_type_Dirichlet = "Dirichlet";

    class Solver {
      public:
        char* Preconditioner = "fsils";
        int Max_iterations = 15;
        double Tolerance = 1e-5;
        int Krylov_space_dimension = 250;
  
        int NS_GM_max_iterations = 10; 
        double NS_GM_tolerance = 1e-3;
      
        int NS_CG_max_iterations = 300;
        double NS_CG_tolerance = 1e-3;
    };

    Solver solver;
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

    Sv4GuiSimXmlWriterParameters parameters;

    // The root element of the document.
    tinyxml2::XMLElement* root_;

    template <typename T>
    tinyxml2::XMLElement* add_child(tinyxml2::XMLElement* parent, const std::string& name, T value);

    tinyxml2::XMLElement* add_sub_child(tinyxml2::XMLElement* parent, const std::string& name);

    void add_equation(sv4guiSimJob* job);

    void add_equation_bcs(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

    void add_mesh(sv4guiSimJob* job);

    void add_resistance_bc(GuiProperties& props, sv4guiSimJob* job, tinyxml2::XMLElement* xml_boundary_condition);

    void add_velocity_bc(GuiProperties& props, sv4guiSimJob* job, const std::string& face_name,
        tinyxml2::XMLElement* xml_boundary_condition);

    void add_wall_bc(sv4guiSimJob* job, tinyxml2::XMLElement* boundary_condition);

/*
    void add_equation_output(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

*/
    void add_equation_solver(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation);

    void add_general(sv4guiSimJob* job);

/*
    void add_projection(const sv4guisvFSIJob* job);

    void add_remeshing(const sv4guisvFSIJob* job, const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation);

    void add_single_physics_equation(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation);

    std::vector<sv4guisvFSIDomain> sort_domains(const sv4guisvFSIJob* job);
*/

};


#endif 
