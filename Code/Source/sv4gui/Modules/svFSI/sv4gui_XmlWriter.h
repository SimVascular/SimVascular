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

#ifndef SV4GUI_XMLWRITER_H
#define SV4GUI_XMLWRITER_H 

#include "sv4gui_svFSIJob.h"

#include <fstream>
#include <string>
#include <vector>

#include <tinyxml2.h>

//-----------------
// Sv4GuiXmlWriter
//-----------------
// The Sv4GuiXmlWriter class is used to write svFSIplus input commands 
// in an XML format.
//
class Sv4GuiXmlWriter 
{
  static const char path_sep_ = 
  #ifdef _WIN32
  '\\';
  #else
  '/';
  #endif

  public:
    Sv4GuiXmlWriter();
    ~Sv4GuiXmlWriter();

    void create_document(const sv4guisvFSIJob* job, const std::string& file_name);

  private:
    
    // The XML document to create.
    tinyxml2::XMLDocument doc_;

    // The root element of the document.
    tinyxml2::XMLElement* root_;

    template <typename T>
    tinyxml2::XMLElement* add_child(tinyxml2::XMLElement* parent, const std::string& name, T value);

    tinyxml2::XMLElement* add_sub_child(tinyxml2::XMLElement* parent, const std::string& name);

    void add_equations(const sv4guisvFSIJob* job);

    void add_equation_solver(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation);

    void add_fsi_equation(const sv4guisvFSIeqClass& equation, tinyxml2::XMLElement* xml_equation);

    void add_general(const sv4guisvFSIJob* job);

    void add_mesh(const sv4guisvFSIJob* job, sv4guisvFSIDomain& domain, const int domain_id);

    void add_projection(const sv4guisvFSIJob* job);

    void add_single_physics_equation(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation);

    std::vector<sv4guisvFSIDomain> sort_domains(const sv4guisvFSIJob* job);

};


#endif 
