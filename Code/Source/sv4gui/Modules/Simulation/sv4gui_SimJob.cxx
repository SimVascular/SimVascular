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

#include "sv4gui_SimJob.h"

// These values must match those set for the Qt 'comboBoxBCType' widget.
std::string sv4guiSimJobBCType::lpm = "LPM";
std::string sv4guiSimJobBCType::flow = "Prescribed Velocities";
std::string sv4guiSimJobBCType::rcr = "RCR";
std::string sv4guiSimJobBCType::resistance = "Resistance";

sv4guiSimJob::sv4guiSimJob()
    : m_VelocityCapNumber(0)
    , m_PressureCapNumber(0)
{

// Map the section names in 
// org.sv.gui.qt.simulation/resources/solvertemplate.xml file
// to the appropriate class.
//
solver_section_names = { 
  { "Output",           &solver_output_props},
  { "Time Step",        &solver_time_props},
  { "Nonlinear Solver", &nonlinear_solver_props},
  { "Linear Solver",    &linear_solver_props}
};

}

// Copy method.
//
sv4guiSimJob::sv4guiSimJob(const sv4guiSimJob &other)
{
    basic_props = other.basic_props;
    zerod_interface_props = other.zerod_interface_props;
    cap_props = other.cap_props;
    wall_props = other.wall_props;
    cmm_props = other.cmm_props;
    run_props = other.run_props;

    solver_output_props = other.solver_output_props;
    solver_time_props = other.solver_time_props;
    linear_solver_props = other.linear_solver_props;
    nonlinear_solver_props = other.nonlinear_solver_props;

   solver_section_names = other.solver_section_names;
}

sv4guiSimJob::~sv4guiSimJob()
{
}

sv4guiSimJob* sv4guiSimJob::Clone()
{
    return new sv4guiSimJob(*this);
}

//---------------
// SetSolverProp
//---------------
// Set the property value for the solver section given by 'section_name'.
//
// This is used to store solver properties for each section in the 
// org.sv.gui.qt.simulation/resources/solvertemplate.xml file.

void sv4guiSimJob::SetSolverProp(const std::string& section_name, const std::string& key, const std::string& value) 
{
  if (solver_section_names.count(section_name) == 0) {
    throw std::runtime_error("[sv4guiSimJob::SetSolverProp] Internal error: no known solver property section name '" + 
        section_name + ".");
  }
  auto props = solver_section_names[section_name];
  props->Set(key, value);
}

//---------------
// GetSolverProp
//---------------
// Get the property value for the solver section given by 'section_name'.
//
std::string sv4guiSimJob::GetSolverProp(const std::string& section_name, const std::string& key)
{
  if (solver_section_names.count(section_name) == 0) {
    throw std::runtime_error("[sv4guiSimJob::SetSolverProp] Internal error: no known solver property section name '" +
        section_name + ".");
  }
  auto props = solver_section_names[section_name];
  return props->Get(key);
}

//--------
// SetIDs
//--------
//
void sv4guiSimJob::SetIDs(std::map<std::string,int> IDs)
{
    m_IDs=IDs;
}

//--------
// GetIDs
//--------
//
std::map<std::string,int> sv4guiSimJob::GetIDs()
{
    return m_IDs;
}

void sv4guiSimJob::SetVelocityCapNumber(int number)
{
    m_VelocityCapNumber=number;
}

int sv4guiSimJob::GetVelocityCapNumber()
{
    return m_VelocityCapNumber;
}

void sv4guiSimJob::SetPressureCapNumber(int number)
{
    m_PressureCapNumber=number;
}

int sv4guiSimJob::GetPressureCapNumber()
{
    return m_PressureCapNumber;
}
