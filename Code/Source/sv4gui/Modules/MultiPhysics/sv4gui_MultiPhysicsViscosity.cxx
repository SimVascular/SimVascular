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

// sv4guiMultiPhysicsViscosity class methods.
//
// Note: This is not yet used.

#include "sv4gui_MultiPhysicsViscosity.h"

// Set viscosity model names.
//
// Note: These names must match those used in the MultiPhysics solver.
//
std::string MultiPhysicsViscosityModelNames::CARREAU_YASUDA = "carreau-yasuda";
std::string MultiPhysicsViscosityModelNames::CASSONS = "cassons";
std::string MultiPhysicsViscosityModelNames::NEWTONIAN = "newtonian";
std::vector<std::string> MultiPhysicsViscosityModelNames::list = {
  MultiPhysicsViscosityModelNames::CARREAU_YASUDA, 
  MultiPhysicsViscosityModelNames::CASSONS,
  MultiPhysicsViscosityModelNames::NEWTONIAN 
};

//----------------------
// sv4guiMultiPhysicsViscosity 
//----------------------
//
sv4guiMultiPhysicsViscosity::sv4guiMultiPhysicsViscosity()
{
}

sv4guiMultiPhysicsViscosity::~sv4guiMultiPhysicsViscosity() {}

////////////////////////////////////////////////////////////////////
//                          CarreauYasuda                         //
////////////////////////////////////////////////////////////////////

sv4guiMultiPhysicsViscosityCarreauYasuda::sv4guiMultiPhysicsViscosityCarreauYasuda() 
{ 
  model_name_ = MultiPhysicsViscosityModelNames::CARREAU_YASUDA;
  limiting_high_shear_rate_viscosity_ = 0.0;
  limiting_low_shear_rate_viscosity_ = 0.0;
  shear_rate_tensor_multiplier_ = 0.0;
  shear_rate_tensor_exponent_ = 0.0;
  power_law_index_ = 0.0;
};

std::map<std::string,double> sv4guiMultiPhysicsViscosityCarreauYasuda::get_values()
{
  std::map<std::string,double> values = { 
    {"Limiting high shear-rate viscosity", limiting_high_shear_rate_viscosity_},
    {"Limiting low shear-rate viscosity", limiting_low_shear_rate_viscosity_},
    {"Shear-rate tensor multiplier (lamda)", shear_rate_tensor_multiplier_},
    {"Shear-rate tensor exponent (a)", shear_rate_tensor_exponent_},
    {"Power-law index (n)", power_law_index_},
  };

  return values;
}

sv4guiMultiPhysicsViscosityCarreauYasuda::~sv4guiMultiPhysicsViscosityCarreauYasuda() { }

////////////////////////////////////////////////////////////////////
//                          Cassons                               //
////////////////////////////////////////////////////////////////////

sv4guiMultiPhysicsViscosityCassons::sv4guiMultiPhysicsViscosityCassons()
{
  model_name_ = MultiPhysicsViscosityModelNames::CASSONS;
  asymptotic_viscosity_ = 0.0;
  yield_stress_ = 0.0;
  low_shear_rate_threshold_ = 0.0;
};

sv4guiMultiPhysicsViscosityCassons::~sv4guiMultiPhysicsViscosityCassons() { }

std::map<std::string,double> sv4guiMultiPhysicsViscosityCassons::get_values()
{
  std::map<std::string,double> values = {  
    {"Asymptotic viscosity parameter", asymptotic_viscosity_},
    {"Yield stress parameter", yield_stress_},
    {"Low shear-rate threshold", low_shear_rate_threshold_},
  };
  
  return values;
}

////////////////////////////////////////////////////////////////////
//                          Newtonian                             //
////////////////////////////////////////////////////////////////////

sv4guiMultiPhysicsViscosityNewtonian::sv4guiMultiPhysicsViscosityNewtonian()
{
  model_name_ = MultiPhysicsViscosityModelNames::NEWTONIAN;
  constant_value_ = 0.0;
}

sv4guiMultiPhysicsViscosityNewtonian::~sv4guiMultiPhysicsViscosityNewtonian() { }

std::map<std::string,double> sv4guiMultiPhysicsViscosityNewtonian::get_values()
{
  std::map<std::string,double> values = {  
    {"Value", constant_value_}
  };
  
  return values;
}

