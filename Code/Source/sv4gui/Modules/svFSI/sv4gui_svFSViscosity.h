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

// The 'sv4guisvFSIViscosity' class is used to store data for the various viscosity models.

#ifndef SV4GUI_SVFSI_VISCOSITY_H
#define SV4GUI_SVFSI_VISCOSITY_H 

#include "sv4guiModulesvFSIExports.h"

#include <map>
#include <string>
#include <vector>

//------------------------
// FSIViscosityModelNames
//------------------------
// Stores the viscosity model names recognised by svFSI.
//
class FSIViscosityModelNames {
  public:
    static std::string CARREAU_YASUDA;
    static std::string CASSONS;
    static std::string NEWTONIAN;
    static std::vector<std::string> list;
};

class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIViscosity 
{
  public:
    sv4guisvFSIViscosity();
    ~sv4guisvFSIViscosity();
    std::string model_name_;
    std::map<std::string,double> values_;
    virtual std::map<std::string,double> get_values() = 0;
};

class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIViscosityCarreauYasuda: sv4guisvFSIViscosity
{
  public:
    sv4guisvFSIViscosityCarreauYasuda();
    ~sv4guisvFSIViscosityCarreauYasuda();
    double limiting_high_shear_rate_viscosity_;
    double limiting_low_shear_rate_viscosity_;
    double shear_rate_tensor_multiplier_;
    double shear_rate_tensor_exponent_;
    double power_law_index_;
    std::map<std::string,double> get_values();
};

class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIViscosityCassons: sv4guisvFSIViscosity
{
  public:
    sv4guisvFSIViscosityCassons();
    ~sv4guisvFSIViscosityCassons();
    double asymptotic_viscosity_;
    double yield_stress_;
    double low_shear_rate_threshold_;
    std::map<std::string,double> get_values();
};

class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIViscosityNewtonian : sv4guisvFSIViscosity
{
  public:
    sv4guisvFSIViscosityNewtonian();
    ~sv4guisvFSIViscosityNewtonian();
    double constant_value_;
    std::map<std::string,double> get_values();
};


#endif 
