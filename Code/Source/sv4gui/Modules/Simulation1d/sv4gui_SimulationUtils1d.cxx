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

#include "sv4gui_SimulationUtils1d.h"

#include "sv4gui_StringUtils.h"
#include "sv_integrate_surface.h"

#include <sstream>
#include <iostream>
#include <string>
#include <fstream>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkPointData.h>
#include <vtkDoubleArray.h>
#include <vtkAbstractArray.h>

// Include property names (e.g. Basic::FLUID_DENSITY).
using namespace SimJob1dProperty;

//-----------------------
// CreateRCRTFileContent
//-----------------------
// Generate the content of a RCR boundary condition file. 
//
// Unlike the standard rcrt.dat file, this file contains the outlet face name. 
// This is used by the Python 1D solver input file generation script to identify 
// an outlet face with each boundary condition. 
//
std::string sv4guiSimulationUtils1d::CreateRCRTFileContent(const sv4guiSimJob1d* job)
{
    std::stringstream ss;
    auto capProps = job->GetCapProps();
    auto it = capProps.begin();

    for (auto const& cap : capProps) {
        if (cap.first == "") {
            continue;
        }
        auto props = cap.second;

        if (props[BC::TYPE] == BC::RCR) {
            auto values = sv4guiStringUtils_split(props["Values"],' ');
            if (values.size() == 3) {
                ss << "2\n";
                ss << cap.first + "\n";
                ss << values[0] << "\n";
                ss << values[1] << "\n";
                ss << values[2] << "\n";
                ss << "0.0 " << props[BC::PRESSURE] << "\n";
                ss << "1.0 " << props[BC::PRESSURE] << "\n";
            }
        }
    }
   
    if (ss.str() == "") {
        return "";
    } else {
        return "2\n" + ss.str();
   }
}

