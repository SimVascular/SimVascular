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

#include "sv4gui_ROMSimulationUtils.h"

#include "sv4gui_StringUtils.h"
#include "sv_integrate_surface.h"

#include <array>
#include <sstream>
#include <iostream>
#include <string>
#include <fstream>

#include <vtkXMLPolyDataReader.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkPointData.h>
#include <vtkDoubleArray.h>
#include <vtkAbstractArray.h>

//-----------------------
// CreateRCRTFileContent
//-----------------------
// Generate the content of a RCR boundary condition file. 
//
// Unlike the standard rcrt.dat file, this file contains the outlet face name. 
// This is used by the Python 1D solver input file generation script to identify 
// an outlet face with each boundary condition. 
//
std::string sv4guiROMSimulationUtils::CreateRCRTFileContent(const sv4guiROMSimJob* job)
{
    std::stringstream ss;
    auto capProps = job->GetCapProps();
    auto it = capProps.begin();

    for (auto const& cap : capProps) {
        if (cap.first == "") { 
            continue;
        }
        auto props = cap.second;

        if (props["BC Type"] == "RCR") {
            auto values = sv4guiStringUtils_split(props["Values"],' ');
            if (values.size() == 3) {
                ss << "2\n";
                ss << cap.first + "\n";
                ss << values[0] << "\n";
                ss << values[1] << "\n";
                ss << values[2] << "\n";
                ss << "0.0 " << props["Pressure"] << "\n";
                ss << "1.0 " << props["Pressure"] << "\n";
            }
        }
    }
   
    if (ss.str() == "") {
        return "";
    } else {
        return "2\n" + ss.str();
   }
}

//-----------------------
// CreateCORTFileContent
//-----------------------
// Generate the content of a coronary boundary condition file. 
//
// Unlike the standard cort.dat file, this file contains the outlet face name. 
//
std::string sv4guiROMSimulationUtils::CreateCORTFileContent(const sv4guiROMSimJob* job)
{
    std::stringstream ss;
    auto capProps = job->GetCapProps();
    int maxStepNumber = 0;
    std::string contents;

    for (auto const& cap : capProps) {
        if (cap.first == "") { 
            continue;
        }
        auto props = cap.second;

        if (props["BC Type"] != "Coronary") {
            continue;
        }

        auto values = sv4guiStringUtils_split(props["Values"],' ');
        if (values.size() != 5) {
            return contents;
        }
        double pressurePeriod = std::stod(props["Pressure Period"]);
        double pressureScaling = std::stod(props["Pressure Scaling"]);

        auto timePressureStr = sv4guiStringUtils_split(props["Timed Pressure"],'\n');
        int numTimeSteps = timePressureStr.size();
        if (numTimeSteps > maxStepNumber) {
            maxStepNumber = numTimeSteps;
        }

        // Convert the time-pressure data.
        std::vector<std::array<double,2>> timePressure(numTimeSteps);
        transform(timePressureStr.begin(), timePressureStr.end(), timePressure.begin(), [](std::string const& entry) { 
          auto vals = sv4guiStringUtils_split(entry,' '); return std::array<double,2>{std::stod(vals[0]), std::stod(vals[1])};} );

        double orignalPeriod = timePressure.back()[0]; 
        double timeFactor = 1.0;
        if ((pressurePeriod > 0.0) && (pressurePeriod != orignalPeriod)) {
            timeFactor = pressurePeriod / orignalPeriod;
        }

        double pressureFactor = 1.0;
        if ((pressureScaling != 0.0) && (pressureScaling != 1.0)) {
            pressureFactor = pressureScaling;
        }

        double Ra = std::stod(values[0]);
        double Ca = std::stod(values[1]);
        double Ram = std::stod(values[2]);
        double Cim = std::stod(values[3]);
        double Rv = std::stod(values[4]);

        double q0 = Ra + Ram + Rv;
        double q1 = Ra*Ca*(Ram+Rv) + Cim*(Ra+Ram)*Rv;
        double q2 = Ca*Cim*Ra*Ram*Rv;

        double p0 = 1.0;
        double p1 = Ram*Ca + Rv*(Ca+Cim);
        double p2 = Ca*Cim*Ram*Rv;

        double b0 = 0.0;
        double b1 = Cim*Rv;
        double b2 = 0.0;

        ss << numTimeSteps << "\n";
        ss << cap.first << "\n";
        ss << q0 << "\n";
        ss << q1 << "\n";
        ss << q2 << "\n";
        ss << p0 << "\n";
        ss << p1 << "\n";
        ss << p2 << "\n";
        ss << b0 << "\n";
        ss << b1 << "\n";
        ss << b2 << "\n";
        ss << "0.0 \n";
        ss << "100.0 \n";

        for (auto entry : timePressure) {
            auto time = timeFactor * entry[0];
            auto pressure = pressureFactor * entry[1];
            ss << time << " " << pressure << "\n";
        }
    }

    if (ss.str() == "") { 
        contents = "";
    } else {
        std::stringstream newss;
        newss<<maxStepNumber << "\n" <<ss.str();
        contents = newss.str();
    }

    return contents;
}
