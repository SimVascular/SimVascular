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

// This class is used to generate a 1D mesh from centerlines geometry. 
//
#ifndef SV4GUI_ROM_SIMULATION_MESH_H
#define SV4GUI_ROM_SIMULATION_MESH_H

#include <iostream>
#include <array>
#include <set>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>

class sv4guiROMSimulationMeshParamNames
{ 
  public: 
    sv4guiROMSimulationMeshParamNames() {
      allNames.insert(AvgBranchLength);
      allNames.insert(BranchAngle);
      allNames.insert(BranchSegLength);
      allNames.insert(FirstPoint);
      allNames.insert(NumBranchGenerations);
      allNames.insert(RepulsiveParameter);
      allNames.insert(SecondPoint);
    }
    const std::string AvgBranchLength = "avgBranchLength";
    const std::string BranchAngle = "branchAngle";
    const std::string BranchSegLength = "branchSegLength";
    const std::string FirstPoint = "firstPoint";
    const std::string NumBranchGenerations = "numBranchGenerations";
    const std::string RepulsiveParameter = "repulsiveParameter";
    const std::string SecondPoint = "secondPoint";
    std::set<std::string> allNames;
};

class sv4guiROMSimulationMesh 
{
  public:
    sv4guiROMSimulationMesh(const std::string name, const std::array<double,3>& firstPoint, 
        const std::array<double,3>& secondPoint);
    sv4guiROMSimulationMesh() = delete; 
    ~sv4guiROMSimulationMesh(); 
    bool GenerateNetwork(const std::string outputPath);
    bool WriteMesh(const std::string fileName);
    std::string CreateCommand(const std::string infile, const std::string outfile);
    void SetParameters(std::map<std::string, std::string>& params);
    bool WriteParameters(const std::string fileName, std::map<std::string, std::string>& params);

    std::string name; 
    std::string networkFileName; 
    std::array<double,3> firstPoint;
    std::array<double,3> secondPoint;
    /*
    int numBranchGenerations;
    float avgBranchLength;
    float branchAngle;
    float repulsiveParameter;
    float branchSegLength;
    */
    vtkSmartPointer<vtkPolyData> meshPolyData;
    sv4guiROMSimulationMeshParamNames parameterNames;
    std::map<std::string, std::string> parameterValues;
};

#endif //SV4GUI_ROMSIMULATION_MESH_H
