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

// The classes defined here store FSI simulation parameters.

#ifndef sv4guisvFSIJOB_H
#define sv4guisvFSIJOB_H

#include "sv4guiModulesvFSIExports.h"

#include "sv4gui_svFSIeqClass.h"

#include <map>
#include <vector>
#include <sstream>
#include <iostream>
#include <string>

//-------------------
// sv4guisvFSIDomain
//-------------------
// Store information defining a mesh domain.
//
struct SV4GUIMODULESVFSI_EXPORT sv4guisvFSIDomain
{
    std::string name;
    std::string folderName;
    std::string fileName;
    std::string surfaceName;
    std::string type; //fluid, or solid for FSI
    int id;

    std::string faceFolderName;
    std::vector<std::string> faceNames;

    double edgeSize; //max edge size for remesher

    sv4guisvFSIDomain() : name("") , folderName("") , fileName("") , surfaceName(""), type("fluid"), 
        faceFolderName("mesh-surfaces"), id(0), edgeSize(2.6)
    {
    }

};

//----------------
// sv4guisvFSIJob
//----------------
// The sv4guisvFSIJob class is primarily used to store simulation parameters. 
//
// Note: All of the data members are public and set in sv4gui_svFSIView.cxx 
// directly from the GUI.
//
class SV4GUIMODULESVFSI_EXPORT sv4guisvFSIJob
{
  public:
    sv4guisvFSIJob();
    sv4guisvFSIJob(const sv4guisvFSIJob &other);
    virtual ~sv4guisvFSIJob();
    virtual sv4guisvFSIJob* Clone();

    int nsd;
    int timeSteps;
    std::string stepSize;
    bool continuePrevious;

    std::string restartFileName;
    int restartInc;
    int startSavingStep;

    bool vtkSaveResults;
    std::string vtkFileName;
    int vtkInc;

    bool saveAvgResult;
    double rhoInf;
    std::string stopFileName;
    bool verbose;
    bool warn;
    bool debug;

    bool remeshing;

    std::map<std::string,sv4guisvFSIDomain> m_Domains;

    std::vector<sv4guisvFSIeqClass> m_Eqs;

    bool WriteFile(std::string filePath);
  };

#endif // sv4guisvFSIJOB_H
