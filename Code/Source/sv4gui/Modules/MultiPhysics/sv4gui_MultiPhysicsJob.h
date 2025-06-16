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

// The classes defined here store MultiPhysics simulation parameters.

#ifndef sv4guiMultiPhysicsJOB_H
#define sv4guiMultiPhysicsJOB_H

#include "sv4guiModuleMultiPhysicsExports.h"

#include "sv4gui_MultiPhysicseqClass.h"

#include <map>
#include <vector>
#include <sstream>
#include <iostream>
#include <string>

//-------------------
// sv4guiMultiPhysicsDomain
//-------------------
// Store information defining a mesh domain.
//
struct SV4GUIMODULEMULTIPHYSICS_EXPORT sv4guiMultiPhysicsDomain
{
    std::string name;
    std::string folderName;
    std::string fileName;
    std::string surfaceName;
    std::string type; //fluid, or solid for MultiPhysics 
    int id;

    std::string faceFolderName;
    std::vector<std::string> faceNames;

    double edgeSize; //max edge size for remesher

    sv4guiMultiPhysicsDomain() : name("") , folderName("") , fileName("") , surfaceName(""), type("fluid"), 
        faceFolderName("mesh-surfaces"), id(0), edgeSize(2.6)
    {
    }

};

//----------------
// sv4guiMultiPhysicsJob
//----------------
// The sv4guiMultiPhysicsJob class is primarily used to store simulation parameters. 
//
// Note: All of the data members are public and set in sv4gui_MultiPhysicsView.cxx 
// directly from the GUI.
//
class SV4GUIMODULEMULTIPHYSICS_EXPORT sv4guiMultiPhysicsJob
{
  public:
    sv4guiMultiPhysicsJob();
    sv4guiMultiPhysicsJob(const sv4guiMultiPhysicsJob &other);
    virtual ~sv4guiMultiPhysicsJob();
    virtual sv4guiMultiPhysicsJob* Clone();

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

    std::map<std::string,sv4guiMultiPhysicsDomain> m_Domains;

    std::vector<sv4guiMultiPhysicseqClass> m_Eqs;

    bool WriteFile(std::string filePath);
    bool WriteXmlFile(std::string filePath);
  };

#endif // sv4guiMultiPhysicsJOB_H
