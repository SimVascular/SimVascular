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

#ifndef SV4GUI_MESH_H
#define SV4GUI_MESH_H

#include <sv4guiModuleMeshExports.h>

#include "sv4gui_ModelElement.h"

#include <vtkSmartPointer.h>

#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>

class SV4GUIMODULEMESH_EXPORT sv4guiMesh
{

public:

    sv4guiMesh();

    sv4guiMesh(const sv4guiMesh &other);

    virtual ~sv4guiMesh();

    virtual sv4guiMesh* Clone();

    std::string GetType() const;

    virtual void InitNewMesher() {}

//    std::string GetModelName() const;

//    void SetModelName(std::string name);

    sv4guiModelElement* GetModelElement() const;

    virtual bool SetModelElement(sv4guiModelElement* modelElement);

    void SetModelElementOnly(sv4guiModelElement* modelElement);

    void CalculateBoundingBox(double *bounds);

    bool ExecuteCommand(std::string cmd, std::string& msg);

    virtual bool Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg) {return false;}

    virtual bool ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg) {return false;}

    bool ExecuteCommands(std::vector<std::string> cmds, std::string& msg);

//    bool ExecuteCommandFile(std::string filePath);

    std::vector<std::string> GetCommandHistory() const;

    void SetCommandHistory(std::vector<std::string> history);

    bool ExecuteCommandHistory(std::string& msg);

    vtkSmartPointer<vtkPolyData> GetSurfaceMesh();

    vtkSmartPointer<vtkUnstructuredGrid> GetVolumeMesh();

    void SetSurfaceMesh(vtkSmartPointer<vtkPolyData> surfaceMesh);

    void SetVolumeMesh(vtkSmartPointer<vtkUnstructuredGrid> volumeMesh);

    std::vector<std::string> GetFileExtensions(){return m_FileExtensions;}

    virtual bool WriteSurfaceFile(std::string filePath);

    virtual bool WriteVolumeFile(std::string filePath);

    virtual bool ReadSurfaceFile(std::string filePath);

    virtual bool ReadVolumeFile(std::string filePath);

    virtual vtkSmartPointer<vtkPolyData> CreateSurfaceMeshFromFile(std::string filePath);

    virtual vtkSmartPointer<vtkUnstructuredGrid> CreateVolumeMeshFromFile(std::string filePath);

//    virtual bool WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, sv4guiModelElement* modelElement, std::string meshDir) {return false;}

//    virtual bool WriteMeshComplete(std::string meshDir);

  protected:

//    std::string m_ModelName;

    sv4guiModelElement* m_ModelElement;

    std::string m_Type;

    vtkSmartPointer<vtkPolyData> m_SurfaceMesh;

    vtkSmartPointer<vtkUnstructuredGrid> m_VolumeMesh;

    std::vector<std::string> m_CommandHistory;

    std::vector<std::string> m_FileExtensions;

  };

#endif // SV4GUI_MESH_H
