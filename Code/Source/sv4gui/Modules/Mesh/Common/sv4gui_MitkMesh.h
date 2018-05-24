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

#ifndef SV4GUI_MITKMESH_H
#define SV4GUI_MITKMESH_H

#include <sv4guiModuleMeshExports.h>

#include "sv4gui_Mesh.h"
#include "sv4gui_MitkMeshOperation.h"

#include "mitkBaseData.h"

class SV4GUIMODULEMESH_EXPORT sv4guiMitkMesh : public mitk::BaseData
{
public:

    mitkClassMacro(sv4guiMitkMesh, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual void Expand( unsigned int timeSteps = 1 ) override;

    virtual void ExecuteOperation(mitk::Operation *operation) override;

    virtual bool IsEmptyTimeStep(unsigned int t) const override;

    virtual unsigned int GetTimeSize() const;

    sv4guiMesh* GetMesh(unsigned int t=0) const;

    void SetMesh(sv4guiMesh* mesh, unsigned int t=0);

    void CalculateBoundingBox(double *bounds,unsigned int t = 0 );

    void SetType(std::string type);

    std::string GetType() const;

    std::string GetModelName() const;

    void SetModelName(std::string name);

    virtual void UpdateOutputInformation() override;
    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    bool IsDataModified(){return m_DataModified;}
    void SetDataModified(bool modified = true){m_DataModified=modified;}

  protected:

    mitkCloneMacro(Self);

    sv4guiMitkMesh();
    sv4guiMitkMesh(const sv4guiMitkMesh &other);
    virtual ~sv4guiMitkMesh();

    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
    virtual void ClearData() override;
    virtual void InitializeEmpty() override;

    std::vector< sv4guiMesh* > m_MeshSet;

    bool m_CalculateBoundingBox;

    std::string m_Type;

    std::string m_ModelName;

    bool m_DataModified;
};

itkEventMacro( sv4guiMitkMeshEvent, itk::AnyEvent );

itkEventMacro( sv4guiMitkMeshExtendTimeRangeEvent, sv4guiMitkMeshEvent );
itkEventMacro( sv4guiMitkMeshSetEvent, sv4guiMitkMeshEvent );

#endif // SV4GUI_MITKMESH_H
