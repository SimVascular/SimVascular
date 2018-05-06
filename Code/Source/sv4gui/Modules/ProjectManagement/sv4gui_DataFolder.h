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

#ifndef SV4GUI_DATAFOLDER_H
#define SV4GUI_DATAFOLDER_H

#include "SimVascular.h"

#include <sv4guiModuleProjectManagementExports.h>

#include "mitkBaseData.h"

class SV4GUIMODULEPROJECTMANAGEMENT_EXPORT sv4guiDataFolder : public mitk::BaseData
{
public:

    mitkClassMacro(sv4guiDataFolder, mitk::BaseData);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

//    virtual void Expand( unsigned int timeSteps ) override;
//    virtual void ExecuteOperation(mitk::Operation* operation) override;
//    virtual bool IsEmptyTimeStep(unsigned int t) const override;
//    virtual void UpdateOutputInformation() override;

    virtual void SetRequestedRegionToLargestPossibleRegion() override;
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() override;
    virtual bool VerifyRequestedRegion() override;
    virtual void SetRequestedRegion(const itk::DataObject *data) override;

    std::vector<std::string> GetNodeNamesToRemove();
    void AddToRemoveList(std::string nodeName);
    void ClearRemoveList();


//    bool isActive();
//    void SetActive(bool active);

protected:

    mitkCloneMacro(Self);

    sv4guiDataFolder();
    sv4guiDataFolder(const sv4guiDataFolder &other);
    virtual ~sv4guiDataFolder();

//    virtual void PrintSelf(std::ostream& os, itk::Indent indent) const override;
//    virtual void ClearData() override;

    virtual void InitializeEmpty() override;

//    bool m_CalculateBoundingBox;

//    bool m_Active;

        std::vector<std::string> m_NodeNamesToRemove;

  };


#endif // SV4GUI_DATAFOLDER_H
