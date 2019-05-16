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

#include "sv4gui_MitkSimJob1d.h"

//--------------------
// sv4guiMitkSimJob1d
//--------------------
//
sv4guiMitkSimJob1d::sv4guiMitkSimJob1d() : m_CalculateBoundingBox(true), m_MeshName(""), m_ModelName(""), 
    m_Status("Inlet face needs to be selected"), m_DataModified(false)
{
    this->InitializeEmpty();
}

//--------------------
// sv4guiMitkSimJob1d
//--------------------
// Copy a sv4guiMitkSimJob1d object.
//
sv4guiMitkSimJob1d::sv4guiMitkSimJob1d(const sv4guiMitkSimJob1d &other) : mitk::BaseData(other), 
    m_MeshName(other.m_MeshName), m_ModelName(other.m_ModelName), m_JobSet(other.GetTimeSize()), 
    m_DataModified(true), m_CalculateBoundingBox(true)
{
    for (std::size_t t = 0; t < other.m_JobSet.size(); ++t) {
        if(other.m_JobSet[t]) {
            m_JobSet[t] = other.m_JobSet[t]->Clone();
        }
    }
}

sv4guiMitkSimJob1d::~sv4guiMitkSimJob1d()
{
    this->ClearData();
}

//-----------
// ClearData
//-----------
// Delete job objects.
//
void sv4guiMitkSimJob1d::ClearData()
{
    for (int t=0;t<m_JobSet.size();t++) {
        delete m_JobSet[t];
    }
    m_JobSet.clear();
    Superclass::ClearData();
}

//-----------------
// InitializeEmpty
//-----------------
//
void sv4guiMitkSimJob1d::InitializeEmpty()
{
    if (!m_JobSet.empty()) {
      this->ClearData();
    }

    m_JobSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

//--------
// Expand
//--------
//
void sv4guiMitkSimJob1d::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_JobSet.size();

    if ( timeSteps > oldSize ) {
        Superclass::Expand( timeSteps );
        m_JobSet.resize( timeSteps );
        m_CalculateBoundingBox = true;
        this->InvokeEvent( sv4guiMitkSimJob1dEvent() );
    }
}

unsigned int sv4guiMitkSimJob1d::GetTimeSize() const
{
    return m_JobSet.size();
}

bool sv4guiMitkSimJob1d::IsEmptyTimeStep(unsigned int t) const
{
    return false;
}

void sv4guiMitkSimJob1d::CalculateBoundingBox(double *bounds,unsigned int t)
{
}

//-------------------------
// UpdateOutputInformation
//-------------------------
//
void sv4guiMitkSimJob1d::UpdateOutputInformation()
{
    if ( this->GetSource( ) ) {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();

    if ( timeGeometry->CountTimeSteps() != m_JobSet.size() ) {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_JobSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    // [DaveP] What is a bounding box used for?
    if (m_CalculateBoundingBox) {
        for ( unsigned int t = 0 ; t < m_JobSet.size() ; ++t ) {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void sv4guiMitkSimJob1d::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiMitkSimJob1d::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiMitkSimJob1d::VerifyRequestedRegion()
{
    return true;
}

void sv4guiMitkSimJob1d::SetRequestedRegion(const DataObject * )
{
}

//-----------
// GetSimJob
//-----------
// Get a sv4guiSimJob1d object for time t.
//
sv4guiSimJob1d* sv4guiMitkSimJob1d::GetSimJob(unsigned int t) const
{
    if ( t < m_JobSet.size() ) {
        return m_JobSet[t];
    } else {
        return NULL;
    }
}

//-----------
// SetSimJob
//-----------
//
void sv4guiMitkSimJob1d::SetSimJob(sv4guiSimJob1d* job, unsigned int t)
{
    if (t < m_JobSet.size()) {
        m_JobSet[t] = job;
        m_CalculateBoundingBox = true;
        m_DataModified=true;
        this->UpdateOutputInformation();
        this->InvokeEvent( sv4guiMitkSimJob1dEvent() );
    }
}

//-------------
// SetMeshName
//-------------
// Set the name of the job mesh.
//
void sv4guiMitkSimJob1d::SetMeshName(std::string meshName)
{
    m_MeshName=meshName;
}

std::string sv4guiMitkSimJob1d::GetMeshName() const
{
    return m_MeshName;
}

//--------------
// SetModelName
//--------------
// Set the name of the job model.
//
void sv4guiMitkSimJob1d::SetModelName(std::string modelName)
{
    m_ModelName = modelName;
}

std::string sv4guiMitkSimJob1d::GetModelName() const
{
    return m_ModelName;
}

void sv4guiMitkSimJob1d::SetStatus(std::string status)
{
    m_Status=status;
}

std::string sv4guiMitkSimJob1d::GetStatus() const
{
    return m_Status;
}
