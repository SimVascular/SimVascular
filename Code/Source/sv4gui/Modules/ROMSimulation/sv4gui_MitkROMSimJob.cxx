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

#include "sv4gui_MitkROMSimJob.h"

const std::string sv4guiMitkROMSimJob::JobBasicParameters::FLUID_DENSITY = "Fluid Density";
const std::string sv4guiMitkROMSimJob::JobBasicParameters::FLUID_VISCOSITY = "Fluid Viscosity";
const std::string sv4guiMitkROMSimJob::JobBasicParameters::INITIAL_PRESSURE = "Initial Pressure";
const std::string sv4guiMitkROMSimJob::JobBasicParameters::INITIAL_VELOCITIES = "Initial Velocities";
const std::vector<std::string> sv4guiMitkROMSimJob::JobBasicParameters::names = 
{
    sv4guiMitkROMSimJob::JobBasicParameters::FLUID_DENSITY,
    sv4guiMitkROMSimJob::JobBasicParameters::FLUID_VISCOSITY,
    sv4guiMitkROMSimJob::JobBasicParameters::INITIAL_PRESSURE,
    sv4guiMitkROMSimJob::JobBasicParameters::INITIAL_VELOCITIES
};

sv4guiMitkROMSimJob::sv4guiMitkROMSimJob()
    : m_CalculateBoundingBox(true)
//    , m_Job(NULL)
    , m_MeshName("")
    , m_ModelName("")
    , m_ModelOrder("1")
    , m_Status("Inlet face needs to be selected")
    //, m_Status("No Data Files")
    , m_DataModified(false)
{
    this->InitializeEmpty();
}

sv4guiMitkROMSimJob::sv4guiMitkROMSimJob(const sv4guiMitkROMSimJob &other)
    : mitk::BaseData(other)
    , m_MeshName(other.m_MeshName)
    , m_ModelName(other.m_ModelName)
    , m_ModelOrder(other.m_ModelOrder)
    , m_JobSet(other.GetTimeSize())
    , m_DataModified(true)
    , m_CalculateBoundingBox(true)
{
    for (std::size_t t = 0; t < other.m_JobSet.size(); ++t)
    {
        if(other.m_JobSet[t])
            m_JobSet[t]=other.m_JobSet[t]->Clone();
//        else
//            m_JobSet.push_back(NULL);
    }
}

sv4guiMitkROMSimJob::~sv4guiMitkROMSimJob()
{
    this->ClearData();
}

void sv4guiMitkROMSimJob::ClearData()
{
    for(int t=0;t<m_JobSet.size();t++)
        delete m_JobSet[t];

    m_JobSet.clear();
    Superclass::ClearData();
}

void sv4guiMitkROMSimJob::InitializeEmpty()
{
    if (!m_JobSet.empty())
      this->ClearData();

    m_JobSet.resize( 1 );
    Superclass::InitializeTimeGeometry(1);
    m_Initialized = true;
}

void sv4guiMitkROMSimJob::Expand(unsigned int timeSteps)
{
    unsigned int oldSize = m_JobSet.size();

    if ( timeSteps > oldSize )
    {
        Superclass::Expand( timeSteps );

        m_JobSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

        this->InvokeEvent( sv4guiMitkROMSimJobEvent() );
    }

}

unsigned int sv4guiMitkROMSimJob::GetTimeSize() const
{
    return m_JobSet.size();
}

bool sv4guiMitkROMSimJob::IsEmptyTimeStep(unsigned int t) const
{
    return false;
}

void sv4guiMitkROMSimJob::CalculateBoundingBox(double *bounds,unsigned int t)
{
}

void sv4guiMitkROMSimJob::UpdateOutputInformation()
{
    if ( this->GetSource( ) )
    {
        this->GetSource( )->UpdateOutputInformation( );
    }

    mitk::TimeGeometry* timeGeometry = GetTimeGeometry();
    if ( timeGeometry->CountTimeSteps() != m_JobSet.size() )
    {
        itkExceptionMacro(<<"timeGeometry->CountTimeSteps() != m_JobSet.size() -- use Initialize(timeSteps) with correct number of timeSteps!");
    }

    if (m_CalculateBoundingBox)
    {
        for ( unsigned int t = 0 ; t < m_JobSet.size() ; ++t )
        {
            double bounds[6] = {0};
            CalculateBoundingBox(bounds,t);
            this->GetGeometry(t)->SetFloatBounds(bounds);
        }

        m_CalculateBoundingBox = false;
    }

    this->GetTimeGeometry()->Update();
}

void sv4guiMitkROMSimJob::SetRequestedRegionToLargestPossibleRegion()
{
}

bool sv4guiMitkROMSimJob::RequestedRegionIsOutsideOfTheBufferedRegion()
{
    return false;
}

bool sv4guiMitkROMSimJob::VerifyRequestedRegion()
{
    return true;
}

void sv4guiMitkROMSimJob::SetRequestedRegion(const DataObject * )
{
}

sv4guiROMSimJob* sv4guiMitkROMSimJob::GetSimJob(unsigned int t) const
{
    if ( t < m_JobSet.size() )
    {
        return m_JobSet[t];
    }
    else
    {
        return NULL;
    }
}

void sv4guiMitkROMSimJob::SetSimJob(sv4guiROMSimJob* job, unsigned int t)
{
    if(t<m_JobSet.size())
    {
        m_JobSet[t]=job;

        m_CalculateBoundingBox = true;
        m_DataModified=true;

        this->UpdateOutputInformation();
        this->InvokeEvent( sv4guiMitkROMSimJobEvent() );
    }
}

void sv4guiMitkROMSimJob::SetMeshName(std::string meshName)
{
    m_MeshName=meshName;
}

std::string sv4guiMitkROMSimJob::GetMeshName() const
{
    return m_MeshName;
}

void sv4guiMitkROMSimJob::SetModelName(std::string modelName)
{
    m_ModelName=modelName;
}

std::string sv4guiMitkROMSimJob::GetModelName() const
{
    return m_ModelName;
}

void sv4guiMitkROMSimJob::SetModelOrder(std::string modelOrder)
{
    m_ModelOrder=modelOrder;
}

std::string sv4guiMitkROMSimJob::GetModelOrder() const
{
    return m_ModelOrder;
}

void sv4guiMitkROMSimJob::SetStatus(std::string status)
{
    m_Status=status;
}

std::string sv4guiMitkROMSimJob::GetStatus() const
{
    return m_Status;
}
