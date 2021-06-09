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
#include "SimVascular.h"

#include "sv3_PathElement.h"
#include "sv_RepositoryData.h"
#include "sv3_PathGroup.h"
#include "sv_Math.h"

using sv3::PathElement;
using sv3::PathGroup;
PathGroup::PathGroup()
    : cvRepositoryData( PATHGROUP_T )
    , m_CalculateBoundingBox(true)
    , m_PathID(-1)
    , m_Method(sv3::PathElement::CONSTANT_TOTAL_NUMBER)
    , m_CalculationNumber(100)
    , m_Spacing(0)
{
    this->InitializeEmpty();
}

PathGroup::PathGroup(const PathGroup &other)
    : cvRepositoryData( PATHGROUP_T )
    , m_PathID(other.m_PathID)
    , m_Method(other.m_Method)
    , m_CalculationNumber(other.m_CalculationNumber)
    , m_Spacing(other.m_Spacing)
    , m_PathElementSet(other.GetTimeSize())
    , m_CalculateBoundingBox(true)

{
    for (std::size_t t = 0; t < other.GetTimeSize(); ++t)
    {
        m_PathElementSet[t]=other.GetPathElement(t)->Clone();
    }
}

PathGroup::~PathGroup()
{
    this->ClearData();
}

void PathGroup::ClearData()
{
    //may need delele each arrays inside first.
    m_PathElementSet.clear();
}

void PathGroup::InitializeEmpty()
{
    m_PathElementSet.resize( 1 );

}


void PathGroup::Expand( unsigned int timeSteps )
{
    unsigned int oldSize = m_PathElementSet.size();

    if ( timeSteps > oldSize )
    {

        m_PathElementSet.resize( timeSteps );

        m_CalculateBoundingBox = true;

    }
}

std::string PathGroup::GetName() const
{
    return m_Name;
}

void PathGroup::SetName(const std::string& name) 
{
    m_Name = name;
}

unsigned int PathGroup::GetTimeSize() const
{
    return m_PathElementSet.size();
}

int PathGroup::GetSize( unsigned int t ) const
{
    if(GetPathElement(t))
        return GetPathElement(t)->GetControlPointNumber();
    else
        return 0;
}

PathElement* PathGroup::GetPathElement(unsigned int t ) const
{
    if ( t < m_PathElementSet.size() )
    {
        return m_PathElementSet[t];
    }
    else
    {
        return NULL;
    }
}

void PathGroup::SetPathElement(PathElement* pathElement, unsigned int t)
{
    if(t<m_PathElementSet.size())
    {
        m_PathElementSet[t]=pathElement;
    }
}

int PathGroup::GetPathID() const
{
    return m_PathID;
}

void PathGroup::SetPathID(int pathID)
{
    m_PathID=pathID;
}

void PathGroup::CalculateBoundingBox(double *bounds,unsigned int t)
{
    PathElement* pathElement=GetPathElement(t);
    if(pathElement)
    {
        pathElement->CalculateBoundingBox(bounds);
    }
}

void PathGroup::SetSpacing(double spacing)
{
    m_Spacing=spacing;
}

double PathGroup::GetSpacing() const
{
    return m_Spacing;
}

void PathGroup::SetMethod(sv3::PathElement::CalculationMethod method)\
{
    m_Method=method;
}

sv3::PathElement::CalculationMethod PathGroup::GetMethod() const
{
    return m_Method;
}

void PathGroup::SetCalculationNumber(int number)
{
    m_CalculationNumber=number;
}

int PathGroup::GetCalculationNumber() const
{
    return m_CalculationNumber;
}
