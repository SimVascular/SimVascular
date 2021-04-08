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

#include "sv4gui_ROMSimJob.h"

sv4guiROMSimJob::sv4guiROMSimJob()
    : m_VelocityCapNumber(0)
    , m_PressureCapNumber(0)
{
}

sv4guiROMSimJob::sv4guiROMSimJob(const sv4guiROMSimJob &other)
    : m_ModelProps(other.m_ModelProps), m_BasicProps(other.m_BasicProps), m_CapProps(other.m_CapProps), 
      m_ConvertResultsProps(other.m_ConvertResultsProps), m_WallProps(other.m_WallProps), 
      m_VarProps(other.m_VarProps), m_SolverProps(other.m_SolverProps), m_RunProps(other.m_RunProps)
{
}

sv4guiROMSimJob::~sv4guiROMSimJob()
{
}

sv4guiROMSimJob* sv4guiROMSimJob::Clone()
{
    return new sv4guiROMSimJob(*this);
}


///////////////////////////////////////////////////////////
//                   M o d e l  P r o p s               //
/////////////////////////////////////////////////////////

void sv4guiROMSimJob::SetModelProps(std::map<std::string,std::string> basicProps)
{
    m_ModelProps=basicProps;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetModelProps()
{
    return m_ModelProps;
}

void sv4guiROMSimJob::SetModelProp(const std::string& key, std::string value)
{
    m_ModelProps[key]=value;
}

std::string sv4guiROMSimJob::GetModelProp(const std::string& key)
{
    return m_ModelProps[key];
}

///////////////////////////////////////////////////////////
//                   B a s i c  P r o p s               //
/////////////////////////////////////////////////////////

void sv4guiROMSimJob::SetBasicProps(std::map<std::string,std::string> basicProps)
{
    m_BasicProps=basicProps;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetBasicProps()
{
    return m_BasicProps;
}

void sv4guiROMSimJob::SetBasicProp(const std::string& key, std::string value)
{
    m_BasicProps[key]=value;
}

std::string sv4guiROMSimJob::GetBasicProp(const std::string& key)
{
    return m_BasicProps[key];
}

///////////////////////////////////////////////////////////
//                   C a p    P r o p s                 //
/////////////////////////////////////////////////////////

void sv4guiROMSimJob::SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps)
{
    m_CapProps=capProps;
}

std::map<std::string,std::map<std::string,std::string> > sv4guiROMSimJob::GetCapProps() const
{
    return m_CapProps;
}

void sv4guiROMSimJob::SetCapProp(const std::string& capName, const std::string& key, std::string value)
{
    m_CapProps[capName][key]=value;
}

std::string sv4guiROMSimJob::GetCapProp(const std::string& capName, const std::string& key)
{
    return m_CapProps[capName][key];
}

///////////////////////////////////////////////////////////
//        C o n v e r t R e s u l t s     P r o p s     //
/////////////////////////////////////////////////////////

void sv4guiROMSimJob::SetConvertResultsProps(std::map<std::string,std::string> props)
{
    m_ConvertResultsProps = props;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetConvertResultsProps()
{
    return m_ConvertResultsProps;
}

void sv4guiROMSimJob::SetConvertResultsProp(const std::string& key, std::string value)
{
    m_ConvertResultsProps[key] = value;
}

std::string sv4guiROMSimJob::GetConvertResultsProp(const std::string& key)
{
    return m_ConvertResultsProps[key];
}

///////////////////////////////////////////////////////////
//                   M e s h   P r o p s                //
/////////////////////////////////////////////////////////

void sv4guiROMSimJob::SetMeshProps(std::map<std::string,std::string> meshProps)
{
    m_MeshProps=meshProps;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetMeshProps()
{
    return m_MeshProps;
}

void sv4guiROMSimJob::SetMeshProp(const std::string& key, std::string value)
{
    m_MeshProps[key]=value;
}

std::string sv4guiROMSimJob::GetMeshProp(const std::string& key)
{
    return m_MeshProps[key];
}

void sv4guiROMSimJob::SetWallProps(std::map<std::string,std::string> wallProps)
{
    m_WallProps=wallProps;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetWallProps()
{
    return m_WallProps;
}

void sv4guiROMSimJob::SetWallProp(const std::string& key, std::string value)
{
    m_WallProps[key]=value;
}

std::string sv4guiROMSimJob::GetWallProp(const std::string& key)
{
    return m_WallProps[key];
}

void sv4guiROMSimJob::SetVarProps(std::map<std::string,std::map<std::string,std::string> > varProps)
{
    m_VarProps=varProps;
}

std::map<std::string,std::map<std::string,std::string> > sv4guiROMSimJob::GetVarProps()
{
    return m_VarProps;
}

void sv4guiROMSimJob::SetVarProp(const std::string& faceName, const std::string& key, std::string value)
{
    m_VarProps[faceName][key]=value;
}

std::string sv4guiROMSimJob::GetVarProp(const std::string& faceName, const std::string& key)
{
    return m_VarProps[faceName][key];
}

void sv4guiROMSimJob::SetSolverProps(std::map<std::string,std::string> solverProps)
{
    m_SolverProps=solverProps;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetSolverProps()
{
    return m_SolverProps;
}

void sv4guiROMSimJob::SetSolverProp(const std::string& key, std::string value)
{
    m_SolverProps[key]=value;
}

std::string sv4guiROMSimJob::GetSolverProp(const std::string& key)
{
    return m_SolverProps[key];
}

void sv4guiROMSimJob::SetRunProps(std::map<std::string,std::string> runProps)
{
    m_RunProps=runProps;
}

std::map<std::string,std::string> sv4guiROMSimJob::GetRunProps()
{
    return m_RunProps;
}

void sv4guiROMSimJob::SetRunProp(const std::string& key, std::string value)
{
    m_RunProps[key]=value;
}

std::string sv4guiROMSimJob::GetRunProp(const std::string& key)
{
    return m_RunProps[key];
}

void sv4guiROMSimJob::SetIDs(std::map<std::string,int> IDs)
{
    m_IDs=IDs;
}

std::map<std::string,int> sv4guiROMSimJob::GetIDs()
{
    return m_IDs;
}

void sv4guiROMSimJob::SetVelocityCapNumber(int number)
{
    m_VelocityCapNumber=number;
}

int sv4guiROMSimJob::GetVelocityCapNumber()
{
    return m_VelocityCapNumber;
}

void sv4guiROMSimJob::SetPressureCapNumber(int number)
{
    m_PressureCapNumber=number;
}

int sv4guiROMSimJob::GetPressureCapNumber()
{
    return m_PressureCapNumber;
}
