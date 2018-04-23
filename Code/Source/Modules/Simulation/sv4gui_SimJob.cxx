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

#include "sv4gui_SimJob.h"

sv4guiSimJob::sv4guiSimJob()
    : m_VelocityCapNumber(0)
    , m_PressureCapNumber(0)
{
}

sv4guiSimJob::sv4guiSimJob(const sv4guiSimJob &other)
    : m_BasicProps(other.m_BasicProps)
    , m_CapProps(other.m_CapProps)
    , m_WallProps(other.m_WallProps)
    , m_VarProps(other.m_VarProps)
    , m_SolverProps(other.m_SolverProps)
    , m_RunProps(other.m_RunProps)
{
}

sv4guiSimJob::~sv4guiSimJob()
{
}

sv4guiSimJob* sv4guiSimJob::Clone()
{
    return new sv4guiSimJob(*this);
}

void sv4guiSimJob::SetBasicProps(std::map<std::string,std::string> basicProps)
{
    m_BasicProps=basicProps;
}

std::map<std::string,std::string> sv4guiSimJob::GetBasicProps()
{
    return m_BasicProps;
}

void sv4guiSimJob::SetBasicProp(const std::string& key, std::string value)
{
    m_BasicProps[key]=value;
}

std::string sv4guiSimJob::GetBasicProp(const std::string& key)
{
    return m_BasicProps[key];
}

void sv4guiSimJob::SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps)
{
    m_CapProps=capProps;
}

std::map<std::string,std::map<std::string,std::string> > sv4guiSimJob::GetCapProps()
{
    return m_CapProps;
}

void sv4guiSimJob::SetCapProp(const std::string& capName, const std::string& key, std::string value)
{
    m_CapProps[capName][key]=value;
}

std::string sv4guiSimJob::GetCapProp(const std::string& capName, const std::string& key)
{
    return m_CapProps[capName][key];
}

void sv4guiSimJob::SetWallProps(std::map<std::string,std::string> wallProps)
{
    m_WallProps=wallProps;
}

std::map<std::string,std::string> sv4guiSimJob::GetWallProps()
{
    return m_WallProps;
}

void sv4guiSimJob::SetWallProp(const std::string& key, std::string value)
{
    m_WallProps[key]=value;
}

std::string sv4guiSimJob::GetWallProp(const std::string& key)
{
    return m_WallProps[key];
}

void sv4guiSimJob::SetVarProps(std::map<std::string,std::map<std::string,std::string> > varProps)
{
    m_VarProps=varProps;
}

std::map<std::string,std::map<std::string,std::string> > sv4guiSimJob::GetVarProps()
{
    return m_VarProps;
}

void sv4guiSimJob::SetVarProp(const std::string& faceName, const std::string& key, std::string value)
{
    m_VarProps[faceName][key]=value;
}

std::string sv4guiSimJob::GetVarProp(const std::string& faceName, const std::string& key)
{
    return m_VarProps[faceName][key];
}

void sv4guiSimJob::SetSolverProps(std::map<std::string,std::string> solverProps)
{
    m_SolverProps=solverProps;
}

std::map<std::string,std::string> sv4guiSimJob::GetSolverProps()
{
    return m_SolverProps;
}

void sv4guiSimJob::SetSolverProp(const std::string& key, std::string value)
{
    m_SolverProps[key]=value;
}

std::string sv4guiSimJob::GetSolverProp(const std::string& key)
{
    return m_SolverProps[key];
}

void sv4guiSimJob::SetRunProps(std::map<std::string,std::string> runProps)
{
    m_RunProps=runProps;
}

std::map<std::string,std::string> sv4guiSimJob::GetRunProps()
{
    return m_RunProps;
}

void sv4guiSimJob::SetRunProp(const std::string& key, std::string value)
{
    m_RunProps[key]=value;
}

std::string sv4guiSimJob::GetRunProp(const std::string& key)
{
    return m_RunProps[key];
}

void sv4guiSimJob::SetIDs(std::map<std::string,int> IDs)
{
    m_IDs=IDs;
}

std::map<std::string,int> sv4guiSimJob::GetIDs()
{
    return m_IDs;
}

void sv4guiSimJob::SetVelocityCapNumber(int number)
{
    m_VelocityCapNumber=number;
}

int sv4guiSimJob::GetVelocityCapNumber()
{
    return m_VelocityCapNumber;
}

void sv4guiSimJob::SetPressureCapNumber(int number)
{
    m_PressureCapNumber=number;
}

int sv4guiSimJob::GetPressureCapNumber()
{
    return m_PressureCapNumber;
}
