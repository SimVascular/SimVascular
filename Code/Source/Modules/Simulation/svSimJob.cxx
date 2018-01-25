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

#include "svSimJob.h"

svSimJob::svSimJob()
    : m_VelocityCapNumber(0)
    , m_PressureCapNumber(0)
{
}

svSimJob::svSimJob(const svSimJob &other)
    : m_BasicProps(other.m_BasicProps)
    , m_CapProps(other.m_CapProps)
    , m_WallProps(other.m_WallProps)
    , m_VarProps(other.m_VarProps)
    , m_SolverProps(other.m_SolverProps)
    , m_RunProps(other.m_RunProps)
{
}

svSimJob::~svSimJob()
{
}

svSimJob* svSimJob::Clone()
{
    return new svSimJob(*this);
}

void svSimJob::SetBasicProps(std::map<std::string,std::string> basicProps)
{
    m_BasicProps=basicProps;
}

std::map<std::string,std::string> svSimJob::GetBasicProps()
{
    return m_BasicProps;
}

void svSimJob::SetBasicProp(const std::string& key, std::string value)
{
    m_BasicProps[key]=value;
}

std::string svSimJob::GetBasicProp(const std::string& key)
{
    return m_BasicProps[key];
}

void svSimJob::SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps)
{
    m_CapProps=capProps;
}

std::map<std::string,std::map<std::string,std::string> > svSimJob::GetCapProps()
{
    return m_CapProps;
}

void svSimJob::SetCapProp(const std::string& capName, const std::string& key, std::string value)
{
    m_CapProps[capName][key]=value;
}

std::string svSimJob::GetCapProp(const std::string& capName, const std::string& key)
{
    return m_CapProps[capName][key];
}

void svSimJob::SetWallProps(std::map<std::string,std::string> wallProps)
{
    m_WallProps=wallProps;
}

std::map<std::string,std::string> svSimJob::GetWallProps()
{
    return m_WallProps;
}

void svSimJob::SetWallProp(const std::string& key, std::string value)
{
    m_WallProps[key]=value;
}

std::string svSimJob::GetWallProp(const std::string& key)
{
    return m_WallProps[key];
}

void svSimJob::SetVarProps(std::map<std::string,std::map<std::string,std::string> > varProps)
{
    m_VarProps=varProps;
}

std::map<std::string,std::map<std::string,std::string> > svSimJob::GetVarProps()
{
    return m_VarProps;
}

void svSimJob::SetVarProp(const std::string& faceName, const std::string& key, std::string value)
{
    m_VarProps[faceName][key]=value;
}

std::string svSimJob::GetVarProp(const std::string& faceName, const std::string& key)
{
    return m_VarProps[faceName][key];
}

void svSimJob::SetSolverProps(std::map<std::string,std::string> solverProps)
{
    m_SolverProps=solverProps;
}

std::map<std::string,std::string> svSimJob::GetSolverProps()
{
    return m_SolverProps;
}

void svSimJob::SetSolverProp(const std::string& key, std::string value)
{
    m_SolverProps[key]=value;
}

std::string svSimJob::GetSolverProp(const std::string& key)
{
    return m_SolverProps[key];
}

void svSimJob::SetRunProps(std::map<std::string,std::string> runProps)
{
    m_RunProps=runProps;
}

std::map<std::string,std::string> svSimJob::GetRunProps()
{
    return m_RunProps;
}

void svSimJob::SetRunProp(const std::string& key, std::string value)
{
    m_RunProps[key]=value;
}

std::string svSimJob::GetRunProp(const std::string& key)
{
    return m_RunProps[key];
}

void svSimJob::SetIDs(std::map<std::string,int> IDs)
{
    m_IDs=IDs;
}

std::map<std::string,int> svSimJob::GetIDs()
{
    return m_IDs;
}

void svSimJob::SetVelocityCapNumber(int number)
{
    m_VelocityCapNumber=number;
}

int svSimJob::GetVelocityCapNumber()
{
    return m_VelocityCapNumber;
}

void svSimJob::SetPressureCapNumber(int number)
{
    m_PressureCapNumber=number;
}

int svSimJob::GetPressureCapNumber()
{
    return m_PressureCapNumber;
}
