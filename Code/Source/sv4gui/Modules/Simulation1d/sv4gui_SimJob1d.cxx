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

#include "sv4gui_SimJob1d.h"

sv4guiSimJob1d::sv4guiSimJob1d()
    : m_VelocityCapNumber(0)
    , m_PressureCapNumber(0)
{
}

sv4guiSimJob1d::sv4guiSimJob1d(const sv4guiSimJob1d &other)
    : m_ModelProps(other.m_ModelProps), m_BasicProps(other.m_BasicProps), m_CapProps(other.m_CapProps), 
      m_WallProps(other.m_WallProps), m_VarProps(other.m_VarProps), m_SolverProps(other.m_SolverProps), 
      m_RunProps(other.m_RunProps)
{
}

sv4guiSimJob1d::~sv4guiSimJob1d()
{
}

sv4guiSimJob1d* sv4guiSimJob1d::Clone()
{
    return new sv4guiSimJob1d(*this);
}


///////////////////////////////////////////////////////////
//                   M o d e l  P r o p s               //
/////////////////////////////////////////////////////////

void sv4guiSimJob1d::SetModelProps(std::map<std::string,std::string> basicProps)
{
    m_ModelProps=basicProps;
}

std::map<std::string,std::string> sv4guiSimJob1d::GetModelProps()
{
    return m_ModelProps;
}

void sv4guiSimJob1d::SetModelProp(const std::string& key, std::string value)
{
    m_ModelProps[key]=value;
}

std::string sv4guiSimJob1d::GetModelProp(const std::string& key)
{
    return m_ModelProps[key];
}

///////////////////////////////////////////////////////////
//                   B a s i c  P r o p s               //
/////////////////////////////////////////////////////////

void sv4guiSimJob1d::SetBasicProps(std::map<std::string,std::string> basicProps)
{
    m_BasicProps=basicProps;
}

std::map<std::string,std::string> sv4guiSimJob1d::GetBasicProps()
{
    return m_BasicProps;
}

void sv4guiSimJob1d::SetBasicProp(const std::string& key, std::string value)
{
    m_BasicProps[key]=value;
}

std::string sv4guiSimJob1d::GetBasicProp(const std::string& key)
{
    return m_BasicProps[key];
}

void sv4guiSimJob1d::SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps)
{
    m_CapProps=capProps;
}

std::map<std::string,std::map<std::string,std::string> > sv4guiSimJob1d::GetCapProps() const
{
    return m_CapProps;
}

void sv4guiSimJob1d::SetCapProp(const std::string& capName, const std::string& key, std::string value)
{
    m_CapProps[capName][key]=value;
}

std::string sv4guiSimJob1d::GetCapProp(const std::string& capName, const std::string& key)
{
    return m_CapProps[capName][key];
}

void sv4guiSimJob1d::SetMeshProps(std::map<std::string,std::string> meshProps)
{
    m_MeshProps=meshProps;
}

std::map<std::string,std::string> sv4guiSimJob1d::GetMeshProps()
{
    return m_MeshProps;
}

void sv4guiSimJob1d::SetMeshProp(const std::string& key, std::string value)
{
    m_MeshProps[key]=value;
}

std::string sv4guiSimJob1d::GetMeshProp(const std::string& key)
{
    return m_MeshProps[key];
}

void sv4guiSimJob1d::SetWallProps(std::map<std::string,std::string> wallProps)
{
    m_WallProps=wallProps;
}

std::map<std::string,std::string> sv4guiSimJob1d::GetWallProps()
{
    return m_WallProps;
}

void sv4guiSimJob1d::SetWallProp(const std::string& key, std::string value)
{
    m_WallProps[key]=value;
}

std::string sv4guiSimJob1d::GetWallProp(const std::string& key)
{
    return m_WallProps[key];
}

void sv4guiSimJob1d::SetVarProps(std::map<std::string,std::map<std::string,std::string> > varProps)
{
    m_VarProps=varProps;
}

std::map<std::string,std::map<std::string,std::string> > sv4guiSimJob1d::GetVarProps()
{
    return m_VarProps;
}

void sv4guiSimJob1d::SetVarProp(const std::string& faceName, const std::string& key, std::string value)
{
    m_VarProps[faceName][key]=value;
}

std::string sv4guiSimJob1d::GetVarProp(const std::string& faceName, const std::string& key)
{
    return m_VarProps[faceName][key];
}

void sv4guiSimJob1d::SetSolverProps(std::map<std::string,std::string> solverProps)
{
    m_SolverProps=solverProps;
}

std::map<std::string,std::string> sv4guiSimJob1d::GetSolverProps()
{
    return m_SolverProps;
}

void sv4guiSimJob1d::SetSolverProp(const std::string& key, std::string value)
{
    m_SolverProps[key]=value;
}

std::string sv4guiSimJob1d::GetSolverProp(const std::string& key)
{
    return m_SolverProps[key];
}

void sv4guiSimJob1d::SetRunProps(std::map<std::string,std::string> runProps)
{
    m_RunProps=runProps;
}

std::map<std::string,std::string> sv4guiSimJob1d::GetRunProps()
{
    return m_RunProps;
}

void sv4guiSimJob1d::SetRunProp(const std::string& key, std::string value)
{
    m_RunProps[key]=value;
}

std::string sv4guiSimJob1d::GetRunProp(const std::string& key)
{
    return m_RunProps[key];
}

void sv4guiSimJob1d::SetIDs(std::map<std::string,int> IDs)
{
    m_IDs=IDs;
}

std::map<std::string,int> sv4guiSimJob1d::GetIDs()
{
    return m_IDs;
}

void sv4guiSimJob1d::SetVelocityCapNumber(int number)
{
    m_VelocityCapNumber=number;
}

int sv4guiSimJob1d::GetVelocityCapNumber()
{
    return m_VelocityCapNumber;
}

void sv4guiSimJob1d::SetPressureCapNumber(int number)
{
    m_PressureCapNumber=number;
}

int sv4guiSimJob1d::GetPressureCapNumber()
{
    return m_PressureCapNumber;
}
