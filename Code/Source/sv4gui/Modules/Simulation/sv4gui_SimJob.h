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

#ifndef SV4GUI_SIMJOB_H
#define SV4GUI_SIMJOB_H

#include <sv4guiModuleSimulationExports.h>

#include <map>
#include <sstream>
#include <iostream>
#include <string>

class SV4GUIMODULESIMULATION_EXPORT sv4guiSimJob
{

public:

    sv4guiSimJob();

    sv4guiSimJob(const sv4guiSimJob &other);

    virtual ~sv4guiSimJob();

    virtual sv4guiSimJob* Clone();

    void SetBasicProps(std::map<std::string,std::string> basicProps);
    std::map<std::string,std::string> GetBasicProps();
    void SetBasicProp(const std::string& key, std::string value);
    std::string GetBasicProp(const std::string& key);

    void SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps);
    std::map<std::string,std::map<std::string,std::string> > GetCapProps();
    void SetCapProp(const std::string& capName, const std::string& key, std::string value);
    std::string GetCapProp(const std::string& capName, const std::string& key);

    void SetWallProps(std::map<std::string,std::string> wallProps);
    std::map<std::string,std::string> GetWallProps();
    void SetWallProp(const std::string& key, std::string value);
    std::string GetWallProp(const std::string& key);

    void SetVarProps(std::map<std::string,std::map<std::string,std::string> > varProps);
    std::map<std::string,std::map<std::string,std::string> > GetVarProps();
    void SetVarProp(const std::string& faceName, const std::string& key, std::string value);
    std::string GetVarProp(const std::string& faceName, const std::string& key);

    void SetSolverProps(std::map<std::string,std::string> solverProps);
    std::map<std::string,std::string> GetSolverProps();
    void SetSolverProp(const std::string& key, std::string value);
    std::string GetSolverProp(const std::string& key);

    void SetRunProps(std::map<std::string,std::string> runProps);
    std::map<std::string,std::string> GetRunProps();
    void SetRunProp(const std::string& key, std::string value);
    std::string GetRunProp(const std::string& key);

    void SetIDs(std::map<std::string,int> IDs);
    std::map<std::string,int> GetIDs();

    void SetVelocityCapNumber(int number);
    int GetVelocityCapNumber();

    void SetPressureCapNumber(int number);
    int GetPressureCapNumber();

  protected:

    std::map<std::string,std::string> m_BasicProps;
    std::map<std::string,std::map<std::string,std::string> > m_CapProps;
    std::map<std::string,std::string> m_WallProps;
    std::map<std::string,std::map<std::string,std::string> > m_VarProps;
    std::map<std::string,std::string> m_SolverProps;
    std::map<std::string,std::string> m_RunProps;

    std::map<std::string,int> m_IDs;

    int m_VelocityCapNumber; //for caps with prescribed velosities
    int m_PressureCapNumber; //for caps with prescribed velosities

  };


#endif // SV4GUI_SIMJOB_H
