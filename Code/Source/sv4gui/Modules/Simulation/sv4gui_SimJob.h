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

using SimJobPropertyMap = std::map<std::string,std::string>;
using SimJobCapPropertyMap = std::map<std::string,std::map<std::string,std::string>> ;

class sv4guiSimJobBCType {
  public:
    static std::string lpm;
    static std::string flow;
    static std::string rcr;
    static std::string resistance;
};


class SV4GUIMODULESIMULATION_EXPORT sv4guiSimJobCapProperties
{
  public:
    SimJobCapPropertyMap properties; 
    void SetAll(SimJobCapPropertyMap props) { properties = props; };
    SimJobCapPropertyMap GetAll() { return properties; };
    void Set(const std::string& capName, const std::string& key, std::string value) { properties[capName][key] = value; };
    std::string Get(const std::string& capName, const std::string& key) { return properties[capName][key]; };
};

class SV4GUIMODULESIMULATION_EXPORT sv4guiSimJobProperties
{
  public:
    SimJobPropertyMap properties; 
    SimJobPropertyMap GetAll() { return properties; };
    std::string Get(const std::string& key) { return properties[key]; };
    void Set(const std::string& key, const std::string& value) { properties[key] = value; };
    void SetAll(SimJobPropertyMap props) { properties = props; };
};

class SV4GUIMODULESIMULATION_EXPORT sv4guiSimJob
{
  public:
    sv4guiSimJob();
    sv4guiSimJob(const sv4guiSimJob &other);
    virtual ~sv4guiSimJob();
    virtual sv4guiSimJob* Clone();

    sv4guiSimJobProperties basic_props;
    sv4guiSimJobCapProperties cap_props;
    sv4guiSimJobProperties wall_props;
    sv4guiSimJobProperties cmm_props;
    sv4guiSimJobProperties run_props;
    sv4guiSimJobProperties zerod_interface_props;

    sv4guiSimJobProperties solver_output_props;
    sv4guiSimJobProperties solver_time_props;
    sv4guiSimJobProperties linear_solver_props;
    sv4guiSimJobProperties nonlinear_solver_props;

    std::map<std::string, sv4guiSimJobProperties*> solver_section_names;

    std::string GetSolverProp(const std::string& section_name, const std::string& key);
    void SetSolverProp(const std::string& section_name, const std::string& key, const std::string& value);
                   
    void SetIDs(std::map<std::string,int> IDs);
    std::map<std::string,int> GetIDs();

    void SetVelocityCapNumber(int number);
    int GetVelocityCapNumber();

    void SetPressureCapNumber(int number);
    int GetPressureCapNumber();

  protected:

    std::map<std::string,int> m_IDs;
    int m_VelocityCapNumber; //for caps with prescribed velosities
    int m_PressureCapNumber; //for caps with prescribed velosities

  };


#endif // SV4GUI_SIMJOB_H
