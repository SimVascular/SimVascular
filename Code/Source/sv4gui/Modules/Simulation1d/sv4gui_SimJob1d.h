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

// The sv4guiSimJob1d class is used to store data for 1D simulation job.
//
// The data for a 1D simulation job is defined as a set of properties
// used to define the simulation model, boundary conditions, physical 
// parameters (e.g. density) and solver parameters (e.g. time step).
//
// Job properties are stored as key / value pairs usin a c++ map
// of type std::map<std::string,std::string>. There are maps for
//
//    m_ModelProps - 
//    m_BasicProps - 
//    m_CapProps - 
//    m_WallProps - 
//    m_VarProps - 
//    m_SolverProps - 
//    m_RunProps - 
//
#ifndef SV4GUI_SIMJOB1D_H
#define SV4GUI_SIMJOB1D_H

#include <sv4guiModuleSimulation1dExports.h>

#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

// Define the names used to access parameters from the
// property maps. 
//
namespace SimJob1dProperty {

  // Basic (physical) parameters.
  namespace Basic {
    const std::string FLUID_DENSITY = "Fluid Density";
    const std::string FLUID_VISCOSITY = "Fluid Viscosity";
    const std::string INITIAL_PRESSURE = "Initial Pressure";
    const std::string INITIAL_VELOCITIES = "Initial Velocities";
    const std::vector<std::string> names = {
      FLUID_DENSITY, FLUID_VISCOSITY, INITIAL_PRESSURE, INITIAL_VELOCITIES
      };
  };

  namespace BC {
    const std::string TYPE = "BC TYPE";
    const std::string ANALYTIC_SHAPE = "Analytic Shape";
    const std::string PERIOD = "Period";
    const std::string PRESCRIBED_VELOCITIES = "Prescribed Velocities";
    const std::string PRESSURE = "Pressure";
    const std::string RCR = "RCR";
  };


};

class SV4GUIMODULESIMULATION1D_EXPORT sv4guiSimJob1d
{
  public:
    sv4guiSimJob1d();
    sv4guiSimJob1d(const sv4guiSimJob1d &other);
    virtual sv4guiSimJob1d* Clone();

    virtual ~sv4guiSimJob1d();

    void SetModelProps(std::map<std::string,std::string> modelProps);
    std::map<std::string,std::string> GetModelProps();
    void SetModelProp(const std::string& key, std::string value);
    std::string GetModelProp(const std::string& key);

    void SetBasicProps(std::map<std::string,std::string> basicProps);
    std::map<std::string,std::string> GetBasicProps();
    void SetBasicProp(const std::string& key, std::string value);
    std::string GetBasicProp(const std::string& key);

    void SetCapProps(std::map<std::string,std::map<std::string,std::string> > capProps);
    std::map<std::string,std::map<std::string,std::string> > GetCapProps() const;
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
    std::map<std::string,std::string> m_ModelProps;
    std::map<std::string,std::string> m_RunProps;
    std::map<std::string,std::string> m_SolverProps;
    std::map<std::string,std::map<std::string,std::string> > m_VarProps;
    std::map<std::string,std::string> m_WallProps;

    std::map<std::string,int> m_IDs;

    int m_VelocityCapNumber; //for caps with prescribed velocities
    int m_PressureCapNumber; //for caps with prescribed velocities

  };


#endif // SV4GUI_SIMJOB1D_H
