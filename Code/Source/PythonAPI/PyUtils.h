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

#ifndef PY_UTILS_H__
#define PY_UTILS_H__

#include "SimVascular.h"

#include "svPythonAPIExports.h"

#include "Python.h"
#include "vtkPythonUtil.h"
#include "vtkSmartPointer.h"
#include "vtkPolyData.h"
#include "sv3_Contour.h"

#include <array>
#include <map>
#include <string>

//-------------------
// PyUtilApiFunction
//-------------------
// This class is used to manage argument formats for API methods 
// and error reporting.
//
class SV_EXPORT_PYTHON_API PyUtilApiFunction
{
  public:
      PyUtilApiFunction(const std::string& format, PyObject* pyRunTimeErr, const char* funcName);
      void error(std::string msg);
      PyObject * argsError();
      void warning(std::string msg);
      std::string formatString; 
      const char* format; 
      std::string msgp; 
      PyObject* pyError; 
};

// Types and functions used to extract data from PathFrame objects.
//
extern PyTypeObject PyPathFrameType;
extern bool PyPathFrameGetData(PyObject* object, int& id, std::array<double,3>&  position, std::array<double,3>& normal,
  std::array<double,3>& tangent, std::string& msg);

// General funtions.
//
bool PyUtilCheckPointData(PyObject* pointData, std::string& msg);

bool PyUtilCheckPointDataList(PyObject* pointData, std::string& msg);

double PyUtilComputeDistPointsToPlane(const std::array<double,3>& center, const std::array<double,3>& normal, 
  const std::vector<std::array<double,3>>& points);

std::array<double,3> PyUtilComputeNormalFromlPoints(const std::vector<std::array<double,3>>& points);

std::array<double,3> 
PyUtilComputePointsCenter(const std::vector<std::array<double,3>>& points);

bool PyUtilConvertPointData(PyObject* data, int index, std::string& msg, double point[3]);

bool PyUtilConvertPointData(PyObject* data, int index, std::string& msg, int point[3]);

std::string PyUtilGetObjectType(PyObject* obj);

void PyUtilGetPyErrorInfo(PyObject* item, std::string& valMsg, std::string& itemStr);

bool PyUtilGetFrameData(PyUtilApiFunction& api, PyObject* centerArg, std::array<double,3>& center,                  
  PyObject* normalArg, std::array<double,3>& normal, PyObject* frameObj, sv3::PathElement::PathPoint& pathPoint);

std::string PyUtilGetFunctionName(const char* functionName);

std::vector<std::map<std::string,std::string>> PyUtilGetDictListAttr(PyObject* obj, std::string name);

bool PyUtilGetBoolAttr(PyObject* obj, std::string name);

double PyUtilGetDoubleAttr(PyObject* obj, std::string name);

int PyUtilGetIntAttr(PyObject* obj, std::string name);

std::string PyUtilGetStringAttr(PyObject* obj, std::string name);

std::vector<std::string> PyUtilGetStringListAttr(PyObject* obj, std::string name);

std::string PyUtilGetMsgPrefix(const std::string& functionName);

template <typename T>
bool PyUtilGetPointData(PyObject* pyPoint, std::string& msg, T point[3]);

bool PyUtilGetPointVectorData(PyObject *pointsObj, std::vector<std::array<double,3>>& points, std::string& msg);

PyObject* PyUtilGetVtkObject(PyUtilApiFunction& api, vtkSmartPointer<vtkPolyData> polydata);

vtkPolyData * PyUtilGetVtkPolyData(PyUtilApiFunction& api, PyObject* obj);

PyObject * PyUtilPointVectorDataToPyList(const std::vector<std::array<double,3>>& points);

PyObject* PyUtilResetException(PyObject * PyRunTimeErr);

void PyUtilSetErrorMsg(PyObject* pyRunTimeErr, std::string& msgp, std::string msg);

void PyUtilSetupApiFunction(const char* functionName, std::string& format, std::string& msg);

#endif 
