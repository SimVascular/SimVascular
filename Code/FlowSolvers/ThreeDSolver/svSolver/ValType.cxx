/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * Rensselaer Polytechnic Institute, Charles A. Taylor, 
 * Kenneth E. Jansen.
 * 
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution. 
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior 
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <string>
#include <vector>
#include <sstream>

#include <stdlib.h>

#include "ValType.h"

ValType::operator int()
{
  used = true;
  return get_int(str);
}

ValType::operator vector<double>()
{
  used = true;
  return get_vector(str);
}

ValType::operator vector<int>()
{
  used = true;
  return get_ivector(str);
}

ValType::operator double()
{
  used = true;
  return get_double(str);
}

ValType::operator double*()
{
  used = true;
  return get_double_array(str);
}

ValType::operator string()
{
  used = true;
  return get_string(str);
}

//
//  function implementations for type specific conversions
//

int ValType::get_int(string str)
{
  int i = atoi(str.c_str());
  return i;
}

double ValType::get_double(string str)
{
  double x = atof(str.c_str());
  return x;
}

double *ValType::get_double_array(string str)
{
  std::istringstream ist(str);
  vector<double> vec;
  double v;
  while ( ist >> v ) {
    vec.push_back(v);
  }
  int n = vec.size();
  double *x = new double[n];
  for (int i=0; i < n; i++) {
    x[i] = vec[i];
  }
  return x;
}

vector<double> ValType::get_vector(string str)
{
  std::istringstream ist(str);
  vector<double> vec;
  double v;
  while ( ist >> v ) {
    vec.push_back(v);
  }
  return vec;
}

vector<int> ValType::get_ivector(string str)
{
  std::istringstream ist(str);
  vector<int> vec;
  int v;
  while ( ist >> v ) {
    vec.push_back(v);
  }
  return vec;
}

string ValType::get_string(string str)
{
  return str;
}



