# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Note: these are duplicated in SimVascular/Externals/CMake/SvExtVersions.cmake

#-----------------------------------------------------------------------------
# URLs for external downloads and git repositories
set(SV_EXTERNALS_VERSION_NUMBER  "2024.05" CACHE STRING "SimVascular Externals version")
#set(SV_EXTERNALS_VERSION_NUMBER  "2022.10" CACHE STRING "SimVascular Externals version")
set_property(CACHE SV_EXTERNALS_VERSION_NUMBER PROPERTY STRINGS "2019.06" "2022.10" "2024.05")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# set external version based on externals version number

if(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2019.06")

  set(Qt5_VERSION "5.11.3")
  set(HDF5_VERSION "1.10.1")
  set(TINYXML2_VERSION "6.2.0")
  set(TCL_VERSION "8.6.4")
  set(TK_VERSION "8.6.4")
  set(TCLLIB_VERSION "1.17")
  set(TKLIB_VERSION "0.6")
  set(PYTHON_VERSION "3.5.5")
  set(PIP_VERSION "0.0.0")
  set(NUMPY_VERSION "1.14.3")
  set(FREETYPE_VERSION "2.6.3")
  set(SWIG_VERSION "3.0.12")
  set(MMG_VERSION "5.3.9")
  set(GDCM_VERSION "2.6.3")
  set(VTK_VERSION "8.1.1")
  set(ITK_VERSION "4.13.2")
  set(OpenCASCADE_VERSION "7.3.0")
  set(MITK_VERSION "2018.04.2")
  set(ML_VERSION "1.0.0")

elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2022.10")

  set(Qt5_VERSION "5.15.10")
  set(HDF5_VERSION "1.12.2")
  set(TINYXML2_VERSION "6.2.0")
  set(TCL_VERSION "8.6.13")
  set(TK_VERSION "8.6.13")
  set(TCLLIB_VERSION "1.17")
  set(TKLIB_VERSION "0.6")
  set(PYTHON_VERSION "3.9.10")
  set(PIP_VERSION "0.0.0")
  set(NUMPY_VERSION "1.14.3")
  set(FREETYPE_VERSION "2.13.0")
  set(SWIG_VERSION "3.0.12")
  set(MMG_VERSION "5.3.9")
  set(GDCM_VERSION "3.0.10")
  set(VTK_VERSION "9.1.0")
  set(ITK_VERSION "5.2.1")
  set(OpenCASCADE_VERSION "7.6.0")
  set(MITK_VERSION "2022.10")
  set(ML_VERSION "1.0.0")

elseif(SV_EXTERNALS_VERSION_NUMBER VERSION_EQUAL "2024.05")

  set(Qt6_VERSION "6.6.2")
  set(HDF5_VERSION "1.14.3")
  #set(HDF5_VERSION "1.12.2")

  set(TINYXML2_VERSION "8.0.0")
  #set(TINYXML2_VERSION "6.2.0")
  set(TCL_VERSION "8.6.13")
  set(TK_VERSION "8.6.13")
  set(TCLLIB_VERSION "1.17")
  set(TKLIB_VERSION "0.6")

  set(PYTHON_VERSION "3.11.0")
  #set(PYTHON_VERSION "3.13.0")
  #set(PYTHON_VERSION "3.9.10")

  set(PIP_VERSION "0.0.0")
  set(NUMPY_VERSION "1.14.3")
  set(FREETYPE_VERSION "2.13.0")
  set(SWIG_VERSION "3.0.12")
  set(MMG_VERSION "5.3.9")
  set(GDCM_VERSION "3.0.10")

  set(VTK_VERSION "9.3.0")
  #set(VTK_VERSION "9.1.0")

  set(ITK_VERSION "5.4.0")
  #set(ITK_VERSION "5.2.1")

  set(OpenCASCADE_VERSION "7.6.0")
  set(MITK_VERSION "2022.10")
  set(ML_VERSION "1.0.0")

else()

  message(ERROR "Externals version number not valid")

endif()
