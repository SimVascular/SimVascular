# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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

#Creates a python interpreter and stores in global gPythonInterp
proc startTclPython {} {
  global env
  global SV_BUILD_TYPE
  global tcl_platform

  global gPythonInterp
  if [catch {set gPythonInterp [::python::interp new]} msg] {
    set gPythonInterp "error_in_python_init"
    puts "ERROR in pythoninterp int ($msg)"
    return
  }
  #$gPythonInterp exec {print("Python Available")}

  #Create TclPyString global to pass string between tcl and python
  global TclPyString
  $gPythonInterp exec {tcl.eval('global TclPyString')}
  set TclPyString $env(SV_HOME)
  $gPythonInterp exec {simvascular_home = tcl.eval('set dummy $TclPyString')}

  #Initiate modules
  puts "Initiate the python modules from tcl"
  if [catch {$gPythonInterp exec {tcl.eval("solid_initPyMods")}} errmsg] {
    puts "Could not initialize internal python solid modules: $errmsg"
  }
  if [catch {$gPythonInterp exec {tcl.eval("occt_initPyMods")}} errmsg] {
    puts "Could not initialize internal python occt modules: $errmsg"
  }
  if [catch {$gPythonInterp exec {import os}} errmsg] {
    puts "No os module found: $errmsg"
  }
  if [catch {$gPythonInterp exec {import imp}} errmsg] {
    puts "No imp module found: $errmsg"
  }
  if [catch {$gPythonInterp exec {import vtk}} errmsg] {
    puts "No vtk module found: $errmsg"
  }
  if [catch {$gPythonInterp exec {import pySolid}} errmsg] {
    puts "No SimVascular pySolid module found: $errmsg"
  }
  if [catch {$gPythonInterp exec {import numpy as np}} errmsg] {
    puts "No numpy module found: $errmsg"
  }
}

#Kill the python interpreter when youre done!
proc killTclPython {} {
  global gPythonInterp
  ::python::interp delete $gPythonInterp
}

#test the use of Python Modules
#Must first call tcl.eval("solid_initPyMods") to initiate module pySolid
proc testPythonModule {} {
  global gPythonInterp

  puts "Initiate the python modules from tcl"
  $gPythonInterp exec {tcl.eval("solid_initPyMods")}
  $gPythonInterp exec {import vtk}
  $gPythonInterp exec {import pySolid}
  $gPythonInterp exec {pySolid.hello("dummy")}
  $gPythonInterp exec {pySolid.accessHash("dummy")}
}

#Test to export repo item
proc testExport {} {

  global gPythonInterp
  $gPythonInterp exec {vtkobj = pySolid.exportToVtk("/group/left_sup_renal/217")}
}

#Test to import vtk object
proc testImport {} {
  global gPythonInterp
  $gPythonInterp exec {vtkobj = vtk.vtkPolyData()}
  $gPythonInterp exec {pySolid.importVtkPd(vtkobj,"myobj")}
}

#Test to import 1D and 2D lists
proc testList {} {
  global gPythonInterp

  $gPythonInterp exec {d = [2.3,4.5,6.8]}
  $gPythonInterp exec {pySolid.importList1D(d)}
  $gPythonInterp exec {n = [[3,4,5],[1,6,8]]}
  $gPythonInterp exec {pySolid.importList2D(n)}
}

#Test to import and call creation of occt model. Doesn't acutally create
#solid and should return error. Just import test
proc testCreateSurf {} {
  global gPythonInterp
  global gOptions

  set kernel "OpenCASCADE"
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel
  catch {repos_delete -obj myNewObj}
  solid_newObject -name myNewObj

  ##Python Interp
  $gPythonInterp exec {import pyOCCT}
  $gPythonInterp exec {import numpy as np}
  $gPythonInterp exec {X = np.ndarray.tolist(np.array([[3.1,4.3,5.3],[1.1,6.1,8.5]]))}
  $gPythonInterp exec {Y = np.ndarray.tolist(np.array([[3.1,4.3,5.3],[1.1,6.1,8.5]]))}
  $gPythonInterp exec {Z = np.ndarray.tolist(np.array([[3.1,4.3,5.3],[1.1,6.1,8.5]]))}

  $gPythonInterp exec {uK = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {vK = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {uM = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {vM = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {pyOCCT.convertListsToOCCT("myNewObj",X,Y,Z,uK,vK,uM,vM,2,2)}
}
