#Creates a python interpreter and stores in global gPythonInterp
proc startTclPython {} {
  global env
  if [catch {load Lib/liblib_simvascular_tclpython.dylib Tclpython} msg] {
    return -code error "ERROR: Error loading Tclpython: $msg"
  }
  global gPythonInterp
  set gPythonInterp [::python::interp new]
  $gPythonInterp exec {print("Hello Python World")}

  #Create vtkPolyData to pass string between tcl and python
  catch {stringPd Delete}
  catch {repos_delete -obj STRING}
  vtkPolyData stringPd
  repos_importVtkPd -src stringPd -dst STRING
  repos_setstring -obj STRING -str $env(SIMVASCULAR_HOME)
  $gPythonInterp exec {simvascular_home = tcl.eval('repos_getstring -obj STRING')}
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
  $gPythonInterp exec {import numpy as np}
  $gPythonInterp exec {X = np.ndarray.tolist(np.array([[3.1,4.3,5.3],[1.1,6.1,8.5]]))}
  $gPythonInterp exec {Y = np.ndarray.tolist(np.array([[3.1,4.3,5.3],[1.1,6.1,8.5]]))}
  $gPythonInterp exec {Z = np.ndarray.tolist(np.array([[3.1,4.3,5.3],[1.1,6.1,8.5]]))}

  $gPythonInterp exec {uK = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {vK = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {uM = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {vM = np.ndarray.tolist(np.array([3.0,4.0,51.0,6.0,8.0]))}
  $gPythonInterp exec {pySolid.convertListsToOCCT("myNewObj",X,Y,Z,uK,vK,uM,vM,2,2)}
}
