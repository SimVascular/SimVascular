proc startTclpython {} {
  if [catch {load Lib/liblib_simvascular_tclpython.dylib Tclpython} msg] {
    return -code error "ERROR: Error loading Tclpython: $msg"
  }
  global gPythonInterp
  set gPythonInterp [::python::interp new]
  $gPythonInterp exec {print("Hello Python World")}
}

proc killTclpython {} {
  global gPythonInterp
  ::python::interp delete $gPythonInterp
}

proc runPythonScript {script args} {
  global gPythonInterp

  $gPythonInterp exec {import $script}
  $gPythonInterp exec {$script.main([$args])}
}

proc testPythonModule {} {
  global gPythonInterp

  puts "Initiate the python modules from tcl"
  $gPythonInterp exec {tcl.eval("repos_initPyMods")}
  $gPythonInterp exec {import vtk}
  $gPythonInterp exec {import pythonc}
  $gPythonInterp exec {pythonc.pyRepos_hello("dummy")}
  $gPythonInterp exec {pythonc.pyRepos_accessHash("dummy")}
}

proc testExport {} {

  global gPythonInterp
  $gPythonInterp exec {vtkobj = pythonc.pyRepos_exportToVtk("/group/left_sup_renal/217")}
}

proc testImport {} {
  global gPythonInterp
  $gPythonInterp exec {vtkobj = vtk.vtkPolyData()}
  $gPythonInterp exec {pythonc.pyRepos_importVtkPd(vtkobj,"myobj")}
}

