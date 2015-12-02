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
