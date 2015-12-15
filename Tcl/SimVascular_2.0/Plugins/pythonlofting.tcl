proc call_python_lofting {groupName kutype kvtype putype pvtype uDeg vDeg} {
  global gOptions
  global gPythonInterp

  #Initiate modules
  puts "Initiate the python modules from tcl"
  $gPythonInterp exec {tcl.eval("solid_initPyMods")}
  $gPythonInterp exec {import os}
  $gPythonInterp exec {import imp}
  $gPythonInterp exec {import vtk}
  $gPythonInterp exec {import pySolid}
  $gPythonInterp exec {import numpy as np}

  #Pass variables to python!
  repos_setstring -obj STRING -str $groupName
  $gPythonInterp exec {file = tcl.eval('repos_getstring -obj STRING')}
  repos_setstring -obj STRING -str $kutype
  $gPythonInterp exec {kutype = tcl.eval('repos_getstring -obj STRING')}
  repos_setstring -obj STRING -str $kvtype
  $gPythonInterp exec {kvtype = tcl.eval('repos_getstring -obj STRING')}
  repos_setstring -obj STRING -str $putype
  $gPythonInterp exec {putype = tcl.eval('repos_getstring -obj STRING')}
  repos_setstring -obj STRING -str $pvtype
  $gPythonInterp exec {pvtype = tcl.eval('repos_getstring -obj STRING')}
  repos_setstring -obj STRING -str $uDeg
  $gPythonInterp exec {uDeg = tcl.eval('repos_getstring -obj STRING')}
  repos_setstring -obj STRING -str $vDeg
  $gPythonInterp exec {vDeg = tcl.eval('repos_getstring -obj STRING')}

  #Import python lofting code and call
  $gPythonInterp exec {loftfile = os.path.join(simvascular_home,'../../Python/model/occt/nurbs_lofting.py')}
  $gPythonInterp exec {pyloft = imp.load_source('nurbs_lofting', loftfile)}
  $gPythonInterp exec {X,Y,Z,uknots,vknots,u,v,p,q = pyloft.main(["-i",file,"-p",uDeg,"-q",vDeg,"--kutype",kutype,"--kvtype",kvtype,"--putype",putype,"--pvtype",pvtype])}

  #Pass python lofting code surface to c++ which will make a occt model
  set kernel                          "OpenCASCADE"
  set basename                        [file tail $groupName]
  set gOptions(meshing_solid_kernel)  $kernel
  set pysolid                         /python/lofted/solid

  solid_setKernel -name $kernel
  catch {repos_delete -obj $pysolid}
  solid_newObject -name $pysolid
  repos_setstring -obj STRING -str $pysolid

  puts "Converting to python lists and storing"
  $gPythonInterp exec {Xl = np.ndarray.tolist(X)}
  $gPythonInterp exec {Yl = np.ndarray.tolist(Y)}
  $gPythonInterp exec {Zl = np.ndarray.tolist(Z)}

  $gPythonInterp exec {uK = np.ndarray.tolist(uknots)}
  $gPythonInterp exec {vK = np.ndarray.tolist(vknots)}
  $gPythonInterp exec {uM = np.ndarray.tolist(u)}
  $gPythonInterp exec {vM = np.ndarray.tolist(v)}
  $gPythonInterp exec {newmod = tcl.eval('repos_getstring -obj STRING')}
  $gPythonInterp exec {pySolid.convertListsToOCCT(newmod,Xl,Yl,Zl,uK,vK,uM,vM,p,q)}

  #Display the result
  global gRen3d
  catch {repos_delete -obj /python/lofted/vtk}
  /python/lofted/solid GetPolyData -result /python/lofted/vtk

  catch {vis_pRm $gRen3d /python/lofted/vtk}
  gdscGeneralView $gRen3d /python/lofted/vtk
}
