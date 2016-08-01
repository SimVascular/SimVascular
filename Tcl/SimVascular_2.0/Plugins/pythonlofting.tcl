proc call_python_lofting {groupName kutype kvtype putype pvtype uDeg vDeg Du0 DuN Dv0 DvN cap resample_num} {
  global gOptions
  global gPythonInterp
  global gPathBrowser

  set saveGroup          0
  resampleGroupEvenSpace $groupName $resample_num $saveGroup

  #Make sure numpy module exists
  if [catch {$gPythonInterp exec {import pyOCCT}} errmsg] {
    return -code error "ERROR: SimVascular pyOCCT module does not exist. Must have python and OCCT"
  }
  if [catch {$gPythonInterp exec {import numpy as np}} errmsg] {
    return -code error "ERROR: numpy module does not exist. Must install numpy (http://docs.scipy.org/doc/numpy-1.10.1/user/install.html)"
  }
  #Pass lofting info to python
  global TclPyString
  $gPythonInterp exec {tcl.eval('global TclPyString')}
  set TclPyString $groupName
  $gPythonInterp exec {file=tcl.eval('set dummy $TclPyString')}
  set TclPyString $kutype
  $gPythonInterp exec {kutype=tcl.eval('set dummy $TclPyString')}
  set TclPyString $kvtype
  $gPythonInterp exec {kvtype=tcl.eval('set dummy $TclPyString')}
  set TclPyString $putype
  $gPythonInterp exec {putype=tcl.eval('set dummy $TclPyString')}
  set TclPyString $pvtype
  $gPythonInterp exec {pvtype=tcl.eval('set dummy $TclPyString')}
  set TclPyString $uDeg
  $gPythonInterp exec {uDeg=int(tcl.eval('set dummy $TclPyString'))}
  set TclPyString $vDeg
  $gPythonInterp exec {vDeg=int(tcl.eval('set dummy $TclPyString'))}
  set TclPyString $resample_num
  $gPythonInterp exec {numPtsPerSeg=int(tcl.eval('set dummy $TclPyString'))}
  set TclPyString $Du0
  $gPythonInterp exec {Du0=[float(i) for i in (tcl.eval('set dummy $TclPyString')).split()]}
  set TclPyString $DuN
  $gPythonInterp exec {DuN=[float(i) for i in (tcl.eval('set dummy $TclPyString')).split()]}
  set TclPyString $Dv0
  $gPythonInterp exec {Dv0=[float(i) for i in (tcl.eval('set dummy $TclPyString')).split()]}
  set TclPyString $DvN
  $gPythonInterp exec {DvN=[float(i) for i in (tcl.eval('set dummy $TclPyString')).split()]}

  #Get group in python and then pass points to lofting function
  set sortedList [group_get $groupName]
  set TclPyString [llength $sortedList]
  $gPythonInterp exec {numSegs=int(tcl.eval('set dummy $TclPyString'))}
  $gPythonInterp exec {allpoints=np.zeros((numSegs,numPtsPerSeg,3))}
  $gPythonInterp exec {i=0}
  foreach profile $sortedList {
    puts "Processing profile $profile"
    set TclPyString $profile/sample
    $gPythonInterp exec {newvtk  = tcl.eval('set dummy $TclPyString')}
    $gPythonInterp exec {vtkProf = pySolid.exportToVtk(newvtk)}
    for {set j 0} {$j < $resample_num} {incr j} {
      set TclPyString $j
      $gPythonInterp exec {j=int(tcl.eval('set dummy $TclPyString'))}
      $gPythonInterp exec {pt=vtkProf.GetPoint(j)}
      $gPythonInterp exec {allpoints[i][j][0] = pt[0]}
      $gPythonInterp exec {allpoints[i][j][1] = pt[1]}
      $gPythonInterp exec {allpoints[i][j][2] = pt[2]}
    }
    $gPythonInterp exec {i=i+1}
  }

  puts "Calling python lofting code"
  #Import python lofting code and call
  $gPythonInterp exec {loftfile = os.path.join(simvascular_home,'Python/model/occt/nurbs_lofting.py')}
  $gPythonInterp exec {pyloft = imp.load_source('nurbs_lofting', loftfile)}
  $gPythonInterp exec {X,Y,Z,uknots,vknots,u,v,p,q = pyloft.loft(["-a",allpoints,"-p",uDeg,"-q",vDeg,"--kutype",kutype,"--kvtype",kvtype,"--putype",putype,"--pvtype",pvtype,"-o",Du0,"-n",DuN,"-r",Dv0,"-s",DvN])}

  #Pass python lofting code surface to c++ which will make a occt model
  set kernel                          "OpenCASCADE"
  set basename                        [file tail $groupName]
  set gOptions(meshing_solid_kernel)  $kernel
  set pysolid                         /python/lofted/$basename

  solid_setKernel -name $kernel
  catch {repos_delete -obj $pysolid}
  solid_newObject -name $pysolid
  set TclPyString $pysolid

  puts "Converting to python lists and storing"
  $gPythonInterp exec {Xl = np.ndarray.tolist(X)}
  $gPythonInterp exec {Yl = np.ndarray.tolist(Y)}
  $gPythonInterp exec {Zl = np.ndarray.tolist(Z)}

  $gPythonInterp exec {uK = np.ndarray.tolist(uknots)}
  $gPythonInterp exec {vK = np.ndarray.tolist(vknots)}
  $gPythonInterp exec {uM = np.ndarray.tolist(u)}
  $gPythonInterp exec {vM = np.ndarray.tolist(v)}
  $gPythonInterp exec {newmod = tcl.eval('set dummy $TclPyString')}
  $gPythonInterp exec {pyOCCT.convertListsToOCCT(newmod,Xl,Yl,Zl,uK,vK,uM,vM,p,q)}

  #Cap the solid now
  catch {repos_delete -obj $pysolid/capped}
  if {$cap} {
    solid_capSurfToSolid -src $pysolid -dst $pysolid/capped
  } else {
    solid_copy -src $pysolid -dst $pysolid/capped
  }

  # ugly way to keep track of solids created for each group
  global gLoftedSolids
  set gLoftedSolids($groupName) $pysolid/capped

  foreach i [$pysolid/capped GetFaceIds] {
    set facename {}
    $pysolid/capped SetFaceAttr -attr parent -faceId $i -value $groupName
  }
  $pysolid/capped Print

  ##Display the result
  #global gRen3d
  #catch {repos_delete -obj /python/lofted/$basename/vtk}
  #/python/lofted/$basename GetPolyData -result /python/lofted/$basename/vtk

  #catch {vis_pRm $gRen3d /python/lofted/$basename/vtk}
  #generalView $gRen3d /python/lofted/$basename/vtk
}
