#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
#
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#===========================================================================    

proc vis_init {} {

  # @author Ken Wang
  # @c Initialize Visualization Layer.

  # 8-bit grayscale lookup table
  # ----------------------------
  vtkLookupTable g8bitGrayscaleLUT
    g8bitGrayscaleLUT SetHueRange 0.0 0.0
    g8bitGrayscaleLUT SetSaturationRange 0.0 0.0
    g8bitGrayscaleLUT SetValueRange 0.0 1.0
#    g8bitGrayscaleLUT SetNumberOfColors 256
    g8bitGrayscaleLUT SetNumberOfColors 16384
    g8bitGrayscaleLUT Build

  # Red-to-blue colormap
  # --------------------
  vtkLookupTable gBlueToRedLUT
    gBlueToRedLUT SetHueRange 0.6667 0.0
    gBlueToRedLUT SetSaturationRange 1.0 1.0
    gBlueToRedLUT SetValueRange 1.0 1.0
    gBlueToRedLUT SetAlphaRange 1.0 1.0
#    gBlueToRedLUT SetNumberOfColors 256
    gBlueToRedLUT SetNumberOfColors 16384
    gBlueToRedLUT Build

}



# ----------
# vis_renNew
# ----------
# Create a new vtkRenderer in the given vtkRenderWindow.  If the named
# vtkRenderWindow does not exist, then create it.  Multiple renderers
# can be contained in a particular render window.  These renderers are
# named with a sequentially assigned identifier.

proc vis_renNew {renWin} {

    if { ![cmdExists $renWin] } {
        set renId 1
        vtkRenderWindow $renWin
        set iren [format "iren_%s" $renWin]
        vtkRenderWindowInteractor $iren
        $iren SetRenderWindow $renWin
        $iren SetUserMethod { wm deiconify .vtkInteract }
        $iren Initialize

    } else {
        set numRens [[$renWin GetRenderers] GetNumberOfItems]
        set renId [expr $numRens + 1]
    }

    set ren [format "%s_ren%d" $renWin $renId]
    vtkRenderer $ren

    # Multiple renderers in the same vtkRenderWindow require us to
    # think about viewports, which I haven't done yet.

    $renWin AddRenderer $ren
    $ren SetBackground 0 0 0

    $renWin SetSize 320 320

    return $ren
}


# --------------------
# vis_renSetBackground
# --------------------

proc vis_renSetBackground {ren r g b} {

    if { ![cmdExists $ren] } {
        return
    }

    set allren $ren

    global gRen3d
    global gRen3dCopies
    if {$ren == $gRen3d} {
      set allren "$ren $gRen3dCopies"
    }

    foreach ren $allren {
      $ren SetBackground $r $g $b
      vis_render $ren
    }

}


# -------------------------------
# vis_renSetBackgroundChooseColor
# -------------------------------

proc vis_renSetBackgroundChooseColor {ren} {

    if { ![cmdExists $ren] } {
        return
    }

    set color [tk_chooseColor]
    if {$color == ""} {
      return
    }

    set rgb [vis_utilsGetRGBfromColorString $color]

    vis_renSetBackground $ren [lindex $rgb 0] [lindex $rgb 1] [lindex $rgb 2]

}


# ------------
# vis_renReset
# ------------

proc vis_renReset {ren} {
  $ren ResetCamera
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      $ren ResetCamera
    }
  }
}


# ---------------
# vis_flatShading
# ---------------

proc vis_flatShading {ren} {
    set actors [$ren GetActors]
    $actors InitTraversal
    while {[llength [set a [$actors GetNextItem]]] > 0} {
	[$a GetProperty] SetInterpolationToFlat
    }
    [$ren GetRenderWindow] Render
}


# ------------------
# vis_gouraudShading
# ------------------

proc vis_gouraudShading {ren} {
    set actors [$ren GetActors]
    $actors InitTraversal
    while {[llength [set a [$actors GetNextItem]]] > 0} {
	[$a GetProperty] SetInterpolationToGouraud
    }
    [$ren GetRenderWindow] Render
}


# ----------------
# vis_phongShading
# ----------------

proc vis_phongShading {ren} {
    set actors [$ren GetActors]
    $actors InitTraversal
    while {[llength [set a [$actors GetNextItem]]] > 0} {
	[$a GetProperty] SetInterpolationToPhong
    }
    [$ren GetRenderWindow] Render
}


# -----------------
# vis_renSaveCamera
# -----------------

proc vis_renSaveCamera {ren arr} {
    upvar $arr a
    set cam [$ren GetActiveCamera]
    set setMethods [FindSetMethods $cam]
    set getMethods [FindGetMethods $cam]
    foreach gm $getMethods {
	set root [string range $gm 1 end]
	set target [format "S%s" $root]
	set id [lsearch -exact $setMethods $target]
	if {$id >= 0} {
	    set sm [lindex $setMethods $id]
	    catch {set a($root) [$cam $gm]}
	}
    }
}


# --------------------
# vis_renRestoreCamera
# --------------------
# There seems to be some additional issue relating to lights which
# is not handled by this Save/Restore pair.

proc vis_renRestoreCamera {ren arr} {
    upvar $arr a
    set cam [$ren GetActiveCamera]
    set roots [array names a]
    foreach r $roots {
	set prefix [format "%s S%s" $cam $r]
	set cmd [buildCmd $prefix $a($r)]
	eval $cmd
    }
    [$ren GetRenderWindow] Render
}


# --------------------
# vis_renSaveCamToFile
# --------------------

proc vis_renSaveCamToFile {ren fn} {
    if {[file exists $fn]} {
	return -code error "File $fn exists."
    }
    vis_renSaveCamera $ren camera
    set fp [open $fn w]
    set names [array names camera]
    foreach n $names {
	puts $fp [format "%s %s" $n $camera($n)]
    }
    close $fp
}


# -------------------------
# vis_renRestoreCamFromFile
# -------------------------

proc vis_renRestoreCamFromFile {ren fn} {
    if {![file exists $fn]} {
	return -code error "File $fn not found."
    }
    set fp [open $fn r]
    while {[gets $fp line] >= 0} {
	set toks [cleanList [split $line]]
	if {[llength $toks] == 0} {
	    continue
	}
	if {[string range [lindex $toks 0] 0 0] == "#"} {
	    continue
	}
	set name [lindex $toks 0]
	set camera($name) [lrange $toks 1 end]
    }
    close $fp
    vis_renRestoreCamera $ren camera
}


# -------------------------
# vis_renParallelProjection
# -------------------------

proc vis_renParallelProjection {ren} {
  [$ren GetActiveCamera] ParallelProjectionOn
  [$ren GetRenderWindow] Render
  return
}


# ----------------------------
# vis_renPerspectiveProjection
# ----------------------------

proc vis_renPerspectiveProjection {ren} {
  [$ren GetActiveCamera] ParallelProjectionOff
  [$ren GetRenderWindow] Render
  return
}


# ---------
# vis_objRm
# ---------

proc vis_objRm {ren glist name} {
    
    upvar #0 $glist varList

    set tag [format "%s_%s" $ren $name]
    set actors [$ren GetActors]

    foreach r $varList {
	set obj [format "%s_%s" $r $tag]
	if {[cmdExists $obj]} {
	    set type [$obj GetClassName]
	    if {[string match "vtk*Actor" $type]} {
		if {![catch {$actors IsItemPresent $obj} msg]} {
		    $actors RemoveItem $obj
		} else {
		    puts $msg
		}
	    }
	    $obj Delete
	}
    }
    vis_renReset $ren
}


# ------------
# vis_objRmAll
# ------------

proc vis_objRmAll {ren glist} {
    
    upvar #0 $glist varList

    set actors [$ren GetActors]
    foreach r $varList {
	set extRoot [format "%s_%s" $r $ren]
	set cmds [findCmds $extRoot]
	foreach obj $cmds {
	    set type [$obj GetClassName]
	    if {[string match "vtk*Actor" $type]} {
		if {![catch {$actors IsItemPresent $obj} msg]} {
		    $actors RemoveItem $obj
		} else {
		    puts $msg
		}
	    }
	    $obj Delete
	}
    }
#    vis_renReset $ren
}


# ----------
# vis_initgr
# ----------
# Better name for the now-entrenched initgr.

proc vis_initgr {title} {
    return [initgr $title]
}


# ---
# crd
# ---
# Clear repository objects from all renderers.

proc crd {} {
    set rs [vtkRenderer ListInstances]
    foreach r $rs {
	crd_ren $r
    }
}


# -------
# crd_ren
# -------
# Clear repository objects from given renderer.

proc crd_ren {ren} {
  global gRen3dFreeze
  set oldFreeze $gRen3dFreeze
  set gRen3dFreeze 1
  catch {vis_pRmAll $ren}
  catch {vis_vRmAll $ren}
  catch {vis_nodeRmAll $ren}
  set gRen3dFreeze $oldFreeze
}


# -----------
# rmAllActors
# -----------
# Note that the way in which vtk deals with actors changes from v2.2
# to later versions.

proc rmAllActors {rendererName} {

    global $rendererName

    set actors [vtkActor ListInstances]
    foreach a $actors {
	$rendererName RemoveActor $a
    }
}


# ----------------
# vis_deleteWindow
# ----------------

proc vis_deleteWindow {window ren} {
  # clear all displayed objects
  catch {crd_ren $ren}

  # assume the window was created using vis_initTKgr!
  # delete generic interactors
  catch {genericInteractor_$ren Delete}
  catch {genericInteractorStyle_$ren Delete}
  catch {$window Delete}
  catch {destroy .$window}
  catch {$ren Delete}

  return GDSC_OK
}


# ------------
# vis_initTKgr
# ------------

proc vis_initTKgr {renWin title specialCmd deleteWindowCmd} {

  #@author Nathan Wilson
  #@c  Create a TK render widget containing
  #@c  a vtk render window.
  #@a  renWin:  unique toplevel name.
  #@a  title:  Title of window displayed by wm.
  #@a  specialCmd:  a special menu command to appear under
  #@a  specialCmd:  the "Window" heading.  Typically will
  #@a  specialCmd:  will contain a valid Tcl command firing
  #@a  specialCmd:  up an additional window-specific gui.
  #@a  deleteWindowCmd:  Command to be called when user
  #@a  deleteWindowCmd:  clicks on delete window icon.
  #@r  vtk render created.

  # if a null ptr is passed, disable special
  # menu item
  set specialState normal
  if {$specialCmd == ""} {
    set specialState disabled
  }

  # -------
  # Set ren
  # -------

  set renId 1
  vtkRenderWindow $renWin
  set ren [format "%s_ren%d" $renWin $renId]
  vtkRenderer $ren

  toplevel .$renWin
  wm title .$renWin $title

  if {$deleteWindowCmd != ""} {
    wm protocol .$renWin WM_DELETE_WINDOW $deleteWindowCmd
  } else {
    wm protocol .$renWin WM_DELETE_WINDOW [list vis_deleteWindow $renWin $ren]
  }

  frame .$renWin.f1

  vtkTkRenderWidget .$renWin.f1.r1 -width 320 -height 320 -rw $renWin
  BindTkRenderWidget .$renWin.f1.r1

  pack .$renWin.f1.r1 -side left -padx 3 -pady 3 -fill both -expand t
  pack .$renWin.f1  -fill both -expand t

  set renWin1 [.$renWin.f1.r1 GetRenderWindow]

  $renWin1 AddRenderer $ren
  $ren SetBackground 0 0 0

  # build widget .renWin.frame1
  frame .$renWin.frame1 \
    -borderwidth {0} \
    -height {30} \
    -relief {flat} \
    -width {30}

  # build widget .renWin.frame1.menubutton2
  menubutton .$renWin.frame1.menubutton2 \
    -activebackground {light green} \
    -activeforeground {blue} \
    -background {white} \
    -font {Helvetica 10} \
    -foreground {blue} \
    -menu ".$renWin.frame1.menubutton2.m" \
    -padx {7} \
    -pady {5} \
    -text {Actor}

  # build widget .renWin.frame1.menubutton2.m
  menu .$renWin.frame1.menubutton2.m \
    -activeborderwidth {1} \
    -borderwidth {1} \
    -font {Helvetica 10}
  .$renWin.frame1.menubutton2.m add command \
    -background {light green} \
    -command "global guiRWH;set guiRWH(render) $ren;guiLaunchRenWinHelper" \
    -label {Modify Actor Properties}
  .$renWin.frame1.menubutton2.m add command \
    -background {light green} \
    -command "vis_deletePickedActor $ren" \
    -label {Delete Actor}
  .$renWin.frame1.menubutton2.m add command \
    -background {light green} \
    -command "crd_ren $ren" \
    -label {Delete All Actors}

  # build widget .renWin.frame1.menubutton3
  menubutton .$renWin.frame1.menubutton3 \
    -activebackground {light green} \
    -background {white} \
    -font {Helvetica 10} \
    -foreground {blue} \
    -menu ".$renWin.frame1.menubutton3.m" \
    -padx {7} \
    -pady {5} \
    -text {Window}

  # build widget .renWin.frame1.menubutton3.m
  menu .$renWin.frame1.menubutton3.m \
    -activeborderwidth {1} \
    -borderwidth {1} \
    -font {Helvetica 10}
  .$renWin.frame1.menubutton3.m add command \
    -background {light green} \
    -command $specialCmd \
    -state $specialState \
    -label {Special}
  .$renWin.frame1.menubutton3.m add command \
    -background {light green} \
    -command "vis_renWriteJPEG $ren" \
    -state normal \
    -label {Save to File}
  .$renWin.frame1.menubutton3.m add command \
    -background {light green} \
    -command "crd_ren $ren" \
    -label {Clear Window}

  # pack master .renWin.frame1
  pack configure .$renWin.frame1.menubutton2 \
    -expand 1 \
    -fill both \
    -side left
  pack configure .$renWin.frame1.menubutton3 \
    -expand 1 \
    -fill both \
    -side left

  # pack master .renWin
  pack configure .$renWin.frame1 \
    -fill both

  # add generic interactors
  catch {genericInteractor_$ren Delete}
  catch {genericInteractorStyle_$ren Delete}
  vtkGenericRenderWindowInteractor genericInteractor_$ren
  vtkInteractorStyleSwitch genericInteractorStyle_$ren
  genericInteractorStyle_$ren SetCurrentStyleToTrackballCamera
  genericInteractor_$ren SetInteractorStyle genericInteractorStyle_$ren
  set iren genericInteractor_$ren
  $iren SetRenderWindow $renWin1

  return $ren

}


proc vis_renWriteJPEG {ren args} {

  global tcl_platform

  if {$args == ""} {
    # prompt the user for a filename
    set filename [tk_getSaveFile -filetypes {{{JPEG image file} {.jpg .JPG .jpeg .JPEG}} {{all files} *} } -title "Save Window As JPEG" -defaultextension .jpg]
  } else {
    set filename $args
  }
  if {$filename == ""} {return}

  set w2i tmp-vis_initSaveWindowImage-w2i
  set jwriter tmp-vis_initSaveWindowImage-jwriter
  catch {$w2i Delete}
  catch {$jwriter Delete}

  vtkJPEGWriter $jwriter
  $jwriter ProgressiveOff
  $jwriter SetQuality 95

  if {$tcl_platform(os) != "Darwin"} {
    [$ren GetRenderWindow] Render
    [$ren GetRenderWindow] OffScreenRenderingOn
  }

  vtkWindowToImageFilter $w2i
  $w2i SetInput [$ren GetRenderWindow]
  $w2i Update

  $jwriter SetInputDataObject [$w2i GetOutput]
  $jwriter SetFileName $filename
  $jwriter Write

  if {$tcl_platform(os) != "Darwin"} {
    [$ren GetRenderWindow] OffScreenRenderingOff
  }

  catch {$w2i Delete}
  catch {$jwriter Delete}

}


proc vis_deletePickedActor {ren} {

      global PickedAssembly PrePickedProperty
      global PickedAssemblyRenderer
      #puts "pre $PrePickedProperty"

      # ignore passed ren option, use renderer in which
      # actor was selected instead
      set ren $PickedAssemblyRenderer

      set a $PickedAssembly

      DeselectPickedActor

      $ren RemoveActor $a
      [$ren GetRenderWindow] Render
}


proc vis_renAddActor {ren actor} {

  $ren AddActor $actor
  #puts "ren : $ren"
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
        $ren AddActor $actor
    }
  }
}


proc vis_renRmActor {ren actor} {
  $ren RemoveActor $actor
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
        $ren RemoveActor $actor
    }
  }
}


proc vis_renAddProp {ren prop} {
  $ren AddViewProp $prop
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      $ren AddViewProp $prop
    }
  }
}


proc vis_renRmProp {ren prop} {
  $ren RemoveViewProp $prop
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      $ren RemoveViewProp $prop
    }
  }
}


proc vis_render {ren} {
  global gRen3d
  if {$ren == $gRen3d} {
    global gRen3dFreeze
    if {$gRen3dFreeze == 1} {
      return
    }
  }
  [$ren GetRenderWindow] Render
  global gRen3d
  global gRen3dCopies
  if {$ren == $gRen3d} {
    foreach ren $gRen3dCopies {
      [$ren GetRenderWindow] Render
    }
  }
}
