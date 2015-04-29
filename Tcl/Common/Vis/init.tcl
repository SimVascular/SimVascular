# Copyright (c) 2009-2011 Open Source Medical Software Corporation, 
# University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
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

# -------------------
# ::vis::deleteWindow
# -------------------

proc ::vis::deleteWindow {window ren} {

  #@author Nathan Wilson
  #@c Delete window displaying single renderer
  #@a window: window to be destroyed
  #@a ren: renderer to be destroyed

  # clear all displayed objects
  ::vis::imgRmAll $ren
  ::vis::polyRmAll $ren
  ::vis::actorRmAll $ren

  # delete generic interactors
  genericInteractor_$ren Delete
  genericInteractorStyle_$ren Delete
  $window Delete
  destroy .$window
  $ren Delete

}


# ---------------
# ::vis::initTKgr
# ---------------

proc ::vis::initTKgr {renWin title specialCmd deleteWindowCmd} {

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
    wm protocol .$renWin WM_DELETE_WINDOW [list ::vis::deleteWindow $renWin $ren]
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
    -command "::vis::deletePickedActor $ren" \
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
    -command "::vis::renWriteJPEG $ren" \
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

  set ::vis::frozen($ren) 0

  return $ren

}


proc ::vis::deletePickedActor {ren} {

  #@author Nathan Wilson
  #@c Deletes the currently selected actor from the renderer
  #@a ren: renderer

  ::vis::tkDeselectPickedActor
  ::vis::actorRm $::vis::tkvars(PickedAssemblyRenderer) $::vis::tkvars(PickedAssembly)

}
