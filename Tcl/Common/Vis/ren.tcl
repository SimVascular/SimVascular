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

# -------------
# ::vis::renNew
# -------------

proc ::vis::renNew {renWin} {

    #@author Nathan Wilson
    #@c Create a new vtkRenderer in the given vtkRenderWindow.  If the named
    #@c vtkRenderWindow does not exist, then create it.  Multiple renderers
    #@c can be contained in a particular render window.  These renderers are
    #@c named with a sequentially assigned identifier.
    #@a renWin: Existing render window, or name of render window to
    #@a renWin: to be created.

    if {[info commands $renWin] == ""} {
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


# -----------------------
# ::vis::renSetBackground
# -----------------------

proc ::vis::renSetBackground {ren r g b} {

    #@author Nathan Wilson
    #@c Set the background color of the given
    #@c renderer.
    #@a ren: renderer
    #@a r: red (value between 0 and 1)
    #@a g: green (value between 0 and 1)
    #@a b: blue (value between 0 and 1)
    
    $ren SetBackground $r $g $b
    ::vis::render $ren

}


# ---------------
# ::vis::renReset
# ---------------

proc ::vis::renReset {ren} {

  #@author Nathan Wilson
  #@c Reset the viewpoint of the renderer.
  #@a ren: renderer

  $ren ResetCamera
  ::vis::render $ren

}


# --------------------
# ::vis::renSetFlatAll
# --------------------

proc ::vis::renSetFlatAll {ren} {

    #@author Nathan Wilson
    #@c Set the interpolation mode of all actors
    #@c displayed by given renderer to flat
    #@a ren: renderer

    set actors [$ren GetActors]
    $actors InitTraversal
    while {[llength [set a [$actors GetNextItem]]] > 0} {
	[$a GetProperty] SetInterpolationToFlat
    }
    ::vis::render $ren

}


# -----------------------
# ::vis::renSetGouraudAll
# -----------------------

proc ::vis::renSetGouraudAll {ren} {

    #@author Nathan Wilson
    #@c Set interpolation to Gouraud shading
    #@c for all actors displayed in given renderer.
    #@a ren: renderer

    set actors [$ren GetActors]
    $actors InitTraversal
    while {[llength [set a [$actors GetNextItem]]] > 0} {
	[$a GetProperty] SetInterpolationToGouraud
    }
    ::vis::render $ren

}


# ---------------------
# ::vis::renSetPhongAll
# ---------------------

proc ::vis::renSetPhongAll {ren} {

    #@author Nathan Wilson
    #@c Set interpolation to Phong shading for all
    #@c for all actors in given renderer.
    #@a ren: renderer

    set actors [$ren GetActors]
    $actors InitTraversal
    while {[llength [set a [$actors GetNextItem]]] > 0} {
	[$a GetProperty] SetInterpolationToPhong
    }
    ::vis::render $ren

}


# ----------------------------
# ::vis::renParallelProjection
# ----------------------------

proc ::vis::renParallelProjection {ren} {

  #@author Nathan Wilson
  #@c Turn on parallel projection for given 
  #@c renderer.
  #@a ren: renderer

  [$ren GetActiveCamera] ParallelProjectionOn
  ::vis::render $ren

}


# -------------------------------
# ::vis::renPerspectiveProjection
# -------------------------------

proc ::vis::renPerspectiveProjection {ren} {

  #@author Nathan Wilson
  #@c Turn on perspective projection for 
  #@c given renderer
  #@a ren: renderer

  [$ren GetActiveCamera] ParallelProjectionOff
  ::vis::render $ren

}


# -------------------
# ::vis::renWriteJPEG
# -------------------

proc ::vis::renWriteJPEG {ren args} {

  #@author Nathan Wilson
  #@c Save the current window image to a file
  #@c in jpeg format.  If no filename is specified,
  #@c then an interactive window is popped up asking
  #@c for a name to be given.
  #@a ren: renderer
  #@a args: filename

  if {$args == ""} {
    # prompt the user for a filename
    set filename [tk_getSaveFile -filetypes {{{JPEG image file} {.jpg .JPG .jpeg .JPEG}} {{all files} *} } -title "Save Window As JPEG" -defaultextension .jpg]
  } else {
    set filename $args
  }
  if {$filename == ""} {return}

  set w2i [tmpobj]
  set jwriter [tmpobj]
 
  vtkJPEGWriter $jwriter
  $jwriter ProgressiveOff
  $jwriter SetQuality 95

  [$ren GetRenderWindow] Render

  vtkWindowToImageFilter $w2i
  $w2i SetInput [$ren GetRenderWindow]
  $w2i Update

  $jwriter SetInput [$w2i GetOutput]
  $jwriter SetFileName $filename
  $jwriter Write

  $w2i Delete
  $jwriter Delete

}


# ------------------
# ::vis::renAddActor
# ------------------

proc ::vis::renAddActor {ren actor} {

  #@author Nathan Wilson
  #@c Add the specified actor to the given 
  #@c renderer.
  #@a ren: renderer
  #@a actor: vtkActor object
  #@warning should check here to see if
  #@warning actor has already been added

  $ren AddActor $actor

}

# -----------------
# ::vis::renRmActor
# -----------------

proc ::vis::renRmActor {ren actor} {

  #@author Nathan Wilson
  #@c Remove given actor from renderer.
  #@a ren: renderer
  #@a actor: vtkActor object.

  $ren RemoveActor $actor
}


# -----------------
# ::vis::renAddProp
# -----------------

proc ::vis::renAddProp {ren prop} {

  #@author Nathan Wilson
  #@c Add given prop to renderer
  #@a ren: renderer
  #@a prop: vtkProp object

  $ren AddProp $prop
}


# ----------------
# ::vis::renRmProp
# ----------------

proc ::vis::renRmProp {ren prop} {

  #@author Nathan Wilson
  #@c Remove given prop from renderer
  #@a ren: renderer
  #@a prop:  vtkProp object

  $ren RemoveProp $prop

}


# -------------
# ::vis::render
# -------------

proc ::vis::render {ren} {

  #@author Nathan Wilson
  #@c Render the current window.
  #@note If the renderer is frozen, do not
  #@note update display.
  #@a ren: renderer

  if {$::vis::frozen($ren) == 1} {
    return
  }

  ::vis::renderWindow [$ren GetRenderWindow]

}


# ----------------
# ::vis::renFreeze
# ----------------

proc ::vis::renFreeze {ren} {

  #@author Nathan Wilson
  #@c Freeze the current window.
  #@a ren: renderer

  set ::vis::frozen($ren) 1

}


# ------------------
# ::vis::renUnfreeze
# ------------------

proc ::vis::renThaw {ren} {

  #@author Nathan Wilson
  #@c unfreeze current window
  #@a ren: renderer

  if {$::vis::frozen($ren) == 1} {
    set ::vis::frozen($ren) 0
    ::vis::renderWindow [$ren GetRenderWindow]
  }

}


# -------------------
# ::vis::renderWindow
# -------------------

proc ::vis::renderWindow {win} {

  #@author Nathan Wilson
  #@c Render the current window.
  #@a win:  window name
 
  $win Render

}
