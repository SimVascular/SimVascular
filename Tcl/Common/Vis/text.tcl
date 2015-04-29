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

# -----------
# ::vis::text
# -----------

proc ::vis::text {ren position str} {

    #@author Nathan Wilson
    #@c Add text to current render window as overlay
    #@a ren: renderer
    #@a position: tcl list specifying text location
    #@a position: (if empty list then default of 10 10 is used)
    #@a str: text string (can be blank)

    if {$position == ""} {
      set position "10 10"
    }

    set tag [::vis::textTag $ren $position]

    ::vis::textRm $ren $position

    set map [::vis::newobj text_map_$tag]
    set act [::vis::newobj text_act2D_$tag]
    set prop [::vis::newobj text_prop_$tag]  

    vtkTextMapper $map
    $map SetInput $str

    vtkTextProperty $prop
    $prop SetFontSize 18
    $prop SetFontFamilyToArial
    $prop BoldOn
    $prop ItalicOn
    $prop ShadowOff
    $prop SetColor 1 0 0

    $map SetTextProperty $prop

    vtkActor2D $act
    $act SetMapper $map
    $act SetPosition [lindex $position 0] [lindex $position 1]
    $act VisibilityOn

    $ren AddActor2D $act
    ::vis::render $ren

}


# --------------
# ::vis::textTag
# --------------

proc ::vis::textTag {ren position} {

    #author Nathan Wilson
    #@c Create the unique tag for text objs
    #@a ren: renderer
    #@a position: corner position of text

    if {$position == ""} {
	set position [list 10 10]
    }

    if {[llength $position] != 2} {
      return -code error "ERROR:  incorrent position ($position)"
    }

    set tag "$ren\_[lindex $position 0]\_[lindex $position 1]"

    return $tag

}


# ---------------------
# ::vis::textUpdateText
# ---------------------

proc ::vis::textUpdateText {ren position str} {

    #@author Nathan Wilson
    #@c Update the text for an existing actor
    #@a ren: renderer
    #@a position: position
    #@a str: text string to display

    set tag [::vis::textTag $ren $position]

    set map [::vis::getobj text_map_$tag]
    $map SetInput $str
    ::vis::render $ren

}


# -------------
# ::vis::textRm
# -------------

proc ::vis::textRm {ren position} {

    #@c Remove the text actor from the renderer
    #@a ren: Renderer
    #@a position:  position of text

    set tag [::vis::textTag $ren $position]

    catch {::vis::textUnshow $ren $position}

    ::vis::rmobj text_map_$tag
    ::vis::rmobj text_act2D_$tag
    ::vis::rmobj text_prop_$tag  

}


# ----------------
# ::vis::textRmAll
# ----------------

proc ::vis::textRmAll {ren} {
  
  #@author Nathan Wilson
  #@c Remove all text actors from renderer
  #@a ren: renderer

  ::vis::textUnshowAll $ren

  ::vis::rmall text_map_$ren
  ::vis::rmall text_act2D_$ren
  ::vis::rmall text_prop_$ren  

}


# ---------------
# ::vis::textShow
# ---------------

proc ::vis::textShow {ren position} {

    #@author Nathan Wilson
    #@c Show the text given by position in the renderer
    #@a ren: renderer
    #@a position: location of text

    ::vis::textUnshow $ren $position
    set tag [::vis::textTag $ren $position]
    set actor [::vis::getobj text_act2D_$tag]
    $ren AddActor $actor
    ::vis::render $ren

}


# -----------------
# ::vis::textUnshow
# -----------------

proc ::vis::textUnshow {ren position} {

    #@author Nathan Wilson
    #@c Remove the text at position from the renderer
    #@a ren: renderer
    #@a position: location of text

    set tag [::vis::textTag $ren $position]

    set actor [::vis::getobj text_act2D_$tag]

    ::vis::actor2DRm $actor

}


# --------------------
# ::vis::textUnshowAll
# --------------------

proc ::vis::textUnshowAll {ren} {

    #@author Nathan Wilson
    #@c Remove all text from given renderer
    #@a ren: renderer

    set allnames [::vis::getall text_act2D_$ren\_]

    foreach name $allnames {      
      set actor [::vis::getobj $name]
      ::vis::actor2DRm $ren $actor
    }

}


# -------------------
# ::vis::textSetColor
# -------------------

proc ::vis::textSetColor {ren position r g b} {

    #@author Nathan Wilson
    #@c set the color of text in renderer
    #@a ren: renderer
    #@a position: text position
    #@a r: red
    #@a g: green
    #@a b: blue

    set tag [::vis::textTag $ren $position]
    set prop [::vis::getobj text_prop_$tag]
    $prop SetColor $r $g $b
    ::vis::render $ren

}	


# ----------------------
# ::vis::textSetFontSize
# ----------------------

proc ::vis::textSetFontSize {ren position size} {

    #@author Nathan Wilson
    #@c Change current font size
    #@a ren: renderer
    #@a position: tcl list specifying text location
    #@a position: (if empty list then default of 10 10 is used)
    #@a size: text size (default is 18)

    if {$position == ""} {
      set position "10 10"
    }

    set tag [::vis::textTag $ren $position]

    set prop [::vis::getobj text_prop_$tag]  
    $prop SetFontSize $size

    ::vis::render $ren

}
