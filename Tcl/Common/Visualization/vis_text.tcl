#===========================================================================
#    
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                           University of California, San Diego.
#
# All rights reserved.
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

# -------------
# vis_textSetup
# -------------

proc vis_textSetup {ren} {

    if {[vis_textExists $ren]} {
	return
    }

    # Set up a static pipeline:

    vtkTextMapper vis_text_mapper_$ren
    vis_text_mapper_$ren SetInput ""

    vtkTextProperty vis_text_prop_$ren
    vis_text_prop_$ren SetFontSize 18
    vis_text_prop_$ren SetFontFamilyToArial
    vis_text_prop_$ren BoldOn
    vis_text_prop_$ren ItalicOn
    vis_text_prop_$ren ShadowOff
    vis_text_mapper_$ren SetTextProperty vis_text_prop_$ren

    vtkActor2D vis_text_actor_$ren
    vis_text_actor_$ren SetMapper vis_text_mapper_$ren
    vis_text_actor_$ren SetPosition 10 10
    vis_text_actor_$ren VisibilityOn
    [vis_text_actor_$ren GetProperty] SetColor 0 1 1

    $ren AddActor2D vis_text_actor_$ren
    vis_render $ren
}


# --------------
# vis_textExists
# --------------

proc vis_textExists {ren} {
    set m vis_text_mapper_$ren
    if {[cmdExists $m]} {
	return 1
    }
    return 0
}


# -----------
# vis_textSet
# -----------

proc vis_textSet {ren txt} {
    if {![vis_textExists $ren]} {
	vis_textSetup $ren
    }
    vis_text_mapper_$ren SetInput $txt
    vis_render $ren
}


# -----------
# vis_textClr
# -----------

proc vis_textClr {ren} {
    vis_text_mapper_$ren SetInput ""
    vis_render $ren
}


# ----------
# vis_textRm
# ----------

proc vis_textRm {ren} {

    if {![vis_textExists $ren]} {
	return
    }
    vis_objRmAll $ren __vis_text_roots
    vis_render $ren
    return
}


# ----------------
# vis_textSetColor
# ----------------

proc vis_textSetColor {ren r g b} {
    set act vis_text_actor_$ren
    if {[cmdExists $act]} {
	[$act GetProperty] SetColor $r $g $b
	vis_render $ren
    }
}	
