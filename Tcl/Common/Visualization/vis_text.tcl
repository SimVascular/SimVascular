# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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
