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

proc guiSV_model_remesh_polydata_mmg {} {
  global gObjects
  global guiMMGvars
  global symbolicName
  global gOptions
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Can only smooth one PolyData model at a time"
  }
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid kernel must be PolyData for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  guiSV_model_add_to_backup_list $kernel $model

  set hmax $guiMMGvars(hmax)
  set hmin $guiMMGvars(hmin)
  set angle $guiMMGvars(angle)
  set hgrad $guiMMGvars(hgrad)
  set hausd $guiMMGvars(hausd)

  set pd /models/PolyData/$model
  if {![repos_exists -obj $pd]} {
    $model GetPolyData -result $pd
  }
  set remesh_pd [polydata_remesh_mmg $pd $hmax $hmin $hausd $angle $hgrad]

  $model SetVtkPolyData -obj $remesh_pd

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

proc polydata_remesh_mmg {pd hmax hmin hausd angle hgrad} {

  set outpd /tmp/remesh/mmg/pd
  catch {repos_delete -obj $outpd}

  mmg_remesh -src $pd -dst $outpd -hmax $hmax -hmin $hmin -hausd $hausd -angle $angle -hgrad $hgrad

  return $outpd
}
