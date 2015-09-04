#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
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
#
#===========================================================================    

# Procedure: guiTGadaptMesh
proc guiTGadaptMesh {} {

  global gOptions
  global gFilenames
  global guiMMvars
  global guiTGvars

  set ascflag                $guiMMvars(phasta_format)
  set surface_mesh_file      $gFilenames(vtp_surface_file)
  set mesh_file              $gFilenames(mesh_file)
  set solution_file          [file join $guiMMvars(error_input_dir) $gFilenames(solution_file)] 
  #set error_file             [file join $guiMMvars(error_input_dir) $gFilenames(error_file)]
  set out_mesh_file          $gFilenames(adapted_mesh_file)
  set out_surface_mesh_file  $gFilenames(adapted_surface_mesh_file)
  set out_solution_file      $gFilenames(adapted_solution_file)
  set stepNumber             $guiMMvars(error_step_number)
  set selectedStrategy       1
  set numberVariables        5
  set reductionRatio         $guiMMvars(error_reduction_factor)
  set maxCoarseFactor        $guiMMvars(gsize)
  set maxRefineFactor        $guiMMvars(min_gsize)

  global gExternalPrograms
  set cvadapt $gExternalPrograms(cvtetadaptor)

  file delete [file join [pwd] adaptor_done_running]
  set fp [open [file join [pwd] run_adaptor.log] w]
  puts $fp "Start running adaptor..."
  puts $fp "exec $cvadapt -surface_mesh_file $surface_mesh_file\n -mesh_file $mesh_file\n -solution_file $solution_file\n -out_mesh_file $out_mesh_file\n -out_surface_mesh_file $out_surface_mesh_file\n -out_solution_file $out_solution_file\n  -out_sn $stepNumber\n -ratio $reductionRatio\n  -hmax $maxCoarseFactor\n -hmin $maxRefineFactor"
  close $fp

  catch {unset ::tail_adaptorlog}
  cancelTail [file join [pwd] run_adaptor.log] ::tail_adaptorlog
  set ::tail_adaptorlog {}

  tail [file join [pwd] run_adaptor.log] .+ 1000 ::tail_adaptorlog
  trace variable ::tail_adaptorlog w guiMMadaptMesh_handle

  catch {exec $cvadapt -surface_mesh_file $surface_mesh_file -mesh_file $mesh_file -solution_file $solution_file -out_mesh_file $out_mesh_file -out_surface_mesh_file $out_surface_mesh_file -out_solution_file $out_solution_file  -out_sn $stepNumber -ratio $reductionRatio  -hmax $maxCoarseFactor -hmin $maxRefineFactor &; } msg
  tk_messageBox -message "Launched adaptor in background!" -title "cvadaptor started" -icon info -type ok

  puts $msg
}

