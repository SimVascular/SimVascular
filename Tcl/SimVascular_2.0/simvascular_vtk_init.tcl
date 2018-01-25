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

# Visualization Toolkit (VTK) Tcl package configuration.
#
#  assumes exact same build options in /SV16
#  assumes vtk 6.2

set not_loaded {}
set loaded {}

set all_vtk_kits {CommonCore CommonMath CommonMisc CommonSystem CommonTransforms CommonDataModel CommonColor CommonExecutionModel FiltersCore CommonComputationalGeometry FiltersGeneral ImagingCore ImagingFourier FiltersStatistics FiltersExtraction InfovisCore FiltersGeometry FiltersSources RenderingCore RenderingFreeType RenderingContextIID ChartsCore IOCore IOGeometry IOXMLParser IOXML DomainsChemistry IOLegacy ParallelCore FiltersAMR FiltersFlowPaths FiltersGeneric ImagingSources FiltersHybrid FiltersHyperTree ImagingGeneral FiltersImaging FiltersModeling FiltersParallel FiltersParallelImaging FiltersProgrammable FiltersSMP FiltersSelection FiltersTexture FiltersVerdict IOImage ImagingHybrid InfovisLayout InteractionStyle ImagingColor RenderingAnnotation RenderingVolume InteractionWidgets ViewsCore GeovisCore IOAMR IOEnSight IOExodus RenderingOpenGL RenderingContextOpenGL RenderingGLtoPS RenderingLabel IOExport IOImport IOInfovis IOLSDyna IOMINC IOMovie IONetCDF IOPLY IOParallel IOParallelXML IOSQL IOVideo ImagingMath ImagingMorphological ImagingStatistics ImagingStencil InteractionImage RenderingFreeTypeOpenGL RenderingImage RenderingLIC RenderingLOD RenderingTk RenderingVolumeOpenGL ViewsContextIID ViewsInfovis}

if {($SV_RELEASE_BUILD != 0) && ($tcl_platform(platform) == "windows")} {
    foreach kit $all_vtk_kits {
      set myfn [file join $simvascular_home vtk${kit}TCL-6.2.dll]
      if [catch {load $myfn vtk${kit}TCL} msg] {
        lappend not_loaded vtk${kit}TCL
      } else {
        lappend loaded vtk${kit}TCL
      }
    }
    puts [format "  %-12s %s" "TclVtk:" "Dynamic Libs (not_loaded: [llength $not_loaded])"]
} elseif {($SV_RELEASE_BUILD == 0) && ($tcl_platform(platform) == "windows")} {
    foreach kit $all_vtk_kits {
      #set myfn [file join $simvascular_home vtk${kit}TCL-6.2.dll]
      if [catch {load vtk${kit}TCL-6.2.dll vtk${kit}TCL} msg] {
        lappend not_loaded vtk${kit}TCL
      } else {
        lappend loaded vtk${kit}TCL
      }
    }
    puts [format "  %-12s %s" "TclVtk:" "Dynamic Libs (not_loaded: [llength $not_loaded])"]
} else {
  set auto_path "$auto_path $env(TCLLIBPATH)"
  #puts "auto_path: $auto_path"
  foreach kit $all_vtk_kits {
    if [catch {load {} vtk${kit}TCL} msg] {
      lappend not_loaded vtk${kit}TCL
    } else {
      lappend loaded vtk${kit}TCL
    }
  }
  if {[llength $not_loaded] > 0 && [llength $loaded] > 0} {
    puts "Error loading the following static vtk packages: $not_loaded"
  } elseif {[llength $not_loaded] > 0 && [llength $loaded] == 0} {
    puts [format "  %-12s %s" "TclVtk:" Shared Libs]
    set auto_path "$auto_path $env(TCLLIBPATH)"
    package require vtk
  } else {
    puts [format "  %-12s %s" "TclVtk:" Static Libs]
  }
}
