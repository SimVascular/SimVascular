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
#  assumes exact same build options in sv_externals

#set sv_vtk_version 6.2
set sv_vtk_version 8.1

#catch {_tmp_vtk_version Delete}
#vtkVersion _tmp_vtk_version
#set sv_vtk_version "[_tmp_vtk_version GetVTKMajorVersion].[_tmp_vtk_version GetVTKMinorVersion]"
#_tmp_vtk_version Delete
#puts "Found vtk version ($sv_vtk_version)."

set not_loaded {}
set loaded {}

if {$sv_vtk_version == "6.2"} {
   set all_vtk_kits [list \
   vtkCommonCoreTcl \
   vtkCommonMathTcl \
   vtkCommonMiscTcl \
   vtkCommonSystemTcl \
   vtkCommonTransformsTcl \
   vtkCommonDataModelTcl \
   vtkCommonColorTcl \
   vtkCommonExecutionModelTcl \
   vtkFiltersCoreTcl \
   vtkCommonComputationalGeometryTcl \
   vtkFiltersGeneralTcl \
   vtkImagingCoreTcl \
   vtkImagingFourierTcl \
   vtkFiltersStatisticsTcl \
   vtkFiltersExtractionTcl \
   vtkInfovisCoreTcl \
   vtkFiltersGeometryTcl \
   vtkFiltersSourcesTcl \
   vtkRenderingCoreTcl \
   vtkRenderingFreeTypeTcl \
   vtkRenderingContextIIDTcl \
   vtkChartsCoreTcl \
   vtkIOCoreTcl \
   vtkIOGeometryTcl \
   vtkIOXMLParserTcl \
   vtkIOXMLTcl \
   vtkDomainsChemistryTcl \
   vtkIOLegacyTcl \
   vtkParallelCoreTcl \
   vtkFiltersAMRTcl \
   vtkFiltersFlowPathsTcl \
   vtkFiltersGenericTcl \
   vtkImagingSourcesTcl \
   vtkFiltersHybridTcl \
   vtkFiltersHyperTreeTcl \
   vtkImagingGeneralTcl \
   vtkFiltersImagingTcl \
   vtkFiltersModelingTcl \
   vtkFiltersParallelTcl \
   vtkFiltersParallelImagingTcl \
   vtkFiltersProgrammableTcl \
   vtkFiltersSMPTcl \
   vtkFiltersSelectionTcl \
   vtkFiltersTextureTcl \
   vtkFiltersVerdictTcl \
   vtkIOImageTcl \
   vtkImagingHybridTcl \
   vtkInfovisLayoutTcl \
   vtkInteractionStyleTcl \
   vtkImagingColorTcl \
   vtkRenderingAnnotationTcl \
   vtkRenderingVolumeTcl \
   vtkInteractionWidgetsTcl \
   vtkViewsCoreTcl \
   vtkGeovisCoreTcl \
   vtkIOAMRTcl \
   vtkIOEnSightTcl \
   vtkIOExodusTcl \
   vtkRenderingOpenGLTcl \
   vtkRenderingContextOpenGLTcl \
   vtkRenderingGLtoPSTcl \
   vtkRenderingLabelTcl \
   vtkIOExportTcl \
   vtkIOImportTcl \
   vtkIOInfovisTcl \
   vtkIOLSDynaTcl \
   vtkIOMINCTcl \
   vtkIOMovieTcl \
   vtkIONetCDFTcl \
   vtkIOPLYTcl \
   vtkIOParallelTcl \
   vtkIOParallelXMLTcl \
   vtkIOSQLTcl \
   vtkIOVideoTcl \
   vtkImagingMathTcl \
   vtkImagingMorphologicalTcl \
   vtkImagingStatisticsTcl \
   vtkImagingStencilTcl \
   vtkInteractionImageTcl \
   vtkRenderingFreeTypeOpenGLTcl \
   vtkRenderingImageTcl \
   vtkRenderingLICTcl \
   vtkRenderingLODTcl \
   vtkRenderingTkTcl \
   vtkRenderingVolumeOpenGLTcl \
   vtkViewsContextIIDTcl \
   vtkViewsInfovisTcl \
   ]
} else {
   puts "assuming vtk 8.1"
   set all_vtk_kits [list \
   vtkChartsCoreTCL \
   vtkCommonColorTCL \
   vtkCommonComputationalGeometryTCL \
   vtkCommonCoreTCL \
   vtkCommonDataModelTCL \
   vtkCommonExecutionModelTCL \
   vtkCommonMathTCL \
   vtkCommonMiscTCL \
   vtkCommonSystemTCL \
   vtkCommonTransformsTCL \
   vtkDomainsChemistryTCL \
   vtkFiltersAMRTCL \
   vtkFiltersCoreTCL \
   vtkFiltersExtractionTCL \
   vtkFiltersFlowPathsTCL \
   vtkFiltersGeneralTCL \
   vtkFiltersGenericTCL \
   vtkFiltersGeometryTCL \
   vtkFiltersHybridTCL \
   vtkFiltersHyperTreeTCL \
   vtkFiltersImagingTCL \
   vtkFiltersModelingTCL \
   vtkFiltersParallelTCL \
   vtkFiltersParallelImagingTCL \
   vtkFiltersPointsTCL \
   vtkFiltersProgrammableTCL \
   vtkFiltersSelectionTCL \
   vtkFiltersSMPTCL \
   vtkFiltersSourcesTCL \
   vtkFiltersStatisticsTCL \
   vtkFiltersTextureTCL \
   vtkFiltersTopologyTCL \
   vtkFiltersVerdictTCL \
   vtkGeovisCoreTCL \
   vtkImagingColorTCL \
   vtkImagingCoreTCL \
   vtkImagingFourierTCL \
   vtkImagingGeneralTCL \
   vtkImagingHybridTCL \
   vtkImagingMathTCL \
   vtkImagingMorphologicalTCL \
   vtkImagingSourcesTCL \
   vtkImagingStatisticsTCL \
   vtkImagingStencilTCL \
   vtkInfovisCoreTCL \
   vtkInfovisLayoutTCL \
   vtkInteractionImageTCL \
   vtkInteractionStyleTCL \
   vtkInteractionWidgetsTCL \
   vtkIOAMRTCL \
   vtkIOCoreTCL \
   vtkIOEnSightTCL \
   vtkIOExodusTCL \
   vtkIOExportTCL \
   vtkIOExportOpenGLIITCL \
   vtkIOGeometryTCL \
   vtkIOImageTCL \
   vtkIOImportTCL \
   vtkIOInfovisTCL \
   vtkIOLegacyTCL \
   vtkIOLSDynaTCL \
   vtkIOMINCTCL \
   vtkIOMovieTCL \
   vtkIONetCDFTCL \
   vtkIOParallelTCL \
   vtkIOParallelXMLTCL \
   vtkIOPLYTCL \
   vtkIOSQLTCL \
   vtkIOTecplotTableTCL \
   vtkIOVideoTCL \
   vtkIOXMLTCL \
   vtkIOXMLParserTCL \
   vtkParallelCoreTCL \
   vtkRenderingAnnotationTCL \
   vtkRenderingChemistryOpenGLIITCL \
   vtkRenderingContextIIDTCL \
   vtkRenderingContextOpenGLIITCL \
   vtkRenderingCoreTCL \
   vtkRenderingFreeTypeTCL \
   vtkRenderingGLtoPSOpenGLIITCL \
   vtkRenderingImageTCL \
   vtkRenderingLabelTCL \
   vtkRenderingLODTCL \
   vtkRenderingOpenGLIITCL \
   vtkRenderingQtTCL \
   vtkRenderingTkTCL \
   vtkRenderingVolumeTCL \
   vtkRenderingVolumeOpenGLIITCL \
   vtkTestingRenderingTCL \
   vtkViewsContextIIDTCL \
   vtkViewsCoreTCL \
   vtkViewsInfovisTCL \
   ]
}

if {($SV_RELEASE_BUILD != 0) && ($tcl_platform(platform) == "windows")} {
    foreach kit $all_vtk_kits {
      set myfn [file join $simvascular_home vtk${kit}TCL-$sv_vtk_version.dll]
      if [catch {load $myfn vtk${kit}TCL} msg] {
        lappend not_loaded vtk${kit}TCL
      } else {
        lappend loaded vtk${kit}TCL
      }
    }
    puts [format "  %-12s %s" "TclVtk:" "Dynamic Libs (not_loaded: [llength $not_loaded])"]
} elseif {($SV_RELEASE_BUILD == 0) && ($tcl_platform(platform) == "windows")} {
    foreach kit $all_vtk_kits {
      #set myfn [file join $simvascular_home vtk${kit}TCL-$sv_vtk_version.dll]
      if [catch {load ${kit}-$sv_vtk_version.dll ${kit}} msg] {
        lappend not_loaded ${kit}
      } else {
        lappend loaded ${kit}
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
