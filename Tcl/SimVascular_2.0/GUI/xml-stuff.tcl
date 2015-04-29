# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2012 Open Source Medical Software Corporation,
#                           University of California, San Diego.
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

proc EStart {tag attlist args} {

    global xPreviousTag
    global xPreviousTagIdentifier
    global xCurrentTag
    global xCurrentTagIdentifier

    global x__cardiovascular_pulmonary_model
    global x__patient_information
    global x__image_temporal_data_series
    global x__image_data_frame
    global x__vtkstructuredgrid_object
    global x__saved_state
    global x__saved_state_loaded_object

    global x__vessel_centerline_path
    global x__vtkpolydata_object

    global x__segmentation_2d
    global x__ordered_vessel_2d_segmentations
    global x__vessels

    global x__anatomic_model_surface_representation
 
    global x__vtkunstructuredgrid_object

    global x__picture_object
    global x__associate_pictures_to_anatomic_model
    global x__surface_results
    global x__associate_surface_results_to_anatomic_model

    set xCurrentTag $tag
    array set attr $attlist
    if [catch {set xCurrentTagIdentifier $attr(identifier)}] {
       set xCurrentTagIdentifier 0
    }

    if {$tag == "cardiovascular_pulmonary_model"} {
      set x__cardiovascular_pulmonary_model $attlist
    } elseif {$tag == "patient_information"} {
      set x__patient_information $attlist
    } elseif {$tag == "image_temporal_data_series"} {
      set x__image_temporal_data_series($attr(identifier)) $attlist
    } elseif {$tag == "image_data_frame"} {
      set x__image_data_frame($attr(identifier)) $attlist
    } elseif {$tag == "vtkstructuredgrid_object"} {
      set x__vtkstructuredgrid_object($attr(identifier)) $attlist
    } elseif {$tag == "vtkunstructuredgrid_object"} {
      set x__vtkunstructuredgrid_object($attr(identifier)) $attlist
    } elseif {$tag == "vtkpolydata_object"} {
      set x__vtkpolydata_object($attr(identifier)) $attlist
    } elseif {$tag == "saved_state"} {
      set x__saved_state($attr(identifier)) $attlist
    } elseif {$tag == "segmentation_2d"} {
      set x__segmentation_2d($attr(identifier)) $attlist
    } elseif {$tag == "ordered_vessel_2d_segmentations"} {
      set x__ordered_vessel_2d_segmentations($attr(identifier)) $attlist
    } elseif {$tag == "vessels"} {
      set x__vessels($attr(identifier)) $attlist
    } elseif {$tag == "vessel_centerline_path"} {
      set x__vessel_centerline_path($attr(identifier)) $attlist
    } elseif {$tag == "anatomic_model_surface_representation"} {
      set x__anatomic_model_surface_representation($attr(identifier)) $attlist
    } elseif {$tag == "saved_state_loaded_object"} {
      set x__saved_state_loaded_object($xPreviousTagIdentifier) $attlist
    } elseif {$tag == "associate_pictures_to_anatomic_model"} {
      set x__associate_pictures_to_anatomic_model($attr(identifier)) $attlist
    } elseif {$tag == "surface_results"} {
      set x__surface_results($attr(identifier)) $attlist
    } elseif {$tag == "associate_surface_results_to_anatomic_model"} {
      set x__associate_surface_results_to_anatomic_model($attr(identifier)) $attlist
    } elseif {$tag == "picture_object"} {
      set x__picture_object($attr(identifier)) $attlist
    }
    puts "Element \"$tag\" started with [array size attr] attributes"
    foreach i [array names attr] {
      puts "  $i -> $attr($i)"
    }
  
    set xPreviousTag $xCurrentTag
    set xPreviousTagIdentifier $xCurrentTagIdentifier

}

proc EStop {tag args} {
    puts "Element \"$tag\" ended with args: $args"
}

proc PCData text {

    if {$text == ""} {
       return
    }
    if {[string trim $text] == ""} {
       return
    }
    global xCurrentTag
    global xCurrentTagIdentifier
    global x__$xCurrentTag\_text
    set x__$xCurrentTag\_text($xCurrentTagIdentifier) [string trim $text]
    puts "*** set x__$xCurrentTag\_text($xCurrentTagIdentifier) [string trim $text]"
    incr ::count [string length $text]
}


proc guiFREE_load_cpm_model {fn} {

  # create parser
  set parser [xml::parser]
  $parser configure -elementstartcommand EStart \
    -characterdatacommand PCData \
    -elementendcommand EStop

  # open file
  if {[catch {open $fn} ch]} {
    return -code error "unable to open file \"$fn\""
  }

  global xPreviousTag
  global xPreviousTagIdentifier
  global xCurrentTag
  global xCurrentTagIdentifier

  global x__cardiovascular_pulmonary_model
  global x__patient_information
  global x__image_temporal_data_series
  global x__image_data_frame
  global x__vtkstructuredgrid_object
  global x__saved_state
  global x__saved_state_loaded_object

  global x__vessel_centerline_path
  global x__vtkpolydata_object

  global x__segmentation_2d
  global x__ordered_vessel_2d_segmentations
  global x__ordered_vessel_2d_segmentations_text

  global x__vessels

  global x__anatomic_model_surface_representation

  global x__vtkunstructuredgrid_object

  global x__picture_object
  global x__associate_pictures_to_anatomic_model
  global x__associate_pictures_to_anatomic_model_text
  global x__surface_results
  global x__associate_surface_results_to_anatomic_model
  global x__associate_surface_results_to_anatomic_model_text

  catch {unset xPreviousTag}
  catch {unset xPreviousTagIdentifier}
  catch {unset xCurrentTag}
  catch {unset xCurrentTagIdentifier}

  catch {unset x__cardiovascular_pulmonary_model}
  catch {unset x__patient_information}
  catch {unset x__image_temporal_data_series}
  catch {unset x__image_data_frame}
  catch {unset x__vtkstructuredgrid_object}
  catch {unset x__saved_state}
  catch {unset x__saved_state_loaded_object}

  catch {unset x__vessel_centerline_path}
  catch {unset x__vtkpolydata_object}

  catch {unset x__segmentation_2d}
  catch {unset x__ordered_vessel_2d_segmentations}
  catch {unset x__ordered_vessel_2d_segmentations_text}

  catch {unset x__vessels}

  catch {unset x__anatomic_model_surface_representation}

  catch {unset x__vtkunstructuredgrid_object}

  catch {unset x__picture_object}
  catch {unset x__associate_pictures_to_anatomic_model}
  catch {unset x__associate_pictures_to_anatomic_model_text}
  catch {unset x__surface_results}
  catch {unset x__associate_surface_results_to_anatomic_model}
  catch {unset x__associate_surface_results_to_anatomic_model_text}

  # parse file
  if {[catch {$parser parse [read $ch]} err]} {
    return -code error $err
  }

  # look for default state, should only be one
  global x__saved_state
  set default_state {}
  foreach mystate [array names x__saved_state] {
    array set state_vars $x__saved_state($mystate)
      if {$state_vars(name) == "default"} {
       set default_state $mystate
       break
      }
  }  
  if {$default_state == ""} {
    return -code error "ERROR:  No default state found!"
  }
  puts "default state id: $default_state"
  set data_series_no $state_vars(selected_image_temporal_data_series)
  set data_frame $state_vars(selected_image_temporal_data_series_frame)

  global x__image_temporal_data_series_text
  set image_series [lindex $x__image_temporal_data_series_text($data_series_no) $data_frame]

  array set image_data_series_vars $x__image_temporal_data_series($data_series_no) 
  puts "image series: $image_series"

  # override axis labels from defaults if defined in image_temporal_data_series
  global guiVIB
  catch {set guiVIB(left_label_1) $image_data_series_vars(negative_x_direction)}
  catch {set guiVIB(left_label_2) $image_data_series_vars(negative_y_direction)}
  catch {set guiVIB(left_label_3) $image_data_series_vars(negative_z_direction)}
  catch {set guiVIB(right_label_1) $image_data_series_vars(positive_x_direction)}
  catch {set guiVIB(right_label_2) $image_data_series_vars(positive_y_direction)}
  catch {set guiVIB(right_label_3) $image_data_series_vars(positive_z_direction)}

  global x__image_data_frame

  array set image_vars $x__image_data_frame($image_series) 
  set image_identifier $image_vars(image_obj_identifier)

  global x__vtkstructuredgrid_object

  array set sg_vars $x__vtkstructuredgrid_object($image_identifier)

  puts "sg_vars: $sg_vars(file)"

  global gImageVol
  set gImageVol(xml_filename) $sg_vars(file)
  #createPREOPloadsaveVolXMLFile
  mc_LoadImageXML

  # recreate gPathPoints from cpm data

  global gPathPoints
  catch {unset gPathPoints}

  global x__vessel_centerline_path
  foreach path_ident [array names x__vessel_centerline_path] {
    array set path_vars $x__vessel_centerline_path($path_ident)
    set pathId $path_vars(path_number)
    set pathName $path_vars(name)
    catch {unset pts_vars}
    array set pts_vars $x__vtkpolydata_object($path_vars(linear_path_obj_identifier))
    set pts_fn $pts_vars(file)
    catch {unset spline_vars}
    array set spline_vars $x__vtkpolydata_object($path_vars(spline_path_obj_identifier))
    set spline_fn $spline_vars(file)

    puts "pathId: $pathId $pts_fn $spline_fn"
    set gPathPoints($pathId,name) $pathName

    set reader guiFREE_load_cpm_model-reader
    catch {$reader Delete}

    # hand picked points
    vtkXMLPolyDataReader $reader
    $reader SetFileName $pts_fn
    $reader Update
    set pd [$reader GetOutput]
    set numPts [$pd GetNumberOfPoints]
    for {set i 0} {$i < $numPts} {incr i} {
      set gPathPoints($pathId,$i) [$pd GetPoint $i]
    }
    $reader Delete

    # spline
    vtkXMLPolyDataReader $reader
    $reader SetFileName $spline_fn
    $reader Update
    set pd [$reader GetOutput]
    set vect_t [[$pd GetPointData] GetArray t]
    set vect_tx [[$pd GetPointData] GetArray tx]
 
    set numPts [$pd GetNumberOfPoints]
    set gPathPoints($pathId,numSplinePts) $numPts

    set spline {}

    for {set i 0} {$i < $numPts} {incr i} {
	set p  [$pd GetPoint $i]
        set t  [$vect_t GetTuple3 $i]
        set tx [$vect_tx GetTuple3 $i]
	lappend spline "p ([lindex $p 0],[lindex $p 1],[lindex $p 2]) t ([lindex $t 0],[lindex $t 1],[lindex $t 2])  tx ([lindex $tx 0],[lindex $tx 1],[lindex $tx 2])"
    }
    $reader Delete
    set gPathPoints($pathId,splinePts) $spline

  }

  # update the gui
  guiSV_path_update_tree


  # recreate groups from cpm data

  # delete old groups
  foreach i [group_names] {
    foreach j [group_get $i] {
      repos_delete -obj $j
    }
    group_delete $i
  }

  global x__segmentation_2d
  global x__ordered_vessel_2d_segmentations
  global x__ordered_vessel_2d_segmentations_text
  
  global x__vessels
  
  global x__vtkpolydata_object

  catch {unset ordered_2d_segs}
  foreach ordered_2d_segs [array names x__ordered_vessel_2d_segmentations] {

    array set ordered_vars $x__ordered_vessel_2d_segmentations($ordered_2d_segs)
    set ordered_segs $x__ordered_vessel_2d_segmentations_text($ordered_2d_segs)

    set name $ordered_vars(name)
    group_create $name

    foreach seg $ordered_segs {

      catch {unset seg_vars}
      array set seg_vars $x__segmentation_2d($seg)
      catch {unset pd_vars}
      array set pd_vars $x__vtkpolydata_object($seg_vars(segmentation_obj_identifier))

      set posId $seg_vars(spline_path_position_id)
      set fn $pd_vars(file)
  
      puts "grp: $name posId: $posId fn: $fn"
 
      set reader guiFREE_load_cpm_model-reader
      catch {$reader Delete}

      # read segmentation
      vtkXMLPolyDataReader $reader
      $reader SetFileName $fn
      $reader Update
      repos_importVtkPd -src [$reader GetOutput] -dst /group/$name/$posId

      group_add $name /group/$name/$posId $posId

      $reader Delete

    }

  }

  # update the gui
  guiSV_group_update_tree

  #
  #  read in anatomic models
  #

  global x__anatomic_model_surface_representation

  global gAnatomicModelSurfaces
  catch {unset gAnatomicModelSurfaces}
  
  foreach surface [array names x__anatomic_model_surface_representation] {

    catch {unset coarse_vars}
    array set coarse_vars $x__anatomic_model_surface_representation($surface)

    catch {unset pd_vars}
    array set pd_vars $x__vtkpolydata_object($coarse_vars(surface_obj_identifier))
    set fn $pd_vars(file)
  
    set reader guiFREE_load_cpm_model-reader

    # read coarse
    catch {$reader Delete}
    catch {repos_delete -obj solid_$coarse_vars(surface_obj_identifier)}
    vtkXMLPolyDataReader $reader
    $reader SetFileName $fn
    $reader Update
    repos_importVtkPd -src [$reader GetOutput] -dst solid_$coarse_vars(surface_obj_identifier)
    repos_setLabel -obj solid_$coarse_vars(surface_obj_identifier) -key name -value $coarse_vars(name)

    set gAnatomicModelSurfaces($coarse_vars(identifier)) solid_$coarse_vars(surface_obj_identifier)

  }

  #
  #  Load in pics
  #
  
  global gPictures
  catch {unset gPictures} 

  foreach mypicstuff [array names x__picture_object] {

    catch {unset pic_vars}
    array set pic_vars $x__picture_object($mypicstuff)
   
    set myimg fullsize_photo_$pic_vars(identifier)
    image create photo $myimg -file [file join [pwd] $pic_vars(file)]  -gamma {1.0}  -height {0}  -width {0}

    set gPictures($pic_vars(identifier)) [list $myimg $pic_vars(text_description)]    

  }
  
  #
  #  associate pics to anatomic model
  #

  global gAnatomicModelSurfacePictures
  catch {unset gAnatomicModelSurfacePictures}

  foreach mypicstuff [array names x__associate_pictures_to_anatomic_model] {

    catch {unset pic_vars}
    array set pic_vars $x__associate_pictures_to_anatomic_model($mypicstuff)
 
    set ordered_pics $x__associate_pictures_to_anatomic_model_text($mypicstuff)
   
    foreach i $ordered_pics {
      set myimg fullsize_photo_[string trim $i]
      lappend gAnatomicModelSurfacePictures($pic_vars(anatomic_model_identifier)) $i 
    }
    
  }

  #
  #  need to read in surfaces results here
  #
  
  global gSurfaceResults
  catch {unset gSurfaceResults} 

  foreach mysurf [array names x__surface_results] {

    catch {unset surf_vars}
    array set surf_vars $x__surface_results($mysurf)
   
    set gSurfaceResults($surf_vars(identifier)) [list \
                                    $surf_vars(surface_obj_identifier) \
                                    $surf_vars(field_name) \
                                    $surf_vars(display_min_data_range) \
                                    $surf_vars(display_max_data_range) \
                                    $surf_vars(text_description) \
						]

  }

  #
  #  associate pics to anatomic model
  #

  global gAnatomicModelSurfaceResults
  catch {unset gAnatomicModelSurfaceResults}

  foreach mysurf [array names x__associate_surface_results_to_anatomic_model] {

    catch {unset surf_vars}
    array set surf_vars $x__associate_surface_results_to_anatomic_model($mysurf)
 
    set ordered_results $x__associate_surface_results_to_anatomic_model_text($mysurf)
   
    foreach i $ordered_results {
      lappend gAnatomicModelSurfaceResults($surf_vars(anatomic_model_identifier)) $i 
    }
    
  }

}


