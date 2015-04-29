#
# Copyright (c) 2012 Open Source Medical Software Corporation.
#
# All rights reserved.
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

proc dcm_ijkLPS {i j Pixel_Spacing Image_Position Image_Orientation} {
   set lps {}
   for {set n 0} {$n < 3} {incr n} {
     lappend lps [expr [lindex $Image_Position $n] + $j * [lindex $Image_Orientation $n] * [lindex $Pixel_Spacing 1] + $i * [lindex $Image_Orientation [expr 3 + $n] ] * [lindex $Pixel_Spacing 0]]
   }
   return $lps
}
proc dcm_ijkRAS {i j Pixel_Spacing Image_Position Image_Orientation} {
   set lps [dcm_ijkLPS $i $j $Pixel_Spacing $Image_Position $Image_Orientation]
   set ras [list [expr -1.0*[lindex $lps 0]] [expr -1.0*[lindex $lps 1]] [lindex $lps 2]]
   return $ras
}

proc dcm_img_readHeader {args} {

  if {$args == ""} {
      puts {dcm_img_readHeader -file <str> [R] -protected <int> [O]}
      return
  }
  set protected 0
  set fn {}
  array set cmd_line_opts $args
  foreach i [array names cmd_line_opts] {
     if {$i == "-file"} {
       set fn $cmd_line_opts($i)
     } elseif {$i == "-protected"} {
       set protected $cmd_line_opts($i)
     }
  }

  global gExternalPrograms
  set dcmdump $gExternalPrograms(dcmdump)
  set gdcmdump $gExternalPrograms(gdcmdump)

  if [catch {set header [exec $dcmdump --load-short --print-all $fn]} errmsg] {
      puts "ERROR with file $fn: $errmsg"
      return -code error "ERROR: $errmsg"   
  }

  set list_of_header [split $header [format "\n"]]

  set Patient_Position   {blank}
  set Rows 0
  set Columns 0
  set Pixel_Spacing {0 0}
  set Image_Position     {blank}
  set Image_Orientation  {0 0 0 0 0 0 0 0 0}
  set Velocity_Encoding {0}
  set Velocity_Encode_Scale {0}
  set Vas_Collapse_Flag {0}
  set Vas_Flags {0}
  set User2 -1
  set User5 -1
  set User6 -1
  set User7 -1
  set User8 -1
  set User9 -1
  set User12 -1
  set User13 -1
  set User14 -1
  set Pulse_Sequence_Name {blank}
  set Heart_Rate 0

  set Study_ID {0}
  set Acquisition_Number {0}
  set Acquisition_Date {1900.0.0}
  set Study_Date {1900.0.0}
  set Instance_Number {0}
  set Series_Number {0}
  set Series_Instance_UID {0}

  set Manufacturer {blank}
  set PatientName {blank}
  set PatientID {-1}
  set StudyID {-1}
  set PatientSex {blank}

  set Modality {unkown}
  
  for {set j 0} {$j < [llength $list_of_header]} {incr j} {

      set line [string trim [lindex $list_of_header $j]]

      if [regexp -nocase {(0018,5100)} $line] {
	  set Patient_Position [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0028,0010)} $line] {
          set Rows [lindex $line 2]
      }
      if [regexp -nocase {(0028,0011)} $line] {
          set Columns [lindex $line 2]
      }
      if [regexp -nocase {(0028,0030)} $line] {
	  set Pixel_Spacing [lindex [split $line {[]}] end-1]
          set Pixel_Spacing [split [string trim $Pixel_Spacing] \\]
      }
      if [regexp -nocase {(0020,0032)} $line] {
	  set Image_Position [lindex [split $line {[]}] end-1]
          set Image_Position [split [string trim $Image_Position] \\]
      }
      if [regexp -nocase {(0020,0037)} $line] {
	  set Image_Orientation [lindex [split $line {[]}] end-1]
          set Image_Orientation [split [string trim $Image_Orientation] \\]
      }
      if [regexp -nocase {(0018,1088)} $line] {
          set Heart_Rate [lindex [split $line {[]}] end-1]
      }

      if [regexp -nocase {Study ID} $line] {
	  set Study_ID_org [lindex [split $line {[]}] end-1]
          regsub -all {\ } $Study_ID_org "_" Study_ID
      }
      if [regexp -nocase {(0020,0011)} $line] {
	  set Series_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0012)} $line] {
	  set Acquisition_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0013)} $line] {
	  set Instance_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0022)} $line] {
	  set Acquisition_Date [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0020)} $line] {
	  set Study_Date [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,000E)} $line] {
	  set Series_Instance_UID [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0060)} $line] {
	  set Modality [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0070)} $line] {
	  set Manufacturer [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,0010)} $line] {
	  set PatientName [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,0020)} $line] {
	  set PatientID [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0010)} $line] {
	  set StudyID [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,0040)} $line] {
	  set PatientSex [lindex [split $line {[]}] end-1]
      }
  }

  global gOptions
  if {$gOptions(image_data_type) == "GE_MR_DICOM"} {
    if [catch {set header [exec $gdcmdump $fn]} errmsg] {
      puts "ERROR with file $i: $errmsg"
      return -code error "ERROR: $errmsg"   
    }

    set list_of_header [split $header [format "\n"]]

    for {set j 0} {$j < [llength $list_of_header]} {incr j} {

      set line [string trim [lindex $list_of_header $j]]

      if [regexp -nocase {(0019,10cc)} $line] {
        if {[lindex $line 1] == "??"} {
          set Velocity_Encoding [lindex $line 3]
	} else {
          set Velocity_Encoding [lindex $line 2]
	}
      }
      if [regexp -nocase {(0019,10e2)} $line] {
          set Velocity_Encode_Scale [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0043,1030)} $line] {
        if {[lindex $line 1] == "??"} {
          set Vas_Collapse_Flag [lindex $line 3]
	} else {
          set Vas_Collapse_Flag [lindex $line 2]
	}
      }
      if [regexp -nocase {(0043,1032)} $line] {
        if {[lindex $line 1] == "??"} {
          set Vas_Flags [lindex $line 3]
	} else {
          set Vas_Flags [lindex $line 2]
	}
      }
      if [regexp -nocase {(0019,10a9)} $line] {
          set User2 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10ac)} $line] {
          set User5 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10ad)} $line] {
          set User6 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10ae)} $line] {
          set User7 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10af)} $line] {
          set User8 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10b0)} $line] {
          set User9 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10b3)} $line] {
          set User12 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10b4)} $line] {
          set User13 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,10b5)} $line] {
          set User14 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0019,109c)} $line] {
          set Pulse_Sequence_Name [lindex [split $line {[]}] end-1]
      }

    }
  }

  set rtnstr {}
  lappend rtnstr [format "extent \{%i %i\}" $Rows $Columns]
  lappend rtnstr [format "voxel_dims \{%.8f %.8f\}" [lindex $Pixel_Spacing 0] [lindex $Pixel_Spacing 1]]
  lappend rtnstr [format "file_hdr_size %i" 0]

  if {$Image_Position == "blank"} {
    set ul {0 0 0}
    set ur {0 0 0}
    set br {0 0 0}
    set nrm {0 0 0}
    set Image_Position {0 0 0}
  } else {
    set ul [dcm_ijkRAS 0     0        $Pixel_Spacing $Image_Position $Image_Orientation]
    set ur [dcm_ijkRAS 0     $Columns $Pixel_Spacing $Image_Position $Image_Orientation]
    set br [dcm_ijkRAS $Rows $Columns $Pixel_Spacing $Image_Position $Image_Orientation]
    set bl [dcm_ijkRAS $Rows 0        $Pixel_Spacing $Image_Position $Image_Orientation]
    set a [math_scaleVec [math_subVectors $ur $ul] 1.0]
    set b [math_scaleVec [math_subVectors $ur $br] 1.0]
    set nrm [math_normalize [math_cross $b $a]]
  }

  lappend rtnstr [format "top_left_corner \{%.8f %.8f %.8f\}" [lindex $ul 0] [lindex $ul 1] [lindex $ul 2]]
  lappend rtnstr [format "top_right_corner \{%.8f %.8f %.8f\}" [lindex $ur 0] [lindex $ur 1] [lindex $ur 2]]
  lappend rtnstr [format "bottom_right_corner \{%.8f %.8f %.8f\}" [lindex $br 0] [lindex $br 1] [lindex $br 2]]
  lappend rtnstr [format "bottom_left_corner \{%.8f %.8f %.8f\}" [lindex $bl 0] [lindex $bl 1] [lindex $bl 2]]

  lappend rtnstr [format "venc %i" $Velocity_Encoding]
  lappend rtnstr [format "vencscale %.8f" $Velocity_Encode_Scale]
  lappend rtnstr [format "vas_collapse %i" $Vas_Collapse_Flag]
  lappend rtnstr [format "user2 %f" $User2]
  lappend rtnstr [format "user5 %f" $User5]
  lappend rtnstr [format "user6 %f" $User6]
  lappend rtnstr [format "user7 %f" $User7]
  lappend rtnstr [format "user8 %f" $User8]
  lappend rtnstr [format "user9 %f" $User9]
  lappend rtnstr [format "user12 %f" $User12]
  lappend rtnstr [format "user13 %f" $User13]
  lappend rtnstr [format "user14 %f" $User14]

  if {$protected != 0} {
    lappend rtnstr [format "patient_id {%s}" $PatientID]
    lappend rtnstr [format "patient_name {%s}" $PatientName]
  }
  lappend rtnstr [format "psdname {%s}" $Pulse_Sequence_Name]
  lappend rtnstr [format "mag_weight_flag %i" $Vas_Flags]
  if {$protected != 0} {
    lappend rtnstr [format "exam_number %s" $StudyID]
  }
  lappend rtnstr [format "normal_to_plane {%.8f %.8f %.8f}" [lindex $nrm 0] [lindex $nrm 1] [lindex $nrm 2]]
  if {$protected != 0} {
    lappend rtnstr [format "acquisition_time %s" $Acquisition_Date]
  }
  lappend rtnstr [format "heart_rate_bpm %i" $Heart_Rate]
  lappend rtnstr [format "im_no %i" $Instance_Number]
  lappend rtnstr [format "im_seno %i" $Series_Number]

  lappend rtnstr [format "image_position \{%.8f %.8f %.8f\}" [lindex $Image_Position 0] [lindex $Image_Position 1] [lindex $Image_Position 2]]
  lappend rtnstr [format "image_orientation \{%.8f %.8f %.8f %.8f %.8f %.8f\}" [lindex $Image_Orientation 0] [lindex $Image_Orientation 1] [lindex $Image_Orientation 2] [lindex $Image_Orientation 3] [lindex $Image_Orientation 4] [lindex $Image_Orientation 5]]

  lappend rtnstr [format "modality \{%s\}" $Modality]
  lappend rtnstr [format "manufacturer \{%s\}" $Manufacturer]
  lappend rtnstr [format "gender \{%s\}" $PatientSex]

  return $rtnstr

}

# ------------------
# dcm_img2_readSlice
# ------------------

proc dcm_img2_readSlice {filename rtnImg} {

  #@author Nathan Wilson
  #@c Read in a slice of image data.
  #@a filename: image filename.
  #@a rtnImg: Repository ImageData object to create.
  #@r status

  set myslice [dcm_img_readSlice $filename]

  repos_importVtkImg -src $myslice -dst $rtnImg

  # delete here?
  $myslice Delete

  return GDSC_OK
}

# ---------------------
# dcm_img2_readSliceROI
# ---------------------

proc dcm_img2_readSliceROI {filename rtnImg roi} {

  #@author Nathan Wilson
  #@c Read in a slice of image data.
  #@a filename: image filename.
  #@a rtnImg: Repository ImageData object to create.
  #@a roi: List (minX, maxX, minY, maxY)
  #@r status

  if {[file exists $filename] == 0} {
    puts "Error:  Filename $filename does not exist (or you don't have permission to read)."
    return -code error GDSC_ERROR
  }

  set myslice [dcm_img_readSliceROI $filename $roi]

  repos_importVtkImg -src $myslice -dst $rtnImg

  # delete here?
  $myslice Delete

  return GDSC_OK
}


# --------------------
# dcm_img_readSliceROI
# --------------------
# Returns a vtk object name.

proc dcm_img_readSliceROI {fn roi} {

    #@author Nathan Wilson
    #@c Read in a region of interest from a 2-D DICOM image file.
    #@a fn: Image filename.
    #@a roi: List (minX, maxX, minY, maxY)
    #@r Returns a vtkImageData object.

    set slice [dcm_img_readSlice $fn]

    set voi __dcm_img_readSlice_voi

    catch {$voi Delete}
    vtkExtractVOI $voi
    $voi SetInputDataObject $slice
    $voi SetVOI [lindex $roi 0] [lindex $roi 1] [lindex $roi 2] [lindex $roi 3] 0 0
    $voi Update

    return [$voi GetOutput]

}

# -----------------
# dcm_img_readSlice
# -----------------
# Returns a vtk object name.

proc dcm_img_readSlice {fn} {

    #@author Natha Wilson
    #@c Read in a 2-D DICOM image file.
    #@a fn: Image filename.
    #@r Returns a vtkImageData object.
    if {![file exists $fn]} {
	return -code error "couldn't find file $fn"
    }

    set rdr __dcm_img_readSlice_reader

    catch {$rdr Delete}
    vtkDICOMImageReader $rdr

    # glob also does tilde filename expansion:
    $rdr SetFileName [glob $fn]
    $rdr Update

    return [$rdr GetOutput]
}

# See:
#
# http://forum.dcmtk.org/viewtopic.php?f=4&t=910
#
# for more details
#

proc dicom_generateUID {} {
  
   set SITE_INSTANCE_UID_ROOT 1.2.276.0.7230010.3
   set hostid [expr int(rand()*pow(2,31))]
   set uid "$SITE_INSTANCE_UID_ROOT.1.2.$hostid.[pid].[clock seconds].[expr int(rand()*65535)]"
   return $uid

}

proc dicom_deidentify_multiple_files {studyID patID name gender age savePrivate inFiles outDir} {

  foreach inFile $inFiles {
    puts "Working on file $inFile..."
    set newFN [file join $outDir $inFile]
    file mkdir [file dirname $newFN]
    dicom_deidentify_file $studyID $patID $name $gender $age $savePrivate $inFile $newFN
  }

}

proc dicom_deidentify_file {studyID patID name gender age savePrivate inFile outFile} {

  file mkdir [file dirname $outFile]
  file copy -force $inFile $outFile
  dicom_deidentify_destroy_file $studyID $patID $name $gender $age $savePrivate $outFile

}

proc dicom_deidentify_destroy_file {studyID patID name gender age savePrivate filename} {

  # original list of fields from http://sourceforge.net/apps/mediawiki/cardiacatlas/index.php?title=HIPAA_Compliance
  # added field 0001,0030 from thread http://forum.dcmtk.org/viewtopic.php?t=1330
  # added more fields related to date & time from dumping a header
  # deleted fields 0032 and 0033 since they sometimes contain PHI

  global gExternalPrograms
  set dcmodify $gExternalPrograms(dcmodify)

  set protected_fields [list \
		      [list 0008,0014 "Instance Creator UID"] \
		      [list 0008,0050 "Accession Number"] \
		      [list 0008,0080 "Institution Name"] \
		      [list 0008,0081 "Institution Address"] \
		      [list 0008,0090 "Referring Physician's Name"] \
		      [list 0008,0092 "Referring Physician's Address"] \
		      [list 0008,0094 "Referring Physician's Telephone Numbers"] \
		      [list 0008,1010 "Station Name"] \
		      [list 0008,1030 "Study Description"] \
		      [list 0008,103E "Series Description"] \
		      [list 0008,1040 "Institutional Department Name"] \
		      [list 0008,1048 "Physician(s) of Record"] \
		      [list 0008,1050 "Performing Physicians' Name"] \
		      [list 0008,1060 "Name of Physician(s) Reading Study"] \
		      [list 0008,1070 "Operators' Name"] \
		      [list 0008,1080 "Admitting Diagnoses Description"] \
		      [list 0008,1155 "Referenced SOP Instance UID"] \
		      [list 0008,2111 "Derivation Description"] \
		      [list 0010,0030 "Patient’s Birth Date"] \
		      [list 0010,1000 "Other Patient Ids"] \
		      [list 0010,1001 "Other Patient Names"] \
		      [list 0010,1090 "Medical Record Locator"] \
		      [list 0010,2180 "Occupation"] \
		      [list 0010,21B0 "Additional Patient's History"] \
		      [list 0010,4000 "Patient Comments"] \
		      [list 0018,1000 "Device Serial Number"] \
		      [list 0018,1030 "Protocol Name"] \
		      [list 0020,0052 "Frame of Reference UID"] \
		      [list 0020,0200 "Synchronization Frame of Reference UID"] \
		      [list 0020,4000 "Image Comments"] \
		      [list 0040,0275 "Request Attributes Sequence"] \
		      [list 0040,A730 "Content Sequence"] \
		      [list 0088,0140 "Storage Media File-set UID"] \
		      [list 3006,0024 "Referenced Frame of Reference UID"] \
		      [list 3006,00C2 "Related Frame of Reference UID"] \
		      [list 0008,0012 "InstanceCreationDate"] \
		      [list 0008,0013 "InstanceCreationTime"] \
		      [list 0008,0020 "StudyDate"] \
		      [list 0008,0021 "SeriesDate"] \
		      [list 0008,0022 "AcquisitionDate"] \
		      [list 0008,0023 "ContentDate"] \
		      [list 0008,0030 "StudyTime"] \
		      [list 0008,0031 "SeriesTime"] \
		      [list 0008,0032 "AcquisitionTime"] \
		      [list 0008,0033 "ContentTime"] \
		      [list 0040,0244 "PerformedProcedureStepStartDate"] \
		      [list 0040,0245 "PerformedProcedureStepStartTime"] \
		      [list 0040,0253 "PerformedProcedureStepID"] \
		      [list 0040,0254 "PerformedProcedureStepDescription"] \
		      [list 0032,000a "RETIRED_StudyStatusID"] \
		      [list 0032,000c "RETIRED_StudyPriorityID"] \
                      [list 0032,1030 "RETIRED_ReasonForStudy"] \
                      [list 0032,1032 "RequestingPhysician"] \
                      [list 0032,1033 "RequestingService"] \
		      [list 0033,1004 "StudyDescription"] \
		      [list 0033,100c "ReasonforStudy"] \
		      [list 0033,1013 "Patient's Name"] \
		      [list 0033,1019 "physician"] \
		     ]

  set uids_to_update   [list \
		      [list 0008,0018 "SOP Instance UID"] \
		      [list 0020,000D "Study Instance UID"] \
		      [list 0020,000E "Series Instance UID"] \
		      [list 0040,A124 "UID"] \
		     ]

  # build a string of options for dcmodify

  # can automatically set uid's, but we will manually generate new ones
  #set opts {-v -ie -nb -ep -gin -gst -gse}

  set opts {-v -ie -nb}

  if {!$savePrivate} {
    set opts "$opts -ep"
  } else {
    #puts "NOTE:  NOT deleting private field data."
  }

  foreach uid $uids_to_update {
    set opts "$opts -m \"[lindex $uid 0]=[dicom_generateUID]\""
  }
  #puts $opts

  # (0010,0010) "Patient's Name"
  # (0010,0020) "Patient ID"
  # (0010,0040) "PatientSex"
  # (0010,1010) "PatientAge"
  # (0020,0010) "StudyID"

  set opts "$opts -i \"0010,0010=$name\""
  set opts "$opts -i \"0010,0020=$patID\""
  if {$gender != ""} {
    set opts "$opts -i \"0010,0040=$gender\""
  }
  if {$age != ""} {
    set opts "$opts -i \"0010,1010=$age\""
  }
  set opts "$opts -i \"0020,0010=$studyID\""

 foreach killme $protected_fields {
    set opts "$opts -ea \"([lindex $killme 0])\""
  }

  #puts $opts
  catch {eval exec \"$dcmodify\" $opts $filename} msg
  #puts "$msg"

}


proc dicom_createPatientFile {filenames } {

  set fn1 [lindex $filenames 0]
  set fn2 [lindex $filenames 1]

  # just use the first filename to find the pixel spacing
  set fp [open $fn1 r]
  while {[gets $fp line] >= 0} {
       if {[regexp -nocase "Pixel Spacing" $line] == 1} {
         #puts $line
	 set spacing [lindex [split [string trim $line] {[]}] end-1]
	 set spacing [split $spacing \\]
         puts "spacing: $spacing"
       }
  }
  close $fp

  # just use the first filename to find the number of rows
  set fp [open $fn1 r]
  while {[gets $fp line] >= 0} {
       if {[regexp -nocase "Rows" $line] == 1} {
         #puts $line
	 set rows [lindex [split [string trim $line] {[]}] end-1]
         puts "rows: $rows"
       }
   }
   close $fp

  # just use the first filename to find the number of columns
  set fp [open $fn1 r]
  while {[gets $fp line] >= 0} {
       if {[regexp -nocase "Columns" $line] == 1} {
         #puts $line
	 set columns [lindex [split [string trim $line] {[]}] end-1]
         puts "columns: $columns"
       }
   }
   close $fp

   # use the first two files to determine the spacing between
   # slices
   set fp [open $fn1 r]
   while {[gets $fp line] >= 0} {
     if {[regexp -nocase "Image Position" $line] == 1} {
         #puts $line
	 set pt1 [lindex [split [string trim $line] {[]}] end-1]
	 set pt1 [split $pt1 \\]
         puts "pt1: $pt1"
     }
   }
   close $fp
   set fp [open $fn2 r]
   while {[gets $fp line] >= 0} {
     if {[regexp -nocase "Image Position" $line] == 1} {
         #puts $line
	 set pt2 [lindex [split [string trim $line] {[]}] end-1]
	 set pt2 [split $pt2 \\]
         puts "pt2: $pt2"
     }
   }
   close $fp

   set diffpt [math_subVectors $pt2 $pt1]
   puts "slick thickness: $diffpt"

   # assume axial for now
   set slice_thickness [expr abs([lindex $diffpt 2])]

   global gImageVol
   global gOptions
   set gImageVol(vdims_x) [lindex $spacing 0]
   set gImageVol(vdims_y) [lindex $spacing 1]
   set gImageVol(vdims_z) $slice_thickness
   set gImageVol(file_hdr_size) 0
   set gImageVol(ext_i) $rows
   set gImageVol(ext_j) $columns
   set gImageVol(ext_k) [llength $filenames]
   set gImageVol(voi_x0) 0
   set gImageVol(voi_x1) [expr $rows - 1]
   set gImageVol(voi_y0) 0
   set gImageVol(voi_y1) [expr $columns - 1]
   set gImageVol(voi_z0) 0
   set gImageVol(voi_z1) [expr [llength $filenames] -1]
   #set gImageVol(filename) $fn1
   set gOptions(image_data_type) {DICOM}
   set gImageVol(filePattern) "\%s.\%04d"
   dicom_findMaxMinFromHeaders $filenames minpt maxpt
   set gImageVol(min_RAS) $minpt

   set gImageVol(directionCosines) {{1 0 0} {0 -1 0} {0 0 -1}}

}

proc dicom_findMaxMinFromHeaders {filenames rtnMinPt rtnMaxPt} {

  upvar $rtnMinPt minpt
  upvar $rtnMaxPt maxpt

  set minX 999999
  set minY 999999
  set minZ 999999
  set maxX -999999
  set maxY -999999
  set maxZ -999999

  # loop over the files
  foreach fn $filenames {
    #puts "$fn"
    set fp [open $fn r]
    while {[gets $fp line] >= 0} {
       if {[regexp -nocase "Image Position" $line] == 1} {
         #puts $line
	 set pt [lindex [split [string trim $line] {[]}] end-1]
	 set pt [split $pt \\]
         #puts "$fn: $pt"
	 if {[lindex $pt 0] > $maxX} {
             set maxX [lindex $pt 0]
	 }
	 if {[lindex $pt 1] > $maxY} {
             set maxY [lindex $pt 1]
	 }
	 if {[lindex $pt 2] > $maxZ} {
             set maxZ [lindex $pt 2]
	 }
	 if {[lindex $pt 0] < $minX} {
             set minX [lindex $pt 0]
	 }
	 if {[lindex $pt 1] < $minY} {
             set minY [lindex $pt 1]
	 }
	 if {[lindex $pt 2] < $minZ} {
             set minZ [lindex $pt 2]
	 }
       }
    }
    close $fp

  }

  set maxpt [list $maxX $maxY $maxZ]
  set minpt [list $minX $minY $minZ]

}


proc dicom_sortCardiacFiles {filenames rmHDRflag} {

  set numProblems 0
  
  global positions
  catch {unset positions}
  global mapPositions
  catch {unset mapPositions}

  global gExternalPrograms
  set dicom2 $gExternalPrograms(dicom2)

  foreach i $filenames {

    if [catch {set header [exec $dicom2 -t1 --warn=n $i]} errmsg] {
      puts "ERROR with file $i: $errmsg"
      incr numProblems
      continue
   }

    set list_of_header [split $header [format "\n"]]

    set Study_ID {0}
    set Acquisition_Number {0}
    set Instance_Number {0}
    set Series_Number {0}
    set Image_Position {}

    for {set j 0} {$j < [llength $list_of_header]} {incr j} {
      set line [string trim [lindex $list_of_header $j]]
      if [regexp -nocase {Study ID} $line] {
	  set Study_ID_org [lindex [split $line {[]}] end-1]
          regsub -all {\ } $Study_ID_org "_" Study_ID
      }
      if [regexp -nocase {(0020,0011)} $line] {
	  set Series_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0012)} $line] {
	  set Acquisition_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0013)} $line] {
	  set Instance_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0032)} $line] {
	  set Image_Position [lindex [split $line {[]}] end-1]
      }
    }
  
    global positions
    lappend positions($Image_Position) $Instance_Number 
    global mapPositions 
    set mapPositions($Instance_Number) $Image_Position

    #if {$Image_Position == ""} {
    #   return -code error "No image position in header!"
    #}
   
    if {($Image_Position == "") || ($Study_ID == "") ||  \
        ($Series_Number == "") || ($Instance_Number == "")} {
      puts "ERROR finding required header info:"
      puts "Study_ID: $Study_ID"
      puts "Acquisition_Number: $Acquisition_Number"
      puts "Series_Number: $Series_Number"
      puts "Instance_Number: $Instance_Number"
      puts "Image_Position: $Image_Position"
      incr numProblems
      continue
    }

    puts "Processing $Study_ID $Acquisition_Number $Series_Number $Instance_Number"
    flush stdout

    set seriesDir [file join $Study_ID [format "%03i" $Series_Number]]
    set imageFileName [file join $seriesDir I.[format "%04i" $Instance_Number]]
    set imageHeaderFileName [file join $seriesDir hdr.[format "%04i" $Instance_Number]]

    file mkdir $seriesDir

    set changeme [string range [file extension $i] 1 end]
    if {$changeme == ""} {
       set changeme $i
    }

    if {0 == 1} {
    if {$rmHDRflag == 0} {
      if [catch {file copy $i [file join $seriesDir I.[format "%04i" $Instance_Number].dcm]} msg] {
         puts "error msg: $msg"
      }
    } else {
      catch {exec $dicom2 --to=$Study_ID -r1 $i --warn=n --rename=changeme:$changeme}
      file rename [file join $Study_ID changeme-$changeme.raw] [file join $seriesDir I.[format "%04i" $Instance_Number]]
      set fp [open $imageHeaderFileName w]
      puts $fp $header
      close $fp
    }
    }

  }

    # check that all mappings have the same number of slices
    set numSubDirs [llength $positions([lindex [array names positions] 0])]
    foreach i [array names positions] {
      if {[llength $positions($i)] != $numSubDirs} {
         return -code error "not enough slices at $i"
      }
    }
    for {set i 0} {$i < $numSubDirs} {incr i} {
      file mkdir sub[format "%03i" $i]
    }
    foreach i [array names positions] {
	for {set s 0} {$s < $numSubDirs} {incr s} {
	    set fn I.[format %04i [lindex [lsort -integer $positions($i)] $s]].dcm
	    file rename $fn [file join sub[format %03i $s] $fn]
	}
    }

  puts "Total problems found: $numProblems."

}


proc dicom_sortFiles {filenames args} {

  set numProblems 0
  
  global gExternalPrograms
  set dcmdump $gExternalPrograms(dcmdump)

  set logFn {}
  if {$args != ""} {
     set logFn $args
  }

  foreach i $filenames {
    if {$logFn != ""} {
      set logfp [open $logFn a]
    } else {
      set logfp stdout
    }
    puts $logfp "Processing file ($i)"
    if {$logFn != ""} {
      close $logfp
    }
    if [catch {set header [exec $dcmdump --load-short --print-all $i]} errmsg] {
      if {$logFn != ""} {
        set logfp [open $logFn a]
      } else {
        set logfp stdout
      }
      puts $logfp "ERROR with file $i: $errmsg"
      if {$logFn != ""} {
        close $logfp
      }
      incr numProblems
      continue
    }

    set list_of_header [split $header [format "\n"]]

    set Study_ID {0}
    set Acquisition_Number {0}
    set Acquisition_Date {1900.0.0}
    set Study_Date {1900.0.0}
    set Instance_Number {0}
    set Series_Number {0}
    set Image_Position {}
    set Series_Instance_UID {0}

    for {set j 0} {$j < [llength $list_of_header]} {incr j} {
      set line [string trim [lindex $list_of_header $j]]
      if [regexp -nocase {Study ID} $line] {
	  set Study_ID_org [lindex [split $line {[]}] end-1]
          regsub -all {\ } $Study_ID_org "_" Study_ID
      }
      if [regexp -nocase {(0020,0011)} $line] {
	  set Series_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0012)} $line] {
	  set Acquisition_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0013)} $line] {
	  set Instance_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0032)} $line] {
	  set Image_Position [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0022)} $line] {
	  set Acquisition_Date [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0020)} $line] {
	  set Study_Date [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,000E)} $line] {
	  set Series_Instance_UID [lindex [split $line {[]}] end-1]
      }
    }

    # we can ignore Image_Position for now, only
    # need if we need to sort passes (I think...)

    if {($Study_ID == "") ||  \
        ($Series_Number == "") || ($Instance_Number == "")} {
      if {$logFn != ""} {
        set logfp [open $logFn a]
      } else {
        set logfp stdout
      }
      puts $logfp "ERROR finding required header info:"
      puts $logfp "Filename: $i"
      puts $logfp "Study_ID: $Study_ID"
      puts $logfp "Acquisition_Number: $Acquisition_Number"
      puts $logfp "Series_Number: $Series_Number"
      puts $logfp "Instance_Number: $Instance_Number"
      puts $logfp "Image_Position: $Image_Position"
      puts $logfp "Acquisition_Date: $Acquisition_Date"
      puts $logfp "Study_Date: $Study_Date"
      if {$logFn != ""} {
       close $logfp
      }
      incr numProblems
      continue
    }

    if {$logFn != ""} {
      set logfp [open $logFn a]
    } else {
      set logfp stdout
    }
    puts $logfp "Processing $Study_ID $Acquisition_Number $Series_Number $Instance_Number"
    if {$logFn != ""} {
      close $logfp
    }

    set seriesDir [file join $Study_ID $Study_Date [format "%03i" $Series_Number] $Series_Instance_UID]
    set imageFileName [file join $seriesDir I.[format "%04i" $Instance_Number].dcm]
 
    file mkdir $seriesDir

    if [catch {file copy $i $imageFileName} msg] {
      if {$logFn != ""} {
         set logfp [open $logFn a]
      } else {
         set logfp stdout
      }
      puts $logfp "error during file copy ($i): $msg"
      if {$logFn != ""} {
         close $logfp
      }
      incr numProblems
    }

  }

  if {$logFn != ""} {
    set logfp [open $logFn a]
  } else {
    set logfp stdout
  }
  puts $logfp "Total problems found: $numProblems."
  if {$logFn != ""} {
    close $logfp
  }

}


proc dicom_summary {dirnames outFn} {

  global gExternalPrograms
  set dcmdump $gExternalPrograms(dcmdump)

  set numProblems 0

  set fp [open $outFn w]
  puts $fp "Directory\tStudy_ID\tAcquisition_Number\tSeries_Number\tInstance_Number\tStudy_Description\tSeries_Description\tProtocol_Name\tNumber_Of_Files\tAcquisition_Date\tStudy_Date\tPatient_Name\tPatient_Sex\tPatient_Age\tPatient_ID\tSeries_Instance_UID\tSpecial_1\tContrast_Agent\tManufacturer\tManufacturer_Model_Name"
  close $fp

  set dirnames [lsort -dictionary $dirnames]

  foreach i $dirnames {

    if {![file isdirectory $i]} {
       puts "Not a directory ($i), ignored!"
       continue
    }
  
    if [catch {set fns [glob $i/*.dcm]}] {
       puts "No images found in ($i), ignored"
       continue
    }


    # use the second file in case there is a mip or something in
    # slot zero
    if {[llength $fns] == 1} {
      set use_fn $fns
    } else {
      set use_fn [lindex [lsort -dictionary $fns] 1]
    }

    #puts "use_fn: $use_fn"

    if [catch {set header [exec $dcmdump --load-short --print-all $use_fn]} errmsg] {
      puts "ERROR with file ($use_fn): $errmsg"
      incr numProblems
      continue
   }

    set list_of_header [split $header [format "\n"]]

    set Study_ID {0}
    set Acquisition_Number {0}
    set Acquisition_Date {0}
    set Instance_Number {0}
    set Series_Number {0}
    set Patient_Name {Unknown}
    set Patient_Sex {Unknown}
    set Patient_Age {0}
    set Patient_ID {0}
    set Protocol_Name {Unknown}
    set Contrast_Agent {Unknown}
    set Study_Description {Unknown}
    set Series_Description {Unknown}
    set Study_Date {1900.0.0}
    set Series_Instance_UID {0}
    set Special_1 {blank}
    set Manufacturer {Unknown}
    set Manufacturer_Model_Name {Unknown}

    set Number_Of_Files [llength $fns]

    for {set j 0} {$j < [llength $list_of_header]} {incr j} {
      set line [string trim [lindex $list_of_header $j]]
      if [regexp -nocase {Study ID} $line] {
	  set Study_ID_org [lindex [split $line {[]}] end-1]
          regsub -all {\ } $Study_ID_org "_" Study_ID
      }
      if [regexp -nocase {(0020,0011)} $line] {
	  set Series_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0012)} $line] {
	  set Acquisition_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,0013)} $line] {
	  set Instance_Number [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,0010)} $line] {
	  set Patient_Name [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,0020)} $line] {
	  set Patient_ID [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,0040)} $line] {
	  set Patient_Sex [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0010,1010)} $line] {
	  set Patient_Age [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0018,1030)} $line] {
	  set Protocol_Name [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0018,0010)} $line] {
	  set Contrast_Agent [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0022)} $line] {
	  set Acquisition_Date [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,1030)} $line] {
	  set Study_Description [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,103E)} $line] {
	  set Series_Description [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0020)} $line] {
	  set Study_Date [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0020,000E)} $line] {
	  set Series_Instance_UID [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0040,0254)} $line] {
	  set Special_1 [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,0070)} $line] {
	  set Manufacturer [lindex [split $line {[]}] end-1]
      }
      if [regexp -nocase {(0008,1090)} $line] {
	  set Manufacturer_Model_Name [lindex [split $line {[]}] end-1]
      }

    }
    set fp [open $outFn a]
    puts $fp "$i\t$Study_ID\t$Acquisition_Number\t$Series_Number\t$Instance_Number\t$Study_Description\t$Series_Description\t$Protocol_Name\t$Number_Of_Files\t$Acquisition_Date\t$Study_Date\t$Patient_Name\t$Patient_Sex\t$Patient_Age\t$Patient_ID\t$Series_Instance_UID\t$Special_1\t$Contrast_Agent\t$Manufacturer\t$Manufacturer_Model_Name"
    close $fp

    if {($Study_ID == "") ||  \
        ($Series_Number == "") || ($Instance_Number == "")} {
      puts "ERROR finding required header info:"
      puts "Study_ID: $Study_ID"
      puts "Acquisition_Number: $Acquisition_Number"
      puts "Series_Number: $Series_Number"
      puts "Instance_Number: $Instance_Number"
      incr numProblems
      continue
    }

  }

  puts "Total problems found: $numProblems."

}

