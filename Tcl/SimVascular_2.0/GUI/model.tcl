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

# -----------
# model_names
# -----------

proc model_names {} {
    #@author Adam Updegrove
    #@c Return the model names.
    #@r list of model names.
    global gModel
    return [array names gModel]
}

# -------------
# model_iditems
# -------------

proc model_iditems {name id} {
    #@author Adam Updegrove
    #@c Return a string of ids with the name of the given id interspersed.
    #@a name: name of model.
    #@a id: the id to insert the object name into the return string.
    #@r A string of ids with the object name corresponding to id in place.
    #@note  This routine confuses me quite a bit as it is not obvious how
    #@note  the output of this procedure would be useful (N.W.)
    global gModel
    if {![model_exists $name]} {
	return {}
    }
    set l $gModel($name)
    set out {}
    foreach obj $l {
	set i [lappend out [lindex $obj 0]]
	if {$id == $i} {
	    set out [lappend out [lindex $obj 1]]
	}
    }
    return $out
}
# ------------
# model_exists
# ------------

proc model_exists {name} {
    #@author Adam Updegrove
    #@c Check to see if model exists.
    #@a name:  Name of model.
    #@r Boolean (0 or 1).
    global gModel
    set elems [array names gModel]
    if {[lsearch -exact $elems $name] < 0} {
      return 0
    } else {
      return 1
    }
}

# ------------
# model_create
# ------------

proc model_create {kernel name} {
    #@author Adam Updegrove
    #@c Create a model if it doesn't exist.
    #@a name: Name of model to create.
    #@r status (0 if model already existed, 1 if it was created).
    global gModel
    global gKernel

    set dotsplit [split $name "."]
    set slashsplit [split $name "/"]
    if {[llength $dotsplit] > 1 || [llength $slashsplit] > 1 || $name == ""} {
      return -code error "ERROR: Cannot give name that is blank or contains '.' or '/', give different name"
    }
    if {[model_exists $name]} {
      if {$gKernel($name) == $kernel} {
        return 0
      } else {
	return -code error "ERROR: Cannot give same name to models of different kernel types"
      }
    }
    set gModel($name) {}
    set gKernel($name) $kernel
    return 1
}


# ------------
# model_delete
# ------------

proc model_delete {kernel name} {
    #@author Adam Updegrove
    #@c Delete a model.
    #@a name: Name of model to delete.
    #@r status (0 if model didn't exist, 1 if it was deleted).
    global gModel
    global gKernel
    global gDetached
    global symbolicName
    if {![model_exists $name]} {
	return 0
    }
    unset gModel($name)
    unset gKernel($name)

    set tv $symbolicName(guiSV_model_tree)
    catch {$tv delete .models.$kernel.$name}

    set loc [lsearch -exact $gDetached $name]
    if {$loc != -1} {
      set gDetached [lreplace $gDetached $loc $loc]
    }
    return 1
}


# ---------
# model_get
# ---------
# Returns the (inherently sorted) strings stored in the named model.

proc model_get {name} {
    #@author Adam Updegrove
    #@c Get the list of objects stored in the model.
    #@a name: Model name.
    #@r Returns the (inherently sorted) strings stored in the named model.
    #@r Returns an empty string if the model doesn't exist.
    global gModel
    if {![model_exists $name]} {
	return {}
    }
    set out {}
    set l $gModel($name)
    foreach obj $l {
	set out [lappend out [lindex $obj 1]]
    }
    return $out
}

proc model_set_color {name color} {
    global gModel
    global gModelColor
    if {![model_exists $name]} {return 0}

    set gModelColor($name) $color
}

proc model_get_color {name} {
    global gModelColor
    global gOptions
    if {![model_exists $name]} {return 0}

    if {[lsearch -exact [array names gModelColor] $name] < 0} {
      set gModelColor($name) $gOptions(color_for_model)
    } 
      
    return $gModelColor($name)
}

# ------------
# model_itemix
# ------------
# Returns the position of obj in model name.

proc model_itemix {name obj} {
    #@author Adam Updegrove
    #@c Returns the position of obj in model name.
    #@a name: model name.
    #@a obj: The object to locate in the model.
    #@r Returns the position of obj in model name.
    if {![model_exists $name]} {
	return -1
    }
    set objs [model_get $name]
    set ix [lsearch -exact $objs $obj]
    return $ix
}


# ------------
# model_itemid
# ------------
# Returns the id associated with the given obj.

proc model_itemid {kernel name obj} {
    #@author Adam Updegrove
    #@c Returns the id associated with the given obj.
    #@a name: model name.
    #@a obj: object you want the id for.
    #@r id of the given object.
    global gModel
    set ix [model_itemix $name $obj]
    if {$ix < 0} {
	return -code error "object $obj not found in model $name"
    }
    set item [lindex $gModel($name) $ix]
    return [lindex $item 0]
}

# ------------
# model_idface
# ------------
# Returns the face associated with the given id.
proc model_idface {kernel name id} {
  global symbolicName
  set tv $symbolicName(guiSV_model_tree)

  set faces [model_get $name]
  set facename -1
  foreach face $faces {
    set faceid [lindex [$tv item .models.$kernel.$name.$face -values] 1]
    if {$faceid == $id} {
      set facename $face
      return $facename
    }
  }
  return $facename
}

# -------------
# model_idtaken
# -------------

proc model_idtaken {name id} {
    #@author Adam Updegrove
    #@c Check to see if the id is taken in the given model.
    #@a name: model name.
    #@a id: id number.
    #@r returns an empty string if the model doesn't exist,
    #@r a 1 if the id is used and a 0 if it isn't.
    global gModel
    if {![model_exists $name]} {
	return {}
    }
    set l $gModel($name)
    foreach obj $l {
        set i [lindex $obj 0]
	if {$id == $i} {
	    return 1
	}
    }
    return 0
}

proc model_idvalue {name id} {
    #@author Adam Updegrove
    #@c Check to see if the id is taken in the given model.
    #@a name: model name.
    #@a id: id number.
    #@r returns an empty string if the model doesn't exist,
    #@r a 1 if the id is used and a 0 if it isn't.
    global gModel
    if {![model_exists $name]} {
	return {}
    }
    set l $gModel($name)
    set iter 0
    foreach obj $l {
        set i [lindex $obj 0]
	if {$id == $i} {
	    return $iter
	}
	incr iter
    }
    return 0
}


# ---------
# model_add
# ---------
# The string $obj is stored in the model $name, and its position of
# insertion is governed by a "-dictionary" sort on $id.  This means
# that numbers will be treated numerically (as opposed to the
# situation with "-ascii") and characters are treated alphabetically
# (case-insensitive).

# Note that different strings $obj can be stored in the same model
# with the same $id... this will just lead to equivalence between
# these items in the sorting process.

proc model_add {name obj id} {
    #@author Adam Updegrove
    #@c Add an object to a model.
    #@note The string $obj is stored in the model $name, and its position of
    #@note insertion is governed by a "-dictionary" sort on $id.  This means
    #@note that numbers will be treated numerically (as opposed to the
    #@note situation with "-ascii") and characters are treated alphabetically
    #@note (case-insensitive).&p

    #@note Different strings $obj can be stored in the same model
    #@note with the same $id... this will just lead to equivalence between
    #@note these items in the sorting process.

    #@a name: model name.
    #@a obj: object name.
    #@a id: integer id to associate object with.

    global gModel
    if {![model_exists $name]} {
	return 0
    }
    set ix [model_itemix $name $obj]
    if {$ix >= 0} {
	return 1
    }
    set item [list $id $obj]
    set l $gModel($name)
    set l [lappend l $item]
    set gModel($name) [lsort -dictionary -index 0 $l]
    return 1
}

proc model_add_child {kernel name obj id} {
    #@author Adam Updegrove
    #@c Add an object to a model.
    #@note The string $obj is stored in the model $name, and its position of
    #@note insertion is governed by a "-dictionary" sort on $id.  This means
    #@note that numbers will be treated numerically (as opposed to the
    #@note situation with "-ascii") and characters are treated alphabetically
    #@note (case-insensitive).&p

    #@note Different strings $obj can be stored in the same model
    #@note with the same $id... this will just lead to equivalence between
    #@note these items in the sorting process.

    #@a name: model name.
    #@a obj: object name.
    #@a id: integer id to associate object with.

    global gModel
    global symbolicName

    if {![model_exists $name]} {
	return 0
    }
    set tv $symbolicName(guiSV_model_tree)
    set gModel($name) [linsert $gModel($name) $id [list $obj $obj]]
    $tv insert .models.$kernel.$name $id -id .models.$kernel.$name.$obj -text "$obj"
    return 1
}


# ------------
# model_remove
# ------------

proc model_remove {name obj} {
    #@author Adam Updegrove
    #@c remove an object from a model.
    #@a name: model name.
    #@a obj: object to remove.
    #@r 0 if the model doesn't exist, 1 if the
    #@r the object doesn't exist or was deleted.
    global gModel
    if {![model_exists $name]} {
	return 0
    }
    set l $gModel($name)
    set ix [model_itemix $name $obj]
    if {$ix < 0} {
	return 1
    }
    set gModel($name) [lreplace $l $ix $ix]
    return 1
}

# ----------
# model_size
# ----------

proc model_size {name} {
    #@author Adam Updegrove
    #@c Return the size (length) of the model.
    #@a name: model name.
    #@r length of model (-1 if model doesn't exist).
    global gModel
    if {![model_exists $name]} {
	return -1
    }
    set l $gModel($name)
    return [llength $l]
}



proc model_copy_member { i oldname newname} {

  set orgid [model_itemid $oldname $i]

  set newobjname /model/$newname/$orgid
  puts "rename: $i to $newobjname"
  catch {repos_delete -obj $newobjname}
  geom_copy -src $i -dst $newobjname
  foreach key [repos_getLabelKeys -obj $i] {
   if { $key !="color" || $key !="width" || $key !="opacity"} {
      catch {repos_clearLabel -obj $newobjname -key $key}
      repos_setLabel -obj $newobjname -key $key -value [repos_getLabel -obj $i -key $key]
    }
  }

model_add $newname $newobjname $orgid


}

# Procedure: guiSV_model_selectTree
proc guiSV_model_selectTree { args} {
  global symbolicName
  global gOptions
  global gKernel

  set kernel $gOptions(meshing_solid_kernel)
  #puts "args: $args"
  set tv $args
  set children [$tv select]
  #puts "allSelected: $children"
  # Find in models
  if {[lsearch -exact $children .models.$kernel] >= 0} {
    set children [$tv children .models.$kernel]
    if {$children == ""} {
       return
    }
  }
  if {[guiSV_model_get_tree_current_models_selected] == ""} {
    return
  }

  set firstmodel [lindex [split [lindex $children 0] "."] 3]
  set firstchild [lindex [split [lindex $children 0] "."] 4]
  if {$firstmodel == ""} {
    return
  }
  set kernel $gKernel($firstmodel)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel
  # Do actor stuff!
  $symbolicName(smasherAttNameLabel) delete 0 end
  $symbolicName(smasherAttNameLabel) insert 0 $firstchild
  $symbolicName(smasherEntityIdLabel) config -state normal
  $symbolicName(smasherEntityIdLabel) delete 0 end

  if {$firstchild != ""} {
    guiSV_model_virtual_pick_actor $firstmodel $firstchild
    set id [lindex [$tv item .models.$kernel.$firstmodel.$firstchild -values] 1]
    if {$kernel == "Parasolid"} {
      catch {set identifier [$firstmodel GetFaceAttr -attr identifier -faceId $id]}
      set id $id/$identifier
    }
    $symbolicName(smasherEntityIdLabel) insert 0 $id
    $symbolicName(smasherEntityIdLabel) config -state disabled
  } else {
    $symbolicName(smasherEntityIdLabel) insert 0 ""
    $symbolicName(smasherEntityIdLabel) config -state disabled
  }

  array set keepmodels {}
  set stringlength [expr [string length $kernel] + 9]
  foreach i $children {
    if [regexp .models.$kernel. $i] {
      set keepmodels([lindex [split [string range $i $stringlength end] .] 0]) 1
    } 
  }
  set allmodelselected [lsort -dictionary [array names keepmodels]]

  # only update the model name entry with the first occurance
  global guiSVvars
  set guiSVvars(models_entry_model_name) [lindex $allmodelselected 0]

  global lsGUIcurrentModel
  set lsGUIcurrentModel [lindex $allmodelselected 0]
}

# Procedure: guiSV_set_att_name
proc guiSV_model_set_att_name {} {
 global symbolicName  
 global smasherInputName
 global smasherFaceIds
 global smasherStaticFaceIds
 global smasherFaceNames
 global gOptions
 global gRen3d
 global gKernel

 #set kernel $gOptions(meshing_solid_kernel)
 set faceId [file dirname [$symbolicName(smasherEntityIdLabel) get]]

 set name [$symbolicName(smasherAttNameLabel) get]

 set selected [guiSV_model_get_tree_current_faces_selected]
 if {[llength $selected] != 1} {
   return -code error "ERROR: Only one face can be selected to rename it"
 }
 set modelname [lindex [lindex $selected 0] 0]
 set currname [lindex [lindex $selected 0] 1]
 set names [model_get $modelname]
 if {[lsearch -exact $names $name] != -1} {
   return -code error "Cannot have two faces with the same name!"
 }

 set kernel $gKernel($modelname)
 set gOptions(meshing_solid_kernel) $kernel
 solid_setKernel -name $kernel

 set tv $symbolicName(guiSV_model_tree)
 set id [model_itemix $modelname $currname]
 model_add_child $kernel $modelname $name $id
 if {[lindex [$tv item .models.$kernel.$modelname.$currname -values] 2] == "X"} {
   set oldpd /models/$kernel/$modelname/$currname
   set facepd /models/$kernel/$modelname/$name
   vis_pRm $gRen3d $oldpd
   catch {repos_delete -obj $facepd}
   set faceid [lindex [$tv item .models.$kernel.$modelname.$currname -values] 1]
   if {$kernel == "Parasolid"} {
     $modelname SetFaceAttr -attr gdscName -faceId $faceid -value $name
   }
   $modelname GetFacePolyData -face $faceid -result $facepd
   foreach key [repos_getLabelKeys -obj $oldpd] {
     set value [repos_getLabel -obj $oldpd -key $key]
     repos_setLabel -obj $facepd -key $key -value $value
   }
   catch {repos_delete -obj $oldpd}
   guiSV_model_display_object $kernel/$modelname/$name
 }
 set colvals [$tv item .models.$kernel.$modelname.$currname -values]
 set col1val [lindex $colvals 0]
 set col2val [lindex $colvals 1]
 set col3val [lindex $colvals 2]
 $tv item .models.$kernel.$modelname.$name -values [list $col1val $col2val $col3val] 
 catch {$tv delete .models.$kernel.$modelname.$currname}
 catch {repos_delete -obj /models/$kernel/$modelname}
 model_remove $modelname $currname

 $symbolicName(smasherEntityIdLabel) insert 0 $id
 $symbolicName(smasherEntityIdLabel) config -state disabled
 guiSV_model_update_tree
}

proc guiSV_model_check_array_exists {model datatype arrayname} {
  global symbolicName
  set tv $symbolicName(guiSV_model_tree)

  set cv_tmp /model/checkarray/tmp/cv
  catch {repos_delete -obj $cv_tmp}
  $model GetPolyData -result $cv_tmp 

  set numArrays 0
  set exists 0
  if {$datatype == 0} {
    set numArrays [[[repos_exportToVtk -src $cv_tmp] GetPointData] GetNumberOfArrays]
    for {set i 0} {$i < $numArrays} {incr i} {
      if {[string equal $arrayname [[[repos_exportToVtk -src $cv_tmp] GetPointData] GetArrayName $i]]} {
	set exists 1
      }
    }
  } else {
    set numArrays [[[repos_exportToVtk -src $cv_tmp] GetCellData] GetNumberOfArrays]
    for {set i 0} {$i < $numArrays} {incr i} {
      if {[string equal $arrayname [[[repos_exportToVtk -src $cv_tmp] GetCellData] GetArrayName $i]]} {
	set exists 1
      }
    }
  }
  return $exists
}

# Procedure: guiSV_model_get_tree_current_models_selected
proc guiSV_model_get_tree_current_models_selected {} {

  global symbolicName
  global gOptions
  global gKernel

  set kernel $gOptions(meshing_solid_kernel)
  set splitlength [expr [string length $kernel] + 9]
  set tv $symbolicName(guiSV_model_tree)
  set children [$tv select]

  if {[lsearch -exact $children .models.$kernel] >= 0} {
    set children [$tv children .models.$kernel]
    if {$children == ""} {
       return
    }
  }
  array set selected {}
  foreach i $children {
    #if [regexp .models.$kernel. $i] {
      set grp "[lindex [split $i "."] 3]"
      set selected($grp) 1
    #} 
  }
  set allselected [lsort -dictionary [array names selected]]
  return $allselected
}

# Procedure: guiSV_model_get_tree_current_faces_selected
proc guiSV_model_get_tree_current_faces_selected {} {
  global symbolicName
  global gOptions
  set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set children [$tv select]
  if {[lsearch -exact $children .models.$kernel] >= 0} {
    set children [$tv children .models.$kernel]
    if {$children == ""} {
       return
    }
  }
  # only want terminal nodes of tree
  set rtfaces {}
  foreach i $children {
    set kernel [lindex [split $i "."] 2]
    puts "$kernel"
    set tree_str ".models.$kernel"
    set tree_str_len [string length $tree_str]
    if [regexp $tree_str $i] {
      if {[$tv children $i] != ""} {
        puts "skip ($i)"
        continue
      }
      
      set grp "[lindex [split [string range $i $tree_str_len end] "."] 1]"
      set member [lindex [split [string range $i $tree_str_len end] "."] 2]

      # puts "$grp $member"
      lappend rtfaces [list $grp $member]
    } 
  }
  return $rtfaces
}

proc guiSV_model_get_kernel_type {ext} {
  if {[string compare -length 3 $ext "vtp"] == 0 || \
      [string compare -length 3 $ext "vtk"] == 0 || \
      [string compare -length 3 $ext "stl"] == 0 || \
      [string compare -length 3 $ext "ply"] == 0} {
    set kernel "PolyData"
  } elseif {[string compare -length 3 $ext "dsm"] == 0} {
    set kernel "Discrete"
  } elseif {[string compare -length 7 $ext "xmt_txt"] == 0} {
    set kernel "Parasolid"
  } else {
    return -code error "ERROR: Unkown file type extension: $ext"
  }
  return $kernel
}

# Procedure: guiSV_model_load_model
proc guiSV_model_load_model { {fn "" } } {

   global gOptions
   global gObjects
   global guiMMvars
   global gFilenames
   global gRen3d

   set kernel $gOptions(meshing_solid_kernel)
   if {$fn == ""} {
     if {$kernel == "Parasolid" || $kernel == "Discrete" || $kernel == "PolyData"} {
       set fn [tk_getOpenFile -filetypes {{PARASOLID *.xmt_txt} {Discrete *.dsm} {vtkPolyData *.vtp} {LegacyVTK *.vtk} {Stereolithography *.stl} {"Polygon File Format" *.ply} {"All Files" *.*}} -title "Choose Solid Model"]
     #} elseif {$gOptions(meshing_solid_kernel) == "Discrete"} {
     #  set fn [tk_getOpenFile -filetypes {{Discrete *.dsm} {"All Files" *.*}} -title "Choose Solid Model"]
     #} elseif {$gOptions(meshing_solid_kernel) == "PolyData"} {
     #  set fn [tk_getOpenFile -filetypes {{vtkPolyData *.vtp} {LegacyVTK *.vtk} {Stereolithography *.stl} {"Polygon File Format" *.ply} {"All Files" *.*}} -title "Choose Solid Model"]
     } else {
       set fn [tk_getOpenFile -filetypes {{"All Files" *.*}} -title "Choose Solid Model"]
     }
   }
  if {$fn == ""} return

  set inputName [lindex [split [file tail $fn] "."] 0] 
  set fileExt [lindex [split [file tail $fn] "."] end]
  set gOptions(meshing_solid_kernel) [guiSV_model_get_kernel_type $fileExt]
  set kernel $gOptions(meshing_solid_kernel)

  global symbolicName
  if {[model_exists $inputName]} {
	return -code error "ERROR: Model $inputName already exists. Delete model to load new model"
        #puts "WARNING:  model existed and is being replaced!!"
        #model_delete $inputName
  }

  if {[repos_exists -obj $inputName] == 1} {
    puts "Warning:  Object $inputName exists and is being replaced."
    catch {repos_delete -obj $inputName}
  }
  set gFilenames(generic_solid_file) $fn

  solid_setKernel -name $kernel
  puts "Reading file \"$fn\"."
  solid_readNative -file $fn -obj $inputName
  #puts "Done reading file."

  model_create $kernel $inputName
  guiSV_model_update_tree
  set withFaces 0

  set gObjects($inputName) $inputName
  set gFilenames($inputName) $fn
  if {$kernel == "Parasolid"} {
    guiSV_model_add_faces_to_tree $kernel $inputName 
    set withFaces 1
  }
  # if discrete model, try and load faces file or create defaults
  if {$kernel == "Discrete"} {
    global gDiscreteModelFaceNames
    global gDiscreteModelFaceNamesInfo
    catch {unset gDiscreteModelFaceNames}
    catch {unset gDiscreteModelFaceNamesInfo}
    if [file exists $fn.facenames] {
      puts "sourcing $fn.facenames"
      source $fn.facenames
      package require md5
      set mymd5 [::md5::md5 -hex -file $fn]
      if {$mymd5 != $gDiscreteModelFaceNamesInfo(model_file_md5)} {
        return -code error "ERROR: dsm model ($fn) file doesn't match one used to generate facenames ($fn.facenames)!"
      }
      guiSV_model_add_faces_to_tree $kernel $inputName
      set withFaces 1
    }
  }
  if {$kernel == "PolyData"} {
    global gPolyDataFaceNames
    global gPolyDataFaceNamesInfo
    catch {unset gPolyDataFaceNames}
    catch {unset gPolyDataFaceNamesInfo}
    if [file exists $fn.facenames] {
      puts "sourcing $fn.facenames"
      source $fn.facenames
      package require md5
      set mymd5 [::md5::md5 -hex -file $fn]
      if {$mymd5 != $gPolyDataFaceNamesInfo(model_file_md5)} {
        return -code error "ERROR: dsm model ($fn) file doesn't match one used to generate facenames ($fn.facenames)!"
      }
      guiSV_model_add_faces_to_tree $kernel $inputName
      set withFaces 1
    }
  }

  guiSV_model_display_only_given_model $inputName $withFaces
}

# Procedure: guiSV_model_save_model
proc guiSV_model_save_model {} {
  global gOptions
  global symbolicName
  global smasherInputName
  global gFilenames
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]

  if {[llength $model] != 1} {
    return -code error "Only one model can be written at a time!"
  }
  set kernel $gKernel($model)
  solid_setKernel -name $kernel

  if {$kernel == "Parasolid"} {
    set fn [tk_getSaveFile -filetypes {{PARASOLID *.xmt_txt} {"All Files" *.*}} -title "Choose Solid Model" -initialfile $model]
    if {$fn == ""} return 
    puts "Writing file $fn."
    if {[file extension $fn] == ".xmt_txt"} {
      $model WriteNative -file [file rootname $fn]
    } else {
      $model WriteNative -file $fn
    }
    puts "Done writing file."
  } elseif {$kernel == "Discrete"} {
    set fn $model
    package require md5
    set fn [tk_getSaveFile -filetypes {{DISCRETE *.dsm} {"All Files" *.*}} -title "Choose Solid Model" -initialfile $smasherInputName]
    if {$fn == ""} return 
    puts "Writing discrete model ($fn)"
    $model WriteNative -file $fn
    puts "Done writing discrete model."
    puts "Writing file ($fn.facenames)"
    set mymd5 [::md5::md5 -hex -file $fn]
    set fp [open $fn.facenames w]
    fconfigure $fp -translation lf
    global gDiscreteModelFaceNames
    global gDiscreteModelFaceNamesInfo
    set allids [$model GetFaceIds]
    puts $fp "\# user defined face id to name mapping for model file ($fn)"
    set timestamp [clock seconds]
    puts $fp "\# timestamp: $timestamp  ([clock format $timestamp])"
    puts $fp ""
    puts $fp "global gDiscreteModelFaceNames"
    puts $fp "global gDiscreteModelFaceNamesInfo"
    puts $fp ""
    puts $fp "set gDiscreteModelFaceNamesInfo(timestamp) \{$timestamp\}"
    puts $fp "set gDiscreteModelFaceNamesInfo(model_file_md5) \{$mymd5\}"
    puts $fp "set gDiscreteModelFaceNamesInfo(model_file_name) \{[file tail $fn]\}"
    puts $fp ""
    foreach id $allids {
      set face [model_idface $kernel $model $id]
      puts $fp "set gDiscreteModelFaceNames($id) \{$face\}"
    }
    close $fp
    puts "Done writing facenames file."
  } elseif {$kernel == "PolyData"} {
      set fn $model
      set fn [tk_getSaveFile -defaultextension {.vtp} -filetypes {{vtkPolyData *.vtp} {VTK *.vtk} {vtkUnstructuredGrid *.vtu} {STL *.stl}  {PLY *.ply} {"All Files" *.*}} -title "Choose Solid Model" -initialfile $fn]
      package require md5
      if {$fn == ""} return 
      puts "Writing polydata solid ($fn)"
      $model WriteNative -file $fn
      puts "Done writing polydata solid."
      puts "Writing file ($fn.facenames)"
      set allids [$model GetFaceIds]
      if {[llength $allids] != 0} {
	puts "Writing file ($fn.facenames)"
	set mymd5 [::md5::md5 -hex -file $fn]
	set fp [open $fn.facenames w]
	fconfigure $fp -translation lf
	puts $fp "\# user defined face id to name mapping for model file ($fn)"
	set timestamp [clock seconds]
	puts $fp "\# timestamp: $timestamp  ([clock format $timestamp])"
	puts $fp ""
	puts $fp "global gPolyDataFaceNames"
	puts $fp "global gPolyDataFaceNamesInfo"
	puts $fp ""
	puts $fp "set gPolyDataFaceNamesInfo(timestamp) \{$timestamp\}"
	puts $fp "set gPolyDataFaceNamesInfo(model_file_md5) \{$mymd5\}"
	puts $fp "set gPolyDataFaceNamesInfo(model_file_name) \{[file tail $fn]\}"
	puts $fp ""
	foreach id $allids {
	  set face [model_idface $kernel $model $id]
          puts $fp "set gPolyDataFaceNames($id) \{$face\}"
        }
        close $fp
        puts "Done writing facenames file."
      }
  } else {
    return -code error "ERROR: invalid solid model type."
  }
}

# Procedure: guiSV_model_add_faces_to_tree
proc guiSV_model_add_faces_to_tree {kernel modelname} {
  global gPolyDataFaceNames
  global gDiscreteModelFaceNames
  global smasherFaceNames
  global gOptions

  set maxedgesize $gOptions(facet_max_edge_size)
  set faceIds [$modelname GetFaceIds]
  catch {unset smasherFaceNames}
  foreach id $faceIds {
    if {$kernel == "Parasolid"} {
      set staticFaceId [$modelname GetFaceAttr -attr identifier -faceId $id]
      catch {set facename [$modelname GetFaceAttr -attr gdscName -faceId $id]}
    } elseif {$kernel == "Discrete"} {
      set facename $gDiscreteModelFaceNames($id)
    } elseif {$kernel == "PolyData"} {
      set facename $gPolyDataFaceNames($id)
    } else {
      return -code error "ERROR: Solid kernel $kernel is not a valid kernel"
    }
    lappend smasherFaceNames $facename
    set facepd /models/$kernel/$modelname/$facename 
    catch {repos_delete -obj $facepd}
    if {[catch {$modelname GetFacePolyData -result $facepd -face $id -max_edge_size $maxedgesize} errmsg] == 0} {
      model_add $modelname $facename $facename
      puts "added face $facename to $modelname"
    } else {
      puts "problem with: $modelname GetFacePolyData -result $facepd -face $id -max_edge_size $maxedgesize"
      puts $errmsg
      set errorFaceName {}
      catch {set errorFaceName [$modelname GetFaceAttr -faceId $id -attr gdscName]}
      tk_messageBox -title "Problem Getting Facets on Face ($errorFaceName)" -type ok -message " face name: ($errorFaceName)\n error: ($errmsg)\n cmd: ($modelname GetFacePolyData -result $facepd -face $id -max_edge_size $maxedgesize)\n"
      #return -code error "ERROR: cannot extract face ($staticFaceId ($faceId)).  Try a smaller facet size?"
    }
  }
  if {[llength [lsort -unique $smasherFaceNames]] != [llength $smasherFaceNames]} {
     puts "uh oh, duplicate faces!"
     set pretty_names {}
     set all_ids {}
     foreach i [$modelname GetFaceIds] {
       catch {lappend pretty_names [$modelname GetFaceAttr -attr gdscName -faceId $i]}
       lappend all_ids $i
     }
     set duplist [lsort -dictionary $pretty_names]
     foreach i [lsort -unique $pretty_names] {
        set idx [lsearch -exact $duplist $i]
        set duplist [lreplace $duplist $idx $idx]
     }
     set msg "Duplicate faces found!\n\n"
     set duplistids {}
     foreach dup $duplist {
       set id [lindex $all_ids [lindex [lsearch -exact -all $pretty_names $dup] 0]]
       lappend duplistids $id
     }
     for {set i 0} {$i < [llength $duplist]} {incr i} {
       set dup [lindex $duplist $i]
       set dupid [lindex $duplistids $i]
       set newname [string trim $dup]_2
       set msg "$msg  Duplicate face name $dup is being renamed to $newname\n"
       $modelname SetFaceAttr -attr gdscName -faceId $dupid -value $newname
       set facepd /models/$kernel/$modelname/$newname 
       catch {repos_delete -obj $facepd}
       if {[catch {$modelname GetFacePolyData -result $facepd -face $dupid -max_edge_size $maxedgesize} errmsg] == 0} {
         model_add $modelname $newname $newname
         puts "added face $newname to $modelname"
       } else {
         puts "problem with: $modelname GetFacePolyData -result $facepd -face $dupid -max_edge_size $maxedgesize"
         puts $errmsg
         set errorFaceName {}
         catch {set errorFaceName [$modelname GetFaceAttr -faceId $dupid -attr gdscName]}
         tk_messageBox -title "Problem Getting Facets on Face ($errorFaceName)" -type ok -message " face name: ($errorFaceName)\n error: ($errmsg)\n cmd: ($modelname GetFacePolyData -result $facepd -face $dupid -max_edge_size $maxedgesize)\n"
         #return -code error "ERROR: cannot extract face ($staticFaceId ($faceId)).  Try a smaller facet size?"
       }
     }
     tk_messageBox -title "Duplicate Face Names" -type ok -message $msg
     #guiSV_model_add_faces_to_tree $kernel $modelname
  }

  guiSV_model_update_tree

  foreach id $faceIds {
    if {$kernel == "Parasolid"} {
      catch {set facename [$modelname GetFaceAttr -attr gdscName -faceId $id]}
      #catch {set staticFaceId [$modelname GetFaceAttr -attr identifier -faceId $id]}
      #guiSV_model_set_col_value $kernel.$modelname.$facename 1 $id/$staticFaceId
    } elseif {$kernel == "Discrete"} {
      set facename $gDiscreteModelFaceNames($id)
    } elseif {$kernel == "PolyData"} {
      set facename $gPolyDataFaceNames($id)
    } else {
      return -code error "ERROR: Solid kernel $kernel is not a valid kernel"
    }
    guiSV_model_set_col_value $kernel.$modelname.$facename 1 $id
  }
}

proc guiSV_model_get_face_ids_from_tree {modelname} {
  global gPolyDataFaceNames
  global gOptions
  global symbolicName
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set kernel $gKernel($modelname)
  set gOptions(meshing_solid_kernel) $kernel
  set tv $symbolicName(guiSV_model_tree)
  set faces [model_get $model]

  foreach face $faces {
    set id [lindex [$tv item .models.$kernel.$modelname.$face -values] 1]
    set $gPolyDataFaceNames($id) $face
  }
  guiSV_model_update_tree
}

# Procedure: guiSV_model_fillTree
proc guiSV_model_fillTree { args} {

}

# Procedure: guiSV_model_tree_scroll
proc guiSV_model_tree_scroll { args} {
  global symbolicName
  eval $symbolicName(guiSV_model_tree) yview $args
}


# Procedure: guiSV_model_update_tree
proc guiSV_model_update_tree {} {

  # need to populate the treeview with models
  global gOptions
  global symbolicName
  global lsGUIcurrentModel
  set tv $symbolicName(guiSV_model_tree)
  set kernel $gOptions(meshing_solid_kernel)
  set valexist 0
  set previousCurrentModel $lsGUIcurrentModel
  set modelStillExists 0
  set lsGUIcurrentModel {}


  if {[llength [model_names]] == 0} { 
    $tv delete .models.PolyData
    $tv delete .models.Parasolid
    $tv delete .models.Discrete
    $tv insert {} 0 -id .models.PolyData -text "PolyData" -open 0
    $tv insert {} 1 -id .models.Discrete -text "Discrete" -open 0
    $tv insert {} 2 -id .models.Parasolid -text "Parasolid" -open 0
  }
  $tv configure -columns [list DisplayModel FaceIds DisplayFaces]
  $tv heading \#0 -text "Object"
  #$tv heading Loft -text "Use in Model"
  $tv heading DisplayModel -text "Show Model"
  $tv heading FaceIds -text "Face Id"
  $tv heading DisplayFaces -text "Show Faces"
  
  #Models column
  $tv column \#0 -width 250 -anchor w
  $tv column \#0 -minwidth 100 -anchor w

  ##Use in Model
  $tv column \#1 -width 75 -anchor center
  $tv column \#1 -minwidth 50 -anchor center

  ##Show Solid
  $tv column \#2 -width 75 -anchor center
  $tv column \#2 -minwidth 50 -anchor center

  ##Show Faces
  $tv column \#3 -width 75 -anchor center
  $tv column \#3 -minwidth 50 -anchor center
  
  $tv configure -show [list tree headings]

  # Models 
  global gKernel
  foreach model [lsort -dictionary [model_names]] {
    if {$previousCurrentModel == $model} {
       set modelStillExists 1
    } 
    if {$gKernel($model) == $kernel} {
      if {[$tv exists .models.$kernel.$model] != 1} {
	    $tv insert .models.$kernel end -id .models.$kernel.$model -text "$model" -values {}    
      }
      set i 0
      foreach item [model_iditems $model {}] {
	if {[$tv exists .models.$kernel.$model.$item] != 1} {
	  $tv insert .models.$kernel.$model $i -id .models.$kernel.$model.$item -text "$item"
	}
	incr i
      }
    }
  }

  $tv yview moveto 0
}

proc guiSV_model_display_model_face {showFlag kernel model face} {
  global symbolicName
  global gRen3d
  set tv $symbolicName(guiSV_model_tree)

  set facepd /models/$kernel/$model/$face
  if {![repos_exists -obj $facepd]} {
    set id [lindex [$tv item .models.$kernel.$model.$face -values] 1]
    $model GetFacePolyData -face $id -result $facepd
  }
  if {$showFlag == 0} {
    vis_pRm $gRen3d $facepd
    guiSV_model_set_col_value $kernel.$model.$face 2 ""
  } else {
    guiSV_model_set_col_value $kernel.$model.$face 2 "X"
    guiSV_model_display_object $kernel/$model/$face
  }
}

proc guiSV_model_display_model {showFlag kernel model} {
  global symbolicName 
  global gRen3d
  global gOptions
  set tv $symbolicName(guiSV_model_tree)

  set modelpd /models/$kernel/$model
  if {![repos_exists -obj $modelpd]} {
    $model GetPolyData -result $modelpd -max_edge_size $gOptions(facet_max_edge_size)
  }
  if {$showFlag == 0} {
    vis_pRm $gRen3d $modelpd
    guiSV_model_set_col_value $kernel.$model 0 ""
  } else {
    guiSV_model_display_object $kernel/$model
    guiSV_model_set_col_value $kernel.$model 2 ""
    set names [model_get $model]
    foreach name $names {
      guiSV_model_set_col_value $kernel.$model.$name 2 ""
    }
    guiSV_model_set_col_value $kernel.$model 0 "X"
  }
}

proc guiSV_model_display_selected_models_all_faces {showFlag} {
  global symbolicName
  global gOptions
  global gRen3d
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set selection [guiSV_model_get_tree_current_models_selected]

  foreach model $selection {
    set kernel $gKernel($model)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    set faces [model_get $model]
    if {$faces == ""} {
      return -code error "There are no faces on object"
    }
    foreach face $faces {
      guiSV_model_display_model_face $showFlag $kernel $model $face
    }
    if {$showFlag == 0} {
      guiSV_model_set_col_value $kernel.$model 2 ""
    } else {
      guiSV_model_set_col_value $kernel.$model 2 "X"
      guiSV_model_set_col_value $kernel.$model 0 ""
      guiSV_model_display_model 0 $kernel $model
    }
  }
}

proc guiSV_model_display_model_all_faces {showFlag kernel model} {
  global symbolicName
  global gOptions
  global gRen3d

  set tv $symbolicName(guiSV_model_tree)
  set faces [model_get $model]
  if {$faces == ""} {
    puts "There are no faces on object"
    return
  }
  foreach face $faces {
    guiSV_model_display_model_face $showFlag $kernel $model $face
  }
  if {$showFlag == 0} {
    guiSV_model_set_col_value $kernel.$model 2 ""
  } else {
    guiSV_model_set_col_value $kernel.$model 2 "X"
  }
}

proc guiSV_model_display_selected_faces {showFlag} {
  global gOptions
  global symbolicName
  global gRen3d
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]
  set faces [guiSV_model_get_tree_current_faces_selected]
  set testfaces {}
  foreach face $faces {
    if {[lindex $face 1] != ""} {
      lappend testfaces [lindex $face 1]
    }
  }

  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model can be selected to edit face visualization at a time"
  }
  set kernel $gKernel($model)
  set check [lindex [$tv item .models.$kernel.$model -values] 2]
  if {$check != "X"} {
    return -code error "ERROR: Model must be displayed as faces to toggle viewing of individual faces"
  }

  set names [model_get $model]
  foreach name $names {
    set kernel $gKernel($model)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    if {[$tv exists .models.$kernel.$model.$name] == 1} {
      if {[lsearch -exact $testfaces $name] != -1} {
	guiSV_model_display_model_face $showFlag $kernel $model $name
      }
    }
  }
  guiSV_model_update_tree
}

proc guiSV_model_display_selected_full_model {showFlag} {
  global symbolicName
  global gOptions
  global gRen3d
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set selection [guiSV_model_get_tree_current_models_selected]

  foreach model $selection {
    set kernel $gKernel($model)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    if {[repos_exists -obj $model]} {
      guiSV_model_display_model $showFlag $kernel $model
    }
    if {[llength [model_get $model]] != 0} {
      guiSV_model_set_col_value $kernel.$model 2 ""
      guiSV_model_display_model_all_faces 0 $kernel $model
    }
  }
}

proc guiSV_model_display_only_given_model {modelname withFaces} {
  global symbolicName
  global gRen3d
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  set models [model_names]
  foreach model $models {
    set kernel $gKernel($model)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    if {$model == $modelname} {
      if {$withFaces} {
	guiSV_model_display_model_all_faces 1 $kernel $model
	guiSV_model_set_col_value $kernel.$model 2 "X"
	guiSV_model_set_col_value $kernel.$model 0 ""
      } else {
	guiSV_model_display_model 1 $kernel $model
	guiSV_model_set_col_value $kernel.$model 2 ""
	guiSV_model_set_col_value $kernel.$model 0 "X"
      } 
    } else {
      guiSV_model_display_model 0 $kernel $model
      guiSV_model_display_model_all_faces 0 $kernel $model
      guiSV_model_set_col_value $kernel.$model 2 ""
      guiSV_model_set_col_value $kernel.$model 0 ""
    }
  }
  vis_renReset $gRen3d
}

proc guiSV_model_display_object {object} { 
  global symbolicName
  global gOptions
  global gRen3d

  set spstr [split $object "/"]
  if {[llength $spstr] == 2} {
    set modelcolor $gOptions(color_for_model)
    set opacity $gOptions(opacity_for_model)
  } else {
    set modelcolor $gOptions(color_for_faces)
    set opacity $gOptions(opacity_for_faces)
  }
  if { [lsearch -exact [repos_getLabelKeys -obj /models/$object] color] < 0 } { 
    repos_setLabel -obj /models/$object -key color -value $modelcolor}
  if { [lsearch -exact [repos_getLabelKeys -obj /models/$object] opacity] < 0 } { 
    repos_setLabel -obj /models/$object -key opacity -value $opacity}
  gdscGeneralView $gRen3d /models/$object
}

proc guiSV_model_set_col_value {object col value} {
  global gOptions
  global symbolicName 

  set colvals {}
  set tv $symbolicName(guiSV_model_tree)
  for {set i 0} {$i < 3} {incr i} {
    if {$i == $col} {
      lappend colvals $value
    } else {
      lappend colvals "[lindex [$tv item .models.$object -values] $i]"
    }
  }
  $tv item .models.$object -values $colvals
}

proc guiSV_model_update_actor_selection {actorname} {

  set namesplit [split $actorname "/"]
  if {[lindex $namesplit 1] != "models"} {
    puts "Not a model actor: Return"
    return
  }

  set kernel [lindex $namesplit 2]
  set gOptions(meshing_solid_kernel) $kernel
  if {!($kernel == "PolyData" || $kernel == "Parasolid" || $kernel == "Discrete")} {
    puts "Actor not of valid solid kernel type"
    return
  }

  global symbolicName
  set tv $symbolicName(guiSV_model_tree)
  set modelname [lindex $namesplit 3]
  set facename [lindex $namesplit 4]

  if {$facename != ""} {
    $tv selection set .models.$kernel.$modelname.$facename
  } else {
    $tv selection set .models.$kernel.$modelname
  }
}

proc guiSV_model_change_selected_color {} {
  global gRen3d
  global guiGROUPSvars
  global symbolicName
  global gRen3d
  global PrePickedProperty
  global gOptions
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  #set kernel $gOptions(meshing_solid_kernel)

  set color [tk_chooseColor]
  if {$color == ""} return
  set r [expr (0x[string range $color 1 2])/255.0]
  set g [expr (0x[string range $color 3 4])/255.0]
  set b [expr (0x[string range $color 5 6])/255.0]
  set color "$r $g $b"

  set selected [guiSV_model_get_tree_current_models_selected]
  if {$selected != ""} {
    foreach model $selected {
      set kernel $gKernel($model)
      set gOptions(meshing_solid_kernel) $kernel
      solid_setKernel -name $kernel
      set modelpd /models/$kernel/$model
      set modelShow [lindex [$tv item .models.$kernel.$model -values] 0]
      if {$modelShow == "X"} {
	if {[repos_exists -obj $modelpd]} {
          catch {repos_clearLabel -obj $modelpd -key color}
          repos_setLabel -obj $modelpd -key color -value $color
          model_set_color $model $color
	  gdscGeneralView $gRen3d $modelpd
	  if {$PrePickedProperty != ""} {
	    $PrePickedProperty SetColor $r $g $b
	    if { [lsearch -exact [repos_getLabelKeys -obj $modelpd] opacity] >= 0 } {
	      $PrePickedProperty SetOpacity [repos_getLabel -obj $modelpd -key opacity]
	    }
	  }
	}
      }
    }
  }
  set selected [guiSV_model_get_tree_current_faces_selected]
  set models {}
  set faces {}
  foreach name $selected {
    lappend models [lindex $name 0]
    if {[lindex $name 1] != ""} {
      lappend faces [lindex $name 1]
    }
  }
  if {[llength $faces] != 0} {
    for {set i 0} {$i < [llength $faces]} {incr i} {
      set face [lindex $faces $i]
      set model [lindex $models $i]
      set kernel $gKernel($model)
      set gOptions(meshing_solid_kernel) $kernel
      solid_setKernel -name $kernel
      set facepd /models/$kernel/$model/$face
      set faceShow [lindex [$tv item .models.$kernel.$model.$face -values] 2]
      if {$faceShow == "X"} {
	if {[repos_exists -obj $facepd]} {
	  catch {repos_clearLabel -obj $facepd -key color}
	  repos_setLabel -obj $facepd -key color -value "$r $g $b"
	  gdscGeneralView $gRen3d $facepd
	  if {$PrePickedProperty != ""} {
	    $PrePickedProperty SetColor $r $g $b
	    if { [lsearch -exact [repos_getLabelKeys -obj $facepd] opacity] >= 0 } {
	      $PrePickedProperty SetOpacity [repos_getLabel -obj $facepd -key opacity]
	    }
	  }
	}
      }
    }
  }
  vis_render $gRen3d
}

proc guiSV_model_change_selected_opacity {opacity} {
  global gRen3d
  global guiGROUPSvars
  global symbolicName
  global gRen3d
  global PrePickedProperty
  global gOptions
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set selected [guiSV_model_get_tree_current_models_selected]
  if {$selected != ""} {
    foreach model $selected {
      set kernel $gKernel($model)
      set gOptions(meshing_solid_kernel) $kernel
      solid_setKernel -name $kernel
      set modelpd /models/$kernel/$model
      set modelShow [lindex [$tv item .models.$kernel.$model -values] 0]
      if {$modelShow == "X"} {
	if {[repos_exists -obj $modelpd]} {
	  catch {repos_clearLabel -obj $modelpd -key opacity}
	  repos_setLabel -obj $modelpd -key opacity -value "$opacity"
	  gdscGeneralView $gRen3d $modelpd
	  if {$PrePickedProperty != ""} {
	    if { [lsearch -exact [repos_getLabelKeys -obj $modelpd] color] >= 0 } {
	      set tmpColor [repos_getLabel -obj $modelpd -key color]
	      if {$tmpColor != 0} {
		if {[llength $tmpColor] == 1} {
		  set tmpColor [winfo rgb . $tmpColor]
		}
		$PrePickedProperty SetColor [lindex $tmpColor 0] [lindex $tmpColor 1] [lindex $tmpColor 2]
	      }
	    }
	    $PrePickedProperty SetOpacity $opacity
	  }
	}
      }
    }
  }

  set selected [guiSV_model_get_tree_current_faces_selected]
  set models {}
  set faces {}
  foreach name $selected {
    lappend models [lindex $name 0]
    if {[lindex $name 1] != ""} {
      lappend faces [lindex $name 1]
    }
  }
  if {[llength $faces] != 0} {
    for {set i 0} {$i < [llength $faces]} {incr i} {
      set face [lindex $faces $i]
      set model [lindex $models $i]
      set kernel $gKernel($model)
      set gOptions(meshing_solid_kernel) $kernel
      solid_setKernel -name $kernel
      set facepd /models/$kernel/$model/$face
      set faceShow [lindex [$tv item .models.$kernel.$model.$face -values] 2]
      if {$faceShow == "X"} {
	if {[repos_exists -obj $facepd]} {
	  catch {repos_clearLabel -obj $facepd -key opacity}
	  repos_setLabel -obj $facepd -key opacity -value "$opacity"
	  gdscGeneralView $gRen3d $facepd
	  if {$PrePickedProperty != ""} {
	    if { [lsearch -exact [repos_getLabelKeys -obj $facepd] color] >= 0 } {
	      set tmpColor [repos_getLabel -obj $facepd -key color]
	      if {$tmpColor != 0} {
		if {[llength $tmpColor] == 1} {
		  set tmpColor [winfo rgb . $tmpColor]
		}
		$PrePickedProperty SetColor [lindex $tmpColor 0] [lindex $tmpColor 1] [lindex $tmpColor 2]
	      }
	    }
	    $PrePickedProperty SetOpacity $opacity
	  }
	}
      }
    }
  }
  vis_render $gRen3d
}

proc guiSV_model_calc_selected_face_area {} {
  global symbolicName
  global gRen3d
  global smasherEntityIdentifier
  global gOptions
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set objects [guiSV_model_get_tree_current_faces_selected]
  set models {}
  set faces {}
  foreach object $objects {
    lappend models [lindex $object 0]
    if {[lindex $object 1] != ""} {
      lappend faces [lindex $object 1]
    }
  }

  set area 0
  for {set i 0} {$i < [llength $faces]} {incr i} {
    set face [lindex $faces $i]
    set model [lindex $models $i]
    set kernel $gKernel($model)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    set facepd /models/$kernel/$model/$face
    if {![repos_exists -obj $facepd]} {
      return -code error "ERROR: Selected object does not exist"
    }
    set area [expr $area + [geom_surfArea -src $facepd]]

  }
  tk_messageBox -title "Surface Area" -type ok  -message "Faces: $faces\nArea: $area"
  puts "Faces: $faces\nArea: $area"
}

proc guiSV_model_remove_all_faces {kernel model} {
  global symbolicName
  global gKernel
  set tv $symbolicName(guiSV_model_tree)

  foreach object $model {
    set kernel $gKernel($object)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    set faces [model_get $object]
    if {$faces != ""} {
      guiSV_model_display_model_all_faces 0 $kernel $object
      foreach face $faces {
	catch {$tv delete .models.$kernel.$object.$face}
	catch [repos_delete -obj /models/$kernel/$object/$face]
	model_remove $object $face
      }
    }
  }
}

proc guiSV_model_delete_selected_model {} {
  global symbolicName
  global gOptions
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree) 
  set models [guiSV_model_get_tree_current_models_selected]

  guiSV_model_display_selected_full_model 0
  foreach model $models {
    set kernel $gKernel($model)
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel
    guiSV_model_delete_model $kernel $model
  }
  guiSV_model_update_tree
}

proc guiSV_model_delete_model {kernel model} {
  global symbolicName
  global gRen3d

  set tv $symbolicName(guiSV_model_tree)
  if {[vis_pExists $gRen3d /models/$kernel/$model]} {
    guiSV_model_display_model 0 $kernel $model 
  }
  set faces [model_get $model]
  if {[llength $faces] != 0} {
    foreach face $faces {
      catch {[$tv delete .models.$kernel.$model.$face]}
      catch {repos_delete -obj /models/$kernel/$model/$face}
      vis_pRm $gRen3d /models/$kernel/$model/$face
    }
  }
  model_delete $kernel $model
  catch {$tv delete .models.$kernel.$model} 
  catch {repos_delete -obj /models/$kernel/$model}
  catch {repos_delete -obj $model}
}

# Procedure: guiVMTKCenterlines
proc guiVMTKCenterlines {} {
  global gOptions
  global gObjects
  global guiPDvars
  global gPolyDataFaceNames
  global symbolicName
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  set gOptions(meshing_solid_kernel) PolyData
  #set kernel $gOptions(meshing_solid_kernel)
  
  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model can be used for centerline extraction"
  }
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid must be of type PolyData for centerline extraction"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  global gWaitVar
  set gWaitVar 0
  tk_messageBox -title "Extract Centerlines"  -type ok -message "Extracting the centerlines requires the walls to be defined:\n 1. Click 'OK'\n 2. Select the walls in the Model Object list using ctrl-click or shift-click\n 3. Hit 'Enter' on your keyboard"
  vwait gWaitVar

  set selected [guiSV_model_get_tree_current_faces_selected]
  set faces {}
  foreach name $selected {
    if {[lindex $name 1] != ""} {
      lappend faces [lindex $name 1]
    }
  }
  set all_faces [model_get $model]
  set deletelist {}
  foreach face $all_faces {
    if {[lsearch -exact $faces $face] == -1} { 
      lappend deletelist [lindex [$tv item .models.$kernel.$model.$face -values] 1] 
    }
  }

  set firstsolid /models/$kernel/$model
  set newsolid /tmp/polydata/newsolid
  set finalsolid /tmp/polydata/finalsolid
  catch {repos_delete -obj $firstsolid}
  catch {repos_delete -obj $newsolid}
  catch {repos_delete -obj $finalsolid}

  $model GetPolyData -result $firstsolid

  PolyDataDeleteRegions $model $deletelist

  set cappedsolid [PolyDataVMTKGetCenterIds $model solid]

  set polylist [PolyDataVMTKCenterlines $cappedsolid $model solid]
  crd_ren gRenWin_3D_ren1
  set guiPDvars(vis_centerlines) 1
  guiPDvisObj centerlines no

  set no_ids [geom_cap -src [lindex $polylist 0] -result $newsolid -captype 0]

  geom_mapandcorrectids -src $firstsolid -new $newsolid -result $finalsolid -srcarrayname "ModelFaceID" -newarrayname "CenterlineCapID"
  
  $model SetVtkPolyData -obj $firstsolid
  guiSV_model_display_model_all_faces 0 $kernel $model
  guiSV_model_display_model 1 $kernel $model
  guiSV_model_change_selected_opacity 0.2

  return $finalsolid
}


# Procedure: guiBOUNDARIESextract
proc guiBOUNDARIESextract {} {
  global gOptions
  global smasherInputName
  global guiMMvars
  global guiPDvars
  global symbolicName
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  set extraction_angle $guiPDvars(angleForBoundaries)
  #set kernel $gOptions(meshing_solid_kernel)
  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model allowed for extraction at a time"
  }

  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid must be of type PolyData for centerline extraction"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  #set oldmodel "[string trim $model]_facesextracted"
  guiSV_model_add_to_backup_list $kernel $model
  #if {[model_create $kernel $oldmodel] != 1} {
  #  guiSV_model_delete_model $kernel $oldmodel
  #  catch {repos_delete -obj /models/$kernel/$oldmodel}
  #  model_create $kernel $oldmodel
  #}

  global gPolyDataFaceNames
  global gPolyDataFaceNamesInfo
  catch {unset gPolyDataFaceNames}
  catch {unset gPolyDataFaceNamesInfo}

  puts "Running Boundary Extraction Filter..."
  $model GetBoundaryFaces -angle $extraction_angle
  puts "Got Boundaries"

  set allids [$model GetFaceIds]
  foreach id $allids {
    set gPolyDataFaceNames($id) "noname_$id"
  }
  guiSV_model_remove_faces_from_tree $kernel $model
  guiSV_model_update_tree
  guiSV_model_add_faces_to_tree $kernel $model

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

# Procedure: guiBOUNDARIEScombineSelectedFaces
proc guiBOUNDARIEScombineSelectedFaces {} {
  global gOptions
  global guiPDvars
  global gPolyDataFaceNames
  global smasherInputName
  global symbolicName
  global gKernel
  global gRen3d

  #set kernel $gOptions(meshing_solid_kernel)
  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]
  set selected [guiSV_model_get_tree_current_faces_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: One model needs to be selected to combine faces"
  }
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid kernel must be of type PolyData"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  #set oldmodel [string trim $model]_facescombined
  guiSV_model_add_to_backup_list $kernel $model
  #guiSV_model_copy_model $kernel $model $oldmodel 0
  set faces {}
  foreach name $selected {
    if {[lindex $name 1] != ""} {
      lappend faces [lindex $name 1]
    }
  }
  #guiSV_model_display_model_all_faces 0 $kernel $model
  set iter 0
  set one 0
  foreach face $faces {
    set two {-1}
    set faceid [lindex [$tv item .models.$kernel.$model.$face -values] 1]
    if {$iter == 0} {
      set one $faceid
      vis_pRm $gRen3d /models/$kernel/$model/$face
    } else {
      set two $faceid
    }
    if {$iter > 0 && $two != {-1}} {
      puts "Combining $one and $two"
      CombinePolyDataFaces $model $one $two
      catch {$tv delete .models.$kernel.$model.$face}
      catch [repos_delete -obj /models/$kernel/$model/$face]
      model_remove $model $face
      vis_pRm $gRen3d /models/$kernel/$model/$face
    }
    incr iter
  }

  set all_faces [model_get $model]
  foreach face $all_faces {
    catch [repos_delete -obj /models/$kernel/$model/$face]
  }
  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

# Procedure: guiBOUNDARIESremeshSelectedFaces
proc guiBOUNDARIESremeshSelectedFaces {} {
  global gOptions
  global guiPDvars
  global gPolyDataFaceNames
  global smasherInputName
  global symbolicName
  global gKernel

  #set gOptions(meshing_solid_kernel) PolyData
  set tv $symbolicName(guiSV_model_tree)
    
  set yesno [tk_messageBox -default yes  -message "Select remesh size before running remesh. Continue?"  -title "Set Mesh Size"  -type yesno]

  if {$yesno == "yes"} {
    set model [guiSV_model_get_tree_current_models_selected]
    set selected [guiSV_model_get_tree_current_faces_selected]

    if {[llength $model] != 1} {
      return -code error "ERROR: One model needs to be selected to remesh faces"
    }
    #set kernel $gOptions(meshing_solid_kernel)
    set kernel $gKernel($model)
    if {$kernel != "PolyData"} {
      return -code error "ERROR: Solid kernel must be of type PolyData"
    }
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel

    #set oldmodel [string trim $model]_facesremeshed
    guiSV_model_add_to_backup_list $kernel $model
    #guiSV_model_copy_model $kernel $model $oldmodel 0
    set faces {}
    foreach name $selected {
      if {[lindex $name 1] != ""} {
	lappend faces [lindex $name 1]
      }
    }
    set all_faces [model_get $model]
    set excludelist {}
    foreach face $all_faces {
      if {[lsearch -exact $faces $face] == -1} {
	set faceid [lindex [$tv item .models.$kernel.$model.$face -values] 1]
	lappend excludelist $faceid
      }
    }
  } else {
    return
  }
  PolyDataRemeshSurfaces $model $excludelist

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

# Procedure: guiBOUNDARIESdeleteSelectedFaces
proc guiBOUNDARIESdeleteSelectedFaces {} {
  global gOptions
  global guiPDvars
  global gPolyDataFaceNames
  global smasherInputName
  global symbolicName
  global gKernel
  global gRen3d

  set gOptions(meshing_solid_kernel) PolyData
  set tv $symbolicName(guiSV_model_tree)

  set yesno [tk_messageBox -default yes  -message "This will remove these faces from the full solid model completely. Continue?"  -title "Continue?"  -type yesno]

  if {$yesno == "yes"} {
    set model [guiSV_model_get_tree_current_models_selected]
    if {[llength $model] != 1} {
      return -code error "ERROR: One model needs to be selected for face extraction"
    }
    #set kernel $gOptions(meshing_solid_kernel)
    set kernel $gKernel($model)
    if {$kernel != "PolyData"} {
      return -code error "ERROR: Solid kernel must be of type PolyData"
    }
    set gOptions(meshing_solid_kernel) $kernel
    solid_setKernel -name $kernel

    set selected [guiSV_model_get_tree_current_faces_selected]

    guiSV_model_add_to_backup_list $kernel $model
    #guiSV_model_copy_model $kernel $model $oldmodel 0
    set faces {}
    foreach name $selected {
      if {[lindex $name 1] != ""} {
	lappend faces [lindex $name 1]
      }
    }
    set all_faces [model_get $model]
    set deletelist {}
    foreach face $all_faces {
      if {[lsearch -exact $faces $face] != -1} {
	set faceid [lindex [$tv item .models.$kernel.$model.$face -values] 1]
	lappend deletelist $faceid
	catch {$tv delete .models.$kernel.$model.$face}
	catch {repos_delete -obj /models/$kernel/$model/$face}
	vis_pRm $gRen3d /models/$kernel/$model/$face
	model_remove $model $face
      }
    }
  } else {
    return
  }

  PolyDataDeleteRegions $model $deletelist

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

# Procedure: guiBOUNDARIESfillWithIds
proc guiBOUNDARIESfillWithIds {} {
  global gOptions
  global guiPDvars
  global gPolyDataFaceNames
  global smasherInputName
  global gui3Dvars
  global symbolicName
  global gKernel
  
  set tv $symbolicName(guiSV_model_tree)

  set model [guiSV_model_get_tree_current_models_selected]
  set selected [guiSV_model_get_tree_current_faces_selected]

  if {[llength $model] != 1} {
    return -code error "ERROR: One model needs to be selected for face extraction"
  }

  #set kernel $gOptions(meshing_solid_kernel)
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Must be using PolyData for this operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  #set oldmodel [string trim $model]_filled
  guiSV_model_add_to_backup_list $kernel $model
  #guiSV_model_copy_model $kernel $model $oldmodel 0

  set all_faces [model_get $model]
  set maxid 0
  foreach face $all_faces {
    set faceid [lindex [$tv item .models.$kernel.$model.$face -values] 1]
    if {$faceid > $maxid} {
      set maxid $faceid
    }
  }

  set fillpd /guiBOUNDARIES/tmppd 
  set tmppd /models/$kernel/$model
  catch {repos_delete -obj $fillpd}

  if {![repos_exists -obj $tmppd]} {
    $model GetPolyData -result $tmppd
  }

  set filltype 2
  geom_fillHolesWithIds $tmppd $fillpd $maxid $filltype

  $model SetVtkPolyData -obj $fillpd
  set newids [$model GetFaceIds]
  foreach id $newids {
    if {$id > $maxid} {
      model_add $model "noname_$id" "noname_$id"
    }
  }
  guiSV_model_update_tree
  foreach id $newids {
    if {$id > $maxid} {
      guiSV_model_set_col_value $kernel.$model.noname_$id 1 $id
    }
  }

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

proc guiTRIMcreateCutBox { model side offset vals} {
  global guiWSSvars
  global gRen3d
  global gOptions
  global gKernel

  #set kernel $gOptions(meshing_solid_kernel)
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid kernel sould be PolyData for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set modelpd /models/$kernel/$model
  set obj /tmp/model/obj

  catch {repos_delete -obj $obj}
  if {![repos_exists -obj $modelpd]} {
  catch {repos_delete -obj $modelpd}
    $model GetPolyData -result $modelpd
  }

  set clipper tmp-guiTRIM-clipper
  set impPlanes tmp-guiTRIM-box

  catch {$clipper Delete}
  catch {$impPlanes Delete}

  # create the implicit cut box
  vtkPlanes $impPlanes
  $impPlanes SetBounds 0 1 0 1 0 1
  set pts [$impPlanes GetPoints]
  set nrms [$impPlanes GetNormals]

  for {set i 0} {$i < 6} {incr i} {
    set pt [lrange $vals [expr 6*$i] [expr 6*$i + 2]]
    set nrm [lrange $vals [expr 3+6*$i] [expr 3+6*$i +2]]
    #puts "pt: $pt  nrm: $nrm"
    eval $pts SetPoint $i $pt
    eval $nrms SetTuple3 $i $nrm 
  }

  # do the clipping
  #puts "Using clipper..."
  vtkClipPolyData $clipper
  $clipper SetInputDataObject [repos_exportToVtk -src $modelpd]
  $clipper GenerateClippedOutputOn
  $clipper SetClipFunction $impPlanes
  $clipper SetValue $offset
  $clipper Update

  set triangulator tmp-guiTRIM-triangulator
  catch {$triangulator Delete}
  vtkTriangleFilter $triangulator

  if {$side == "inside"} {
    $triangulator SetInputDataObject [$clipper GetOutput]
  } else {
    $triangulator SetInputDataObject [$clipper GetClippedOutput]
  }
  $triangulator Update

  catch {repos_delete -obj $obj}
  repos_importVtkPd -src [$triangulator GetOutput] -dst $obj
  $model SetVtkPolyData -obj $obj

  global guiTRIMvars
  set guiTRIMvars(num_tris) [[repos_exportToVtk -src $obj] GetNumberOfCells]

  $triangulator Delete
  $clipper Delete
  $impPlanes Delete
}

# Procedure: guiTRIMcreateCutPlane
proc guiTRIMcreateCutPlane { model side} {

  global gui3Dvars
  global guiTRIMvars
  global gRen3d
  global guiPDvars
  global smasherInputName
  global gOptions
  global gKernel

  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid kernel sould be PolyData for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set modelpd /models/$kernel/$model
  set obj /tmp/model/obj

  catch {repos_delete -obj $obj}
  if {![repos_exists -obj $modelpd]} {
  catch {repos_delete -obj $modelpd}
    $model GetPolyData -result $modelpd
  }

  set clipper tmp-guiTRIM-clipper

  set plane tmp-guiTRIM-plane
  set impPlane tmp-guiTRIM-impPlane

  set reposPlane /guiTRIM/slice_plane

  catch {$clipper Delete}
  catch {$plane Delete}
  catch {$impPlane Delete}

  catch {repos_delete -obj $reposPlane}

  set topRight $guiTRIMvars(plane_p1)
  set botRight $guiTRIMvars(plane_p2)
  set topLeft  $guiTRIMvars(plane_p3)

  if {[llength $topRight] != 3 ||
      [llength $botRight] != 3 ||
      [llength $topLeft] != 3} {
    return
    #return -code error "ERROR:  incorrect length of corner pts."
  }

  # create the explicit cut plane

  vtkPlaneSource $plane
  $plane SetOrigin [lindex $topRight 0] [lindex $topRight 1] [lindex $topRight 2]
  $plane SetPoint1 [lindex $topLeft 0] [lindex $topLeft 1] [lindex $topLeft 2]
  $plane SetPoint2 [lindex $botRight 0] [lindex $botRight 1] [lindex $botRight 2]
  $plane Update
  set nrm [$plane GetNormal]
  repos_importVtkPd -src [$plane GetOutput] -dst $reposPlane

  # create the implicit cut plane
  vtkPlane $impPlane
  $impPlane SetOrigin [lindex $topRight 0] [lindex $topRight 1] [lindex $topRight 2]
  $impPlane SetNormal [lindex $nrm 0] [lindex $nrm 1] [lindex $nrm 2]

  # do the clipping
  #puts "Using clipper..."
  catch {$clipper Delete}
  vtkClipPolyData $clipper
  $clipper SetInputDataObject [repos_exportToVtk -src $modelpd]
  $clipper GenerateClippedOutputOn
  $clipper SetClipFunction $impPlane
  $clipper SetValue $guiTRIMvars(plane_offset)
  $clipper Update

  set triangulator tmp-guiTRIM-triangulator
  catch {$triangulator Delete}
  vtkFillHolesFilter $triangulator

  if {$side == "above"} {
    $triangulator SetInputDataObject [$clipper GetOutput]
  } else {
    $triangulator SetInputDataObject [$clipper GetClippedOutput]
  }
  $triangulator Update
  catch {repos_delete -obj $obj}
  repos_importVtkPd -src [$triangulator GetOutput] -dst $obj
  $model SetVtkPolyData -obj $obj

  global guiTRIMvars
  set guiTRIMvars(num_tris) [[repos_exportToVtk -src $obj] GetNumberOfCells]

  $triangulator Delete
  $clipper Delete
  $plane Delete
}

proc guiSV_model_update_new_solid {kernel model newmodel} {
  global symbolicName
  global gOptions
  global gKernel
  global gPolyDataFaceNames

  set kernel $gKernel($model)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set tv $symbolicName(guiSV_model_tree)

  set addfaces 0
  if {$kernel == "Parasolid"} {
    set addfaces 1
  } elseif {$kernel == "PolyData"} {
    if [guiSV_model_check_array_exists $newmodel 1 "ModelFaceID"] {
      set addfaces 1
    }
  }

  if {$addfaces == 1} {
    if {$kernel == "PolyData"} {
      catch {unset gPolyDataFaceNames}
      set faceids [$newmodel GetFaceIds]
      foreach id $faceids {
	set gPolyDataFaceNames($id) [model_idface $kernel $model $id]
      }
    }
    guiSV_model_add_faces_to_tree $kernel $newmodel
  }
  guiSV_model_display_selected_full_model 0
  guiSV_model_display_object $kernel/$newmodel
  guiSV_model_update_tree
  guiSV_model_set_col_value $kernel.$newmodel 0 "X"
  $tv selection set .models.$kernel.$newmodel
}

proc guiSV_model_new_surface_name {{cmd 0}} {
  global symbolicName
  global guiSVvars

  set guiSVvars(models_entry_model_name) ""
  global gWaitVar
  set gWaitVar 0
  set tv $symbolicName(guiSV_model_tree)
  set x [winfo rootx $tv]
  set y [winfo rooty $tv]
  set selected [guiSV_group_get_tree_current_groups_selected]
  ShowWindow.svModelWindow $cmd
  move_window .svModelWindow $x $y
  vwait gWaitVar
  DestroyWindow.svModelWindow
  return $guiSVvars(models_entry_model_name)
}

proc ShowWindow.svModelWindow { {cmd 0}} {

  # build widget .lsGUIselectPath
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .svModelWindow"
  } {
    catch "destroy .svModelWindow"
  }
  toplevel .svModelWindow -background {white}

    # Window manager configurations
  wm positionfrom .svModelWindow program
  wm sizefrom .svModelWindow program
  wm maxsize .svModelWindow 1280 1024
  wm minsize .svModelWindow 150 10
  wm protocol .svModelWindow WM_DELETE_WINDOW {DestroyWindow.svModelWindow}


  ttk::frame .svModelWindow.tframe6  -borderwidth {0}  -relief {flat}  -width {60}  -height {10}
  ttk::frame .svModelWindow.tframe6.frame1  -borderwidth {0}  -relief {flat}  -width {60}  -height {10}
  ttk::frame .svModelWindow.tframe6.frame1.frame2  -width {769}  -height {10}
  ttk::label .svModelWindow.tframe6.frame1.frame2.label9  -font {Helvetica 10}  -relief {flat}  -text {Model Name:}  -width {12}
  ttk::entry .svModelWindow.tframe6.frame1.frame2.entry10  -font {Helvetica 10}  -textvariable {guiSVvars(models_entry_model_name)}  -width {30}

  ttk::button .svModelWindow.tframe6.frame1.frame2.tbutton0  -command {ChangeWaitVar}  -text {Enter}
  ttk::button .svModelWindow.tframe6.frame1.frame2.tbutton1  -command {ChangeWaitVar}  -text {Rename Model}
  ttk::button .svModelWindow.tframe6.frame1.frame2.tbutton2  -command {ChangeWaitVar}  -text {Duplicate Model}
  ttk::button .svModelWindow.tframe6.frame1.frame2.tbutton4  -command {}  -text {Copy Members}
  
  
  ttk::button .svModelWindow.tframe6.frame1.frame2.tbutton3  -command {DestroyWindow.svModelWindow}  -text {Cancel}

  pack configure .svModelWindow.tframe6  -fill both -ipadx 5 -ipady 5

  # pack master .svModelWindow.tframe6
  pack configure .svModelWindow.tframe6.frame1  -fill both
  #pack configure .svModelWindow.tframe6.tframe0  -fill both


  # pack master .svModelWindow.tframe6.frame1
  pack configure .svModelWindow.tframe6.frame1.frame2  -expand 1  -fill both
  # pack configure .svModelWindow.tframe6.frame1.frame1  -fill both

  # pack master .svModelWindow.tframe6.frame1.frame2
  pack configure .svModelWindow.tframe6.frame1.frame2.label9  -fill both  -side left
  pack configure .svModelWindow.tframe6.frame1.frame2.entry10  -expand 1  -fill both  -side left

  if { $cmd == "rename"} {
    pack configure .svModelWindow.tframe6.frame1.frame2.tbutton1  -side left
    bind .svModelWindow.tframe6.frame1.frame2.entry10 <Key-Return> {ChangeWaitVar}
    wm title .svModelWindow {Rename Model}
  } elseif {$cmd == "duplicate"} {
    wm title .svModelWindow {Duplicate Model}
    bind .svModelWindow.tframe6.frame1.frame2.entry10 <Key-Return> {ChangeWaitVar}
    pack configure .svModelWindow.tframe6.frame1.frame2.tbutton2  -side left
  } elseif {$cmd == "copymembers"} {
    wm title .svModelWindow {Copy Members}
    bind .svModelWindow.tframe6.frame1.frame2.entry10 <Key-Return> {}
    pack configure .svModelWindow.tframe6.frame1.frame2.tbutton4  -side left
  } else {
    wm title .svModelWindow {Name for Lofted Model}
    bind .svModelWindow.tframe6.frame1.frame2.entry10 <Key-Return> {ChangeWaitVar}
    pack configure .svModelWindow.tframe6.frame1.frame2.tbutton0  -side left
  }

  pack configure .svModelWindow.tframe6.frame1.frame2.tbutton3  -side left
  focus .svModelWindow.tframe6.frame1.frame2.entry10


  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .svModelWindow"
    after 2 "catch {XFEditSetShowWindows}"
  }

}
proc DestroyWindow.svModelWindow {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .svModelWindow]" != ""} {
      global xfShowWindow.svModelWindow
      set xfShowWindow.svModelWindow 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .svModelWindow; XFEditSetShowWindows"
    }
  } else {
    catch "destroy .svModelWindow"
    update
  }
}

proc guiSV_model_blend_selected_models {} {
  global gObjects
  global symbolicName
  global gOptions
  global gKernel


  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Can only blend one Parasolid model at a time"
  }
  set kernel $gKernel($model)
  if {$kernel != "Parasolid"} {
    return -code error "ERROR: Solid kernel must be Parasolid for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  #set oldmodel "[string trim $model]_blended"
  guiSV_model_add_to_backup_list $kernel $model
  #model_create $kernel $oldmodel
  set faceids [$model GetFaceIds]
  foreach id $faceids {
    set ident [$model GetFaceAttr -attr identifier -faceId $id]
    set facename [$model GetFaceAttr -attr gdscName -faceId $id]
    if {$facename != ""} {
      set ids($facename) $id
    }
    set ids($ident) $id
  }
  set params [$symbolicName(guiBLENDSscript) get 0.0 end]

  set broken [split $params "\n"]
  for {set i 0} {$i < [llength $broken]} {incr i} {
    set trimmed [string trim [lindex $broken $i]]
    if {$trimmed == ""} {continue}
    if {[llength $trimmed] != 3} {
      puts "ERROR: line ($trimmed) ignored!"
    }
    set faceA -1
    set faceB -1
    catch {set faceA $ids([lindex $trimmed 0])}
    catch {set faceB $ids([lindex $trimmed 1])}
    set r [lindex $trimmed 2]
    if {$faceA < 0 || $faceB < 0} {
       puts "ERROR: invalid values in line ($trimmed).  Line Ignored."
       continue
    }
    $model CreateEdgeBlend -faceA $faceA -faceB $faceB -radius $r
    set nameA [$model GetFaceAttr -attr gdscName -faceId $faceA]
    set nameB [$model GetFaceAttr -attr gdscName -faceId $faceB]
    set wallblend 0
    if {[string range $nameA 0 4] == "wall_"} {
       set nameA [string range $nameA 5 end]
       incr wallblend
    }
    if {[string range $nameB 0 4] == "wall_"} {
       set nameB [string range $nameB 5 end]
       incr wallblend
    }

    if {$wallblend > 0} {
       set name "wall_blend"
    } else {
       set name "blend"
    }

    # tag new faces
    set tagger 0
    foreach id [$model GetFaceIds] {
      set facename [$model GetFaceAttr -attr gdscName -faceId $id]
      set value [lsearch -exact $faceids $id]
      if {$value < 0} {
        set name "$name\_$nameA\_$nameB"
  if {$tagger > 0} {
          set name "$name\_tag$tagger"
  }
        incr tagger
        puts "new face id: $id ($name)"
        $model SetFaceAttr -attr gdscName -faceId $id -value $name
      }
    }
    set faceids [$model GetFaceIds]
  }

  set pretty_names {}
  foreach i [$model GetFaceIds] {
    catch {lappend pretty_names [$model GetFaceAttr -attr gdscName -faceId $i]}
  }
  if {[llength [lsort -unique $pretty_names]] != [llength $pretty_names]} {
    set duplist [lsort -dictionary $pretty_names]
    foreach i [lsort -unique $pretty_names] {
      set idx [lsearch -exact $duplist $i]
      set duplist [lreplace $duplist $idx $idx]
    }
    set msg "Duplicate faces found!\n\n"
    foreach dup $duplist {
      set msg "$msg  name: $dup\n"
    }
    tk_messageBox -title "Duplicate Face Names" -type ok -message $msg
  }

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

proc guiSV_model_trim_model {} {

  global symbolicName
  global gObjects

  set model [guiSV_model_get_tree_current_selected_models]
  if {[llength $model] != 1} { 
    return -code error "ERROR: Only one model can be trimmed at a time"
  }
  set gObjects(preop_solid) $model
  set trimmedModel1 "[string trim $model]_trimmed1"
  catch {repos_delete -obj $trimmedModel1}
  model_create Parasolid $trimmedModel1
  set trimmedModel2 "[string trim $model]_trimmed2"
  catch {repos_delete -obj $trimmedModel2}
  model_create Parasolid $trimmedModel2
  set gObjects(prop_trimmed1) $trimmedModel1
  set gObjects(prop_trimmed2) $trimmedModel2

  trimSolid

  set trimmedModel1 $gObjects(preop_trimmed1)
  set trimmedModel2 $gObjects(preop_trimmed2)
  # rename the gdsc face ids so they range from 1 to n.
  # this is required by phasta.
  foreach trimmedModel [list $trimmedModel1 $trimmedModel2] {
    set valid 1
    foreach i [$trimmedModel GetFaceIds] {
      set id {}
      catch {set id [$trimmedModel GetFaceAttr -attr gdscId -faceId $i]}
      if {$id != ""} {
        $trimmedModel SetFaceAttr -attr gdscId -faceId $i -value $valid
        incr valid
      }
    }
  }
  guiSV_model_update_new_solid Parasolid $model $trimmedModel1
  guiSV_model_update_new_solid Parasolid $trimmedModel1 $trimmedModel2

}

proc guiSV_model_copy_selected_model {} {
  global gOptions
  global symbolicName
  global gKernel

  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model can be copied at a time"
  }
  set kernel $gKernel($model)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel
  
  guiSV_model_copy_model $kernel $model "" copy
}

proc guiSV_model_copy_model {kernel model newname op} {
  global gOptions
  global symbolicName
  global gPolyDataFaceNames
  global gDiscreteModelFaceNames

  if {$newname == ""} {
    set oldmodel [guiSV_model_new_surface_name $op]
  } else {
    set oldmodel $newname
  }

  if {$model == $oldmodel} {
    return -code error "ERROR: Cannot copy to same name!"
  }
  catch {repos_delete -obj $oldmodel}
  if {[model_create $kernel $oldmodel] != 1} {
    guiSV_model_delete_model $kernel $oldmodel
    catch {repos_delete -obj /models/$kernel/$oldmodel}
    model_create $kernel $oldmodel
  }
  solid_copy -src $model -dst $oldmodel
  set addfaces 0
  if {$kernel == "Parasolid"} {
    set addfaces 1
  } elseif {$kernel == "PolyData"} {
    if [guiSV_model_check_array_exists $oldmodel 1 "ModelFaceID"] {
      set addfaces 1
    }
  } elseif {$kernel == "Discrete"} {
    set addfaces 1
  }

  if {$addfaces == 1} {
    if {$kernel == "PolyData"} {
      catch {unset gPolyDataFaceNames}
      set faceids [$oldmodel GetFaceIds]
      foreach id $faceids {
	set gPolyDataFaceNames($id) [model_idface $kernel $model $id]
	puts "Check $gPolyDataFaceNames($id)"
      }
    } elseif {$kernel == "Discrete"} {
      catch {unset gDiscreteModelFaceNames}
      set faceids [$oldmodel GetFaceIds]
      foreach id $faceids {
	set gDiscreteModelFaceNames($id) [model_idface $kernel $model $id]
      }
    }
    guiSV_model_add_faces_to_tree $kernel $oldmodel
  }
  guiSV_model_update_tree
}

proc guiSV_model_rename_selected_model {} {
  global gOptions
  global symbolicName
  global gKernel

  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model can be copied at a time"
  }
  set kernel $gKernel($model)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel
  
  guiSV_model_rename_model $kernel $model
}

proc guiSV_model_rename_model {kernel model} {
  global gOptions
  global symbolicName

  guiSV_model_copy_model $kernel $model "" rename
  guiSV_model_delete_model $kernel $model
}

# Procedure: guiSV_model_create_model_polydata
proc guiSV_model_create_model_polydata {} {
  global gRen3d
  global guiPDvars
  global gui3Dvars
  global gOptions
  global guiSVvars

  global guiBOOLEANvars
  set gOptions(meshing_solid_kernel) PolyData
  set kernel $gOptions(meshing_solid_kernel)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set ordered_names    $guiBOOLEANvars(selected_groups)
  set ordered_names2    $guiBOOLEANvars(selected_seg3d)
  set sampling_default $guiBOOLEANvars(sampling_default)
  set lin_multiplier   $guiBOOLEANvars(linear_sampling_along_length_multiplier)
  set useLinearSampleAlongLength   $guiBOOLEANvars(use_linear_sampling_along_length)
  set numModes         $guiBOOLEANvars(num_modes_for_FFT)
  array set overrides  $guiBOOLEANvars(sampling_overrides)
  set useFFT           $guiBOOLEANvars(use_FFT)
  set sample_per_segment $guiBOOLEANvars(sampling_along_length_multiplier)
  set addCaps          $guiBOOLEANvars(add_caps_to_vessels)
  set noInterOut       $guiBOOLEANvars(no_inter_output)
  set tol 	       $guiBOOLEANvars(tolerance)
  set spline           $guiBOOLEANvars(spline_type)


  set model [guiSV_model_new_surface_name 0]
  catch {repos_delete -obj $model}
  if {[model_create $kernel $model] != 1} {
    guiSV_model_delete_model $kernel $model
    catch {repos_delete -obj /models/$kernel/$model}
    model_create $kernel $model
  }

  foreach grp $ordered_names {

    set numOutPtsInSegs $sampling_default

    if [info exists overrides($grp)] {
      set numOutPtsInSegs $overrides($grp)
      puts "overriding default ($sampling_default) with ($numOutPtsInSegs) for ($grp)."
    }
 
    set vecFlag 0
 
    set numSegs [llength [group_get $grp]]

    set numOutPtsAlongLength [expr $sample_per_segment * $numSegs]

    set numPtsInLinearSampleAlongLength [expr $lin_multiplier *$numOutPtsAlongLength]
   
    puts "num pts along length: $numPtsInLinearSampleAlongLength"

    set outPD /guiGROUPS/polydatasurface/$grp
    catch {repos_delete -obj $outPD}

    solid_setKernel -name PolyData
    polysolid_c_create_vessel_from_group $grp $vecFlag  $useLinearSampleAlongLength $numPtsInLinearSampleAlongLength  $useFFT $numModes  $numOutPtsInSegs $numOutPtsAlongLength $addCaps $spline $outPD

  }

  if {[llength $ordered_names] ==0 && [llength $ordered_names2] == 0} {
      tk_messageBox -title "Select Surfaces on Segmentation Tab"  -type ok -message "Please select surfaces for boolean on segmentation tab"
      return
  }
  for {set i 0} {$i < [llength $ordered_names]} {incr i} {
    lappend vessel_names /guiGROUPS/polydatasurface/[lindex $ordered_names $i]
    lappend names [lindex $ordered_names $i]
  }
  global gSeg3D
  for {set i 0} {$i < [llength $ordered_names2]} {incr i} {
    set name [lindex $ordered_names2 $i]
    lappend names $name
    if {![check_surface_for_capids $gSeg3D($name)]} {
      puts "vessel $name doesn't have CapIDs, setting to -1"
      set_capids_for_pd $gSeg3D($name)
    }
    lappend vessel_names $gSeg3D($name)

  }
  puts $vessel_names

  geom_all_union -srclist $vessel_names -intertype $noInterOut -result $model -tolerance $tol
  set pdobject /models/$kernel/$model
  catch {repos_delete -obj $pdobject}
  $model GetPolyData -result $pdobject
  set rtnstr [geom_checksurface -src $pdobject -tolerance $tol] 

  set_facenames_as_groupnames $model $names $addCaps
  #set_facenames_as_groupnames $myresult $names 0

  guiSV_model_add_faces_to_tree $kernel $model
  guiSV_model_display_only_given_model $model 1

  tk_messageBox -title "Solid Unions Complete"  -type ok -message "Number of Free Edges: [lindex $rtnstr 0]\n (Free edges are okay for open surfaces)\n Number of Bad Edges: [lindex $rtnstr 1]"
}

proc guiSV_model_virtual_pick_actor {model face} {
  global CurrentRenderer
  global gRen3d
  global gKernel
  set kernel $gKernel($model)

  if {$face == ""} {
  set actor [vis_pGetActor $gRen3d /models/$kernel/$model]
  } else {
  set actor [vis_pGetActor $gRen3d /models/$kernel/$model/$face]
  }
  if {$actor == ""} {
    return
  }
  VirtualPickActor $actor
  vis_render $gRen3d
}

# Procedure: guiSV_model_create_model_parasolid
proc guiSV_model_create_model_parasolid {} {
   global symbolicName
   global createPREOPgrpKeptSelections
   global gFilenames
   global gObjects
   global gLoftedSolids
   global gOptions

   set gOptions(meshing_solid_kernel) Parasolid
   solid_setKernel -name $gOptions(meshing_solid_kernel)
   set kernel $gOptions(meshing_solid_kernel)
   set gOptions(meshing_solid_kernel) $kernel
   solid_setKernel -name $kernel

   set tv $symbolicName(guiSV_group_tree)
  set children [$tv children {}]

   if {[lsearch -exact $children .groups.all] >= 0} {
     set children [$tv children .groups.all]
     if {$children == ""} {
       return
     }
   }

   set createPREOPgrpKeptSelections {}
   puts "children: $children"
   
    foreach child $children {
      if {[lindex [$tv item $child -values] 0] == "X"} {
  lappend createPREOPgrpKeptSelections [string range $child 12 end]
      }
    }

   set modelname [guiSV_model_new_surface_name 0]
   catch {repos_delete -obj $modelname}
   if {[model_create $kernel $modelname] != 1} {
     guiSV_model_delete_model $kernel $modelname
     catch {repos_delete -obj /models/$kernel/$modelname}
     model_create $kernel $modelname
   }
   guiSV_model_update_tree

    #set modelname $gObjects(preop_solid)

   if {[llength $createPREOPgrpKeptSelections] == 0} {
      puts "No solid models selected.  Nothing done!"
      return
   }

   puts "Will union together the following SolidModel objects:"
   puts "  $createPREOPgrpKeptSelections"

   if {[repos_exists -obj $modelname] == 1} {
      puts "Warning:  object $modelname existed and is being replaced."
      repos_delete -obj $modelname
   }

    if {[repos_exists -obj /models/$kernel/$modelname] == 1} {
      repos_delete -obj /model/$kernel/$modelname
    }

    # for convenience, we offer to make any missing solid models for the
    # the user.  In general, people shouldn't do this, but what can you
    # do?

    set recreateAllChoice [tk_messageBox -title "Recreate all vessels?"  -message "Recreate all solids?"  -icon question -type yesno]

    foreach i $createPREOPgrpKeptSelections {
      set cursolid ""

      if {$cursolid == "" || [repos_exists -obj $cursolid] == 0 || $recreateAllChoice == "yes"} {
         # solid  doesn't exist for current object, ask to create
         set choice [tk_messageBox -title "Missing capped solid branch!"  -message "Create missing solids using defaults?"  -icon question -type yesnocancel]
         switch -- $choice {
           yes {
              # create solids
              foreach j $createPREOPgrpKeptSelections {
                set cursolid ""
    catch {set cursolid $gLoftedSolids($j)}
      if {$cursolid == "" || [repos_exists -obj $cursolid] == 0 || $recreateAllChoice == "yes"} {
                  # loft solid from group
                  global gPathBrowser
                  set keepgrp $gPathBrowser(currGroupName)
                  set gPathBrowser(currGroupName) $j
                  #puts "align"
                  #vis_img_SolidAlignProfiles;
                  #puts "fit"
                  #vis_img_SolidFitCurves;
                  #puts "loft"
                  #vis_img_SolidLoftSurf;
                  #vis_img_SolidCapSurf;
                  # set it back to original
                  global gRen3dFreeze
                  set oldFreeze $gRen3dFreeze
                  set gRen3dFreeze 1
                  nateAFLB
                  set gRen3dFreeze $oldFreeze
                  set gPathBrowser(currGroupName) $keepgrp
    }
              }
              break
     }
           cancel {
              return -code error "Missing solid branches.  Create preop model failed."
     }
           no {
              return -code error "Missing solid braches.  Create preop model failed."
     }
         }

      } elseif {[repos_type -obj $cursolid] != "SolidModel"} {
         puts  "ERROR: Expected SolidModel for $cursolid."
         return -code error "ERROR: Expected SolidModel for $cursolid."
      }
    }

    set shortname [lindex $createPREOPgrpKeptSelections 0]
    set cursolid $gLoftedSolids($shortname)
    solid_copy -src $cursolid -dst $modelname
    puts "copy $cursolid to preop model."

    foreach i [lrange $createPREOPgrpKeptSelections 1 end] {
      set cursolid $gLoftedSolids($i)
      puts "union $cursolid into preop model."
      if {[repos_type -obj $cursolid] != "SolidModel"} {
         puts "Warning:  $cursolid is being ignored."
         continue
      }
       solid_union -result /tmp/preop/$modelname -a $modelname -b $cursolid
       repos_delete -obj $modelname
       solid_copy -src /tmp/preop/$modelname -dst $modelname
       repos_delete -obj /tmp/preop/$modelname
    }

    if {[repos_exists -obj /tmp/preop/$modelname] == 1} {
      repos_delete -obj /tmp/preop/$modelname
    }

    crd_ren gRenWin_3D_ren1
    set pretty_names {}
    set all_ids {}
    foreach i [$modelname GetFaceIds] {
      catch {lappend pretty_names [$modelname GetFaceAttr -attr gdscName -faceId $i]}
      lappend all_ids $i
    }
    set isdups 0
    if {[llength [lsort -unique $pretty_names]] != [llength $pretty_names]} {
     set isdups 1
     set duplist [lsort -dictionary $pretty_names]
     foreach i [lsort -unique $pretty_names] {
        set idx [lsearch -exact $duplist $i]
        set duplist [lreplace $duplist $idx $idx]
     }
     set msg "Duplicate faces found!\n\n"
     set duplistids {}
     foreach dup $duplist {
       set id [lindex $all_ids [lindex [lsearch -exact -all $pretty_names $dup] end]]
       lappend duplistids $id
     }
     for {set i 0} {$i < [llength $duplist]} {incr i} {
       set dup [lindex $duplist $i]
       set dupid [lindex $duplistids $i]
       set newname [string trim $dup]_2
       set msg "$msg  Duplicate face name $dup is being renamed to $newname\n"
       $modelname SetFaceAttr -attr gdscName -faceId $dupid -value $newname
     }
  }

  guiSV_model_add_faces_to_tree $kernel $modelname
  guiSV_model_display_only_given_model $modelname 1
  if {$isdups == 1} {
    tk_messageBox -title "Duplicate Face Names" -type ok -message $msg
  }
}

proc guiSV_model_create_discrete_model_from_polydata {} {
  global guiTRIMvars
  global symoblicName
  global gOptions 
  global gKernel 

  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model allowed to create Discrete at a time"
  }
  if {$gKernel($model) != "PolyData"} {
    return -code error "ERROR: Must use PolyData to create Discrete Model"
  }
  set modelpd /models/PolyData/$model
  if {[repos_exists -obj $modelpd]} {
    $model GetPolyData -result $modelpd
  }

  set gOptions(meshing_solid_kernel) Discrete
  set kernel Discrete
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set newmodel [guiSV_model_new_surface_name 0]
  set newmodelpd /models/$kernel/$newmodel
  catch {repos_delete -obj $newmodel}
  if {[model_create $kernel $newmodel] != 1} {
    guiSV_model_delete_model $kernel $newmodel
    catch {repos_delete -obj $newmodelpd}
    model_create $kernel $newmodel
  }

  if [catch {solid_poly3dSolid -result $newmodel -src $modelpd -facet Union -angle $guiTRIMvars(discrete_angle)} msg] {
    return -code error "ERROR creating solid ($msg)"
  }
  tk_messageBox -message "Model creation complete.\nModel has [llength [$newmodel GetFaceIds]] faces."
  #$outModel WriteNative -file $fn
  global gDiscreteModelFaceNames
  global gPolyDataFaceNames
  set allids [$newmodel GetFaceIds]
  foreach id $allids {
    if {[model_idface PolyData $model $id] == -1} {
      set gDiscreteModelFaceNames($id) "noname_$id"
    } else {
      set gDiscreteModelFaceNames($id) [model_idface PolyData $model $id]
    }
  }
  guiSV_model_add_faces_to_tree $kernel $newmodel
  guiSV_model_display_only_given_model $newmodel 1
}

proc guiSV_model_create_local_surface_macro {type} {
  global gui3Dvars
  global symbolicName
  global gKernel
  global gNumPickedCells
  global gPickedCellIds

  set tv $symbolicName(guiSV_model_tree)

  set addstr {}
  if {$type == "sphere"} {
    set addstr "sphere $gui3Dvars(sphereRadius) $gui3Dvars(sphereCenter) ActiveCells 1\n"
  } elseif {$type == "faces"} {
    set model [guiSV_model_get_tree_current_models_selected]
    if {[llength $model] != 1} {
      return -code error "ERROR: Only one model can be selected for local operations" 
    }
    set kernel $gKernel($model)
    set selected [guiSV_model_get_tree_current_faces_selected]
    set changelist {}
    foreach name $selected {
      if {[lindex $name 1] != ""} {
	set face [lindex $name 1]
	lappend changelist [lindex [$tv item .models.$kernel.$model.$face -values] 1] 
      }
    }
    set addstr "faces [lrange $changelist 0 end] ModelFaceID ActiveCells 1\n"
  } elseif {$type == "cells"} {
    set changelist {}
    for {set i 1} {$i <= $gNumPickedCells} {incr i} {
      lappend changelist $gPickedCellIds($i)
    }
    set addstr "cells [lrange $changelist 0 end] ActiveCells 1\n"
  } elseif {$type == "blend"} {
    set model [guiSV_model_get_tree_current_models_selected]
    if {[llength $model] != 1} {
      return -code error "ERROR: Only one model can be selected for local operations" 
    }
    set kernel $gKernel($model)
    set selected [guiSV_model_get_tree_current_faces_selected]
    set faceids {}
    foreach name $selected {
      if {[lindex $name 1] != ""} {
	set face [lindex $name 1]
	lappend faceids [lindex [$tv item .models.$kernel.$model.$face -values] 1] 
      }
    }
    set addstr "blend [lrange $faceids 0 end] $gui3Dvars(blendSphereRadius) ModelFaceID ActiveCells 1\n"
  } else {
    return -code error "Invalid type to create local operation macro"
  }

  $symbolicName(guiLocalSurfaceOperationParametersTextBox) insert end $addstr
}

proc guiSV_model_send_selected_to_3D_segmentation {} {
  global symbolicName
  global gKernel
  global gOptions
  global gSeg3D

  set tv $symbolicName(guiSV_group_tree)
  set selected [guiSV_model_get_tree_current_models_selected]
  foreach model $selected {
    set kernel $gKernel($model)
    if {$kernel != "PolyData"} {
      puts "Only can turn a PolyData surface into a 3D seed surface, skipping!"
      continue
    }
    set objName [string trim $model]_3D
    catch {repos_delete -obj $objName}
    $model GetPolyData -result $objName
    if {[seg3d_add $model $objName] !=0} {

      set yesno [tk_messageBox -default no  \
      -message "Do you want to replace $model? with this surface? This cannot be undone." \
      -title "Confirm overwrite surface"  -type yesno]
      switch -- $yesno {
	yes {
	  puts "forcing"
	  seg3d_addForce $model $objName
	}
	no {
	 return
        }
      }

    }
    repos_setLabel -obj $objName -key color -value $gOptions(color_for_saved_surface)
    repos_setLabel -obj $objName -key opacity -value $gOptions(opacity_for_saved_surface) 
    guiSV_group_update_tree
    DestroyWindow.svSaveSegWindow
    guiSV_model_display_selected_full_model 0

    $tv see .groups.3d.$model
    $tv selection set .groups.3d.$model
    guiSV_solid_display_selected_groups 1
  }
}

proc guiSV_model_convert_centerlines_to_pathlines {} {
  global guiSVvars
  global guiPDvars
  global symbolicName 
  global gPathPoints
  set tv $symbolicName(guiSV_path_tree)

  set centerlines $guiPDvars(centerlines)
  set centerlinepd /tmp/polydata/centerlines 
  catch {$centerlinepd Delete}
  vtkPolyData $centerlinepd
  set centerlinepd [repos_exportToVtk -src $centerlines]
  #$centerlinepd BuildLinks 0

  set currentPaths [$tv children .paths.all]
  set maxid 0
  foreach path $currentPaths {
    set checkid [lindex [split $path "."] end]
    if {$checkid > $maxid} { set maxid $checkid }
  }
  set numLines [$centerlinepd GetNumberOfLines]

  incr maxid
  for {set i 0} {$i < $numLines} {incr i} {
    set guiSVvars(path_entry_path_id) $maxid
    set guiSVvars(path_entry_path_name) [string trim $centerlines]_$i
    guiSV_path_insert_new_path
    $tv selection set .paths.all.$maxid

    set pointids /tmp/vtk/pointids
    catch {$pointids Delete}
    vtkIdList $pointids
    $centerlinepd GetCellPoints $i $pointids

    for {set j 0} {$j < [$pointids GetNumberOfIds]} {incr j} {
      set id [$pointids GetId $j]
      set pt [$centerlinepd GetPoint $id]
      #guiPPchooserAddSpecifiedPoint $pt
      set maxnum [llength [$tv children .paths.all.$maxid]]
      set gPathPoints($maxid,$maxnum) $pt
      $tv insert .paths.all.$maxid end -id .paths.all.$maxid.$maxnum -text $pt
      set gPathPoints($maxid,splinePts) {}
    }
    guiSV_path_update_tree
    $tv selection set .paths.all.$maxid
    guiPPchooserSplinePts $maxid
    incr maxid
  }
}

proc guiSV_model_add_to_backup_list {kernel model} {
  global gDetached
  global symbolicName

  set backmodel "[string trim $model]_backup"

  set name [string trim $kernel]_$backmodel
  set inlist [lsearch -exact -all -regexp $gDetached $name]
  if {$inlist != ""} {
    set first [lindex $gDetached [lindex $inlist 0]]
    set last [lindex $gDetached [lindex $inlist end]] 
    set num [expr [lindex [split $last "_"] end]+1]
    set loc [string last "_" $last]
    set start [string range $last 0 $loc]
    set name "[string trim $start]$num"
    if {[llength $inlist] > 9} {
      catch {repos_delete -obj $first}
      model_delete $kernel $first
    }
  } else {
    set name "[string trim $name]_0"
  }
  lappend gDetached $name

  catch {repos_delete -obj $name}
  solid_copy -src $model -dst $name
  model_create $kernel $name 

  set tv $symbolicName(guiSV_model_tree)
  $tv insert .models.$kernel end -id .models.$kernel.$name -text "$name" -values {}
  if {$kernel == "PolyData"} {
    if {[guiSV_model_check_array_exists $name 1 "ModelFaceID"]} {
      foreach id [$name GetFaceIds] {
	set facename [model_idface $kernel $model $id]
	if {$facename == -1} {
	  set facename "noname_$id"
	}
	model_add $name $facename $facename
	$tv insert .models.$kernel.$name end -id .models.$kernel.$name.$facename -text "$facename"
        guiSV_model_set_col_value $kernel.$name.$facename 1 $id
      }
    }
  } else {
    foreach id [$name GetFaceIds] {
      if {$kernel == "Parasolid"} {
        catch {set facename [$model GetFaceAttr -attr gdscName -faceId $id]}
      } else {
        set facename [model_idface $kernel $model $id]
      }
      model_add $name $facename $facename
      $tv insert .models.$kernel.$name end -id .models.$kernel.$name.$facename -text "$facename"
    }
  }

  $tv detach .models.$kernel.$name
}

proc guiSV_model_update_view_model {kernel model} {
  global symbolicName
  global gOptions
  global gKernel
  global gPolyDataFaceNames
  global gRen3d

  set kernel $gKernel($model)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  set tv $symbolicName(guiSV_model_tree)

  set hasfaces 0
  if {$kernel == "Parasolid"} {
    set hasfaces 1
  } elseif {$kernel == "PolyData"} {
    if [guiSV_model_check_array_exists $model 1 "ModelFaceID"] {
      set hasfaces 1
    }
  }

  set modelpd /models/$kernel/$model
  if {[repos_exists -obj $modelpd]} {
    foreach key [repos_getLabelKeys -obj $modelpd] {
      set tags($key) [repos_getLabel -obj $modelpd -key $key]
    }
    catch {repos_delete -obj $modelpd}
    vis_pRm $gRen3d $modelpd
    $model GetPolyData -result $modelpd
    foreach key [array names tags]  {
      repos_setLabel -obj $modelpd -key $key -value $tags($key)
    }
  }
  if {$hasfaces == 1} {
    set faceids [$model GetFaceIds]
    foreach id $faceids {
      set face [model_idface $kernel $model $id]
      set facepd /models/$kernel/$model/$face
      if {[repos_exists -obj $facepd]} {
	foreach key [repos_getLabelKeys -obj $facepd] {
	  set tags($key) [repos_getLabel -obj $facepd -key $key]
	}
	catch {repos_delete -obj $facepd}
	vis_pRm $gRen3d $facepd
	$model GetFacePolyData -result $facepd -face $id
	foreach key [array names tags]  {
	  repos_setLabel -obj $facepd -key $key -value $tags($key)
	}
      }
    }
    if {$kernel == "PolyData"} {
      catch {unset gPolyDataFaceNames}
      set faceids [$model GetFaceIds]
      foreach id $faceids {
	set gPolyDataFaceNames($id) [model_idface $kernel $model $id]
      }
    } elseif {$kernel == "Discrete"} {
      catch {unset gDiscreteModelFaceNames}
      set faceids [$model GetFaceIds]
      foreach id $faceids {
	set gDiscreteModelFaceNames($id) [model_idface $kernel $model $id]
      }
    }
    set faces [model_get $model]
    if {[llength $faceids] != [llength $faces]} {
      guiSV_model_remove_faces_from_tree $kernel $model
      guiSV_model_add_faces_to_tree $kernel $model
      guiSV_model_update_tree
    }
  } else {
    guiSV_model_remove_faces_from_tree $kernel $model
  }
  set col3val [lindex [$tv item .models.$kernel.$model -values] 2]
  if {$col3val == "X" && $hasfaces == 1} {
    guiSV_model_display_model_all_faces 1 $kernel $model
  } else {
    guiSV_model_display_model 1 $kernel $model
  }
}

proc guiSV_model_remove_faces_from_tree {kernel model} {
  global symbolicName
  global gRen3d

  set tv $symbolicName(guiSV_model_tree)
  foreach item [model_iditems $model {}] {
    if {[$tv exists .models.$kernel.$model.$item] == 1} {
      catch {$tv delete .models.$kernel.$model.$item}
      model_remove $model $item
      catch {repos_delete -obj /models/$kernel/$model/$item}
      vis_pRm $gRen3d /models/$kernel/$model/$item
    }
  }
}

proc guiSV_model_get_older_version_selected_model {} {
  global gOptions
  global guiPDvars
  global gPolyDataFaceNames
  global smasherInputName
  global symbolicName
  global gKernel

  set tv $symbolicName(guiSV_model_tree)

  set model [guiSV_model_get_tree_current_models_selected]
  set selected [guiSV_model_get_tree_current_faces_selected]

  if {[llength $model] != 1} {
    return -code error "ERROR: One model needs to be selected to get backup"
  }
  #set kernel $gOptions(meshing_solid_kernel)
  set kernel $gKernel($model)
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  guiSV_model_get_older_version_model $kernel $model
}

proc guiSV_model_get_older_version_model {kernel model} {
  global gDetached
  global gSelectDetached
  global gPolyDataFaceNames
  global symbolicName

  set name [string trim $kernel]_$model
  set inlist [lsearch -exact -all -regexp $gDetached $name]
  if {$inlist == -1} {
    return -code error "No backups for selected model"
  }

  set backups [get_backup_versions $kernel $model]
  ::swaplist::swaplist .guiCV.get_backup_versions gSelectDetached [lsort -dictionary [get_backup_versions $kernel $model]] $gSelectDetached -title {Select Backup Version} -llabel {Options} -rlabel {Selected}

  set tv $symbolicName(guiSV_model_tree)
  foreach oldmodel $gSelectDetached {
    $tv move .models.$kernel.$oldmodel .models.$kernel end
  }
  guiSV_model_update_tree
  set gSelectDetached {}
}

proc get_backup_versions {kernel model} {
  global gDetached
  global gSelectDetached

  set name [string trim $kernel]_$model
  set inlist [lsearch -exact -all -regexp $gDetached $name]

  set return_mods {}
  for {set i 0} {$i < [llength $gDetached]} {incr i} {
    if {[lsearch -exact $inlist $i] != -1} {
      lappend return_mods [lindex $gDetached $i]
    }
  }
  return $return_mods
 } 

proc guiSV_model_undo {} {
  global gDetached

  if {[llength $gDetached] == 0} {
    return
  }
  set model [lindex $gDetached end]
  set loc [string first "_" $model]
  set loc2 [string first "_" $model [expr [string length $model]-10]]
  set kernel [string range $model 0 [expr $loc-1]]
  set name [string range $model [expr $loc+1] [expr $loc2-1]]
  if {$kernel == "Discrete"} {
    return -code error "Function is not available for Discrete Kernel"
  }
  guiSV_model_copy_model $kernel $model $name rename
  guiSV_model_update_view_model $kernel $name
  guiSV_model_delete_model $kernel $model
}

proc guiSV_model_create_polydata_solid_from_parasolid {} {
  global guiTRIMvars
  global symoblicName
  global gOptions 
  global gKernel 
  global guiSVvars

  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Only one model allowed to create Discrete at a time"
  }
  if {$gKernel($model) != "Parasolid"} {
    return -code error "ERROR: Must use a Parasolid model to create PolyData Model"
  }
  set kernel $gKernel($model)
  set modelpd /tmp/models/$kernel/$model
  solid_setKernel -name Parasolid
  if {[repos_exists -obj $modelpd] == 1} {
    catch {repos_delete -obj $modelpd}
  }
  $model GetPolyData -result $modelpd -max_edge_size $guiSVvars(facet_max_edge_size)

  set facevtklist {}
  set facenames {}
  set idlist {}
  foreach faceid [$model GetFaceIds] { 
    catch {set facename [$model GetFaceAttr -attr gdscName -faceId $faceid]}
    lappend facenames $facename
    set facepd /tmp/models/$kernel/$model/$facename
    if {[repos_exists -obj $facepd] == 1} {
      catch {repos_delete -obj $facepd}
    }
    $model GetFacePolyData -face $faceid -result $facepd -max_edge_size $guiSVvars(facet_max_edge_size)
    lappend facevtklist $facepd
    lappend idlist $faceid
  }

  set gOptions(meshing_solid_kernel) PolyData
  set kernel PolyData
  solid_setKernel -name $kernel

  set newmodel [guiSV_model_new_surface_name 0]
  set newmodelpd /models/$kernel/$newmodel
  if {[model_create $kernel $newmodel] != 1} {
    guiSV_model_delete_model $kernel $newmodel
    catch {repos_delete -obj $newmodelpd}
    model_create $kernel $newmodel
  }

  model_name_model_from_polydata_names -model $modelpd -facelist $facevtklist -ids $idlist -result $newmodel

  global gPolyDataFaceNames
  set allids [$newmodel GetFaceIds]
  foreach id $allids {
    set loc [lsearch -exact $idlist $id] 
    set newname [lindex $facenames $loc]
    set gPolyDataFaceNames($id) $newname
  }
  guiSV_model_add_faces_to_tree $kernel $newmodel
  guiSV_model_display_only_given_model $newmodel 1
}

proc guiSV_model_name_faces_from_reference {newmodel refmodel} {
  global guiSVvars
  global gOptions
  global gKernel

  set kernel $gKernel($refmodel)
  set modelpd /tmp/models/$kernel/$refmodel
  solid_setKernel -name $kernel
  if {[repos_exists -obj $modelpd] == 1} {
    catch {repos_delete -obj $modelpd}
  }

  if {$kernel == "Parasolid"} {
    $refmodel GetPolyData -result $modelpd -max_edge_side $guiSVvars(facet_max_edge_size)
  } else {
    $refmodel GetPolyData -result $modelpd
  }

  set facevtklist {}
  set facenames {}
  set idlist {}
  foreach faceid [$refmodel GetFaceIds] { 
    set facename {}
    if {$kernel == "Parasolid"} {
      catch {set facename [$refmodel GetFaceAttr -attr gdscName -faceId $faceid]}
    } else {
      set facename [model_idface $kernel $refmodel $faceid] 
    }
    lappend facenames $facename
    set facepd /tmp/models/$kernel/$refmodel/$facename
    if {[repos_exists -obj $facepd] == 1} {
      catch {repos_delete -obj $facepd}
    }
    $refmodel GetFacePolyData -face $faceid -result $facepd -max_edge_size $guiSVvars(facet_max_edge_size)
    lappend facevtklist $facepd
    lappend idlist $faceid
  }

  set kernel $gKernel($newmodel)
  solid_setKernel -name $kernel
  set newmodelpd /tmp/models/$kernel/$newmodel
  catch {repos_delete -obj $newmodelpd}
  $newmodel GetPolyData -result $newmodelpd
  guiSV_model_delete_model $kernel $newmodel

  model_name_model_from_polydata_names -model $newmodelpd -facelist $facevtklist -ids $idlist -result $newmodel

  model_create $kernel $newmodel

  global gPolyDataFaceNames
  set allids [$newmodel GetFaceIds]
  foreach id $allids {
    set loc [lsearch -exact $idlist $id] 
    set newname [lindex $facenames $loc]
    set gPolyDataFaceNames($id) $newname
  }
  guiSV_model_add_faces_to_tree $kernel $newmodel
  guiSV_model_display_only_given_model $newmodel 1


}
