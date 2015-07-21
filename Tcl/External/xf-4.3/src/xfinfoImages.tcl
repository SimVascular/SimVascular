# Program: xf
# Description: info routine for procedures
#
# $Header: xfinfoImgs.tcl[2.5] Wed Mar 2? 1996 mcody@dfw.net frozen $

##########
# Procedure: XFInfoImages
# Description: show the selected information
# Arguments: xfTarget - put current selection to this entry
#            xfSelectImg - image name to select initially
# Returns: none
# Sideeffects: none
##########
proc XFInfoImages {xfTarget {xfSelectImg ""}} {
  global xfBind
  global xfConf
  global xfStatus

  XFEditSetStatus "Calling image list..."

  # building widget structure
  XFTmpltToplevel .xfInfoImg 400x500 {XF images}

  XFTmpltFrame .xfInfoImg.frame1 0

  button .xfInfoImg.frame1.add \
    -text {Insert} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoImg.current.current get]" != ""} {
        set xfCurrentName [.xfInfoImg.current.current get]
      }
      if {"$xfCurrentName" != ""} {
        if {[lsearch [image names] $xfCurrentName] >= 0} {
          XFProcError "Image with that name exists"
          return
        }
      }
      XFInfoImagesSet $xfCurrentName
    }

  button .xfInfoImg.frame1.edit \
    -text {Edit} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoImg.current.current get]" != ""} {
        set xfCurrentName [.xfInfoImg.current.current get]
      } {
        if {"[.xfInfoImg.imgs.imgs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoImg.imgs.imgs get [.xfInfoImg.imgs.imgs curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        XFInfoImagesSet $xfCurrentName
      }}

  button .xfInfoImg.frame1.remove \
    -text {Remove} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoImg.current.current get]" != ""} {
        set xfCurrentName [.xfInfoImg.current.current get]
      } {
        if {"[.xfInfoImg.imgs.imgs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoImg.imgs.imgs get [.xfInfoImg.imgs.imgs curselect]]
        }
      }
      if {"$xfCurrentName" != ""} {
        if {[lsearch [image names] $xfCurrentName] == "-1"} {
          XFProcError "The image does not exist"
          return
        }
        catch "image delete $xfCurrentName"
        XFMiscSetText .xfInfoImg.current.current ""
        XFMiscSetInfo images .xfInfoImg.imgs.imgs 1
      }}

  button .xfInfoImg.frame1.help \
    -text {Help} \
    -command {
      set xfCurrentName ""
      if {"[.xfInfoImg.current.current get]" != ""} {
        set xfCurrentName [.xfInfoImg.current.current get]
      } {
        if {"[.xfInfoImg.imgs.imgs curselect]" != ""} {
          set xfCurrentName \
            [.xfInfoImg.imgs.imgs get [.xfInfoImg.imgs.imgs curselect]]
        }
      }
      XFProcHelpHelp * commands $xfCurrentName}

  XFTmpltFrame .xfInfoImg.frame2 0

  radiobutton .xfInfoImg.frame2.include \
    -text {Include} \
    -value {Include} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 1
      set xfStatus(includeExcludeString) \
        [.xfInfoImg.pattern.pattern get]
      XFMiscSetInfo images .xfInfoImg.imgs.imgs 1}

  radiobutton .xfInfoImg.frame2.exclude \
    -text {Exclude} \
    -value {Exclude} \
    -command {
      global xfStatus
      set xfStatus(includeExclude) 0
      set xfStatus(includeExcludeString) \
        [.xfInfoImg.pattern.pattern get]
      XFMiscSetInfo images .xfInfoImg.imgs.imgs 1}

  if {$xfStatus(includeExclude)} {
    .xfInfoImg.frame2.include select
  } {
    .xfInfoImg.frame2.exclude select
  }

  XFTmpltFrame .xfInfoImg.frame3 0

  button .xfInfoImg.frame3.ok \
    -text {OK} \
    -command { destroy .xfInfoImg }

  button .xfInfoImg.frame3.rescan \
    -text {Rescan} \
    -command "XFMiscSetInfo images .xfInfoImg.imgs.imgs 1"

  checkbutton .xfInfoImg.frame3.rescanperm \
    -text {Rescan permanently} \
    -variable xfStatus(rescanInfo) \
    -onvalue 1 \
    -offvalue 0

  XFTmpltListbox .xfInfoImg imgs

  XFTmpltFrame .xfInfoImg.type 0
  menubutton .xfInfoImg.type.type -text {Image type:} -menu .xfInfoImg.type.type.m
  menu .xfInfoImg.type.type.m -tearoff 0
  set typelist [lsort [image types]]
  foreach imgtype $typelist {
    set comstr [list XFInfoImagesNew $imgtype]
    .xfInfoImg.type.type.m add command -label $imgtype -command $comstr
  }

  label .xfInfoImg.type.val -anchor w

  XFTmpltLabledEntry .xfInfoImg current {Name:} {}
  XFTmpltLabledEntry .xfInfoImg data {Data:} {}
  XFTmpltLabledEntry .xfInfoImg file {File:} {}

  XFTmpltFrame .xfInfoImg.frame4 0


  XFTmpltLabledEntry .xfInfoImg pattern {Pattern:} \
    $xfStatus(includeExcludeString)

  XFMiscSetInfo images .xfInfoImg.imgs.imgs 1

  if {"$xfSelectImg" == ""} {
    catch ".xfInfoImg.imgs.imgs get 0" xfSelectImg
  } {
    set xfCounter 0
    set xfLast [.xfInfoImg.imgs.imgs size]
    if {"$xfLast" == "none"} {
      set xfLast -1
    }
    while {$xfCounter < $xfLast} {
      if {"$xfSelectImg" == "[.xfInfoImg.imgs.imgs get $xfCounter]"} {
        .xfInfoImg.imgs.imgs select anchor $xfCounter
        .xfInfoImg.imgs.imgs select set $xfCounter
        break
      }
      incr xfCounter
    }
  }

  XFMiscSetText .xfInfoImg.current.current $xfSelectImg

  # bindings
  if {"$xfTarget" != ""} {
    bind .xfInfoImg.imgs.imgs $xfBind(select1) "
      if {\[%W size\] > 0} {
        XFBindSelectOneIntoEntry %W %y .xfInfoImg.current.current
        XFMiscInsertTextIntoWidget \"$xfTargetP\" \
          \[.xfInfoImg.current.current get\]
        destroy .xfInfoImg
      }"
  }

  bind .xfInfoImg.pattern.pattern <Return> {
    global xfStatus
    set xfStatus(includeExcludeString) \
      [.xfInfoImg.pattern.pattern get]
    XFMiscSetInfo images .xfInfoImg.imgs.imgs 1}

  bind .xfInfoImg.imgs.imgs <ButtonPress-1> {
    XFInfoImagesSelect %W %y}
  bind .xfInfoImg.imgs.imgs <Button1-Motion> {
    XFInfoImagesSelect %W %y}
  bind .xfInfoImg.imgs.imgs <Shift-Button1-Motion> {
    XFInfoImagesSelect %W %y}
  bind .xfInfoImg.imgs.imgs <Shift-ButtonPress-1> {
    XFInfoImagesSelect %W %y}

  # packing
  pack append .xfInfoImg.frame1 \
              .xfInfoImg.frame1.add {left fill expand} \
              .xfInfoImg.frame1.edit {left fill expand} \
              .xfInfoImg.frame1.remove {left fill expand} \
              .xfInfoImg.frame1.help {left fill expand}
  pack append .xfInfoImg.frame2 \
              .xfInfoImg.frame2.include {left fill expand} \
              .xfInfoImg.frame2.exclude {left fill expand}
  pack append .xfInfoImg.frame3 \
              .xfInfoImg.frame3.ok {left fill expand} \
              .xfInfoImg.frame3.rescan {left fill expand} \
              .xfInfoImg.frame3.rescanperm {left fill expand}

# set up widgets for image parameters
  XFInfoImagesParams $xfSelectImg

  pack append .xfInfoImg.type \
              .xfInfoImg.type.type {left} \
              .xfInfoImg.type.val {left fill expand}

  pack append .xfInfoImg \
              .xfInfoImg.frame3 {bottom fill} \
              .xfInfoImg.frame2 {bottom fill} \
              .xfInfoImg.pattern {bottom fill} \
              .xfInfoImg.frame1 {bottom fill} \
              .xfInfoImg.frame4 {bottom fill expand} \
              .xfInfoImg.data {bottom fill} \
              .xfInfoImg.file {bottom fill} \
              .xfInfoImg.current {bottom fill} \
              .xfInfoImg.type {bottom fill} \
              .xfInfoImg.imgs {top expand fill}

  XFEditSetStatus "Calling image list...done"
}


##########
# Procedure: XFInfoImagesNew
# Description: set up for new image
# Arguments: imgtype - the image type
# Returns: none
# Sideeffects: none
##########
proc XFInfoImagesNew {imgtype} {
  XFMiscSetText .xfInfoImg.current.current ""
  .xfInfoImg.type.val config -text $imgtype
  image create $imgtype xfTmpImage
  XFInfoImagesParams xfTmpImage
  image delete xfTmpImage
}


##########
# Procedure: XFInfoImagesParams
# Description: set up parameters
# Arguments: img - the image name
# Returns: none
# Sideeffects: none
##########
proc XFInfoImagesParams {img} {
  # remove current children of widget .xfInfoImg.frame4
  foreach child [winfo children .xfInfoImg.frame4] {
    destroy $child
  }

  if {"$img" != ""} {
    set imgtype [image type $img]
    set config_list [$img config]
  } {
    set imgtype bitmap
    set config_list \
       "\{-background \{\} \{\} \{\} \{\} \} \
        \{-data \{\} \{\} \{\} \{\} \} \
        \{-file \{\} \{\} \{\} \{\} \} \
        \{-foreground \{\} \{\} #000000 #000000 \} \
        \{-maskdata \{\} \{\} \{\} \{\} \} \
        \{-maskfile \{\} \{\} \{\} \{\} \}"
  }

  .xfInfoImg.type.val config -text $imgtype
  foreach param $config_list {
    switch -- [lindex $param 0] {
      -file { XFMiscSetText .xfInfoImg.file.file [lindex $param 4] }
      -data { XFMiscSetText .xfInfoImg.data.data [lindex $param 4] }
      default { set child [string range [lindex $param 0] 1 end]
        set title [string toupper [string index $child 0]][string range $child 1 end]
        XFTmpltLabledEntry .xfInfoImg.frame4 $child $title: [lindex $param 4]
        pack append .xfInfoImg.frame4 .xfInfoImg.frame4.$child {top fill}
      }
    }
  }
}

##########
# Procedure: XFInfoImagesSelect
# Description: select a procedure
# Arguments: xfImgName - the procedure name
# Returns: none
# Sideeffects: none
##########
proc XFInfoImagesSelect {xfW xfY} {
  global xfConf
  global xfStatus

  if {[$xfW size] > 0} {
    XFBindSelectOneIntoEntry $xfW $xfY .xfInfoImg.current.current
    set xfStatus(imgIndex) [$xfW nearest $xfY]
    set xfStatus(imgName) [.xfInfoImg.current.current get]
    set xfSelectImg $xfStatus(imgName)
    XFInfoImagesParams $xfSelectImg
  }
}

##########
# Procedure: XFInfoImagesSet
# Description: insert the current procedure
# Arguments: xfImgName - the procedure name
# Returns: none
# Sideeffects: none
##########
proc XFInfoImagesSet {xfImgName} {
  global xfConf

  set type [.xfInfoImg.type.val cget -text]
  set attrb [.xfInfoImg.data.data get]
  set xfAttrbs {}
  if {$attrb != ""} {
    lappend xfAttrbs -data $attrb
  }    
  set attrb [.xfInfoImg.file.file get]    
  if {$attrb != ""} {
    lappend xfAttrbs -file $attrb
  }

  foreach child [winfo children .xfInfoImg.frame4] {
    set child [split $child .]
    set attrb [lindex $child end]
    lappend child $attrb
    set child [join $child .]
    set val [$child get]
    if {$val != ""} {
      lappend xfAttrbs -$attrb $val
    }
  }

  if {[catch "image create $type $xfImgName $xfAttrbs " xfResult]} {
    XFProcError "$xfResult"
  } {
    XFMiscSetInfo images .xfInfoImg.imgs.imgs 0
    XFMiscSetText .xfInfoImg.current.current "$xfResult"
  }
}

# eof

