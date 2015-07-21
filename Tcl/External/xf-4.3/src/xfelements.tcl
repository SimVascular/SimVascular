# Program: xf
# Description: the elements used for configure windows
#
# $Header: xfelements.tcl[2.4] Wed Mar 10 12:05:47 1993 garfield@garfield frozen $

##########
# Procedure: XFElementAnchor
# Description: build anchor frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementAnchor {xfW xfRoot xfType xfClass xfFunction} {
  global xfMisc

  XFTmpltFrame $xfRoot.params1.params2.anchor 0

  label $xfRoot.params1.params2.anchor.label1 \
    -text "Anchor:"

  XFTmpltFrame $xfRoot.params1.params2.anchor.anchor 0

  XFTmpltFrame $xfRoot.params1.params2.anchor.anchor.f1 0

  XFTmpltFrame $xfRoot.params1.params2.anchor.anchor.f2 0

  XFTmpltFrame $xfRoot.params1.params2.anchor.anchor.f3 0

  radiobutton $xfRoot.params1.params2.anchor.anchor.f1.nw \
    -anchor w \
    -text {NW} \
    -value nw \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f1.w \
    -anchor w \
    -text {W} \
    -value w \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f1.sw \
    -anchor w \
    -text {SW} \
    -value sw \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f2.n \
    -anchor w \
    -text {N} \
    -value n \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f2.c \
    -anchor w \
    -text {C} \
    -value c \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f2.s \
    -anchor w \
    -text {S} \
    -value s \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f3.ne \
    -anchor w \
    -text {NE} \
    -value ne \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f3.e \
    -anchor w \
    -text {E} \
    -value e \
    -variable xfMisc(anchor)

  radiobutton $xfRoot.params1.params2.anchor.anchor.f3.se \
    -anchor w \
    -text {SE} \
    -value se \
    -variable xfMisc(anchor)

  if {"$xfType" == "add"} {
    $xfRoot.params1.params2.anchor.anchor.f2.c select
  } {
    case [lindex [$xfW configure -anchor] 4] in {
      {nw} {
        $xfRoot.params1.params2.anchor.anchor.f1.nw select
      }
      {w} {
        $xfRoot.params1.params2.anchor.anchor.f1.w select
      }
      {sw} {
        $xfRoot.params1.params2.anchor.anchor.f1.sw select
      }
      {n} {
        $xfRoot.params1.params2.anchor.anchor.f2.n select
      }
      {c} {
        $xfRoot.params1.params2.anchor.anchor.f2.c select
      }
      {s} {
        $xfRoot.params1.params2.anchor.anchor.f2.s select
      }
      {ne} {
        $xfRoot.params1.params2.anchor.anchor.f3.ne select
      }
      {e} {
        $xfRoot.params1.params2.anchor.anchor.f3.e select
      }
      {se} {
        $xfRoot.params1.params2.anchor.anchor.f3.se select
      }
      {default} {
        $xfRoot.params1.params2.anchor.anchor.f2.c select
      }
    }
  }
  if {"$xfType" != "add"} {
    $xfRoot.params1.params2.anchor.anchor.f1.nw configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f1.w configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f1.sw configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f2.n configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f2.c configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f2.s configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f3.ne configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f3.e configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.anchor.anchor.f3.se configure \
      -command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.anchor.anchor.f1 \
              $xfRoot.params1.params2.anchor.anchor.f1.nw {top fillx} \
              $xfRoot.params1.params2.anchor.anchor.f1.w {top fillx} \
              $xfRoot.params1.params2.anchor.anchor.f1.sw {top fillx}
  pack append $xfRoot.params1.params2.anchor.anchor.f2 \
              $xfRoot.params1.params2.anchor.anchor.f2.n {top fillx} \
              $xfRoot.params1.params2.anchor.anchor.f2.c {top fillx} \
              $xfRoot.params1.params2.anchor.anchor.f2.s {top fillx}
  pack append $xfRoot.params1.params2.anchor.anchor.f3 \
              $xfRoot.params1.params2.anchor.anchor.f3.ne {top fillx} \
              $xfRoot.params1.params2.anchor.anchor.f3.e {top fillx} \
              $xfRoot.params1.params2.anchor.anchor.f3.se {top fillx}
  pack append $xfRoot.params1.params2.anchor.anchor \
              $xfRoot.params1.params2.anchor.anchor.f1 {left frame center} \
              $xfRoot.params1.params2.anchor.anchor.f2 {left frame center} \
              $xfRoot.params1.params2.anchor.anchor.f3 {left frame center}
  pack append $xfRoot.params1.params2.anchor \
              $xfRoot.params1.params2.anchor.label1 {top frame center} \
              $xfRoot.params1.params2.anchor.anchor {top frame center}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.anchor {top fillx pady 6}  
}

##########
# Procedure: XFElementAspect
# Description: build aspect frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementAspect {xfW xfRoot xfType xfClass xfFunction} {
  global xfStatus

  if {"$xfType" == "add"} {
    XFTmpltScale $xfRoot.params1.params2 aspect \
      "Aspect:" "100*width/height" 2000
    $xfRoot.params1.params2.aspect.aspect set 1500
  } {
    XFTmpltScale $xfRoot.params1.params2 aspect \
      "Aspect:" "100*width/height" 2000 \
        [lindex [$xfW configure -aspect] 4]
  }

  XFMiscSetResource $xfRoot.params1.params2.aspect.aspect \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.aspect.label1 \
    width $xfStatus(elementWidth)
  XFMiscSetResource $xfRoot.params1.params2.aspect.label1 \
    anchor e
  XFMiscSetResource $xfRoot.params1.params2.aspect.label1 \
    relief flat
  if {"$xfType" != "add"} {
    XFMiscSetResource $xfRoot.params1.params2.aspect.aspect \
      command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.aspect {top fillx pady 6}
}

##########
# Procedure: XFElementBitmap
# Description: build bitmap frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementBitmap {xfW xfRoot xfType xfClass xfFunction} {
  global xfBind

  if {"$xfType" == "add"} {
    XFTmpltLabledEntry $xfRoot.params1.params2 bitmap "Bitmap:"
    if {"[$xfW configure -bitmap]" != ""} {
      $xfRoot.params1.params2.bitmap.bitmap insert 0 \
        [lindex [$xfW configure -bitmap] 4]
    }
  } {
    XFTmpltLabledEntry $xfRoot.params1.params2 bitmap "Bitmap:" \
      [lindex [$xfW configure -bitmap] 4]
  }

  XFMiscInitElement $xfRoot bitmap

  # bindings
  bind $xfRoot.params1.params2.bitmap.bitmap $xfBind(configure) "
    XFProcFSBoxPixmap {$xfRoot.params1.params2.bitmap.bitmap}"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.bitmap.bitmap <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.bitmap {top fillx pady 6}
}

##########
# Procedure: XFElementImage
# Description: build image frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementImage {xfW xfRoot xfType xfClass xfFunction} {
  global xfBind

  if {"$xfType" == "add"} {
    XFTmpltLabledEntry $xfRoot.params1.params2 image "Image:"
    if {"[$xfW configure -image]" != ""} {
      $xfRoot.params1.params2.image.image insert 0 \
        [lindex [$xfW configure -image] 4]
    }
  } {
    XFTmpltLabledEntry $xfRoot.params1.params2 image "Image:" \
      [lindex [$xfW configure -image] 4]
  }

  XFMiscInitElement $xfRoot image

  # bindings
  bind $xfRoot.params1.params2.image.image $xfBind(configure) "
    XFProcFSBoxPixmap {$xfRoot.params1.params2.image.image}"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.image.image <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.image {top fillx pady 6}
}

##########
# Procedure: XFElementBoolean
# Description: build boolean frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfName - the name of the resource
#            xfLabel - the label to display
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementBoolean {xfW xfRoot xfType xfClass xfName xfLabel xfFunction} {
  global xfMisc

  set xfMisc($xfName) 0

  XFTmpltFrame $xfRoot.params1.params2.$xfName 0

  checkbutton $xfRoot.params1.params2.$xfName.$xfName \
    -offvalue 0 \
    -onvalue 1 \
    -relief raised \
    -variable xfMisc($xfName) \
    -text "$xfLabel"

  set xfTmpValue [lindex [$xfW configure -[string tolower $xfName]] 4]
  if {"$xfTmpValue" == "1" || "$xfTmpValue" == "on" ||
      "$xfTmpValue" == "true"} {
    $xfRoot.params1.params2.$xfName.$xfName select
  } {
    $xfRoot.params1.params2.$xfName.$xfName deselect
  }
  if {"$xfType" != "add"} {
    $xfRoot.params1.params2.$xfName.$xfName configure \
      -command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.$xfName \
              $xfRoot.params1.params2.$xfName.$xfName {top frame center}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.$xfName {top fillx pady 6}
}

##########
# Procedure: XFElementColor
# Description: build color frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfName - the name of the widget
#            xfResource - the resource
#            xfResClass - the resource class
#            xfLabel - the label
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementColor {xfW xfRoot xfType xfClass xfName xfResource \
                     xfResClass xfLabel xfFunction} {
  global xfBind

  if {"$xfType" == "config" || "$xfW" != "."} {
    XFTmpltLabledEntry $xfRoot.params1.params2 $xfName "$xfLabel:" \
      [lindex [$xfW configure -[string tolower $xfResource]] 4]
  } {
    XFTmpltLabledEntry $xfRoot.params1.params2 $xfName "$xfLabel:"
  }

  XFMiscInitElement $xfRoot $xfName

  # bindings
  bind $xfRoot.params1.params2.$xfName.$xfName $xfBind(configure) \
    "global xfConf
     if {\$xfConf(applyParameters)} {
       XFProcColorBox \"[string tolower [string trimleft $xfResource -]]\" \
	 {$xfRoot.params1.params2.$xfName.$xfName} $xfW
     } {
       XFProcColorBox \"[string tolower [string trimleft $xfResource -]]\" \
	 {$xfRoot.params1.params2.$xfName.$xfName} \
     }"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.$xfName.$xfName <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.$xfName {top fillx pady 6}
}

##########
# Procedure: XFElementCommand
# Description: build command frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
# Returns: none
# Sideeffects: none
##########
proc XFElementCommand {xfW xfRoot xfType {specialFlag 0}} {
  global xfBind

  XFTmpltFrame $xfRoot.params1.params2.command 0

  if {$specialFlag == 0} {
    label $xfRoot.params1.params2.command.label1 \
      -text "Command:"

    XFTmpltText $xfRoot.params1.params2.command command 0 \
      [lindex [$xfW configure -command] 4]
  } {
    label $xfRoot.params1.params2.command.label1 \
      -text "Post command:"

    XFTmpltText $xfRoot.params1.params2.command command 0 \
      [lindex [$xfW configure -postcommand] 4]
  }

  # bindings
  bind $xfRoot.params1.params2.command.command.command $xfBind(configure) \
    "XFInfoCommands $xfRoot.params1.params2.command.command.command"

  # packing of subwidgets
  pack append $xfRoot.params1.params2.command \
              $xfRoot.params1.params2.command.label1 {top frame center} \
              $xfRoot.params1.params2.command.command {top fill expand}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.command {top fill expand}  
}

##########
# Procedure: XFElementCursor
# Description: build cursor frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementCursor {xfW xfRoot xfType xfClass xfFunction} {
  global xfBind

  XFTmpltLabledEntry $xfRoot.params1.params2 cursor "Cursor:" \
    [lindex [$xfW configure -cursor] 4]

  XFMiscInitElement $xfRoot cursor

  # bindings
  bind $xfRoot.params1.params2.cursor.cursor $xfBind(configure) \
    "global xfConf
     if {\$xfConf(applyParameters)} {
       XFProcCursorBox cursor {$xfRoot.params1.params2.cursor.cursor} $xfW
     } {
       XFProcCursorBox cursor {$xfRoot.params1.params2.cursor.cursor}
     }"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.cursor.cursor <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.cursor {top fillx pady 6}
}

##########
# Procedure: XFElementFont
# Description: build font frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfName - the name of the widget
#            xfResource - the resource
#            xfResClass - the resource class
#            xfLabel - the label
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementFont {xfW xfRoot xfType xfClass xfName xfResource xfResClass xfLabel xfFunction} {
  global xfBind

  XFTmpltLabledEntry $xfRoot.params1.params2 $xfName "$xfLabel:" \
    [lindex [$xfW configure -[string tolower $xfResource]] 4]

  XFMiscInitElement $xfRoot $xfName

  # bindings
  bind $xfRoot.params1.params2.$xfName.$xfName $xfBind(configure) \
    "global xfConf
     if {\$xfConf(applyParameters)} {
       XFProcFontBox \"$xfResource\" {$xfRoot.params1.params2.$xfName.$xfName} $xfW
     } {
       XFFontFontBox \"$xfResource\" {$xfRoot.params1.params2.$xfName.$xfName}
     }"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.$xfName.$xfName <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.$xfName {top fillx pady 6}
}

##########
# Procedure: XFElementGeometry
# Description: build geometry frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementGeometry {xfW xfRoot xfType xfClass xfFunction} {
  global xfStatus

  if {"[string tolower $xfClass]" == "listbox"} {
    XFTmpltScaleDouble $xfRoot.params1.params2 geo "Geometry:" \
      "Width" "Height" 500 500
  } {
    XFTmpltScaleDouble $xfRoot.params1.params2 geo "Geometry:" \
      "Width" "Height" 1000 1000
  }

  set xfGeoWidth ""
  set xfGeoHeight ""
  # 11/15/98 - D. LaBelle - corrected retrieval of window geometry
#  set xfGeo [lindex [$xfW configure -width] 4]
  if {[winfo class $xfW] == "Frame"} {
      set xfGeo [split [winfo geometry $xfW] "x+"]
     } {
        set xfGeo "[lindex [$xfW configure -width] 4] [lindex [$xfW configure -height] 4]"
       }
  if {"$xfGeo]" != ""} {
#    set xfGeoWidth [string range $xfGeo 0 [expr [string last "x" $xfGeo]-1]]
#    set xfGeoHeight [string range $xfGeo [expr [string last "x" $xfGeo]+1] end]
     set xfGeoWidth [lindex $xfGeo 0]
     set xfGeoHeight [lindex $xfGeo 1]
  }
  if {"$xfGeoWidth" != ""} {
    $xfRoot.params1.params2.geo.geo1.geo1 set $xfGeoWidth
  }
  if {"$xfGeoHeight" != ""} {
    $xfRoot.params1.params2.geo.geo2.geo2 set $xfGeoHeight
  }

  XFMiscSetResource $xfRoot.params1.params2.geo.geo1.geo1 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.geo.geo2.geo2 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.geo.label1 \
    width $xfStatus(elementWidth)
  XFMiscSetResource $xfRoot.params1.params2.geo.label1 \
    anchor e
  XFMiscSetResource $xfRoot.params1.params2.geo.label1 \
    relief flat
  if {"$xfType" != "add"} {
    XFMiscSetResource $xfRoot.params1.params2.geo.geo1.geo1 \
      command "$xfFunction $xfW 0 $xfClass"
    XFMiscSetResource $xfRoot.params1.params2.geo.geo2.geo2 \
      command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.geo {top fillx pady 6}
}

##########
# Procedure: XFElementInit
# Description: build common window structure for configuration
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
#            xfMessage - The top message
#            xfName - the name for a newly created widget
#            xfDialog - which type of configuration
# Returns: none
# Sideeffects: none
##########
proc XFElementInit {xfW xfRoot xfType xfClass xfFunction xfMessage xfName xfDialog} {
  global symbolicName
  global xfConf
  global xfStatus

  XFTmpltFrame $xfRoot.leave 0

  XFTmpltFrame $xfRoot.additional 0

  XFTmpltFrame $xfRoot.params1

  XFTmpltFrame $xfRoot.params1.params2 0

  XFTmpltFrame $xfRoot.params1.params2.name 0

  XFTmpltFrame $xfRoot.params1.params2.symname 0

  label $xfRoot.params1.params2.name.label1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text {Name:}

  if {"$xfType" == "add"} {
    entry $xfRoot.params1.params2.name.name \
      -relief sunken
  } {
    label $xfRoot.params1.params2.name.name \
      -anchor w
  }

  label $xfRoot.params1.params2.symname.label1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text {Symbolic name:}

  entry $xfRoot.params1.params2.symname.symname \
    -relief sunken

  if {"$xfType" == "add"} {
    button $xfRoot.leave.ok \
      -text {OK} \
      -command "
        global xfConf
        global xfStatus
        if {\"\[$xfRoot.params1.params2.name.name get\]\" != \"\"} {
          set xfTmpValue \$xfConf(getWidgetName)
          set xfConf(getWidgetName) 0
          if {\[catch \"XFAdd.$xfClass $xfW \[$xfRoot.params1.params2.name.name get\] config@\" xfResult\]} {
            XFProcError \$xfResult
            XFEditSetStatus \"Insertion of widget...aborted\"
          }
          set xfConf(getWidgetName) \$xfTmpValue
          if {\"[string tolower $xfClass]\" == \"toplevel\"} {
            $xfFunction .\[$xfRoot.params1.params2.name.name get\] 1 $xfClass
            XFEditSetPath \$xfStatus(path)
            XFEditSetShowWindows
          } {
            if {\"\$xfStatus(path)\" == \".\"} {
              $xfFunction .\[$xfRoot.params1.params2.name.name get\] 1 $xfClass
            } {
              $xfFunction \$xfStatus(path).\[$xfRoot.params1.params2.name.name get\] 1 $xfClass
            }
          }
        }
        destroy $xfRoot"

    button $xfRoot.leave.apply \
      -text {Apply} \
      -state disabled

    checkbutton $xfRoot.leave.applyperm \
      -text {Apply permanently} \
      -state disabled \
      -variable xfConf(applyParameters) \
      -onvalue 1 \
      -offvalue 0

    button $xfRoot.leave.cancel \
      -text {Cancel} \
      -command "destroy $xfRoot"
  } {
    button $xfRoot.leave.ok \
      -text {OK} \
      -command "
        $xfFunction $xfW 1 $xfClass
        destroy $xfRoot"

    button $xfRoot.leave.apply \
      -text {Apply} \
      -command "$xfFunction $xfW 1 $xfClass"

    checkbutton $xfRoot.leave.applyperm \
      -text {Apply permanently} \
      -variable xfConf(applyParameters) \
      -onvalue 1 \
      -offvalue 0 \
      -command "$xfFunction $xfW 0 $xfClass"

    button $xfRoot.leave.cancel \
      -text {Cancel} \
      -command "
        global xfMisc
        if {\[info exists xfMisc($xfClass,saveValue)\]} {
          foreach xfCounter \$xfMisc($xfClass,saveValue) {
            catch \"$xfW config -\[lindex \$xfCounter 0\] \\\"\[lindex \$xfCounter 1\]\\\"\"
          }
        }
        destroy $xfRoot"

    if {$xfDialog == 4} {
      button $xfRoot.additional.params \
        -text {Special} \
        -command "
          XFProcConfConfigure $xfW 5 .xf${xfClass}Config$xfDialog $xfClass $xfType"
    } {
      button $xfRoot.additional.params \
        -text {Small} \
        -command "
          XFProcConfConfigure $xfW 4 .xf${xfClass}Config$xfDialog $xfClass $xfType"
    }

    button $xfRoot.additional.general \
      -text {General} \
      -command "
        XFProcConfParametersGeneral $xfW .xf${xfClass}Config$xfDialog"

    button $xfRoot.additional.packing \
      -text {Geometry} \
      -command "
        XFProcConfGeometryDefault $xfW .xf${xfClass}Config$xfDialog"
  
    button $xfRoot.additional.binding \
      -text {Binding} \
      -command "
        XFProcConfConfigure $xfW 3 .xf${xfClass}Config$xfDialog $xfClass $xfType"

    pack append $xfRoot.additional \
                $xfRoot.additional.params {left fill expand} \
                $xfRoot.additional.general {left fill expand} \
                $xfRoot.additional.packing {left fill expand} \
                $xfRoot.additional.binding {left fill expand}
  }
  
  XFElementPathname $xfW $xfRoot $xfType
  if {"$xfType" == "add"} {
    $xfRoot.params1.params2.name.name insert 0 $xfName
  } {
    if {"[string tolower $xfClass]" == "toplevel"} {
      $xfRoot.params1.params2.name.name configure \
        -text [winfo name $xfW]
    } {
      $xfRoot.params1.params2.name.name configure \
        -text [string range $xfW [expr [string last . $xfW]+1] end]
    }
    foreach xfCounter [array names symbolicName] {
      set xfArrayName ""
      append xfArrayName symbolicName ( $xfCounter )
      if {"$xfW" == "[set $xfArrayName]"} {
        $xfRoot.params1.params2.symname.symname insert 0 $xfCounter
      }
    }
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.name \
              $xfRoot.params1.params2.name.label1 {left} \
              $xfRoot.params1.params2.name.name {left fillx expand}
  pack append $xfRoot.params1.params2.symname \
              $xfRoot.params1.params2.symname.label1 {left} \
              $xfRoot.params1.params2.symname.symname {left fillx expand}
  pack append $xfRoot.leave \
              $xfRoot.leave.ok {left fill expand} \
              $xfRoot.leave.apply {left fill expand} \
              $xfRoot.leave.applyperm {left fill expand} \
              $xfRoot.leave.cancel {left fill expand}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.name {top fillx pady 6} \
              $xfRoot.params1.params2.symname {top fillx pady 6}
  pack append $xfRoot.params1 \
              $xfRoot.params1.params2 {top frame nw fill expand}
}

##########
# Procedure: XFElementJustify
# Description: build justify frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementJustify {xfW xfRoot xfType xfClass xfFunction} {
  global xfMisc
  global xfStatus

  set xfMisc(justify) center

  XFTmpltFrame $xfRoot.params1.params2.justify 0

  label $xfRoot.params1.params2.justify.label1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text "Justify:"

  radiobutton $xfRoot.params1.params2.justify.left \
    -text "Left" \
    -value left \
    -variable xfMisc(justify)

  radiobutton $xfRoot.params1.params2.justify.center \
    -text "Center" \
    -value center \
    -variable xfMisc(justify)

  radiobutton $xfRoot.params1.params2.justify.right \
    -text "Right" \
    -value right \
    -variable xfMisc(justify)

  if {"$xfType" == "add"} {
    $xfRoot.params1.params2.justify.center select
  } {
    case [lindex [$xfW configure -justify] 4] in {
      {left} {
        $xfRoot.params1.params2.justify.left select
      }
      {right} {
        $xfRoot.params1.params2.justify.right select
      }
      {default} {
        $xfRoot.params1.params2.justify.center select
      }
    }
  }
  if {"$xfType" != "add"} {
    $xfRoot.params1.params2.justify.left configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.justify.center configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.justify.right configure \
      -command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.justify \
              $xfRoot.params1.params2.justify.label1 {left} \
              $xfRoot.params1.params2.justify.left {left fillx expand} \
              $xfRoot.params1.params2.justify.center {left fillx expand} \
              $xfRoot.params1.params2.justify.right {left fillx expand}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.justify {top fillx pady 6}
}

##########
# Procedure: XFElementOrient
# Description: build orient frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementOrient {xfW xfRoot xfType xfClass xfFunction} {
  global xfMisc
  global xfStatus

  set xfMisc(orient) horizontal

  XFTmpltFrame $xfRoot.params1.params2.orient 0

  label $xfRoot.params1.params2.orient.label1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text "Orient:"

  radiobutton $xfRoot.params1.params2.orient.horizontal \
    -text "Horizontal" \
    -value horizontal \
    -variable xfMisc(orient)

  radiobutton $xfRoot.params1.params2.orient.vertical \
    -text "Vertical" \
    -value vertical \
    -variable xfMisc(orient)

  if {"$xfType" == "add"} {
    $xfRoot.params1.params2.orient.vertical select
  } {
    case [lindex [$xfW configure -orient] 4] in {
      {h*} {
        $xfRoot.params1.params2.orient.horizontal select
      }
      {default} {
        $xfRoot.params1.params2.orient.vertical select
      }
    }
  }
  if {"$xfType" != "add"} {
    $xfRoot.params1.params2.orient.horizontal configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.orient.vertical configure \
      -command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.orient \
              $xfRoot.params1.params2.orient.label1 {left} \
              $xfRoot.params1.params2.orient.horizontal {left fillx expand} \
              $xfRoot.params1.params2.orient.vertical {left fillx expand}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.orient {top fillx pady 6}
}

##########
# Procedure: XFElementPad
# Description: build pad frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementPad {xfW xfRoot xfType xfClass xfFunction} {
  global xfStatus

  XFTmpltScaleDouble $xfRoot.params1.params2 pad "Pad:" "x" "y" \
    250 250

  $xfRoot.params1.params2.pad.pad1.pad1 set [lindex [$xfW configure -padx] 4]
  $xfRoot.params1.params2.pad.pad2.pad2 set [lindex [$xfW configure -pady] 4]

  XFMiscSetResource $xfRoot.params1.params2.pad.pad1.pad1 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.pad.pad2.pad2 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.pad.label1 \
    width $xfStatus(elementWidth)
  XFMiscSetResource $xfRoot.params1.params2.pad.label1 \
    anchor e
  XFMiscSetResource $xfRoot.params1.params2.pad.label1 \
    relief flat
  if {"$xfType" != "add"} {
    XFMiscSetResource $xfRoot.params1.params2.pad.pad1.pad1 \
      command "$xfFunction $xfW 0 $xfClass"
    XFMiscSetResource $xfRoot.params1.params2.pad.pad2.pad2 \
      command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.pad {top fillx pady 6}
}

##########
# Procedure: XFElementPathname
# Description: build pathname frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
# Returns: none
# Sideeffects: none
##########
proc XFElementPathname {xfW xfRoot xfType} {
  global xfStatus

  XFTmpltFrame $xfRoot.pathname

  XFTmpltFrame $xfRoot.pathname.pathname 0

  label $xfRoot.pathname.pathname.label1 \
    -text "Widget path:"

  label $xfRoot.pathname.pathname.pathname

  if {"$xfType" == "add"} {
    XFMiscSetResource $xfRoot.pathname.pathname.pathname \
      text $xfStatus(path)
  } {
    if {"$xfW" == "."} {
      XFMiscSetResource $xfRoot.pathname.pathname.pathname \
        text "root"
    } {
      if {"[string range $xfW 0 [expr [string last "." $xfW]-1]]" == ""} {
        XFMiscSetResource $xfRoot.pathname.pathname.pathname \
          text "."
      } {
        XFMiscSetResource $xfRoot.pathname.pathname.pathname \
          text [string range $xfW 0 [expr [string last "." $xfW]-1]]
      }
    }
  }

  # packing of subwidgets
  pack append $xfRoot.pathname.pathname \
              $xfRoot.pathname.pathname.label1 {left} \
              $xfRoot.pathname.pathname.pathname {left}
  pack append $xfRoot.pathname \
              $xfRoot.pathname.pathname {top}
}

##########
# Procedure: XFElementRelief
# Description: build relief frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementRelief {xfW xfRoot xfType xfClass xfFunction} {
  global xfMisc
  global xfStatus

  set xfMisc(relief) raised

  XFTmpltFrame $xfRoot.params1.params2.relief 0

  label $xfRoot.params1.params2.relief.label1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text "Relief:"

  XFTmpltFrame $xfRoot.params1.params2.relief.relief1 0

  XFTmpltFrame $xfRoot.params1.params2.relief.relief2 0

  radiobutton $xfRoot.params1.params2.relief.relief1.raised \
    -text "Raised" \
    -value raised \
    -variable xfMisc(relief)

  radiobutton $xfRoot.params1.params2.relief.relief1.sunken \
    -text "Sunken" \
    -value sunken \
    -variable xfMisc(relief)

  radiobutton $xfRoot.params1.params2.relief.relief1.flat \
    -text "Flat" \
    -value flat \
    -variable xfMisc(relief)

  radiobutton $xfRoot.params1.params2.relief.relief2.ridge \
    -text "Ridge" \
    -value ridge \
    -variable xfMisc(relief)

  radiobutton $xfRoot.params1.params2.relief.relief2.groove \
    -text "Groove" \
    -value groove \
    -variable xfMisc(relief)

  if {"$xfType" == "add"} {
    $xfRoot.params1.params2.relief.relief1.raised select
  } {
    case [lindex [$xfW configure -relief] 4] in {
      {sunken} {
        $xfRoot.params1.params2.relief.relief1.sunken select
      }
      {flat} {
        $xfRoot.params1.params2.relief.relief1.flat select
      }
      {ridge} {
        $xfRoot.params1.params2.relief.relief2.ridge select
      }
      {groove} {
        $xfRoot.params1.params2.relief.relief2.groove select
      }
      {raised} {
        $xfRoot.params1.params2.relief.relief1.raised select
      }
      {default} {
        $xfRoot.params1.params2.relief.relief1.raised select
      }
    }
  }
  if {"$xfType" != "add"} {
    $xfRoot.params1.params2.relief.relief1.raised configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.relief.relief1.sunken configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.relief.relief1.flat configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.relief.relief2.ridge configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.relief.relief2.groove configure \
      -command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.relief.relief1 \
              $xfRoot.params1.params2.relief.relief1.raised {left fillx expand} \
              $xfRoot.params1.params2.relief.relief1.sunken {left fillx expand} \
              $xfRoot.params1.params2.relief.relief1.flat {left fillx expand}
  pack append $xfRoot.params1.params2.relief.relief2 \
              $xfRoot.params1.params2.relief.relief2.ridge {left fillx expand} \
              $xfRoot.params1.params2.relief.relief2.groove {left fillx expand}
  pack append $xfRoot.params1.params2.relief \
              $xfRoot.params1.params2.relief.label1 {left} \
              $xfRoot.params1.params2.relief.relief1 {top fill expand} \
              $xfRoot.params1.params2.relief.relief2 {top fill expand}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.relief {top fillx pady 6}
}

##########
# Procedure: XFElementSave
# Description: save current parameters for widget
# Arguments: xfW - the widget we configure
#            xfClass - the class we configure
#            xfResources - the parameters to save
# Returns: none
# Sideeffects: none
##########
proc XFElementSave {xfW xfClass xfResources} {
  global xfMisc

  set xfMisc($xfClass,saveValue) ""
  foreach xfCounter $xfResources {
    set xfValue [lindex [$xfW configure -$xfCounter] 4]
    if {"$xfCounter" == "state"} {
      if {"$xfValue" == "active"} {
        set xfValue normal
      }
    }
    if {"$xfCounter" == "command" && "$xfClass" == "Scale" &&
        "$xfValue" == ""} {
      continue
    }
    if {"$xfCounter" == "command" && "$xfClass" == "Scrollbar" &&
        "$xfValue" == ""} {
      continue
    }
    if {"$xfCounter" == "scrollregion" && "$xfClass" == "Canvas" &&
        "$xfValue" == ""} {
      continue
    }
    if {"$xfCounter" == "file" && "$xfClass" == "TkEmacs" &&
        "$xfValue" == ""} {
      continue
    }
    if {"$xfCounter" == "scrollcommand" ||
        "$xfCounter" == "xscrollcommand" ||
        "$xfCounter" == "yscrollcommand"} {
      if {"$xfValue" == ""} {
        continue
      }
    }
    set xfTmpValue $xfCounter
    lappend xfTmpValue $xfValue
    lappend xfMisc($xfClass,saveValue) $xfTmpValue
  }
}

##########
# Procedure: XFElementScale
# Description: build scale frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfName - the name of the widget
#            xfResource - the resource
#            xfResClass - the resource class
#            xfLabel1 - the left label
#            xfLabel2 - the scale label
#            xfTo - the to value
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementScale {xfW xfRoot xfType xfClass xfName xfResource \
                     xfResClass xfLabel1 xfLabel2 xfTo xfFunction} {
  global xfStatus

  if {"$xfType" == "config" || "$xfW" != "."} {
    XFTmpltScale $xfRoot.params1.params2 $xfName \
      "$xfLabel1:" $xfLabel2 $xfTo \
        [lindex [$xfW configure -[string tolower $xfResource]] 4]
  } {
    XFTmpltScale $xfRoot.params1.params2 $xfName \
      "$xfLabel1:" $xfLabel2 $xfTo
  }

  XFMiscSetResource $xfRoot.params1.params2.$xfName.$xfName \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.$xfName.label1 \
    width $xfStatus(elementWidth)
  XFMiscSetResource $xfRoot.params1.params2.$xfName.label1 \
    anchor e
  XFMiscSetResource $xfRoot.params1.params2.$xfName.label1 \
    relief flat
  if {"$xfType" != "add"} {
    XFMiscSetResource $xfRoot.params1.params2.$xfName.$xfName \
      command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.$xfName {top fillx pady 6}
}

##########
# Procedure: XFElementScaleDouble
# Description: build double scale frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfName - the name of the widget
#            xfLabel1 - the left label
#            xfLabel2 - the scale label
#            xfLabel3 - the scale label
#            xfTo1 - the to value
#            xfTo2 - the to value
#            {xfCheckVar} - variable for label (as checkbutton)
# Returns: none
# Sideeffects: none
##########
proc XFElementScaleDouble {xfW xfRoot xfType xfClass xfName xfLabel1 \
                           xfLabel2 xfLabel3 xfTo1 xfTo2 {xfCheckVar ""}} {
  global xfStatus

  XFTmpltScaleDouble $xfRoot.params1.params2 $xfName \
    "$xfLabel1:" $xfLabel2 $xfLabel3 $xfTo1 $xfTo2 0 0 $xfCheckVar

  XFMiscSetResource $xfRoot.params1.params2.$xfName.${xfName}1.${xfName}1 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.$xfName.${xfName}2.${xfName}2 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.$xfName.label1 \
    width [expr $xfStatus(elementWidth)-3]
  XFMiscSetResource $xfRoot.params1.params2.$xfName.label1 \
    anchor e
  XFMiscSetResource $xfRoot.params1.params2.$xfName.label1 \
    relief flat

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.$xfName {top fillx pady 6}
}

##########
# Procedure: XFElementSize
# Description: build width and height frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementSize {xfW xfRoot xfType xfClass xfFunction} {
  global xfStatus

  XFTmpltScaleDouble $xfRoot.params1.params2 size "Size:" \
    "Width" "Height" 1000 1000
  $xfRoot.params1.params2.size.size1.size1 set \
    [lindex [$xfW configure -width] 4]
  $xfRoot.params1.params2.size.size2.size2 set \
    [lindex [$xfW configure -height] 4]

  XFMiscSetResource $xfRoot.params1.params2.size.size1.size1 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.size.size2.size2 \
    relief sunken
  XFMiscSetResource $xfRoot.params1.params2.size.label1 \
    width $xfStatus(elementWidth)
  XFMiscSetResource $xfRoot.params1.params2.size.label1 \
    anchor e
  XFMiscSetResource $xfRoot.params1.params2.size.label1 \
    relief flat
  if {"$xfType" != "add"} {
    XFMiscSetResource $xfRoot.params1.params2.size.size1.size1 \
      command "$xfFunction $xfW 0 $xfClass"
    XFMiscSetResource $xfRoot.params1.params2.size.size2.size2 \
      command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.size {top fillx pady 6}
}

##########
# Procedure: XFElementState
# Description: build state frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementState {xfW xfRoot xfType xfClass xfFunction} {
  global xfMisc
  global xfStatus

  set xfMisc(state) normal

  XFTmpltFrame $xfRoot.params1.params2.state 0

  label $xfRoot.params1.params2.state.label1 \
    -anchor e \
    -width $xfStatus(elementWidth) \
    -text "State:"

  radiobutton $xfRoot.params1.params2.state.normal \
    -text "Normal" \
    -value normal \
    -variable xfMisc(state)

  radiobutton $xfRoot.params1.params2.state.active \
    -text "Active" \
    -value active \
    -variable xfMisc(state)

  radiobutton $xfRoot.params1.params2.state.disabled \
    -text "Disabled" \
    -value disabled \
    -variable xfMisc(state)

  if {"$xfType" == "add"} {
    $xfRoot.params1.params2.state.normal select
  } {
    case [lindex [$xfW configure -state] 4] in {
      {disabled} {
        $xfRoot.params1.params2.state.disabled select
      }
      {default} {
        $xfRoot.params1.params2.state.normal select
      }
    }
    $xfRoot.params1.params2.state.normal configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.state.active configure \
      -command "$xfFunction $xfW 0 $xfClass"

    $xfRoot.params1.params2.state.disabled configure \
      -command "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2.state \
              $xfRoot.params1.params2.state.label1 {left} \
              $xfRoot.params1.params2.state.normal {left fillx expand} \
              $xfRoot.params1.params2.state.active {left fillx expand} \
              $xfRoot.params1.params2.state.disabled {left fillx expand}
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.state {top fillx pady 6}
}

##########
# Procedure: XFElementText
# Description: build text frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfName - the name of the widget
#            xfResource - the resource
#            xfResClass - the resource class
#            xfLabel - the label
#            xfFunction - the function that sets the values
#            xfValue - the default contents
# Returns: none
# Sideeffects: none
##########
proc XFElementText {xfW xfRoot xfType xfClass xfName xfResource xfResClass \
                    xfLabel xfFunction {xfValue ""}} {

  if {"$xfType" == "add"} {
    if {"$xfValue]" != ""} {
      XFTmpltLabledEntry $xfRoot.params1.params2 $xfName "$xfLabel:" \
        $xfValue
    } {
      XFTmpltLabledEntry $xfRoot.params1.params2 $xfName "$xfLabel:" \
        [lindex [$xfW configure -[string tolower $xfResource]] 4]
    }
  } {
    XFTmpltLabledEntry $xfRoot.params1.params2 $xfName "$xfLabel:" \
      [lindex [$xfW configure -[string tolower $xfResource]] 4]
  }

  XFMiscInitElement $xfRoot $xfName

  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.$xfName.$xfName <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.$xfName {top fillx pady 6}
}

##########
# Procedure: XFElementTextVariable
# Description: build text variable frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementTextVariable {xfW xfRoot xfType xfClass xfFunction} {
  global xfBind

  XFTmpltLabledEntry $xfRoot.params1.params2 textvar "Text variable:" \
    [lindex [$xfW configure -textvariable] 4]

  XFMiscInitElement $xfRoot textvar

  # bindings
  bind $xfRoot.params1.params2.textvar.textvar $xfBind(configure) \
    "XFInfoGlobals $xfRoot.params1.params2.textvar.textvar"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.textvar.textvar <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.textvar {top fillx pady 6}
}

##########
# Procedure: XFElementVariable
# Description: build variable frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementVariable {xfW xfRoot xfType xfClass xfFunction} {
  global xfBind

  XFTmpltLabledEntry $xfRoot.params1.params2 variable "Variable:" \
    [lindex [$xfW configure -variable] 4]

  XFMiscInitElement $xfRoot variable

  # bindings
  bind $xfRoot.params1.params2.variable.variable $xfBind(configure) \
    "XFInfoGlobals $xfRoot.params1.params2.variable.variable"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.variable.variable <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.variable {top fillx pady 6}
}

##########
# Procedure: XFElementTileWidth
# Description: build text variable frame
# Arguments: xfW - the widget we configure
#            xfRoot - the root name
#            xfType - add or config
#            xfClass - the class we configure
#            xfFunction - the function that sets the values
# Returns: none
# Sideeffects: none
##########
proc XFElementTileWidth {xfW xfRoot xfType xfClass xfFunction} {
  global xfBind

  XFTmpltLabledEntry $xfRoot.params1.params2 widthvar "Width:" \
    [lindex [$xfW configure -width] 4]

  XFMiscInitElement $xfRoot widthvar

  # bindings
  bind $xfRoot.params1.params2.widthvar.widthvar $xfBind(configure) \
    "XFInfoGlobals $xfRoot.params1.params2.widthvar.widthvar"
  if {"$xfType" != "add"} {
    bind $xfRoot.params1.params2.widthvar.widthvar <Return> \
      "$xfFunction $xfW 0 $xfClass"
  }

  # packing of subwidgets
  pack append $xfRoot.params1.params2 \
              $xfRoot.params1.params2.widthvar {top fillx pady 6}
}

# eof



