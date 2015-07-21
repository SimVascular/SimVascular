# Program: xf
# Description: template for commonly used widget structures
#
# $Header: xftmplt.tcl[2.4] Wed Mar 10 12:08:31 1993 garfield@garfield frozen $

##########
# Procedure: XFTmpltFrame
# Description: build a frame widget structure
# Arguments: xfW - the name of the widget we want to build
#            {xfBorderWidth} - the width of the border
# Returns: none
# Sideeffects: none
##########
proc XFTmpltFrame {xfW {xfBorderWidth ""}} {
  global xfStatus

  # should we use the default width ?
  if {"$xfBorderWidth" == ""} {
    set xfBorderWidth $xfStatus(borderWidth)
  }

  # build widgets
  frame $xfW \
    -borderwidth $xfBorderWidth \
    -relief raised
}

##########
# Procedure: XFTmpltLabledEntry
# Description: build a labled entry widget structure
# Arguments: xfW - the parent widget
#            xfName - the name of the widget we want to build
#            xfLabel - the label text
#            {xfEntry} - the default contents of the entry
# Returns: none
# Sideeffects: none
##########
proc XFTmpltLabledEntry {xfW xfName xfLabel {xfEntry ""}} {

  # build widgets
  frame $xfW.$xfName \
    -borderwidth 0 \
    -relief raised

  label $xfW.$xfName.label$xfName \
    -relief raised \
    -text "$xfLabel"

  entry $xfW.$xfName.$xfName \
    -relief raised

  # insert the text
  $xfW.$xfName.$xfName delete 0 end
  $xfW.$xfName.$xfName insert 0 $xfEntry

  # packing of the subwidgets
  pack configure $xfW.$xfName.label$xfName -side left
  pack configure $xfW.$xfName.$xfName -side left -fill both -expand 1
}

##########
# Procedure: XFTmpltListbox
# Description: build a listbox widget structure
# Arguments: xfW - the parent widget
#            xfName - the name of the widget we want to build
# Returns: none
# Sideeffects: none
##########
proc XFTmpltListbox {xfW xfName} {
  global xfConf

  # build widgets
  frame $xfW.$xfName \
    -borderwidth 0 \
    -relief raised

  scrollbar $xfW.$xfName.vscroll \
    -relief raised \
    -highlightthickness 0 \
    -command "$xfW.$xfName.$xfName yview"

  scrollbar $xfW.$xfName.hscroll \
    -orient horizontal \
    -relief raised \
    -highlightthickness 0 \
    -command "$xfW.$xfName.$xfName xview"

  listbox $xfW.$xfName.$xfName \
    -exportselection false \
    -highlightthickness 0 \
    -relief raised \
    -xscrollcommand "$xfW.$xfName.hscroll set" \
    -yscrollcommand "$xfW.$xfName.vscroll set"

  # packing of the subwidgets
  pack configure $xfW.$xfName.vscroll -side $xfConf(scrollSide) -fill y
  pack configure $xfW.$xfName.hscroll -side bottom -fill x
  pack configure $xfW.$xfName.$xfName -side left -fill both -expand 1
}

##########
# Procedure: XFTmpltScale
# Description: build a scale widget structure
# Arguments: xfW - the parent widget
#            xfName - the name of the scale
#            xfLabel1 - the label left from scale
#            xfLabel2 - the label of the scale
#            xfTo - the maximal value of the scale
#            {xfValue} - the default value of the scale
# Returns: none
# Sideeffects: none
##########
proc XFTmpltScale {xfW xfName xfLabel1 xfLabel2 xfTo {xfValue 0}} {

  # build widgets
  frame $xfW.$xfName \
    -borderwidth 0 \
    -relief raised

  label $xfW.$xfName.label1 \
    -relief raised \
    -text "$xfLabel1"

  scale $xfW.$xfName.$xfName \
    -from 0 \
    -label "$xfLabel2" \
    -orient horizontal \
    -sliderlength 15 \
    -to $xfTo \
    -width 8

  # set the scale
  if {"$xfValue" != ""} {
    $xfW.$xfName.$xfName set [lindex [split $xfValue .] 0]
  } {
    $xfW.$xfName.$xfName set 0
  }

  # packing of the subwidgets
  pack configure $xfW.$xfName.label1 -side left
  pack configure $xfW.$xfName.$xfName -side left -fill x -expand 1
}

##########
# Procedure: XFTmpltScaleDouble
# Description: build a double scale widget structure
# Arguments: xfW - the parent widget
#            xfName - the name of the scales
#            xfLabel1 - the label left from the scales
#            xfLabel2 - the label of the 1st scale
#            xfLabel3 - the label of the 2nd scale
#            xfTo1 - the maximal value of the 1st scale
#            xfTo2 - the maximal value of the 2nd scale
#            {xfValue1} - the default value of the 1st scale
#            {xfValue2} - the default value of the 2nd scale
#            {xfCheckVar} - if not "", xfLabel1 will be a checkbutton
#                           with this -variable
# Returns: none
# Sideeffects: none
##########
proc XFTmpltScaleDouble {xfW xfName xfLabel1 xfLabel2 \
                         xfLabel3 xfTo1 xfTo2 {xfValue1 0} {xfValue2 0} {xfCheckVar ""}} {

  # build widgets
  frame $xfW.$xfName \
    -borderwidth 0 \
    -relief raised

  frame $xfW.$xfName.${xfName}1 \
    -borderwidth 0 \
    -relief raised

  frame $xfW.$xfName.${xfName}2 \
    -borderwidth 0 \
    -relief raised

  if {$xfCheckVar != ""} {
    checkbutton $xfW.$xfName.label1 \
      -relief raised \
      -text $xfLabel1 \
      -variable $xfCheckVar
  } {
    label $xfW.$xfName.label1 \
      -relief raised \
      -text "$xfLabel1"
  } 
  
  scale $xfW.$xfName.${xfName}1.${xfName}1 \
    -from 0 \
    -label "$xfLabel2" \
    -orient horizontal \
    -sliderlength 15 \
    -to $xfTo1 \
    -width 8

  scale $xfW.$xfName.${xfName}2.${xfName}2 \
    -from 0 \
    -label "$xfLabel3" \
    -orient horizontal \
    -sliderlength 15 \
    -to $xfTo2 \
    -width 8

  # set scales
  if {"$xfValue1" != ""} {
    $xfW.$xfName.${xfName}1.${xfName}1 set [lindex [split $xfValue1 .] 0]
  } {
    $xfW.$xfName.${xfName}1.${xfName}1 set 0
  }  

  if {"$xfValue2" != ""} {
    $xfW.$xfName.${xfName}2.${xfName}2 set [lindex [split $xfValue2 .] 0]
  } {
    $xfW.$xfName.${xfName}2.${xfName}2 set 0
  }

  # packing of the subwidgets
  pack configure $xfW.$xfName.label1 -side left
  pack configure \
    $xfW.$xfName.${xfName}1.${xfName}1 \
    $xfW.$xfName.${xfName}2.${xfName}2 \
    $xfW.$xfName.${xfName}1 \
    $xfW.$xfName.${xfName}2 \
    -side left -fill x -expand 1
}

##########
# Procedure: XFTmpltText
# Description: build a text widget structure
# Arguments: xfW - the parent widget
#            xfName - the name of the widget we want to build
#            {xfReadOnly} - 1 if text is read only, 0 otherwise
#            {xfText} - the text to insert in the widget
# Returns: none
# Sideeffects: none
##########
proc XFTmpltText {xfW xfName {xfReadOnly 0} {xfText ""}} {

  XFTmpltTextLong $xfW $xfName $xfReadOnly $xfText 1
}

##########
# Procedure: XFTmpltTextLong
# Description: build a text widget structure(long version)
# Arguments: xfW - the parent widget
#            xfName - the name of the widget we want to build
#            xfReadOnly - 1 if text is read only, 0 otherwise
#            {xfText} - the text to insert in the widget
#            {xfNoEmacs} - 1 if no emacs is wanted, 0 otherwise
# Returns: none
# Sideeffects: none
##########
proc XFTmpltTextLong {xfW xfName xfReadOnly {xfText ""} {xfNoEmacs 1}} {
  global xfConf
  global xfFile
  global xfStatus

  # build widgets
  frame $xfW.$xfName \
    -borderwidth 0 \
    -relief raised

  if {!$xfNoEmacs && $xfConf(interpreterHasTkemacs)} {
    if {[catch "tkemacs $xfW.$xfName.$xfName \
                  -command $xfFile(emacsCmd) \
                  -lispfile $xfFile(emacsLisp) \
                  -relief raised \
                  -borderwidth $xfStatus(borderWidth) \
                  -xscrollcommand \"$xfW.$xfName.hscroll set\" \
                  -yscrollcommand \"$xfW.$xfName.vscroll set\"" xfResult]} {
      puts stderr "$xfResult"
      catch "destroy $xfW.$xfName.$xfName"
      set xfConf(interpreterHasTkemacs) 0
    } {
      scrollbar $xfW.$xfName.hscroll \
        -relief raised \
        -orient horizontal \
        -highlightthickness 0 \
        -command "$xfW.$xfName.$xfName xview"

      scrollbar $xfW.$xfName.vscroll \
        -relief raised \
        -highlightthickness 0 \
        -command "$xfW.$xfName.$xfName yview"

      # set text contents
      XFMiscSetText $xfW.$xfName.$xfName $xfText

      # packing of the subwidgets
      pack configure $xfW.$xfName.vscroll -side $xfConf(scrollSide) -fill y
      pack configure $xfW.$xfName.hscroll -side bottom -fill x
      pack configure $xfW.$xfName.$xfName -side left -fill both -expand 1 
    }
  }
  if {$xfNoEmacs || !$xfConf(interpreterHasTkemacs)} {
    text $xfW.$xfName.$xfName \
      -relief raised \
      -wrap none \
      -borderwidth $xfStatus(borderWidth) \
      -highlightthickness 0 \
      -yscrollcommand "$xfW.$xfName.vscroll set" \
      -xscrollcommand "$xfW.$xfName.hscroll set"

    scrollbar $xfW.$xfName.vscroll \
      -relief raised \
      -highlightthickness 0 \
      -command "$xfW.$xfName.$xfName yview"

    scrollbar $xfW.$xfName.hscroll \
      -relief raised \
      -orient horizontal \
      -highlightthickness 0 \
      -command "$xfW.$xfName.$xfName xview"

    # set text contents
    XFMiscSetText $xfW.$xfName.$xfName $xfText

    # special bindings
    if {$xfReadOnly} {
      bind $xfW.$xfName.$xfName <Any-Key> {
        NoFunction}
    }

    # packing of the subwidgets
    pack configure $xfW.$xfName.vscroll -side $xfConf(scrollSide) -fill y
    pack configure $xfW.$xfName.hscroll -side bottom -fill x
    pack configure $xfW.$xfName.$xfName -side left -fill both -expand 1
  }
}

##########
# Procedure: XFTmpltToplevel
# Description: build a toplevel window
# Arguments: xfName - the name of the widget we want to build
#            xfGeometry - the geometry of the toplevel
#            xfTitle - the title of the toplevel
#            {xfLeader} - the leading window of the toplevel
# Returns: none
# Sideeffects: none
##########
proc XFTmpltToplevel {xfName xfGeometry xfTitle {xfLeader ""}} {

  XFTmpltToplevelLong $xfName $xfGeometry $xfTitle {1000 1000} {100 100} \
    $xfLeader
}

##########
# Procedure: XFTmpltToplevelLong
# Description: build a toplevel window (long version)
# Arguments: xfName - the name of the widget we want to build
#            xfGeometry - the geometry of the toplevel
#            xfTitle - the title of the toplevel
#            xfMaxSize - the maximal size of the toplevel
#            xfMinSize - the minimal size of the toplevel
#            {xfLeader} - the leading window of the toplevel
# Returns: none
# Sideeffects: none
##########
proc XFTmpltToplevelLong {xfName xfGeometry xfTitle xfMaxSize xfMinSize {xfLeader ""}} {
  global xfConf
  global xfPos

  # look if we can use an existing toplevel
  if {"[info commands $xfName]" != ""} {
    # destroy children
    foreach xfCounter [winfo children $xfName] {
      catch "XFDestroy $xfCounter"
    }
  } {
    # make new toplevel
    toplevel $xfName \
      -borderwidth 0
  }
  wm title $xfName $xfTitle
  wm maxsize $xfName [lindex $xfMaxSize 0] [lindex $xfMaxSize 1]
  wm minsize $xfName [lindex $xfMinSize 0] [lindex $xfMinSize 1]
  wm withdraw $xfName
  if {"$xfGeometry" != ""} {
    wm geometry $xfName $xfGeometry
  }
  wm deiconify $xfName

  if {[string match .xf* $xfName]} {
    wm protocol $xfName WM_DELETE_WINDOW XFProcFileQuit
  }
  if {"$xfLeader" != ""} {
    if {!$xfConf(autoStack)} {
      return
    }
    # place new window over the leader
    XFMiscSetRootPos $xfName $xfGeometry $xfLeader
  } {
    if {!$xfConf(autoPos) && !$xfConf(autoSize)} {
      return
    }
    # place XF windows
    set xfWindowName ""
    case $xfName in {
      {.xfEdit} {
        wm minsize $xfName 10 10
        set xfWindowName edit
      }
      {.xfFSBox} {
        set xfWindowName fs
      }
      {.xfReadBox} {
        set xfWindowName read
      }
      {.xfEditProc} {
        set xfWindowName editProc
      }
      {.xfModules} {
        set xfWindowName modules
      }
      {.xfMenuBar} {
        set xfWindowName menuBarConf
      }
      {.xfIconBar*} {
        if {$xfConf(autoPos)} {
          wm geometry $xfName +[lindex $xfPos(iconBar) 2]+[lindex $xfPos(iconBar) 3]
          bind $xfName <Configure> "
            set xfTmpGeometry \[XFMiscParseGeometry \[wm geometry $xfName\] $xfName\]
            set xfPos(iconBar) \"\[winfo width $xfName\] \[winfo height $xfName\] \[lindex \$xfTmpGeometry 2\] \[lindex \$xfTmpGeometry 3\]\""
        }
      }
      {.xfIconBarEdit} {
        set xfWindowName iconBarEdit
      }
      {.xfInfoAliases} {
        set xfWindowName infoAliases
      }
      {.xfInfoCmd} {
        set xfWindowName infoCmds
      }
      {.xfInfoErrors} {
        set xfWindowName infoErrors
      }
      {.xfInfoGlob} {
        set xfWindowName infoGlob
      }
      {.xfInfoPixmaps} {
        set xfWindowName infoPixmaps
      }
      {.xfInfoProc} {
        set xfWindowName infoProc
      }
      {.xfLayout} {
        set xfWindowName layout
      }
      {.xfOptionsBind} {
        set xfWindowName optionsBind
      }
      {.xfOptionsGeneral} {
        set xfWindowName optionsGeneral
      }
      {.xfOptionsGroups} {
        set xfWindowName groups
      }
      {.xfOptionsInterp} {
        set xfWindowName optionsInterp
      }
      {.xfOptionsPath} {
        set xfWindowName optionsPath
      }
      {.xfOptionsSource} {
        set xfWindowName optionsSource
      }
      {.xfOptionsVersion} {
        set xfWindowName optionsVersion
      }
      {.xfOptionsWindow} {
        set xfWindowName optionsWindow
      }
      {.xfParameters} {
        set xfWindowName parameters
      }
      {.xfShowName} {
        set xfWindowName showName
      }
      {.xfTextBoxScript} {
        set xfWindowName script
      }
      {.xfCutPaste} {
        set xfWindowName pasteTree
      }
      {.xfCutbuffer} {
        set xfWindowName pasteScript
      }
      {.xfSizing} {
        set xfWindowName sizing
      }
      {.xfWidgetTree} {
        set xfWindowName widgetTree
      }
      {.xfPacking*} {
        set xfWindowName packing
      }
      {.xfPlacing*} {
        set xfWindowName placing
      }
      {.xfBinding*} {
        set xfWindowName binding
      }
      {.xf*Config0* | .xf*Config1* | .xf*Config2* | .xf*Config3* | .xf*Config4* | .xf*Config5*} {
        if {$xfConf(autoPos) && $xfConf(autoSize)} {
          wm geometry $xfName \
            $xfGeometry+[lindex $xfPos(config) 2]+[lindex $xfPos(config) 3]
        } {
          wm geometry $xfName $xfGeometry
        }
      }
      {.xfAlertBox* | .xfYesNoBox*} {
        if {$xfConf(autoPos) && $xfConf(autoSize)} {
          wm geometry $xfName \
            $xfGeometry+[lindex $xfPos(messages) 2]+[lindex $xfPos(messages) 3]
        } {
          wm geometry $xfName $xfGeometry
        }
      }
    }

    if {"$xfWindowName" != ""} {
      if {$xfConf(autoPos) && $xfConf(autoSize)} {
        wm geometry $xfName [lindex $xfPos($xfWindowName) 0]x[lindex $xfPos($xfWindowName) 1]+[lindex $xfPos($xfWindowName) 2]+[lindex $xfPos($xfWindowName) 3]
      } {
        set xfTmpGeometry [XFMiscParseGeometry $xfGeometry $xfName]
        if {$xfConf(autoPos)} {
          if {[lindex $xfTmpGeometry 0] == 0 ||
              [lindex $xfTmpGeometry 1] == 0} {
            wm geometry $xfName +[lindex $xfPos($xfWindowName) 2]+[lindex $xfPos($xfWindowName) 3]
          } {
            wm geometry $xfName [lindex $xfTmpGeometry 0]x[lindex $xfTmpGeometry 1]+[lindex $xfPos($xfWindowName) 2]+[lindex $xfPos($xfWindowName) 3]
          }
        } {
          if {[lindex $xfTmpGeometry 2] == 0 ||
              [lindex $xfTmpGeometry 3] == 0} {
            wm geometry $xfName [lindex $xfPos($xfWindowName) 0]x[lindex $xfPos($xfWindowName) 1]
          } {
            wm geometry $xfName [lindex $xfPos($xfWindowName) 0]x[lindex $xfPos($xfWindowName) 1]+[lindex $xfTmpGeometry 2]+[lindex $xfTmpGeometry 3]
          }
        }
      }
      bind $xfName <Configure> "
        set xfTmpGeometry \[XFMiscParseGeometry \[wm geometry $xfName\] $xfName\]
        set xfPos($xfWindowName) \"\[winfo width $xfName\] \[winfo height $xfName\] \[lindex \$xfTmpGeometry 2\] \[lindex \$xfTmpGeometry 3\]\""
    }
  }
}

# eof

