# Program: xf
# Description: specify parameters for interactive layouting
#
# $Header: xflayout.tcl[2.3] Wed Mar 10 12:06:32 1993 garfield@garfield frozen $

##########
# Procedure: XFLayout
# Description: specify parameters for layouting
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFLayout {} {
  global xfMisc

  # build widget structure
  XFTmpltToplevel .xfLayout 350x150 "XF layout"

  XFTmpltFrame .xfLayout.frame1

  button .xfLayout.frame1.ok \
    -text {OK} \
    -command "destroy .xfLayout"

  XFTmpltFrame .xfLayout.frame3 0

  XFTmpltFrame .xfLayout.frame3.frame4

  XFTmpltFrame .xfLayout.frame3.frame4.width 0

  XFTmpltFrame .xfLayout.frame3.frame4.height 0

  XFTmpltFrame .xfLayout.frame3.frame4.x 0

  XFTmpltFrame .xfLayout.frame3.frame4.y 0

  XFTmpltFrame .xfLayout.frame3.frame5

  radiobutton .xfLayout.frame3.frame4.placer \
    -anchor w \
    -text {Use Placer} \
    -value {placer} \
    -variable xfConf(geometry)

  radiobutton .xfLayout.frame3.frame4.width.abs \
    -anchor w \
    -text {Absolute width} \
    -value 1 \
    -variable xfMisc(layoutWidth) \
    -command {XFLayoutToggleScale width}

  radiobutton .xfLayout.frame3.frame4.width.rel \
    -anchor w \
    -text {Relative width} \
    -value 0 \
    -variable xfMisc(layoutWidth) \
    -command {XFLayoutToggleScale width}

  radiobutton .xfLayout.frame3.frame4.height.abs \
    -anchor w \
    -text {Absolute height} \
    -value 1 \
    -variable xfMisc(layoutHeight) \
    -command {XFLayoutToggleScale height}

  radiobutton .xfLayout.frame3.frame4.height.rel \
    -anchor w \
    -text {Relative height} \
    -value 0 \
    -variable xfMisc(layoutHeight) \
    -command {XFLayoutToggleScale height}

  radiobutton .xfLayout.frame3.frame4.x.abs \
    -anchor w \
    -text {Absolute x} \
    -value 1 \
    -variable xfMisc(layoutX) \
    -command {XFLayoutToggleScale x}

  radiobutton .xfLayout.frame3.frame4.x.rel \
    -anchor w \
    -text {Relative x} \
    -value 0 \
    -variable xfMisc(layoutX) \
    -command {XFLayoutToggleScale x}

  radiobutton .xfLayout.frame3.frame4.y.abs \
    -anchor w \
    -text {Absolute y} \
    -value 1 \
    -variable xfMisc(layoutY) \
    -command {XFLayoutToggleScale y}

  radiobutton .xfLayout.frame3.frame4.y.rel \
    -anchor w \
    -text {Relative y} \
    -value 0 \
    -variable xfMisc(layoutY) \
    -command {XFLayoutToggleScale y}

  radiobutton .xfLayout.frame3.frame5.packer \
    -anchor w \
    -text {Use Packer} \
    -value {packer} \
    -variable xfConf(geometry)

  checkbutton .xfLayout.frame3.frame5.expand \
    -anchor w \
    -text {Expand widget} \
    -variable xfMisc(layoutExpand) \
    -onvalue 1 \
    -offvalue 0

  checkbutton .xfLayout.frame3.frame5.fillx \
    -anchor w \
    -text {Fill X} \
    -offvalue 0 \
    -onvalue 1 \
    -variable xfMisc(layoutFillX)

  checkbutton .xfLayout.frame3.frame5.filly \
    -anchor w \
    -text {Fill Y} \
    -offvalue 0 \
    -onvalue 1 \
    -variable xfMisc(layoutFillY)

  # packing
  pack append .xfLayout.frame1 \
              .xfLayout.frame1.ok {left padx 4 pady 4 expand}
  pack append .xfLayout.frame3.frame4.width \
              .xfLayout.frame3.frame4.width.abs {left fill expand} \
              .xfLayout.frame3.frame4.width.rel {left fill expand}
  pack append .xfLayout.frame3.frame4.height \
              .xfLayout.frame3.frame4.height.abs {left fill expand} \
              .xfLayout.frame3.frame4.height.rel {left fill expand}
  pack append .xfLayout.frame3.frame4.x \
              .xfLayout.frame3.frame4.x.abs {left fill expand} \
              .xfLayout.frame3.frame4.x.rel {left fill expand}
  pack append .xfLayout.frame3.frame4.y \
              .xfLayout.frame3.frame4.y.abs {left fill expand} \
              .xfLayout.frame3.frame4.y.rel {left fill expand}
  pack append .xfLayout.frame3.frame4 \
              .xfLayout.frame3.frame4.placer {top fillx pady 6} \
              .xfLayout.frame3.frame4.width {top fillx} \
              .xfLayout.frame3.frame4.height {top fillx} \
              .xfLayout.frame3.frame4.x {top fillx} \
              .xfLayout.frame3.frame4.y {top fillx}
  pack append .xfLayout.frame3.frame5 \
              .xfLayout.frame3.frame5.packer {top fillx pady 6} \
              .xfLayout.frame3.frame5.expand {top fillx} \
              .xfLayout.frame3.frame5.fillx {top fillx} \
              .xfLayout.frame3.frame5.filly {top fillx}
  pack append .xfLayout.frame3 \
              .xfLayout.frame3.frame4 {left fill} \
              .xfLayout.frame3.frame5 {left fill expand}
  pack append .xfLayout \
              .xfLayout.frame1 {bottom fill} \
              .xfLayout.frame3 {top fill expand}
}

##########
# Procedure: XFLayoutPosPress
# Description: interactive position setting begins
# Arguments: xfW - the widget we configure
#            xfX - root x position
#            xfY - root y position
#            xfLocalX - local x position
#            xfLocalY - local y position
# Returns: none
# Sideeffects: none
##########
proc XFLayoutPosPress {xfW xfX xfY xfLocalX xfLocalY} {
  global xfConf
  global xfMisc
  global xfStatus

  if {"[winfo class $xfW]" == "Toplevel" || "$xfW" == "."} {
    return
  }
  if {("[winfo class $xfW]" == "Canvas" &&
       "[info commands .xfCanvasConfig5]" != "")} {
    XFCanvasDrawItem $xfW $xfLocalX $xfLocalY
  }
  if {"[info commands .xfLayout]" == "" && $xfConf(layoutAlways) == 0} {
    return
  }

  set tmpWidth [winfo width $xfW]
  set tmpHeight [winfo height $xfW]
  set xfStatus(sizingHBorder) 0
  set xfStatus(sizingVBorder) 0
  if {$xfLocalX <= $xfConf(layoutBorder)} {
    set xfStatus(sizingHBorder) 1
  } {
    if {$xfLocalX >= [expr $tmpWidth-$xfConf(layoutBorder)]} {
      set xfStatus(sizingHBorder) 2
    }
  }
  if {$xfLocalY <= $xfConf(layoutBorder)} {
    set xfStatus(sizingVBorder) 1
  } {
    if {$xfLocalY >= [expr $tmpHeight-$xfConf(layoutBorder)]} {
      set xfStatus(sizingVBorder) 2
    }  
  }

  if {"$xfConf(geometry)" == "placer"} {
    set xfSlaves [place slaves [winfo parent $xfW]]
    if {"$xfSlaves" == ""} {
      XFLayoutRequestSizing $xfW
    } {
      if {"[place info $xfW]" == ""} {
        pack unpack $xfW
        place $xfW -x [winfo x $xfW] -y [winfo y $xfW]
      }
      set xfCounter1 0
      set xfPlaceInfo [place info $xfW]
      if {"$xfPlaceInfo" == ""} {
        return
      }
      set xfStatus(placeCurMaster) [winfo parent $xfW]
      set xfStatus(placeCurX) ""
      set xfStatus(placeCurY) ""
      set xfStatus(placeCurRelX) ""
      set xfStatus(placeCurRelY) ""
      set xfStatus(placeCurWidth) ""
      set xfStatus(placeCurHeight) ""
      set xfStatus(placeCurRelWidth) ""
      set xfStatus(placeCurRelHeight) ""
      foreach xfCounter $xfPlaceInfo {
        if {"$xfCounter" == "-in"} {
         set xfStatus(placeCurMaster) \
           [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        } 
        if {"$xfCounter" == "-x"} {
          set xfStatus(placeCurX) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-y"} {
          set xfStatus(placeCurY) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-relx"} {
          set xfStatus(placeCurRelX) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-rely"} {
          set xfStatus(placeCurRelY) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-width"} {
          set xfStatus(placeCurWidth) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-height"} {
          set xfStatus(placeCurHeight) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-relwidth"} {
          set xfStatus(placeCurRelWidth) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        if {"$xfCounter" == "-relheight"} {
          set xfStatus(placeCurRelHeight) \
            [lindex $xfPlaceInfo [expr $xfCounter1+1]]
        }
        incr xfCounter1 1
      }
      set xfStatus(placeOffsetX) \
        [expr ($xfX-[winfo rootx $xfStatus(placeCurMaster)])-[winfo x $xfW]]
      set xfStatus(placeOffsetY) \
        [expr ($xfY-[winfo rooty $xfStatus(placeCurMaster)])-[winfo y $xfW]]
    }
  }
}

##########
# Procedure: XFLayoutPosMove
# Description: interactive position setting
# Arguments: xfW - the widget we configure
#            xfX - root x position
#            xfY - root y position
#            xfLocalX - local x position
#            xfLocalY - local y position
# Returns: none
# Sideeffects: none
##########
proc XFLayoutPosMove {xfW xfX xfY xfLocalX xfLocalY} {
  global xfConf
  global xfMisc
  global xfStatus

  if {"[winfo class $xfW]" == "Toplevel" || "$xfW" == "." ||
      ("[winfo class $xfW]" == "Canvas" &&
       "[info commands .xfCanvasConfig5]" != "")} {
    return
  }
  if {"[info commands .xfLayout]" == "" && $xfConf(layoutAlways) == 0} {
    return
  }

  if {"$xfConf(geometry)" == "placer"} {
    set xfNewWidth ""
    set xfNewHeight ""
    set xfNewXPos ""
    set xfNewYPos ""
    if {$xfStatus(sizingHBorder) != 0 || $xfStatus(sizingVBorder)} {
      if {$xfStatus(sizingHBorder) == 1} {
        if {"$xfStatus(placeCurX)" != "" ||
            "$xfStatus(placeCurRelX)" != ""} {
          set xfNewXPos [expr ($xfX-[winfo rootx $xfStatus(placeCurMaster)])-$xfStatus(placeOffsetX)]
        }
        set xfNewWidth [expr [winfo width $xfW]+([winfo x $xfW]-(($xfX-[winfo rootx $xfStatus(placeCurMaster)])-$xfStatus(placeOffsetX)))]
      } {
        if {$xfStatus(sizingHBorder) == 2} {
          set xfNewWidth [expr ($xfX-[winfo rootx $xfStatus(placeCurMaster)])-[winfo x $xfW]]
        }
      }
      if {$xfStatus(sizingVBorder) == 1} {
        if {"$xfStatus(placeCurY)" != "" ||
            "$xfStatus(placeCurRelY)" != ""} {
          set xfNewYPos [expr ($xfY-[winfo rooty $xfStatus(placeCurMaster)])-$xfStatus(placeOffsetY)]
        }
        set xfNewHeight [expr [winfo height $xfW]+([winfo y $xfW]-(($xfY-[winfo rooty $xfStatus(placeCurMaster)])-$xfStatus(placeOffsetY)))]
      } {
        if {$xfStatus(sizingVBorder) == 2} {
          set xfNewHeight [expr ($xfY-[winfo rooty $xfStatus(placeCurMaster)])-[winfo y $xfW]]
        }
      }
    } {
      if {"$xfStatus(placeCurX)" != "" ||
          "$xfStatus(placeCurRelX)" != ""} {
        set xfNewXPos [expr ($xfX-[winfo rootx $xfStatus(placeCurMaster)])-$xfStatus(placeOffsetX)]
      }
      if {"$xfStatus(placeCurY)" != "" ||
          "$xfStatus(placeCurRelY)" != ""} {
        set xfNewYPos [expr ($xfY-[winfo rooty $xfStatus(placeCurMaster)])-$xfStatus(placeOffsetY)]
      }
    }

    if {$xfConf(gridX) > 0 && "$xfNewXPos" != ""} {
      set xfTmpPos [expr $xfNewXPos/$xfConf(gridX)]
      if {[expr $xfNewXPos%$xfConf(gridX)] > [expr $xfConf(gridX)/2]} {
        set xfNewXPos [expr ($xfTmpPos*$xfConf(gridX))+$xfConf(gridX)]
      } {
        set xfNewXPos [expr $xfTmpPos*$xfConf(gridX)]
      }
    }
    if {$xfConf(gridY) > 0 && "$xfNewYPos" != ""} {
      set xfTmpPos [expr $xfNewYPos/$xfConf(gridY)]
      if {[expr $xfNewYPos%$xfConf(gridY)] > [expr $xfConf(gridY)/2]} {
        set xfNewYPos [expr ($xfTmpPos*$xfConf(gridY))+$xfConf(gridY)]
      } {
        set xfNewYPos [expr $xfTmpPos*$xfConf(gridY)]
      }
    }
    if {$xfConf(gridX) > 0 && "$xfNewWidth" != ""} {
      set xfTmpSize [expr $xfNewWidth/$xfConf(gridX)]
      if {[expr $xfNewWidth%$xfConf(gridX)] > [expr $xfConf(gridX)/2]} {
        set xfNewWidth [expr ($xfTmpSize*$xfConf(gridX))+$xfConf(gridX)]
      } {
        set xfNewWidth [expr $xfTmpSize*$xfConf(gridX)]
      }
    }
    if {$xfConf(gridY) > 0 && "$xfNewHeight" != ""} {
      set xfTmpSize [expr $xfNewHeight/$xfConf(gridY)]
      if {[expr $xfNewHeight%$xfConf(gridY)] > [expr $xfConf(gridY)/2]} {
        set xfNewHeight [expr ($xfTmpSize*$xfConf(gridY))+$xfConf(gridY)]
      } {
        set xfNewHeight [expr $xfTmpSize*$xfConf(gridY)]
      }
    }

    set xfPlacingWindows ""
    if {"[info commands .xfPlacing*]" != ""} {
      foreach xfElement [winfo children .] {
        if {[string match .xfPlacing* $xfElement]} { 
          if {"[$xfElement.frame2.children.childs.childs get [$xfElement.frame2.children.childs.childs curselection]]" == "$xfW"} {
            lappend xfPlacingWindows $xfElement
          }
        }
      }
    }

    if {"$xfNewWidth" != "" && ("$xfStatus(placeCurWidth)" != "" ||
        ("[info commands .xfLayout]" != "" && $xfMisc(layoutWidth) == 1))} {
      place $xfW -width $xfNewWidth
      foreach xfElement $xfPlacingWindows {
        set tmpCommand \
          [lindex [$xfElement.frame2.geo.geo1 config -command] 4]
        $xfElement.frame2.geo.geo1 config -command {NoFunction}
        $xfElement.frame2.geo.geo1 set \
          [lindex [split $xfNewWidth .] 0]
        $xfElement.frame2.geo.geo1 config -command $tmpCommand
        XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
      }
    } {
      if {"$xfNewWidth" != "" && ("$xfStatus(placeCurRelWidth)" != "" ||
          ("[info commands .xfLayout]" != "" && $xfMisc(layoutWidth) != 1))} {
        set xfParentWidth [winfo width $xfStatus(placeCurMaster)]
        set xfNewWidth [expr (($xfNewWidth*100)/$xfParentWidth.0)/100.0]
        place $xfW -relwidth $xfNewWidth
        set xfNewWidth [format %.0f [expr $xfNewWidth*100]]
        foreach xfElement $xfPlacingWindows {
          set tmpCommand \
            [lindex [$xfElement.frame2.geo.geo1 config -command] 4]
          $xfElement.frame2.geo.geo1 config -command {NoFunction}
          $xfElement.frame2.geo.geo1 set \
            [lindex [split [expr $xfNewWidth*100] .] 0]
          $xfElement.frame2.geo.geo1 config -command $tmpCommand
          XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
        }
      } {
        if {"$xfNewWidth" != "" && $xfMisc(layoutWidth) == 1} {
          place $xfW -width $xfNewWidth
          foreach xfElement $xfPlacingWindows {
            set tmpCommand \
              [lindex [$xfElement.frame2.geo.geo1 config -command] 4]
            $xfElement.frame2.geo.geo1 config -command {NoFunction}
            $xfElement.frame2.geo.geo1 set \
              [lindex [split $xfNewWidth .] 0]
            $xfElement.frame2.geo.geo1 config -command $tmpCommand
            XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
          }
        } {
          if {"$xfNewWidth" != "" && $xfMisc(layoutWidth) != 1} {
            set xfParentWidth [winfo width $xfStatus(placeCurMaster)]
            set xfNewWidth [expr (($xfNewWidth*100)/$xfParentWidth.0)/100.0]
            place $xfW -relwidth $xfNewWidth
            set xfNewWidth [format %.0f [expr $xfNewWidth*100]]
            foreach xfElement $xfPlacingWindows {
              set tmpCommand \
                [lindex [$xfElement.frame2.geo.geo1 config -command] 4]
              $xfElement.frame2.geo.geo1 config -command {NoFunction}
              $xfElement.frame2.geo.geo1 set \
                [lindex [split [expr $xfNewWidth*100] .] 0]
              $xfElement.frame2.geo.geo1 config -command $tmpCommand
              XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
            }
          }
        }
      }
    }
    if {"$xfNewHeight" != "" && ("$xfStatus(placeCurHeight)" != "" ||
        ("[info commands .xfLayout]" != "" && $xfMisc(layoutHeight) == 1))} {
      place $xfW -height $xfNewHeight
      foreach xfElement $xfPlacingWindows {
        set tmpCommand \
          [lindex [$xfElement.frame2.geo.geo2 config -command] 4]
        $xfElement.frame2.geo.geo2 config -command {NoFunction}
        $xfElement.frame2.geo.geo2 set \
          [lindex [split $xfNewHeight .] 0]
        $xfElement.frame2.geo.geo2 config -command $tmpCommand
        XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
      }
    } {
      if {"$xfNewHeight" != "" && ("$xfStatus(placeCurRelHeight)" != "" ||
          ("[info commands .xfLayout]" != "" && $xfMisc(layoutHeight) != 1))} {
        set xfParentHeight [winfo height $xfStatus(placeCurMaster)]
        set xfNewHeight [expr (($xfNewHeight*100)/$xfParentHeight.0)/100.0]
        place $xfW -relheight $xfNewHeight
        set xfNewHeight [format %.0f [expr $xfNewHeight*100]]
        foreach xfElement $xfPlacingWindows {
          set tmpCommand \
            [lindex [$xfElement.frame2.geo.geo2 config -command] 4]
          $xfElement.frame2.geo.geo2 config -command {NoFunction}
          $xfElement.frame2.geo.geo2 set \
            [lindex [split [expr $xfNewHeight*100] .] 0]
          $xfElement.frame2.geo.geo2 config -command $tmpCommand
          XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
        }
      } {
        if {"$xfNewHeight" != "" && $xfMisc(layoutHeight) == 1} {
          place $xfW -height $xfNewHeight
          foreach xfElement $xfPlacingWindows {
            set tmpCommand \
              [lindex [$xfElement.frame2.geo.geo2 config -command] 4]
            $xfElement.frame2.geo.geo2 config -command {NoFunction}
            $xfElement.frame2.geo.geo2 set \
              [lindex [split $xfNewHeight .] 0]
            $xfElement.frame2.geo.geo2 config -command $tmpCommand
            XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
          }
        } {
          if {"$xfNewHeight" != "" && $xfMisc(layoutHeight) != 1} {
            set xfParentHeight [winfo height $xfStatus(placeCurMaster)]
            set xfNewHeight [expr (($xfNewHeight*100)/$xfParentHeight.0)/100.0]
            place $xfW -relheight $xfNewHeight
            set xfNewHeight [format %.0f [expr $xfNewHeight*100]]
            foreach xfElement $xfPlacingWindows {
              set tmpCommand \
                [lindex [$xfElement.frame2.geo.geo2 config -command] 4]
              $xfElement.frame2.geo.geo2 config -command {NoFunction}
              $xfElement.frame2.geo.geo2 set \
                [lindex [split [expr $xfNewHeight*100] .] 0]
              $xfElement.frame2.geo.geo2 config -command $tmpCommand
              XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
            }
          }
        }
      }
    }

    if {"$xfNewXPos" != "" && ("$xfStatus(placeCurX)" != "" ||
        ("[info commands .xfLayout]" != "" && $xfMisc(layoutX) == 1))} {
      set xfStatus(placeCurRelX) ""
      set xfStatus(placeCurX) $xfNewXPos
      place $xfW -x $xfNewXPos
      foreach xfElement $xfPlacingWindows {
        set tmpCommand \
          [lindex [$xfElement.frame2.pos.pos1 config -command] 4]
        $xfElement.frame2.pos.pos1 config -command {NoFunction}
        $xfElement.frame2.pos.pos1 set \
          [lindex [split $xfNewXPos .] 0]
        $xfElement.frame2.pos.pos1 config -command $tmpCommand
        XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
      }
    } {
      if {"$xfNewXPos" != "" && ("$xfStatus(placeCurRelX)" != "" ||
           ("[info commands .xfLayout]" != "" && $xfMisc(layoutX) != 1))} {
        set xfParentWidth [winfo width $xfStatus(placeCurMaster)]
        set xfStatus(placeCurX) ""
        set xfNewXPos [expr (($xfNewXPos*100)/$xfParentWidth)/100.0]
        set xfStatus(placeCurRelX) $xfNewXPos
        place $xfW -relx $xfNewXPos
        foreach xfElement $xfPlacingWindows {
          set tmpCommand \
            [lindex [$xfElement.frame2.pos.pos1 config -command] 4]
          $xfElement.frame2.pos.pos1 config -command {NoFunction}
          $xfElement.frame2.pos.pos1 set \
            [lindex [split [expr $xfNewXPos*100] .] 0]
          $xfElement.frame2.pos.pos1 config -command $tmpCommand
          XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
        }
      }
    }
    if {"$xfNewYPos" != "" && ("$xfStatus(placeCurY)" != "" ||
        ("[info commands .xfLayout]" != "" && $xfMisc(layoutY) == 1))} {
      set xfStatus(placeCurRelY) ""
      set xfStatus(placeCurY) $xfNewYPos
      place $xfW -y $xfNewYPos
      foreach xfElement $xfPlacingWindows {
        set tmpCommand \
          [lindex [$xfElement.frame2.pos.pos2 config -command] 4]
        $xfElement.frame2.pos.pos2 config -command {NoFunction}
        $xfElement.frame2.pos.pos2 set \
          [lindex [split $xfNewYPos .] 0]
        $xfElement.frame2.pos.pos2 config -command $tmpCommand
        XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
      }
    } {
      if {"$xfNewYPos" != "" && ("$xfStatus(placeCurRelY)" != "" ||
          ("[info commands .xfLayout]" != "" && $xfMisc(layoutY) != 1))} {
        set xfParentHeight [winfo height $xfStatus(placeCurMaster)]
        set xfStatus(placeCurY) ""
        set xfNewYPos [expr (($xfNewYPos*100)/$xfParentHeight)/100.0]
        set xfStatus(placeCurRelY) $xfNewYPos
        place $xfW -rely $xfNewYPos
        foreach xfElement $xfPlacingWindows {
          set tmpCommand \
            [lindex [$xfElement.frame2.pos.pos2 config -command] 4]
          $xfElement.frame2.pos.pos2 config -command {NoFunction}
          $xfElement.frame2.pos.pos2 set \
            [lindex [split [expr $xfNewYPos*100] .] 0]
          $xfElement.frame2.pos.pos2 config -command $tmpCommand
          XFPlacingUpdatePlacing $xfW [string range [winfo toplevel $xfElement] 10 end] 0
        }
      }
    }
  } {
    if {$xfStatus(sizingHBorder) != 0 || $xfStatus(sizingVBorder)} {
      set xfPackList [pack slave [winfo parent $xfW]]
      set xfElementType "window"
      set xfPackCommand "pack append [winfo parent $xfW]"
      set xfThisOne 0
      foreach xfCounter $xfPackList {
        if {"$xfElementType" == "window"} {
          append xfPackCommand " $xfCounter"
          if {"$xfW" == "$xfCounter"} {
            set xfThisOne 1
          } {
            set xfThisOne 0
          }
          set xfElementType "options"
        } {
          if {$xfThisOne == 1} {
            set xfNewOptions ""
            foreach xfCounter1 $xfCounter {
              if {"$xfCounter1" != "fill" && "$xfCounter1" != "fillx" &&
                  "$xfCounter1" != "filly" && "$xfCounter1" != "expand"} {
                append xfNewOptions " $xfCounter1"
              }
            }
            if {$xfMisc(layoutFillX) == 1 && $xfMisc(layoutFillY) == 1} {
              append xfNewOptions " fill"
            } {
              if {$xfMisc(layoutFillX) == 1} {
                append xfNewOptions " fillx"
              } {
                if {$xfMisc(layoutFillY) == 1} {
                  append xfNewOptions " filly"
                }
              }
            }
            if {$xfMisc(layoutExpand) == 1} {
              append xfNewOptions " expand"
            }
            append xfPackCommand " {$xfNewOptions}"
          } {
            append xfPackCommand " {$xfCounter}"
          }
          set xfElementType "window"
        }
      }
      if {[string length $xfPackCommand] > 12} {
        eval $xfPackCommand
      }
    }
  }
}

##########
# Procedure: XFLayoutPosRelease
# Description: interactive position setting (packer)
# Arguments: xfW - the widget we configure
#            xfX - root x position
#            xfY - root y position
#            xfLocalX - local x position
#            xfLocalY - local y position
# Returns: none
# Sideeffects: none
##########
proc XFLayoutPosRelease {xfW xfX xfY xfLocalX xfLocalY} {
  global xfConf
  global xfMisc

  if {"[winfo class $xfW]" == "Toplevel" || "$xfW" == "." ||
      ("[winfo class $xfW]" == "Canvas" &&
       "[info commands .xfCanvasConfig5]" != "")} {
    return
  }
  if {"[info commands .xfLayout]" == "" && $xfConf(layoutAlways) == 0} {
    return
  }
  if {"$xfConf(geometry)" == "packer"} {
    place forget $xfW
    set xfAlreadyPacked 0
    set xfNewSide top
    set xfCurXPos [expr $xfX-[winfo rootx [winfo parent $xfW]]]
    set xfCurYPos [expr $xfY-[winfo rooty [winfo parent $xfW]]]
    set xfWidth [winfo width [winfo parent $xfW]]
    set xfHeight [winfo height [winfo parent $xfW]]
    if {$xfCurXPos < [expr $xfWidth/2]} {
      if {$xfCurYPos < [expr $xfHeight/2]} {
        if {$xfCurXPos < $xfCurYPos} {
          set xfNewSide left
        } {
          set xfNewSide top
        }
      } {
        if {$xfCurXPos < [expr $xfHeight-$xfCurYPos]} {
          set xfNewSide left
        } {
          set xfNewSide bottom
        }
      }
    } {
      if {$xfCurYPos < [expr $xfHeight/2]} {
        if {[expr $xfWidth-$xfCurXPos] < $xfCurYPos} {
          set xfNewSide right
        } {
          set xfNewSide top
        }
      } {
        if {[expr $xfWidth-$xfCurXPos] < [expr $xfHeight-$xfCurYPos]} {
          set xfNewSide right
        } {
          set xfNewSide bottom
        }
      }
    }
    set xfPackCommand "pack $xfW "
    append xfPackCommand " -side $xfNewSide"
    if {$xfMisc(layoutFillX) == 1 && $xfMisc(layoutFillY) == 1} {
      append xfPackCommand " -fill both"
    } {
      if {$xfMisc(layoutFillX) == 1} {
        append xfPackCommand " -fill x"
      } {
        if {$xfMisc(layoutFillY) == 1} {
          append xfPackCommand " -fill y"
        }
      }
    }
    if {$xfMisc(layoutExpand) == 1} {
      append xfPackCommand " -expand 1"
    }
    eval $xfPackCommand

    set xfPackingWindow ""
    if {"[info commands .xfPacking*]" != ""} {
      foreach xfElement [winfo children .] {
        if {[string match .xfPacking* $xfElement]} { 
          if {"[lindex [$xfElement.frame2.children.childs.childs get [$xfElement.frame2.children.childs.childs curselection]] 1]" == "$xfW"} {
            set xfPackingWindow $xfElement
          }
        }
      }
    }
    if {"$xfPackingWindow" != ""} {
      set xfClass [string range [winfo toplevel $xfPackingWindow] 10 end]
      set xfMisc(${xfClass},side) $xfNewSide
      set xfMisc(${xfClass},fillX) $xfMisc(layoutFillX)
      set xfMisc(${xfClass},fillY) $xfMisc(layoutFillY)
      set xfMisc(${xfClass},expand) $xfMisc(layoutExpand)
      XFPackingUpdatePacking $xfW $xfClass 0
    }
  }
}

##########
# Procedure: XFLayoutRequestSizing
# Description: ask the user if he wants to place
# Arguments: xfW - the widget we configure
# Returns: none
# Sideeffects: none
##########
proc XFLayoutRequestSizing {xfW} {

  # build widget structure
  XFTmpltToplevelLong .xfSizing 350x100 "XF placing" \
    {700 700} {100 100}

  message .xfSizing.message1 \
    -justify center \
    -width 290 \
    -anchor n \
    -text "The widget:\n $xfW\nwould be the first placed widget!\nReally place this widget ?" \
    -relief raised

  XFTmpltFrame .xfSizing.frame1 0

  button .xfSizing.frame1.keep \
    -text {Place and keep parent size} \
    -command "
      global xfStatus
      if {\"\[winfo parent $xfW\]\" != \".\"} {
        set xfStatus(placeCurMaster) \[winfo parent $xfW\]
        set xfStatus(placeCurX) \"\"
        set xfStatus(placeCurY) \"\"
        set xfStatus(placeCurRelX) \"\"
        set xfStatus(placeCurRelY) \"\"
        set xfStatus(placeCurWidth) \"\"
        set xfStatus(placeCurHeight) \"\"
        set xfStatus(placeCurRelWidth) \"\"
        set xfStatus(placeCurRelHeight) \"\"
        set xfWidth \[winfo width \[winfo parent $xfW\]\]
        set xfHeight \[winfo height \[winfo parent $xfW\]\]
        set xfX \[winfo x \[winfo parent $xfW\]\]
        set xfY \[winfo y \[winfo parent $xfW\]\]
        pack unpack \[winfo parent $xfW\]
        place \[winfo parent $xfW\] -width \$xfWidth -height \$xfHeight -x \$xfX -y \$xfY
      }
      pack unpack $xfW
      place $xfW -x [winfo x $xfW] -y [winfo y $xfW]
      catch \"destroy .xfSizing\""

  button .xfSizing.frame1.place \
    -text {Place} \
    -command "
      global xfStatus
      set xfStatus(placeCurMaster) \[winfo parent $xfW\]
      set xfStatus(placeCurX) \"\"
      set xfStatus(placeCurY) \"\"
      set xfStatus(placeCurRelX) \"\"
      set xfStatus(placeCurRelY) \"\"
      set xfStatus(placeCurWidth) \"\"
      set xfStatus(placeCurHeight) \"\"
      set xfStatus(placeCurRelWidth) \"\"
      set xfStatus(placeCurRelHeight) \"\"
      place $xfW -x \[winfo x $xfW\] -y \[winfo y $xfW\]
      catch \"destroy .xfSizing\""

  button .xfSizing.frame1.cancel \
    -text {Don't place} \
    -command "catch \"destroy .xfSizing\""

  # packing
  pack append .xfSizing.frame1 \
              .xfSizing.frame1.keep {left fill expand} \
              .xfSizing.frame1.place {left fill expand} \
              .xfSizing.frame1.cancel {left fill expand}
  pack append .xfSizing \
              .xfSizing.frame1 {bottom fill} \
              .xfSizing.message1 {top fill expand}
}

##########
# Procedure: XFLayoutToggleScale
# Description: change the scale from abs to rel and vice versa
# Arguments: xfScale - the scale we toggle
# Returns: none
# Sideeffects: none
##########
proc XFLayoutToggleScale {xfScale} {

  if {"[info commands .xfPlacing*]" != ""} {
    foreach xfElement [winfo children .] {
      if {[string match .xfPlacing* $xfElement]} {
        set xfClass [string range [winfo toplevel $xfElement] 10 end]
        XFPlacingToggleScale $xfClass $xfScale
      }
    }
  }
}

# eof

