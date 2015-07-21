# XFNoParsing
# Program: template
# Description: configure the iconbar
#
# $Header: xficonBar.tcl[2.3] Wed Mar 10 12:06:01 1993 garfield@garfield frozen $

global xfIconBar
set xfIconBar(activeBackground) ""
set xfIconBar(activeForeground) ""
set xfIconBar(background) ""
set xfIconBar(font) ""
set xfIconBar(foreground) ""
set xfIconBar(scrollActiveForeground) ""
set xfIconBar(scrollBackground) ""
set xfIconBar(scrollForeground) ""
set xfIconBar(barBorder) 2
set xfIconBar(barIgnoreSep) 0
set xfIconBar(barRelief) sunken
set xfIconBar(iconBorder) 2
set xfIconBar(iconHeight) 20
set xfIconBar(iconOffset) 0
set xfIconBar(iconRelief) raised
set xfIconBar(iconWidth) 20
set xfIconBar(curBar) 0
set xfIconBar(curIcon) 0
set xfIconBar(packing) ""
set xfIconBar(scrollBitmap) gray50
set xfIconBar(switchBitmap) gray50
set xfIconBar(separator) :

proc XFIconBarInit {xfIconBarUserFile xfIconBarFile xfIconBarIcons} {# xf ignore me 5
##########
# Procedure: XFIconBarInit
# Description: initialize the configuration of menubuttons and
#              menus of specified pathnames
# Arguments: xfIconBarUserFile - the user specific loadfile
#            xfIconBarFile - the default loadfile
#            xfIconBarIcons - the default icon pathname
# Returns: none
# Sideeffects: none
##########

  global xfIconBar tcl_platform

  if {![string compare "windows" $tcl_platform(platform)]} {
    set xfIconBar(separator) ";"
  } {
    set xfIconBar(separator) ":"
  }
  set xfIconBar(initialized) 1
  set xfIconBar(file) $xfIconBarFile
  set xfIconBar(userFile) $xfIconBarUserFile
  set xfIconBar(icons) $xfIconBarIcons
  if {[file exists $xfIconBar(userFile)]} {
    if {[catch "source \"$xfIconBar(userFile)\"" xfIconBarResult]} {
      puts stderr $xfIconBarResult
    }
  } {
    if {[file exists $xfIconBar(file)]} {
      if {[catch "source \"$xfIconBar(file)\"" xfIconBarResult]} {
        puts stderr $xfIconBarResult
      }
    }
  }

  set tmpIcon xfIconBarSw
  set tmpIconName gray50
  foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
    if {[file exists $tmpIconPath/$tmpIcon]} {
      set tmpIconName @$tmpIconPath/$tmpIcon
      break
    }
  }
  set xfIconBar(switchBitmap) $tmpIconName

  set tmpIcon xfIconBarSc
  set tmpIconName gray50
  foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
    if {[file exists $tmpIconPath/$tmpIcon]} {
      set tmpIconName @$tmpIconPath/$tmpIcon
      break
    }
  }
  set xfIconBar(scrollBitmap) $tmpIconName
}

proc XFIconBarRemove {xfIconBarName {xfIconBarPath ""}} {# xf ignore me 5
##########
# Procedure: XFIconBarRemove
# Description: remove the icon bar
# Arguments: xfIconBarName - the icon bar name
#            {xfIconBarPath} - the instertation pathname
# Returns: none
# Sideeffects: none
##########

  set tmpPath .xfIconBar$xfIconBarName
  if {"[info commands $tmpPath]" != ""} {
    if {"[info commands XFDestroy]" != ""} {
      catch "XFDestroy $tmpPath"
    } {
      catch "destroy $tmpPath"
    }
  }
  if {"[info commands $xfIconBarPath]" != ""} {
    foreach counter [winfo children $xfIconBarPath] {
      if {"[info commands XFDestroy]" != ""} {
        catch "XFDestroy $counter"
      } {
        catch "destroy $counter"
      }
    }
    pack unpack $xfIconBarPath
  }
}

proc XFIconBarShow {xfIconBarName {xfIconBarPath ""} {xfIconBarStatus ""}} {# xf ignore me 5
##########
# Procedure: XFIconBarShow
# Description: show the icon bar
# Arguments: xfIconBarName - the icon bar name
#            {xfIconBarPath} - the instertation pathname
#            {xfIconBarStatus} - create "toplevel" or "child"
# Returns: none
# Sideeffects: none
##########
# 
# global xfIconBar(activeBackground) - active background color
# global xfIconBar(activeForeground) - active foreground color
# global xfIconBar(background) - background color
# global xfIconBar(barBorder) - the width of the icon bar frame
# global xfIconBar(barIgnoreSep) - ignore separators in toplevel mode
# global xfIconBar(barRelief) - the relief of the icon bar frame
# global xfIconBar(font) - text font
# global xfIconBar(foreground) - foreground color
# global xfIconBar(iconBorder) - the border of the icons
# global xfIconBar(iconHeight) - the height of the icons
# global xfIconBar(iconOffset) - the offset between the icons
# global xfIconBar(iconRelief) - the relief of the buttons
# global xfIconBar(iconWidth) - the width of the icons
# global xfIconBar(scrollActiveForeground) - scrollbar active background color
# global xfIconBar(scrollBackground) - scrollbar background color
# global xfIconBar(scrollForeground) - scrollbar foreground color
# global xfIconBar(scrollSide) - side where scrollbar is located

  global xfIconBar tcl_platform

  if {![string compare "windows" $tcl_platform(platform)]} {
    set xfIconBar(separator) ";"
  } {
    set xfIconBar(separator) ":"
  }
  if {![info exists xfIconBar(initialized)]} {
    return
  }
  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$xfIconBar(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfIconBar(activeBackground)\" "
  }
  if {"$xfIconBar(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfIconBar(activeForeground)\" "
  }
  if {"$xfIconBar(background)" != ""} {
    append tmpButtonOpt "-background \"$xfIconBar(background)\" "
    append tmpFrameOpt "-background \"$xfIconBar(background)\" "
    append tmpMessageOpt "-background \"$xfIconBar(background)\" "
  }
  if {"$xfIconBar(font)" != ""} {
    append tmpButtonOpt "-font \"$xfIconBar(font)\" "
    append tmpMessageOpt "-font \"$xfIconBar(font)\" "
  }
  if {"$xfIconBar(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfIconBar(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfIconBar(foreground)\" "
  }

  if {"[info commands $xfIconBarPath]" == ""} {
    set xfIconBarStatus "toplevel"
  } {
    foreach counter [winfo children $xfIconBarPath] {
      if {"[info commands XFDestroy]" != ""} {
        catch "XFDestroy $counter"
      } {
        catch "destroy $counter"
      }
    }
  }

  if {"$xfIconBarStatus" == "toplevel"} {
    set tmpPath .xfIconBar$xfIconBarName
    if {"[info commands $tmpPath]" == ""} {
      if {"$xfIconBar(packing)" == "" && \
          "[info commands $xfIconBarPath]" != ""} {
        set xfIconBar(packing) [pack slave [winfo parent $xfIconBarPath]]
        pack unpack $xfIconBarPath
      }
        XFTmpltToplevel $tmpPath "" "Icon bar: $xfIconBarName"
    } {
      foreach counter [winfo children $tmpPath] {
        if {"[info commands XFDestroy]" != ""} {
          catch "XFDestroy $counter"
        } {
          catch "destroy $counter"
        }
      }
    }
        
    if {$xfIconBar(barIgnoreSep)} {
      frame $tmpPath.xfIconBar \
        -borderwidth $xfIconBar(barBorder) \
        -relief $xfIconBar(barRelief)
      catch "$tmpPath.xfIconBar config $tmpFrameOpt"

      if {[info exists xfIconBar(bar,$xfIconBarName)]} {
        set tmpCounter 0
        foreach counter $xfIconBar(bar,$xfIconBarName) {
          if {"[lindex $counter 0]" == "Iconbar-space"} {
            frame $tmpPath.xfIconBar.space$tmpCounter \
              -borderwidth 0 \
              -height 2 \
              -relief flat \
              -width 6
            catch "$tmpPath.xfIconBar.space$tmpCounter config $tmpFrameOpt"

            pack append $tmpPath.xfIconBar \
                        $tmpPath.xfIconBar.space$tmpCounter "left padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
            incr tmpCounter
          } {
            if {"[lindex $counter 0]" != "Iconbar-separator"} {
              set tmpIcon [lindex $counter 0]
              set tmpIconName gray50
              foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
                if {[file exists $tmpIconPath/$tmpIcon]} {
                  set tmpIconName @$tmpIconPath/$tmpIcon
                  break
                }
              }
              button $tmpPath.xfIconBar.button$tmpCounter \
                -bitmap $tmpIconName \
                -foreground "$xfIconBar(foreground)" \
                -relief $xfIconBar(iconRelief) \
                -height $xfIconBar(iconHeight) \
                -width $xfIconBar(iconWidth) \
                -command [lindex $counter 1]
              catch "$tmpPath.xfIconBar.button$tmpCounter config $tmpButtonOpt"

              pack append $tmpPath.xfIconBar \
                          $tmpPath.xfIconBar.button$tmpCounter "left padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
              incr tmpCounter
            }
          }
        }
      }

      if {"[info commands $xfIconBarPath]" != ""} {
        button $tmpPath.xfIconBar.buttonswitch \
          -bitmap $xfIconBar(switchBitmap) \
          -borderwidth $xfIconBar(iconBorder) \
          -relief $xfIconBar(iconRelief) \
          -height $xfIconBar(iconHeight) \
          -width $xfIconBar(iconWidth) \
          -command "XFIconBarShow $xfIconBarName $xfIconBarPath child"
        catch "$tmpPath.xfIconBar.buttonswitch config $tmpButtonOpt"
         pack append $tmpPath.xfIconBar \
                    $tmpPath.xfIconBar.buttonswitch "right padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
      }
      pack append $tmpPath \
                  $tmpPath.xfIconBar {top fill expand}
    } {
      frame $tmpPath.xfIconBar \
        -borderwidth 0
      catch "$tmpPath.xfIconBar config $tmpFrameOpt"

      set sepCounter 0
      set tmpCounter 0
      frame $tmpPath.xfIconBar.row$sepCounter \
        -borderwidth $xfIconBar(barBorder) \
        -relief $xfIconBar(barRelief)
      catch "$tmpPath.xfIconBar.row$sepCounter config $tmpFrameOpt"

      if {[info exists xfIconBar(bar,$xfIconBarName)]} {
        foreach counter $xfIconBar(bar,$xfIconBarName) {
          if {"[lindex $counter 0]" == "Iconbar-separator"} {
            pack append $tmpPath.xfIconBar \
                        $tmpPath.xfIconBar.row$sepCounter {top fill}
            incr sepCounter
            frame $tmpPath.xfIconBar.row$sepCounter \
              -borderwidth $xfIconBar(barBorder) \
              -relief $xfIconBar(barRelief)
            catch "$tmpPath.xfIconBar.row$sepCounter config $tmpFrameOpt"
          }
          if {"[lindex $counter 0]" == "Iconbar-space"} {
            frame $tmpPath.xfIconBar.row$sepCounter.space$tmpCounter \
              -borderwidth 0 \
              -height 2 \
              -relief flat \
              -width 6
            catch "$tmpPath.xfIconBar.row$sepCounter.space$tmpCounter config $tmpFrameOpt"

            pack append $tmpPath.xfIconBar.row$sepCounter \
                        $tmpPath.xfIconBar.row$sepCounter.space$tmpCounter "left padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
            incr tmpCounter
          } {
            if {"[lindex $counter 0]" != "Iconbar-separator"} {
              set tmpIcon [lindex $counter 0]
              set tmpIconName gray50
              foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
                if {[file exists $tmpIconPath/$tmpIcon]} {
                  set tmpIconName @$tmpIconPath/$tmpIcon
                  break
                }
              }
              button $tmpPath.xfIconBar.row$sepCounter.button$tmpCounter \
                -bitmap $tmpIconName \
                -borderwidth $xfIconBar(iconBorder) \
                -relief $xfIconBar(iconRelief) \
                -height $xfIconBar(iconHeight) \
                -width $xfIconBar(iconWidth) \
                -command [lindex $counter 1]
              catch "$tmpPath.xfIconBar.row$sepCounter.button$tmpCounter config $tmpButtonOpt"

              pack append $tmpPath.xfIconBar.row$sepCounter \
                          $tmpPath.xfIconBar.row$sepCounter.button$tmpCounter "left padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
              incr tmpCounter
            }
          }
        }
      }

      if {"[info commands $xfIconBarPath]" != ""} {
        button $tmpPath.xfIconBar.row$sepCounter.buttonswitch \
          -bitmap $xfIconBar(switchBitmap) \
          -borderwidth $xfIconBar(iconBorder) \
          -relief $xfIconBar(iconRelief) \
          -height $xfIconBar(iconHeight) \
          -width $xfIconBar(iconWidth) \
          -command "XFIconBarShow $xfIconBarName $xfIconBarPath child"
        catch "$tmpPath.xfIconBar.row$sepCounter.buttonswitch config $tmpButtonOpt"
         pack append $tmpPath.xfIconBar.row$sepCounter \
                    $tmpPath.xfIconBar.row$sepCounter.buttonswitch "right padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
      }
      pack append $tmpPath.xfIconBar \
                  $tmpPath.xfIconBar.row$sepCounter {top fill}
      pack append $tmpPath \
                  $tmpPath.xfIconBar {top fill expand}
    }
  } {
    set tmpPath .xfIconBar$xfIconBarName
    if {"$xfIconBar(packing)" != ""} {
      catch "pack append [winfo parent $xfIconBarPath] $xfIconBar(packing)"
      set xfIconBar(packing) ""
    }
    if {"[info commands $tmpPath]" != ""} {
      if {"[info commands XFDestroy]" != ""} {
         catch "XFDestroy $tmpPath"
      } {
        catch "destroy $tmpPath"
      }
    }
    set tmpPath $xfIconBarPath
      
    frame $tmpPath.xfIconBar \
      -borderwidth $xfIconBar(barBorder) \
      -relief $xfIconBar(barRelief)
    catch "$tmpPath.xfIconBar config $tmpFrameOpt"

    if {[info exists xfIconBar(bar,$xfIconBarName)]} {
      set tmpCounter 0
      set sepCounter 0
      foreach counter $xfIconBar(bar,$xfIconBarName) {
        if {"[lindex $counter 0]" == "Iconbar-separator"} {
          incr sepCounter
        }
      }

      if {$sepCounter < $xfIconBar(curBar)} {
        set xfIconBar(curBar) 0
      }
      set sepCounter 0
      foreach counter $xfIconBar(bar,$xfIconBarName) {
        if {"[lindex $counter 0]" == "Iconbar-separator"} {
          incr sepCounter
        }
        if {$sepCounter == $xfIconBar(curBar)} {
          if {"[lindex $counter 0]" == "Iconbar-space"} {
            frame $tmpPath.xfIconBar.space$tmpCounter \
              -borderwidth 0 \
              -height 2 \
              -relief flat \
              -width 6
            catch "$tmpPath.xfIconBar.space$tmpCounter config $tmpFrameOpt"

            pack append $tmpPath.xfIconBar \
                        $tmpPath.xfIconBar.space$tmpCounter "left padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
            incr tmpCounter
          } {
            if {"[lindex $counter 0]" != "Iconbar-separator"} {
              set tmpIcon [lindex $counter 0]
              set tmpIconName gray50
              foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
                if {[file exists $tmpIconPath/$tmpIcon]} {
                  set tmpIconName @$tmpIconPath/$tmpIcon
                  break
                }
              }
              button $tmpPath.xfIconBar.button$tmpCounter \
                -bitmap $tmpIconName \
                -borderwidth $xfIconBar(iconBorder) \
                -height $xfIconBar(iconHeight) \
                -relief $xfIconBar(iconRelief) \
                -width $xfIconBar(iconWidth) \
                -command [lindex $counter 1]
              catch "$tmpPath.xfIconBar.button$tmpCounter config $tmpButtonOpt"

              pack append $tmpPath.xfIconBar \
                          $tmpPath.xfIconBar.button$tmpCounter "left padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
              incr tmpCounter
            }
          }
        }
      }
    }

    button $tmpPath.xfIconBar.buttonscroll \
      -bitmap $xfIconBar(scrollBitmap) \
      -borderwidth $xfIconBar(iconBorder) \
      -height $xfIconBar(iconHeight) \
      -relief $xfIconBar(iconRelief) \
      -width $xfIconBar(iconWidth) \
      -command "
        global xfIconBar
        incr xfIconBar(curBar)
        XFIconBarShow $xfIconBarName $xfIconBarPath child"
    catch "$tmpPath.xfIconBar.buttonscroll config $tmpButtonOpt"

    button $tmpPath.xfIconBar.buttonswitch \
      -bitmap $xfIconBar(switchBitmap) \
      -borderwidth $xfIconBar(iconBorder) \
      -height $xfIconBar(iconHeight) \
      -relief $xfIconBar(iconRelief) \
      -width $xfIconBar(iconWidth) \
      -command "XFIconBarShow $xfIconBarName $xfIconBarPath toplevel"
    catch "$tmpPath.xfIconBar.buttonswitch config $tmpButtonOpt"

    pack append $tmpPath.xfIconBar \
                $tmpPath.xfIconBar.buttonswitch "right padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)" \
                $tmpPath.xfIconBar.buttonscroll "right padx $xfIconBar(iconOffset) pady $xfIconBar(iconOffset)"
    pack append $tmpPath \
                $tmpPath.xfIconBar {top fill expand}
  }
}

proc XFIconBarConf {xfIconBarName {xfIconBarPath ""} {xfIconBarProcs ""}} {# xf ignore me 5
##########
# Procedure: XFIconBarConf
# Description: configure the menubutton and menus of
#              the given pathnames
# Arguments: xfIconBarName - the icon bar we configure
#            {xfIconBarPath} - the instertation pathname
#            {xfIconBarProcs} - the procedures to handle
# Returns: none
# Sideeffects: none
##########
# 
# global xfIconBar(activeBackground) - active background color
# global xfIconBar(activeForeground) - active foreground color
# global xfIconBar(background) - background color
# global xfIconBar(barBorder) - the width of the icon bar frame
# global xfIconBar(barRelief) - the relief of the icon bar frame
# global xfIconBar(font) - text font
# global xfIconBar(foreground) - foreground color
# global xfIconBar(iconBorder) - the border of the icons
# global xfIconBar(iconHeight) - the height of the icons
# global xfIconBar(iconOffset) - the offset between the icons
# global xfIconBar(iconRelief) - the relief of the buttons
# global xfIconBar(iconWidth) - the width of the icons
# global xfIconBar(scrollActiveForeground) - scrollbar active background color
# global xfIconBar(scrollBackground) - scrollbar background color
# global xfIconBar(scrollForeground) - scrollbar foreground color

  global xfIconBar

  if {![info exists xfIconBar(initialized)]} {
    return
  }
  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$xfIconBar(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfIconBar(activeBackground)\" "
  }
  if {"$xfIconBar(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfIconBar(activeForeground)\" "
  }
  if {"$xfIconBar(background)" != ""} {
    append tmpButtonOpt "-background \"$xfIconBar(background)\" "
    append tmpFrameOpt "-background \"$xfIconBar(background)\" "
    append tmpMessageOpt "-background \"$xfIconBar(background)\" "
    append tmpScaleOpt "-background \"$xfIconBar(background)\" "
  }
  if {"$xfIconBar(font)" != ""} {
    append tmpButtonOpt "-font \"$xfIconBar(font)\" "
    append tmpMessageOpt "-font \"$xfIconBar(font)\" "
  }
  if {"$xfIconBar(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfIconBar(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfIconBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$xfIconBar(foreground)\" "
  }
  if {"$xfIconBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$xfIconBar(scrollActiveForeground)\" "
    append tmpScrollOpt "-activeforeground \"$xfIconBar(scrollActiveForeground)\" "
  }
  if {"$xfIconBar(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfIconBar(scrollBackground)\" "
  }
  if {"$xfIconBar(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfIconBar(scrollForeground)\" "
  }

  set xfIconBar(curIcon) 0
  XFTmpltToplevel .xfIconBarEdit 520x400 "XF iconbar configuration"

  frame .xfIconBarEdit.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame2 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame2 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame2.frame4 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame2.frame4 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame2.frame5 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame2.frame5 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame2.frame6 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame2.frame6 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame7 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame7 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame3 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame3 config $tmpFrameOpt"
 
  frame .xfIconBarEdit.frame1.frame4 \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame4 config $tmpFrameOpt"
 
  label .xfIconBarEdit.frame1.frame2.frame4.message1 \
    -anchor c \
    -relief raised \
    -text "Icon pictures:"
  catch ".xfIconBarEdit.frame1.frame2.frame4.message1 config $tmpMessageOpt"
  
  scrollbar .xfIconBarEdit.frame1.frame2.frame4.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame2.frame4.icons yview"
  catch ".xfIconBarEdit.frame1.frame2.frame4.vscroll config $tmpScrollOpt"

  scrollbar .xfIconBarEdit.frame1.frame2.frame4.hscroll \
    -highlightthickness 0 \
    -orient horiz \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame2.frame4.icons xview"
  catch ".xfIconBarEdit.frame1.frame2.frame4.hscroll config $tmpScrollOpt"

  listbox .xfIconBarEdit.frame1.frame2.frame4.icons \
    -highlightthickness 0 \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfIconBarEdit.frame1.frame2.frame4.hscroll set" \
    -yscrollcommand ".xfIconBarEdit.frame1.frame2.frame4.vscroll set"
  catch ".xfIconBarEdit.frame1.frame2.frame4.icons config $tmpMessageOpt"

  label .xfIconBarEdit.frame1.frame2.frame5.message1 \
    -anchor c \
    -relief raised \
    -text "Procedures:"
  catch ".xfIconBarEdit.frame1.frame2.frame5.message1 config $tmpMessageOpt"
  
  scrollbar .xfIconBarEdit.frame1.frame2.frame5.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame2.frame5.procs yview"
  catch ".xfIconBarEdit.frame1.frame2.frame5.vscroll config $tmpScrollOpt"

  scrollbar .xfIconBarEdit.frame1.frame2.frame5.hscroll \
    -highlightthickness 0 \
    -orient horiz \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame2.frame.procs xview"
  catch ".xfIconBarEdit.frame1.frame2.frame5.hscroll config $tmpScrollOpt"

  listbox .xfIconBarEdit.frame1.frame2.frame5.procs \
    -highlightthickness 0 \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfIconBarEdit.frame1.frame2.frame5.hscroll set" \
    -yscrollcommand ".xfIconBarEdit.frame1.frame2.frame5.vscroll set"
  catch ".xfIconBarEdit.frame1.frame2.frame5.procs config $tmpMessageOpt"

  label .xfIconBarEdit.frame1.frame2.frame6.message1 \
    -anchor c \
    -relief raised \
    -text "Iconbar Icons:"
  catch ".xfIconBarEdit.frame1.frame2.frame6.message1 config $tmpMessageOpt"
  
  scrollbar .xfIconBarEdit.frame1.frame2.frame6.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame2.frame6.bar yview"
  catch ".xfIconBarEdit.frame1.frame2.frame6.vscroll config $tmpScrollOpt"

  scrollbar .xfIconBarEdit.frame1.frame2.frame6.hscroll \
    -highlightthickness 0 \
    -orient horiz \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame2.frame6.bar xview"
  catch ".xfIconBarEdit.frame1.frame2.frame6.hscroll config $tmpScrollOpt"

  listbox .xfIconBarEdit.frame1.frame2.frame6.bar \
    -highlightthickness 0 \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfIconBarEdit.frame1.frame2.frame6.hscroll set" \
    -yscrollcommand ".xfIconBarEdit.frame1.frame2.frame6.vscroll set"
  catch ".xfIconBarEdit.frame1.frame2.frame6.bar config $tmpMessageOpt"

  scale .xfIconBarEdit.frame1.frame2.frame6.mover \
    -highlightthickness 0 \
    -orient vertical \
    -width 8 \
    -relief raised \
    -sliderlength 15 \
    -from 0 \
    -command "XFIconBarReposition \"$xfIconBarName\""
  catch ".xfIconBarEdit.frame1.frame2.frame6.mover config $tmpScaleOpt"

  frame .xfIconBarEdit.frame1.frame4.bitmap \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame4.bitmap config $tmpFrameOpt"
 
  label .xfIconBarEdit.frame1.frame4.bitmap.message1 \
    -anchor c \
    -relief raised \
    -text "Current bitmap:"
  catch ".xfIconBarEdit.frame1.frame4.bitmap.message1 config $tmpMessageOpt"
  
  label .xfIconBarEdit.frame1.frame4.bitmap.bitmap \
    -bitmap gray50 \
    -anchor c \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame4.bitmap.bitmap config $tmpMessageOpt"
  
  label .xfIconBarEdit.frame1.frame4.message1 \
    -anchor c \
    -relief raised \
    -text "Command:"
  catch ".xfIconBarEdit.frame1.frame4.message1 config $tmpMessageOpt"
  
  frame .xfIconBarEdit.frame1.frame4.command \
    -borderwidth 0 \
    -relief raised
  catch ".xfIconBarEdit.frame1.frame4.command config $tmpFrameOpt"

  text .xfIconBarEdit.frame1.frame4.command.command \
    -highlightthickness 0 \
    -height 7 \
    -relief raised \
    -wrap none \
    -borderwidth 2 \
    -yscrollcommand ".xfIconBarEdit.frame1.frame4.command.vscroll set"
  catch ".xfIconBarEdit.frame1.frame4.command.command config $tmpMessageOpt"

  scrollbar .xfIconBarEdit.frame1.frame4.command.vscroll \
    -highlightthickness 0 \
    -relief raised \
    -command ".xfIconBarEdit.frame1.frame4.command.command yview"
  catch ".xfIconBarEdit.frame1.frame4.command.vscroll config $tmpScrollOpt"

  button .xfIconBarEdit.frame1.frame7.insert \
    -text "Add" \
    -command "XFIconBarInsert \"$xfIconBarName\""
  catch ".xfIconBarEdit.frame1.frame7.insert config $tmpButtonOpt"

  button .xfIconBarEdit.frame1.frame7.insertsep \
    -text "Add separator" \
    -command "XFIconBarInsert \"$xfIconBarName\" sep"
  catch ".xfIconBarEdit.frame1.frame7.insertsep config $tmpButtonOpt"

  button .xfIconBarEdit.frame1.frame7.insertspace \
    -text "Add space" \
    -command "XFIconBarInsert \"$xfIconBarName\" space"
  catch ".xfIconBarEdit.frame1.frame7.insertspace config $tmpButtonOpt"

  button .xfIconBarEdit.frame1.frame7.modify \
    -text "Edit" \
    -command "XFIconBarModify \"$xfIconBarName\""
  catch ".xfIconBarEdit.frame1.frame7.modify config $tmpButtonOpt"

  button .xfIconBarEdit.frame1.frame7.delete \
    -text "Remove" \
    -command "
      if {\[.xfIconBarEdit.frame1.frame2.frame6.bar size\] > 0} {
        XFIconBarDelete \"$xfIconBarName\"
      }"
  catch ".xfIconBarEdit.frame1.frame7.delete config $tmpButtonOpt"

  button .xfIconBarEdit.frame1.frame3.ok \
    -text "OK" \
    -command "
      if {\"$xfIconBarPath\" != \"\"} {
        XFIconBarShow $xfIconBarName $xfIconBarPath
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfIconBarEdit}
      } {
        catch {destroy .xfIconBarEdit}
      }"
  catch ".xfIconBarEdit.frame1.frame3.ok config $tmpButtonOpt"

  button .xfIconBarEdit.frame1.frame3.save \
    -text "Save" \
    -command "XFIconBarSave"
  catch ".xfIconBarEdit.frame1.frame3.save config $tmpButtonOpt"

  # bindings
  bind .xfIconBarEdit.frame1.frame2.frame4.icons <ButtonPress-1> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame4.icons <Button1-Motion> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame4.icons <Shift-ButtonPress-1> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame4.icons <Shift-Button1-Motion> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame5.procs <ButtonPress-1> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame5.procs <Button1-Motion> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame5.procs <Shift-ButtonPress-1> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame5.procs <Shift-Button1-Motion> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame6.bar <ButtonPress-1> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame6.bar <Button1-Motion> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame6.bar <Shift-ButtonPress-1> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"
  bind .xfIconBarEdit.frame1.frame2.frame6.bar <Shift-Button1-Motion> "
   XFIconBarSelect %W \"$xfIconBarName\" %y"

  XFIconBarReadIcons
  XFIconBarReadProcs $xfIconBarProcs
  XFIconBarReadXFIconBar $xfIconBarName

  # packing
  pack append .xfIconBarEdit.frame1.frame7 \
              .xfIconBarEdit.frame1.frame7.insert {left fill expand} \
              .xfIconBarEdit.frame1.frame7.insertsep {left fill expand} \
              .xfIconBarEdit.frame1.frame7.insertspace {left fill expand} \
              .xfIconBarEdit.frame1.frame7.modify {left fill expand} \
              .xfIconBarEdit.frame1.frame7.delete {left fill expand}
  pack append .xfIconBarEdit.frame1.frame3 \
              .xfIconBarEdit.frame1.frame3.ok {left fill expand} \
              .xfIconBarEdit.frame1.frame3.save {left fill expand}
  pack append .xfIconBarEdit.frame1.frame2.frame4 \
              .xfIconBarEdit.frame1.frame2.frame4.message1 {top fill} \
              .xfIconBarEdit.frame1.frame2.frame4.vscroll "$xfIconBar(scrollSide) filly" \
              .xfIconBarEdit.frame1.frame2.frame4.hscroll {bottom fillx} \
              .xfIconBarEdit.frame1.frame2.frame4.icons {left fill expand}
  pack append .xfIconBarEdit.frame1.frame2.frame5 \
              .xfIconBarEdit.frame1.frame2.frame5.message1 {top fill} \
              .xfIconBarEdit.frame1.frame2.frame5.vscroll "$xfIconBar(scrollSide) filly" \
              .xfIconBarEdit.frame1.frame2.frame5.hscroll {bottom fillx} \
              .xfIconBarEdit.frame1.frame2.frame5.procs {left fill expand}
  pack append .xfIconBarEdit.frame1.frame2.frame6 \
              .xfIconBarEdit.frame1.frame2.frame6.message1 {top fill} \
              .xfIconBarEdit.frame1.frame2.frame6.mover {right filly} \
              .xfIconBarEdit.frame1.frame2.frame6.vscroll "$xfIconBar(scrollSide) filly" \
              .xfIconBarEdit.frame1.frame2.frame6.hscroll {bottom fillx} \
              .xfIconBarEdit.frame1.frame2.frame6.bar {left fill expand}
  pack append .xfIconBarEdit.frame1.frame2 \
              .xfIconBarEdit.frame1.frame2.frame4 {left fill expand} \
              .xfIconBarEdit.frame1.frame2.frame5 {left fill expand} \
              .xfIconBarEdit.frame1.frame2.frame6 {left fill expand}
  pack append .xfIconBarEdit.frame1.frame4.bitmap \
              .xfIconBarEdit.frame1.frame4.bitmap.message1 {top fill} \
              .xfIconBarEdit.frame1.frame4.bitmap.bitmap {top fill expand}
  pack append .xfIconBarEdit.frame1.frame4.command \
              .xfIconBarEdit.frame1.frame4.command.vscroll "$xfIconBar(scrollSide) filly" \
              .xfIconBarEdit.frame1.frame4.command.command {left fill expand}
  pack append .xfIconBarEdit.frame1.frame4 \
              .xfIconBarEdit.frame1.frame4.bitmap {left fill} \
              .xfIconBarEdit.frame1.frame4.message1 {top fill} \
              .xfIconBarEdit.frame1.frame4.command {top fill expand}
  pack append .xfIconBarEdit.frame1 \
              .xfIconBarEdit.frame1.frame3 {bottom fill} \
              .xfIconBarEdit.frame1.frame7 {bottom fill} \
              .xfIconBarEdit.frame1.frame4 {bottom fill} \
              .xfIconBarEdit.frame1.frame2 {top fill expand}
  pack append .xfIconBarEdit \
              .xfIconBarEdit.frame1 {top fill expand}

  update idletask
}

##########
# Procedure: XFIconBarDelete
# Description: delete a icon button
# Arguments: xfIconBarName - the iconbar we configure
# Returns: none
# Sideeffects: none
##########
proc XFIconBarDelete {xfIconBarName} {# xf ignore me 6
  global xfIconBar

  if {[.xfIconBarEdit.frame1.frame2.frame6.bar size] > 0} {
    .xfIconBarEdit.frame1.frame2.frame6.bar delete $xfIconBar(curIcon)
    if {[info exists xfIconBar(bar,$xfIconBarName)]} {
      set xfIconBar(bar,$xfIconBarName) \
        [lreplace $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) $xfIconBar(curIcon)]
    }
    .xfIconBarEdit.frame1.frame2.frame6.bar select anchor $xfIconBar(curIcon)
    .xfIconBarEdit.frame1.frame2.frame6.bar select set $xfIconBar(curIcon)
    set xfIconBar(curIcon) [.xfIconBarEdit.frame1.frame2.frame6.bar curselection]
    .xfIconBarEdit.frame1.frame2.frame6.mover config \
      -to [llength $xfIconBar(bar,$xfIconBarName)]
  } {
    .xfIconBarEdit.frame1.frame2.frame6.mover config \
      -to 0
  }
}

##########
# Procedure: XFIconBarInsert
# Description: insert a icon button
# Arguments: xfIconBarName - the iconbar we configure
#            xfIconBarType - what do we insert, a "sep"arator ?
# Returns: none
# Sideeffects: none
##########
proc XFIconBarInsert {xfIconBarName {xfIconBarType ""}} {# xf ignore me 6
  global xfIconBar

  if {"$xfIconBarType" == "sep"} {
    set tmpValue "Iconbar-separator"
    .xfIconBarEdit.frame1.frame2.frame6.bar insert $xfIconBar(curIcon) $tmpValue
    lappend tmpValue { }
    if {[info exists xfIconBar(bar,$xfIconBarName)]} {
      set xfIconBar(bar,$xfIconBarName) \
        [linsert $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) $tmpValue]
    } {
      set xfIconBar(bar,$xfIconBarName) ""
      lappend xfIconBar(bar,$xfIconBarName) $tmpValue
    }
  } {
    if {"$xfIconBarType" == "space"} {
      set tmpValue "Iconbar-space"
      .xfIconBarEdit.frame1.frame2.frame6.bar insert $xfIconBar(curIcon) $tmpValue
      lappend tmpValue { }
      if {[info exists xfIconBar(bar,$xfIconBarName)]} {
        set xfIconBar(bar,$xfIconBarName) \
          [linsert $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) $tmpValue]
      } {
        set xfIconBar(bar,$xfIconBarName) ""
        lappend xfIconBar(bar,$xfIconBarName) $tmpValue
      }
    } {
      set tmpValue [file tail [lindex [.xfIconBarEdit.frame1.frame4.bitmap.bitmap config -bitmap] 4]]
      .xfIconBarEdit.frame1.frame2.frame6.bar insert $xfIconBar(curIcon) $tmpValue
      lappend tmpValue [.xfIconBarEdit.frame1.frame4.command.command get 1.0 end]
      if {[info exists xfIconBar(bar,$xfIconBarName)]} {
        set xfIconBar(bar,$xfIconBarName) \
          [linsert $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) $tmpValue]
      } {
        set xfIconBar(bar,$xfIconBarName) ""
        lappend xfIconBar(bar,$xfIconBarName) $tmpValue
      }
    }
  }
  .xfIconBarEdit.frame1.frame2.frame6.mover config \
    -to [llength $xfIconBar(bar,$xfIconBarName)]
  incr xfIconBar(curIcon)
}

##########
# Procedure: XFIconBarModify
# Description: modify a icon button
# Arguments: xfIconBarName - the iconbar we configure
# Returns: none
# Sideeffects: none
##########
proc XFIconBarModify {xfIconBarName} {# xf ignore me 6
  global xfIconBar

  set tmpValue [file tail [lindex [.xfIconBarEdit.frame1.frame4.bitmap.bitmap config -bitmap] 4]]
  if {"[lindex [lindex $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon)] 0]" != "Iconbar-separator" &&
      "[lindex [lindex $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon)] 0]" != "Iconbar-space"} {
    .xfIconBarEdit.frame1.frame2.frame6.bar delete $xfIconBar(curIcon)
    .xfIconBarEdit.frame1.frame2.frame6.bar insert $xfIconBar(curIcon) $tmpValue
    lappend tmpValue [.xfIconBarEdit.frame1.frame4.command.command get 1.0 end]
    if {[info exists xfIconBar(bar,$xfIconBarName)]} {
      set xfIconBar(bar,$xfIconBarName) \
        [lreplace $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) $xfIconBar(curIcon) $tmpValue]
    } {
      set xfIconBar(bar,$xfIconBarName) $tmpValue
    }
  }
}

##########
# Procedure: XFIconBarReadIcons
# Description: read the icons
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFIconBarReadIcons {} {# xf ignore me 6
  global xfIconBar

  foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
    if {[file exists $tmpIconPath] && [file readable $tmpIconPath] &&
        [file isdirectory $tmpIconPath]} {
      foreach tmpName [lsort [glob -nocomplain $tmpIconPath/*]] {
        set tmpIcon [file tail $tmpName]
        if {"$tmpIcon" != "Makefile" && "$tmpIcon" != "Shapefile" &&
            "$tmpIcon" != "Version" && "$tmpIcon" != "Dependencies" &&
              [file isfile $tmpIconPath/$tmpIcon]} {
          set tmpIconList($tmpIcon) ""
        }
      }
    }
  }
  if {[info exists tmpIconList]} {
    foreach tmpIcon [lsort [array names tmpIconList]] {
      .xfIconBarEdit.frame1.frame2.frame4.icons insert end $tmpIcon
    }
  }
}

##########
# Procedure: XFIconBarReadXFIconBar
# Description: read the icon bar
# Arguments: xfIconBarName - the name of the current iconbar
# Returns: none
# Sideeffects: none
##########
proc XFIconBarReadXFIconBar {xfIconBarName} {# xf ignore me 6
  global xfIconBar

  if {[info exists xfIconBar(bar,$xfIconBarName)]} {
    foreach tmpIcon $xfIconBar(bar,$xfIconBarName) {
      .xfIconBarEdit.frame1.frame2.frame6.bar insert end [lindex $tmpIcon 0]
    }
  }
  if {[.xfIconBarEdit.frame1.frame2.frame6.bar size] > 0} {
    .xfIconBarEdit.frame1.frame2.frame6.mover config \
      -to [llength $xfIconBar(bar,$xfIconBarName)]
  }
}

##########
# Procedure: XFIconBarReadProcs
# Description: read the procedures
# Arguments: xfIconBarProcs
# Returns: none
# Sideeffects: none
##########
proc XFIconBarReadProcs {xfIconBarProcs} {# xf ignore me 6

  foreach tmpProcs [lsort $xfIconBarProcs] {
    .xfIconBarEdit.frame1.frame2.frame5.procs insert end $tmpProcs
  }
}

##########
# Procedure: XFIconBarReposition
# Description: reposition a menu item
# Arguments: xfIconBarName - the iconbar we configure
#            xfIconBarPos - the new position of the item
# Returns: none
# Sideeffects: none
##########
proc XFIconBarReposition {xfIconBarName xfIconBarPos} {# xf ignore me 6
  global xfIconBar

  if {[.xfIconBarEdit.frame1.frame2.frame6.bar size] > 0} {
    if {$xfIconBarPos < [llength $xfIconBar(bar,$xfIconBarName)]} {
      .xfIconBarEdit.frame1.frame2.frame6.bar delete $xfIconBar(curIcon)
      if {[info exists xfIconBar(bar,$xfIconBarName)]} {
       set tmpSaveValue [lindex $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon)]
         set xfIconBar(bar,$xfIconBarName) \
          [lreplace $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) $xfIconBar(curIcon)]
      }
      set xfIconBar(curIcon) $xfIconBarPos
      .xfIconBarEdit.frame1.frame2.frame6.bar insert $xfIconBar(curIcon) \
        [file tail [lindex $tmpSaveValue 0]]
      if {[info exists xfIconBar(bar,$xfIconBarName)]} {
        set xfIconBar(bar,$xfIconBarName) \
          [linsert $xfIconBar(bar,$xfIconBarName) $xfIconBar(curIcon) \
            $tmpSaveValue]
      } {
        set xfIconBar(bar,$xfIconBarName) ""
        lappend xfIconBar(bar,$xfIconBarName) $tmpSaveValue
      }
    }
    .xfIconBarEdit.frame1.frame2.frame6.bar select anchor $xfIconBar(curIcon)
    .xfIconBarEdit.frame1.frame2.frame6.bar select set $xfIconBar(curIcon)
  }
}

##########
# Procedure: XFIconBarSave
# Description: save the current definition
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFIconBarSave {} {# xf ignore me 6
  global xfIconBar

  if {![catch "open $xfIconBar(userFile) w" outFile]} {
    puts $outFile "# icon bar configuration"
    puts $outFile "global xfIconBar"
    foreach counter [lsort [array names xfIconBar]]) {
      if {[string match "bar,*" $counter]} {
        puts $outFile "# icon bar: [string range $counter 4 end]"
        puts $outFile "set xfIconBar($counter) \{[set xfIconBar($counter)]\}"
      }
    }
    puts $outFile "# eof"
    close $outFile
  } {
    puts stderr $outFile
  }
}

##########
# Procedure: XFIconBarSelect
# Description: select a specified icon/proc/iconbutton
# Arguments: xfIconBarW - the listbox
#            xfIconBarName - the iconbar we configure
#            xfIconBarY - the y coordinate in listbox
# Returns: none
# Sideeffects: none
##########
proc XFIconBarSelect {xfIconBarW xfIconBarName xfIconBarY} {# xf ignore me 6
  global xfIconBar

  .xfIconBarEdit.frame1.frame2.frame4.icons select clear 0 end
  .xfIconBarEdit.frame1.frame2.frame5.procs select clear 0 end
  .xfIconBarEdit.frame1.frame2.frame6.bar select clear 0 end

  set xfIconBarNearest [$xfIconBarW nearest $xfIconBarY]
  if {$xfIconBarNearest >= 0} {
    $xfIconBarW select anchor $xfIconBarNearest
    $xfIconBarW select set $xfIconBarNearest
    set selectedValue [$xfIconBarW get $xfIconBarNearest]
    if {"$xfIconBarW" == ".xfIconBarEdit.frame1.frame2.frame4.icons"} {
      if {"$selectedValue" != "Iconbar-separator" &&
          "$selectedValue" != "Iconbar-space"} {
        foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
          if {[file exists $tmpIconPath] && [file readable $tmpIconPath] &&
              [file isdirectory $tmpIconPath] &&
              [file exists $tmpIconPath/$selectedValue]} {
            .xfIconBarEdit.frame1.frame4.bitmap.bitmap config \
              -bitmap @$tmpIconPath/$selectedValue
            break
          }
        }
      } {
        .xfIconBarEdit.frame1.frame4.bitmap.bitmap config \
          -bitmap gray50
      }
    } {
      if {"$xfIconBarW" == ".xfIconBarEdit.frame1.frame2.frame5.procs"} {
        .xfIconBarEdit.frame1.frame4.command.command delete 1.0 end
        .xfIconBarEdit.frame1.frame4.command.command insert 1.0 $selectedValue
      } {
        set xfIconBar(curIcon) $xfIconBarNearest
        if {"$selectedValue" != "Iconbar-separator" &&
            "$selectedValue" != "Iconbar-space"} {
          foreach tmpIconPath [split $xfIconBar(icons) $xfIconBar(separator)] {
            if {[file exists $tmpIconPath] && [file readable $tmpIconPath] &&
                [file isdirectory $tmpIconPath] &&
                [file exists $tmpIconPath/$selectedValue]} {
              .xfIconBarEdit.frame1.frame4.bitmap.bitmap config \
                -bitmap @$tmpIconPath/$selectedValue
              break
            }
          }
        } {
          .xfIconBarEdit.frame1.frame4.bitmap.bitmap config \
            -bitmap gray50
        }
        .xfIconBarEdit.frame1.frame4.command.command delete 1.0 end
        .xfIconBarEdit.frame1.frame4.command.command insert 1.0 [lindex [lindex $xfIconBar(bar,$xfIconBarName) $xfIconBarNearest] 1]
      }
    }
  }
}

# eof

