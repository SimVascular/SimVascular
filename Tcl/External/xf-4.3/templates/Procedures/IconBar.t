# XFNoParsing
# Program: template
# Description: configure the iconbar
#
# $Header: IconBar.t[2.4] Wed Mar 10 12:03:08 1993 garfield@garfield frozen $

global iconBar
set iconBar(activeBackground) ""
set iconBar(activeForeground) ""
set iconBar(background) ""
set iconBar(font) ""
set iconBar(foreground) ""
set iconBar(scrollActiveForeground) ""
set iconBar(scrollBackground) ""
set iconBar(scrollForeground) ""
set iconBar(scrollSide) left
set iconBar(barBorder) 2
set iconBar(barIgnoreSep) 0
set iconBar(barRelief) sunken
set iconBar(iconBorder) 2
set iconBar(iconHeight) 20
set iconBar(iconOffset) 0
set iconBar(iconRelief) raised
set iconBar(iconWidth) 20
set iconBar(icons) "/usr/local/lib/tk/xf/lib/icons"
set iconBar(file) ""
set iconBar(userFile) ""
set iconBar(curBar) 0
set iconBar(curIcon) 0
set iconBar(packing) ""
set iconBar(scrollBitmap) gray50
set iconBar(switchBitmap) gray50
set iconBar(separator) :

proc IconBarInit {iconBarUserFile iconBarFile iconBarIcons} {# xf ignore me 5
##########
# Procedure: IconBarInit
# Description: initialize the configuration of menubuttons and
#              menus of specified pathnames
# Arguments: iconBarUserFile - the user specific loadfile
#            iconBarFile - the default loadfile
#            iconBarIcons - the default icon pathname
# Returns: none
# Sideeffects: none
##########

  global iconBar tcl_platform

  if {![string compare "windows" $tcl_platform(platform)]} {
    set iconBar(separator) ";"
  } {
    set iconBar(separator) ":"
  }
  set iconBar(initialized) 1
  set iconBar(file) $iconBarFile
  set iconBar(userFile) $iconBarUserFile
  set iconBar(icons) $iconBarIcons
  if {[file exists $iconBar(userFile)]} {
    if {[catch "source \"$iconBar(userFile)\"" iconBarResult]} {
      puts stderr $iconBarResult
    }
  } {
    if {[file exists $iconBar(file)]} {
      if {[catch "source \"$iconBar(file)\"" iconBarResult]} {
        puts stderr $iconBarResult
      }
    }
  }

  set tmpIcon iconBarSw
  set tmpIconName gray50
  foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
    if {[file exists $tmpIconPath/$tmpIcon]} {
      set tmpIconName @$tmpIconPath/$tmpIcon
      break
    }
  }
  set iconBar(switchBitmap) $tmpIconName

  set tmpIcon iconBarSc
  set tmpIconName gray50
  foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
    if {[file exists $tmpIconPath/$tmpIcon]} {
      set tmpIconName @$tmpIconPath/$tmpIcon
      break
    }
  }
  set iconBar(scrollBitmap) $tmpIconName
}

proc IconBarRemove {iconBarName {iconBarPath ""}} {# xf ignore me 5
##########
# Procedure: IconBarRemove
# Description: remove the icon bar
# Arguments: iconBarName - the icon bar name
#            {iconBarPath} - the instertation pathname
# Returns: none
# Sideeffects: none
##########

  set tmpPath .iconBar$iconBarName
  if {"[info commands $tmpPath]" != ""} {
    if {"[info commands XFDestroy]" != ""} {
      catch "XFDestroy $tmpPath"
    } {
      catch "destroy $tmpPath"
    }
  }
  if {"[info commands $iconBarPath]" != ""} {
    foreach counter [winfo children $iconBarPath] {
      if {"[info commands XFDestroy]" != ""} {
        catch "XFDestroy $counter"
      } {
        catch "destroy $counter"
      }
    }
    pack unpack $iconBarPath
  }
}

proc IconBarShow {iconBarName {iconBarPath ""} {iconBarStatus ""}} {# xf ignore me 5
##########
# Procedure: IconBarShow
# Description: show the icon bar
# Arguments: iconBarName - the icon bar name
#            {iconBarPath} - the instertation pathname
#            {iconBarStatus} - create "toplevel" or "child"
# Returns: none
# Sideeffects: none
##########
# 
# global iconBar(activeBackground) - active background color
# global iconBar(activeForeground) - active foreground color
# global iconBar(background) - background color
# global iconBar(barBorder) - the width of the icon bar frame
# global iconBar(barIgnoreSep) - ignore separators in toplevel mode
# global iconBar(barRelief) - the relief of the icon bar frame
# global iconBar(font) - text font
# global iconBar(foreground) - foreground color
# global iconBar(iconBorder) - the border of the icons
# global iconBar(iconHeight) - the height of the icons
# global iconBar(iconOffset) - the offset between the icons
# global iconBar(iconRelief) - the relief of the buttons
# global iconBar(iconWidth) - the width of the icons
# global iconBar(scrollActiveForeground) - scrollbar active background color
# global iconBar(scrollBackground) - scrollbar background color
# global iconBar(scrollForeground) - scrollbar foreground color
# global iconBar(scrollSide) - side where scrollbar is located

  global iconBar tcl_platform

  if {![string compare "windows" $tcl_platform(platform)]} {
    set iconBar(separator) ";"
  } {
    set iconBar(separator) ":"
  }
  if {![info exists iconBar(initialized)]} {
    return
  }
  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$iconBar(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$iconBar(activeBackground)\" "
  }
  if {"$iconBar(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$iconBar(activeForeground)\" "
  }
  if {"$iconBar(background)" != ""} {
    append tmpButtonOpt "-background \"$iconBar(background)\" "
    append tmpFrameOpt "-background \"$iconBar(background)\" "
    append tmpMessageOpt "-background \"$iconBar(background)\" "
  }
  if {"$iconBar(font)" != ""} {
    append tmpButtonOpt "-font \"$iconBar(font)\" "
    append tmpMessageOpt "-font \"$iconBar(font)\" "
  }
  if {"$iconBar(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$iconBar(foreground)\" "
    append tmpMessageOpt "-foreground \"$iconBar(foreground)\" "
  }

  if {"[info commands $iconBarPath]" == ""} {
    set iconBarStatus "toplevel"
  } {
    foreach counter [winfo children $iconBarPath] {
      if {"[info commands XFDestroy]" != ""} {
        catch "XFDestroy $counter"
      } {
        catch "destroy $counter"
      }
    }
  }

  if {"$iconBarStatus" == "toplevel"} {
    set tmpPath .iconBar$iconBarName
    if {"[info commands $tmpPath]" == ""} {
      if {"$iconBar(packing)" == "" && \
          "[info commands $iconBarPath]" != ""} {
        set iconBar(packing) [pack info [winfo parent $iconBarPath]]
        pack unpack $iconBarPath
      }
      # start build of iconbar toplevel
      toplevel $tmpPath \
        -borderwidth 0
      catch "$tmpPath config $tmpFrameOpt"
      wm title $tmpPath "Icon bar: $iconBarName"
      # end build of iconbar toplevel
    } {
      foreach counter [winfo children $tmpPath] {
        if {"[info commands XFDestroy]" != ""} {
          catch "XFDestroy $counter"
        } {
          catch "destroy $counter"
        }
      }
    }
        
    if {$iconBar(barIgnoreSep)} {
      frame $tmpPath.iconBar \
        -borderwidth $iconBar(barBorder) \
        -relief $iconBar(barRelief)
      catch "$tmpPath.iconBar config $tmpFrameOpt"

      if {[info exists iconBar(bar,$iconBarName)]} {
        set tmpCounter 0
        foreach counter $iconBar(bar,$iconBarName) {
          if {"[lindex $counter 0]" == "Iconbar-space"} {
            frame $tmpPath.iconBar.space$tmpCounter \
              -borderwidth 0 \
              -height 2 \
              -relief flat \
              -width 6
            catch "$tmpPath.iconBar.space$tmpCounter config $tmpFrameOpt"

            pack append $tmpPath.iconBar \
                        $tmpPath.iconBar.space$tmpCounter "left padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
            incr tmpCounter
          } {
            if {"[lindex $counter 0]" != "Iconbar-separator"} {
              set tmpIcon [lindex $counter 0]
              set tmpIconName gray50
              foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
                if {[file exists $tmpIconPath/$tmpIcon]} {
                  set tmpIconName @$tmpIconPath/$tmpIcon
                  break
                }
              }
              button $tmpPath.iconBar.button$tmpCounter \
                -bitmap $tmpIconName \
                -foreground "$iconBar(foreground)" \
                -relief $iconBar(iconRelief) \
                -height $iconBar(iconHeight) \
                -width $iconBar(iconWidth) \
                -command [lindex $counter 1]
              catch "$tmpPath.iconBar.button$tmpCounter config $tmpButtonOpt"

              pack append $tmpPath.iconBar \
                          $tmpPath.iconBar.button$tmpCounter "left padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
              incr tmpCounter
            }
          }
        }
      }

      if {"[info commands $iconBarPath]" != ""} {
        button $tmpPath.iconBar.buttonswitch \
          -bitmap $iconBar(switchBitmap) \
          -borderwidth $iconBar(iconBorder) \
          -relief $iconBar(iconRelief) \
          -height $iconBar(iconHeight) \
          -width $iconBar(iconWidth) \
          -command "IconBarShow $iconBarName $iconBarPath child"
        catch "$tmpPath.iconBar.buttonswitch config $tmpButtonOpt"
         pack append $tmpPath.iconBar \
                    $tmpPath.iconBar.buttonswitch "right padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
      }
      pack append $tmpPath \
                  $tmpPath.iconBar {top fill expand}
    } {
      frame $tmpPath.iconBar \
        -borderwidth 0
      catch "$tmpPath.iconBar config $tmpFrameOpt"

      set sepCounter 0
      set tmpCounter 0
      frame $tmpPath.iconBar.row$sepCounter \
        -borderwidth $iconBar(barBorder) \
        -relief $iconBar(barRelief)
      catch "$tmpPath.iconBar.row$sepCounter config $tmpFrameOpt"

      if {[info exists iconBar(bar,$iconBarName)]} {
        foreach counter $iconBar(bar,$iconBarName) {
          if {"[lindex $counter 0]" == "Iconbar-separator"} {
            pack append $tmpPath.iconBar \
                        $tmpPath.iconBar.row$sepCounter {top fill}
            incr sepCounter
            frame $tmpPath.iconBar.row$sepCounter \
              -borderwidth $iconBar(barBorder) \
              -relief $iconBar(barRelief)
            catch "$tmpPath.iconBar.row$sepCounter config $tmpFrameOpt"
          }
          if {"[lindex $counter 0]" == "Iconbar-space"} {
            frame $tmpPath.iconBar.row$sepCounter.space$tmpCounter \
              -borderwidth 0 \
              -height 2 \
              -relief flat \
              -width 6
            catch "$tmpPath.iconBar.row$sepCounter.space$tmpCounter config $tmpFrameOpt"

            pack append $tmpPath.iconBar.row$sepCounter \
                        $tmpPath.iconBar.row$sepCounter.space$tmpCounter "left padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
            incr tmpCounter
          } {
            if {"[lindex $counter 0]" != "Iconbar-separator"} {
              set tmpIcon [lindex $counter 0]
              set tmpIconName gray50
              foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
                if {[file exists $tmpIconPath/$tmpIcon]} {
                  set tmpIconName @$tmpIconPath/$tmpIcon
                  break
                }
              }
              button $tmpPath.iconBar.row$sepCounter.button$tmpCounter \
                -bitmap $tmpIconName \
                -borderwidth $iconBar(iconBorder) \
                -relief $iconBar(iconRelief) \
                -height $iconBar(iconHeight) \
                -width $iconBar(iconWidth) \
                -command [lindex $counter 1]
              catch "$tmpPath.iconBar.row$sepCounter.button$tmpCounter config $tmpButtonOpt"

              pack append $tmpPath.iconBar.row$sepCounter \
                          $tmpPath.iconBar.row$sepCounter.button$tmpCounter "left padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
              incr tmpCounter
            }
          }
        }
      }

      if {"[info commands $iconBarPath]" != ""} {
        button $tmpPath.iconBar.row$sepCounter.buttonswitch \
          -bitmap $iconBar(switchBitmap) \
          -borderwidth $iconBar(iconBorder) \
          -relief $iconBar(iconRelief) \
          -height $iconBar(iconHeight) \
          -width $iconBar(iconWidth) \
          -command "IconBarShow $iconBarName $iconBarPath child"
        catch "$tmpPath.iconBar.row$sepCounter.buttonswitch config $tmpButtonOpt"
         pack append $tmpPath.iconBar.row$sepCounter \
                    $tmpPath.iconBar.row$sepCounter.buttonswitch "right padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
      }
      pack append $tmpPath.iconBar \
                  $tmpPath.iconBar.row$sepCounter {top fill}
      pack append $tmpPath \
                  $tmpPath.iconBar {top fill expand}
    }
  } {
    set tmpPath .iconBar$iconBarName
    if {"$iconBar(packing)" != ""} {
      catch "pack append [winfo parent $iconBarPath] $iconBar(packing)"
      set iconBar(packing) ""
    }
    if {"[info commands $tmpPath]" != ""} {
      if {"[info commands XFDestroy]" != ""} {
         catch "XFDestroy $tmpPath"
      } {
        catch "destroy $tmpPath"
      }
    }
    set tmpPath $iconBarPath
      
    frame $tmpPath.iconBar \
      -borderwidth $iconBar(barBorder) \
      -relief $iconBar(barRelief)
    catch "$tmpPath.iconBar config $tmpFrameOpt"

    if {[info exists iconBar(bar,$iconBarName)]} {
      set tmpCounter 0
      set sepCounter 0
      foreach counter $iconBar(bar,$iconBarName) {
        if {"[lindex $counter 0]" == "Iconbar-separator"} {
          incr sepCounter
        }
      }

      if {$sepCounter < $iconBar(curBar)} {
        set iconBar(curBar) 0
      }
      set sepCounter 0
      foreach counter $iconBar(bar,$iconBarName) {
        if {"[lindex $counter 0]" == "Iconbar-separator"} {
          incr sepCounter
        }
        if {$sepCounter == $iconBar(curBar)} {
          if {"[lindex $counter 0]" == "Iconbar-space"} {
            frame $tmpPath.iconBar.space$tmpCounter \
              -borderwidth 0 \
              -height 2 \
              -relief flat \
              -width 6
            catch "$tmpPath.iconBar.space$tmpCounter config $tmpFrameOpt"

            pack append $tmpPath.iconBar \
                        $tmpPath.iconBar.space$tmpCounter "left padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
            incr tmpCounter
          } {
            if {"[lindex $counter 0]" != "Iconbar-separator"} {
              set tmpIcon [lindex $counter 0]
              set tmpIconName gray50
              foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
                if {[file exists $tmpIconPath/$tmpIcon]} {
                  set tmpIconName @$tmpIconPath/$tmpIcon
                  break
                }
              }
              button $tmpPath.iconBar.button$tmpCounter \
                -bitmap $tmpIconName \
                -borderwidth $iconBar(iconBorder) \
                -height $iconBar(iconHeight) \
                -relief $iconBar(iconRelief) \
                -width $iconBar(iconWidth) \
                -command [lindex $counter 1]
              catch "$tmpPath.iconBar.button$tmpCounter config $tmpButtonOpt"

              pack append $tmpPath.iconBar \
                          $tmpPath.iconBar.button$tmpCounter "left padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
              incr tmpCounter
            }
          }
        }
      }
    }

    button $tmpPath.iconBar.buttonscroll \
      -bitmap $iconBar(scrollBitmap) \
      -borderwidth $iconBar(iconBorder) \
      -height $iconBar(iconHeight) \
      -relief $iconBar(iconRelief) \
      -width $iconBar(iconWidth) \
      -command "
        global iconBar
        incr iconBar(curBar)
        IconBarShow $iconBarName $iconBarPath child"
    catch "$tmpPath.iconBar.buttonscroll config $tmpButtonOpt"

    button $tmpPath.iconBar.buttonswitch \
      -bitmap $iconBar(switchBitmap) \
      -borderwidth $iconBar(iconBorder) \
      -height $iconBar(iconHeight) \
      -relief $iconBar(iconRelief) \
      -width $iconBar(iconWidth) \
      -command "IconBarShow $iconBarName $iconBarPath toplevel"
    catch "$tmpPath.iconBar.buttonswitch config $tmpButtonOpt"

    pack append $tmpPath.iconBar \
                $tmpPath.iconBar.buttonswitch "right padx $iconBar(iconOffset) pady $iconBar(iconOffset)" \
                $tmpPath.iconBar.buttonscroll "right padx $iconBar(iconOffset) pady $iconBar(iconOffset)"
    pack append $tmpPath \
                $tmpPath.iconBar {top fill expand}
  }
}

proc IconBarConf {iconBarName {iconBarPath ""} {iconBarProcs ""}} {# xf ignore me 5
##########
# Procedure: IconBarConf
# Description: configure the menubutton and menus of
#              the given pathnames
# Arguments: iconBarName - the icon bar we configure
#            {iconBarPath} - the instertation pathname
#            {iconBarProcs} - the procedures to handle
# Returns: none
# Sideeffects: none
##########
# 
# global iconBar(activeBackground) - active background color
# global iconBar(activeForeground) - active foreground color
# global iconBar(background) - background color
# global iconBar(barBorder) - the width of the icon bar frame
# global iconBar(barRelief) - the relief of the icon bar frame
# global iconBar(font) - text font
# global iconBar(foreground) - foreground color
# global iconBar(iconBorder) - the border of the icons
# global iconBar(iconHeight) - the height of the icons
# global iconBar(iconOffset) - the offset between the icons
# global iconBar(iconRelief) - the relief of the buttons
# global iconBar(iconWidth) - the width of the icons
# global iconBar(scrollActiveForeground) - scrollbar active background color
# global iconBar(scrollBackground) - scrollbar background color
# global iconBar(scrollForeground) - scrollbar foreground color

  global iconBar

  if {![info exists iconBar(initialized)]} {
    return
  }
  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$iconBar(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$iconBar(activeBackground)\" "
  }
  if {"$iconBar(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$iconBar(activeForeground)\" "
  }
  if {"$iconBar(background)" != ""} {
    append tmpButtonOpt "-background \"$iconBar(background)\" "
    append tmpFrameOpt "-background \"$iconBar(background)\" "
    append tmpMessageOpt "-background \"$iconBar(background)\" "
    append tmpScaleOpt "-background \"$iconBar(background)\" "
  }
  if {"$iconBar(font)" != ""} {
    append tmpButtonOpt "-font \"$iconBar(font)\" "
    append tmpMessageOpt "-font \"$iconBar(font)\" "
  }
  if {"$iconBar(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$iconBar(foreground)\" "
    append tmpMessageOpt "-foreground \"$iconBar(foreground)\" "
    append tmpScaleOpt "-foreground \"$iconBar(foreground)\" "
  }
  if {"$iconBar(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$iconBar(scrollActiveForeground)\" "
    append tmpScrollOpt "-activeforeground \"$iconBar(scrollActiveForeground)\" "
  }
  if {"$iconBar(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$iconBar(scrollBackground)\" "
  }
  if {"$iconBar(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$iconBar(scrollForeground)\" "
  }

  set iconBar(curIcon) 0
  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy .iconBarEdit}
  } {
    catch {destroy .iconBarEdit}
  }
  toplevel .iconBarEdit \
    -borderwidth 0
  catch ".iconBarEdit config $tmpFrameOpt"
  wm geometry .iconBarEdit 520x400
  wm title .iconBarEdit {Menu configuration}
  wm maxsize .iconBarEdit 1000 1000
  wm minsize .iconBarEdit 100 100
  # end build of toplevel

  frame .iconBarEdit.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame2 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame2 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame2.frame4 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame2.frame4 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame2.frame5 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame2.frame5 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame2.frame6 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame2.frame6 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame7 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame7 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame3 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame3 config $tmpFrameOpt"
 
  frame .iconBarEdit.frame1.frame4 \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame4 config $tmpFrameOpt"
 
  label .iconBarEdit.frame1.frame2.frame4.message1 \
    -anchor c \
    -relief raised \
    -text "Icon pictures:"
  catch ".iconBarEdit.frame1.frame2.frame4.message1 config $tmpMessageOpt"
  
  scrollbar .iconBarEdit.frame1.frame2.frame4.vscroll \
    -relief raised \
    -command ".iconBarEdit.frame1.frame2.frame4.icons yview"
  catch ".iconBarEdit.frame1.frame2.frame4.vscroll config $tmpScrollOpt"

  scrollbar .iconBarEdit.frame1.frame2.frame4.hscroll \
    -orient horiz \
    -relief raised \
    -command ".iconBarEdit.frame1.frame2.frame4.icons xview"
  catch ".iconBarEdit.frame1.frame2.frame4.hscroll config $tmpScrollOpt"

  listbox .iconBarEdit.frame1.frame2.frame4.icons \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".iconBarEdit.frame1.frame2.frame4.hscroll set" \
    -yscrollcommand ".iconBarEdit.frame1.frame2.frame4.vscroll set"
  catch ".iconBarEdit.frame1.frame2.frame4.icons config $tmpMessageOpt"

  label .iconBarEdit.frame1.frame2.frame5.message1 \
    -anchor c \
    -relief raised \
    -text "Procedures:"
  catch ".iconBarEdit.frame1.frame2.frame5.message1 config $tmpMessageOpt"
  
  scrollbar .iconBarEdit.frame1.frame2.frame5.vscroll \
    -relief raised \
    -command ".iconBarEdit.frame1.frame2.frame5.procs yview"
  catch ".iconBarEdit.frame1.frame2.frame5.vscroll config $tmpScrollOpt"

  scrollbar .iconBarEdit.frame1.frame2.frame5.hscroll \
    -orient horiz \
    -relief raised \
    -command ".iconBarEdit.frame1.frame2.frame.procs xview"
  catch ".iconBarEdit.frame1.frame2.frame5.hscroll config $tmpScrollOpt"

  listbox .iconBarEdit.frame1.frame2.frame5.procs \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".iconBarEdit.frame1.frame2.frame5.hscroll set" \
    -yscrollcommand ".iconBarEdit.frame1.frame2.frame5.vscroll set"
  catch ".iconBarEdit.frame1.frame2.frame5.procs config $tmpMessageOpt"

  label .iconBarEdit.frame1.frame2.frame6.message1 \
    -anchor c \
    -relief raised \
    -text "Iconbar Icons:"
  catch ".iconBarEdit.frame1.frame2.frame6.message1 config $tmpMessageOpt"
  
  scrollbar .iconBarEdit.frame1.frame2.frame6.vscroll \
    -relief raised \
    -command ".iconBarEdit.frame1.frame2.frame6.bar yview"
  catch ".iconBarEdit.frame1.frame2.frame6.vscroll config $tmpScrollOpt"

  scrollbar .iconBarEdit.frame1.frame2.frame6.hscroll \
    -orient horiz \
    -relief raised \
    -command ".iconBarEdit.frame1.frame2.frame6.bar xview"
  catch ".iconBarEdit.frame1.frame2.frame6.hscroll config $tmpScrollOpt"

  listbox .iconBarEdit.frame1.frame2.frame6.bar \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".iconBarEdit.frame1.frame2.frame6.hscroll set" \
    -yscrollcommand ".iconBarEdit.frame1.frame2.frame6.vscroll set"
  catch ".iconBarEdit.frame1.frame2.frame6.bar config $tmpMessageOpt"

  scale .iconBarEdit.frame1.frame2.frame6.mover \
    -orient vertical \
    -width 8 \
    -relief raised \
    -sliderlength 15 \
    -from 0 \
    -command "IconBarReposition \"$iconBarName\""
  catch ".iconBarEdit.frame1.frame2.frame6.mover config $tmpScaleOpt"

  frame .iconBarEdit.frame1.frame4.bitmap \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame4.bitmap config $tmpFrameOpt"
 
  label .iconBarEdit.frame1.frame4.bitmap.message1 \
    -anchor c \
    -relief raised \
    -text "Current bitmap:"
  catch ".iconBarEdit.frame1.frame4.bitmap.message1 config $tmpMessageOpt"
  
  label .iconBarEdit.frame1.frame4.bitmap.bitmap \
    -bitmap gray50 \
    -anchor c \
    -relief raised
  catch ".iconBarEdit.frame1.frame4.bitmap.bitmap config $tmpMessageOpt"
  
  label .iconBarEdit.frame1.frame4.message1 \
    -anchor c \
    -relief raised \
    -text "Command:"
  catch ".iconBarEdit.frame1.frame4.message1 config $tmpMessageOpt"
  
  frame .iconBarEdit.frame1.frame4.command \
    -borderwidth 0 \
    -relief raised
  catch ".iconBarEdit.frame1.frame4.command config $tmpFrameOpt"

  text .iconBarEdit.frame1.frame4.command.command \
    -height 7 \
    -relief raised \
    -wrap none \
    -borderwidth 2 \
    -yscrollcommand ".iconBarEdit.frame1.frame4.command.vscroll set"
  catch ".iconBarEdit.frame1.frame4.command.command config $tmpMessageOpt"

  scrollbar .iconBarEdit.frame1.frame4.command.vscroll \
    -relief raised \
    -command ".iconBarEdit.frame1.frame4.command.command yview"
  catch ".iconBarEdit.frame1.frame4.command.vscroll config $tmpScrollOpt"

  button .iconBarEdit.frame1.frame7.insert \
    -text "Add" \
    -command "IconBarInsert \"$iconBarName\""
  catch ".iconBarEdit.frame1.frame7.insert config $tmpButtonOpt"

  button .iconBarEdit.frame1.frame7.insertsep \
    -text "Add separator" \
    -command "IconBarInsert \"$iconBarName\" sep"
  catch ".iconBarEdit.frame1.frame7.insertsep config $tmpButtonOpt"

  button .iconBarEdit.frame1.frame7.insertspace \
    -text "Add space" \
    -command "IconBarInsert \"$iconBarName\" space"
  catch ".iconBarEdit.frame1.frame7.insertspace config $tmpButtonOpt"

  button .iconBarEdit.frame1.frame7.modify \
    -text "Edit" \
    -command "IconBarModify \"$iconBarName\""
  catch ".iconBarEdit.frame1.frame7.modify config $tmpButtonOpt"

  button .iconBarEdit.frame1.frame7.delete \
    -text "Remove" \
    -command "
      if {\[.iconBarEdit.frame1.frame2.frame6.bar size\] > 0} {
        IconBarDelete \"$iconBarName\"
      }"
  catch ".iconBarEdit.frame1.frame7.delete config $tmpButtonOpt"

  button .iconBarEdit.frame1.frame3.ok \
    -text "OK" \
    -command "
      if {\"$iconBarPath\" != \"\"} {
        IconBarShow $iconBarName $iconBarPath
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .iconBarEdit}
      } {
        catch {destroy .iconBarEdit}
      }"
  catch ".iconBarEdit.frame1.frame3.ok config $tmpButtonOpt"

  button .iconBarEdit.frame1.frame3.save \
    -text "Save" \
    -command "IconBarSave"
  catch ".iconBarEdit.frame1.frame3.save config $tmpButtonOpt"

  # bindings
  bind .iconBarEdit.frame1.frame2.frame4.icons <ButtonPress-1> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame4.icons <Button1-Motion> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame4.icons <Shift-ButtonPress-1> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame4.icons <Shift-Button1-Motion> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame5.procs <ButtonPress-1> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame5.procs <Button1-Motion> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame5.procs <Shift-ButtonPress-1> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame5.procs <Shift-Button1-Motion> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame6.bar <ButtonPress-1> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame6.bar <Button1-Motion> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame6.bar <Shift-ButtonPress-1> "
   IconBarSelect %W \"$iconBarName\" %y"
  bind .iconBarEdit.frame1.frame2.frame6.bar <Shift-Button1-Motion> "
   IconBarSelect %W \"$iconBarName\" %y"

  IconBarReadIcons
  IconBarReadProcs $iconBarProcs
  IconBarReadIconBar $iconBarName

  # packing
  pack append .iconBarEdit.frame1.frame7 \
              .iconBarEdit.frame1.frame7.insert {left fill expand} \
              .iconBarEdit.frame1.frame7.insertsep {left fill expand} \
              .iconBarEdit.frame1.frame7.insertspace {left fill expand} \
              .iconBarEdit.frame1.frame7.modify {left fill expand} \
              .iconBarEdit.frame1.frame7.delete {left fill expand}
  pack append .iconBarEdit.frame1.frame3 \
              .iconBarEdit.frame1.frame3.ok {left fill expand} \
              .iconBarEdit.frame1.frame3.save {left fill expand}
  pack append .iconBarEdit.frame1.frame2.frame4 \
              .iconBarEdit.frame1.frame2.frame4.message1 {top fill} \
              .iconBarEdit.frame1.frame2.frame4.vscroll "$iconBar(scrollSide) filly" \
              .iconBarEdit.frame1.frame2.frame4.hscroll {bottom fillx} \
              .iconBarEdit.frame1.frame2.frame4.icons {left fill expand}
  pack append .iconBarEdit.frame1.frame2.frame5 \
              .iconBarEdit.frame1.frame2.frame5.message1 {top fill} \
              .iconBarEdit.frame1.frame2.frame5.vscroll "$iconBar(scrollSide) filly" \
              .iconBarEdit.frame1.frame2.frame5.hscroll {bottom fillx} \
              .iconBarEdit.frame1.frame2.frame5.procs {left fill expand}
  pack append .iconBarEdit.frame1.frame2.frame6 \
              .iconBarEdit.frame1.frame2.frame6.message1 {top fill} \
              .iconBarEdit.frame1.frame2.frame6.mover {right filly} \
              .iconBarEdit.frame1.frame2.frame6.vscroll "$iconBar(scrollSide) filly" \
              .iconBarEdit.frame1.frame2.frame6.hscroll {bottom fillx} \
              .iconBarEdit.frame1.frame2.frame6.bar {left fill expand}
  pack append .iconBarEdit.frame1.frame2 \
              .iconBarEdit.frame1.frame2.frame4 {left fill expand} \
              .iconBarEdit.frame1.frame2.frame5 {left fill expand} \
              .iconBarEdit.frame1.frame2.frame6 {left fill expand}
  pack append .iconBarEdit.frame1.frame4.bitmap \
              .iconBarEdit.frame1.frame4.bitmap.message1 {top fill} \
              .iconBarEdit.frame1.frame4.bitmap.bitmap {top fill expand}
  pack append .iconBarEdit.frame1.frame4.command \
              .iconBarEdit.frame1.frame4.command.vscroll "$iconBar(scrollSide) filly" \
              .iconBarEdit.frame1.frame4.command.command {left fill expand}
  pack append .iconBarEdit.frame1.frame4 \
              .iconBarEdit.frame1.frame4.bitmap {left fill} \
              .iconBarEdit.frame1.frame4.message1 {top fill} \
              .iconBarEdit.frame1.frame4.command {top fill expand}
  pack append .iconBarEdit.frame1 \
              .iconBarEdit.frame1.frame3 {bottom fill} \
              .iconBarEdit.frame1.frame7 {bottom fill} \
              .iconBarEdit.frame1.frame4 {bottom fill} \
              .iconBarEdit.frame1.frame2 {top fill expand}
  pack append .iconBarEdit \
              .iconBarEdit.frame1 {top fill expand}

  update idletask
}

##########
# Procedure: IconBarDelete
# Description: delete a icon button
# Arguments: iconBarName - the iconbar we configure
# Returns: none
# Sideeffects: none
##########
proc IconBarDelete {iconBarName} {# xf ignore me 6
  global iconBar

  if {[.iconBarEdit.frame1.frame2.frame6.bar size] > 0} {
    .iconBarEdit.frame1.frame2.frame6.bar delete $iconBar(curIcon)
    if {[info exists iconBar(bar,$iconBarName)]} {
      set iconBar(bar,$iconBarName) \
        [lreplace $iconBar(bar,$iconBarName) $iconBar(curIcon) $iconBar(curIcon)]
    }
    .iconBarEdit.frame1.frame2.frame6.bar select clear 0 end
    .iconBarEdit.frame1.frame2.frame6.bar select set $iconBar(curIcon)
    set iconBar(curIcon) [.iconBarEdit.frame1.frame2.frame6.bar curselection]
    .iconBarEdit.frame1.frame2.frame6.mover config \
      -to [llength $iconBar(bar,$iconBarName)]
  } {
    .iconBarEdit.frame1.frame2.frame6.mover config \
      -to 0
  }
}

##########
# Procedure: IconBarInsert
# Description: insert a icon button
# Arguments: iconBarName - the iconbar we configure
#            iconBarType - what do we insert, a "sep"arator ?
# Returns: none
# Sideeffects: none
##########
proc IconBarInsert {iconBarName {iconBarType ""}} {# xf ignore me 6
  global iconBar

  if {"$iconBarType" == "sep"} {
    set tmpValue "Iconbar-separator"
    .iconBarEdit.frame1.frame2.frame6.bar insert $iconBar(curIcon) $tmpValue
    lappend tmpValue { }
    if {[info exists iconBar(bar,$iconBarName)]} {
      set iconBar(bar,$iconBarName) \
        [linsert $iconBar(bar,$iconBarName) $iconBar(curIcon) $tmpValue]
    } {
      set iconBar(bar,$iconBarName) ""
      lappend iconBar(bar,$iconBarName) $tmpValue
    }
  } {
    if {"$iconBarType" == "space"} {
      set tmpValue "Iconbar-space"
      .iconBarEdit.frame1.frame2.frame6.bar insert $iconBar(curIcon) $tmpValue
      lappend tmpValue { }
      if {[info exists iconBar(bar,$iconBarName)]} {
        set iconBar(bar,$iconBarName) \
          [linsert $iconBar(bar,$iconBarName) $iconBar(curIcon) $tmpValue]
      } {
        set iconBar(bar,$iconBarName) ""
        lappend iconBar(bar,$iconBarName) $tmpValue
      }
    } {
      set tmpValue [file tail [lindex [.iconBarEdit.frame1.frame4.bitmap.bitmap config -bitmap] 4]]
      .iconBarEdit.frame1.frame2.frame6.bar insert $iconBar(curIcon) $tmpValue
      lappend tmpValue [.iconBarEdit.frame1.frame4.command.command get 1.0 end]
      if {[info exists iconBar(bar,$iconBarName)]} {
        set iconBar(bar,$iconBarName) \
          [linsert $iconBar(bar,$iconBarName) $iconBar(curIcon) $tmpValue]
      } {
        set iconBar(bar,$iconBarName) ""
        lappend iconBar(bar,$iconBarName) $tmpValue
      }
    }
  }
  .iconBarEdit.frame1.frame2.frame6.mover config \
    -to [llength $iconBar(bar,$iconBarName)]
  incr iconBar(curIcon)
}

##########
# Procedure: IconBarModify
# Description: modify a icon button
# Arguments: iconBarName - the iconbar we configure
# Returns: none
# Sideeffects: none
##########
proc IconBarModify {iconBarName} {# xf ignore me 6
  global iconBar

  set tmpValue [file tail [lindex [.iconBarEdit.frame1.frame4.bitmap.bitmap config -bitmap] 4]]
  if {"[lindex [lindex $iconBar(bar,$iconBarName) $iconBar(curIcon)] 0]" != "Iconbar-separator" &&
      "[lindex [lindex $iconBar(bar,$iconBarName) $iconBar(curIcon)] 0]" != "Iconbar-space"} {
    .iconBarEdit.frame1.frame2.frame6.bar delete $iconBar(curIcon)
    .iconBarEdit.frame1.frame2.frame6.bar insert $iconBar(curIcon) $tmpValue
    lappend tmpValue [.iconBarEdit.frame1.frame4.command.command get 1.0 end]
    if {[info exists iconBar(bar,$iconBarName)]} {
      set iconBar(bar,$iconBarName) \
        [lreplace $iconBar(bar,$iconBarName) $iconBar(curIcon) $iconBar(curIcon) $tmpValue]
    } {
      set iconBar(bar,$iconBarName) $tmpValue
    }
  }
}

##########
# Procedure: IconBarReadIcons
# Description: read the icons
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc IconBarReadIcons {} {# xf ignore me 6
  global iconBar

  foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
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
      .iconBarEdit.frame1.frame2.frame4.icons insert end $tmpIcon
    }
  }
}

##########
# Procedure: IconBarReadIconBar
# Description: read the icon bar
# Arguments: iconBarName - the name of the current iconbar
# Returns: none
# Sideeffects: none
##########
proc IconBarReadIconBar {iconBarName} {# xf ignore me 6
  global iconBar

  if {[info exists iconBar(bar,$iconBarName)]} {
    foreach tmpIcon $iconBar(bar,$iconBarName) {
      .iconBarEdit.frame1.frame2.frame6.bar insert end [lindex $tmpIcon 0]
    }
  }
  if {[.iconBarEdit.frame1.frame2.frame6.bar size] > 0} {
    .iconBarEdit.frame1.frame2.frame6.mover config \
      -to [llength $iconBar(bar,$iconBarName)]
  }
}

##########
# Procedure: IconBarReadProcs
# Description: read the procedures
# Arguments: iconBarProcs
# Returns: none
# Sideeffects: none
##########
proc IconBarReadProcs {iconBarProcs} {# xf ignore me 6

  foreach tmpProcs [lsort $iconBarProcs] {
    .iconBarEdit.frame1.frame2.frame5.procs insert end $tmpProcs
  }
}

##########
# Procedure: IconBarReposition
# Description: reposition a menu item
# Arguments: iconBarName - the iconbar we configure
#            iconBarPos - the new position of the item
# Returns: none
# Sideeffects: none
##########
proc IconBarReposition {iconBarName iconBarPos} {# xf ignore me 6
  global iconBar

  if {[.iconBarEdit.frame1.frame2.frame6.bar size] > 0} {
    if {$iconBarPos < [llength $iconBar(bar,$iconBarName)]} {
      .iconBarEdit.frame1.frame2.frame6.bar delete $iconBar(curIcon)
      if {[info exists iconBar(bar,$iconBarName)]} {
       set tmpSaveValue [lindex $iconBar(bar,$iconBarName) $iconBar(curIcon)]
         set iconBar(bar,$iconBarName) \
          [lreplace $iconBar(bar,$iconBarName) $iconBar(curIcon) $iconBar(curIcon)]
      }
      set iconBar(curIcon) $iconBarPos
      .iconBarEdit.frame1.frame2.frame6.bar insert $iconBar(curIcon) \
        [file tail [lindex $tmpSaveValue 0]]
      if {[info exists iconBar(bar,$iconBarName)]} {
        set iconBar(bar,$iconBarName) \
          [linsert $iconBar(bar,$iconBarName) $iconBar(curIcon) \
            $tmpSaveValue]
      } {
        set iconBar(bar,$iconBarName) ""
        lappend iconBar(bar,$iconBarName) $tmpSaveValue
      }
    }
    .iconBarEdit.frame1.frame2.frame6.bar select clear 0 end
    .iconBarEdit.frame1.frame2.frame6.bar select set $iconBar(curIcon)
  }
}

##########
# Procedure: IconBarSave
# Description: save the current definition
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc IconBarSave {} {# xf ignore me 6
  global iconBar

  if {![catch "open $iconBar(userFile) w" outFile]} {
    puts $outFile "# icon bar configuration"
    puts $outFile "global iconBar"
    foreach counter [lsort [array names iconBar]]) {
      if {[string match "bar,*" $counter]} {
        puts $outFile "# icon bar: [string range $counter 4 end]"
        puts $outFile "set iconBar($counter) \{[set iconBar($counter)]\}"
      }
    }
    puts $outFile "# eof"
    close $outFile
  } {
    puts stderr $outFile
  }
}

##########
# Procedure: IconBarSelect
# Description: select a specified icon/proc/iconbutton
# Arguments: iconBarW - the listbox
#            iconBarName - the iconbar we configure
#            iconBarY - the y coordinate in listbox
# Returns: none
# Sideeffects: none
##########
proc IconBarSelect {iconBarW iconBarName iconBarY} {# xf ignore me 6
  global iconBar

  .iconBarEdit.frame1.frame2.frame4.icons select clear
  .iconBarEdit.frame1.frame2.frame5.procs select clear
  .iconBarEdit.frame1.frame2.frame6.bar select clear

  set iconBarNearest [$iconBarW nearest $iconBarY]
  if {$iconBarNearest >= 0} {
    $iconBarW select clear 0 end
    $iconBarW select set $iconBarNearest
    set selectedValue [$iconBarW get $iconBarNearest]
    if {"$iconBarW" == ".iconBarEdit.frame1.frame2.frame4.icons"} {
      if {"$selectedValue" != "Iconbar-separator" &&
          "$selectedValue" != "Iconbar-space"} {
        foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
          if {[file exists $tmpIconPath] && [file readable $tmpIconPath] &&
              [file isdirectory $tmpIconPath] &&
              [file exists $tmpIconPath/$selectedValue]} {
            .iconBarEdit.frame1.frame4.bitmap.bitmap config \
              -bitmap @$tmpIconPath/$selectedValue
            break
          }
        }
      } {
        .iconBarEdit.frame1.frame4.bitmap.bitmap config \
          -bitmap gray50
      }
    } {
      if {"$iconBarW" == ".iconBarEdit.frame1.frame2.frame5.procs"} {
        .iconBarEdit.frame1.frame4.command.command delete 1.0 end
        .iconBarEdit.frame1.frame4.command.command insert 1.0 $selectedValue
      } {
        set iconBar(curIcon) $iconBarNearest
        if {"$selectedValue" != "Iconbar-separator" &&
            "$selectedValue" != "Iconbar-space"} {
          foreach tmpIconPath [split $iconBar(icons) $iconBar(separator)] {
            if {[file exists $tmpIconPath] && [file readable $tmpIconPath] &&
                [file isdirectory $tmpIconPath] &&
                [file exists $tmpIconPath/$selectedValue]} {
              .iconBarEdit.frame1.frame4.bitmap.bitmap config \
                -bitmap @$tmpIconPath/$selectedValue
              break
            }
          }
        } {
          .iconBarEdit.frame1.frame4.bitmap.bitmap config \
            -bitmap gray50
        }
        .iconBarEdit.frame1.frame4.command.command delete 1.0 end
        .iconBarEdit.frame1.frame4.command.command insert 1.0 [lindex [lindex $iconBar(bar,$iconBarName) $iconBarNearest] 1]
      }
    }
  }
}

# eof

