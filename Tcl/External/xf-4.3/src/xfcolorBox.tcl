# XFNoParsing
# Program: template
# Description: select colors
#
# The HSV <-> RGB converting routines are from the
# tcolor demo that is part of the demo site of Tk.
#
# $Header: xfcolorBox.tcl[2.5] Wed Mar 10 13:18:48 1993 garfield@garfield frozen $

global xfColorBox
set xfColorBox(activeBackground) ""
set xfColorBox(activeForeground) ""
set xfColorBox(background) ""
set xfColorBox(font) ""
set xfColorBox(foreground) ""
set xfColorBox(palette) {white black gray50 blue red green yellow orange}
set xfColorBox(scrollActiveForeground) ""
set xfColorBox(scrollBackground) ""
set xfColorBox(scrollForeground) ""
set xfColorBox(colorName) ""
set xfColorBox(type) rgb
set xfColorBox(paletteNr) 0

proc XFColorBox {{xfColorBoxFileColor "/usr/local/lib/xf/lib/Colors"} {xfColorBoxMessage "Color"} {xfColorBoxEntryW ""} {xfColorBoxTargetW ""}} {# xf ignore me 5
##########
# Procedure: XFColorBox
# Description: select a color
# Arguments: {xfColorBoxFileColor} - the color file with all colornames
#            {xfColorBoxMessage} - a message to display
#            {xfColorBoxEntryW} - the widget name for the resulting color name
#            {xfColorBoxTargetW} - the widget we configure
# Returns: colorname, or nothing
# Sideeffects: none
##########
# 
# global xfColorBox(activeBackground) - active background color
# global xfColorBox(activeForeground) - active foreground color
# global xfColorBox(background) - background color
# global xfColorBox(font) - text font
# global xfColorBox(foreground) - foreground color
# global xfColorBox(palette) - a palette of colors
# global xfColorBox(scrollActiveForeground) - scrollbar active background color
# global xfColorBox(scrollBackground) - scrollbar background color
# global xfColorBox(scrollForeground) - scrollbar foreground color
# global xfColorBox(scrollSide) - side where scrollbar is located

  global xfColorBox

  set xfColorBox(colorName) ""
  set xfColorBox(paletteNr) 0

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$xfColorBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfColorBox(activeBackground)\" "
  }
  if {"$xfColorBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfColorBox(activeForeground)\" "
  }
  if {"$xfColorBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfColorBox(background)\" "
    append tmpFrameOpt "-background \"$xfColorBox(background)\" "
    append tmpMessageOpt "-background \"$xfColorBox(background)\" "
    append tmpScaleOpt "-background \"$xfColorBox(background)\" "
  }
  if {"$xfColorBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfColorBox(font)\" "
    append tmpMessageOpt "-font \"$xfColorBox(font)\" "
  }
  if {"$xfColorBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfColorBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfColorBox(foreground)\" "
    append tmpScaleOpt "-foreground \"$xfColorBox(foreground)\" "
  }
  if {"$xfColorBox(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$xfColorBox(scrollActiveForeground)\" "
    append tmpScrollOpt "-activeforeground \"$xfColorBox(scrollActiveForeground)\" "
  }
  if {"$xfColorBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfColorBox(scrollBackground)\" "
  }
  if {"$xfColorBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfColorBox(scrollForeground)\" "
  }

  # get color file name
  if {!([file exists $xfColorBoxFileColor] &&
        [file readable $xfColorBoxFileColor])} {
    set xfColorBoxFileColor ""
  }
  if {"$xfColorBoxFileColor" == ""} {
    global env
    if {[info exists env(XF_COLOR_FILE)]} {
      if {[file exists $env(XF_COLOR_FILE)] &&
          [file readable $env(XF_COLOR_FILE)]} {
        set xfColorBoxFileColor $env(XF_COLOR_FILE)
      }
    }
  }
  if {"$xfColorBoxMessage" == ""} {
    set xfColorBoxMessage "Color"
  }

  # save the the current widget color
  if {"$xfColorBoxTargetW" != ""} {
    if {[catch "$xfColorBoxTargetW config -[string tolower $xfColorBoxMessage]" result]} {
      set xfColorBoxSavedColor ""
    } {
      set xfColorBoxSavedColor [lindex $result 4]
    }
  } {
    set xfColorBoxSavedColor ""
  }

  # look if there is already a color window
  if {"[info commands .xfColorBox]" == ""} {
    # build widget structure

    XFTmpltToplevel .xfColorBox 400x300 {XF color select}

    set xfColorBox(oldWidget) $xfColorBoxEntryW

    frame .xfColorBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".xfColorBox.frame1 config $tmpFrameOpt"
 
    button .xfColorBox.frame1.ok \
      -text "OK"
    catch ".xfColorBox.frame1.ok config $tmpButtonOpt"

    button .xfColorBox.frame1.cancel \
      -text "Cancel"
    catch ".xfColorBox.frame1.cancel config $tmpButtonOpt"

    frame .xfColorBox.frame2 \
      -borderwidth 0 \
      -relief raised
    catch ".xfColorBox.frame2 config $tmpFrameOpt"
 
    radiobutton .xfColorBox.frame2.rgb \
      -value rgb \
      -command "XFColorBoxShowSlides $xfColorBoxMessage \"$xfColorBoxTargetW\"" \
      -text "RGB" \
      -variable xfColorBox(type)
    catch ".xfColorBox.frame2.rgb config $tmpButtonOpt"

    radiobutton .xfColorBox.frame2.hsv \
      -value hsv \
      -command "XFColorBoxShowSlides $xfColorBoxMessage \"$xfColorBoxTargetW\"" \
      -text "HSV" \
      -variable xfColorBox(type)
    catch ".xfColorBox.frame2.hsv config $tmpButtonOpt"

    radiobutton .xfColorBox.frame2.list \
      -value list \
      -command "XFColorBoxShowSlides $xfColorBoxMessage \"$xfColorBoxTargetW\"" \
      -text "List" \
      -variable xfColorBox(type)
    catch ".xfColorBox.frame2.list config $tmpButtonOpt"

    frame .xfColorBox.palette \
      -borderwidth 0 \
      -relief raised
    catch ".xfColorBox.palette config $tmpFrameOpt"
 
    set counter 0
    foreach element $xfColorBox(palette) {
      button .xfColorBox.palette.palette$counter \
        -command "XFColorBoxSetPalette $xfColorBoxMessage \"$xfColorBoxTargetW\" $counter" \
               -width 3
      catch ".xfColorBox.palette.palette$counter config \
        -activebackground \"$element\" \
        -background \"$element\""

      pack append .xfColorBox.palette .xfColorBox.palette.palette$counter {left fill expand}
      incr counter
    }

    scale .xfColorBox.red \
      -background "red" \
      -from 0 \
      -label "Red" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 255 \
      -width 8
    catch ".xfColorBox.red config $tmpScaleOpt"

    scale .xfColorBox.green \
      -background "green" \
      -from 0 \
      -label "Green" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 255 \
      -width 8
    catch ".xfColorBox.green config $tmpScaleOpt"

    scale .xfColorBox.blue \
      -background "blue" \
      -from 0 \
      -label "Blue" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 255 \
      -width 8
    catch ".xfColorBox.blue config $tmpScaleOpt"

    scale .xfColorBox.h \
      -from 0 \
      -label "Hue" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 1000 \
      -width 8
    catch ".xfColorBox.h config $tmpScaleOpt"

   scale .xfColorBox.s \
     -from 0 \
     -label "Saturation * 100" \
     -orient horizontal \
     -relief raised \
     -sliderlength 15 \
     -to 1000 \
     -width 8
    catch ".xfColorBox.s config $tmpScaleOpt"

    scale .xfColorBox.v \
      -from 0 \
      -label "Value" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 1000 \
      -width 8
    catch ".xfColorBox.v config $tmpScaleOpt"

    label .xfColorBox.demo \
      -relief raised \
      -text "This text shows the results :-)"
    catch ".xfColorBox.demo config $tmpMessageOpt"

    frame .xfColorBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".xfColorBox.current config $tmpFrameOpt"

    label .xfColorBox.current.labelcurrent \
      -relief raised
    catch ".xfColorBox.current.labelcurrent config $tmpMessageOpt"

    entry .xfColorBox.current.current \
      -relief raised
    catch ".xfColorBox.current.current config $tmpMessageOpt"

    frame .xfColorBox.colors \
      -borderwidth 0 \
      -relief raised
    catch ".xfColorBox.colors config $tmpFrameOpt"

    scrollbar .xfColorBox.colors.vscroll \
      -relief raised \
      -command ".xfColorBox.colors.colors yview"
    catch ".xfColorBox.colors.vscroll config $tmpScrollOpt"

    scrollbar .xfColorBox.colors.hscroll \
      -orient horiz \
      -relief raised \
      -command ".xfColorBox.colors.colors xview"
    catch ".xfColorBox.colors.hscroll config $tmpScrollOpt"

    listbox .xfColorBox.colors.colors \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".xfColorBox.colors.hscroll set" \
      -yscrollcommand ".xfColorBox.colors.vscroll set"
    catch ".xfColorBox.colors.colors config $tmpMessageOpt"

    # read color file
    if {"$xfColorBoxFileColor" != ""} {
      if {[catch "open $xfColorBoxFileColor r" colorInFile]} {
        set xfColorBoxFileColor ""
        if {"[info commands XFAlertBox]" != ""} {
          XFAlertBox "$colorInFile"
        } {
          puts stderr "$colorInFile"
        }
      } {
        set colorReadList [read $colorInFile]
        close $colorInFile
        foreach colorLine [split $colorReadList "\n"] {
          if {"[string trim $colorLine]" != "" && ![regexp "!" $colorLine]} {
            set colorNewLine [lrange $colorLine 3 end]
            append colorNewLine " " [format #%02x [lindex $colorLine 0]]
            append colorNewLine [format %02x [lindex $colorLine 1]]
            append colorNewLine [format %02x [lindex $colorLine 2]]
            .xfColorBox.colors.colors insert end $colorNewLine
          }
        }
      }
    }

    # bindings
    bind .xfColorBox.colors.colors <ButtonPress-1> "
      XFColorBoxSelectColor %W $xfColorBoxMessage \"$xfColorBoxTargetW\" %y"
    bind .xfColorBox.colors.colors <Button1-Motion> "
      XFColorBoxSelectColor %W $xfColorBoxMessage \"$xfColorBoxTargetW\" %y"
    bind .xfColorBox.colors.colors <Shift-ButtonPress-1> "
      XFColorBoxSelectColor %W $xfColorBoxMessage \"$xfColorBoxTargetW\" %y"
    bind .xfColorBox.colors.colors <Shift-Button1-Motion> "
      XFColorBoxSelectColor %W $xfColorBoxMessage \"$xfColorBoxTargetW\" %y"
  } {
    if {"[winfo class $xfColorBox(oldWidget)]" == "Text"} {
      catch "$xfColorBox(oldWidget) delete 1.0 end"
      catch "$xfColorBox(oldWidget) insert 1.0 [.xfColorBox.current.current get]"
    } {
      if {"[winfo class $xfColorBox(oldWidget)]" == "Entry"} {
        catch "$xfColorBox(oldWidget) delete 0 end"
        catch "$xfColorBox(oldWidget) insert 0 [.xfColorBox.current.current get]"
      }
    }

    set xfColorBox(oldWidget) $xfColorBoxEntryW
  }
   
  .xfColorBox.frame1.ok config \
    -command "
      global xfColorBox
      set xfColorBox(colorName) \[.xfColorBox.current.current get\]
      if {\"$xfColorBoxEntryW\" != \"\"} {
        if {\"\[winfo class $xfColorBoxEntryW\]\" == \"Text\"} {
          catch \"$xfColorBoxEntryW delete 1.0 end\"
          catch \"$xfColorBoxEntryW insert 1.0 \\\"\$xfColorBox(colorName)\\\"\"
        } {
          if {\"\[winfo class $xfColorBoxEntryW\]\" == \"Entry\"} {
            catch \"$xfColorBoxEntryW delete 0 end\"
            catch \"$xfColorBoxEntryW insert 0 \\\"\$xfColorBox(colorName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfColorBox}
      } {
        catch {destroy .xfColorBox}
      }"

  .xfColorBox.frame1.cancel config \
    -command "
      global xfColorBox
      set xfColorBox(colorName) {}
      if {\"$xfColorBoxTargetW\" != \"\"} {
        catch \"$xfColorBoxTargetW config -\[string tolower $xfColorBoxMessage\] $xfColorBoxSavedColor\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfColorBox}
      } {
        catch {destroy .xfColorBox}
      }"

  .xfColorBox.red config \
    -command "XFColorBoxSetRGBColor $xfColorBoxMessage \"$xfColorBoxTargetW\""

  .xfColorBox.green config \
    -command "XFColorBoxSetRGBColor $xfColorBoxMessage \"$xfColorBoxTargetW\""

  .xfColorBox.blue config \
    -command "XFColorBoxSetRGBColor $xfColorBoxMessage \"$xfColorBoxTargetW\""

  .xfColorBox.h config \
    -command "XFColorBoxSetHSVColor $xfColorBoxMessage \"$xfColorBoxTargetW\""

  .xfColorBox.s config \
    -command "XFColorBoxSetHSVColor $xfColorBoxMessage \"$xfColorBoxTargetW\""

  .xfColorBox.v config \
    -command "XFColorBoxSetHSVColor $xfColorBoxMessage \"$xfColorBoxTargetW\""

  .xfColorBox.current.labelcurrent config \
    -text "$xfColorBoxMessage:"

  # bindings
  bind .xfColorBox.current.current <Return> "
    XFColorBoxSetPaletteList \[.xfColorBox.current.current get\]
    XFColorBoxSetColor $xfColorBoxMessage \"$xfColorBoxTargetW\" text \[.xfColorBox.current.current get\]"

  bind .xfColorBox.colors.colors <Double-1> "
    XFColorBoxSelectColor %W $xfColorBoxMessage \"$xfColorBoxTargetW\" %y
    global xfColorBox
    set xfColorBox(colorName) \[.xfColorBox.current.current get\]
    if {\"$xfColorBoxEntryW\" != \"\"} {
      if {\"\[winfo class $xfColorBoxEntryW\]\" == \"Text\"} {
        catch \"$xfColorBoxEntryW delete 1.0 end\"
        catch \"$xfColorBoxEntryW insert 1.0 \\\"\$xfColorBox(colorName)\\\"\"
      } {
        if {\"\[winfo class $xfColorBoxEntryW\]\" == \"Entry\"} {
          catch \"$xfColorBoxEntryW delete 0 end\"
          catch \"$xfColorBoxEntryW insert 0 \\\"\$xfColorBox(colorName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .xfColorBox}
    } {
      catch {destroy .xfColorBox}
    }"

  # set up current value
  .xfColorBox.current.current delete 0 end
  if {"$xfColorBoxEntryW" != ""} {
    if {"[winfo class $xfColorBoxEntryW]" == "Text"} {
      .xfColorBox.current.current insert 0 [$xfColorBoxEntryW get 1.0 end]
    } {
      if {"[winfo class $xfColorBoxEntryW]" == "Entry"} {
        .xfColorBox.current.current insert 0 [$xfColorBoxEntryW get]
      }
    }
  }
  if {"[.xfColorBox.current.current get]" != ""} {
    XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW text [.xfColorBox.current.current get]
  }
    
  # packing
  pack append .xfColorBox.frame1 \
              .xfColorBox.frame1.ok {left fill expand} \
              .xfColorBox.frame1.cancel {left fill expand}
  pack append .xfColorBox.frame2 \
              .xfColorBox.frame2.rgb {left fill expand} \
              .xfColorBox.frame2.hsv {left fill expand} \
              .xfColorBox.frame2.list {left fill expand}
  pack append .xfColorBox.current \
              .xfColorBox.current.labelcurrent {left} \
              .xfColorBox.current.current {left fill expand}
  pack append .xfColorBox.colors \
              .xfColorBox.colors.vscroll "$xfColorBox(scrollSide) filly" \
              .xfColorBox.colors.hscroll {bottom fillx} \
              .xfColorBox.colors.colors {left fill expand}

  XFColorBoxShowSlides $xfColorBoxMessage $xfColorBoxTargetW

  catch "wm deiconify .xfColorBox"

  if {"$xfColorBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .xfColorBox
    tkwait window .xfColorBox

    return $xfColorBox(colorName)
  }
}

##########
# Procedure: XFColorBoxSelectColor
# Description: select color for color composing
# Arguments: colorW - the widget
#            xfColorBoxMessage - the message for the color
#            xfColorBoxTargetW - the widget we configure
#            colorY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxSelectColor {colorW xfColorBoxMessage xfColorBoxTargetW colorY} {# xf ignore me 6

  set colorNearest [$colorW nearest $colorY]
  if {$colorNearest >= 0} {
    $colorW select anchor $colorNearest
    $colorW select set $colorNearest
    set colorTmpValue [$colorW get $colorNearest]
    set colorCurrentColor [lrange $colorTmpValue 0 \
          [expr [llength $colorTmpValue]-2]]
    set colorCurrentValue [lrange $colorTmpValue \
          [expr [llength $colorTmpValue]-1] end]

    scan [string range $colorCurrentValue 1 2] "%x" xfColorBoxValue
    .xfColorBox.red set $xfColorBoxValue
    scan [string range $colorCurrentValue 3 4] "%x" xfColorBoxValue
    .xfColorBox.green set $xfColorBoxValue
    scan [string range $colorCurrentValue 5 6] "%x" xfColorBoxValue
    .xfColorBox.blue set $xfColorBoxValue

    .xfColorBox.current.current delete 0 end
    .xfColorBox.current.current insert 0 $colorCurrentColor
    XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW list $colorCurrentColor
    XFColorBoxSetPaletteList $colorCurrentColor
  }
}

##########
# Procedure: XFColorBoxSetColor
# Description: set the new color
# Arguments: xfColorBoxMessage - the message for the color
#            xfColorBoxTargetW - the widget we configure
#            xfColorBoxType - who wants to set the demo area
#            xfColorBoxValue - the value to set
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxSetColor {xfColorBoxMessage xfColorBoxTargetW xfColorBoxType xfColorBoxValue} {# xf ignore me 6
  global xfColorBox

  .xfColorBox.red config \
    -command "NoFunction"
  .xfColorBox.green config \
    -command "NoFunction"
  .xfColorBox.blue config \
    -command "NoFunction"
  .xfColorBox.h config \
    -command "NoFunction"
  .xfColorBox.s config \
    -command "NoFunction"
  .xfColorBox.v config \
    -command "NoFunction"

  set xfColorBoxSetColor ""
  if {"$xfColorBoxValue" != ""} {
    if {"$xfColorBoxType" != "text"} {
      .xfColorBox.current.current delete 0 end
      .xfColorBox.current.current insert 0 $xfColorBoxValue
    }
    if {[string match "*oreground*" $xfColorBoxMessage]} {
      catch ".xfColorBox.demo config -foreground $xfColorBoxValue"
    } {
      catch ".xfColorBox.demo config -background $xfColorBoxValue"
    }
    if {"$xfColorBoxTargetW" != ""} {
      catch "$xfColorBoxTargetW config -[string tolower $xfColorBoxMessage] \
        $xfColorBoxValue"
    }
  }
  case $xfColorBoxType in {
    {text palette} {
      if {[string match "*oreground*" $xfColorBoxMessage]} {
        set red [expr [lindex [winfo rgb .xfColorBox.demo [lindex [.xfColorBox.demo config -foreground] 4]] 0]/256]
        set green [expr [lindex [winfo rgb .xfColorBox.demo [lindex [.xfColorBox.demo config -foreground] 4]] 1]/256]
        set blue [expr [lindex [winfo rgb .xfColorBox.demo [lindex [.xfColorBox.demo config -foreground] 4]] 2]/256]
      } {
        set red [expr [lindex [winfo rgb .xfColorBox.demo [lindex [.xfColorBox.demo config -background] 4]] 0]/256]
        set green [expr [lindex [winfo rgb .xfColorBox.demo [lindex [.xfColorBox.demo config -background] 4]] 1]/256]
        set blue [expr [lindex [winfo rgb .xfColorBox.demo [lindex [.xfColorBox.demo config -background] 4]] 2]/256]
      }
      if {"$xfColorBox(type)" == "rgb"} {
        .xfColorBox.red set $red
        .xfColorBox.green set $green
        .xfColorBox.blue set $blue
      } {
        if {"$xfColorBox(type)" == "hsv"} {
          set xfColorBoxHSV [XFColorBoxRGBToHSV [expr $red*256] [expr $green*256] [expr $blue*256]]
          .xfColorBox.h set [format %.0f [expr [lindex $xfColorBoxHSV 0]*1000.0]]
          .xfColorBox.s set [format %.0f [expr [lindex $xfColorBoxHSV 1]*1000.0]]
          .xfColorBox.v set [format %.0f [expr [lindex $xfColorBoxHSV 2]*1000.0]]
        }
      }
    }
  }
  .xfColorBox.red config \
    -command "XFColorBoxSetRGBColor $xfColorBoxMessage \"$xfColorBoxTargetW\""
  .xfColorBox.green config \
    -command "XFColorBoxSetRGBColor $xfColorBoxMessage \"$xfColorBoxTargetW\""
  .xfColorBox.blue config \
    -command "XFColorBoxSetRGBColor $xfColorBoxMessage \"$xfColorBoxTargetW\""
  .xfColorBox.h config \
    -command "XFColorBoxSetHSVColor $xfColorBoxMessage \"$xfColorBoxTargetW\""
  .xfColorBox.s config \
    -command "XFColorBoxSetHSVColor $xfColorBoxMessage \"$xfColorBoxTargetW\""
  .xfColorBox.v config \
    -command "XFColorBoxSetHSVColor $xfColorBoxMessage \"$xfColorBoxTargetW\""
}

##########
# Procedure: XFColorBoxSetRGBColor
# Description: set the color as RGB value
# Arguments: xfColorBoxMessage - the message for the color
#            xfColorBoxTargetW - the widget we configure
#            xfColorBoxValue - the passed value from scale
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxSetRGBColor {xfColorBoxMessage xfColorBoxTargetW xfColorBoxValue} {# xf ignore me 6
  global xfColorBox

  XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW rgb \
    [format #%02x%02x%02x [.xfColorBox.red get] \
      [.xfColorBox.green get] [.xfColorBox.blue get]]
  XFColorBoxSetPaletteList [format #%02x%02x%02x [.xfColorBox.red get] \
    [.xfColorBox.green get] [.xfColorBox.blue get]]
}

##########
# Procedure: XFColorBoxSetHSVColor
# Description: set the color as HSV value
# Arguments: xfColorBoxMessage - the message for the color
#            xfColorBoxTargetW - the widget we configure
#            xfColorBoxValue - the passed value from scale
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxSetHSVColor {xfColorBoxMessage xfColorBoxTargetW xfColorBoxValue} {# xf ignore me 6
  global xfColorBox

  set xfColorBoxRGB [XFColorBoxHSVToRGB [expr [.xfColorBox.h get]/1000.0] [expr [.xfColorBox.s get]/1000.0] [expr [.xfColorBox.v get]/1000.0]]
  XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW hsv \
    [format #%04x%04x%04x [lindex $xfColorBoxRGB 0] [lindex $xfColorBoxRGB 1] [lindex $xfColorBoxRGB 2]]
  XFColorBoxSetPaletteList [format #%04x%04x%04x [lindex $xfColorBoxRGB 0] [lindex $xfColorBoxRGB 1] [lindex $xfColorBoxRGB 2]]
}

##########
# Procedure: XFColorBoxSetPalette
# Description: set the palette color
# Arguments: xfColorBoxMessage - the message for the color
#            xfColorBoxTargetW - the widget we configure
#            xfColorBoxElement - the palette element
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxSetPalette {xfColorBoxMessage xfColorBoxTargetW xfColorBoxElement} {# xf ignore me 6
  global xfColorBox

  set xfColorBox(paletteNr) $xfColorBoxElement
  XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW palette \
    [lindex [.xfColorBox.palette.palette$xfColorBoxElement config -background] 4]
}

##########
# Procedure: XFColorBoxSetPaletteList
# Description: set the palette color list
# Arguments: xfColorBoxValue - the new palette value
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxSetPaletteList {xfColorBoxValue} {# xf ignore me 6
  global xfColorBox

  catch ".xfColorBox.palette.palette$xfColorBox(paletteNr) config \
      -activebackground $xfColorBoxValue"
  catch ".xfColorBox.palette.palette$xfColorBox(paletteNr) config \
      -background $xfColorBoxValue"
  set xfColorBox(palette) \
    [lreplace $xfColorBox(palette) $xfColorBox(paletteNr) $xfColorBox(paletteNr) \
      $xfColorBoxValue]
}

##########
# Procedure: XFColorBoxShowSlides
# Description: select color for color composing
# Arguments: xfColorBoxMessage - the message for the color
#            xfColorBoxTargetW - the widget we configure
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxShowSlides {xfColorBoxMessage xfColorBoxTargetW} {# xf ignore me 6
  global xfColorBox

  catch "pack unpack .xfColorBox.frame1"
  catch "pack unpack .xfColorBox.frame2"
  catch "pack unpack .xfColorBox.current"
  catch "pack unpack .xfColorBox.demo"
  catch "pack unpack .xfColorBox.h"
  catch "pack unpack .xfColorBox.s"
  catch "pack unpack .xfColorBox.v"
  catch "pack unpack .xfColorBox.red"
  catch "pack unpack .xfColorBox.green"
  catch "pack unpack .xfColorBox.blue"
  catch "pack unpack .xfColorBox.colors"
  case $xfColorBox(type) in {
    {rgb} {
      pack append .xfColorBox \
                  .xfColorBox.frame1 {bottom fillx} \
                  .xfColorBox.frame2 {bottom fillx} \
                  .xfColorBox.current {bottom fillx} \
                  .xfColorBox.palette {bottom fillx} \
                  .xfColorBox.red {top fillx} \
                  .xfColorBox.green {top fillx} \
                  .xfColorBox.blue {top fillx} \
                  .xfColorBox.demo {bottom fill expand}
    }
    {hsv} {
      pack append .xfColorBox \
                  .xfColorBox.frame1 {bottom fillx} \
                  .xfColorBox.frame2 {bottom fillx} \
                  .xfColorBox.current {bottom fillx} \
                  .xfColorBox.palette {bottom fillx} \
                  .xfColorBox.h {top fillx} \
                  .xfColorBox.s {top fillx} \
                  .xfColorBox.v {top fillx} \
                  .xfColorBox.demo {bottom fill expand}
    }
    {list} {
      pack append .xfColorBox \
                  .xfColorBox.frame1 {bottom fillx} \
                  .xfColorBox.frame2 {bottom fillx} \
                  .xfColorBox.current {bottom fillx} \
                  .xfColorBox.palette {bottom fillx} \
                  .xfColorBox.demo {bottom fillx} \
                  .xfColorBox.colors {top fill expand}
    }
  }
  if {[string match "*oreground*" $xfColorBoxMessage]} {
    XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW text \
      [lindex [.xfColorBox.demo config -foreground] 4]
  } {
    XFColorBoxSetColor $xfColorBoxMessage $xfColorBoxTargetW text \
      [lindex [.xfColorBox.demo config -background] 4]
  }
}

##########
# Procedure: XFColorBoxHSVToRGB
# Description: modify hsv color values to rgb values
# Arguments: xfColorBoxHue - the hue
#            xfColorBoxSat - the saturation
#            xfColorBoxValue - the value
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxHSVToRGB {xfColorBoxHue xfColorBoxSat xfColorBoxValue} {# xf ignore me 6
# The HSV <-> RGB converting routines are from the
# tcolor demo that is part of the demo site of Tk.

  set xfColorBoxV [format %.0f [expr 65535.0*$xfColorBoxValue]]
  if {$xfColorBoxSat == 0} {
    return "$xfColorBoxV $xfColorBoxV $xfColorBoxV"
  } else {
    set xfColorBoxHue [expr $xfColorBoxHue*6.0]
    if {$xfColorBoxHue >= 6.0} {
      set xfColorBoxHue 0.0
    }
    scan $xfColorBoxHue. %d i
    set xfColorBoxF [expr $xfColorBoxHue-$i]
    set xfColorBoxP [format %.0f [expr {65535.0*$xfColorBoxValue*(1 - $xfColorBoxSat)}]]
    set xfColorBoxQ [format %.0f [expr {65535.0*$xfColorBoxValue*(1 - ($xfColorBoxSat*$xfColorBoxF))}]]
    set xfColorBoxT [format %.0f [expr {65535.0*$xfColorBoxValue*(1 - ($xfColorBoxSat*(1 - $xfColorBoxF)))}]]
    case $i \
      0 {return "$xfColorBoxV $xfColorBoxT $xfColorBoxP"} \
      1 {return "$xfColorBoxQ $xfColorBoxV $xfColorBoxP"} \
      2 {return "$xfColorBoxP $xfColorBoxV $xfColorBoxT"} \
      3 {return "$xfColorBoxP $xfColorBoxQ $xfColorBoxV"} \
      4 {return "$xfColorBoxT $xfColorBoxP $xfColorBoxV"} \
      5 {return "$xfColorBoxV $xfColorBoxP $xfColorBoxQ"}
    error "i value $i is out of range"
  }
}

##########
# Procedure: XFColorBoxRGBToHSV
# Description: modify rgb color values to hsv values
# Arguments: xfColorBoxRed - the red value
#            xfColorBoxGreen - the green value
#            xfColorBoxBlue - the blue value
# Returns: none
# Sideeffects: none
##########
proc XFColorBoxRGBToHSV {xfColorBoxRed xfColorBoxGreen xfColorBoxBlue} {# xf ignore me 6
# The HSV <-> RGB converting routines are from the
# tcolor demo that is part of the demo site of Tk.

  if {$xfColorBoxRed > $xfColorBoxGreen} {
    set xfColorBoxMax $xfColorBoxRed.0
    set xfColorBoxMin $xfColorBoxGreen.0
  } else {
    set xfColorBoxMax $xfColorBoxGreen.0
    set xfColorBoxMin $xfColorBoxRed.0
  }
  if {$xfColorBoxBlue > $xfColorBoxMax} {
    set xfColorBoxMax $xfColorBoxBlue.0
  } else {
    if {$xfColorBoxBlue < $xfColorBoxMin} {
      set xfColorBoxMin $xfColorBoxBlue.0
    }
  }
  set range [expr $xfColorBoxMax-$xfColorBoxMin]
  if {$xfColorBoxMax == 0} {
    set xfColorBoxSat 0
  } else {
    set xfColorBoxSat [expr {($xfColorBoxMax-$xfColorBoxMin)/$xfColorBoxMax}]
  }
  if {$xfColorBoxSat == 0} {
    set xfColorBoxHue 0
  } else {
    set xfColorBoxRC [expr {($xfColorBoxMax - $xfColorBoxRed)/$range}]
    set xfColorBoxGC [expr {($xfColorBoxMax - $xfColorBoxGreen)/$range}]
    set xfColorBoxBC [expr {($xfColorBoxMax - $xfColorBoxBlue)/$range}]
    if {$xfColorBoxRed == $xfColorBoxMax} {
      set xfColorBoxHue [expr {.166667*($xfColorBoxBC - $xfColorBoxGC)}]
    } else {
      if {$xfColorBoxGreen == $xfColorBoxMax} {
        set xfColorBoxHue [expr {.166667*(2 + $xfColorBoxRC - $xfColorBoxBC)}]
      } else {
        set xfColorBoxHue [expr {.166667*(4 + $xfColorBoxGC - $xfColorBoxRC)}]
      }
    }
  }
  return [list $xfColorBoxHue $xfColorBoxSat [expr {$xfColorBoxMax/65535}]]
}

proc NoFunction {args} {# xf ignore me 7
##########
# Procedure: NoFunction
# Description: do nothing
# Arguments: args - any kind of arguments
# Returns: none
# Sideeffects: none
##########

}

# eof

