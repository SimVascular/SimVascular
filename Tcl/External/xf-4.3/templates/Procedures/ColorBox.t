# XFNoParsing
# Program: template
# Description: select colors
#
# The HSV <-> RGB converting routines are from the
# tcolor demo that is part of the demo site of Tk.
#
# $Header: ColorBox.t[2.6] Wed Mar 10 13:25:55 1993 garfield@garfield frozen $

global colorBox
set colorBox(activeBackground) ""
set colorBox(activeForeground) ""
set colorBox(background) ""
set colorBox(font) ""
set colorBox(foreground) ""
set colorBox(palette) {white black gray50 blue red green yellow orange}
set colorBox(scrollActiveForeground) ""
set colorBox(scrollBackground) ""
set colorBox(scrollForeground) ""
set colorBox(scrollSide) left
set colorBox(colorName) ""
set colorBox(type) rgb
set colorBox(paletteNr) 0

proc ColorBox {{colorBoxFileColor "/usr/local/lib/xf/lib/Colors"} {colorBoxMessage "Color"} {colorBoxEntryW ""} {colorBoxTargetW ""}} {# xf ignore me 5
##########
# Procedure: ColorBox
# Description: select a color
# Arguments: {colorBoxFileColor} - the color file with all colornames
#            {colorBoxMessage} - a message to display
#            {colorBoxEntryW} - the widget name for the resulting color name
#            {colorBoxTargetW} - the widget we configure
# Returns: colorname, or nothing
# Sideeffects: none
##########
# 
# global colorBox(activeBackground) - active background color
# global colorBox(activeForeground) - active foreground color
# global colorBox(background) - background color
# global colorBox(font) - text font
# global colorBox(foreground) - foreground color
# global colorBox(palette) - a palette of colors
# global colorBox(scrollActiveForeground) - scrollbar active background color
# global colorBox(scrollBackground) - scrollbar background color
# global colorBox(scrollForeground) - scrollbar foreground color
# global colorBox(scrollSide) - side where scrollbar is located

  global colorBox

  set colorBox(colorName) ""
  set colorBox(paletteNr) 0

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$colorBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$colorBox(activeBackground)\" "
  }
  if {"$colorBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$colorBox(activeForeground)\" "
  }
  if {"$colorBox(background)" != ""} {
    append tmpButtonOpt "-background \"$colorBox(background)\" "
    append tmpFrameOpt "-background \"$colorBox(background)\" "
    append tmpMessageOpt "-background \"$colorBox(background)\" "
    append tmpScaleOpt "-background \"$colorBox(background)\" "
  }
  if {"$colorBox(font)" != ""} {
    append tmpButtonOpt "-font \"$colorBox(font)\" "
    append tmpMessageOpt "-font \"$colorBox(font)\" "
  }
  if {"$colorBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$colorBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$colorBox(foreground)\" "
    append tmpScaleOpt "-foreground \"$colorBox(foreground)\" "
  }
  if {"$colorBox(scrollActiveForeground)" != ""} {
    append tmpScaleOpt "-activeforeground \"$colorBox(scrollActiveForeground)\" "
    append tmpScrollOpt "-activeforeground \"$colorBox(scrollActiveForeground)\" "
  }
  if {"$colorBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$colorBox(scrollBackground)\" "
  }
  if {"$colorBox(scrollForeground)" != ""} {
    append tmpScaleOpt "-sliderforeground \"$colorBox(scrollForeground)\" "
    append tmpScrollOpt "-foreground \"$colorBox(scrollForeground)\" "
  }

  # get color file name
  if {!([file exists $colorBoxFileColor] &&
        [file readable $colorBoxFileColor])} {
    set colorBoxFileColor ""
  }
  if {"$colorBoxFileColor" == ""} {
    global env
    if {[info exists env(XF_COLOR_FILE)]} {
      if {[file exists $env(XF_COLOR_FILE)] &&
          [file readable $env(XF_COLOR_FILE)]} {
        set colorBoxFileColor $env(XF_COLOR_FILE)
      }
    }
  }
  if {"$colorBoxMessage" == ""} {
    set colorBoxMessage "Color"
  }

  # save the the current widget color
  if {"$colorBoxTargetW" != ""} {
    if {[catch "$colorBoxTargetW config -[string tolower $colorBoxMessage]" result]} {
      set colorBoxSavedColor ""
    } {
      set colorBoxSavedColor [lindex $result 4]
    }
  } {
    set colorBoxSavedColor ""
  }

  # look if there is already a color window
  if {"[info commands .colorBox]" == ""} {
    # build widget structure

    # start build of toplevel
    if {"[info commands XFDestroy]" != ""} {
      catch {XFDestroy .colorBox}
    } {
      catch {destroy .colorBox}
    }
    toplevel .colorBox \
      -borderwidth 0
    catch ".colorBox config $tmpFrameOpt"
    wm geometry .colorBox 400x300
    wm title .colorBox {Color box}
    wm maxsize .colorBox 1000 1000
    wm minsize .colorBox 100 100
    # end build of toplevel

    set colorBox(oldWidget) $colorBoxEntryW

    frame .colorBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".colorBox.frame1 config $tmpFrameOpt"
 
    button .colorBox.frame1.ok \
      -text "OK"
    catch ".colorBox.frame1.ok config $tmpButtonOpt"

    button .colorBox.frame1.cancel \
      -text "Cancel"
    catch ".colorBox.frame1.cancel config $tmpButtonOpt"

    frame .colorBox.frame2 \
      -borderwidth 0 \
      -relief raised
    catch ".colorBox.frame2 config $tmpFrameOpt"
 
    radiobutton .colorBox.frame2.rgb \
      -command "ColorBoxShowSlides $colorBoxMessage \"$colorBoxTargetW\"" \
      -text "RGB" \
      -value "rgb" \
      -variable colorBox(type)
    catch ".colorBox.frame2.rgb config $tmpButtonOpt"

    radiobutton .colorBox.frame2.hsv \
      -command "ColorBoxShowSlides $colorBoxMessage \"$colorBoxTargetW\"" \
      -text "HSV" \
      -value "hsv" \
      -variable colorBox(type)
    catch ".colorBox.frame2.hsv config $tmpButtonOpt"

    radiobutton .colorBox.frame2.list \
      -command "ColorBoxShowSlides $colorBoxMessage \"$colorBoxTargetW\"" \
      -text "List" \
      -value "list" \
      -variable colorBox(type)
    catch ".colorBox.frame2.list config $tmpButtonOpt"

    frame .colorBox.palette \
      -borderwidth 0 \
      -relief raised
    catch ".colorBox.palette config $tmpFrameOpt"
 
    set counter 0
    foreach element $colorBox(palette) {
      button .colorBox.palette.palette$counter \
        -command "ColorBoxSetPalette $colorBoxMessage \"$colorBoxTargetW\" $counter" \
               -width 3
      catch ".colorBox.palette.palette$counter config \
        -activebackground \"$element\" \
        -background \"$element\""

      pack append .colorBox.palette .colorBox.palette.palette$counter {left fill expand}
      incr counter
    }

    scale .colorBox.red \
      -background "red" \
      -from 0 \
      -label "Red" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 255 \
      -width 8
    catch ".colorBox.red config $tmpScaleOpt"

    scale .colorBox.green \
      -background "green" \
      -from 0 \
      -label "Green" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 255 \
      -width 8
    catch ".colorBox.green config $tmpScaleOpt"

    scale .colorBox.blue \
      -background "blue" \
      -from 0 \
      -label "Blue" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 255 \
      -width 8
    catch ".colorBox.blue config $tmpScaleOpt"

    scale .colorBox.h \
      -from 0 \
      -label "Hue" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 1000 \
      -width 8
    catch ".colorBox.h config $tmpScaleOpt"

   scale .colorBox.s \
     -from 0 \
     -label "Saturation * 100" \
     -orient horizontal \
     -relief raised \
     -sliderlength 15 \
     -to 1000 \
     -width 8
    catch ".colorBox.s config $tmpScaleOpt"

    scale .colorBox.v \
      -from 0 \
      -label "Value" \
      -orient horizontal \
      -relief raised \
      -sliderlength 15 \
      -to 1000 \
      -width 8
    catch ".colorBox.v config $tmpScaleOpt"

    label .colorBox.demo \
      -relief raised \
      -text "This text shows the results :-)"
    catch ".colorBox.demo config $tmpMessageOpt"

    frame .colorBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".colorBox.current config $tmpFrameOpt"

    label .colorBox.current.labelcurrent \
      -relief raised
    catch ".colorBox.current.labelcurrent config $tmpMessageOpt"

    entry .colorBox.current.current \
      -relief raised
    catch ".colorBox.current.current config $tmpMessageOpt"

    frame .colorBox.colors \
      -borderwidth 0 \
      -relief raised
    catch ".colorBox.colors config $tmpFrameOpt"

    scrollbar .colorBox.colors.vscroll \
      -relief raised \
      -command ".colorBox.colors.colors yview"
    catch ".colorBox.colors.vscroll config $tmpScrollOpt"

    scrollbar .colorBox.colors.hscroll \
      -orient horiz \
      -relief raised \
      -command ".colorBox.colors.colors xview"
    catch ".colorBox.colors.hscroll config $tmpScrollOpt"

    listbox .colorBox.colors.colors \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".colorBox.colors.hscroll set" \
      -yscrollcommand ".colorBox.colors.vscroll set"
    catch ".colorBox.colors.colors config $tmpMessageOpt"

    # read color file
    if {"$colorBoxFileColor" != ""} {
      if {[catch "open $colorBoxFileColor r" colorInFile]} {
        set colorBoxFileColor ""
        if {"[info commands AlertBox]" != ""} {
          AlertBox "$colorInFile"
        } {
          puts stderr "$colorInFile"
        }
      } {
        set colorReadList [read $colorInFile]
        close $colorInFile
        foreach colorLine [split $colorReadList "\n"] {
          if {("[string trim $colorLine]" != "") && ![regexp "!" $colorLine]} { 
            set colorNewLine [lrange $colorLine 3 end]
            append colorNewLine " " [format #%02x [lindex $colorLine 0]]
            append colorNewLine [format %02x [lindex $colorLine 1]]
            append colorNewLine [format %02x [lindex $colorLine 2]]
            .colorBox.colors.colors insert end $colorNewLine
          }
        }
      }
    }

    # bindings
    bind .colorBox.colors.colors <ButtonPress-1> "
      ColorBoxSelectColor %W $colorBoxMessage \"$colorBoxTargetW\" %y"
    bind .colorBox.colors.colors <Button1-Motion> "
      ColorBoxSelectColor %W $colorBoxMessage \"$colorBoxTargetW\" %y"
    bind .colorBox.colors.colors <Shift-ButtonPress-1> "
      ColorBoxSelectColor %W $colorBoxMessage \"$colorBoxTargetW\" %y"
    bind .colorBox.colors.colors <Shift-Button1-Motion> "
      ColorBoxSelectColor %W $colorBoxMessage \"$colorBoxTargetW\" %y"
  } {
    if {"[winfo class $colorBox(oldWidget)]" == "Text"} {
      catch "$colorBox(oldWidget) delete 1.0 end"
      catch "$colorBox(oldWidget) insert 1.0 [.colorBox.current.current get]"
    } {
      if {"[winfo class $colorBox(oldWidget)]" == "Entry"} {
        catch "$colorBox(oldWidget) delete 0 end"
        catch "$colorBox(oldWidget) insert 0 [.colorBox.current.current get]"
      }
    }

    set colorBox(oldWidget) $colorBoxEntryW
  }
   
  .colorBox.frame1.ok config \
    -command "
      global colorBox
      set colorBox(colorName) \[.colorBox.current.current get\]
      if {\"$colorBoxEntryW\" != \"\"} {
        if {\"\[winfo class $colorBoxEntryW\]\" == \"Text\"} {
          catch \"$colorBoxEntryW delete 1.0 end\"
          catch \"$colorBoxEntryW insert 1.0 \\\"\$colorBox(colorName)\\\"\"
        } {
          if {\"\[winfo class $colorBoxEntryW\]\" == \"Entry\"} {
            catch \"$colorBoxEntryW delete 0 end\"
            catch \"$colorBoxEntryW insert 0 \\\"\$colorBox(colorName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .colorBox}
      } {
        catch {destroy .colorBox}
      }"

  .colorBox.frame1.cancel config \
    -command "
      global colorBox
      set colorBox(colorName) {}
      if {\"$colorBoxTargetW\" != \"\"} {
        catch \"$colorBoxTargetW config -\[string tolower $colorBoxMessage\] $colorBoxSavedColor\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .colorBox}
      } {
        catch {destroy .colorBox}
      }"

  .colorBox.red config \
    -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.green config \
    -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.blue config \
    -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.h config \
    -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.s config \
    -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.v config \
    -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.current.labelcurrent config \
    -text "$colorBoxMessage:"

  # bindings
  bind .colorBox.current.current <Return> "
    ColorBoxSetPaletteList \[.colorBox.current.current get\]
    ColorBoxSetColor $colorBoxMessage \"$colorBoxTargetW\" text \[.colorBox.current.current get\]"

  bind .colorBox.colors.colors <Double-1> "
    ColorBoxSelectColor %W $colorBoxMessage \"$colorBoxTargetW\" %y
    global colorBox
    set colorBox(colorName) \[.colorBox.current.current get\]
    if {\"$colorBoxEntryW\" != \"\"} {
      if {\"\[winfo class $colorBoxEntryW\]\" == \"Text\"} {
        catch \"$colorBoxEntryW delete 1.0 end\"
        catch \"$colorBoxEntryW insert 1.0 \\\"\$colorBox(colorName)\\\"\"
      } {
        if {\"\[winfo class $colorBoxEntryW\]\" == \"Entry\"} {
          catch \"$colorBoxEntryW delete 0 end\"
          catch \"$colorBoxEntryW insert 0 \\\"\$colorBox(colorName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .colorBox}
    } {
      catch {destroy .colorBox}
    }"

  # set up current value
  .colorBox.current.current delete 0 end
  if {"$colorBoxEntryW" != ""} {
    if {"[winfo class $colorBoxEntryW]" == "Text"} {
      .colorBox.current.current insert 0 [$colorBoxEntryW get 1.0 end]
    } {
      if {"[winfo class $colorBoxEntryW]" == "Entry"} {
        .colorBox.current.current insert 0 [$colorBoxEntryW get]
      }
    }
  }
  if {"[.colorBox.current.current get]" != ""} {
    ColorBoxSetColor $colorBoxMessage $colorBoxTargetW text [.colorBox.current.current get]
  }
    
  # packing
  pack append .colorBox.frame1 \
              .colorBox.frame1.ok {left fill expand} \
              .colorBox.frame1.cancel {left fill expand}
  pack append .colorBox.frame2 \
              .colorBox.frame2.rgb {left fill expand} \
              .colorBox.frame2.hsv {left fill expand} \
              .colorBox.frame2.list {left fill expand}
  pack append .colorBox.current \
              .colorBox.current.labelcurrent {left} \
              .colorBox.current.current {left fill expand}
  pack append .colorBox.colors \
              .colorBox.colors.vscroll "$colorBox(scrollSide) filly" \
              .colorBox.colors.hscroll {bottom fillx} \
              .colorBox.colors.colors {left fill expand}

  ColorBoxShowSlides $colorBoxMessage $colorBoxTargetW

  catch "wm deiconify .colorBox"

  if {"$colorBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .colorBox
    tkwait window .colorBox

    return $colorBox(colorName)
  }
}

##########
# Procedure: ColorBoxSelectColor
# Description: select color for color composing
# Arguments: colorW - the widget
#            colorBoxMessage - the message for the color
#            colorBoxTargetW - the widget we configure
#            colorY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc ColorBoxSelectColor {colorW colorBoxMessage colorBoxTargetW colorY} {# xf ignore me 6

  set colorNearest [$colorW nearest $colorY]
  if {$colorNearest >= 0} {
    $colorW select clear 0 end
    $colorW select set $colorNearest
    set colorTmpValue [$colorW get $colorNearest]
    set colorCurrentColor [lrange $colorTmpValue 0 \
          [expr [llength $colorTmpValue]-2]]
    set colorCurrentValue [lrange $colorTmpValue \
          [expr [llength $colorTmpValue]-1] end]

    scan [string range $colorCurrentValue 1 2] "%x" colorBoxValue
    .colorBox.red set $colorBoxValue
    scan [string range $colorCurrentValue 3 4] "%x" colorBoxValue
    .colorBox.green set $colorBoxValue
    scan [string range $colorCurrentValue 5 6] "%x" colorBoxValue
    .colorBox.blue set $colorBoxValue

    .colorBox.current.current delete 0 end
    .colorBox.current.current insert 0 $colorCurrentColor
    ColorBoxSetColor $colorBoxMessage $colorBoxTargetW list $colorCurrentColor
    ColorBoxSetPaletteList $colorCurrentColor
  }
}

##########
# Procedure: ColorBoxSetColor
# Description: set the new color
# Arguments: colorBoxMessage - the message for the color
#            colorBoxTargetW - the widget we configure
#            colorBoxType - who wants to set the demo area
#            colorBoxValue - the value to set
# Returns: none
# Sideeffects: none
##########
proc ColorBoxSetColor {colorBoxMessage colorBoxTargetW colorBoxType colorBoxValue} {# xf ignore me 6
  global colorBox

  .colorBox.red config \
    -command "NoFunction"
  .colorBox.green config \
    -command "NoFunction"
  .colorBox.blue config \
    -command "NoFunction"
  .colorBox.h config \
    -command "NoFunction"
  .colorBox.s config \
    -command "NoFunction"
  .colorBox.v config \
    -command "NoFunction"

  set colorBoxSetColor ""
  if {"$colorBoxValue" != ""} {
    if {"$colorBoxType" != "text"} {
      .colorBox.current.current delete 0 end
      .colorBox.current.current insert 0 $colorBoxValue
    }
    if {[string match "*oreground*" $colorBoxMessage]} {
      catch ".colorBox.demo config -foreground $colorBoxValue"
    } {
      catch ".colorBox.demo config -background $colorBoxValue"
    }
    if {"$colorBoxTargetW" != ""} {
      catch "$colorBoxTargetW config -[string tolower $colorBoxMessage] \
        $colorBoxValue"
    }
  }
  case $colorBoxType in {
    {text palette} {
      if {[string match "*oreground*" $colorBoxMessage]} {
        set red [expr [lindex [winfo rgb .colorBox.demo [lindex [.colorBox.demo config -foreground] 4]] 0]/256]
        set green [expr [lindex [winfo rgb .colorBox.demo [lindex [.colorBox.demo config -foreground] 4]] 1]/256]
        set blue [expr [lindex [winfo rgb .colorBox.demo [lindex [.colorBox.demo config -foreground] 4]] 2]/256]
      } {
        set red [expr [lindex [winfo rgb .colorBox.demo [lindex [.colorBox.demo config -background] 4]] 0]/256]
        set green [expr [lindex [winfo rgb .colorBox.demo [lindex [.colorBox.demo config -background] 4]] 1]/256]
        set blue [expr [lindex [winfo rgb .colorBox.demo [lindex [.colorBox.demo config -background] 4]] 2]/256]
      }
      if {"$colorBox(type)" == "rgb"} {
        .colorBox.red set $red
        .colorBox.green set $green
        .colorBox.blue set $blue
      } {
        if {"$colorBox(type)" == "hsv"} {
          set colorBoxHSV [ColorBoxRGBToHSV [expr $red*256] [expr $green*256] [expr $blue*256]]
          .colorBox.h set [format %.0f [expr [lindex $colorBoxHSV 0]*1000.0]]
          .colorBox.s set [format %.0f [expr [lindex $colorBoxHSV 1]*1000.0]]
          .colorBox.v set [format %.0f [expr [lindex $colorBoxHSV 2]*1000.0]]
        }
      }
    }
  }
  .colorBox.red config \
    -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.green config \
    -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.blue config \
    -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.h config \
    -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.s config \
    -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.v config \
    -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""
}

##########
# Procedure: ColorBoxSetRGBColor
# Description: set the color as RGB value
# Arguments: colorBoxMessage - the message for the color
#            colorBoxTargetW - the widget we configure
#            colorBoxValue - the passed value from scale
# Returns: none
# Sideeffects: none
##########
proc ColorBoxSetRGBColor {colorBoxMessage colorBoxTargetW colorBoxValue} {# xf ignore me 6
  global colorBox

  ColorBoxSetColor $colorBoxMessage $colorBoxTargetW rgb \
    [format #%02x%02x%02x [.colorBox.red get] \
      [.colorBox.green get] [.colorBox.blue get]]
  ColorBoxSetPaletteList [format #%02x%02x%02x [.colorBox.red get] \
    [.colorBox.green get] [.colorBox.blue get]]
}

##########
# Procedure: ColorBoxSetHSVColor
# Description: set the color as HSV value
# Arguments: colorBoxMessage - the message for the color
#            colorBoxTargetW - the widget we configure
#            colorBoxValue - the passed value from scale
# Returns: none
# Sideeffects: none
##########
proc ColorBoxSetHSVColor {colorBoxMessage colorBoxTargetW colorBoxValue} {# xf ignore me 6
  global colorBox

  set colorBoxRGB [ColorBoxHSVToRGB [expr [.colorBox.h get]/1000.0] [expr [.colorBox.s get]/1000.0] [expr [.colorBox.v get]/1000.0]]
  ColorBoxSetColor $colorBoxMessage $colorBoxTargetW hsv \
    [format #%04x%04x%04x [lindex $colorBoxRGB 0] [lindex $colorBoxRGB 1] [lindex $colorBoxRGB 2]]
  ColorBoxSetPaletteList [format #%04x%04x%04x [lindex $colorBoxRGB 0] [lindex $colorBoxRGB 1] [lindex $colorBoxRGB 2]]
}

##########
# Procedure: ColorBoxSetPalette
# Description: set the palette color
# Arguments: colorBoxMessage - the message for the color
#            colorBoxTargetW - the widget we configure
#            colorBoxElement - the palette element
# Returns: none
# Sideeffects: none
##########
proc ColorBoxSetPalette {colorBoxMessage colorBoxTargetW colorBoxElement} {# xf ignore me 6
  global colorBox

  set colorBox(paletteNr) $colorBoxElement
  ColorBoxSetColor $colorBoxMessage $colorBoxTargetW palette \
    [lindex [.colorBox.palette.palette$colorBoxElement config -background] 4]
}

##########
# Procedure: ColorBoxSetPaletteList
# Description: set the palette color list
# Arguments: colorBoxValue - the new palette value
# Returns: none
# Sideeffects: none
##########
proc ColorBoxSetPaletteList {colorBoxValue} {# xf ignore me 6
  global colorBox

  catch ".colorBox.palette.palette$colorBox(paletteNr) config \
      -activebackground $colorBoxValue"
  catch ".colorBox.palette.palette$colorBox(paletteNr) config \
      -background $colorBoxValue"
  set colorBox(palette) \
    [lreplace $colorBox(palette) $colorBox(paletteNr) $colorBox(paletteNr) \
      $colorBoxValue]
}

##########
# Procedure: ColorBoxShowSlides
# Description: select color for color composing
# Arguments: colorBoxMessage - the message for the color
#            colorBoxTargetW - the widget we configure
# Returns: none
# Sideeffects: none
##########
proc ColorBoxShowSlides {colorBoxMessage colorBoxTargetW} {# xf ignore me 6
  global colorBox

  catch "pack unpack .colorBox.frame1"
  catch "pack unpack .colorBox.frame2"
  catch "pack unpack .colorBox.current"
  catch "pack unpack .colorBox.demo"
  catch "pack unpack .colorBox.h"
  catch "pack unpack .colorBox.s"
  catch "pack unpack .colorBox.v"
  catch "pack unpack .colorBox.red"
  catch "pack unpack .colorBox.green"
  catch "pack unpack .colorBox.blue"
  catch "pack unpack .colorBox.colors"
  case $colorBox(type) in {
    {rgb} {
      pack append .colorBox \
                  .colorBox.frame1 {bottom fillx} \
                  .colorBox.frame2 {bottom fillx} \
                  .colorBox.current {bottom fillx} \
                  .colorBox.palette {bottom fillx} \
                  .colorBox.red {top fillx} \
                  .colorBox.green {top fillx} \
                  .colorBox.blue {top fillx} \
                  .colorBox.demo {bottom fill expand}
    }
    {hsv} {
      pack append .colorBox \
                  .colorBox.frame1 {bottom fillx} \
                  .colorBox.frame2 {bottom fillx} \
                  .colorBox.current {bottom fillx} \
                  .colorBox.palette {bottom fillx} \
                  .colorBox.h {top fillx} \
                  .colorBox.s {top fillx} \
                  .colorBox.v {top fillx} \
                  .colorBox.demo {bottom fill expand}
    }
    {list} {
      pack append .colorBox \
                  .colorBox.frame1 {bottom fillx} \
                  .colorBox.frame2 {bottom fillx} \
                  .colorBox.current {bottom fillx} \
                  .colorBox.palette {bottom fillx} \
                  .colorBox.demo {bottom fillx} \
                  .colorBox.colors {top fill expand}
    }
  }
  if {[string match "*oreground*" $colorBoxMessage]} {
    ColorBoxSetColor $colorBoxMessage $colorBoxTargetW text \
      [lindex [.colorBox.demo config -foreground] 4]
  } {
    ColorBoxSetColor $colorBoxMessage $colorBoxTargetW text \
      [lindex [.colorBox.demo config -background] 4]
  }
}

##########
# Procedure: ColorBoxHSVToRGB
# Description: modify hsv color values to rgb values
# Arguments: colorBoxHue - the hue
#            colorBoxSat - the saturation
#            colorBoxValue - the value
# Returns: none
# Sideeffects: none
##########
proc ColorBoxHSVToRGB {colorBoxHue colorBoxSat colorBoxValue} {# xf ignore me 6
# The HSV <-> RGB converting routines are from the
# tcolor demo that is part of the demo site of Tk.

  set colorBoxV [format %.0f [expr 65535.0*$colorBoxValue]]
  if {$colorBoxSat == 0} {
    return "$colorBoxV $colorBoxV $colorBoxV"
  } else {
    set colorBoxHue [expr $colorBoxHue*6.0]
    if {$colorBoxHue >= 6.0} {
      set colorBoxHue 0.0
    }
    scan $colorBoxHue. %d i
    set colorBoxF [expr $colorBoxHue-$i]
    set colorBoxP [format %.0f [expr {65535.0*$colorBoxValue*(1 - $colorBoxSat)}]]
    set colorBoxQ [format %.0f [expr {65535.0*$colorBoxValue*(1 - ($colorBoxSat*$colorBoxF))}]]
    set colorBoxT [format %.0f [expr {65535.0*$colorBoxValue*(1 - ($colorBoxSat*(1 - $colorBoxF)))}]]
    case $i \
      0 {return "$colorBoxV $colorBoxT $colorBoxP"} \
      1 {return "$colorBoxQ $colorBoxV $colorBoxP"} \
      2 {return "$colorBoxP $colorBoxV $colorBoxT"} \
      3 {return "$colorBoxP $colorBoxQ $colorBoxV"} \
      4 {return "$colorBoxT $colorBoxP $colorBoxV"} \
      5 {return "$colorBoxV $colorBoxP $colorBoxQ"}
    error "i value $i is out of range"
  }
}

##########
# Procedure: ColorBoxRGBToHSV
# Description: modify rgb color values to hsv values
# Arguments: colorBoxRed - the red value
#            colorBoxGreen - the green value
#            colorBoxBlue - the blue value
# Returns: none
# Sideeffects: none
##########
proc ColorBoxRGBToHSV {colorBoxRed colorBoxGreen colorBoxBlue} {# xf ignore me 6
# The HSV <-> RGB converting routines are from the
# tcolor demo that is part of the demo site of Tk.

  if {$colorBoxRed > $colorBoxGreen} {
    set colorBoxMax $colorBoxRed.0
    set colorBoxMin $colorBoxGreen.0
  } else {
    set colorBoxMax $colorBoxGreen.0
    set colorBoxMin $colorBoxRed.0
  }
  if {$colorBoxBlue > $colorBoxMax} {
    set colorBoxMax $colorBoxBlue.0
  } else {
    if {$colorBoxBlue < $colorBoxMin} {
      set colorBoxMin $colorBoxBlue.0
    }
  }
  set range [expr $colorBoxMax-$colorBoxMin]
  if {$colorBoxMax == 0} {
    set colorBoxSat 0
  } else {
    set colorBoxSat [expr {($colorBoxMax-$colorBoxMin)/$colorBoxMax}]
  }
  if {$colorBoxSat == 0} {
    set colorBoxHue 0
  } else {
    set colorBoxRC [expr {($colorBoxMax - $colorBoxRed)/$range}]
    set colorBoxGC [expr {($colorBoxMax - $colorBoxGreen)/$range}]
    set colorBoxBC [expr {($colorBoxMax - $colorBoxBlue)/$range}]
    if {$colorBoxRed == $colorBoxMax} {
      set colorBoxHue [expr {.166667*($colorBoxBC - $colorBoxGC)}]
    } else {
      if {$colorBoxGreen == $colorBoxMax} {
        set colorBoxHue [expr {.166667*(2 + $colorBoxRC - $colorBoxBC)}]
      } else {
        set colorBoxHue [expr {.166667*(4 + $colorBoxGC - $colorBoxRC)}]
      }
    }
  }
  return [list $colorBoxHue $colorBoxSat [expr {$colorBoxMax/65535}]]
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

