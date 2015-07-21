# XFNoParsing
# Program: template
# Description: font selection
#
# $Header: xffontBox.tcl[2.4] Wed Mar 10 12:05:51 1993 garfield@garfield frozen $

global xfFontBox
set xfFontBox(activeBackground) ""
set xfFontBox(activeForeground) ""
set xfFontBox(background) ""
set xfFontBox(font) ""
set xfFontBox(font-demo) "This text shows the results :-)"
set xfFontBox(kanjifont-demo) "これがそのフォントです。"
set xfFontBox(foreground) ""
set xfFontBox(scrollActiveForeground) ""
set xfFontBox(scrollBackground) ""
set xfFontBox(scrollForeground) ""
set xfFontBox(fontName) ""
set xfFontBox(fontFamily) *
set xfFontBox(fontPixels) *
set xfFontBox(fontSlant) *
set xfFontBox(fontUnderline) *
set xfFontBox(fontOverstrike) *
set xfFontBox(fontWeight) *
set xfFontBox(resource) "font"

proc XFFontBox {{xfFontBoxFileFont "/usr/local/lib/xf/lib/Fonts"} {xfFontBoxResource "Font"} {xfFontBoxEntryW ""} {xfFontBoxTargetW ""}} {# xf ignore me 5
##########
# Procedure: XFFontBox
# Description: select a font
# Arguments: {xfFontBoxFileFont} - the font file with all fontnames
#            {xfFontBoxResource} - the resource to modify
#            {xfFontBoxEntryW} - the widget name for the resulting font name
#            {xfFontBoxTargetW} - the widget we configure
# Returns: fontname, or nothing
# Sideeffects: none
##########
# 
# global xfFontBox(activeBackground) - active background color
# global xfFontBox(activeForeground) - active foreground color
# global xfFontBox(background) - background color
# global xfFontBox(font) - text font
# global xfFontBox(foreground) - foreground color
# global xfFontBox(scrollActiveForeground) - scrollbar active background color
# global xfFontBox(scrollBackground) - scrollbar background color
# global xfFontBox(scrollForeground) - scrollbar foreground color
# global xfFontBox(scrollSide) - side where scrollbar is located

  global xfFontBox
  global tcl_platform

  set xfFontBox(fontName) ""
  set xfFontBox(fontFamily) *
  set xfFontBox(fontPixels) *
  set xfFontBox(fontSlant)  *
  set xfFontBox(fontUnderline) *
  set xfFontBox(fontOverstrike) *
  set xfFontBox(fontWeight) *

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$xfFontBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfFontBox(activeBackground)\" "
  }
  if {"$xfFontBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfFontBox(activeForeground)\" "
  }
  if {"$xfFontBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfFontBox(background)\" "
    append tmpFrameOpt "-background \"$xfFontBox(background)\" "
    append tmpMessageOpt "-background \"$xfFontBox(background)\" "
  }
  if {"$xfFontBox(font)" != ""} {
    append tmpButtonOpt "-$xfFontBox(resource) \"$xfFontBox(font)\" "
    append tmpMessageOpt "-$xfFontBox(resource) \"$xfFontBox(font)\" "
  }
  if {"$xfFontBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfFontBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfFontBox(foreground)\" "
  }
  if {"$xfFontBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfFontBox(scrollActiveForeground)\" "
  }
  if {"$xfFontBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfFontBox(scrollBackground)\" "
  }
  if {"$xfFontBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfFontBox(scrollForeground)\" "
  }

  set xfFontBoxTmpFileFont $xfFontBoxFileFont 
  # get font file name
  if {!([file exists $xfFontBoxFileFont] &&
        [file readable $xfFontBoxFileFont])} {
    set xfFontBoxFileFont ""
  }
  if {"$xfFontBoxFileFont" == ""} {
    global env
    if {[info exists env(XF_FONT_FILE)]} {
      if {[file exists $env(XF_FONT_FILE)] &&
          [file readable $env(XF_FONT_FILE)]} {
        set xfFontBoxFileFont $env(XF_FONT_FILE)
      }
    }
  }
  if {"$xfFontBoxResource" == ""} {
    set xfFontBoxResource "Font"
  }
  set xfFontBox(resource) [string tolower $xfFontBoxResource]

  # save the the current widget color
  if {"$xfFontBoxTargetW" != ""} {
    if {[catch "$xfFontBoxTargetW config -$xfFontBox(resource)" result]} {
      set xfFontBoxSavedFont ""
    } {
      set xfFontBoxSavedFont [lindex $result 4]
    }
  } {
    set xfFontBoxSavedFont ""
  }

  # look if there is already a font window
  if {"[info commands .xfFontBox]" == ""} {
    # build widget structure

    XFTmpltToplevel .xfFontBox 600x300 {XF font select}

    set xfFontBox(oldWidget) $xfFontBoxEntryW

    frame .xfFontBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".xfFontBox.frame1 config $tmpFrameOpt"

    frame .xfFontBox.frame2 \
      -borderwidth 2 \
      -relief raised
    catch ".xfFontBox.frame2 config $tmpFrameOpt"

    button .xfFontBox.frame1.ok \
      -text "OK"
    catch ".xfFontBox.frame1.ok config $tmpButtonOpt"

    button .xfFontBox.frame1.rescan \
      -text "Rescan fonts" \
      -command {.xfFontBox.fonts.fonts delete 0 end
                eval ".xfFontBox.fonts.fonts insert end [lsort [font families]]"}

    catch ".xfFontBox.frame1.rescan config $tmpButtonOpt"

    button .xfFontBox.frame1.cancel \
      -text "Cancel"
    catch ".xfFontBox.frame1.cancel config $tmpButtonOpt"

    if {[info exists xfFontBox($xfFontBox(resource)-demo)]} {
      label .xfFontBox.demo \
        -relief raised \
        -text $xfFontBox($xfFontBox(resource)-demo)
    } {
      label .xfFontBox.demo \
        -relief raised \
        -text "This text shows the results :-)"
    }
    catch ".xfFontBox.demo config $tmpMessageOpt"

    frame .xfFontBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".xfFontBox.current config $tmpFrameOpt"

    label .xfFontBox.current.labelcurrent \
      -relief raised
    catch ".xfFontBox.current.labelcurrent config $tmpMessageOpt"

    entry .xfFontBox.current.current \
      -relief raised
    catch ".xfFontBox.current.current config $tmpMessageOpt"

    frame .xfFontBox.fonts \
      -borderwidth 0 \
      -relief raised
    catch ".xfFontBox.fonts config $tmpFrameOpt"

    label .xfFontBox.fonts.title \
	-text "Font Family" \
	-relief raised \
	-borderwidth 1 \
	-foreground black \
	-background grey

    scrollbar .xfFontBox.fonts.vscroll \
      -relief raised \
      -command ".xfFontBox.fonts.fonts yview"
    catch ".xfFontBox.fonts.vscroll config $tmpScrollOpt"

    scrollbar .xfFontBox.fonts.hscroll \
      -orient horiz \
      -relief raised \
      -command ".xfFontBox.fonts.fonts xview"
    catch ".xfFontBox.fonts.hscroll config $tmpScrollOpt"

    listbox .xfFontBox.fonts.fonts \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".xfFontBox.fonts.hscroll set" \
      -yscrollcommand ".xfFontBox.fonts.vscroll set"
    catch ".xfFontBox.fonts.fonts config $tmpMessageOpt"

    # weight menu
    menubutton .xfFontBox.frame2.weight \
      -text "Weight" \
      -menu ".xfFontBox.frame2.weight.m"
    catch ".xfFontBox.frame2.weight config $tmpButtonOpt"

    menu .xfFontBox.frame2.weight.m
    catch ".xfFontBox.frame2.weight.m config $tmpButtonOpt"

    .xfFontBox.frame2.weight.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable xfFontBox(fontWeight) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.weight.m add radiobutton \
      -label "Normal" \
      -value "normal" \
      -variable xfFontBox(fontWeight) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.weight.m add radiobutton \
      -label "Bold" \
      -value "bold" \
      -variable xfFontBox(fontWeight) \
      -command "XFFontBoxComposeFont"

    # Slant menu
    menubutton .xfFontBox.frame2.slant \
      -text "Slant" \
      -menu ".xfFontBox.frame2.slant.m"
    catch ".xfFontBox.frame2.slant config $tmpButtonOpt"

    menu .xfFontBox.frame2.slant.m
    catch ".xfFontBox.frame2.slant.m config $tmpButtonOpt"

    .xfFontBox.frame2.slant.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable xfFontBox(fontStyle) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.slant.m add radiobutton \
      -label "Roman" \
      -value "roman" \
      -variable xfFontBox(fontSlant) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.slant.m add radiobutton \
      -label "Italic" \
      -value "italic" \
      -variable xfFontBox(fontSlant) \
      -command "XFFontBoxComposeFont"

    # Underline menu
    menubutton .xfFontBox.frame2.underline \
      -text "Underline" \
      -menu ".xfFontBox.frame2.underline.m"
    catch ".xfFontBox.frame2.underline config $tmpButtonOpt"

    menu .xfFontBox.frame2.underline.m
    catch ".xfFontBox.frame2.underline.m config $tmpButtonOpt"

    .xfFontBox.frame2.underline.m add radiobutton \
      -label "OFF" \
      -value "*" \
      -variable xfFontBox(fontUnderline) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.underline.m add radiobutton \
      -label "ON" \
      -value "underline" \
      -variable xfFontBox(fontUnderline) \
      -command "XFFontBoxComposeFont"

    # Overstrike menu
    menubutton .xfFontBox.frame2.overstrike \
      -text "Overstrike" \
      -menu ".xfFontBox.frame2.overstrike.m"
    catch ".xfFontBox.frame2.overstrike config $tmpButtonOpt"

    menu .xfFontBox.frame2.overstrike.m
    catch ".xfFontBox.frame2.overstrike.m config $tmpButtonOpt"

    .xfFontBox.frame2.overstrike.m add radiobutton \
      -label "OFF" \
      -value "*" \
      -variable xfFontBox(fontOverstrike) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.overstrike.m add radiobutton \
      -label "ON" \
      -value "overstrike" \
      -variable xfFontBox(fontOverstrike) \
      -command "XFFontBoxComposeFont"

    # pixels menu
    menubutton .xfFontBox.frame2.pixels \
      -text "Points" \
      -menu ".xfFontBox.frame2.pixels.m"
    catch ".xfFontBox.frame2.pixels config $tmpButtonOpt"

    menu .xfFontBox.frame2.pixels.m
    catch ".xfFontBox.frame2.pixels.m config $tmpButtonOpt"

    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "6 pixels" \
      -value "6" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "8 pixels" \
      -value "8" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "10 pixels" \
      -value "10" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "12 pixels" \
      -value "12" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "13 pixels" \
      -value "13" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "14 pixels" \
      -value "14" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "16 pixels" \
      -value "16" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "18 pixels" \
      -value "18" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "24 pixels" \
      -value "24" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "28 pixels" \
      -value "28" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"
    .xfFontBox.frame2.pixels.m add radiobutton \
      -label "30 pixels" \
      -value "30" \
      -variable xfFontBox(fontPixels) \
      -command "XFFontBoxComposeFont"

    # read font file
    # 12/11/98 - D. LaBelle - Used new TK8.0 font mechanism
    .xfFontBox.fonts.fonts delete 0 end
    eval ".xfFontBox.fonts.fonts insert end [lsort [font families]]"
  } {
    if {"[winfo class $xfFontBox(oldWidget)]" == "Text"} {
      catch "$xfFontBox(oldWidget) delete 1.0 end"
      catch "$xfFontBox(oldWidget) insert 1.0 [.xfFontBox.current.current get]"
    } {
      if {"[winfo class $xfFontBox(oldWidget)]" == "Entry"} {
        catch "$xfFontBox(oldWidget) delete 0 end"
        catch "$xfFontBox(oldWidget) insert 0 [.xfFontBox.current.current get]"
      }
    }

    set xfFontBox(oldWidget) $xfFontBoxEntryW
  }

  .xfFontBox.frame1.ok config \
    -command "
      global xfFontBox
      set xfFontBox(fontName) \[.xfFontBox.current.current get\]
      if {\"$xfFontBoxEntryW\" != \"\"} {
        if {\"\[winfo class $xfFontBoxEntryW\]\" == \"Text\"} {
          catch \"$xfFontBoxEntryW delete 1.0 end\"
          catch \"$xfFontBoxEntryW insert 1.0 \\\"\$xfFontBox(fontName)\\\"\"
        } {
          if {\"\[winfo class $xfFontBoxEntryW\]\" == \"Entry\"} {
            catch \"$xfFontBoxEntryW delete 0 end\"
            catch \"$xfFontBoxEntryW insert 0 \\\"\$xfFontBox(fontName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfFontBox}
      } {
        catch {destroy .xfFontBox}
      }"

  .xfFontBox.frame1.cancel config \
    -command "
      global xfFontBox
      set xfFontBox(fontName) {}
      if {\"$xfFontBoxTargetW\" != \"\"} {
        catch \"$xfFontBoxTargetW config -$xfFontBox(resource) $xfFontBoxSavedFont\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfFontBox}
      } {
        catch {destroy .xfFontBox}
      }"

  .xfFontBox.current.labelcurrent config \
    -text "$xfFontBoxResource:"

  # bindings
  bind .xfFontBox.current.current <Return> "
    XFFontBoxSetFont \"$xfFontBoxTargetW\""

  bind .xfFontBox.fonts.fonts <Double-1> "
    XFFontBoxSelectFont %W \"$xfFontBoxTargetW\" %y
    global xfFontBox
    set xfFontBox(fontName) \[.xfFontBox.current.current get\]
    if {\"$xfFontBoxEntryW\" != \"\"} {
      if {\"\[winfo class $xfFontBoxEntryW\]\" == \"Text\"} {
        catch \"$xfFontBoxEntryW delete 1.0 end\"
        catch \"$xfFontBoxEntryW insert 1.0 \\\"\$xfFontBox(fontName)\\\"\"
      } {
        if {\"\[winfo class $xfFontBoxEntryW\]\" == \"Entry\"} {
          catch \"$xfFontBoxEntryW delete 0 end\"
          catch \"$xfFontBoxEntryW insert 0 \\\"\$xfFontBox(fontName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .xfFontBox}
    } {
      catch {destroy .xfFontBox}
    }"
  bind .xfFontBox.fonts.fonts <ButtonPress-1> "
    XFFontBoxSelectFont %W \"$xfFontBoxTargetW\" %y"
  bind .xfFontBox.fonts.fonts <Button1-Motion> "
    XFFontBoxSelectFont %W \"$xfFontBoxTargetW\" %y"
  bind .xfFontBox.fonts.fonts <Shift-ButtonPress-1> "
    XFFontBoxSelectFont %W \"$xfFontBoxTargetW\" %y"
  bind .xfFontBox.fonts.fonts <Shift-Button1-Motion> "
    XFFontBoxSelectFont %W \"$xfFontBoxTargetW\" %y"

  # set up current value
  .xfFontBox.current.current delete 0 end
  if {"$xfFontBoxEntryW" != ""} {
    if {"[winfo class $xfFontBoxEntryW]" == "Text"} {
      .xfFontBox.current.current insert 0 [$xfFontBoxEntryW get 1.0 end]
    } {
      if {"[winfo class $xfFontBoxEntryW]" == "Entry"} {
        .xfFontBox.current.current insert 0 [$xfFontBoxEntryW get]
      }
    }
  }

  # packing
  pack append .xfFontBox.frame1 \
              .xfFontBox.frame1.ok {left fill expand} \
              .xfFontBox.frame1.rescan {left fill expand} \
              .xfFontBox.frame1.cancel {left fill expand}
  pack append .xfFontBox.frame2 \
              .xfFontBox.frame2.weight {left} \
              .xfFontBox.frame2.slant {left} \
              .xfFontBox.frame2.pixels {left} \
              .xfFontBox.frame2.underline {left} \
              .xfFontBox.frame2.overstrike {left}
  pack append .xfFontBox.current \
              .xfFontBox.current.labelcurrent {left} \
              .xfFontBox.current.current {left fill expand}
  pack append .xfFontBox.fonts \
              .xfFontBox.fonts.vscroll "$xfFontBox(scrollSide) filly" \
		  .xfFontBox.fonts.title {top fillx} \
              .xfFontBox.fonts.hscroll {bottom fillx} \
              .xfFontBox.fonts.fonts {left fill expand}

  if {"$xfFontBoxFileFont" != ""} {
    pack append .xfFontBox \
                .xfFontBox.frame1 {bottom fillx} \
                .xfFontBox.current {bottom fillx} \
                .xfFontBox.demo {bottom fillx} \
                .xfFontBox.frame2 {top fill} \
                .xfFontBox.fonts {left expand fill}
  } {
    wm geometry .xfFontBox 400x90
    pack append .xfFontBox \
                .xfFontBox.frame1 {bottom fillx} \
                .xfFontBox.current {bottom fillx} \
                .xfFontBox.frame2 {top fill} \
                .xfFontBox.demo {bottom fill expand}
  }
  catch "wm deiconify .xfFontBox"

  if {"$xfFontBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .xfFontBox
    tkwait window .xfFontBox

    return $xfFontBox(fontName)
  }
}

##########
# Procedure: XFFontBoxComposeFont
# Description: set the font
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFFontBoxComposeFont {} {# xf ignore me 6
  global xfFontBox
# DRL - Modified to support TK8.0 font mechanism

  set fontNewFont ""

  if {"$xfFontBox(fontFamily)" != "*"} {
    lappend fontNewFont $xfFontBox(fontFamily)
  } {lappend fontNewFont courier}

  if {"$xfFontBox(fontPixels)" != "*"} {
    lappend fontNewFont $xfFontBox(fontPixels)
  } {lappend fontNewFont 10}

  if {"$xfFontBox(fontWeight)" != "*"} {
    lappend fontNewFont $xfFontBox(fontWeight)
  }

  if {"$xfFontBox(fontSlant)" != "*"} {
    lappend fontNewFont $xfFontBox(fontSlant)
  }

  if {"$xfFontBox(fontUnderline)" != "*"} {
    lappend fontNewFont $xfFontBox(fontUnderline)
  }

  if {"$xfFontBox(fontOverstrike)" != "*"} {
    lappend fontNewFont $xfFontBox(fontOverstrike)
  }

  .xfFontBox.current.current delete 0 end
  .xfFontBox.current.current insert 0 $fontNewFont
  catch ".xfFontBox.demo config -$xfFontBox(resource) \{$fontNewFont\}" result
}

##########
# Procedure: XFFontBoxSelectFont
# Description: select font for font composing
# Arguments: xfFontBoxW - the widget
#            xfFontBoxTargetW - the widget we configure
#            xfFontBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFFontBoxSelectFont {xfFontBoxW xfFontBoxTargetW xfFontBoxY} {# xf ignore me 6
global xfFontBox

  set xfFontBoxNearest [$xfFontBoxW nearest $xfFontBoxY]
  if {$xfFontBoxNearest >= 0} {
    $xfFontBoxW select anchor $xfFontBoxNearest
    $xfFontBoxW select set $xfFontBoxNearest
#    .xfFontBox.current.current delete 0 end
#    .xfFontBox.current.current insert 0 [$xfFontBoxW get $xfFontBoxNearest]
#    XFFontBoxSetFont "$xfFontBoxTargetW"
     set xfFontBox(fontFamily) [$xfFontBoxW get $xfFontBoxNearest]
     XFFontBoxComposeFont
  }
}

##########
# Procedure: XFFontBoxSetFont
# Description: set font for the widget
# Arguments: xfFontBoxTargetW - the widget we configure
# Returns: none
# Sideeffects: none
##########
proc XFFontBoxSetFont {xfFontBoxTargetW} {# xf ignore me 6
  global xfFontBox

  if {"[.xfFontBox.current.current get]" != ""} {
    catch ".xfFontBox.demo config -$xfFontBox(resource) \
      [.xfFontBox.current.current get]"
    if {"$xfFontBoxTargetW" != ""} {
      catch "$xfFontBoxTargetW config -$xfFontBox(resource) \
        [.xfFontBox.current.current get]"
    }
  }
}

# eof

