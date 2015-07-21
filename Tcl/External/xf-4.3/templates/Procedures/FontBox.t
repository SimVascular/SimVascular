# XFNoParsing
# Program: template
# Description: font selection
#
# $Header: FontBox.t[2.4] Wed Mar 10 12:03:03 1993 garfield@garfield frozen $

global fontBox
set fontBox(activeBackground) ""
set fontBox(activeForeground) ""
set fontBox(background) ""
set fontBox(font) ""
set fontBox(font-demo) "This text shows the results :-)"
set fontBox(kanjifont-demo) "これがそのフォントです。"
set fontBox(foreground) ""
set fontBox(resource) "font"
set fontBox(scrollActiveForeground) ""
set fontBox(scrollBackground) ""
set fontBox(scrollForeground) ""
set fontBox(scrollSide) left
set fontBox(fontName) ""
set fontBox(fontFamily) *
set fontBox(fontPixels) *
set fontBox(fontSlant) *
set fontBox(fontSWidth) *
set fontBox(fontWeight) *
set fontBox(resource) "font"

proc FontBox {{fontBoxFileFont "/usr/local/lib/xf/lib/Fonts"} {fontBoxResource "Font"} {fontBoxEntryW ""} {fontBoxTargetW ""}} {# xf ignore me 5
##########
# Procedure: FontBox
# Description: select a font
# Arguments: {fontBoxFileFont} - the font file with all fontnames
#            {fontBoxResource} - the resource to modify
#            {fontBoxEntryW} - the widget name for the resulting font name
#            {fontBoxTargetW} - the widget we configure
# Returns: fontname, or nothing
# Sideeffects: none
##########
# 
# global fontBox(activeBackground) - active background color
# global fontBox(activeForeground) - active foreground color
# global fontBox(background) - background color
# global fontBox(font) - text font
# global fontBox(foreground) - foreground color
# global fontBox(resource) - resource name
# global fontBox(scrollActiveForeground) - scrollbar active background color
# global fontBox(scrollBackground) - scrollbar background color
# global fontBox(scrollForeground) - scrollbar foreground color
# global fontBox(scrollSide) - side where scrollbar is located

  global fontBox
  global tkVersion

  set fontBox(fontName) ""
  set fontBox(fontFamily) *
  set fontBox(fontPixels) *
  set fontBox(fontSlant) *
  set fontBox(fontSWidth) *
  set fontBox(fontWeight) *

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$fontBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$fontBox(activeBackground)\" "
  }
  if {"$fontBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$fontBox(activeForeground)\" "
  }
  if {"$fontBox(background)" != ""} {
    append tmpButtonOpt "-background \"$fontBox(background)\" "
    append tmpFrameOpt "-background \"$fontBox(background)\" "
    append tmpMessageOpt "-background \"$fontBox(background)\" "
  }
  if {"$fontBox(font)" != ""} {
    append tmpButtonOpt "-$fontBox(resource) \"$fontBox(font)\" "
    append tmpMessageOpt "-$fontBox(resource) \"$fontBox(font)\" "
  }
  if {"$fontBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$fontBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$fontBox(foreground)\" "
  }
  if {"$fontBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$fontBox(scrollActiveForeground)\" "
  }
  if {"$fontBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$fontBox(scrollBackground)\" "
  }
  if {"$fontBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$fontBox(scrollForeground)\" "
  }

  set fontBoxTmpFileFont $fontBoxFileFont 
  # get font file name
  if {!([file exists $fontBoxFileFont] &&
        [file readable $fontBoxFileFont])} {
    set fontBoxFileFont ""
  }
  if {"$fontBoxFileFont" == ""} {
    global env
    if {[info exists env(XF_FONT_FILE)]} {
      if {[file exists $env(XF_FONT_FILE)] &&
          [file readable $env(XF_FONT_FILE)]} {
        set fontBoxFileFont $env(XF_FONT_FILE)
      }
    }
  }
  if {"$fontBoxResource" == ""} {
    set fontBoxResource "Font"
  }
  set fontBox(resource) [string tolower $fontBoxResource]

  # save the the current widget color
  if {"$fontBoxTargetW" != ""} {
    if {[catch "$fontBoxTargetW config -$fontBox(resource)" result]} {
      set fontBoxSavedFont ""
    } {
      set fontBoxSavedFont [lindex $result 4]
    }
  } {
    set fontBoxSavedFont ""
  }

  # look if there is already a font window
  if {"[info commands .fontBox]" == ""} {
    # build widget structure

    # start build of toplevel
    if {"[info commands XFDestroy]" != ""} {
      catch {XFDestroy .fontBox}
    } {
      catch {destroy .fontBox}
    }
    toplevel .fontBox \
      -borderwidth 0
    catch ".fontBox config $tmpFrameOpt"
    wm geometry .fontBox 600x300
    wm title .fontBox {Font box}
    wm maxsize .fontBox 1000 1000
    wm minsize .fontBox 100 90
    # end build of toplevel

    set fontBox(oldWidget) $fontBoxEntryW

    frame .fontBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".fontBox.frame1 config $tmpFrameOpt"

    frame .fontBox.frame2 \
      -borderwidth 2 \
      -relief raised
    catch ".fontBox.frame2 config $tmpFrameOpt"

    button .fontBox.frame1.ok \
      -text "OK"
    catch ".fontBox.frame1.ok config $tmpButtonOpt"

    button .fontBox.frame1.rescan \
      -text "Rescan fonts" \
      -command "
        if {\"$fontBoxTmpFileFont\" != \"\"} {
          catch \"exec xlsfonts > $fontBoxTmpFileFont\"
          if {\[.fontBox.fonts.fonts size\] > 0} {
            .fontBox.fonts.fonts delete 0 end
          }
          if {\[catch \"open $fontBoxTmpFileFont r\" fontInFile\]} {
            if {\"\[info commands AlertBox\]\" != \"\"} {
              AlertBox \"\$fontInFile\"
            } {
              puts stderr \"\$fontInFile\"
            }
          } {
            set fontReadList \[read \$fontInFile\]
            close \$fontInFile
            foreach fontLine \[split \$fontReadList \"\\n\"\] {
              if {\"\[string trim \$fontLine\]\" != \"\"} {
                .fontBox.fonts.fonts insert end \$fontLine
              }
            }
          }
        }"
    catch ".fontBox.frame1.rescan config $tmpButtonOpt"

    button .fontBox.frame1.cancel \
      -text "Cancel"
    catch ".fontBox.frame1.cancel config $tmpButtonOpt"

    if {[info exists fontBox($fontBox(resource)-demo)]} {
      label .fontBox.demo \
        -relief raised \
        -text $fontBox($fontBox(resource)-demo)
    } {
      label .fontBox.demo \
        -relief raised \
        -text "This text shows the results :-)"
    }
    catch ".fontBox.demo config $tmpMessageOpt"

    frame .fontBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".fontBox.current config $tmpFrameOpt"

    label .fontBox.current.labelcurrent \
      -relief raised
    catch ".fontBox.current.labelcurrent config $tmpMessageOpt"

    entry .fontBox.current.current \
      -relief raised
    catch ".fontBox.current.current config $tmpMessageOpt"

    frame .fontBox.fonts \
      -borderwidth 0 \
      -relief raised
    catch ".fontBox.fonts config $tmpFrameOpt"

    scrollbar .fontBox.fonts.vscroll \
      -relief raised \
      -command ".fontBox.fonts.fonts yview"
    catch ".fontBox.fonts.vscroll config $tmpScrollOpt"

    scrollbar .fontBox.fonts.hscroll \
      -orient horiz \
      -relief raised \
      -command ".fontBox.fonts.fonts xview"
    catch ".fontBox.fonts.hscroll config $tmpScrollOpt"

    listbox .fontBox.fonts.fonts \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".fontBox.fonts.hscroll set" \
      -yscrollcommand ".fontBox.fonts.vscroll set"
    catch ".fontBox.fonts.fonts config $tmpMessageOpt"

    # family menu
    menubutton .fontBox.frame2.family \
      -text "Family" \
      -menu ".fontBox.frame2.family.m"
    catch ".fontBox.frame2.family config $tmpButtonOpt"

    menu .fontBox.frame2.family.m
    catch ".fontBox.frame2.family.m config $tmpButtonOpt"

    .fontBox.frame2.family.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "charter" \
      -value "charter" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "courier" \
      -value "courier" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "fixed" \
      -value "fixed" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "helvetica" \
      -value "helvetica" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "jis" \
      -value "jis" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "lucida" \
      -value "lucida" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "terminal" \
      -value "terminal" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton \
      -label "times" \
      -value "times" \
      -variable fontBox(fontFamily) \
      -command "FontBoxComposeFont"

    # weight menu
    menubutton .fontBox.frame2.weight \
      -text "Weight" \
      -menu ".fontBox.frame2.weight.m"
    catch ".fontBox.frame2.weight config $tmpButtonOpt"

    menu .fontBox.frame2.weight.m
    catch ".fontBox.frame2.weight.m config $tmpButtonOpt"

    .fontBox.frame2.weight.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable fontBox(fontWeight) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.weight.m add radiobutton \
      -label "bold" \
      -value "bold" \
      -variable fontBox(fontWeight) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.weight.m add radiobutton \
      -label "demibold" \
      -value "demibold" \
      -variable fontBox(fontWeight) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.weight.m add radiobutton \
      -label "medium" \
      -value "medium" \
      -variable fontBox(fontWeight) \
      -command "FontBoxComposeFont"

    # Slant menu
    menubutton .fontBox.frame2.slant \
      -text "Slant" \
      -menu ".fontBox.frame2.slant.m"
    catch ".fontBox.frame2.slant config $tmpButtonOpt"

    menu .fontBox.frame2.slant.m
    catch ".fontBox.frame2.slant.m config $tmpButtonOpt"

    .fontBox.frame2.slant.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable fontBox(fontSlant) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.slant.m add radiobutton \
      -label "i" \
      -value "i" \
      -variable fontBox(fontSlant) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.slant.m add radiobutton \
      -label "o" \
      -value "o" \
      -variable fontBox(fontSlant) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.slant.m add radiobutton \
      -label "r" \
      -value "r" \
      -variable fontBox(fontSlant) \
      -command "FontBoxComposeFont"

    # Set width menu
    menubutton .fontBox.frame2.swidth \
      -text "Set width" \
      -menu ".fontBox.frame2.swidth.m"
    catch ".fontBox.frame2.swidth config $tmpButtonOpt"

    menu .fontBox.frame2.swidth.m
    catch ".fontBox.frame2.swidth.m config $tmpButtonOpt"

    .fontBox.frame2.swidth.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable fontBox(fontSWidth) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.swidth.m add radiobutton \
      -label "normal" \
      -value "normal" \
      -variable fontBox(fontSWidth) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.swidth.m add radiobutton \
      -label "semicondensed" \
      -value "semicondensed" \
      -variable fontBox(fontSWidth) \
      -command "FontBoxComposeFont"

    # pixels menu
    menubutton .fontBox.frame2.pixels \
      -text "Pixels" \
      -menu ".fontBox.frame2.pixels.m"
    catch ".fontBox.frame2.pixels config $tmpButtonOpt"

    menu .fontBox.frame2.pixels.m
    catch ".fontBox.frame2.pixels.m config $tmpButtonOpt"

    .fontBox.frame2.pixels.m add radiobutton \
      -label "*" \
      -value "*" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "6 pixels" \
      -value "6" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "8 pixels" \
      -value "8" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "10 pixels" \
      -value "10" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "12 pixels" \
      -value "12" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "13 pixels" \
      -value "13" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "14 pixels" \
      -value "14" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "16 pixels" \
      -value "16" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "18 pixels" \
      -value "18" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "24 pixels" \
      -value "24" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "28 pixels" \
      -value "28" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton \
      -label "30 pixels" \
      -value "30" \
      -variable fontBox(fontPixels) \
      -command "FontBoxComposeFont"

    # read font file
    if {"$fontBoxFileFont" != ""} {
      if {[catch "open $fontBoxFileFont r" fontInFile]} {
        set fontBoxFileFont ""
        if {"[info commands AlertBox]" != ""} {
          AlertBox "$fontInFile"
        } {
          puts stderr "$fontInFile"
        }
      } {
        set fontReadList [read $fontInFile]
        close $fontInFile
        foreach fontLine [split $fontReadList "\n"] {
          if {"[string trim $fontLine]" != ""} {
            .fontBox.fonts.fonts insert end $fontLine
          }
        }
      }
    }
  } {
    if {"[winfo class $fontBox(oldWidget)]" == "Text"} {
      catch "$fontBox(oldWidget) delete 1.0 end"
      catch "$fontBox(oldWidget) insert 1.0 [.fontBox.current.current get]"
    } {
      if {"[winfo class $fontBox(oldWidget)]" == "Entry"} {
        catch "$fontBox(oldWidget) delete 0 end"
        catch "$fontBox(oldWidget) insert 0 [.fontBox.current.current get]"
      }
    }

    set fontBox(oldWidget) $fontBoxEntryW
  }

  .fontBox.frame1.ok config \
    -command "
      global fontBox
      set fontBox(fontName) \[.fontBox.current.current get\]
      if {\"$fontBoxEntryW\" != \"\"} {
        if {\"\[winfo class $fontBoxEntryW\]\" == \"Text\"} {
          catch \"$fontBoxEntryW delete 1.0 end\"
          catch \"$fontBoxEntryW insert 1.0 \\\"\$fontBox(fontName)\\\"\"
        } {
          if {\"\[winfo class $fontBoxEntryW\]\" == \"Entry\"} {
            catch \"$fontBoxEntryW delete 0 end\"
            catch \"$fontBoxEntryW insert 0 \\\"\$fontBox(fontName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fontBox}
      } {
        catch {destroy .fontBox}
      }"

  .fontBox.frame1.cancel config \
    -command "
      global fontBox
      set fontBox(fontName) {}
      if {\"$fontBoxTargetW\" != \"\"} {
        catch \"$fontBoxTargetW config -$fontBox(resource) $fontBoxSavedFont\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fontBox}
      } {
        catch {destroy .fontBox}
      }"

  .fontBox.current.labelcurrent config \
    -text "$fontBoxResource:"

  # bindings
  bind .fontBox.current.current <Return> "
    FontBoxSetFont \"$fontBoxTargetW\""

  bind .fontBox.fonts.fonts <Double-1> "
    FontBoxSelectFont %W \"$fontBoxTargetW\" %y
    global fontBox
    set fontBox(fontName) \[.fontBox.current.current get\]
    if {\"$fontBoxEntryW\" != \"\"} {
      if {\"\[winfo class $fontBoxEntryW\]\" == \"Text\"} {
        catch \"$fontBoxEntryW delete 1.0 end\"
        catch \"$fontBoxEntryW insert 1.0 \\\"\$fontBox(fontName)\\\"\"
      } {
        if {\"\[winfo class $fontBoxEntryW\]\" == \"Entry\"} {
          catch \"$fontBoxEntryW delete 0 end\"
          catch \"$fontBoxEntryW insert 0 \\\"\$fontBox(fontName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .fontBox}
    } {
      catch {destroy .fontBox}
    }"
  bind .fontBox.fonts.fonts <ButtonPress-1> "
    FontBoxSelectFont %W \"$fontBoxTargetW\" %y"
  bind .fontBox.fonts.fonts <Button1-Motion> "
    FontBoxSelectFont %W \"$fontBoxTargetW\" %y"
  bind .fontBox.fonts.fonts <Shift-ButtonPress-1> "
    FontBoxSelectFont %W \"$fontBoxTargetW\" %y"
  bind .fontBox.fonts.fonts <Shift-Button1-Motion> "
    FontBoxSelectFont %W \"$fontBoxTargetW\" %y"

  # set up current value
  .fontBox.current.current delete 0 end
  if {"$fontBoxEntryW" != ""} {
    if {"[winfo class $fontBoxEntryW]" == "Text"} {
      .fontBox.current.current insert 0 [$fontBoxEntryW get 1.0 end]
    } {
      if {"[winfo class $fontBoxEntryW]" == "Entry"} {
        .fontBox.current.current insert 0 [$fontBoxEntryW get]
      }
    }
  }

  # packing
  pack append .fontBox.frame1 \
              .fontBox.frame1.ok {left fill expand} \
              .fontBox.frame1.rescan {left fill expand} \
              .fontBox.frame1.cancel {left fill expand}
  pack append .fontBox.frame2 \
              .fontBox.frame2.family {left} \
              .fontBox.frame2.weight {left} \
              .fontBox.frame2.slant {left} \
              .fontBox.frame2.swidth {left} \
              .fontBox.frame2.pixels {left}
  pack append .fontBox.current \
              .fontBox.current.labelcurrent {left} \
              .fontBox.current.current {left fill expand}
  pack append .fontBox.fonts \
              .fontBox.fonts.vscroll "$fontBox(scrollSide) filly" \
              .fontBox.fonts.hscroll {bottom fillx} \
              .fontBox.fonts.fonts {left fill expand}

  if {"$fontBoxFileFont" != ""} {
    pack append .fontBox \
                .fontBox.frame1 {bottom fillx} \
                .fontBox.current {bottom fillx} \
                .fontBox.demo {bottom fillx} \
                .fontBox.frame2 {top fill} \
                .fontBox.fonts {left expand fill}
  } {
    wm geometry .fontBox 400x90
    pack append .fontBox \
                .fontBox.frame1 {bottom fillx} \
                .fontBox.current {bottom fillx} \
                .fontBox.frame2 {top fill} \
                .fontBox.demo {bottom fill expand}
  }
  catch "wm deiconify .fontBox"

  if {"$fontBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .fontBox
    tkwait window .fontBox

    return $fontBox(fontName)
  }
}

##########
# Procedure: FontBoxComposeFont
# Description: set the font
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc FontBoxComposeFont {} {# xf ignore me 6
  global fontBox
  
  if {"$fontBox(fontFamily)" != "*"} {
    append fontNewFont * $fontBox(fontFamily)
  } {
    append fontNewFont *
  }
  if {"$fontBox(fontWeight)" != "*"} {
    append fontNewFont - $fontBox(fontWeight)
  } {
    append fontNewFont - *
  }
  if {"$fontBox(fontSlant)" != "*"} {
    append fontNewFont - $fontBox(fontSlant)
  } {
    append fontNewFont - *
  }
  if {"$fontBox(fontSWidth)" != "*"} {
    append fontNewFont - $fontBox(fontSWidth)
  } {
    append fontNewFont - *
  }
  append fontNewFont - *
  if {"$fontBox(fontPixels)" != "*"} {
    append fontNewFont - $fontBox(fontPixels)
  } {
    append fontNewFont -
  }
  append fontNewFont *

  .fontBox.current.current delete 0 end
  .fontBox.current.current insert 0 $fontNewFont
  catch ".fontBox.demo config \
    -$fontBox(resource) $fontNewFont"
}

##########
# Procedure: FontBoxSelectFont
# Description: select font for font composing
# Arguments: fontBoxW - the widget
#            fontBoxTargetW - the widget we configure
#            fontBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc FontBoxSelectFont {fontBoxW fontBoxTargetW fontBoxY} {# xf ignore me 6

  set fontBoxNearest [$fontBoxW nearest $fontBoxY]
  if {$fontBoxNearest >= 0} {
    $fontBoxW select clear 0 end
    $fontBoxW select set $fontBoxNearest
    .fontBox.current.current delete 0 end
    .fontBox.current.current insert 0 [$fontBoxW get $fontBoxNearest]
    FontBoxSetFont "$fontBoxTargetW"
  }
}

##########
# Procedure: FontBoxSetFont
# Description: set font for the widget
# Arguments: fontBoxTargetW - the widget we configure
# Returns: none
# Sideeffects: none
##########
proc FontBoxSetFont {fontBoxTargetW} {# xf ignore me 6
  global fontBox

  if {"[.fontBox.current.current get]" != ""} {
    catch ".fontBox.demo config -$fontBox(resource) \
      [.fontBox.current.current get]"
    if {"$fontBoxTargetW" != ""} {
      catch "$fontBoxTargetW config -$fontBox(resource) \
        [.fontBox.current.current get]"
    }
  }
}

# eof

