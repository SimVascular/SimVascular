# XFNoParsing
# Program: template
# Description: cursor selection
#
# $Header: xfcursorBox.tcl[2.3] Wed Mar 10 12:05:33 1993 garfield@garfield frozen $

global xfCursorBox
set xfCursorBox(activeBackground) ""
set xfCursorBox(activeForeground) ""
set xfCursorBox(background) ""
set xfCursorBox(font) ""
set xfCursorBox(foreground) ""
set xfCursorBox(scrollActiveForeground) ""
set xfCursorBox(scrollBackground) ""
set xfCursorBox(scrollForeground) ""
set xfCursorBox(cursorName) ""

proc XFCursorBox {{xfCursorBoxFileCursor "/usr/local/lib/xf/lib/Cursors"} {xfCursorBoxFileColor "/usr/local/lib/xf/lib/Colors"} {xfCursorBoxMessage "Cursor"} {xfCursorBoxEntryW ""} {xfCursorBoxTargetW ""}} {# xf ignore me 5
##########
# Procedure: XFCursorBox
# Description: select a cursor
# Arguments: {xfCursorBoxFileCursor} - the color file with all cursornames
#            {xfCursorBoxFileColor} - the color file with all colornames
#            {xfCursorBoxMessage} - a message to display
#            {xfCursorBoxEntryW} - the widget name for the resulting color name
#            {xfCursorBoxTargetW} - the widget we configure
# Returns: cursorname, or nothing
# Sideeffects: none
##########
#
# global xfCursorBox(activeBackground) - active background color
# global xfCursorBox(activeForeground) - active foreground color
# global xfCursorBox(background) - background color
# global xfCursorBox(font) - text font
# global xfCursorBox(foreground) - foreground color
# global xfCursorBox(scrollActiveForeground) - scrollbar active background color
# global xfCursorBox(scrollBackground) - scrollbar background color
# global xfCursorBox(scrollForeground) - scrollbar foreground color
# global xfCursorBox(scrollSide) - side where scrollbar is located

  global xfCursorBox

  set xfCursorBox(cursorName) ""

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$xfCursorBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfCursorBox(activeBackground)\" "
  }
  if {"$xfCursorBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfCursorBox(activeForeground)\" "
  }
  if {"$xfCursorBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfCursorBox(background)\" "
    append tmpFrameOpt "-background \"$xfCursorBox(background)\" "
    append tmpMessageOpt "-background \"$xfCursorBox(background)\" "
  }
  if {"$xfCursorBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfCursorBox(font)\" "
    append tmpMessageOpt "-font \"$xfCursorBox(font)\" "
  }
  if {"$xfCursorBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfCursorBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfCursorBox(foreground)\" "
  }
  if {"$xfCursorBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfCursorBox(scrollActiveForeground)\" "
  }
  if {"$xfCursorBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfCursorBox(scrollBackground)\" "
  }
  if {"$xfCursorBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfCursorBox(scrollForeground)\" "
  }

  # get cursor file name
  if {!([file exists $xfCursorBoxFileCursor] &&
        [file readable $xfCursorBoxFileCursor])} {
    set xfCursorBoxFileCursor ""
  }
  if {"$xfCursorBoxFileCursor" == ""} {
    global env
    if {[info exists env(XF_CURSOR_FILE)]} {
      if {[file exists $env(XF_CURSOR_FILE)] &&
          [file readable $env(XF_CURSOR_FILE)]} {
        set xfCursorBoxFileCursor $env(XF_CURSOR_FILE)
      }
    }
  }

  # get color file name
  if {!([file exists $xfCursorBoxFileColor] &&
        [file readable $xfCursorBoxFileColor])} {
    set xfCursorBoxFileColor ""
  }
  if {"$xfCursorBoxFileColor" == ""} {
    global env
    if {[info exists env(XF_COLOR_FILE)]} {
      if {[file exists $env(XF_COLOR_FILE)] &&
          [file readable $env(XF_COLOR_FILE)]} {
        set xfCursorBoxFileColor $env(XF_COLOR_FILE)
      }
    }
  }
  if {"$xfCursorBoxMessage" == ""} {
    set xfCursorBoxMessage "Cursor"
  }

  # save the the current widget cursor
  if {"$xfCursorBoxTargetW" != ""} {
    if {[catch "$xfCursorBoxTargetW config -[string tolower $xfCursorBoxMessage]" result]} {
      set xfCursorBoxSavedCursor ""
    } {
      set xfCursorBoxSavedCursor [lindex $result 4]
    }
  } {
    set xfCursorBoxSavedCursor ""
  }

  # look if there is already a font window
  if {"[info commands .xfCursorBox]" == ""} {
    # build widget structure

    XFTmpltToplevel .xfCursorBox 400x300 {XF cursor select}

    set xfCursorBox(oldWidget) $xfCursorBoxEntryW

    frame .xfCursorBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".xfCursorBox.frame1 config $tmpFrameOpt"
 
    button .xfCursorBox.frame1.ok \
      -text "OK"
    catch ".xfCursorBox.frame1.ok config $tmpButtonOpt"

    button .xfCursorBox.frame1.cancel \
      -text "Cancel"
    catch ".xfCursorBox.frame1.cancel config $tmpButtonOpt"

    label .xfCursorBox.demo \
      -relief raised \
      -text "This text shows the results :-) (enter this widget)"
    catch ".xfCursorBox.demo config $tmpMessageOpt"

    frame .xfCursorBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".xfCursorBox.current config $tmpFrameOpt"

    label .xfCursorBox.current.labelcurrent \
      -relief raised
    catch ".xfCursorBox.current.labelcurrent config $tmpMessageOpt"

    entry .xfCursorBox.current.current \
      -relief raised
    catch ".xfCursorBox.current.current config $tmpMessageOpt"

    frame .xfCursorBox.fg \
      -borderwidth 0 \
      -relief raised
    catch ".xfCursorBox config.fg $tmpFrameOpt"

    label .xfCursorBox.fg.labelfg \
      -relief raised \
      -text "Foreground:"
    catch ".xfCursorBox.fg.labelfg config $tmpMessageOpt"

    entry .xfCursorBox.fg.fg \
      -relief raised
    catch ".xfCursorBox.fg.fg config $tmpMessageOpt"

    frame .xfCursorBox.bg \
      -borderwidth 0 \
      -relief raised
    catch ".xfCursorBox.bg config $tmpFrameOpt"

    label .xfCursorBox.bg.labelbg \
      -relief raised \
      -text "Background:"
    catch ".xfCursorBox.bg.labelbg config $tmpMessageOpt"

    entry .xfCursorBox.bg.bg \
      -relief raised
    catch ".xfCursorBox.bg.bg config $tmpMessageOpt"

    frame .xfCursorBox.cursors \
      -borderwidth 0 \
      -relief raised
    catch ".xfCursorBox.cursor config $tmpFrameOpt"

    scrollbar .xfCursorBox.cursors.vscroll \
      -relief raised \
      -command ".xfCursorBox.cursors.cursors yview"
    catch ".xfCursorBox.cursors.vscroll config $tmpScrollOpt"

    scrollbar .xfCursorBox.cursors.hscroll \
      -orient horiz \
      -relief raised \
      -command ".xfCursorBox.cursors.cursors xview"
    catch ".xfCursorBox.cursors.hscroll config $tmpScrollOpt"

    listbox .xfCursorBox.cursors.cursors \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".xfCursorBox.cursors.hscroll set" \
      -yscrollcommand ".xfCursorBox.cursors.vscroll set"
    catch ".xfCursorBox.cursors.cursors config $tmpMessageOpt"

    # read cursor file
    if {"$xfCursorBoxFileCursor" != ""} {
      if {[catch "open $xfCursorBoxFileCursor r" cursorInFile]} {
        set xfCursorBoxFileCursor ""
        if {"[info commands XFAlertBox]" != ""} {
          XFAlertBox "$cursorInFile"
        } {
          puts stderr "$cursorInFile"
        }
      } {
        set cursorReadList [read $cursorInFile]
        close $cursorInFile
        foreach cursorLine [split $cursorReadList "\n"] {
          if {"[string trim $cursorLine]" != ""} {
            .xfCursorBox.cursors.cursors insert end $cursorLine
          }
        }
      }
    }
  
    # bindings
    global xfBind
    set tmpBinding "<Double-Button-3>"
    if {[info exists xfBind(configure)]} {
      set tmpBinding $xfBind(configure)
    }
    bind .xfCursorBox.fg.fg $tmpBinding "
      if {\"\[info commands XFColorBox\]\" != \"\"} {
        .xfCursorBox.fg.fg delete 0 end
        .xfCursorBox.fg.fg insert 0 \[XFColorBox $xfCursorBoxFileColor\]
        update idletask
        grab .xfCursorBox
        tkwait window .xfCursorBox
      }"
    catch "bind .xfCursorBox.fg.fg <Up> {}"
    bind .xfCursorBox.fg.fg <Down> {
      .xfCursorBox.bg.bg icursor 0
      focus .xfCursorBox.bg.bg}

    bind .xfCursorBox.bg.bg $tmpBinding "
      if {\"\[info commands XFColorBox\]\" != \"\"} {
        .xfCursorBox.bg.bg delete 0 end
        .xfCursorBox.bg.bg insert 0 \[XFColorBox $xfCursorBoxFileColor\]
       update idletask
       grab .xfCursorBox
       tkwait window .xfCursorBox
      }"
    bind .xfCursorBox.bg.bg <Up> {
      .xfCursorBox.fg.fg icursor 0
      focus .xfCursorBox.fg.fg}
    bind .xfCursorBox.bg.bg <Down> {
      .xfCursorBox.current.current icursor 0
      focus .xfCursorBox.current.current}

    bind .xfCursorBox.current.current <Up> {
      .xfCursorBox.bg.bg icursor 0
      focus .xfCursorBox.bg.bg}
    catch "bind .xfCursorBox.current.current <Down> {}"
  } {
    if {"[.xfCursorBox.fg.fg get]" != "" &&
        "[.xfCursorBox.bg.bg get]" != ""} {
     set tmpXFCursorBox \
         "[.xfCursorBox.current.current get] {[.xfCursorBox.fg.fg get]} {[.xfCursorBox.bg.bg get]}"
    } {
      if {"[.xfCursorBox.fg.fg get]" != ""} {
        set tmpXFCursorBox \
          "[.xfCursorBox.current.current get] {[.xfCursorBox.fg.fg get]}"
      } {
        if {"[.xfCursorBox.bg.bg get]" != ""} {
          set tmpXFCursorBox \
            "[.xfCursorBox.current.current get] {[.xfCursorBox.bg.bg get]}"
        } {
          set tmpXFCursorBox \
            "[.xfCursorBox.current.current get]"
        }
      }
    }
    if {"[winfo class $xfCursorBox(oldWidget)]" == "Text"} {
      catch "$xfCursorBox(oldWidget) delete 1.0 end"
      catch "$xfCursorBox(oldWidget) insert 1.0 [.xfCursorBox.current.current get]"
    } {
      if {"[winfo class $xfCursorBox(oldWidget)]" == "Entry"} {
        catch "$xfCursorBox(oldWidget) delete 0 end"
        catch "$xfCursorBox(oldWidget) insert 0 [.xfCursorBox.current.current get]"
      }
    }

    set xfCursorBox(oldWidget) $xfCursorBoxEntryW
  }

  .xfCursorBox.frame1.ok config \
    -command "
      global xfCursorBox
      if {\"\[.xfCursorBox.fg.fg get\]\" != \"\" &&
          \"\[.xfCursorBox.bg.bg get\]\" != \"\"} {
       set xfCursorBox(cursorName) \
           \"\[.xfCursorBox.current.current get\] {\[.xfCursorBox.fg.fg get\]} {\[.xfCursorBox.bg.bg get\]}\"
      } {
        if {\"\[.xfCursorBox.fg.fg get\]\" != \"\"} {
          set xfCursorBox(cursorName) \
            \"\[.xfCursorBox.current.current get\] {\[.xfCursorBox.fg.fg get\]}\"
        } {
          if {\"\[.xfCursorBox.bg.bg get\]\" != \"\"} {
            set xfCursorBox(cursorName) \
              \"\[.xfCursorBox.current.current get\] {\[.xfCursorBox.bg.bg get\]}\"
          } {
            set xfCursorBox(cursorName) \
              \"\[.xfCursorBox.current.current get\]\"
          }
        }
      }
      if {\"$xfCursorBoxEntryW\" != \"\"} {
        if {\"\[winfo class $xfCursorBoxEntryW\]\" == \"Text\"} {
          catch \"$xfCursorBoxEntryW delete 1.0 end\"
          catch \"$xfCursorBoxEntryW insert 1.0 \\\"\$xfCursorBox(cursorName)\\\"\"
        } {
          if {\"\[winfo class $xfCursorBoxEntryW\]\" == \"Entry\"} {
            catch \"$xfCursorBoxEntryW delete 0 end\"
            catch \"$xfCursorBoxEntryW insert 0 \\\"\$xfCursorBox(cursorName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfCursorBox}
      } {
        catch {destroy .xfCursorBox}
      }"

  .xfCursorBox.frame1.cancel config \
    -command "
      global xfCursorBox
      set xfCursorBox(cursorName) {}
      if {\"$xfCursorBoxTargetW\" != \"\"} {
        catch \"$xfCursorBoxTargetW config -\[string tolower $xfCursorBoxMessage\] $xfCursorBoxSavedCursor\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfCursorBox}
      } {
        catch {destroy .xfCursorBox}
      }"

  .xfCursorBox.current.labelcurrent config \
    -text "$xfCursorBoxMessage:"

  # bindings
  bind .xfCursorBox.fg.fg <Return> "
    XFCursorBoxSetCursor \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\"
    .xfCursorBox.bg.bg icursor 0
    focus .xfCursorBox.bg.bg"

  bind .xfCursorBox.bg.bg <Return> "
    XFCursorBoxSetCursor \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\"
    .xfCursorBox.current.current icursor 0
    focus .xfCursorBox.current.current"

  bind .xfCursorBox.current.current <Return> "
    XFCursorBoxSetCursor \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\""

  bind .xfCursorBox.cursors.cursors <Double-1> "
    XFCursorBoxSelectCursor %W \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\" %y
    global xfCursorBox
    if {\"\[.xfCursorBox.fg.fg get\]\" != \"\" &&
        \"\[.xfCursorBox.bg.bg get\]\" != \"\"} {
     set xfCursorBox(cursorName) \
         \"\[.xfCursorBox.current.current get\] {\[.xfCursorBox.fg.fg get\]} {\[.xfCursorBox.bg.bg get\]}\"
    } {
      if {\"\[.xfCursorBox.fg.fg get\]\" != \"\"} {
         set xfCursorBox(cursorName) \
           \"\[.xfCursorBox.current.current get\] {\[.xfCursorBox.fg.fg get\]}\"
      } {
        if {\"\[.xfCursorBox.bg.bg get\]\" != \"\"} {
          set xfCursorBox(cursorName) \
            \"\[.xfCursorBox.current.current get\] {\[.xfCursorBox.bg.bg get\]}\"
        } {
          set xfCursorBox(cursorName) \
            \"\[.xfCursorBox.current.current get\]\"
        }
      }
    }
    if {\"$xfCursorBoxEntryW\" != \"\"} {
      if {\"\[winfo class $xfCursorBoxEntryW\]\" == \"Text\"} {
        catch \"$xfCursorBoxEntryW delete 1.0 end\"
        catch \"$xfCursorBoxEntryW insert 1.0 \\\"\$xfCursorBox(cursorName)\\\"\"
      } {
        if {\"\[winfo class $xfCursorBoxEntryW\]\" == \"Entry\"} {
          catch \"$xfCursorBoxEntryW delete 0 end\"
          catch \"$xfCursorBoxEntryW insert 0 \\\"\$xfCursorBox(cursorName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .xfCursorBox}
    } {
      catch {destroy .xfCursorBox}
    }"

  bind .xfCursorBox.cursors.cursors <ButtonPress-1> "
    XFCursorBoxSelectCursor %W \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\" %y"
  bind .xfCursorBox.cursors.cursors <Button1-Motion> "
    XFCursorBoxSelectCursor %W \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\" %y"
  bind .xfCursorBox.cursors.cursors <Shift-ButtonPress-1> "
    XFCursorBoxSelectCursor %W \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\" %y"
  bind .xfCursorBox.cursors.cursors <Shift-Button1-Motion> "
    XFCursorBoxSelectCursor %W \"$xfCursorBoxMessage\" \"$xfCursorBoxTargetW\" %y"

  # set up current value
  .xfCursorBox.current.current delete 0 end
  if {"$xfCursorBoxEntryW" != ""} {
    if {"[winfo class $xfCursorBoxEntryW]" == "Text"} {
      .xfCursorBox.fg.fg insert 0 [lindex [$xfCursorBoxEntryW get 1.0 end] 1]
      .xfCursorBox.bg.bg insert 0 [lindex [$xfCursorBoxEntryW get 1.0 end] 2]
      .xfCursorBox.current.current insert 0 [lindex [$xfCursorBoxEntryW get 1.0 end] 0]
    } {
      if {"[winfo class $xfCursorBoxEntryW]" == "Entry"} {
        .xfCursorBox.fg.fg insert 0 [lindex [$xfCursorBoxEntryW get] 1]
        .xfCursorBox.bg.bg insert 0 [lindex [$xfCursorBoxEntryW get] 2]
        .xfCursorBox.current.current insert 0 [lindex [$xfCursorBoxEntryW get] 0]
      }
    }
  }

  # packing
  pack append .xfCursorBox.frame1 \
              .xfCursorBox.frame1.ok {left fill expand} \
              .xfCursorBox.frame1.cancel {left fill expand}
  pack append .xfCursorBox.current \
              .xfCursorBox.current.labelcurrent {left} \
              .xfCursorBox.current.current {left fill expand}
  pack append .xfCursorBox.fg \
              .xfCursorBox.fg.labelfg {left} \
              .xfCursorBox.fg.fg {left fill expand}
  pack append .xfCursorBox.bg \
              .xfCursorBox.bg.labelbg {left} \
              .xfCursorBox.bg.bg {left fill expand}
  pack append .xfCursorBox.cursors \
              .xfCursorBox.cursors.vscroll "$xfCursorBox(scrollSide) filly" \
              .xfCursorBox.cursors.hscroll {bottom fillx} \
              .xfCursorBox.cursors.cursors {left fill expand}

  if {"$xfCursorBoxFileCursor" != ""} {
    pack append .xfCursorBox \
                .xfCursorBox.frame1 {bottom fillx} \
                .xfCursorBox.current {bottom fillx} \
                .xfCursorBox.bg {bottom fillx} \
                .xfCursorBox.fg {bottom fillx} \
                .xfCursorBox.demo {bottom fillx} \
                .xfCursorBox.cursors {left expand fill}
  } {
    wm geometry .xfCursorBox 400x110
    pack append .xfCursorBox \
                .xfCursorBox.frame1 {bottom fillx} \
                .xfCursorBox.current {bottom fillx} \
                .xfCursorBox.bg {bottom fillx} \
                .xfCursorBox.fg {bottom fillx} \
                .xfCursorBox.demo {bottom fill expand}
  }
  catch "wm deiconify .xfCursorBox"

  if {"$xfCursorBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .xfCursorBox
    tkwait window .xfCursorBox

    return $xfCursorBox(cursorName)
  }
}

##########
# Procedure: XFCursorBoxSelectCursor
# Description: select cursor for cursor composing
# Arguments: xfCursorBoxW - the widget
#            xfCursorBoxMessage - the message for the cursor
#            xfCursorBoxTargetW - the widget we configure
#            xfCursorBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFCursorBoxSelectCursor {xfCursorBoxW xfCursorBoxMessage xfCursorBoxTargetW xfCursorBoxY} {# xf ignore me 6

  set xfCursorBoxNearest [$xfCursorBoxW nearest $xfCursorBoxY]
  if {$xfCursorBoxNearest >= 0} {
    $xfCursorBoxW select anchor $xfCursorBoxNearest
    $xfCursorBoxW select set $xfCursorBoxNearest
    .xfCursorBox.current.current delete 0 end
    .xfCursorBox.current.current insert 0 [$xfCursorBoxW get $xfCursorBoxNearest]
    XFCursorBoxSetCursor "$xfCursorBoxMessage" "$xfCursorBoxTargetW"
  }
}

##########
# Procedure: XFCursorBoxSetCursor
# Description: set cursor for the widget
# Arguments: xfCursorBoxMessage - the message for the cursor
#            xfCursorBoxTargetW - the widget we configure
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFCursorBoxSetCursor {xfCursorBoxMessage xfCursorBoxTargetW} {# xf ignore me 6
  global tcl_platform

  if {"$tcl_platform(platform)" == "windows"} {
    catch ".xfCursorBox.demo config -cursor \
      \"{[.xfCursorBox.current.current get]}\""
    if {"$xfCursorBoxTargetW" != ""} {
      catch "$xfCursorBoxTargetW config -[string tolower $xfCursorBoxMessage] \
        \"{[.xfCursorBox.current.current get]}\""
    }
    return
  }

  if {"[.xfCursorBox.current.current get]" != ""} {
    if {"[.xfCursorBox.fg.fg get]" != "" &&
        "[.xfCursorBox.bg.bg get]" != ""} {
      catch ".xfCursorBox.demo config -cursor \
        \"{[.xfCursorBox.current.current get]} {[.xfCursorBox.fg.fg get]} {[.xfCursorBox.bg.bg get]}\""
      if {"$xfCursorBoxTargetW" != ""} {
        catch "$xfCursorBoxTargetW config -[string tolower $xfCursorBoxMessage] \
          \"{[.xfCursorBox.current.current get]} {[.xfCursorBox.fg.fg get]} {[.xfCursorBox.bg.bg get]}\""
      }
    } {
      if {"[.xfCursorBox.fg.fg get]" != ""} {
        catch ".xfCursorBox.demo config -cursor \
          \"{[.xfCursorBox.current.current get]} {[.xfCursorBox.fg.fg get]}\""
        if {"$xfCursorBoxTargetW" != ""} {
          catch "$xfCursorBoxTargetW config -[string tolower $xfCursorBoxMessage] \
            \"{[.xfCursorBox.current.current get]} {[.xfCursorBox.fg.fg get]}\""
        }
      } {
        if {"[.xfCursorBox.bg.bg get]" != ""} {
          catch ".xfCursorBox.demo config -cursor \
            \"{[.xfCursorBox.current.current get]} {[.xfCursorBox.bg.bg get]}\""
          if {"$xfCursorBoxTargetW" != ""} {
            catch "$xfCursorBoxTargetW config -[string tolower $xfCursorBoxMessage] \
              \"{[.xfCursorBox.current.current get]} {[.xfCursorBox.bg.bg get]}\""
          }
        } {
          catch ".xfCursorBox.demo config -cursor \
            \"{[.xfCursorBox.current.current get]}\""
          if {"$xfCursorBoxTargetW" != ""} {
            catch "$xfCursorBoxTargetW config -[string tolower $xfCursorBoxMessage] \
              \"{[.xfCursorBox.current.current get]}\""
          }
        }
      }
    }
  }
}

# eof

