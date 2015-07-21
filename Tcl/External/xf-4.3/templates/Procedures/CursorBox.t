# XFNoParsing
# Program: template
# Description: cursor selection
#
# $Header: CursorBox.t[2.3] Wed Mar 10 12:02:47 1993 garfield@garfield frozen $

global cursorBox
set cursorBox(activeBackground) ""
set cursorBox(activeForeground) ""
set cursorBox(background) ""
set cursorBox(font) ""
set cursorBox(foreground) ""
set cursorBox(scrollActiveForeground) ""
set cursorBox(scrollBackground) ""
set cursorBox(scrollForeground) ""
set cursorBox(scrollSide) left
set cursorBox(cursorName) ""

proc CursorBox {{cursorBoxFileCursor "/usr/local/lib/xf/lib/Cursors"} {cursorBoxFileColor "/usr/local/lib/xf/lib/Colors"} {cursorBoxMessage "Cursor"} {cursorBoxEntryW ""} {cursorBoxTargetW ""}} {# xf ignore me 5
##########
# Procedure: CursorBox
# Description: select a cursor
# Arguments: {cursorBoxFileCursor} - the color file with all cursornames
#            {cursorBoxFileColor} - the color file with all colornames
#            {cursorBoxMessage} - a message to display
#            {cursorBoxEntryW} - the widget name for the resulting color name
#            {cursorBoxTargetW} - the widget we configure
# Returns: cursorname, or nothing
# Sideeffects: none
##########
#
# global cursorBox(activeBackground) - active background color
# global cursorBox(activeForeground) - active foreground color
# global cursorBox(background) - background color
# global cursorBox(font) - text font
# global cursorBox(foreground) - foreground color
# global cursorBox(scrollActiveForeground) - scrollbar active background color
# global cursorBox(scrollBackground) - scrollbar background color
# global cursorBox(scrollForeground) - scrollbar foreground color
# global cursorBox(scrollSide) - side where scrollbar is located

  global cursorBox

  set cursorBox(cursorName) ""

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$cursorBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$cursorBox(activeBackground)\" "
  }
  if {"$cursorBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$cursorBox(activeForeground)\" "
  }
  if {"$cursorBox(background)" != ""} {
    append tmpButtonOpt "-background \"$cursorBox(background)\" "
    append tmpFrameOpt "-background \"$cursorBox(background)\" "
    append tmpMessageOpt "-background \"$cursorBox(background)\" "
  }
  if {"$cursorBox(font)" != ""} {
    append tmpButtonOpt "-font \"$cursorBox(font)\" "
    append tmpMessageOpt "-font \"$cursorBox(font)\" "
  }
  if {"$cursorBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$cursorBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$cursorBox(foreground)\" "
  }
  if {"$cursorBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$cursorBox(scrollActiveForeground)\" "
  }
  if {"$cursorBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$cursorBox(scrollBackground)\" "
  }
  if {"$cursorBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$cursorBox(scrollForeground)\" "
  }

  # get cursor file name
  if {!([file exists $cursorBoxFileCursor] &&
        [file readable $cursorBoxFileCursor])} {
    set cursorBoxFileCursor ""
  }
  if {"$cursorBoxFileCursor" == ""} {
    global env
    if {[info exists env(XF_CURSOR_FILE)]} {
      if {[file exists $env(XF_CURSOR_FILE)] &&
          [file readable $env(XF_CURSOR_FILE)]} {
        set cursorBoxFileCursor $env(XF_CURSOR_FILE)
      }
    }
  }

  # get color file name
  if {!([file exists $cursorBoxFileColor] &&
        [file readable $cursorBoxFileColor])} {
    set cursorBoxFileColor ""
  }
  if {"$cursorBoxFileColor" == ""} {
    global env
    if {[info exists env(XF_COLOR_FILE)]} {
      if {[file exists $env(XF_COLOR_FILE)] &&
          [file readable $env(XF_COLOR_FILE)]} {
        set cursorBoxFileColor $env(XF_COLOR_FILE)
      }
    }
  }
  if {"$cursorBoxMessage" == ""} {
    set cursorBoxMessage "Cursor"
  }

  # save the the current widget cursor
  if {"$cursorBoxTargetW" != ""} {
    if {[catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage]" result]} {
      set cursorBoxSavedCursor ""
    } {
      set cursorBoxSavedCursor [lindex $result 4]
    }
  } {
    set cursorBoxSavedCursor ""
  }

  # look if there is already a font window
  if {"[info commands .cursorBox]" == ""} {
    # build widget structure

    # start build of toplevel
    if {"[info commands XFDestroy]" != ""} {
      catch {XFDestroy .cursorBox}
    } {
      catch {destroy .cursorBox}
    }
    toplevel .cursorBox \
      -borderwidth 0
    catch ".cursorBox config $tmpFrameOpt"
    wm geometry .cursorBox 400x300
    wm title .cursorBox {Cursor box}
    wm maxsize .cursorBox 1000 1000
    wm minsize .cursorBox 100 100
    # end build of toplevel

    set cursorBox(oldWidget) $cursorBoxEntryW

    frame .cursorBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".cursorBox.frame1 config $tmpFrameOpt"
 
    button .cursorBox.frame1.ok \
      -text "OK"
    catch ".cursorBox.frame1.ok config $tmpButtonOpt"

    button .cursorBox.frame1.cancel \
      -text "Cancel"
    catch ".cursorBox.frame1.cancel config $tmpButtonOpt"

    label .cursorBox.demo \
      -relief raised \
      -text "This text shows the results :-) (enter this widget)"
    catch ".cursorBox.demo config $tmpMessageOpt"

    frame .cursorBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".cursorBox.current config $tmpFrameOpt"

    label .cursorBox.current.labelcurrent \
      -relief raised
    catch ".cursorBox.current.labelcurrent config $tmpMessageOpt"

    entry .cursorBox.current.current \
      -relief raised
    catch ".cursorBox.current.current config $tmpMessageOpt"

    frame .cursorBox.fg \
      -borderwidth 0 \
      -relief raised
    catch ".cursorBox config.fg $tmpFrameOpt"

    label .cursorBox.fg.labelfg \
      -relief raised \
      -text "Foreground:"
    catch ".cursorBox.fg.labelfg config $tmpMessageOpt"

    entry .cursorBox.fg.fg \
      -relief raised
    catch ".cursorBox.fg.fg config $tmpMessageOpt"

    frame .cursorBox.bg \
      -borderwidth 0 \
      -relief raised
    catch ".cursorBox.bg config $tmpFrameOpt"

    label .cursorBox.bg.labelbg \
      -relief raised \
      -text "Background:"
    catch ".cursorBox.bg.labelbg config $tmpMessageOpt"

    entry .cursorBox.bg.bg \
      -relief raised
    catch ".cursorBox.bg.bg config $tmpMessageOpt"

    frame .cursorBox.cursors \
      -borderwidth 0 \
      -relief raised
    catch ".cursorBox.cursor config $tmpFrameOpt"

    scrollbar .cursorBox.cursors.vscroll \
      -relief raised \
      -command ".cursorBox.cursors.cursors yview"
    catch ".cursorBox.cursors.vscroll config $tmpScrollOpt"

    scrollbar .cursorBox.cursors.hscroll \
      -orient horiz \
      -relief raised \
      -command ".cursorBox.cursors.cursors xview"
    catch ".cursorBox.cursors.hscroll config $tmpScrollOpt"

    listbox .cursorBox.cursors.cursors \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".cursorBox.cursors.hscroll set" \
      -yscrollcommand ".cursorBox.cursors.vscroll set"
    catch ".cursorBox.cursors.cursors config $tmpMessageOpt"

    # read cursor file
    if {"$cursorBoxFileCursor" != ""} {
      if {[catch "open $cursorBoxFileCursor r" cursorInFile]} {
        set cursorBoxFileCursor ""
        if {"[info commands AlertBox]" != ""} {
          AlertBox "$cursorInFile"
        } {
          puts stderr "$cursorInFile"
        }
      } {
        set cursorReadList [read $cursorInFile]
        close $cursorInFile
        foreach cursorLine [split $cursorReadList "\n"] {
          if {"[string trim $cursorLine]" != ""} {
            .cursorBox.cursors.cursors insert end $cursorLine
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
    bind .cursorBox.fg.fg $tmpBinding "
      if {\"\[info commands ColorBox\]\" != \"\"} {
        .cursorBox.fg.fg delete 0 end
        .cursorBox.fg.fg insert 0 \[ColorBox $cursorBoxFileColor\]
        update idletask
        grab .cursorBox
        tkwait window .cursorBox
      }"
    catch "bind .cursorBox.fg.fg <Up> {}"
    bind .cursorBox.fg.fg <Down> {
      global tkVersion
      if {$tkVersion >= 3.0} {
        .cursorBox.bg.bg icursor 0
      } {
        .cursorBox.bg.bg cursor 0
      }
      focus .cursorBox.bg.bg}

    bind .cursorBox.bg.bg $tmpBinding "
      if {\"\[info commands ColorBox\]\" != \"\"} {
        .cursorBox.bg.bg delete 0 end
        .cursorBox.bg.bg insert 0 \[ColorBox $cursorBoxFileColor\]
       update idletask
       grab .cursorBox
       tkwait window .cursorBox
      }"
    bind .cursorBox.bg.bg <Up> {
      global tkVersion
      if {$tkVersion >= 3.0} {
        .cursorBox.fg.fg icursor 0
      } {
        .cursorBox.fg.fg cursor 0
      }
      focus .cursorBox.fg.fg}
    bind .cursorBox.bg.bg <Down> {
      global tkVersion
      if {$tkVersion >= 3.0} {
        .cursorBox.current.current icursor 0
      } {
        .cursorBox.current.current cursor 0
      }
      focus .cursorBox.current.current}

    bind .cursorBox.current.current <Up> {
      global tkVersion
      if {$tkVersion >= 3.0} {
        .cursorBox.bg.bg icursor 0
      } {
        .cursorBox.bg.bg cursor 0
      }
      focus .cursorBox.bg.bg}
    catch "bind .cursorBox.current.current <Down> {}"
  } {
    if {"[.cursorBox.fg.fg get]" != "" &&
        "[.cursorBox.bg.bg get]" != ""} {
     set tmpCursorBox \
         "[.cursorBox.current.current get] {[.cursorBox.fg.fg get]} {[.cursorBox.bg.bg get]}"
    } {
      if {"[.cursorBox.fg.fg get]" != ""} {
        set tmpCursorBox \
          "[.cursorBox.current.current get] {[.cursorBox.fg.fg get]}"
      } {
        if {"[.cursorBox.bg.bg get]" != ""} {
          set tmpCursorBox \
            "[.cursorBox.current.current get] {[.cursorBox.bg.bg get]}"
        } {
          set tmpCursorBox \
            "[.cursorBox.current.current get]"
        }
      }
    }
    if {"[winfo class $cursorBox(oldWidget)]" == "Text"} {
      catch "$cursorBox(oldWidget) delete 1.0 end"
      catch "$cursorBox(oldWidget) insert 1.0 [.cursorBox.current.current get]"
    } {
      if {"[winfo class $cursorBox(oldWidget)]" == "Entry"} {
        catch "$cursorBox(oldWidget) delete 0 end"
        catch "$cursorBox(oldWidget) insert 0 [.cursorBox.current.current get]"
      }
    }

    set cursorBox(oldWidget) $cursorBoxEntryW
  }

  .cursorBox.frame1.ok config \
    -command "
      global cursorBox
      if {\"\[.cursorBox.fg.fg get\]\" != \"\" &&
          \"\[.cursorBox.bg.bg get\]\" != \"\"} {
       set cursorBox(cursorName) \
           \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]} {\[.cursorBox.bg.bg get\]}\"
      } {
        if {\"\[.cursorBox.fg.fg get\]\" != \"\"} {
          set cursorBox(cursorName) \
            \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]}\"
        } {
          if {\"\[.cursorBox.bg.bg get\]\" != \"\"} {
            set cursorBox(cursorName) \
              \"\[.cursorBox.current.current get\] {\[.cursorBox.bg.bg get\]}\"
          } {
            set cursorBox(cursorName) \
              \"\[.cursorBox.current.current get\]\"
          }
        }
      }
      if {\"$cursorBoxEntryW\" != \"\"} {
        if {\"\[winfo class $cursorBoxEntryW\]\" == \"Text\"} {
          catch \"$cursorBoxEntryW delete 1.0 end\"
          catch \"$cursorBoxEntryW insert 1.0 \\\"\$cursorBox(cursorName)\\\"\"
        } {
          if {\"\[winfo class $cursorBoxEntryW\]\" == \"Entry\"} {
            catch \"$cursorBoxEntryW delete 0 end\"
            catch \"$cursorBoxEntryW insert 0 \\\"\$cursorBox(cursorName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .cursorBox}
      } {
        catch {destroy .cursorBox}
      }"

  .cursorBox.frame1.cancel config \
    -command "
      global cursorBox
      set cursorBox(cursorName) {}
      if {\"$cursorBoxTargetW\" != \"\"} {
        catch \"$cursorBoxTargetW config -\[string tolower $cursorBoxMessage\] $cursorBoxSavedCursor\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .cursorBox}
      } {
        catch {destroy .cursorBox}
      }"

  .cursorBox.current.labelcurrent config \
    -text "$cursorBoxMessage:"

  # bindings
  bind .cursorBox.fg.fg <Return> "
    CursorBoxSetCursor \"$cursorBoxMessage\" \"$cursorBoxTargetW\"
    global tkVersion
    if {\$tkVersion >= 3.0} {
      .cursorBox.bg.bg icursor 0
    } {
      .cursorBox.bg.bg cursor 0
    }
    focus .cursorBox.bg.bg"

  bind .cursorBox.bg.bg <Return> "
    CursorBoxSetCursor \"$cursorBoxMessage\" \"$cursorBoxTargetW\"
    global tkVersion
    if {\$tkVersion >= 3.0} {
      .cursorBox.current.current icursor 0
    } {
      .cursorBox.current.current cursor 0
    }
    focus .cursorBox.current.current"

  bind .cursorBox.current.current <Return> "
    CursorBoxSetCursor \"$cursorBoxMessage\" \"$cursorBoxTargetW\""

  bind .cursorBox.cursors.cursors <Double-1> "
    CursorBoxSelectCursor %W \"$cursorBoxMessage\" \"$cursorBoxTargetW\" %y
    global cursorBox
    if {\"\[.cursorBox.fg.fg get\]\" != \"\" &&
        \"\[.cursorBox.bg.bg get\]\" != \"\"} {
     set cursorBox(cursorName) \
         \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]} {\[.cursorBox.bg.bg get\]}\"
    } {
      if {\"\[.cursorBox.fg.fg get\]\" != \"\"} {
         set cursorBox(cursorName) \
           \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]}\"
      } {
        if {\"\[.cursorBox.bg.bg get\]\" != \"\"} {
          set cursorBox(cursorName) \
            \"\[.cursorBox.current.current get\] {\[.cursorBox.bg.bg get\]}\"
        } {
          set cursorBox(cursorName) \
            \"\[.cursorBox.current.current get\]\"
        }
      }
    }
    if {\"$cursorBoxEntryW\" != \"\"} {
      if {\"\[winfo class $cursorBoxEntryW\]\" == \"Text\"} {
        catch \"$cursorBoxEntryW delete 1.0 end\"
        catch \"$cursorBoxEntryW insert 1.0 \\\"\$cursorBox(cursorName)\\\"\"
      } {
        if {\"\[winfo class $cursorBoxEntryW\]\" == \"Entry\"} {
          catch \"$cursorBoxEntryW delete 0 end\"
          catch \"$cursorBoxEntryW insert 0 \\\"\$cursorBox(cursorName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .cursorBox}
    } {
      catch {destroy .cursorBox}
    }"

  bind .cursorBox.cursors.cursors <ButtonPress-1> "
    CursorBoxSelectCursor %W \"$cursorBoxMessage\" \"$cursorBoxTargetW\" %y"
  bind .cursorBox.cursors.cursors <Button1-Motion> "
    CursorBoxSelectCursor %W \"$cursorBoxMessage\" \"$cursorBoxTargetW\" %y"
  bind .cursorBox.cursors.cursors <Shift-ButtonPress-1> "
    CursorBoxSelectCursor %W \"$cursorBoxMessage\" \"$cursorBoxTargetW\" %y"
  bind .cursorBox.cursors.cursors <Shift-Button1-Motion> "
    CursorBoxSelectCursor %W \"$cursorBoxMessage\" \"$cursorBoxTargetW\" %y"

  # set up current value
  .cursorBox.current.current delete 0 end
  if {"$cursorBoxEntryW" != ""} {
    if {"[winfo class $cursorBoxEntryW]" == "Text"} {
      .cursorBox.fg.fg insert 0 [lindex [$cursorBoxEntryW get 1.0 end] 1]
      .cursorBox.bg.bg insert 0 [lindex [$cursorBoxEntryW get 1.0 end] 2]
      .cursorBox.current.current insert 0 [lindex [$cursorBoxEntryW get 1.0 end] 0]
    } {
      if {"[winfo class $cursorBoxEntryW]" == "Entry"} {
        .cursorBox.fg.fg insert 0 [lindex [$cursorBoxEntryW get] 1]
        .cursorBox.bg.bg insert 0 [lindex [$cursorBoxEntryW get] 2]
        .cursorBox.current.current insert 0 [lindex [$cursorBoxEntryW get] 0]
      }
    }
  }

  # packing
  pack append .cursorBox.frame1 \
              .cursorBox.frame1.ok {left fill expand} \
              .cursorBox.frame1.cancel {left fill expand}
  pack append .cursorBox.current \
              .cursorBox.current.labelcurrent {left} \
              .cursorBox.current.current {left fill expand}
  pack append .cursorBox.fg \
              .cursorBox.fg.labelfg {left} \
              .cursorBox.fg.fg {left fill expand}
  pack append .cursorBox.bg \
              .cursorBox.bg.labelbg {left} \
              .cursorBox.bg.bg {left fill expand}
  pack append .cursorBox.cursors \
              .cursorBox.cursors.vscroll "$cursorBox(scrollSide) filly" \
              .cursorBox.cursors.hscroll {bottom fillx} \
              .cursorBox.cursors.cursors {left fill expand}

  if {"$cursorBoxFileCursor" != ""} {
    pack append .cursorBox \
                .cursorBox.frame1 {bottom fillx} \
                .cursorBox.current {bottom fillx} \
                .cursorBox.bg {bottom fillx} \
                .cursorBox.fg {bottom fillx} \
                .cursorBox.demo {bottom fillx} \
                .cursorBox.cursors {left expand fill}
  } {
    wm geometry .cursorBox 400x110
    pack append .cursorBox \
                .cursorBox.frame1 {bottom fillx} \
                .cursorBox.current {bottom fillx} \
                .cursorBox.bg {bottom fillx} \
                .cursorBox.fg {bottom fillx} \
                .cursorBox.demo {bottom fill expand}
  }
  catch "wm deiconify .cursorBox"

  if {"$cursorBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .cursorBox
    tkwait window .cursorBox

    return $cursorBox(cursorName)
  }
}

##########
# Procedure: CursorBoxSelectCursor
# Description: select cursor for cursor composing
# Arguments: cursorBoxW - the widget
#            cursorBoxMessage - the message for the cursor
#            cursorBoxTargetW - the widget we configure
#            cursorBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc CursorBoxSelectCursor {cursorBoxW cursorBoxMessage cursorBoxTargetW cursorBoxY} {# xf ignore me 6

  set cursorBoxNearest [$cursorBoxW nearest $cursorBoxY]
  if {$cursorBoxNearest >= 0} {
    $cursorBoxW select from $cursorBoxNearest
    $cursorBoxW select to $cursorBoxNearest
    .cursorBox.current.current delete 0 end
    .cursorBox.current.current insert 0 [$cursorBoxW get $cursorBoxNearest]
    CursorBoxSetCursor "$cursorBoxMessage" "$cursorBoxTargetW"
  }
}

##########
# Procedure: CursorBoxSetCursor
# Description: set cursor for the widget
# Arguments: cursorBoxMessage - the message for the cursor
#            cursorBoxTargetW - the widget we configure
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc CursorBoxSetCursor {cursorBoxMessage cursorBoxTargetW} {# xf ignore me 6
  global tcl_platform

  if {"$tcl_platform(platform)" == "windows"} {
    catch ".cursorBox.demo config -cursor \
      \"{[.cursorBox.current.current get]}\""
    if {"$cursorBoxTargetW" != ""} {
      catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage] \
        \"{[.cursorBox.current.current get]}\""
    }
    return
  }

  if {"[.cursorBox.current.current get]" != ""} {
    if {"[.cursorBox.fg.fg get]" != "" &&
        "[.cursorBox.bg.bg get]" != ""} {
      catch ".cursorBox.demo config -cursor \
        \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]} {[.cursorBox.bg.bg get]}\""
      if {"$cursorBoxTargetW" != ""} {
        catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage] \
          \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]} {[.cursorBox.bg.bg get]}\""
      }
    } {
      if {"[.cursorBox.fg.fg get]" != ""} {
        catch ".cursorBox.demo config -cursor \
          \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]}\""
        if {"$cursorBoxTargetW" != ""} {
          catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage] \
            \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]}\""
        }
      } {
        if {"[.cursorBox.bg.bg get]" != ""} {
          catch ".cursorBox.demo config -cursor \
            \"{[.cursorBox.current.current get]} {[.cursorBox.bg.bg get]}\""
          if {"$cursorBoxTargetW" != ""} {
            catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage] \
              \"{[.cursorBox.current.current get]} {[.cursorBox.bg.bg get]}\""
          }
        } {
          catch ".cursorBox.demo config -cursor \
            \"{[.cursorBox.current.current get]}\""
          if {"$cursorBoxTargetW" != ""} {
            catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage] \
              \"{[.cursorBox.current.current get]}\""
          }
        }
      }
    }
  }
}

# eof

