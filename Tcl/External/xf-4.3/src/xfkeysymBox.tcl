# XFNoParsing
# Program: template
# Description: keysym selection
#
# $Header: xfkeysymBox.tcl[2.3] Wed Mar 10 12:06:28 1993 garfield@garfield frozen $

global xfKeysymBox
set xfKeysymBox(activeBackground) ""
set xfKeysymBox(activeForeground) ""
set xfKeysymBox(background) ""
set xfKeysymBox(font) ""
set xfKeysymBox(foreground) ""
set xfKeysymBox(overwrite) 0
set xfKeysymBox(scrollActiveForeground) ""
set xfKeysymBox(scrollBackground) ""
set xfKeysymBox(scrollForeground) ""
set xfKeysymBox(keysymName) ""
set xfKeysymBox(focus) ""

proc XFKeysymBox {{xfKeysymBoxFileKeysym "/usr/local/lib/xf/lib/Keysyms"} {xfKeysymBoxMessage "Keysym"} {xfKeysymBoxEntryW ""}} {# xf ignore me 5
##########
# Procedure: XFKeysymBox
# Description: select a keysym
# Arguments: {xfKeysymBoxFileKeysym} - the keysym file with all keysyms
#            {xfKeysymBoxMessage} - a message to display
#            {xfKeysymBoxEntryW} - the widget name for the resulting keysym name
# Returns: keysym, or nothing
# Sideeffects: none
##########
# 
# global xfKeysymBox(activeBackground) - active background color
# global xfKeysymBox(activeForeground) - active foreground color
# global xfKeysymBox(background) - background color
# global xfKeysymBox(font) - text font
# global xfKeysymBox(foreground) - foreground color
# global xfKeysymBox(overwrite) - insert or overwrite in xfKeysymBoxEntryW
# global xfKeysymBox(scrollActiveForeground) - scrollbar active background color
# global xfKeysymBox(scrollBackground) - scrollbar background color
# global xfKeysymBox(scrollForeground) - scrollbar foreground color
# global xfKeysymBox(scrollSide) - side where scrollbar is located

  global xfKeysymBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$xfKeysymBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfKeysymBox(activeBackground)\" "
  }
  if {"$xfKeysymBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfKeysymBox(activeForeground)\" "
  }
  if {"$xfKeysymBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfKeysymBox(background)\" "
    append tmpFrameOpt "-background \"$xfKeysymBox(background)\" "
    append tmpMessageOpt "-background \"$xfKeysymBox(background)\" "
  }
  if {"$xfKeysymBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfKeysymBox(font)\" "
    append tmpMessageOpt "-font \"$xfKeysymBox(font)\" "
  }
  if {"$xfKeysymBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfKeysymBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfKeysymBox(foreground)\" "
  }
  if {"$xfKeysymBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfKeysymBox(scrollActiveForeground)\" "
  }
  if {"$xfKeysymBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfKeysymBox(scrollBackground)\" "
  }
  if {"$xfKeysymBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfKeysymBox(scrollForeground)\" "
  }

  # get keysym file name
  if {!([file exists $xfKeysymBoxFileKeysym] &&
        [file readable $xfKeysymBoxFileKeysym])} {
    set xfKeysymBoxFileKeysym ""
  }
  if {"$xfKeysymBoxFileKeysym" == ""} {
    global env
    if {[info exists env(XF_KEYSYM_FILE)]} {
      if {[file exists $env(XF_KEYSYM_FILE)] &&
          [file readable $env(XF_KEYSYM_FILE)]} {
        set xfKeysymBoxFileKeysym $env(XF_KEYSYM_FILE)
      }
    }
  }
  if {"$xfKeysymBoxMessage" == ""} {
    set xfKeysymBoxMessage "Keysym"
  }

  # look if there is already a keysym window
  if {"[info commands .xfKeysymBox]" == ""} {
    # build widget structure

    XFTmpltToplevel .xfKeysymBox 400x300 {XF keysym box}

    frame .xfKeysymBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".xfKeysymBox.frame1 config $tmpFrameOpt"

    button .xfKeysymBox.frame1.ok \
      -text "OK"
    catch ".xfKeysymBox.frame1.ok config $tmpButtonOpt"

    button .xfKeysymBox.frame1.cancel \
      -text "Cancel"
    catch ".xfKeysymBox.frame1.cancel config $tmpButtonOpt"

    label .xfKeysymBox.example \
      -relief raised \
      -text "Press key to insert keysym!"
    catch ".xfKeysymBox.example config $tmpMessageOpt"

    frame .xfKeysymBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".xfKeysymBox.current config $tmpFrameOpt"

    label .xfKeysymBox.current.labelcurrent \
      -relief raised \
      -text "$xfKeysymBoxMessage:"
    catch ".xfKeysymBox.current.labelcurrent config $tmpMessageOpt"

    entry .xfKeysymBox.current.current \
      -relief raised
    catch ".xfKeysymBox.current.current config $tmpMessageOpt"

    frame .xfKeysymBox.keysyms \
      -borderwidth 0 \
      -relief raised
    catch ".xfKeysymBox.keysyms config $tmpFrameOpt"

    scrollbar .xfKeysymBox.keysyms.vscroll \
      -relief raised \
      -command ".xfKeysymBox.keysyms.keysyms yview"
    catch ".xfKeysymBox.keysyms.vscroll config $tmpScrollOpt"

    scrollbar .xfKeysymBox.keysyms.hscroll \
      -orient horiz \
      -relief raised \
      -command ".xfKeysymBox.keysyms.keysyms xview"
    catch ".xfKeysymBox.keysyms.hscroll config $tmpScrollOpt"

    listbox .xfKeysymBox.keysyms.keysyms \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".xfKeysymBox.keysyms.hscroll set" \
      -yscrollcommand ".xfKeysymBox.keysyms.vscroll set"
    catch ".xfKeysymBox.keysyms.keysyms config $tmpMessageOpt"

    # read keysym file
    if {"$xfKeysymBoxFileKeysym" != ""} {
      if {[catch "open $xfKeysymBoxFileKeysym r" keysymInFile]} {
        set xfKeysymBoxFileKeysym ""
        if {"[info commands XFAlertBox]" != ""} {
          XFAlertBox "$keysymInFile"
        } {
          puts stderr "$keysymInFile"
        }
      } {
        set keysymReadList [read $keysymInFile]
        close $keysymInFile
        foreach keysymLine [split $keysymReadList "\n"] {
          if {"[string trim $keysymLine]" != ""} {
            .xfKeysymBox.keysyms.keysyms insert end $keysymLine
          }
        }
      }
    }

    # bindings
    bind .xfKeysymBox.example <Enter> "
      global xfKeysymBox
      set xfKeysymBox(focus) \[focus\]
      focus .xfKeysymBox.example"
    bind .xfKeysymBox.example <Leave> "
      global xfKeysymBox
      focus \$xfKeysymBox(focus)"
    bind .xfKeysymBox.example <Any-KeyPress> "
      .xfKeysymBox.current.current delete 0 end
      .xfKeysymBox.current.current insert 0 \"Key-%K\""

    bind .xfKeysymBox.keysyms.keysyms <ButtonPress-1> "
      XFKeysymBoxSelectKeysym %W %y"
    bind .xfKeysymBox.keysyms.keysyms <Button1-Motion> "
      XFKeysymBoxSelectKeysym %W %y"
    bind .xfKeysymBox.keysyms.keysyms <Shift-ButtonPress-1> "
      XFKeysymBoxSelectKeysym %W %y"
    bind .xfKeysymBox.keysyms.keysyms <Shift-Button1-Motion> "
      XFKeysymBoxSelectKeysym %W %y"
  }

  .xfKeysymBox.frame1.ok config \
    -command "
      global xfKeysymBox
      set xfKeysymBox(keysymName) \[.xfKeysymBox.current.current get\]
      if {\"$xfKeysymBoxEntryW\" != \"\"} {
        if {\"\[winfo class $xfKeysymBoxEntryW\]\" == \"Text\"} {
          if {\$xfKeysymBox(overwrite)} {
            catch \"$xfKeysymBoxEntryW delete 1.0 end\"
          }
          catch \"$xfKeysymBoxEntryW insert insert \\\"\$xfKeysymBox(keysymName)\\\"\"
        } {
          if {\"\[winfo class $xfKeysymBoxEntryW\]\" == \"Entry\"} {
            if {\$xfKeysymBox(overwrite)} {
              catch \"$xfKeysymBoxEntryW delete 0 end\"
            }
            catch \"$xfKeysymBoxEntryW insert insert \\\"\$xfKeysymBox(keysymName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfKeysymBox}
      } {
        catch {destroy .xfKeysymBox}
      }"

  .xfKeysymBox.frame1.cancel config \
    -command "
      global xfKeysymBox
      set xfKeysymBox(keysymName) {}
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfKeysymBox}
      } {
        catch {destroy .xfKeysymBox}
      }"

  .xfKeysymBox.current.labelcurrent config \
    -text "$xfKeysymBoxMessage:"

  # bindings
  bind .xfKeysymBox.keysyms.keysyms <Double-1> "
    XFKeysymBoxSelectKeysym %W %y
    global xfKeysymBox
    set xfKeysymBox(keysymName) \[.xfKeysymBox.current.current get\]
    if {\"$xfKeysymBoxEntryW\" != \"\"} {
      if {\"\[winfo class $xfKeysymBoxEntryW\]\" == \"Text\"} {
        if {\$xfKeysymBox(overwrite)} {
          catch \"$xfKeysymBoxEntryW delete 1.0 end\"
        }
        catch \"$xfKeysymBoxEntryW insert insert \\\"\$xfKeysymBox(keysymName)\\\"\"
      } {
        if {\"\[winfo class $xfKeysymBoxEntryW\]\" == \"Entry\"} {
          if {\$xfKeysymBox(overwrite)} {
            catch \"$xfKeysymBoxEntryW delete 0 end\"
          }
          catch \"$xfKeysymBoxEntryW insert insert \\\"\$xfKeysymBox(keysymName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .xfKeysymBox}
    } {
      catch {destroy .xfKeysymBox}
    }"

  # set up current value
  .xfKeysymBox.current.current delete 0 end

  # packing
  pack append .xfKeysymBox.frame1 \
              .xfKeysymBox.frame1.ok {left fill expand} \
              .xfKeysymBox.frame1.cancel {left fill expand}
  pack append .xfKeysymBox.current \
              .xfKeysymBox.current.labelcurrent {left} \
              .xfKeysymBox.current.current {left fill expand}
  pack append .xfKeysymBox.keysyms \
              .xfKeysymBox.keysyms.vscroll "$xfKeysymBox(scrollSide) filly" \
              .xfKeysymBox.keysyms.hscroll {bottom fillx} \
              .xfKeysymBox.keysyms.keysyms {left fill expand}
  catch "wm deiconify .xfKeysymBox"

  if {"$xfKeysymBoxFileKeysym" != ""} {
    pack append .xfKeysymBox \
                .xfKeysymBox.frame1 {bottom fillx} \
                .xfKeysymBox.current {bottom fillx} \
                .xfKeysymBox.example {bottom fillx} \
                .xfKeysymBox.keysyms {left expand fill}
  } {
    wm geometry .xfKeysymBox 400x90
    pack append .xfKeysymBox \
                .xfKeysymBox.frame1 {bottom fillx} \
                .xfKeysymBox.current {bottom fillx} \
                .xfKeysymBox.example {bottom fillx}
  }

  if {"$xfKeysymBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .xfKeysymBox
    tkwait window .xfKeysymBox

    return $xfKeysymBox(keysymName)
  }
}

##########
# Procedure: XFKeysymBoxSelectKeysym
# Description: select keysym
# Arguments: xfKeysymBoxW - the widget
#            xfKeysymBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFKeysymBoxSelectKeysym {xfKeysymBoxW xfKeysymBoxY} {# xf ignore me 6

  set xfKeysymBoxNearest [$xfKeysymBoxW nearest $xfKeysymBoxY]
  if {$xfKeysymBoxNearest >= 0} {
    $xfKeysymBoxW select anchor $xfKeysymBoxNearest
    $xfKeysymBoxW select set $xfKeysymBoxNearest
    .xfKeysymBox.current.current delete 0 end
    .xfKeysymBox.current.current insert 0 [$xfKeysymBoxW get $xfKeysymBoxNearest]
  }
}

# eof

