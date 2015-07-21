# XFNoParsing
# Program: template
# Description: keysym selection
#
# $Header: KeysymBox.t[2.3] Wed Mar 10 12:03:19 1993 garfield@garfield frozen $

global keysymBox
set keysymBox(activeBackground) ""
set keysymBox(activeForeground) ""
set keysymBox(background) ""
set keysymBox(font) ""
set keysymBox(foreground) ""
set keysymBox(overwrite) 0
set keysymBox(scrollActiveForeground) ""
set keysymBox(scrollBackground) ""
set keysymBox(scrollForeground) ""
set keysymBox(scrollSide) left
set keysymBox(keysymName) ""
set keysymBox(focus) ""

proc KeysymBox {{keysymBoxFileKeysym "/usr/local/lib/xf/lib/Keysyms"} {keysymBoxMessage "Keysym"} {keysymBoxEntryW ""}} {# xf ignore me 5
##########
# Procedure: KeysymBox
# Description: select a keysym
# Arguments: {keysymBoxFileKeysym} - the keysym file with all keysyms
#            {keysymBoxMessage} - a message to display
#            {keysymBoxEntryW} - the widget name for the resulting keysym name
# Returns: keysym, or nothing
# Sideeffects: none
##########
# 
# global keysymBox(activeBackground) - active background color
# global keysymBox(activeForeground) - active foreground color
# global keysymBox(background) - background color
# global keysymBox(font) - text font
# global keysymBox(foreground) - foreground color
# global keysymBox(overwrite) - insert or overwrite in keysymBoxEntryW
# global keysymBox(scrollActiveForeground) - scrollbar active background color
# global keysymBox(scrollBackground) - scrollbar background color
# global keysymBox(scrollForeground) - scrollbar foreground color
# global keysymBox(scrollSide) - side where scrollbar is located

  global keysymBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScrollOpt ""
  if {"$keysymBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$keysymBox(activeBackground)\" "
  }
  if {"$keysymBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$keysymBox(activeForeground)\" "
  }
  if {"$keysymBox(background)" != ""} {
    append tmpButtonOpt "-background \"$keysymBox(background)\" "
    append tmpFrameOpt "-background \"$keysymBox(background)\" "
    append tmpMessageOpt "-background \"$keysymBox(background)\" "
  }
  if {"$keysymBox(font)" != ""} {
    append tmpButtonOpt "-font \"$keysymBox(font)\" "
    append tmpMessageOpt "-font \"$keysymBox(font)\" "
  }
  if {"$keysymBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$keysymBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$keysymBox(foreground)\" "
  }
  if {"$keysymBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$keysymBox(scrollActiveForeground)\" "
  }
  if {"$keysymBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$keysymBox(scrollBackground)\" "
  }
  if {"$keysymBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$keysymBox(scrollForeground)\" "
  }

  # get keysym file name
  if {!([file exists $keysymBoxFileKeysym] &&
        [file readable $keysymBoxFileKeysym])} {
    set keysymBoxFileKeysym ""
  }
  if {"$keysymBoxFileKeysym" == ""} {
    global env
    if {[info exists env(XF_KEYSYM_FILE)]} {
      if {[file exists $env(XF_KEYSYM_FILE)] &&
          [file readable $env(XF_KEYSYM_FILE)]} {
        set keysymBoxFileKeysym $env(XF_KEYSYM_FILE)
      }
    }
  }
  if {"$keysymBoxMessage" == ""} {
    set keysymBoxMessage "Keysym"
  }

  # look if there is already a keysym window
  if {"[info commands .keysymBox]" == ""} {
    # build widget structure

    # start build of toplevel
    if {"[info commands XFDestroy]" != ""} {
      catch {XFDestroy .keysymBox}
    } {
      catch {destroy .keysymBox}
    }
    toplevel .keysymBox \
      -borderwidth 0
    catch ".keysymBox config $tmpFrameOpt"
    wm geometry .keysymBox 400x300
    wm title .keysymBox {Keysym box}
    wm maxsize .keysymBox 1000 1000
    wm minsize .keysymBox 100 90
    # end build of toplevel

    frame .keysymBox.frame1 \
      -borderwidth 0 \
      -relief raised
    catch ".keysymBox.frame1 config $tmpFrameOpt"

    button .keysymBox.frame1.ok \
      -text "OK"
    catch ".keysymBox.frame1.ok config $tmpButtonOpt"

    button .keysymBox.frame1.cancel \
      -text "Cancel"
    catch ".keysymBox.frame1.cancel config $tmpButtonOpt"

    label .keysymBox.example \
      -relief raised \
      -text "Press key to insert keysym!"
    catch ".keysymBox.example config $tmpMessageOpt"

    frame .keysymBox.current \
      -borderwidth 0 \
      -relief raised
    catch ".keysymBox.current config $tmpFrameOpt"

    label .keysymBox.current.labelcurrent \
      -relief raised \
      -text "$keysymBoxMessage:"
    catch ".keysymBox.current.labelcurrent config $tmpMessageOpt"

    entry .keysymBox.current.current \
      -relief raised
    catch ".keysymBox.current.current config $tmpMessageOpt"

    frame .keysymBox.keysyms \
      -borderwidth 0 \
      -relief raised
    catch ".keysymBox.keysyms config $tmpFrameOpt"

    scrollbar .keysymBox.keysyms.vscroll \
      -relief raised \
      -command ".keysymBox.keysyms.keysyms yview"
    catch ".keysymBox.keysyms.vscroll config $tmpScrollOpt"

    scrollbar .keysymBox.keysyms.hscroll \
      -orient horiz \
      -relief raised \
      -command ".keysymBox.keysyms.keysyms xview"
    catch ".keysymBox.keysyms.hscroll config $tmpScrollOpt"

    listbox .keysymBox.keysyms.keysyms \
      -exportselection false \
      -relief raised \
      -xscrollcommand ".keysymBox.keysyms.hscroll set" \
      -yscrollcommand ".keysymBox.keysyms.vscroll set"
    catch ".keysymBox.keysyms.keysyms config $tmpMessageOpt"

    # read keysym file
    if {"$keysymBoxFileKeysym" != ""} {
      if {[catch "open $keysymBoxFileKeysym r" keysymInFile]} {
        set keysymBoxFileKeysym ""
        if {"[info commands AlertBox]" != ""} {
          AlertBox "$keysymInFile"
        } {
          puts stderr "$keysymInFile"
        }
      } {
        set keysymReadList [read $keysymInFile]
        close $keysymInFile
        foreach keysymLine [split $keysymReadList "\n"] {
          if {"[string trim $keysymLine]" != ""} {
            .keysymBox.keysyms.keysyms insert end $keysymLine
          }
        }
      }
    }

    # bindings
    bind .keysymBox.example <Enter> "
      global keysymBox
      set keysymBox(focus) \[focus\]
      focus .keysymBox.example"
    bind .keysymBox.example <Leave> "
      global keysymBox
      focus \$keysymBox(focus)"
    bind .keysymBox.example <Any-KeyPress> "
      .keysymBox.current.current delete 0 end
      .keysymBox.current.current insert 0 \"Key-%K\""

    bind .keysymBox.keysyms.keysyms <ButtonPress-1> "
      KeysymBoxSelectKeysym %W %y"
    bind .keysymBox.keysyms.keysyms <Button1-Motion> "
      KeysymBoxSelectKeysym %W %y"
    bind .keysymBox.keysyms.keysyms <Shift-ButtonPress-1> "
      KeysymBoxSelectKeysym %W %y"
    bind .keysymBox.keysyms.keysyms <Shift-Button1-Motion> "
      KeysymBoxSelectKeysym %W %y"
  }

  .keysymBox.frame1.ok config \
    -command "
      global keysymBox
      set keysymBox(keysymName) \[.keysymBox.current.current get\]
      if {\"$keysymBoxEntryW\" != \"\"} {
        if {\"\[winfo class $keysymBoxEntryW\]\" == \"Text\"} {
          if {\$keysymBox(overwrite)} {
            catch \"$keysymBoxEntryW delete 1.0 end\"
          }
          catch \"$keysymBoxEntryW insert insert \\\"\$keysymBox(keysymName)\\\"\"
        } {
          if {\"\[winfo class $keysymBoxEntryW\]\" == \"Entry\"} {
            if {\$keysymBox(overwrite)} {
              catch \"$keysymBoxEntryW delete 0 end\"
            }
            catch \"$keysymBoxEntryW insert insert \\\"\$keysymBox(keysymName)\\\"\"
          }
        }
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .keysymBox}
      } {
        catch {destroy .keysymBox}
      }"

  .keysymBox.frame1.cancel config \
    -command "
      global keysymBox
      set keysymBox(keysymName) {}
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .keysymBox}
      } {
        catch {destroy .keysymBox}
      }"

  .keysymBox.current.labelcurrent config \
    -text "$keysymBoxMessage:"

  # bindings
  bind .keysymBox.keysyms.keysyms <Double-1> "
    KeysymBoxSelectKeysym %W %y
    global keysymBox
    set keysymBox(keysymName) \[.keysymBox.current.current get\]
    if {\"$keysymBoxEntryW\" != \"\"} {
      if {\"\[winfo class $keysymBoxEntryW\]\" == \"Text\"} {
        if {\$keysymBox(overwrite)} {
          catch \"$keysymBoxEntryW delete 1.0 end\"
        }
        catch \"$keysymBoxEntryW insert insert \\\"\$keysymBox(keysymName)\\\"\"
      } {
        if {\"\[winfo class $keysymBoxEntryW\]\" == \"Entry\"} {
          if {\$keysymBox(overwrite)} {
            catch \"$keysymBoxEntryW delete 0 end\"
          }
          catch \"$keysymBoxEntryW insert insert \\\"\$keysymBox(keysymName)\\\"\"
        }
      }
    }
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .keysymBox}
    } {
      catch {destroy .keysymBox}
    }"

  # set up current value
  .keysymBox.current.current delete 0 end

  # packing
  pack append .keysymBox.frame1 \
              .keysymBox.frame1.ok {left fill expand} \
              .keysymBox.frame1.cancel {left fill expand}
  pack append .keysymBox.current \
              .keysymBox.current.labelcurrent {left} \
              .keysymBox.current.current {left fill expand}
  pack append .keysymBox.keysyms \
              .keysymBox.keysyms.vscroll "$keysymBox(scrollSide) filly" \
              .keysymBox.keysyms.hscroll {bottom fillx} \
              .keysymBox.keysyms.keysyms {left fill expand}
  catch "wm deiconify .keysymBox"

  if {"$keysymBoxFileKeysym" != ""} {
    pack append .keysymBox \
                .keysymBox.frame1 {bottom fillx} \
                .keysymBox.current {bottom fillx} \
                .keysymBox.example {bottom fillx} \
                .keysymBox.keysyms {left expand fill}
  } {
    wm geometry .keysymBox 400x90
    pack append .keysymBox \
                .keysymBox.frame1 {bottom fillx} \
                .keysymBox.current {bottom fillx} \
                .keysymBox.example {bottom fillx}
  }

  if {"$keysymBoxEntryW" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .keysymBox
    tkwait window .keysymBox

    return $keysymBox(keysymName)
  }
}

##########
# Procedure: KeysymBoxSelectKeysym
# Description: select keysym
# Arguments: keysymBoxW - the widget
#            keysymBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc KeysymBoxSelectKeysym {keysymBoxW keysymBoxY} {# xf ignore me 6

  set keysymBoxNearest [$keysymBoxW nearest $keysymBoxY]
  if {$keysymBoxNearest >= 0} {
    $keysymBoxW select clear 0 end
    $keysymBoxW select set $keysymBoxNearest
    .keysymBox.current.current delete 0 end
    .keysymBox.current.current insert 0 [$keysymBoxW get $keysymBoxNearest]
  }
}

# eof

