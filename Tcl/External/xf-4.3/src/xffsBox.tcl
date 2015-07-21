# XFNoParsing
# Program: template
# Description: file selector box
#
# $Header: xffsBox.tcl[2.3] Wed Mar 10 12:05:54 1993 garfield@garfield frozen $

global xfFSBox
set xfFSBox(activeBackground) ""
set xfFSBox(activeForeground) ""
set xfFSBox(background) ""
set xfFSBox(font) ""
set xfFSBox(foreground) ""
set xfFSBox(scrollActiveForeground) ""
set xfFSBox(scrollBackground) ""
set xfFSBox(scrollForeground) ""
set xfFSBox(showPixmap) 0
set xfFSBox(name) ""
set xfFSBox(path) [pwd]
set xfFSBox(pattern) *
set xfFSBox(all) 0
set xfFSBox(button) 0
set xfFSBox(extensions) 0
set xfFSBox(internalPath) [pwd]

proc XFFSBox {{xfFSBoxMessage "Select file:"} {xfFSBoxFileName ""} {xfFSBoxActionOk ""} {xfFSBoxActionCancel ""}} {# xf ignore me 5
##########
# Procedure: XFFSBox
# Description: show file selector box
# Arguments: xfFSBoxMessage - the text to display
#            xfFSBoxFileName - a file name that should be selected
#            xfFSBoxActionOk - the action that should be performed on ok
#            xfFSBoxActionCancel - the action that should be performed on cancel
# Returns: the filename that was selected, or nothing
# Sideeffects: none
##########
# 
# global xfFSBox(activeBackground) - active background color
# global xfFSBox(activeForeground) - active foreground color
# global xfFSBox(background) - background color
# global xfFSBox(font) - text font
# global xfFSBox(foreground) - foreground color
# global xfFSBox(extensions) - scan directory for extensions
# global xfFSBox(scrollActiveForeground) - scrollbar active background color
# global xfFSBox(scrollBackground) - scrollbar background color
# global xfFSBox(scrollForeground) - scrollbar foreground color
# global xfFSBox(scrollSide) - side where scrollbar is located

  global xfFSBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$xfFSBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfFSBox(activeBackground)\" "
  }
  if {"$xfFSBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfFSBox(activeForeground)\" "
  }
  if {"$xfFSBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfFSBox(background)\" "
    append tmpFrameOpt "-background \"$xfFSBox(background)\" "
    append tmpMessageOpt "-background \"$xfFSBox(background)\" "
  }
  if {"$xfFSBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfFSBox(font)\" "
    append tmpMessageOpt "-font \"$xfFSBox(font)\" "
  }
  if {"$xfFSBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfFSBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$xfFSBox(foreground)\" "
  }
  if {"$xfFSBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$xfFSBox(scrollActiveForeground)\" "
  }
  if {"$xfFSBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$xfFSBox(scrollBackground)\" "
  }
  if {"$xfFSBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$xfFSBox(scrollForeground)\" "
  }

  if {[file exists [file tail $xfFSBoxFileName]] &&
      [XFIsAFile [file tail $xfFSBoxFileName]]} {
    set xfFSBox(name) [file tail $xfFSBoxFileName]
  } {
    set xfFSBox(name) ""
  }
  if {[file exists $xfFSBoxFileName] && [XFIsADir $xfFSBoxFileName]} {
    set xfFSBox(path) $xfFSBoxFileName
  } {
    if {"[file dirname $xfFSBoxFileName]" != "."} {
      set xfFSBox(path) [file dirname $xfFSBoxFileName]
    }
  }
  if {$xfFSBox(showPixmap)} {
    set xfFSBox(path) [string trimleft $xfFSBox(path) @]
  }
  if {"$xfFSBox(path)" != "" && [file exists $xfFSBox(path)] &&
      [XFIsADir $xfFSBox(path)]} {
    set xfFSBox(internalPath) $xfFSBox(path)
  } {
    if {"$xfFSBox(internalPath)" == "" ||
        ![file exists $xfFSBox(internalPath)]} {
      set xfFSBox(internalPath) [pwd]
    }
  }
  # build widget structure

  XFTmpltToplevel .xfFSBox 350x300 {XF file select}

  label .xfFSBox.message1 \
    -anchor c \
    -relief raised \
    -text "$xfFSBoxMessage"
  catch ".xfFSBox.message1 config $tmpMessageOpt"

  frame .xfFSBox.frame1 \
    -borderwidth 0 \
    -relief raised
  catch ".xfFSBox.frame1 config $tmpFrameOpt"

  button .xfFSBox.frame1.ok \
    -text "OK" \
    -command "
      global xfFSBox
      set xfFSBox(name) \[.xfFSBox.file.file get\]
      if {$xfFSBox(showPixmap)} {
        set xfFSBox(path) @\[.xfFSBox.path.path get\]
      } {
        set xfFSBox(path) \[.xfFSBox.path.path get\]
      }
      set xfFSBox(internalPath) \[.xfFSBox.path.path get\]
      $xfFSBoxActionOk
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfFSBox}
      } {
        catch {destroy .xfFSBox}
      }"
  catch ".xfFSBox.frame1.ok config $tmpButtonOpt"

  button .xfFSBox.frame1.rescan \
    -text "Rescan" \
    -command {
      global xfFSBox
      XFFSBoxFSShow [.xfFSBox.path.path get] \
        [.xfFSBox.pattern.pattern get] $xfFSBox(all)}
  catch ".xfFSBox.frame1.rescan config $tmpButtonOpt"

  button .xfFSBox.frame1.cancel \
    -text "Cancel" \
    -command "
      global xfFSBox
      set xfFSBox(name) {}
      set xfFSBox(path) {}
      $xfFSBoxActionCancel
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .xfFSBox}
      } {
        catch {destroy .xfFSBox}
      }"
  catch ".xfFSBox.frame1.cancel config $tmpButtonOpt"

  if {$xfFSBox(showPixmap)} {
    frame .xfFSBox.frame2 \
      -borderwidth 0 \
      -relief raised
    catch ".xfFSBox.frame2 config $tmpFrameOpt"

    scrollbar .xfFSBox.frame2.scrollbar3 \
      -command {.xfFSBox.frame2.canvas2 xview} \
      -orient {horizontal} \
      -relief {raised}
    catch ".xfFSBox.frame2.scrollbar3 config $tmpScrollOpt"

    scrollbar .xfFSBox.frame2.scrollbar1 \
      -command {.xfFSBox.frame2.canvas2 yview} \
      -relief {raised}
    catch ".xfFSBox.frame2.scrollbar1 config $tmpScrollOpt"

    canvas .xfFSBox.frame2.canvas2 \
      -confine {true} \
      -relief {raised} \
      -scrollregion {0c 0c 20c 20c} \
      -width {100} \
      -xscrollcommand {.xfFSBox.frame2.scrollbar3 set} \
      -yscrollcommand {.xfFSBox.frame2.scrollbar1 set}
    catch ".xfFSBox.frame2.canvas2 config $tmpFrameOpt"

    .xfFSBox.frame2.canvas2 addtag currentBitmap withtag [.xfFSBox.frame2.canvas2 create bitmap 5 5 -anchor nw]
  }

  frame .xfFSBox.path \
    -borderwidth 0 \
    -relief raised
  catch ".xfFSBox.path config $tmpFrameOpt"

  frame .xfFSBox.path.paths \
    -borderwidth 2 \
    -relief raised
  catch ".xfFSBox.path.paths config $tmpFrameOpt"

  menubutton .xfFSBox.path.paths.paths \
    -borderwidth 0 \
    -menu ".xfFSBox.path.paths.paths.menu" \
    -relief flat \
    -text "Pathname:"
  catch ".xfFSBox.path.paths.paths config $tmpButtonOpt"

  menu .xfFSBox.path.paths.paths.menu
  catch ".xfFSBox.path.paths.paths.menu config $tmpButtonOpt"

  .xfFSBox.path.paths.paths.menu add command \
     -label "[string trimright $xfFSBox(internalPath) {/@}]" \
     -command "
       global xfFSBox
       XFFSBoxFSShow \[.xfFSBox.path.path get\] \
         \[.xfFSBox.pattern.pattern get\] \$xfFSBox(all)
       .xfFSBox.path.path delete 0 end
       .xfFSBox.path.path insert 0 [string trimright $xfFSBox(internalPath) {/@}]"

  entry .xfFSBox.path.path \
    -relief raised
  catch ".xfFSBox.path.path config $tmpMessageOpt"

  if {![XFIsADir $xfFSBox(internalPath)]} {
    set $xfFSBox(internalPath) [pwd]
  }
  .xfFSBox.path.path insert 0 $xfFSBox(internalPath)

  frame .xfFSBox.pattern \
    -borderwidth 0 \
    -relief raised
  catch ".xfFSBox.pattern config $tmpFrameOpt"

  frame .xfFSBox.pattern.patterns \
    -borderwidth 2 \
    -relief raised
  catch ".xfFSBox.pattern.patterns config $tmpFrameOpt"

  menubutton .xfFSBox.pattern.patterns.patterns \
    -borderwidth 0 \
    -menu ".xfFSBox.pattern.patterns.patterns.menu" \
    -relief flat \
    -text "Selection pattern:"
  catch ".xfFSBox.pattern.patterns.patterns config $tmpButtonOpt"

  menu .xfFSBox.pattern.patterns.patterns.menu
  catch ".xfFSBox.pattern.patterns.patterns.menu config $tmpButtonOpt"

  .xfFSBox.pattern.patterns.patterns.menu add checkbutton \
    -label "Scan extensions" \
    -variable xfFSBox(extensions) \
    -command {
      global xfFSBox
      XFFSBoxFSShow [.xfFSBox.path.path get] \
        [.xfFSBox.pattern.pattern get] $xfFSBox(all)}

  entry .xfFSBox.pattern.pattern \
    -relief raised
  catch ".xfFSBox.pattern.pattern config $tmpMessageOpt"

  .xfFSBox.pattern.pattern insert 0 $xfFSBox(pattern)
  
  frame .xfFSBox.files \
    -borderwidth 0 \
    -relief raised
  catch ".xfFSBox.files config $tmpFrameOpt"

  scrollbar .xfFSBox.files.vscroll \
    -relief raised \
    -command ".xfFSBox.files.files yview"
  catch ".xfFSBox.files.vscroll config $tmpScrollOpt"

  scrollbar .xfFSBox.files.hscroll \
    -orient horiz \
    -relief raised \
    -command ".xfFSBox.files.files xview"
  catch ".xfFSBox.files.hscroll config $tmpScrollOpt"

  listbox .xfFSBox.files.files \
    -exportselection false \
    -relief raised \
    -xscrollcommand ".xfFSBox.files.hscroll set" \
    -yscrollcommand ".xfFSBox.files.vscroll set"
  catch ".xfFSBox.files.files config $tmpMessageOpt"

  frame .xfFSBox.file \
    -borderwidth 0 \
    -relief raised
  catch ".xfFSBox.file config $tmpFrameOpt"

  label .xfFSBox.file.labelfile \
    -relief raised \
    -text "Filename:"
  catch ".xfFSBox.file.labelfile config $tmpMessageOpt"

  entry .xfFSBox.file.file \
    -relief raised
  catch ".xfFSBox.file.file config $tmpMessageOpt"

  .xfFSBox.file.file delete 0 end
  .xfFSBox.file.file insert 0 $xfFSBox(name)
  
  checkbutton .xfFSBox.pattern.all \
    -offvalue 0 \
    -onvalue 1 \
    -text "Show all files" \
    -variable xfFSBox(all) \
    -command {
      global xfFSBox
      XFFSBoxFSShow [.xfFSBox.path.path get] \
        [.xfFSBox.pattern.pattern get] $xfFSBox(all)}
  catch ".xfFSBox.pattern.all config $tmpButtonOpt"

  XFFSBoxFSShow $xfFSBox(internalPath) $xfFSBox(pattern) $xfFSBox(all)

  # bindings
  bindtags .xfFSBox.files.files {.xfFSBox.files.files "" "" "" }
  bind .xfFSBox.files.files <Double-Button-1> "
    XFFSBoxFSFileSelectDouble %W $xfFSBox(showPixmap) \{$xfFSBoxActionOk\} %y"
  bind .xfFSBox.files.files <ButtonPress-1> "
    XFFSBoxFSFileSelect %W $xfFSBox(showPixmap) %y"
  bind .xfFSBox.files.files <Button1-Motion> "
    XFFSBoxFSFileSelect %W $xfFSBox(showPixmap) %y"
  bind .xfFSBox.files.files <Shift-Button1-Motion> "
    XFFSBoxFSFileSelect %W $xfFSBox(showPixmap) %y"
  bind .xfFSBox.files.files <Shift-ButtonPress-1> "
    XFFSBoxFSFileSelect %W $xfFSBox(showPixmap) %y"

  bind .xfFSBox.path.path <Tab> {
    XFFSBoxFSNameComplete path}
  bind .xfFSBox.path.path <Return> {
    global xfFSBox
    XFFSBoxFSShow [.xfFSBox.path.path get] \
      [.xfFSBox.pattern.pattern get] $xfFSBox(all)
    XFFSBoxFSInsertPath
    .xfFSBox.file.file icursor end
    focus .xfFSBox.file.file}
  catch "bind .xfFSBox.path.path <Up> {}"
  bind .xfFSBox.path.path <Down> {
    .xfFSBox.file.file icursor end
    focus .xfFSBox.file.file}

  bind .xfFSBox.file.file <Tab> {
    XFFSBoxFSNameComplete file}
  bind .xfFSBox.file.file <Return> "
    global xfFSBox
    set xfFSBox(name) \[.xfFSBox.file.file get\]
    if {$xfFSBox(showPixmap)} {
      set xfFSBox(path) @\[.xfFSBox.path.path get\]
    } {
      set xfFSBox(path) \[.xfFSBox.path.path get\]
    }
    set xfFSBox(internalPath) \[.xfFSBox.path.path get\]
    $xfFSBoxActionOk
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .xfFSBox}
    } {
      catch {destroy .xfFSBox}
    }"
  bind .xfFSBox.file.file <Up> {
    .xfFSBox.path.path icursor end
    focus .xfFSBox.path.path}
  bind .xfFSBox.file.file <Down> {
    .xfFSBox.pattern.pattern icursor end
    focus .xfFSBox.pattern.pattern}

  bind .xfFSBox.pattern.pattern <Return> {
    global xfFSBox
    XFFSBoxFSShow [.xfFSBox.path.path get] \
      [.xfFSBox.pattern.pattern get] $xfFSBox(all)}
  bind .xfFSBox.pattern.pattern <Up> {
    .xfFSBox.file.file icursor end
    focus .xfFSBox.file.file}
  catch "bind .xfFSBox.pattern.pattern <Down> {}"

  # packing
  pack append .xfFSBox.files \
              .xfFSBox.files.vscroll "$xfFSBox(scrollSide) filly" \
              .xfFSBox.files.hscroll {bottom fillx} \
              .xfFSBox.files.files {left fill expand}
  pack append .xfFSBox.file \
              .xfFSBox.file.labelfile {left} \
              .xfFSBox.file.file {left fill expand}
  pack append .xfFSBox.frame1 \
              .xfFSBox.frame1.ok {left fill expand} \
              .xfFSBox.frame1.rescan {left fill expand} \
              .xfFSBox.frame1.cancel {left fill expand}
  pack append .xfFSBox.path.paths \
              .xfFSBox.path.paths.paths {left}
  pack append .xfFSBox.pattern.patterns \
              .xfFSBox.pattern.patterns.patterns {left}
  pack append .xfFSBox.path \
              .xfFSBox.path.paths {left} \
              .xfFSBox.path.path {left fill expand}
  pack append .xfFSBox.pattern \
              .xfFSBox.pattern.patterns {left} \
              .xfFSBox.pattern.all {right fill} \
              .xfFSBox.pattern.pattern {left fill expand}
  if {$xfFSBox(showPixmap)} {
    pack append .xfFSBox.frame2 \
                .xfFSBox.frame2.scrollbar1 {left filly} \
                .xfFSBox.frame2.canvas2 {top expand fill} \
                .xfFSBox.frame2.scrollbar3 {top fillx} 

    pack append .xfFSBox \
                .xfFSBox.message1 {top fill} \
                .xfFSBox.frame1 {bottom fill} \
                .xfFSBox.pattern {bottom fill} \
                .xfFSBox.file {bottom fill} \
                .xfFSBox.path {bottom fill} \
                .xfFSBox.frame2 {right fill} \
                .xfFSBox.files {left fill expand}
  } {
    pack append .xfFSBox \
                .xfFSBox.message1 {top fill} \
                .xfFSBox.frame1 {bottom fill} \
                .xfFSBox.pattern {bottom fill} \
                .xfFSBox.file {bottom fill} \
                .xfFSBox.path {bottom fill} \
                .xfFSBox.files {left fill expand}
  }

  if {"$xfFSBoxActionOk" == "" && "$xfFSBoxActionCancel" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .xfFSBox
    tkwait window .xfFSBox

    if {"[string trim $xfFSBox(path)]" != "" ||
        "[string trim $xfFSBox(name)]" != ""} {
      if {"[string trimleft [string trim $xfFSBox(name)] /]" == ""} {
        return [string trimright [string trim $xfFSBox(path)] /]
      } {
        return [string trimright [string trim $xfFSBox(path)] /]/[string trimleft [string trim $xfFSBox(name)] /]
      }
    }
  }
}

##########
# Procedure: XFFSBoxFSFileSelect
# Description: select file name
# Arguments: xfFSBoxW - the widget
#            xfFSBoxShowPixmap - show pixmaps
#            xfFSBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFFSBoxFSFileSelect {xfFSBoxW xfFSBoxShowPixmap xfFSBoxY} {# xf ignore me 6
  global xfFSBox

  XFFSBoxBindSelectOne $xfFSBoxW $xfFSBoxY
  set xfFSBoxNearest [$xfFSBoxW nearest $xfFSBoxY]
  if {$xfFSBoxNearest >= 0} {
    set xfFSBoxTmpEntry [$xfFSBoxW get $xfFSBoxNearest]
    if {"[string index $xfFSBoxTmpEntry \
          [expr [string length $xfFSBoxTmpEntry]-1]]" == "/" ||
        "[string index $xfFSBoxTmpEntry \
          [expr [string length $xfFSBoxTmpEntry]-1]]" == "@"} {
      set xfFSBoxFileName [string range $xfFSBoxTmpEntry 0 \
            [expr [string length $xfFSBoxTmpEntry]-2]]
      if {![XFIsADir [string trimright $xfFSBox(internalPath)/$xfFSBoxFileName @]] &&
          ![XFIsASymlink [string trimright $xfFSBox(internalPath)/$xfFSBoxFileName @]]} {
        set xfFSBoxFileName $xfFSBoxTmpEntry
      }
    } {
      if {"[string index $xfFSBoxTmpEntry \
            [expr [string length $xfFSBoxTmpEntry]-1]]" == "*"} {
        set xfFSBoxFileName [string range $xfFSBoxTmpEntry 0 \
          [expr [string length $xfFSBoxTmpEntry]-2]]
        if {![file executable $xfFSBox(internalPath)/$xfFSBoxFileName]} {
          set xfFSBoxFileName $xfFSBoxTmpEntry
        }
      } {
        set xfFSBoxFileName $xfFSBoxTmpEntry
      }
    }
    if {![XFIsADir [string trimright $xfFSBox(internalPath)/$xfFSBoxFileName @]]} {
      set xfFSBox(name) $xfFSBoxFileName
      .xfFSBox.file.file delete 0 end
      .xfFSBox.file.file insert 0 $xfFSBox(name)
      if {$xfFSBoxShowPixmap} {
        catch ".xfFSBox.frame2.canvas2 itemconfigure currentBitmap -bitmap \"@$xfFSBox(internalPath)/$xfFSBox(name)\""
      }
    }
  }
}

##########
# Procedure: XFFSBoxFSFileSelectDouble
# Description: select file when double clicked
# Arguments: xfFSBoxW - the widget
#            xfFSBoxShowPixmap - show pixmaps
#            xfFSBoxAction - the action bound to the ok button
#            xfFSBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFFSBoxFSFileSelectDouble {xfFSBoxW xfFSBoxShowPixmap xfFSBoxAction xfFSBoxY} {# xf ignore me 6
  global xfFSBox

  XFFSBoxBindSelectOne $xfFSBoxW $xfFSBoxY
  set xfFSBoxNearest [$xfFSBoxW nearest $xfFSBoxY]
  if {$xfFSBoxNearest >= 0} {
    set xfFSBoxTmpEntry [$xfFSBoxW get $xfFSBoxNearest]
    if {"$xfFSBoxTmpEntry" == "../"} {
      set xfFSBoxTmpEntry [string trimright [string trim $xfFSBox(internalPath)] "@/"]
      if {"$xfFSBoxTmpEntry" == ""} {
        return
      }
      XFFSBoxFSShow [file dirname $xfFSBoxTmpEntry] \
        [.xfFSBox.pattern.pattern get] $xfFSBox(all)
      .xfFSBox.path.path delete 0 end
      .xfFSBox.path.path insert 0 $xfFSBox(internalPath)
    } {
      if {"[string index $xfFSBoxTmpEntry \
            [expr [string length $xfFSBoxTmpEntry]-1]]" == "/" ||
          "[string index $xfFSBoxTmpEntry \
            [expr [string length $xfFSBoxTmpEntry]-1]]" == "@"} {
        set xfFSBoxFileName [string range $xfFSBoxTmpEntry 0 \
              [expr [string length $xfFSBoxTmpEntry]-2]]
        if {![XFIsADir [string trimright $xfFSBox(internalPath)/$xfFSBoxFileName @]] &&
            ![XFIsASymlink [string trimright $xfFSBox(internalPath)/$xfFSBoxFileName @]]} {
          set xfFSBoxFileName $xfFSBoxTmpEntry
        }
      } {
        if {"[string index $xfFSBoxTmpEntry \
              [expr [string length $xfFSBoxTmpEntry]-1]]" == "*"} {
          set xfFSBoxFileName [string range $xfFSBoxTmpEntry 0 \
                [expr [string length $xfFSBoxTmpEntry]-2]]
          if {![file executable $xfFSBox(internalPath)/$xfFSBoxFileName]} {
            set xfFSBoxFileName $xfFSBoxTmpEntry
          }
        } {
          set xfFSBoxFileName $xfFSBoxTmpEntry
        }
      }
      if {[XFIsADir [string trimright $xfFSBox(internalPath)/$xfFSBoxFileName @]]} {
        set xfFSBox(internalPath) "[string trimright $xfFSBox(internalPath) {/@}]/$xfFSBoxFileName"
        XFFSBoxFSShow $xfFSBox(internalPath) \
          [.xfFSBox.pattern.pattern get] $xfFSBox(all)
        .xfFSBox.path.path delete 0 end
        .xfFSBox.path.path insert 0 $xfFSBox(internalPath)
      } {
        set xfFSBox(name) $xfFSBoxFileName
        if {$xfFSBoxShowPixmap} {
          set xfFSBox(path) @$xfFSBox(internalPath)
        } {
          set xfFSBox(path) $xfFSBox(internalPath)
        }
        if {"$xfFSBoxAction" != ""} {
          eval "global xfFSBox; $xfFSBoxAction"
        }
        if {"[info commands XFDestroy]" != ""} {
          catch {XFDestroy .xfFSBox}
        } {
          catch {destroy .xfFSBox}
        }
      }
    }
  }
}

##########
# Procedure: XFFSBoxFSInsertPath
# Description: insert current pathname into menu
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFFSBoxFSInsertPath {} {# xf ignore me 6
  global xfFSBox

  set xfFSBoxLast [.xfFSBox.path.paths.paths.menu index last]
  set xfFSBoxNewEntry [string trimright [.xfFSBox.path.path get] "/@"]
  for {set xfFSBoxCounter 0} {$xfFSBoxCounter <= $xfFSBoxLast} {incr xfFSBoxCounter 1} {
    if {"$xfFSBoxNewEntry" == \
          "[lindex [.xfFSBox.path.paths.paths.menu entryconfigure \
                    $xfFSBoxCounter -label] 4]"} {
      return
    }
  }
  if {$xfFSBoxLast < 9} {
    .xfFSBox.path.paths.paths.menu add command \
      -label "$xfFSBoxNewEntry" \
      -command "
        global xfFSBox
        XFFSBoxFSShow $xfFSBoxNewEntry \
          \[.xfFSBox.pattern.pattern get\] \$xfFSBox(all)
        .xfFSBox.path.path delete 0 end
        .xfFSBox.path.path insert 0 $xfFSBoxNewEntry"
  } {
    for {set xfFSBoxCounter 0} {$xfFSBoxCounter < $xfFSBoxLast} {incr xfFSBoxCounter 1} {
      .xfFSBox.path.paths.paths.menu entryconfigure \
        $xfFSBoxCounter -label \
          [lindex [.xfFSBox.path.paths.paths.menu entryconfigure \
            [expr $xfFSBoxCounter+1] -label] 4]
      .xfFSBox.path.paths.paths.menu entryconfigure $xfFSBoxCounter \
        -command "
          global xfFSBox
          XFFSBoxFSShow [lindex [.xfFSBox.path.paths.paths.menu entryconfigure \
            [expr $xfFSBoxCounter+1] -label] 4] \
            \[.xfFSBox.pattern.pattern get\] \$xfFSBox(all)
          .xfFSBox.path.path delete 0 end
          .xfFSBox.path.path insert 0 [lindex \
            [.xfFSBox.path.paths.paths.menu entryconfigure \
              [expr $xfFSBoxCounter+1] -label] 4]"
    }
    .xfFSBox.path.paths.paths.menu entryconfigure $xfFSBoxLast \
      -label "$xfFSBoxNewEntry"
    .xfFSBox.path.paths.paths.menu entryconfigure $xfFSBoxCounter \
      -command "
        global xfFSBox
        XFFSBoxFSShow \[.xfFSBox.path.path get\] \
          \[.xfFSBox.pattern.pattern get\] \$xfFSBox(all)
        .xfFSBox.path.path delete 0 end
        .xfFSBox.path.path insert 0 $xfFSBoxNewEntry"
  }
}

##########
# Procedure: XFFSBoxFSNameComplete
# Description: perform name completion for fs box
# Arguments: xfFSBoxType - the type we want to complete (path or file)
# Returns: none
# Sideeffects: none
##########
proc XFFSBoxFSNameComplete {xfFSBoxType} {# xf ignore me 6
  global xfFSBox

  set xfFSBoxNewFile ""
  if {"$xfFSBoxType" == "path"} {
    set xfFSBoxDirName [file dirname [.xfFSBox.path.path get]]
    set xfFSBoxFileName [file tail [.xfFSBox.path.path get]]
  } {
    set xfFSBoxDirName [file dirname [.xfFSBox.path.path get]/]
    set xfFSBoxFileName [file tail [.xfFSBox.file.file get]]
  }

  set xfFSBoxNewFile ""
  if {[XFIsADir [string trimright $xfFSBoxDirName @]]} {
    catch "glob -nocomplain $xfFSBoxDirName/${xfFSBoxFileName}*" xfFSBoxResult
    foreach xfFSBoxCounter $xfFSBoxResult {
      if {"$xfFSBoxNewFile" == ""} {
        set xfFSBoxNewFile [file tail $xfFSBoxCounter]
      } {
        if {"[string index [file tail $xfFSBoxCounter] 0]" !=
            "[string index $xfFSBoxNewFile 0]"} {
          set xfFSBoxNewFile ""
          break
        }
        set xfFSBoxCounter1 0
        set xfFSBoxTmpFile1 $xfFSBoxNewFile
        set xfFSBoxTmpFile2 [file tail $xfFSBoxCounter]
        set xfFSBoxLength1 [string length $xfFSBoxTmpFile1]
        set xfFSBoxLength2 [string length $xfFSBoxTmpFile2]
        set xfFSBoxNewFile ""
        if {$xfFSBoxLength1 > $xfFSBoxLength2} {
          set xfFSBoxLength1 $xfFSBoxLength2
        }
        while {$xfFSBoxCounter1 < $xfFSBoxLength1} {
          if {"[string index $xfFSBoxTmpFile1 $xfFSBoxCounter1]" == \
                "[string index $xfFSBoxTmpFile2 $xfFSBoxCounter1]"} {
            append xfFSBoxNewFile [string index $xfFSBoxTmpFile1 $xfFSBoxCounter1]
          } {
            break
          }
          incr xfFSBoxCounter1 1
        }
      }
    }
  }
  if {"$xfFSBoxNewFile" != ""} {
    if {[XFIsADir [string trimright $xfFSBoxDirName/$xfFSBoxNewFile @]] ||
        ![XFIsAFile [string trimright $xfFSBoxDirName/$xfFSBoxNewFile @]]} {
      if {[XFIsADir [string trimright $xfFSBoxDirName/$xfFSBoxNewFile @]]} {
        if {"$xfFSBoxDirName" == "/"} {
          .xfFSBox.path.path delete 0 end
          .xfFSBox.path.path insert 0 "/[string trimright [string trim $xfFSBoxNewFile /] @]/"
        } {
          .xfFSBox.path.path delete 0 end
          .xfFSBox.path.path insert 0 "[string trimright $xfFSBoxDirName /]/[string trimright [string trim $xfFSBoxNewFile /] @]/"
        }
        XFFSBoxFSShow [.xfFSBox.path.path get] \
          [.xfFSBox.pattern.pattern get] $xfFSBox(all)
        XFFSBoxFSInsertPath
      } {
        .xfFSBox.path.path delete 0 end
        .xfFSBox.path.path insert 0 "[string trimright $xfFSBoxDirName /]/[string trimright [string trim $xfFSBoxNewFile /] @]"
      }
    } {
      .xfFSBox.path.path delete 0 end
      .xfFSBox.path.path insert 0 "[string trimright $xfFSBoxDirName {@/}]/"
      .xfFSBox.file.file delete 0 end
      .xfFSBox.file.file insert 0 $xfFSBoxNewFile
      .xfFSBox.file.file icursor end
      focus .xfFSBox.file.file
    }
  }
}

##########
# Procedure: XFFSBoxFSShow
# Description: show the file list
# Arguments: xfFSBoxPath - the path to show
#            xfFSBoxPattern - selection pattern
#            xfFSBoxAll - show all files
# Returns: none
# Sideeffects: none
##########
proc XFFSBoxFSShow {xfFSBoxPath xfFSBoxPattern xfFSBoxAll} {# xf ignore me 6
  global xfFSBox

  set tmpButtonOpt ""
  if {"$xfFSBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$xfFSBox(activeBackground)\" "
  }
  if {"$xfFSBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$xfFSBox(activeForeground)\" "
  }
  if {"$xfFSBox(background)" != ""} {
    append tmpButtonOpt "-background \"$xfFSBox(background)\" "
  }
  if {"$xfFSBox(font)" != ""} {
    append tmpButtonOpt "-font \"$xfFSBox(font)\" "
  }
  if {"$xfFSBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$xfFSBox(foreground)\" "
  }

  set xfFSBox(pattern) $xfFSBoxPattern
  if {[file exists $xfFSBoxPath] && [file readable $xfFSBoxPath] &&
      [XFIsADir $xfFSBoxPath]} {
    set xfFSBox(internalPath) $xfFSBoxPath
  } {
    if {[file exists $xfFSBoxPath] && [file readable $xfFSBoxPath] &&
        [XFIsAFile $xfFSBoxPath]} {
      set xfFSBox(internalPath) [file dirname $xfFSBoxPath]
      .xfFSBox.file.file delete 0 end
      .xfFSBox.file.file insert 0 [file tail $xfFSBoxPath]
      set xfFSBoxPath $xfFSBox(internalPath)
    } {
      while {"$xfFSBoxPath" != "" && "$xfFSBoxPath" != "/" &&
             ![file isdirectory $xfFSBoxPath]} {
        set xfFSBox(internalPath) [file dirname $xfFSBoxPath]
         set xfFSBoxPath $xfFSBox(internalPath)
      }
    }
  }
  if {"$xfFSBoxPath" == ""} {
    set xfFSBoxPath "/"
    set xfFSBox(internalPath) "/"
  }
  .xfFSBox.path.path delete 0 end
  .xfFSBox.path.path insert 0 $xfFSBox(internalPath)

  if {[.xfFSBox.files.files size] > 0} {
    .xfFSBox.files.files delete 0 end
  }
  if {$xfFSBoxAll} {
    if {[catch "Ls -F -a $xfFSBoxPath" xfFSBoxResult]} {
      puts stderr "$xfFSBoxResult"
    }
  } {
    if {[catch "Ls -F $xfFSBoxPath" xfFSBoxResult]} {
      puts stderr "$xfFSBoxResult"
    }
  }
  set xfFSBoxElementList [lsort $xfFSBoxResult]

  foreach xfFSBoxCounter [winfo children .xfFSBox.pattern.patterns.patterns] {
    if {[string length [info commands XFDestroy]] > 0} {
      catch {XFDestroy $xfFSBoxCounter}
    } {
      catch {destroy $xfFSBoxCounter}
    }
  }
  menu .xfFSBox.pattern.patterns.patterns.menu
  catch ".xfFSBox.pattern.patterns.patterns.menu config $tmpButtonOpt"

  if {$xfFSBox(extensions)} {
    .xfFSBox.pattern.patterns.patterns.menu add command \
      -label "*" \
      -command {
        global xfFSBox
        set xfFSBox(pattern) "*"
        .xfFSBox.pattern.pattern delete 0 end
        .xfFSBox.pattern.pattern insert 0 $xfFSBox(pattern)
        XFFSBoxFSShow [.xfFSBox.path.path get] $xfFSBox(pattern) \
          $xfFSBox(all)}
  }

  if {"$xfFSBoxPath" != "/"} {
    .xfFSBox.files.files insert end "../"
  }
  foreach xfFSBoxCounter $xfFSBoxElementList {
    if {[string match $xfFSBoxPattern $xfFSBoxCounter] ||
        [XFIsADir [string trimright $xfFSBoxPath/$xfFSBoxCounter "/@"]]} {
      if {"$xfFSBoxCounter" != "../" &&
          "$xfFSBoxCounter" != "./"} {
        .xfFSBox.files.files insert end $xfFSBoxCounter
      }
    }

    if {$xfFSBox(extensions)} {
      catch "file rootname $xfFSBoxCounter" xfFSBoxRootName
      catch "file extension $xfFSBoxCounter" xfFSBoxExtension
      set xfFSBoxExtension [string trimright $xfFSBoxExtension "/*@"]
      if {"$xfFSBoxExtension" != "" && "$xfFSBoxRootName" != ""} {
        set xfFSBoxInsert 1
        set xfFSBoxLast [.xfFSBox.pattern.patterns.patterns.menu index last]
        for {set xfFSBoxCounter1 0} {$xfFSBoxCounter1 <= $xfFSBoxLast} {incr xfFSBoxCounter1 1} {
          if {"*$xfFSBoxExtension" == \
                "[lindex [.xfFSBox.pattern.patterns.patterns.menu entryconfigure \
                        $xfFSBoxCounter1 -label] 4]"} {
            set xfFSBoxInsert 0
          }
        }
	if {$xfFSBoxInsert} {
          .xfFSBox.pattern.patterns.patterns.menu add command \
            -label "*$xfFSBoxExtension" \
            -command "
              global xfFSBox
              set xfFSBox(pattern) \"*$xfFSBoxExtension\"
              .xfFSBox.pattern.pattern delete 0 end
              .xfFSBox.pattern.pattern insert 0 \$xfFSBox(pattern)
              XFFSBoxFSShow \[.xfFSBox.path.path get\] \$xfFSBox(pattern) \
                \$xfFSBox(all)"
        }
      }
    }
  }
  if {$xfFSBox(extensions)} {
    .xfFSBox.pattern.patterns.patterns.menu add separator
  }
  if {$xfFSBox(extensions) || 
      "[.xfFSBox.pattern.patterns.patterns.menu index last]" == "none"} {
    .xfFSBox.pattern.patterns.patterns.menu add checkbutton \
      -label "Scan extensions" \
      -variable "xfFSBox(extensions)" \
      -command {
        global xfFSBox
        XFFSBoxFSShow [.xfFSBox.path.path get] \
          [.xfFSBox.pattern.pattern get] $xfFSBox(all)}
  }
}

##########
# Procedure: XFFSBoxBindSelectOne
# Description: action to select the current list item
# Arguments: xfFSBoxW - the widget
#            xfFSBoxY - the y position in the listbox
# Returns: none
# Sideeffects: none
##########
proc XFFSBoxBindSelectOne {xfFSBoxW xfFSBoxY} {# xf ignore me 6

  set xfFSBoxNearest [$xfFSBoxW nearest $xfFSBoxY]
  if {$xfFSBoxNearest >= 0} {
    $xfFSBoxW select anchor $xfFSBoxNearest
    $xfFSBoxW select set $xfFSBoxNearest
  }
}

proc XFIsADir {pathName} {# xf ignore me 5
##########
# Procedure: XFIsADir
# Description: check if name is a directory (including symbolic links)
# Arguments: pathName - the path to check
# Returns: 1 if its a directory, otherwise 0
# Sideeffects: none
##########

  if {[file isdirectory $pathName]} {
    return 1
  } {
    catch "file type $pathName" fileType
    if {"$fileType" == "link"} {
      if {[catch "file readlink $pathName" linkName]} {
        return 0
      }
      catch "file type $linkName" fileType
      while {"$fileType" == "link"} {
        if {[catch "file readlink $linkName" linkName]} {
          return 0
        }
        catch "file type $linkName" fileType
      }
      return [file isdirectory $linkName]
    }
  }
  return 0
}

proc XFIsAFile {fileName} {# xf ignore me 5
##########
# Procedure: XFIsAFile
# Description: check if filename is a file (including symbolic links)
# Arguments: fileName - the filename to check
# Returns: 1 if its a file, otherwise 0
# Sideeffects: none
##########

  if {[file isfile $fileName]} {
    return 1
  } {
    catch "file type $fileName" fileType
    if {"$fileType" == "link"} {
      if {[catch "file readlink $fileName" linkName]} {
        return 0
      }
      catch "file type $linkName" fileType
      while {"$fileType" == "link"} {
        if {[catch "file readlink $linkName" linkName]} {
          return 0
        }
        catch "file type $linkName" fileType
      }
      return [file isfile $linkName]
    }
  }
  return 0
}

proc XFIsASymlink {fileName} {# xf ignore me 5
##########
# Procedure: XFIsASymlink
# Description: check if filename is a symbolic link
# Arguments: fileName - the path/filename to check
# Returns: none
# Sideeffects: none
##########

  catch "file type $fileName" fileType
  if {"$fileType" == "link"} {
    return 1
  }
  return 0
}

# eof

