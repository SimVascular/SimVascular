#! /bin/sh
# The next line is executed by /bin/sh, but not Tcl \
  exec wish $0 ${1+"$@"}

# procedure to show window .
proc ShowWindow. {args} {# xf ignore me 7

  # Window manager configurations
  wm positionfrom . user
  wm sizefrom . ""
  wm geometry . 350x390
  wm maxsize . 1024 1024
  wm minsize . 0 0
  wm title . {xfappdef: Xdefaults}


  # build widget .frame0
  frame .frame0 \
    -borderwidth {2} \
    -relief {raised}

  # build widget .frame0.menubutton0
  menubutton .frame0.menubutton0 \
    -menu {.frame0.menubutton0.m} \
    -text {Classes} \
    -underline {0}

  # build widget .frame0.menubutton0.m
  menu .frame0.menubutton0.m -tearoff 0
  .frame0.menubutton0.m add cascade \
    -label {Tk-Classes} \
    -menu {.frame0.menubutton0.m.m}
  .frame0.menubutton0.m add separator
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Bitmap} \
    -label {B} \
    -menu {.frame0.menubutton0.m.mB}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Chooser} \
    -label {C} \
    -menu {.frame0.menubutton0.m.mC}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Doc} \
    -label {D} \
    -menu {.frame0.menubutton0.m.mD}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Editres} \
    -label {E} \
    -menu {.frame0.menubutton0.m.mE}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor GXditview} \
    -label {G} \
    -menu {.frame0.menubutton0.m.mG}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Idemo} \
    -label {I} \
    -menu {.frame0.menubutton0.m.mI}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Main} \
    -label {M} \
    -menu {.frame0.menubutton0.m.mM}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Seyon} \
    -label {S} \
    -menu {.frame0.menubutton0.m.mS}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor Viewres} \
    -label {V} \
    -menu {.frame0.menubutton0.m.mV}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor XCal} \
    -label {X} \
    -menu {.frame0.menubutton0.m.mX}
  .frame0.menubutton0.m add cascade \
    -command {.frame1.frame20.frame.entry2 insert cursor xf.ad} \
    -label {x} \
    -menu {.frame0.menubutton0.m.mx}

  # build widget .frame0.menubutton0.m.m
  menu .frame0.menubutton0.m.m -tearoff 0
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Button} \
    -label {Button}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *CheckButton} \
    -label {CheckButton}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Entry} \
    -label {Entry}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Frame} \
    -label {Frame}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Label} \
    -label {Label}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Listbox} \
    -label {Listbox}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *MenuButton} \
    -label {MenuButton}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Message} \
    -label {Message}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *RadioButton} \
    -label {RadioButton}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Scale} \
    -label {Scale}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Scrollbar} \
    -label {Scrollbar}
  .frame0.menubutton0.m.m add command \
    -command {[SymbolicName ResourceName] insert insert *Toplevel} \
    -label {Toplevel}

  # build widget .frame0.menubutton0.m.mB
  menu .frame0.menubutton0.m.mB -tearoff 0
  .frame0.menubutton0.m.mB add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Bitmap} \
    -label {Bitmap}
  .frame0.menubutton0.m.mB add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Bitmap-color} \
    -label {Bitmap-color}

  # build widget .frame0.menubutton0.m.mC
  menu .frame0.menubutton0.m.mC -tearoff 0
  .frame0.menubutton0.m.mC add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Chooser} \
    -label {Chooser}
  .frame0.menubutton0.m.mC add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Clock-color} \
    -label {Clock-color}

  # build widget .frame0.menubutton0.m.mD
  menu .frame0.menubutton0.m.mD -tearoff 0
  .frame0.menubutton0.m.mD add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Doc} \
    -label {Doc}

  # build widget .frame0.menubutton0.m.mE
  menu .frame0.menubutton0.m.mE -tearoff 0
  .frame0.menubutton0.m.mE add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Editres} \
    -label {Editres}
  .frame0.menubutton0.m.mE add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Editres-color} \
    -label {Editres-color}

  # build widget .frame0.menubutton0.m.mG
  menu .frame0.menubutton0.m.mG -tearoff 0
  .frame0.menubutton0.m.mG add command \
    -command {.frame1.frame20.frame.entry2 insert cursor GXditview} \
    -label {GXditview}
  .frame0.menubutton0.m.mG add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Ghostview} \
    -label {Ghostview}

  # build widget .frame0.menubutton0.m.mI
  menu .frame0.menubutton0.m.mI -tearoff 0
  .frame0.menubutton0.m.mI add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Idemo} \
    -label {Idemo}
  .frame0.menubutton0.m.mI add command \
    -command {.frame1.frame20.frame.entry2 insert cursor InterViews} \
    -label {InterViews}

  # build widget .frame0.menubutton0.m.mM
  menu .frame0.menubutton0.m.mM -tearoff 0
  .frame0.menubutton0.m.mM add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Main} \
    -label {Main}

  # build widget .frame0.menubutton0.m.mS
  menu .frame0.menubutton0.m.mS -tearoff 0
  .frame0.menubutton0.m.mS add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Seyon} \
    -label {Seyon}
  .frame0.menubutton0.m.mS add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Seyon-color} \
    -label {Seyon-color}

  # build widget .frame0.menubutton0.m.mV
  menu .frame0.menubutton0.m.mV -tearoff 0
  .frame0.menubutton0.m.mV add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Viewres} \
    -label {Viewres}

  # build widget .frame0.menubutton0.m.mX
  menu .frame0.menubutton0.m.mX -tearoff 0
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XCal} \
    -label {XCal}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XCalc} \
    -label {XCalc}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XCalc-color} \
    -label {XCalc-color}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XClipboard} \
    -label {XClipboard}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XClock} \
    -label {XClock}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XConsole} \
    -label {XConsole}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XFontSel} \
    -label {XFontSel}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XGas} \
    -label {XGas}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XGrab} \
    -label {XGrab}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XLoad} \
    -label {XLoad}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XLogo} \
    -label {XLogo}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XLogo-color} \
    -label {XLogo-color}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XPaint} \
    -label {XPaint}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor XTerm} \
    -label {XTerm}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xcb} \
    -label {Xcb}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xditview} \
    -label {Xditview}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xditview-chrtr} \
    -label {Xditview-chrtr}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xedit} \
    -label {Xedit}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xfd} \
    -label {Xfd}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xgc} \
    -label {Xgc}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xmag} \
    -label {Xmag}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xman} \
    -label {Xman}
  .frame0.menubutton0.m.mX add command \
    -command {.frame1.frame20.frame.entry2 insert cursor Xmh} \
    -label {Xmh}

  # build widget .frame0.menubutton0.m.mx
  menu .frame0.menubutton0.m.mx -tearoff 0
  .frame0.menubutton0.m.mx add command \
    -command {.frame1.frame20.frame.entry2 insert cursor xf.ad} \
    -label {xf.ad}

  # build widget .frame0.menubutton1
  menubutton .frame0.menubutton1 \
    -menu {.frame0.menubutton1.m} \
    -text {Resources} \
    -underline {0}

  # build widget .frame0.menubutton1.m
  menu .frame0.menubutton1.m -tearoff 0
  .frame0.menubutton1.m add cascade \
    -label {Colors} \
    -menu {.frame0.menubutton1.m.m}
  .frame0.menubutton1.m add cascade \
    -label {Size/Position} \
    -menu {.frame0.menubutton1.m.m2}
  .frame0.menubutton1.m add cascade \
    -label {Misc} \
    -menu {.frame0.menubutton1.m.m3}

  # build widget .frame0.menubutton1.m.m
  menu .frame0.menubutton1.m.m -tearoff 0
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .activeBackground} \
    -label {activeBackground}
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .activeForeground} \
    -label {activeForeground}
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .background} \
    -label {background}
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .foreground} \
    -label {foreground}
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .selector} \
    -label {selector}
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .selectBackground} \
    -label {selectBackground}
  .frame0.menubutton1.m.m add command \
    -command {[SymbolicName ResourceName] insert insert .selectForeground} \
    -label {selectForeground}

  # build widget .frame0.menubutton1.m.m2
  menu .frame0.menubutton1.m.m2 -tearoff 0
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .anchor} \
    -label {anchor}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .borderWidth} \
    -label {borderWidth}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .geometry} \
    -label {geometry}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .height} \
    -label {height}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .orient} \
    -label {orient}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .padx} \
    -label {padx}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .pady} \
    -label {pady}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .selectBorderWidth} \
    -label {selectBorderWidth}
  .frame0.menubutton1.m.m2 add command \
    -command {[SymbolicName ResourceName] insert insert .width} \
    -label {width}

  # build widget .frame0.menubutton1.m.m3
  menu .frame0.menubutton1.m.m3 -tearoff 0
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .bitmap} \
    -label {bitmap}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .command} \
    -label {command}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .cursor} \
    -label {cursor}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .exportSelection} \
    -label {exportSelection}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .font} \
    -label {font}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .relief} \
    -label {relief}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .text} \
    -label {text}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .textVariable} \
    -label {textVariable}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .variable} \
    -label {variable}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .xScrollCommand} \
    -label {xScrollCommand}
  .frame0.menubutton1.m.m3 add command \
    -command {[SymbolicName ResourceName] insert insert .yScrollCommand} \
    -label {yScrollCommand}

  # build widget .frame0.menubutton1.m.m3.m
  menu .frame0.menubutton1.m.m3.m -tearoff 0

  # build widget .frame0.menubutton12
  menubutton .frame0.menubutton12 \
    -menu {.frame0.menubutton12.m} \
    -text {File} \
    -underline {0}

  # build widget .frame0.menubutton12.m
  menu .frame0.menubutton12.m -tearoff 0
  .frame0.menubutton12.m add command \
    -command {LoadFile} \
    -label {Load ...} \
    -underline {0}
  .frame0.menubutton12.m add command \
    -command {MergeFile} \
    -label {Merge ...} \
    -underline {0}
  .frame0.menubutton12.m add command \
    -command {SaveFile} \
    -label {Save} \
    -underline {0}
  .frame0.menubutton12.m add command \
    -command {SaveFileAs} \
    -label {Save as ...} \
    -underline {5}
  .frame0.menubutton12.m add separator
  .frame0.menubutton12.m add command \
    -command {QuitProgram} \
    -label {Quit} \
    -underline {0}

  # pack widget .frame0
  pack append .frame0 \
    .frame0.menubutton12 {left frame center} \
    .frame0.menubutton0 {left frame center} \
    .frame0.menubutton1 {left frame center} 

  # build widget .frame1
  frame .frame1 \
    -relief {raised}

  # build widget .frame1.frame
  frame .frame1.frame 

  # build widget .frame1.frame.scrollbar2
  scrollbar .frame1.frame.scrollbar2 \
    -command {.frame1.frame.listbox1 yview} \
    -relief {raised}

  # build widget .frame1.frame.scrollbar3
  scrollbar .frame1.frame.scrollbar3 \
    -command {.frame1.frame.listbox1 xview} \
    -orient {horizontal} \
    -relief {raised}

  # build widget .frame1.frame.listbox1
  listbox .frame1.frame.listbox1 \
    -exportselection {0} \
    -width {5} \
    -height {5} \
    -relief {raised} \
    -xscrollcommand {.frame1.frame.scrollbar3 set} \
    -yscrollcommand {.frame1.frame.scrollbar2 set}
  # bindings
  bind .frame1.frame.listbox1 <B1-Motion> {SelectResource %W %y}
  bind .frame1.frame.listbox1 <Button-1> {SelectResource %W %y}
  bind .frame1.frame.listbox1 <Shift-B1-Motion> {SelectResource %W %y}
  bind .frame1.frame.listbox1 <Shift-Button-1> {SelectResource %W %y}

  # pack widget .frame1.frame
  pack append .frame1.frame \
    .frame1.frame.scrollbar2 {left frame center filly} \
    .frame1.frame.listbox1 {top frame center expand fill} \
    .frame1.frame.scrollbar3 {bottom frame center fillx} 

  # build widget .frame1.frame20
  frame .frame1.frame20 \
    -borderwidth {2} \
    -relief {raised}

  # build widget .frame1.frame20.frame
  frame .frame1.frame20.frame 

  # build widget .frame1.frame20.frame.scrollbar1
  scrollbar .frame1.frame20.frame.scrollbar1 \
    -command {.frame1.frame20.frame.entry2 view} \
    -orient {horizontal} \
    -width {11}

  # build widget .frame1.frame20.frame.entry2
  entry .frame1.frame20.frame.entry2 \
    -relief {sunken} \
    -xscrollcommand {.frame1.frame20.frame.scrollbar1 set}
  # bindings
  bind .frame1.frame20.frame.entry2 <Key-Return> {InsertResource}

  # pack widget .frame1.frame20.frame
  pack append .frame1.frame20.frame \
    .frame1.frame20.frame.entry2 {top frame center expand fill} \
    .frame1.frame20.frame.scrollbar1 {top frame center fillx} 

  # build widget .frame1.frame20.label1
  label .frame1.frame20.label1 \
    -text {Resource:}

  # pack widget .frame1.frame20
  pack append .frame1.frame20 \
    .frame1.frame20.label1 {left frame center filly} \
    .frame1.frame20.frame {top frame center fillx} 

  # build widget .frame1.frame7
  frame .frame1.frame7 \
    -relief {raised}

  # build widget .frame1.frame7.scrollbar1
  scrollbar .frame1.frame7.scrollbar1 \
    -command {.frame1.frame7.text2 yview} \
    -relief {raised}

  # build widget .frame1.frame7.text2
  text .frame1.frame7.text2 \
    -borderwidth {2} \
    -height 0 \
    -relief {raised} \
    -width 0 \
    -wrap {none} \
    -yscrollcommand {.frame1.frame7.scrollbar1 set}

  # pack widget .frame1.frame7
  pack append .frame1.frame7 \
    .frame1.frame7.scrollbar1 {left frame center filly} \
    .frame1.frame7.text2 {top frame center expand fill} 

  # build widget .frame1.frame8
  frame .frame1.frame8 \
    -relief {raised}

  # build widget .frame1.frame8.button10
  button .frame1.frame8.button10 \
    -command {InsertResource} \
    -text {Insert resource}

  # build widget .frame1.frame8.button11
  button .frame1.frame8.button11 \
    -command {DeleteResource} \
    -text {Delete resource}

  # pack widget .frame1.frame8
  pack append .frame1.frame8 \
    .frame1.frame8.button10 {left frame center expand fillx} \
    .frame1.frame8.button11 {left frame center expand fillx} 

  # pack widget .frame1
  pack append .frame1 \
    .frame1.frame {top frame center expand fill} \
    .frame1.frame8 {top frame center fillx} \
    .frame1.frame20 {top frame center fillx} \
    .frame1.frame7 {top frame center expand fill} 

  # build widget .frame2
  frame .frame2 \
    -relief {raised}

  # build widget .frame2.button
  button .frame2.button \
    -command {SelectFile} \
    -text {File}

  # build widget .frame2.button0
  button .frame2.button0 \
    -command {SelectPixmap} \
    -text {Pixmap}

  # build widget .frame2.button1
  button .frame2.button1 \
    -command {SelectCursor} \
    -text {Cursor}

  # build widget .frame2.button13
  button .frame2.button13 \
    -command {SelectColor} \
    -text {Color}

  # build widget .frame2.button16
  button .frame2.button16 \
    -command {SelectFont} \
    -text {Font}

  # pack widget .frame2
  pack append .frame2 \
    .frame2.button13 {left frame center expand fillx} \
    .frame2.button16 {left frame center expand fillx} \
    .frame2.button0 {left frame center expand fillx} \
    .frame2.button {left frame center expand fillx} \
    .frame2.button1 {left frame center expand fillx} 

  # build widget .templist
  listbox .templist 

  # pack widget .
  pack append . \
    .frame0 {top frame center fillx} \
    .frame1 {top frame center expand fill} \
    .frame2 {top frame center fillx} 

  if {"[info procs XFEdit]" != ""} {
    XFEditSetShowWindows
    XFMiscBindWidgetTree .xfFSBox
  }

  .frame1.frame.listbox1 insert end {*BitmapIcon}
  .frame1.frame.listbox1 insert end {*Menufreeze}
  .frame1.frame.listbox1 insert end {*Panefont}
  .frame1.frame.listbox1 insert end {*SelectionFont}
  .frame1.frame.listbox1 insert end {*Customization}
  .frame1.frame.listbox1 insert end {xterm*BorderWidth}
  .frame1.frame.listbox1 insert end {xterm*autoRaise}
  .frame1.frame.listbox1 insert end {xterm*visualBell}
  .frame1.frame.listbox1 insert end {xterm*scrollBar}
  .frame1.frame.listbox1 insert end {xterm*scrollInput}
  .frame1.frame.listbox1 insert end {xterm*scrollKey}
  .frame1.frame.listbox1 insert end {xterm*saveLines}
  .frame1.frame.listbox1 insert end {xterm*panespread}
  .frame1.frame.listbox1 insert end {xterm.windowGeometry}
  .frame1.frame.listbox1 insert end {xterm*background}
  .frame1.frame.listbox1 insert end {xterm*foreground}
  .frame1.frame.listbox1 insert end {xterm*font}
  .frame1.frame.listbox1 insert end {xterm*boldFont}
  .frame1.frame.listbox1 insert end {xterm*cursorColor}
  .frame1.frame.listbox1 insert end {emacs*BorderWidth}
  .frame1.frame.listbox1 insert end {emacs*background}
  .frame1.frame.listbox1 insert end {emacs*foreground}
  .frame1.frame.listbox1 insert end {emacs*font}
  .frame1.frame.listbox1 insert end {XConsole*consoleLog}
  .frame1.frame.listbox1 insert end {XConsole*text.translations}
  .frame1.frame.listbox1 insert end {XConsole*background}
  .frame1.frame.listbox1 insert end {XConsole*foreground}
  .frame1.frame20.frame.entry2 insert end {}
  .frame1.frame7.text2 insert end {}
  .templist insert end {true}
  .templist insert end {true}
  .templist insert end {8x13}
  .templist insert end {8x13}
  .templist insert end {-co}
  .templist insert end {1}
  .templist insert end {true}
  .templist insert end {true}
  .templist insert end {true}
  .templist insert end {true}
  .templist insert end {true}
  .templist insert end {400}
  .templist insert end {.25}
  .templist insert end {=80x32+5+5}
  .templist insert end {LightGoldenrodYellow}
  .templist insert end {black}
  .templist insert end {*courier-bold-r*14*}
  .templist insert end {*courier-bold-o*14*}
  .templist insert end {red}
  .templist insert end {1}
  .templist insert end {LightGoldenrodYellow}
  .templist insert end {black}
  .templist insert end {*courier-bold-r*14*}
  .templist insert end {/tmp/conslog}
  .templist insert end {#replace \
     <Btn1Down>:	select-start() \
     <Btn1Motion>:	extend-adjust() \
     <Btn1Up>:	extend-end(PRIMARY, CUT_BUFFER0)}
  .templist insert end {LightGoldenrodYellow}
  .templist insert end {black}


}


# Procedure: AlertBox
proc AlertBox { {alertBoxMessage "Alert message"} {alertBoxCommand ""} {alertBoxGeometry "350x150"} {alertBoxTitle "Alert box"} args} {
# xf ignore me 5
##########
# Procedure: AlertBox
# Description: show alert box
# Arguments: {alertBoxMessage} - the text to display
#            {alertBoxCommand} - the command to call after ok
#            {alertBoxGeometry} - the geometry for the window
#            {alertBoxTitle} - the title for the window
#            {args} - labels of buttons
# Returns: The number of the selected button, ot nothing
# Sideeffects: none
# Notes: there exist also functions called:
#          AlertBoxFile - to open and read a file automatically
#          AlertBoxFd - to read from an already opened filedescriptor
##########
#
# global alertBox(activeBackground) - active background color
# global alertBox(activeForeground) - active foreground color
# global alertBox(after) - destroy alert box after n seconds
# global alertBox(anchor) - anchor for message box
# global alertBox(background) - background color
# global alertBox(font) - message font
# global alertBox(foreground) - foreground color
# global alertBox(justify) - justify for message box
# global alertBox(toplevelName) - the toplevel name

  global alertBox

  # show alert box
  if {[llength $args] > 0} {
    eval AlertBoxInternal "\{$alertBoxMessage\}" "\{$alertBoxCommand\}" "\{$alertBoxGeometry\}" "\{$alertBoxTitle\}" $args
  } {
    AlertBoxInternal $alertBoxMessage $alertBoxCommand $alertBoxGeometry $alertBoxTitle
  }

  if {[llength $args] > 0} {
    # wait for the box to be destroyed
    update idletask
    grab $alertBox(toplevelName)
    tkwait window $alertBox(toplevelName)

    return $alertBox(button)
  }
}


# Procedure: AlertBoxInternal
proc AlertBoxInternal { alertBoxMessage alertBoxCommand alertBoxGeometry alertBoxTitle args} {
# xf ignore me 6
  global alertBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  if {"$alertBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$alertBox(activeBackground)\" "
  }
  if {"$alertBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$alertBox(activeForeground)\" "
  }
  if {"$alertBox(background)" != ""} {
    append tmpButtonOpt "-background \"$alertBox(background)\" "
    append tmpFrameOpt "-background \"$alertBox(background)\" "
    append tmpMessageOpt "-background \"$alertBox(background)\" "
  }
  if {"$alertBox(font)" != ""} {
    append tmpButtonOpt "-font \"$alertBox(font)\" "
    append tmpMessageOpt "-font \"$alertBox(font)\" "
  }
  if {"$alertBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$alertBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$alertBox(foreground)\" "
  }

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy $alertBox(toplevelName)}
  } {
    catch {destroy $alertBox(toplevelName)}
  }
  toplevel $alertBox(toplevelName)     -borderwidth 0
  catch "$alertBox(toplevelName) config $tmpFrameOpt"
  if {[catch "wm geometry $alertBox(toplevelName) $alertBoxGeometry"]} {
    wm geometry $alertBox(toplevelName) 350x150
  }
  wm title $alertBox(toplevelName) $alertBoxTitle
  wm maxsize $alertBox(toplevelName) 1000 1000
  wm minsize $alertBox(toplevelName) 100 100
  # end build of toplevel

  message $alertBox(toplevelName).message1     -anchor "$alertBox(anchor)"     -justify "$alertBox(justify)"     -relief raised     -text "$alertBoxMessage"
  catch "$alertBox(toplevelName).message1 config $tmpMessageOpt"

  set xfTmpWidth     [string range $alertBoxGeometry 0 [expr [string first x $alertBoxGeometry]-1]]
  if {"$xfTmpWidth" != ""} {
    # set message size
    catch "$alertBox(toplevelName).message1 configure       -width [expr $xfTmpWidth-10]"
  } {
    $alertBox(toplevelName).message1 configure       -aspect 1500
  }

  frame $alertBox(toplevelName).frame1     -borderwidth 0     -relief raised
  catch "$alertBox(toplevelName).frame1 config $tmpFrameOpt"

  set alertBoxCounter 0
  set buttonNum [llength $args]
  if {$buttonNum > 0} {
    while {$alertBoxCounter < $buttonNum} {
      button $alertBox(toplevelName).frame1.button$alertBoxCounter         -text "[lindex $args $alertBoxCounter]"         -command "
          global alertBox
          set alertBox(button) $alertBoxCounter
          if {\"\[info commands XFDestroy\]\" != \"\"} {
            catch {XFDestroy $alertBox(toplevelName)}
          } {
            catch {destroy $alertBox(toplevelName)}
          }"
      catch "$alertBox(toplevelName).frame1.button$alertBoxCounter config $tmpButtonOpt"

      pack append $alertBox(toplevelName).frame1                   $alertBox(toplevelName).frame1.button$alertBoxCounter {left fillx expand}

      incr alertBoxCounter
    }
  } {
    button $alertBox(toplevelName).frame1.button0       -text "OK"       -command "
        global alertBox
        set alertBox(button) 0
        if {\"\[info commands XFDestroy\]\" != \"\"} {
          catch {XFDestroy $alertBox(toplevelName)}
        } {
          catch {destroy $alertBox(toplevelName)}
        }
        $alertBoxCommand"
    catch "$alertBox(toplevelName).frame1.button0 config $tmpButtonOpt"

    pack append $alertBox(toplevelName).frame1                 $alertBox(toplevelName).frame1.button0 {left fillx expand}
  }

  # packing
  pack append $alertBox(toplevelName)               $alertBox(toplevelName).frame1 {bottom fill}               $alertBox(toplevelName).message1 {top fill expand}

  if {$alertBox(after) != 0} {
    after [expr $alertBox(after)*1000]       "catch \"$alertBox(toplevelName).frame1.button0 invoke\""
  }
}


# Procedure: Alias
proc Alias { args} {
# xf ignore me 7
##########
# Procedure: Alias
# Description: establish an alias for a procedure
# Arguments: args - no argument means that a list of all aliases
#                   is returned. Otherwise the first parameter is
#                   the alias name, and the second parameter is
#                   the procedure that is aliased.
# Returns: nothing, the command that is bound to the alias or a
#          list of all aliases - command pairs. 
# Sideeffects: internalAliasList is updated, and the alias
#              proc is inserted
##########
  global internalAliasList

  if {[llength $args] == 0} {
    return $internalAliasList
  } {
    if {[llength $args] == 1} {
      set xfTmpIndex [lsearch $internalAliasList "[lindex $args 0] *"]
      if {$xfTmpIndex != -1} {
        return [lindex [lindex $internalAliasList $xfTmpIndex] 1]
      }
    } {
      if {[llength $args] == 2} {
        eval "proc [lindex $args 0] {args} {#xf ignore me 4
return \[eval \"[lindex $args 1] \$args\"\]}"
        set xfTmpIndex [lsearch $internalAliasList "[lindex $args 0] *"]
        if {$xfTmpIndex != -1} {
          set internalAliasList [lreplace $internalAliasList $xfTmpIndex $xfTmpIndex "[lindex $args 0] [lindex $args 1]"]
        } {
          lappend internalAliasList "[lindex $args 0] [lindex $args 1]"
        }
      } {
        error "Alias: wrong number or args: $args"
      }
    }
  }
}


# Procedure: ClearList
proc ClearList { listWidget} {
  # Description: clear listbox widget
  # Arguments: listWidget - the widget to clear
  # Returns: none

  if {[$listWidget size] > 0} {
    $listWidget delete 0 end
  }
}


# Procedure: ColorBox
proc ColorBox { {colorBoxFileColor "/usr/local/lib/xf/lib/Colors"} {colorBoxMessage "Color"} {colorBoxEntryW ""} {colorBoxTargetW ""}} {
# xf ignore me 5
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
    toplevel .colorBox       -borderwidth 0
    catch ".colorBox config $tmpFrameOpt"
    wm geometry .colorBox 400x300
    wm title .colorBox {Color box}
    wm maxsize .colorBox 1000 1000
    wm minsize .colorBox 100 100
    # end build of toplevel

    set colorBox(oldWidget) $colorBoxEntryW

    frame .colorBox.frame1       -borderwidth 0       -relief raised
    catch ".colorBox.frame1 config $tmpFrameOpt"
 
    button .colorBox.frame1.ok       -text "OK"
    catch ".colorBox.frame1.ok config $tmpButtonOpt"

    button .colorBox.frame1.cancel       -text "Cancel"
    catch ".colorBox.frame1.cancel config $tmpButtonOpt"

    frame .colorBox.frame2       -borderwidth 0       -relief raised
    catch ".colorBox.frame2 config $tmpFrameOpt"
    radiobutton .colorBox.frame2.rgb  -command "ColorBoxShowSlides $colorBoxMessage \"$colorBoxTargetW\""  -text "RGB"  -variable colorBox(type) -value {rgb}
    catch ".colorBox.frame2.rgb config $tmpButtonOpt"

    radiobutton .colorBox.frame2.hsv  -command "ColorBoxShowSlides $colorBoxMessage \"$colorBoxTargetW\""  -text "HSV"  -variable colorBox(type) -value {hsv}
    catch ".colorBox.frame2.hsv config $tmpButtonOpt"

    radiobutton .colorBox.frame2.list  -command "ColorBoxShowSlides $colorBoxMessage \"$colorBoxTargetW\""  -text "List"  -variable colorBox(type) -value {list}
    catch ".colorBox.frame2.list config $tmpButtonOpt"

    frame .colorBox.palette       -borderwidth 0       -relief raised
    catch ".colorBox.palette config $tmpFrameOpt"
 
    set counter 0
    foreach element $colorBox(palette) {
      button .colorBox.palette.palette$counter         -command "ColorBoxSetPalette $colorBoxMessage \"$colorBoxTargetW\" $counter"                -width 3
      catch ".colorBox.palette.palette$counter config         -activebackground \"$element\"         -background \"$element\""

      pack append .colorBox.palette .colorBox.palette.palette$counter {left fill expand}
      incr counter
    }

    scale .colorBox.red       -background "red"       -from 0       -label "Red"       -orient horizontal       -relief raised       -sliderlength 15       -to 255       -width 8
    catch ".colorBox.red config $tmpScaleOpt"

    scale .colorBox.green       -background "green"       -from 0       -label "Green"       -orient horizontal       -relief raised       -sliderlength 15       -to 255       -width 8
    catch ".colorBox.green config $tmpScaleOpt"

    scale .colorBox.blue       -background "blue"       -from 0       -label "Blue"       -orient horizontal       -relief raised       -sliderlength 15       -to 255       -width 8
    catch ".colorBox.blue config $tmpScaleOpt"

    scale .colorBox.h       -from 0       -label "Hue"       -orient horizontal       -relief raised       -sliderlength 15       -to 1000       -width 8
    catch ".colorBox.h config $tmpScaleOpt"

   scale .colorBox.s      -from 0      -label "Saturation * 100"      -orient horizontal      -relief raised      -sliderlength 15      -to 1000      -width 8
    catch ".colorBox.s config $tmpScaleOpt"

    scale .colorBox.v       -from 0       -label "Value"       -orient horizontal       -relief raised       -sliderlength 15       -to 1000       -width 8
    catch ".colorBox.v config $tmpScaleOpt"

    label .colorBox.demo       -relief raised       -text "This text shows the results :-)"
    catch ".colorBox.demo config $tmpMessageOpt"

    frame .colorBox.current       -borderwidth 0       -relief raised
    catch ".colorBox.current config $tmpFrameOpt"

    label .colorBox.current.labelcurrent       -relief raised
    catch ".colorBox.current.labelcurrent config $tmpMessageOpt"

    entry .colorBox.current.current       -relief raised
    catch ".colorBox.current.current config $tmpMessageOpt"

    frame .colorBox.colors       -borderwidth 0       -relief raised
    catch ".colorBox.colors config $tmpFrameOpt"

    scrollbar .colorBox.colors.vscroll       -relief raised       -command ".colorBox.colors.colors yview"
    catch ".colorBox.colors.vscroll config $tmpScrollOpt"

    scrollbar .colorBox.colors.hscroll       -orient horiz       -relief raised       -command ".colorBox.colors.colors xview"
    catch ".colorBox.colors.hscroll config $tmpScrollOpt"

    listbox .colorBox.colors.colors       -exportselection false       -relief raised       -xscrollcommand ".colorBox.colors.hscroll set"       -yscrollcommand ".colorBox.colors.vscroll set"
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
        set colorReadList [lrange [split $colorReadList "\n"] 1 end]
        foreach colorLine $colorReadList {
          if {"[string trim $colorLine]" != ""} {
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
   
  .colorBox.frame1.ok config     -command "
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

  .colorBox.frame1.cancel config     -command "
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

  .colorBox.red config     -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.green config     -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.blue config     -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.h config     -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.s config     -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.v config     -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""

  .colorBox.current.labelcurrent config     -text "$colorBoxMessage:"

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
    
  # packing
  pack append .colorBox.frame1               .colorBox.frame1.ok {left fill expand}               .colorBox.frame1.cancel {left fill expand}
  pack append .colorBox.frame2               .colorBox.frame2.rgb {left fill expand}               .colorBox.frame2.hsv {left fill expand}               .colorBox.frame2.list {left fill expand}
  pack append .colorBox.current               .colorBox.current.labelcurrent {left}               .colorBox.current.current {left fill expand}
  pack append .colorBox.colors               .colorBox.colors.vscroll "$colorBox(scrollSide) filly"               .colorBox.colors.hscroll {bottom fillx}               .colorBox.colors.colors {left fill expand}

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


# Procedure: ColorBoxHSVToRGB
proc ColorBoxHSVToRGB { colorBoxHue colorBoxSat colorBoxValue} {
# xf ignore me 6

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
    case $i       0 {return "$colorBoxV $colorBoxT $colorBoxP"}       1 {return "$colorBoxQ $colorBoxV $colorBoxP"}       2 {return "$colorBoxP $colorBoxV $colorBoxT"}       3 {return "$colorBoxP $colorBoxQ $colorBoxV"}       4 {return "$colorBoxT $colorBoxP $colorBoxV"}       5 {return "$colorBoxV $colorBoxP $colorBoxQ"}
    error "i value $i is out of range"
  }
}


# Procedure: ColorBoxRGBToHSV
proc ColorBoxRGBToHSV { colorBoxRed colorBoxGreen colorBoxBlue} {
# xf ignore me 6

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


# Procedure: ColorBoxSelectColor
proc ColorBoxSelectColor { colorW colorBoxMessage colorBoxTargetW colorY} {
# xf ignore me 6

  set colorNearest [$colorW nearest $colorY]
  if {$colorNearest >= 0} {
    $colorW select anchor $colorNearest
    $colorW select set anchor $colorNearest
    set colorTmpValue [$colorW get $colorNearest]
    set colorCurrentColor [lrange $colorTmpValue 0           [expr [llength $colorTmpValue]-2]]
    set colorCurrentValue [lrange $colorTmpValue           [expr [llength $colorTmpValue]-1] end]

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


# Procedure: ColorBoxSetColor
proc ColorBoxSetColor { colorBoxMessage colorBoxTargetW colorBoxType colorBoxValue} {
# xf ignore me 6
  global colorBox

  .colorBox.red config     -command "NoFunction"
  .colorBox.green config     -command "NoFunction"
  .colorBox.blue config     -command "NoFunction"
  .colorBox.h config     -command "NoFunction"
  .colorBox.s config     -command "NoFunction"
  .colorBox.v config     -command "NoFunction"

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
      catch "$colorBoxTargetW config -[string tolower $colorBoxMessage]         $colorBoxValue"
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
  .colorBox.red config     -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.green config     -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.blue config     -command "ColorBoxSetRGBColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.h config     -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.s config     -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""
  .colorBox.v config     -command "ColorBoxSetHSVColor $colorBoxMessage \"$colorBoxTargetW\""
}


# Procedure: ColorBoxSetHSVColor
proc ColorBoxSetHSVColor { colorBoxMessage colorBoxTargetW colorBoxValue} {
# xf ignore me 6
  global colorBox

  set colorBoxRGB [ColorBoxHSVToRGB [expr [.colorBox.h get]/1000.0] [expr [.colorBox.s get]/1000.0] [expr [.colorBox.v get]/1000.0]]
  ColorBoxSetColor $colorBoxMessage $colorBoxTargetW hsv     [format #%04x%04x%04x [lindex $colorBoxRGB 0] [lindex $colorBoxRGB 1] [lindex $colorBoxRGB 2]]
  ColorBoxSetPaletteList [format #%04x%04x%04x [lindex $colorBoxRGB 0] [lindex $colorBoxRGB 1] [lindex $colorBoxRGB 2]]
}


# Procedure: ColorBoxSetPalette
proc ColorBoxSetPalette { colorBoxMessage colorBoxTargetW colorBoxElement} {
# xf ignore me 6
  global colorBox

  set colorBox(paletteNr) $colorBoxElement
  ColorBoxSetColor $colorBoxMessage $colorBoxTargetW palette     [lindex [.colorBox.palette.palette$colorBoxElement config -background] 4]
}


# Procedure: ColorBoxSetPaletteList
proc ColorBoxSetPaletteList { colorBoxValue} {
# xf ignore me 6
  global colorBox

  catch ".colorBox.palette.palette$colorBox(paletteNr) config       -activebackground $colorBoxValue"
  catch ".colorBox.palette.palette$colorBox(paletteNr) config       -background $colorBoxValue"
  set colorBox(palette)     [lreplace $colorBox(palette) $colorBox(paletteNr) $colorBox(paletteNr)       $colorBoxValue]
}


# Procedure: ColorBoxSetRGBColor
proc ColorBoxSetRGBColor { colorBoxMessage colorBoxTargetW colorBoxValue} {
# xf ignore me 6
  global colorBox

  ColorBoxSetColor $colorBoxMessage $colorBoxTargetW rgb     [format #%02x%02x%02x [.colorBox.red get]       [.colorBox.green get] [.colorBox.blue get]]
  ColorBoxSetPaletteList [format #%02x%02x%02x [.colorBox.red get]     [.colorBox.green get] [.colorBox.blue get]]
}


# Procedure: ColorBoxShowSlides
proc ColorBoxShowSlides { colorBoxMessage colorBoxTargetW} {
# xf ignore me 6
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
      pack append .colorBox                   .colorBox.frame1 {bottom fillx}                   .colorBox.frame2 {bottom fillx}                   .colorBox.current {bottom fillx}                   .colorBox.palette {bottom fillx}                   .colorBox.red {top fillx}                   .colorBox.green {top fillx}                   .colorBox.blue {top fillx}                   .colorBox.demo {bottom fill expand}
    }
    {hsv} {
      pack append .colorBox                   .colorBox.frame1 {bottom fillx}                   .colorBox.frame2 {bottom fillx}                   .colorBox.current {bottom fillx}                   .colorBox.palette {bottom fillx}                   .colorBox.h {top fillx}                   .colorBox.s {top fillx}                   .colorBox.v {top fillx}                   .colorBox.demo {bottom fill expand}
    }
    {list} {
      pack append .colorBox                   .colorBox.frame1 {bottom fillx}                   .colorBox.frame2 {bottom fillx}                   .colorBox.current {bottom fillx}                   .colorBox.palette {bottom fillx}                   .colorBox.demo {bottom fillx}                   .colorBox.colors {top fill expand}
    }
  }
  if {[string match "*oreground*" $colorBoxMessage]} {
    ColorBoxSetColor $colorBoxMessage $colorBoxTargetW text       [lindex [.colorBox.demo config -foreground] 4]
  } {
    ColorBoxSetColor $colorBoxMessage $colorBoxTargetW text       [lindex [.colorBox.demo config -background] 4]
  }
}


# Procedure: CursorBox
proc CursorBox { {cursorBoxFileCursor "/usr/local/lib/xf/lib/Cursors"} {cursorBoxFileColor "/usr/local/lib/xf/lib/Colors"} {cursorBoxMessage "Cursor"} {cursorBoxEntryW ""} {cursorBoxTargetW ""}} {
# xf ignore me 5
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
    toplevel .cursorBox       -borderwidth 0
    catch ".cursorBox config $tmpFrameOpt"
    wm geometry .cursorBox 400x300
    wm title .cursorBox {Cursor box}
    wm maxsize .cursorBox 1000 1000
    wm minsize .cursorBox 100 100
    # end build of toplevel

    set cursorBox(oldWidget) $cursorBoxEntryW

    frame .cursorBox.frame1       -borderwidth 0       -relief raised
    catch ".cursorBox.frame1 config $tmpFrameOpt"
 
    button .cursorBox.frame1.ok       -text "OK"
    catch ".cursorBox.frame1.ok config $tmpButtonOpt"

    button .cursorBox.frame1.cancel       -text "Cancel"
    catch ".cursorBox.frame1.cancel config $tmpButtonOpt"

    label .cursorBox.demo       -relief raised       -text "This text shows the results :-) (enter this widget)"
    catch ".cursorBox.demo config $tmpMessageOpt"

    frame .cursorBox.current       -borderwidth 0       -relief raised
    catch ".cursorBox.current config $tmpFrameOpt"

    label .cursorBox.current.labelcurrent       -relief raised
    catch ".cursorBox.current.labelcurrent config $tmpMessageOpt"

    entry .cursorBox.current.current       -relief raised
    catch ".cursorBox.current.current config $tmpMessageOpt"

    frame .cursorBox.fg       -borderwidth 0       -relief raised
    catch ".cursorBox config.fg $tmpFrameOpt"

    label .cursorBox.fg.labelfg       -relief raised       -text "Foreground:"
    catch ".cursorBox.fg.labelfg config $tmpMessageOpt"

    entry .cursorBox.fg.fg       -relief raised
    catch ".cursorBox.fg.fg config $tmpMessageOpt"

    frame .cursorBox.bg       -borderwidth 0       -relief raised
    catch ".cursorBox.bg config $tmpFrameOpt"

    label .cursorBox.bg.labelbg       -relief raised       -text "Background:"
    catch ".cursorBox.bg.labelbg config $tmpMessageOpt"

    entry .cursorBox.bg.bg       -relief raised
    catch ".cursorBox.bg.bg config $tmpMessageOpt"

    frame .cursorBox.cursors       -borderwidth 0       -relief raised
    catch ".cursorBox.cursor config $tmpFrameOpt"

    scrollbar .cursorBox.cursors.vscroll       -relief raised       -command ".cursorBox.cursors.cursors yview"
    catch ".cursorBox.cursors.vscroll config $tmpScrollOpt"

    scrollbar .cursorBox.cursors.hscroll       -orient horiz       -relief raised       -command ".cursorBox.cursors.cursors xview"
    catch ".cursorBox.cursors.hscroll config $tmpScrollOpt"

    listbox .cursorBox.cursors.cursors       -exportselection false       -relief raised       -xscrollcommand ".cursorBox.cursors.hscroll set"       -yscrollcommand ".cursorBox.cursors.vscroll set"
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
        while {1} {
          if {[gets $cursorInFile cursorLine] == -1} {
            break
          }
          .cursorBox.cursors.cursors insert end $cursorLine
        }
        close $cursorInFile
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
      .cursorBox.bg.bg icursor 0
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
      .cursorBox.fg.fg icursor 0
      focus .cursorBox.fg.fg}
    bind .cursorBox.bg.bg <Down> {
      .cursorBox.current.current icursor 0
      focus .cursorBox.current.current}

    bind .cursorBox.current.current <Up> {
      .cursorBox.bg.bg icursor 0
      focus .cursorBox.bg.bg}
    catch "bind .cursorBox.current.current <Down> {}"
  } {
    if {"[.cursorBox.fg.fg get]" != "" &&
        "[.cursorBox.bg.bg get]" != ""} {
     set tmpCursorBox          "[.cursorBox.current.current get] {[.cursorBox.fg.fg get]} {[.cursorBox.bg.bg get]}"
    } {
      if {"[.cursorBox.fg.fg get]" != ""} {
        set tmpCursorBox           "[.cursorBox.current.current get] {[.cursorBox.fg.fg get]}"
      } {
        if {"[.cursorBox.bg.bg get]" != ""} {
          set tmpCursorBox             "[.cursorBox.current.current get] {[.cursorBox.bg.bg get]}"
        } {
          set tmpCursorBox             "[.cursorBox.current.current get]"
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

  .cursorBox.frame1.ok config     -command "
      global cursorBox
      if {\"\[.cursorBox.fg.fg get\]\" != \"\" &&
          \"\[.cursorBox.bg.bg get\]\" != \"\"} {
       set cursorBox(cursorName)            \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]} {\[.cursorBox.bg.bg get\]}\"
      } {
        if {\"\[.cursorBox.fg.fg get\]\" != \"\"} {
          set cursorBox(cursorName)             \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]}\"
        } {
          if {\"\[.cursorBox.bg.bg get\]\" != \"\"} {
            set cursorBox(cursorName)               \"\[.cursorBox.current.current get\] {\[.cursorBox.bg.bg get\]}\"
          } {
            set cursorBox(cursorName)               \"\[.cursorBox.current.current get\]\"
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

  .cursorBox.frame1.cancel config     -command "
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

  .cursorBox.current.labelcurrent config     -text "$cursorBoxMessage:"

  # bindings
  bind .cursorBox.fg.fg <Return> "
    CursorBoxSetCursor \"$cursorBoxMessage\" \"$cursorBoxTargetW\"
    .cursorBox.bg.bg icursor 0
    focus .cursorBox.bg.bg"

  bind .cursorBox.bg.bg <Return> "
    CursorBoxSetCursor \"$cursorBoxMessage\" \"$cursorBoxTargetW\"
    .cursorBox.current.current icursor 0
    focus .cursorBox.current.current"

  bind .cursorBox.current.current <Return> "
    CursorBoxSetCursor \"$cursorBoxMessage\" \"$cursorBoxTargetW\""

  bind .cursorBox.cursors.cursors <Double-1> "
    CursorBoxSelectCursor %W \"$cursorBoxMessage\" \"$cursorBoxTargetW\" %y
    global cursorBox
    if {\"\[.cursorBox.fg.fg get\]\" != \"\" &&
        \"\[.cursorBox.bg.bg get\]\" != \"\"} {
     set cursorBox(cursorName)          \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]} {\[.cursorBox.bg.bg get\]}\"
    } {
      if {\"\[.cursorBox.fg.fg get\]\" != \"\"} {
         set cursorBox(cursorName)            \"\[.cursorBox.current.current get\] {\[.cursorBox.fg.fg get\]}\"
      } {
        if {\"\[.cursorBox.bg.bg get\]\" != \"\"} {
          set cursorBox(cursorName)             \"\[.cursorBox.current.current get\] {\[.cursorBox.bg.bg get\]}\"
        } {
          set cursorBox(cursorName)             \"\[.cursorBox.current.current get\]\"
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
  pack append .cursorBox.frame1               .cursorBox.frame1.ok {left fill expand}               .cursorBox.frame1.cancel {left fill expand}
  pack append .cursorBox.current               .cursorBox.current.labelcurrent {left}               .cursorBox.current.current {left fill expand}
  pack append .cursorBox.fg               .cursorBox.fg.labelfg {left}               .cursorBox.fg.fg {left fill expand}
  pack append .cursorBox.bg               .cursorBox.bg.labelbg {left}               .cursorBox.bg.bg {left fill expand}
  pack append .cursorBox.cursors               .cursorBox.cursors.vscroll "$cursorBox(scrollSide) filly"               .cursorBox.cursors.hscroll {bottom fillx}               .cursorBox.cursors.cursors {left fill expand}

  if {"$cursorBoxFileCursor" != ""} {
    pack append .cursorBox                 .cursorBox.frame1 {bottom fillx}                 .cursorBox.current {bottom fillx}                 .cursorBox.bg {bottom fillx}                 .cursorBox.fg {bottom fillx}                 .cursorBox.demo {bottom fillx}                 .cursorBox.cursors {left expand fill}
  } {
    wm geometry .cursorBox 400x110
    pack append .cursorBox                 .cursorBox.frame1 {bottom fillx}                 .cursorBox.current {bottom fillx}                 .cursorBox.bg {bottom fillx}                 .cursorBox.fg {bottom fillx}                 .cursorBox.demo {bottom fill expand}
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


# Procedure: CursorBoxSelectCursor
proc CursorBoxSelectCursor { cursorBoxW cursorBoxMessage cursorBoxTargetW cursorBoxY} {
# xf ignore me 6

  set cursorBoxNearest [$cursorBoxW nearest $cursorBoxY]
  if {$cursorBoxNearest >= 0} {
    $cursorBoxW select anchor $cursorBoxNearest
    $cursorBoxW select set anchor $cursorBoxNearest
    .cursorBox.current.current delete 0 end
    .cursorBox.current.current insert 0 [$cursorBoxW get $cursorBoxNearest]
    CursorBoxSetCursor "$cursorBoxMessage" "$cursorBoxTargetW"
  }
}


# Procedure: CursorBoxSetCursor
proc CursorBoxSetCursor { cursorBoxMessage cursorBoxTargetW} {
# xf ignore me 6

  if {"[.cursorBox.current.current get]" != ""} {
    if {"[.cursorBox.fg.fg get]" != "" &&
        "[.cursorBox.bg.bg get]" != ""} {
      catch ".cursorBox.demo config -cursor         \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]} {[.cursorBox.bg.bg get]}\""
      if {"$cursorBoxTargetW" != ""} {
        catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage]           \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]} {[.cursorBox.bg.bg get]}\""
      }
    } {
      if {"[.cursorBox.fg.fg get]" != ""} {
        catch ".cursorBox.demo config -cursor           \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]}\""
        if {"$cursorBoxTargetW" != ""} {
          catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage]             \"{[.cursorBox.current.current get]} {[.cursorBox.fg.fg get]}\""
        }
      } {
        if {"[.cursorBox.bg.bg get]" != ""} {
          catch ".cursorBox.demo config -cursor             \"{[.cursorBox.current.current get]} {[.cursorBox.bg.bg get]}\""
          if {"$cursorBoxTargetW" != ""} {
            catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage]               \"{[.cursorBox.current.current get]} {[.cursorBox.bg.bg get]}\""
          }
        } {
          catch ".cursorBox.demo config -cursor             \"{[.cursorBox.current.current get]}\""
          if {"$cursorBoxTargetW" != ""} {
            catch "$cursorBoxTargetW config -[string tolower $cursorBoxMessage]               \"{[.cursorBox.current.current get]}\""
          }
        }
      }
    }
  }
}


# Procedure: DeleteResource
proc DeleteResource {} {

  set counter 0
  set current [[SymbolicName ResourceName] get]
  set listLength [[SymbolicName ResourceNameList] size]
  while {$counter < $listLength} {
    if {"$current" == "[[SymbolicName ResourceNameList] get $counter]"} {
      [SymbolicName ResourceNameList] delete $counter
      [SymbolicName ResourceValueList] delete $counter
      [SymbolicName ResourceName] delete 0 end
      [SymbolicName ResourceValue] delete 1.0 end
      [SymbolicName ResourceNameList] select anchor $counter
      [SymbolicName ResourceNameList] select set anchor $counter
      [SymbolicName ResourceName] insert 0 [[SymbolicName ResourceNameList] get $counter]
      [SymbolicName ResourceValue] insert 1.0 [[SymbolicName ResourceValueList] get $counter]
    }
    incr counter 1
  }
}


# Procedure: FSBox
proc FSBox { {fsBoxMessage "Select file:"} {fsBoxFileName ""} {fsBoxActionOk ""} {fsBoxActionCancel ""}} {
# xf ignore me 5
##########
# Procedure: FSBox
# Description: show file selector box
# Arguments: fsBoxMessage - the text to display
#            fsBoxFileName - a file name that should be selected
#            fsBoxActionOk - the action that should be performed on ok
#            fsBoxActionCancel - the action that should be performed on cancel
# Returns: the filename that was selected, or nothing
# Sideeffects: none
##########
# 
# global fsBox(activeBackground) - active background color
# global fsBox(activeForeground) - active foreground color
# global fsBox(background) - background color
# global fsBox(font) - text font
# global fsBox(foreground) - foreground color
# global fsBox(extensions) - scan directory for extensions
# global fsBox(scrollActiveForeground) - scrollbar active background color
# global fsBox(scrollBackground) - scrollbar background color
# global fsBox(scrollForeground) - scrollbar foreground color
# global fsBox(scrollSide) - side where scrollbar is located

  global fsBox

  set tmpButtonOpt ""
  set tmpFrameOpt ""
  set tmpMessageOpt ""
  set tmpScaleOpt ""
  set tmpScrollOpt ""
  if {"$fsBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$fsBox(activeBackground)\" "
  }
  if {"$fsBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$fsBox(activeForeground)\" "
  }
  if {"$fsBox(background)" != ""} {
    append tmpButtonOpt "-background \"$fsBox(background)\" "
    append tmpFrameOpt "-background \"$fsBox(background)\" "
    append tmpMessageOpt "-background \"$fsBox(background)\" "
  }
  if {"$fsBox(font)" != ""} {
    append tmpButtonOpt "-font \"$fsBox(font)\" "
    append tmpMessageOpt "-font \"$fsBox(font)\" "
  }
  if {"$fsBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$fsBox(foreground)\" "
    append tmpMessageOpt "-foreground \"$fsBox(foreground)\" "
  }
  if {"$fsBox(scrollActiveForeground)" != ""} {
    append tmpScrollOpt "-activeforeground \"$fsBox(scrollActiveForeground)\" "
  }
  if {"$fsBox(scrollBackground)" != ""} {
    append tmpScrollOpt "-background \"$fsBox(scrollBackground)\" "
  }
  if {"$fsBox(scrollForeground)" != ""} {
    append tmpScrollOpt "-foreground \"$fsBox(scrollForeground)\" "
  }

  if {[file exists [file tail $fsBoxFileName]] &&
      [IsAFile [file tail $fsBoxFileName]]} {
    set fsBox(name) [file tail $fsBoxFileName]
  } {
    set fsBox(name) ""
  }
  if {[file exists $fsBoxFileName] && [IsADir $fsBoxFileName]} {
    set fsBox(path) $fsBoxFileName
  } {
    if {"[file rootname $fsBoxFileName]" != "."} {
      set fsBox(path) [file rootname $fsBoxFileName]
    }
  }
  if {$fsBox(showPixmap)} {
    set fsBox(path) [string trimleft $fsBox(path) @]
  }
  if {"$fsBox(path)" != "" && [file exists $fsBox(path)] &&
      [IsADir $fsBox(path)]} {
    set fsBox(internalPath) $fsBox(path)
  } {
    if {"$fsBox(internalPath)" == "" ||
        ![file exists $fsBox(internalPath)]} {
      set fsBox(internalPath) [pwd]
    }
  }
  # build widget structure

  # start build of toplevel
  if {"[info commands XFDestroy]" != ""} {
    catch {XFDestroy .fsBox}
  } {
    catch {destroy .fsBox}
  }
  toplevel .fsBox     -borderwidth 0
  catch ".fsBox config $tmpFrameOpt"
  wm geometry .fsBox 350x300 
  wm title .fsBox {File select box}
  wm maxsize .fsBox 1000 1000
  wm minsize .fsBox 100 100
  # end build of toplevel

  label .fsBox.message1     -anchor c     -relief raised     -text "$fsBoxMessage"
  catch ".fsBox.message1 config $tmpMessageOpt"

  frame .fsBox.frame1     -borderwidth 0     -relief raised
  catch ".fsBox.frame1 config $tmpFrameOpt"

  button .fsBox.frame1.ok     -text "OK"     -command "
      global fsBox
      set fsBox(name) \[.fsBox.file.file get\]
      if {$fsBox(showPixmap)} {
        set fsBox(path) @\[.fsBox.path.path get\]
      } {
        set fsBox(path) \[.fsBox.path.path get\]
      }
      set fsBox(internalPath) \[.fsBox.path.path get\]
      $fsBoxActionOk
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fsBox}
      } {
        catch {destroy .fsBox}
      }"
  catch ".fsBox.frame1.ok config $tmpButtonOpt"

  button .fsBox.frame1.rescan     -text "Rescan"     -command {
      global fsBox
      FSBoxFSShow [.fsBox.path.path get]         [.fsBox.pattern.pattern get] $fsBox(all)}
  catch ".fsBox.frame1.rescan config $tmpButtonOpt"

  button .fsBox.frame1.cancel     -text "Cancel"     -command "
      global fsBox
      set fsBox(name) {}
      set fsBox(path) {}
      $fsBoxActionCancel
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fsBox}
      } {
        catch {destroy .fsBox}
      }"
  catch ".fsBox.frame1.cancel config $tmpButtonOpt"

  if {$fsBox(showPixmap)} {
    frame .fsBox.frame2       -borderwidth 0       -relief raised
    catch ".fsBox.frame2 config $tmpFrameOpt"

    scrollbar .fsBox.frame2.scrollbar3       -command {.fsBox.frame2.canvas2 xview}       -orient {horizontal}       -relief {raised}
    catch ".fsBox.frame2.scrollbar3 config $tmpScrollOpt"

    scrollbar .fsBox.frame2.scrollbar1       -command {.fsBox.frame2.canvas2 yview}       -relief {raised}
    catch ".fsBox.frame2.scrollbar1 config $tmpScrollOpt"

    canvas .fsBox.frame2.canvas2       -confine {true}       -relief {raised}       -scrollregion {0c 0c 20c 20c}       -width {100}       -xscrollcommand {.fsBox.frame2.scrollbar3 set}       -yscrollcommand {.fsBox.frame2.scrollbar1 set}
    catch ".fsBox.frame2.canvas2 config $tmpFrameOpt"

    .fsBox.frame2.canvas2 addtag currentBitmap withtag [.fsBox.frame2.canvas2 create bitmap 5 5 -anchor nw]
  }

  frame .fsBox.path     -borderwidth 0     -relief raised
  catch ".fsBox.path config $tmpFrameOpt"

  frame .fsBox.path.paths     -borderwidth 2     -relief raised
  catch ".fsBox.path.paths config $tmpFrameOpt"

  menubutton .fsBox.path.paths.paths     -borderwidth 0     -menu ".fsBox.path.paths.paths.menu"     -relief flat     -text "Pathname:"
  catch ".fsBox.path.paths.paths config $tmpButtonOpt"

  menu .fsBox.path.paths.paths.menu -tearoff 0
  catch ".fsBox.path.paths.paths.menu config $tmpButtonOpt"

  .fsBox.path.paths.paths.menu add command      -label "[string trimright $fsBox(internalPath) {/@}]"      -command "
       global fsBox
       FSBoxFSShow \[.fsBox.path.path get\]          \[.fsBox.pattern.pattern get\] \$fsBox(all)
       .fsBox.path.path delete 0 end
       .fsBox.path.path insert 0 [string trimright $fsBox(internalPath) {/@}]"

  entry .fsBox.path.path     -relief raised
  catch ".fsBox.path.path config $tmpMessageOpt"

  if {![IsADir $fsBox(internalPath)]} {
    set $fsBox(internalPath) [pwd]
  }
  .fsBox.path.path insert 0 $fsBox(internalPath)

  frame .fsBox.pattern     -borderwidth 0     -relief raised
  catch ".fsBox.pattern config $tmpFrameOpt"

  frame .fsBox.pattern.patterns     -borderwidth 2     -relief raised
  catch ".fsBox.pattern.patterns config $tmpFrameOpt"

  menubutton .fsBox.pattern.patterns.patterns     -borderwidth 0     -menu ".fsBox.pattern.patterns.patterns.menu"     -relief flat     -text "Selection pattern:"
  catch ".fsBox.pattern.patterns.patterns config $tmpButtonOpt"

  menu .fsBox.pattern.patterns.patterns.menu -tearoff 0
  catch ".fsBox.pattern.patterns.patterns.menu config $tmpButtonOpt"

  .fsBox.pattern.patterns.patterns.menu add checkbutton     -label "Scan extensions"     -variable fsBoxExtensions     -command {
      global fsBox
      FSBoxFSShow [.fsBox.path.path get]         [.fsBox.pattern.pattern get] $fsBox(all)}

  entry .fsBox.pattern.pattern     -relief raised
  catch ".fsBox.pattern.pattern config $tmpMessageOpt"

  .fsBox.pattern.pattern insert 0 $fsBox(pattern)
  
  frame .fsBox.files     -borderwidth 0     -relief raised
  catch ".fsBox.files config $tmpFrameOpt"

  scrollbar .fsBox.files.vscroll     -relief raised     -command ".fsBox.files.files yview"
  catch ".fsBox.files.vscroll config $tmpScrollOpt"

  scrollbar .fsBox.files.hscroll     -orient horiz     -relief raised     -command ".fsBox.files.files xview"
  catch ".fsBox.files.hscroll config $tmpScrollOpt"

  listbox .fsBox.files.files     -exportselection false     -relief raised     -xscrollcommand ".fsBox.files.hscroll set"     -yscrollcommand ".fsBox.files.vscroll set"
  catch ".fsBox.files.files config $tmpMessageOpt"

  frame .fsBox.file     -borderwidth 0     -relief raised
  catch ".fsBox.file config $tmpFrameOpt"

  label .fsBox.file.labelfile     -relief raised     -text "Filename:"
  catch ".fsBox.file.labelfile config $tmpMessageOpt"

  entry .fsBox.file.file     -relief raised
  catch ".fsBox.file.file config $tmpMessageOpt"

  .fsBox.file.file delete 0 end
  .fsBox.file.file insert 0 $fsBox(name)
  
  checkbutton .fsBox.pattern.all     -offvalue 0     -onvalue 1     -text "Show all files"     -variable fsBox(all)     -command {
      global fsBox
      FSBoxFSShow [.fsBox.path.path get]         [.fsBox.pattern.pattern get] $fsBox(all)}
  catch ".fsBox.pattern.all config $tmpButtonOpt"

  FSBoxFSShow $fsBox(internalPath) $fsBox(pattern) $fsBox(all)

  # bindings
  bind .fsBox.files.files <Double-Button-1> "
    FSBoxFSFileSelectDouble %W $fsBox(showPixmap) \{$fsBoxActionOk\} %y"
  bind .fsBox.files.files <ButtonPress-1> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"
  bind .fsBox.files.files <Button1-Motion> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"
  bind .fsBox.files.files <Shift-Button1-Motion> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"
  bind .fsBox.files.files <Shift-ButtonPress-1> "
    FSBoxFSFileSelect %W $fsBox(showPixmap) %y"

  bind .fsBox.path.path <Tab> {
    FSBoxFSNameComplete path}
  bind .fsBox.path.path <Return> {
    global fsBox
    FSBoxFSShow [.fsBox.path.path get]       [.fsBox.pattern.pattern get] $fsBox(all)
    FSBoxFSInsertPath
    .fsBox.file.file icursor end
    focus .fsBox.file.file}
  catch "bind .fsBox.path.path <Up> {}"
  bind .fsBox.path.path <Down> {
    .fsBox.file.file icursor end
    focus .fsBox.file.file}

  bind .fsBox.file.file <Tab> {
    FSBoxFSNameComplete file}
  bind .fsBox.file.file <Return> "
    global fsBox
    set fsBox(name) \[.fsBox.file.file get\]
    if {$fsBox(showPixmap)} {
      set fsBox(path) @\[.fsBox.path.path get\]
    } {
      set fsBox(path) \[.fsBox.path.path get\]
    }
    set fsBox(internalPath) \[.fsBox.path.path get\]
    $fsBoxActionOk
    if {\"\[info commands XFDestroy\]\" != \"\"} {
      catch {XFDestroy .fsBox}
    } {
      catch {destroy .fsBox}
    }"
  bind .fsBox.file.file <Up> {
    .fsBox.path.path icursor end
    focus .fsBox.path.path}
  bind .fsBox.file.file <Down> {
    .fsBox.pattern.pattern icursor end
    focus .fsBox.pattern.pattern}

  bind .fsBox.pattern.pattern <Return> {
    global fsBox
    FSBoxFSShow [.fsBox.path.path get]       [.fsBox.pattern.pattern get] $fsBox(all)}
  bind .fsBox.pattern.pattern <Up> {
    .fsBox.file.file icursor end
    focus .fsBox.file.file}
  catch "bind .fsBox.pattern.pattern <Down> {}"

  # packing
  pack append .fsBox.files               .fsBox.files.vscroll "$fsBox(scrollSide) filly"               .fsBox.files.hscroll {bottom fillx}               .fsBox.files.files {left fill expand}
  pack append .fsBox.file               .fsBox.file.labelfile {left}               .fsBox.file.file {left fill expand}
  pack append .fsBox.frame1               .fsBox.frame1.ok {left fill expand}               .fsBox.frame1.rescan {left fill expand}               .fsBox.frame1.cancel {left fill expand}
  pack append .fsBox.path.paths               .fsBox.path.paths.paths {left}
  pack append .fsBox.pattern.patterns               .fsBox.pattern.patterns.patterns {left}
  pack append .fsBox.path               .fsBox.path.paths {left}               .fsBox.path.path {left fill expand}
  pack append .fsBox.pattern               .fsBox.pattern.patterns {left}               .fsBox.pattern.all {right fill}               .fsBox.pattern.pattern {left fill expand}
  if {$fsBox(showPixmap)} {
    pack append .fsBox.frame2                 .fsBox.frame2.scrollbar1 {left filly}                 .fsBox.frame2.canvas2 {top expand fill}                 .fsBox.frame2.scrollbar3 {top fillx} 

    pack append .fsBox                 .fsBox.message1 {top fill}                 .fsBox.frame1 {bottom fill}                 .fsBox.pattern {bottom fill}                 .fsBox.file {bottom fill}                 .fsBox.path {bottom fill}                 .fsBox.frame2 {right fill}                 .fsBox.files {left fill expand}
  } {
    pack append .fsBox                 .fsBox.message1 {top fill}                 .fsBox.frame1 {bottom fill}                 .fsBox.pattern {bottom fill}                 .fsBox.file {bottom fill}                 .fsBox.path {bottom fill}                 .fsBox.files {left fill expand}
  }

  if {"$fsBoxActionOk" == "" && "$fsBoxActionCancel" == ""} {
    # wait for the box to be destroyed
    update idletask
    grab .fsBox
    tkwait window .fsBox

    if {"[string trim $fsBox(path)]" != "" ||
        "[string trim $fsBox(name)]" != ""} {
      if {"[string trimleft [string trim $fsBox(name)] /]" == ""} {
        return [string trimright [string trim $fsBox(path)] /]
      } {
        return [string trimright [string trim $fsBox(path)] /]/[string trimleft [string trim $fsBox(name)] /]
      }
    }
  }
}


# Procedure: FSBoxBindSelectOne
proc FSBoxBindSelectOne { fsBoxW fsBoxY} {
# xf ignore me 6

  set fsBoxNearest [$fsBoxW nearest $fsBoxY]
  if {$fsBoxNearest >= 0} {
    $fsBoxW select anchor $fsBoxNearest
    $fsBoxW select set anchor $fsBoxNearest
  }
}


# Procedure: FSBoxFSFileSelect
proc FSBoxFSFileSelect { fsBoxW fsBoxShowPixmap fsBoxY} {
# xf ignore me 6
  global fsBox

  FSBoxBindSelectOne $fsBoxW $fsBoxY
  set fsBoxNearest [$fsBoxW nearest $fsBoxY]
  if {$fsBoxNearest >= 0} {
    set fsBoxTmpEntry [$fsBoxW get $fsBoxNearest]
    if {"[string index $fsBoxTmpEntry           [expr [string length $fsBoxTmpEntry]-1]]" == "/" ||
        "[string index $fsBoxTmpEntry           [expr [string length $fsBoxTmpEntry]-1]]" == "@"} {
      set fsBoxFileName [string range $fsBoxTmpEntry 0             [expr [string length $fsBoxTmpEntry]-2]]
      if {![IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]] &&
          ![IsASymlink [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
        set fsBoxFileName $fsBoxTmpEntry
      }
    } {
      if {"[string index $fsBoxTmpEntry             [expr [string length $fsBoxTmpEntry]-1]]" == "*"} {
        set fsBoxFileName [string range $fsBoxTmpEntry 0           [expr [string length $fsBoxTmpEntry]-2]]
        if {![file executable $fsBox(internalPath)/$fsBoxFileName]} {
          set fsBoxFileName $fsBoxTmpEntry
        }
      } {
        set fsBoxFileName $fsBoxTmpEntry
      }
    }
    if {![IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
      set fsBox(name) $fsBoxFileName
      .fsBox.file.file delete 0 end
      .fsBox.file.file insert 0 $fsBox(name)
      if {$fsBoxShowPixmap} {
        catch ".fsBox.frame2.canvas2 itemconfigure currentBitmap -bitmap \"@$fsBox(internalPath)/$fsBox(name)\""
      }
    }
  }
}


# Procedure: FSBoxFSFileSelectDouble
proc FSBoxFSFileSelectDouble { fsBoxW fsBoxShowPixmap fsBoxAction fsBoxY} {
# xf ignore me 6
  global fsBox

  FSBoxBindSelectOne $fsBoxW $fsBoxY
  set fsBoxNearest [$fsBoxW nearest $fsBoxY]
  if {$fsBoxNearest >= 0} {
    set fsBoxTmpEntry [$fsBoxW get $fsBoxNearest]
    if {"$fsBoxTmpEntry" == "../"} {
      set fsBoxTmpEntry [string trimright [string trim $fsBox(internalPath)] "@/"]
      if {"$fsBoxTmpEntry" == ""} {
        return
      }
      FSBoxFSShow [file dirname $fsBoxTmpEntry]         [.fsBox.pattern.pattern get] $fsBox(all)
      .fsBox.path.path delete 0 end
      .fsBox.path.path insert 0 $fsBox(internalPath)
    } {
      if {"[string index $fsBoxTmpEntry             [expr [string length $fsBoxTmpEntry]-1]]" == "/" ||
          "[string index $fsBoxTmpEntry             [expr [string length $fsBoxTmpEntry]-1]]" == "@"} {
        set fsBoxFileName [string range $fsBoxTmpEntry 0               [expr [string length $fsBoxTmpEntry]-2]]
        if {![IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]] &&
            ![IsASymlink [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
          set fsBoxFileName $fsBoxTmpEntry
        }
      } {
        if {"[string index $fsBoxTmpEntry               [expr [string length $fsBoxTmpEntry]-1]]" == "*"} {
          set fsBoxFileName [string range $fsBoxTmpEntry 0                 [expr [string length $fsBoxTmpEntry]-2]]
          if {![file executable $fsBox(internalPath)/$fsBoxFileName]} {
            set fsBoxFileName $fsBoxTmpEntry
          }
        } {
          set fsBoxFileName $fsBoxTmpEntry
        }
      }
      if {[IsADir [string trimright $fsBox(internalPath)/$fsBoxFileName @]]} {
        set fsBox(internalPath) "[string trimright $fsBox(internalPath) {/@}]/$fsBoxFileName"
        FSBoxFSShow $fsBox(internalPath)           [.fsBox.pattern.pattern get] $fsBox(all)
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 $fsBox(internalPath)
      } {
        set fsBox(name) $fsBoxFileName
        if {$fsBoxShowPixmap} {
          set fsBox(path) @$fsBox(internalPath)
        } {
          set fsBox(path) $fsBox(internalPath)
        }
        if {"$fsBoxAction" != ""} {
          eval "global fsBox; $fsBoxAction"
        }
        if {"[info commands XFDestroy]" != ""} {
          catch {XFDestroy .fsBox}
        } {
          catch {destroy .fsBox}
        }
      }
    }
  }
}


# Procedure: FSBoxFSInsertPath
proc FSBoxFSInsertPath {} {
# xf ignore me 6
  global fsBox

  set fsBoxLast [.fsBox.path.paths.paths.menu index last]
  set fsBoxNewEntry [string trimright [.fsBox.path.path get] "/@"]
  for {set fsBoxCounter 0} {$fsBoxCounter <= $fsBoxLast} {incr fsBoxCounter 1} {
    if {"$fsBoxNewEntry" ==           "[lindex [.fsBox.path.paths.paths.menu entryconfigure                     $fsBoxCounter -label] 4]"} {
      return
    }
  }
  if {$fsBoxLast < 9} {
    .fsBox.path.paths.paths.menu add command       -label "$fsBoxNewEntry"       -command "
        global fsBox
        FSBoxFSShow $fsBoxNewEntry           \[.fsBox.pattern.pattern get\] \$fsBox(all)
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 $fsBoxNewEntry"
  } {
    for {set fsBoxCounter 0} {$fsBoxCounter < $fsBoxLast} {incr fsBoxCounter 1} {
      .fsBox.path.paths.paths.menu entryconfigure         $fsBoxCounter -label           [lindex [.fsBox.path.paths.paths.menu entryconfigure             [expr $fsBoxCounter+1] -label] 4]
      .fsBox.path.paths.paths.menu entryconfigure $fsBoxCounter         -command "
          global fsBox
          FSBoxFSShow [lindex [.fsBox.path.paths.paths.menu entryconfigure             [expr $fsBoxCounter+1] -label] 4]             \[.fsBox.pattern.pattern get\] \$fsBox(all)
          .fsBox.path.path delete 0 end
          .fsBox.path.path insert 0 [lindex             [.fsBox.path.paths.paths.menu entryconfigure               [expr $fsBoxCounter+1] -label] 4]"
    }
    .fsBox.path.paths.paths.menu entryconfigure $fsBoxLast       -label "$fsBoxNewEntry"
    .fsBox.path.paths.paths.menu entryconfigure $fsBoxCounter       -command "
        global fsBox
        FSBoxFSShow \[.fsBox.path.path get\]           \[.fsBox.pattern.pattern get\] \$fsBox(all)
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 $fsBoxNewEntry"
  }
}


# Procedure: FSBoxFSNameComplete
proc FSBoxFSNameComplete { fsBoxType} {
# xf ignore me 6
  global fsBox

  set fsBoxNewFile ""
  if {"$fsBoxType" == "path"} {
    set fsBoxDirName [file dirname [.fsBox.path.path get]]
    set fsBoxFileName [file tail [.fsBox.path.path get]]
  } {
    set fsBoxDirName [file dirname [.fsBox.path.path get]/]
    set fsBoxFileName [file tail [.fsBox.file.file get]]
  }

  set fsBoxNewFile ""
  if {[IsADir [string trimright $fsBoxDirName @]]} {
    catch "glob -nocomplain $fsBoxDirName/${fsBoxFileName}*" fsBoxResult
    foreach fsBoxCounter $fsBoxResult {
      if {"$fsBoxNewFile" == ""} {
        set fsBoxNewFile [file tail $fsBoxCounter]
      } {
        if {"[string index [file tail $fsBoxCounter] 0]" !=
            "[string index $fsBoxNewFile 0]"} {
          set fsBoxNewFile ""
          break
        }
        set fsBoxCounter1 0
        set fsBoxTmpFile1 $fsBoxNewFile
        set fsBoxTmpFile2 [file tail $fsBoxCounter]
        set fsBoxLength1 [string length $fsBoxTmpFile1]
        set fsBoxLength2 [string length $fsBoxTmpFile2]
        set fsBoxNewFile ""
        if {$fsBoxLength1 > $fsBoxLength2} {
          set fsBoxLength1 $fsBoxLength2
        }
        while {$fsBoxCounter1 < $fsBoxLength1} {
          if {"[string index $fsBoxTmpFile1 $fsBoxCounter1]" ==                 "[string index $fsBoxTmpFile2 $fsBoxCounter1]"} {
            append fsBoxNewFile [string index $fsBoxTmpFile1 $fsBoxCounter1]
          } {
            break
          }
          incr fsBoxCounter1 1
        }
      }
    }
  }
  if {"$fsBoxNewFile" != ""} {
    if {[IsADir [string trimright $fsBoxDirName/$fsBoxNewFile @]] ||
        ![IsAFile [string trimright $fsBoxDirName/$fsBoxNewFile @]]} {
      if {[IsADir [string trimright $fsBoxDirName/$fsBoxNewFile @]]} {
        if {"$fsBoxDirName" == "/"} {
          .fsBox.path.path delete 0 end
          .fsBox.path.path insert 0 "/[string trimright [string trim $fsBoxNewFile /] @]/"
        } {
          .fsBox.path.path delete 0 end
          .fsBox.path.path insert 0 "[string trimright $fsBoxDirName /]/[string trimright [string trim $fsBoxNewFile /] @]/"
        }
        FSBoxFSShow [.fsBox.path.path get]           [.fsBox.pattern.pattern get] $fsBox(all)
        FSBoxFSInsertPath
      } {
        .fsBox.path.path delete 0 end
        .fsBox.path.path insert 0 "[string trimright $fsBoxDirName /]/[string trimright [string trim $fsBoxNewFile /] @]"
      }
    } {
      .fsBox.path.path delete 0 end
      .fsBox.path.path insert 0 "[string trimright $fsBoxDirName {@/}]/"
      .fsBox.file.file delete 0 end
      .fsBox.file.file insert 0 $fsBoxNewFile
      .fsBox.file.file icursor end
      focus .fsBox.file.file
    }
  }
}


# Procedure: FSBoxFSShow
proc FSBoxFSShow { fsBoxPath fsBoxPattern fsBoxAll} {
# xf ignore me 6
  global fsBox

  set tmpButtonOpt ""
  if {"$fsBox(activeBackground)" != ""} {
    append tmpButtonOpt "-activebackground \"$fsBox(activeBackground)\" "
  }
  if {"$fsBox(activeForeground)" != ""} {
    append tmpButtonOpt "-activeforeground \"$fsBox(activeForeground)\" "
  }
  if {"$fsBox(background)" != ""} {
    append tmpButtonOpt "-background \"$fsBox(background)\" "
  }
  if {"$fsBox(font)" != ""} {
    append tmpButtonOpt "-font \"$fsBox(font)\" "
  }
  if {"$fsBox(foreground)" != ""} {
    append tmpButtonOpt "-foreground \"$fsBox(foreground)\" "
  }

  set fsBox(pattern) $fsBoxPattern
  if {[file exists $fsBoxPath] && [file readable $fsBoxPath] &&
      [IsADir $fsBoxPath]} {
    set fsBox(internalPath) $fsBoxPath
  } {
    if {[file exists $fsBoxPath] && [file readable $fsBoxPath] &&
        [IsAFile $fsBoxPath]} {
      set fsBox(internalPath) [file dirname $fsBoxPath]
      .fsBox.file.file delete 0 end
      .fsBox.file.file insert 0 [file tail $fsBoxPath]
      set fsBoxPath $fsBox(internalPath)
    } {
      while {"$fsBoxPath" != "" && "$fsBoxPath" != "/" &&
             ![file isdirectory $fsBoxPath]} {
        set fsBox(internalPath) [file dirname $fsBoxPath]
         set fsBoxPath $fsBox(internalPath)
      }
    }
  }
  if {"$fsBoxPath" == ""} {
    set fsBoxPath "/"
    set fsBox(internalPath) "/"
  }
  .fsBox.path.path delete 0 end
  .fsBox.path.path insert 0 $fsBox(internalPath)

  if {[.fsBox.files.files size] > 0} {
    .fsBox.files.files delete 0 end
  }
  if {$fsBoxAll} {
    if {[catch "Ls -F -a $fsBoxPath" fsBoxResult]} {
      puts stderr "$fsBoxResult"
    }
  } {
    if {[catch "Ls -F $fsBoxPath" fsBoxResult]} {
      puts stderr "$fsBoxResult"
    }
  }
  set fsBoxElementList [lsort $fsBoxResult]

  foreach fsBoxCounter [winfo children .fsBox.pattern.patterns.patterns] {
    if {[string length [info commands XFDestroy]] > 0} {
      catch {XFDestroy $fsBoxCounter}
    } {
      catch {destroy $fsBoxCounter}
    }
  }
  menu .fsBox.pattern.patterns.patterns.menu -tearoff 0
  catch ".fsBox.pattern.patterns.patterns.menu config $tmpButtonOpt"

  if {$fsBox(extensions)} {
    .fsBox.pattern.patterns.patterns.menu add command       -label "*"       -command {
        global fsBox
        set fsBox(pattern) "*"
        .fsBox.pattern.pattern delete 0 end
        .fsBox.pattern.pattern insert 0 $fsBox(pattern)
        FSBoxFSShow [.fsBox.path.path get] $fsBox(pattern)           $fsBox(all)}
  }

  if {"$fsBoxPath" != "/"} {
    .fsBox.files.files insert end "../"
  }
  foreach fsBoxCounter $fsBoxElementList {
    if {[string match $fsBoxPattern $fsBoxCounter] ||
        [IsADir [string trimright $fsBoxPath/$fsBoxCounter "/@"]]} {
      if {"$fsBoxCounter" != "../" &&
          "$fsBoxCounter" != "./"} {
        .fsBox.files.files insert end $fsBoxCounter
      }
    }

    if {$fsBox(extensions)} {
      catch "file rootname $fsBoxCounter" fsBoxRootName
      catch "file extension $fsBoxCounter" fsBoxExtension
      set fsBoxExtension [string trimright $fsBoxExtension "/*@"]
      if {"$fsBoxExtension" != "" && "$fsBoxRootName" != ""} {
        set fsBoxInsert 1
        set fsBoxLast [.fsBox.pattern.patterns.patterns.menu index last]
        for {set fsBoxCounter1 0} {$fsBoxCounter1 <= $fsBoxLast} {incr fsBoxCounter1 1} {
          if {"*$fsBoxExtension" ==                 "[lindex [.fsBox.pattern.patterns.patterns.menu entryconfigure                         $fsBoxCounter1 -label] 4]"} {
            set fsBoxInsert 0
          }
        }
	if {$fsBoxInsert} {
          .fsBox.pattern.patterns.patterns.menu add command             -label "*$fsBoxExtension"             -command "
              global fsBox
              set fsBox(pattern) \"*$fsBoxExtension\"
              .fsBox.pattern.pattern delete 0 end
              .fsBox.pattern.pattern insert 0 \$fsBox(pattern)
              FSBoxFSShow \[.fsBox.path.path get\] \$fsBox(pattern)                 \$fsBox(all)"
        }
      }
    }
  }
  if {$fsBox(extensions)} {
    .fsBox.pattern.patterns.patterns.menu add separator
  }
  if {$fsBox(extensions) || 
      "[.fsBox.pattern.patterns.patterns.menu index last]" == "none"} {
    .fsBox.pattern.patterns.patterns.menu add checkbutton       -label "Scan extensions"       -variable "fsBox(extensions)"       -command {
        global fsBox
        FSBoxFSShow [.fsBox.path.path get]           [.fsBox.pattern.pattern get] $fsBox(all)}
  }
}


# Procedure: FontBox
proc FontBox { {fontBoxFileFont "/usr/local/lib/xf/lib/Fonts"} {fontBoxMessage "Font"} {fontBoxEntryW ""} {fontBoxTargetW ""}} {
# xf ignore me 5
##########
# Procedure: FontBox
# Description: select a font
# Arguments: {fontBoxFileFont} - the font file with all fontnames
#            {fontBoxMessage} - a message to display
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
# global fontBox(scrollActiveForeground) - scrollbar active background color
# global fontBox(scrollBackground) - scrollbar background color
# global fontBox(scrollForeground) - scrollbar foreground color
# global fontBox(scrollSide) - side where scrollbar is located

  global fontBox

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
    append tmpButtonOpt "-font \"$fontBox(font)\" "
    append tmpMessageOpt "-font \"$fontBox(font)\" "
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
  if {"$fontBoxMessage" == ""} {
    set fontBoxMessage "Font"
  }

  # save the the current widget color
  if {"$fontBoxTargetW" != ""} {
    if {[catch "$fontBoxTargetW config -font" result]} {
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
    toplevel .fontBox       -borderwidth 0
    catch ".fontBox config $tmpFrameOpt"
    wm geometry .fontBox 600x300
    wm title .fontBox {Font box}
    wm maxsize .fontBox 1000 1000
    wm minsize .fontBox 100 90
    # end build of toplevel

    set fontBox(oldWidget) $fontBoxEntryW

    frame .fontBox.frame1       -borderwidth 0       -relief raised
    catch ".fontBox.frame1 config $tmpFrameOpt"

    frame .fontBox.frame2       -borderwidth 2       -relief raised
    catch ".fontBox.frame2 config $tmpFrameOpt"

    button .fontBox.frame1.ok       -text "OK"
    catch ".fontBox.frame1.ok config $tmpButtonOpt"

    button .fontBox.frame1.rescan       -text "Rescan fonts"       -command "
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
            while {1} {
              if {\[gets \$fontInFile fontLine\] == -1} {
                break
              }
              .fontBox.fonts.fonts insert end \$fontLine
            }
            close \$fontInFile
          }
        }"
    catch ".fontBox.frame1.rescan config $tmpButtonOpt"

    button .fontBox.frame1.cancel       -text "Cancel"
    catch ".fontBox.frame1.cancel config $tmpButtonOpt"

    label .fontBox.demo       -relief raised       -text "This text shows the results :-)"
    catch ".fontBox.demo config $tmpMessageOpt"

    frame .fontBox.current       -borderwidth 0       -relief raised
    catch ".fontBox.current config $tmpFrameOpt"

    label .fontBox.current.labelcurrent       -relief raised
    catch ".fontBox.current.labelcurrent config $tmpMessageOpt"

    entry .fontBox.current.current       -relief raised
    catch ".fontBox.current.current config $tmpMessageOpt"

    frame .fontBox.fonts       -borderwidth 0       -relief raised
    catch ".fontBox.fonts config $tmpFrameOpt"

    scrollbar .fontBox.fonts.vscroll       -relief raised       -command ".fontBox.fonts.fonts yview"
    catch ".fontBox.fonts.vscroll config $tmpScrollOpt"

    scrollbar .fontBox.fonts.hscroll       -orient horiz       -relief raised       -command ".fontBox.fonts.fonts xview"
    catch ".fontBox.fonts.hscroll config $tmpScrollOpt"

    listbox .fontBox.fonts.fonts       -exportselection false       -relief raised       -xscrollcommand ".fontBox.fonts.hscroll set"       -yscrollcommand ".fontBox.fonts.vscroll set"
    catch ".fontBox.fonts.fonts config $tmpMessageOpt"

    # family menu
    menubutton .fontBox.frame2.family       -text "Family"       -underline 0       -menu ".fontBox.frame2.family.m"
    catch ".fontBox.frame2.family config $tmpButtonOpt"

    menu .fontBox.frame2.family.m -tearoff 0
    catch ".fontBox.frame2.family.m config $tmpButtonOpt"

    .fontBox.frame2.family.m add radiobutton       -label "*"       -value "*"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "charter"       -value "charter"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "courier"       -value "courier"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "fixed"       -value "fixed"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "helvetica"       -value "helvetica"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "lucida"       -value "lucida"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "terminal"       -value "terminal"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"
    .fontBox.frame2.family.m add radiobutton       -label "times"       -value "times"       -variable fontBox(fontFamily)       -command "FontBoxComposeFont"

    # weight menu
    menubutton .fontBox.frame2.weight       -text "Weight"       -underline 0       -menu ".fontBox.frame2.weight.m"
    catch ".fontBox.frame2.weight config $tmpButtonOpt"

    menu .fontBox.frame2.weight.m -tearoff 0
    catch ".fontBox.frame2.weight.m config $tmpButtonOpt"

    .fontBox.frame2.weight.m add radiobutton       -label "*"       -value "*"       -variable fontBox(fontWeight)       -command "FontBoxComposeFont"
    .fontBox.frame2.weight.m add radiobutton       -label "bold"       -value "bold"       -variable fontBox(fontWeight)       -command "FontBoxComposeFont"
    .fontBox.frame2.weight.m add radiobutton       -label "demibold"       -value "demibold"       -variable fontBox(fontWeight)       -command "FontBoxComposeFont"
    .fontBox.frame2.weight.m add radiobutton       -label "medium"       -value "medium"       -variable fontBox(fontWeight)       -command "FontBoxComposeFont"

    # Slant menu
    menubutton .fontBox.frame2.slant       -text "Slant"       -underline 0       -menu ".fontBox.frame2.slant.m"
    catch ".fontBox.frame2.slant config $tmpButtonOpt"

    menu .fontBox.frame2.slant.m -tearoff 0
    catch ".fontBox.frame2.slant.m config $tmpButtonOpt"

    .fontBox.frame2.slant.m add radiobutton       -label "*"       -value "*"       -variable fontBox(fontSlant)       -command "FontBoxComposeFont"
    .fontBox.frame2.slant.m add radiobutton       -label "i"       -value "i"       -variable fontBox(fontSlant)       -command "FontBoxComposeFont"
    .fontBox.frame2.slant.m add radiobutton       -label "o"       -value "o"       -variable fontBox(fontSlant)       -command "FontBoxComposeFont"
    .fontBox.frame2.slant.m add radiobutton       -label "r"       -value "r"       -variable fontBox(fontSlant)       -command "FontBoxComposeFont"

    # Set width menu
    menubutton .fontBox.frame2.swidth       -text "Set width"       -underline 1       -menu ".fontBox.frame2.swidth.m"
    catch ".fontBox.frame2.swidth config $tmpButtonOpt"

    menu .fontBox.frame2.swidth.m -tearoff 0
    catch ".fontBox.frame2.swidth.m config $tmpButtonOpt"

    .fontBox.frame2.swidth.m add radiobutton       -label "*"       -value "*"       -variable fontBox(fontSWidth)       -command "FontBoxComposeFont"
    .fontBox.frame2.swidth.m add radiobutton       -label "normal"       -value "normal"       -variable fontBox(fontSWidth)       -command "FontBoxComposeFont"
    .fontBox.frame2.swidth.m add radiobutton       -label "semicondensed"       -value "semicondensed"       -variable fontBox(fontSWidth)       -command "FontBoxComposeFont"

    # pixels menu
    menubutton .fontBox.frame2.pixels       -text "Pixels"       -underline 0       -menu ".fontBox.frame2.pixels.m"
    catch ".fontBox.frame2.pixels config $tmpButtonOpt"

    menu .fontBox.frame2.pixels.m -tearoff 0
    catch ".fontBox.frame2.pixels.m config $tmpButtonOpt"

    .fontBox.frame2.pixels.m add radiobutton       -label "*"       -value "*"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "6 pixels"       -value "6"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "8 pixels"       -value "8"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "10 pixels"       -value "10"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "12 pixels"       -value "12"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "13 pixels"       -value "13"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "14 pixels"       -value "14"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "16 pixels"       -value "16"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "18 pixels"       -value "18"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "24 pixels"       -value "24"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "28 pixels"       -value "28"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"
    .fontBox.frame2.pixels.m add radiobutton       -label "30 pixels"       -value "30"       -variable fontBox(fontPixels)       -command "FontBoxComposeFont"

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
        while {1} {
          if {[gets $fontInFile fontLine] == -1} {
            break
          }
          .fontBox.fonts.fonts insert end $fontLine
        }
        close $fontInFile
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

  .fontBox.frame1.ok config     -command "
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

  .fontBox.frame1.cancel config     -command "
      global fontBox
      set fontBox(fontName) {}
      if {\"$fontBoxTargetW\" != \"\"} {
        catch \"$fontBoxTargetW config -font $fontBoxSavedFont\"
      }
      if {\"\[info commands XFDestroy\]\" != \"\"} {
        catch {XFDestroy .fontBox}
      } {
        catch {destroy .fontBox}
      }"

  .fontBox.current.labelcurrent config     -text "$fontBoxMessage:"

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
  pack append .fontBox.frame1               .fontBox.frame1.ok {left fill expand}               .fontBox.frame1.rescan {left fill expand}               .fontBox.frame1.cancel {left fill expand}
  pack append .fontBox.frame2               .fontBox.frame2.family {left}               .fontBox.frame2.weight {left}               .fontBox.frame2.slant {left}               .fontBox.frame2.swidth {left}               .fontBox.frame2.pixels {left}
  pack append .fontBox.current               .fontBox.current.labelcurrent {left}               .fontBox.current.current {left fill expand}
  pack append .fontBox.fonts               .fontBox.fonts.vscroll "$fontBox(scrollSide) filly"               .fontBox.fonts.hscroll {bottom fillx}               .fontBox.fonts.fonts {left fill expand}

  if {"$fontBoxFileFont" != ""} {
    pack append .fontBox                 .fontBox.frame1 {bottom fillx}                 .fontBox.current {bottom fillx}                 .fontBox.demo {bottom fillx}                 .fontBox.frame2 {top fill}                 .fontBox.fonts {left expand fill}
  } {
    wm geometry .fontBox 400x90
    pack append .fontBox                 .fontBox.frame1 {bottom fillx}                 .fontBox.current {bottom fillx}                 .fontBox.frame2 {top fill}                 .fontBox.demo {bottom fill expand}
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


# Procedure: FontBoxComposeFont
proc FontBoxComposeFont {} {
# xf ignore me 6
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
  catch ".fontBox.demo config     -font $fontNewFont"
}


# Procedure: FontBoxSelectFont
proc FontBoxSelectFont { fontBoxW fontBoxTargetW fontBoxY} {
# xf ignore me 6

  set fontBoxNearest [$fontBoxW nearest $fontBoxY]
  if {$fontBoxNearest >= 0} {
    $fontBoxW select anchor $fontBoxNearest
    $fontBoxW select set anchor $fontBoxNearest
    .fontBox.current.current delete 0 end
    .fontBox.current.current insert 0 [$fontBoxW get $fontBoxNearest]
    FontBoxSetFont "$fontBoxTargetW"
  }
}


# Procedure: FontBoxSetFont
proc FontBoxSetFont { fontBoxTargetW} {
# xf ignore me 6

  if {"[.fontBox.current.current get]" != ""} {
    catch ".fontBox.demo config -font       [.fontBox.current.current get]"
    if {"$fontBoxTargetW" != ""} {
      catch "$fontBoxTargetW config -font         [.fontBox.current.current get]"
    }
  }
}


# Procedure: GetSelection
if {"[info procs GetSelection]" == ""} {
proc GetSelection {} {
# xf ignore me 7
##########
# Procedure: GetSelection
# Description: get current selection
# Arguments: none
# Returns: none
# Sideeffects: none
##########

  # the save way
  set xfSelection ""
  catch "selection get" xfSelection
  if {"$xfSelection" == "selection doesn't exist or form \"STRING\" not defined"} {
    return ""
  } {
    return $xfSelection
  }
}
}


# Procedure: InsertResource
proc InsertResource {} {

  set replaced 0
  set counter 0
  set current [[SymbolicName ResourceName] get]
  set currentValue [[SymbolicName ResourceValue] get 1.0 end]
  set listLength [[SymbolicName ResourceNameList] size]
  if {[string length $current] > 0} {
    while {$counter < $listLength} {
      if {"$current" == "[[SymbolicName ResourceNameList] get $counter]"} {
        [SymbolicName ResourceValueList] delete $counter
        [SymbolicName ResourceValueList] insert $counter $currentValue
        set replaced 1
      }
      incr counter 1
    }
    if {$replaced == 0} {
      [SymbolicName ResourceNameList] insert end $current
      [SymbolicName ResourceValueList] insert end $currentValue
    }
  }
}


# Procedure: IsADir
proc IsADir { pathName} {
# xf ignore me 5
##########
# Procedure: IsADir
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


# Procedure: IsAFile
proc IsAFile { fileName} {
# xf ignore me 5
##########
# Procedure: IsAFile
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


# Procedure: IsASymlink
proc IsASymlink { fileName} {
# xf ignore me 5
##########
# Procedure: IsASymlink
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


# Procedure: LoadFile
proc LoadFile {} {
  global appFileName

  set selFile [FSBox]
  if {"$selFile" != ""} {
    set appFileName $selFile
    ClearList [SymbolicName ResourceNameList]
    ClearList [SymbolicName ResourceValueList]
    ReadFile $appFileName
  }
}


proc Ls {args} {
global tcl_platform
if {$tcl_platform(platform) == "unix"} {eval exec ls $args} {
   set last [lindex $args end]
   if {[lsearch $last -* ] >= 0 || $last == "" }\
      {set path "*"}\
      {set path $last/*}
   set lst [glob $path]
   set lst1 ""
   if {[lsearch -exact $args "-F"] >= 0} then {set car "/"} else {set car ""}
   foreach f $lst {
      if {[file isdirectory $f]} \
         {set f [file tail $f]$car}\
         {set f [file tail $f]}
      lappend lst1 $f
   }
   return [lsort $lst1]
}
}


# Procedure: MergeFile
proc MergeFile {} {

  set selFile [FSBox]
  if {"$selFile" != ""} {
    ReadFile $selFile
  }
}


# Procedure: NoFunction
if {"[info procs NoFunction]" == ""} {
proc NoFunction { args} {
# xf ignore me 7
##########
# Procedure: NoFunction
# Description: do nothing (especially with scales and scrollbars)
# Arguments: args - a number of ignored parameters
# Returns: none
# Sideeffects: none
##########
}
}


# Procedure: QuitProgram
proc QuitProgram {} {

  catch "destroy ."
  catch "exit 0"
}


# Procedure: ReadFile
proc ReadFile { fileName {firstRead "0"}} {

  wm title . "xfappdef: $fileName"
  if {[catch "open $fileName r" inFile]} {
    if {!$firstRead} {
      AlertBox "$inFile"
    }
  } {
    set newLine ""
    set fileContents [read $inFile]
    close $inFile
    foreach readLine [split $fileContents "\n"] {
      if {"" == "[string trim $readLine]" || "!" == "[string index $readLine 0]" || "#" == "[string index $readLine 0]"} {
        continue
      }
      if {"\\" == "[string index $readLine [expr [string length $readLine]-1]]"} {
        append newLine [string range $readLine 0 [expr [string length $readLine]-1]]
      } {
        append newLine $readLine
        if {"[string trim $newLine]" != ""} {
          [SymbolicName ResourceNameList] insert end [string trim [string range $newLine 0 [expr [string first ":" $newLine]-1]]]
          set resValue [string trim [string range $newLine [expr [string first ":" $newLine]+1] end]]
          if {[regsub -all "\[\\\]n\[\\\]" $resValue "\\\n" resResult]} {
            set resValue $resResult
          }
          if {[regsub -all "\[\\\]n" $resValue "\n" resResult]} {
            set resValue $resResult
          }
          [SymbolicName ResourceValueList] insert end $resValue
        }
        set newLine ""
      }
    }
  }
}


# Procedure: ReadFirstFile
proc ReadFirstFile {} {
  global env
  global appFileName

  ClearList [SymbolicName ResourceNameList]
  ClearList [SymbolicName ResourceValueList]
  set xfFileList ""
  if {[info exists env(XUSERFILESEARCHPATH)]} {
    append xfFileList [split $env(XUSERFILESEARCHPATH) :]
  }
  if {[info exists env(XAPPLRESDIR)]} {
    append xfFileList [split $env(XAPPLRESDIR) :]
  }
  if {[info exists env(XFILESEARCHPATH)]} {
    append xfFileList [split $env(XFILESEARCHPATH) :]
  }
  append xfFileList " /usr/lib/X11/app-defaults"
  append xfFileList " /usr/X11/lib/X11/app-defaults"

  foreach xfCounter2 $xfFileList {
    set xfPathName $xfCounter2
    if {[regsub -all "%N" "$xfPathName" "" xfResult]} {
      set xfPathName $xfResult
    }
    if {[regsub -all "%T" "$xfPathName" "app-defaults" xfResult]} {
      set xfPathName $xfResult
    }
    if {[regsub -all "%S" "$xfPathName" "" xfResult]} {
      set xfPathName $xfResult
    }
    if {[regsub -all "%C" "$xfPathName" "" xfResult]} {
      set xfPathName $xfResult
    }
    catch "glob -nocomplain $xfPathName/*" xfResult
    foreach xfCounter1 $xfResult {
      set fileList($xfCounter1) ""
    }
  }
  set menuLast [.frame0.menubutton0.m index last]
  if {"$menuLast" == "none"} {
    set menuLast -1
  }
  for {set xfCounter1 2} {$xfCounter1 <= $menuLast} {incr xfCounter1 1} {
    destroy [lindex [.frame0.menubutton0.m entryconfig $xfCounter1 -menu] 4]
  }
  for {set xfCounter1 2} {$xfCounter1 <= $menuLast} {incr xfCounter1 1} {
    .frame0.menubutton0.m delete 2
  }
  set firstChar ""
  foreach xfCounter1 [lsort [array names fileList]] {
    if {"[string index $xfCounter1 0]" != $firstChar} {
      set firstChar [string index $xfCounter1 0]
      menu .frame0.menubutton0.m.m$firstChar -tearoff 0
      .frame0.menubutton0.m add cascade        -label "$firstChar"        -menu ".frame0.menubutton0.m.m$firstChar"
    }

    .frame0.menubutton0.m.m$firstChar add command      -command "[SymbolicName ResourceName] insert insert $xfCounter1"       -label "$xfCounter1"
  }
  if {[info exists fileList]} {unset fileList}

  ReadFile $appFileName 1
}


# Procedure: SN
if {"[info procs SN]" == ""} {
proc SN { {xfName ""}} {
# xf ignore me 7
##########
# Procedure: SN
# Description: map a symbolic name to the widget path
# Arguments: xfName
# Returns: the symbolic name
# Sideeffects: none
##########

  SymbolicName $xfName
}
}


# Procedure: SaveFile
proc SaveFile {} {
  global appFileName

  set counter 0
  set listLength [[SymbolicName ResourceNameList] size]
  if {[file exists $appFileName]} {
    catch "mv $appFileName $appFileName~"
  }
  if {[catch "open $appFileName w" outFile]} {
    AlertBox "$outFile"
  } {
    while {$counter < $listLength} {
      puts $outFile "[[SymbolicName ResourceNameList] get $counter]:" nonewline
      set currentValue [string trim [[SymbolicName ResourceValueList] get $counter]]
      set currentValueLength [string length $currentValue]
      set position 0
      set oldPosition 0
      while {$position < $currentValueLength} {
        while {$position < $currentValueLength} {
          set current [string index $currentValue $position]
          if {[string match $current "\n"] &&
              ![string match $current "\*"]} {
            break
          }
          incr position 1
        }
        if {[string match $current "\n"] &&
            ![string match $current "\*"] &&
            $position < $currentValueLength} {
          if {"\\" == "[string index $currentValue [expr $position-1]]"} {
            puts $outFile "[string range $currentValue $oldPosition [expr $position-1]]n\\"
          } {
            puts $outFile "[string range $currentValue $oldPosition [expr $position-1]]\\n\\"
          }
        } {
           puts $outFile "[string range $currentValue $oldPosition $position]"
        }
        incr position 1
        set oldPosition $position
      }
      incr counter 1
    }
    close $outFile
  }
}


# Procedure: SaveFileAs
proc SaveFileAs {} {
  global appFileName

  set selFile [FSBox]
  if {"$selFile" != ""} {
    set appFileName $selFile
    SaveFile
  }
}


# Procedure: SelectColor
proc SelectColor {} {

  set selColor [ColorBox]
  if {"$selColor" != ""} {
    [SymbolicName ResourceValue] delete 1.0 end
    [SymbolicName ResourceValue] insert end $selColor
  }
}


# Procedure: SelectCursor
proc SelectCursor {} {

  set selCursor [CursorBox]
  if {"$selCursor" != ""} {
    [SymbolicName ResourceValue] delete 1.0 end
    [SymbolicName ResourceValue] insert end $selCursor
  }
}


# Procedure: SelectFile
proc SelectFile {} {

  set selFile [FSBox]
  if {"$selFile" != ""} {
    [SymbolicName ResourceValue] delete 1.0 end
    [SymbolicName ResourceValue] insert end $selFile
  }
}


# Procedure: SelectFont
proc SelectFont {} {

  set selFont [FontBox]
  if {"$selFont" != ""} {
    [SymbolicName ResourceValue] delete 1.0 end
    [SymbolicName ResourceValue] insert end $selFont
  }
}


# Procedure: SelectPixmap
proc SelectPixmap {} {
  global fsBox

  set fsBox(showPixmap) 1
  set selFile [FSBox]
  set fsBox(showPixmap) 0
  if {"$selFile" != ""} {
    [SymbolicName ResourceValue] delete 1.0 end
    if {"[string index $selFile 0]" == "@"} {
      [SymbolicName ResourceValue] insert end [string range $selFile 1 end]
    } {
      [SymbolicName ResourceValue] insert end $selFile
    }
  }
}


# Procedure: SelectResource
proc SelectResource { w y} {

  set nearest [$w nearest $y]
  $w select anchor $nearest
  $w select set anchor $nearest
  [SymbolicName ResourceName] delete 0 end
  [SymbolicName ResourceName] insert end [[SymbolicName ResourceNameList] get $nearest]
  [SymbolicName ResourceValue] delete 1.0 end
  [SymbolicName ResourceValue] insert 1.0 [[SymbolicName ResourceValueList] get $nearest]
}


# Procedure: SymbolicName
if {"[info procs SymbolicName]" == ""} {
proc SymbolicName { {xfName ""}} {
# xf ignore me 7
##########
# Procedure: SymbolicName
# Description: map a symbolic name to the widget path
# Arguments: xfName
# Returns: the symbolic name
# Sideeffects: none
##########

  global symbolicName

  if {"$xfName" != ""} {
    set xfArrayName ""
    append xfArrayName symbolicName ( $xfName )
    if {![catch "set \"$xfArrayName\"" xfValue]} {
      return $xfValue
    } {
      if {"[info commands XFProcError]" != ""} {
        XFProcError "Unknown symbolic name:\n$xfName"
      } {
        puts stderr "XF error: unknown symbolic name:\n$xfName"
      }
    }
  }
  return ""
}
}


# Procedure: Unalias
proc Unalias { aliasName} {
# xf ignore me 7
##########
# Procedure: Unalias
# Description: remove an alias for a procedure
# Arguments: aliasName - the alias name to remove
# Returns: none
# Sideeffects: internalAliasList is updated, and the alias
#              proc is removed
##########
  global internalAliasList

  set xfIndex [lsearch $internalAliasList "$aliasName *"]
  if {$xfIndex != -1} {
    rename $aliasName ""
    set internalAliasList [lreplace $internalAliasList $xfIndex $xfIndex]
  }
}

# module load procedure
proc XFLocalIncludeModule {{moduleName ""}} {
  global env
  global xfLoadInfo
  global xfLoadPath
  global xfStatus

  foreach p [split $xfLoadPath :] {
    if {[file exists "$p/$moduleName"]} {
      if {![file readable "$p/$moduleName"]} {
        puts stderr "Cannot read $p/$moduleName (permission denied)"
        continue
      }
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName..."
      }
      source "$p/$moduleName"
      return 1
    }
    # first see if we have a load command
    if {[info exists env(XF_VERSION_SHOW)]} {
      set xfCommand $env(XF_VERSION_SHOW)
      regsub -all {\$xfFileName} $xfCommand $p/$moduleName xfCommand
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName...($xfCommand)"
      }
      if {[catch "$xfCommand" contents]} {
        continue
      } {
        eval $contents
        return 1
      }
    }
    # are we able to load versions from wish ?
    if {[catch "afbind $p/$moduleName" aso]} {
      # try to use xf version load command
      global xfVersion
      if {[info exists xfVersion(showDefault)]} {
        set xfCommand $xfVersion(showDefault)
      } {
	# our last hope
        set xfCommand "vcat -q $p/$moduleName"
      }
      regsub -all {\$xfFileName} $xfCommand $p/$moduleName xfCommand
      if {$xfLoadInfo} {
        puts stdout "Loading $p/$moduleName...($xfCommand)"
      }
      if {[catch "$xfCommand" contents]} {
        continue
      } {
        eval $contents
        return 1
      }
    } {
      # yes we can load versions directly
      if {[catch "$aso open r" inFile]} {
        puts stderr "Cannot open $p/[$aso attr af_bound] (permission denied)"
        continue
      }
      if {$xfLoadInfo} {
        puts stdout "Loading $p/[$aso attr af_bound]..."
      }
      if {[catch "read \{$inFile\}" contents]} {
        puts stderr "Cannot read $p/[$aso attr af_bound] (permission denied)"
        close $inFile
        continue
      }
      close $inFile
      eval $contents
      return 1
    }
  }
  puts stderr "Cannot load module $moduleName -- check your xf load path"
  catch "destroy ."
  catch "exit 0"
}

# application parsing procedure
proc XFLocalParseAppDefs {xfAppDefFile} {
  global xfAppDefaults

  # basically from: Michael Moore
  if {[file exists $xfAppDefFile] &&
      [file readable $xfAppDefFile] &&
      "[file type $xfAppDefFile]" == "link"} {
    catch "file type $xfAppDefFile" xfType
    while {"$xfType" == "link"} {
      if {[catch "file readlink $xfAppDefFile" xfAppDefFile]} {
        return
      }
      catch "file type $xfAppDefFile" xfType
    }
  }
  if {!("$xfAppDefFile" != "" &&
        [file exists $xfAppDefFile] &&
        [file readable $xfAppDefFile] &&
        "[file type $xfAppDefFile]" == "file")} {
    return
  }
  if {![catch "open $xfAppDefFile r" xfResult]} {
    set xfAppFileContents [read $xfResult]
    close $xfResult
    foreach line [split $xfAppFileContents "\n"] {
      # backup indicates how far to backup.  It applies to the
      # situation where a resource name ends in . and when it
      # ends in *.  In the second case you want to keep the *
      # in the widget name for pattern matching, but you want
      # to get rid of the . if it is the end of the name. 
      set backup -2  
      set line [string trim $line]
      if {[string index $line 0] == "#" || "$line" == ""} {
        # skip comments and empty lines
        continue
      }
      set list [split $line ":"]
      set resource [string trim [lindex $list 0]]
      set i [string last "." $resource]
      set j [string last "*" $resource]
      if {$j > $i} { 
        set i $j
        set backup -1
      }
      incr i
      set name [string range $resource $i end]
      incr i $backup
      set widname [string range $resource 0 $i]
      set value [string trim [lindex $list 1]]
      if {"$widname" != "" && "$widname" != "*"} {
        # insert the widget and resourcename to the application
        # defaults list.
        set xfAppDefaults($widname:[string tolower $name]) $value
      }
    }
  }
}

# application loading procedure
proc XFLocalLoadAppDefs {xfClasses {xfPriority "startupFile"} {xfAppDefFile ""}} {
  global env

  if {"$xfAppDefFile" == ""} {
    set xfFileList ""
    if {[info exists env(XUSERFILESEARCHPATH)]} {
      append xfFileList [split $env(XUSERFILESEARCHPATH) :]
    }
    if {[info exists env(XAPPLRESDIR)]} {
      append xfFileList [split $env(XAPPLRESDIR) :]
    }
    if {[info exists env(XFILESEARCHPATH)]} {
      append xfFileList [split $env(XFILESEARCHPATH) :]
    }
    append xfFileList " /usr/lib/X11/app-defaults"
    append xfFileList " /usr/X11/lib/X11/app-defaults"

    foreach xfCounter1 $xfClasses {
      foreach xfCounter2 $xfFileList {
        set xfPathName $xfCounter2
        if {[regsub -all "%N" "$xfPathName" "$xfCounter1" xfResult]} {
          set xfPathName $xfResult
        }
        if {[regsub -all "%T" "$xfPathName" "app-defaults" xfResult]} {
          set xfPathName $xfResult
        }
        if {[regsub -all "%S" "$xfPathName" "" xfResult]} {
          set xfPathName $xfResult
        }
        if {[regsub -all "%C" "$xfPathName" "" xfResult]} {
          set xfPathName $xfResult
        }
        if {[file exists $xfPathName] &&
            [file readable $xfPathName] &&
            ("[file type $xfPathName]" == "file" ||
             "[file type $xfPathName]" == "link")} {
          catch "option readfile $xfPathName $xfPriority"
          if {"[info commands XFParseAppDefs]" != ""} {
            XFParseAppDefs $xfPathName
          } {
            if {"[info commands XFLocalParseAppDefs]" != ""} {
              XFLocalParseAppDefs $xfPathName
            }
          }
        } {
          if {[file exists $xfCounter2/$xfCounter1] &&
              [file readable $xfCounter2/$xfCounter1] &&
              ("[file type $xfCounter2/$xfCounter1]" == "file" ||
               "[file type $xfCounter2/$xfCounter1]" == "link")} {
            catch "option readfile $xfCounter2/$xfCounter1 $xfPriority"
            if {"[info commands XFParseAppDefs]" != ""} {
              XFParseAppDefs $xfCounter2/$xfCounter1
            } {
              if {"[info commands XFLocalParseAppDefs]" != ""} {
                XFLocalParseAppDefs $xfCounter2/$xfCounter1
              }
            }
          }
        }
      }
    }
  } {
    # load a specific application defaults file
    if {[file exists $xfAppDefFile] &&
        [file readable $xfAppDefFile] &&
        ("[file type $xfAppDefFile]" == "file" ||
         "[file type $xfAppDefFile]" == "link")} {
      catch "option readfile $xfAppDefFile $xfPriority"
      if {"[info commands XFParseAppDefs]" != ""} {
        XFParseAppDefs $xfAppDefFile
      } {
        if {"[info commands XFLocalParseAppDefs]" != ""} {
          XFLocalParseAppDefs $xfAppDefFile
        }
      }
    }
  }
}

# application setting procedure
proc XFLocalSetAppDefs {{xfWidgetPath "."}} {
  global xfAppDefaults

  if {![info exists xfAppDefaults]} {
    return
  }
  foreach xfCounter [array names xfAppDefaults] {
    if {[string match "${xfWidgetPath}*" $xfCounter]} {
      set widname [string range $xfCounter 0 [expr [string first : $xfCounter]-1]]
      set name [string range $xfCounter [expr [string first : $xfCounter]+1] end]
      # Now lets see how many tcl commands match the name
      # pattern specified.
      set widlist [info command $widname]
      if {"$widlist" != ""} {
        foreach widget $widlist {
          # make sure this command is a widget.
          if {![catch "winfo id $widget"]} {
            catch "$widget configure -[string tolower $name] $xfAppDefaults($xfCounter)" 
          }
        }
      }
    }
  }
}



# end source
proc EndSrc {} {
  global argc
  global argv
  global appFileName
  global symbolicName

  set tmpArgv ""
  set appFileName "Xdefaults"
  for {set counter 0} {$counter < $argc} {incr counter 1} {
    case [string tolower [lindex $argv $counter]] in {
      {default} {
        set appFileName "[lindex $argv $counter]"
      }
    }
  }
  wm title . "xfappdef: $appFileName"

  # make internal listbox
  catch "destroy .templist"
  listbox .templist
  set {symbolicName(ResourceValueList)} .templist

  if {"[info procs XFShowHelp]" == ""} {
    if {"[info commands pinfo]" != ""} {
      if {[file exist File.xpm]} {
        .frame2.button configure -bitmap {@File.xpm}
        .frame2.button0 configure -bitmap {@Pixmap.xpm}
        .frame2.button1 configure -bitmap {@Cursor.xpm}
        .frame2.button13 configure -bitmap {@Color.xpm}
        .frame2.button16 configure -bitmap {@Font.xpm}
      }
    } {
      if {[file exist File.bm]} {
        .frame2.button configure -bitmap {@File.bm}
        .frame2.button0 configure -bitmap {@Pixmap.bm}
        .frame2.button1 configure -bitmap {@Cursor.bm}
        .frame2.button13 configure -bitmap {@Color.bm}
        .frame2.button16 configure -bitmap {@Font.bm}
      }
    }
  }

  ReadFirstFile
}

# initialize global variables
global {alertBox}
set {alertBox(activeBackground)} {}
set {alertBox(activeForeground)} {}
set {alertBox(after)} {0}
set {alertBox(anchor)} {nw}
set {alertBox(background)} {}
set {alertBox(button)} {0}
set {alertBox(font)} {}
set {alertBox(foreground)} {}
set {alertBox(justify)} {center}
set {alertBox(toplevelName)} {.alertBox}
global {appFileName}
set {appFileName} {Xdefaults}
global {colorBox}
set {colorBox(activeBackground)} {}
set {colorBox(activeForeground)} {}
set {colorBox(background)} {}
set {colorBox(colorName)} {}
set {colorBox(font)} {}
set {colorBox(foreground)} {}
set {colorBox(palette)} {white black gray50 blue red green yellow orange}
set {colorBox(paletteNr)} {0}
set {colorBox(scrollActiveForeground)} {}
set {colorBox(scrollBackground)} {}
set {colorBox(scrollForeground)} {}
set {colorBox(scrollSide)} {left}
set {colorBox(type)} {rgb}
global {cursorBox}
set {cursorBox(activeBackground)} {}
set {cursorBox(activeForeground)} {}
set {cursorBox(background)} {}
set {cursorBox(cursorName)} {}
set {cursorBox(font)} {}
set {cursorBox(foreground)} {}
set {cursorBox(scrollActiveForeground)} {}
set {cursorBox(scrollBackground)} {}
set {cursorBox(scrollForeground)} {}
set {cursorBox(scrollSide)} {left}
global {fontBox}
set {fontBox(activeBackground)} {}
set {fontBox(activeForeground)} {}
set {fontBox(background)} {}
set {fontBox(font)} {}
set {fontBox(fontFamily)} {*}
set {fontBox(fontName)} {}
set {fontBox(fontPixels)} {*}
set {fontBox(fontSWidth)} {*}
set {fontBox(fontSlant)} {*}
set {fontBox(fontWeight)} {*}
set {fontBox(foreground)} {}
set {fontBox(scrollActiveForeground)} {}
set {fontBox(scrollBackground)} {}
set {fontBox(scrollForeground)} {}
set {fontBox(scrollSide)} {left}
global {fsBox}
set {fsBox(activeBackground)} {}
set {fsBox(activeForeground)} {}
set {fsBox(all)} {0}
set {fsBox(background)} {}
set {fsBox(button)} {0}
set {fsBox(extensions)} {0}
set {fsBox(font)} {}
set {fsBox(foreground)} {}
set {fsBox(internalPath)} {/}
set {fsBox(name)} {}
set {fsBox(path)} {/}
set {fsBox(pattern)} {*}
set {fsBox(scrollActiveForeground)} {}
set {fsBox(scrollBackground)} {}
set {fsBox(scrollForeground)} {}
set {fsBox(scrollSide)} {left}
set {fsBox(showPixmap)} {0}

# please don't modify the following
# variables. They are needed by xf.
global {autoLoadList}
set {autoLoadList(fsBox.tcl)} {0}
set {autoLoadList(main.tcl)} {0}
global {internalAliasList}
set {internalAliasList} {}
global {moduleList}
set {moduleList(alertBox.tcl)} { AlertBox AlertBoxInternal}
set {moduleList(colorBox.tcl)} { ColorBox ColorBoxSelectColor ColorBoxSetColor ColorBoxSetHSVColor ColorBoxSetPalette ColorBoxSetPaletteList ColorBoxSetRGBColor ColorBoxShowSlides ColorBoxHSVToRGB ColorBoxRGBToHSV}
set {moduleList(cursorBox.tcl)} { CursorBox CursorBoxSelectCursor CursorBoxSetCursor}
set {moduleList(extrnl.tcl)} { Alias ClearList GetSelection NoFunction SN SymbolicName Unalias Ls}
set {moduleList(fnctns.tcl)} { DeleteResource InsertResource LoadFile MergeFile QuitProgram ReadFile ReadFirstFile SaveFile SaveFileAs SelectColor SelectCursor SelectFile SelectFont SelectPixmap SelectResource}
set {moduleList(fontBox.tcl)} { FontBox FontBoxComposeFont FontBoxSelectFont FontBoxSetFont}
set {moduleList(fsBox.tcl)} { FSBox FSBoxBindSelectOne FSBoxFSFileSelect FSBoxFSFileSelectDouble FSBoxFSInsertPath FSBoxFSNameComplete FSBoxFSShow IsADir IsAFile IsASymlink}
set {moduleList(interface.tcl)} { .}
set {moduleList(main.tcl)} {}
global {preloadList}
set {preloadList(xfInternal)} {}
global {symbolicName}
set {symbolicName(ResourceName)} {.frame1.frame20.frame.entry2}
set {symbolicName(ResourceNameList)} {.frame1.frame.listbox1}
set {symbolicName(ResourceValue)} {.frame1.frame7.text2}
set {symbolicName(ResourceValueList)} {.templist}
set {symbolicName(root)} {.}
global {xfWmSetPosition}
set {xfWmSetPosition} {}
global {xfWmSetSize}
set {xfWmSetSize} {.}
global {xfAppDefToplevels}
set {xfAppDefToplevels} {}

# display/remove toplevel windows.
ShowWindow.

# load default bindings.
if {[info exists env(XF_BIND_FILE)] &&
    "[info procs XFShowHelp]" == ""} {
  source $env(XF_BIND_FILE)
}

# parse and apply application defaults.
XFLocalLoadAppDefs Main
XFLocalSetAppDefs

# end source
EndSrc

# eof
#

