# Program: xf
# Description: the option dialog for general options
#
# $Header: xfoptGnrl.tcl[2.4] Wed Mar 10 12:07:13 1993 garfield@garfield frozen $

##########
# Procedure: XFOptionsGeneral
# Description: allow the editing of the general options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsGeneral {} {
  global xfBind
  global xfBindShowLevel
  global xfConf
  global xfMisc
  global xfProcShowLevel

  set xfMisc(saveOptions) $xfConf(saveOptions)
  set xfMisc(savePositions) $xfConf(savePositions)
  set xfMisc(layoutAlways) $xfConf(layoutAlways)
  set xfMisc(geometry) $xfConf(geometry)
  set xfMisc(getWidgetName) $xfConf(getWidgetName)
  set xfMisc(scrollSide) $xfConf(scrollSide)
  set xfMisc(bindShowLevel1) $xfBindShowLevel(1)
  set xfMisc(bindShowLevel2) $xfBindShowLevel(2)
  set xfMisc(bindShowLevel3) $xfBindShowLevel(3)
  set xfMisc(bindShowLevel4) $xfBindShowLevel(4)
  set xfMisc(bindShowLevel5) $xfBindShowLevel(5)
  set xfMisc(bindShowLevel6) $xfBindShowLevel(6)
  set xfMisc(bindShowLevel7) $xfBindShowLevel(7)
  set xfMisc(bindShowLevel8) $xfBindShowLevel(8)
  set xfMisc(procShowLevel1) $xfProcShowLevel(1)
  set xfMisc(procShowLevel2) $xfProcShowLevel(2)
  set xfMisc(procShowLevel3) $xfProcShowLevel(3)
  set xfMisc(procShowLevel4) $xfProcShowLevel(4)
  set xfMisc(procShowLevel5) $xfProcShowLevel(5)
  set xfMisc(procShowLevel6) $xfProcShowLevel(6)
  set xfMisc(procShowLevel7) $xfProcShowLevel(7)
  set xfMisc(procShowLevel8) $xfProcShowLevel(8)

  # build widget structure
  XFTmpltToplevel .xfOptionsGeneral 400x600 {XF general options}

  XFTmpltFrame .xfOptionsGeneral.frame1 0

  button .xfOptionsGeneral.frame1.ok \
    -text {OK} \
    -command {
      XFOptionsGeneralSet
      destroy .xfOptionsGeneral}

  button .xfOptionsGeneral.frame1.save \
    -text {Save + OK} \
    -command {
      XFOptionsGeneralSet
      XFProcOptionsSaveOptions
      destroy .xfOptionsGeneral}

  button .xfOptionsGeneral.frame1.cancel \
    -text {Cancel} \
    -command {destroy .xfOptionsGeneral}

  XFTmpltFrame .xfOptionsGeneral.frame2

  XFTmpltScaleDouble .xfOptionsGeneral.frame2 autosave "Auto save:" \
    "Interval(min)" "File number" 120 100 
  .xfOptionsGeneral.frame2.autosave.autosave1.autosave1 set $xfConf(saveInterval)
  .xfOptionsGeneral.frame2.autosave.autosave2.autosave2 set $xfConf(maxSaveId)
  XFMiscSetResource .xfOptionsGeneral.frame2.autosave.label1 \
    relief flat

  XFTmpltFrame .xfOptionsGeneral.frame3

  checkbutton .xfOptionsGeneral.frame3.getname \
    -offvalue 0 \
    -onvalue 1 \
    -text {Ask for widget name on insertion} \
    -variable xfMisc(getWidgetName)

  XFTmpltFrame .xfOptionsGeneral.frame4

  label .xfOptionsGeneral.frame4.message1 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Default geometry manager:}

  radiobutton .xfOptionsGeneral.frame4.packer \
    -text {packer} \
    -value {packer} \
    -variable xfMisc(geometry)

  radiobutton .xfOptionsGeneral.frame4.placer \
    -text {placer} \
    -value {placer} \
    -variable xfMisc(geometry)

  XFTmpltFrame .xfOptionsGeneral.frame9

  checkbutton .xfOptionsGeneral.frame9.layout \
    -offvalue 0 \
    -onvalue 1 \
    -text {Allow layouting without layout window} \
    -variable xfMisc(layoutAlways)

  XFTmpltFrame .xfOptionsGeneral.frame9.frame2 0

  label .xfOptionsGeneral.frame9.frame2.message1 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Layout border width:}

  scale .xfOptionsGeneral.frame9.frame2.border \
    -from 0 \
    -label "pixel" \
    -orient horizontal \
    -sliderlength 15 \
    -to 100 \
    -width 8
  .xfOptionsGeneral.frame9.frame2.border set $xfConf(layoutBorder)

  XFTmpltFrame .xfOptionsGeneral.frame9.frame1 0

  scale .xfOptionsGeneral.frame9.frame1.gridx \
    -from 0 \
    -label "Grid X" \
    -orient horizontal \
    -sliderlength 15 \
    -to 100 \
    -width 8
  .xfOptionsGeneral.frame9.frame1.gridx set $xfConf(gridX)

  scale .xfOptionsGeneral.frame9.frame1.gridy \
    -from 0 \
    -label "Grid Y" \
    -orient horizontal \
    -sliderlength 15 \
    -to 100 \
    -width 8
  .xfOptionsGeneral.frame9.frame1.gridy set $xfConf(gridY)

  XFTmpltFrame .xfOptionsGeneral.frame6

  checkbutton .xfOptionsGeneral.frame6.saveOpt \
    -offvalue 0 \
    -onvalue 1 \
    -text {Save options on exit} \
    -variable xfMisc(saveOptions)

  checkbutton .xfOptionsGeneral.frame6.savePos \
    -offvalue 0 \
    -onvalue 1 \
    -text {Save positions on exit} \
    -variable xfMisc(savePositions)

  XFTmpltFrame .xfOptionsGeneral.frame10

  label .xfOptionsGeneral.frame10.message1 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Scrollbar side:}

  radiobutton .xfOptionsGeneral.frame10.left \
    -text {left} \
    -value {left} \
    -variable xfMisc(scrollSide)

  radiobutton .xfOptionsGeneral.frame10.right \
    -text {right} \
    -value {right} \
    -variable xfMisc(scrollSide)

  XFTmpltFrame .xfOptionsGeneral.frame5 0

  XFTmpltFrame .xfOptionsGeneral.frame5.frame5

  label .xfOptionsGeneral.frame5.frame5.message2 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Editor:}

  label .xfOptionsGeneral.frame5.frame5.message3 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Message font:}

  label .xfOptionsGeneral.frame5.frame5.message4 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Flash color:}

  XFTmpltFrame .xfOptionsGeneral.frame7

  label .xfOptionsGeneral.frame7.mess \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Binding show levels:}

  checkbutton .xfOptionsGeneral.frame7.level1 \
    -offvalue 0 \
    -onvalue 1 \
    -text {1} \
    -variable xfMisc(bindShowLevel1)

  checkbutton .xfOptionsGeneral.frame7.level2 \
    -offvalue 0 \
    -onvalue 1 \
    -text {2} \
    -variable xfMisc(bindShowLevel2)

  checkbutton .xfOptionsGeneral.frame7.level3 \
    -offvalue 0 \
    -onvalue 1 \
    -text {3} \
    -variable xfMisc(bindShowLevel3)

  checkbutton .xfOptionsGeneral.frame7.level4 \
    -offvalue 0 \
    -onvalue 1 \
    -text {4} \
    -variable xfMisc(bindShowLevel4)

  checkbutton .xfOptionsGeneral.frame7.level5 \
    -offvalue 0 \
    -onvalue 1 \
    -text {5} \
    -variable xfMisc(bindShowLevel5)

  checkbutton .xfOptionsGeneral.frame7.level6 \
    -offvalue 0 \
    -onvalue 1 \
    -text {6} \
    -variable xfMisc(bindShowLevel6)

  checkbutton .xfOptionsGeneral.frame7.level7 \
    -offvalue 0 \
    -onvalue 1 \
    -text {7} \
    -variable xfMisc(bindShowLevel7)

  checkbutton .xfOptionsGeneral.frame7.level8 \
    -offvalue 0 \
    -onvalue 1 \
    -text {8} \
    -variable xfMisc(bindShowLevel8)

  XFTmpltFrame .xfOptionsGeneral.frame8

  label .xfOptionsGeneral.frame8.mess \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Procedure show levels:}

  checkbutton .xfOptionsGeneral.frame8.level1 \
    -offvalue 0 \
    -onvalue 1 \
    -text {1} \
    -variable xfMisc(procShowLevel1)

  checkbutton .xfOptionsGeneral.frame8.level2 \
    -offvalue 0 \
    -onvalue 1 \
    -text {2} \
    -variable xfMisc(procShowLevel2)

  checkbutton .xfOptionsGeneral.frame8.level3 \
    -offvalue 0 \
    -onvalue 1 \
    -text {3} \
    -variable xfMisc(procShowLevel3)

  checkbutton .xfOptionsGeneral.frame8.level4 \
    -offvalue 0 \
    -onvalue 1 \
    -text {4} \
    -variable xfMisc(procShowLevel4)

  checkbutton .xfOptionsGeneral.frame8.level5 \
    -offvalue 0 \
    -onvalue 1 \
    -text {5} \
    -variable xfMisc(procShowLevel5)

  checkbutton .xfOptionsGeneral.frame8.level6 \
    -offvalue 0 \
    -onvalue 1 \
    -text {6} \
    -variable xfMisc(procShowLevel6)

  checkbutton .xfOptionsGeneral.frame8.level7 \
    -offvalue 0 \
    -onvalue 1 \
    -text {7} \
    -variable xfMisc(procShowLevel7)

  checkbutton .xfOptionsGeneral.frame8.level8 \
    -offvalue 0 \
    -onvalue 1 \
    -text {8} \
    -variable xfMisc(procShowLevel8)

  XFTmpltFrame .xfOptionsGeneral.frame5.frame6

  entry .xfOptionsGeneral.frame5.frame6.editor \
    -relief sunken
  .xfOptionsGeneral.frame5.frame6.editor insert 0 $xfConf(externalEditor)

  entry .xfOptionsGeneral.frame5.frame6.messg \
    -relief sunken
  .xfOptionsGeneral.frame5.frame6.messg insert 0 $xfConf(fontMessage)

  entry .xfOptionsGeneral.frame5.frame6.flash \
    -relief sunken
  .xfOptionsGeneral.frame5.frame6.flash insert 0 $xfConf(flash)

  # bindings
  bind .xfOptionsGeneral.frame5.frame6.editor $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsGeneral.frame5.frame6.messg $xfBind(configure) {
    XFProcFontBox font %W}
  bind .xfOptionsGeneral.frame5.frame6.flash $xfBind(configure) {
    XFProcColorBox background %W}

  # packing
  pack append .xfOptionsGeneral.frame1 \
              .xfOptionsGeneral.frame1.ok {left fill expand} \
              .xfOptionsGeneral.frame1.save {left fill expand} \
              .xfOptionsGeneral.frame1.cancel {left fill expand}
  pack append .xfOptionsGeneral.frame2 \
              .xfOptionsGeneral.frame2.autosave {top padx 10 pady 10}
  pack append .xfOptionsGeneral.frame3 \
              .xfOptionsGeneral.frame3.getname {top padx 10 pady 10}
  pack append .xfOptionsGeneral.frame4 \
              .xfOptionsGeneral.frame4.message1 {left padx 10 pady 10} \
              .xfOptionsGeneral.frame4.packer {left padx 10 pady 10} \
              .xfOptionsGeneral.frame4.placer {left padx 10 pady 10}
  pack append .xfOptionsGeneral.frame9.frame2 \
              .xfOptionsGeneral.frame9.frame2.message1 {left padx 10 pady 10} \
              .xfOptionsGeneral.frame9.frame2.border {left fill expand}
  pack append .xfOptionsGeneral.frame9.frame1 \
              .xfOptionsGeneral.frame9.frame1.gridx {left fill expand} \
              .xfOptionsGeneral.frame9.frame1.gridy {left fill expand}
  pack append .xfOptionsGeneral.frame9 \
              .xfOptionsGeneral.frame9.layout {top padx 10 pady 10} \
              .xfOptionsGeneral.frame9.frame2 {top fill expand padx 10 pady 10} \
              .xfOptionsGeneral.frame9.frame1 {top fill expand padx 10 pady 10}
  pack append .xfOptionsGeneral.frame10 \
              .xfOptionsGeneral.frame10.message1 {left fill padx 10 pady 10} \
              .xfOptionsGeneral.frame10.left {left padx 10 pady 10} \
              .xfOptionsGeneral.frame10.right {left padx 10 pady 10}
  pack append .xfOptionsGeneral.frame5.frame5 \
              .xfOptionsGeneral.frame5.frame5.message2 {top fillx} \
              .xfOptionsGeneral.frame5.frame5.message3 {top fillx} \
              .xfOptionsGeneral.frame5.frame5.message4 {top fillx}
  pack append .xfOptionsGeneral.frame5.frame6 \
              .xfOptionsGeneral.frame5.frame6.editor {top fillx} \
              .xfOptionsGeneral.frame5.frame6.messg {top fillx} \
              .xfOptionsGeneral.frame5.frame6.flash {top fillx}
  pack append .xfOptionsGeneral.frame5 \
              .xfOptionsGeneral.frame5.frame5 {left filly} \
              .xfOptionsGeneral.frame5.frame6 {left fill expand}
  pack append .xfOptionsGeneral.frame6 \
              .xfOptionsGeneral.frame6.saveOpt {top padx 10 pady 10} \
              .xfOptionsGeneral.frame6.savePos {top padx 10 pady 10}
  pack append .xfOptionsGeneral.frame7 \
              .xfOptionsGeneral.frame7.mess {left fill padx 10 pady 10} \
              .xfOptionsGeneral.frame7.level1 {left fill expand} \
              .xfOptionsGeneral.frame7.level2 {left fill expand} \
              .xfOptionsGeneral.frame7.level3 {left fill expand} \
              .xfOptionsGeneral.frame7.level4 {left fill expand} \
              .xfOptionsGeneral.frame7.level5 {left fill expand} \
              .xfOptionsGeneral.frame7.level6 {left fill expand} \
              .xfOptionsGeneral.frame7.level7 {left fill expand} \
              .xfOptionsGeneral.frame7.level8 {left fill expand}
  pack append .xfOptionsGeneral.frame8 \
              .xfOptionsGeneral.frame8.mess {left padx 10 pady 10} \
              .xfOptionsGeneral.frame8.level1 {left fill expand} \
              .xfOptionsGeneral.frame8.level2 {left fill expand} \
              .xfOptionsGeneral.frame8.level3 {left fill expand} \
              .xfOptionsGeneral.frame8.level4 {left fill expand} \
              .xfOptionsGeneral.frame8.level5 {left fill expand} \
              .xfOptionsGeneral.frame8.level6 {left fill expand} \
              .xfOptionsGeneral.frame8.level7 {left fill expand} \
              .xfOptionsGeneral.frame8.level8 {left fill expand}
  pack append .xfOptionsGeneral \
              .xfOptionsGeneral.frame1 {bottom fill} \
              .xfOptionsGeneral.frame2 {top fill} \
              .xfOptionsGeneral.frame3 {top fill} \
              .xfOptionsGeneral.frame4 {top fill} \
              .xfOptionsGeneral.frame9 {top fill} \
              .xfOptionsGeneral.frame10 {top fill} \
              .xfOptionsGeneral.frame6 {top fill} \
              .xfOptionsGeneral.frame7 {top fill} \
              .xfOptionsGeneral.frame8 {top fill} \
              .xfOptionsGeneral.frame5 {top fill expand}

  XFBindFormConnect .xfOptionsGeneral.frame5.frame6
}

##########
# Procedure: XFOptionsGeneralSet
# Description: set the new options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsGeneralSet {} {
  global xfBindShowLevel
  global xfConf
  global xfMisc
  global xfProcShowLevel
  global xfColorBox
  global xfCursorBox
  global xfFSBox
  global xfFontBox
  global xfIconBarBox
  global xfInputBox
  global xfKeysymBox
  global xfMenuBar
  global xfReadBox
  global xfTextBox

  set xfConf(getWidgetName) $xfMisc(getWidgetName)
  set xfConf(layoutAlways) $xfMisc(layoutAlways)
  set xfConf(layoutBorder) [.xfOptionsGeneral.frame9.frame2.border get]
  set xfConf(geometry) $xfMisc(geometry)
  set xfConf(externalEditor) [.xfOptionsGeneral.frame5.frame6.editor get]
  set xfConf(flash) [.xfOptionsGeneral.frame5.frame6.flash get]
  set xfConf(fontMessage) [.xfOptionsGeneral.frame5.frame6.messg get]
  set xfConf(saveInterval) \
    [.xfOptionsGeneral.frame2.autosave.autosave1.autosave1 get]
  set xfConf(saveOptions) $xfMisc(saveOptions)
  set xfConf(savePositions) $xfMisc(savePositions)
  set xfConf(scrollSide) $xfMisc(scrollSide)
  set xfConf(maxSaveId) \
    [.xfOptionsGeneral.frame2.autosave.autosave2.autosave2 get]
  set xfConf(saveInterval) \
    [.xfOptionsGeneral.frame2.autosave.autosave1.autosave1 get]
  set xfConf(gridX) [.xfOptionsGeneral.frame9.frame1.gridx get]
  set xfConf(gridY) [.xfOptionsGeneral.frame9.frame1.gridy get]
  set xfBindShowLevel(1) $xfMisc(bindShowLevel1)
  set xfBindShowLevel(2) $xfMisc(bindShowLevel2)
  set xfBindShowLevel(3) $xfMisc(bindShowLevel3)
  set xfBindShowLevel(4) $xfMisc(bindShowLevel4)
  set xfBindShowLevel(5) $xfMisc(bindShowLevel5)
  set xfBindShowLevel(6) $xfMisc(bindShowLevel6)
  set xfBindShowLevel(7) $xfMisc(bindShowLevel7)
  set xfBindShowLevel(8) $xfMisc(bindShowLevel8)
  set xfProcShowLevel(1) $xfMisc(procShowLevel1)
  set xfProcShowLevel(2) $xfMisc(procShowLevel2)
  set xfProcShowLevel(3) $xfMisc(procShowLevel3)
  set xfProcShowLevel(4) $xfMisc(procShowLevel4)
  set xfProcShowLevel(5) $xfMisc(procShowLevel5)
  set xfProcShowLevel(6) $xfMisc(procShowLevel6)
  set xfProcShowLevel(7) $xfMisc(procShowLevel7)
  set xfProcShowLevel(8) $xfMisc(procShowLevel8)

  set xfColorBox(scrollSide) $xfConf(scrollSide)
  set xfCursorBox(scrollSide) $xfConf(scrollSide)
  set xfFSBox(scrollSide) $xfConf(scrollSide)
  set xfFontBox(scrollSide) $xfConf(scrollSide)
  set xfIconBarBox(scrollSide) $xfConf(scrollSide)
  set xfInputBox(scrollSide) $xfConf(scrollSide)
  set xfKeysymBox(scrollSide) $xfConf(scrollSide)
  set xfMenuBar(scrollSide) $xfConf(scrollSide)
  set xfReadBox(scrollSide) $xfConf(scrollSide)
  set xfTextBox(scrollSide) $xfConf(scrollSide)

  # start auto saving
  if {$xfConf(saveInterval) > 0} {
    after [expr $xfConf(saveInterval)*60000] XFMiscAutoSave $xfConf(saveInterval)
  }
}

# eof

