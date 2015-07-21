# Program: xf
# Description: the option dialog for general options
#
# $Header: xfoptInterp.tcl[2.3] Wed Mar 10 12:07:16 1993 garfield@garfield frozen $

##########
# Procedure: XFOptionsInterpreter
# Description: allow the editing of the interpreter options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsInterpreter {} {
  global tk_strictMotif
  global xfBind
  global xfConf
  global xfMisc

  set xfMisc(interpreterHasTkemacs) $xfConf(interpreterHasTkemacs)
  set xfMisc(strictMotif) $tk_strictMotif
  set xfMisc(kanji) $xfConf(kanji)

  # build widget structure
  XFTmpltToplevel .xfOptionsInterp 400x250 {XF interpreter options}

  XFTmpltFrame .xfOptionsInterp.frame1 0

  button .xfOptionsInterp.frame1.ok \
    -text {OK} \
    -command {
      XFOptionsInterpreterSet
      destroy .xfOptionsInterp}

  button .xfOptionsInterp.frame1.save \
    -text {Save + OK} \
    -command {
      XFOptionsInterpreterSet
      XFProcOptionsSaveOptions
      destroy .xfOptionsInterp}

  button .xfOptionsInterp.frame1.cancel \
    -text {Cancel} \
    -command {destroy .xfOptionsInterp}

  XFTmpltFrame .xfOptionsInterp.frame2

  checkbutton .xfOptionsInterp.frame2.tkemacs \
    -offvalue 0 \
    -onvalue 1 \
    -text {Interpreter has the tkEmacs widget} \
    -variable xfMisc(interpreterHasTkemacs)

  checkbutton .xfOptionsInterp.frame2.kanji \
    -offvalue 0 \
    -onvalue 1 \
    -text {Tk handles Kanji Fonts} \
    -variable xfMisc(kanji)

  checkbutton .xfOptionsInterp.frame2.strict \
    -offvalue 0 \
    -onvalue 1 \
    -text {Motif look & feel} \
    -variable xfMisc(strictMotif)

  XFTmpltFrame .xfOptionsInterp.frame3 0

  XFTmpltFrame .xfOptionsInterp.frame3.frame5

  label .xfOptionsInterp.frame3.frame5.message2 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Interpreter:}

  label .xfOptionsInterp.frame3.frame5.message3 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Interpreter (editor):}

  label .xfOptionsInterp.frame3.frame5.message4 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Interpreter (testing):}

  label .xfOptionsInterp.frame3.frame5.message5 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Interpreter (tutorial):}

  XFTmpltFrame .xfOptionsInterp.frame3.frame6

  entry .xfOptionsInterp.frame3.frame6.interpreter \
    -relief sunken
  .xfOptionsInterp.frame3.frame6.interpreter insert 0 $xfConf(interpreter)

  entry .xfOptionsInterp.frame3.frame6.interpreteredit \
    -relief sunken
  .xfOptionsInterp.frame3.frame6.interpreteredit insert 0 $xfConf(interpreterEdit)

  entry .xfOptionsInterp.frame3.frame6.interpretertest \
    -relief sunken
  .xfOptionsInterp.frame3.frame6.interpretertest insert 0 $xfConf(interpreterTest)

  entry .xfOptionsInterp.frame3.frame6.interpretertut \
    -relief sunken
  .xfOptionsInterp.frame3.frame6.interpretertut insert 0 $xfConf(interpreterTut)

  # bindings
  bind .xfOptionsInterp.frame3.frame6.interpreter $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsInterp.frame3.frame6.interpreteredit $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsInterp.frame3.frame6.interpretertest $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsInterp.frame3.frame6.interpretertut $xfBind(configure) {
    XFProcFSBoxFile %W}

  # packing
  pack append .xfOptionsInterp.frame1 \
              .xfOptionsInterp.frame1.ok {left fill expand} \
              .xfOptionsInterp.frame1.save {left fill expand} \
              .xfOptionsInterp.frame1.cancel {left fill expand}
  pack append .xfOptionsInterp.frame2 \
              .xfOptionsInterp.frame2.strict {top padx 10 pady 10} \
              .xfOptionsInterp.frame2.tkemacs {top padx 10 pady 10} \
              .xfOptionsInterp.frame2.kanji {top padx 10 pady 10}
  pack append .xfOptionsInterp.frame3.frame5 \
              .xfOptionsInterp.frame3.frame5.message2 {top fillx} \
              .xfOptionsInterp.frame3.frame5.message3 {top fillx} \
              .xfOptionsInterp.frame3.frame5.message4 {top fillx} \
              .xfOptionsInterp.frame3.frame5.message5 {top fillx}
  pack append .xfOptionsInterp.frame3.frame6 \
              .xfOptionsInterp.frame3.frame6.interpreter {top fillx} \
              .xfOptionsInterp.frame3.frame6.interpreteredit {top fillx} \
              .xfOptionsInterp.frame3.frame6.interpretertest {top fillx} \
              .xfOptionsInterp.frame3.frame6.interpretertut {top fillx}
  pack append .xfOptionsInterp.frame3 \
              .xfOptionsInterp.frame3.frame5 {left filly} \
              .xfOptionsInterp.frame3.frame6 {left fill expand}
  pack append .xfOptionsInterp \
              .xfOptionsInterp.frame1 {bottom fill} \
              .xfOptionsInterp.frame2 {top fill} \
              .xfOptionsInterp.frame3 {top fill expand}

  XFBindFormConnect .xfOptionsInterp.frame3.frame6
}

##########
# Procedure: XFOptionsInterpreterSet
# Description: set the new options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsInterpreterSet {} {
  global tk_strictMotif
  global xfConf
  global xfMisc

  set xfConf(interpreterHasTkemacs) $xfMisc(interpreterHasTkemacs)
  set xfConf(strictMotif) $xfMisc(strictMotif)
  set xfConf(kanji) $xfMisc(kanji)
  set tk_strictMotif $xfMisc(strictMotif)
  set xfConf(interpreter) [.xfOptionsInterp.frame3.frame6.interpreter get]
  set xfConf(interpreterTut) \
    [.xfOptionsInterp.frame3.frame6.interpretertut get]
  set xfConf(interpreterEdit) \
    [.xfOptionsInterp.frame3.frame6.interpreteredit get]
  set xfConf(interpreterTest) \
    [.xfOptionsInterp.frame3.frame6.interpretertest get]
}

# eof

