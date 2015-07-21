# Program: xf
# Description: the option dialog for general options
#
# $Header: xfoptSource.tcl[2.3] Wed Mar 10 12:07:22 1993 garfield@garfield frozen $

##########
# Procedure: XFOptionsSource
# Description: allow the editing of the source options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsSource {} {
  global xfBindSaveLevel
  global xfComment
  global xfConf
  global xfMisc
  global xfProcSaveLevel
  global xfStatus

  set xfMisc(encloseBinding) $xfConf(encloseBinding)
  set xfMisc(encloseConfigure) $xfConf(encloseConfigure)
  set xfMisc(createAppdefCode) $xfConf(createAppdefCode)
  set xfMisc(createClassBinding) $xfConf(createClassBinding)
  set xfMisc(createFormCode) $xfConf(createFormCode)
  set xfMisc(createParseCode) $xfConf(createParseCode)
  set xfMisc(createPixmapCode) $xfConf(createPixmapCode)
  set xfMisc(writeShellScript) $xfConf(writeShellScript)
  set xfMisc(writeTclIndex) $xfConf(writeTclIndex)
  set xfMisc(writeNewTclIndex) $xfConf(writeNewTclIndex)
  set xfMisc(commentfile) $xfComment(file)
  set xfMisc(commentmodule) $xfComment(module)
  set xfMisc(commentproc) $xfComment(proc)
  set xfMisc(commenttemplate) $xfComment(template)
  set xfMisc(bindSaveLevel1) $xfBindSaveLevel(1)
  set xfMisc(bindSaveLevel2) $xfBindSaveLevel(2)
  set xfMisc(bindSaveLevel3) $xfBindSaveLevel(3)
  set xfMisc(bindSaveLevel4) $xfBindSaveLevel(4)
  set xfMisc(bindSaveLevel5) $xfBindSaveLevel(5)
  set xfMisc(bindSaveLevel6) $xfBindSaveLevel(6)
  set xfMisc(bindSaveLevel7) $xfBindSaveLevel(7)
  set xfMisc(bindSaveLevel8) $xfBindSaveLevel(8)
  set xfMisc(procSaveLevel1) $xfProcSaveLevel(1)
  set xfMisc(procSaveLevel2) $xfProcSaveLevel(2)
  set xfMisc(procSaveLevel3) $xfProcSaveLevel(3)
  set xfMisc(procSaveLevel4) $xfProcSaveLevel(4)
  set xfMisc(procSaveLevel5) $xfProcSaveLevel(5)
  set xfMisc(procSaveLevel6) $xfProcSaveLevel(6)
  set xfMisc(procSaveLevel7) $xfProcSaveLevel(7)
  set xfMisc(procSaveLevel8) $xfProcSaveLevel(8)

  # build widget structure
  XFTmpltToplevel .xfOptionsSource 425x560 {XF source options}

  XFTmpltFrame .xfOptionsSource.frame1 0

  button .xfOptionsSource.frame1.ok \
    -text {OK} \
    -command {
      XFOptionsSourceSet
      destroy .xfOptionsSource}

  button .xfOptionsSource.frame1.save \
    -text {Save + OK} \
    -command {
      XFOptionsSourceSet
      XFProcOptionsSaveOptions
      destroy .xfOptionsSource}

  button .xfOptionsSource.frame1.cancel \
    -text {Cancel} \
    -command {destroy .xfOptionsSource}

  XFTmpltFrame .xfOptionsSource.frame2

  checkbutton .xfOptionsSource.frame2.appdef \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create application default code} \
    -variable xfMisc(createAppdefCode)

  checkbutton .xfOptionsSource.frame2.form \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create form support code} \
    -variable xfMisc(createFormCode)

  checkbutton .xfOptionsSource.frame2.parse \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create commandline parsing code} \
    -variable xfMisc(createParseCode)

  checkbutton .xfOptionsSource.frame2.preload \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create pixmap preloading code} \
    -variable xfMisc(createPixmapCode)

  checkbutton .xfOptionsSource.frame2.classbind \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create class bindings} \
    -variable xfMisc(createClassBinding)

  checkbutton .xfOptionsSource.frame2.tclindex \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create tclIndex file} \
    -variable xfMisc(writeTclIndex)

  checkbutton .xfOptionsSource.frame2.newtclindex \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create new style tclIndex file} \
    -variable xfMisc(writeNewTclIndex)

  checkbutton .xfOptionsSource.frame2.script \
    -offvalue 0 \
    -onvalue 1 \
    -text {Create shell script} \
    -variable xfMisc(writeShellScript)

  XFTmpltFrame .xfOptionsSource.frame3

  XFTmpltFrame .xfOptionsSource.frame3.frame8 0

  label .xfOptionsSource.frame3.frame8.message2 \
    -borderwidth 0 \
    -text {Bindings are surrounded by:}

  radiobutton .xfOptionsSource.frame3.frame8.binding1 \
    -text {"..."} \
    -value 0 \
    -variable xfMisc(encloseBinding)

  radiobutton .xfOptionsSource.frame3.frame8.binding2 \
    -text {{...}} \
    -value 1 \
    -variable xfMisc(encloseBinding)

  XFTmpltFrame .xfOptionsSource.frame3.frame9 0

  label .xfOptionsSource.frame3.frame9.message2 \
    -borderwidth 0 \
    -text {Widget parameters are surrounded by:}

  radiobutton .xfOptionsSource.frame3.frame9.config1 \
    -text {"..."} \
    -value 0 \
    -variable xfMisc(encloseConfigure)

  radiobutton .xfOptionsSource.frame3.frame9.config2 \
    -text {{...}} \
    -value 1 \
    -variable xfMisc(encloseConfigure)

  XFTmpltFrame .xfOptionsSource.frame4 0

  XFTmpltFrame .xfOptionsSource.frame4.frame5 0

  label .xfOptionsSource.frame4.message1 \
    -relief raised \
    -text {Comment layout:}

  XFTmpltText .xfOptionsSource.frame4 text 0 \
    [set xfMisc(comment$xfStatus(comment))]

  radiobutton .xfOptionsSource.frame4.frame5.file \
    -command {XFOptionsSourceComment} \
    -text {File} \
    -value file \
    -variable xfStatus(comment)

  radiobutton .xfOptionsSource.frame4.frame5.module \
    -command {XFOptionsSourceComment} \
    -text {Module} \
    -value module \
    -variable xfStatus(comment)

  radiobutton .xfOptionsSource.frame4.frame5.template \
    -command {XFOptionsSourceComment} \
    -text {Template} \
    -value template \
    -variable xfStatus(comment)

  radiobutton .xfOptionsSource.frame4.frame5.proc \
    -command {XFOptionsSourceComment} \
    -text {Procedure} \
    -value proc \
    -variable xfStatus(comment)

  set xfStatus(currentComment) $xfStatus(comment)

  XFTmpltFrame .xfOptionsSource.frame7

  label .xfOptionsSource.frame7.mess \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Binding save levels:}

  checkbutton .xfOptionsSource.frame7.level1 \
    -offvalue 0 \
    -onvalue 1 \
    -text {1} \
    -variable xfMisc(bindSaveLevel1)

  checkbutton .xfOptionsSource.frame7.level2 \
    -offvalue 0 \
    -onvalue 1 \
    -text {2} \
    -variable xfMisc(bindSaveLevel2)

  checkbutton .xfOptionsSource.frame7.level3 \
    -offvalue 0 \
    -onvalue 1 \
    -text {3} \
    -variable xfMisc(bindSaveLevel3)

  checkbutton .xfOptionsSource.frame7.level4 \
    -offvalue 0 \
    -onvalue 1 \
    -text {4} \
    -variable xfMisc(bindSaveLevel4)

  checkbutton .xfOptionsSource.frame7.level5 \
    -offvalue 0 \
    -onvalue 1 \
    -text {5} \
    -variable xfMisc(bindSaveLevel5)

  checkbutton .xfOptionsSource.frame7.level6 \
    -offvalue 0 \
    -onvalue 1 \
    -text {6} \
    -variable xfMisc(bindSaveLevel6)

  checkbutton .xfOptionsSource.frame7.level7 \
    -offvalue 0 \
    -onvalue 1 \
    -text {7} \
    -variable xfMisc(bindSaveLevel7)

  checkbutton .xfOptionsSource.frame7.level8 \
    -offvalue 0 \
    -onvalue 1 \
    -text {8} \
    -variable xfMisc(bindSaveLevel8)

  XFTmpltFrame .xfOptionsSource.frame8

  label .xfOptionsSource.frame8.mess \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -text {Procedure save levels:}

  checkbutton .xfOptionsSource.frame8.level1 \
    -offvalue 0 \
    -onvalue 1 \
    -text {1} \
    -variable xfMisc(procSaveLevel1)

  checkbutton .xfOptionsSource.frame8.level2 \
    -offvalue 0 \
    -onvalue 1 \
    -text {2} \
    -variable xfMisc(procSaveLevel2)

  checkbutton .xfOptionsSource.frame8.level3 \
    -offvalue 0 \
    -onvalue 1 \
    -text {3} \
    -variable xfMisc(procSaveLevel3)

  checkbutton .xfOptionsSource.frame8.level4 \
    -offvalue 0 \
    -onvalue 1 \
    -text {4} \
    -variable xfMisc(procSaveLevel4)

  checkbutton .xfOptionsSource.frame8.level5 \
    -offvalue 0 \
    -onvalue 1 \
    -text {5} \
    -variable xfMisc(procSaveLevel5)

  checkbutton .xfOptionsSource.frame8.level6 \
    -offvalue 0 \
    -onvalue 1 \
    -text {6} \
    -variable xfMisc(procSaveLevel6)

  checkbutton .xfOptionsSource.frame8.level7 \
    -offvalue 0 \
    -onvalue 1 \
    -text {7} \
    -variable xfMisc(procSaveLevel7)

  checkbutton .xfOptionsSource.frame8.level8 \
    -offvalue 0 \
    -onvalue 1 \
    -text {8} \
    -variable xfMisc(procSaveLevel8)

  # packing
  pack append .xfOptionsSource.frame1 \
              .xfOptionsSource.frame1.ok {left fill expand} \
              .xfOptionsSource.frame1.save {left fill expand} \
              .xfOptionsSource.frame1.cancel {left fill expand}
  pack append .xfOptionsSource.frame2 \
              .xfOptionsSource.frame2.appdef {top padx 10 pady 10} \
              .xfOptionsSource.frame2.form {top padx 10 pady 10} \
              .xfOptionsSource.frame2.parse {top padx 10 pady 10} \
              .xfOptionsSource.frame2.preload {top padx 10 pady 10} \
              .xfOptionsSource.frame2.classbind {top padx 10 pady 10} \
              .xfOptionsSource.frame2.tclindex {top padx 10 pady 10} \
              .xfOptionsSource.frame2.newtclindex {top padx 10 pady 10} \
              .xfOptionsSource.frame2.script {top padx 10 pady 10}
  pack append .xfOptionsSource.frame3.frame8 \
              .xfOptionsSource.frame3.frame8.message2 {left fill expand} \
              .xfOptionsSource.frame3.frame8.binding1 {left fill expand} \
              .xfOptionsSource.frame3.frame8.binding2 {left fill expand}
  pack append .xfOptionsSource.frame3.frame9 \
              .xfOptionsSource.frame3.frame9.message2 {left fill expand} \
              .xfOptionsSource.frame3.frame9.config1 {left fill expand} \
              .xfOptionsSource.frame3.frame9.config2 {left fill expand}
  pack append .xfOptionsSource.frame3 \
              .xfOptionsSource.frame3.frame8 {top padx 10 pady 10} \
              .xfOptionsSource.frame3.frame9 {top padx 10 pady 10}
  pack append .xfOptionsSource.frame4.frame5 \
              .xfOptionsSource.frame4.frame5.file {left fill expand} \
              .xfOptionsSource.frame4.frame5.module {left fill expand} \
              .xfOptionsSource.frame4.frame5.template {left fill expand} \
              .xfOptionsSource.frame4.frame5.proc {left fill expand}
  pack append .xfOptionsSource.frame4 \
              .xfOptionsSource.frame4.message1 {top fill} \
              .xfOptionsSource.frame4.frame5 {bottom fill} \
              .xfOptionsSource.frame4.text {top fill expand}
  pack append .xfOptionsSource.frame7 \
              .xfOptionsSource.frame7.mess {left fill padx 10 pady 10} \
              .xfOptionsSource.frame7.level1 {left fill expand} \
              .xfOptionsSource.frame7.level2 {left fill expand} \
              .xfOptionsSource.frame7.level3 {left fill expand} \
              .xfOptionsSource.frame7.level4 {left fill expand} \
              .xfOptionsSource.frame7.level5 {left fill expand} \
              .xfOptionsSource.frame7.level6 {left fill expand} \
              .xfOptionsSource.frame7.level7 {left fill expand} \
              .xfOptionsSource.frame7.level8 {left fill expand}
  pack append .xfOptionsSource.frame8 \
              .xfOptionsSource.frame8.mess {left padx 10 pady 10} \
              .xfOptionsSource.frame8.level1 {left fill expand} \
              .xfOptionsSource.frame8.level2 {left fill expand} \
              .xfOptionsSource.frame8.level3 {left fill expand} \
              .xfOptionsSource.frame8.level4 {left fill expand} \
              .xfOptionsSource.frame8.level5 {left fill expand} \
              .xfOptionsSource.frame8.level6 {left fill expand} \
              .xfOptionsSource.frame8.level7 {left fill expand} \
              .xfOptionsSource.frame8.level8 {left fill expand}
  pack append .xfOptionsSource \
              .xfOptionsSource.frame1 {bottom fill} \
              .xfOptionsSource.frame2 {top fill} \
              .xfOptionsSource.frame3 {top fill} \
              .xfOptionsSource.frame7 {top fill} \
              .xfOptionsSource.frame8 {top fill} \
              .xfOptionsSource.frame4 {top fill expand}
}

##########
# Procedure: XFOptionsSourceComment
# Description: set the comment value
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsSourceComment {} {
  global xfConf
  global xfMisc
  global xfStatus

  set xfMisc(comment$xfStatus(currentComment)) \
    [XFMiscGetText .xfOptionsSource.frame4.text.text]
  XFMiscSetText .xfOptionsSource.frame4.text.text \
    [set xfMisc(comment$xfStatus(comment))]
  set xfStatus(currentComment) $xfStatus(comment)
}

##########
# Procedure: XFOptionsSourceSet
# Description: set the new options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsSourceSet {} {
  global xfBindSaveLevel
  global xfComment
  global xfConf
  global xfMisc
  global xfPath
  global xfProcSaveLevel
  global xfStatus

  set xfConf(encloseBinding) $xfMisc(encloseBinding)
  set xfConf(encloseConfigure) $xfMisc(encloseConfigure)
  set xfConf(createAppdefCode) $xfMisc(createAppdefCode)
  set xfConf(createClassBinding) $xfMisc(createClassBinding)
  set xfConf(createFormCode) $xfMisc(createFormCode)
  set xfConf(createParseCode) $xfMisc(createParseCode)
  set xfConf(createPixmapCode) $xfMisc(createPixmapCode)
  set xfConf(writeShellScript) $xfMisc(writeShellScript)
  set xfConf(writeTclIndex) $xfMisc(writeTclIndex)
  set xfConf(writeNewTclIndex) $xfMisc(writeNewTclIndex)
  if {$xfConf(writeNewTclIndex)} {
    set xfConf(writeTclIndex) 1
  }
  set xfMisc(comment$xfStatus(currentComment)) \
    [XFMiscGetText .xfOptionsSource.frame4.text.text]
  set xfComment(file) $xfMisc(commentfile)
  set xfComment(module) $xfMisc(commentmodule)
  set xfComment(proc) $xfMisc(commentproc)
  set xfComment(template) $xfMisc(commenttemplate)
  set xfBindSaveLevel(1) $xfMisc(bindSaveLevel1)
  set xfBindSaveLevel(2) $xfMisc(bindSaveLevel2)
  set xfBindSaveLevel(3) $xfMisc(bindSaveLevel3)
  set xfBindSaveLevel(4) $xfMisc(bindSaveLevel4)
  set xfBindSaveLevel(5) $xfMisc(bindSaveLevel5)
  set xfBindSaveLevel(6) $xfMisc(bindSaveLevel6)
  set xfBindSaveLevel(7) $xfMisc(bindSaveLevel7)
  set xfBindSaveLevel(8) $xfMisc(bindSaveLevel8)
  set xfProcSaveLevel(1) $xfMisc(procSaveLevel1)
  set xfProcSaveLevel(2) $xfMisc(procSaveLevel2)
  set xfProcSaveLevel(3) $xfMisc(procSaveLevel3)
  set xfProcSaveLevel(4) $xfMisc(procSaveLevel4)
  set xfProcSaveLevel(5) $xfMisc(procSaveLevel5)
  set xfProcSaveLevel(6) $xfMisc(procSaveLevel6)
  set xfProcSaveLevel(7) $xfMisc(procSaveLevel7)
  set xfProcSaveLevel(8) $xfMisc(procSaveLevel8)
}

# eof

