# Program: xf
# Description: the option dialog for general options
#
# $Header: xfoptWindow.tcl[2.4] Wed Mar 10 12:07:27 1993 garfield@garfield frozen $

##########
# Procedure: XFOptionsWindow
# Description: allow the editing of the window options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsWindow {} {
  global xfBind
  global xfConf
  global xfMisc

  set xfMisc(autoPos) $xfConf(autoPos)
  set xfMisc(autoRootPos) $xfConf(autoRootPos)
  set xfMisc(autoSize) $xfConf(autoSize)
  set xfMisc(autoStack) $xfConf(autoStack)
  set xfMisc(editListsHidden) $xfConf(editListsHidden)
  set xfMisc(menuBarHidden) $xfConf(menuBarHidden)
  set xfMisc(pathNameHidden) $xfConf(pathNameHidden)
  set xfMisc(iconBar) $xfConf(iconBar)
  set xfMisc(iconBarHidden) $xfConf(iconBarHidden)
  set xfMisc(onlyOneWindow) $xfConf(onlyOneWindow)
  set xfMisc(statusHidden) $xfConf(statusHidden)

  # build widget structure
  XFTmpltToplevel .xfOptionsWindow 400x420 {XF window options}

  XFTmpltFrame .xfOptionsWindow.frame1 0

  button .xfOptionsWindow.frame1.ok \
    -text {OK} \
    -command {
      XFOptionsWindowSet
      destroy .xfOptionsWindow}

  button .xfOptionsWindow.frame1.save \
    -text {Save + OK} \
    -command {
      XFOptionsWindowSet
      XFProcOptionsSaveOptions
      destroy .xfOptionsWindow}

  button .xfOptionsWindow.frame1.cancel \
    -text {Cancel} \
    -command {destroy .xfOptionsWindow}

  XFTmpltFrame .xfOptionsWindow.frame4

  checkbutton .xfOptionsWindow.frame4.autopos \
    -offvalue 0 \
    -onvalue 1 \
    -text {Automatic window placing} \
    -variable xfMisc(autoPos)

  checkbutton .xfOptionsWindow.frame4.autosize \
    -offvalue 0 \
    -onvalue 1 \
    -text {Automatic window sizing} \
    -variable xfMisc(autoSize)

  checkbutton .xfOptionsWindow.frame4.autostack \
    -offvalue 0 \
    -onvalue 1 \
    -text {Automatic window stacking} \
    -variable xfMisc(autoStack)

  checkbutton .xfOptionsWindow.frame4.onewindow \
    -offvalue 0 \
    -onvalue 1 \
    -text {One window per window class} \
    -variable xfMisc(onlyOneWindow)

  checkbutton .xfOptionsWindow.frame4.autorootpos \
    -offvalue 0 \
    -onvalue 1 \
    -text {Automatic root window placing} \
    -variable xfMisc(autoRootPos)

  XFTmpltFrame .xfOptionsWindow.frame5

  checkbutton .xfOptionsWindow.frame5.editlistshidden \
    -offvalue 0 \
    -onvalue 1 \
    -text {Hide widget lists} \
    -variable xfMisc(editListsHidden)

  checkbutton .xfOptionsWindow.frame5.iconbarhidden \
    -offvalue 0 \
    -onvalue 1 \
    -text {Hide iconbar} \
    -variable xfMisc(iconBarHidden)

  checkbutton .xfOptionsWindow.frame5.menubarhidden \
    -offvalue 0 \
    -onvalue 1 \
    -text {Hide menubar} \
    -variable xfMisc(menuBarHidden)

  checkbutton .xfOptionsWindow.frame5.pathnamehidden \
    -offvalue 0 \
    -onvalue 1 \
    -text {Hide path name} \
    -variable xfMisc(pathNameHidden)

  checkbutton .xfOptionsWindow.frame5.statushidden \
    -offvalue 0 \
    -onvalue 1 \
    -text {Hide status line} \
    -variable xfMisc(statusHidden)

  XFTmpltFrame .xfOptionsWindow.frame6

  checkbutton .xfOptionsWindow.frame6.iconbar \
    -offvalue child \
    -onvalue toplevel \
    -text {Show iconbar as toplevel} \
    -variable xfMisc(iconBar)

  # packing
  pack append .xfOptionsWindow.frame1 \
              .xfOptionsWindow.frame1.ok {left fill expand} \
              .xfOptionsWindow.frame1.save {left fill expand} \
              .xfOptionsWindow.frame1.cancel {left fill expand}
  pack append .xfOptionsWindow.frame4 \
              .xfOptionsWindow.frame4.autopos {top padx 10 pady 10} \
              .xfOptionsWindow.frame4.autosize {top padx 10 pady 10} \
              .xfOptionsWindow.frame4.autostack {top padx 10 pady 10} \
              .xfOptionsWindow.frame4.onewindow {top padx 10 pady 10} \
              .xfOptionsWindow.frame4.autorootpos {top padx 10 pady 10}
  pack append .xfOptionsWindow.frame5 \
              .xfOptionsWindow.frame5.editlistshidden {top padx 10 pady 10} \
              .xfOptionsWindow.frame5.iconbarhidden {top padx 10 pady 10} \
              .xfOptionsWindow.frame5.menubarhidden {top padx 10 pady 10} \
              .xfOptionsWindow.frame5.pathnamehidden {top padx 10 pady 10} \
              .xfOptionsWindow.frame5.statushidden {top padx 10 pady 10}
  pack append .xfOptionsWindow.frame6 \
              .xfOptionsWindow.frame6.iconbar {top padx 10 pady 10}
  pack append .xfOptionsWindow \
              .xfOptionsWindow.frame1 {bottom fill} \
              .xfOptionsWindow.frame4 {top fill} \
              .xfOptionsWindow.frame5 {top fill expand} \
              .xfOptionsWindow.frame6 {top fill expand}
}

##########
# Procedure: XFOptionsWindowSet
# Description: set the new options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsWindowSet {} {
  global xfConf
  global xfMisc

  set xfConf(autoPos) $xfMisc(autoPos)
  set xfConf(autoSize) $xfMisc(autoSize)
  set xfConf(autoStack) $xfMisc(autoStack)
  set xfConf(autoRootPos) $xfMisc(autoRootPos)
  set xfConf(editListsHidden) $xfMisc(editListsHidden)
  set xfConf(iconBar) $xfMisc(iconBar)
  set xfConf(iconBarHidden) $xfMisc(iconBarHidden)
  set xfConf(menuBarHidden) $xfMisc(menuBarHidden)
  set xfConf(onlyOneWindow) $xfMisc(onlyOneWindow)
  set xfConf(pathNameHidden) $xfMisc(pathNameHidden)
  set xfConf(statusHidden) $xfMisc(statusHidden)
}

# eof

