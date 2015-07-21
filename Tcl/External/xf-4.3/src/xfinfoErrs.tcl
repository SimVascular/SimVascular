# Program: xf
# Description: info routine for error status
#
# $Header: xfinfoErrs.tcl[2.3] Wed Mar 10 12:06:10 1993 garfield@garfield frozen $

##########
# Procedure: XFInfoErrors
# Description: show the current error status
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFInfoErrors {} {
  global errorCode
  global errorInfo

  # build widget structure
  XFTmpltToplevel .xfInfoErrors 400x300 {XF errors}

  XFTmpltFrame .xfInfoErrors.frame2

  button .xfInfoErrors.frame2.ok \
    -text {OK (or probably not ?? :-)} \
    -command {destroy .xfInfoErrors}

  XFTmpltFrame .xfInfoErrors.frame1 0

  XFTmpltFrame .xfInfoErrors.frame1.frame2

  label .xfInfoErrors.frame1.frame2.message1 \
    -anchor w \
    -text "Last error code:"

  label .xfInfoErrors.frame1.frame2.errcode \
    -anchor w \
    -textvariable errorCode

  XFTmpltFrame .xfInfoErrors.frame1.frame3 0

  label .xfInfoErrors.frame1.frame3.message1 \
    -anchor c \
    -relief raised \
    -text "Last error info:"

  message .xfInfoErrors.frame1.frame3.errinfo \
    -anchor nw \
    -borderwidth 2 \
    -textvariable errorInfo \
    -relief raised

  # packing
  pack append .xfInfoErrors.frame1.frame2 \
              .xfInfoErrors.frame1.frame2.message1 {left fill} \
              .xfInfoErrors.frame1.frame2.errcode {left fill expand}
  pack append .xfInfoErrors.frame1.frame3 \
              .xfInfoErrors.frame1.frame3.message1 {top fillx} \
              .xfInfoErrors.frame1.frame3.errinfo {left fill expand}
  pack append .xfInfoErrors.frame1 \
              .xfInfoErrors.frame1.frame2 {top fill} \
              .xfInfoErrors.frame1.frame3 {top fill expand}
  pack append .xfInfoErrors.frame2 \
              .xfInfoErrors.frame2.ok {bottom padx 4 pady 4 expand}
  pack append .xfInfoErrors \
              .xfInfoErrors.frame2 {bottom fill} \
              .xfInfoErrors.frame1 {bottom fill expand}
}

# eof

