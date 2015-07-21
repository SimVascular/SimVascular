# .squeezer
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.
# Tcl version: 7.0 (Tcl/Tk/XF)
# Tk version: 3.3
# XF version: 2.2
#

package require tile

  # build widget .squeezer
  ttk::frame .squeezer \
    -borderwidth {4} \
    -relief {ridge}

  # build widget .squeezer.tframe4
  ttk::frame .squeezer.tframe4 \
    -width 30 \
    -height 30 \
    -relief {raised}

  # build widget .squeezer.tcheckbutton8
  ttk::checkbutton .squeezer.tcheckbutton8 \
    -command {if {$squeeze(.squeezer.tcheckbutton8)} {
  pack configure .squeezer.tframe4 -side bottom -fill both -expand true
} {
  pack forget .squeezer.tframe4
} } \
    -text {Messages} \
    -variable {squeeze(.squeezer.tcheckbutton8)}

  # pack master .squeezer
  pack configure .squeezer.tcheckbutton8 -anchor w

  # pack slave .squeezer
  pack configure .squeezer -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 -side top

# end of widget tree

