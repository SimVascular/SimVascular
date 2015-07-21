# .squeezer
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.
# Tcl version: 7.0 (Tcl/Tk/XF)
# Tk version: 3.3
# XF version: 2.2
#

  # build widget .squeezer
  frame .squeezer \
    -borderwidth {4} \
    -relief {ridge}

  # build widget .squeezer.frame4
  frame .squeezer.frame4

  # build widget .squeezer.frame4.listbox5
  listbox .squeezer.frame4.listbox5 \
    -width 1 \
    -height 6 \
    -relief {sunken} \
    -xscrollcommand {.squeezer.frame4.scrollbar7 set} \
    -yscrollcommand {.squeezer.frame4.scrollbar6 set}

  # build widget .squeezer.frame4.scrollbar6
  scrollbar .squeezer.frame4.scrollbar6 \
    -command {.squeezer.frame4.listbox5 yview} \
    -relief {sunken}

  # build widget .squeezer.frame4.scrollbar7
  scrollbar .squeezer.frame4.scrollbar7 \
    -command {.squeezer.frame4.listbox5 xview} \
    -orient {horizontal} \
    -relief {sunken}

  # build widget .squeezer.checkbutton8
  checkbutton .squeezer.checkbutton8 \
    -anchor {w} \
    -command {if {$squeeze(.squeezer.checkbutton8)} {
  pack configure .squeezer.frame4 -side bottom -fill both -expand true
} {
  pack forget .squeezer.frame4
} } \
    -relief {flat} \
    -text {Messages} \
    -variable {squeeze(.squeezer.checkbutton8)}

  # pack master .squeezer.frame4
  pack configure .squeezer.frame4.scrollbar6 \
    -fill y \
    -side right
  pack configure .squeezer.frame4.listbox5 \
    -expand 1 \
    -fill both
  pack configure .squeezer.frame4.scrollbar7 \
    -fill x \
    -side bottom

  # pack master .squeezer
  pack configure .squeezer.checkbutton8 \
    -anchor w

  # pack slave .squeezer
  pack configure .squeezer -anchor center -expand 0 -fill x -ipadx 0 -ipady 0 -padx 0 -pady 0 -side top

  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.96.11" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.96.11" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.1.79" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.96.11" connected with MIT-MAGIC-COOKIE-1 authorization}
  .squeezer.frame4.listbox5 insert end {%XSERVER-I-NEWCLIENT, host "131.162.96.11" connected with MIT-MAGIC-COOKIE-1 authorization}


# end of widget tree

