# .frame
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.
# Tcl version: 7.0 (Tcl/Tk/XF)
# Tk version: 3.3
# XF version: 2.2
# by: Lionel Mallet

  # build widget .frame
  frame .frame \
    -borderwidth {5}

  # build widget .frame.scrollbar2
  scrollbar .frame.scrollbar2 \
    -command {.frame.listbox1 yview} \
    -relief {sunken}

  # build widget .frame.frame
  frame .frame.frame

  # build widget .frame.frame.scrollbar3
  scrollbar .frame.frame.scrollbar3 \
    -command {.frame.listbox1 xview} \
    -orient {horizontal} \
    -relief {sunken}

  # build widget .frame.frame.frame
  frame .frame.frame.frame \
    -width {19} \
    -height {10}

  # pack widget .frame.frame
  pack append .frame.frame \
    .frame.frame.scrollbar3 {left frame center pady 8 expand fillx} \
    .frame.frame.frame {left frame center padx 8}

  # build widget .frame.label1
  label .frame.label1 \
    -text {Parent types}

  # build widget .frame.listbox1
  listbox .frame.listbox1 \
    -exportselection {0} \
    -width {14} \
    -height {10} \
    -relief {sunken} \
    -xscrollcommand {.frame.frame.scrollbar3 set} \
    -yscrollcommand {.frame.scrollbar2 set}
  # bindings
  bind .frame.listbox1 <B1-Motion> {%W select from [%W nearest %y]}
  bind .frame.listbox1 <Button-1> {%W select from [%W nearest %y]}
  bind .frame.listbox1 <ButtonRelease-1> {set selection [%W nearest %y]}
  bind .frame.listbox1 <Shift-B1-Motion> {%W select from [%W nearest %y]}
  bind .frame.listbox1 <Shift-Button-1> {%W select from [%W nearest %y]}

  # pack widget .frame
  pack append .frame \
    .frame.label1 {top frame center fillx} \
    .frame.frame {bottom frame center fillx} \
    .frame.scrollbar2 {right frame center padx 8 filly} \
    .frame.listbox1 {top frame center expand fill}
  # pack widgets
  pack append . \
    .frame {left frame center expand fill}
# end of widget tree
