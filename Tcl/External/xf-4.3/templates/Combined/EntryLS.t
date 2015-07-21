# .frame
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.

# The symbolic names.
global symbolicName

  # build widget .frame
  frame .frame

  # build widget .frame.scrollbar7
  scrollbar .frame.scrollbar7 \
    -command {.frame.frame3.entry5 xview} \
    -orient {horizontal} \
    -relief {raised} \
    -width {11}

  # build widget .frame.frame3
  frame .frame.frame3

  # build widget .frame.frame3.entry5
  entry .frame.frame3.entry5 \
    -exportselection {true} \
    -relief {raised} \
    -xscrollcommand {.frame.scrollbar7 set}

  # build widget .frame.frame3.label4
  label .frame.frame3.label4 \
    -padx {2} \
    -pady {1} \
    -relief {raised} \
    -text {Entry:}

  # pack widget .frame.frame3
  pack append .frame.frame3 \
    .frame.frame3.label4   {left frame center} \
    .frame.frame3.entry5   {top frame center expand fill} 

  # pack widget .frame
  pack append .frame \
    .frame.frame3   {top frame center fillx} \
    .frame.scrollbar7   {top frame center fillx} 

  # pack widgets
  pack append . \
    .frame   {top fill frame center} 

# end of widget tree

