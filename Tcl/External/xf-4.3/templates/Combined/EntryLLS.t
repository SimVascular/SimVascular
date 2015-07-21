# .frame
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.
# This template was build by: Mark Costlow

# The symbolic names.
global symbolicName

  # build widget .frame
  frame .frame \
    -borderwidth {2} \
    -relief {raised}

  # build widget .frame.frame
  frame .frame.frame

  # build widget .frame.frame.scrollbar1
  scrollbar .frame.frame.scrollbar1 \
    -command {.frame.frame.entry2 xview} \
    -orient {horizontal} \
    -width {11}

  # build widget .frame.frame.entry2
  entry .frame.frame.entry2 \
    -exportselection {true} \
    -relief {sunken} \
    -xscrollcommand {.frame.frame.scrollbar1 set}

  # pack widget .frame.frame
  pack append .frame.frame \
    .frame.frame.entry2   {top frame center expand fill} \
    .frame.frame.scrollbar1   {top frame center fillx} 

  # build widget .frame.label1
  label .frame.label1 \
    -text {Entry:}

  # pack widget .frame
  pack append .frame \
    .frame.label1   {left frame center filly} \
    .frame.frame   {top frame center fillx} 

  # pack widgets
  pack append . \
    .frame   {top fill frame center} 

# end of widget tree

