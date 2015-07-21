# .frame
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.

# The symbolic names.
global symbolicName

  # build widget .frame
  frame .frame

  # build widget .frame.scrollbar1
  scrollbar .frame.scrollbar1 \
    -command {.frame.entry2 xview} \
    -orient {horizontal} \
    -relief {raised} \
    -width {11}

  # build widget .frame.entry2
  entry .frame.entry2 \
    -exportselection {true} \
    -relief {raised} \
    -xscrollcommand {.frame.scrollbar1 set}

  # pack widget .frame
  pack append .frame \
    .frame.entry2   {top frame center fill} \
    .frame.scrollbar1   {top frame center fillx} 

  # pack widgets
  pack append . \
    .frame   {top fill frame center} 

# end of widget tree

