# .frame
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.

# The symbolic names.
global symbolicName

  # build widget .frame
  frame .frame

  # build widget .frame.scrollbar3
  scrollbar .frame.scrollbar3 \
    -command {.frame.listbox1 xview} \
    -orient {horizontal} \
    -relief {raised}

  # build widget .frame.scrollbar2
  scrollbar .frame.scrollbar2 \
    -command {.frame.listbox1 yview} \
    -relief {raised}

  # build widget .frame.listbox1
  listbox .frame.listbox1 \
    -exportselection {true} \
    -relief {raised} \
    -xscrollcommand {.frame.scrollbar3 set} \
    -yscrollcommand {.frame.scrollbar2 set}

  # pack widget .frame
  pack append .frame \
    .frame.scrollbar2   {right frame center filly} \
    .frame.listbox1   {top frame center expand fill} \
    .frame.scrollbar3   {bottom frame center fillx} 

  # pack widgets
  pack append . \
    .frame   {top fill frame center} 

# end of widget tree

