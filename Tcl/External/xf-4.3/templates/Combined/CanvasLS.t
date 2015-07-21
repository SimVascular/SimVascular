# .frame0
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.

# The symbolic names.
global symbolicName

  # build widget .frame0
  frame .frame0 \
    -relief {raised}

  # build widget .frame0.scrollbar3
  scrollbar .frame0.scrollbar3 \
    -command {.frame0.canvas2 xview} \
    -orient {horizontal} \
    -relief {raised}

  # build widget .frame0.scrollbar1
  scrollbar .frame0.scrollbar1 \
    -command {.frame0.canvas2 yview} \
    -relief {raised}

  # build widget .frame0.canvas2
  canvas .frame0.canvas2 \
    -confine {true} \
    -height {207} \
    -relief {raised} \
    -scrollregion {0c 0c 20c 20c} \
    -width {295} \
    -xscrollcommand {.frame0.scrollbar3 set} \
    -yscrollcommand {.frame0.scrollbar1 set}

  # build canvas items .frame0.canvas2

  # pack widget .frame0
  pack append .frame0 \
    .frame0.scrollbar1   {left frame center filly} \
    .frame0.canvas2   {top frame center expand fill} \
    .frame0.scrollbar3   {top frame center fillx} 

  # pack widgets
  pack append . \
    .frame0   {top fill frame center} 

# end of widget tree

