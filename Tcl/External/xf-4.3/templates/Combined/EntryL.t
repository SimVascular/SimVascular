# .frame
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.

# The symbolic names.
global symbolicName

  # build widget .frame
  frame .frame \
    -relief {raised}

  # build widget .frame.label4
  label .frame.label4 \
    -padx {2} \
    -pady {1} \
    -relief {raised} \
    -text {Entry:}

  # build widget .frame.entry5
  entry .frame.entry5 \
    -exportselection {true} \
    -relief {raised} \
    -xscrollcommand {NoFunction}

  # pack widget .frame
  pack append .frame \
    .frame.label4   {left frame center} \
    .frame.entry5   {top frame center expand fill} 

  # pack widgets
  pack append . \
    .frame   {top fill frame center} 

# end of widget tree

