# .frame0
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.


  # The symbolic names.
  global symbolicName

  # build widget .frame0
  frame .frame0 \
    -borderwidth {2} \
    -relief {raised}

  # build widget .frame0.menubutton1
  menubutton .frame0.menubutton1 \
    -menu {.frame0.menubutton1.m} \
    -text {File} \
    -underline {0}

  # build widget .frame0.menubutton1.m
  menu .frame0.menubutton1.m

  # build widget .frame0.menubutton2
  menubutton .frame0.menubutton2 \
    -menu {.frame0.menubutton2.m} \
    -text {Help} \
    -underline {0}

  # build widget .frame0.menubutton2.m
  menu .frame0.menubutton2.m

  # pack widget .frame0
  pack append .frame0 \
    .frame0.menubutton1 {left} \
    .frame0.menubutton2 {right} 

  # pack widgets
  pack append . \
    .frame0 {top fillx} 

# end of widget tree

