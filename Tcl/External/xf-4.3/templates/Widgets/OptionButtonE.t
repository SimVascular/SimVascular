# .frame0
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.


  # The symbolic names.
  global symbolicName

  # build widget .frame0
  frame .frame0 \
    -borderwidth {2} \
    -relief {raised}

  # build widget .frame0.label1
  label .frame0.label1 \
    -borderwidth {0} \
    -text {Label:}

  # build widget .frame0.value
  entry .frame0.value \
    -relief {sunken}
  .frame0.value insert 0 {Value 1}

  # build widget .frame0.menubutton2
  menubutton .frame0.menubutton2 \
    -menu {.frame0.menubutton2.m} \
    -text {v}

  # build widget .frame0.menubutton2.m
  menu .frame0.menubutton2.m

  # Menu widget code
  .frame0.menubutton2.m add command \
    -command {OptionButtonSet .frame0} \
    -label {Value 1}
  .frame0.menubutton2.m add command \
    -command {OptionButtonSet .frame0} \
    -label {Value 2}
  .frame0.menubutton2.m add command \
    -command {OptionButtonSet .frame0} \
    -label {Value 3}

  # pack widget .frame0
  pack append .frame0 \
    .frame0.label1 {left fill} \
    .frame0.value {left expand fill} \
    .frame0.menubutton2 {right fill} 

  # pack widgets
  pack append . \
    .frame0 {top fillx} 

proc OptionButtonGet {widget} {

  if {"[winfo class $widget.value]" == "Label"} {
    return [lindex [$widget.value config -text] 4]
  } {
    if {"[winfo class $widget.value]" == "Entry"} {
      return [$widget.value get]
    }
  }
}

proc OptionButtonSet {widget} {

  if {"[winfo class $widget.value]" == "Label"} {
    $widget.value config -text [lindex [$widget.menubutton2.m entryconfig [$widget.menubutton2.m index active] -label] 4]
  } {
    if {"[winfo class $widget.value]" == "Entry"} {
      $widget.value delete 0 end
      $widget.value insert 0 [lindex [$widget.menubutton2.m entryconfig [$widget.menubutton2.m index active] -label] 4]
    }
  }
}

# end of widget tree

