# .menu0
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.


  # The symbolic names.
  global symbolicName

  # build widget .menu0
  menu .menu0

  .menu0 add command \
    -label {Empty menu}

  bind [winfo parent .menu0] <Shift-Button-1> {tk_popup .menu0 %X %Y}

