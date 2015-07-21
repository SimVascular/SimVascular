# .text
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.

# The symbolic names.
global symbolicName

  # build widget .text1
  text .text1 \
    -relief {raised} \
    -borderwidth 2 \
    -wrap {none}
  # bindings
  bind .text1 <Any-Key> {NoFunction} 

  # pack widgets
  pack append . \
    .text1   {top fill frame center} 

# end of widget tree

