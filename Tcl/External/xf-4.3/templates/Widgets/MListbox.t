# .frame0
# The above line makes pasting MUCH easier for me.
# It contains the pathname of the cutted widget.
# by: Juergen Nickelsen

  # The symbolic names.
  global symbolicName

  # build widget .frame0
  frame .frame0 \
    -borderwidth {2}

  # build widget .frame0.frame1
  frame .frame0.frame1 

  # build widget .frame0.frame1.frame3
  frame .frame0.frame1.frame3 \
    -borderwidth {2}

  # build widget .frame0.frame1.frame3.listbox8
  listbox .frame0.frame1.frame3.listbox8 \
    -width {10} \
    -height {2} \
    -relief {sunken} \
    -xscrollcommand {.frame0.frame2.frame5.scrollbar10 set} \
    -yscrollcommand {.frame0.frame1.frame4.scrollbar9 set}


  # pack widget .frame0.frame1.frame3
  pack append .frame0.frame1.frame3 \
    .frame0.frame1.frame3.listbox8 {left frame center expand fill} 


  # build widget .frame0.frame1.frame4
  frame .frame0.frame1.frame4 \
    -borderwidth {2}

  # build widget .frame0.frame1.frame4.scrollbar9
  scrollbar .frame0.frame1.frame4.scrollbar9 \
    -command {.frame0.frame1.frame3.listbox8 yview} \
    -relief {sunken} \
    -width {13}

  # pack widget .frame0.frame1.frame4
  pack append .frame0.frame1.frame4 \
    .frame0.frame1.frame4.scrollbar9 {top frame center expand filly} 


  # pack widget .frame0.frame1
  pack append .frame0.frame1 \
    .frame0.frame1.frame3 {left frame center expand fill} \
    .frame0.frame1.frame4 {right frame center filly} 


  # build widget .frame0.frame2
  frame .frame0.frame2 \
    -borderwidth {1}

  # build widget .frame0.frame2.frame5
  frame .frame0.frame2.frame5 \
    -borderwidth {2}

  # build widget .frame0.frame2.frame5.scrollbar10
  scrollbar .frame0.frame2.frame5.scrollbar10 \
    -command {.frame0.frame1.frame3.listbox8 xview} \
    -orient {horizontal} \
    -relief {sunken} \
    -width {13}

  # pack widget .frame0.frame2.frame5
  pack append .frame0.frame2.frame5 \
    .frame0.frame2.frame5.scrollbar10 {left frame center expand fillx} 


  # build widget .frame0.frame2.frame6
  frame .frame0.frame2.frame6 \
    -borderwidth {2}

  # build widget .frame0.frame2.frame6.frame11
  frame .frame0.frame2.frame6.frame11 \
    -borderwidth {2} \
    -height {13} \
    -width {16}

  # pack widget .frame0.frame2.frame6
  pack append .frame0.frame2.frame6 \
    .frame0.frame2.frame6.frame11 {top frame center expand fill} 


  # pack widget .frame0.frame2
  pack append .frame0.frame2 \
    .frame0.frame2.frame5 {left frame center expand fill} \
    .frame0.frame2.frame6 {right frame center filly} 


  # pack widget .frame0
  pack append .frame0 \
    .frame0.frame1 {top frame center expand fill} \
    .frame0.frame2 {bottom frame center fillx} 


  # pack widgets
  pack append . \
    .frame0 {top frame center expand fill} 

# end of widget tree

