# Program: xf
# Description: info routine for aliases
#
# $Header: xfinfoAliases.tcl[2.3] Wed Mar 10 12:06:05 1993 garfield@garfield frozen $

##########
# Procedure: XFInfoAliases
# Description: show the currently defined aliases
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFInfoAliases {} {
  global internalAliasList

  XFEditSetStatus "Calling alias list..."

  # build widget structure
  XFTmpltToplevel .xfInfoAliases 450x400 {XF aliases}

  XFTmpltFrame .xfInfoAliases.frame3

  button .xfInfoAliases.frame3.ok \
    -text {OK} \
    -command {destroy .xfInfoAliases}

  button .xfInfoAliases.frame3.rescan \
    -text {Rescan} \
    -command {XFMiscSetInfo commands .xfInfoAliases.frame1.commands.commands 0}

  XFTmpltFrame .xfInfoAliases.frame1 0

  XFTmpltListbox .xfInfoAliases.frame1 aliases
  .xfInfoAliases.frame1.aliases.aliases configure \
    -width 20 -height 30

  label .xfInfoAliases.frame1.aliases.aliasesMess \
    -relief raised \
    -text {Aliases:}

  XFTmpltListbox .xfInfoAliases.frame1 commands
  .xfInfoAliases.frame1.commands.commands configure \
    -width 20 -height 30

  label .xfInfoAliases.frame1.commands.commandsMess \
    -relief raised \
    -text {Commands:}

  XFTmpltLabledEntry .xfInfoAliases.frame1 alias "Alias name:"
  
  XFTmpltLabledEntry .xfInfoAliases.frame1 aliased "Aliased command:"
  
  XFTmpltFrame .xfInfoAliases.frame2

  button .xfInfoAliases.frame2.insert \
    -text {Insert} \
    -command {
      if {"[.xfInfoAliases.frame1.alias.alias get]" != "" &&
          "[.xfInfoAliases.frame1.aliased.aliased get]" != ""} {
        global internalAliasList
        Alias [.xfInfoAliases.frame1.alias.alias get] [.xfInfoAliases.frame1.aliased.aliased get]
        XFMiscClearList .xfInfoAliases.frame1.aliases.aliases
        foreach xfElement [lsort $internalAliasList] {
          .xfInfoAliases.frame1.aliases.aliases insert end \
            [lindex $xfElement 0]
        }
      }}

  button .xfInfoAliases.frame2.delete \
    -text {Delete} \
    -command {
      if {"[.xfInfoAliases.frame1.alias.alias get]" != ""} {
        Unalias [.xfInfoAliases.frame1.alias.alias get]
        XFMiscClearList .xfInfoAliases.frame1.aliases.aliases
        foreach xfElement $internalAliasList {
          .xfInfoAliases.frame1.aliases.aliases insert end \
            [lindex $xfElement 0]
        }
        .xfInfoAliases.frame1.alias.alias delete 0 end
        .xfInfoAliases.frame1.aliased.aliased delete 0 end
      }}

  foreach xfElement [lsort $internalAliasList] {
    .xfInfoAliases.frame1.aliases.aliases insert end [lindex $xfElement 0]
  }

  XFMiscSetInfo commands .xfInfoAliases.frame1.commands.commands 0

  # bindings
  bind .xfInfoAliases.frame1.aliases.aliases <ButtonPress-1> {
    XFBindSelectOne %W %y
    set xfCurSelect [.xfInfoAliases.frame1.aliases.aliases curselect]
    if {$xfCurSelect >= 0} {
      set xfTmpValue [.xfInfoAliases.frame1.aliases.aliases get $xfCurSelect]
      .xfInfoAliases.frame1.alias.alias delete 0 end
      .xfInfoAliases.frame1.alias.alias insert end $xfTmpValue
      .xfInfoAliases.frame1.aliased.aliased delete 0 end
      .xfInfoAliases.frame1.aliased.aliased insert end [Alias $xfTmpValue]
    }}
  bind .xfInfoAliases.frame1.aliases.aliases <Button1-Motion> {
    XFBindSelectOne %W %y}
  bind .xfInfoAliases.frame1.aliases.aliases <Shift-ButtonPress-1> {
    XFBindSelectOne %W %y}
  bind .xfInfoAliases.frame1.aliases.aliases <Shift-Button1-Motion> {
    XFBindSelectOne %W %y}

  bind .xfInfoAliases.frame1.commands.commands <ButtonPress-1> {
    XFBindSelectOne %W %y
    set xfCurSelect [.xfInfoAliases.frame1.commands.commands curselect]
    if {$xfCurSelect >= 0} {
      set xfTmpValue [.xfInfoAliases.frame1.commands.commands get $xfCurSelect]
      .xfInfoAliases.frame1.aliased.aliased delete 0 end
      .xfInfoAliases.frame1.aliased.aliased insert end $xfTmpValue
    }}
  bind .xfInfoAliases.frame1.commands.commands <Button1-Motion> {
    XFBindSelectOne %W %y}
  bind .xfInfoAliases.frame1.commands.commands <Shift-ButtonPress-1> {
    XFBindSelectOne %W %y}
  bind .xfInfoAliases.frame1.commands.commands <Shift-Button1-Motion> {
    XFBindSelectOne %W %y}

  bind .xfInfoAliases.frame1.alias.alias <Return> {
    XFBindFocusIn .xfInfoAliases.frame1.aliased.aliased
  }
  bind .xfInfoAliases.frame1.aliased.aliased <Return> {
    global internalAliasList
    if {"[.xfInfoAliases.frame1.alias.alias get]" != "" &&
        "[.xfInfoAliases.frame1.aliased.aliased get]" != ""} {
      Alias [.xfInfoAliases.frame1.alias.alias get] [.xfInfoAliases.frame1.aliased.aliased get]
      XFMiscClearList .xfInfoAliases.frame1.aliases.aliases
      foreach xfElement [lsort $internalAliasList] {
        .xfInfoAliases.frame1.aliases.aliases insert end [lindex $xfElement 0]
      }
    }
  }

  # packing
  pack before .xfInfoAliases.frame1.aliases.vscroll \
              .xfInfoAliases.frame1.aliases.aliasesMess {top fillx}
  pack before .xfInfoAliases.frame1.commands.vscroll \
              .xfInfoAliases.frame1.commands.commandsMess {top fillx}
  pack append .xfInfoAliases.frame1 \
              .xfInfoAliases.frame1.aliased {bottom fillx} \
              .xfInfoAliases.frame1.alias {bottom fillx} \
              .xfInfoAliases.frame1.aliases {left fill expand} \
              .xfInfoAliases.frame1.commands {left fill expand}
  pack append .xfInfoAliases.frame2 \
              .xfInfoAliases.frame2.insert {left padx 4 pady 4 expand} \
              .xfInfoAliases.frame2.delete {left padx 4 pady 4 expand}
  pack append .xfInfoAliases.frame3 \
              .xfInfoAliases.frame3.ok {left padx 4 pady 4 expand} \
              .xfInfoAliases.frame3.rescan {left padx 4 pady 4 expand}
  pack append .xfInfoAliases \
              .xfInfoAliases.frame3 {bottom fill} \
              .xfInfoAliases.frame2 {bottom fill} \
              .xfInfoAliases.frame1 {bottom fill expand}

  XFEditSetStatus "Calling alias list...done"
}

# eof

