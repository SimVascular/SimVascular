# Program: xf
# Description: the option dialog for general options
#
# $Header: xfoptPath.tcl[2.3] Wed Mar 10 12:07:18 1993 garfield@garfield frozen $

##########
# Procedure: XFOptionsPath
# Description: allow the editing of the path options
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsPath {} {
  global xfBind
  global xfFile
  global xfLoadPath
  global xfPath

  # build widget structure 
  XFTmpltToplevel .xfOptionsPath 400x590 {XF path names}

  XFTmpltFrame .xfOptionsPath.frame1 0

  XFTmpltFrame .xfOptionsPath.frame2

  XFTmpltFrame .xfOptionsPath.frame3

  button .xfOptionsPath.frame1.ok \
    -text {OK} \
    -command {
      XFOptionsPathSet
      destroy .xfOptionsPath}

  button .xfOptionsPath.frame1.save \
    -text {Save + OK} \
    -command {
      XFOptionsPathSet
      XFProcOptionsSaveOptions
      destroy .xfOptionsPath}

  button .xfOptionsPath.frame1.cancel \
    -text {Cancel} \
    -command {destroy .xfOptionsPath}

  label .xfOptionsPath.frame2.message2 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {XF path:}

  label .xfOptionsPath.frame2.message3 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Additionals path:}

  label .xfOptionsPath.frame2.message4 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Elements path:}

  label .xfOptionsPath.frame2.message6 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Icon path:}

  label .xfOptionsPath.frame2.message7 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Library path:}

  label .xfOptionsPath.frame2.message8 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Module load path:}

  label .xfOptionsPath.frame2.message9 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Procedures path:}

  label .xfOptionsPath.frame2.message10 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Source path:}

  label .xfOptionsPath.frame2.message11 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Template path:}

  label .xfOptionsPath.frame2.message12 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Tmp path:}

  label .xfOptionsPath.frame2.message13 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {AppDef file:}

  label .xfOptionsPath.frame2.message14 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Binding file:}

  label .xfOptionsPath.frame2.message15 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Color file:}

  label .xfOptionsPath.frame2.message16 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Config file:}

  label .xfOptionsPath.frame2.message17 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Cursor file:}

  label .xfOptionsPath.frame2.message18 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Font file:}

  label .xfOptionsPath.frame2.message19 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Iconbar file:}

  label .xfOptionsPath.frame2.message20 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Keysym file:}

  label .xfOptionsPath.frame2.message21 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Menubar file:}

  label .xfOptionsPath.frame2.message22 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Position file:}

  label .xfOptionsPath.frame2.message23 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {Startup file:}

  label .xfOptionsPath.frame2.message24 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {TkEmacs editor:}

  label .xfOptionsPath.frame2.message25 \
    -anchor e \
    -padx 0 \
    -pady 0 \
    -highlightthickness 3 \
    -text {TkEmacs lisp file:}

  entry .xfOptionsPath.frame3.xf \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.xf insert 0 $xfPath(base)

  entry .xfOptionsPath.frame3.additional \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.additional insert 0 $xfPath(additionals)

  entry .xfOptionsPath.frame3.element \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.element insert 0 $xfPath(elements)

  entry .xfOptionsPath.frame3.icons \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.icons insert 0 $xfPath(icons)

  entry .xfOptionsPath.frame3.lib \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.lib insert 0 $xfPath(lib)

  entry .xfOptionsPath.frame3.modules \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.modules insert 0 $xfLoadPath

  entry .xfOptionsPath.frame3.procedures \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.procedures insert 0 $xfPath(procedures)

  entry .xfOptionsPath.frame3.src \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.src insert 0 $xfPath(src)

  entry .xfOptionsPath.frame3.tmplate \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.tmplate insert 0 $xfPath(templates)

  entry .xfOptionsPath.frame3.tmp \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.tmp insert 0 $xfPath(tmp)

  entry .xfOptionsPath.frame3.appdef \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.appdef insert 0 $xfFile(appdef)

  entry .xfOptionsPath.frame3.bindings \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.bindings insert 0 $xfFile(bindings)

  entry .xfOptionsPath.frame3.colors \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.colors insert 0 $xfFile(colors)

  entry .xfOptionsPath.frame3.config \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.config insert 0 $xfFile(config)

  entry .xfOptionsPath.frame3.cursor \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.cursor insert 0 $xfFile(cursors)

  entry .xfOptionsPath.frame3.fonts \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.fonts insert 0 $xfFile(fonts)

  entry .xfOptionsPath.frame3.iconbar \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.iconbar insert 0 $xfFile(iconbar)

  entry .xfOptionsPath.frame3.keysyms \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.keysyms insert 0 $xfFile(keysyms)

  entry .xfOptionsPath.frame3.menu \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.menu insert 0 $xfFile(menu)

  entry .xfOptionsPath.frame3.positions \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.positions insert 0 $xfFile(positions)

  entry .xfOptionsPath.frame3.startup \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.startup insert 0 $xfFile(startup)

  entry .xfOptionsPath.frame3.emacscmd \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.emacscmd insert 0 $xfFile(emacsCmd)

  entry .xfOptionsPath.frame3.emacslisp \
    -highlightthickness 2 \
    -relief sunken
  .xfOptionsPath.frame3.emacslisp insert 0 $xfFile(emacsLisp)

  # bindings
  bind .xfOptionsPath.frame3.xf $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.additional $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.element $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.icons $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.lib $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.modules $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.procedures $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.src $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.tmplate $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.tmp $xfBind(configure) {
    XFProcFSBoxPath %W}
  bind .xfOptionsPath.frame3.appdef $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.bindings $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.colors $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.config $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.cursor $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.fonts $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.iconbar $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.keysyms $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.menu $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.positions $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.startup $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.emacscmd $xfBind(configure) {
    XFProcFSBoxFile %W}
  bind .xfOptionsPath.frame3.emacslisp $xfBind(configure) {
    XFProcFSBoxFile %W}

  # packing
  pack append .xfOptionsPath.frame1 \
              .xfOptionsPath.frame1.ok {left fill expand} \
              .xfOptionsPath.frame1.save {left fill expand} \
              .xfOptionsPath.frame1.cancel {left fill expand}
  pack append .xfOptionsPath.frame2 \
              .xfOptionsPath.frame2.message2 {top fillx} \
              .xfOptionsPath.frame2.message3 {top fillx} \
              .xfOptionsPath.frame2.message4 {top fillx} \
              .xfOptionsPath.frame2.message6 {top fillx} \
              .xfOptionsPath.frame2.message7 {top fillx} \
              .xfOptionsPath.frame2.message8 {top fillx} \
              .xfOptionsPath.frame2.message9 {top fillx} \
              .xfOptionsPath.frame2.message10 {top fillx} \
              .xfOptionsPath.frame2.message11 {top fillx} \
              .xfOptionsPath.frame2.message12 {top fillx} \
              .xfOptionsPath.frame2.message13 {top fillx} \
              .xfOptionsPath.frame2.message14 {top fillx} \
              .xfOptionsPath.frame2.message15 {top fillx} \
              .xfOptionsPath.frame2.message16 {top fillx} \
              .xfOptionsPath.frame2.message17 {top fillx} \
              .xfOptionsPath.frame2.message18 {top fillx} \
              .xfOptionsPath.frame2.message19 {top fillx} \
              .xfOptionsPath.frame2.message20 {top fillx} \
              .xfOptionsPath.frame2.message21 {top fillx} \
              .xfOptionsPath.frame2.message22 {top fillx} \
              .xfOptionsPath.frame2.message23 {top fillx} \
              .xfOptionsPath.frame2.message24 {top fillx} \
              .xfOptionsPath.frame2.message25 {top fillx}
  pack append .xfOptionsPath.frame3 \
              .xfOptionsPath.frame3.xf {top fillx} \
              .xfOptionsPath.frame3.additional {top fillx} \
              .xfOptionsPath.frame3.element {top fillx} \
              .xfOptionsPath.frame3.icons {top fillx} \
              .xfOptionsPath.frame3.lib {top fillx} \
              .xfOptionsPath.frame3.modules {top fillx} \
              .xfOptionsPath.frame3.procedures {top fillx} \
              .xfOptionsPath.frame3.src {top fillx} \
              .xfOptionsPath.frame3.tmplate {top fillx} \
              .xfOptionsPath.frame3.tmp {top fillx} \
              .xfOptionsPath.frame3.appdef {top fillx} \
              .xfOptionsPath.frame3.bindings {top fillx} \
              .xfOptionsPath.frame3.colors {top fillx} \
              .xfOptionsPath.frame3.config {top fillx} \
              .xfOptionsPath.frame3.cursor {top fillx} \
              .xfOptionsPath.frame3.fonts {top fillx} \
              .xfOptionsPath.frame3.iconbar {top fillx} \
              .xfOptionsPath.frame3.keysyms {top fillx} \
              .xfOptionsPath.frame3.menu {top fillx} \
              .xfOptionsPath.frame3.positions {top fillx} \
              .xfOptionsPath.frame3.startup {top fillx} \
              .xfOptionsPath.frame3.emacscmd {top fillx} \
              .xfOptionsPath.frame3.emacslisp {top fillx}
  pack append .xfOptionsPath \
              .xfOptionsPath.frame1 {bottom fill} \
              .xfOptionsPath.frame2 {left filly} \
              .xfOptionsPath.frame3 {left fill expand}

  XFBindFormConnect .xfOptionsPath.frame3
}

##########
# Procedure: XFOptionsPathSet
# Description: set the new paths
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFOptionsPathSet {} {
  global xfFile
  global xfLoadPath
  global xfPath
  global xfStatus

  if {[XFMiscIsDir [.xfOptionsPath.frame3.xf get]] == 1} {
    set xfPath(base) [.xfOptionsPath.frame3.xf get]
  }
  if {[XFMiscIsDir [.xfOptionsPath.frame3.element get]] == 1} {
    set xfPath(elements) [.xfOptionsPath.frame3.element get]
  }
  if {[XFMiscIsDir [.xfOptionsPath.frame3.lib get]] == 1} {
    set xfPath(lib) [.xfOptionsPath.frame3.lib get]
  }
  if {[XFMiscIsDir [.xfOptionsPath.frame3.src get]] == 1} {
    set xfPath(src) [.xfOptionsPath.frame3.src get]
  }
  if {[XFMiscIsDir [.xfOptionsPath.frame3.tmp get]] == 1} {
    set xfPath(tmp) [.xfOptionsPath.frame3.tmp get]
  }
  set xfLoadPath [.xfOptionsPath.frame3.modules get]
  set xfPath(additionals) [.xfOptionsPath.frame3.additional get]
  set xfPath(icons) [.xfOptionsPath.frame3.icons get]
  set xfPath(procedures) [.xfOptionsPath.frame3.procedures get]
  set xfPath(templates) [.xfOptionsPath.frame3.tmplate get]
  set xfStatus(tmpltPath) ""
  XFEditReadTemplates $xfStatus(tmpltPath)

  set xfFile(appdef) [.xfOptionsPath.frame3.appdef get]
  set xfFile(bindings) [.xfOptionsPath.frame3.bindings get]
  set xfFile(colors) [.xfOptionsPath.frame3.colors get]
  set xfFile(config) [.xfOptionsPath.frame3.config get]
  set xfFile(cursors) [.xfOptionsPath.frame3.cursor get]
  set xfFile(fonts) [.xfOptionsPath.frame3.fonts get]
  set xfFile(iconbar) [.xfOptionsPath.frame3.iconbar get]
  set xfFile(keysyms) [.xfOptionsPath.frame3.keysyms get]
  set xfFile(menu) [.xfOptionsPath.frame3.menu get]
  set xfFile(positions) [.xfOptionsPath.frame3.positions get]
  set xfFile(startup) [.xfOptionsPath.frame3.startup get]
  set xfFile(emacsCmd) [.xfOptionsPath.frame3.emacscmd get]
  set xfFile(emacsLisp) [.xfOptionsPath.frame3.emacslisp get]
}

# eof

