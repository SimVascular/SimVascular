# xf options file
set xfOptionVersionNr 4.3
# misc
set xfLoadPath "."
# bindings
set xfBind(configure) {<Double-Button-3>}
set xfBind(placing) "<$xf_ALT-Button-1>"
set xfBind(placingMotion) "<$xf_ALT-B1-Motion>"
set xfBind(placingRelease) "<$xf_ALT-ButtonRelease-1>"
set xfBind(popup) {3}
set xfBind(removeName) {<Any-ButtonRelease-2>}
set xfBind(select) {<Control-Button-3>}
set xfBind(select1) {<Double-Button-1>}
set xfBind(select2) {<Double-Button-2>}
set xfBind(select3) {<Double-Button-3>}
set xfBind(showName) {<Mod2-ButtonPress-2>}
# configuration
set xfConf(applyBinding) {1}
set xfConf(applyPacking) {1}
set xfConf(applyParameters) {1}
set xfConf(applyPlacing) {1}
set xfConf(autoPos) {0}
set xfConf(autoRootPos) {1}
set xfConf(autoSize) {1}
set xfConf(autoStack) {0}
set xfConf(createAppdefCode) {0}
set xfConf(createClassBinding) {0}
set xfConf(createFormCode) {0}
set xfConf(createParseCode) {0}
set xfConf(createPixmapCode) {1}
set xfConf(editListsHidden) {0}
set xfConf(encloseBinding) {1}
set xfConf(encloseConfigure) {1}
set xfConf(externalEditor) {}
set xfConf(flash) {HotPink}
set xfConf(fontMessage) {Helvetica 18 bold}
set xfConf(geometry) {packer}
set xfConf(getWidgetName) {0}
set xfConf(gridX) {0}
set xfConf(gridY) {0}
set xfConf(iconBar) {child}
set xfConf(iconBarHidden) {0}
set xfConf(interpreter) {wish}
set xfConf(interpreterEdit) {wish}
set xfConf(interpreterHasTkemacs) {0}
set xfConf(interpreterTest) {wish}
set xfConf(interpreterTut) {wish}
set xfConf(kanji) {0}
set xfConf(layoutAlways) {1}
set xfConf(layoutBorder) {3}
set xfConf(maxSaveId) {10}
set xfConf(menuBarHidden) {0}
set xfConf(onlyOneWindow) {0}
set xfConf(pathNameHidden) {0}
set xfConf(programName) {main.tcl}
set xfConf(programNameOld) {main.tcl}
set xfConf(programPath) {}
set xfConf(saveInterval) {30}
set xfConf(saveOptions) {0}
set xfConf(savePositions) {0}
set xfConf(scanTree) {1}
set xfConf(scrollSide) {left}
set xfConf(statusHidden) {0}
set xfConf(strictMotif) {0}
set xfConf(writeNewTclIndex) {1}
set xfConf(writeShellScript) {0}
set xfConf(writeTclIndex) {1}
# file
set xfFile(appdef) "$xf_DIR/xf.ad"
set xfFile(bindings) "$xf_DIR/lib/xfdefbind.tcl"
set xfFile(colors) "$xf_DIR/lib/Colors"
set xfFile(config) "$xf_DIR/xf-config.tcl"
set xfFile(cursors) "$xf_DIR/lib/Cursors"
set xfFile(emacsCmd) {TkEmacs}
set xfFile(emacsLisp) {tkemacs.el}
set xfFile(fonts) "$xf_DIR/lib/Fonts"
set xfFile(iconbar) "$xf_DIR/.xf-iconbar"
set xfFile(keysyms) "$xf_DIR/lib/Keysyms"
set xfFile(menu) {~/.xf-menubar}
set xfFile(positions) {~/.xf-positions}
set xfFile(startup) {.xf-init}
# path
set xfPath(additionals) "$xf_DIR/additionals"
set xfPath(base) "$xf_DIR"
set xfPath(elements) "$xf_DIR/elements"
set xfPath(icons) "$xf_DIR/lib/icons"
set xfPath(lib) "$xf_DIR/lib"
set xfPath(procedures) "$xf_DIR/procedures"
set xfPath(src) "$xf_DIR/src"
set xfPath(templates) "$xf_DIR/templates"
set xfPath(tmp) $xf_TMP
# version control
# comments
set xfComment(file) {
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical
#     Software Corporation, University of California, San Diego.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including 
# without limitation the rights to use, copy, modify, merge, publish, 
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
# 
# The above copyright notice and this permission notice shall be included 
# in all copies or substantial portions of the Software.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
# THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

#!$interpreter -f
# Program: $programName
# Tcl version: $tclVersion ($magicCookie)
# Tk version: $tk_version
# XF version: $xfVersion
#}
set xfComment(module) {# Module: $moduleName
# Tcl version: $tclVersion ($magicCookie)
# Tk version: $tk_version
# XF version: $xfVersion
#}
set xfComment(proc) {# Procedure: $procedureName}
set xfComment(template) {# Tcl version: $tclVersion ($magicCookie)
# Tk version: $tk_version
# XF version: $xfVersion
#}
# binding levels
set xfBindSaveLevel(1) {1}
set xfBindSaveLevel(2) {1}
set xfBindSaveLevel(3) {1}
set xfBindSaveLevel(4) {1}
set xfBindSaveLevel(5) {1}
set xfBindSaveLevel(6) {1}
set xfBindSaveLevel(7) {1}
set xfBindSaveLevel(8) {1}
set xfBindShowLevel(1) {1}
set xfBindShowLevel(2) {1}
set xfBindShowLevel(3) {1}
set xfBindShowLevel(4) {1}
set xfBindShowLevel(5) {1}
set xfBindShowLevel(6) {0}
set xfBindShowLevel(7) {0}
set xfBindShowLevel(8) {1}
# procedure levels
set xfProcSaveLevel(1) {1}
set xfProcSaveLevel(2) {1}
set xfProcSaveLevel(3) {1}
set xfProcSaveLevel(4) {1}
set xfProcSaveLevel(5) {1}
set xfProcSaveLevel(6) {1}
set xfProcSaveLevel(7) {0}
set xfProcSaveLevel(8) {1}
set xfProcShowLevel(1) {1}
set xfProcShowLevel(2) {1}
set xfProcShowLevel(3) {1}
set xfProcShowLevel(4) {1}
set xfProcShowLevel(5) {1}
set xfProcShowLevel(6) {0}
set xfProcShowLevel(7) {0}
set xfProcShowLevel(8) {1}
#
global tk_strictMotif
set tk_strictMotif 0
# eof
