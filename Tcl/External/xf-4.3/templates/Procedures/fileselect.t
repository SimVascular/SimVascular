# XFNoParsing
# Program: template
# Description: alert box
# Author: Mario Jorge Silva (msilva@cs.Berkeley.EDU)
#         University of California Berkeley           Ph: +1(510)642-8248
#         Computer Science Division, 571 Evans Hall   Fax: +1(510)642-5775
#         Berkeley CA 94720                                 
# Source: fileselect.t
# Version: 1.1
#
# simple file selector.
#
# Layout:
#
#  file:                  +----+
#  ____________________   | OK |
#                         +----+
#
#  +------------------+    Cancel
#  | ..               |S
#  | file1            |c
#  | file2            |r
#  |                  |b
#  | filen            |a
#  |                  |r
#  +------------------+
#  currrent-directory
#
# Copyright 1993 Regents of the University of California
# Permission to use, copy, modify, and distribute this
# software and its documentation for any purpose and without
# fee is hereby granted, provided that this copyright
# notice appears in all copies.  The University of California
# makes no representations about the suitability of this
# software for any purpose.  It is provided "as is" without
# express or implied warranty.
#


# names starting with "fileselect" are reserved by this module
# no other names used.


# use the "option" command for further configuration

option add *Listbox*font \
    "-*-helvetica-medium-r-normal-*-12-*-*-*-p-*-iso8859-1" startupFile
option add *Entry*font \
    "-*-helvetica-medium-r-normal-*-12-*-*-*-p-*-iso8859-1" startupFile
option add *Label*font \
    "-*-helvetica-medium-r-normal-*-12-*-*-*-p-*-iso8859-1" startupFile


# this is the default proc  called when "OK" is pressed
# to indicate yours, give it as the first arg to "fileselect"

proc fileselect.default.cmd {f} {# xf ignore me 5
  puts stderr "selected file $f"
}


# this is the proc that creates the file selector box

proc fileselect {
    {cmd fileselect.default.cmd} 
    {purpose "Open file:"} 
    {w .fileSelectWindow} } {# xf ignore me 5
##########
# Procedure: fileselect
# Author: Mario Jorge Silva (msilva@cs.Berkeley.EDU)
# Description: show file selection box
# Arguments: cmd - the default command to execute
#            purpose - the purpose of the selection box
#            w - the toplevel widget path
# Returns: None
# Notes: names starting with "fileselect" are reserved by this
#        module no other names used. The procedure passed as
#        the first parameter get the file name as parameter. This
#        procedure is evaluated when the OK button is pressed.
##########

    if {"[info commands XFDestroy]" != ""} {
        catch {XFDestroy $w}
    } {
        catch {destroy $w}
    }

    toplevel $w
    grab $w
    wm title $w "Select File"


    # path independent names for the widgets
    global fileselect_entry fileselect_list \
	fileselect_ok fileselect_cancel fileselect_dirlabel

    set fileselect_entry $w.file.eframe.entry
    set fileselect_list $w.file.sframe.list
    set fileselect_scroll $w.file.sframe.scroll
    set fileselect_ok $w.bframe.okframe.ok
    set fileselect_cancel $w.bframe.cancel
    set fileselect_dirlabel $w.file.dirlabel

    # widgets
    frame $w.file -bd 10 
    frame $w.bframe -bd 10
    pack append $w \
        $w.file {left filly} \
        $w.bframe {left expand frame n}

    frame $w.file.eframe
    frame $w.file.sframe
    label $w.file.dirlabel -anchor e -width 24 -text [pwd] 

    pack append $w.file \
        $w.file.eframe {top frame w} \
	$w.file.sframe {top fillx} \
	$w.file.dirlabel {top frame w}


    label $w.file.eframe.label -anchor w -width 24 -text $purpose
    entry $w.file.eframe.entry -relief sunken 

    pack append $w.file.eframe \
		$w.file.eframe.label {top expand frame w} \
                $w.file.eframe.entry {top fillx frame w}


    scrollbar $w.file.sframe.yscroll -relief sunken \
	 -command "$w.file.sframe.list yview"
    listbox $w.file.sframe.list -relief sunken \
	-yscroll "$w.file.sframe.yscroll set" 

    pack append $w.file.sframe \
        $w.file.sframe.yscroll {right filly} \
 	$w.file.sframe.list {left fill} 

    # buttons
    frame $w.bframe.okframe -borderwidth 2 -relief sunken
 
    button $w.bframe.okframe.ok -text OK -relief raised -padx 10 \
        -command "fileselect.ok.cmd $w $cmd"

    button $w.bframe.cancel -text cancel -relief raised -padx 10 \
        -command "fileselect.cancel.cmd $w"
    pack append $w.bframe.okframe $w.bframe.okframe.ok {padx 10 pady 10}

    pack append $w.bframe $w.bframe.okframe {expand padx 20 pady 20} \
                          $w.bframe.cancel {top}

    # Fill the listbox with a list of all the files in the directory (run
    # the "ls" command to get that information).
 
    foreach i [Ls -a [pwd]] {
        if {[string compare $i "."] != 0} {
            $fileselect_list insert end $i
        }
    }

    # Set up bindings for the browser.
    bind $fileselect_entry <Return> {eval $fileselect_ok invoke}
    bind $fileselect_entry <Control-c> {eval $fileselect_cancel invoke}

    bind $w <Control-c> {eval $fileselect_cancel invoke}
    bind $w <Return> {eval $fileselect_ok invoke}

    bind $fileselect_list <ButtonRelease-1> {
        # puts stderr "button 1"
        %W select clear 0 end
        %W select set [%W nearest %y]
	eval $fileselect_entry delete 0 end
	eval $fileselect_entry insert 0 [%W get [%W nearest %y]]
    }

    bind $fileselect_list <Button-1> {
        # puts stderr "button 1 release"
        %W select clear 0 end
        %W select set [%W nearest %y]
	eval $fileselect_entry delete 0 end
	eval $fileselect_entry insert 0 [%W get [%W nearest %y]]
    }

    bind $fileselect_list <Key> {
        %W select clear 0 end
        %W select set [%W nearest %y]
        eval $fileselect_entry delete 0 end
	eval $fileselect_entry insert 0 [%W get [%W nearest %y]]
    }

    bind $fileselect_list <Double-ButtonPress-1> {
        # puts stderr "double button 1"
	eval $fileselect_ok invoke
    }

    bind $fileselect_list <Return> {
        %W select clear 0 end
        %W select set [%W nearest %y]
	eval $fileselect_entry delete 0 end
	eval $fileselect_entry insert 0 [%W get [%W nearest %y]]
	eval $fileselect_ok invoke
    }

    # set kbd focus to entry widget

    focus $fileselect_entry
}


# auxiliary button procedures

proc fileselect.cancel.cmd {w} {# xf ignore me 6
    # puts stderr "Cancel"
    if {"[info commands XFDestroy]" != ""} {
        catch {XFDestroy $w}
    } {
        catch {destroy $w}
    }
}

proc fileselect.ok.cmd {w cmd} {# xf ignore me 6
    global fileselect_entry fileselect_dirlabel fileselect_list 
    set selected [$fileselect_entry get]

    if {[file isdirectory $selected] != 0} {
	cd $selected
	set dir [pwd]
	eval $fileselect_dirlabel configure -text $dir
	eval $fileselect_entry delete 0 end
	eval $fileselect_list delete 0 end
	foreach i [Ls -a $dir] {
	    if {[string compare $i "."] != 0} {
		eval $fileselect_list insert end $i
	    }
	}
	return
    }

    if {"[info commands XFDestroy]" != ""} {
        catch {XFDestroy $w}
    } {
        catch {destroy $w}
    }
    eval $cmd \"$selected\"

}

