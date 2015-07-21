# Program: xf
# Description: Canvas drawing tools added by Dennis LaBelle
#
# Date: March 15, 1998

##########
# Procedure: XFProcDrawTools
# Description: Show canvas drawing tool bar
# Arguments: none
# Returns: none
# Sideeffects: none
##########
proc XFProcDrawTools {} {
global xfgrouplist
global xfgrouplrx
global xfgrouplry
global xfgroupulx
global xfgroupuly
global xftoolpath
global xftoolstat
global xfundelcmd
global xfstretchhalo
global xfxftoolpathorg
global xfkeytime
global xfstretchobj
global xfiditem

  # build widget .xfcanvastools
  if {[winfo exists .xfcanvastools]} {catch "XFDestroy .xfcanvastools"}

  toplevel .xfcanvastools -relief {raised}

  # Window manager configurations
  wm positionfrom .xfcanvastools user
  wm sizefrom .xfcanvastools user
  wm maxsize .xfcanvastools 1024 768
  wm minsize .xfcanvastools 132 1
  wm protocol .xfcanvastools WM_DELETE_WINDOW {
	if {[winfo exists .xfcanvastools]} {catch "XFDestroy .xfcanvastools"}
  }
  wm title .xfcanvastools {Draw Tools}

  # initialize variables
  set xfstretchhalo -1.0
  set xfxftoolpathorg ""
  set xfkeytime    ""
  set xfstretchobj ""
  set xfgrouplist  ""
  set xfgrouplrx   ""
  set xfgrouplry   ""
  set xfgroupulx   ""
  set xfgroupuly   ""
  set xftoolpath   ""
  set xfundelcmd   ""
  set xfiditem     ""

  frame .xfcanvastools.frame0 -height {30} -width {30}
  label .xfcanvastools.frame0.label1 -background {steelblue} -borderwidth {1} \
    -font {Helvetica 10} -foreground {white} -relief {raised} -text {Current  Path:}

  # bindings
  bind .xfcanvastools.frame0.label1 <Button-1> {%W configure -cursor question_arrow}
  bind .xfcanvastools.frame0.label1 <ButtonRelease-1> {%W configure -cursor ""
set xftemp [winfo containing %X %Y]
if {$xftemp != "" && [winfo class $xftemp] == "Canvas"} {
    XFcancel_bind $xftoolpath
    XFToolChoose cancel
    set xftoolpath $xftemp
    XFDTcolor_stretch_set $xftoolpath
   }}

  # build widget .xfcanvastools.frame0.entry2
  entry .xfcanvastools.frame0.entry2 \
    -background {white} \
    -borderwidth {1} \
    -font {Helvetica 10} \
    -textvariable {xftoolpath}
  # bindings
  bind .xfcanvastools.frame0.entry2 <Key> {if {$xfkeytime == ""} {
    set xfxftoolpathorg $xftoolpath
    after 500 XFcheckybd
   }
set xfkeytime [clock clicks]}

  # build widget .xfcanvastools.frame0.label2
  label .xfcanvastools.frame0.label2 \
    -borderwidth {0} \
    -font {Helvetica 10} \
    -foreground {blue} \
    -textvariable {xfiditem}

  # build widget .xfcanvastools.frame0.label4
  label .xfcanvastools.frame0.label4 \
    -borderwidth {0} \
    -font {Helvetica 10} \
    -text {ID = }

  # build widget .xfcanvastools.frame10
  frame .xfcanvastools.frame10 \
    -height {30} \
    -highlightthickness {2} \
    -width {30}

  # build widget .xfcanvastools.frame10.button8
  button .xfcanvastools.frame10.button8 \
    -borderwidth {1} \
    -command {XFDrawToolsDelete} \
    -font {Helvetica 10} \
    -padx {4} \
    -text {Done} \
    -width {4}

  # build widget .xfcanvastools.frame10.button0
  button .xfcanvastools.frame10.button0 \
    -borderwidth {1} \
    -command {XFDTundelete $xftoolpath} \
    -font {Helvetica 10} \
    -padx {3} \
    -text {Paste} \
    -width {4}

  # build widget .xfcanvastools.frame2
  frame .xfcanvastools.frame2 \
    -height {30} \
    -width {30}

  # build widget .xfcanvastools.frame2.radiobutton4
  radiobutton .xfcanvastools.frame2.radiobutton4 \
    -command {XFToolChoose objmove} \
    -font {Helvetica 10} \
    -text {ObjMove} \
    -value {objmove} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame2.radiobutton6
  radiobutton .xfcanvastools.frame2.radiobutton6 \
    -command {XFToolChoose objadd} \
    -font {Helvetica 10} \
    -text {Attach} \
    -value {objadd} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame2.radiobutton8
  radiobutton .xfcanvastools.frame2.radiobutton8 \
    -command {XFToolChoose objdel} \
    -font {Helvetica 10} \
    -text {Detach} \
    -value {objdel} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame2.radiobutton7
  radiobutton .xfcanvastools.frame2.radiobutton7 \
    -command {XFToolChoose ID} \
    -font {Helvetica 10} \
    -text {ID} \
    -value {ID} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame2.radiobutton9
  radiobutton .xfcanvastools.frame2.radiobutton9 \
    -command {XFToolChoose cancel} \
    -font {Helvetica 10} \
    -text {None} \
    -value {cancel} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame3
  frame .xfcanvastools.frame3 \
    -height {30} \
    -width {30}

  # build widget .xfcanvastools.frame3.radiobutton5
  radiobutton .xfcanvastools.frame3.radiobutton5 \
    -command {XFToolChoose move} \
    -font {Helvetica 10} \
    -text {GrpMove} \
    -value {move} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame3.radiobutton6
  radiobutton .xfcanvastools.frame3.radiobutton6 \
    -command {XFToolChoose stretch} \
    -font {Helvetica 10} \
    -text {Stretch} \
    -value {stretch} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame3.radiobutton7
  radiobutton .xfcanvastools.frame3.radiobutton7 \
    -command {XFToolChoose delete} \
    -font {Helvetica 10} \
    -text {Cut} \
    -value {delete} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame3.radiobutton8
  radiobutton .xfcanvastools.frame3.radiobutton8 \
    -command {XFToolChoose copy} \
    -font {Helvetica 10} \
    -text {Copy} \
    -value {copy} \
    -variable {toolcur}

  # build widget .xfcanvastools.frame3.radiobutton10
  radiobutton .xfcanvastools.frame3.radiobutton10 \
    -command {XFToolChoose onemove} \
    -font {Helvetica 10} \
    -text {OneMove} \
    -value {onemove} \
    -variable {toolcur}

  # pack master .xfcanvastools.frame0
  pack configure .xfcanvastools.frame0.label1 \
    -side left
  pack configure .xfcanvastools.frame0.entry2 \
    -ipadx 1 \
    -padx 2 \
    -side left
  pack configure .xfcanvastools.frame0.label4 \
    -padx 2 \
    -side left
  pack configure .xfcanvastools.frame0.label2 \
    -side left

  # pack master .xfcanvastools.frame10
  pack configure .xfcanvastools.frame10.button8 \
    -expand 1 \
    -fill x \
    -padx 2 \
    -side left
  pack configure .xfcanvastools.frame10.button0 \
    -anchor n \
    -padx 2 \
    -side right

  # pack master .xfcanvastools.frame2
  pack configure .xfcanvastools.frame2.radiobutton4 \
    -side left
  pack configure .xfcanvastools.frame2.radiobutton6 \
    -side left
  pack configure .xfcanvastools.frame2.radiobutton8 \
    -side left
  pack configure .xfcanvastools.frame2.radiobutton7 \
    -side left
  pack configure .xfcanvastools.frame2.radiobutton9 \
    -side left

  # pack master .xfcanvastools.frame3
  pack configure .xfcanvastools.frame3.radiobutton5 \
    -side left
  pack configure .xfcanvastools.frame3.radiobutton6 \
    -side left
  pack configure .xfcanvastools.frame3.radiobutton7 \
    -side left
  pack configure .xfcanvastools.frame3.radiobutton8 \
    -side left
  pack configure .xfcanvastools.frame3.radiobutton10 \
    -side left

  # pack master .xfcanvastools
  pack configure .xfcanvastools.frame10 \
    -anchor e \
    -pady 3 \
    -side bottom
  pack configure .xfcanvastools.frame0 \
    -anchor w \
    -fill x
  pack configure .xfcanvastools.frame3 \
    -anchor w
  pack configure .xfcanvastools.frame2 \
    -anchor w
}

proc XFDrawToolsDelete {} {
     XFDestroy .xfcanvastools
}

# Procedure: XFcancel_bind
proc XFcancel_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {$cname == ""} {return}
$cname delete groupbox
set xfB1_press_list ""
set xfB1_motion_list ""
set xfB1_release_list ""
bind DrawTools <ButtonPress-1> {XFB1_press %W [%W canvasx %x] [%W canvasy %y]}
bind DrawTools <B1-Motion> {XFB1_motion %W [%W canvasx %x] [%W canvasy %y]}
bind DrawTools <ButtonRelease-1> {XFB1_release %W [%W canvasx %x] [%W canvasy %y]}
set blist [bindtags $cname]
if {[lsearch $blist DrawTools] == -1} {
	lappend blist DrawTools
	bindtags $cname $blist
  }
}


# Procedure: XFcheckybd
proc XFcheckybd {} {
global xfkeytime
global xftoolpath
global xfxftoolpathorg

set curtime [clock clicks]
set timeout [expr $xfkeytime + 1000]
if {$curtime > $timeout} {
    if {[winfo exists $xftoolpath] && [winfo class $xftoolpath] == "Canvas"} {
	XFcancel_bind $xfxftoolpathorg
	XFToolChoose cancel
	XFDTcolor_stretch_set $xftoolpath
       } { set xftoolpath $xfxftoolpathorg }
    set xfkeytime ""
   } {
    after 500 XFcheckybd
   }
}


# Procedure: XFcopy_bind
proc XFcopy_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {[lsearch $xfB1_press_list group] == -1} {lappend xfB1_press_list group}
if {[lsearch $xfB1_motion_list group] == -1} {lappend xfB1_motion_list group}
if {[lsearch $xfB1_release_list copy] == -1} {lappend xfB1_release_list copy}
}


# Procedure: XFcopy_group
proc XFcopy_group { window xpos ypos} {
global xfgrouplist

if {$xfgrouplist == ""} {return}
set newxfgrouplist ""
foreach item $xfgrouplist {
	set optlist [$window itemconfigure $item]
	if {$optlist != ""} {
	    set cmd "$window create [$window type $item] [$window coords $item]"
	    foreach option $optlist {
		    lappend cmd [lindex $option 0]
		    lappend cmd [lindex $option 4]
		  }
	    set newtag [eval $cmd]
	    lappend newxfgrouplist $newtag
	   }
      }
set xfgrouplist $newxfgrouplist
XFmove_group $window $xpos $ypos
}


# Procedure: XFdelete_bind
proc XFdelete_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {[lsearch $xfB1_press_list group] == -1} {lappend xfB1_press_list group}
if {[lsearch $xfB1_motion_list group] == -1} {lappend xfB1_motion_list group}
if {[lsearch $xfB1_release_list delete] == -1} {lappend xfB1_release_list delete}
}


# Procedure: XFdelete_group
proc XFdelete_group { window} {
global xfgrouplist
global xfundelcmd

$window delete groupbox
if {$xfgrouplist == "groupbox"} {
    set xfgrouplist ""
    return
   }
set xfundelcmd ""
foreach item $xfgrouplist {
	set optlist [$window itemconfigure $item]
	if {$optlist != ""} {
	    set cmd "$window create [$window type $item] [$window coords $item]"
	    foreach option $optlist {
		    lappend cmd [lindex $option 0]
		    lappend cmd [lindex $option 4]
		  }
	    lappend xfundelcmd $cmd
	   }
      }
eval "$window delete $xfgrouplist"
}


# Procedure: XFmove_bind
proc XFmove_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {[lsearch $xfB1_press_list group] == -1} {lappend xfB1_press_list group}
if {[lsearch $xfB1_motion_list group] == -1} {lappend xfB1_motion_list group}
if {[lsearch $xfB1_release_list move] == -1} {lappend xfB1_release_list move}
}


# Procedure: XFmove_group
proc XFmove_group { window xpos ypos} {
global xfgroupulx
global xfgroupuly
global xfgrouplrx
global xfgrouplry
global xfgrouplist

if {$xfgrouplist == ""} {return}
set xcenter [expr ($xfgroupulx + $xfgrouplrx) / 2]
set xmove [expr $xpos - $xcenter]
set ycenter [expr ($xfgroupuly + $xfgrouplry) / 2]
set ymove [expr $ypos - $ycenter]
foreach item $xfgrouplist {
	$window move $item $xmove $ymove
      }
set cmd "$window bbox $xfgrouplist"
set newbox [eval $cmd]
set xfgroupulx [lindex $newbox 0]
set xfgroupuly [lindex $newbox 1]
set xfgrouplrx [lindex $newbox 2]
set xfgrouplry [lindex $newbox 3]
}


# Procedure: XFstretch_bind
proc XFstretch_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {[lsearch $xfB1_press_list stretch] == -1} {lappend xfB1_press_list stretch}
if {[lsearch $xfB1_motion_list stretch] == -1} {lappend xfB1_motion_list stretch}
if {[lsearch $xfB1_release_list stretch] == -1} {lappend xfB1_release_list stretch}
}


# Procedure: XFstretch_move
proc XFstretch_move { cname xpos ypos} {
# This procedure modifies the closest vertice to the mouse
#
global xfstretchhalo
global xfstretchobj

set object $xfstretchobj
if {$object != ""} {
    set coords [$cname coords $object]
    set totlen [llength $coords]
    set smallest $xfstretchhalo
    set closest -1
    set curidx 0
    for {set ctr 0} {$ctr < $totlen} {incr ctr 2} {
	 set curx [lindex $coords $ctr]
	 set cury [lindex $coords [expr $ctr + 1] ]
	 set xsquare  [expr ($curx - $xpos) * ($curx - $xpos)]
	 set ysquare  [expr ($cury - $ypos) * ($cury - $ypos)]
	 set sumsquare [expr $xsquare + $ysquare]
	 set distance [expr sqrt($sumsquare)]
	 if {$distance < $smallest} {
		set smallest $distance
		set closest  $ctr
	    }
	 incr curidx
	}
    if {$closest != -1} {
	set coords [lreplace $coords $closest [expr $closest + 1] $xpos $ypos]
	set cmd "$cname coords $object $coords"
	eval $cmd
       }
}
}


# Procedure: XFstretch_start
proc XFstretch_start { cname xpos ypos} {
# This procedure modifies the closest vertice to the mouse
#
global xfstretchhalo
global xfstretchobj

set xfstretchhalo 5.0
set object [lindex [$cname find closest $xpos $ypos] 0]
if {$object != ""} {
    set xfstretchobj $object
    set coords [$cname coords $object]
    set totlen [llength $coords]
    set smallest $xfstretchhalo
    set closest -1
    set curidx 0
    for {set ctr 0} {$ctr < $totlen} {incr ctr 2} {
	 set curx [lindex $coords $ctr]
	 set cury [lindex $coords [expr $ctr + 1] ]
	 set xsquare  [expr ($curx - $xpos) * ($curx - $xpos)]
	 set ysquare  [expr ($cury - $ypos) * ($cury - $ypos)]
	 set sumsquare [expr $xsquare + $ysquare]
	 set distance [expr sqrt($sumsquare)]
	 if {$distance < $smallest} {
		set smallest $distance
		set closest  $ctr
	    }
	 incr curidx
	}
    if {$closest != -1} {
	set coords [lreplace $coords $closest [expr $closest + 1] $xpos $ypos]
	set cmd "$cname coords $object $coords"
	eval $cmd
	set xfstretchhalo 1000000.0
       } else { set xfstretchhalo -1.0 }
}
}


# Procedure: XFstretch_stop
proc XFstretch_stop {} {
# This procedure terminates stretching
#
global xfstretchhalo

set xfstretchhalo -1.0
}


# Procedure: XFToolChoose
proc XFToolChoose { position} {
global xftoolpath
global xftoolstat
global toolcur
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list
global objaddfirst
global objdelfirst
global objmovefirst

set xftoolstat($xftoolpath) $position
set highcolor red
set normalcolor systembuttonface
XFcancel_bind $xftoolpath
switch $position {
	cancel	{XFcancel_bind $xftoolpath; set toolcur cancel}
	conpipe	{XFDTconpipe_bind $xftoolpath}
	copy	{XFcopy_bind $xftoolpath}
	delete	{XFdelete_bind $xftoolpath}
	ID	{ if {[lsearch $xfB1_press_list ID] == -1} {lappend xfB1_press_list ID} }
	move	{XFmove_bind $xftoolpath}
	onemove {XFDTonemove_bind $xftoolpath}
	objmove	{
		  XFDTobj_bind $xftoolpath
		  set objmovefirst 1
		}
	objadd	{
		  if {[lsearch $xfB1_press_list objadd] == -1} {lappend xfB1_press_list objadd}
		  set objaddfirst 1
		}
	objdel	{
		  if {[lsearch $xfB1_press_list objdel] == -1} {lappend xfB1_press_list objdel}
		  set objdelfirst 1
		}
	stretch	{XFstretch_bind $xftoolpath}
   }
}


# Procedure: XFDTundelete
proc XFDTundelete { window} {
global xfgrouplist
global xfundelcmd

if {$xfundelcmd == ""} {return}
set newxfgrouplist ""
foreach cmd $xfundelcmd {
	set newtag [eval $cmd]
	if {$newtag != ""} {
	    lappend newxfgrouplist $newtag
	   }
      }
set xfgrouplist $newxfgrouplist
set xfundelcmd ""
}


# Procedure: XFDTobj_add
proc XFDTobj_add { cname} {
# This procedure associates a canvas item with the current object
#
global XFDTobjcur

set curid [$cname find withtag current]
if {$curid == ""} {return}
set taglist ""
foreach tag [$cname gettags $curid] {
	if {[string range $tag 0 3] != "obj_"} {
		lappend taglist $tag
	   }
      }
lappend taglist $XFDTobjcur
$cname itemconfigure $curid -tags $taglist
}


# Procedure: XFDTobj_bind
proc XFDTobj_bind { cname} {
global xfB1_press_list
global xfB1_motion_list

if {[lsearch $xfB1_press_list objmove] == -1} {lappend xfB1_press_list objmove}
if {[lsearch $xfB1_motion_list objmove] == -1} {lappend xfB1_motion_list objmove}
}


# Procedure: XFDTobj_delete
proc XFDTobj_delete { cname} {
# This procedure disassociates an item from any objects
#
global XFDTobjcur

set curid [$cname find withtag current]
if {$curid == ""} {return}
set taglist ""
foreach tag [$cname gettags $curid] {
	if {[string range $tag 0 3] != "obj_"} {
		lappend taglist $tag
	   }
      }
$cname itemconfigure $curid -tags $taglist
}


# Procedure: XFDTobj_getobj
proc XFDTobj_getobj { cname id} {
# return object ID number of a canvas item
foreach tag [$cname gettags $id] {
	if {[string range $tag 0 3] == "obj_"} {
		return [string range $tag 4 end]
	   }
      }
return ""
}


# Procedure: XFDTobj_move
proc XFDTobj_move { window xpos ypos} {
global xposlast
global yposlast
global XFDTobjcur

if {$XFDTobjcur == ""} {return}
set xmove [expr $xpos - $xposlast]
set xposlast $xpos
set ymove [expr $ypos - $yposlast]
set yposlast $ypos
$window move $XFDTobjcur $xmove $ymove
}


# Procedure: XFDTobj_next
proc XFDTobj_next { cname} {
# find all object ID numbers
set objnumbers ""
set objlist [$cname find all]
foreach curid $objlist {
	foreach tag [$cname gettags $curid] {
		if {[string range $tag 0 3] == "obj_"} {
			lappend objnumbers [string range $tag 4 end]
		   }
	      }
      }

# select an object ID number not currently used
set rtnval ""
for {set ctr 0} {$ctr < 10000} {incr ctr} {
	if {[lsearch $objnumbers $ctr] == -1} {
		set rtnval obj_$ctr
		break
	   }
    }
return $rtnval
}


# Procedure: XFDTobj_select
proc XFDTobj_select { cname} {
global XFDTobjcur

set XFDTobjcur ""
foreach tag [$cname gettags current] {
	if {[string range $tag 0 3] == "obj_"} {
		set XFDTobjcur $tag
		break
	   }
      }
if {$XFDTobjcur == ""} {
    set XFDTobjcur [XFDTobj_next $cname]
   }
}


# Procedure: XFDTobj_start
proc XFDTobj_start { window xpos ypos} {
global xposlast
global yposlast
global XFDTobjcur

set curid [$window find withtag current]
set XFDTobjcur ""
foreach item [$window gettags $curid] {
	if {[string range $item 0 3] == "obj_"} {
		set XFDTobjcur $item
		break
	   }
          }
set xposlast $xpos
set yposlast $ypos

global result
set result $XFDTobjcur
}


# Procedure: XFDTcolor_stretch_set
proc XFDTcolor_stretch_set { cname} {
global XFDTcolor_stretch

set XFDTcolor_stretch yellow; return
if {$cname == ""} {return}
set clist [winfo rgb $cname [$cname cget -bg]]
set xlist "#"
foreach color $clist {
	set delta [expr abs(255 - ($color >> 8))]
	set xlist "${xlist}[format "%02X" $delta]"
      }
set XFDTcolor_stretch $xlist
}


# Procedure: XFDTconnect_resize
proc XFDTconnect_resize { window xpos ypos} {
global tether_startx
global tether_starty
global tether_endx
global tether_endy

if {[$window find withtag tether] != ""} {
	$window coords tether $tether_startx $tether_starty $xpos $ypos
	set tether_endx $xpos
	set tether_endy $ypos
   }
}


# Procedure: XFDTconnect_start
proc XFDTconnect_start { window xpos ypos} {
global tether_startx
global tether_starty
global XFDTcolor_stretch

set taglist [$window gettags current]
if {[lsearch $taglist output] != -1} {
	set bbox [$window bbox current]
	set tether_startx [expr ([lindex $bbox 2] + [lindex $bbox 0]) / 2]
	set tether_starty [expr ([lindex $bbox 3] + [lindex $bbox 1]) / 2]
	$window create line $tether_startx $tether_starty $tether_startx $tether_starty -tags tether -fill $XFDTcolor_stretch
   }
}


# Procedure: XFDTconnect_stop
proc XFDTconnect_stop { window xpos ypos} {
global tether_startx
global tether_starty
global tether_endx
global tether_endy
global color_pipe
global pipe_start
global pipe_end

if {[$window find withtag tether] != ""} {
	$window delete tether
	set hitlist [$window find overlapping $xpos $ypos $xpos $ypos]
	foreach curid $hitlist {
		if {$curid != ""} {
		    if {[lsearch [$window gettags $curid] input] != -1} {
			set bbox [$window bbox $curid]
			set tether_endx [expr ([lindex $bbox 2] + [lindex $bbox 0]) / 2]
			set tether_endy [expr ([lindex $bbox 3] + [lindex $bbox 1]) / 2]
			set newtag pipe_[pipe_getnew $window]
			$window create line $tether_startx $tether_starty $tether_endx $tether_endy -tags $newtag -width 2 -fill $color_pipe
			set obj_id [XFDTobj_getobj $window $curid]
			if {[info exists pipe_end($obj_id)]} {
			      lappend pipe_end($obj_id) $newtag
			   } {set pipe_end([XFDTobj_getobj $window $curid]) $newtag}
			break
		       }
		   }
	            }
   }
}


# Procedure: XFDTconpipe_bind
proc XFDTconpipe_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {[lsearch $xfB1_press_list conpipe] == -1} {lappend xfB1_press_list conpipe}
if {[lsearch $xfB1_motion_list conpipe] == -1} {lappend xfB1_motion_list conpipe}
if {[lsearch $xfB1_release_list conpipe] == -1} {lappend xfB1_release_list conpipe}
}


# Procedure: XFDTgroup_resize
proc XFDTgroup_resize { window xpos ypos} {
global xfgroupulx
global xfgroupuly
global xfgrouplrx
global xfgrouplry

$window coords groupbox $xfgroupulx $xfgroupuly $xpos $ypos
set xfgrouplrx $xpos
set xfgrouplry $ypos
}


# Procedure: XFDTgroup_start
proc XFDTgroup_start { window xpos ypos} {
global xfgroupulx
global xfgroupuly
global XFDTcolor_stretch

$window create rectangle $xpos $ypos $xpos $ypos -tags groupbox -outline $XFDTcolor_stretch
set xfgroupulx $xpos
set xfgroupuly $ypos
}


# Procedure: XFDTgroup_stop
proc XFDTgroup_stop { window xpos ypos} {
global xfgroupulx
global xfgroupuly
global xfgrouplist
global xfgrouplrx
global xfgrouplry

set xfgrouplist [$window find enclosed $xfgroupulx $xfgroupuly $xpos $ypos]
if {$xfgrouplist != ""} {
    set cmd "$window bbox $xfgrouplist"
    set bbox [eval $cmd]
   } else { set bbox "" }
if {$bbox != ""} {
    set xfgroupulx [lindex $bbox 0]
    set xfgroupuly [lindex $bbox 1]
    set xfgrouplrx [lindex $bbox 2]
    set xfgrouplry [lindex $bbox 3]
    $window coords groupbox $xfgroupulx $xfgroupuly $xfgrouplrx $xfgrouplry
   }
lappend xfgrouplist groupbox
}


# Procedure: XFDTldelete
proc XFDTldelete { vname item} {
set pos [lsearch $vname $item]
if {$pos > -1} {
	set rtnval [lreplace $vname $pos $pos]
   } {	set rtnval $vname }
return $rtnval
}


# Procedure: XFDTpipe_getnew
proc XFDTpipe_getnew { cname} {
# find all pipe ID numbers
set pipenumbers ""
set objlist [$cname find all]
foreach curid $objlist {
	foreach tag [$cname gettags $curid] {
		if {[string range $tag 0 4] == "pipe_"} {
			lappend pipenumbers [string range $tag 5 end]
		   }
	      }
      }

# select a pipe ID number not currently used
for {set ctr 0} {$ctr < 10000} {incr ctr} {
	if {[lsearch $pipenumbers $ctr] == -1} {
		return $ctr
	   }
    }
return -1
}


# Procedure: XFDTidentify
proc XFDTidentify { cname xpos ypos} {
global xfiditem

set xfiditem [$cname find withtag current]
if {[winfo exists .xfCanvasConfig5]} {
   set taglist [$cname gettags $xfiditem]
   set pos [lsearch $taglist current]
   if {$pos != -1} {
	 set newtags [lreplace $taglist $pos $pos]
	 $cname itemconfigure $xfiditem -tags $newtags
	}
   set lname .xfCanvasConfig5.params1.params2.frame1.children.items.items
   set size [$lname size]
   for {set index 0} {$index < $size} {incr index} {
         if {[lsearch [$lname get $index] $xfiditem] != -1} {
	$lname selection clear 0 $size
	$lname selection set $index
	$lname see $index
	XFCanvasFlashItem $cname Canvas
	XFCanvasSetItem $cname Canvas
	break
           }
       }
  }
}


# Procedure: XFDTone_start
proc XFDTone_start { cname xpos ypos} {
global xfgroupulx
global xfgroupuly
global XFDTcolor_stretch
global xfgrouplist
global xfgroupulx
global xfgroupuly
global xfgrouplrx
global xfgrouplry

set xfgrouplist [$cname find withtag current]
if {$xfgrouplist != ""} {
    set bbox [eval "$cname bbox $xfgrouplist"]
    set xfgroupulx [lindex $bbox 0]
    incr xfgroupulx -1
    set xfgroupuly [lindex $bbox 1]
    incr xfgroupuly -1
    set xfgrouplrx [lindex $bbox 2]
    incr xfgrouplrx
    set xfgrouplry [lindex $bbox 3]
    incr xfgrouplry
    $cname create rectangle $xfgroupulx $xfgroupuly $xfgrouplrx $xfgrouplry -tags groupbox -outline $XFDTcolor_stretch
    lappend xfgrouplist groupbox
   }
}


# Procedure: XFDTonemove_bind
proc XFDTonemove_bind { cname} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list

if {[lsearch $xfB1_press_list group] == -1} {lappend xfB1_press_list onemove}
if {[lsearch $xfB1_motion_list group] == -1} {lappend xfB1_motion_list move}
if {[lsearch $xfB1_release_list move] == -1} {lappend xfB1_release_list endonemove}
}


# Procedure: XFB1_motion
proc XFB1_motion { cname xpos ypos} {
global xfB1_motion_list
global xftoolpath

#if {$cname != $xftoolpath} {return}
foreach tool $xfB1_motion_list {
	switch $tool {
		conpipe	{XFDTconnect_resize $cname $xpos $ypos}
		group	{XFDTgroup_resize   $cname $xpos $ypos}
		move	{XFmove_group   $cname $xpos $ypos}
		objmove	{XFDTobj_move     $cname $xpos $ypos}
		stretch	{XFstretch_move $cname $xpos $ypos}
	     }
      }
}


# Procedure: XFB1_press
proc XFB1_press { cname xpos ypos} {
global xfB1_press_list
global groupboxid
global xftoolpath
global objaddfirst
global objdelfirst

#if {$cname != $xftoolpath} {return}
foreach tool $xfB1_press_list {
	switch $tool {
		copy	{
			  XFcopy_group    $cname $xpos $ypos
			  $cname delete $groupboxid
			}
		conpipe	{XFDTconnect_start $cname $xpos $ypos}
		ID	{ XFDTidentify $cname $xpos $ypos }
		group	{XFDTgroup_start   $cname $xpos $ypos}
		move	{XFmove_group    $cname $xpos $ypos}
		onemove {
			 XFDTone_start $cname $xpos $ypos
			 XFmove_group $cname $xpos $ypos
			}
		objadd	{
			  if {$objaddfirst} {
			      XFDTobj_select $cname
			      set objaddfirst 0
			     }
			  XFDTobj_add $cname
			}
		objmove	{ XFDTobj_start $cname $xpos $ypos }
		objdel	{
			  if {$objdelfirst} {
			      XFDTobj_select $cname
			      set objdelfirst 0
			     }
			  XFDTobj_delete    $cname
			}
		stretch	{XFstretch_start $cname $xpos $ypos}
	     }
      }
}


# Procedure: XFB1_release
proc XFB1_release { cname xpos ypos} {
global xfB1_press_list
global xfB1_motion_list
global xfB1_release_list
global groupboxid
global xftoolpath

#if {$cname != $xftoolpath} {return}
foreach tool $xfB1_release_list {
	switch $tool {
		conpipe	{ XFDTconnect_stop $cname $xpos $ypos}
		copy	{
			  set groupboxid [$cname find withtag groupbox]
			  XFDTgroup_stop $cname $xpos $ypos
			  set xfB1_press_list [XFDTldelete $xfB1_press_list group]
			  set xfB1_motion_list [XFDTldelete $xfB1_motion_list group]
			  set xfB1_release_list [XFDTldelete $xfB1_release_list copy]
			  if {[lsearch $xfB1_press_list copy] == -1} {lappend xfB1_press_list copy}
			  if {[lsearch $xfB1_motion_list move] == -1} {lappend xfB1_motion_list move}
			  if {[lsearch $xfB1_release_list endcopy] == -1} {lappend xfB1_release_list endcopy}
			}
		endcopy	{
			  $cname delete groupbox
			  set xfB1_press_list [XFDTldelete $xfB1_press_list copy]
			  set xfB1_motion_list [XFDTldelete $xfB1_motion_list move]
			  set xfB1_release_list [XFDTldelete $xfB1_release_list endcopy]
			  XFcopy_bind $cname
			}
		endmove {
			  $cname delete groupbox
			  set xfB1_press_list [XFDTldelete $xfB1_press_list move]
			  set xfB1_motion_list [XFDTldelete $xfB1_motion_list move]
			  set xfB1_release_list [XFDTldelete $xfB1_release_list endmove]
			  XFmove_bind $cname
			}
		endonemove {
			  $cname delete groupbox
			  set xfB1_press_list [XFDTldelete $xfB1_press_list onemove]
			  set xfB1_motion_list [XFDTldelete $xfB1_motion_list move]
			  set xfB1_release_list [XFDTldelete $xfB1_release_list endonemove]
			  XFDTonemove_bind $cname
			}
		delete	{
			  XFDTgroup_stop $cname $xpos $ypos
			  XFdelete_group $cname
			}
		move	{
			  XFDTgroup_stop $cname $xpos $ypos
			  set xfB1_press_list [XFDTldelete $xfB1_press_list group]
			  set xfB1_motion_list [XFDTldelete $xfB1_motion_list group]
			  set xfB1_release_list [XFDTldelete $xfB1_release_list move]
			  if {[lsearch $xfB1_press_list move] == -1} {lappend xfB1_press_list move}
			  if {[lsearch $xfB1_motion_list move] == -1} {lappend xfB1_motion_list move}
			  if {[lsearch $xfB1_release_list endmove] == -1} {lappend xfB1_release_list endmove}
			}
		objmove	{XFDTobj_move     $cname $xpos $ypos}
		stretch	{XFstretch_stop}
	     }
      }
}
