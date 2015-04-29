# tclxslt.tcl --
#
#	Tcl library for TclXSLT package.
#
# Copyright (c) 2001-2003 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: tclxslt.tcl,v 1.4 2003/12/09 04:57:35 balls Exp $

namespace eval xslt {
    namespace export getprocs
}

proc xslt::getprocs ns {
    set functions {}
    set elements {}
    foreach proc [info commands ${ns}::*] {
	if {[regexp {::([^:]+)$} $proc discard name]} {
	    if {[string equal [lindex [info args $proc] end] "args"]} {
		lappend functions $name
	    } else {
		lappend elements $name
	    }
	}
    }
    return [list $elements $functions]
}
