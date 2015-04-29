# xml__tcl.tcl --
#
#	This file provides a Tcl implementation of the parser
#	class support found in ../tclxml.c.  It is only used
#	when the C implementation is not installed (for some reason).
#
# Copyright (c) 2005 by Explain.
# http://www.explain.com.au/
# Copyright (c) 2000-2004 Zveno Pty Ltd
# http://www.zveno.com/
# 
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: xml__tcl.tcl,v 1.15.2.1 2005/12/28 06:49:51 balls Exp $

package provide xml::tcl 3.2

namespace eval xml {
    namespace export configure parser parserclass

    # Parser implementation classes
    variable classes
    array set classes {}

    # Default parser class
    variable default {}

    # Counter for generating unique names
    variable counter 0
}

# xml::configure --
#
#	Configure the xml package
#
# Arguments:
#	None
#
# Results:
#	None (not yet implemented)

proc xml::configure args {}

# xml::parserclass --
#
#	Implements the xml::parserclass command for managing
#	parser implementations.
#
# Arguments:
#	method	subcommand
#	args	method arguments
#
# Results:
#	Depends on method

proc xml::parserclass {method args} {
    variable classes
    variable default

    switch -- $method {

	create {
	    if {[llength $args] < 1} {
		return -code error "wrong number of arguments, should be xml::parserclass create name ?args?"
	    }

	    set name [lindex $args 0]
	    if {[llength [lrange $args 1 end]] % 2} {
		return -code error "missing value for option \"[lindex $args end]\""
	    }
	    array set classes [list $name [list \
		    -createcommand [namespace current]::noop \
		    -createentityparsercommand [namespace current]::noop \
		    -parsecommand [namespace current]::noop \
		    -configurecommand [namespace current]::noop \
		    -getcommand [namespace current]::noop \
		    -deletecommand [namespace current]::noop \
	    ]]
	    # BUG: we're not checking that the arguments are kosher
	    set classes($name) [lrange $args 1 end]
	    set default $name
	}

	destroy {
	    if {[llength $args] < 1} {
		return -code error "wrong number of arguments, should be xml::parserclass destroy name"
	    }

	    if {[info exists classes([lindex $args 0])]} {
		unset classes([lindex $args 0])
	    } else {
		return -code error "no such parser class \"[lindex $args 0]\""
	    }
	}

	info {
	    if {[llength $args] < 1} {
		return -code error "wrong number of arguments, should be xml::parserclass info method"
	    }

	    switch -- [lindex $args 0] {
		names {
		    return [array names classes]
		}
		default {
		    return $default 
		}
	    }
	}

	default {
	    return -code error "unknown method \"$method\""
	}
    }

    return {}
}

# xml::parser --
#
#	Create a parser object instance
#
# Arguments:
#	args	optional name, configuration options
#
# Results:
#	Returns object name.  Parser instance created.

proc xml::parser args {
    variable classes
    variable default

    if {[llength $args] < 1} {
	# Create unique name, no options
	set parserName [FindUniqueName]
    } else {
	if {[string index [lindex $args 0] 0] == "-"} {
	    # Create unique name, have options
	    set parserName [FindUniqueName]
	} else {
	    # Given name, optional options
	    set parserName [lindex $args 0]
	    set args [lrange $args 1 end]
	}
    }

    array set options [list \
	-parser $default
    ]
    array set options $args

    if {![info exists classes($options(-parser))]} {
	return -code error "no such parser class \"$options(-parser)\""
    }

    # Now create the parser instance command and data structure
    # The command must be created in the caller's namespace
    uplevel 1 [list proc $parserName {method args} "eval [namespace current]::ParserCmd [list $parserName] \[list \$method\] \$args"]
    upvar #0 [namespace current]::$parserName data
    array set data [list class $options(-parser)]

    array set classinfo $classes($options(-parser))
    if {[string compare $classinfo(-createcommand) ""]} {
	eval $classinfo(-createcommand) [list $parserName]
    }
    if {[string compare $classinfo(-configurecommand) ""] && \
	    [llength $args]} {
	eval $classinfo(-configurecommand) [list $parserName] $args
    }

    return $parserName
}

# xml::FindUniqueName --
#
#	Generate unique object name
#
# Arguments:
#	None
#
# Results:
#	Returns string.

proc xml::FindUniqueName {} {
    variable counter
    return xmlparser[incr counter]
}

# xml::ParserCmd --
#
#	Implements parser object command
#
# Arguments:
#	name	object reference
#	method	subcommand
#	args	method arguments
#
# Results:
#	Depends on method

proc xml::ParserCmd {name method args} {
    variable classes
    upvar #0 [namespace current]::$name data

    array set classinfo $classes($data(class))

    switch -- $method {

	configure {
	    # BUG: We're not checking for legal options
	    array set data $args
	    eval $classinfo(-configurecommand) [list $name] $args
	    return {}
	}

	cget {
	    return $data([lindex $args 0])
	}

	entityparser {
	    set new [FindUniqueName]

	    upvar #0 [namespace current]::$name parent
	    upvar #0 [namespace current]::$new data
	    array set data [array get parent]

	    uplevel 1 [list proc $new {method args} "eval [namespace current]::ParserCmd [list $new] \[list \$method\] \$args"]

	    return [eval $classinfo(-createentityparsercommand) [list $name $new] $args]
	}

	free {
	    eval $classinfo(-deletecommand) [list $name]
	    unset data
	    uplevel 1 [list rename $name {}]
	}

	get {
	    eval $classinfo(-getcommand) [list $name] $args
	}

	parse {
	    if {[llength $args] < 1} {
		return -code error "wrong number of arguments, should be $name parse xml ?options?"
	    }
	    eval $classinfo(-parsecommand) [list $name] $args
	}

	reset {
	    eval $classinfo(-resetcommand) [list $name]
	}

	default {
	    return -code error "unknown method"
	}
    }

    return {}
}

# xml::noop --
#
#	Do nothing utility proc
#
# Arguments:
#	args	whatever
#
# Results:
#	Nothing happens

proc xml::noop args {}
