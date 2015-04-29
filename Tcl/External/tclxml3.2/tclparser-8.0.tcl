# tclparser-8.0.tcl --
#
#	This file provides a Tcl implementation of a XML parser.
#	This file supports Tcl 8.0.
#
#	See xml-8.[01].tcl for definitions of character sets and
#	regular expressions.
#
# Copyright (c) 2005-2008 by Explain.
# http://www.explain.com.au/
# Copyright (c) 1998-2004 Zveno Pty Ltd
# http://www.zveno.com/
# 
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: tclparser-8.0.tcl,v 1.10.2.1 2005/12/28 06:49:51 balls Exp $

package require -exact Tcl 8.0

package require xmldefs 3.2

package require sgmlparser 1.0

package provide xml::tclparser 3.2

namespace eval xml {

    # Procedures for parsing XML documents
    namespace export parser
    # Procedures for parsing XML DTDs
    namespace export DTDparser

    # Counter for creating unique parser objects
    variable ParserCounter 0

}

# xml::parser --
#
#	Creates XML parser object.
#
# Arguments:
#	args	Unique name for parser object
#		plus option/value pairs
#
# Recognised Options:
#	-final			Indicates end of document data
#	-elementstartcommand	Called when an element starts
#	-elementendcommand	Called when an element ends
#	-characterdatacommand	Called when character data occurs
#	-processinginstructioncommand	Called when a PI occurs
#	-externalentityrefcommand	Called for an external entity reference
#
#	(Not compatible with expat)
#	-xmldeclcommand		Called when the XML declaration occurs
#	-doctypecommand		Called when the document type declaration occurs
#
#	-errorcommand		Script to evaluate for a fatal error
#	-warningcommand		Script to evaluate for a reportable warning
#	-statevariable		global state variable
#	-reportempty		whether to provide empty element indication
#
# Results:
#	The state variable is initialised.

proc xml::parser {args} {
    variable ParserCounter

    if {[llength $args] > 0} {
	set name [lindex $args 0]
	set args [lreplace $args 0 0]
    } else {
	set name parser[incr ParserCounter]
    }

    if {[info command [namespace current]::$name] != {}} {
	return -code error "unable to create parser object \"[namespace current]::$name\" command"
    }

    # Initialise state variable and object command
    upvar \#0 [namespace current]::$name parser
    set sgml_ns [namespace parent]::sgml
    array set parser [list name $name			\
	-final 1					\
	-elementstartcommand ${sgml_ns}::noop		\
	-elementendcommand ${sgml_ns}::noop		\
	-characterdatacommand ${sgml_ns}::noop		\
	-processinginstructioncommand ${sgml_ns}::noop	\
	-externalentityrefcommand ${sgml_ns}::noop	\
	-xmldeclcommand ${sgml_ns}::noop		\
	-doctypecommand ${sgml_ns}::noop		\
	-warningcommand ${sgml_ns}::noop		\
	-statevariable [namespace current]::$name	\
	-reportempty 0					\
	internaldtd {}					\
    ]

    proc [namespace current]::$name {method args} \
	"eval ParseCommand $name \$method \$args"

    eval ParseCommand [list $name] configure $args

    return [namespace current]::$name
}

# xml::ParseCommand --
#
#	Handles parse object command invocations
#
# Valid Methods:
#	cget
#	configure
#	parse
#	reset
#
# Arguments:
#	parser	parser object
#	method	minor command
#	args	other arguments
#
# Results:
#	Depends on method

proc xml::ParseCommand {parser method args} {
    upvar \#0 [namespace current]::$parser state

    switch -- $method {
	cget {
	    return $state([lindex $args 0])
	}
	configure {
	    foreach {opt value} $args {
		set state($opt) $value
	    }
	}
	parse {
	    ParseCommand_parse $parser [lindex $args 0]
	}
	reset {
	    if {[llength $args]} {
		return -code error "too many arguments"
	    }
	    ParseCommand_reset $parser
	}
	default {
	    return -code error "unknown method \"$method\""
	}
    }

    return {}
}

# xml::ParseCommand_parse --
#
#	Parses document instance data
#
# Arguments:
#	object	parser object
#	xml	data
#
# Results:
#	Callbacks are invoked, if any are defined

proc xml::ParseCommand_parse {object xml} {
    upvar \#0 [namespace current]::$object parser
    variable Wsp
    variable tokExpr
    variable substExpr

    set parent [namespace parent]
    if {![string compare :: $parent]} {
	set parent {}
    }

    set tokenised [lrange \
	    [${parent}::sgml::tokenise $xml \
	    $tokExpr \
	    $substExpr \
	    -internaldtdvariable [namespace current]::${object}(internaldtd)] \
	4 end]

    eval ${parent}::sgml::parseEvent \
	[list $tokenised \
	    -emptyelement [namespace code ParseEmpty] \
	    -parseattributelistcommand [namespace code ParseAttrs]] \
	[array get parser -*command] \
	[array get parser -entityvariable] \
	[array get parser -reportempty] \
	[array get parser -final] \
	-normalize 0 \
	-internaldtd [list $parser(internaldtd)]

    return {}
}

# xml::ParseEmpty --  Tcl 8.0 version
#
#       Used by parser to determine whether an element is empty.
#       This should be dead easy in XML.  The only complication is
#       that the RE above can't catch the trailing slash, so we have
#       to dig it out of the tag name or attribute list.
#
#       Tcl 8.1 REs should fix this.
#
# Arguments:
#       tag     element name
#       attr    attribute list (raw)
#       e       End tag delimiter.
#
# Results:
#       "/" if the trailing slash is found.  Optionally, return a list
#       containing new values for the tag name and/or attribute list.

proc xml::ParseEmpty {tag attr e} {

    if {[string match */ [string trimright $tag]] && \
            ![string length $attr]} {
        regsub {/$} $tag {} tag
        return [list / $tag $attr]
    } elseif {[string match */ [string trimright $attr]]} {
        regsub {/$} [string trimright $attr] {} attr
        return [list / $tag $attr]
    } else {
        return {}
    }

}

# xml::ParseAttrs --
#
#	Parse element attributes.
#
# There are two forms for name-value pairs:
#
#	name="value"
#	name='value'
#
# Watch out for the trailing slash on empty elements.
#
# Arguments:
#	attrs	attribute string given in a tag
#
# Results:
#	Returns a Tcl list representing the name-value pairs in the 
#	attribute string

proc xml::ParseAttrs attrs {
    variable Wsp
    variable Name

    # First check whether there's any work to do
    if {![string compare {} [string trim $attrs]]} {
	return {}
    }

    # Strip the trailing slash on empty elements
    regsub [format {/[%s]*$} " \t\n\r"] $attrs {} atList

    set mode name
    set result {}
    foreach component [split $atList =] {
	switch $mode {
	    name {
		set component [string trim $component]
		if {[regexp $Name $component]} {
		    lappend result $component
		} else {
		    return -code error "invalid attribute name \"$component\""
		}
		set mode value:start
	    }
	    value:start {
		set component [string trimleft $component]
		set delimiter [string index $component 0]
		set value {}
		switch -- $delimiter {
		    \" -
		    ' {
			if {[regexp [format {%s([^%s]*)%s(.*)} $delimiter $delimiter $delimiter] $component discard value remainder]} {
			    lappend result $value
			    set remainder [string trim $remainder]
			    if {[string length $remainder]} {
				if {[regexp $Name $remainder]} {
				    lappend result $remainder
				    set mode value:start
				} else {
				    return -code error "invalid attribute name \"$remainder\""
				}
			    } else {
				set mode end
			    }
			} else {
			    set value [string range $component 1 end]
			    set mode value:continue
			}
		    }
		    default {
			return -code error "invalid value for attribute \"[lindex $result end]\""
		    }
		}
	    }
	    value:continue {
		if {[regexp [format {([^%s]*)%s(.*)} $delimiter $delimiter] $component discard valuepart remainder]} {
		    append value = $valuepart
		    lappend result $value
		    set remainder [string trim $remainder]
		    if {[string length $remainder]} {
			if {[regexp $Name $remainder]} {
			    lappend result $remainder
			    set mode value:start
			} else {
			    return -code error "invalid attribute name \"$remainder\""
			}
		    } else {
			set mode end
		    }
		} else {
		    append value = $component
		}
	    }
	    end {
		return -code error "unexpected data found after end of attribute list"
	    }
	}
    }

    switch $mode {
	name -
	end {
	    # This is normal
	}
	default {
	    return -code error "unexpected end of attribute list"
	}
    }

    return $result
}

# xml::ParseCommand_reset --
#
#	Initialize parser data
#
# Arguments:
#	object	parser object
#
# Results:
#	Parser data structure initialised

proc xml::ParseCommand_reset object {
    upvar \#0 [namespace current]::$object parser

    array set parser [list \
	    -final 1		\
	    internaldtd {}	\
    ]
}

