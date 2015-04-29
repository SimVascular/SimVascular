# xmldep.tcl --
#
#	Find the dependencies in an XML document.
#	Supports external entities and XSL include/import.
#
# TODO:
#	XInclude
#
# Copyright (c) 2001-2003 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: xmldep.tcl,v 1.3 2003/12/09 04:43:15 balls Exp $

package require xml

package provide xml::dep 1.0

namespace eval xml::dep {
    namespace export depend

    variable extEntities
    array set extEntities {}

    variable XSLTNS http://www.w3.org/1999/XSL/Transform
}

# xml::dep::depend --
#
#	Find the resources which an XML document
#	depends on.  The document is parsed
#	sequentially, rather than using DOM, for efficiency.
#
# TODO:
#	Asynchronous parsing.
#
# Arguments:
#	xml	XML document entity
#	args	configuration options
#
# Results:
#	Returns list of resource (system) identifiers

proc xml::dep::depend {xml args} {
    variable resources
    variable entities

    set resources {}
    catch {unset entities}
    array set entities {}

    set p [xml::parser \
	    -elementstartcommand [namespace code ElStart]	\
	    -doctypecommand [namespace code DocTypeDecl]	\
	    -entitydeclcommand [namespace code EntityDecl]	\
	    -entityreferencecommand [namespace code EntityReference]	\
	    -validate 1	\
	    ]
    if {[llength $args]} {
	eval [list $p] configure $args
    }
    $p parse $xml

    return $resources
}

# xml::dep::ElStart --
#
#	Process start element
#
# Arguments:
#	name	tag name
#	atlist	attribute list
#	args	options
#
# Results:
#	May add to resources list

proc xml::dep::ElStart {name atlist args} {
    variable XSLTNS
    variable resources

    array set opts {
	-namespace {}
    }
    array set opts $args

    switch -- $opts(-namespace) \
	    $XSLTNS {
	switch $name {
	    import -
	    include {
		array set attr {
		    href {}
		}
		array set attr $atlist

		if {[string length $attr(href)]} {
		    if {[lsearch $resources $attr(href)] < 0} {
			lappend resources $attr(href)
		    }
		}

	    }
	}
    }
}

# xml::dep::DocTypeDecl --
#
#	Process Document Type Declaration
#
# Arguments:
#	name	Document element
#	pubid	Public identifier
#	sysid	System identifier
#	dtd	Internal DTD Subset
#
# Results:
#	Resource added to list

proc xml::dep::DocTypeDecl {name pubid sysid dtd} {
    variable resources

    puts stderr [list DocTypeDecl $name $pubid $sysid dtd]

    if {[string length $sysid] && \
	    [lsearch $resources $sysid] < 0} {
	lappend resources $sysid
    }

    return {}
}

# xml::dep::EntityDecl --
#
#	Process entity declaration, looking for external entity
#
# Arguments:
#	name	entity name
#	sysid	system identifier
#	pubid	public identifier or repl. text
#
# Results:
#	Store external entity info for later reference

proc xml::dep::EntityDecl {name sysid pubid} {
    variable extEntities

    puts stderr [list EntityDecl $name $sysid $pubid]

    set extEntities($name) $sysid
}

# xml::dep::EntityReference --
#
#	Process entity reference
#
# Arguments:
#	name	entity name
#
# Results:
#	May add to resources list

proc xml::dep::EntityReference name {
    variable extEntities
    variable resources

    puts stderr [list EntityReference $name]

    if {[info exists extEntities($name)] && \
	[lsearch $resources $extEntities($name)] < 0} {
	lappend resources $extEntities($name)
    }

}

