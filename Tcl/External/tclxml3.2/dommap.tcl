# dommap.tcl --
#
#	Apply a mapping function to a DOM structure
#
# Copyright (c) 1998-2003 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: dommap.tcl,v 1.5 2003/12/09 04:56:43 balls Exp $

package provide dommap 1.0

# We need the DOM
package require dom 2.6

namespace eval dommap {
    namespace export map
}

# dommap::apply --
#
#	Apply a function to a DOM document.
#
#	The callback command is invoked with the node ID of the
#	matching DOM node as its argument.  The command may return
#	an error, continue or break code to alter the processing
#	of further nodes.
#
#	Filter functions may be applied to match particular
#	nodes.  Valid functions include:
#
#	-nodeType regexp
#	-nodeName regexp
#	-nodeValue regexp
#	-attribute {regexp regexp}
#
#	If a filter is specified then the node must match for the
#	callback command to be invoked.  If a filter is not specified
#	then all nodes match that filter.
#
# Arguments:
#	node	DOM document node
#	cmd	callback command
#	args	configuration options
#
# Results:
#	Depends on callback command

proc dommap::apply {node cmd args} {
    array set opts $args

    # Does this node match?
    set match 1
    catch {set match [expr $match && [regexp $opts(-nodeType) [::dom::node cget $node -nodeType]]]}
    catch {set match [expr $match && [regexp $opts(-nodeName) [::dom::node cget $node -nodeName]]]}
    catch {set match [expr $match && [regexp $opts(-nodeValue) [::dom::node cget $node -nodeValue]]]}
    if {$match && ![string compare [::dom::node cget $node -nodeType] element]} {
	set match 0
	foreach {attrName attrValue} [array get [::dom::node cget $node -attributes]] {
	    set match 1
	    catch {set match [expr $match && [regexp [lindex $opts(-attribute) 0] $attrName]]}
	    catch {set match [expr $match && [regexp [lindex $opts(-attribute) 1] $attrValue]]}
	    if {$match} break
	}
    }
    if {$match && [set code [catch {eval $cmd [list $node]} msg]]} {
	switch $code {
	    0 {}
	    3 {
		return -code break
	    }
	    4 {
		return -code continue
	    }
	    default {
		return -code error $msg
	    }
	}
    }

    # Process children
    foreach child [::dom::node children $node] {
	switch [catch {eval apply [list $child] [list $cmd] $args} msg] {
	    0 {
		# No action required
	    }
	    3 {
		# break
		return -code break
	    }
	    4 {
		# continue - skip processing of siblings
		return
	    }
	    1 -
	    2 -
	    default {
		# propagate the error message
		return -code error $msg
	    }
	}
    }

    return {}
}

