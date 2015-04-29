# xmlswitch.tcl --
#
#	This file implements a control structure for Tcl.
#	'xmlswitch' iterates over an XML document.  Features in
#	the document may be specified using XPath location paths,
#	and these will trigger Tcl scripts when matched.
#
# Copyright (c) 2008 Explain
# http://www.explain.com.au/
# Copyright (c) 2000-2003 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: xmlswitch.tcl,v 1.6 2003/12/09 04:56:43 balls Exp $

package provide xmlswitch 3.2

# We need the xml, dom and xpath packages

package require xml 3.2
package require dom 3.2
package require xpath 1.0

namespace eval xmlswitch {
    namespace export xmlswitch xmlswitchcont xmlswitchend
    namespace export domswitch
    namespace export free rootnode

    variable counter 0

    variable typemap
    array set typemap {
	text textNode
	comment comment
	processing-instruction processingInstruction
    }
}

# xmlswitch::xmlswitch --
#
#	Parse XML data, matching for XPath locations along the way
#	and (possibly) triggering callbacks.
#
#	A DOM tree is built as a side-effect (necessary for resolving
#	XPath location paths).
#
# Arguments:
#	xml	XML document
#	args	configuration options,
#		plus a single path/script expression, or multiple expressions
#
# Results:
#	Tcl callbacks may be invoked.
#	If -async option is true returns a token for this "process".

proc xmlswitch::xmlswitch {xml args} {
    variable counter

    set stateVarName [namespace current]::State[incr counter]
    upvar #0 $stateVarName state
    set state(stateVarName) $stateVarName
    set state(-async) 0

    set state(pathArray) ${stateVarName}Paths
    upvar #0 $state(pathArray) paths
    array set paths {}

    set cleanup {
	unset state
	unset paths
    }

    # Find configuration options and remove
    set numOpts 0
    foreach {opt value} $args {
	switch -glob -- $opt {
	    -* {
		set state($opt) $value
		incr numOpts 2
	    }
	    default {
		set args [lrange $args $numOpts end]
		break
	    }
	}
    }

    switch -- [llength $args] {
	0 {
	    # Nothing to do
	    eval $cleanup
	    return $stateVarName
	}
	1 {
	    foreach {path script} [lindex $args 0] {
		set paths([xpath::split $path]) $script
	    }
	}
	default {
	    if {[llength $args] % 2} {
		eval $cleanup
		return -code error "no script matching location path \"[lindex $args end]\""
	    }
	    foreach {path script} $args {
		set paths([xpath::split $path]) $script
	    }
	}
    }

    set root [set state(root) [dom::DOMImplementation create]]
    set state(current) $root

    # Parse the document
    # We're going to do this incrementally, so the caller can
    # break at any time
    set state(parser) [eval xml::parser [array get state -parser]]
    #append cleanup "\n $parser destroy\n"
    $state(parser) configure \
	    -elementstartcommand [namespace code [list ParseElementStart $stateVarName]]	\
	    -elementendcommand [namespace code [list ParseElementEnd $stateVarName]]		\
	    -characterdatacommand [namespace code [list ParseCharacterData $stateVarName]]	\
	    -final 0

#	    -processinginstructioncommand [namespace code [list ParsePI $stateVarName]]		\
#	    -commentcommand [namespace code [list ParseComment]]

    if {[catch {$state(parser) parse $xml} err]} {
	eval $cleanup
	return -code error $err
    }

    if {$state(-async)} {
	return $stateVarName
    } else {
	eval $cleanup
	return {}
    }
}

# xmlswitch::xmlswitchcont --
#
#	Provide more XML data to parse
#
# Arguments:
#	token	state variable name
#	xml	XML data
#
# Results:
#	More parsing

proc xmlswitch::xmlswitchcont {token xml} {
    upvar #0 $token state

    $state(parser) parse $xml

    return {}
}

# xmlswitch::xmlswitchend --
#
#	Signal that no further data is available
#
# Arguments:
#	token	state array
#
# Results:
#	Parser configuration changed

proc xmlswitch::xmlswitchend token {
    upvar #0 $token state

    $state(parser) configure -final true

    return {}
}

# xmlswitch::rootnode --
#
#	Get the root node
#
# Arguments:
#	token	state array
#
# Results:
#	Returns root node token

proc xmlswitch::rootnode token {
    upvar #0 $token state

    return $state(root)
}

# xmlswitch::free --
#
#	Free resources EXCEPT the DOM tree.
#	"-all" causes DOM tree to be destroyed too.
#
# Arguments:
#	token	state array
#	args	options
#
# Results:
#	Resources freed.

proc xmlswitch::free {token args} {
    upvar #0 $token state

    if {[lsearch $args "-all"] >= 0} {
	dom::DOMImplementation destroy $state(root)
    }

    catch {unset $state(pathArray)}
    catch {unset state}

    catch {$state(parser) free}

    return {}
}

# xmlswitch::ParseElementStart --
#
#	Handle element start tag
#
# Arguments:
#	token	state array
#	name	element type
#	attrList attribute list
#	args	options
# Results:
#	All XPath location paths are checked for a match,
#	and script evaluated for matching XPath.
#	DOM tree node added.

proc xmlswitch::ParseElementStart:dbgdisabled {token name attrList args} {
    if {[catch {eval ParseElementStart:dbg [list $token $name $attrList] $args} msg]} {
	puts stderr [list ParseElementStart failed with msg $msg]
	puts stderr $::errorInfo
	return -code error $msg
    } else {
	puts stderr [list ParseElementStart returned OK]
    }
    return $msg
}
proc xmlswitch::ParseElementStart {token name attrList args} {

    upvar #0 $token state
    array set opts $args

    #puts stderr [list xmlswitch::ParseElementStart $token $name $attrList $args]

    lappend state(current) \
	    [dom::document createElement [lindex $state(current) end] $name]
    foreach {name value} $attrList {
	dom::element setAttribute [lindex $state(current) end] $name $value
    }

    MatchTemplates $token [lindex $state(current) end]

    return {}
}

# xmlswitch::ParseElementEnd --
#
#	Handle element end tag
#
# Arguments:
#	token	state array
#	name	element type
#	args	options
# Results:
#	State changed

proc xmlswitch::ParseElementEnd {token name args} {
    upvar #0 $token state

    set state(current) [lreplace $state(current) end end]

    return {}
}

# xmlswitch::ParseCharacterData --
#
#	Handle character data
#
# Arguments:
#	token	state array
#	data	pcdata
#
# Results:
#	All XPath location paths are checked for a match,
#	and script evaluated for matching XPath.
#	DOM tree node added.

proc xmlswitch::ParseCharacterData {token data} {
    upvar #0 $token state

    lappend state(current) \
	    [dom::document createTextNode [lindex $state(current) end] $data]

    MatchTemplates $token [lindex $state(current) end]

    set state(current) [lreplace $state(current) end end]

    return {}
}

# xmlswitch::domswitch --
#
#	Similar to xmlswitch above, but iterates over a pre-built
#	DOM tree.
#
# Arguments:
#	xml	XML document
#	args	a single path/script expression, or multiple expressions
#
# Results:
#	Tcl callbacks may be invoked.

proc xmlswitch::domswitch {xml args} {
}

# xmlswitch::MatchTemplates --
#
#	Check all templates for one which matches
#	the current node.
#
# Arguments:
#	token	state array
#	node	Current DOM node
#
# Results:
#	If a template matches, its script is evaluated

proc xmlswitch::MatchTemplates {token node} {
    upvar #0 $token state
    upvar #0 $state(pathArray) paths

    #puts stderr [list xmlswitch::MatchTemplates $token $node (type: [dom::node cget $node -nodeType]) (name: [dom::node cget $node -nodeName])]

    set matches {}

    foreach {path script} [array get paths] {

	#puts stderr [list checking path $path for a match]

	set context $node

	# Work backwards along the path, reversing each axis
	set match 0
	set i [llength $path]
	#puts stderr [list $i steps to be tested]
	while {[incr i -1] >= 0} {
	    #puts stderr [list step $i [lindex $path $i]]
	    switch -glob [llength [lindex $path $i]],$i {
		0,0 {
		    #puts stderr [list absolute path, end of steps - am I at the root?]
		    if {![string length [dom::node parent $context]]} {
			#puts stderr [list absolute path matched]
			lappend matches [list $path $script]
		    } else {
			#puts stderr [list absolute path did not match]
		    }
		}
		*,0 {
		    #puts stderr [list last step, relative path]
		    switch [lindex [lindex $path $i] 0] {
			child {
			    if {[NodeTest [lindex $path $i] $context] && \
				    [CheckPredicates [lindex $path $i] $context]} {
				#puts stderr [list relative path matched]
				lappend matches [list $path $script]
			    } else {
				#puts stderr [list relative path did not match]
			    }
			}
			default {
			    return -code error "axis \"[lindex [lindex $path $i] 0]\" not supported"
			}
		    }
		}
		default {
		    #puts stderr [list continuing checking steps]
		    switch [lindex [lindex $path $i] 0] {
			child {
			    if {[NodeTest [lindex $path $i] $context] && \
				    [CheckPredicates [lindex $path $i] $context]} {
				set context [dom::node parent $context]
			    } else {
				#puts stderr [list no match]
			    }
			}
			default {
			    return -code error "axis \"[lindex [lindex $path $i] 0]\" not supported"
			}
		    }
		}
	    }
	}
    }

    # TODO: If there are multiple matches then we must pick the
    # most specific match

    if {[llength $matches] > 1} {
	# For the moment we'll just take the first match
	set matches [list [lindex $matches 0]]
    }

    if {[llength $matches]} {
	#puts stderr [list evaluating callback at level [info level]]
	uplevel 3 [lindex [lindex $matches 0] 1]
    }

    return {}
}

# xmlswitch::NodeTest --
#
#	Check that the node passes the node (type) test
#
# Arguments:
#	step	Location step
#	node	DOM node
#
# Results:
#	Boolean

proc xmlswitch::NodeTest {step node} {

    if {[llength [lindex $step 1]] > 1} {
	switch -glob -- [lindex [lindex $step 1] 0],[dom::node cget $node -nodeType] {
	    node,* -
	    text,textNode -
	    comment,comment -
	    processing-instruction,processingInstruction {
		return 1
	    }
	    default {
		return 0
	    }
	}
    } elseif {![string compare [lindex $step 1] "*"]} {
	return 1
    } elseif {![string compare [lindex $step 1] [dom::node cget $node -nodeName]]} {
	return 1
    } else {
	return 0
    }
}

# xmlswitch::CheckPredicates --
#
#	Check that the node passes the predicates
#
# Arguments:
#	step	Location step
#	node	DOM node
#
# Results:
#	Boolean

proc xmlswitch::CheckPredicates {step node} {
    variable typemap

    set predicates [lindex $step 2]
    # Shortcut: no predicates means everything passes
    if {![llength $predicates]} {
	return 1
    }

    # Get the context node set
    switch [lindex $step 0] {
	child {
	    set nodeset {}
	    if {[llength [lindex $step 1]]} {
		foreach {name typetest} [lindex $step 1] break
		switch -- $name {
		    node {
			set nodeset [dom::node children [dom::node parent $node]]
		    }
		    text -
		    comment -
		    processing-instruction {
			foreach child [dom::node children [dom::node parent $node]] {
			    if {![string compare [dom::node cget $child -nodeType] $typemap($name)]} {
				lappend nodeset $child
			    }
			}
		    }
		    default {
			# Error
		    }
		}
	    } else {
		foreach child [dom::node children [dom::node parent $node]] {
		    if {![string compare [lindex $step 1] [dom::node cget $child -nodeName]]} {
			lappend nodeset $child
		    }
		}
	    }
	}
	default {
	    return -code error "axis \"[lindex $step 0]\" not supported"
	}
    }

    foreach predicate $predicates {
	# position() is the only supported predicate
	if {[lsearch $nodeset $node] + 1 == $predicate} {
	    # continue
	} else {
	    return 0
	}
    }

    return 1
}

