# dom.tcl --
#
#	This file implements the Tcl language binding for the DOM -
#	the Document Object Model.  Support for the core specification
#	is given here.  Layered support for specific languages, 
#	such as HTML, will be in separate modules.
#
# Copyright (c) 1998-2004 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: dom.tcl,v 1.23 2004/02/25 20:10:28 balls Exp $

# We need the xml package, so that we get Name defined

package require xml 3.0

package provide dom::tcl 3.0

# Define generic constants

namespace eval dom {
    namespace export DOMImplementation
    namespace export hasFeature createDocument create createDocumentType
    namespace export createNode destroy isNode parse selectNode serialize
    namespace export trim

    namespace export document documentFragment node
    namespace export element textNode attribute
    namespace export processingInstruction
    namespace export documenttype
    namespace export event

    variable maxSpecials
    if {![info exists maxSpecials]} {
	set maxSpecials 10
    }

    variable strictDOM 0

    # Default -indentspec value
    #	spaces-per-indent-level {collapse-re collapse-value}
    variable indentspec [list 2 [list {        } \t]]

    # The Namespace URI for XML Namespace declarations
    variable xmlnsURI http://www.w3.org/2000/xmlns/

    # DOM Level 2 Event defaults
    variable bubbles
    array set bubbles {
	DOMFocusIn 1
	DOMFocusOut 1
	DOMActivate 1
	click 1
	mousedown 1
	mouseup 1
	mouseover 1
	mousemove 1
	mouseout 1
	DOMSubtreeModified 1
	DOMNodeInserted 1
	DOMNodeRemoved 1
	DOMNodeInsertedIntoDocument 0
	DOMNodeRemovedFromDocument 0
	DOMAttrModified 1
	DOMAttrRemoved 1
	DOMCharacterDataModified 1
    }
    variable cancelable
    array set cancelable {
	DOMFocusIn 0
	DOMFocusOut 0
	DOMActivate 1
	click 1
	mousedown 1
	mouseup 1
	mouseover 1
	mousemove 0
	mouseout 1
	DOMSubtreeModified 0
	DOMNodeInserted 0
	DOMNodeRemoved 0
	DOMNodeInsertedIntoDocument 0
	DOMNodeRemovedFromDocument 0
	DOMAttrModified 0
	DOMAttrRemoved 0
	DOMCharacterDataModified 0
    }
}

namespace eval dom::tcl {
    namespace export DOMImplementation
    namespace export hasFeature createDocument create createDocumentType
    namespace export createNode destroy isNode parse selectNode serialize
    namespace export trim

    namespace export document documentFragment node
    namespace export element textNode attribute
    namespace export processingInstruction
    namespace export event
}

foreach p {DOMImplementation hasFeature createDocument create createDocumentType createNode destroy isNode parse selectNode serialize trim document documentFragment node element textNode attribute processingInstruction event documenttype} {

    proc dom::$p args "return \[eval tcl::$p \$args\]"

}

# Data structures
#
# Documents are stored in a Tcl namespace within the ::dom namespace.
# The Document array variable stores data for the document itself.
# Each node has an array variable for its data.
#
# "Live" data objects are stored as a separate Tcl variable.
# Lists, such as child node lists, are Tcl list variables (ie scalar)
# and keyed-value lists, such as attribute lists, are Tcl array
# variables.  The accessor function returns the variable name,
# which the application should treat as a read-only object.
#
# A token is a FQ Tcl variable name.

# dom::tcl::DOMImplementation --
#
#	Implementation-dependent functions.
#	Most importantly, this command provides a function to
#	create a document instance.
#
# Arguments:
#	method	method to invoke
#	token	token for node
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable DOMImplementationOptions {}
    variable DOMImplementationCounter
    if {![info exists DOMImplementationCounter]} {
	set DOMImplementationCounter 0
    }
}

proc dom::tcl::DOMImplementation {method args} {
    variable DOMImplementationOptions
    variable DOMImplementationCounter

    switch -- $method {

	hasFeature {

	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be dom::DOMImplementation method args..."
	    }

	    # Later on, could use Tcl package facility
	    if {[regexp {create|destroy|parse|query|serialize|trim|Events|UIEvents|isNode} [lindex $args 0]]} {
		if {![string compare [lindex $args 1] "1.0"]} {
		    return 1
		} else {
		    return 0
		}
	    } else {
		return 0
	    }

	}

	createDocument {
	    # createDocument introduced in DOM Level 2

	    if {[llength $args] != 3} {
		return -code error "wrong # args: should be DOMImplementation nsURI name doctype"
	    }

	    set doc [DOMImplementation create]

	    if {[string length [lindex $args 2]]} {
		array set $doc [list document:doctype [lindex $args 2]]
	    }

	    document createElementNS $doc [lindex $args 0] [lindex $args 1]

	    return $doc
	}

	create {

	    # Non-standard method (see createDocument)
	    # Bootstrap a document instance

	    if {[llength $args] > 0} {
		return -code error "wrong # args: should be DOMImplementation create"
	    }

	    # Allocate unique document array name
	    set ns [namespace current]::document[incr DOMImplementationCounter]
	    set name ${ns}::Document

	    # Create the Tcl namespace for this document
	    namespace eval $ns {
		namespace export Document
	    }

	    set varPrefix ${name}var
	    set arrayPrefix ${name}arr

	    array set $name [list counter 1 \
		node:nodeType document			\
		node:parentNode {}			\
		node:nodeName #document			\
		node:nodeValue {}			\
		node:childNodes ${varPrefix}1		\
		documentFragment:masterDoc $name	\
		document:implementation [namespace current]::DOMImplementation		\
		document:xmldecl {version 1.0}		\
		document:documentElement {}		\
		document:doctype {}			\
		]

	    # Initialise child node list
	    set $varPrefix {}

	    # Create a Tcl command for the document
	    proc $name {method args} "return \[eval [namespace current]::document \[list \$method\] $name \$args\]"

	    # Capture destruction of the document
	    trace add command $name delete [namespace code [list Document:Delete $name]]

	    # Return the new toplevel node
	    return $name
	}

	createDocumentType {
	    # Introduced in DOM Level 2

	    # Patch from c.l.t., Richard Calmbach (rc@hnc.com )

	    if {[llength $args] < 3 || [llength $args] > 4} {
		return -code error "wrong # args: should be: DOMImplementation createDocumentType qname publicid systemid ?internaldtd?"
	    }

	    return [eval CreateDocType $args]
	}

	createNode {
	    # Non-standard method
	    # Creates node(s) in the given document given an XPath expression

	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be dom::DOMImplementation createNode xpath"
	    }

	    package require xpath

	    return [XPath:CreateNode [lindex $args 0] [lindex $args 1]]
	}

	destroy {

	    # Free all memory associated with a node

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be dom::DOMImplementation destroy token"
	    }

	    if {[catch {upvar #0 [lindex $args 0] node}]} {
		# If the document is being destroyed then the Tcl namespace no longer exists
		return {}
	    }

	    switch $node(node:nodeType) {

		document -
		documentFragment {

		    if {[string length $node(node:parentNode)]} {
			unset $node(node:childNodes)

			# Dispatch events
			event postMutationEvent $node(node:parentNode) DOMSubtreeModified

			return {}
		    }

		    # else this is the root document node,
		    # and we can optimize the cleanup.
		    # No need to dispatch events.

		    # First remove all command traces
		    foreach nodecmd [info commands [namespace qualifiers [lindex $args 0]]::*] {
			trace remove command $nodecmd delete [namespace code [list Node:Delete $nodecmd]]
		    }

		    namespace delete [namespace qualifiers [lindex $args 0]]
		}

		documentType {
		    trace remove command [lindex $args 0] delete [namespace code [list DocumentType:Delete [lindex $args 0]]]
		    rename [lindex $args 0] {}
		    unset [lindex $args 0]
		}

		element {
		    # First make sure the node is removed from the tree
		    if {[string length $node(node:parentNode)]} {
			node removeChild $node(node:parentNode) [lindex $args 0]
		    }
		    unset $node(node:childNodes)
		    unset $node(element:attributeList)
		    unset node
		    set name [lindex $args 0]
		    trace remove command $name delete [namespace code [list Node:Delete $name]]
		    rename $name {}

		    # Don't dispatch events here -
		    # already done by removeChild
		}

		event {
		    set name [lindex $args 0]
		    trace remove command $name delete [namespace code [list Node:Delete $name]]
		    rename $name {}
		    unset node
		}

		default {
		    # Store the parent for later
		    set parent $node(node:parentNode)

		    # First make sure the node is removed from the tree
		    if {[string length $node(node:parentNode)]} {
			node removeChild $node(node:parentNode) [lindex $args 0]
		    }
		    unset node
		    set name [lindex $args 0]
		    trace remove command $name delete [namespace code [list Node:Delete $name]]
		    rename $name {}

		    # Dispatch events
		    event postMutationEvent $parent DOMSubtreeModified

		}

	    }

	    return {}

	}

	isNode {
	    # isNode - non-standard method
	    # Sometimes it is useful to check if an arbitrary string
	    # refers to a DOM node

	    upvar #0 [lindex $args 0] node

	    if {![info exists node]} {
		return 0
	    } elseif {[info exists node(node:nodeType)]} {
		return 1
	    } else {
		return 0
	    }
	}

	parse {

	    # This implementation uses TclXML version 2.0.
	    # TclXML can choose the best installed parser.

	    if {[llength $args] < 1} {
		return -code error "wrong # args: should be dom::DOMImplementation parse xml ?args...?"
	    }

	    array set opts {-parser {} -progresscommand {} -chunksize 8196}
	    if {[catch {array set opts [lrange $args 1 end]}]} {
		return -code error "bad configuration options"
	    }

	    # Create a state array for this parse session
	    set state [namespace current]::parse[incr DOMImplementationCounter]
	    array set $state [array get opts -*]
	    array set $state [list progCounter 0]
	    set errorCleanup {}

	    if {[string length $opts(-parser)]} {
		set parserOpt [list -parser $opts(-parser)]
	    } else {
		set parserOpt {}
	    }
	    if {[catch {package require xml} version]} {
		eval $errorCleanup
		return -code error "unable to load XML parsing package"
	    }
	    set parser [eval xml::parser $parserOpt]

	    $parser configure \
		-elementstartcommand [namespace code [list ParseElementStart $state]]	\
		-elementendcommand [namespace code [list ParseElementEnd $state]]	\
		-characterdatacommand [namespace code [list ParseCharacterData $state]] \
		-processinginstructioncommand [namespace code [list ParseProcessingInstruction $state]] \
		-commentcommand [namespace code [list ParseComment $state]] \
		-entityreferencecommand [namespace code [list ParseEntityReference $state]] \
		-xmldeclcommand [namespace code [list ParseXMLDeclaration $state]] \
		-doctypecommand [namespace code [list ParseDocType $state]] \
		-final 1

	    # Create top-level document
	    array set $state [list docNode [DOMImplementation create]]
	    array set $state [list current [lindex [array get $state docNode] 1]]

	    # Parse data
	    # Bug in TclExpat - doesn't handle non-final inputs
	    if {0 && [string length $opts(-progresscommand)]} {
		$parser configure -final false
		while {[string length [lindex $args 0]]} {
		    $parser parse [string range [lindex $args 0] 0 $opts(-chunksize)]
		    set args [lreplace $args 0 0 \
			[string range [lindex $args 0] $opts(-chunksize) end]]
		    uplevel #0 $opts(-progresscommand)
		}
		$parser configure -final true
	    } elseif {[catch {$parser parse [lindex $args 0]} err]} {
		catch {rename $parser {}}
		catch {unset $state}
		return -code error $err
	    }

	    # Free data structures which are no longer required
	    $parser free
	    catch {rename $parser {}}

	    set doc [lindex [array get $state docNode] 1]
	    unset $state
	    return $doc

	}

	selectNode {
	    # Non-standard method
	    # Returns nodeset in the given document matching an XPath expression

	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be dom::DOMImplementation selectNode token xpath"
	    }

	    package require xpath

	    return [XPath:SelectNode [lindex $args 0] [lindex $args 1]]
	}

	serialize {

	    if {[llength $args] < 1} {
		return -code error "wrong # args: should be dom::DOMImplementation serialize token"
	    }

	    upvar #0 [lindex $args 0] node

	    return [eval [list Serialize:$node(node:nodeType)] $args]

	}

	trim {

	    # Removes textNodes that only contain white space

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be dom::DOMImplementation trim token"
	    }

	    Trim [lindex $args 0]

	    # Dispatch DOMSubtreeModified event once here?

	    return {}

	}

	default {
	    return -code error "unknown method \"$method\""
	}

    }

    return {}
}

namespace eval dom::tcl {
    foreach method {hasFeature createDocument create createDocumentType createNode destroy isNode parse selectNode serialize trim} {
	proc $method args "eval [namespace current]::DOMImplementation $method \$args"
    }
}

# dom::tcl::Document:Delete --
#
#	Handle destruction of a document
#
# Arguments:
#	name	document token
#	old	)
#	new	) args added by trace command
#	op	)

proc dom::tcl::Document:Delete {name old new op} {
    DOMImplementation destroy $name
    return {}
}

# dom::tcl::document --
#
#	Functions for a document node.
#
# Arguments:
#	method	method to invoke
#	token	token for node
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable documentOptionsRO doctype|implementation|documentElement
    variable documentOptionsRW actualEncoding|encoding|standalone|version
}

proc dom::tcl::document {method token args} {
    variable documentOptionsRO
    variable documentOptionsRW

    upvar #0 $token node

    set result {}

    switch -- $method {
	cget {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::document method token ?args ...?\""
	    }
	    if {[regexp [format {^-(%s)$} $documentOptionsRO] [lindex $args 0] discard option]} {
		return $node(document:$option)
	    } elseif {[regexp [format {^-(%s)$} $documentOptionsRW] [lindex $args 0] discard option]} {
		switch -- $option {
		    encoding -
		    version -
		    standalone {
			array set xmldecl $node(document:xmldecl)
			return $xmldecl($option)
		    }
		    default {
			return $node(document:$option)
		    }
		}
	    } else {
		return -code error "bad option \"[lindex $args 0]\""
	    }
	}
	configure {
	    if {[llength $args] == 1} {
		return [document cget $token [lindex $args 0]]
	    } elseif {[expr [llength $args] % 2]} {
		return -code error "no value specified for option \"[lindex $args end]\""
	    } else {
		foreach {option value} $args {
		    if {[regexp [format {^-(%s)$} $documentOptionsRW] $option discard opt]} {
			switch -- $opt {
			    encoding {
				catch {unset xmldecl}
				array set xmldecl $node(document:xmldecl)
				set xmldecl(encoding) $value
				set node(document:xmldecl) [array get xmldecl]
			    }
			    standalone {
				if {[string is boolean $value]} {
				    catch {unset xmldecl}
				    array set xmldecl $node(document:xmldecl)
				    if {[string is true $value]} {
					set xmldecl(standalone) yes
				    } else {
					set xmldecl(standalone) no
				    }
				    set node(document:xmldecl) [array get xmldecl]
				} else {
				    return -code error "unsupported value for option \"$option\" - must be boolean"
				}
			    }
			    version {
				if {$value == "1.0"} {
				    catch {unset xmldecl}
				    array set xmldecl $node(document:xmldecl)
				    set xmldecl(version) $value
				    set node(document:xmldecl) [array get xmldecl]
				} else {
				    return -code error "unsupported value for option \"$option\""
				}
			    }
			    default {
				set node(document:$opt) $value
			    }
			}
		    } elseif {[regexp [format {^-(%s)$} $documentOptionsRO] $option discard opt]} {
			return -code error "attribute \"$option\" is read-only"
		    } else {
			return -code error "bad option \"$option\""
		    }
		}
	    }
	}

	createElement {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createElement token name\""
	    }

	    # Check that the element name is kosher
	    if {![regexp ^$::xml::Name\$ [lindex $args 0]]} {
		return -code error "invalid element name \"[lindex $args 0]\""
	    }

	    # Invoke internal factory function
	    set result [CreateElement $token [lindex $args 0] {}]

	}
	createDocumentFragment {
	    if {[llength $args]} {
		return -code error "wrong # args: should be \"document createDocumentFragment token\""
	    }

	    set result [CreateGeneric $token node:nodeType documentFragment node:nodeName #document-fragment node:nodeValue {}]
	}
	createTextNode {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createTextNode token text\""
	    }

	    set result [CreateTextNode $token [lindex $args 0]]
	}
	createComment {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createComment token data\""
	    }

	    set result [CreateGeneric $token node:nodeType comment node:nodeName #comment node:nodeValue [lindex $args 0]]
	}
	createCDATASection {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createCDATASection token data\""
	    }

	    set result [CreateTextNode $token [lindex $args 0]]
	    node configure $result -cdatasection 1
	}
	createProcessingInstruction {
	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be \"document createProcessingInstruction token target data\""
	    }

	    set result [CreateGeneric $token node:nodeType processingInstruction \
		    node:nodeName [lindex $args 0] node:nodeValue [lindex $args 1]]
	}
	createAttribute {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createAttributes token name\""
	    }

	    # Check that the attribute name is kosher
	    if {![regexp ^$::xml::Name\$ [lindex $args 0]]} {
		return -code error "invalid attribute name \"[lindex $args 0]\""
	    }

	    set result [CreateGeneric $token node:nodeType attribute node:nodeName [lindex $args 0]]
	}
	createEntity {
	    set result [CreateGeneric $token node:nodeType entity]
	}
	createEntityReference {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createEntityReference token name\""
	    }
	    set result [CreateGeneric $token node:nodeType entityReference node:nodeName [lindex $args 0]]
	}

	importNode {
	    # Introduced in DOM Level 2

	    if {[llength $args] < 1} {
		return -code error "wrong # args: should be \"importNode token ?-deep boolean?\""
	    }
	    array set opts {
		-deep 1
	    }
	    array set opts [lrange $args 1 end]
	    set opts(-deep) [Boolean $opts(-deep)]

	    if {[namespace qualifiers [lindex $args 0]] == [namespace qualifiers $token]} {
		return -code error "source node \"[lindex $args 0]\" is in the same document"
	    }

	    switch [node cget [lindex $args 0] -nodeType] {
		document -
		documentType {
		    return -code error "node type \"[node cget [lindex $args 0] -type]\" cannot be imported"
		}
		documentFragment {
		    set result [document createDocumentFragment $token]
		    if {$opts(-deep)} {
			foreach child [node children [lindex $args 0]] {
			    $result appendChild [$token importNode $child -deep 1]
			}
		    }
		}
		element {
		    set result [CreateElement {} [node cget [lindex $args 0] -nodeName] [array get [node cget [lindex $args 0] -attributes]] -document $token]
		    if {$opts(-deep)} {
			foreach child [node children [lindex $args 0]] {
			    $result appendChild [$token importNode $child -deep 1]
			}
		    }
		}
		textNode {
		    set result [CreateTextNode {} [node cget [lindex $args 0] -nodeValue] -document $token]
		}
		attribute -
		processingInstruction -
		comment {
		    set result [CreateGeneric {} -document $token node:nodeType [node cget [lindex $args 0] -nodeType] node:nodeName [node cget [lindex $args 0] -nodeName] node:nodeValue [node cget [lindex $args 0] -nodeValue]]
		}
	    }
	}

	createElementNS {
	    # Introduced in DOM Level 2

	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be: \"createElementNS nsuri qualname\""
	    }

	    # Check that the qualified name is kosher
	    if {[catch {foreach {prefix localname} [::xml::qnamesplit [lindex $args 1]]  break} err]} {
		return -code error "invalid qualified name \"[lindex $args 1]\" due to \"$err\""
	    }

	    # Invoke internal factory function
	    set result [CreateElement $token [lindex $args 1] {} -prefix $prefix -namespace [lindex $args 0] -localname $localname]
	}

	createAttributeNS {
	    # Introduced in DOM Level 2

	    return -code error "not yet implemented"
	}

	getElementsByTagNameNS {
	    # Introduced in DOM Level 2

	    return -code error "not yet implemented"
	}

	getElementsById {
	    # Introduced in DOM Level 2

	    return -code error "not yet implemented"
	}

	createEvent {
	    # Introduced in DOM Level 2

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"document createEvent token type\""
	    }

	    set result [CreateEvent $token [lindex $args 0]]

	}

	getElementsByTagName {
	    if {[llength $args] < 1} {
		return -code error "wrong # args: should be \"document getElementsByTagName token what\""
	    }

	    return [eval Element:GetByTagName [list $token [lindex $args 0]] \
		    [lrange $args 1 end]]
	}

	default {
	    return -code error "unknown method \"$method\""
	}

    }

    # Dispatch events

    # Node insertion events are generated here instead of the
    # internal factory procedures.  This is because the factory
    # procedures are meant to be mean-and-lean during the parsing
    # phase, and dispatching events at that time would be an
    # excessive overhead.  The factory methods here are pretty
    # heavyweight anyway.

    if {[string match create* $method] && [string compare $method "createEvent"]} {

	event postMutationEvent $result DOMNodeInserted -relatedNode $token
	event postMutationEvent $result DOMNodeInsertedIntoDocument
	event postMutationEvent $token DOMSubtreeModified

    }

    return $result
}

###	Factory methods
###
### These are lean-and-mean for fastest possible tree building

# dom::tcl::CreateElement --
#
#	Append an element to the given (parent) node (if any)
#
# Arguments:
#	token	parent node (if empty -document option is mandatory)
#	name	element name (no checking performed here)
#	aList	attribute list
#	args	configuration options
#
# Results:
#	New node created, parent optionally modified

proc dom::tcl::CreateElement {token name aList args} {
    array set opts $args

    if {[string length $token]} {
	upvar #0 $token parent
	upvar #0 [namespace qualifiers $token]::Document document
	set child [namespace qualifiers $token]::node[incr document(counter)]
    } elseif {[info exists opts(-document)]} {
	upvar #0 $opts(-document) document
	set child [namespace qualifiers $opts(-document)]::node[incr document(counter)]
    } else {
	return -code error "no parent or document specified"
    }

    upvar #0 $child new

    # Create the new node
    # NB. normally we'd use Node:create here,
    # but inline it instead for performance
    array set new [list \
	    node:parentNode $token		\
	    node:childNodes ${child}var		\
	    node:nodeType element		\
	    node:nodeName $name			\
	    node:namespaceURI {}		\
	    node:prefix {}			\
	    node:localName $name		\
	    node:nodeValue {}			\
	    element:attributeList ${child}arr	\
	    element:attributeNodes {}		\
    ]

    catch {set new(node:namespaceURI) $opts(-namespace)}
    catch {set new(node:localName) $opts(-localname)}
    catch {set new(node:prefix) $opts(-prefix)}

    # Initialise associated variables
    set ${child}var {}
    array set ${child}arr $aList
    catch {
	foreach {ns nsAttrList} $opts(-namespaceattributelists) {
	    foreach {attrName attrValue} $nsAttrList {
		array set ${child}arr [list $ns^$attrName $attrValue]
	    }
	}
    }

    # Update parent record

    # Does this element qualify as the document element?
    # If so, then has a document element already been set?

    if {[string length $token] && 
	[string equal $parent(node:nodeType) document]} {

	if {$token == $parent(documentFragment:masterDoc)} {
	    if {[info exists parent(document:documentElement)] && \
		    [string length $parent(document:documentElement)]} {
		# Do not attach to the tree
		set new(node:parentNode) {}
	    } else {

		# Check against document type decl
		if {[string length $parent(document:doctype)]} {
		    upvar #0 $parent(document:doctype) doctypedecl
		    if {[string compare $name $doctypedecl(doctype:name)]} {
			return -code error "mismatch between root element type in document type declaration \"$doctypedecl(doctype:name)\" and root element \"$name\""
		    }

		} else {
		    # Synthesize document type declaration
		    set doctype [CreateDocType $name {} {}]
		    set document(document:doctype) $doctype
		}

		set parent(document:documentElement) $child
		catch {lappend $parent(node:childNodes) $child}
	    }
	} else {
	    catch {lappend $parent(node:childNodes) $child}
	}
    } else {
	catch {lappend $parent(node:childNodes) $child}
    }

    proc $child {method args} "return \[eval [namespace current]::node \[list \$method\] $child \$args\]"
    trace add command $child delete [namespace code [list Node:Delete $child]]

    return $child
}

# dom::tcl::CreateTextNode --
#
#	Append a textNode node to the given (parent) node (if any).
#
#	This factory function can also be performed by
#	CreateGeneric, but text nodes are created so often
#	that this specific factory procedure speeds things up.
#
# Arguments:
#	token	parent node (if empty -document option is mandatory)
#	text	initial text
#	args	additional configuration options
#
# Results:
#	New node created, parent optionally modified

proc dom::tcl::CreateTextNode {token text args} {
    array set opts $args

    if {[string length $token]} {
	upvar #0 $token parent
	upvar #0 [namespace qualifiers $token]::Document document
	set child [namespace qualifiers $token]::node[incr document(counter)]
    } elseif {[info exists opts(-document)]} {
	upvar #0 $opts(-document) document
	set child [namespace qualifiers $opts(-document)]::node[incr document(counter)]
    } else {
	return -code error "no parent or document specified"
    }

    upvar #0 $child new

    # Create the new node
    # NB. normally we'd use Node:create here,
    # but inline it instead for performance

    # Text nodes never have children, so don't create a variable

    array set new [list \
	    node:parentNode $token		\
	    node:childNodes ${child}var		\
	    node:nodeType textNode		\
	    node:nodeValue $text		\
	    node:nodeName #text			\
	    node:cdatasection 0			\
    ]

    set ${child}var {}

    # Update parent record
    catch {lappend $parent(node:childNodes) $child}

    proc $child {method args} "return \[eval [namespace current]::node \[list \$method\] $child \$args\]"
    trace add command $child delete [namespace code [list Node:Delete $child]]

    return $child
}

# dom::tcl::CreateGeneric --
#
#	This is a template used for type-specific factory procedures
#
# Arguments:
#	token	parent node (if empty -document option is mandatory)
#	args	optional values
#
# Results:
#	New node created, parent modified

proc dom::tcl::CreateGeneric {token args} {
    array set opts $args

    if {[string length $token]} {
	upvar #0 $token parent
	upvar #0 [namespace qualifiers $token]::Document document
	set child [namespace qualifiers $token]::node[incr document(counter)]
    } elseif {[info exists opts(-document)]} {
	upvar #0 $opts(-document) document
	set child [namespace qualifiers $opts(-document)]::node[incr document(counter)]
    } else {
	return -code error "no parent or document specified"
    }
    upvar #0 $child new

    # Create the new node
    # NB. normally we'd use Node:create here,
    # but inline it instead for performance
    array set new [eval list [list \
	    node:parentNode $token	\
	    node:childNodes ${child}var	] \
	    $args			\
    ]
    set ${child}var {}

    switch -glob -- [string length $token],$opts(node:nodeType) {
	0,* -
	*,attribute -
	*,namespace {
	    # These type of nodes are not children of their parent
	}

	default {
	    # Update parent record
	    lappend $parent(node:childNodes) $child
	}
    }

    proc $child {method args} "return \[eval [namespace current]::node \[list \$method\] $child \$args\]"
    trace add command $child delete [namespace code [list Node:Delete $child]]

    return $child
}

### Specials

# dom::tcl::CreateDocType --
#
#	Create a Document Type Declaration node.
#
# Arguments:
#	name	root element type
#	publicid	public identifier
#	systemid	system identifier
#	internaldtd	internal DTD subset
#
# Results:
#	Returns node id of the newly created node.

proc dom::tcl::CreateDocType {name publicid systemid {internaldtd {}}} {
    if {![regexp ^$::xml::QName\$ $name]} {
	return -code error "invalid QName \"$name\""
    }

    set nodename [namespace current]::$name
    upvar #0 $nodename doctype
    if {[info exists doctype]} {
	return $nodename
    }

    if {[llength $internaldtd] == 1 && [string length [lindex $internaldtd 0]] == 0} {
	set dtd {}
    }

    array set doctype [list \
	    node:childNodes {} \
	    node:nodeType documentType \
	    node:nodeName $name \
	    node:nodeValue {} \
	    doctype:name $name \
	    doctype:entities {} \
	    doctype:notations {} \
	    doctype:publicId $publicid \
	    doctype:systemId $systemid \
	    doctype:internalSubset $internaldtd \
    ]

    proc $nodename {method args} "return \[eval [namespace current]::documenttype \[list \$method\] $nodename \$args\]"
    trace add command $nodename delete [namespace code [list DocumentType:Delete $nodename]]

    return $nodename
}

# dom::tcl::documenttype --
#
#	Functions for a document type declaration node.
#
# Arguments:
#	method	method to invoke
#	token	token for node
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable documenttypeOptionsRO name|entities|notations|publicId|systemId|internalSubset
    variable documenttypeOptionsRW {}
}

proc dom::tcl::documenttype {method token args} {
    variable documenttypeOptionsRO
    variable documenttypeOptionsRW

    upvar #0 $token node

    set result {}

    switch -- $method {
	cget {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::documenttype method token ?args ...?\""
	    }
	    if {[regexp [format {^-(%s)$} $documenttypeOptionsRO] [lindex $args 0] discard option]} {
		switch -- $option {
		    name {
			return $node(node:nodeName)
		    }
		    default {
			return $node(doctype:$option)
		    }
		}
	    } elseif {[regexp [format {^-(%s)$} $documenttypeOptionsRW] [lindex $args 0] discard option]} {
		return $node(doctype:$option)
	    } else {
		return -code error "bad option \"[lindex $args 0]\""
	    }
	}
	configure {
	    if {[llength $args] == 1} {
		return [documenttype cget $token [lindex $args 0]]
	    } elseif {[expr [llength $args] % 2]} {
		return -code error "no value specified for option \"[lindex $args end]\""
	    } else {
		foreach {option value} $args {
		    if {[regexp [format {^-(%s)$} $documenttypeOptionsRW] $option discard opt]} {
			switch -- $opt {
			    default {
				set node(doctype:$opt) $value
			    }
			}
		    } elseif {[regexp [format {^-(%s)$} $documenttypeOptionsRO] $option discard opt]} {
			return -code error "attribute \"$option\" is read-only"
		    } else {
			return -code error "bad option \"$option\""
		    }
		}
	    }
	}
    }

    return $result
}

# dom::tcl::DocumentType:Delete --
#
#	Handle node destruction
#
# Arguments:
#	name	node token
#	old	)
#	new	) arguments appended by trace command
#	op	)
#
# Results:
#	Node is destroyed

proc dom::tcl::DocumentType:Delete {name old new op} {
    DOMImplementation destroy $name
}

# dom::tcl::node --
#
#	Functions for a general node.
#
#	Implements EventTarget Interface - introduced in DOM Level 2
#
# Arguments:
#	method	method to invoke
#	token	token for node
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable nodeOptionsRO nodeType|parentNode|childNodes|firstChild|lastChild|previousSibling|nextSibling|attributes|namespaceURI|prefix|localName|ownerDocument
    variable nodeOptionsRW nodeValue|cdatasection

    # Allowing nodeName to be rw is not standard DOM.
    # A validating implementation would have to be very careful
    # in allowing this feature
    if {$::dom::strictDOM} {
	append nodeOptionsRO |nodeName
    } else {
	append nodeOptionsRW |nodeName
    }
}
# NB. cdatasection is not a standard DOM option

proc dom::tcl::node {method token args} {
    variable nodeOptionsRO
    variable nodeOptionsRW

    upvar #0 $token node

    set result {}

    switch -glob -- $method {
	cg* {
	    # cget

	    # Some read-only configuration options are computed
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::node cget token option\""
	    }
	    if {[regexp [format {^-(%s)$} $nodeOptionsRO] [lindex $args 0] discard option]} {
		switch $option {
		    nodeName {
			set result $node(node:nodeName)
			switch $node(node:nodeType) {
			    textNode {
				catch {set result [expr {$node(node:cdatasection) ? "#cdata-section" : $node(node:nodeName)}]}
			    }
			    default {
			    }
			}
		    }
		    childNodes {
			# How are we going to handle documentElement?
			set result $node(node:childNodes)
		    }
		    firstChild {
			upvar #0 $node(node:childNodes) children
			switch $node(node:nodeType) {
			    document {
				set result [lindex $children 0]
				catch {set result $node(document:documentElement)}
			    }
			    default {
				set result [lindex $children 0]
			    }
			}
		    }
		    lastChild {
			upvar #0 $node(node:childNodes) children
			switch $node(node:nodeType) {
			    document {
				set result [lindex $children end]
				catch {set result $node(document:documentElement)}
			    }
			    default {
				set result [lindex $children end]
			    }
			}
		    }
		    previousSibling {
			# BUG: must take documentElement into account
			# Find the parent node
			upvar #0 $node(node:parentNode) parent
			upvar #0 $parent(node:childNodes) children
			set idx [lsearch $children $token]
			if {$idx >= 0} {
			    set sib [lindex $children [incr idx -1]]
			    if {[llength $sib]} {
				set result $sib
			    } else {
				set result {}
			    }
			} else {
			    set result {}
			}
		    }
		    nextSibling {
			# BUG: must take documentElement into account
			# Find the parent node
			upvar #0 $node(node:parentNode) parent
			upvar #0 $parent(node:childNodes) children
			set idx [lsearch $children $token]
			if {$idx >= 0} {
			    set sib [lindex $children [incr idx]]
			    if {[llength $sib]} {
				set result $sib
			    } else {
				set result {}
			    }
			} else {
			    set result {}
			}
		    }
		    attributes {
			if {[string compare $node(node:nodeType) element]} {
			    set result {}
			} else {
			    set result $node(element:attributeList)
			}
		    }
		    ownerDocument {
			if {[string compare $node(node:parentNode) {}]} {
			    return [namespace qualifiers $token]::Document
			} else {
			    return $token
			}
		    }
		    default {
			return [GetField node(node:$option)]
		    }
		}
	    } elseif {[regexp [format {^-(%s)$} $nodeOptionsRW] [lindex $args 0] discard option]} {
		return [GetField node(node:$option)]
	    } else {
		return -code error "unknown option \"[lindex $args 0]\""
	    }
	}
	co* {
	    # configure

	    if {[llength $args] == 1} {
		return [node cget $token [lindex $args 0]]
	    } elseif {[expr [llength $args] % 2]} {
		return -code error "wrong \# args: should be \"::dom::node configure node option\""
	    } else {
		foreach {option value} $args {
		    if {[regexp [format {^-(%s)$} $nodeOptionsRW] $option discard opt]} {

			switch $opt,$node(node:nodeType) {
			    nodeValue,textNode -
			    nodeValue,processingInstruction {
				# Dispatch event
				set evid [CreateEvent $token DOMCharacterDataModified]
				event initMutationEvent $evid DOMCharacterDataModified 1 0 {} $node(node:nodeValue) $value {} {}
				set node(node:nodeValue) $value
				node dispatchEvent $token $evid
				DOMImplementation destroy $evid
			    }
			    default {
				set node(node:$opt) $value
			    }
			}

		    } elseif {[regexp [format {^-(%s)$} $nodeOptionsRO] $option discard opt]} {
			return -code error "attribute \"$option\" is read-only"
		    } else {
			return -code error "unknown option \"$option\""
		    }
		}
	    }
	}

	in* {

	    # insertBefore

	    # Previous and next sibling relationships are OK, 
	    # because they are dynamically determined

	    if {[llength $args] < 1 || [llength $args] > 2} {
		return -code error "wrong # args: should be \"dom::node insertBefore token new ?ref?\""
	    }

	    upvar #0 [lindex $args 0] newChild
	    if {[string compare [namespace qualifiers [lindex $args 0]] [namespace qualifiers $token]]} {
		return -code error "new node must be in the same document"
	    }

	    switch [llength $args] {
		1 {
		    # Append as the last node
		    if {[string length $newChild(node:parentNode)]} {
			node removeChild $newChild(node:parentNode) [lindex $args 0]
		    }
		    lappend $node(node:childNodes) [lindex $args 0]
		    set newChild(node:parentNode) $token
		}
		2 {
		    upvar #0 [lindex $args 1] refChild

		    if {[string compare [namespace qualifiers [lindex $args 1]] [namespace qualifiers [lindex $args 0]]]} {
			return -code error "nodes must be in the same document"
		    }
		    set idx [lsearch [set $node(node:childNodes)] [lindex $args 1]]
		    if {$idx < 0} {
			return -code error "no such reference child"
		    } else {

			# Remove from previous parent
			if {[string length $newChild(node:parentNode)]} {
			    node removeChild $newChild(node:parentNode) [lindex $args 0]
			}

			# Insert into new node
			set $node(node:childNodes) \
				[linsert [set $node(node:childNodes)] $idx [lindex $args 0]]
			set newChild(node:parentNode) $token
		    }
		}
	    }

	    event postMutationEvent [lindex $args 0] DOMNodeInserted -relatedNode $token
	    FireNodeInsertedEvents [lindex $args 0]
	    event postMutationEvent $token DOMSubtreeModified

	    set result [lindex $args 0]

	}

	rep* {

	    # replaceChild

	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be \"dom::node replaceChild token new old\""
	    }

	    upvar #0 [lindex $args 0] newChild
	    upvar #0 [lindex $args 1] oldChild
	    upvar #0 $node(node:childNodes) children

	    # Find where to insert new child
	    set idx [lsearch $children [lindex $args 1]]
	    if {$idx < 0} {
		return -code error "no such old child"
	    }

	    # Remove new child from current parent
	    if {[string length $newChild(node:parentNode)]} {
		node removeChild $newChild(node:parentNode) [lindex $args 0]
	    }

	    set children \
		[lreplace $children $idx $idx [lindex $args 0]]
	    set newChild(node:parentNode) $token

	    # Update old child to reflect lack of parentage
	    set oldChild(node:parentNode) {}

	    set result [lindex $args 1]

	    event postMutationEvent [lindex $args 0] DOMNodeInserted -relatedNode $token
	    FireNodeInsertedEvents [lindex $args 0]
	    event postMutationEvent $token DOMSubtreeModified

	}

	removeC* {

	    # removeChild

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::node removeChild token child\""
	    }
	    upvar #0 [lindex $args 0] oldChild
	    if {[string compare [namespace qualifiers [lindex $args 0]] [namespace qualifiers $token]]} {
		return -code error "node \"[lindex $args 0]\" is not a child"
	    }

	    # Remove the child from the parent
	    upvar #0 $node(node:childNodes) myChildren
	    if {[set idx [lsearch $myChildren [lindex $args 0]]] < 0} {
		return -code error "node \"[lindex $args 0]\" is not a child"
	    }
	    set myChildren [lreplace $myChildren $idx $idx]

	    # Update the child to reflect lack of parentage
	    set oldChild(node:parentNode) {}

	    set result [lindex $args 0]

	    # Event propagation has a problem here:
	    # Nodes that until recently were ancestors may
	    # want to capture the event, but we've just removed
	    # the parentage information.  They get a DOMSubtreeModified
	    # instead.
	    event postMutationEvent [lindex $args 0] DOMNodeRemoved -relatedNode $token
	    FireNodeRemovedEvents [lindex $args 0]
	    event postMutationEvent $token DOMSubtreeModified

	}

	ap* {

	    # appendChild

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::node appendChild token child\""
	    }

	    # Add to new parent
	    node insertBefore $token [lindex $args 0]

	    set result [lindex $args 0]

	}

	hasChildNodes {
	    set result [Min 1 [llength [set $node(node:childNodes)]]]
	}

	isSameNode {
	    # Introduced in DOM Level 3
	    switch [llength $args] {
		1 {
		    return [expr {$token == [lindex $args 0]}]
		}
		default {
		    return -code error "wrong # args: should be \"dom::node isSameNode token ref\""
		}
	    }
	}

	cl* {
	    # cloneNode

	    # May need to pay closer attention to generation of events here

	    set deep 0
	    switch [llength $args] {
		0 {
		}
		2 {
		    foreach {opt value} $args {
			switch -- $opt {
			    -deep {
				set deep [Boolean $value]
			    }
			    default {
				return -code error "bad option \"$opt\""
			    }
			}
		    }
		}
		default {
		    return -code error "wrong # args: should be \"dom::node cloneNode token ?-deep boolean?\""
		}
	    }

	    switch $node(node:nodeType) {
		element {
		    set result [CreateElement {} $node(node:nodeName) [array get $node(element:attributeList)] -document [namespace qualifiers $token]::Document]
		    if {$deep} {
			foreach child [set $node(node:childNodes)] {
			    node appendChild $result [node cloneNode $child -deep 1]
			}
		    }
		}
		textNode {
		    set result [CreateTextNode {} $node(node:nodeValue) -document [namespace qualifiers $token]::Document]
		}
		document {
		    set result [DOMImplementation create]
		    upvar #0 $result clonedDoc
		    array set clonedDoc [array get node document:doctype]
		    if {$deep} {
			foreach child [set $node(node:childNodes)] {
			    node appendChild $result [document importNode $result $child -deep 1]
			}
		    }
		}
		documentFragment -
		default {
		    set result [CreateGeneric {} node:nodeType $node(node:nodeType) -document [namespace qualifiers $token]::Document]
		    if {$deep} {
			foreach child [set $node(node:childNodes)] {
			    node appendChild $result [node cloneNode $child -deep 1]
			}
		    }
		}
	    }
	}

	ch* {
	    # children -- non-standard method

	    # If this is a textNode, then catch the error
	    set result {}
	    catch {set result [set $node(node:childNodes)]}

	}

	par* {
	    # parent -- non-standard method

	    return $node(node:parentNode)

	}

	pat* {
	    # path -- non-standard method

	    for {
		set ancestor $token
		upvar #0 $token ancestorNd
		set result {}
	    } {[string length $ancestorNd(node:parentNode)]} {
		set ancestor $ancestorNd(node:parentNode)
		upvar #0 $ancestor ancestorNd
	    } {
		set result [linsert $result 0 $ancestor]
	    }
	    # The last node is the document node
	    set result [linsert $result 0 $ancestor]

	}

	createNode {
	    # createNode -- non-standard method

	    # Creates node(s) in this document given an XPath expression.
	    # Relative location paths have this node as their initial context.

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::node createNode token path\""
	    }

	    package require xpath

	    return [XPath:CreateNode $token [lindex $args 0]]
	}

	selectNode {
	    # selectNode -- non-standard method

	    # Returns nodeset in this document matching an XPath expression.
	    # Relative location paths have this node as their initial context.

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::node selectNode token path\""
	    }

	    package require xpath

	    return [XPath:SelectNode $token [lindex $args 0]]
	}

	stringValue {
	    # stringValue -- non-standard method
	    # Returns string value of a node, as defined by XPath Rec.

	    if {[llength $args] > 0} {
		return -code error "wrong # args: should be \"dom::node stringValue token\""
	    }

	    switch $node(node:nodeType) {
		document -
		documentFragment -
		element {
		    set value {}
		    foreach child [set $node(node:childNodes)] {
			switch [node cget $child -nodeType] {
			    element -
			    textNode {
				append value [node stringValue $child]
			    }
			    default {
				# Other nodes are not considered
			    }
			}
		    }
		    return $value
		}
		attribute -
		textNode -
		processingInstruction -
		comment {
		    return $node(node:nodeValue)
		}
		default {
		    return {}
		}
	    }

	}

	addEv* {
	    # addEventListener -- introduced in DOM Level 2

	    if {[llength $args] < 1} {
		return -code error "wrong # args: should be \"dom::node addEventListener token type ?listener? ?option value...?\""
	    }

	    set type [lindex $args 0]
	    set args [lrange $args 1 end]
	    set listener [lindex $args 0]
	    if {[llength $args] == 1} {
		set args {}
	    } elseif {[llength $args] > 1} {
		if {[string match -* $listener]} {
		    set listener {}
		} else {
		    set args [lrange $args 1 end]
		}
	    }
	    array set opts {-usecapture 0}
	    if {[catch {array set opts $args}]} {
		return -code error "missing value for option \"[lindex $args end]\""
	    }
	    set opts(-usecapture) [Boolean $opts(-usecapture)]
	    set listenerType [expr {$opts(-usecapture) ? "capturer" : "listener"}]

	    if {[string length $listener]} {
		if {![info exists node(event:$type:$listenerType)] || \
			[lsearch $node(event:$type:$listenerType) $listener] < 0} {
		    lappend node(event:$type:$listenerType) $listener
		}
		# else avoid registering same listener twice
	    } else {
		# List all listeners
		set result {}
		catch {set result $node(event:$type:$listenerType)}
		return $result
	    }
	}

	removeE* {
	    # removeEventListener -- introduced in DOM Level 2

	    if {[llength $args] < 2} {
		return -code error "wrong # args: should be \"dom::node removeEventListener token type listener ?option value...?\""
	    }

	    set type [lindex $args 0]
	    set listener [lindex $args 1]
	    array set opts {-usecapture 0}
	    array set opts [lrange $args 2 end]
	    set opts(-usecapture) [Boolean $opts(-usecapture)]
	    set listenerType [expr {$opts(-usecapture) ? "capturer" : "listener"}]

	    set idx [lsearch $node(event:$type:$listenerType) $listener]
	    if {$idx >= 0} {
		set node(event:$type:$listenerType) [lreplace $node(event:$type:$listenerType) $idx $idx]
	    }

	}

	disp* {
	    # dispatchEvent -- introduced in DOM Level 2

	    # This is where the fun happens!
	    # Check to see if there one or more event listener,
	    # if so trigger the listener(s).
	    # Then pass the event up to the ancestor.
	    # This may be modified by event capturing and bubbling.

	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::node dispatchEvent token eventnode\""
	    }

	    set eventId [lindex $args 0]
	    upvar #0 $eventId event
	    set type $event(type)

	    if {![string length $event(eventPhase)]} {

		# This is the initial dispatch of the event.
		# First trigger any capturing event listeners
		# Starting from the root, proceed downward

		set event(eventPhase) capturing_phase
		set event(target) $token

		# DOM L2 specifies that the ancestors are determined
		# at the moment of event dispatch, so using a static
		# list is the correct thing to do

		foreach ancestor [lreplace [node path $token] end end] {
		    set event(currentNode) $ancestor

		    upvar #0 $ancestor ancNode

		    if {[info exists ancNode(event:$type:capturer)]} {
			foreach capturer $ancNode(event:$type:capturer) {
			    if {[catch {uplevel #0 $capturer [list $eventId]} capturerError]} {
				bgerror "error in capturer \"$capturerError\""
			    }
			}

			# A listener may stop propagation,
			# but we check here to let all of the
			# listeners at that level complete

			if {$event(cancelable) && $event(stopPropagation)} {
			    break
			}
		    }
		}

		# Prepare for next phase
		set event(eventPhase) at_target

	    }

	    set event(currentNode) $token

	    if {[info exists node(event:$type:listener)]} {
		foreach listener $node(event:$type:listener) {
		    if {[catch {uplevel #0 $listener [list $eventId]} listenerError]} {
			bgerror "error in listener \"$listenerError\""
		    }
		}
	    }

	    set event(eventPhase) bubbling_phase

	    # Now propagate the event
	    if {$event(cancelable) && $event(stopPropagation)} {
		# Event has been cancelled
	    } elseif {[llength $node(node:parentNode)]} {
		# Go ahead and propagate
		node dispatchEvent $node(node:parentNode) $eventId
	    }

	    set event(dispatched) 1
	}

	default {
	    return -code error "unknown method \"$method\""
	}

    }

    return $result
}

# dom::tcl::Node:create --
#
#	Generic node creation.
#	See also CreateElement, CreateTextNode, CreateGeneric.
#
# Arguments:
#	pVar	array in caller which contains parent details
#	args	configuration options
#
# Results:
#	New child node created.

proc dom::tcl::Node:create {pVar args} {
    upvar #0 $pVar parent

    array set opts {-name {} -value {}}
    array set opts $args

    upvar #0 [namespace qualifiers $pVar]::Document document

    # Create new node
    if {![info exists opts(-id)]} {
	set opts(-id) node[incr document(counter)]
    }
    set child [namespace qualifiers $pVar]::$opts(-id)
    upvar #0 $child new
    array set new [list \
	    node:parentNode $opts(-parent)	\
	    node:childNodes ${child}var		\
	    node:nodeType $opts(-type)		\
	    node:nodeName $opts(-name)		\
	    node:nodeValue $opts(-value)	\
	    element:attributeList ${child}arr	\
    ]
    set ${child}var {}
    array set ${child}arr {}

    # Update parent node
    if {![info exists parent(document:documentElement)]} {
	lappend parent(node:childNodes) $child
    }

    proc $child {method args} "return \[eval [namespace current]::node \[list \$method\] $child \$args\]"
    trace add command $child delete [namespace code [list Node:Delete $child]]

    return $child
}

# dom::tcl::Node:set --
#
#	Generic node update
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	Node modified.

proc dom::tcl::Node:set {token args} {
    upvar #0 $token node

    foreach {key value} $args {
	set node($key) $value
    }

    return {}
}

# dom::tcl::Node:Delete --
#
#	Handle node destruction
#
# Arguments:
#	name	node token
#	old	)
#	new	) arguments appended by trace command
#	op	)
#
# Results:
#	Node is destroyed

proc dom::tcl::Node:Delete {name old new op} {
    if {[catch {DOMImplementation destroy $name} ret]} {
	# Document has been deleted... namespace has been destroyed
    } else {
	return $ret
    }
}

# dom::tcl::FireNodeInsertedEvents --
#
#	Recursively descend the tree triggering DOMNodeInserted
#	events as we go.
#
# Arguments:
#	nodeid	Node ID
#
# Results:
#	DOM L2 DOMNodeInserted events posted

proc dom::tcl::FireNodeInsertedEvents nodeid {
    event postMutationEvent $nodeid DOMNodeInsertedIntoDocument
    foreach child [node children $nodeid] {
	FireNodeInsertedEvents $child
    }

    return {}
}

# dom::tcl::FireNodeRemovedEvents --
#
#	Recursively descend the tree triggering DOMNodeRemoved
#	events as we go.
#
# Arguments:
#	nodeid	Node ID
#
# Results:
#	DOM L2 DOMNodeRemoved events posted

proc dom::tcl::FireNodeRemovedEvents nodeid {
    event postMutationEvent $nodeid DOMNodeRemovedFromDocument
    foreach child [node children $nodeid] {
	FireNodeRemovedEvents $child
    }

    return {}
}

# dom::tcl::element --
#
#	Functions for an element.
#
# Arguments:
#	method	method to invoke
#	token	token for node
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable elementOptionsRO tagName|empty
    variable elementOptionsRW {}
}

proc dom::tcl::element {method token args} {
    variable elementOptionsRO
    variable elementOptionsRW

    upvar #0 $token node

    if {[string compare $node(node:nodeType) "element"]} {
	return -code error "malformed node token \"$token\""
    }
    set result {}

    switch -- $method {

	cget {
	    # Some read-only configuration options are computed
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::element cget token option\""
	    }
	    if {[regexp [format {^-(%s)$} $elementOptionsRO] [lindex $args 0] discard option]} {
		switch $option {
		    tagName {
			set result [lindex $node(node:nodeName) 0]
		    }
		    empty {
			if {![info exists node(element:empty)]} {
			    return 0
			} else {
			    return $node(element:empty)
			}
		    }
		    default {
			return $node(node:$option)
		    }
		}
	    } elseif {[regexp [format {^-(%s)$} $elementOptionsRW] [lindex $args 0] discard option]} {
		return $node(node:$option)
	    } else {
		return -code error "bad option \"[lindex $args 0]\""
	    }
	}
	configure {
	    if {[llength $args] == 1} {
		return [document cget $token [lindex $args 0]]
	    } elseif {[expr [llength $args] % 2]} {
		return -code error "no value specified for option \"[lindex $args end]\""
	    } else {
		foreach {option value} $args {
		    if {[regexp [format {^-(%s)$} $elementOptionsRO] $option discard opt]} {
			return -code error "option \"$option\" cannot be modified"
		    } elseif {[regexp [format {^-(%s)$} $elementOptionsRW] $option discard opt]} {
			return -code error "not implemented"
		    } else {
			return -code error "bad option \"$option\""
		    }
		}
	    }
	}

	getAttribute {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::element getAttribute token name\""
	    }

	    set result {}

	    upvar #0 $node(element:attributeList) attrList
	    catch {set result $attrList([lindex $args 0])}

	    return $result

	}

	setAttribute {
	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be \"dom::element setAttribute token name value\""
	    }

	    # Check that the attribute name is kosher
	    if {![regexp ^$::xml::Name\$ [lindex $args 0]]} {
		return -code error "invalid attribute name \"[lindex $args 0]\""
	    }

	    upvar #0 $node(element:attributeList) attrList
	    set evid [CreateEvent $token DOMAttrModified]
	    set oldValue {}
	    catch {set oldValue $attrList([lindex $args 0])}
	    event initMutationEvent $evid DOMAttrModified 1 0 $token $oldValue [lindex $args 1] [lindex $args 0] [expr {[info exists attrList([lindex $args 0])] ? "modification" : "addition"}]
	    set result [set attrList([lindex $args 0]) [lindex $args 1]]
	    node dispatchEvent $token $evid
	    DOMImplementation destroy $evid

	}

	removeAttribute {
	    if {[llength $args] != 1} {
		return -code error "wrong # args: should be \"dom::element removeAttribute token name\""
	    }

	    upvar #0 $node(element:attributeList) attrList
	    catch {unset attrList([lindex $args 0])}

	    event postMutationEvent $token DOMAttrRemoved -attrName [lindex $args 0] -attrChange removal

	}

	getAttributeNS {
	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be \"dom::element getAttributeNS token ns name\""
	    }

	    set result {}
	    upvar #0 $node(element:attributeList) attrList
	    catch {set result $attrList([lindex $args 0]^[lindex $args 1])}

	    return $result

	}

	setAttributeNS {
	    if {[llength $args] != 3} {
		return -code error "wrong # args: should be \"dom::element setAttributeNS token ns attr value\""
	    }

	    # Check that the attribute name is kosher
	    if {![regexp ^$::xml::QName\$ [lindex $args 1] discard prefix localName]} {
		return -code error "invalid qualified attribute name \"[lindex $args 1]\""
	    }

	    # BUG: At the moment the prefix is ignored

	    upvar #0 $node(element:attributeList) attrList
	    set evid [CreateEvent $token DOMAttrModified]
	    set oldValue {}
	    catch {set oldValue $attrList([lindex $args 0]^$localName)}
	    event initMutationEvent $evid DOMAttrModified 1 0 $token $oldValue [lindex $args 2] [lindex $args 0]^localName [expr {[info exists attrList([lindex $args 0]^$localName)] ? "modification" : "addition"}]
	    set result [set attrList([lindex $args 0]^$localName) [lindex $args 2]]
	    node dispatchEvent $token $evid
	    DOMImplementation destroy $evid

	}

	removeAttributeNS {
	    if {[llength $args] != 2} {
		return -code error "wrong # args: should be \"dom::element removeAttributeNS token ns name\""
	    }

	    upvar #0 $node(element:attributeList) attrList
	    catch {unset attrList([lindex $args 0]^[lindex $args 1])}

	    event postMutationEvent $token DOMAttrRemoved -attrName [lindex $args 0]^[lindex $args 1] -attrChange removal

	}

	getAttributeNode {
	    array set tmp [array get $node(element:attributeList)]
	    if {![info exists tmp([lindex $args 0])]} {
		return {}
	    }

	    # Synthesize an attribute node if one doesn't already exist
	    array set attrNodes $node(element:attributeNodes)
	    if {[catch {set result $attrNodes([lindex $args 0])}]} {
		set result [CreateGeneric $token node:nodeType attribute node:nodeName [lindex $args 0] node:nodeValue $tmp([lindex $args 0])]
		lappend node(element:attributeNodes) [lindex $args 0] $result
	    }
	}

	setAttributeNode -
	removeAttributeNode -
	getAttributeNodeNS -
	setAttributeNodeNS -
	removeAttributeNodeNS {
	    return -code error "not yet implemented"
	}

	getElementsByTagName {
	    if {[llength $args] < 1} {
		return -code error "wrong # args: should be \"dom::element getElementsByTagName token name\""
	    }

	    return [eval Element:GetByTagName [list $token [lindex $args 0]] \
		    [lrange $args 1 end]]
	}

	normalize {
	    if {[llength $args]} {
		return -code error "wrong # args: should be dom::element normalize token"
	    }

	    Element:Normalize node [set $node(node:childNodes)]
	}

	default {
	    return -code error "bad method \"$method\": should be cget, configure, getAttribute, setAttribute, removeAttribute, getAttributeNS, setAttributeNS, removeAttributeNS, getAttributeNode, setAttributeNode, removeAttributeNode, getAttributeNodeNS, setAttributeNodeNS, removeAttributeNodeNS, getElementsByTagName or normalize"
	}

    }

    return $result
}

# dom::tcl::Element:GetByTagName --
#
#	Search for (child) elements
#
#	This used to be non-recursive, but then I read the DOM spec
#	properly and discovered that it should recurse.  The -deep
#	option allows for backward-compatibility, and defaults to the
#	DOM-specified value of true.
#
# Arguments:
#	token	parent node
#	name	element type to search for
#	args	configuration options
#
# Results:
#	The name of the variable containing the list of matching node tokens

proc dom::tcl::Element:GetByTagName {token name args} {
    upvar #0 $token node
    upvar #0 [namespace qualifiers $token]::Document document

    array set cfg {-deep 1}
    array set cfg $args
    set cfg(-deep) [Boolean $cfg(-deep)]

    # Guard against arbitrary glob characters
    # Checking that name is a legal XML Name does this
    # However, '*' is permitted
    if {![regexp ^$::xml::Name\$ $name] && [string compare $name "*"]} {
	return -code error "invalid element name"
    }

    # Allocate variable name for this search
    set searchVar ${token}search[incr document(counter)]
    upvar \#0 $searchVar search

    # Make list live by interposing on variable reads
    # I don't think we need to interpose on unsets,
    # and writing to this variable by the application is
    # not permitted.

    trace variable $searchVar w [namespace code Element:GetByTagName:Error]

    if {[string compare $node(node:nodeType) "document"]} {
	trace variable $searchVar r [namespace code [list Element:GetByTagName:Search [set $node(node:childNodes)] $name $cfg(-deep)]]
    } elseif {[llength $node(document:documentElement)]} {
	# Document Element must exist and must be an element type node
	trace variable $searchVar r [namespace code [list Element:GetByTagName:Search $node(document:documentElement) $name $cfg(-deep)]]
    }

    return $searchVar
}

# dom::tcl::Element:GetByTagName:Search --
#
#	Search for elements.  This does the real work.
#	Because this procedure is invoked everytime
#	the variable is read, it returns the live list.
#
# Arguments:
#	tokens	nodes to search (inclusive)
#	name	element type to search for
#	deep	whether to search recursively
#	name1	\
#	name2	 > appended by trace command
#	op	/
#
# Results:
#	List of matching node tokens

proc dom::tcl::Element:GetByTagName:Search {tokens name deep name1 name2 op} {
    set result {}

    foreach tok $tokens {
	upvar #0 $tok nodeInfo
	switch -- $nodeInfo(node:nodeType) {
	    element {
		if {[string match $name [GetField nodeInfo(node:nodeName)]]} {
		    lappend result $tok
		}
		if {$deep} {
		    set childResult [Element:GetByTagName:Search [set $nodeInfo(node:childNodes)] $name $deep {} {} {}]
		    if {[llength $childResult]} {
			eval lappend result $childResult
		    }
		}
	    }
	}
    }

    if {[string length $name1]} {
	set $name1 $result
	return {}
    } else {
	return $result
    }
}

# dom::tcl::Element:GetByTagName:Error --
#
#	Complain about the application writing to a variable
#	that this package maintains.
#
# Arguments:
#	name1	\
#	name2	 > appended by trace command
#	op	/
#
# Results:
#	Error code returned.

proc dom::tcl::Element:GetByTagName:Error {name1 name2 op} {
    return -code error "dom: Read-only variable"
}

# dom::tcl::Element:Normalize --
#
#	Normalize the text nodes
#
# Arguments:
#	pVar	parent array variable in caller
#	nodes	list of node tokens
#
# Results:
#	Adjacent text nodes are coalesced

proc dom::tcl::Element:Normalize {pVar nodes} {
    upvar #0 $pVar parent

    set textNode {}

    foreach n $nodes {
	upvar #0 $n child
	set cleanup {}

	switch $child(node:nodeType) {
	    textNode {
		if {[llength $textNode]} {

		    # Coalesce into previous node
		    set evid [CreateEvent $n DOMCharacterDataModified]
		    event initMutationEvent $evid DOMCharacterDataModified 1 0 {} $text(node:nodeValue) $text(node:nodeValue)$child(node:nodeValue) {} {}
		    append text(node:nodeValue) $child(node:nodeValue)
		    node dispatchEvent $n $evid
		    DOMImplementation destroy $evid

		    # Remove this child
		    upvar #0 $parent(node:childNodes) childNodes
		    set idx [lsearch $childNodes $n]
		    set childNodes [lreplace $childNodes $idx $idx]
		    unset $n
		    set cleanup [list event postMutationEvent [node parent $n] DOMSubtreeModified]
		    event postMutationEvent $n DOMNodeRemoved

		    set $textNode [array get text]
		} else {
		    set textNode $n
		    catch {unset text}
		    array set text [array get child]
		}
	    }
	    element -
	    document -
	    documentFragment {
		set textNode {}
		Element:Normalize child [set $child(node:childNodes)]
	    }
	    default {
		set textNode {}
	    }
	}

	eval $cleanup
    }

    return {}
}

# dom::tcl::processinginstruction --
#
#	Functions for a processing intruction.
#
# Arguments:
#	method	method to invoke
#	token	token for node
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable piOptionsRO target
    variable piOptionsRW data
}

proc dom::tcl::processinginstruction {method token args} {
    variable piOptionsRO
    variable piOptionsRW

    upvar #0 $token node

    set result {}

    switch -- $method {

	cget {
	    # Some read-only configuration options are computed
	    if {[llength $args] != 1} {
		return -code error "too many arguments"
	    }
	    if {[regexp [format {^-(%s)$} $elementOptionsRO] [lindex $args 0] discard option]} {
		switch $option {
		    target {
			set result [lindex $node(node:nodeName) 0]
		    }
		    default {
			return $node(node:$option)
		    }
		}
	    } elseif {[regexp [format {^-(%s)$} $elementOptionsRW] [lindex $args 0] discard option]} {
		switch $option {
		    data {
			return $node(node:nodeValue)
		    }
		    default {
			return $node(node:$option)
		    }
		}
	    } else {
		return -code error "unknown option \"[lindex $args 0]\""
	    }
	}
	configure {
	    if {[llength $args] == 1} {
		return [document cget $token [lindex $args 0]]
	    } elseif {[expr [llength $args] % 2]} {
		return -code error "no value specified for option \"[lindex $args end]\""
	    } else {
		foreach {option value} $args {
		    if {[regexp [format {^-(%s)$} $elementOptionsRO] $option discard opt]} {
			return -code error "attribute \"$option\" is read-only"
		    } elseif {[regexp [format {^-(%s)$} $elementOptionsRW] $option discard opt]} {
			switch $opt {
			    data {
				set evid [CreateEvent $token DOMCharacterDataModified]
				event initMutationEvent $evid DOMCharacterModified 1 0 {} $node(node:nodeValue) $value {} {}
				set node(node:nodeValue) $value
				node dispatchEvent $token $evid
				DOMImplementation destroy $evid
			    }
			    default {
				set node(node:$opt) $value
			    }
			}
		    } else {
			return -code error "unknown option \"$option\""
		    }
		}
	    }
	}

	default {
	    return -code error "unknown method \"$method\""
	}

    }

    return $result
}

#################################################
#
# DOM Level 2 Interfaces
#
#################################################

# dom::tcl::event --
#
#	Implements Event Interface
#
#	Subclassed Interfaces are also defined here,
#	such as UIEvents.
#
# Arguments:
#	method	method to invoke
#	token	token for event
#	args	arguments for method
#
# Results:
#	Depends on method used.

namespace eval dom::tcl {
    variable eventOptionsRO type|target|currentNode|eventPhase|bubbles|cancelable|timeStamp|detail|view|screenX|screenY|clientX|clientY|ctrlKey|shiftKey|altKey|metaKey|button|relatedNode|prevValue|newValue|attrName|attrChange
    variable eventOptionsRW {}

    # Issue: should the attributes belonging to the subclassed Interface
    # be separated out?

    variable uieventOptionsRO detail|view
    variable uieventOptionsRW {}

    variable mouseeventOptionsRO screenX|screenY|clientX|clientY|ctrlKey|shiftKey|altKey|metaKey|button|relatedNode
    variable mouseeventOptionsRW {}

    variable mutationeventOptionsRO relatedNode|prevValue|newValue|attrName
    variable mutationeventOptionsRW {}
}

proc dom::tcl::event {method token args} {
    variable eventOptionsRO
    variable eventOptionsRW

    upvar #0 $token event

    set result {}

    switch -glob -- $method {

	cg* {
	    # cget

	    if {[llength $args] != 1} {
		return -code error "too many arguments"
	    }
	    if {[regexp [format {^-(%s)$} $eventOptionsRO] [lindex $args 0] discard option]} {
		return $event($option)
	    } elseif {[regexp [format {^-(%s)$} $eventOptionsRW] [lindex $args 0] discard option]} {
		return $event($option)
	    } else {
		return -code error "unknown option \"[lindex $args 0]\""
	    }
	}

	co* {
	    # configure

	    if {[llength $args] == 1} {
		return [event cget $token [lindex $args 0]]
	    } elseif {[expr [llength $args] % 2]} {
		return -code error "no value specified for option \"[lindex $args end]\""
	    } else {
		foreach {option value} $args {
		    if {[regexp [format {^-(%s)$} $eventOptionsRW] $option discard opt]} {
			set event($opt) $value
		    } elseif {[regexp [format {^-(%s)$} $eventOptionsRO] $option discard opt]} {
			return -code error "attribute \"$option\" is read-only"
		    } else {
			return -code error "unknown option \"$option\""
		    }
		}
	    }

	}

	st* {
	    # stopPropagation

	    set event(stopPropagation) 1
	}

	pr* {
	    # preventDefault

	    set event(preventDefault) 1
	}

	initE* {
	    # initEvent

	    if {[llength $args] != 3} {
		return -code error "wrong # args: should be dom::event initEvent token type bubbles cancelable"
	    }

	    if {$event(dispatched)} {
		return -code error "event has been dispatched"
	    }

	    foreach {event(type) event(bubbles) event(cancelable)} $args break
	}

	initU* {
	    # initUIEvent

	    if {[llength $args] < 4 || [llength $args] > 5} {
		return -code error "wrong # args: should be dom::event initUIEvent token type bubbles cancelable view detail"
	    }

	    if {$event(dispatched)} {
		return -code error "event has been dispatched"
	    }

	    set event(detail) 0
	    foreach {event(type) event(bubbles) event(cancelable) event(view) event(detail)} $args break
	}

	initMo* {
	    # initMouseEvent

	    if {[llength $args] != 15} {
		return -code error "wrong # args: should be dom::event initMouseEvent token type bubbles cancelable view detail screenX screenY clientX clientY ctrlKey altKey shiftKey metaKey button relatedNode"
	    }

	    if {$event(dispatched)} {
		return -code error "event has been dispatched"
	    }

	    set event(detail) 1
	    foreach {event(type) event(bubbles) event(cancelable) event(view) event(detail) event(screenX) event(screenY) event(clientX) event(clientY) event(ctrlKey) event(altKey) event(shiftKey) event(metaKey) event(button) event(relatedNode)} $args break
	}

	initMu* {
	    # initMutationEvent

	    if {[llength $args] != 8} {
		return -code error "wrong # args: should be dom::event initMutationEvent token type bubbles cancelable relatedNode prevValue newValue attrName attrChange"
	    }

	    if {$event(dispatched)} {
		return -code error "event has been dispatched"
	    }

	    foreach {event(type) event(bubbles) event(cancelable) event(relatedNode) event(prevValue) event(newValue) event(attrName) event(attrChange)} $args break
	}

	postUI* {
	    # postUIEvent, non-standard convenience method

	    set evType [lindex $args 0]
	    array set evOpts [list \
		    -bubbles $::dom::bubbles($evType) -cancelable $::dom::cancelable($evType)	\
		    -view {}			\
		    -detail {}			\
	    ]
	    array set evOpts [lrange $args 1 end]

	    set evid [CreateEvent $token $evType]
	    event initUIEvent $evid $evType $evOpts(-bubbles) $evOpts(-cancelable) $evOpts(-view) $evOpts(-detail)
	    node dispatchEvent $token $evid
	    DOMImplementation destroy $evid

	}

	postMo* {
	    # postMouseEvent, non-standard convenience method

	    set evType [lindex $args 0]
	    array set evOpts [list \
		    -bubbles $::dom::bubbles($evType) -cancelable $::dom::cancelable($evType)	\
		    -view {}			\
		    -detail {}			\
		    -screenX {}			\
		    -screenY {}			\
		    -clientX {}			\
		    -clientY {}			\
		    -ctrlKey {}			\
		    -altKey {}			\
		    -shiftKey {}		\
		    -metaKey {}			\
		    -button {}			\
		    -relatedNode {}		\
	    ]
	    array set evOpts [lrange $args 1 end]

	    set evid [CreateEvent $token $evType]
	    event initMouseEvent $evid $evType $evOpts(-bubbles) $evOpts(-cancelable) $evOpts(-view) $evOpts(-detail) $evOpts(-screenX) $evOpts(-screenY) $evOpts(-clientX) $evOpts(-clientY) $evOpts(-ctrlKey) $evOpts(-altKey) $evOpts(-shiftKey) $evOpts(-metaKey) $evOpts(-button) $evOpts(-relatedNode)
	    node dispatchEvent $token $evid
	    DOMImplementation destroy $evid

	}

	postMu* {
	    # postMutationEvent, non-standard convenience method

	    set evType [lindex $args 0]
	    array set evOpts [list \
		    -bubbles $::dom::bubbles($evType) -cancelable $::dom::cancelable($evType)	\
		    -relatedNode {}			\
		    -prevValue {} -newValue {}		\
		    -attrName {} -attrChange {}		\
	    ]
	    array set evOpts [lrange $args 1 end]

	    set evid [CreateEvent $token $evType]
	    event initMutationEvent $evid $evType $evOpts(-bubbles) $evOpts(-cancelable) $evOpts(-relatedNode) $evOpts(-prevValue) $evOpts(-newValue) $evOpts(-attrName) $evOpts(-attrChange)
	    node dispatchEvent $token $evid
	    DOMImplementation destroy $evid

	}

	default {
	    return -code error "unknown method \"$method\""
	}
    }

    return $result
}

# dom::tcl::CreateEvent --
#
#	Create an event object
#
# Arguments:
#	token	parent node
#	type	event type
#	args	configuration options
#
# Results:
#	Returns event token

proc dom::tcl::CreateEvent {token type args} {
    array set opts $args
    if {[string length $token]} {
	upvar #0 $token parent
	upvar #0 [namespace qualifiers $token]::Document document
	set child [namespace qualifiers $token]::event[incr document(counter)]
    } elseif {[info exists $opts(-document)]} {
	upvar #0 $opts(-document) document
	set child [namespace qualifiers $opts(-document)]::event[incr document(counter)]
    }

    upvar #0 $child event

    # Create the event
    array set event [list \
	    node:nodeType event	\
	    type $type		\
	    target {}		\
	    currentNode {}	\
	    cancelable 1	\
	    stopPropagation 0	\
	    preventDefault 0	\
	    dispatched 0	\
	    bubbles 1		\
	    eventPhase {}	\
	    timeStamp [clock clicks -milliseconds]	\
	    ]

    proc $child {method args} "return \[eval [namespace current]::event \[list \$method\] $child \$args\]"
    trace add command $child delete [namespace code [list Node:Delete $child]]

    return $child
}

#################################################
#
# Serialisation
#
#################################################

# dom::tcl::Serialize:documentFragment --
#
#	Produce text for documentFragment.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:documentFragment {token args} {
    upvar #0 $token node

    if {[string compare "Document" [namespace tail $token]]} {
	return [eval [list Serialize:node $token] $args]
    } else {
	if {[string compare {} [GetField node(document:documentElement)]]} {
	    return [eval Serialize:document [list $token] $args]
	} else {
	    return -code error "document has no document element"
	}
    }

}

# dom::tcl::Serialize:document --
#
#	Produce text for document.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:document {token args} {
    upvar #0 $token node
    array set opts {
	-showxmldecl 1
	-showdoctypedecl 1
    }
    array set opts $args

    set result {}

    if {[string length $node(document:doctype)]} {

	upvar #0 $node(document:doctype) doctype

	# Bug fix: can't use Serialize:attributeList for XML declaration,
	# since attributes must occur in a given order (XML 2.8 [23])

	set result {}

	if {$opts(-showxmldecl)} {
	    append result <?xml[Serialize:XMLDecl version $node(document:xmldecl)][Serialize:XMLDecl encoding $node(document:xmldecl)][Serialize:XMLDecl standalone $node(document:xmldecl)]?>\n
	}
	if {$opts(-showdoctypedecl)} {
	    # Is document element in an XML Namespace?
	    # If so then include prefix in doctype decl
	    foreach {prefix localName} [::xml::qnamesplit $doctype(doctype:name)] break
	    if {![string length $prefix]} {
		# The prefix may not have been allocated yet
		upvar #0 $node(document:documentElement) docel
		if {[info exists docel(node:namespaceURI)] && \
			[string length $docel(node:namespaceURI)]} {
		    set declPrefix [GetNamespacePrefix $node(document:documentElement) $docel(node:namespaceURI)]
		    set docelName $declPrefix:$doctype(doctype:name)
		} else {
		    set docelName $doctype(doctype:name)
		}
	    } else {
		set docelName $doctype(doctype:name)
	    }
	    # Applied patch by Marco Gonnelli, bug #590914
	    append result <!DOCTYPE\ $docelName[Serialize:ExternalID $doctype(doctype:publicId) $doctype(doctype:systemId)][expr {[string length $doctype(doctype:internalSubset)] ? " \[[string trim $doctype(doctype:internalSubset) \{\} ]\]" : {}}]>\n
	}
    }

    # BUG #525505: Want to serialize all children including the
    # document element.

    if {[info exists $node(node:childNodes)]} {
	foreach child [set $node(node:childNodes)] {
	    append result [eval Serialize:[node cget $child -nodeType] [list $child] $args]
	}
    }

    return $result
}

# dom::tcl::Serialize:ExternalID --
#
#	Returned appropriately quoted external identifiers
#
# Arguments:
#	publicid	public identifier
#	systemid	system identifier
#
# Results:
#	text

proc dom::tcl::Serialize:ExternalID {publicid systemid} {

    switch -glob -- [string length $publicid],[string length $systemid] {
	0,0 {
	    return {}
	}
	0,* {
	    return " SYSTEM \"$systemid\""
	}
	*,* {
	    # Patch from c.l.t., Richard Calmbach (rc@hnc.com )
	    return " PUBLIC \"$publicid\" \"$systemid\""
	}
    }

    return {}
}

# dom::tcl::Serialize:XMLDecl --
#
#	Produce text for XML Declaration attribute.
#	Order is determine by document serialisation procedure.
#
# Arguments:
#	attr	required attribute
#	attList	attribute list
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:XMLDecl {attr attrList} {
    array set data $attrList
    if {![info exists data($attr)]} {
	return {}
    } elseif {[string length $data($attr)]} {
	return " $attr='$data($attr)'"
    } else {
	return {}
    }
}

# dom::tcl::Serialize:node --
#
#	Produce text for an arbitrary node.
#	This simply serializes the child nodes of the node.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:node {token args} {
    upvar #0 $token node
    array set opts $args

    if {[info exists opts(-indent)]} {
	# NB. 0|1 cannot be used as booleans - mention this in docn
	if {[regexp {^false|no|off$} $opts(-indent)]} {
	    # No action required
	} elseif {[regexp {^true|yes|on$} $opts(-indent)]} {
	    set opts(-indent) 1
	} else {
	    incr opts(-indent)
	}
    }

    set result {}
    foreach childToken [set $node(node:childNodes)] {
	upvar #0 $childToken child
	append result [eval [list Serialize:$child(node:nodeType) $childToken] [array get opts]]
    }

    return $result
}

# dom::tcl::Serialize:element --
#
#	Produce text for an element.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:element {token args} {
    upvar #0 $token node
    array set opts {-newline {}}
    array set opts $args

    set result {}
    set newline {}
    if {[lsearch $opts(-newline) $node(node:nodeName)] >= 0} {
	append result \n
	set newline \n
    }
    append result [eval Serialize:Indent [array get opts]]
    switch [info exists node(node:namespaceURI)],[info exists node(node:prefix)] {

	1,1 {
	    # XML Namespace is in scope, prefix supplied
	    if {[string length $node(node:prefix)]} {
		# Make sure that there's a declaration for this XML Namespace
		set declPrefix [GetNamespacePrefix $token $node(node:namespaceURI) -prefix $node(node:prefix)]
		# ASSERTION: $declPrefix == $node(node:prefix)
		set nsPrefix $node(node:prefix):
	    } elseif {[string length $node(node:namespaceURI)]} {
		set nsPrefix [GetNamespacePrefix $token $node(node:namespaceURI)]:
	    } else {
		set nsPrefix {}
	    }
	}

	1,0 {
	    # XML Namespace is in scope, no prefix
	    set nsPrefix [GetNamespacePrefix $token $node(node:namespaceURI)]:
	    if {![string compare $nsPrefix :]} {
		set nsPrefix {}
	    }
	}

	0,1 {
	    # Internal error
	    set nsPrefix {}
	}

	0,0 -
	default {
	    # No XML Namespace is in scope
	    set nsPrefix {}
	}
    }
    append result <$nsPrefix$node(node:localName)

    append result [Serialize:attributeList [array get $node(element:attributeList)]]

    if {![llength [set $node(node:childNodes)]]} {

	append result />$newline

    } else {

	append result >$newline

	# Do the children
	if {[hasmixedcontent $token]} {
	    set opts(-indent) no
	}
	append result [eval Serialize:node [list $token] [array get opts]]

	append result [eval Serialize:Indent [array get opts]]
	append result "$newline</$nsPrefix$node(node:localName)>$newline"

    }

    return $result
}

# dom::tcl::GetNamespacePrefix --
#
#	Determine the XML Namespace prefix for a Namespace URI
#
# Arguments:
#	token	node token
#	nsuri	XML Namespace URI
#	args	configuration options
#
# Results:
#	Returns prefix.
#	May add prefix information to node

proc dom::tcl::GetNamespacePrefix {token nsuri args} {
    upvar #0 $token node
    array set options $args

    GetNamespaceDecl $token $nsuri declNode prefix

    if {[llength $declNode]} {
	# A declaration was found for this Namespace URI
	return $prefix
    } else {
	# No declaration found.  Allocate a prefix
	# and add XML Namespace declaration
	set prefix {}
	catch {set prefix $options(-prefix)}
	if {![string compare $prefix {}]} {
	    upvar #0 [namespace qualifiers $token]::Document document
	    set prefix ns[incr document(counter)]
	}
	set node(node:prefix) $prefix
	upvar \#0 $node(element:attributeList) attrs
	set attrs(${::dom::xmlnsURI}^$prefix) $nsuri

	return $prefix
    }
}

# dom::tcl::GetNamespaceDecl --
#
#	Find the XML Namespace declaration.
#
# Arguments:
#	token	node token
#	nsuri	XML Namespace URI
#	nodeVar	Variable name for declaration
#	prefVar Variable for prefix
#
# Results:
#	If the declaration is found returns node and prefix

proc dom::tcl::GetNamespaceDecl {token nsuri nodeVar prefVar} {
    upvar #0 $token node
    upvar $nodeVar declNode
    upvar $prefVar prefix

    while {[string length $node(node:parentNode)]} {

	# Check this node's XML Namespace declarations
	catch {unset attrs}
	array set attrs [array get $node(element:attributeList)]
	foreach {nsdecl decluri} [array get attrs ${::dom::xmlnsURI}^*] {
	    if {![string compare $decluri $nsuri]} {
		regexp [format {%s\^(.*)} $::dom::xmlnsURI] $nsdecl dummy prefix
		set declNode $token
		return
	    }
	}

	# Move up to parent
	set token $node(node:parentNode)
	upvar #0 $token node
    }

    # Got to Document node and didn't find XML NS decl
    set prefix {}
    set declNode {}
}

# dom::tcl::Serialize:textNode --
#
#	Produce text for a text node.  This procedure may
#	return a CDATA section where appropriate.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:textNode {token args} {
    upvar #0 $token node

    if {$node(node:cdatasection)} {
	return [Serialize:CDATASection $node(node:nodeValue)]
    } elseif {[Serialize:ExceedsThreshold $node(node:nodeValue)]} {
	return [Serialize:CDATASection $node(node:nodeValue)]
    } else {
	return [Encode $node(node:nodeValue)]
    }
}

# dom::tcl::Serialize:ExceedsThreshold --
#
#	Applies heuristic(s) to determine whether a text node
#	should be formatted as a CDATA section.
#
# Arguments:
#	text	node text
#
# Results:
#	Boolean.

proc dom::tcl::Serialize:ExceedsThreshold {text} {
    return [expr {[regsub -all {[<>&]} $text {} discard] > $::dom::maxSpecials}]
}

# dom::tcl::Serialize:CDATASection --
#
#	Formats a CDATA section.
#
# Arguments:
#	text	node text
#
# Results:
#	XML text.

proc dom::tcl::Serialize:CDATASection {text} {
    set result {}
    while {[regexp {(.*)]]>(.*)} $text discard text trailing]} {
	set result \]\]&gt\;<!\[CDATA\[$trailing\]\]>$result
    }
    return <!\[CDATA\[$text\]\]>$result
}

# dom::tcl::Serialize:processingInstruction --
#
#	Produce text for a PI node.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:processingInstruction {token args} {
    upvar #0 $token node

    return "[eval Serialize:Indent $args]<?$node(node:nodeName)[expr {$node(node:nodeValue) == "" ? "" : " $node(node:nodeValue)"}]?>"
}

# dom::tcl::Serialize:comment --
#
#	Produce text for a comment node.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:comment {token args} {
    upvar #0 $token node

    return [eval Serialize:Indent $args]<!--$node(node:nodeValue)-->
}

# dom::tcl::Serialize:entityReference --
#
#	Produce text for an entity reference.
#
# Arguments:
#	token	node token
#	args	configuration options
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:entityReference {token args} {
    upvar #0 $token node

    return &$node(node:nodeName)\;
}

# dom::tcl::Encode --
#
#	Encode special characters
#
# Arguments:
#	value	text value
#
# Results:
#	XML format text.

proc dom::tcl::Encode value {
    array set Entity {
	$ $
	< &lt;
	> &gt;
	& &amp;
	\" &quot;
	' &apos;
    }

    regsub -all {([$<>&"'])} $value {$Entity(\1)} value

    return [subst -nocommand -nobackslash $value]
}

# dom::tcl::Serialize:attributeList --
#
#	Produce text for an attribute list.
#
# Arguments:
#	l	name/value paired list
#
# Results:
#	XML format text.

proc dom::tcl::Serialize:attributeList {l} {

    set result {}
    foreach {name value} $l {

	if {[regexp {^([^^]+)\^(.*)$} $name discard nsuri prefix]} {
	    if {[string compare $nsuri $::dom::xmlnsURI]} {
		# Need the node token to resolve the Namespace URI
		append result { } ?:$prefix =
	    } else {
		# A Namespace declaration
		append result { } xmlns:$prefix =
	    }
	} else {
	    append result { } $name =
	}

	# Handle special characters
	regsub -all & $value {\&amp;} value
	regsub -all < $value {\&lt;} value

	if {![string match *\"* $value]} {
	    append result \"$value\"
	} elseif {![string match *'* $value]} {
	    append result '$value'
	} else {
	    regsub -all \" $value {\&quot;} value
	    append result \"$value\"
	}

    }

    return $result
}

# dom::tcl::Serialize:Indent --
#
#	Calculate the indentation required, if any
#
# Arguments:
#	args	configuration options, which may specify -indent
#
# Results:
#	May return white space

proc dom::tcl::Serialize:Indent args {
    array set opts [list -indentspec $::dom::indentspec]
    array set opts $args

    if {![info exists opts(-indent)] || \
	    [regexp {^false|no|off$} $opts(-indent)]} {
	return {}
    }

    if {[regexp {^true|yes|on$} $opts(-indent)]} {
	# Default indent level is 0
	return \n
    }

    if {!$opts(-indent)} {
	return \n
    }

    set ws [format \n%\ [expr $opts(-indent) * [lindex $opts(-indentspec) 0]]s { }]
    regsub -all [lindex [lindex $opts(-indentspec) 1] 0] $ws [lindex [lindex $opts(-indentspec) 1] 1] ws

    return $ws

}

#################################################
#
# Parsing
#
#################################################

# dom::tcl::ParseElementStart --
#
#	Push a new element onto the stack.
#
# Arguments:
#	stateVar	global state array variable
#	name		element name
#	attrList	attribute list
#	args		configuration options
#
# Results:
#	An element is created within the currently open element.

proc dom::tcl::ParseElementStart {stateVar name attrList args} {

    upvar #0 $stateVar state
    array set opts $args

    # Push namespace declarations
    # We need to be able to map namespaceURI's back to prefixes
    set nsattrlists {}
    catch {
	foreach {namespaceURI prefix} $opts(-namespacedecls) {
	    lappend state(NS:$namespaceURI) $prefix

	    # Also, synthesize namespace declaration attributes
	    # TclXML is a little too clever when it parses them away!

	    lappend nsattrlists $prefix $namespaceURI
	}
	lappend opts(-namespaceattributelists) $::dom::xmlnsURI $nsattrlists

    }

    set nsarg {}
    catch {
	lappend nsarg -namespace $opts(-namespace)
	lappend nsarg -localname $name
	lappend nsarg -prefix [lindex $state(NS:$opts(-namespace)) end]
    }

    lappend state(current) \
	[eval CreateElement [list [lindex $state(current) end] $name $attrList] $nsarg [array get opts -namespaceattributelists]]

    if {[info exists opts(-empty)] && $opts(-empty)} {
	# Flag this node as being an empty element
	upvar #0 [lindex $state(current) end] node
	set node(element:empty) 1
    }

    # Temporary: implement -progresscommand here, because of broken parser
    if {[string length $state(-progresscommand)]} {
	if {!([incr state(progCounter)] % $state(-chunksize))} {
	    uplevel #0 $state(-progresscommand)
	}
    }
}

# dom::tcl::ParseElementEnd --
#
#	Pop an element from the stack.
#
# Arguments:
#	stateVar	global state array variable
#	name		element name
#	args		configuration options
#
# Results:
#	Currently open element is closed.

proc dom::tcl::ParseElementEnd {stateVar name args} {
    upvar #0 $stateVar state

    set state(current) [lreplace $state(current) end end]
}

# dom::tcl::ParseCharacterData --
#
#	Add a textNode to the currently open element.
#
# Arguments:
#	stateVar	global state array variable
#	data		character data
#
# Results:
#	A textNode is created.

proc dom::tcl::ParseCharacterData {stateVar data} {
    upvar #0 $stateVar state

    CreateTextNode [lindex $state(current) end] $data
}

# dom::tcl::ParseProcessingInstruction --
#
#	Add a PI to the currently open element.
#
# Arguments:
#	stateVar	global state array variable
#	name		PI name
#	target		PI target
#
# Results:
#	A processingInstruction node is created.

proc dom::tcl::ParseProcessingInstruction {stateVar name target} {
    upvar #0 $stateVar state

    CreateGeneric [lindex $state(current) end] node:nodeType processingInstruction node:nodeName $name node:nodeValue $target
}

# dom::tcl::ParseXMLDeclaration --
#
#	Add information from the XML Declaration to the document.
#
# Arguments:
#	stateVar	global state array variable
#	version		version identifier
#	encoding	character encoding
#	standalone	standalone document declaration
#
# Results:
#	Document node modified.

proc dom::tcl::ParseXMLDeclaration {stateVar version encoding standalone} {
    upvar #0 $stateVar state

    upvar #0 $state(docNode) document
    array set xmldecl $document(document:xmldecl)

    array set xmldecl [list version $version	\
	    standalone $standalone		\
	    encoding $encoding			\
    ]

    set document(document:xmldecl) [array get xmldecl]

    return {}
}

# dom::tcl::ParseDocType --
#
#	Add a Document Type Declaration node to the document.
#
# Arguments:
#	stateVar	global state array variable
#	root		root element type
#	publit		public identifier literal
#	systemlist	system identifier literal
#	dtd		internal DTD subset
#
# Results:
#	DocType node added

proc dom::tcl::ParseDocType {stateVar root {publit {}} {systemlit {}} {dtd {}} args} {
    upvar #0 $stateVar state
    upvar #0 $state(docNode) document

    set document(document:doctype) [CreateDocType $state(docNode) $publit $systemlit $dtd]

    return {}
}

# dom::tcl::ParseComment --
#
#	Parse comment
#
# Arguments:
#	stateVar	state array
#	data		comment data
#
# Results:
#	Comment node added to DOM tree

proc dom::tcl::ParseComment {stateVar data} {
    upvar #0 $stateVar state

    CreateGeneric [lindex $state(current) end] node:nodeType comment node:nodeValue $data

    return {}
}

# dom::tcl::ParseEntityReference --
#
#	Parse an entity reference
#
# Arguments:
#	stateVar	state variable
#	ref		entity
#
# Results:
#	Entity reference node added to DOM tree

proc dom::tcl::ParseEntityReference {stateVar ref} {
    upvar #0 $stateVar state

    CreateGeneric [lindex $state(current) end] node:nodeType entityReference node:nodeName $ref

    return {}
}

#################################################
#
# Trim white space
#
#################################################

# dom::tcl::Trim --
#
#	Remove textNodes that only contain white space
#
# Arguments:
#	nodeid	node to trim
#
# Results:
#	textNode nodes may be removed (from descendants)

proc dom::tcl::Trim nodeid {
    upvar #0 $nodeid node

    switch $node(node:nodeType) {

	textNode {
	    if {![string length [string trim $node(node:nodeValue)]]} {
		node removeChild $node(node:parentNode) $nodeid
	    }
	}

	default {
	    # Some nodes have no child list.  Reported by Jim Hollister <jhollister@objectspace.com>
	    set children {}
	    catch {set children [set $node(node:childNodes)]}
	    foreach child $children {
		Trim $child
	    }
	}

    }

    return {}
}

#################################################
#
# XPath support
#
#################################################

# dom::tcl::XPath:CreateNode --
#
#	Given an XPath expression, create the node
#	referred to by the expression.  Nodes required
#	as steps of the path are created if they do
#	not exist.
#
# Arguments:
#	node	context node
#	path	location path
#
# Results:
#	Node(s) created in the DOM tree.
#	Returns token for deepest node in the expression.

proc dom::tcl::XPath:CreateNode {node path} {

    set root [::dom::node cget $node -ownerDocument]

    set spath [::xpath::split $path]

    if {[llength $spath] <= 1} {
	# / - do nothing
	return $root
    }

    if {![llength [lindex $spath 0]]} {
	# Absolute location path
	set context $root
	set spath [lrange $spath 1 end]
	set contexttype document
    } else {
	set context $node
	set contexttype [::dom::node cget $node -nodeType]
    }

    foreach step $spath {

	# Sanity check on path
	switch $contexttype {
	    document -
	    documentFragment -
	    element {}
	    default {
		return -code error "node type \"$contexttype\" have no children"
	    }
	}

	switch [lindex $step 0] {

	    child {
		if {[llength [lindex $step 1]] > 1} {
		    foreach {nodetype discard} [lindex $step 1] break

		    switch -- $nodetype {
			text {
			    set posn [CreateNode:FindPosition [lindex $step 2]]

			    set count 0
			    set targetNode {}
			    foreach child [::dom::node children $context] {
				switch [::dom::node cget $child -nodeType] {
				    textNode {
					incr count
					if {$count == $posn} {
					    set targetNode $child
					    break
					}
				    }
				    default {}
				}
			    }

			    if {[string length $targetNode]} {
				set context $targetNode
			    } else {
				# Creating sequential textNodes doesn't make sense
				set context [::dom::document createTextNode $context {}]
			    }
			    set contexttype textNode
			}
			default {
			    return -code error "node type test \"${nodetype}()\" not supported"
			}
		    }
		} else {
		    # Find the child element
		    set posn [CreateNode:FindPosition [lindex $step 2]]

		    set count 0
		    set targetNode {}
		    foreach child [::dom::node children $context] {
			switch [node cget $child -nodeType] {
			    element {
				if {![string compare [lindex $step 1] [::dom::node cget $child -nodeName]]} {
				    incr count
				    if {$count == $posn} {
					set targetNode $child
					break
				    }
				}
			    }
			    default {}
			}
		    }

		    if {[string length $targetNode]} {
			set context $targetNode
		    } else {
			# Didn't find it so create required elements
			while {$count < $posn} {
			    set child [::dom::document createElement $context [lindex $step 1]]
			    incr count
			}
			set context $child
		    }
		    set contexttype element

		}
	    }

	    default {
		return -code error "axis \"[lindex $step 0]\" is not supported"
	    }
	}
    }

    return $context
}

# dom::tcl::CreateNode:FindPosition --

proc dom::tcl::CreateNode:FindPosition predicates {
    switch [llength $predicates] {
	0 {
	    return 1
	}
	1 {
	    # Fall-through
	}
	default {
	    return -code error "multiple predicates not yet supported"
	}
    }
    set predicate [lindex $predicates 0]

    switch -- [lindex [lindex $predicate 0] 0] {
	function {
	    switch -- [lindex [lindex $predicate 0] 1] {
		position {
		    if {[lindex $predicate 1] == "="} {
			if {[string compare [lindex [lindex $predicate 2] 0] "number"]} {
			    return -code error "operand must be a number"
			} else {
			    set posn [lindex [lindex $predicate 2] 1]
			}
		    } else {
			return -code error "operator must be \"=\""
		    }
		}
		default {
		    return -code error "predicate function \"[lindex [lindex $predicate 0] 1]\" not supported"
		}
	    }
	}
	default {
	    return -code error "predicate must be position() function"
	}
    }

    return $posn
}

# dom::tcl::XPath:SelectNode --
#
#	Match nodes with an XPath location path
#
# Arguments:
#	ctxt	context - Tcl list
#	path	location path
#
# Results:
#	Returns Tcl list of matching nodes

proc dom::tcl::XPath:SelectNode {ctxt path} {

    if {![llength $ctxt]} {
	return {}
    }

    set spath [xpath::split $path]

    if {[string length [node parent [lindex $ctxt 0]]]} {
	set root [namespace qualifiers [lindex $ctxt 0]]::Document
    } else {
	set root [lindex $ctxt 0]
    }

    if {[llength $spath] == 0} {
	return $root
    }
    if {[llength $spath] == 1 && [llength [lindex $spath 0]] == 0} {
	return $root
    }

    if {![llength [lindex $spath 0]]} {
	set ctxt $root
	set spath [lrange $spath 1 end]
    }

    return [XPath:SelectNode:Rel $ctxt $spath]
}

# dom::tcl::XPath:SelectNode:Rel --
#
#	Match nodes with an XPath location path
#
# Arguments:
#	ctxt	context - Tcl list
#	path	split location path
#
# Results:
#	Returns Tcl list of matching nodes

proc dom::tcl::XPath:SelectNode:Rel {ctxt spath} {
    if {![llength $spath]} {
	return $ctxt
    }

    set step [lindex $spath 0]
    set result {}
    switch [lindex $step 0] {

	child {
	    # All children are candidates
	    set children {}
	    foreach node [XPath:SN:GetElementTypeNodes $ctxt] {
		eval lappend children [node children $node]
	    }

	    # Now apply node test to each child
	    foreach node $children {
		if {[XPath:SN:ApplyNodeTest $node [lindex $step 1]]} {
		    lappend result $node
		}
	    }

	}

	descendant-or-self {
	    foreach node $ctxt {
		if {[XPath:SN:ApplyNodeTest $node [lindex $step 1]]} {
		    lappend result $node
		}
		eval lappend result [XPath:SN:DescendAndTest [node children $node] [lindex $step 1]]
	    }
	}

	descendant {
	    foreach node $ctxt {
		eval lappend result [XPath:SN:DescendAndTest [node children $node] [lindex $step 1]]
	    }
	}

	attribute {
	    if {[string compare [lindex $step 1] "*"]} {
		foreach node $ctxt {
		    set attrNode [element getAttributeNode $node [lindex $step 1]]
		    if {[llength $attrNode]} {
			lappend result $attrNode
		    }
		}
	    } else {
		# All attributes are returned
		foreach node $ctxt {
		    foreach attrName [array names [node cget $node -attributes]] {
			set attrNode [element getAttributeNode $node $attrName]
			if {[llength $attrNode]} {
			    lappend result $attrNode
			}
		    }
		}
	    }
	}

	default {
	    return -code error "axis \"[lindex $step 0]\" is not supported"
	}
    }

    # Now apply predicates
    set result [XPath:ApplyPredicates $result [lindex $step 2]]

    # Apply the next location step
    return [XPath:SelectNode:Rel $result [lrange $spath 1 end]]
}

# dom::tcl::XPath:SN:GetElementTypeNodes --
#
#	Reduce nodeset to those nodes of element type
#
# Arguments:
#	nodeset	set of nodes
#
# Results:
#	Returns nodeset in which all nodes are element type

proc dom::tcl::XPath:SN:GetElementTypeNodes nodeset {
    set result {}
    foreach node $nodeset {
	switch [node cget $node -nodeType] {
	    document -
	    documentFragment -
	    element {
		lappend result $node
	    }
	    default {}
	}
    }
    return $result
}

# dom::tcl::XPath:SN:ApplyNodeTest --
#
#	Apply the node test to a node
#
# Arguments:
#	node	DOM node to test
#	test	node test
#
# Results:
#	1 if node passes, 0 otherwise

proc dom::tcl::XPath:SN:ApplyNodeTest {node test} {
    if {[llength $test] > 1} {
	foreach {name typetest} $test break
	# Node type test
	switch -glob -- $name,[node cget $node -nodeType] {
	    node,* {
		return 1
	    }
	    text,textNode -
	    comment,comment -
	    processing-instruction,processingInstruction {
		return 1
	    }
	    text,* -
	    comment,* -
	    processing-instruction,* {
		return 0
	    }
	    default {
		return -code error "illegal node type test \"[lindex $step 1]\""
	    }
	}
    } else {
	# Node name test
	switch -glob -- $test,[node cget $node -nodeType],[node cget $node -nodeName] \
		\\*,element,* {
	    return 1
	} \
		\\*,* {
	    return 0
	} \
		*,element,$test {
	    return 1
	}
    }

    return 0
}

# dom::tcl::XPath:SN:DescendAndTest --
#
#	Descend the element hierarchy,
#	apply the node test as we go
#
# Arguments:
#	nodeset	nodes to be tested and descended
#	test	node test
#
# Results:
#	Returned nodeset of nodes which pass the test

proc dom::tcl::XPath:SN:DescendAndTest {nodeset test} {
    set result {}

    foreach node $nodeset {
	if {[XPath:SN:ApplyNodeTest $node $test]} {
	    lappend result $node
	}
	switch [node cget $node -nodeType] {
	    document -
	    documentFragment -
	    element {
		eval lappend result [XPath:SN:DescendAndTest [node children $node] $test]
	    }
	}
    }

    return $result
}

# dom::tcl::XPath:ApplyPredicates --
#
#	Filter a nodeset with predicates
#
# Arguments:
#	ctxt	current context nodeset
#	preds	list of predicates
#
# Results:
#	Returns new (possibly reduced) context nodeset

proc dom::tcl::XPath:ApplyPredicates {ctxt preds} {

    set result {}
    foreach node $ctxt {
	set passed 1
	foreach predicate $preds {
	    if {![XPath:ApplyPredicate $node $predicate]} {
		set passed 0
		break
	    }
	}
	if {$passed} {
	    lappend result $node
	}
    }

    return $result
}

# dom::tcl::XPath:ApplyPredicate --
#
#	Filter a node with a single predicate
#
# Arguments:
#	node	current context node
#	pred	predicate
#
# Results:
#	Returns boolean

proc dom::tcl::XPath:ApplyPredicate {node pred} {

    switch -- [lindex $pred 0] {
	= -
	!= -
	>= -
	<= -
	> -
	> {

	    if {[llength $pred] != 3} {
		return -code error "malformed expression"
	    }

	    set operand1 [XPath:Pred:ResolveExpr $node [lindex $pred 1]]
	    set operand2 [XPath:Pred:ResolveExpr $node [lindex $pred 2]]

	    # Convert operands to the correct type, if necessary
	    switch -glob [lindex $operand1 0],[lindex $operand2 0] {
		literal,literal {
		    return [XPath:Pred:CompareLiterals [lindex $pred 0] [lindex $operand1 1] [lindex $operand2 1]]
		}

		number,number -
		literal,number -
		number,literal {
		    # Compare as numbers
		    return [XPath:Pred:CompareNumbers [lindex $pred 0] [lindex $operand1 1] [lindex $operand2 1]]
		}

		boolean,boolean {
		    # Compare as booleans
		    return -code error "boolean comparison not yet implemented"
		}

		node,node {
		    # Nodeset comparison
		    return -code error "nodeset comparison not yet implemented"
		}

		node,* {
		    set value {}
		    if {[llength [lindex $operand1 1]]} {
			set value [node stringValue [lindex [lindex $operand1 1] 0]]
		    }
		    return [XPath:Pred:CompareLiterals [lindex $pred 0] $value [lindex $operand2 1]]
		}
		*,node {
		    set value {}
		    if {[llength [lindex $operand2 1]]} {
			set value [node stringValue [lindex [lindex $operand2 1] 0]]
		    }
		    return [XPath:Pred:CompareLiterals [lindex $pred 0] $value [lindex $operand1 1]]
		}

		default {
		    return -code error "can't compare [lindex $operand1 0] to [lindex $operand2 0]"
		}
	    }
	}

	function {
	    return -code error "invalid predicate"
	}
	number -
	literal {
	    return -code error "invalid predicate"
	}

	path {
	    set nodeset [XPath:SelectNode:Rel $node [lindex $pred 1]]
	    return [expr {[llength $nodeset] > 0 ? 1 : 0}]
	}

    }

    return 1
}

# dom::tcl::XPath:Pred:Compare --

proc dom::tcl::XPath:Pred:CompareLiterals {op operand1 operand2} {
    set result [string compare $operand1 $operand2]

    # The obvious:
    #return [expr {$result $opMap($op) 0}]
    # doesn't compile
    
    switch $op {
	= {
	    return [expr {$result == 0}]
	}
	!= {
	    return [expr {$result != 0}]
	}
	<= {
	    return [expr {$result <= 0}]
	}
	>= {
	    return [expr {$result >= 0}]
	}
	< {
	    return [expr {$result < 0}]
	}
	> {
	    return [expr {$result > 0}]
	}
    }
    return -code error "internal error"
}

# dom::tcl::XPath:Pred:ResolveExpr --

proc dom::tcl::XPath:Pred:ResolveExpr {node expr} {

    switch [lindex $expr 0] {
	path {
	    return [list node [XPath:SelectNode:Rel $node [lindex $expr 1]]]
	}

	function -
	group {
	    return -code error "[lindex $expr 0] not yet implemented"
	}
	literal -
	number -
	boolean {
	    return $expr
	}

	default {
	    return -code error "internal error"
	}
    }

    return {}
}

#################################################
#
# Miscellaneous
#
#################################################

# dom::tcl::hasmixedcontent --
#
#	Determine whether an element contains mixed content
#
# Arguments:
#	token	dom node
#
# Results:
#	Returns 1 if element contains mixed content,
#	0 otherwise

proc dom::tcl::hasmixedcontent token {
    upvar #0 $token node

    if {[string compare $node(node:nodeType) "element"]} {
	# Really undefined
	return 0
    }

    foreach child [set $node(node:childNodes)] {
	upvar #0 $child childnode
	if {![string compare $childnode(node:nodeType) "textNode"]} {
	    return 1
	}
    }

    return 0
}

# dom::tcl::prefix2namespaceURI --
#
#	Given an XML Namespace prefix, find the corresponding Namespace URI
#
# Arguments:
#	node	DOM Node
#	prefix	XML Namespace prefix
#
# Results:
#	Returns URI

proc dom::tcl::prefix2namespaceURI {node prefix} {

    # Search this node and its ancestors for the appropriate
    # XML Namespace declaration

    set parent [dom::node parent $node]
    set nsuri [dom::element getAttributeNS $node $::dom::xmlnsURI $prefix]
    if {[string length $parent] && ![string length $nsuri]} {
	set nsuri [dom::element getAttributeNS $parent $::dom::xmlnsURI $prefix]
	set parent [dom::node parent $parent]
    }

    if {[string length $nsuri]} {
	return $nsuri
    } else {
	return -code error "unable to find namespace URI for prefix \"$prefix\""
    }

}

# dom::tcl::namespaceURI2prefix --
#
#	Given an XML Namespace URI, find the corresponding prefix
#
# Arguments:
#	node	DOM Node
#	nsuri	XML Namespace URI
#
# Results:
#	Returns prefix

proc dom::tcl::namespaceURI2prefix {node nsuri} {

    # Search this node and its ancestors for the desired
    # XML Namespace declaration

    set found 0
    set prefix {}
    set parent [dom::node parent $node]
    while {[string length $parent]} {
	upvar #0 $node nodeinfo
	catch {unset attrs}
	array set attrs [array get $nodeinfo(element:attributeList)]
	foreach {nsdecl declNSuri} [array get attrs ${::dom::xmlnsURI}^*] {
	    if {![string compare $declNSuri $nsuri]} {
		set found 1
		set prefix [lindex [split $nsdecl ^] 1]
		break
	    }
	}
	if {$found} {
	    break
	}
	set node $parent
	set parent [dom::node parent $node]
    }

    if {$found} {
	return $prefix
    } else {
	return -code error "unable to find prefix for namespace URI \"$nsuri\""
    }

}

# dom::tcl::GetField --
#
#	Return a value, or empty string if not defined
#
# Arguments:
#	var	name of variable to return
#
# Results:
#	Returns the value, or empty string if variable is not defined.

proc dom::tcl::GetField var {
    upvar $var v
    if {[info exists v]} {
	return $v
    } else {
	return {}
    }
}

# dom::tcl::Min --
#
#	Return the minimum of two numeric values
#
# Arguments:
#	a	a value
#	b	another value
#
# Results:
#	Returns the value which is lower than the other.

proc dom::tcl::Min {a b} {
    return [expr {$a < $b ? $a : $b}]
}

# dom::tcl::Max --
#
#	Return the maximum of two numeric values
#
# Arguments:
#	a	a value
#	b	another value
#
# Results:
#	Returns the value which is greater than the other.

proc dom::tcl::Max {a b} {
    return [expr {$a > $b ? $a : $b}]
}

# dom::tcl::Boolean --
#
#	Return a boolean value
#
# Arguments:
#	b	value
#
# Results:
#	Returns 0 or 1

proc dom::tcl::Boolean b {
    regsub -nocase {^(true|yes|1|on)$} $b 1 b
    regsub -nocase {^(false|no|0|off)$} $b 0 b
    return $b
}

