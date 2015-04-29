# tclparser-8.1.tcl --
#
#	This file provides a Tcl implementation of a XML parser.
#	This file supports Tcl 8.1.
#
#	See xml-8.[01].tcl for definitions of character sets and
#	regular expressions.
#
# Copyright (c) 2005-2008 by Explain.
# http://www.explain.com.au/
# Copyright (c) 1998-2003 Zveno Pty Ltd
# http://www.zveno.com/
# 
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: tclparser-8.1.tcl,v 1.26.2.1 2005/12/28 06:49:51 balls Exp $

package require Tcl 8.1

package provide xml::tclparser 3.2

package require xmldefs 3.2

package require sgmlparser 1.0

namespace eval xml::tclparser {

    namespace export create createexternal externalentity parse configure get delete

    # Tokenising expressions

    variable tokExpr $::xml::tokExpr
    variable substExpr $::xml::substExpr

    # Register this parser class

    ::xml::parserclass create tcl \
	    -createcommand [namespace code create] \
	    -createentityparsercommand [namespace code createentityparser] \
	    -parsecommand [namespace code parse] \
	    -configurecommand [namespace code configure] \
	    -deletecommand [namespace code delete] \
	    -resetcommand [namespace code reset]
}

# xml::tclparser::create --
#
#	Creates XML parser object.
#
# Arguments:
#	name	unique identifier for this instance
#
# Results:
#	The state variable is initialised.

proc xml::tclparser::create name {

    # Initialise state variable
    upvar \#0 [namespace current]::$name parser
    array set parser [list -name $name			\
	-cmd [uplevel 3 namespace current]::$name	\
	-final 1					\
	-validate 0					\
	-statevariable [namespace current]::$name	\
	-baseuri {}					\
	internaldtd {}					\
	entities [namespace current]::Entities$name	\
	extentities [namespace current]::ExtEntities$name	\
	parameterentities [namespace current]::PEntities$name	\
	externalparameterentities [namespace current]::ExtPEntities$name	\
	elementdecls [namespace current]::ElDecls$name	\
	attlistdecls [namespace current]::AttlistDecls$name	\
	notationdecls [namespace current]::NotDecls$name	\
	depth 0						\
	leftover {}                                     \
    ]

    # Initialise entities with predefined set
    array set [namespace current]::Entities$name [array get ::sgml::EntityPredef]

    return $parser(-cmd)
}

# xml::tclparser::createentityparser --
#
#	Creates XML parser object for an entity.
#
# Arguments:
#	name	name for the new parser
#	parent	name of parent parser
#
# Results:
#	The state variable is initialised.

proc xml::tclparser::createentityparser {parent name} {
    upvar #0 [namespace current]::$parent p

    # Initialise state variable
    upvar \#0 [namespace current]::$name external
    array set external [array get p]

    regsub $parent $p(-cmd) {} parentns

    array set external [list -name $name		\
	-cmd $parentns$name				\
	-statevariable [namespace current]::$name	\
	internaldtd {}					\
	line 0						\
    ]
    incr external(depth)

    return $external(-cmd)
}

# xml::tclparser::configure --
#
#	Configures a XML parser object.
#
# Arguments:
#	name	unique identifier for this instance
#	args	option name/value pairs
#
# Results:
#	May change values of config options

proc xml::tclparser::configure {name args} {
    upvar \#0 [namespace current]::$name parser

    # BUG: very crude, no checks for illegal args
    # Mats: Should be synced with sgmlparser.tcl
    set options {-elementstartcommand -elementendcommand \
      -characterdatacommand -processinginstructioncommand \
      -externalentitycommand -xmldeclcommand \
      -doctypecommand -commentcommand \
      -entitydeclcommand -unparsedentitydeclcommand \
      -parameterentitydeclcommand -notationdeclcommand \
      -elementdeclcommand -attlistdeclcommand \
      -paramentityparsing -defaultexpandinternalentities \
      -startdoctypedeclcommand -enddoctypedeclcommand \
      -entityreferencecommand -warningcommand \
      -defaultcommand -unknownencodingcommand -notstandalonecommand \
      -startcdatasectioncommand -endcdatasectioncommand \
      -errorcommand -final \
      -validate -baseuri -baseurl \
      -name -cmd -emptyelement \
      -parseattributelistcommand -parseentitydeclcommand \
      -normalize -internaldtd -dtdsubset \
      -reportempty -ignorewhitespace \
      -reportempty \
    }
    set usage [join $options ", "]
    regsub -all -- - $options {} options
    set pat ^-([join $options |])$
    foreach {flag value} $args {
	if {[regexp $pat $flag]} {
	    # Validate numbers
	    if {[info exists parser($flag)] && \
		    [string is integer -strict $parser($flag)] && \
		    ![string is integer -strict $value]} {
		return -code error "Bad value for $flag ($value), must be integer"
	    }
	    set parser($flag) $value
	} else {
	    return -code error "Unknown option $flag, can be: $usage"
	}
    }

    # Backward-compatibility: -baseuri is a synonym for -baseurl
    catch {set parser(-baseuri) $parser(-baseurl)}

    return {}
}

# xml::tclparser::parse --
#
#	Parses document instance data
#
# Arguments:
#	name	parser object
#	xml	data
#	args	configuration options
#
# Results:
#	Callbacks are invoked

proc xml::tclparser::parse {name xml args} {

    array set options $args
    upvar \#0 [namespace current]::$name parser
    variable tokExpr
    variable substExpr

    # Mats:
    if {[llength $args]} {
	eval {configure $name} $args
    }

    set parseOptions [list \
	    -emptyelement [namespace code ParseEmpty] \
	    -parseattributelistcommand [namespace code ParseAttrs] \
	    -parseentitydeclcommand [namespace code ParseEntity] \
	    -normalize 0]
    eval lappend parseOptions \
	    [array get parser -*command] \
	    [array get parser -reportempty] \
	    [array get parser -ignorewhitespace] \
	    [array get parser -name] \
	    [array get parser -cmd] \
	    [array get parser -baseuri] \
	    [array get parser -validate] \
	    [array get parser -final] \
	    [array get parser -defaultexpandinternalentities] \
	    [array get parser entities] \
	    [array get parser extentities] \
	    [array get parser parameterentities] \
	    [array get parser externalparameterentities] \
	    [array get parser elementdecls] \
	    [array get parser attlistdecls] \
	    [array get parser notationdecls]

    # Mats:
    # If -final 0 we also need to maintain the state with a -statevariable !
    if {!$parser(-final)} {
	eval lappend parseOptions [array get parser -statevariable]
    }

    set dtdsubset no
    catch {set dtdsubset $options(-dtdsubset)}
    switch -- $dtdsubset {
	internal {
	    # Bypass normal parsing
	    lappend parseOptions -statevariable $parser(-statevariable)
	    array set intOptions [array get ::sgml::StdOptions]
	    array set intOptions $parseOptions
	    ::sgml::ParseDTD:Internal [array get intOptions] $xml
	    return {}
	}
	external {
	    # Bypass normal parsing
	    lappend parseOptions -statevariable $parser(-statevariable)
	    array set intOptions [array get ::sgml::StdOptions]
	    array set intOptions $parseOptions
	    ::sgml::ParseDTD:External [array get intOptions] $xml
	    return {}
	}
	default {
	    # Pass through to normal processing
	}
    }

    lappend tokenOptions  \
      -internaldtdvariable [namespace current]::${name}(internaldtd)
    
    # Mats: If -final 0 we also need to maintain the state with a -statevariable !
    if {!$parser(-final)} {
	eval lappend tokenOptions [array get parser -statevariable] \
	  [array get parser -final]
    }
    
    # Mats:
    # Why not the first four? Just padding? Lrange undos \n interp.
    # It is necessary to have the first four as well if chopped off in
    # middle of pcdata.
    set tokenised [lrange \
	    [eval {::sgml::tokenise $xml $tokExpr $substExpr} $tokenOptions] \
	0 end]

    lappend parseOptions -internaldtd [list $parser(internaldtd)]
    eval ::sgml::parseEvent [list $tokenised] $parseOptions

    return {}
}

# xml::tclparser::ParseEmpty --  Tcl 8.1+ version
#
#	Used by parser to determine whether an element is empty.
#	This is usually dead easy in XML, but as always not quite.
#	Have to watch out for empty element syntax
#
# Arguments:
#	tag	element name
#	attr	attribute list (raw)
#	e	End tag delimiter.
#
# Results:
#	Return value of e

proc xml::tclparser::ParseEmpty {tag attr e} {
    switch -glob [string length $e],[regexp "/[::xml::cl $::xml::Wsp]*$" $attr] {
	0,0 {
	    return {}
	}
	0,* {
	    return /
	}
	default {
	    return $e
	}
    }
}

# xml::tclparser::ParseAttrs -- Tcl 8.1+ version
#
#	Parse element attributes.
#
# There are two forms for name-value pairs:
#
#	name="value"
#	name='value'
#
# Arguments:
#	opts	parser options
#	attrs	attribute string given in a tag
#
# Results:
#	Returns a Tcl list representing the name-value pairs in the 
#	attribute string
#
#	A ">" occurring in the attribute list causes problems when parsing
#	the XML.  This manifests itself by an unterminated attribute value
#	and a ">" appearing the element text.
#	In this case return a three element list;
#	the message "unterminated attribute value", the attribute list it
#	did manage to parse and the remainder of the attribute list.

proc xml::tclparser::ParseAttrs {opts attrs} {

    set result {}

    while {[string length [string trim $attrs]]} {
	if {[regexp [::sgml::cl $::xml::Wsp]*($::xml::Name)[::sgml::cl $::xml::Wsp]*=[::sgml::cl $::xml::Wsp]*("|')([::sgml::cl ^<]*?)\\2(.*) $attrs discard attrName delimiter value attrs]} {
	    lappend result $attrName [NormalizeAttValue $opts $value]
	} elseif {[regexp [::sgml::cl $::xml::Wsp]*$::xml::Name[::sgml::cl $::xml::Wsp]*=[::sgml::cl $::xml::Wsp]*("|')[::sgml::cl ^<]*\$ $attrs]} {
	    return -code error [list {unterminated attribute value} $result $attrs]
	} else {
	    return -code error "invalid attribute list"
	}
    }

    return $result
}

# xml::tclparser::NormalizeAttValue --
#
#	Perform attribute value normalisation.  This involves:
#	. character references are appended to the value
#	. entity references are recursively processed and replacement value appended
#	. whitespace characters cause a space to be appended
#	. other characters appended as-is
#
# Arguments:
#	opts	parser options
#	value	unparsed attribute value
#
# Results:
#	Normalised value returned.

proc xml::tclparser::NormalizeAttValue {opts value} {

    # sgmlparser already has backslashes protected
    # Protect Tcl specials
    regsub -all {([][$])} $value {\\\1} value

    # Deal with white space
    regsub -all "\[$::xml::Wsp\]" $value { } value

    # Find entity refs
    regsub -all {&([^;]+);} $value {[NormalizeAttValue:DeRef $opts {\1}]} value

    return [subst $value]
}

# xml::tclparser::NormalizeAttValue:DeRef --
#
#	Handler to normalize attribute values
#
# Arguments:
#	opts	parser options
#	ref	entity reference
#
# Results:
#	Returns character

proc xml::tclparser::NormalizeAttValue:DeRef {opts ref} {
    # SRB: Bug fix 2008-11-18 #812051: surround case labels in braces for compatibility with Freewrap
    switch -glob -- $ref {
	{#x*} {
	    scan [string range $ref 2 end] %x value
	    set char [format %c $value]
	    # Check that the char is legal for XML
	    if {[regexp [format {^[%s]$} $::xml::Char] $char]} {
		return $char
	    } else {
		return -code error "illegal character"
	    }
	}
	{#*} {
	    scan [string range $ref 1 end] %d value
	    set char [format %c $value]
	    # Check that the char is legal for XML
	    if {[regexp [format {^[%s]$} $::xml::Char] $char]} {
		return $char
	    } else {
		return -code error "illegal character"
	    }
	}
	lt -
	gt -
	amp -
	quot -
	apos {
	    array set map {lt < gt > amp & quot \" apos '}
	    return $map($ref)
	}
	default {
	    # A general entity.  Must resolve to a text value - no element structure.

	    array set options $opts
	    upvar #0 $options(entities) map

	    if {[info exists map($ref)]} {

		if {[regexp < $map($ref)]} {
		    return -code error "illegal character \"<\" in attribute value"
		}

		if {![regexp & $map($ref)]} {
		    # Simple text replacement
		    return $map($ref)
		}

		# There are entity references in the replacement text.
		# Can't use child entity parser since must catch element structures

		return [NormalizeAttValue $opts $map($ref)]

	    } elseif {[string compare $options(-entityreferencecommand) "::sgml::noop"]} {

		set result [uplevel #0 $options(-entityreferencecommand) [list $ref]]

		return $result

	    } else {
		return -code error "unable to resolve entity reference \"$ref\""
	    }
	}
    }
}

# xml::tclparser::ParseEntity --
#
#	Parse general entity declaration
#
# Arguments:
#	data	text to parse
#
# Results:
#	Tcl list containing entity declaration

proc xml::tclparser::ParseEntity data {
    set data [string trim $data]
    if {[regexp $::sgml::ExternalEntityExpr $data discard type delimiter1 id1 discard delimiter2 id2 optNDATA ndata]} {
	switch $type {
	    PUBLIC {
		return [list external $id2 $id1 $ndata]
	    }
	    SYSTEM {
		return [list external $id1 {} $ndata]
	    }
	}
    } elseif {[regexp {^("|')(.*?)\1$} $data discard delimiter value]} {
	return [list internal $value]
    } else {
	return -code error "badly formed entity declaration"
    }
}

# xml::tclparser::delete --
#
#	Destroy parser data
#
# Arguments:
#	name	parser object
#
# Results:
#	Parser data structure destroyed

proc xml::tclparser::delete name {
    upvar \#0 [namespace current]::$name parser
    catch {::sgml::ParserDelete $parser(-statevariable)}
    catch {unset parser}
    return {}
}

# xml::tclparser::get --
#
#	Retrieve additional information from the parser
#
# Arguments:
#	name	parser object
#	method	info to retrieve
#	args	additional arguments for method
#
# Results:
#	Depends on method

proc xml::tclparser::get {name method args} {
    upvar #0 [namespace current]::$name parser

    switch -- $method {

	elementdecl {
	    switch [llength $args] {

		0 {
		    # Return all element declarations
		    upvar #0 $parser(elementdecls) elements
		    return [array get elements]
		}

		1 {
		    # Return specific element declaration
		    upvar #0 $parser(elementdecls) elements
		    if {[info exists elements([lindex $args 0])]} {
			return [array get elements [lindex $args 0]]
		    } else {
			return -code error "element \"[lindex $args 0]\" not declared"
		    }
		}

		default {
		    return -code error "wrong number of arguments: should be \"elementdecl ?element?\""
		}
	    }
	}

	attlist {
	    if {[llength $args] != 1} {
		return -code error "wrong number of arguments: should be \"get attlist element\""
	    }

	    upvar #0 $parser(attlistdecls)

	    return {}
	}

	entitydecl {
	}

	parameterentitydecl {
	}

	notationdecl {
	}

	default {
	    return -code error "unknown method \"$method\""
	}
    }

    return {}
}

# xml::tclparser::ExternalEntity --
#
#	Resolve and parse external entity
#
# Arguments:
#	name	parser object
#	base	base URL
#	sys	system identifier
#	pub	public identifier
#
# Results:
#	External entity is fetched and parsed

proc xml::tclparser::ExternalEntity {name base sys pub} {
}

# xml::tclparser:: --
#
#	Reset a parser instance, ready to parse another document
#
# Arguments:
#	name	parser object
#
# Results:
#	Variables unset

proc xml::tclparser::reset {name} {
    upvar \#0 [namespace current]::$name parser

    # Has this parser object been properly initialised?
    if {![info exists parser] || \
	    ![info exists parser(-name)]} {
	return [create $name]
    }

    array set parser {
	-final 1
	depth 0
	leftover {}
    }

    foreach var {Entities ExtEntities PEntities ExtPEntities ElDecls AttlistDecls NotDecls} {
	catch {unset [namespace current]::${var}$name}
    }

    # Initialise entities with predefined set
    array set [namespace current]::Entities$name [array get ::sgml::EntityPredef]

    return {}
}
