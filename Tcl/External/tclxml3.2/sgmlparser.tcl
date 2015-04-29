# sgmlparser.tcl --
#
#	This file provides the generic part of a parser for SGML-based
#	languages, namely HTML and XML.
#
#	NB.  It is a misnomer.  There is no support for parsing
#	arbitrary SGML as such.
#
#	See sgml.tcl for variable definitions.
#
# Copyright (c) 2008 Explain
# http://www.explain.com.au/
# Copyright (c) 1998-2003 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: sgmlparser.tcl,v 1.32 2003/12/09 04:43:15 balls Exp $

package require sgml 1.9

package require uri 1.1

package provide sgmlparser 1.1

namespace eval sgml {
    namespace export tokenise parseEvent

    namespace export parseDTD

    # NB. Most namespace variables are defined in sgml-8.[01].tcl
    # to account for differences between versions of Tcl.
    # This especially includes the regular expressions used.

    variable ParseEventNum
    if {![info exists ParseEventNum]} {
	set ParseEventNum 0
    }
    variable ParseDTDnum
    if {![info exists ParseDTDNum]} {
	set ParseDTDNum 0
    }

    variable declExpr [cl $::sgml::Wsp]*([cl ^$::sgml::Wsp]+)[cl $::sgml::Wsp]*([cl ^>]*)
    variable EntityExpr [cl $::sgml::Wsp]*(%[cl $::sgml::Wsp])?[cl $::sgml::Wsp]*($::sgml::Name)[cl $::sgml::Wsp]+(.*)

    #variable MarkupDeclExpr <([cl ^$::sgml::Wsp>]+)[cl $::sgml::Wsp]*([cl ^$::sgml::Wsp]+)[cl $::sgml::Wsp]*([cl ^>]*)>
    #variable MarkupDeclSub "} {\\1} {\\2} {\\3} {"
    variable MarkupDeclExpr <[cl $::sgml::Wsp]*([cl ^$::sgml::Wsp>]+)[cl $::sgml::Wsp]*([cl ^>]*)>
    variable MarkupDeclSub "\} {\\1} {\\2} \{"

    variable ExternalEntityExpr ^(PUBLIC|SYSTEM)[cl $::sgml::Wsp]+("|')(.*?)\\2([cl $::sgml::Wsp]+("|')(.*?)\\2)?([cl $::sgml::Wsp]+NDATA[cl $::sgml::Wsp]+($::xml::Name))?\$

    variable StdOptions
    array set StdOptions [list \
	-elementstartcommand		[namespace current]::noop	\
	-elementendcommand		[namespace current]::noop	\
	-characterdatacommand		[namespace current]::noop	\
	-processinginstructioncommand	[namespace current]::noop	\
	-externalentitycommand		{}				\
	-xmldeclcommand			[namespace current]::noop	\
	-doctypecommand			[namespace current]::noop	\
	-commentcommand			[namespace current]::noop	\
	-entitydeclcommand		[namespace current]::noop	\
	-unparsedentitydeclcommand	[namespace current]::noop	\
	-parameterentitydeclcommand	[namespace current]::noop	\
	-notationdeclcommand		[namespace current]::noop	\
	-elementdeclcommand		[namespace current]::noop	\
	-attlistdeclcommand		[namespace current]::noop	\
	-paramentityparsing		1				\
	-defaultexpandinternalentities	1				\
	-startdoctypedeclcommand	[namespace current]::noop	\
	-enddoctypedeclcommand		[namespace current]::noop	\
	-entityreferencecommand		{}				\
	-warningcommand			[namespace current]::noop	\
	-errorcommand			[namespace current]::Error	\
	-final				1				\
	-validate			0				\
	-baseuri			{}				\
	-name				{}				\
	-cmd				{}				\
	-emptyelement			[namespace current]::EmptyElement	\
	-parseattributelistcommand	[namespace current]::noop	\
	-parseentitydeclcommand		[namespace current]::noop	\
	-normalize			1				\
	-internaldtd			{}				\
	-reportempty			0				\
	-ignorewhitespace		0				\
    ]
}

# sgml::tokenise --
#
#	Transform the given HTML/XML text into a Tcl list.
#
# Arguments:
#	sgml		text to tokenize
#	elemExpr	RE to recognise tags
#	elemSub		transform for matched tags
#	args		options
#
# Valid Options:
#       -internaldtdvariable
#	-final		boolean		True if no more data is to be supplied
#	-statevariable	varName		Name of a variable used to store info
#
# Results:
#	Returns a Tcl list representing the document.

proc sgml::tokenise {sgml elemExpr elemSub args} {
    array set options {-final 1}
    array set options $args
    set options(-final) [Boolean $options(-final)]

    # If the data is not final then there must be a variable to store
    # unused data.
    if {!$options(-final) && ![info exists options(-statevariable)]} {
	return -code error {option "-statevariable" required if not final}
    }

    # Pre-process stage
    #
    # Extract the internal DTD subset, if any

    catch {upvar #0 $options(-internaldtdvariable) dtd}
    if {[regexp {<!DOCTYPE[^[<]+\[([^]]+)\]} $sgml discard dtd]} {
	regsub {(<!DOCTYPE[^[<]+)(\[[^]]+\])} $sgml {\1\&xml:intdtd;} sgml
    }

    # Protect Tcl special characters
    regsub -all {([{}\\])} $sgml {\\\1} sgml

    # Do the translation

    if {[info exists options(-statevariable)]} {
	# Mats: Several rewrites here to handle -final 0 option.
	# If any cached unparsed xml (state(leftover)), prepend it.
	upvar #0 $options(-statevariable) state
	if {[string length $state(leftover)]} {
	    regsub -all $elemExpr $state(leftover)$sgml $elemSub sgml
	    set state(leftover) {}
	} else {
	    regsub -all $elemExpr $sgml $elemSub sgml
	}
	set sgml "{} {} {} \{$sgml\}"

	# Performance note (Tcl 8.0):
	#	Use of lindex, lreplace will cause parsing to list object

	# This RE only fixes chopped inside tags, not chopped text.
	if {[regexp {^([^<]*)(<[^>]*$)} [lindex $sgml end] x text rest]} {
	    set sgml [lreplace $sgml end end $text]
	    # Mats: unmatched stuff means that it is chopped off. Cache it for next round.
	    set state(leftover) $rest
	}

	# Patch from bug report #596959, Marshall Rose
	if {[string compare [lindex $sgml 4] ""]} {
	    set sgml [linsert $sgml 0 {} {} {} {} {}]
	}

    } else {

	# Performance note (Tcl 8.0):
	#	In this case, no conversion to list object is performed

	# Mats: This fails if not -final and $sgml is chopped off right in a tag.	
	regsub -all $elemExpr $sgml $elemSub sgml
	set sgml "{} {} {} \{$sgml\}"
    }

    return $sgml

}

# sgml::parseEvent --
#
#	Produces an event stream for a XML/HTML document,
#	given the Tcl list format returned by tokenise.
#
#	This procedure checks that the document is well-formed,
#	and throws an error if the document is found to be not
#	well formed.  Warnings are passed via the -warningcommand script.
#
#	The procedure only check for well-formedness,
#	no DTD is required.  However, facilities are provided for entity expansion.
#
# Arguments:
#	sgml		Instance data, as a Tcl list.
#	args		option/value pairs
#
# Valid Options:
#	-final			Indicates end of document data
#	-validate		Boolean to enable validation
#	-baseuri		URL for resolving relative URLs
#	-elementstartcommand	Called when an element starts
#	-elementendcommand	Called when an element ends
#	-characterdatacommand	Called when character data occurs
#	-entityreferencecommand	Called when an entity reference occurs
#	-processinginstructioncommand	Called when a PI occurs
#	-externalentitycommand	Called for an external entity reference
#
#	-xmldeclcommand		Called when the XML declaration occurs
#	-doctypecommand		Called when the document type declaration occurs
#	-commentcommand		Called when a comment occurs
#	-entitydeclcommand	Called when a parsed entity is declared
#	-unparsedentitydeclcommand	Called when an unparsed external entity is declared
#	-parameterentitydeclcommand	Called when a parameter entity is declared
#	-notationdeclcommand	Called when a notation is declared
#	-elementdeclcommand	Called when an element is declared
#	-attlistdeclcommand	Called when an attribute list is declared
#	-paramentityparsing	Boolean to enable/disable parameter entity substitution
#	-defaultexpandinternalentities	Boolean to enable/disable expansion of entities declared in internal DTD subset
#
#	-startdoctypedeclcommand	Called when the Doc Type declaration starts (see also -doctypecommand)
#	-enddoctypedeclcommand	Called when the Doc Type declaration ends (see also -doctypecommand)
#
#	-errorcommand		Script to evaluate for a fatal error
#	-warningcommand		Script to evaluate for a reportable warning
#	-statevariable		global state variable
#	-normalize		whether to normalize names
#	-reportempty		whether to include an indication of empty elements
#	-ignorewhitespace	whether to automatically strip whitespace
#
# Results:
#	The various callback scripts are invoked.
#	Returns empty string.
#
# BUGS:
#	If command options are set to empty string then they should not be invoked.

proc sgml::parseEvent {sgml args} {
    variable Wsp
    variable noWsp
    variable Nmtoken
    variable Name
    variable ParseEventNum
    variable StdOptions

    array set options [array get StdOptions]
    catch {array set options $args}

    # Mats:
    # If the data is not final then there must be a variable to persistently store the parse state.
    if {!$options(-final) && ![info exists options(-statevariable)]} {
	return -code error {option "-statevariable" required if not final}
    }
    
    foreach {opt value} [array get options *command] {
	if {[string compare $opt "-externalentitycommand"] && ![string length $value]} {
	    set options($opt) [namespace current]::noop
	}
    }

    if {![info exists options(-statevariable)]} {
	set options(-statevariable) [namespace current]::ParseEvent[incr ParseEventNum]
    }
    if {![info exists options(entities)]} {
	set options(entities) [namespace current]::Entities$ParseEventNum
	array set $options(entities) [array get [namespace current]::EntityPredef]
    }
    if {![info exists options(extentities)]} {
	set options(extentities) [namespace current]::ExtEntities$ParseEventNum
    }
    if {![info exists options(parameterentities)]} {
	set options(parameterentities) [namespace current]::ParamEntities$ParseEventNum
    }
    if {![info exists options(externalparameterentities)]} {
	set options(externalparameterentities) [namespace current]::ExtParamEntities$ParseEventNum
    }
    if {![info exists options(elementdecls)]} {
	set options(elementdecls) [namespace current]::ElementDecls$ParseEventNum
    }
    if {![info exists options(attlistdecls)]} {
	set options(attlistdecls) [namespace current]::AttListDecls$ParseEventNum
    }
    if {![info exists options(notationdecls)]} {
	set options(notationdecls) [namespace current]::NotationDecls$ParseEventNum
    }
    if {![info exists options(namespaces)]} {
	set options(namespaces) [namespace current]::Namespaces$ParseEventNum
    }

    # For backward-compatibility
    catch {set options(-baseuri) $options(-baseurl)}

    # Choose an external entity resolver

    if {![string length $options(-externalentitycommand)]} {
	if {$options(-validate)} {
	    set options(-externalentitycommand) [namespace code ResolveEntity]
	} else {
	    set options(-externalentitycommand) [namespace code noop]
	}
    }

    upvar #0 $options(-statevariable) state
    upvar #0 $options(entities) entities

    # Mats:
    # The problem is that the state is not maintained when -final 0 !
    # I've switched back to an older version here. 
    
    if {![info exists state(line)]} {
	# Initialise the state variable
	array set state {
	    mode normal
	    haveXMLDecl 0
	    haveDocElement 0
	    inDTD 0
	    context {}
	    stack {}
	    line 0
	    defaultNS {}
	    defaultNSURI {}
	}
    }

    foreach {tag close param text} $sgml {

	# Keep track of lines in the input
	incr state(line) [regsub -all \n $param {} discard]
	incr state(line) [regsub -all \n $text {} discard]

	# If the current mode is cdata or comment then we must undo what the
	# regsub has done to reconstitute the data

	set empty {}
	switch $state(mode) {
	    comment {
		# This had "[string length $param] && " as a guard -
		# can't remember why :-(
		if {[regexp ([cl ^-]*)--\$ $tag discard comm1]} {
		    # end of comment (in tag)
		    set tag {}
		    set close {}
		    set state(mode) normal
		    DeProtect1 $options(-commentcommand) $state(commentdata)<$comm1
		    unset state(commentdata)
		} elseif {[regexp ([cl ^-]*)--\$ $param discard comm1]} {
		    # end of comment (in attributes)
		    DeProtect1 $options(-commentcommand) $state(commentdata)<$close$tag>$comm1
		    unset state(commentdata)
		    set tag {}
		    set param {}
		    set close {}
		    set state(mode) normal
		} elseif {[regexp ([cl ^-]*)-->(.*) $text discard comm1 text]} {
		    # end of comment (in text)
		    DeProtect1 $options(-commentcommand) $state(commentdata)<$close$tag$param>$comm1
		    unset state(commentdata)
		    set tag {}
		    set param {}
		    set close {}
		    set state(mode) normal
		} else {
		    # comment continues
		    append state(commentdata) <$close$tag$param>$text
		    continue
		}
	    }
	    cdata {
		if {[string length $param] && [regexp ([cl ^\]]*)\]\][cl $Wsp]*\$ $tag discard cdata1]} {
		    # end of CDATA (in tag)
		    PCDATA [array get options] $state(cdata)<[subst -nocommand -novariable $close$cdata1]
		    set text [subst -novariable -nocommand $text]
		    set tag {}
		    unset state(cdata)
		    set state(mode) normal
		} elseif {[regexp ([cl ^\]]*)\]\][cl $Wsp]*\$ $param discard cdata1]} {
		    # end of CDATA (in attributes)
		    PCDATA [array get options] $state(cdata)<[subst -nocommand -novariable $close$tag$cdata1]
		    set text [subst -novariable -nocommand $text]
		    set tag {}
		    set param {}
		    unset state(cdata)
		    set state(mode) normal
		} elseif {[regexp (.*)\]\][cl $Wsp]*>(.*) $text discard cdata1 text]} {
		    # end of CDATA (in text)
		    PCDATA [array get options] $state(cdata)<[subst -nocommand -novariable $close$tag$param>$cdata1]
		    set text [subst -novariable -nocommand $text]
		    set tag {}
		    set param {}
		    set close {}
		    unset state(cdata)
		    set state(mode) normal
		} else {
		    # CDATA continues
		    append state(cdata) [subst -nocommand -novariable <$close$tag$param>$text]
		    continue
		}
	    }
	    continue {
		# We're skipping elements looking for the close tag
		switch -glob -- [string length $tag],[regexp {^\?|!.*} $tag],$close {
		    0,* {
			continue
		    }
		    *,0, {
			if {![string compare $tag $state(continue:tag)]} {
			    set empty [uplevel #0 $options(-emptyelement) [list $tag $param $empty]]
			    if {![string length $empty]} {
				incr state(continue:level)
			    }
			}
			continue
		    }
		    *,0,/ {
			if {![string compare $tag $state(continue:tag)]} {
			    incr state(continue:level) -1
			}
			if {!$state(continue:level)} {
			    unset state(continue:tag)
			    unset state(continue:level)
			    set state(mode) {}
			}
		    }
		    default {
			continue
		    }
		}
	    }
	    default {
		# The trailing slash on empty elements can't be automatically separated out
		# in the RE, so we must do it here.
		regexp (.*)(/)[cl $Wsp]*$ $param discard param empty
	    }
	}

	# default: normal mode

	# Bug: if the attribute list has a right angle bracket then the empty
	# element marker will not be seen

	set empty [uplevel #0 $options(-emptyelement) [list $tag $param $empty]]

	switch -glob -- [string length $tag],[regexp {^\?|!.*} $tag],$close,$empty {

	    0,0,, {
		# Ignore empty tag - dealt with non-normal mode above
	    }
	    *,0,, {

		# Start tag for an element.

		# Check if the internal DTD entity is in an attribute value
		regsub -all &xml:intdtd\; $param \[$options(-internaldtd)\] param

		set code [catch {ParseEvent:ElementOpen $tag $param [array get options]} msg]
		set state(haveDocElement) 1
		switch $code {
		    0 {# OK}
		    3 {
			# break
			return {}
		    }
		    4 {
			# continue
			# Remember this tag and look for its close
			set state(continue:tag) $tag
			set state(continue:level) 1
			set state(mode) continue
			continue
		    }
		    default {
			return -code $code -errorinfo $::errorInfo $msg
		    }
		}

	    }

	    *,0,/, {

		# End tag for an element.

		set code [catch {ParseEvent:ElementClose $tag [array get options]} msg]
		switch $code {
		    0 {# OK}
		    3 {
			# break
			return {}
		    }
		    4 {
			# continue
			# skip sibling nodes
			set state(continue:tag) [lindex $state(stack) end]
			set state(continue:level) 1
			set state(mode) continue
			continue
		    }
		    default {
			return -code $code -errorinfo $::errorInfo $msg
		    }
		}

	    }

	    *,0,,/ {

		# Empty element

		# The trailing slash sneaks through into the param variable
		regsub -all /[cl $::sgml::Wsp]*\$ $param {} param

		set code [catch {ParseEvent:ElementOpen $tag $param [array get options] -empty 1} msg]
		set state(haveDocElement) 1
		switch $code {
		    0 {# OK}
		    3 {
			# break
			return {}
		    }
		    4 {
			# continue
			# Pretty useless since it closes straightaway
		    }
		    default {
			return -code $code -errorinfo $::errorInfo $msg
		    }
		}
		set code [catch {ParseEvent:ElementClose $tag [array get options] -empty 1} msg]
		switch $code {
		    0 {# OK}
		    3 {
			# break
			return {}
		    }
		    4 {
			# continue
			# skip sibling nodes
			set state(continue:tag) [lindex $state(stack) end]
			set state(continue:level) 1
			set state(mode) continue
			continue
		    }
		    default {
			return -code $code -errorinfo $::errorInfo $msg
		    }
		}

	    }

	    *,1,* {
		# Processing instructions or XML declaration
		switch -glob -- $tag {

		    {\?xml} {
			# XML Declaration
			if {$state(haveXMLDecl)} {
			    uplevel #0 $options(-errorcommand) [list illegalcharacter "unexpected characters \"<$tag\" around line $state(line)"]
			} elseif {![regexp {\?$} $param]} {
			    uplevel #0 $options(-errorcommand) [list missingcharacters "XML Declaration missing characters \"?>\" around line $state(line)"]
			} else {

			    # We can do the parsing in one step with Tcl 8.1 RE's
			    # This has the benefit of performing better WF checking

			    set adv_re [format {^[%s]*version[%s]*=[%s]*("|')(-+|[a-zA-Z0-9_.:]+)\1([%s]+encoding[%s]*=[%s]*("|')([A-Za-z][-A-Za-z0-9._]*)\4)?([%s]*standalone[%s]*=[%s]*("|')(yes|no)\7)?[%s]*\?$} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp]

			    if {[catch {regexp $adv_re $param discard delimiter version discard delimiter encoding discard delimiter standalone} matches]} {
				# Otherwise we must fallback to 8.0.
				# This won't detect certain well-formedness errors

				# Get the version number
				if {[regexp [format {[%s]*version[%s]*=[%s]*"(-+|[a-zA-Z0-9_.:]+)"[%s]*} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp] $param discard version] || [regexp [format {[%s]*version[%s]*=[%s]*'(-+|[a-zA-Z0-9_.:]+)'[%s]*} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp] $param discard version]} {
				    if {[string compare $version "1.0"]} {
					# Should we support future versions?
					# At least 1.X?
					uplevel #0 $options(-errorcommand) [list versionincompatibility "document XML version \"$version\" is incompatible with XML version 1.0"]
				    }
				} else {
				    uplevel #0 $options(-errorcommand) [list missingversion "XML Declaration missing version information around line $state(line)"]
				}

				# Get the encoding declaration
				set encoding {}
				regexp [format {[%s]*encoding[%s]*=[%s]*"([A-Za-z]([A-Za-z0-9._]|-)*)"[%s]*} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp] $param discard encoding
				regexp [format {[%s]*encoding[%s]*=[%s]*'([A-Za-z]([A-Za-z0-9._]|-)*)'[%s]*} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp] $param discard encoding

				# Get the standalone declaration
				set standalone {}
				regexp [format {[%s]*standalone[%s]*=[%s]*"(yes|no)"[%s]*} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp] $param discard standalone
				regexp [format {[%s]*standalone[%s]*=[%s]*'(yes|no)'[%s]*} $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp $::sgml::Wsp] $param discard standalone

				# Invoke the callback
				uplevel #0 $options(-xmldeclcommand) [list $version $encoding $standalone]

			    } elseif {$matches == 0} {
				uplevel #0 $options(-errorcommand) [list illformeddeclaration "XML Declaration not well-formed around line $state(line)"]
			    } else {

				# Invoke the callback
				uplevel #0 $options(-xmldeclcommand) [list $version $encoding $standalone]

			    }

			}

		    }

		    {\?*} {
			# Processing instruction
			set tag [string range $tag 1 end]
			if {[regsub {\?$} $tag {} tag]} {
			    if {[string length [string trim $param]]} {
				uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$param\" in processing instruction around line $state(line)"]
			    }
			} elseif {![regexp ^$Name\$ $tag]} {
			    uplevel #0 $options(-errorcommand) [list illegalcharacter "illegal character in processing instruction target \"$tag\""]
			} elseif {[regexp {[xX][mM][lL]} $tag]} {
			    uplevel #0 $options(-errorcommand) [list illegalcharacters "characters \"xml\" not permitted in processing instruction target \"$tag\""]
			} elseif {![regsub {\?$} $param {} param]} {
			    uplevel #0 $options(-errorcommand) [list missingquestion "PI: expected '?' character around line $state(line)"]
			}
			set code [catch {uplevel #0 $options(-processinginstructioncommand) [list $tag [string trimleft $param]]} msg]
			switch $code {
			    0 {# OK}
			    3 {
				# break
				return {}
			    }
			    4 {
				# continue
				# skip sibling nodes
				set state(continue:tag) [lindex $state(stack) end]
				set state(continue:level) 1
				set state(mode) continue
				continue
			    }
			    default {
				return -code $code -errorinfo $::errorInfo $msg
			    }
			}
		    }

		    !DOCTYPE {
			# External entity reference
			# This should move into xml.tcl
			# Parse the params supplied.  Looking for Name, ExternalID and MarkupDecl
			set matched [regexp ^[cl $Wsp]*($Name)[cl $Wsp]*(.*) $param x state(doc_name) param]
			set state(doc_name) [Normalize $state(doc_name) $options(-normalize)]
			set externalID {}
			set pubidlit {}
			set systemlit {}
			set externalID {}
			if {[regexp -nocase ^[cl $Wsp]*(SYSTEM|PUBLIC)(.*) $param x id param]} {
			    switch [string toupper $id] {
				SYSTEM {
				    if {[regexp ^[cl $Wsp]+"([cl ^"]*)"(.*) $param x systemlit param] || [regexp ^[cl $Wsp]+'([cl ^']*)'(.*) $param x systemlit param]} {
					set externalID [list SYSTEM $systemlit] ;# "
				    } else {
					uplevel #0 $options(-errorcommand) {syntaxerror {syntax error: SYSTEM identifier not followed by literal}}
				    }
				}
				PUBLIC {
				    if {[regexp ^[cl $Wsp]+"([cl ^"]*)"(.*) $param x pubidlit param] || [regexp ^[cl $Wsp]+'([cl ^']*)'(.*) $param x pubidlit param]} {
					if {[regexp ^[cl $Wsp]+"([cl ^"]*)"(.*) $param x systemlit param] || [regexp ^[cl $Wsp]+'([cl ^']*)'(.*) $param x systemlit param]} {
					    set externalID [list PUBLIC $pubidlit $systemlit]
					} else {
					    uplevel #0 $options(-errorcommand) [list syntaxerror "syntax error: PUBLIC identifier not followed by system literal around line $state(line)"]
					}
				    } else {
					uplevel #0 $options(-errorcommand) [list syntaxerror "syntax error: PUBLIC identifier not followed by literal around line $state(line)"]
				    }
				}
			    }
			    if {[regexp -nocase ^[cl $Wsp]+NDATA[cl $Wsp]+($Name)(.*) $param x notation param]} {
				lappend externalID $notation
			    }
			}

			set state(inDTD) 1

			ParseEvent:DocTypeDecl [array get options] $state(doc_name) $pubidlit $systemlit $options(-internaldtd)

			set state(inDTD) 0

		    }

		    !--* {

			# Start of a comment
			# See if it ends in the same tag, otherwise change the
			# parsing mode

			regexp {!--(.*)} $tag discard comm1
			if {[regexp ([cl ^-]*)--[cl $Wsp]*\$ $comm1 discard comm1_1]} {
			    # processed comment (end in tag)
			    uplevel #0 $options(-commentcommand) [list $comm1_1]
			} elseif {[regexp ([cl ^-]*)--[cl $Wsp]*\$ $param discard comm2]} {
			    # processed comment (end in attributes)
			    uplevel #0 $options(-commentcommand) [list $comm1$comm2]
			} elseif {[regexp ([cl ^-]*)-->(.*) $text discard comm2 text]} {
			    # processed comment (end in text)
			    uplevel #0 $options(-commentcommand) [list $comm1$param$empty>$comm2]
			} else {
			    # start of comment
			    set state(mode) comment
			    set state(commentdata) "$comm1$param$empty>$text"
			    continue
			}
		    }

		    {!\[CDATA\[*} {

			regexp {!\[CDATA\[(.*)} $tag discard cdata1
			if {[regexp {(.*)]]$} $cdata1 discard cdata2]} {
			    # processed CDATA (end in tag)
			    PCDATA [array get options] [subst -novariable -nocommand $cdata2]
			    set text [subst -novariable -nocommand $text]
			} elseif {[regexp {(.*)]]$} $param discard cdata2]} {
			    # processed CDATA (end in attribute)
			    # Backslashes in param are quoted at this stage
			    PCDATA [array get options] $cdata1[subst -novariable -nocommand $cdata2]
			    set text [subst -novariable -nocommand $text]
			} elseif {[regexp {(.*)]]>(.*)} $text discard cdata2 text]} {
			    # processed CDATA (end in text)
			    # Backslashes in param and text are quoted at this stage
			    PCDATA [array get options] $cdata1[subst -novariable -nocommand $param]$empty>[subst -novariable -nocommand $cdata2]
			    set text [subst -novariable -nocommand $text]
			} else {
			    # start CDATA
			    set state(cdata) "$cdata1$param>$text"
			    set state(mode) cdata
			    continue
			}

		    }

		    !ELEMENT -
		    !ATTLIST -
		    !ENTITY -
		    !NOTATION {
			uplevel #0 $options(-errorcommand) [list illegaldeclaration "[string range $tag 1 end] declaration not expected in document instance around line $state(line)"]
		    }

		    default {
			uplevel #0 $options(-errorcommand) [list unknowninstruction "unknown processing instruction \"<$tag>\" around line $state(line)"]
		    }
		}
	    }
	    *,1,* -
	    *,0,/,/ {
		# Syntax error
	    	uplevel #0 $options(-errorcommand) [list syntaxerror "syntax error: closed/empty tag: tag $tag param $param empty $empty close $close around line $state(line)"]
	    }
	}

	# Process character data

	if {$state(haveDocElement) && [llength $state(stack)]} {

	    # Check if the internal DTD entity is in the text
	    regsub -all &xml:intdtd\; $text \[$options(-internaldtd)\] text

	    # Look for entity references
	    if {([array size entities] || \
		    [string length $options(-entityreferencecommand)]) && \
		    $options(-defaultexpandinternalentities) && \
		    [regexp {&[^;]+;} $text]} {

		# protect Tcl specials
		# NB. braces and backslashes may already be protected
		regsub -all {\\({|}|\\)} $text {\1} text
		regsub -all {([][$\\{}])} $text {\\\1} text

		# Mark entity references
		regsub -all {&([^;]+);} $text [format {%s; %s {\1} ; %s %s} \}\} [namespace code [list Entity [array get options] $options(-entityreferencecommand) [namespace code [list PCDATA [array get options]]] $options(entities)]] [namespace code [list DeProtect [namespace code [list PCDATA [array get options]]]]] \{\{] text
		set text "uplevel #0 [namespace code [list DeProtect1 [namespace code [list PCDATA [array get options]]]]] {{$text}}"
		eval $text
	    } else {

		# Restore protected special characters
		regsub -all {\\([][{}\\])} $text {\1} text
		PCDATA [array get options] $text
	    }
	} elseif {[string length [string trim $text]]} {
	    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$text\" in document prolog around line $state(line)"]
	}

    }

    # If this is the end of the document, close all open containers
    if {$options(-final) && [llength $state(stack)]} {
	eval $options(-errorcommand) [list unclosedelement "element [lindex $state(stack) end] remains unclosed around line $state(line)"]
    }

    return {}
}

# sgml::DeProtect --
#
#	Invoke given command after removing protecting backslashes
#	from given text.
#
# Arguments:
#	cmd	Command to invoke
#	text	Text to deprotect
#
# Results:
#	Depends on command

proc sgml::DeProtect1 {cmd text} {
    if {[string compare {} $text]} {
	regsub -all {\\([]$[{}\\])} $text {\1} text
	uplevel #0 $cmd [list $text]
    }
}
proc sgml::DeProtect {cmd text} {
    set text [lindex $text 0]
    if {[string compare {} $text]} {
	regsub -all {\\([]$[{}\\])} $text {\1} text
	uplevel #0 $cmd [list $text]
    }
}

# sgml::ParserDelete --
#
#	Free all memory associated with parser
#
# Arguments:
#	var	global state array
#
# Results:
#	Variables unset

proc sgml::ParserDelete var {
    upvar #0 $var state

    if {![info exists state]} {
	return -code error "unknown parser"
    }

    catch {unset $state(entities)}
    catch {unset $state(parameterentities)}
    catch {unset $state(elementdecls)}
    catch {unset $state(attlistdecls)}
    catch {unset $state(notationdecls)}
    catch {unset $state(namespaces)}

    unset state

    return {}
}

# sgml::ParseEvent:ElementOpen --
#
#	Start of an element.
#
# Arguments:
#	tag	Element name
#	attr	Attribute list
#	opts	Options
#	args	further configuration options
#
# Options:
#	-empty boolean
#		indicates whether the element was an empty element
#
# Results:
#	Modify state and invoke callback

proc sgml::ParseEvent:ElementOpen {tag attr opts args} {
    variable Name
    variable Wsp

    array set options $opts
    upvar #0 $options(-statevariable) state
    array set cfg {-empty 0}
    array set cfg $args
    set handleEmpty 0

    if {$options(-normalize)} {
	set tag [string toupper $tag]
    }

    # Update state
    lappend state(stack) $tag

    # Parse attribute list into a key-value representation
    if {[string compare $options(-parseattributelistcommand) {}]} {
	if {[catch {uplevel #0 $options(-parseattributelistcommand) [list $opts $attr]} attr]} {
	    if {[string compare [lindex $attr 0] "unterminated attribute value"]} {
		uplevel #0 $options(-errorcommand) [list unterminatedattribute "$attr around line $state(line)"]
		set attr {}
	    } else {

		# It is most likely that a ">" character was in an attribute value.
		# This manifests itself by ">" appearing in the element's text.
		# In this case the callback should return a three element list;
		# the message "unterminated attribute value", the attribute list it
		# did manage to parse and the remainder of the attribute list.

		foreach {msg attlist brokenattr} $attr break

		upvar text elemText
		if {[string first > $elemText] >= 0} {

		    # Now piece the attribute list back together
		    regexp [cl $Wsp]*($Name)[cl $Wsp]*=[cl $Wsp]*("|')(.*) $brokenattr discard attname delimiter attvalue
		    regexp (.*)>([cl ^>]*)\$ $elemText discard remattlist elemText
		    regexp ([cl ^$delimiter]*)${delimiter}(.*) $remattlist discard remattvalue remattlist

		    # Gotcha: watch out for empty element syntax
		    if {[string match */ [string trimright $remattlist]]} {
			set remattlist [string range $remattlist 0 end-1]
			set handleEmpty 1
			set cfg(-empty) 1
		    }

		    append attvalue >$remattvalue
		    lappend attlist $attname $attvalue

		    # Complete parsing the attribute list
		    if {[catch {uplevel #0 $options(-parseattributelistcommand) [list $options(-statevariable) $remattlist]} attr]} {
			uplevel #0 $options(-errorcommand) [list unterminatedattribute "$attr around line $state(line)"]
			set attr {}
			set attlist {}
		    } else {
			eval lappend attlist $attr
		    }

		    set attr $attlist

		} else {
		    uplevel #0 $options(-errorcommand) [list unterminatedattribute "$attr around line $state(line)"]
		    set attr {}
		}
	    }
	}
    }

    set empty {}
    if {$cfg(-empty) && $options(-reportempty)} {
	set empty {-empty 1}
    }

    # Check for namespace declarations
    upvar #0 $options(namespaces) namespaces
    set nsdecls {}
    if {[llength $attr]} {
	array set attrlist $attr
	foreach {attrName attrValue} [array get attrlist xmlns*] {
	    unset attrlist($attrName)
	    set colon [set prefix {}]
	    if {[regexp {^xmlns(:(.+))?$} $attrName discard colon prefix]} {
		switch -glob [string length $colon],[string length $prefix] {
		    0,0 {
			# default NS declaration
			lappend state(defaultNSURI) $attrValue
			lappend state(defaultNS) [llength $state(stack)]
			lappend nsdecls $attrValue {}
		    }
		    0,* {
			# Huh?
		    }
		    *,0 {
			# Error
			uplevel #0 $state(-warningcommand) "no prefix specified for namespace URI \"$attrValue\" in element \"$tag\""
		    }
		    default {
			set namespaces($prefix,[llength $state(stack)]) $attrValue
			lappend nsdecls $attrValue $prefix
		    }
		}
	    }
	}
	if {[llength $nsdecls]} {
	    set nsdecls [list -namespacedecls $nsdecls]
	}
	set attr [array get attrlist]
    }

    # Check whether this element has an expanded name
    set ns {}
    if {[regexp {([^:]+):(.*)$} $tag discard prefix tag]} {
	set nsspec [lsort -dictionary -decreasing [array names namespaces $prefix,*]]
	if {[llength $nsspec]} {
	    set nsuri $namespaces([lindex $nsspec 0])
	    set ns [list -namespace $nsuri]
	} else {
	    uplevel #0 $options(-errorcommand) [list namespaceundeclared "no namespace declared for prefix \"$prefix\" in element $tag"]
	}
    } elseif {[llength $state(defaultNSURI)]} {
	set ns [list -namespace [lindex $state(defaultNSURI) end]]
    }

    # Invoke callback
    set code [catch {uplevel #0 $options(-elementstartcommand) [list $tag $attr] $empty $ns $nsdecls} msg]

    # Sometimes empty elements must be handled here (see above)
    if {$code == 0 && $handleEmpty} {
	ParseEvent:ElementClose $tag $opts -empty 1
    }

    return -code $code -errorinfo $::errorInfo $msg
}

# sgml::ParseEvent:ElementClose --
#
#	End of an element.
#
# Arguments:
#	tag	Element name
#	opts	Options
#	args	further configuration options
#
# Options:
#	-empty boolean
#		indicates whether the element as an empty element
#
# Results:
#	Modify state and invoke callback

proc sgml::ParseEvent:ElementClose {tag opts args} {
    array set options $opts
    upvar #0 $options(-statevariable) state
    array set cfg {-empty 0}
    array set cfg $args

    # WF check
    if {[string compare $tag [lindex $state(stack) end]]} {
	uplevel #0 $options(-errorcommand) [list illegalendtag "end tag \"$tag\" does not match open element \"[lindex $state(stack) end]\" around line $state(line)"]
	return
    }

    # Check whether this element has an expanded name
    upvar #0 $options(namespaces) namespaces
    set ns {}
    if {[regexp {([^:]+):(.*)$} $tag discard prefix tag]} {
	set nsuri $namespaces([lindex [lsort -dictionary -decreasing [array names namespaces $prefix,*]] 0])
	set ns [list -namespace $nsuri]
    } elseif {[llength $state(defaultNSURI)]} {
	set ns [list -namespace [lindex $state(defaultNSURI) end]]
    }

    # Pop namespace stacks, if any
    if {[llength $state(defaultNS)]} {
	if {[llength $state(stack)] == [lindex $state(defaultNS) end]} {
	    set state(defaultNS) [lreplace $state(defaultNS) end end]
	}
    }
    foreach nsspec [array names namespaces *,[llength $state(stack)]] {
	unset namespaces($nsspec)
    }

    # Update state
    set state(stack) [lreplace $state(stack) end end]

    set empty {}
    if {$cfg(-empty) && $options(-reportempty)} {
	set empty {-empty 1}
    }

    # Invoke callback
    # Mats: Shall be same as sgml::ParseEvent:ElementOpen to handle exceptions in callback.
    set code [catch {uplevel #0 $options(-elementendcommand) [list $tag] $empty $ns} msg]
    return -code $code -errorinfo $::errorInfo $msg
}

# sgml::PCDATA --
#
#	Process PCDATA before passing to application
#
# Arguments:
#	opts	options
#	pcdata	Character data to be processed
#
# Results:
#	Checks that characters are legal,
#	checks -ignorewhitespace setting.

proc sgml::PCDATA {opts pcdata} {
    array set options $opts

    if {$options(-ignorewhitespace) && \
	    ![string length [string trim $pcdata]]} {
	return {}
    }

    if {![regexp ^[cl $::sgml::Char]*\$ $pcdata]} {
	upvar \#0 $options(-statevariable) state
	uplevel \#0 $options(-errorcommand) [list illegalcharacters "illegal, non-Unicode characters found in text \"$pcdata\" around line $state(line)"]
    }

    uplevel \#0 $options(-characterdatacommand) [list $pcdata]
}

# sgml::Normalize --
#
#	Perform name normalization if required
#
# Arguments:
#	name	name to normalize
#	req	normalization required
#
# Results:
#	Name returned as upper-case if normalization required

proc sgml::Normalize {name req} {
    if {$req} {
	return [string toupper $name]
    } else {
	return $name
    }
}

# sgml::Entity --
#
#	Resolve XML entity references (syntax: &xxx;).
#
# Arguments:
#	opts		options
#	entityrefcmd	application callback for entity references
#	pcdatacmd	application callback for character data
#	entities	name of array containing entity definitions.
#	ref		entity reference (the "xxx" bit)
#
# Results:
#	Returns substitution text for given entity.

proc sgml::Entity {opts entityrefcmd pcdatacmd entities ref} {
    array set options $opts
    upvar #0 $options(-statevariable) state

    if {![string length $entities]} {
	set entities [namespace current]::EntityPredef
    }

    # SRB: Bug fix 2008-11-18 #812051: surround case labels in braces for compatibility with Freewrap
    switch -glob -- $ref {
	{%*} {
	    # Parameter entity - not recognised outside of a DTD
	}
	{#x*} {
	    # Character entity - hex
	    if {[catch {format %c [scan [string range $ref 2 end] %x tmp; set tmp]} char]} {
		return -code error "malformed character entity \"$ref\""
	    }
	    uplevel #0 $pcdatacmd [list $char]

	    return {}

	}
	{#*} {
	    # Character entity - decimal
	    if {[catch {format %c [scan [string range $ref 1 end] %d tmp; set tmp]} char]} {
		return -code error "malformed character entity \"$ref\""
	    }
	    uplevel #0 $pcdatacmd [list $char]

	    return {}

	}
	default {
	    # General entity
	    upvar #0 $entities map
	    if {[info exists map($ref)]} {

		if {![regexp {<|&} $map($ref)]} {

		    # Simple text replacement - optimise
		    uplevel #0 $pcdatacmd [list $map($ref)]

		    return {}

		}

		# Otherwise an additional round of parsing is required.
		# This only applies to XML, since HTML doesn't have general entities

		# Must parse the replacement text for start & end tags, etc
		# This text must be self-contained: balanced closing tags, and so on

		set tokenised [tokenise $map($ref) $::xml::tokExpr $::xml::substExpr]
		set options(-final) 0
		eval parseEvent [list $tokenised] [array get options]

		return {}

	    } elseif {[string compare $entityrefcmd "::sgml::noop"]} {

		set result [uplevel #0 $entityrefcmd [list $ref]]

		if {[string length $result]} {
		    uplevel #0 $pcdatacmd [list $result]
		}

		return {}

	    } else {

		# Reconstitute entity reference

		uplevel #0 $options(-errorcommand) [list illegalentity "undefined entity reference \"$ref\""]

		return {}

	    }
	}
    }

    # If all else fails leave the entity reference untouched
    uplevel #0 $pcdatacmd [list &$ref\;]

    return {}
}

####################################
#
# DTD parser for SGML (XML).
#
# This DTD actually only handles XML DTDs.  Other language's
# DTD's, such as HTML, must be written in terms of a XML DTD.
#
####################################

# sgml::ParseEvent:DocTypeDecl --
#
#	Entry point for DTD parsing
#
# Arguments:
#	opts	configuration options
#	docEl	document element name
#	pubId	public identifier
#	sysId	system identifier (a URI)
#	intSSet	internal DTD subset

proc sgml::ParseEvent:DocTypeDecl {opts docEl pubId sysId intSSet} {
    array set options {}
    array set options $opts

    set code [catch {uplevel #0 $options(-doctypecommand) [list $docEl $pubId $sysId $intSSet]} err]
    switch $code {
	3 {
	    # break
	    return {}
	}
	0 -
	4 {
	    # continue
	}
	default {
	    return -code $code $err
	}
    }

    # Otherwise we'll parse the DTD and report it piecemeal

    # The internal DTD subset is processed first (XML 2.8)
    # During this stage, parameter entities are only allowed
    # between markup declarations

    ParseDTD:Internal [array get options] $intSSet

    # The external DTD subset is processed last (XML 2.8)
    # During this stage, parameter entities may occur anywhere

    # We must resolve the external identifier to obtain the
    # DTD data.  The application may supply its own resolver.

    if {[string length $pubId] || [string length $sysId]} {
	uplevel #0 $options(-externalentitycommand) [list $options(-cmd) $options(-baseuri) $sysId $pubId]
    }

    return {}
}

# sgml::ParseDTD:Internal --
#
#	Parse the internal DTD subset.
#
#	Parameter entities are only allowed between markup declarations.
#
# Arguments:
#	opts	configuration options
#	dtd	DTD data
#
# Results:
#	Markup declarations parsed may cause callback invocation

proc sgml::ParseDTD:Internal {opts dtd} {
    variable MarkupDeclExpr
    variable MarkupDeclSub

    array set options {}
    array set options $opts

    upvar #0 $options(-statevariable) state
    upvar #0 $options(parameterentities) PEnts
    upvar #0 $options(externalparameterentities) ExtPEnts

    # Bug 583947: remove comments before further processing
    regsub -all {<!--.*?-->} $dtd {} dtd

    # Tokenize the DTD

    # Protect Tcl special characters
    regsub -all {([{}\\])} $dtd {\\\1} dtd

    regsub -all $MarkupDeclExpr $dtd $MarkupDeclSub dtd

    # Entities may have angle brackets in their replacement
    # text, which breaks the RE processing.  So, we must
    # use a similar technique to processing doc instances
    # to rebuild the declarations from the pieces

    set mode {} ;# normal
    set delimiter {}
    set name {}
    set param {}

    set state(inInternalDTD) 1

    # Process the tokens
    foreach {decl value text} [lrange "{} {} \{$dtd\}" 3 end] {

	# Keep track of line numbers
	incr state(line) [regsub -all \n $text {} discard]

	ParseDTD:EntityMode [array get options] mode replText decl value text $delimiter $name $param

	ParseDTD:ProcessMarkupDecl [array get options] decl value delimiter name mode replText text param

	# There may be parameter entity references between markup decls

	if {[regexp {%.*;} $text]} {

	    # Protect Tcl special characters
	    regsub -all {([{}\\])} $text {\\\1} text

	    regsub -all %($::sgml::Name)\; $text "\} {\\1} \{" text

	    set PElist "\{$text\}"
	    set PElist [lreplace $PElist end end]
	    foreach {text entref} $PElist {
		if {[string length [string trim $text]]} {
		    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text in internal DTD subset around line $state(line)"]
		}

		# Expand parameter entity and recursively parse
		# BUG: no checks yet for recursive entity references

		if {[info exists PEnts($entref)]} {
		    set externalParser [$options(-cmd) entityparser]
		    $externalParser parse $PEnts($entref) -dtdsubset internal
		} elseif {[info exists ExtPEnts($entref)]} {
		    set externalParser [$options(-cmd) entityparser]
		    $externalParser parse $ExtPEnts($entref) -dtdsubset external
		    #$externalParser free
		} else {
		    uplevel #0 $options(-errorcommand) [list illegalreference "reference to undeclared parameter entity \"$entref\""]
		}
	    }

	}

    }

    return {}
}

# sgml::ParseDTD:EntityMode --
#
#	Perform special processing for various parser modes
#
# Arguments:
#	opts	configuration options
#	modeVar	pass-by-reference mode variable
#	replTextVar	pass-by-ref
#	declVar	pass-by-ref
#	valueVar	pass-by-ref
#	textVar	pass-by-ref
#	delimiter	delimiter currently in force
#	name
#	param
#
# Results:
#	Depends on current mode

proc sgml::ParseDTD:EntityMode {opts modeVar replTextVar declVar valueVar textVar delimiter name param} {
    upvar 1 $modeVar mode
    upvar 1 $replTextVar replText
    upvar 1 $declVar decl
    upvar 1 $valueVar value
    upvar 1 $textVar text
    array set options $opts

    switch $mode {
	{} {
	    # Pass through to normal processing section
	}
	entity {
	    # Look for closing delimiter
	    if {[regexp ([cl ^$delimiter]*)${delimiter}(.*) $decl discard val1 remainder]} {
		append replText <$val1
		DTD:ENTITY [array get options] $name [string trim $param] $delimiter$replText$delimiter
		set decl /
		set text $remainder\ $value>$text
		set value {}
		set mode {}
	    } elseif {[regexp ([cl ^$delimiter]*)${delimiter}(.*) $value discard val2 remainder]} {
		append replText <$decl\ $val2
		DTD:ENTITY [array get options] $name [string trim $param] $delimiter$replText$delimiter
		set decl /
		set text $remainder>$text
		set value {}
		set mode {}
	    } elseif {[regexp ([cl ^$delimiter]*)${delimiter}(.*) $text discard val3 remainder]} {
		append replText <$decl\ $value>$val3
		DTD:ENTITY [array get options] $name [string trim $param] $delimiter$replText$delimiter
		set decl /
		set text $remainder
		set value {}
		set mode {}
	    } else {

		# Remain in entity mode
		append replText <$decl\ $value>$text
		return -code continue

	    }
	}

	ignore {
	    upvar #0 $options(-statevariable) state

	    if {[regexp {]](.*)$} $decl discard remainder]} {
		set state(condSections) [lreplace $state(condSections) end end]
		set decl $remainder
		set mode {}
	    } elseif {[regexp {]](.*)$} $value discard remainder]} {
		set state(condSections) [lreplace $state(condSections) end end]
		regexp <[cl $::sgml::Wsp]*($::sgml::Name)(.*) $remainder discard decl value
		set mode {}
	    } elseif {[regexp {]]>(.*)$} $text discard remainder]} {
		set state(condSections) [lreplace $state(condSections) end end]
		set decl /
		set value {}
		set text $remainder
		#regexp <[cl $::sgml::Wsp]*($::sgml::Name)([cl ^>]*)>(.*) $remainder discard decl value text
		set mode {}
	    } else {
		set decl /
	    }

	}

	comment {
	    # Look for closing comment delimiter

	    upvar #0 $options(-statevariable) state

	    if {[regexp (.*?)--(.*)\$ $decl discard data1 remainder]} {
	    } elseif {[regexp (.*?)--(.*)\$ $value discard data1 remainder]} {
	    } elseif {[regexp (.*?)--(.*)\$ $text discard data1 remainder]} {
	    } else {
		# comment continues
		append state(commentdata) <$decl\ $value>$text
		set decl /
		set value {}
		set text {}
	    }
	}

    }

    return {}
}

# sgml::ParseDTD:ProcessMarkupDecl --
#
#	Process a single markup declaration
#
# Arguments:
#	opts	configuration options
#	declVar	pass-by-ref
#	valueVar	pass-by-ref
#	delimiterVar	pass-by-ref for current delimiter in force
#	nameVar	pass-by-ref
#	modeVar	pass-by-ref for current parser mode
#	replTextVar	pass-by-ref
#	textVar	pass-by-ref
#	paramVar	pass-by-ref
#
# Results:
#	Depends on markup declaration.  May change parser mode

proc sgml::ParseDTD:ProcessMarkupDecl {opts declVar valueVar delimiterVar nameVar modeVar replTextVar textVar paramVar} {
    upvar 1 $modeVar mode
    upvar 1 $replTextVar replText
    upvar 1 $textVar text
    upvar 1 $declVar decl
    upvar 1 $valueVar value
    upvar 1 $nameVar name
    upvar 1 $delimiterVar delimiter
    upvar 1 $paramVar param

    variable declExpr
    variable ExternalEntityExpr

    array set options $opts
    upvar #0 $options(-statevariable) state

    switch -glob -- $decl {

	/ {
	    # continuation from entity processing
	}

	!ELEMENT {
	    # Element declaration
	    if {[regexp $declExpr $value discard tag cmodel]} {
		DTD:ELEMENT [array get options] $tag $cmodel
	    } else {
		uplevel #0 $options(-errorcommand) [list illegaldeclaration "malformed element declaration around line $state(line)"]
	    }
	}

	!ATTLIST {
	    # Attribute list declaration
	    variable declExpr
	    if {[regexp $declExpr $value discard tag attdefns]} {
		if {[catch {DTD:ATTLIST [array get options] $tag $attdefns} err]} {
		    #puts stderr "Stack trace: $::errorInfo\n***\n"
		    # Atttribute parsing has bugs at the moment
		    #return -code error "$err around line $state(line)"
		    return {}
		}
	    } else {
		uplevel #0 $options(-errorcommand) [list illegaldeclaration "malformed attribute list declaration around line $state(line)"]
	    }
	}

	!ENTITY {
	    # Entity declaration
	    variable EntityExpr

	    if {[regexp $EntityExpr $value discard param name value]} {

		# Entity replacement text may have a '>' character.
		# In this case, the real delimiter will be in the following
		# text.  This is complicated by the possibility of there
		# being several '<','>' pairs in the replacement text.
		# At this point, we are searching for the matching quote delimiter.

		if {[regexp $ExternalEntityExpr $value]} {
		    DTD:ENTITY [array get options] $name [string trim $param] $value
		} elseif {[regexp ("|')(.*?)\\1(.*) $value discard delimiter replText value]} {

		    if {[string length [string trim $value]]} {
			uplevel #0 $options(-errorcommand) [list illegaldeclaration "malformed entity declaration around line $state(line)"]
		    } else {
			DTD:ENTITY [array get options] $name [string trim $param] $delimiter$replText$delimiter
		    }
		} elseif {[regexp ("|')(.*) $value discard delimiter replText]} {
		    append replText >$text
		    set text {}
		    set mode entity
		} else {
		    uplevel #0 $options(-errorcommand) [list illegaldeclaration "no delimiter for entity declaration around line $state(line)"]
		}

	    } else {
		uplevel #0 $options(-errorcommand) [list illegaldeclaration "malformed entity declaration around line $state(line)"]
	    }
	}

	!NOTATION {
	    # Notation declaration
	    if {[regexp $declExpr param discard tag notation]} {
		DTD:ENTITY [array get options] $tag $notation
	    } else {
		uplevel #0 $options(-errorcommand) [list illegaldeclaration "malformed entity declaration around line $state(line)"]
	    }
	}

	!--* {
	    # Start of a comment

	    if {[regexp !--(.*?)--\$ $decl discard data]} {
		if {[string length [string trim $value]]} {
		    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$value\""]
		}
		uplevel #0 $options(-commentcommand) [list $data]
		set decl /
		set value {}
	    } elseif {[regexp -- ^(.*?)--\$ $value discard data2]} {
		regexp !--(.*)\$ $decl discard data1
		uplevel #0 $options(-commentcommand) [list $data1\ $data2]
		set decl /
		set value {}
	    } elseif {[regexp (.*?)-->(.*)\$ $text discard data3 remainder]} {
		regexp !--(.*)\$ $decl discard data1
		uplevel #0 $options(-commentcommand) [list $data1\ $value>$data3]
		set decl /
		set value {}
		set text $remainder
	    } else {
		regexp !--(.*)\$ $decl discard data1
		set state(commentdata) $data1\ $value>$text
		set decl /
		set value {}
		set text {}
		set mode comment
	    }
	}

	!*INCLUDE* -
	!*IGNORE* {
	    if {$state(inInternalDTD)} {
		uplevel #0 $options(-errorcommand) [list illegalsection "conditional section not permitted in internal DTD subset around line $state(line)"]
	    }

	    if {[regexp {^!\[INCLUDE\[(.*)} $decl discard remainder]} {
		# Push conditional section stack, popped by ]]> sequence

		if {[regexp {(.*?)]]$} $remainder discard r2]} {
		    # section closed immediately
		    if {[string length [string trim $r2]]} {
			uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$r2\" in conditional section"]
		    }
		} elseif {[regexp {(.*?)]](.*)} $value discard r2 r3]} {
		    # section closed immediately
		    if {[string length [string trim $r2]]} {
			uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$r2\" in conditional section"]
		    }
		    if {[string length [string trim $r3]]} {
			uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$r3\" in conditional section"]
		    }
		} else {

		    lappend state(condSections) INCLUDE

		    set parser [$options(-cmd) entityparser]
		    $parser parse $remainder\ $value> -dtdsubset external
		    #$parser free

		    if {[regexp {(.*?)]]>(.*)} $text discard t1 t2]} {
			if {[string length [string trim $t1]]} {
			    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$t1\""]
			}
			if {![llength $state(condSections)]} {
			    uplevel #0 $options(-errorcommand) [list illegalsection "extraneous conditional section close"]
			}
			set state(condSections) [lreplace $state(condSections) end end]
			set text $t2
		    }

		}
	    } elseif {[regexp {^!\[IGNORE\[(.*)} $decl discard remainder]} {
		# Set ignore mode.  Still need a stack
		set mode ignore

		if {[regexp {(.*?)]]$} $remainder discard r2]} {
		    # section closed immediately
		    if {[string length [string trim $r2]]} {
			uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$r2\" in conditional section"]
		    }
		} elseif {[regexp {(.*?)]](.*)} $value discard r2 r3]} {
		    # section closed immediately
		    if {[string length [string trim $r2]]} {
			uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$r2\" in conditional section"]
		    }
		    if {[string length [string trim $r3]]} {
			uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$r3\" in conditional section"]
		    }
		} else {
		    
		    lappend state(condSections) IGNORE

		    if {[regexp {(.*?)]]>(.*)} $text discard t1 t2]} {
			if {[string length [string trim $t1]]} {
			    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$t1\""]
			}
			if {![llength $state(condSections)]} {
			    uplevel #0 $options(-errorcommand) [list illegalsection "extraneous conditional section close"]
			}
			set state(condSections) [lreplace $state(condSections) end end]
			set text $t2
		    }

		}
	    } else {
		uplevel #0 $options(-errorcommand) [list illegaldeclaration "illegal markup declaration \"$decl\" around line $state(line)"]
	    }

	}

	default {
	    if {[regexp {^\?(.*)} $decl discard target]} {
		# Processing instruction
	    } else {
		uplevel #0 $options(-errorcommand) [list illegaldeclaration "illegal markup declaration \"$decl\""]
	    }
	}
    }

    return {}
}

# sgml::ParseDTD:External --
#
#	Parse the external DTD subset.
#
#	Parameter entities are allowed anywhere.
#
# Arguments:
#	opts	configuration options
#	dtd	DTD data
#
# Results:
#	Markup declarations parsed may cause callback invocation

proc sgml::ParseDTD:External {opts dtd} {
    variable MarkupDeclExpr
    variable MarkupDeclSub
    variable declExpr

    array set options $opts
    upvar #0 $options(parameterentities) PEnts
    upvar #0 $options(externalparameterentities) ExtPEnts
    upvar #0 $options(-statevariable) state

    # As with the internal DTD subset, watch out for
    # entities with angle brackets
    set mode {} ;# normal
    set delimiter {}
    set name {}
    set param {}

    set oldState 0
    catch {set oldState $state(inInternalDTD)}
    set state(inInternalDTD) 0

    # Initialise conditional section stack
    if {![info exists state(condSections)]} {
	set state(condSections) {}
    }
    set startCondSectionDepth [llength $state(condSections)]

    while {[string length $dtd]} {
	set progress 0
	set PEref {}
	if {![string compare $mode "ignore"]} {
	    set progress 1
	    if {[regexp {]]>(.*)} $dtd discard dtd]} {
		set remainder {}
		set mode {} ;# normal
		set state(condSections) [lreplace $state(condSections) end end]
		continue
	    } else {
		uplevel #0 $options(-errorcommand) [list missingdelimiter "IGNORE conditional section closing delimiter not found"]
	    }
	} elseif {[regexp ^(.*?)%($::sgml::Name)\;(.*)\$ $dtd discard data PEref remainder]} {
	    set progress 1
	} else {
	    set data $dtd
	    set dtd {}
	    set remainder {}
	}

	# Tokenize the DTD (so far)

	# Protect Tcl special characters
	regsub -all {([{}\\])} $data {\\\1} dataP

	set n [regsub -all $MarkupDeclExpr $dataP $MarkupDeclSub dataP]

	if {$n} {
	    set progress 1
	    # All but the last markup declaration should have no text
	    set dataP [lrange "{} {} \{$dataP\}" 3 end]
	    if {[llength $dataP] > 3} {
		foreach {decl value text} [lrange $dataP 0 [expr [llength $dataP] - 4]] {
		    ParseDTD:EntityMode [array get options] mode replText decl value text $delimiter $name $param
		    ParseDTD:ProcessMarkupDecl [array get options] decl value delimiter name mode repltextVar text param

		    if {[string length [string trim $text]]} {
			# check for conditional section close
			if {[regexp {]]>(.*)$} $text discard text]} {
			    if {[string length [string trim $text]]} {
				uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$text\""]
			    }
			    if {![llength $state(condSections)]} {
				uplevel #0 $options(-errorcommand) [list illegalsection "extraneous conditional section close"]
			    }
			    set state(condSections) [lreplace $state(condSections) end end]
			    if {![string compare $mode "ignore"]} {
				set mode {} ;# normal
			    }
			} else {
			    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$text\""]
			}
		    }
		}
	    }
	    # Do the last declaration
	    foreach {decl value text} [lrange $dataP [expr [llength $dataP] - 3] end] {
		ParseDTD:EntityMode [array get options] mode replText decl value text $delimiter $name $param
		ParseDTD:ProcessMarkupDecl [array get options] decl value delimiter name mode repltextVar text param
	    }
	}

	# Now expand the PE reference, if any
	switch -glob $mode,[string length $PEref],$n {
	    ignore,0,* {
		set dtd $text
	    }
	    ignore,*,* {
		set dtd $text$remainder
	    }
	    *,0,0 {
		set dtd $data
	    }
	    *,0,* {
		set dtd $text
	    }
	    *,*,0 {
		if {[catch {append data $PEnts($PEref)}]} {
		    if {[info exists ExtPEnts($PEref)]} {
			set externalParser [$options(-cmd) entityparser]
			$externalParser parse $ExtPEnts($PEref) -dtdsubset external
			#$externalParser free
		    } else {
			uplevel #0 $options(-errorcommand) [list entityundeclared "parameter entity \"$PEref\" not declared"]
		    }
		}
		set dtd $data$remainder
	    }
	    default {
		if {[catch {append text $PEnts($PEref)}]} {
		    if {[info exists ExtPEnts($PEref)]} {
			set externalParser [$options(-cmd) entityparser]
			$externalParser parse $ExtPEnts($PEref) -dtdsubset external
			#$externalParser free
		    } else {
			uplevel #0 $options(-errorcommand) [list entityundeclared "parameter entity \"$PEref\" not declared"]
		    }
		}
		set dtd $text$remainder
	    }
	}

	# Check whether a conditional section has been terminated
	if {[regexp {^(.*?)]]>(.*)$} $dtd discard t1 t2]} {
	    if {![regexp <.*> $t1]} {
		if {[string length [string trim $t1]]} {
		    uplevel #0 $options(-errorcommand) [list unexpectedtext "unexpected text \"$t1\""]
		}
		if {![llength $state(condSections)]} {
		    uplevel #0 $options(-errorcommand) [list illegalsection "extraneous conditional section close"]
		}
		set state(condSections) [lreplace $state(condSections) end end]
		if {![string compare $mode "ignore"]} {
		    set mode {} ;# normal
		}
		set dtd $t2
		set progress 1
	    }
	}

	if {!$progress} {
	    # No parameter entity references were found and 
	    # the text does not contain a well-formed markup declaration
	    # Avoid going into an infinite loop
	    upvar #0 $options(-errorcommand) [list syntaxerror "external entity does not contain well-formed markup declaration"]
	    break
	}
    }

    set state(inInternalDTD) $oldState

    # Check that conditional sections have been closed properly
    if {[llength $state(condSections)] > $startCondSectionDepth} {
	uplevel #0 $options(-errorcommand) [list syntaxerror "[lindex $state(condSections) end] conditional section not closed"]
    }
    if {[llength $state(condSections)] < $startCondSectionDepth} {
	uplevel #0 $options(-errorcommand) [list syntaxerror "too many conditional section closures"]
    }

    return {}
}

# Procedures for handling the various declarative elements in a DTD.
# New elements may be added by creating a procedure of the form
# parse:DTD:_element_

# For each of these procedures, the various regular expressions they use
# are created outside of the proc to avoid overhead at runtime

# sgml::DTD:ELEMENT --
#
#	<!ELEMENT ...> defines an element.
#
#	The content model for the element is stored in the contentmodel array,
#	indexed by the element name.  The content model is parsed into the
#	following list form:
#
#		{}	Content model is EMPTY.
#			Indicated by an empty list.
#		*	Content model is ANY.
#			Indicated by an asterix.
#		{ELEMENT ...}
#			Content model is element-only.
#		{MIXED {element1 element2 ...}}
#			Content model is mixed (PCDATA and elements).
#			The second element of the list contains the 
#			elements that may occur.  #PCDATA is assumed 
#			(ie. the list is normalised).
#
# Arguments:
#	opts	configuration options
#	name	element GI
#	modspec	unparsed content model specification

proc sgml::DTD:ELEMENT {opts name modspec} {
    variable Wsp
    array set options $opts

    upvar #0 $options(elementdecls) elements

    if {$options(-validate) && [info exists elements($name)]} {
	eval $options(-errorcommand) [list elementdeclared "element \"$name\" already declared"]
    } else {
	switch -- $modspec {
	    EMPTY {
	    	set elements($name) {}
		uplevel #0 $options(-elementdeclcommand) $name {{}}
	    }
	    ANY {
	    	set elements($name) *
		uplevel #0 $options(-elementdeclcommand) $name *
	    }
	    default {
		# Don't parse the content model for now,
		# just pass the model to the application
		if {0 && [regexp [format {^\([%s]*#PCDATA[%s]*(\|([^)]+))?[%s]*\)*[%s]*$} $Wsp $Wsp $Wsp $Wsp] discard discard mtoks]} {
		    set cm($name) [list MIXED [split $mtoks |]]
		} elseif {0} {
		    if {[catch {CModelParse $state(state) $value} result]} {
			eval $options(-errorcommand) [list element? $result]
		    } else {
			set cm($id) [list ELEMENT $result]
		    }
		} else {
		    set elements($name) $modspec
		    uplevel #0 $options(-elementdeclcommand) $name [list $modspec]
		}
	    }
	}
    }
}

# sgml::CModelParse --
#
#	Parse an element content model (non-mixed).
#	A syntax tree is constructed.
#	A transition table is built next.
#
#	This is going to need alot of work!
#
# Arguments:
#	state	state array variable
#	value	the content model data
#
# Results:
#	A Tcl list representing the content model.

proc sgml::CModelParse {state value} {
    upvar #0 $state var

    # First build syntax tree
    set syntaxTree [CModelMakeSyntaxTree $state $value]

    # Build transition table
    set transitionTable [CModelMakeTransitionTable $state $syntaxTree]

    return [list $syntaxTree $transitionTable]
}

# sgml::CModelMakeSyntaxTree --
#
#	Construct a syntax tree for the regular expression.
#
#	Syntax tree is represented as a Tcl list:
#	rep {:choice|:seq {{rep list1} {rep list2} ...}}
#	where:	rep is repetition character, *, + or ?. {} for no repetition
#		listN is nested expression or Name
#
# Arguments:
#	spec	Element specification
#
# Results:
#	Syntax tree for element spec as nested Tcl list.
#
#	Examples:
#	(memo)
#		{} {:seq {{} memo}}
#	(front, body, back?)
#		{} {:seq {{} front} {{} body} {? back}}
#	(head, (p | list | note)*, div2*)
#		{} {:seq {{} head} {* {:choice {{} p} {{} list} {{} note}}} {* div2}}
#	(p | a | ul)+
#		+ {:choice {{} p} {{} a} {{} ul}}

proc sgml::CModelMakeSyntaxTree {state spec} {
    upvar #0 $state var
    variable Wsp
    variable name

    # Translate the spec into a Tcl list.

    # None of the Tcl special characters are allowed in a content model spec.
    if {[regexp {\$|\[|\]|\{|\}} $spec]} {
	return -code error "illegal characters in specification"
    }

    regsub -all [format {(%s)[%s]*(\?|\*|\+)?[%s]*(,|\|)?} $name $Wsp $Wsp] $spec [format {%sCModelSTname %s {\1} {\2} {\3}} \n $state] spec
    regsub -all {\(} $spec "\nCModelSTopenParen $state " spec
    regsub -all [format {\)[%s]*(\?|\*|\+)?[%s]*(,|\|)?} $Wsp $Wsp] $spec [format {%sCModelSTcloseParen %s {\1} {\2}} \n $state] spec

    array set var {stack {} state start}
    eval $spec

    # Peel off the outer seq, its redundant
    return [lindex [lindex $var(stack) 1] 0]
}

# sgml::CModelSTname --
#
#	Processes a name in a content model spec.
#
# Arguments:
#	state	state array variable
#	name	name specified
#	rep	repetition operator
#	cs	choice or sequence delimiter
#
# Results:
#	See CModelSTcp.

proc sgml::CModelSTname {state name rep cs args} {
    if {[llength $args]} {
	return -code error "syntax error in specification: \"$args\""
    }

    CModelSTcp $state $name $rep $cs
}

# sgml::CModelSTcp --
#
#	Process a content particle.
#
# Arguments:
#	state	state array variable
#	name	name specified
#	rep	repetition operator
#	cs	choice or sequence delimiter
#
# Results:
#	The content particle is added to the current group.

proc sgml::CModelSTcp {state cp rep cs} {
    upvar #0 $state var

    switch -glob -- [lindex $var(state) end]=$cs {
	start= {
	    set var(state) [lreplace $var(state) end end end]
	    # Add (dummy) grouping, either choice or sequence will do
	    CModelSTcsSet $state ,
	    CModelSTcpAdd $state $cp $rep
	}
	:choice= -
	:seq= {
	    set var(state) [lreplace $var(state) end end end]
	    CModelSTcpAdd $state $cp $rep
	}
	start=| -
	start=, {
	    set var(state) [lreplace $var(state) end end [expr {$cs == "," ? ":seq" : ":choice"}]]
	    CModelSTcsSet $state $cs
	    CModelSTcpAdd $state $cp $rep
	}
	:choice=| -
	:seq=, {
	    CModelSTcpAdd $state $cp $rep
	}
	:choice=, -
	:seq=| {
	    return -code error "syntax error in specification: incorrect delimiter after \"$cp\", should be \"[expr {$cs == "," ? "|" : ","}]\""
	}
	end=* {
	    return -code error "syntax error in specification: no delimiter before \"$cp\""
	}
	default {
	    return -code error "syntax error"
	}
    }
    
}

# sgml::CModelSTcsSet --
#
#	Start a choice or sequence on the stack.
#
# Arguments:
#	state	state array
#	cs	choice oir sequence
#
# Results:
#	state is modified: end element of state is appended.

proc sgml::CModelSTcsSet {state cs} {
    upvar #0 $state var

    set cs [expr {$cs == "," ? ":seq" : ":choice"}]

    if {[llength $var(stack)]} {
	set var(stack) [lreplace $var(stack) end end $cs]
    } else {
	set var(stack) [list $cs {}]
    }
}

# sgml::CModelSTcpAdd --
#
#	Append a content particle to the top of the stack.
#
# Arguments:
#	state	state array
#	cp	content particle
#	rep	repetition
#
# Results:
#	state is modified: end element of state is appended.

proc sgml::CModelSTcpAdd {state cp rep} {
    upvar #0 $state var

    if {[llength $var(stack)]} {
	set top [lindex $var(stack) end]
    	lappend top [list $rep $cp]
	set var(stack) [lreplace $var(stack) end end $top]
    } else {
	set var(stack) [list $rep $cp]
    }
}

# sgml::CModelSTopenParen --
#
#	Processes a '(' in a content model spec.
#
# Arguments:
#	state	state array
#
# Results:
#	Pushes stack in state array.

proc sgml::CModelSTopenParen {state args} {
    upvar #0 $state var

    if {[llength $args]} {
	return -code error "syntax error in specification: \"$args\""
    }

    lappend var(state) start
    lappend var(stack) [list {} {}]
}

# sgml::CModelSTcloseParen --
#
#	Processes a ')' in a content model spec.
#
# Arguments:
#	state	state array
#	rep	repetition
#	cs	choice or sequence delimiter
#
# Results:
#	Stack is popped, and former top of stack is appended to previous element.

proc sgml::CModelSTcloseParen {state rep cs args} {
    upvar #0 $state var

    if {[llength $args]} {
	return -code error "syntax error in specification: \"$args\""
    }

    set cp [lindex $var(stack) end]
    set var(stack) [lreplace $var(stack) end end]
    set var(state) [lreplace $var(state) end end]
    CModelSTcp $state $cp $rep $cs
}

# sgml::CModelMakeTransitionTable --
#
#	Given a content model's syntax tree, constructs
#	the transition table for the regular expression.
#
#	See "Compilers, Principles, Techniques, and Tools",
#	Aho, Sethi and Ullman.  Section 3.9, algorithm 3.5.
#
# Arguments:
#	state	state array variable
#	st	syntax tree
#
# Results:
#	The transition table is returned, as a key/value Tcl list.

proc sgml::CModelMakeTransitionTable {state st} {
    upvar #0 $state var

    # Construct nullable, firstpos and lastpos functions
    array set var {number 0}
    foreach {nullable firstpos lastpos} [	\
	TraverseDepth1st $state $st {
	    # Evaluated for leaf nodes
	    # Compute nullable(n)
	    # Compute firstpos(n)
	    # Compute lastpos(n)
	    set nullable [nullable leaf $rep $name]
	    set firstpos [list {} $var(number)]
	    set lastpos [list {} $var(number)]
	    set var(pos:$var(number)) $name
	} {
	    # Evaluated for nonterminal nodes
	    # Compute nullable, firstpos, lastpos
	    set firstpos [firstpos $cs $firstpos $nullable]
	    set lastpos  [lastpos  $cs $lastpos  $nullable]
	    set nullable [nullable nonterm $rep $cs $nullable]
	}	\
    ] break

    set accepting [incr var(number)]
    set var(pos:$accepting) #

    # var(pos:N) maps from position to symbol.
    # Construct reverse map for convenience.
    # NB. A symbol may appear in more than one position.
    # var is about to be reset, so use different arrays.

    foreach {pos symbol} [array get var pos:*] {
	set pos [lindex [split $pos :] 1]
	set pos2symbol($pos) $symbol
	lappend sym2pos($symbol) $pos
    }

    # Construct the followpos functions
    catch {unset var}
    followpos $state $st $firstpos $lastpos

    # Construct transition table
    # Dstates is [union $marked $unmarked]
    set unmarked [list [lindex $firstpos 1]]
    while {[llength $unmarked]} {
	set T [lindex $unmarked 0]
	lappend marked $T
	set unmarked [lrange $unmarked 1 end]

	# Find which input symbols occur in T
	set symbols {}
	foreach pos $T {
	    if {$pos != $accepting && [lsearch $symbols $pos2symbol($pos)] < 0} {
		lappend symbols $pos2symbol($pos)
	    }
	}
	foreach a $symbols {
	    set U {}
	    foreach pos $sym2pos($a) {
		if {[lsearch $T $pos] >= 0} {
		    # add followpos($pos)
	    	    if {$var($pos) == {}} {
	    	    	lappend U $accepting
	    	    } else {
	    	    	eval lappend U $var($pos)
	    	    }
		}
	    }
	    set U [makeSet $U]
	    if {[llength $U] && [lsearch $marked $U] < 0 && [lsearch $unmarked $U] < 0} {
		lappend unmarked $U
	    }
	    set Dtran($T,$a) $U
	}
	
    }

    return [list [array get Dtran] [array get sym2pos] $accepting]
}

# sgml::followpos --
#
#	Compute the followpos function, using the already computed
#	firstpos and lastpos.
#
# Arguments:
#	state		array variable to store followpos functions
#	st		syntax tree
#	firstpos	firstpos functions for the syntax tree
#	lastpos		lastpos functions
#
# Results:
#	followpos functions for each leaf node, in name/value format

proc sgml::followpos {state st firstpos lastpos} {
    upvar #0 $state var

    switch -- [lindex [lindex $st 1] 0] {
	:seq {
	    for {set i 1} {$i < [llength [lindex $st 1]]} {incr i} {
	    	followpos $state [lindex [lindex $st 1] $i]			\
			[lindex [lindex $firstpos 0] [expr $i - 1]]	\
			[lindex [lindex $lastpos 0] [expr $i - 1]]
	    	foreach pos [lindex [lindex [lindex $lastpos 0] [expr $i - 1]] 1] {
		    eval lappend var($pos) [lindex [lindex [lindex $firstpos 0] $i] 1]
		    set var($pos) [makeSet $var($pos)]
	    	}
	    }
	}
	:choice {
	    for {set i 1} {$i < [llength [lindex $st 1]]} {incr i} {
		followpos $state [lindex [lindex $st 1] $i]			\
			[lindex [lindex $firstpos 0] [expr $i - 1]]	\
			[lindex [lindex $lastpos 0] [expr $i - 1]]
	    }
	}
	default {
	    # No action at leaf nodes
	}
    }

    switch -- [lindex $st 0] {
	? {
	    # We having nothing to do here ! Doing the same as
	    # for * effectively converts this qualifier into the other.
	}
	* {
	    foreach pos [lindex $lastpos 1] {
		eval lappend var($pos) [lindex $firstpos 1]
		set var($pos) [makeSet $var($pos)]
	    }
	}
    }

}

# sgml::TraverseDepth1st --
#
#	Perform depth-first traversal of a tree.
#	A new tree is constructed, with each node computed by f.
#
# Arguments:
#	state	state array variable
#	t	The tree to traverse, a Tcl list
#	leaf	Evaluated at a leaf node
#	nonTerm	Evaluated at a nonterminal node
#
# Results:
#	A new tree is returned.

proc sgml::TraverseDepth1st {state t leaf nonTerm} {
    upvar #0 $state var

    set nullable {}
    set firstpos {}
    set lastpos {}

    switch -- [lindex [lindex $t 1] 0] {
	:seq -
	:choice {
	    set rep [lindex $t 0]
	    set cs [lindex [lindex $t 1] 0]

	    foreach child [lrange [lindex $t 1] 1 end] {
		foreach {childNullable childFirstpos childLastpos} \
			[TraverseDepth1st $state $child $leaf $nonTerm] break
		lappend nullable $childNullable
		lappend firstpos $childFirstpos
		lappend lastpos  $childLastpos
	    }

	    eval $nonTerm
	}
	default {
	    incr var(number)
	    set rep [lindex [lindex $t 0] 0]
	    set name [lindex [lindex $t 1] 0]
	    eval $leaf
	}
    }

    return [list $nullable $firstpos $lastpos]
}

# sgml::firstpos --
#
#	Computes the firstpos function for a nonterminal node.
#
# Arguments:
#	cs		node type, choice or sequence
#	firstpos	firstpos functions for the subtree
#	nullable	nullable functions for the subtree
#
# Results:
#	firstpos function for this node is returned.

proc sgml::firstpos {cs firstpos nullable} {
    switch -- $cs {
	:seq {
	    set result [lindex [lindex $firstpos 0] 1]
	    for {set i 0} {$i < [llength $nullable]} {incr i} {
	    	if {[lindex [lindex $nullable $i] 1]} {
	    	    eval lappend result [lindex [lindex $firstpos [expr $i + 1]] 1]
		} else {
		    break
		}
	    }
	}
	:choice {
	    foreach child $firstpos {
		eval lappend result $child
	    }
	}
    }

    return [list $firstpos [makeSet $result]]
}

# sgml::lastpos --
#
#	Computes the lastpos function for a nonterminal node.
#	Same as firstpos, only logic is reversed
#
# Arguments:
#	cs		node type, choice or sequence
#	lastpos		lastpos functions for the subtree
#	nullable	nullable functions forthe subtree
#
# Results:
#	lastpos function for this node is returned.

proc sgml::lastpos {cs lastpos nullable} {
    switch -- $cs {
	:seq {
	    set result [lindex [lindex $lastpos end] 1]
	    for {set i [expr [llength $nullable] - 1]} {$i >= 0} {incr i -1} {
		if {[lindex [lindex $nullable $i] 1]} {
		    eval lappend result [lindex [lindex $lastpos $i] 1]
		} else {
		    break
		}
	    }
	}
	:choice {
	    foreach child $lastpos {
		eval lappend result $child
	    }
	}
    }

    return [list $lastpos [makeSet $result]]
}

# sgml::makeSet --
#
#	Turn a list into a set, ie. remove duplicates.
#
# Arguments:
#	s	a list
#
# Results:
#	A set is returned, which is a list with duplicates removed.

proc sgml::makeSet s {
    foreach r $s {
	if {[llength $r]} {
	    set unique($r) {}
	}
    }
    return [array names unique]
}

# sgml::nullable --
#
#	Compute the nullable function for a node.
#
# Arguments:
#	nodeType	leaf or nonterminal
#	rep		repetition applying to this node
#	name		leaf node: symbol for this node, nonterm node: choice or seq node
#	subtree		nonterm node: nullable functions for the subtree
#
# Results:
#	Returns nullable function for this branch of the tree.

proc sgml::nullable {nodeType rep name {subtree {}}} {
    switch -glob -- $rep:$nodeType {
	:leaf -
	+:leaf {
	    return [list {} 0]
	}
	\\*:leaf -
	\\?:leaf {
	    return [list {} 1]
	}
	\\*:nonterm -
	\\?:nonterm {
	    return [list $subtree 1]
	}
	:nonterm -
	+:nonterm {
	    switch -- $name {
		:choice {
		    set result 0
		    foreach child $subtree {
			set result [expr $result || [lindex $child 1]]
		    }
		}
		:seq {
		    set result 1
		    foreach child $subtree {
			set result [expr $result && [lindex $child 1]]
		    }
		}
	    }
	    return [list $subtree $result]
	}
    }
}

# sgml::DTD:ATTLIST --
#
#	<!ATTLIST ...> defines an attribute list.
#
# Arguments:
#	opts	configuration opions
#	name	Element GI
#	attspec	unparsed attribute definitions
#
# Results:
#	Attribute list variables are modified.

proc sgml::DTD:ATTLIST {opts name attspec} {
    variable attlist_exp
    variable attlist_enum_exp
    variable attlist_fixed_exp

    array set options $opts

    # Parse the attribute list.  If it were regular, could just use foreach,
    # but some attributes may have values.
    regsub -all {([][$\\])} $attspec {\\\1} attspec
    regsub -all $attlist_exp $attspec "\}\nDTDAttribute {$options(-attlistdeclcommand)} $name $options(attlistdecls) {\\1} {\\2} {\\3} {} \{" attspec
    regsub -all $attlist_enum_exp $attspec "\}\nDTDAttribute {$options(-attlistdeclcommand)} $name $options(attlistdecls) {\\1} {\\2} {} {\\4} \{" attspec
    regsub -all $attlist_fixed_exp $attspec "\}\nDTDAttribute {$options(-attlistdeclcommand)} $name $options(attlistdecls) {\\1} {\\2} {\\3} {\\4} \{" attspec

    eval "noop \{$attspec\}"

    return {}
}

# sgml::DTDAttribute --
#
#	Parse definition of a single attribute.
#
# Arguments:
#	callback	attribute defn callback
#	name	element name
#	var	array variable
#	att	attribute name
#	type	type of this attribute
#	default	default value of the attribute
#	value	other information
#	text	other text (should be empty)
#
# Results:
#	Attribute defn added to array, unless it already exists

proc sgml::DTDAttribute args {
    # BUG: Some problems with parameter passing - deal with it later
    foreach {callback name var att type default value text} $args break

    upvar #0 $var atts

    if {[string length [string trim $text]]} {
	return -code error "unexpected text \"$text\" in attribute definition"
    }

    # What about overridden attribute defns?
    # A non-validating app may want to know about them
    # (eg. an editor)
    if {![info exists atts($name/$att)]} {
	set atts($name/$att) [list $type $default $value]
	uplevel #0 $callback [list $name $att $type $default $value]
    }

    return {}
}

# sgml::DTD:ENTITY --
#
#	<!ENTITY ...> declaration.
#
#	Callbacks:
#	-entitydeclcommand for general entity declaration
#	-unparsedentitydeclcommand for unparsed external entity declaration
#	-parameterentitydeclcommand for parameter entity declaration
#
# Arguments:
#	opts	configuration options
#	name	name of entity being defined
#	param	whether a parameter entity is being defined
#	value	unparsed replacement text
#
# Results:
#	Modifies the caller's entities array variable

proc sgml::DTD:ENTITY {opts name param value} {

    array set options $opts

    if {[string compare % $param]} {
	# Entity declaration - general or external
	upvar #0 $options(entities) ents
	upvar #0 $options(extentities) externals

	if {[info exists ents($name)] || [info exists externals($name)]} {
	    eval $options(-warningcommand) entity [list "entity \"$name\" already declared"]
	} else {
	    if {[catch {uplevel #0 $options(-parseentitydeclcommand) [list $value]} value]} {
		return -code error "unable to parse entity declaration due to \"$value\""
	    }
	    switch -glob [lindex $value 0],[lindex $value 3] {
		internal, {
		    set ents($name) [EntitySubst [array get options] [lindex $value 1]]
		    uplevel #0 $options(-entitydeclcommand) [list $name $ents($name)]
		}
		internal,* {
		    return -code error "unexpected NDATA declaration"
		}
		external, {
		    set externals($name) [lrange $value 1 2]
		    uplevel #0 $options(-entitydeclcommand) [eval list $name [lrange $value 1 2]]
		}
		external,* {
		    set externals($name) [lrange $value 1 3]
		    uplevel #0 $options(-unparsedentitydeclcommand) [eval list $name [lrange $value 1 3]]
		}
		default {
		    return -code error "internal error: unexpected parser state"
		}
	    }
	}
    } else {
	# Parameter entity declaration
	upvar #0 $options(parameterentities) PEnts
	upvar #0 $options(externalparameterentities) ExtPEnts

	if {[info exists PEnts($name)] || [info exists ExtPEnts($name)]} {
	    eval $options(-warningcommand) parameterentity [list "parameter entity \"$name\" already declared"]
	} else {
	    if {[catch {uplevel #0 $options(-parseentitydeclcommand) [list $value]} value]} {
		return -code error "unable to parse parameter entity declaration due to \"$value\""
	    }
	    if {[string length [lindex $value 3]]} {
		return -code error "NDATA illegal in parameter entity declaration"
	    }
	    switch [lindex $value 0] {
		internal {
		    # Substitute character references and PEs (XML: 4.5)
		    set value [EntitySubst [array get options] [lindex $value 1]]

		    set PEnts($name) $value
		    uplevel #0 $options(-parameterentitydeclcommand) [list $name $value]
		}
		external -
		default {
		    # Get the replacement text now.
		    # Could wait until the first reference, but easier
		    # to just do it now.

		    set token [uri::geturl [uri::resolve $options(-baseuri) [lindex $value 1]]]

		    set ExtPEnts($name) [lindex [array get $token data] 1]
		    uplevel #0 $options(-parameterentitydeclcommand) [eval list $name [lrange $value 1 2]]
		}
	    }
	}
    }
}

# sgml::EntitySubst --
#
#	Perform entity substitution on an entity replacement text.
#	This differs slightly from other substitution procedures,
#	because only parameter and character entity substitution
#	is performed, not general entities.
#	See XML Rec. section 4.5.
#
# Arguments:
#	opts	configuration options
#	value	Literal entity value
#
# Results:
#	Expanded replacement text

proc sgml::EntitySubst {opts value} {
    array set options $opts

    # Protect Tcl special characters
    regsub -all {([{}\\])} $value {\\\1} value

    # Find entity references
    regsub -all (&#\[0-9\]+|&#x\[0-9a-fA-F\]+|%${::sgml::Name})\; $value "\[EntitySubstValue [list $options(parameterentities)] {\\1}\]" value

    set result [subst $value]

    return $result
}

# sgml::EntitySubstValue --
#
#	Handle a single character or parameter entity substitution
#
# Arguments:
#	PEvar	array variable containing PE declarations
#	ref	character or parameter entity reference
#
# Results:
#	Replacement text

proc sgml::EntitySubstValue {PEvar ref} {
    # SRB: Bug fix 2008-11-18 #812051: surround case labels in braces for compatibility with Freewrap
    switch -glob -- $ref {
	{&#x*} {
	    scan [string range $ref 3 end] %x hex
	    return [format %c $hex]
	}
	{&#*} {
	    return [format %c [string range $ref 2 end]]
	}
	{%*} {
	    upvar #0 $PEvar PEs
	    set ref [string range $ref 1 end]
	    if {[info exists PEs($ref)]} {
		return $PEs($ref)
	    } else {
		return -code error "parameter entity \"$ref\" not declared"
	    }
	}
	default {
	    return -code error "internal error - unexpected entity reference"
	}
    }
    return {}
}

# sgml::DTD:NOTATION --
#
#	Process notation declaration
#
# Arguments:
#	opts	configuration options
#	name	notation name
#	value	unparsed notation spec

proc sgml::DTD:NOTATION {opts name value} {
    return {}

    variable notation_exp
    upvar opts state

    if {[regexp $notation_exp $value x scheme data] == 2} {
    } else {
	eval $state(-errorcommand) [list notationvalue "notation value \"$value\" incorrectly specified"]
    }
}

# sgml::ResolveEntity --
#
#	Default entity resolution routine
#
# Arguments:
#	cmd	command of parent parser
#	base	base URL for relative URLs
#	sysId	system identifier
#	pubId	public identifier

proc sgml::ResolveEntity {cmd base sysId pubId} {
    variable ParseEventNum

    if {[catch {uri::resolve $base $sysId} url]} {
	return -code error "unable to resolve system identifier \"$sysId\""
    }
    if {[catch {uri::geturl $url} token]} {
	return -code error "unable to retrieve external entity \"$url\" for system identifier \"$sysId\""
    }

    upvar #0 $token data

    set parser [uplevel #0 $cmd entityparser]

    set body {}
    catch {set body $data(body)}
    catch {set body $data(data)}
    if {[string length $body]} {
	uplevel #0 $parser parse [list $body] -dtdsubset external
    }
    $parser free

    return {}
}
