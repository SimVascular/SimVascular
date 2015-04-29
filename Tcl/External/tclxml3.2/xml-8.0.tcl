# xml-8.0.tcl --
#
#	This file provides generic XML services for all implementations.
#	This file supports Tcl 8.0 regular expressions.
#
#	See xmlparse.tcl for the Tcl implementation of a XML parser.
#
# Copyright (c) 2005 by Explain.
# http://www.explain.com.au/
# Copyright (c) 1998-2004 Zveno Pty Ltd
# http://www.zveno.com/
# 
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: xml-8.0.tcl,v 1.7.2.1 2005/12/28 06:49:51 balls Exp $

package require -exact Tcl 8.0

package require sgml 1.8

package provide xmldefs 3.2

namespace eval xml {

    # Convenience routine
    proc cl x {
	return "\[$x\]"
    }

    # Define various regular expressions

    # Characters
    variable Char $::sgml::Char

    # white space
    variable Wsp " \t\r\n"
    variable noWsp [cl ^$Wsp]

    # Various XML names and tokens

    variable NameChar $::sgml::NameChar
    variable Name $::sgml::Name
    variable Names $::sgml::Names
    variable Nmtoken $::sgml::Nmtoken
    variable Nmtokens $::sgml::Nmtokens

    # The definition of the Namespace URI for XML Namespaces themselves.
    # The prefix 'xml' is automatically bound to this URI.
    variable xmlnsNS http://www.w3.org/XML/1998/namespace

    # Tokenising expressions

    variable tokExpr <(/?)([cl ^$Wsp>/]+)([cl $Wsp]*[cl ^>]*)>
    variable substExpr "\}\n{\\2} {\\1} {\\3} \{"

    # table of predefined entities

    variable EntityPredef
    array set EntityPredef {
	lt <   gt >   amp &   quot \"   apos '
    }

}

###
###	General utility procedures
###

# xml::noop --
#
# A do-nothing proc

proc xml::noop args {}

### Following procedures are based on html_library

# xml::zapWhite --
#
#	Convert multiple white space into a single space.
#
# Arguments:
#	data	plain text
#
# Results:
#	As above

proc xml::zapWhite data {
    regsub -all "\[ \t\r\n\]+" $data { } data
    return $data
}

