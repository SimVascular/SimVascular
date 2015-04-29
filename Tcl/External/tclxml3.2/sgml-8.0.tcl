# sgml-8.0.tcl --
#
#	This file provides generic parsing services for SGML-based
#	languages, namely HTML and XML.
#	This file supports Tcl 8.0 characters and regular expressions.
#
#	NB.  It is a misnomer.  There is no support for parsing
#	arbitrary SGML as such.
#
# Copyright (c) 1998,1999 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id: sgml-8.0.tcl,v 1.4 2003/12/09 04:43:15 balls Exp $

package require -exact Tcl 8.0

package provide sgml 1.9

namespace eval sgml {

    # Convenience routine
    proc cl x {
	return "\[$x\]"
    }

    # Define various regular expressions

    # Character classes
    variable Char \t\n\r\ -\xFF
    variable BaseChar A-Za-z
    variable Letter $BaseChar
    variable Digit 0-9
    variable CombiningChar {}
    variable Extender {}
    variable Ideographic {}

    # white space
    variable Wsp " \t\r\n"
    variable noWsp [cl ^$Wsp]

    # Various XML names
    variable NameChar \[-$Letter$Digit._:$CombiningChar$Extender\]
    variable Name \[_:$BaseChar$Ideographic\]$NameChar*
    variable Names ${Name}(?:$Wsp$Name)*
    variable Nmtoken $NameChar+
    variable Nmtokens ${Nmtoken}(?:$Wsp$Nmtoken)*

    # table of predefined entities for XML

    variable EntityPredef
    array set EntityPredef {
	lt <   gt >   amp &   quot \"   apos '
    }

}

# These regular expressions are defined here once for better performance

namespace eval sgml {
    variable Wsp

    # Watch out for case-sensitivity

    set attlist_exp [cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*(#REQUIRED|#IMPLIED)
    set attlist_enum_exp [cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*\\(([cl ^)]*)\\)[cl $Wsp]*("([cl ^")])")? ;# "
    set attlist_fixed_exp [cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*(#FIXED)[cl $Wsp]*([cl ^$Wsp]+)

    set param_entity_exp [cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*([cl ^"$Wsp]*)[cl $Wsp]*"([cl ^"]*)"

    set notation_exp [cl $Wsp]*([cl ^$Wsp]+)[cl $Wsp]*(.*)

}

### Utility procedures

# sgml::noop --
#
#	A do-nothing proc
#
# Arguments:
#	args	arguments
#
# Results:
#	Nothing.

proc sgml::noop args {
    return 0
}

# sgml::identity --
#
#	Identity function.
#
# Arguments:
#	a	arbitrary argument
#
# Results:
#	$a

proc sgml::identity a {
    return $a
}

# sgml::Error --
#
#	Throw an error
#
# Arguments:
#	args	arguments
#
# Results:
#	Error return condition.

proc sgml::Error args {
    uplevel return -code error [list $args]
}

### Following procedures are based on html_library

# sgml::zapWhite --
#
#	Convert multiple white space into a single space.
#
# Arguments:
#	data	plain text
#
# Results:
#	As above

proc sgml::zapWhite data {
    regsub -all "\[ \t\r\n\]+" $data { } data
    return $data
}

proc sgml::Boolean value {
    regsub {1|true|yes|on} $value 1 value
    regsub {0|false|no|off} $value 0 value
    return $value
}

