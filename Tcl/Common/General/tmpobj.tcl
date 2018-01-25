# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# much catch globals for tkautodoc
namespace eval obj {}

catch {set ::obj::tmp_id 0}
catch {set ::obj::debugLevel 2}

proc ::obj::tmp_name {} {
  #@author Nathan Wilson
  #@c Create a temporary name
  #@r next temporary name
  incr ::obj::tmp_id
  return "tmpvtkobj-$::obj::tmp_id"
}

proc ::obj::new {args} {
  #@author Nathan Wilson
  #@c Create a new object name
  #@r object name
  set name [::obj::tmp_name]
  catch {$name Delete}
  set obj $name
  if {$::obj::debugLevel > 0} {
    if {[info level] == 0} {
      set ::obj::callfunc($obj) "global"
    } elseif {[info level] == 1} {
      set ::obj::callfunc($obj) [info level [expr [info level] - 1]]
    } else {
      for {set i [expr [info level] - 1]} {$i > 0} {incr i -1} {
	if {$::obj::debugLevel == 1} {
          lappend ::obj::callfunc($obj) [lindex [info level [expr [info level] - $i]] 0]
	} else {
          lappend ::obj::callfunc($obj) [info level [expr [info level] - $i]]
	}
      }
    }
  }
  if {$args != ""} {
    if {[llength $args] > 0} {
      set ::obj::tags($name) $args
    }
  }

  return $name
}

proc ::obj::delete {name} {
  #@author Nathan Wilson
  #@c Delete the object corresponding to name
  #@a name: object name to delete
  #@note  This function does not return an error if the
  #@note  object to be deleted does not exist
  catch {$name Delete}
  catch {unset ::obj::callfunc($name)}
  catch {unset ::obj::tags($name)}
  return
}

proc ::obj::print {} {
  #@author Nathan Wilson
  #@c Print out the list of objects created via ::obj::
  foreach i [info commands tmpvtkobj*] {
      if [info exists ::obj::tags($i)] {
        puts "[format %15s $i] [format %30s [$i GetClassName]]   $::obj::tags($i)   $::obj::callfunc($i)"
      } else {
        puts "[format %15s $i] [format %30s [$i GetClassName]]   $::obj::callfunc($i)"
      }
  }
}

proc ::obj::debug {level} {
  #@author Nathan Wilson
  #@c Set the debugging level
  #@a level: 0,1, 2
  set ::obj::debugLevel $level
}

proc tmpobj {args} {
  #@author Nathan Wilson
  #@c Create a new object name, track who asked for its creation based on
  #@c debug flag.
  set obj [::obj::new $args]

  return $obj
}



