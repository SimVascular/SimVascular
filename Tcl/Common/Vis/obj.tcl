# Copyright (c) 2009-2011 Open Source Medical Software Corporation, 
# University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
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

catch {set ::vis::objs(empty) ""} 

proc ::vis::newobj {name} {

  #@author Nathan Wilson
  #@c Create a new unique object name that can be referenced by name
  #@a name:  unique string name that can be used with out ::vis:: obj
  #@a name:  related commands

  ::vis::rmobj $name
  set ::vis::objs($name) [tmpobj]
  return $::vis::objs($name)

}

proc ::vis::getobj {name} {
  #@author Nathan Wilson
  #@c Return actual object name using named reference 
  #@a name:  unique string name previously registered using ::vis::newobj
  
  if {![info exists ::vis::objs($name)]} {
     return -code error "ERROR:  ::vis::objs($name) var does not exist!"
  }
  if {[info commands $::vis::objs($name)] == ""} {
     return -code error "ERROR:  ::vis::objs($name) obj does not exist!"
  }
  return $::vis::objs($name)
}

proc ::vis::getname {obj} {

  #@author Nathan Wilson
  #@c Return the name for a given actor obj
  #@a obj:  object

  # check and see if an object has already been created for
  # this slot and remove it if it has
  foreach name [array names ::vis::objs] {
   if {$::vis::objs($name) == $obj} {
     if {[info commands $::vis::objs($name)] != ""} {
       return $name
     }
     return
   }
  }

}

proc ::vis::exists {name} {
  #@author Nathan Wilson
  #@c Check and see if the obj name exists
  #@a name: string name
  if {![info exists ::vis::objs($name)]} {
     return 0
  }
  if {[info commands $::vis::objs($name)] == ""} {
     return 0
  }
  return 1
}

proc ::vis::print {} {
  #@author Nathan Wilson
  #@c Print out the current list of visualization objects
  ::u::printArray ::vis::objs
}

proc ::vis::getall {prefix} {
    #@author Nathan Wilson
    #@c Get all the vis objects starting with prefix
    #@a prefix: string prefix
    set allnames [array names ::vis::objs $prefix*]
    return $allnames
}


proc ::vis::rmobj {name} {

  #@author Nathan Wilson
  #@c Remove named object from vis list of objects
  #@a name: unique string

  # check and see if an object has already been created for
  # this slot and remove it if it has
  if [info exists ::vis::objs($name)] {
    if {[info commands $::vis::objs($name)] != ""} {
       $::vis::objs($name) Delete
    }
    unset ::vis::objs($name)
  }

}

proc ::vis::rmentry {obj} {

  #@author Nathan Wilson
  #@c Remove the actual object from a named slot
  #@a obj:  object

  # check and see if an object has already been created for
  # this slot and remove it if it has
  foreach name [array names ::vis::objs] {
   if {$::vis::objs($name) == $obj} {
      #puts "removing $::vis::objs($name)"
     if {[info commands $::vis::objs($name)] != ""} {
       puts "removing $::vis::objs($name)"
       $::vis::objs($name) Delete
     }
     unset ::vis::objs($name)
     return
   }
  }

}


proc ::vis::rmall {prefix} {

    #@author Nathan Wilson
    #@c Remove all existing visualization objects
    #@c starting with prefix
    #@a prefix:  string prefix

    foreach name [::vis::getall $prefix] {
       ::vis::rmobj $name
    }

}
