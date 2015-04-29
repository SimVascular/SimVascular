# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
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

proc clickLink { x y } {
  global symbolicName
  set tbox $symbolicName(svhelp_textbox)
  set i [$tbox index @$x,$y]
  set range [$tbox tag prevrange helplink $i]
  set url [eval $tbox get $range]
  guiSVHELPshow $url
}

# procedure to show window ShowWindow.svhelp
proc ShowWindow.svhelp { args} {
# xf ignore me 7

  # build widget .svhelp
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .svhelp"
  } {
    catch "destroy .svhelp"
  }
  toplevel .svhelp   -relief {raised}

  # Window manager configurations
  wm positionfrom .svhelp program
  wm sizefrom .svhelp program
  wm maxsize .svhelp 1920 1200
  wm minsize .svhelp 150 150
  wm protocol .svhelp WM_DELETE_WINDOW {XFProcError {Application windows can not be destroyed. Please use the "Current widget path:" to show/hide windows.}}
  wm title .svhelp {SimVascular Help}

  # build widget .svhelp.frame0
  ttk::frame .svhelp.frame0  -borderwidth {0}  -relief {flat}  -width {600}  -height {360}
  # build widget .svhelp.frame0.frame2
  ttk::frame .svhelp.frame0.frame2  -width {618}  -height {30}

  # build widget .svhelp.frame0.frame2.button3
  ttk::button .svhelp.frame0.frame2.button3  -command {DestroyWindow.svhelp}  -text {Close}

  # build widget .svhelp.frame0.frame2.button2
  #ttk::button .svhelp.frame0.frame2.button2  -command {guiSVMSGsave}  -text {Save Message}

  # build widget .svhelp.frame0.frame
  ttk::frame .svhelp.frame0.frame  -relief {flat}

  # build widget .svhelp.frame0.frame.scrollbar1
  ttk::scrollbar .svhelp.frame0.frame.scrollbar1  -command {.svhelp.frame0.frame.text2 yview}

  # build widget .svhelp.frame0.frame.text2
  text .svhelp.frame0.frame.text2  -font {Helvetica 10}  -height {30}  -relief {raised}  -wrap {word}  -yscrollcommand {.svhelp.frame0.frame.scrollbar1 set}
  # bindings
  bind .svhelp.frame0.frame.text2 <Key> {NoFunction}


  # pack master .svhelp.frame0
  pack configure .svhelp.frame0.frame  -expand 1  -fill both
  pack configure .svhelp.frame0.frame2  -fill both

  # pack master .svhelp.frame0.frame2
  pack configure .svhelp.frame0.frame2.button3  -expand 1  -fill both  -side left

  # pack master .svhelp.frame0.frame
  pack configure .svhelp.frame0.frame.scrollbar1  -fill y  -side right
  pack configure .svhelp.frame0.frame.text2  -expand 1  -fill both

  # pack master .svhelp
  pack configure .svhelp.frame0  -expand 1  -fill both

  .svhelp.frame0.frame.text2 insert end { }



  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .svhelp"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc DestroyWindow.svhelp {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .svmessage]" != ""} {
      global xfShowWindow.svhelp
      set xfShowWindow.svhelp 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .svmessage; XFEditSetShowWindows"
    }
  } {
    catch "destroy .svhelp"
    update
  }
}


# Procedure: guiSVMSGshow
proc guiSVHELPshow { file} {
  global symbolicName
  ShowWindow.svhelp
  set tbox $symbolicName(svhelp_textbox)
  
  $tbox tag bind helplink <Button-1> {clickLink %x %y}

  $tbox tag configure header1 -font "helvetica 20" \
  -justify "center" \
  -underline on

    $tbox tag configure header2 -font "helvetica 16" \
  -justify "left" \
  -underline on

  $tbox tag configure header3 -font "helvetica 14" \
  -justify "left" \
  -underline on

  $tbox tag configure topic -font "helvetica 10 bold" \
  -justify "left"

  $tbox tag configure helplink -font "helvetica 10" \
  -justify "left" \
  -foreground blue \
  -underline on

  set regex_map [list header3 {\#\#\#((\w|\s)*)$} 0\
  header2 {\#\#((\w|\s)*)$} 0 \
  header1 {\#((\w|\s)*)$} 0 \
  topic {\*((\w|\s)*)\*} 1 \
  helplink {\[((\w|\s|.|\\|/)*)\]} 1 \
  ]

  global env
  set simvascular_help "$env(SIMVASCULAR_HOME)/Tcl/Help/"
  set fp [open "$simvascular_help$file" r]
  set data [read $fp]
  close $fp

  $tbox delete 1.0 end

  set mdlist {}
  set n 1


  foreach line [split $data "\n"] {
    foreach {tagname regex strdiff} $regex_map {
      #puts "processing $tagname"
      if { [ string length $line ] != 0 } {
        set idx 0

        while {$idx < [ string length $line ] && $idx >= 0} {
          if { [info exists location] } {
            unset location
          }

          regexp -indices -start $idx $regex $line location sub1

          if { [info exists location] } {
            regexp -start $idx $regex $line match sub1
            set start [expr [lindex $location 0]]
            set end [expr [lindex $location 1] ]

            set line [string replace $line $start $end $sub1]

            #puts "strdiff $strdiff"
            set end [expr $end - $strdiff]
            lappend mdlist $tagname $n.$start $n.$end

            set idx [expr $end + 1]
            } {
              set idx -1 
            }
          }
        }
      }
      $tbox insert $n.0 $line
      foreach {tagname start stop} $mdlist {
        #puts "setting $tagname at $start $stop"
        $tbox tag add $tagname $start $stop
      }
      $tbox insert end \n
      incr n
      set mdlist {}
    }
  }
