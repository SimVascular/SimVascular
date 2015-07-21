#! /bin/sh
# The next line is executed by /bin/sh, but not Tcl \
  exec wish $0 ${1+"$@"}

# global variables
# external
global argc
global argv
global tk_version

# tutorial
set tutBlock 0
set tutChapter intro
set tutSection 0

# test tk version
if {$tk_version < 4.0} {
  puts stderr "\nTutorial error: wrong TK version: need at least 4.0\n"
  catch "destroy ."
  catch "exit 0"
}

# 03/17/98 - Dennis LaBelle - Added next line to allow tutorial to run under Windows
if {![info exists argc]} {set argc 0}

if {$argc != 2 && ![info exists xf_TUTSCRIPT]} {
  puts stderr "Please call the tutorial not directly!"
  puts stderr "The program that should use the tutorial must"
  puts stderr "provide a way to activate the tutorial."
  puts stderr "In XF activate (Help|Tutorial)."
  catch "destroy ."
  catch "exit 0"
}

if {[info exists xf_TUTSCRIPT]} {
  set tutScriptPath $xf_TUTSCRIPT
  set tutInterpreter $xf_TUTINTERP
} {
  set tutScriptPath [lindex $argv 0]
  set tutInterpreter [lindex $argv 1]
}

# execute action list
proc TUTDoIt {} {
  global tutBlock
  global tutChapter
  global tutSection
  global ${tutChapter}LastSectionDone

  if {$tutBlock == 0} {
    if {[set ${tutChapter}LastSectionDone] < $tutSection} {
      TUTEvalCommand 0
      set ${tutChapter}LastSectionDone $tutSection
    }
  }
}

# eval command
proc TUTEvalCommand {tutNormal} {
  global tutBlock
  global tutChapter
  global tutSection
  global tutInterpreter

  if {"[info globals ${tutChapter}Command${tutSection}]" != ""} {
    global ${tutChapter}Command${tutSection}

    TUTBlock $tutNormal
    if {[info exists xf_TUTSCRIPT]} {
       set sendCommand "send \{XFTUTEval \{[set ${tutChapter}Command${tutSection}]\} \{[winfo name .]\}\}"
       eval $sendCommand
    } {
      set sendCommand "send $tutInterpreter \{XFTUTEval \{[set ${tutChapter}Command${tutSection}]\} \{[winfo name .]\}\}"
      if {[catch $sendCommand tutResult]} {
        # ignore timout
        if {"remote interpreter did not respond" != "$tutResult"} {
          puts stderr "Tutorial error: $tutResult"
        }
      }
    }
  }
}

# block tutorial
proc TUTBlock {tutNormal} {
  global tutBlock
  global tutSetNormal
  
  set tutBlock 1
  set tutSetNormal $tutNormal
  .frame2.prev configure -state disabled
  .frame2.doit configure -state disabled
  .frame2.next configure -state disabled
}

# unblock tutorial
proc TUTUnblock {} {
  global tutBlock
  global tutSetNormal
  
  set tutBlock 0
  .frame2.prev configure -state normal
  .frame2.next configure -state normal
  if {$tutSetNormal == 1} {
    .frame2.doit configure -state normal
  }
}

# insert string into list
proc TUTInsertStringIntoList {tutList tutString} {

  set tutLength [string length $tutString]
  set tutPosition 0
  set tutOldPosition 0
  while {$tutPosition < $tutLength} {
    # currently the only working version
    while {$tutPosition < $tutLength} {
      set tutCurrent [string index $tutString $tutPosition]
      if {[string match $tutCurrent "\n"] &&
          ![string match $tutCurrent "\*"]} {
        break
      }
      incr tutPosition 1
    }
    $tutList insert end \
      "[string range $tutString $tutOldPosition $tutPosition]"
    incr tutPosition 1
    set tutOldPosition $tutPosition
  }
}

# next page
proc TUTNextPage {} {
  global tutBlock
  global tutChapter
  global tutSection
  global ${tutChapter}Last
  global ${tutChapter}LastSectionDone

  if {$tutBlock == 0} {
    if {$tutSection > [set ${tutChapter}LastSectionDone]} {
      TUTEvalCommand 1
      set ${tutChapter}LastSectionDone $tutSection
    }
    if {$tutSection < [set ${tutChapter}Last]} {
      incr tutSection 1
    } {
      set tutTmpChapter $tutChapter
      case $tutTmpChapter in {
        {intro} {
          set tutChapter design
        }
        {design} {
          set tutChapter working
        }
        {working} {
          set tutChapter expert
        }
        {expert} {
          set tutChapter expert
        }
        {packing} {
          set tutChapter intro
        }
        {placing} {
          set tutChapter intro
        }
        {example} {
          set tutChapter intro
        }
      }
      if {[string compare $tutTmpChapter expert] == 0} {
        global ${tutChapter}Last

        set tutSection [set ${tutChapter}Last]
      } {
        set tutSection 0
      }
    }
    TUTShowText
  }
}

# previous page
proc TUTPrevPage {} {
  global tutBlock
  global tutChapter
  global tutSection

  if {$tutBlock == 0} {
    if {$tutSection > 0} {
      incr tutSection -1
    } {
      set tutTmpChapter $tutChapter
      case $tutTmpChapter in {
        {intro} {
          set tutChapter intro
        }
        {design} {
          set tutChapter intro
        }
        {working} {
          set tutChapter design
        }
        {expert} {
          set tutChapter working
        }
        {packing} {
          set tutChapter intro
        }
        {placing} {
          set tutChapter intro
        }
        {example} {
          set tutChapter intro
        }
      }
      if {[string compare $tutTmpChapter intro] == 0} {
        set tutSection 0
      } {
        global ${tutChapter}Last

        set tutSection [set ${tutChapter}Last]
      }
    }
    TUTShowText
  }
}

# print text
proc TUTPrintText {} {

  set outFile [open /tmp/xf-tut.ms w]
  foreach tutChapter "header intro design working expert packing placing example" {
    global ${tutChapter}Last

    set tutCounter 0
    set tutChapterName ""
    while {$tutCounter <= [set ${tutChapter}Last]} {
      if {"[info globals ${tutChapter}Name${tutCounter}]" != ""} {
        global ${tutChapter}Name$tutCounter
        if {"$tutChapterName" == ""} {
          set tutChapterName [set ${tutChapter}Name$tutCounter]
          puts $outFile ".NH"
          puts $outFile [string trim [set ${tutChapter}Name$tutCounter]]
          puts $outFile ".PP"
        } {
          if {"$tutChapterName" != "[set ${tutChapter}Name$tutCounter]"} {
            set tutChapterName [set ${tutChapter}Name$tutCounter]
            puts $outFile ".NH 2"
            puts $outFile [string trim [set ${tutChapter}Name$tutCounter]]
            puts $outFile ".PP"
          }
        }
      }
      if {"[info globals ${tutChapter}Text${tutCounter}]" != ""} {
        global ${tutChapter}Text$tutCounter
        puts $outFile [string trim [set ${tutChapter}Text$tutCounter]]
        puts $outFile ""
      }
      incr tutCounter 1
    }
  }
  puts $outFile ""
  close $outFile
}

# set new chapter
proc TUTSetChapter {tutNewChapter} {
  global tutChapter
  global tutSection

  set tutChapter $tutNewChapter
  set tutSection 0

  TUTShowText
}

# display text
proc TUTShowText {} {
  global tutBlock
  global tutChapter
  global tutSection
  global ${tutChapter}Name${tutSection}
  global ${tutChapter}Command${tutSection}
  global ${tutChapter}LastSectionDone

  .frame5.chapter configure \
    -text [set ${tutChapter}Name${tutSection}]
  .frame5.page configure \
    -text " Page: [expr ${tutSection}+1] "
  if {"[info globals ${tutChapter}Text${tutSection}]" != ""} {
    global ${tutChapter}Text${tutSection}

    if {[.frame3.thelist size] > 0} {
      .frame3.thelist delete 0 end
    }
    TUTInsertStringIntoList .frame3.thelist \
      [string trim [set ${tutChapter}Text${tutSection}]]
  }
  if {"[info globals ${tutChapter}Command${tutSection}]" != ""} {
    if {[set ${tutChapter}LastSectionDone] < $tutSection} {
      if {$tutBlock == 0} {
        .frame2.prev configure \
          -state normal
        .frame2.doit configure \
          -state normal
        .frame2.next configure \
          -state normal
      }
    } {
      .frame2.prev configure \
        -state normal
      .frame2.doit configure \
        -state disabled
      .frame2.next configure \
        -state normal
    }
  } {
    .frame2.prev configure \
      -state normal
    .frame2.doit configure \
      -state disabled
    .frame2.next configure \
      -state normal
  }
}

# create tutorial toplevel
wm geometry . 420x420
wm minsize . 100 100
wm maxsize . 1000 1000

frame .frame4 \
  -borderwidth 2 \
  -relief raised

menubutton .frame4.file \
  -text "File" \
  -underline 0 \
  -menu ".frame4.file.m"

menu .frame4.file.m
.frame4.file.m add command \
  -label {Print to /tmp/xf-tut.ms} \
  -underline 0 \
  -command {TUTPrintText}
.frame4.file.m add separator
if {[info exists xf_TUTSCRIPT]} {
  .frame4.file.m add command \
    -label {Quit} \
    -underline 0 \
    -command {send {interp delete xftut}}
} {
  .frame4.file.m add command \
    -label {Quit} \
    -underline 0 \
    -command {catch "destroy ."; catch "exit 0"}
}

menubutton .frame4.chapters \
  -text "Chapters" \
  -underline 0 \
  -menu ".frame4.chapters.m"

menu .frame4.chapters.m

frame .frame5 \
  -borderwidth 0

label .frame5.chapter \
  -relief raised \
  -text ""

label .frame5.page \
  -relief raised \
  -text ""

frame .frame2 \
  -borderwidth 0

button .frame2.prev \
  -text {Previous page} \
  -command {TUTPrevPage}

button .frame2.doit \
  -text {Perform action} \
  -command {TUTDoIt}

button .frame2.next \
  -text {Next page} \
  -command {TUTNextPage}

frame .frame3 \
  -borderwidth 0

scrollbar .frame3.vscroll \
  -relief raised \
  -command ".frame3.thelist yview"

scrollbar .frame3.hscroll \
  -orient horiz \
  -relief raised \
  -command ".frame3.thelist xview"

listbox .frame3.thelist \
  -relief raised \
  -xscrollcommand ".frame3.hscroll set" \
  -yscrollcommand ".frame3.vscroll set"

tk_menuBar .frame4 .frame4.file .frame4.chapters

# packing
pack append .frame2 \
            .frame2.prev {left fill expand} \
            .frame2.doit {left fill expand} \
            .frame2.next {left fill expand}
pack append .frame3 \
            .frame3.vscroll {left filly} \
            .frame3.hscroll {bottom fillx} \
            .frame3.thelist {left fill expand}
pack append .frame4 \
            .frame4.file {left} \
            .frame4.chapters {left}
pack append .frame5 \
            .frame5.page {right} \
            .frame5.chapter {left fillx expand}
pack append . \
            .frame4 {top fillx} \
            .frame5 {top fillx} \
            .frame2 {bottom fill} \
            .frame3 {bottom fill expand}

# load tutorial scripts
if {[file exists $tutScriptPath]} {
  foreach counter [glob $tutScriptPath/*] {
    if {"[file extension $counter]" == ".scrpt"} {
      source $counter
    }
  }
}

foreach counter $chapterList {
  if {"$counter" != ""} {
    .frame4.chapters.m add command \
      -label "[lindex $counter 0]" \
      -underline 0 \
      -command "TUTSetChapter [lindex $counter 2]"
    if {"[lindex $counter 1]" != ""} {
      .frame4.chapters.m entryconfig last \
        -underline [lindex $counter 1]
    }
  } {
    .frame4.chapters.m add separator
  }
}

if {[info exists xf_TUTSCRIPT]} {
  send {proc XFTUTEval {cmd interp} {eval $cmd; xftut eval TUTUnblock}}
} {
  if {[catch "send \"$tutInterpreter\" \"set argc\""]} {
    puts "It seems that your X server prevents send(n) from working. Please"
    puts "check the Tcl/Tk FAQ files on how to make Tk's send(n) command"
    puts "working. A short description is also given in the XF FAQ file"
    puts "that comes with the distribution."
  }
  send $tutInterpreter {proc XFTUTEval {cmd interp} {eval $cmd; send $interp TUTUnblock}}
}


TUTSetChapter intro

# eof
