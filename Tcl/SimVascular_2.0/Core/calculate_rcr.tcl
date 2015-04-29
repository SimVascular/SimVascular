#
# Copyright (c) 2012-2013 Open Source Medical Software Corporation.
#
# All rights reserved.
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# this file was originally called calculate_rcr-v2_ana.tcl (2012-07-11)
# but added the RCR_version proc
#
# version 1.0: see above
# version 1.1: 2013-03-06: added ability to write stdout to file
# version 1.2: 2013-04-01: added proc to create rcrt.dat
#                          for special formatted params file
#

proc RCR_version {} {
    global gRCR_stdout_fp
    set rcr_stdout $gRCR_stdout_fp
    # major_ver_no minor_ver_no release_date
    return [list 1 1 2013-03-06]
}

proc RCR_stdout_to_fn {fn} {
   global gRCR_stdout_fp
   if {$gRCR_stdout_fp != "stdout"} {
     close $gRCR_stdout_fp
   }
   if {$fn == ""} {
     set gRCR_stdout_fp stdout
   } else {
     set fp [open $fn a]
     set gRCR_stdout_fp $fp
   }
}
proc RCR_read_flow_file {fn} {

  global gRCR_stdout_fp
  set rcr_stdout $gRCR_stdout_fp

  set fp [open $fn r]

  set pts {}
  while {[gets $fp line] >= 0} {
    set line [string trim $line]
    if {[string index $line 0] == "#" || $line == ""} {
      continue
    }
    if {[scan $line "%lf %lf" time flow] != 2} {
       close $fp
       return -code error "Incorrect format of line in flow rate file ($line)."
    }
    lappend pts [list $time $flow]
  }
  close $fp

  return $pts

}


proc RCR_calc_pressures {R C period ratio units flow_with_time scale_factor_q mean_q mean_p} {

    global gRCR_stdout_fp
    set rcr_stdout $gRCR_stdout_fp

    upvar 1 mean_q Qzero
    upvar 1 mean_p Pzero

    set I [::math::complexnumbers::complex 0 1]

    # ratio value determines split of R

    set Rc [expr $R*$ratio]
    set Rd [expr $R*(1-$ratio)]

    # number of modes to keep?  arbitrarily use 2x number of flow points
    set num_kept_modes [expr [llength $flow_with_time] * 4]

    # get fourier modes for flow waveform
 
    set modes [math_FFT -pts $flow_with_time -nterms $num_kept_modes -numInterpPts 1024]
    set Qzero [lindex [lindex $modes 0] 0]
    
    # for convolution?

    for {set i 0} {$i < $num_kept_modes} {incr i} {
      set freq [expr 1.0/$period*$i]
      lappend frequencies $freq
      lappend omega [expr 2.0*[math_pi]*$freq]
    }

    foreach omg $omega {
        #  this is the expression we need to calculate with complex numbers
        #  $Rc+1.0/(1.0/$Rd+$I*$omg*$C)
        set den1 [list [expr 1.0/$Rd] 0]
        set den2 [::math::complexnumbers::* [list 0 1] [list [expr $omg*$C] 0]]
        set den [::math::complexnumbers::+ $den1 $den2]
        set part2 [::math::complexnumbers::/ [list 1.0 0] $den]
        set Z [::math::complexnumbers::+ [list $Rc 0] $part2]
	lappend Z_RCR $Z
    }
   
    # convolve?

    for {set i 0} {$i < [llength $Z_RCR]} {incr i} {
      lappend modified_modes [::math::complexnumbers::* [lindex $modes $i] [lindex $Z_RCR $i]]
    }

    set number_of_points_per_cycle [expr [llength $flow_with_time] * 2]
    
    # Calculating the time
    set dt [expr double($period)/($number_of_points_per_cycle)]    
    set pomega [expr 2.0*[math_pi]*1.0/$period]
    set pressure [math_inverseFFT -terms $modified_modes -t0 0 -dt $dt -omega $pomega -numPts [expr $number_of_points_per_cycle + 1]]
    set pZero [lindex [lindex $modified_modes 0] 0]
    #puts $rcr_stdout "pressure: $pressure"

    # debugging code!
    #set flows [math_inverseFFT -terms $modes -t0 0 -dt $dt -omega $pomega -numPts [expr $number_of_points_per_cycle + 1]]
    #foreach i $flows {
    #  puts $rcr_stdout $i
    #}

    return $pressure

}


proc RCR_estimator {R C period ratio units flow_filename scale_factor flip_flow_sign} {

    global gRCR_stdout_fp
    set rcr_stdout $gRCR_stdout_fp

    set flow_pts [RCR_read_flow_file $flow_filename]

    # flip the sign on the flow if desired
    if {[string toupper $flip_flow_sign] == "TRUE"} {
      foreach pt $flow_pts {
	lappend flipped_pts [list [lindex $pt 0] [expr -1.0*[lindex $pt 1]]]
      }
      set flow_pts $flipped_pts
    }

    # scale flow if desired
    foreach pt $flow_pts {
	lappend new_pts [list [lindex $pt 0] [expr $scale_factor*[lindex $pt 1]]]
    }
    set flow_pts $new_pts
    
    set mean_q 0
    set mean_r 0

    set pressure [RCR_calc_pressures $R $C $period $ratio $units $flow_pts $scale_factor mean_q mean_p]

    if {$units == "cm"} {
      set pressure_units_in_1_mmHg 1333.2237
    } elseif {$units == "mm"} {
      set pressure_units_in_1_mmHg 133.32237
    } else {
      return -code error "ERROR:  invalid units option ($units)!"
    }

    foreach p [lrange $pressure 0 end-1] {
	lappend scaled_pressure [expr [lindex $p 1] / double($pressure_units_in_1_mmHg)]
    }
    #puts $rcr_stdout "scaled_pressure: $scaled_pressure"
    set sorted_p [lsort -real -increasing $scaled_pressure]
    #puts $rcr_stdout "sorted_p: $sorted_p"

    set systolic_pressure [lindex $sorted_p end]
    set diastolic_pressure [lindex $sorted_p 0]
    set pulse_pressure [expr $systolic_pressure-$diastolic_pressure]
     
    set mean_p 0
    foreach p $scaled_pressure  {
	set mean_p [expr $mean_p + $p]
    }
    set mean_p [expr $mean_p/(1.0*[llength $scaled_pressure])]

    puts $rcr_stdout [format "%20s %12.3f" "diastolic pressure:" $diastolic_pressure]
    puts $rcr_stdout [format "%20s %12.3f" "mean pressure:" $mean_p]
    puts $rcr_stdout [format "%20s %12.3f" "systolic pressure:" $systolic_pressure]
    puts $rcr_stdout [format "%20s %12.3f" "pulse pressure:" $pulse_pressure]
    puts $rcr_stdout [format "%20s %12.3f" "mean flow:" $mean_q]
    
    return [list $diastolic_pressure $mean_p $systolic_pressure $pulse_pressure $mean_q]

}


proc RCR_branch_estimator {targetQ targetP TAC period ratio units flow_filename flip_flow_sign} {

  global gRCR_stdout_fp
  set rcr_stdout $gRCR_stdout_fp

  #
  # calculate total inflow
  #
  if {$units == "cm"} {
    set pressure_units_in_1_mmHg 1333.2237
  } elseif {$units == "mm"} {
    set pressure_units_in_1_mmHg 133.32237
  } else {
    return -code error "ERROR:  invalid units option ($units)!"
  }

  set targetP [expr $targetP * $pressure_units_in_1_mmHg]

  set flow_pts [RCR_read_flow_file $flow_filename]
  # number of modes to keep?  arbitrarily use 2x number of flow points
  set num_kept_modes [expr [llength $flow_pts] * 4]
  set modes [math_FFT -pts $flow_pts -nterms $num_kept_modes -numInterpPts 1024]
  set total_flow [lindex [lindex $modes 0] 0]
  if {[string toupper $flip_flow_sign] == "TRUE"} {
    set total_flow [expr -1.0*$total_flow]
  } 

  # if targetQ is blank, use total flow
  if {$targetQ == ""} {
    set targetQ $total_flow
  }

  set R [expr 1.0 * $targetP / $targetQ ]
  set scale_factor [expr 1.0 * $targetQ / $total_flow]

  set C [expr $scale_factor * $TAC]

  set Rc [expr $R*$ratio]
  set Rd [expr $R*(1-$ratio)]

  # store so we can access values elsewhere
  global gRCRvars
  set gRCRvars(branch_estimator_last_C) $C
  set gRCRvars(branch_estimator_last_Rc) $Rc
  set gRCRvars(branch_estimator_last_Rd) $Rd
  set gRCRvars(branch_estimator_last_ratio) $ratio

  puts $rcr_stdout ""
  puts $rcr_stdout [format "%20s %12.3f" "R total:" $R]
  puts $rcr_stdout [format "%20s %12.3f" " % flow:" [expr 100.0*$scale_factor]]
  puts $rcr_stdout [format "%20s %12.3f" "Rp:" $Rc]
  puts $rcr_stdout [format "%20s %12.3f" "C:" $C]
  puts $rcr_stdout [format "%20s %12.3f" "Rd:" $Rd]

  set pressures [RCR_estimator $R $C $period $ratio $units $flow_filename $scale_factor $flip_flow_sign]

  puts -nonewline $rcr_stdout "\n RCR "
  puts $rcr_stdout [format "%15.6f %15.6f %15.6f" $Rc $C $Rd]
  puts $rcr_stdout ""

  return $pressures

}

proc RCR_TAC_optimize {ratio targetSP targetDP targetMP period units flow_filename flip_flow_sign} {

  global gRCR_stdout_fp
  set rcr_stdout $gRCR_stdout_fp

  package require math::optimize

  global gRCRvars
  set gRCRvars(targetQ) {}
  set gRCRvars(targetSP) $targetSP
  set gRCRvars(targetDP) $targetDP
  set gRCRvars(targetMP) $targetMP
  set gRCRvars(TAC) {}
  set gRCRvars(ratio) $ratio
  set gRCRvars(period) $period
  set gRCRvars(units) $units
  set gRCRvars(flow_filename) $flow_filename
  set gRCRvars(flip_flow_sign) $flip_flow_sign

  set minnie 0.0
  if {$units == "mm"} {
    set maxie  100.0
  } elseif {$units == "cm"} {
    set maxie  0.0100
  } else {
    return -code error "ERROR: invalid units ($units)!"
  }
  set TAC [::math::optimize::min_bound_1d RCR_utility_optimize_func $minnie $maxie -trace 1]

  puts $rcr_stdout "\n\n  optimal TAC: $TAC"
  return $TAC
  
}

proc RCR_branch_optimize {TAC targetQ targetSP targetDP targetMP period units flow_filename flip_flow_sign} {

  global gRCR_stdout_fp
  set rcr_stdout $gRCR_stdout_fp

  package require math::optimize

  global gRCRvars
  set gRCRvars(targetQ) $targetQ
  set gRCRvars(targetSP) $targetSP
  set gRCRvars(targetDP) $targetDP
  set gRCRvars(targetMP) $targetMP
  set gRCRvars(TAC) $TAC
  set gRCRvars(ratio) {}
  set gRCRvars(period) $period
  set gRCRvars(units) $units
  set gRCRvars(flow_filename) $flow_filename
  set gRCRvars(flip_flow_sign) $flip_flow_sign

  set minnie 0.0
  set maxie  1.0
  set ratio [::math::optimize::min_bound_1d RCR_utility_optimize_func $minnie $maxie -trace 1]

  puts $rcr_stdout "\n\n  optimal ratio: $ratio"

  # retrieve values from elsewhere
  global gRCRvars
  set C $gRCRvars(branch_estimator_last_C)
  set Rc $gRCRvars(branch_estimator_last_Rc)
  set Rd $gRCRvars(branch_estimator_last_Rd)
  set ratio $gRCRvars(branch_estimator_last_ratio)

  return [list $Rc $C $Rd $ratio] 

}


proc RCR_branch_optimize_PP {TAC targetQ targetPP targetMP period units flow_filename flip_flow_sign} {
    global gRCR_stdout_fp
    set rcr_stdout $gRCR_stdout_fp
    return [RCR_branch_optimize $TAC $targetQ $targetPP 0.0 $targetMP $period $units $flow_filename $flip_flow_sign] 
}


proc RCR_utility_optimize_func { x } {

  global gRCR_stdout_fp
  set rcr_stdout $gRCR_stdout_fp

  global gRCRvars

  # can either be optimizing ratio or TAC, but not both!
  if {$gRCRvars(ratio) == ""} {
    set ratio $x
    set TAC   $gRCRvars(TAC)
  } else {
    set ratio $gRCRvars(ratio)
    set TAC   $x
  }

  set targetQ  $gRCRvars(targetQ)
  set targetSP $gRCRvars(targetSP)
  set targetDP $gRCRvars(targetDP)
  set targetMP $gRCRvars(targetMP)
  set period   $gRCRvars(period)
  set units    $gRCRvars(units)
  set flow_filename  $gRCRvars(flow_filename)
  set flip_flow_sign $gRCRvars(flip_flow_sign)

  set targetPP [expr 1.0*($targetSP - $targetDP)]

  set pressures [RCR_branch_estimator $targetQ $targetMP $TAC $period $ratio $units $flow_filename $flip_flow_sign]

  set  diastolic_pressure [lindex $pressures 0] 
  set  mean_p [lindex $pressures 1]
  set  systolic_pressure [lindex $pressures 2]
  set  pulse_pressure [lindex $pressures 3]
  set  mean_q [lindex $pressures 4]

# set diffSP [expr abs(($systolic_pressure - $targetSP)/$targetSP)]
#  set diffDP [expr abs(($diastolic_pressure - $targetDP)/$targetDP)]
  set diffPP [expr abs(($pulse_pressure - $targetPP)/$targetPP)]

  #puts $rcr_stdout "ratio: $ratio diffs: $diffSP $diffDP $diffPP"
  return [expr 100.0*$diffPP]
  #return [expr 100.0*($diffSP + $diffDP + $diffPP)]

}



proc RCR_create_multi_branch_rcrtdat {in_fn out_fn_excel out_fn} {

  #
  #  read in params
  #

  catch {unset target_flow}
  catch {unset target_MAP}
  catch {unset myvars}

  set fp [open $in_fn r]

  set foundFaceName 0

  while {[gets $fp line] >= 0} {
    set line [string trim $line]
    if {$line == ""} continue
    if {[string index $line 0] == "\#"} continue
    if {$foundFaceName} {
      set target_flow([lindex $line 0]) [lindex $line 1]
      set target_MAP([lindex $line 0]) [lindex $line 2]     
    } else {
      if {[lindex $line 0] == "face_name"} {
        set foundFaceName 1
        continue
      }
      set myvars([lindex $line 0]) [lindex $line 1]
    }

  }

  close $fp

  #
  #  calculate RCR for each branch
  #

  catch {unset branch_rcr}

  set all_face_names [lsort -dictionary [array names target_flow]]

  foreach face $all_face_names {
    # RCR_branch_optimize_PP TAC targetQ targetPP targetMP period units flow_filename flip_flow_sign
    set branch_rcr($face) [RCR_branch_optimize_PP $myvars(TAC) \
                                                $target_flow($face) \
                                                $myvars(targetPP) \
                                                $target_MAP($face) \
                                                $myvars(period) \
                                                $myvars(units) \
                                                $myvars(flow_filename) \
		                                $myvars(flip_flow_sign)]                    
  }

  set fp [open $out_fn_excel w]
  puts $fp "face_name\tRp\tC\tRd\tratio"
  foreach face $all_face_names {
    set rcr $branch_rcr($face)
    puts $fp "$face\t[lindex $rcr 0]\t[lindex $rcr 1]\t[lindex $rcr 2]\t[lindex $rcr 3]"
  }
  close $fp

  set nptsRCRmax 2
  set numDataRCR 2
  set ValuePdist1 "0 0"
  set ValuePdist2 "$myvars(period) 0"
 
  set fp [open $out_fn w]
  fconfigure $fp -translation lf

  puts $fp $nptsRCRmax

  foreach face $all_face_names {
    set rcr $branch_rcr($face)
    puts $fp $numDataRCR
    puts $fp [lindex $rcr 0]
    puts $fp [lindex $rcr 1]
    puts $fp [lindex $rcr 2]
    puts $fp $ValuePdist1
    puts $fp $ValuePdist2
  }

  close $fp

}

