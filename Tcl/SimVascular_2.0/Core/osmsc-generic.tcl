#
# Copyright (c) 2009-2013 Open Source Medical Software Corporation.
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
#  generic helpers
#

proc stripzeros {value} {
   set retval [string trimleft $value 0]
   if { ![string length $retval] } {
      return 0
   }
   return $retval
}

proc shortwait {} {
  set onesecond 0
  after 100 {set onesecond 1}
  vwait onesecond
}

proc vtp_calc_avg_pressure {exterior_results_surface_mesh sim_units outPD} {

  set pointData [[repos_exportToVtk -src $exterior_results_surface_mesh] GetPointData]

  set arraycalc tmp-array-calculator
  catch {$arraycalc Delete}
  vtkArrayCalculator $arraycalc
  $arraycalc SetAttributeModeToUsePointData
  $arraycalc SetInputDataObject [repos_exportToVtk -src $exterior_results_surface_mesh]

  set myfunc "( "
  set id 0
  for {set i 0} {$i < [$pointData GetNumberOfArrays]} {incr i} {
    set abstractArrayName [[$pointData GetAbstractArray $i] GetName]
    if {[string range $abstractArrayName 0 8] == "pressure_"} {
      puts "$abstractArrayName"
      $arraycalc AddScalarVariable s$id $abstractArrayName 0
      set myfunc "$myfunc s$id +"
      incr id 
    }
  }
  set myfunc "$myfunc 0 ) / $id"

  if {$sim_units == "cm"} {
    set myfunc "$myfunc * 76.0 / 101325.0"
  } elseif {$sim_units == "mm"} {
    set myfunc "$myfunc * 760.0 / 101325.0"
  }

  puts "myfunc: $myfunc"

  $arraycalc SetFunction $myfunc
  $arraycalc SetResultArrayName pressure_avg_mmHg
  $arraycalc Update

  set aa tmp-aa
  catch {$aa Delete}
  vtkAssignAttribute $aa
  $aa SetInputConnection [$arraycalc GetOutputPort]
  $aa Assign pressure_avg_mmHg SCALARS POINT_DATA
  $aa Update

  catch {repos_delete -obj $outPD}
  repos_importVtkPd -src [$aa GetOutput] -dst $outPD

}


proc combine_all_wall_files {all_wall_files out_file} {

  set wall /tmp/combo/wall
  set wall_merged /tmp/combo/wall_merged

  # combine all walls into single wall file
  set all_wall_objs {}
  foreach i $all_wall_files {
    set facename [file rootname [file tail $i]]
    set wall_obj /tmp/combo/original_name/$facename
    catch {repos_delete -obj $wall_obj}
    repos_readXMLPolyData $i $wall_obj
    lappend all_wall_objs $wall_obj
  }
  catch {repos_delete -obj $wall}
  geom_appendPds $all_wall_objs $wall
  catch {repos_delete -obj $wall_merged}
  geom_mergePts -src $wall -dst $wall_merged
  repos_writeXMLPolyData $wall_merged $out_file

  foreach i $all_wall_objs {
    repos_delete -obj $i
  }
  repos_delete -obj $wall
  repos_delete -obj $wall_merged

}
