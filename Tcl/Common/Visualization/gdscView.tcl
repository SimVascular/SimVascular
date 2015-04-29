#===========================================================================
#    
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
#
# Portions of the code Copyright (c) 1998-2007 Stanford University,
# Charles Taylor, Nathan Wilson, Ken Wang.
#
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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
#===========================================================================    

# --------
# gdscView
# --------
#
proc gdscView args {

#@author Nathan Wilson
#@a args: passed directly onto <p gdscGeneralView> prepended
#@a args: with vtkRenderer gdscViewWindow(ren) which is automatically
#@a args: created by this proc.
#@c This is a user-friendly wrapper around <p gdscGeneralView>.
#@c
#@c It allows the user to not worry about tracking which window
#@c is being used for display, and clears the window prior to
#@c updating.  See <p gdscGeneralView> for more discussion.
#@r see <p gdscGeneralView>.
  # check to see if the render window exists.  If it doesn't create it.
  global gdscViewWindow

  set createWindow 0
  if {[info commands .gdscRenWin] == ""} {
     set createWindow 1
  } elseif {[info commands $gdscViewWindow(ren)] == ""} {
     set createWindow 1
  }

  # create the window if its needed
  if {$createWindow == 0} {

    # clear the window of polydata objects
    catch {vis_pRmAll $gdscViewWindow(ren)}

    # clear the window of all other actors
    set actorCollection [$gdscViewWindow(ren) GetActors]
    set numActors [$actorCollection GetNumberOfItems]
    $actorCollection InitTraversal
    for {set i 0} {$i < $numActors} {incr i} {
       set actor [$actorCollection GetNextActor]
       $gdscViewWindow(ren) RemoveActor $actor

    }

  } else {

    set gdscViewWindow(exists) true

    # Briefly, the "vis_initgr" command
    # creates a vtk rendering window and returns the name
    # of a vtkRenderer object.  We store that returned value in
    # the variable "ren".

    #set gdscViewWindow(ren) [vis_initgr gdscRenWin]
    set gdscViewWindow(ren) [vis_initTKgr gdscRenWin "SimVascular Graphics Window" {} {}]
  }

  if {[llength $args] == 0} {
    #puts "No objects specified in gdscView."
    #puts "Nothing done."
    return
  }

  # save the names of the polydata objects currently
  # displayed in the window.

  # need to be tricky so that args is handled properly
  set gdscViewWindow(workRepPD) [eval {gdscGeneralView $gdscViewWindow(ren)} $args]

  catch {Reset .gdscRenWin.f1.r1 1 1}

}

# ---------------
# gdscGeneralView
# ---------------
#


proc gdscGeneralView args {

#@author Nathan Wilson
#@a args: a vtkRenderer and a set of objects to display.
#@c This routine takes a list of inputs of SolidModels, meshes,
#@c vtkPolyData repository objects.  In addition, if the name
#@c given isn't in the repository, it checks to see if the
#@c name is a vtk image object and displays that as well.&p
#@c
#@c More specifically, for each SolidModel in "objlist",
#@c a temporary polyDataSet is created (using the
#@c object method GetPolyData).&p
#@c
#@c Each repository object is checked for the following keys:&p
#@c  color      == string specifying a valid unix color name
#@c                or vtk color (default is blue)&p
#@c                or 3 real valued numbers for rgb color (sum==1)
#@c  opacity    == transparency level&p
#@c  background == background color&p
#@c  width  == background color&p
#@c  showK      == show scalar information&p
#@c
#@c inputs:  a list, of which the first argument is a
#@c          render window, the rest are repository
#@c          objects&p
#@c
#@c outputs:  updates a render window with objects
#@c           to be displayed.&p
#@c
#@r a list of polydata objects added to window
#@r          (images are handled separately)
#@c


  #  Check to make sure that objlist contains valid
  #  solid model objects, delete invalid objects from
  #  a "working list".

  if {[llength $args] < 1} {
    puts "No objects specified in gdscGeneralView."
    puts "Nothing done."
    return
  }

  set stuff_added {}
  set GeneralRenderWindow [lindex $args 0]

  set workObjs {}
  set imageObjs {}

  foreach j [lrange $args 1 end] {
    foreach i [split $j] {
      if {[repos_exists -obj $i] == 0} {
          puts "Warning:  Object \"$i\" does not exist and"
          puts "          is being ignored in gdscGeneralView."
          continue
      }
      

      if {[repos_type -obj $i] != "SolidModel" && \
          [repos_type -obj $i] != "PolyData" && \
          [repos_type -obj $i] != "Mesh" && \
          [repos_type -obj $i] != "StructuredPts"} {
        puts "Warning:  Object \"$i\" is not of type SolidModel, PolyData,"
        puts "          Mesh, or StructuredPts and is being ignored in gdscGeneralView."
        continue
      }

      lappend workObjs $i 
    }
  }

  if {[llength $workObjs] == 0} {
    puts "Warning:  No valid objects in list passed to"
    puts "          gdscGeneralView, nothing done."
    return
  }
 

   
  # Create tmp vtkPolyData sets for SolidModel objects 
  # needed by code below.

  # workPDS contains the working list of vtk pointers to vtkPolyDataSets.
  set workPDS {}
  # workRepPD contains the working list of repository PolyDatas.
  set workRepPD {}

  # collect objects from args to be displayed
  foreach i $workObjs {
    if {[repos_type -obj $i] == "PolyData"} {
      lappend workPDS [repos_exportToVtk -src $i]
      lappend workRepPD $i
    } elseif {[repos_type -obj $i] == "StructuredPts"} {
      lappend imageObjs $i
    } elseif {[repos_type -obj $i] == "SolidModel"} {
      global gOptions
      if {$gOptions(facet_max_edge_size) != ""} {
        $i GetPolyData -result /tmp/gdscView/$i \
             -max_edge_size $gOptions(facet_max_edge_size)
      } else {
        $i GetPolyData -result /tmp/gdscView/$i
      }
      lappend workPDS [repos_exportToVtk -src /tmp/gdscView/$i]
      lappend workRepPD /tmp/gdscView/$i
    } elseif {[repos_type -obj $i] == "Mesh"} {
      $i GetPolyData -result /tmp/gdscView/$i
    } else {
      puts "Error:  Object $i not of type PolyData or SolidModel."
      return
    } 
  }
 
  # default to black for background color
  set bgcolor black

  #  Loop over the objects
  for {set i 0} {$i < [llength $workPDS]} {incr i} {
    set curPDS [lindex $workPDS $i]
    set curObj [lindex $workObjs $i]
    set curRepObj [lindex $workRepPD $i]

    # get color from keys defined on solid object, 
    # default to blue otherwise.

    if {[lsearch [repos_getLabelKeys -obj $curObj] color] == -1} {
        #puts "Warning:  No color specified for $curObj."
        #puts "          Defaulting to blue."
        set color blue
    } else {
        set color [repos_getLabel -obj $curObj -key color]
    }

    # if color is a 3 digit string, keep it as rgb
    set b {}
    foreach c $color { lappend b [string is double $c]}
    if { [lsearch b 0] != -1 || [llength $color] != 3} {
    set rgb [vis_utilsGetRGBfromColorString $color]
    } else {
      set rgb $color
    }

    set actor_curRepObj [vis_pGetActor $GeneralRenderWindow $curRepObj]
    if { $actor_curRepObj == ""} {
      set actor_curRepObj [vis_pRepos $GeneralRenderWindow $curRepObj]
    }
    lappend stuff_added $curRepObj

    # shade curvature for appropriate polydata
    if {[lsearch [repos_getLabelKeys -obj $curObj] showK] != -1} {
      set p $actor_curRepObj
      [$p GetMapper] ScalarVisibilityOn
      [$p GetMapper] SetLookupTable gBlueToRedLUT
      set range [repos_getLabel -obj $curObj -key showK]
      if {[llength $range] != 2} {
        set range [[[[[$p GetMapper] GetInput] GetPointData] GetScalars] GetRange]
      }
      [$p GetMapper] SetScalarRange [lindex $range 0] [lindex $range 1]
    }

    [$actor_curRepObj GetProperty] SetColor \
	[lindex $rgb 0] [lindex $rgb 1] [lindex $rgb 2]
     
    if {[lsearch [repos_getLabelKeys -obj $curObj] opacity] == -1} {
      [$actor_curRepObj GetProperty] SetOpacity 1.0
    } else { 
      set opacity [repos_getLabel -obj $curObj -key opacity]
      [$actor_curRepObj GetProperty] SetOpacity $opacity    
    }

    if {[lsearch [repos_getLabelKeys -obj $curObj] width] != -1} {
      set width [repos_getLabel -obj $curObj -key width]
      [$actor_curRepObj GetProperty] SetLineWidth $width
     } 

    if {[lsearch [repos_getLabelKeys -obj $curObj] background] != -1} {
      set bgcolor [repos_getLabel -obj $curObj -key background]
    }
  }

  # note: only the bgcolor color of the last object is used,
  #       the rest are ignored.
  if {[info globals $bgcolor] == ""} {
      set rgb [gdscGetUnixColor $bgcolor]
  } else {
      global $bgcolor
      set rgb [eval set $bgcolor]
  }
  $GeneralRenderWindow SetBackground [lindex $rgb 0] [lindex $rgb 1] [lindex $rgb 2]

  # display images
  foreach i $imageObjs {
    vis_imgRepos $GeneralRenderWindow $i
  }

  # delete temporary polydata objects
  foreach i $workObjs {
    if {[repos_type -obj $i] == "SolidModel"} {
      repos_delete -obj /tmp/gdscView/$i
    } 
  }

  # update display
  vis_render $GeneralRenderWindow

  return $stuff_added
}


