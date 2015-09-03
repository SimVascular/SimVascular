proc guiMeshSimAdaptMesh {} {

  set scriptfile [meshSimWriteAdaptMeshScript]

  meshSimSourceAdaptScript $scriptfile

  global guiMMvars
  global gObjects
  set gObjects(mesh_object) /adapt/internal/meshobject
  set pd /adapt/polydata
  catch {repos_delete -obj $pd}
  set gObjects(mesh_object_pd) $pd
  set guiMMvars(viewMeshVtkFlag) 1

  guiMMviewMeshVtk

  guiMMprintMeshStats
}

proc meshSimWriteAdaptMeshScript {} {
  global gOptions
  global gObjects
  global gFilenames
  global guiMMvars

  set guiMMvars(metric_array_name) dummy

  set basename [file join $gFilenames(adapted_mesh_dir) [file tail $gFilenames(adapted_mesh_dir)]] 
  set gFilenames(adapted_mesh_file) $basename.sms 
  set gFilenames(adapted_vtu_mesh_file) $basename.vtu 
  set gFilenames(adapted_vtp_surface_file) $basename.vtp 
  set gFilenames(adapted_solution_file) $basename.restart.$guiMMvars(last_step_number).1 

  set ascflag                $guiMMvars(phasta_format)
  set model_file             $gFilenames(atdb_solid_file)
  set mesh_file              $gFilenames(mesh_file)
  set vtu_mesh_file          $gFilenames(vtu_mesh_file)
  set vtp_surface_file       $gFilenames(vtp_surface_file)

  set solution_file          $gFilenames(solution_file)
  set out_mesh_file          $gFilenames(adapted_mesh_file)
  set out_vtu_mesh_file      $gFilenames(adapted_vtu_mesh_file)
  set out_vtp_mesh_file      $gFilenames(adapted_vtp_surface_file)
  set out_solution_file      $gFilenames(adapted_solution_file)

  set instep                 $guiMMvars(first_step_number)
  set outstep                $guiMMvars(last_step_number)
  set incr                   $guiMMvars(step_incr)

  set adapt_strategy         $guiMMvars(ms_strategy)
  set adapt_option           $guiMMvars(adapt_option)
  set array_name             $guiMMvars(metric_array_name)

  set reductionRatio         $guiMMvars(error_reduction_factor)
  set maxCoarseFactor        $guiMMvars(gsize)
  set maxRefineFactor        $guiMMvars(min_gsize)
  
  puts "Writing MeshSim Adapt Script File"
  set script_filename [file rootname $out_mesh_file].msas

  set object /adapt/object
  set fp [open $script_filename "w"]

  puts $fp "# MeshSim Adapt Script File"
  set timestamp [clock seconds]
  puts $fp "# Timestamp: $timestamp  ([clock format $timestamp])"
  puts $fp "####################################################"
  puts $fp ""
  puts $fp "set object $object"
  puts $fp "catch {repos_delete -obj $object}"
  puts $fp "global gOptions"
  puts $fp "global gFilenames"
  puts $fp "global gObjects"
  puts $fp "set gOptions(meshing_kernel) MeshSim"
  puts $fp "set gOptions(meshing_solid_kernel) $gOptions(meshing_solid_kernel)"
  puts $fp "mesh_setKernel -name MeshSim"
  puts $fp "solid_setKernel -name $gOptions(meshing_solid_kernel)"
  puts $fp ""
  puts $fp "####################################################"
  puts $fp "# Set up and run Adaptor object"
  puts $fp "####################################################"
  puts $fp ""
  puts $fp "adapt_newObject -result $object"
  puts $fp "$object CreateInternalMeshObject"
  puts $fp "$object LoadModel -file $model_file"
  puts $fp "$object LoadMesh -file $mesh_file"
  puts $fp "$object LoadMesh -file $vtu_mesh_file"
  puts $fp "$object LoadYbarFromFile -file $solution_file"
  puts $fp "$object SetAdaptOptions -flag strategy -value $adapt_strategy"
  puts $fp "$object SetAdaptOptions -flag metric_option -value $adapt_option"
  puts $fp "$object SetAdaptOptions -flag ratio -value $reductionRatio"
  puts $fp "$object SetAdaptOptions -flag hmin -value $maxRefineFactor"
  puts $fp "$object SetAdaptOptions -flag hmax -value $maxCoarseFactor"
  puts $fp "$object SetAdaptOptions -flag instep -value $instep"
  puts $fp "$object SetAdaptOptions -flag outstep -value $outstep"
  puts $fp "$object SetAdaptOptions -flag step_incr -value $incr"
  if {$adapt_strategy == 4} {
    puts $fp "$object SetMetric -input $array_name"
  } else {
    puts $fp "$object SetMetric -input $solution_file"
  }
  puts $fp "$object SetupMesh"
  puts $fp "$object RunAdaptor"
  puts $fp "$object GetAdaptedMesh"
  puts $fp "$object TransferSolution"
  puts $fp "$object WriteAdaptedMesh -file $out_mesh_file"
  puts $fp "$object WriteAdaptedMesh -file $out_vtu_mesh_file"
  puts $fp "$object WriteAdaptedSolution -file $out_solution_file"
  puts $fp ""
  close $fp

  return $script_filename
}

proc meshSimSourceAdaptScript {scriptfile} {

  if {[file exists $scriptfile] == 0} {
    puts "ERROR: Script file does not exist."
    return -code error "ERROR: Script file does not exist."
  }

  source $scriptfile
}

proc guiTetGenAdaptMesh {} {

  set scriptfile [tetGenWriteAdaptMeshScript]

  tetGenSourceAdaptScript $scriptfile

  global guiMMvars
  global gObjects
  set gObjects(mesh_object) /adapt/internal/meshobject
  set pd /adapt/polydata
  catch {repos_delete -obj $pd}
  set gObjects(mesh_object_pd) $pd
  set guiMMvars(viewMeshVtkFlag) 1

  guiMMviewMeshVtk

  guiMMprintMeshStats

}

proc tetGenWriteAdaptMeshScript {} {
  global gOptions
  global gObjects
  global gFilenames
  global guiMMvars

  set basename [file join $gFilenames(adapted_mesh_dir) [file tail $gFilenames(adapted_mesh_dir)]] 
  set gFilenames(adapted_vtu_mesh_file) $basename.vtu 
  set gFilenames(adapted_vtp_surface_file) $basename.vtp 
  set gFilenames(adapted_solution_file) $basename.restart.$guiMMvars(last_step_number).1 

  set ascflag                $guiMMvars(phasta_format)
  set vtu_mesh_file          $gFilenames(vtu_mesh_file)
  set vtp_surface_file       $gFilenames(vtp_surface_file)

  set solution_file          $gFilenames(solution_file)
  set out_vtu_mesh_file      $gFilenames(adapted_vtu_mesh_file) 
  set out_solution_file      $gFilenames(adapted_solution_file)
  set out_vtp_mesh_file      $gFilenames(adapted_vtp_surface_file)

  set instep                 $guiMMvars(first_step_number)
  set outstep                $guiMMvars(last_step_number)
  set incr                   $guiMMvars(step_incr)

  set adapt_strategy         $guiMMvars(tg_strategy)
  set adapt_option           $guiMMvars(adapt_option)

  set reductionRatio         $guiMMvars(error_reduction_factor)
  set maxCoarseFactor        $guiMMvars(gsize)
  set maxRefineFactor        $guiMMvars(min_gsize)
  
  puts "Writing TetGen Adapt Script File"
  set script_filename [file rootname $out_vtu_mesh_file].tgas

  set object /adapt/object
  set fp [open $script_filename "w"]

  puts $fp "# TetGen Adapt Script File"
  set timestamp [clock seconds]
  puts $fp "# Timestamp: $timestamp  ([clock format $timestamp])"
  puts $fp "####################################################"
  puts $fp ""
  puts $fp "set object $object"
  puts $fp "catch {repos_delete -obj $object}"
  puts $fp "global gOptions"
  puts $fp "global gFilenames"
  puts $fp "global gObjects"
  puts $fp "set gOptions(meshing_kernel) TetGen"
  puts $fp "set gOptions(meshing_solid_kernel) PolyData"
  puts $fp "mesh_setKernel -name TetGen"
  puts $fp "solid_setKernel -name PolyData"
  puts $fp ""
  puts $fp "####################################################"
  puts $fp "# Set up and run Adaptor object"
  puts $fp "####################################################"
  puts $fp ""
  puts $fp "adapt_newObject -result $object"
  puts $fp "$object CreateInternalMeshObject"
  puts $fp "$object LoadModel -file $vtp_surface_file"
  puts $fp "$object LoadMesh -file $vtu_mesh_file"
  puts $fp "$object LoadYbarFromFile -file $solution_file"
  puts $fp "$object SetAdaptOptions -flag strategy -value $adapt_strategy"
  puts $fp "$object SetAdaptOptions -flag metric_option -value $adapt_option"
  puts $fp "$object SetAdaptOptions -flag ratio -value $reductionRatio"
  puts $fp "$object SetAdaptOptions -flag hmin -value $maxRefineFactor"
  puts $fp "$object SetAdaptOptions -flag hmax -value $maxCoarseFactor"
  puts $fp "$object SetAdaptOptions -flag instep -value $instep"
  puts $fp "$object SetAdaptOptions -flag outstep -value $outstep"
  puts $fp "$object SetAdaptOptions -flag step_incr -value $incr"
  if {$adapt_strategy == 4} {
    puts $fp "$object SetMetric -input $array_name"
  } else {
    puts $fp "$object SetMetric -input $solution_file"
  }
  puts $fp "$object SetupMesh"
  puts $fp "$object RunAdaptor"
  puts $fp "$object GetAdaptedMesh"
  puts $fp "$object TransferSolution"
  puts $fp "$object WriteAdaptedMesh -file $out_vtu_mesh_file"
  puts $fp "$object WriteAdaptedModel -file $out_vtp_mesh_file"
  puts $fp "$object WriteAdaptedSolution -file $out_solution_file"
  puts $fp ""
  close $fp

  return $script_filename
}

proc tetGenSourceAdaptScript {scriptfile} {
  if {[file exists $scriptfile] == 0} {
    puts "ERROR: Script file does not exist."
    return -code error "ERROR: Script file does not exist."
  }

  source $scriptfile
}

proc guiChooseOutputDirectory {gFilename} {
  global gFilenames

  set currentdir {}
  if {$gFilenames($gFilename) == "adapt-complete"} {
    set currentdir [pwd]
  } else {
    set currentdir $gFilenames($gFilename)
  }

  set newdir [tk_chooseDirectory -mustexist 1 -title "Choose adapt-complete directory" -initialdir $currentdir]

  if {$newdir == ""} {
    return -code error "Cannot have blank directory"
  } else {
    set gFilenames($gFilename) $newdir
  }
}
