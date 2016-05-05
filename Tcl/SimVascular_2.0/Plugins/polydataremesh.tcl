proc guiSV_model_remesh_polydata_mmg {} {
  global gObjects
  global guiMMGvars
  global symbolicName
  global gOptions
  global gKernel

  set tv $symbolicName(guiSV_model_tree)
  set model [guiSV_model_get_tree_current_models_selected]
  if {[llength $model] != 1} {
    return -code error "ERROR: Can only smooth one PolyData model at a time"
  }
  set kernel $gKernel($model)
  if {$kernel != "PolyData"} {
    return -code error "ERROR: Solid kernel must be PolyData for operation"
  }
  set gOptions(meshing_solid_kernel) $kernel
  solid_setKernel -name $kernel

  guiSV_model_add_to_backup_list $kernel $model

  set hmax $guiMMGvars(hmax)
  set hmin $guiMMGvars(hmin)
  set angle $guiMMGvars(angle)
  set hgrad $guiMMGvars(hgrad)
  set hausd $guiMMGvars(hausd)

  set pd /models/PolyData/$model
  if {![repos_exists -obj $pd]} {
    $model GetPolyData -result $pd
  }
  set remesh_pd [polydata_remesh_mmg $pd $hmax $hmin $hausd $angle $hgrad]

  $model SetVtkPolyData -obj $remesh_pd

  guiSV_model_update_tree
  guiSV_model_update_view_model $kernel $model
}

proc polydata_remesh_mmg {pd hmax hmin hausd angle hgrad} {

  set outpd /tmp/remesh/mmg/pd
  catch {repos_delete -obj $outpd}

  mmg_remesh -src $pd -dst $outpd -hmax $hmax -hmin $hmin -hausd $hausd -angle $angle -hgrad $hgrad

  return $outpd
}
