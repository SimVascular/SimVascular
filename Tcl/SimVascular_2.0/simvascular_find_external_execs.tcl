# ----------------
# Find executables
# ----------------

proc modules_registry_query {regpath regpathwow key} {
    global tcl_platform
    set value {}
    if {$tcl_platform(platform) == "windows"} {
      package require registry
	if {![catch {set value [registry get $regpath $key]} msg]} {
	  return $value
        } else {
	  if {![catch {set value [registry get $regpathwow $key]} msg]} {
	    return $value
	  }
	}
    } else {
	return
    }
}

proc modules_registry_query_latest {regpath regpathwow key} {
    global tcl_platform
    package require registry
    set value {}
    if {$tcl_platform(platform) == "windows"} {
	if {![catch {set mostrecent [lsort -dictionary [registry keys $regpath]]} msg]} {
	  if {$mostrecent != ""} {
	    if {![catch {set value [registry get $regpath\\$mostrecent $key]} msg]} {
	      return $value
            }
	  }
	} else {
	  if {![catch {set mostrecent [lsort -dictionary [registry keys $regpathwow]]} msg]} {
	    if {$mostrecent != ""} {
	      if {![catch {set value [registry get $regpathwow\\$mostrecent $key]} msg]} {
	        return $value
	      }
	    }
	  }
	}
    } else {
	return
    }
}

#
#  Set the paths to external binaries called by the GUI
#

global tcl_platform
if {![file exists [file join $simvascular_home/Tcl/externals_configure.tcl]] } {
    if {$tcl_platform(platform) == "windows"} {
      package require registry
        if {[string range $SV_VERSION end-1 end] == "32"} {
          if [catch {set rundir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP" RunDir]} msg] {
            puts "ERROR:  could not find registry key!"
            set rundir ""
          }
      } else {
        if [catch {set rundir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP" RunDir]} msg] {
          if [catch {set rundir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP" RunDir]} msg] {
            puts "ERROR: could not find registry key:\nHKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP RunDir"
            set rundir ""
          }
        }
      }
      set execext {.exe}
      set execbinext {-bin.exe}
    } else {
      if {$SV_RELEASE_BUILD} {
        set rundir /usr/local/bin
      } else {
      set rundir ""
      }
      set execext {}
      set execbinext {}
    }
}

global gExternalPrograms

# cmake build directly creates a tcl script with the executable paths
if {[file exists [file join $simvascular_home/Tcl/externals_configure.tcl]] } {

    source [file join $simvascular_home/Tcl/externals_configure.tcl]

} else {

  set executable_home $simvascular_home
  set gExternalPrograms(rundir) $rundir

  if {$SV_RELEASE_BUILD != 1} {

    # developer build
    set gExternalPrograms(svpre)          [file join $simvascular_home mypre]
    set gExternalPrograms(svpost)         [file join $simvascular_home mypost]
    set gExternalPrograms(svsolver-nompi) [file join $simvascular_home mysolver-nompi]
    set gExternalPrograms(svsolver-mpi)   [file join $simvascular_home mysolver-mpi]
    set gExternalPrograms(mpiexec)        mpiexec
    set gExternalPrograms(dicom2)         [file join $simvascular_home dicom2$execext]
    set gExternalPrograms(dcmodify)       dcmodify$execext
    set gExternalPrograms(dcmdump)        dcmdump$execext
    set gExternalPrograms(gdcmdump)       gdcmdump$execext

  } else {

     # installed release build
     set gExternalPrograms(svpre)          [file join $executable_home svpre$execbinext]
     set gExternalPrograms(svpost)         [file join $executable_home svpost$execbinext]
     set gExternalPrograms(svsolver-nompi) [file join $executable_home svsolver-nompi$execbinext]
     set gExternalPrograms(svsolver-mpi)   [file join $executable_home svsolver-mpi$execbinext]
     set gExternalPrograms(mpiexec)        mpiexec
     set gExternalPrograms(dicom2)         [file join $simvascular_home dicom2$execext]
     set gExternalPrograms(dcmdump)        dcmdump$execext
     set gExternalPrograms(gdcmdump)       gdcmdump$execext

     # use registry to find seperately installed svsolver package on windows
     if {$tcl_platform(platform) == "windows"} {
       set svpre_exe [modules_registry_query_latest HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\svSolver \
			                            HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\svSolver \
				                    SVPRE_EXE]
       if {$svpre_exe != ""} {
	  if [file exists $svpre_exe] {
	      puts "Found svPre ($svpre_exe)"
	      regsub -all {\\} $svpre_exe / gExternalPrograms(svpre)
	  }
       }
       set svpost_exe [modules_registry_query_latest HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\svSolver \
		  	                             HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\svSolver \
				                     SVPOST_EXE]
       if {$svpost_exe != ""} {
	  if [file exists $svpost_exe] {
	      puts "Found svPost ($svpost_exe)"
	      regsub -all {\\} $svpost_exe / gExternalPrograms(svpost)
	  }
       }
       set svsolver_nompi_exe [modules_registry_query_latest HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\svSolver \
			                                     HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\svSolver \
				                             SVSOLVER_NOMPI_EXE]
       if {$svsolver_nompi_exe != ""} {
	  if [file exists $svsolver_nompi_exe] {
	      puts "Found svSolver ($svsolver_nompi_exe)"
	      regsub -all {\\} $svsolver_nompi_exe / gExternalPrograms(svsolver-nompi)
	  }
       }
       set svsolver_msmpi_exe [modules_registry_query_latest HKEY_LOCAL_MACHINE\\SOFTWARE\\SimVascular\\svSolver \
			                                     HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\SimVascular\\svSolver \
				                             SVSOLVER_MSMPI_EXE]
       if {$svsolver_msmpi_exe != ""} {
	  if [file exists $svsolver_msmpi_exe] {
	      puts "Found svSolver ($svsolver_msmpi_exe)"
	      regsub -all {\\} $svsolver_msmpi_exe / gExternalPrograms(svsolver-mpi)
	  }
       }
     }
   }
}

#
#  use registry to find mpiexec on windows
#

if {$tcl_platform(platform) == "windows"} {
  if {![catch {set mpi_install_dir [registry get "HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Microsoft\\MPI" InstallRoot]} msg]} {
      regsub -all {\\} $mpi_install_dir/bin/mpiexec.exe / gExternalPrograms(mpiexec)
  }
}
