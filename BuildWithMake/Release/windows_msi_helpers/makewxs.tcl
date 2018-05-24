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

set SV_VERSION [lindex $argv 0]
set SV_PLATFORM [lindex $argv 1]
set SV_TIMESTAMP [file tail [glob [lindex $argv 2]/*]]
set SV_RELEASE_VERSION_NO [lindex $argv 3]
set SV_SHORT_NAME [lindex $argv 4]
set SV_EXECUTABLE [lindex $argv 5]
set SV_FILES [lindex $argv 6]
set SV_FULL_VER_NO [lindex $argv 7]
set TCL_LIBRARY_TAIL [file tail [lindex $argv 8]]
set TK_LIBRARY_TAIL [file tail [lindex $argv 9]]

puts "SV_FILES $SV_FILES"

global pwd
global releasedir

set pwd {P:/package}

if {$tcl_platform(platform) == "windows"} {
  set releasedir [pwd]
} else {
  #assume cygwin
  set releasedir [exec cygpath -m [pwd]]
}

puts "building wxs for $argv"

set outputRegistry 0

proc file_find {dir wildcard args} {

  global pwd
  global releasedir
  global SV_SHORT_NAME
  global TCL_LIBRARY_TAIL
  global TK_LIBRARY_TAIL

  if {[llength $args] == 0} {
     set rtnme {}
  } else {
     upvar rtnme rtnme
  }

  foreach j $dir {
    set files [glob -nocomplain [file join $j $wildcard]]
    # print out headers
    global idno
    global outfp
    set id [incr idno]
    global component_ids
    lappend component_ids $id
    set guid [exec tmp/uuidgen.exe 1]
    puts $outfp "<Component Id='id[format %04i $id]' Guid='$guid'>"
    global outputRegistry
    if {!$outputRegistry} {
        global SV_TIMESTAMP
        global SV_RELEASE_VERSION_NO
        global SV_VERSION
        global SV_SHORT_NAME
        global SV_EXECUTABLE
        global SV_FILES
        set outputRegistry 1
	puts $outfp "<Registry Id='regid1' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='InstallDir' Action='write' Type='string' Value='\[INSTALLDIR\]' />"
	puts $outfp "<Registry Id='regid2' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='TimeStamp' Action='write' Type='string' Value='$SV_TIMESTAMP' />"
	puts $outfp "<Registry Id='regid3' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='RunDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP' />"
	puts $outfp "<Registry Id='regid4' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='TclLibDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP\\lib\\$TCL_LIBRARY_TAIL' />"
	puts $outfp "<Registry Id='regid5' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='TkLibDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP\\lib\\$TK_LIBRARY_TAIL' />"
	puts $outfp "<Registry Id='regid7' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='HomeDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP' />"
	puts $outfp "<Registry Id='regid8' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='PythonHome' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP\\Python27' />"
	puts $outfp "<Registry Id='regid9' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='PythonPackagesDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP\\Python27\\Lib\\vtk-packages;\[INSTALLDIR\]$SV_TIMESTAMP\\Python27\\Lib\\site-packages;\[INSTALLDIR\]$SV_TIMESTAMP' />"
	puts $outfp "<Registry Id='regid10' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='SV_PLUGIN_PATH' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP\\mitk\\bin\\plugins\\RelWithDebInfo;\[INSTALLDIR\]$SV_TIMESTAMP\\mitk\\bin\\RelWithDebInfo;\[INSTALLDIR\]$SV_TIMESTAMP\\plugins' />"
	puts $outfp "<Registry Id='regid11' Root='HKLM' Key='Software\\SimVascular\\$SV_VERSION\\$SV_TIMESTAMP' Name='QT_PLUGIN_PATH' Action='write' Type='string' Value='\[INSTALLDIR\]$SV_TIMESTAMP\\qt-plugins' />"

    }
    foreach i $files {
      global outfp
      if {![file isdirectory $i]} {
        global idno
	  set id [incr idno]
          global SV_VERSION
          global SV_EXECUTABLE
          if {[file tail $i] == $SV_EXECUTABLE} {
            global curdirID
	    puts $outfp "<File Id='id[format %04i $id]' Name='[file tail $i]' Source='$i' DiskId='1'>"
            puts $outfp "<Shortcut Id='ids12' Directory='ProgramMenuDir' Name='$SV_VERSION' Arguments='Tcl/SimVascular_2.0/simvascular_startup.tcl --ignore-pro' WorkingDirectory='$curdirID' Icon='idico' IconIndex='0' />"
	    puts $outfp "<Shortcut Id='ids13' Directory='DesktopFolder' Name='$SV_VERSION' Arguments='Tcl/SimVascular_2.0/simvascular_startup.tcl --ignore-pro' WorkingDirectory='$curdirID' Icon='idico' IconIndex='0' />"
            puts $outfp "</File>"
            #puts $outfp "<RemoveFolder Directory='ProgramMenuDir' Name='$SV_VERSION' On='uninstall' />"
            puts $outfp "<RemoveFile Id='ids12' On='uninstall' Name='*.*' />"
            puts $outfp "<RemoveFile Id='ids13' On='uninstall' Name='*.*' />"
            puts $outfp "<RegistryValue Root=\"HKCU\" Key=\"Software\Microsoft\$SV_VERSION\" Name=\"installed\" Type=\"integer\" Value=\"1\" KeyPath=\"yes\"/>"

            puts $outfp "<RemoveFolder Id='RemoveProgramMenuDir' Directory='ProgramMenuDir' On='uninstall' />"
	  } else {
	  puts $outfp "<File Id='id[format %04i $id]' Name='[file tail $i]' Source='$i' DiskId='1' />"
	  }
      }
      lappend rtnme $i
    }
    set id [incr idno]
    puts $outfp "<RemoveFile Id='id[format %04i $id]' On='uninstall' Name='*.*' />"
    set id [incr idno]
    puts $outfp "<RemoveFolder Id='id[format %04i $id]' On='uninstall' />"
    puts $outfp "</Component>"

    set files [glob -nocomplain [file join $j *]]
    foreach i $files {
      if {[file isdirectory $i] == 1} {
	if {[file tail $i] != ".svn"} {
          global outfp
          global idno
	  set id [incr idno]
          global curdirID
          set curdirID "id[format %04i $id]"
	  puts $outfp "<Directory Id='id[format %04i $id]' Name='[file tail $i]'>"

          file_find $i $wildcard 1
          puts $outfp "</Directory>"
	}
      }
    }
  }
  return [lsort -unique $rtnme]

}

#global component_ids
set component_ids {}

set idno 1000

#set outfp [open $SV_VERSION.wxs w]
set outfp [open tmp/sv.wxs w]

puts $outfp "<?xml version='1.0' encoding='windows-1252'?>"
puts $outfp "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'"
puts $outfp "     xmlns:util=\"http://schemas.microsoft.com/wix/UtilExtension\">"

set product_id [exec tmp/uuidgen.exe 1]
set package_id [exec tmp/uuidgen.exe 1]
set upgrade_id [exec tmp/uuidgen.exe 1]

puts $outfp "<Product Name='$SV_VERSION' Id='$product_id' UpgradeCode='$upgrade_id' Language='1033' Codepage='1252' Version='$SV_FULL_VER_NO' Manufacturer='SimVascular'>"
puts $outfp "<Package Id='$package_id' Keywords='Installer' Description='$SV_VERSION Installer' Comments='SimVascular $SV_PLATFORM version' Manufacturer='SimVascular' InstallerVersion='100' Languages='1033' Compressed='yes' Platform='x64' SummaryCodepage='1252' />"

puts $outfp "<WixVariable Id=\"WixUILicenseRtf\" Value=\"License.rtf\" />"
puts $outfp "<WixVariable Id=\"WixUIBannerBmp\" Value=\"windows_msi_helpers/msi-banner.bmp\" />"
puts $outfp "<WixVariable Id=\"WixUIDialogBmp\" Value=\"windows_msi_helpers/msi-dialog.bmp\" />"

puts $outfp "<Media Id='1' Cabinet='Sample.cab' EmbedCab='yes' />"
puts $outfp "<Property Id='INSTALLLEVEL' Value='999' />"
puts $outfp "<Property Id='ALLUSERS' Value='1' />"

puts $outfp "<Directory Id='TARGETDIR' Name='SourceDir'>"
puts $outfp "\t<Directory Id='ProgramFiles64Folder' Name='PFiles'>"
puts $outfp "\t\t<Directory Id='id19' Name='SimVascular'>"
puts $outfp "\t\t\t<Directory Id='INSTALLDIR' Name='$SV_VERSION'>"

#puts $outfp "<Component Id='ain_id23' Guid='A7FFADE1-74BB-4CC8-8052-06B214B93701'>"

file_find $pwd/$SV_FILES/ *
# need to do for each nested directory!
#puts $outfp <Directory Id='id22' Name='bin'>
#<Component Id='ain_id23' Guid='A7FFADE1-74BB-4CC8-8052-06B214B93701'>
#<File Id='id24' Name='tcl85t.dll' Source='C:\cygwin\home\nwilson\svnwork\x64\simvascular\branches\gui_reorg\Release\package\simvascular64\1205022177\bin\tcl85t.dll' DiskId='1' />
#<RemoveFile Id='id29' On='uninstall' Name='*.*' />
#<RemoveFolder Id='id22' On='uninstall' />
#</Component>
#</Directory>

#<Component Id='ain_id30' Guid='E346C1C4-17D5-4EB8-8A6B-251EBA7E687B'>
#<Registry Id='id31' Root='HKLM' Key='Software\SimVascular\simvascular64 $SV_RELEASE_VERSION_NO' Name='InstallDir' Action='write' Type='string' Value='[INSTALLDIR]' />
#<File Id='id32' Name='wish85t.exe' Source='C:\cygwin\home\nwilson\svnwork\x64\simvascular\branches\gui_reorg\Release\package\simvascular64\1205022177\wish85t.exe' DiskId='1' />
#<RemoveFile Id='id33' On='uninstall' Name='*.*' />
#<RemoveFolder Id='ProgramMenuDir' On='uninstall' />
#</Component>

#puts $outfp "</Component>"
puts $outfp "\t\t\t</Directory>"
puts $outfp "\t\t</Directory>"
puts $outfp "\t</Directory>"

puts $outfp "<Directory Id='ProgramMenuFolder' Name='Programs'>"
puts $outfp "\t<Directory Id='ProgramMenuDir' Name='$SV_VERSION' />"
puts $outfp "</Directory>"
puts $outfp "<Directory Id='DesktopFolder' Name='Desktop' />"
puts $outfp "</Directory>"
puts $outfp "<Feature Id='Complete' Title='$SV_VERSION $SV_RELEASE_VERSION_NO' Description='The complete package.' Display='expand' Level='1' ConfigurableDirectory='INSTALLDIR'>"
puts $outfp "\t<Feature Id='Main' Title='Main' Description='Required Files' Display='expand' Level='1'>"

# need components to match directories above!
foreach i $component_ids {
  puts $outfp "\t\t<ComponentRef Id='id$i' />"
}
#puts $outfp "\t\t<ComponentRef Id='ain_id23' />"
#puts $outfp "\t\t<ComponentRef Id='ain_id30' />"

puts $outfp "\t</Feature>"
puts $outfp "</Feature>"

puts $outfp "<Property Id='WIXUI_INSTALLDIR' Value='INSTALLDIR' />		<UIRef Id='WixUI_InstallDir' />"
puts $outfp "<Icon Id='idico' SourceFile='$releasedir\\windows_msi_helpers\\simvascular.ico' />"
puts $outfp "</Product>"
puts $outfp "</Wix>"

close $outfp
