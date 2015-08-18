#
# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical 
# Software Corporation, University of California, San Diego.
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

set SIMVASCULAR_VERSION [lindex $argv 0]
set SIMVASCULAR_PLATFORM [lindex $argv 1]
set SIMVASCULAR_TIMESTAMP [file tail [glob [lindex $argv 2]/*]]
set SIMVASCULAR_RELEASE_VERSION_NO [lindex $argv 3]
set SIMVASCULAR_SHORT_NAME [lindex $argv 4]
set SIMVASCULAR_EXECUTABLE [lindex $argv 5]
set SIMVASCULAR_FILES [lindex $argv 6]
set SIMVASCULAR_FULL_VER_NO [lindex $argv 7]
puts "SIMVASCULAR_FILES $SIMVASCULAR_FILES"

global pwd
if {$tcl_platform(platform) == "windows"} {
  set pwd [pwd]
} else {
  #assume cygwin
  set pwd [exec cygpath -m [pwd]]
}

puts "building wxs for $argv"

set outputRegistry 0

proc file_find {dir wildcard args} {

  global pwd
  global SIMVASCULAR_SHORT_NAME

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
        global SIMVASCULAR_TIMESTAMP
        global SIMVASCULAR_RELEASE_VERSION_NO
        global SIMVASCULAR_VERSION
        global SIMVASCULAR_SHORT_NAME
        global SIMVASCULAR_EXECUTABLE
        global SIMVASCULAR_FILES
        set outputRegistry 1
	puts $outfp "<Registry Id='regid1' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='InstallDir' Action='write' Type='string' Value='\[INSTALLDIR\]' />"
	puts $outfp "<Registry Id='regid2' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='TimeStamp' Action='write' Type='string' Value='$SIMVASCULAR_TIMESTAMP' />"
	puts $outfp "<Registry Id='regid3' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='RunDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SIMVASCULAR_TIMESTAMP' />"
	puts $outfp "<Registry Id='regid4' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='TclLibDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SIMVASCULAR_TIMESTAMP\\lib\\tcl8.5' />"
	puts $outfp "<Registry Id='regid5' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='TkLibDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SIMVASCULAR_TIMESTAMP\\lib\\tk8.5' />"
	puts $outfp "<Registry Id='regid6' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='PSchemaDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SIMVASCULAR_TIMESTAMP\\schema' />"
	puts $outfp "<Registry Id='regid7' Root='HKLM' Key='Software\\SimVascular\\$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Name='HomeDir' Action='write' Type='string' Value='\[INSTALLDIR\]$SIMVASCULAR_TIMESTAMP' />"
    }
    foreach i $files {
      global outfp
      if {![file isdirectory $i]} {
        global idno
	  set id [incr idno]
          global SIMVASCULAR_VERSION
          global SIMVASCULAR_EXECUTABLE
          if {[file tail $i] == $SIMVASCULAR_EXECUTABLE} {
            global curdirID
	    puts $outfp "<File Id='id[format %04i $id]' Name='[file tail $i]' Source='$pwd/$i' DiskId='1'>"
            puts $outfp "<Shortcut Id='ids12' Directory='ProgramMenuDir' Name='$SIMVASCULAR_VERSION' Arguments='Tcl/SimVascular_2.0/simvascular_startup.tcl' WorkingDirectory='$curdirID' Icon='idico' IconIndex='0' />"
	    puts $outfp "<Shortcut Id='ids13' Directory='DesktopFolder' Name='$SIMVASCULAR_VERSION' Arguments='Tcl/SimVascular_2.0/simvascular_startup.tcl' WorkingDirectory='$curdirID' Icon='idico' IconIndex='0' />"
            puts $outfp "</File>"
            #puts $outfp "<RemoveFolder Directory='ProgramMenuDir' Name='$SIMVASCULAR_VERSION' On='uninstall' />"
            puts $outfp "<RemoveFile Id='ids12' On='uninstall' Name='*.*' />"
            puts $outfp "<RemoveFile Id='ids13' On='uninstall' Name='*.*' />"
            puts $outfp "<RegistryValue Root=\"HKCU\" Key=\"Software\Microsoft\$SIMVASCULAR_VERSION\" Name=\"installed\" Type=\"integer\" Value=\"1\" KeyPath=\"yes\"/>"

            puts $outfp "<RemoveFolder Id='RemoveProgramMenuDir' Directory='ProgramMenuDir' On='uninstall' />"
	  } else {
	  puts $outfp "<File Id='id[format %04i $id]' Name='[file tail $i]' Source='$pwd/$i' DiskId='1' />"
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

#set outfp [open $SIMVASCULAR_VERSION.wxs w]
set outfp [open tmp/sv.wxs w]

puts $outfp "<?xml version='1.0' encoding='windows-1252'?>"
puts $outfp "<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'"
puts $outfp "     xmlns:util=\"http://schemas.microsoft.com/wix/UtilExtension\">"

if {$SIMVASCULAR_VERSION == "simvascular"} {
  puts $outfp "<Product Name='$SIMVASCULAR_VERSION' Id='986308C0-4613-427A-8971-B976F6A4A823' UpgradeCode='F55C7E60-DBD1-4D25-BDC0-DCFFF5651920' Language='1033' Codepage='1252' Version='$SIMVASCULAR_FULL_VER_NO' Manufacturer='SimVascular'>"
  puts $outfp "<Package Id='104C5565-1E4D-4C92-852B-FAC30A602DC6' Keywords='Installer' Description='$SIMVASCULAR_VERSION Installer' Comments='SimVascular $SIMVASCULAR_PLATFORM version' Manufacturer='SimVascular' InstallerVersion='100' Languages='1033' Compressed='yes' SummaryCodepage='1252' />"
} elseif {$SIMVASCULAR_VERSION == "beta"} {
  puts $outfp "<Product Name='$SIMVASCULAR_VERSION' Id='BDC232D8-EBE3-4730-BEC1-01D4991C1EE4' UpgradeCode='DCD272C9-71BC-4917-8EE9-AAE95D2B8578' Language='1033' Codepage='1252' Version='$SIMVASCULAR_FULL_VER_NO' Manufacturer='SimVascular'>"
  puts $outfp "<Package Id='06558B20-CCE8-4FDA-BB51-A5073901CA68' Keywords='Installer' Description='$SIMVASCULAR_VERSION Installer' Comments='SimVascular $SIMVASCULAR_PLATFORM version' Manufacturer='SimVascular' InstallerVersion='100' Languages='1033' Compressed='yes' SummaryCodepage='1252' />"
}

puts $outfp "<WixVariable Id=\"WixUILicenseRtf\" Value=\"License.rtf\" />"
puts $outfp "<WixVariable Id=\"WixUIBannerBmp\" Value=\"windows_msi_helpers/msi-banner.bmp\" />"
puts $outfp "<WixVariable Id=\"WixUIDialogBmp\" Value=\"windows_msi_helpers/msi-dialog.bmp\" />"

puts $outfp "<Media Id='1' Cabinet='Sample.cab' EmbedCab='yes' />"
puts $outfp "<Property Id='INSTALLLEVEL' Value='999' />"
puts $outfp "<Property Id='ALLUSERS' Value='1' />" 

puts $outfp "<Directory Id='TARGETDIR' Name='SourceDir'>"
puts $outfp "\t<Directory Id='ProgramFilesFolder' Name='PFiles'>"
puts $outfp "\t\t<Directory Id='id19' Name='SimVascular'>"
puts $outfp "\t\t\t<Directory Id='INSTALLDIR' Name='$SIMVASCULAR_SHORT_NAME'>"

#puts $outfp "<Component Id='ain_id23' Guid='A7FFADE1-74BB-4CC8-8052-06B214B93701'>"

file_find package/$SIMVASCULAR_FILES/ *
# need to do for each nested directory!
#puts $outfp <Directory Id='id22' Name='bin'>
#<Component Id='ain_id23' Guid='A7FFADE1-74BB-4CC8-8052-06B214B93701'>
#<File Id='id24' Name='tcl85t.dll' Source='C:\cygwin\home\nwilson\svnwork\x64\simvascular\branches\gui_reorg\Release\package\simvascular64\1205022177\bin\tcl85t.dll' DiskId='1' />
#<RemoveFile Id='id29' On='uninstall' Name='*.*' />
#<RemoveFolder Id='id22' On='uninstall' />
#</Component>
#</Directory>

#<Component Id='ain_id30' Guid='E346C1C4-17D5-4EB8-8A6B-251EBA7E687B'>
#<Registry Id='id31' Root='HKLM' Key='Software\SimVascular\simvascular64 $SIMVASCULAR_RELEASE_VERSION_NO' Name='InstallDir' Action='write' Type='string' Value='[INSTALLDIR]' />
#<File Id='id32' Name='wish85t.exe' Source='C:\cygwin\home\nwilson\svnwork\x64\simvascular\branches\gui_reorg\Release\package\simvascular64\1205022177\wish85t.exe' DiskId='1' />
#<RemoveFile Id='id33' On='uninstall' Name='*.*' />
#<RemoveFolder Id='ProgramMenuDir' On='uninstall' />
#</Component>

#puts $outfp "</Component>"
puts $outfp "\t\t\t</Directory>"
puts $outfp "\t\t</Directory>"
puts $outfp "\t</Directory>"

puts $outfp "<Directory Id='ProgramMenuFolder' Name='Programs'>"
puts $outfp "\t<Directory Id='ProgramMenuDir' Name='$SIMVASCULAR_VERSION' />"
puts $outfp "</Directory>"
puts $outfp "<Directory Id='DesktopFolder' Name='Desktop' />"
puts $outfp "</Directory>"
puts $outfp "<Feature Id='Complete' Title='$SIMVASCULAR_VERSION $SIMVASCULAR_RELEASE_VERSION_NO' Description='The complete package.' Display='expand' Level='1' ConfigurableDirectory='INSTALLDIR'>"
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
puts $outfp "<Icon Id='idico' SourceFile='$pwd\\windows_msi_helpers\\simvascular.ico' />"
puts $outfp "</Product>"
puts $outfp "</Wix>"

close $outfp
