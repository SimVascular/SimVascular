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
#set SIMVASCULAR_TIMESTAMP [file tail [glob [lindex $argv 2]/*]]
set SIMVASCULAR_RELEASE_VERSION_NO [lindex $argv 3]
set MESHSIM_LICENSE_FILE [lindex $argv 4]

puts "building wxs for $argv"

set component_ids {}

set idno 1000

set outfp [open tmp/simvascular-licenses.wxs w]

puts $outfp "<?xml version='1.0' encoding='windows-1252'?>"
puts $outfp "<Wix xmlns='http://schemas.microsoft.com/wix/2003/01/wi'>"

puts $outfp "<Product Name='SimVascular Licenses' Id='39068F8A-D4E9-405F-B22B-9B417849AEE6' UpgradeCode='2359EF69-C03F-401C-9BC3-C19B10297E3B' Language='1033' Codepage='1252' Version='$SIMVASCULAR_RELEASE_VERSION_NO' Manufacturer='SimVascular'>"
puts $outfp "<Package Id='585A82F4-077B-4857-9E0A-B0C87A49DE83' Keywords='Installer' Description='SimVascular Licenses Installer' Comments='SimVascular Licenses' Manufacturer='SimVascular' InstallerVersion='100' Languages='1033' Compressed='yes' SummaryCodepage='1252' />"

puts $outfp "<Media Id='1' Cabinet='Sample.cab' EmbedCab='yes' />"
puts $outfp "<Property Id='INSTALLLEVEL' Value='999' />"
puts $outfp "<Property Id='ALLUSERS' Value='1' />" 

puts $outfp "<Directory Id='TARGETDIR' Name='SourceDir'>"
puts $outfp "\t<Directory Id='ProgramFilesFolder' Name='PFiles'>"
puts $outfp "\t\t<Directory Id='id19' Name='id20' LongName='SimVascular'>"
puts $outfp "\t\t\t<Directory Id='INSTALLDIR' Name='Licenses'>"

#puts $outfp "<Component Id='ain_id23' Guid='A7FFADE1-74BB-4CC8-8052-06B214B93701'>"

set id [incr idno]
lappend component_ids $id
set guid [exec tmp/uuidgen.exe 1]
puts $outfp "<Component Id='id[format %04i $id]' Guid='$guid'>"

puts $outfp "<Registry Id='regid10' Root='HKLM' Key='Software\\SimVascular\\Licenses' Name='SimVascular' Action='write' Type='string' Value='B6BBB851-04C9-4341-B4B1-1A594924D4AA' />"

set regid 11
set licfp [open $MESHSIM_LICENSE_FILE r]
while {[gets $licfp line] >= 0} {
  if {$line == ""} continue
  set line [string trim $line]
  if {[string index $line 0] == "\#"} continue
  set keyname [lindex $line 1]
  set keyname [string toupper $keyname]
puts $outfp "<Registry Id='regid$regid' Root='HKLM' Key='Software\\SimVascular\\Licenses' Name='MESHSIM_KEY_$keyname' Action='write' Type='string' Value='$line' />"
  incr regid
}
close $licfp
puts $outfp "</Component>"
puts $outfp "\t\t\t</Directory>"
puts $outfp "\t\t</Directory>"
puts $outfp "\t</Directory>"

puts $outfp "<Directory Id='ProgramMenuFolder' Name='PMenu' LongName='Programs'>"
puts $outfp "\t<Directory Id='ProgramMenuDir' Name='Licenses' />"
puts $outfp "</Directory>"
puts $outfp "<Directory Id='DesktopFolder' Name='Desktop' />"
puts $outfp "</Directory>"
puts $outfp "<Feature Id='Complete' Title='$SIMVASCULAR_VERSION Licenses' Description='The complete package.' Display='expand' Level='1' ConfigurableDirectory='INSTALLDIR'>"
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
#puts $outfp "<Icon Id='idico' SourceFile='[pwd]\\windows_msi_helpers\\simvascular.ico' />"
puts $outfp "</Product>"
puts $outfp "</Wix>"

close $outfp
