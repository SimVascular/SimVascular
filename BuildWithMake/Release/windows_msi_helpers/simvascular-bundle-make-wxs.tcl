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
set SV_FULL_VER_NO [lindex $argv 1]
set SV_TIMESTAMP [lindex $argv 2]
set out_fn [lindex $argv 3]

set outfp [open $out_fn w]
fconfigure $outfp -translation lf

#set product_id [exec tmp/uuidgen.exe 1]
#set package_id [exec tmp/uuidgen.exe 1]
set upgrade_id [exec tmp/uuidgen.exe 1]

puts $outfp "<Wix xmlns=\"http://schemas.microsoft.com/wix/2006/wi\""
puts $outfp "     xmlns:bal=\"http://schemas.microsoft.com/wix/BalExtension\""
puts $outfp "     xmlns:util=\"http://schemas.microsoft.com/wix/UtilExtension\">"
puts $outfp ""
puts $outfp "  <Bundle Name=\"SimVascular Windows Installer (64-bit, svSolver not included)\" Version=\"$SV_FULL_VER_NO\" Manufacturer=\"simvascular.org\" UpgradeCode=\"$upgrade_id\" IconSourceFile=\"windows_msi_helpers/simvascular.ico\" AboutUrl=\"http://www.simvascular.org\">"
puts $outfp ""
puts $outfp "     <BootstrapperApplicationRef Id=\"WixStandardBootstrapperApplication.RtfLicense\">"
puts $outfp "        <bal:WixStandardBootstrapperApplication LicenseFile=\"License.rtf\" LogoFile=\"windows_installer_packages/msi-logo.png\" />"
puts $outfp "     </BootstrapperApplicationRef>"
puts $outfp ""
puts $outfp "      <util:RegistrySearch Root=\"HKLM\" Key=\"SOFTWARE\\Microsoft\\DevDiv\\vc\\Servicing\\14.0\\RuntimeMinimum\" Value=\"Version\" Variable=\"MSVC14_EXISTS\" Result=\"exists\" Win64=\"yes\"/>"
puts $outfp "      <util:RegistrySearch Root=\"HKLM\" Key=\"SOFTWARE\\Microsoft\\DevDiv\\vc\\Servicing\\14.0\\RuntimeMinimum\" Value=\"Version\" Variable=\"MSVC14_VERSION\" Result=\"value\" Win64=\"yes\"/>"
puts $outfp "      <util:RegistrySearch Root=\"HKLM\" Key=\"SOFTWARE\\Microsoft\\DevDiv\\vc\\Servicing\\12.0\\RuntimeMinimum\" Value=\"Version\" Variable=\"MSVC12_EXISTS\" Result=\"exists\" Win64=\"yes\"/>"
puts $outfp "      <util:RegistrySearch Root=\"HKLM\" Key=\"SOFTWARE\\Microsoft\\DevDiv\\vc\\Servicing\\12.0\\RuntimeMinimum\" Value=\"Version\" Variable=\"MSVC12_VERSION\" Result=\"value\" Win64=\"yes\"/>"
puts $outfp "      <util:RegistrySearch Root=\"HKLM\" Key=\"HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\VisualStudio\\10.0\\VC\\VCRedist\\x64\" Value=\"Installed\" Variable=\"MSVC10_EXISTS\" Result=\"exists\" Win64=\"no\"/>"
puts $outfp ""
puts $outfp "     <Chain>"
puts $outfp ""
puts $outfp "     <ExePackage"
puts $outfp "       Id=\"Dependency1\""
puts $outfp "       DisplayName=\"MSVC 2017 C++ Runtime Libraries\""
puts $outfp "       SourceFile=\"windows_installer_packages/vcredist_x64-MSVC-2017-14.21.27702.exe\""
puts $outfp "       Vital=\"yes\""
puts $outfp "       DetectCondition=\"(MSVC14_EXISTS)\""
puts $outfp "       Permanent=\"yes\""
puts $outfp "       >"
puts $outfp "       <!-- Ignore \"Newer version installed\" error -->"
puts $outfp "       <ExitCode Value=\"1638\" Behavior=\"success\"/>"
puts $outfp "     </ExePackage>"
puts $outfp ""
puts $outfp "     <ExePackage"
puts $outfp "       Id=\"Dependency2\""
puts $outfp "       DisplayName=\"MSVC 2013 C++ Runtime Libraries\""
puts $outfp "       SourceFile=\"windows_installer_packages/vcredist_x64-MSVC-2013-update-4.exe\""
puts $outfp "       Vital=\"yes\""
puts $outfp "       DetectCondition=\"(MSVC12_EXISTS)\""
puts $outfp "       Permanent=\"yes\""
puts $outfp "       >"
puts $outfp "       <!-- Ignore \"Newer version installed\" error -->"
puts $outfp "       <ExitCode Value=\"1638\" Behavior=\"success\"/>"
puts $outfp "     </ExePackage>"
puts $outfp ""
puts $outfp "     <ExePackage"
puts $outfp "       Id=\"Dependency3\""
puts $outfp "       DisplayName=\"MSVC 2010 C++ Runtime Libraries\""
puts $outfp "       SourceFile=\"windows_installer_packages/vcredist_x64-MSVC-2010.exe\""
puts $outfp "       Vital=\"no\""
puts $outfp "       DetectCondition=\"(MSVC10_EXISTS)\""
puts $outfp "       Permanent=\"yes\""
puts $outfp "       >"
puts $outfp "       <!-- Ignore \"Newer version installed\" error -->"
puts $outfp "       <ExitCode Value=\"1638\" Behavior=\"success\"/>"
puts $outfp "     </ExePackage>"
puts $outfp ""
puts $outfp "     <MsiPackage Id=\"MainPackage\" SourceFile=\"$SV_VERSION-$SV_TIMESTAMP-Windows-64bit.msi\" Vital=\"yes\" DisplayInternalUI=\"yes\"/>"
puts $outfp ""
puts $outfp "    </Chain>"
puts $outfp ""
puts $outfp "  </Bundle>"
puts $outfp ""
puts $outfp "</Wix>"

close $outfp
