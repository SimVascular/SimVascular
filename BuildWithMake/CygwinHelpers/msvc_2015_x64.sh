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

#
#  Microsoft Visual Studio 20105 SP3 x64 (installed with 64-bit Windows 10)
#

parse_reg_dir()
{
reg query "$1" /v $2 | sed ':a;N;$!ba;s/\n//g;s/\r//g;s+\\+/+g' | awk '{gsub(/^[ \t]+/,"",$0);gsub(/^[ \t]+$/,"",$0);print}' | awk '{$1="";$2="";$3="";print}' | awk '{gsub(/^[ \t]+/,"",$0);gsub(/^[ \t]+$/,"",$0);print}'
}

parse_reg_dir_with_extra_space()
{
reg query "$1" /v $2 | sed ':a;N;$!ba;s/\n//g;s/\r//g;s+\\+/+g' | awk '{gsub(/^[ \t]+/,"",$0);gsub(/^[ \t]+$/,"",$0);print}' | awk '{$1="";$2="";$3="";$4="";print}' | awk '{gsub(/^[ \t]+/,"",$0);gsub(/^[ \t]+$/,"",$0);print}'
}

export VisualStudioVersion=14.0
export Framework40Version=v4.0

export NETFXSDKDir="C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.6.1\\"
export WindowsSDKVersion="10.0.14393.0\\"
export WindowsSDKLibVersion="10.0.14393.0\\"

# SDK vars

export WindowsSdkDir=`parse_reg_dir_with_extra_space "HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\Microsoft SDKs\\\\Windows\\\\v10.0" InstallationFolder`
export WindowsSDK_ExecutablePath_x86=`parse_reg_dir_with_extra_space "HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\Microsoft SDKs\\\\NETFXSDK\\\\4.6.1\\\\WinSDK-NetFx40Tools-x86" InstallationFolder`
export WindowsSDK_ExecutablePath_x64=`parse_reg_dir_with_extra_space "HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\Microsoft SDKs\\\\NETFXSDK\\\\4.6.1\\\\WinSDK-NetFx40Tools-x64" InstallationFolder`

export VS140COMNTOOLS=`parse_reg_dir HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\VisualStudio\\\\SxS\\\\VS7 $VisualStudioVersion`/Common7/Tools

export VSINSTALLDIR=`parse_reg_dir HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\VisualStudio\\\\SxS\\\\VS7 $VisualStudioVersion`
export VCINSTALLDIR=`parse_reg_dir HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\VisualStudio\\\\SxS\\\\VC7 $VisualStudioVersion`

FrameworkDIR64=`parse_reg_dir HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\VisualStudio\\\\SxS\\\\VC7 FrameworkDir64`
FrameworkVersion64=`parse_reg_dir HKLM\\\\SOFTWARE\\\\Wow6432Node\\\\Microsoft\\\\VisualStudio\\\\SxS\\\\VC7 FrameworkVer64`

export FrameworkDir=$FrameworkDir64
export FrameworkVersion=$FrameworkVersion64

export UniversalCRTSdkDir=$WindowsSdkDir
export UCRTVersion="10.0.14393.0"

export ExtensionSdkDir="$WindowsSdkDir\\ExtensionSDKs"
#export WindowsLibPath="$ExtensionSdkDir/References/CommonConfiguration/Neutral"

# check below here

export ProgramFiles_x86="C:/Program Files (x86)"

# is ExtensionSDKDir defined???

if [ -d "${WindowsSDK_ExecutablePath_x64}" ]; then
  PATH=`cygpath "${WindowsSDK_ExecutablePath_x64}"`:$PATH
fi

if [ -d "${WindowsSdkDir}" ]; then
  PATH=`cygpath "${WindowsSdkDir}bin/x64;${WindowsSdkDir}bin/x86"`:$PATH
  INCLUDE=${WindowsSdkDir}include/${WindowsSDKVersion}shared\;${WindowsSdkDir}include/${WindowsSDKVersion}um\;${WindowsSdkDir}include/${WindowsSDKVersion}winrt\;$INCLUDE
  LIB=${WindowsSdkDir}lib/${WindowsSDKLibVersion}um/x64\;$LIB
  LIBPATH=${WindowsLibPath}\;${ExtensionSDKDir}/Microsoft.VCLibs/14.0/References/CommonConfiguration/neutral\;$LIBPATH
fi

if [ -d "${NETFXSDKDir}" ]; then
   INCLUDE=${NETFXSDKDir}include/um\;$INCLUDE
   LIB=${NETFXSDKDir}lib/um/x64\;$LIB
fi

if [ "${UCRTVersion}" != "" ]; then
   INCLUDE=${UniversalCRTSdkDir}include/${UCRTVersion}/ucrt\;$INCLUDE
   LIB=${UniversalCRTSdkDir}lib/${UCRTVersion}/ucrt/x64\;$LIB
fi

if [ -d "${VSINSTALLDIR}Team Tools/Performance Tools/x64" ]; then
  PATH=`cygpath "${VSINSTALLDIR}Team Tools/Performance Tools/x64"`:$PATH
  PATH=`cygpath "${VSINSTALLDIR}Team Tools/Performance Tools"`:$PATH
fi

if [ -d "${ProgramFiles}/HTML Help Workshop" ]; then
  PATH=`cygpath "${ProgramFiles}/HTML Help Workshop"`:$PATH
fi

if [ -d "${ProgramFiles_x86}/HTML Help Workshop" ]; then
  PATH=`cygpath "${ProgramFiles_x86}/HTML Help Workshop"`:$PATH
fi

if [ -d "${VSINSTALLDIR}Common7/Tools" ]; then
  PATH=`cygpath "${VSINSTALLDIR}Common7/Tools"`:$PATH
fi

if [ -d "${VSINSTALLDIR}Common7/IDE" ]; then
  PATH=`cygpath "${VSINSTALLDIR}Common7/IDE"`:$PATH
fi

if [ -d "${VCINSTALLDIR}VCPackages" ]; then
  PATH=`cygpath "${VCINSTALLDIR}VCPackages"`:$PATH
fi

if [ -d "${FrameworkDir}/${Framework40Version}" ]; then
  PATH=`cygpath "${FrameworkDir}/${Framework40Version}"`:$PATH
fi

if [ -d "${FrameworkDir}/${FrameworkVersion}" ]; then
  PATH=`cygpath "${FrameworkDir}/${FrameworkVersion}"`:$PATH
fi

if [ -d "${VCINSTALLDIR}BIN/amd64" ]; then
  PATH=`cygpath "${VCINSTALLDIR}BIN/amd64"`:$PATH
fi

if [ -d "${ProgramFiles}/MSBuild/14.0/bin/amd64" ]; then
  PATH=`cygpath "${ProgramFiles}/MSBuild/14.0/bin/amd64"`:$PATH
fi

if [ -d "${ProgramFiles_x86}/MSBuild/14.0/bin/amd64" ]; then
  PATH=`cygpath "${ProgramFiles_x86}/MSBuild/14.0/bin/amd64"`:$PATH
fi

if [ -d "${VSINSTALLDIR}Common7/IDE/CommonExtensions/Microsoft/TestWindow" ]; then
  PATH=`cygpath "${VSINSTALLDIR}Common7/IDE/CommonExtensions/Microsoft/TestWindow"`:$PATH
fi

if [ -d "${VCINSTALLDIR}ATLMFC/INCLUDE" ]; then
  INCLUDE=${VCINSTALLDIR}ATLMFC/INCLUDE\;$INCLUDE
fi

if [ -d "${VCINSTALLDIR}INCLUDE" ]; then
  INCLUDE=${VCINSTALLDIR}INCLUDE\;$INCLUDE
fi

if [ -d "${VCINSTALLDIR}ATLMFC/LIB/amd64" ]; then
  LIB=${VCINSTALLDIR}ATLMFC/LIB/amd64\;$LIB
fi

if [ -d "${VCINSTALLDIR}LIB/amd64" ]; then
  LIB=${VCINSTALLDIR}LIB/amd64\;$LIB
fi

if [ -d "${VCINSTALLDIR}ATLMFC/LIB/amd64" ]; then
  LIBPATH=${VCINSTALLDIR}ATLMFC/LIB/amd64\;$LIBPATH
fi

if [ -d "${VCINSTALLDIR}LIB/amd64" ]; then
  LIBPATH=${VCINSTALLDIR}LIB/amd64\;$LIBPATH
fi

if [ -d "${FrameworkDir}/${Framework40Version}" ]; then
  LIBPATH=${FrameworkDir}/${Framework40Version}\;$LIBPATH
fi

if [ -d "${FrameworkDir}/${FrameworkVersion}" ]; then
  LIBPATH=${FrameworkDir}/${FrameworkVersion}\;$LIBPATH
fi

export Platform=X64
export CommandPromptType=Native

export INCLUDE
export LIB
export PATH
export LIBPATH
