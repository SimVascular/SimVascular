# Copyright (c) 2012 Open Source Medical Software Corporation.  All Rights Reserved.

# win32 helper script

# default to MSVC2010 SP1 compilers
if [ ${USE_MVSC_COMPILERS:-vs10sp1} == 'vs8' ] ; then
  export USE_MSVC_COMPILERS=vs8
else
  export USE_MSVC_COMPILERS=vs10sp1
fi

# default to 64-bit compilers

if [ ${USE_32BIT_COMPILERS:-NO} == 'YES' ] ; then
  export USE_32BIT_COMPILERS=YES
  echo "build 32-bit libraries (${USE_MSVC_COMPILERS})"
  mkdir -p /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x86/sparse-1.4
  chmod a+rx /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x86/sparse-1.4
  make -f Makefile.windows.osmsc clean
  make -f Makefile.windows.osmsc 
  cp sp*.h libsparse.lib *.pdb /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x86/sparse-1.4
  chmod -R a+rx /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x86/sparse-1.4
  make -f Makefile.windows.osmsc clean
else
  export USE_32BIT_COMPILERS=NO
  echo "build 64-bit libraries (${USE_MSVC_COMPILERS})"
  mkdir -p /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x64/sparse-1.4
  chmod a+rx /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x64/sparse-1.4
  make -f Makefile.windows.osmsc clean
  make -f Makefile.windows.osmsc 
  cp sp*.h libsparse.lib *.pdb /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x64/sparse-1.4
  chmod -R a+rx /sv_extern/bin/win/${USE_MSVC_COMPILERS}/x64/sparse-1.4
  make -f Makefile.windows.osmsc clean
fi


