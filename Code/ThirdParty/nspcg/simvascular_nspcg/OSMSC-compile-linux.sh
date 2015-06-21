# Copyright (c) 2012 Open Source Medical Software Corporation.  All Rights Reserved.

# default to 64-bit compilers

  USE_LINUX_COMPILERS=intel_13.0 
  SV_EXTERN=/sv_extern

  export  CC=icc
  export CXX=icpc

  OSMSC_NSPCG_DIR=${SV_EXTERN}/bin/linux/${USE_LINUX_COMPILERS}/x64/nspcg
  echo "build 64-bit libraries (${USE_LINUX_COMPILERS})"
  mkdir -p ${OSMSC_NSPCG_DIR}
  chmod a+rx ${OSMSC_NSPCG_DIR}

  rm -f *.o *.a
  ifort -O2 -c nspcg*.f
  ar rv ${OSMSC_NSPCG_DIR}/libnspcg.a *.o


