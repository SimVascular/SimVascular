# Copyright (c) 2012 Open Source Medical Software Corporation.  All Rights Reserved.

# default to 64-bit compilers

  USE_LINUX_COMPILERS=intel_13.0 
  SV_EXTERN=/sv_extern

  make

  SPARSE_DIR=${SV_EXTERN}/bin/linux/${USE_LINUX_COMPILERS}/x64/sparse-1.4
  echo "build 64-bit libraries (${USE_LINUX_COMPILERS})"
  mkdir -p ${SPARSE_DIR}
  chmod a+rx ${SPARSE_DIR}

  cp sp*.h ${SPARSE_DIR}
  cp ../lib/sparse.a ${SPARSE_DIR}/libsparse.a

  chmod -R a+rx ${SPARSE_DIR}




