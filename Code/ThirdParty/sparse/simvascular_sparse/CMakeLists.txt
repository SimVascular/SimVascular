cmake_minimum_required(VERSION 2.8.8)

Project(SPARSE)
if (NOT SPARSE_LIBRARY_NAME)
    set(SPARSE_LIBRARY_NAME sparse)
endif()

set(HDRS spConfig.h spDefs.h spMatrix.h)
set(CSRCS spAllocate.c spBuild.c spFactor.c spOutput.c spSolve.c spUtils.c spFortran.c)

add_library(${SPARSE_LIBRARY_NAME} STATIC ${CSRCS})