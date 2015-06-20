set(FSRCS mpi_stubs.f)
set(CSRCS mpi_stubs_c.c)

ADD_LIBRARY(lib_fortran_simvascular_dummympi ${FSRCS})
ADD_LIBRARY(lib_extra_simvascular_dummympi ${CSRCS})