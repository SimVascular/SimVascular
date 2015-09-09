ifeq ($(CLUSTER), x64_cygwin)
    MPI_NAME    = mpich
    MPI_TOP     = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/../../unknown/x64/mpich2-1.4p1
    MPI_INCDIR  = -I $(MPI_TOP)/include
    MPI_LIBS    = $(LIBPATH_COMPILER_FLAG)$(MPI_TOP)/lib $(LIBFLAG)mpi.lib $(LIBFLAG)fmpich2.lib
    MPI_SO_PATH = $(MPI_TOP)/lib
    MPIEXEC_PATH  = $(MPI_TOP)/bin
    MPIEXEC       = mpiexec.exe
endif
