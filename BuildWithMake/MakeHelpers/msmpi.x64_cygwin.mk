ifeq ($(CLUSTER), x64_cygwin)
    MPI_NAME    = msmpi
    #MPI_TOP    = C:/Program\ Files\ \(x86\)/Microsoft\ SDKs/MPI
    #MPI_INCDIR = -I$(MPI_TOP)/Include -I$(MPI_TOP)/Include/x64

    MPI_TOP    = $(TOP)/../Code/ThirdParty/msmpi
    MPI_INCDIR = -I$(MPI_TOP)/Include -I$(MPI_TOP)/Include/x64
    MPI_LIBS   = $(LIBPATH_COMPILER_FLAG)$(MPI_TOP)/Lib/x64 $(LIBFLAG)msmpifmc$(LIBLINKEXT) $(LIBFLAG)msmpi$(LIBLINKEXT)

    MPI_SO_PATH   = 
    MPIEXEC_PATH  = 
    MPIEXEC       =
endif
