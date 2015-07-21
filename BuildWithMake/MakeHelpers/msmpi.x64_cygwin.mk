ifeq ($(CLUSTER), x64_cygwin)
    #MPICH_TOP    = C:/Program\ Files\ \(x86\)/Microsoft\ SDKs/MPI
    #MPICH_INCDIR = -I$(MPICH_TOP)/Include -I$(MPICH_TOP)/Include/x64

    MPICH_TOP    = $(TOP)/../Code/ThirdParty/msmpi
    MPICH_INCDIR = -I$(MPICH_TOP)/Include -I$(MPICH_TOP)/Include/x64
    MPICH_LIBS   = $(LIBPATH_COMPILER_FLAG)$(MPICH_TOP)/Lib/x64 $(LIBFLAG)msmpifmc$(LIBLINKEXT) $(LIBFLAG)msmpi$(LIBLINKEXT)

    MPICH_SO_PATH = 
    MPIEXEC_PATH  = 
    MPIEXEC       =
endif
