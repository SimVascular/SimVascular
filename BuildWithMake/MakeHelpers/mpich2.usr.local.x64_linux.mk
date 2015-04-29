ifeq ($(CLUSTER), x64_linux)
    MPICH_TOP     =  /usr/local
    MPICH_INCDIR  = -I$(MPICH_TOP)/include -L$(MPICH_TOP)/lib
    MPICH_LIBS    = -lmpichf90 -lmpich -lopa -lmpl -lrt -lpthread
    MPICH_SO_PATH = $(MPICH_TOP)/mpich2/lib
    MPIEXEC_PATH  = /usr/bin
    MPIEXEC       = mpiexec
endif
