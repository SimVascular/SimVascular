ifeq ($(CLUSTER), x64_linux)
    MPI_TOP     =  /usr/lib/mpich
    MPI_INCDIR  = -I $(MPI_TOP)/include -L $(MPI_TOP)/lib
    MPI_LIBS    = -lmpichf90 -lmpich -lopa -lmpl -lrt -lpthread
    MPI_SO_PATH = $(MPI_TOP)/lib
    MPIEXEC_PATH  = /usr/bin
    MPIEXEC       = mpiexec
endif
