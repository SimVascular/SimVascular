MPI_NAME    = mpich
MPI_INCDIR  = -I $(MPI_TOP)/include -L $(MPI_TOP)/lib
MPI_LIBS    = $(shell mpif90 -link_info | awk '{print substr($0, index($0,$2))}')
MPI_SO_PATH = $(shell which mpiexec)/..
MPIEXEC_PATH  = $(dir $(shell which mpiexec))
MPIEXEC       = mpiexec

