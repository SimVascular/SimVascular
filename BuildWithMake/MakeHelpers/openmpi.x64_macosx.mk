MPI_NAME    = openmpi
MPI_INCDIR  = $(shell mpif90 --showme:compile)
MPI_LIBS    = $(shell mpicxx --showme:link) $(shell mpif90 --showme:link)
MPI_SO_PATH = $(shell which mpiexec)/../..
MPIEXEC_PATH  = $(dir $(shell which mpiexec))
MPIEXEC       = mpiexec
