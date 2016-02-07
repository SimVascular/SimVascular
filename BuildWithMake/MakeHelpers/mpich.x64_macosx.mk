MPI_NAME    = mpich
MPI_INCDIR  = $(wordlist 2,99,$(shell mpif90 -link_info))
MPI_LIBS    = $(wordlist 2,99,$(shell mpicxx -link_info)) $(wordlist 2,99,$(shell mpif90 -link_info))
MPI_SO_PATH = 
MPIEXEC_PATH  = 
MPIEXEC       = mpiexec

