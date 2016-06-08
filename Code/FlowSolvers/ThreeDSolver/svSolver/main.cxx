/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Rensselaer Polytechnic Institute, Charles A. Taylor,
 * Kenneth E. Jansen.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *=========================================================================*/

#include "cvFlowsolverOptions.h"

#include "mpi.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "cvSolverIO.h"
#include "common_c.h"

#include <iostream>

#ifdef WIN32
#include <direct.h>
#define chdir _chdir
#define getpid _getpid
#include <errno.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#ifdef SV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
#define input INPUT
#define proces PROCES
#endif

#ifdef SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
#define input input_
#define proces proces_
#endif

#ifdef WIN32
int lmuCoLite = 0;
#endif

extern "C" char cvsolver_iotype[80];
char cvsolver_iotype[80];

extern void Partition_Problem( int, char[], char[] );
extern "C" void proces();
extern "C" void input(int* size,int* myrank);
extern int input_fform(char inpfname[]);

int myrank; /* made file global for ease in debugging */

void
catchDebugger() {
    static volatile int debuggerPresent =0;
    while (!debuggerPresent ); // assign debuggerPresent=1
}

// some useful debugging functions

void
pdarray( void* darray , int start, int end ) {
    for( int i=start; i < end; i++ ){
        std::cout << ((double*)darray)[i] << std::endl;
    }
}

void
piarray( void* iarray , int start, int end ) {
    for( int i=start; i < end; i++ ){
        std::cout << ((int*)iarray)[i] << std::endl;
    }
}

extern "C" {
  FILE *simvascular_flowsolver_stdout = NULL;
  FILE *simvascular_flowsolver_stderr = NULL;
}

extern "C" int main( int argc, char *argv[] );

int main( int argc, char *argv[] ) {

    int size,ierr;
    char inpfilename[255];

    // do we need this?
    //ios::sync_with_stdio();

#if (defined WIN32)
    if(argc > 3 ){
      if( myrank == 0 ) {
        fprintf(stdout,"\n\nThe process ID for master is %d.\n\n", getpid());
        fflush(stdout);
        catchDebugger();
      }
    }
#endif
#if ( defined LAUNCH_GDB ) && !( defined WIN32 )

    if ( getenv( "catchDebugger" ) ) {

        int parent_pid = getpid();
        int gdb_child = fork();

        if( gdb_child == 0 ) {

            std::cout << "Debugger Process initiating" << std::endl;
            strstream exec_string;

#if ( defined LINUX )
            exec_string <<"idb -gui"
                        << " -pid "<< parent_pid <<" "<< argv[0] << std::endl;
#endif

            system( exec_string.str() );
            exit(0);
        }
        catchDebugger();
    }

#endif

  int rslt;

  MPI_Init(&argc,&argv);
  MPI_Comm_size (MPI_COMM_WORLD, &size);
  MPI_Comm_rank (MPI_COMM_WORLD, &myrank);

#ifdef WIN32

  char systemcmd[2048];
  systemcmd[0]='\0';

  //if (myrank == 1) {
    char* flowsolver_mount_shared_drive = NULL;
    flowsolver_mount_shared_drive = getenv("FLOWSOLVER_MOUNT_SHARED_DRIVE");

    if (flowsolver_mount_shared_drive) {
      rslt = system(flowsolver_mount_shared_drive);
      fprintf(stdout,"system call (%s)  rslt (%i)\n",flowsolver_mount_shared_drive,rslt);
      fflush(stdout);fflush(stderr);
    }
  //}

  char* flowsolver_map_shared_directory = NULL;
  flowsolver_map_shared_directory = getenv("FLOWSOLVER_MAP_SHARED_DIRECTORY");

  if(flowsolver_map_shared_directory != NULL) {
      std::cout << "Changing to shared directory \("
                << flowsolver_map_shared_directory << "\)" << std::endl;
      fflush(stdout);
      fflush(stderr);
      if(_chdir( flowsolver_map_shared_directory ) )
        {
           switch (errno)
           {
           case ENOENT:
             fprintf(stderr, "Unable to locate the directory: %s\n", flowsolver_map_shared_directory );
             break;
           case EINVAL:
             fprintf(stderr, "Invalid buffer.\n");
             break;
           default:
             fprintf(stderr, "Unknown error.\n");
           }
        }
  }

#endif

    /* Input data  */
    if(argc > 1 ){
        strcpy(inpfilename,argv[1]);
    } else {
        strcpy(inpfilename,"solver.inp");
    }

   setvbuf(stdout, NULL, _IONBF, 0);
   setvbuf(stderr, NULL, _IONBF, 0);

   //#ifdef BUILD_WITH_FLOWSOLVER_STDOUT_STDERR_REDIRECT

    char stdoutFileName[1024];
    stdoutFileName[0]='\0';
    if(argc > 2) {
      sprintf(stdoutFileName,"%s.%05i.txt",argv[2],myrank);

      simvascular_flowsolver_stdout = freopen( stdoutFileName, "a", stdout );
      // Note: freopen is deprecated; consider using freopen_s instead

      if( simvascular_flowsolver_stdout == NULL ) {
        fprintf( stdout, "error on reassigning stdout\n" );
        fflush ( stdout );
      } else {
        setvbuf(simvascular_flowsolver_stdout, NULL, _IONBF, 0);
      }

      simvascular_flowsolver_stderr = freopen( stdoutFileName, "a", stderr );
      // Note: freopen is deprecated; consider using freopen_s instead

      if( simvascular_flowsolver_stderr == NULL ) {
        fprintf( stderr, "error on reassigning stderr\n" );
        fflush ( stderr );
      } else {
        setvbuf(simvascular_flowsolver_stderr, NULL, _IONBF, 0);
      }
    }

    //#endif

    fprintf(stdout,"\nThe process ID for myrank (%i) is (%d).\n\n", myrank, getpid());
    fflush(stdout);

    if( myrank == 0 ){
    	fprintf(stdout,"\nThe number of processes is %d.\n\n", size);
    }
    fflush(stdout);

    ierr = input_fform(inpfilename);

    if(!ierr){

        /* Preprocess data and run the problem  */
        /* Partition the problem to the correct number of processors */
        if( size > 1 ) {
            if( myrank == 0 ) {
                 Partition_Problem( size, cvsolver_iotype,
                                    cvsolver_iotype);
                 MPI_Barrier(MPI_COMM_WORLD);
            } else {
                 MPI_Barrier(MPI_COMM_WORLD);
                 sprintf(inpfilename,"%d-procs_case/",size);
                 if( !chdir( inpfilename ) ) {
//                    std::cout << "Changed to the directory "
//                              << inpfilename << std::endl;
                 }else {
                    std::cerr << "Process rank=" << myrank << " could not change to the directory "
                              << inpfilename << std::endl;
                    return 1;
                 }
            }
        }else{
            /*  shouldn't alter geombc.dat.1 from solver!
            int igeombc;
            openfile_( "geombc.dat.1", "append", &igeombc );

            int iarray[10];
            int size = 2;
            int nitems = 1 ;
            iarray[0] = size;

		       writeheader_( &igeombc, "material properties ", (void*)iarray, &nitems, &size,
                          "double", cvsolver_iotype);

            double matprops[10];
            nitems = size;
            matprops[0]=matdat.datmat[0][0][0];
            matprops[1]=matdat.datmat[0][1][0];
		       writedatablock_( &igeombc, "material properties ", (void*)matprops, &nitems,
					"double", cvsolver_iotype);

		       closefile_( &igeombc, "append" );
             */
        }

        if(inpdat.solverTask==0){
            input(&size,&myrank);
            /* now we can start the solver */
            proces();
        }
    }
    else{
        printf("error during reading ascii input \n");
    }

    MPI_Finalize();

    if (simvascular_flowsolver_stdout != NULL) fclose(simvascular_flowsolver_stdout);
	// for now, redirecting stdout / stderr to one file!
    //fclose(simvascular_flowsolver_stderr);

    if (myrank == 1) {
    char* flowsolver_unmount_shared_drive = NULL;
    flowsolver_unmount_shared_drive = getenv("FLOWSOLVER_UNMOUNT_SHARED_DRIVE");

    if (flowsolver_unmount_shared_drive) {
      rslt = system(flowsolver_unmount_shared_drive);
      fprintf(stdout,"system call (%s)  rslt (%i)\n",flowsolver_unmount_shared_drive,rslt);
      fflush(stdout);fflush(stderr);
    }
    }

    fflush(stdout);
    fflush(stderr);

    return ierr;
}
