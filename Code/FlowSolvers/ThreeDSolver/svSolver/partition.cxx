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

#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <utility>
#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>
#include "cvSolverIO.h"

#include <stdlib.h>
#include <string.h>

#ifdef WIN32 
  #include <direct.h>
  #define chdir _chdir
  #define stat _stat
  extern "C" void bzero(void* ptr, size_t sz);
#else
  #include <unistd.h>
  #include <strings.h>
#endif

#include "common_c.h"

#include "cvFlowsolverOptions.h"

char keyphrase[100];
char filename[255];
char _directory_name[256];

extern "C" {
    void
    METIS_PartGraphKway(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,int*);
}

void 
Gather_Headers( int* fileDescriptor, std::vector< std::string >& headers );
    
using namespace std;

typedef vector< int > block ;
map< int, map< int, int >  > ParallelData;
 
// ParallelData is the most important data structure of this code it maitains 
// the  global to local mapping of shapefuntion numbers 
// 
// ParallelData[ global_number][ processor_id ] = processor_local_no 
//
// all shape function numbers, global and local are stored in fortran indexing

struct lessKey{
    bool operator()(block b1, block b2)const 
        {
            return ( 100*b1[1] + 10*b1[5] < 100*b2[1] + 10*b2[5] );
        }
};

typedef struct lessKey lessKey;
typedef map< block, vector< vector< int > >, lessKey > Block_Map;

struct CommuTask {
    int tag;
    int type;
    int other_pid;
    vector<int> shpFn ; 
};

typedef struct CommuTask CommuTask;

// since ParallelData[x] is a sorted data structure, the lowest pid 
// adjacency always  becomes the master.

int 
getMaster( int x ) {
    map< int , int >::iterator iter  =  ParallelData[x].begin();
    return (*iter).first;

}
void 
generate_keyphrase(char* target, const char* prefix, block& tpblock) {

    strcpy(target,prefix);  /* interior or boundary */

    switch( tpblock[1] ){
    case 1:
        strcat(target,"linear ");
        break;
    case 2:
        strcat(target,"quadratic ");
        break;
    case 3:
        strcat(target,"cubic ");
        break;
    case 4:
        strcat(target,"quartic ");
        break;
    }

    switch( tpblock[5] ){
    case 1:
        strcat(target,"tetrahedron ");
        break;
    case 2:
        strcat(target,"hexahedron ");
        break;
    case 3:
        if(!strcmp(prefix,"boundary "))
            strcat(target,"wedge triface ");
        else
            strcat(target,"wedge ");
        break;
    case 4:
        strcat(target,"wedge quadface ");
        break;
    case 5:
        if(!strcmp(prefix,"boundary "))
            strcat(target,"pyramid quadface ");
        else
            strcat(target,"pyramid ");
        break;
    case 6:
        strcat(target,"pyramid triface ");
        break;
    }
}


void 
Partition_Problem( int  numProcs,
                        char iformat[],
                        char oformat[]
                        ) {

    int rank =0;
    int stepno;
    int igeombc;  /* file handle for geombc */
    int irestart; /* file handle for restart */
    int iarray[10];
    int ione=1;
    int itwo=2;
    int ithree=3;
    int iseven=7;
    int ieight=8;
    int isize;
    int ierr;
    int nitems;
    int use_restart=1;
    int use_acc=0;//use time derivative of solution

    /* Lets us first load the dual and partition it */

    _directory_name[0]='\0';

    sprintf(_directory_name,"%d-procs_case/",numProcs);

#ifdef WIN32
    struct _stat buf;
    char probdir[256];
    probdir[0]='\0';
    sprintf(probdir,"%d-procs_case",numProcs);
    int result = _stat( probdir, &buf );
   
//    if ( (result == 0 ) &&  ( 01 & ( buf.st_mode >> _S_IFDIR ) ) ) 
      if ( (result == 0 ) &&  ( buf.st_mode & _S_IFDIR ) ) 
#else
    struct stat buf;
    if ( !stat( _directory_name, &buf ) &&  S_ISDIR( buf.st_mode ) ) 
#endif
    {
            cout << "Directory " << _directory_name <<" already exists " << endl;
            cout << " Using the existing inputfiles" << endl;
            if ( !chdir( _directory_name ) ) {/* cd successful */
                cout <<"Changed to the directory " << _directory_name << endl;
                return;
            }else { 
                 cout << "Please check the permissions for the problem";
                 cout << " directory " << _directory_name << endl;
                 exit(0);
            }

     } else {
            cout << _directory_name << " does not exist or is unusable" << endl;
            cout << "creating a new one" << endl;
#ifdef WIN32
            _unlink( _directory_name );
#else
            unlink( _directory_name );
#endif
     }
    // else if the case does not already exist

#ifdef WIN32
    if ( _mkdir( _directory_name ) ) 
#else
    if ( mkdir( _directory_name, 00755 ) ) 
#endif
    {
        cerr << "cannot create directory " << _directory_name << endl;
        exit(0);
    }
    filename[0]='\0';
    sprintf(filename,"%snumpe.in",_directory_name);
    ofstream npe(filename);
    npe << numProcs << endl ;
    npe.close();

    // copy the bct.dat file if one exists
	
    char systemcmd[2048];
 
    int rslt;

#ifdef WIN32
    systemcmd[0]='\0';
    sprintf(systemcmd,"copy /Y *.dat %d-procs_case",numProcs);
    rslt = system(systemcmd);
    if(inpdat.BCTFlag==1){
	    systemcmd[0]='\0';
	    sprintf(systemcmd,"copy /Y *.vtp %d-procs_case",numProcs);
	    rslt = system(systemcmd);
    }
#else
    systemcmd[0]='\0';
    sprintf(systemcmd,"cp -f *.dat %s",_directory_name);
    rslt = system(systemcmd);
    if(inpdat.BCTFlag==1){
        systemcmd[0]='\0';
        sprintf(systemcmd,"cp -f *.vtp %s",_directory_name);
        rslt = system(systemcmd);
    }
#endif
	
    openfile_( "geombc.dat.1", "read", &igeombc );
    vector< string > headers;
    Gather_Headers( &igeombc, headers );

    int numel, nIfaces, spebc;
    readheader_( &igeombc, "keyword xadj", (void*)iarray,
                 &itwo, "integer", iformat );
    numel = iarray[0];
    spebc = iarray[1];
    isize = numel + 1;
    int* xadj = new int [ isize ];
    readdatablock_( &igeombc, "keyword xadj", (void*)xadj,
                    &isize, "integer", iformat );


    readheader_( &igeombc, "keyword adjncy", (void*)iarray,
                 &ione, "integer", iformat );
    nIfaces = iarray[0];
    isize = 2*nIfaces;
    int* adjncy = new int [ isize ];
    readdatablock_( &igeombc, "keyword adjncy", (void*)adjncy,
                    &isize, "integer", iformat );


    isize = numel;
    int* vwgt = new int [ numel ];
    readheader_( &igeombc, "keyword vwgt", (void*)iarray,
                 &ione, "integer", iformat );
    readdatablock_( &igeombc, "keyword vwgt", (void*)vwgt,
                    &isize, "integer", iformat );

    int wgtflag = 2;
    int numflag = 0;
    int edgecut;
    int options[10];
    options[0] = 0; 
    int* epart = new int [ numel ];
    METIS_PartGraphKway( &numel, xadj, adjncy, vwgt, NULL, &wgtflag,
                         &numflag, &numProcs, options, &edgecut, epart);

    /* putting SPEBC elements together on the processor 0 */
    if ( spebc > 0 ) {
        int swap;
        for (int i=0; i<numel; i++)
            if ( vwgt[i] > 1 ) { 
                swap = epart[i];
                break;
            }
        for (int i=0; i<numel; i++) {
            if ( epart[i] == swap ) epart[i] = 0;
            else if ( epart[i] == 0 ) epart[i] = swap;
            if ( vwgt[i] == 0 ) epart[i] = 0;
        }
    }

    delete [] xadj;
    delete [] adjncy;
    delete [] vwgt;

    /* now we have the epart array which is all the partition Info we need */

    int ndof=5; 
    ifstream numstart("numstart.dat");
    numstart >> stepno;
    numstart.close();

    readheader_( &igeombc, "number of global modes", (void*)iarray, 
                 &ione, "integer", iformat );
    int nshgTot = iarray[0];
    int nshg = iarray[0] ;

    readheader_( &igeombc, "number of variables", (void*)iarray, 
                 &ione, "integer", iformat );

    ndof = iarray[0] ;

    readheader_( &igeombc, "maximum number of element nodes", (void*)iarray, 
                 &ione, "integer", iformat );
    int nenmax = iarray[0];

    readheader_( &igeombc, "number of interior tpblocks", (void*)iarray, 
                 &ione, "integer", iformat  );
    int nblock = iarray[0];

    readheader_( &igeombc, "number of nodes in the mesh", (void*)iarray, 
                 &ione, "integer", iformat );
    int numnp = iarray[0];

    readheader_( &igeombc, "number of edges in the mesh", (void*)iarray, 
                 &ione, "integer", iformat );
    int numedges = iarray[0];

    vector< Block_Map >  ien( numProcs );

    vector< map< int, int > > ecorp( numProcs ) ;
    int* epcount=new int[numProcs]();

    int* vcount= new int [numProcs];
    int* ecount= new int [numProcs];
    int* fcount= new int [numProcs];

    for( int e=0; e < numProcs; e++ ) 
        vcount[e] = ecount[e] = fcount[e]= 1;

    for( int b = 0; b < nblock ; b++ ) {//nblock is always 1 since only one block for linear tetrahedron

        readheader_( &igeombc, "connectivity interior?", (void*)iarray,
                     &iseven, "integer", iformat );
        block CurrentBlock ;
        for( int w=1; w< iseven; w++ ) 
            CurrentBlock.push_back( iarray[w] );
        isize = iarray[0]*iarray[3];
        int* ient = new int [ isize ];

        readdatablock_( &igeombc, "connectivity interior?", (void*)ient, &isize,
                        "integer", iformat );

        readheader_( &igeombc, "ien to sms?", (void*)iarray, &ione,
                     "integer", iformat );

        isize=iarray[0];
        int* ient_sms = new int [ isize ];
        readdatablock_( &igeombc, "ien to sms?", (void*)ient_sms,
                        &isize, "integer", iformat );

        for( int c=0; c < iarray[0] ;  c++ ) {
            vector< int > element;
            for( int d=0; d < iarray[3] ; d++ ) {
                element.push_back( ient[d*iarray[0]+c] );
            }
            int gid = ient_sms[c];
            int pid = epart[ gid ];

            ien[pid][CurrentBlock].push_back( element );

            ecorp[pid][epcount[pid]] = gid+1 ;
            epcount[pid]++;

            for( vector< int >::iterator iter = element.begin();
                 iter != element.end();
                 iter++ ) {
                if( !ParallelData[ abs(*iter) ][pid] ) 
                    ParallelData[ abs(*iter) ][pid] 
                                = (abs(*iter) > numnp) ? 
                                    ((abs(*iter) > numnp+numedges )? 
                                       fcount[pid]++: ecount[pid]++) 
                                    : vcount[pid]++ ;
            }
            element.clear();
        }
        delete [] ient;
        delete [] ient_sms;
    }

    delete [] epcount;

    /* At this point the values stored in vcount ecount and fcount are 1 more 
     * than  the number of vertices edges and faces respectively , sp we need 
     * to decrement  all of them by one so that we can directly added them to 
     * the lower order counts  in the next  step. 
     */
 
    for( int e=0; e < numProcs; e++ ) {
        --vcount[e] ; 
        --ecount[e] ; 
        --fcount[e];
    }

    /* our parallel data structure has indepenent numbering for vertices, faces
     * and edges, now we need to correct it and add the correct offsets , we
     * cannot write out ien before we make this correction 
     */

    for( map< int, map< int, int >  >::iterator iter = ParallelData.begin();
         iter != ParallelData.end();
         iter++ ) {
        if ( (*iter).first > numnp ) {         // it is not a vertex 
            if ( (*iter).first > ( numnp + numedges ) ) { // it is not a edge, 
                // must be a face
                for( map< int, int> :: iterator modeIter 
                                                = (*iter).second.begin();
                     modeIter != (*iter).second.end();
                     modeIter++ ) 
                    (*modeIter).second = (*modeIter).second 
                                         + vcount[ (*modeIter).first ] 
                                         + ecount[ (*modeIter).first ] ;
            } else { // is an edge

                for( map< int, int> :: iterator modeIter 
                                                = (*iter).second.begin();
                     modeIter != (*iter).second.end();
                     modeIter++ ) 
                    (*modeIter).second = (*modeIter).second 
                                         + vcount[ (*modeIter).first ];
            }
        }
    }

    delete [] vcount;
    delete [] ecount;
    delete [] fcount;

    /* after correction all local mode numbers are accurate ( hopefully ) and
     * are in fortran style ( starting 1 ) */
                        
    /* at this point, ien still has global numbering */
    /* we now use the parallel data structure to correct ien */

    for( int p = 0 ; p < numProcs; p++ ) {
        for( Block_Map::iterator iter = ien[p].begin();
             iter != ien[p].end();
             iter++ ) {
            for( vector< vector< int > >:: iterator iter2 
                                                    = ((*iter).second).begin();
                 iter2 != ((*iter).second).end();
                 iter2++ ) {
                for( vector< int >:: iterator iter3 = (*iter2).begin();
                     iter3 != (*iter2).end();
                     iter3++ ) {
                    *iter3 = (*iter3 > 0 )? ParallelData[ *iter3 ][ p ] :
                                            -1* ParallelData[ abs(*iter3) ][ p];
                }
            }
        }
    }

    /* now we can write out ien and delete that data structure. */
    /* ien is stored as ien[pid][block][iel][nshl] */
    /* we need to write this block by block for each pid */

    int magic_number = 362436;
    int* mptr = &magic_number;
    int fgeom;
    int frest;

    for( int p=0; p< numProcs; p++ ) {
        int numel=0;
        
        bzero( (void*)filename,255);
        sprintf(filename,"%sgeombc.dat.%d",_directory_name,p+1 );
        openfile_( filename, "write", &fgeom );

        for( vector< string >::iterator iter = headers.begin();
             iter != headers.end();
             iter++ ) {
            writestring_( &fgeom, (*iter).c_str() );
            writestring_( &fgeom, "\n" );
        }
        
#if defined ( DEBUG )
        sprintf(filename,"%sascii_out.%d",_directory_name,p+1);
        ofstream fascii( filename );
        fascii <<"Interior Element Modal Connectivity:"<<endl;
        fascii <<"------------------------------------"<<endl;
#endif

        isize = 1;
        nitems = 1;
        iarray[ 0 ] = 1;
        writeheader_( &fgeom, "byteorder magic number ", 
                      (void*)iarray, &nitems, &isize, "integer", oformat );

        nitems = 1;
        writedatablock_( &fgeom, "byteorder magic number ",
                         (void*)mptr, &nitems, "integer", oformat );

        bzero( (void*)filename,255 );
        sprintf( filename, "number of interior tpblocks : < 0 > %d \n",
                 (int) ien[p].size()  );
        writestring_( &fgeom, filename );

        bzero( (void*)filename,255 );
        sprintf( filename, "number of global modes : < 0 > %d \n" ,nshgTot );
        writestring_( &fgeom, filename );

        for( Block_Map::iterator bIter = ien[p].begin();
             bIter != ien[p].end();
             bIter++ ) {
            block CurrentBlock = (*bIter).first;
            vector< vector< int > > blockIEN = (*bIter).second;
            numel += blockIEN.size();
            isize = blockIEN.size()*CurrentBlock[2];
            iarray[ 0 ] = blockIEN.size();        

#if defined ( DEBUG )
            fascii << endl;
            fascii << "********************"<< endl;
#endif

            int xct = 1;
            for( block::iterator ibtr = CurrentBlock.begin();
                 ibtr != CurrentBlock.end();
                 ibtr++ ) {
                iarray[ xct++ ] = *ibtr;

#if defined ( DEBUG )
                fascii << *ibtr <<" ";
#endif
            }

            generate_keyphrase( keyphrase, "connectivity interior ", 
                                CurrentBlock );
            writeheader_( &fgeom, keyphrase, (void*)iarray, &xct, &isize,
                          "integer", oformat );
   
#if defined ( DEBUG )
            fascii << endl;
            fascii << "********************"<< endl;
#endif
            /* now we need to create the fortran style array for this block of
             * ien, so need to invert it */

            int* ient = new int [ blockIEN.size()*CurrentBlock[2] ];
            for( int h=0; h < blockIEN.size(); h++ ) {
                for( int y=0; y< CurrentBlock[2]; y++ ) {
                    ient[ y*blockIEN.size() + h] = blockIEN[h][y];
#if defined ( DEBUG )
                    fascii << ient[ y*blockIEN.size() + h ] <<" ";
#endif
                }
#if defined ( DEBUG )
                fascii << endl;
#endif
            }
            nitems = blockIEN.size()*CurrentBlock[2];
            writedatablock_( &fgeom, keyphrase, (void*)( ient ),
                             &nitems, "integer", oformat ); 
            
            delete [] ient;
            blockIEN.clear();
        }// loop over blocks with in a processor
        ien[p].clear();

        bzero( (void*)filename, 255 );
        sprintf(filename ,"number of interior elements : < 0 > %d \n",numel);
        writestring_( &fgeom, filename );

        closefile_( &fgeom, "write" );
#if defined ( DEBUG )
        fascii.close();
#endif
    }// loop over procs 
    //ien.clear();
   
    /* co-ords */

    readheader_( &igeombc, "co-ordinates", (void*)iarray, &itwo, "double",
                 iformat );
    isize =  iarray[0]*iarray[1];
    double* xloc = new double [ isize ];
    readdatablock_( &igeombc, "co-ordinates", (void*)xloc, &isize,
                    "double", iformat );
    vector< map<int, vector< double > > > Xpart( numProcs );

    for( int x=1; x < iarray[0]+1; x++ ){
        for( map< int, int>::iterator iter = ParallelData[x].begin();
             iter != ParallelData[x].end();
             iter++ ) {
            for( int s=0; s < 3 ; s++ ) 
                Xpart[(*iter).first][(*iter).second].push_back( 
                                                       xloc[x-1+s*iarray[0]] );
        }
    }
    delete [] xloc;

    int* vnumnp;

    for( int p=0; p< numProcs; p++ ) {

        bzero((void*)filename, 255 );
        sprintf(filename,"%sgeombc.dat.%d",_directory_name,p+1 );
        openfile_( filename, "append", &fgeom );

        bzero((void*)filename, 255 );
        sprintf(filename,"number of nodes : < 0 > %d \n", (int) Xpart[p].size() );
        writestring_( &fgeom, filename );

        bzero((void*)filename, 255 );
        sprintf(filename,"number of processors : < 0 > %d \n", numProcs);
        writestring_( &fgeom, filename );
        
        double* xf = new double [ Xpart[p].size() * 3 ];
        for(   map< int, vector< double > >::iterator mIter = Xpart[p].begin();
               mIter != Xpart[p].end();
               mIter++ )
            for( int f=0; f<3; f++) 
                xf[ f*Xpart[p].size()+ (*mIter).first - 1] = (*mIter).second[f];
            
        isize = Xpart[p].size() * 3 ;
        nitems = 2;
        iarray[ 0 ] = Xpart[p].size();
        iarray[ 1 ] = 3;
        writeheader_( &fgeom, "co-ordinates", (void*)iarray, &nitems, &isize,
                      "double", oformat );

        nitems = Xpart[p].size() * 3;
        writedatablock_( &fgeom, "co-ordinates", (void*)( xf ), &nitems,
                         "double", oformat );

#if defined ( DEBUG )
        sprintf( filename, "%sascii_out.%d",_directory_name, p+1 );
        FILE* cfascii=fopen(filename,"a");
        fprintf(cfascii,"\n");
        fprintf(cfascii, "Nodal coordinates:\n");
        fprintf(cfascii, "------------------\n");
        int numnp_tmp = Xpart[p].size();
        for( int c=0; c< numnp_tmp ; c++ )
            fprintf( cfascii,"%f %f %f \n", 
                     xf[ c ], xf[ c + numnp_tmp ],xf[c+2*numnp_tmp] );
        fclose( cfascii );
#endif          

        delete [] xf ;
        Xpart[p].clear();
        closefile_( &fgeom, "append" );
    }
    Xpart.clear();

    /* varwallprop */
#if(VER_VARWALL == 1)
  if (nomodule.ivarwallprop != 0) {
    iarray[0]=0;
    readheader_( &igeombc, "varwallprop", (void*)iarray, &itwo, "double",
                 iformat );

    if(iarray[0]>0){
    	use_restart=0;
    	int numNode=iarray[0],numComp=iarray[1];
        isize =  numNode*numComp;
        double* wploc = new double [ isize ];
        readdatablock_( &igeombc, "varwallprop", (void*)wploc, &isize,
                        "double", iformat );
        vector< map<int, vector< double > > > Wpart( numProcs );

        for( int x=1; x < numNode+1; x++ ){
            for( map< int, int>::iterator iter = ParallelData[x].begin();
                 iter != ParallelData[x].end();
                 iter++ ) {
                for( int s=0; s < numComp ; s++ )
                    Wpart[(*iter).first][(*iter).second].push_back(
                                                           wploc[x-1+s*numNode] );
            }
        }
        delete [] wploc;

        for( int p=0; p< numProcs; p++ ) {

            bzero((void*)filename, 255 );
            sprintf(filename,"%sgeombc.dat.%d",_directory_name,p+1 );
            openfile_( filename, "append", &fgeom );

            double* wf = new double [ Wpart[p].size() * numComp ];
            for(   map< int, vector< double > >::iterator mIter = Wpart[p].begin();
                   mIter != Wpart[p].end();
                   mIter++ )
                for( int f=0; f<numComp; f++)
                    wf[ f*Wpart[p].size()+ (*mIter).first - 1] = (*mIter).second[f];

            isize = Wpart[p].size() * numComp ;
            nitems = 2;
            iarray[ 0 ] = Wpart[p].size();
            iarray[ 1 ] = numComp;
            writeheader_( &fgeom, "varwallprop", (void*)iarray, &nitems, &isize,
                          "double", oformat );

            nitems = Wpart[p].size() * numComp;
            writedatablock_( &fgeom, "varwallprop", (void*)( wf ), &nitems,
                             "double", oformat );

            delete [] wf ;
            Wpart[p].clear();
            closefile_( &fgeom, "append" );
        }
        Wpart.clear();

    }
  }
#endif
   
    /* let us take care of Essential BCs.*/
    // BCs are in the indirect numbering of nBC so only nBC needs to be sorted 
    // according to nshg
    // BC abd iBC are numbered as numpbc which us given by nBC.

    int numEBC = ndof + 7;
    readheader_( &igeombc, "bc mapping array", (void*)iarray, &ione, "integer",
                 iformat );
    nshg = iarray[0];
    int* nBC = new int [ nshg ];
    readdatablock_( &igeombc, "bc mapping array", (void*)nBC, &nshg, "integer",
                    iformat );

    readheader_( &igeombc, "bc codes array", (void*)iarray, &ione,"integer",
                 iformat );
    int numpbc = iarray[0];
    int* iBC = new int [ numpbc ];
    readdatablock_( &igeombc, "bc codes array", (void*)iBC, &numpbc,
                    "integer", iformat  );

    readheader_( &igeombc, "boundary condition array", (void*)iarray,
                 &ione, "double", iformat );
    double* BC = new double  [ numpbc*numEBC ];
    readdatablock_( &igeombc, "boundary condition array", (void*)BC,
                    &iarray[0], "double", iformat );

    int* inumpbc = new int [ numProcs ];
    for( int c=0; c < numProcs; c++ ) inumpbc[ c ] = 0;
    vector< map< int, int> > nBCpart( numProcs );
    vector< vector< int> > iBCpart ( numProcs );
    vector< vector< double> > BCpart( numProcs );
    
    for( int x=1; x < nshg+1 ; x++ ) {
        if ( nBC[x-1] > 0 ) {
            for( map< int, int>::iterator iter = ParallelData[x].begin();
                 iter != ParallelData[x].end();
                 iter++ ) {
                nBCpart[ (*iter).first ][ (*iter).second ] 
                                                = inumpbc[ (*iter).first ]++; 
                iBCpart[ (*iter).first ].push_back( iBC[ nBC[x-1] - 1 ] );
                for( int g=0; g< numEBC; g++ )
                    BCpart[ (*iter).first ].push_back(BC[nBC[x-1]-1+g*numpbc]); 
            }
        } else {
            for( map< int, int>::iterator iter = ParallelData[x].begin();
                 iter != ParallelData[x].end();
                 iter++ ) 
                nBCpart[ (*iter).first ][ (*iter).second ] = -1;
        }
    }
   
    delete [] nBC;
    delete [] iBC;
    delete [] BC;

    // we will calculate a maxnshg here for later use with sonfath.
    
    int maxnshg =0;

    for( int p=0; p< numProcs; p++ ) {

        bzero( (void*)filename, 255 );
        sprintf(filename,"%sgeombc.dat.%d",_directory_name,p+1 );
        openfile_( filename, "append", &fgeom );

#if defined ( DEBUG )
        sprintf( filename, "%sascii_out.%d",_directory_name, p+1 );
        FILE* cfascii = fopen( filename, "a" );
        fprintf(cfascii,"\nBoundary Condition Mapping Array (nBC):\n" ); 
        fprintf(cfascii,"---------------------------------------\n" );
#endif

        bzero( (void*)filename, 255 );
        sprintf( filename,  "maximum number of element nodes : < 0 > %d\n",
                 nenmax );
        writestring_( &fgeom, filename );
        
        bzero( (void*)filename, 255 );
        sprintf(filename,  "number of modes : < 0 > %d\n", (int) nBCpart[p].size() );
        writestring_( &fgeom, filename );
        
        bzero( (void*)filename, 255 );
        sprintf( filename,
                 "number of shapefunctions soved on processor : < 0 > %d\n", 
                 (int) nBCpart[p].size() );
        writestring_( &fgeom, filename );
        
        bzero( (void*)filename, 255 );
        sprintf( filename, 
                 "number of nodes with Dirichlet BCs : < 0 > %d\n",
                 inumpbc[ p ] );
        writestring_( &fgeom, filename );
        
        isize =  nBCpart[p].size();
        nitems = 1 ;
        iarray[ 0 ] = nBCpart[p].size();
        writeheader_( &fgeom, "bc mapping array", (void*)iarray, &nitems,
                      &isize, "integer", oformat );
        
        int* inBC= new int [ nBCpart[p].size() ];
        int count = 0;
        for( map< int, int>::iterator iter = nBCpart[p].begin();
             iter != nBCpart[p].end();
             iter++ ) {
            inBC[ count++ ] = ((*iter).second + 1) ;

#if defined ( DEBUG )
            fprintf( cfascii, "%d\n", inBC[ count - 1 ] );
#endif

        }
        writedatablock_( &fgeom, "bc mapping array", (void*)( inBC ), &count,
                         "integer", oformat );
        delete [] inBC;
        nBCpart[p].clear();

#if defined ( DEBUG )
        fprintf(cfascii,"\nBoundary Condition Codes Array (iBC):\n" ); 
        fprintf(cfascii,"-------------------------------------\n" );
#endif
        isize = inumpbc[p];
        nitems = 1;
        iarray[ 0 ] = inumpbc[ p ];

        writeheader_( &fgeom, "bc codes array ", (void*) iarray, &nitems,
                      &isize, "integer", oformat );

        int* iiBC = new int [ inumpbc[p] ];
        if ( inumpbc[p] != iBCpart[p].size() ) 
            cerr<<"Error 1" << __LINE__ <<endl;
        for( int i = 0; i< inumpbc[p]; i++ ) {
            iiBC[i] = iBCpart[p][i];
#if defined ( DEBUG )
            fprintf( cfascii, "%d\n", iiBC[ i ] );
#endif
        }
        iBCpart[p].clear();
        nitems = inumpbc[ p ];
        writedatablock_( &fgeom, "bc codes array ", (void*)( iiBC ), &nitems,
                         "integer", oformat );
        delete [] iiBC;
        
#if defined ( DEBUG )
        fprintf(cfascii,"\nBoundary Condition Array (BC):\n" ); 
        fprintf(cfascii,"--------------------------------\n" );
#endif
        isize = inumpbc[p]*numEBC;
        nitems = 1;
        iarray[ 0 ] = inumpbc[p]*numEBC;
        writeheader_( &fgeom, "boundary condition array ", (void*) iarray, 
                      &nitems, &isize, "double", oformat );

        double* BCf = new double [inumpbc[p]*numEBC];
        for( int a = 0 ; a < inumpbc[p]; a++){
            for( int b =0; b < numEBC; b++ ){
                BCf[b*inumpbc[p]+a] = BCpart[p][a*numEBC+b];

#if defined ( DEBUG )
                fprintf( cfascii,"%.3f ",BCpart[p][a*numEBC+b]);
#endif

            }

#if defined ( DEBUG )
            fprintf( cfascii, "\n" );
#endif

        }
        BCpart[p].clear();
        nitems = inumpbc[p]*numEBC; 
        writedatablock_( &fgeom, "boundary condition array ", (void*)( BCf ), 
                         &nitems, "double", oformat );

        delete [] BCf;
        closefile_( &fgeom, "append" );

#if defined ( DEBUG )
        fclose( cfascii );
#endif
    }

    iBCpart.clear();
    BCpart.clear();
        

    /* done writing ebcs */

    /* ienb and BCB and iBCB */

    
    int numNBC = ndof + 1;


    readheader_( &igeombc, "number of boundary tpblocks", (void*)iarray, 
                 &ione, "integer", iformat );
    nblock = iarray[0];

    //ienb -- Boundary Element Nodal Connectivity.
    //ibcb -- Natural Boundary Condition Codes
    //bcb  -- Natural Boundary Condition Values

    vector< map< block, vector< int >, lessKey > > iBCBpart( numProcs );
    vector< map< block, vector< double >, lessKey > > BCBpart( numProcs );

    for( int b = 0; b < nblock ; b++ ) {
        
        readheader_( &igeombc, "connectivity boundary?", (void*)iarray, &ieight,
                     "integer", iformat );

        block CurrentBlock;
        for( int w=1; w< ieight; w++ ) CurrentBlock.push_back( iarray[w] );
        
        isize = iarray[0]*iarray[3];
        int* ient = new int [ isize ];
        readdatablock_( &igeombc, "connectivity boundary?", (void*)ient,
                        &isize, "integer", iformat );
        
        readheader_( &igeombc, "ienb to sms?", (void*)iarray, &ione,
                     "integer", iformat );

        isize=iarray[0];
        int* ient_sms = new int [ isize ];
        readdatablock_( &igeombc, "ienb to sms?", (void*)ient_sms,
                        &isize, "integer", iformat );


        readheader_( &igeombc, "nbc codes?", (void*)iarray, &ieight,
                     "integer", iformat );

        isize = iarray[0]*2;
        int* iBCB = new int [ isize ];
        readdatablock_( &igeombc, "nbc codes?", (void*)iBCB,
                        &isize, "integer", iformat );

        readheader_( &igeombc, "nbc values?", (void*)iarray, &ieight,
                     "double", iformat );

        isize = iarray[0]*numNBC;
        double* BCB = new double [ isize ];
        readdatablock_( &igeombc,"nbc values?",(void*)BCB, &isize,
                        "double", iformat );


        for( int c=0; c < iarray[0] ;  c++ ) {
            vector< int > element;
            for( int d=0; d < iarray[3] ; d++ )
                element.push_back( ient[d*iarray[0]+c] );

            int gid = ient_sms[c];
            int pid = epart[ gid ];

            for( vector< int >::iterator iter = element.begin();
                 iter != element.end();
                 iter++ ) {
                *iter = ( *iter > 0 ) ? ParallelData[ *iter ][pid] :
                    -1*ParallelData[ abs(*iter) ][pid];
                
            }
            ien[pid][CurrentBlock].push_back( element );
            element.clear();

            iBCBpart[pid][CurrentBlock].push_back( iBCB[c] );
            iBCBpart[pid][CurrentBlock].push_back( iBCB[c + iarray[0]] );

            for( int o=0; o< numNBC; o++) 
                BCBpart[pid][CurrentBlock].push_back( BCB[ c + iarray[0]*o ] ); 

        }
          
        delete [] ient;
        delete [] ient_sms;
        delete [] iBCB;
        delete [] BCB;
    }

    delete [] epart;

    /* format and write out */

    for( int p=0; p< numProcs; p++ ) {
        
        bzero( (void*)filename,255);
        sprintf(filename,"%sgeombc.dat.%d",_directory_name,p+1 );
        openfile_( filename , "append", &fgeom );
        
        bzero( (void*)filename,255);
        sprintf( filename,"number of boundary tpblocks : < 0 > %d\n", 
                 (int) ien[p].size() );
        writestring_( &fgeom, filename );

#if defined ( DEBUG )
        sprintf(filename,"%sascii_out.%d",_directory_name,p+1);
        ofstream fascii( filename, ios_base::app );
        fascii << endl;
        fascii <<"Boundary Element Modal Connectivity:"<<endl;
        fascii <<"------------------------------------"<<endl;
#endif
        int numelb = 0;
        for( Block_Map::iterator bIter = ien[p].begin();
             bIter != ien[p].end();
             bIter++ ) {
            block CurrentBlock = (*bIter).first;
            vector< vector< int > > blockIEN = (*bIter).second;
            numelb += blockIEN.size();
             
            isize = blockIEN.size()*CurrentBlock[2];
            iarray[ 0 ] = blockIEN.size();

#if defined ( DEBUG )
            fascii << endl;
            fascii <<"************************"<< endl;    
#endif
            int xct = 1;
            for( block::iterator ibtr = CurrentBlock.begin();
                 ibtr != CurrentBlock.end();
                 ibtr++ ) {
                iarray[ xct++ ] = *ibtr;
#if defined ( DEBUG )
                fascii << *ibtr <<" ";
#endif
            }
            generate_keyphrase( keyphrase, "connectivity boundary ", 
                                CurrentBlock );
            writeheader_( &fgeom, keyphrase, (void*)iarray, &xct, &isize,
                          "integer", oformat );

#if defined ( DEBUG )
            fascii << endl;
            fascii <<"************************"<< endl;    
#endif

            /* now we need to create the fortran style array for this block of
             * ien, so need to invert it */

            int* ient = new int [ blockIEN.size()*CurrentBlock[2] ];
            for( int h=0; h< blockIEN.size(); h++ ) {
                for( int y=0; y< CurrentBlock[2]; y++ ) {
                    ient[ y*blockIEN.size() + h] = blockIEN[h][y];
#if defined ( DEBUG )
                    fascii << ient[ y*blockIEN.size() + h ] <<" ";
#endif
                }
#if defined ( DEBUG )
                fascii << endl;
#endif
            }
          
            nitems = blockIEN.size()*CurrentBlock[2];
            writedatablock_( &fgeom, keyphrase, (void*)( ient ),
                             &nitems, "integer", oformat ); 

            delete [] ient;

            /* need to also write IBCB and BCB now */
#if defined ( DEBUG )
            fascii << "------------------------" << endl;
            fascii << "iBCB for the above block" << endl;
            fascii << "------------------------" << endl;
#endif

            int* iBCBf = new int [ blockIEN.size() * 2 ];
            for( int u=0; u< blockIEN.size(); u++ )  {
                iBCBf[ u ] = iBCBpart[p][CurrentBlock][u*2];
                iBCBf[ u + blockIEN.size() ] = iBCBpart[p][CurrentBlock][u*2+1];

#if defined ( DEBUG )
                fascii << iBCBf[u] <<"  "<<iBCBf[ u + blockIEN.size() ]<< endl;
#endif

            }

            iBCBpart[p][CurrentBlock].clear();

            isize = blockIEN.size()*2;
            nitems = xct;
            generate_keyphrase( keyphrase, "nbc codes ", CurrentBlock );
            writeheader_( &fgeom, keyphrase, (void*)iarray, &xct, &isize,
                          "integer", oformat );

            nitems = blockIEN.size()*2;
            writedatablock_( &fgeom, keyphrase, (void*)( iBCBf ), &nitems,
                             "integer", oformat );

            delete [] iBCBf;

#if defined ( DEBUG )
            fascii.precision( 8 );
            fascii << "------------------------" << endl;
            fascii << "BCB for the above block" << endl;
            fascii << "------------------------" << endl;
            fascii.precision( 8 );
#endif
            double* BCBf = new double [ blockIEN.size() * numNBC ];
            for( int u=0; u< blockIEN.size(); u++ ) { 
                for( int v=0; v< numNBC; v++ ) { 
                    BCBf[v* blockIEN.size()+u] 
                                   = BCBpart[p][CurrentBlock][u*numNBC+v];

#if defined ( DEBUG )
                    fascii << BCBf[v* blockIEN.size()+u] << " ";
#endif

                }

#if defined ( DEBUG )
                fascii << endl ;
#endif

            }

            BCBpart[p][CurrentBlock].clear();

            isize = blockIEN.size()*numNBC;
            nitems = xct;
            generate_keyphrase( keyphrase, "nbc values ", CurrentBlock );
            writeheader_( &fgeom, keyphrase, (void*)iarray, &xct, &isize,
                          "double", oformat );

            nitems = blockIEN.size()*numNBC;        
            writedatablock_( &fgeom, keyphrase, (void*)( BCBf ), &nitems,
                             "double", oformat );
            delete [] BCBf;
                    
            blockIEN.clear();
        }// loop over blocks with in a processor
        ien[p].clear();

        bzero((void*)filename , 255 );
        sprintf(filename,"number of boundary elements : < 0 > %d \n", numelb );
        writestring_( &fgeom, filename );

        closefile_( &fgeom, "append" );

#if defined ( DEBUG )
        fascii.close();
#endif
    }// loop over procs 
    ien.clear();


    /* now starts commu and periodicity */
    /* We need to generate ilwork and ncorp and the iper array for each
     * processor . To achieve this we first read in the complete iper array and
     * then go through "ParallelData" iterating all the global shapefunctions
     * and then generate a consistant communication trace for all 
     * for on processor periodiciy we just dump an array */

    readheader_( &igeombc, "periodic masters array?", (void*)iarray, &ione, 
                 "integer", iformat );
    int* periodic = new int [ nshg ];
    readdatablock_( &igeombc,"periodic masters array?", (void*)periodic, &nshg,
                    "integer", iformat );
    vector< map< int, int > > PeriodicPart( numProcs );
    
    CommuTask*** rtask ;
    CommuTask*** stask;

    rtask = new CommuTask** [numProcs];   
    stask = new CommuTask** [numProcs];   
    for( int b=0; b< numProcs; b++ ){
        rtask[b] = new CommuTask* [numProcs];   
        stask[b] = new CommuTask* [numProcs];   
        for( int c=0; c< numProcs; c++ ){
            rtask[b][c] = NULL;
            stask[b][c] = NULL;
        }
    }
    int newtag=1;
    bool isPERIODIC = false ;
    for( int x=1; x < nshg+1; x++ ) {

    // first we need to fill a 0 in all the iper entries of x and its images 
    // since this will be the case even if the node has no periodicity at all 

        isPERIODIC = false ;

        for( map< int, int >::iterator iter = ParallelData[x].begin();
             iter!= ParallelData[x].end();
             iter++ ) 
            PeriodicPart[ (*iter).first ][ (*iter).second ] = 0;

             
        int master_pid = getMaster( x ); // assuming no periodicity
        int Global_Master = ParallelData[x][master_pid];

        if ( periodic[ x-1 ] > 0 ){  

    // if there is periodicity, then the real master node and processor are 
    // not that of the current mode but of the master mode,  and need 
    // updating...also now the image which shares this master_pid should get 
    // an iper and not a commu.

            master_pid = getMaster( periodic[x-1] );

    // master_pid is the processor on which the master is real ( present ) 

            Global_Master = ParallelData[periodic[x-1]][master_pid];  

    // Global_Master is the processor local shape function number of the master
    // mode on the master processor

            isPERIODIC = true;
        }
    // periodicity has been taken care of 
    // now we do the simple commu to the master_pid 
         
        for( map< int, int>::iterator iter = ParallelData[x].begin();
             iter != ParallelData[x].end();
             iter++ )             
            if ( (*iter).first != master_pid ) {

                int sender = (*iter).first;
                int receiver = master_pid;
                 
    // if we ever add a send task from A to B then we also immediately add a 
    // receive task from B to A so its enuf if we check one of them 
                if ( !stask[sender][receiver] ) {
                    stask[sender][receiver] = new CommuTask;
                    rtask[receiver][sender] = new CommuTask;
                    stask[sender][receiver]->tag = newtag;
                    rtask[receiver][sender]->tag = newtag++;
                    stask[sender][receiver]->type = 0;
                    rtask[receiver][sender]->type = 1;
                    stask[sender][receiver]->other_pid = receiver;
                    rtask[receiver][sender]->other_pid = sender;
                }
                 
                stask[sender][receiver]->shpFn.push_back( (*iter).second );
                rtask[receiver][sender]->shpFn.push_back( Global_Master );

            } else {
    // we  have an image of the original slave which is on the master
    // processor of the master node for this image we need to fill iper
    // instead of commu. ( ofcourse only for periodic modes )

                if ( isPERIODIC ) {
                    PeriodicPart[ (*iter).first ][ (*iter).second ] 
                                                            = Global_Master;
                }           
            }
    }

    delete [] periodic;

    // now to generate ILwork and Periodicity

    vector<int>  ilwork;

#if defined ( DEBUG)
    sprintf(filename,"%silwork.info",_directory_name );
    ofstream ilwf( filename );
#endif

    for( int a=0; a< numProcs; a++ ){ //outer loop over procs 
        ilwork.push_back( 0 );

#if defined ( DEBUG )
        ilwf << a+1 <<" receives  ";
#endif

        for( int b=0; b< numProcs; b++ )  { // inner loop 1 over procs
            if( rtask[a][b] ) {
                ilwork[0]++;
                ilwork.push_back( rtask[a][b]->tag );
                ilwork.push_back( rtask[a][b]->type );
                ilwork.push_back( rtask[a][b]->other_pid+1 );
                ilwork.push_back( rtask[a][b]->shpFn.size() );

#if defined ( DEBUG )
                ilwf << " from "   << rtask[a][b]->other_pid+1 
                     << " tag "    << rtask[a][b]->tag 
                     << " numSeg " << rtask[a][b]->shpFn.size() << endl ; 
#endif

                for( vector<int>::iterator iter = rtask[a][b]->shpFn.begin();
                     iter != rtask[a][b]->shpFn.end();
                     iter++ ) {
                    ilwork.push_back( *iter );
                    ilwork.push_back( 1 );

#if defined ( DEBUG )
                    ilwf << *iter << " ,";
#endif

                }

#if defined ( DEBUG )
                ilwf << endl;
#endif

                delete rtask[a][b];

            } 

        } // end inner loop 1 over procs

#if defined ( DEBUG )

        ilwf << endl;
        
        ilwf << a+1 << " sends  ";
#endif

        for( int b=0; b< numProcs; b++ ) { // inner loop 2 over procs
            if( stask[a][b] ) {
                ilwork[0]++;
                ilwork.push_back( stask[a][b]->tag );
                ilwork.push_back( stask[a][b]->type );
                ilwork.push_back( stask[a][b]->other_pid+1 );
                ilwork.push_back( stask[a][b]->shpFn.size() );

#if defined ( DEBUG )
                ilwf << " to "   << stask[a][b]->other_pid+1 
                     << " tag "    << stask[a][b]->tag
                     << " numSeg " << stask[a][b]->shpFn.size() << endl;
#endif

                for( vector<int>::iterator iter = stask[a][b]->shpFn.begin();
                     iter != stask[a][b]->shpFn.end();
                     iter++ ) {
                    ilwork.push_back( *iter );
                    ilwork.push_back( 1 );

#if defined ( DEBUG )
                    ilwf << *iter << " ,";
#endif

                }

#if defined ( DEBUG )
                ilwf << endl;
#endif

                delete stask[a][b];
            } 
        } // end inner 2loop over procs

#if defined ( DEBUG )
        ilwf << endl;
        ilwf << "Size of ILwork for "<< a+1 << " : " << ilwork.size() << endl;
        if ( ilwork.size() > 0 ) {
            for( vector<int>::iterator iter = ilwork.begin();
                 iter != ilwork.end();
                 iter++ ) 
                ilwf << *iter <<" ";

            ilwf << endl;
            ilwf << endl;
        }
#endif

        delete [] rtask[a];
        delete [] stask[a];

        bzero( (void*)filename, 255 );
        sprintf(filename,"%sgeombc.dat.%d",_directory_name,a+1 );
        openfile_( filename, "append", &fgeom );

        bzero( (void*)filename, 255 );
        sprintf(filename,"size of ilwork array : < 0 > %d\n", (int) ilwork.size() );
        writestring_( &fgeom, filename );

        isize = ilwork.size();
        nitems = 1;
        iarray[ 0 ] = ilwork.size();
        writeheader_( &fgeom, "ilwork ", (void*)iarray,
                      &nitems, &isize, "integer", oformat );

        int* filwork = new int [ilwork.size()];
        for( int r=0; r< ilwork.size(); r++ ) filwork[r] = ilwork[r];
        nitems = ilwork.size();
        writedatablock_( &fgeom, "ilwork ", (void*)(filwork), &nitems,
                         "integer", oformat );
        ilwork.clear();
        delete [] filwork;

        isize = PeriodicPart[a].size();
        nitems = 1 ;
        iarray[ 0 ] = PeriodicPart[a].size();
        writeheader_( &fgeom, "periodic masters array ", (void* )iarray,
                      &nitems, &isize, "integer", oformat );

        int* fper= new int [ PeriodicPart[a].size() ];
        int count = 0;
        for( map< int, int>::iterator iter = PeriodicPart[a].begin();
             iter != PeriodicPart[a].end();
             iter++ ) 
            fper[ count++ ] = (*iter).second;

#if defined ( DEBUG )

        sprintf( filename, "%sascii_out.%d",_directory_name, a+1 );
        ofstream fascii( filename, ios_base::app );
        fascii <<"\n------------------------"<<endl;
        fascii <<"Periodic Partners Array:"<<endl;
        fascii <<"------------------------"<<endl;
        for( int shg=0; shg < count ; shg++ ) 
            fascii << fper[ shg ] << endl;
        fascii.close();
#endif
        
        nitems = count;
        writedatablock_( &fgeom, "periodic masters array ", (void*)( fper ),
                         &nitems, "integer", oformat );

        delete [] fper;
        PeriodicPart[a].clear();
        closefile_( &fgeom , "append" );

    } // end outer loop over procs

#if defined ( DEBUG )
    ilwf.close();
#endif

    PeriodicPart.clear();

    delete [] rtask;
    delete [] stask;

    // write ncorp
    // generating ncorp 
    // ncorp is our map between the "partition local" and the global (sms) 
    // numbering of modes  ncorp[ proc ][ local number ] = sms_number

    vector< map< int, int > > ncorp( numProcs ) ;
    for( int x=1; x < nshg+1; x++ ) {
        for( map< int, int>::iterator iter=ParallelData[x].begin();
             iter != ParallelData[x].end();
             iter++ )
            ncorp[(*iter).first][(*iter).second] = x ;
    }

 //       Here writing data required for svLS
      for( int a=0; a < numProcs ; a++ ) {
         bzero( (void*)filename, 255 );
         ofstream myfileltg;
         sprintf( filename, "%sltg.dat.%d",_directory_name,a);
         myfileltg.open (filename, ios::out); 
         myfileltg << nshg << endl;
         myfileltg << ncorp[a].size() << endl;
         for( map<int, int>::iterator niter = ncorp[a].begin(); niter != ncorp[a].end(); niter++ ) {
            myfileltg << (*niter).second << endl;
         }
         myfileltg.close();
      }


    for( int a=0; a< numProcs; a++ ){

        bzero( (void*)filename, 255 );
        sprintf( filename, "%sgeombc.dat.%d",_directory_name,a+1 );
        openfile_( filename, "append", &fgeom );
        
        isize = ncorp[a].size() ;
        nitems = 1;
        iarray[ 0 ] = ncorp[a].size() ;
        writeheader_( &fgeom, " mode number map from partition to global",
                      (void*)iarray, &nitems, &isize, "integer", oformat );


        int* fncorp = new int [  ncorp[a].size() ];
        int count = 0;
        for( map< int, int>::iterator iter = ncorp[a].begin();
             iter != ncorp[a].end();
             iter++ )
            fncorp[count++] = (*iter).second ;

        nitems = count;
        writedatablock_( &fgeom, " mode number map from partition to global",
                         (void*)(fncorp), &nitems, "integer", oformat );

        delete [] fncorp ;

        isize=ecorp[a].size();
        nitems=1;
        iarray[0]=ecorp[a].size();
        writeheader_( &fgeom, " element number map from partition to global",
                      (void*)iarray, &nitems, &isize, "integer", oformat );

        int* fecorp = new int [  ecorp[a].size() ];
        count = 0;
        for( map< int, int>::iterator iter = ecorp[a].begin();
             iter != ecorp[a].end();
             iter++ )
            fecorp[count++] = (*iter).second ;

        nitems = count;
        writedatablock_( &fgeom, " element number map from partition to global",
                         (void*)(fecorp), &nitems, "integer", oformat );

        delete [] fecorp ;

        closefile_( &fgeom, "append" );
        ncorp[a].clear();
        ecorp[a].clear();
    } 

    for( int a=0; a< numProcs; a++ ){

        bzero( (void*)filename, 255 );
        sprintf( filename, "%sgeombc.dat.%d",_directory_name,a+1 );
        openfile_( filename, "append", &fgeom );

        isize = 2;
        nitems = 1 ;
        iarray[0] = isize;

        writeheader_( &fgeom, "material properties ", (void*)iarray, &nitems, &isize,
                      "double", oformat  );

        double matprops[10];
        nitems = isize;
        matprops[0]=matdat.datmat[0][0][0];
        matprops[1]=matdat.datmat[0][1][0];
        writedatablock_( &fgeom, "material properties ", (void*)matprops, &nitems,
                         "double", oformat );

        closefile_( &fgeom, "append" );
    }



//       The end of writing data

    //write restart

    sprintf( filename, "restart.%d.1", stepno );  
    openfile_( filename, "read", &irestart );
    readheader_( &irestart, "solution?", (void*)iarray, &ithree, 
                 "double", iformat );

    nshg  = iarray[0];
    ndof  = iarray[1];
    isize = nshg * ndof ;

    double* solution = new double [ isize ];

    readdatablock_( &irestart,"solution?", (void*)solution, &isize, 
                    "double", iformat  );
    
    // now we need to create the partitioned data structure.
    // keeping in mind that unlike BCs this has a direct corelation 
    // to each nshg so we need to use a 
    // sorted data structure and we choose a std::map for simplicity

    vector< map< int, vector< double > > > solPart( numProcs );

    for( int x = 1; x < nshg+1; x++ ){
        for( map<int,int>::iterator pIter = ParallelData[x].begin();
             pIter != ParallelData[x].end();
             pIter++ ) {
            for( int v=0; v< ndof ; v++ )
                solPart[ (*pIter).first ][ (*pIter).second ].push_back( 
                                                   solution[ v*nshg + x -1 ] );
        }
    }
      
    delete [] solution;   

    int nsd = 3;
    double* displacement;
    double* fDisplacement; 
    int nshgLocalDisp;
    vector< map< int, vector< double > > > dispPart( numProcs );

    // check flag to see if we expect displacements to exist
    if (nomodule.ideformwall != 0) {

      //same deal for the displacements
 
      readheader_( &irestart, "displacement?", (void*)iarray, &ithree, 
                 "double", iformat );

      nshg  = iarray[0];
      nsd  = iarray[1];
      isize = nshg * nsd ;

      displacement = new double [ isize ];

      readdatablock_( &irestart,"displacement?", (void*)displacement, &isize, 
                    "double", iformat  );  

      for( int x = 1; x < nshg+1; x++ ){
        for( map<int,int>::iterator pIter = ParallelData[x].begin();
             pIter != ParallelData[x].end();
             pIter++ ) {
            for( int v=0; v< nsd ; v++ )
                dispPart[ (*pIter).first ][ (*pIter).second ].push_back( 
                                                   displacement[ v*nshg + x -1 ] );
        }
      }
      
      delete [] displacement;   

    }

    int nsdtd = 4;
    double* timederiv;
    double* fTimederiv;
    int nshgLocalTimederiv;

    vector< map< int, vector< double > > > timederivPart( numProcs );

	  iarray[0]=0;
	  readheader_( &irestart, "time derivative of solution?", (void*)iarray, &ithree,
				 "double", iformat );

	  if(iarray[0]>0){
		  use_acc=1;

		  nshg  = iarray[0];
		  nsdtd  = iarray[1];
		  isize = nshg * nsdtd ;

		  timederiv = new double [ isize ];

		  readdatablock_( &irestart,"time derivative of solution?", (void*)timederiv, &isize,
						"double", iformat  );

		  for( int x = 1; x < nshg+1; x++ ){
			for( map<int,int>::iterator pIter = ParallelData[x].begin();
				 pIter != ParallelData[x].end();
				 pIter++ ) {
				for( int v=0; v< nsdtd ; v++ )
					timederivPart[ (*pIter).first ][ (*pIter).second ].push_back(
													   timederiv[ v*nshg + x -1 ] );
			}
		  }

		  delete [] timederiv;
	  }

#if(VER_VARWALL == 1)
    int nsdwp = 2;
    double* wallprop;
    double* fWallprop;
    int nshgLocalWallprop;

    vector< map< int, vector< double > > > wallpropPart( numProcs );

    // check flag to see if we expect variable wall properties to exist
    if (nomodule.ivarwallprop != 0 && use_restart==1) {

      iarray[0]=0;
      //same deal for the variable wall thickness and Young Mod
      readheader_( &irestart, "varwallprop?", (void*)iarray, &ithree,
                 "double", iformat );

      if(iarray[0]>0){
      	  use_restart=1;

		  nshg  = iarray[0];
		  nsdwp  = iarray[1];
		  isize = nshg * nsdwp ;

		  wallprop = new double [ isize ];

		  readdatablock_( &irestart,"varwallprop?", (void*)wallprop, &isize,
						"double", iformat  );

		  for( int x = 1; x < nshg+1; x++ ){
			for( map<int,int>::iterator pIter = ParallelData[x].begin();
				 pIter != ParallelData[x].end();
				 pIter++ ) {
				for( int v=0; v< nsdwp ; v++ )
					wallpropPart[ (*pIter).first ][ (*pIter).second ].push_back(
													   wallprop[ v*nshg + x -1 ] );
			}
		  }

		  delete [] wallprop;
      }else{
          use_restart=0;
      }
    }
#endif

    // now we have the partitioned data structure we need to write it out.

    for( int a=0; a< numProcs; a++ ){ 
        
        int nshgLocal = solPart[a].size();
        double* fSolution = new double [ nshgLocal * ndof ];

        if (nomodule.ideformwall != 0) {
          nshgLocalDisp = dispPart[a].size();
          fDisplacement = new double [ nshgLocalDisp * nsd ];
	    }

        if(use_acc==1){
            nshgLocalTimederiv = timederivPart[a].size();
            fTimederiv = new double [ nshgLocalTimederiv * nsdtd ];
        }

#if(VER_VARWALL == 1)
        if (nomodule.ivarwallprop != 0 && use_restart==1) {
          nshgLocalWallprop = wallpropPart[a].size();
          fWallprop = new double [ nshgLocalWallprop * nsdwp ];
	    }
#endif



#if defined ( DEBUG )
        sprintf( filename,"%srestart.asc.%d",_directory_name, a+1 );
        FILE* rascii = fopen( filename,"w");
        fprintf( rascii, "nshgLocal: %d\n", nshgLocal);
        fprintf( rascii, "numVars: %d\n", ndof );
        fprintf( rascii, "Step Number: %d\n", stepno);
        fprintf( rascii, "---------------------------------------------\n");
        for( int y=1; y < nshgLocal+1 ; y++ ) {
            fprintf( rascii, "%d :", y ) ;
            for( int w=0; w< ndof; w++ ) { 
                fprintf(rascii," %f ", solPart[a][y][w] );
            }
            fprintf( rascii, "\n");
        }
        fprintf( rascii, "nshgLocalDisp: %d\n", nshgLocalDisp);
        fprintf( rascii, "numVars: %d\n", nsd );
        fprintf( rascii, "Step Number: %d\n", stepno);
        fprintf( rascii, "---------------------------------------------\n");
        for( int y=1; y < nshgLocalDisp+1 ; y++ ) {
            fprintf( rascii, "%d :", y ) ;
            for( int w=0; w< nsd; w++ ) { 
                fprintf(rascii," %f ", dispPart[a][y][w] );
            }
            fprintf( rascii, "\n");
        }

#if(VER_VARWALL == 1)
        if (nomodule.ivarwallprop != 0 && use_restart==1) {
			fprintf( rascii, "nshgLocalwallprop: %d\n", nshgLocalWallprop);
			fprintf( rascii, "numVars: %d\n", nsdwp );
			fprintf( rascii, "Step Number: %d\n", stepno);
			fprintf( rascii, "---------------------------------------------\n");
			for( int y=1; y < nshgLocalWallprop+1 ; y++ ) {
				fprintf( rascii, "%d :", y ) ;
				for( int w=0; w< nsdwp; w++ ) {
					fprintf(rascii," %f ", wallpropPart[a][y][w] );
				}
				fprintf( rascii, "\n");
			}
        }
#endif


        fclose( rascii );
#endif
        for( int w=0; w< ndof; w++ ) 
            for( int y=1; y < nshgLocal+1 ; y++ ) {
                fSolution[ w*nshgLocal+(y-1) ] = solPart[a][y][w];
            }

        solPart[a].clear();

        if (nomodule.ideformwall != 0) {
          for( int w=0; w< nsd; w++ ) 
            for( int y=1; y < nshgLocalDisp+1 ; y++ ) {
                fDisplacement[ w*nshgLocalDisp+(y-1) ] = dispPart[a][y][w];
            }

          dispPart[a].clear();
        }

        if(use_acc==1){
            for( int w=0; w< nsdtd; w++ )
              for( int y=1; y < nshgLocalTimederiv+1 ; y++ ) {
                  fTimederiv[ w*nshgLocalTimederiv+(y-1) ] = timederivPart[a][y][w];
              }

            timederivPart[a].clear();
        }

#if(VER_VARWALL == 1)
       if (nomodule.ivarwallprop != 0 && use_restart==1) {
          for( int w=0; w< nsdwp; w++ )
            for( int y=1; y < nshgLocalWallprop+1 ; y++ ) {
                fWallprop[ w*nshgLocalWallprop+(y-1) ] = wallpropPart[a][y][w];
            }

          wallpropPart[a].clear();
        }
#endif

        bzero( (void*)filename, 255 );
        sprintf(filename,"%srestart.%d.%d",_directory_name, stepno, a+1 );
        openfile_( filename, "write", &frest );
        
        for( vector< string >::iterator iter = headers.begin();
             iter != headers.end();
             iter++ ) {
            writestring_( &frest, (*iter).c_str() );
            writestring_( &frest, "\n" );
        }
        headers.clear();

        bzero( (void*)filename, 255 );
        sprintf(filename,"number of modes : < 0 > %d\n", nshgLocal);
        writestring_( &frest, filename );
        
        bzero( (void*)filename, 255 );
        sprintf(filename,"number of variables : < 0 > %d\n", ndof);
        writestring_( &frest, filename );
        
        isize = 1;
        nitems = 1;
        iarray[ 0 ] = 1;
        writeheader_( &frest, "byteorder magic number ", 
                      (void*)iarray, &nitems, &isize, "integer", oformat );
        
        nitems = 1;
        writedatablock_( &frest, "byteorder magic number ",
                         (void*)mptr, &nitems, "integer", oformat );
        
        
        isize = nshgLocal*ndof;
        nitems = 3;
        iarray[ 0 ] = nshgLocal;
        iarray[ 1 ] = ndof;
        iarray[ 2 ] = stepno;
        writeheader_( &frest, "solution ", 
                      (void*)iarray, &nitems, &isize, "double", oformat );
        
        
        nitems = nshgLocal*ndof;
        writedatablock_( &frest, "solution ",
                         (void*)(fSolution), &nitems, "double", oformat );

        if (nomodule.ideformwall != 0) {
          isize = nshgLocalDisp*nsd;
          nitems = 3;
          iarray[ 0 ] = nshgLocalDisp;
          iarray[ 1 ] = nsd;
          iarray[ 2 ] = stepno;
          writeheader_( &frest, "displacement ", 
                      (void*)iarray, &nitems, &isize, "double", oformat );
        
        
          nitems = nshgLocalDisp*nsd;
          writedatablock_( &frest, "displacement ",
                         (void*)(fDisplacement), &nitems, "double", oformat );
        }

        if(use_acc==1){
            isize = nshgLocalTimederiv*nsdtd;
            nitems = 3;
            iarray[ 0 ] = nshgLocalTimederiv;
            iarray[ 1 ] = nsdtd;
            iarray[ 2 ] = stepno;
            writeheader_( &frest, "time derivative of solution ",
                        (void*)iarray, &nitems, &isize, "double", oformat );


            nitems = nshgLocalTimederiv*nsdtd;
            writedatablock_( &frest, "time derivative of solution ",
                           (void*)(fTimederiv), &nitems, "double", oformat );
        }

#if(VER_VARWALL == 1)
        if (nomodule.ivarwallprop != 0 && use_restart==1) {
          isize = nshgLocalWallprop*nsdwp;
          nitems = 3;
          iarray[ 0 ] = nshgLocalWallprop;
          iarray[ 1 ] = nsdwp;
          iarray[ 2 ] = stepno;
          writeheader_( &frest, "varwallprop ",
                      (void*)iarray, &nitems, &isize, "double", oformat );
          nitems = nshgLocalWallprop*nsdwp;
          writedatablock_( &frest, "varwallprop ",
                         (void*)(fWallprop), &nitems, "double", oformat );
        }
#endif


        closefile_( &frest, "write" );
        
        delete [] fSolution;
 
        if (nomodule.ideformwall != 0) {
          delete [] fDisplacement;
	    }

        if(use_acc==1){
        	delete [] fTimederiv;
        }

#if(VER_VARWALL == 1)
    if (nomodule.ivarwallprop != 0 && use_restart==1) {
      delete [] fWallprop;
	}
#endif

    }

    filename[0]='\0';    
    sprintf(filename,"%snumstart.dat", _directory_name );
    ofstream nstart( filename );
    nstart << stepno ;
    nstart.close();

    closefile_( &igeombc, "read" );
    closefile_( &irestart, "read" );
    if ( !chdir( _directory_name ) ) /* cd successful */
      cout <<" Changed to the directory " << _directory_name << endl;
    ParallelData.clear();
}
