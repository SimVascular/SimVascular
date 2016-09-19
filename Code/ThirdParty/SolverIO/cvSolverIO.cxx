/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
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

#include <string>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cvSolverIO.h"

#include <ctype.h>

#define INT 1
#define FLOAT 2
#define DOUBLE 3
#define swap_char(A,B) { ucTmp = A; A = B ; B = ucTmp; }

#define FLOWSOLVER_MAGIC_NUMBER 362436

cvsolverIO** cvsolverIOfp = NULL;

cvsolverIO::cvsolverIO () {
    //byte_order_;
    type_of_data_=0;
    DataSize_=0;
    LastHeaderNotFound_ = false;
    Wrong_Endian_ = false ;
    binary_format_ = true; 
    char *mode_ = NULL;
    char *fname_ = NULL;
}

cvsolverIO::~cvsolverIO () {
    if (mode_ != NULL) delete [] mode_;
    if (fname_ != NULL) delete [] fname_;
}

//
//  File Control
//

int cvsolverIO::openFile (const char *filename, const char *mode) {

    filePointer_=Z_NULL ;

    fname_ = StringStripper( filename );
    mode_ = StringStripper( mode );

    if ( cscompare( mode_, "read" ) ) 
        filePointer_ = gzopen( fname_, "rb" );
    else if( cscompare( mode_, "write" ) )
        filePointer_ = gzopen( fname_ , "wb" );
    else if( cscompare( mode_, "append" ) )
        filePointer_ = gzopen( fname_ , "ab" );
    else {
        fprintf(stdout,"ERROR: invalid mode! [%s].\n",mode_);
        return CVSOLVER_IO_ERROR;
    }

    //delete [] fname;
    //delete [] imode;

    if (filePointer_ == Z_NULL) {

    	//temporary way to avoid print out ERROR when trying to open restart.latest.*
    	std::string nametemp(fname_);
        int pos = 0;
        if ( (pos = nametemp.find( "latest")) == std::string::npos) {
        	fprintf(stderr,"ERROR opening file [%s].\n",fname_);
        }
        //fprintf(stderr,"Warnning: can not open file [%s].\n",fname_);
        return CVSOLVER_IO_ERROR;
    }

    //fprintf(stdout,"fname_ : %s\n",fname_);
    //fprintf(stdout,"mode_ : %s\n",mode_);

    return CVSOLVER_IO_OK;    

}

int cvsolverIO::closeFile() {
    if (cscompare(mode_, "write") || cscompare(mode_,"append")) {
      gzflush(filePointer_,Z_FULL_FLUSH);
    }
    gzclose(filePointer_);
    return CVSOLVER_IO_OK;
}

int cvsolverIO::rewindFile() {
     gzrewind(filePointer_);
     gzclearerr(filePointer_);
     return CVSOLVER_IO_OK;
}


//
//  READ functions
//

int cvsolverIO::readHeader (const char* keyphrase,int* valueArray,
                          int  nItems,const char*  datatype,
                          const char*  iotype) {

   int i,skip_size,integer_value;
   int rewinded = 0;

   isBinary( iotype );

   LastHeaderKey_[0] = '\0';

   while (rewinded < 2) {
     while (gzgets(filePointer_, Line_, 1024) != Z_NULL) {
         
         // ignore comment lines
         if (Line_[0] == '#') { 
             //fprintf(stdout,"COMMENT: %s",Line_);
             continue;
         }

         // ignore blank lines
         
         //char* chkblank = StringStripper(Line_);
         //fprintf(stdout,"chkblank: %i\n",strlen(chkblank));
         if (strlen(Line_) <= 1) {
             //fprintf(stdout,"Skipping blank line?\n");
             continue;
         }
         //fprintf(stdout,"Line_: %s",Line_);
         char* token = strtok ( Line_, ":" );
         //fprintf(stdout,"token: %s\n",token);
         if( cscompare( keyphrase , token ) ) {
            LastHeaderKey_[0] = '\0';
            sprintf(LastHeaderKey_,"%s",keyphrase); 
            token = strtok( NULL, " ,;<>" );
            skip_size = 0;
            skip_size = atoi( token );
            for( i=0;i < nItems && ( token = strtok( NULL," ,;<>") );i++) {
                valueArray[i] = atoi(token);
                //fprintf(stdout,"integer (%i): %i\n",i,atoi(token));
            }
            if ( i < nItems ) {
                fprintf(stderr,"Expected # of ints not recoverd from head\n");
                fprintf(stderr,"when looking for : %s\n", keyphrase);
                return CVSOLVER_IO_ERROR;
            } else {
                return CVSOLVER_IO_OK;
            }
         }
         // this really belongs when you open the file!
         if ( cscompare(token,"byteorder magic number") ) {
           if ( binary_format_ ) {
               gzread(filePointer_,&integer_value,sizeof(int));
               char junk;
               gzread(filePointer_,&junk,sizeof(char)); /* reading the new line */
           } else {
	       gzgets(filePointer_,Line_,1024);
               sscanf(Line_,"%i",&integer_value);
           }
           if ( FLOWSOLVER_MAGIC_NUMBER != integer_value ) {
               //fprintf(stdout,"NOTE: Wrong_Endian found.\n");
             Wrong_Endian_ = true;
           }
           continue;
         }

         // skip to next header
         token = strtok( NULL, " ,;<>" );
         skip_size = atoi( token );
         if ( binary_format_ ) {
             gzseek(filePointer_,skip_size,SEEK_CUR);
         } else {
            for( int gama=0; gama < skip_size; gama++ ) {
              gzgets(filePointer_,Line_,1024);
            }
         }

     } // end inner while

     gzrewind(filePointer_);
     gzclearerr(filePointer_);
     rewinded++;

   }  // end outer while

   return CVSOLVER_IO_ERROR;

}


int cvsolverIO::readDataBlock ( const char* keyphrase,
                              void* valueArray,
                              int nItems,
                              const char*  datatype,
                              const char*  iotype ) {

    int n;
    int* valueArrayInt;	
    float* valueArrayFloat;
    double* valueArrayDouble;
    char junk;
   
    // check that the file has been opened
    if (filePointer_ == Z_NULL) {
        fprintf(stderr,"No file associated with Descriptor \n");
        fprintf(stderr,"openfile_ function has to be called before \n");
        fprintf(stderr,"acessing the file\n");
        return CVSOLVER_IO_ERROR;
    }
    
    if ( LastHeaderNotFound_ ) {
       fprintf(stderr,"ERROR:  last header was not found.\n");
       return CVSOLVER_IO_ERROR;
    }

    // error check..
    // since we require that a consistant header always preceed the data block
    // let us check to see that it is actually the case.    

    if ( ! cscompare( LastHeaderKey_, keyphrase ) ) {
        fprintf(stderr,"ERROR: header not consistant with data block\n");
        fprintf(stderr,"Header: %s\n", LastHeaderKey_);
        fprintf(stderr,"DataBlock: %s\n", keyphrase);
        fprintf(stderr,"Please recheck read sequence\n");
        return CVSOLVER_IO_ERROR;
    }

    size_t type_size = typeSize( datatype );

    if (type_size == 0) {
        fprintf(stderr,"ERROR: Requested data type invalid!! \n");
        fprintf(stderr,"Header: %s\n", LastHeaderKey_);
        fprintf(stderr,"DataBlock: %s\n", keyphrase);
        fprintf(stderr,"Please recheck read sequence\n");
        return CVSOLVER_IO_ERROR;
    }
    
    isBinary( iotype );
    
    if ( binary_format_ ) {
        
        //fread(valueArray,type_size,nItems,filePointer_ );
        gzread(filePointer_,valueArray,type_size*nItems);
        //fread(&junk, sizeof(char), 1, filePointer_);
        gzread(filePointer_,&junk,sizeof(char)); /* reading the new line */
        if ( Wrong_Endian_ ) SwapArrayByteOrder( valueArray, type_size, nItems );
        
    } else { 
        char junk;
        switch( type_of_data_ ) {
        case INT:
            valueArrayInt  = static_cast<int*>( valueArray );
            for( n=0; n < nItems ; n++ ) {
               int integer_value;
               Line_[0] = '\0';
               gzgets(filePointer_,Line_,1024);
               if (sscanf(Line_,"%i",&integer_value) != 1) {
                   return CVSOLVER_IO_ERROR;
               }
               valueArrayInt[n] = integer_value;
            }	
            break;
        case FLOAT:
            valueArrayFloat  = static_cast<float*>( valueArray );
            for( n=0; n < nItems ; n++ ) {
               float float_value;
               Line_[0] = '\0';
               gzgets(filePointer_,Line_,1024);
               if (sscanf(Line_,"%f",&float_value) != 1) {
                   return CVSOLVER_IO_ERROR;
               }
               valueArrayFloat[n] = float_value;
            }
            break;
        case DOUBLE:
            valueArrayDouble  = static_cast<double*>( valueArray );
            for( n=0; n < nItems ; n++ ) {
               double double_value;
               Line_[0] = '\0';
               gzgets(filePointer_,Line_,1024);
               if (sscanf(Line_,"%lf",&double_value) != 1) {
                   return CVSOLVER_IO_ERROR;
               }
               valueArrayDouble[n] = double_value;
            }
            break;
        }
    }
    
    return CVSOLVER_IO_OK;
}

int cvsolverIO::readString(char* line) {
  while (gzgets(filePointer_, line, 1024) != Z_NULL) {
        return CVSOLVER_IO_OK;
    }
    return CVSOLVER_IO_ERROR;
}

//
//  WRITE functions
//

int cvsolverIO::writeString(const char* string) {
    gzprintf(filePointer_,"%s",string);
    return CVSOLVER_IO_OK;
}

int cvsolverIO::writeHeader (const char* keyphrase,
                           void* valueArray,
                           int nItems,
                           int ndataItems,
                           const char* datatype,
                           const char* iotype  ) {

    int* valueListInt;
   
    // check that the file has been opened
    if (filePointer_ == Z_NULL) {
        fprintf(stderr,"No file associated with Descriptor \n");
        fprintf(stderr,"openfile_ function has to be called before \n");
        fprintf(stderr,"acessing the file\n");
        return CVSOLVER_IO_ERROR;
    }

    LastHeaderKey_[0] = '\0';
    sprintf(LastHeaderKey_,"%s",keyphrase);

    DataSize_ = ndataItems;

    isBinary( iotype );
    size_t type_size = typeSize( datatype );

    if (type_size == 0) {
        fprintf(stderr,"ERROR: Requested data type invalid!! \n");
        fprintf(stderr,"Header: %s\n", LastHeaderKey_);
        fprintf(stderr,"DataBlock: %s\n", keyphrase);
        fprintf(stderr,"Please recheck write sequence\n");
        return CVSOLVER_IO_ERROR; 
    }

    int size_of_nextblock = 
        ( binary_format_ ) ? type_size*( ndataItems )+sizeof( char ) : ndataItems ;
    
    gzprintf(filePointer_,"%s",keyphrase);
    gzprintf(filePointer_," : < %i > ",size_of_nextblock);
    if( nItems > 0 ) {
        valueListInt = static_cast< int* > ( valueArray );
        for( int i = 0; i < nItems; i++ )
            gzprintf(filePointer_,"%i ",valueListInt [i]);
    }
    gzprintf(filePointer_,"\n");
    
    return CVSOLVER_IO_OK ;
}


int cvsolverIO::writeDataBlock (const char* keyphrase,
                              void* valueArray,
                              int nItems,
                              const char* datatype,
                              const char* iotype ) {
    
    int n;

    // check that the file has been opened
    if (filePointer_ == Z_NULL) {
        fprintf(stderr,"No file associated with Descriptor \n");
        fprintf(stderr,"openfile_ function has to be called before \n");
        fprintf(stderr,"acessing the file\n");
        return CVSOLVER_IO_ERROR;
    }

    // error check..
    // since we require that a consistant header always preceed the data block
    // let us check to see that it is actually the case.    

    if ( ! cscompare( LastHeaderKey_, keyphrase ) ) {
        fprintf(stderr,"ERROR: header not consistant with data block\n");
        fprintf(stderr,"Header: %s\n", LastHeaderKey_);
        fprintf(stderr,"DataBlock: %s\n", keyphrase);
        fprintf(stderr,"Please recheck read sequence\n");
        return CVSOLVER_IO_ERROR;
    }

    int* valueArrayInt;	
    float* valueArrayFloat;
    double* valueArrayDouble;
   
    int header_type = type_of_data_;
    size_t type_size=typeSize( datatype );

    if (type_size == 0) {
        fprintf(stderr,"ERROR: Requested data type invalid!! \n");
        fprintf(stderr,"Header: %s\n", LastHeaderKey_);
        fprintf(stderr,"DataBlock: %s\n", keyphrase);
        fprintf(stderr,"Please recheck write datablock sequence\n");
        return CVSOLVER_IO_ERROR; 
    }

    if ( header_type != type_of_data_ ) {
        fprintf(stderr, "header and datablock differ on typeof data in the block for \n");
        fprintf(stderr, "keyphrase : %s\n", keyphrase);
        fprintf(stderr, "returning\n");
        return CVSOLVER_IO_ERROR;
    }

    int nUnits = nItems;

    if ( nUnits != DataSize_ ) {
        fprintf(stderr, "header and datablock differ on the number of data items for\n");
        fprintf(stderr, "keyphrase : %s\n",keyphrase);
        fprintf(stderr, "returning\n");
        return CVSOLVER_IO_ERROR;
    }
 
    isBinary( iotype );

    if ( binary_format_ ) {
        
      //size_t fwrite(const void* ptr, size_t size, size_t nobj, FILE* stream); 
      //fwrite(static_cast< char* >( valueArray ),type_size, nUnits, filePointer_);
        gzwrite(filePointer_,static_cast< char* >( valueArray ),type_size*nUnits);
        gzprintf(filePointer_,"\n");
        
    } else { 
        
        switch( type_of_data_ ) {
        case INT:
            
            valueArrayInt  = static_cast<int*>( valueArray );
            for( n=0; n < nUnits ; n++ ) 
                gzprintf(filePointer_,"%i\n", valueArrayInt[n]);	
            break;

        case FLOAT:

            valueArrayFloat  = static_cast<float*>( valueArray );
            for( n=0; n < nUnits ; n++ ) 
                gzprintf(filePointer_,"%f\n", valueArrayFloat[n]);	
            break;

        case DOUBLE:

            valueArrayDouble  = static_cast<double*>( valueArray );
            for( n=0; n < nUnits ; n++ ) 
                gzprintf(filePointer_,"%lf\n", valueArrayDouble[n]);	
            break;
        }
    }	
    
    return CVSOLVER_IO_OK;
}


//
//  Helper functions
//

char* cvsolverIO::StringStripper ( const char*  istring ) {
        char* fname;
        int namelength = strcspn( istring, " " );
        fname = new char [ namelength+1 ];
        strncpy( fname, istring , namelength );
        fname [ namelength ] = '\0';
        return fname;
}

int cvsolverIO::cscompare( const char* s1, const char* s2) {
        while( *s1 == ' ') s1++;
        while( *s2 == ' ') s2++;
        while( ( *s1 ) 
               && ( *s2 ) 
               && ( *s2 != '?')
               && ( tolower( *s1 )==tolower( *s2 ) ) ) {
            s1++;
            s2++;
            while( *s1 == ' ') s1++;
            while( *s2 == ' ') s2++;
        }
        if ( !( *s1 ) || ( *s1 == '?') ) return 1;
        else return 0;
}

void cvsolverIO::isBinary( const char* iotype ) {

        char* fname = StringStripper( iotype );
        if ( cscompare( fname, "binary" ) ) binary_format_ = true;
        else binary_format_ = false;
        delete [] fname;

}

size_t cvsolverIO::typeSize( const char* typestring ) {
        char* ts1 = StringStripper( typestring );
        if ( cscompare( ts1, "integer" ) ) {
            type_of_data_ = INT;
            delete [] ts1;
            return sizeof(int);
        } else if ( cscompare( ts1, "float" ) ) {
            type_of_data_ = FLOAT;
            delete [] ts1;
            return sizeof( float ); 
        } else if ( cscompare( ts1, "double" ) ) { 
            type_of_data_ = DOUBLE;
            delete [] ts1;
            return sizeof( double );
        } else { 
            delete [] ts1;
            fprintf(stderr,"unknown type (%s)\n",typestring);
            return 0;
        }
}

void cvsolverIO::SwapArrayByteOrder( void* array, int nbytes, int nItems ) {
        /* This swaps the byte order for the array of nItems each
           of size nbytes , This will be called only locally  */
        int i,j;
        unsigned char ucTmp;
        unsigned char* ucDst = (unsigned char*)array;
        
        for(i=0; i < nItems; i++) {
            for(j=0; j < (nbytes/2); j++)
                swap_char( ucDst[j] , ucDst[(nbytes - 1) - j] );
            ucDst += nbytes;
        }
}

int openfile_( const char* filename, 
                const char* mode,
                int*  fileDescriptor ) {

    int i;

    // hard code allowable number of open files to 2048
    if (cvsolverIOfp == NULL) {
        cvsolverIOfp = new cvsolverIO* [2048];
        for (i = 0; i < 2048; i++) {
            cvsolverIOfp[i] = NULL;
        }
    }
    // skip 0 so it can be returned as an error code
    for (i = 1; i < 2048; i++) {
      if (cvsolverIOfp[i] == NULL) {
        cvsolverIOfp[i] = new cvsolverIO();
        if (cvsolverIOfp[i]->openFile(filename, mode) == CVSOLVER_IO_ERROR) {
            delete cvsolverIOfp[i];
            cvsolverIOfp[i] = NULL;
            *fileDescriptor = 0;
            return CVSOLVER_IO_ERROR;
        }
        *fileDescriptor = i;
        //fprintf(stdout,"file pointer (%i) opened\n",(*fileDescriptor));
        return CVSOLVER_IO_OK;
      }
    }

    fprintf(stderr,"ERROR:  could not open file.\n");
    fprintf(stderr,"        maximum number of files exceeded.\n");
    exit(-1);
    return CVSOLVER_IO_ERROR;

}

void closefile_( int* fileDescriptor, 
                 const char* mode ) {
    cvsolverIOfp[(*fileDescriptor)]->closeFile();
    delete cvsolverIOfp[(*fileDescriptor)];
    cvsolverIOfp[(*fileDescriptor)] = NULL;
    //fprintf(stdout,"file pointer (%i) closed\n",(*fileDescriptor));
    return;
}

void readheader_( int* fileDescriptor,
                  const char* keyphrase,
                  void* valueArray,
                  int*  nItems,
                  const char*  datatype,
                  const char*  iotype ) {
    int num = *nItems;
    cvsolverIOfp[(*fileDescriptor)]->readHeader(keyphrase,(int*)valueArray,
                                            num,datatype,iotype);
    return;
}

void readdatablock_( int*  fileDescriptor,
                     const char* keyphrase,
                     void* valueArray,
                     int*  nItems,
                     const char*  datatype,
                     const char*  iotype ) {
    int num = *nItems;
    cvsolverIOfp[(*fileDescriptor)]->readDataBlock(keyphrase,
                       valueArray,num,datatype,iotype );
    return;
}

void writeheader_ (  int*  fileDescriptor,
                     const char* keyphrase,
                     void* valueArray,
                     int* nItems,
                     int* ndataItems,
                     const char* datatype,
                     const char* iotype  ) {

    int num = *nItems;
    int ndatanum = *ndataItems;
    cvsolverIOfp[(*fileDescriptor)]->writeHeader (keyphrase,
                     valueArray,num,ndatanum,datatype,iotype);
    return;
}

void writedatablock_( int* fileDescriptor,
                      const char* keyphrase,
                      void* valueArray,
                      int* nItems,
                      const char* datatype,
                      const char* iotype ) {
    int num = *nItems;
    cvsolverIOfp[(*fileDescriptor)]->writeDataBlock (keyphrase,
                             valueArray,num,datatype,iotype);
    return;
}

void writestring_( int* fileDescriptor,
                   const char* string ) {
    cvsolverIOfp[(*fileDescriptor)]->writeString(string);
}

#include <vector>
#include <string>
#include <iostream>
void Gather_Headers( int* fileDescriptor, std::vector< std::string >& headers ) {

    char Line[1024];
    cvsolverIOfp[(*fileDescriptor)]->rewindFile();
     while ( cvsolverIOfp[(*fileDescriptor)]->readString(Line) == CVSOLVER_IO_OK) {
        if ( Line[0] == '#' ) {
            headers.push_back( Line );
        } else { 
            break; 
        }
     }
    cvsolverIOfp[(*fileDescriptor)]->rewindFile();    
}

/*
int main (int argc, char **agrv) {

    int fd;
    openfile_("geombc.dat.0","read",&fd);
    int itwo=2;
    int* intfromfile = new int[3];
    readheader_(&fd,"co-ordinates?",intfromfile,&itwo,"double","binary?");
    int numnp=intfromfile[0];
    int nsd=intfromfile[1];
    double *xread = new double[numnp*nsd];
    int ixsiz=numnp*nsd;
    readdatablock_(&fd,"co-ordinates?",xread,&ixsiz,"double","binary?");
    return 0;
}
*/





