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

#ifndef CVSOLVERIO_H
#define CVSOLVERIO_H

#define CVSOLVER_IO_OK 1
#define CVSOLVER_IO_ERROR 0

#include <stdio.h>

#include "cvFlowsolverOptions.h"

#ifdef USE_ZLIB
   #include "simvascular_zlib.h"
#else
   #include <stdlib.h>
   #define gzopen fopen
   #define gzprintf fprintf
   #define gzFile FILE*
   #define gzclose fclose
   #define gzeof feof
   #define gzclearerr clearerr
   #define gzflush(p1,p2) fflush((p1))
   #define gzrewind rewind
   #define gzread(p1,p2,p3) fread((p2),(p3),1,(p1))
   #define gzwrite(p1,p2,p3) fwrite((p2),(p3),1,(p1))
   #define gzseek fseek
   #define gzgets(p1,p2,p3) fgets((p2),(p3),(p1))
   #define Z_NULL NULL
#endif

#ifdef CV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE

#define openfile_ OPENFILE
#define closefile_ CLOSEFILE
#define readheader_ READHEADER
#define readdatablock_ READDATABLOCK
#define writeheader_ WRITEHEADER
#define writedatablock_ WRITEDATABLOCK
#define writestring_ WRITESTRING

#endif

#ifdef __cplusplus

class cvsolverIO {

public:
    
    cvsolverIO();
    ~cvsolverIO();
    
    // file control
    int openFile (const char* filename, const char *mode);
    int closeFile ();
    int rewindFile();
    
    // read info
    int readHeader (const char* keyphrase,
                  int* valueArray,
                  int   nItems,
                  const char*  datatype,
                    const char*  iotype);
    int readDataBlock (const char* keyphrase,
                       void* valueArray,
                       int  nItems,
                       const char*  datatype,
                       const char*  iotype );
    int readString(char* line);

    // write info
    int writeHeader (const char* keyphrase,
                     void* valueArray,
                     int nItems,
                     int ndataItems,
                     const char* datatype,
                     const char* iotype  );

    int writeDataBlock ( const char* keyphrase,
                         void* valueArray,
                         int nItems,
                         const char* datatype,
                         const char* iotype );
    
    int writeString (const char* string );

    // helper functions
    char* StringStripper ( const char*  istring );
    int cscompare ( const char* s1, const char* s2);
    void isBinary ( const char* iotype );
    size_t typeSize ( const char* typestring );
    void SwapArrayByteOrder ( void* array, int nbytes, int nItems );
    
private:

    gzFile filePointer_;

    bool byte_order_;
    int type_of_data_  ;
    int DataSize_  ;

    char LastHeaderKey_[1024];
    bool LastHeaderNotFound_;
    
    bool Wrong_Endian_;
    bool binary_format_;
    
    char *mode_;
    char *fname_;
    char Line_[1024];

};

#endif

#ifdef __cplusplus
extern "C" {
#endif
    
int openfile_( const char* filename, 
                const char* mode,
                int*  fileDescriptor );

void closefile_( int* fileDescriptor, 
                 const char* mode );

void readheader_( int* fileDescriptor,
                  const char* keyphrase,
                  void* valueArray,
                  int*  nItems,
                  const char*  datatype,
                  const char*  iotype );

void
readdatablock_( int*  fileDescriptor,
                const char* keyphrase,
                void* valueArray,
                int*  nItems,
                const char*  datatype,
                const char*  iotype );

void 
writeheader_ (  int*  fileDescriptor,
                const char* keyphrase,
                void* valueArray,
                int* nItems,
                int* ndataItems,
                const char* datatype,
                const char* iotype  );

void 
writedatablock_( int* fileDescriptor,
                 const char* keyphrase,
                 void* valueArray,
                 int* nItems,
                 const char* datatype,
                 const char* iotype );

void 
writestring_( int* fileDescriptor,
              const char* string );

#ifdef __cplusplus
}
#endif

#endif






