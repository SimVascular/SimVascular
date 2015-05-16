c Copyright (c) 2014-2015 The Regents of the University of California.
c All Rights Reserved.
c
c Portions of the code Copyright (c) 2009-2011 Open Source Medical
c Software Corporation, University of California, San Diego.
c
c Portions of the code Copyright (c) 2000-2007, Stanford University,
c     Rensselaer Polytechnic Institute, Kenneth E. Jansen,
c     Charles A. Taylor.
c
c  See SimVascular Acknowledgements file for additional
c  contributors to the source code.
c
c  Redistribution and use in source and binary forms, with or without
c  modification, are permitted provided that the following conditions
c  are met:
c
c  Redistributions of source code must retain the above copyright notice,
c  this list of conditions and the following disclaimer.
c  Redistributions in binary form must reproduce the above copyright
c  notice, this list of conditions and the following disclaimer in the
c  documentation and/or other materials provided with the distribution.
c  Neither the name of the Stanford University or Rensselaer Polytechnic
c  Institute nor the names of its contributors may be used to endorse or
c  promote products derived from this software without specific prior
c  written permission.
c
c  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
c  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
c  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
c  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
c  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
c  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
c  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
c  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
c  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
c  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
c  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
c  DAMAGE.
c
c----------------------------------------------------------------------
c
c This file contains the global settings and global parameter declarations
c needed for the headers/fortran files.
c
c----------------------------------------------------------------------
C
C      Variables are not implicitly declared
C
       IMPLICIT NONE

c      CONSTANTS

       INTEGER MAXBLK,MAXTS,MAXSH,MAXTOP,MAXSURF,MAXQPT,MACHFL,NSD,NITR
c
c.... parameters  IF YOU CHANGE THES YOU HAVE TO CHANGE THEM IN
c                  common_c.h ALSO
c
        parameter     ( MAXBLK = 5000, MAXTS = 100)
        parameter     ( MAXSH = 32, NSD = 3 )
        parameter (MAXQPT = 125)
        parameter     ( MAXTOP = 6, MAXSURF=199 )
c
c----------------------------------------------------------------------
c
c.... parameters        : machine data
c
c machin        : machine type
c                  (set parameter)
c machfl        : single precision floating point lenght in bytes
c                  (set parameter)
c
c----------------------------------------------------------------------
		CHARACTER*8     machin
		parameter     ( machin = 'RS/6000 ' )
		parameter     ( machfl = 4 )
c
c----------------------------------------------------------------------
c.... parameters        : useful constants
c
c zero          : 0.0
c pt125         : 0.125
c pt25          : 0.25
c pt33          : 0.33 (1/3)
c pt39          : 2^(-4/3)
c pt5           : 0.5
c pt57          : 1/sqrt(3)
c pt66          : 0.66 (2/3)
c pt75          : 0.75
c one           : 1.0
c sqt2          : sqrt(2)
c onept5        : 1.5
c two           : 2.0
c three         : 3.0
c four          : 4.0
c five          : 5.0
c pi            : the magical number :-)
c
c----------------------------------------------------------------------
c

        REAL*8 zero,pt25,pt125,pt33,pt39,pt5,pt57,pt66,pt75,sqt2,onept5
        REAL*8 one,two,three,four,five,pi
        parameter
     &           ( zero   = 0.0000000000000000000000000000000d0,
     &             pt125  = 0.1250000000000000000000000000000d0,
     &             pt25   = 0.2500000000000000000000000000000d0,
     &             pt33   = 0.3333333333333333333333333333333d0,
     &             pt39   = 0.3968502629920498686879264098181d0,
     &             pt5    = 0.5000000000000000000000000000000d0,
     &             pt57   = 0.5773502691896257645091487805020d0,
     &             pt66   = 0.6666666666666666666666666666667d0,
     &             pt75   = 0.7500000000000000000000000000000d0,
     &             one    = 1.0000000000000000000000000000000d0,
     &             sqt2   = 1.4142135623730950488016887242097d0,
     &             onept5 = 1.5000000000000000000000000000000d0,
     &             two    = 2.0000000000000000000000000000000d0,
     &             three  = 3.0000000000000000000000000000000d0,
     &             four   = 4.0000000000000000000000000000000d0,
     &             five   = 5.0000000000000000000000000000000d0,
     &             pi     = 3.1415926535897932384626433832795d0)

c
