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
c

#include "cvFlowsolverOptions.h"

!> This routine reads the boundary elements, reorders them and
!! generates traces for the gather/scatter operations.

        subroutine genbkb (ibksz)
c
        use dtnmod
        use pointer_data
c
        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/outpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/nomodule.h"

C     Argument variables
C
      INTEGER             ibksz
C
C     Local variables
C
      INTEGER             i,           ibcbtpsiz,   iblk,        ieight
      INTEGER             iel,         iibcbtpsiz,  iientpsiz,   ipordl
      INTEGER             itpblk     
      INTEGER             n,           n1,          n2
      INTEGER             neltp,       numnbc
C
        integer, allocatable :: ientp(:,:),iBCBtp(:,:)
        real*8, allocatable :: BCBtp(:,:)
        integer materb(ibksz)
        integer intfromfile(50) ! integers read from headers
        character*255 fname1
        iel=1
        itpblk=nelblb
        nelblb=0
        mattyp=0
        ndofl = ndof

        do iblk = 1, itpblk
           ieight=8
           fname1='connectivity boundary?'
           call readheader(igeom,fname1,intfromfile,ieight,
     &                     'integer'//CHAR(0),iotype)
           neltp =intfromfile(1)
           nenl  =intfromfile(2)
           ipordl=intfromfile(3)
           nshl  =intfromfile(4)
           nshlb =intfromfile(5)
           nenbl =intfromfile(6)
           lcsyst=intfromfile(7)
           numnbc=intfromfile(8)
c
           allocate (ientp(neltp,nshl))
           allocate (iBCBtp(neltp,ndiBCB))
           allocate (BCBtp(neltp,ndBCB))
           iientpsiz=neltp*nshl
           call readdatablock(igeom,fname1,ientp,iientpsiz,
     &                     'integer'//CHAR(0),iotype)
c     
c.... Read the boundary flux codes
c     
           fname1='nbc codes?'
           call readheader(igeom,fname1,intfromfile,ieight,
     &                     'integer'//CHAR(0),iotype)
           iiBCBtpsiz=neltp*ndiBCB
           call readdatablock(igeom,fname1,iBCBtp,iiBCBtpsiz,
     &                     'integer'//CHAR(0),iotype)
c     
c.... read the boundary condition data
c     
           fname1='nbc values?'
           call readheader(igeom,fname1,intfromfile,ieight,
     &                     'integer'//CHAR(0),iotype)
           BCBtp    = zero
           iBCBtpsiz=neltp*ndBCB
           call readdatablock(igeom,fname1,BCBtp,iBCBtpsiz,
     &                     'double'//CHAR(0),iotype)
c
c This is a temporary fix until NSpre properly zeros this array where it
c is not set.  DEC has indigestion with these arrays though the
c result is never used (never effects solution).
c
           where(.not.btest(iBCBtp(:,1),0)) BCBtp(:,1)=zero
           where(.not.btest(iBCBtp(:,1),1)) BCBtp(:,2)=zero
           where(.not.btest(iBCBtp(:,1),3)) BCBtp(:,6)=zero
           if(ndBCB.gt.6) then
           do i=6,ndof
           where(.not.btest(iBCBtp(:,1),i-1)) BCBtp(:,i+1)=zero
           enddo
         endif
           where(.not.btest(iBCBtp(:,1),2)) 
              BCBtp(:,3)=zero
              BCBtp(:,4)=zero
              BCBtp(:,5)=zero
           endwhere


           do n=1,neltp,ibksz 
              nelblb=nelblb+1
              npro= min(IBKSZ, neltp - n + 1)
c
              lcblkb(1,nelblb)  = iel
c              lcblkb(2,nelblb)  = iopen ! available for later use
              lcblkb(3,nelblb)  = lcsyst
              lcblkb(4,nelblb)  = ipordl
              lcblkb(5,nelblb)  = nenl
              lcblkb(6,nelblb)  = nenbl
              lcblkb(7,nelblb)  = mattyp
              lcblkb(8,nelblb)  = ndofl
              lcblkb(9,nelblb)  = nshl 
              lcblkb(10,nelblb) = nshlb ! # of shape functions per elt
c
c.... save the element block
c
              n1=n
              n2=n+npro-1
              materb=1   ! all one material for now
c
c.... allocate memory for stack arrays
c

              allocate (mienb(nelblb)%p(npro,nshl))
c
              allocate (miBCB(nelblb)%p(npro,ndiBCB))
c
              allocate (mBCB(nelblb)%p(npro,nshlb,ndBCB))
c
              allocate (mmatb(nelblb)%p(npro))

#if(VER_VARWALL == 1)
              IF (ivarwallprop .eq. 1) THEN
                allocate (wallpropelem(nelblb)%p(npro,2))
              ENDIF
#endif
c
c.... save the boundary element block
c
              call gensvb (ientp(n1:n2,1:nshl),
     &                 iBCBtp(n1:n2,:),      BCBtp(n1:n2,:),
     &                 materb,        mienb(nelblb)%p,
     &                 miBCB(nelblb)%p,        mBCB(nelblb)%p,
     &                 mmatb(nelblb)%p)
c
              iel=iel+npro
           enddo
           deallocate(ientp)
           deallocate(iBCBtp)
           deallocate(BCBtp)
        enddo
        lcblkb(1,nelblb+1) = iel

c
c.... return
c
        return
c
c.... end of file error handling
c
 911    call error ('genbcb  ','end file',igeom)
c
1000    format(a80,//,
     &  ' B o u n d a r y   E l e m e n t   C o n n e c t i v i t y',//,
     &  '   Elem   BC codes',/,
     &  '  Number  C P V H ',5x,27('Node',i1,:,2x))
1100    format(2x,i5,2x,4i2,3x,27i7)
c$$$2000    format(a80,//,
c$$$     &  ' B o u n d a r y   E l e m e n t   B C   D a t a ',//,
c$$$     &  '   Node   ',3x,'mass',/,
c$$$     &  '  Number  ',3x,'flux',6x,'Pressure',6x,'Heat',6x,
c$$$     &  3('Viscous',i1,:,4x))
2100    format(2x,i5,1p,1x,6e12.4)
c
        end




