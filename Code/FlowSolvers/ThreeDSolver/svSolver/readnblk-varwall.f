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

#include "cvFlowsolverOptions.h"

!> Subroutine readnblk ("Reed and Block") -- allocates space for
!! and reads data to be contained in module readarrays.  Reads
!! all remaining data and blocks them with pointers.

      subroutine readnblk
c
      use readarrays

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/matdat.h"
        include "common_blocks/melmcat.h"
        include "common_blocks/mio.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/outpar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
C
C     Local variables
C
      INTEGER             i,           iacsiz,      ibcinpsiz
      INTEGER             ieleven,     ione,        iqsiz,       irstart
      INTEGER             ithree,      itmp,        itwo,        iusiz
      INTEGER             ixsiz,       ndisp,       ndof2
      INTEGER             nsecondrank, nshg2
C
      real*8, allocatable :: xread(:,:), qread(:,:), acread(:,:)
      real*8, allocatable :: uread(:,:)
      real*8, allocatable :: BCinpread(:,:)
      integer, allocatable :: iperread(:), iBCtmpread(:)
      integer, allocatable :: ilworkread(:), nBCread(:)
      character*5 cname
      character*8 mach2
      character*20 fmt1
      character*255 fname1,fnamer,fnamelr
#if(VER_VARWALL == 1)
      character*255 fname0
      integer nwallprop
#endif
      character*255 warning
c     CAREFUL IGEOM,IBNDC,IRSTIN REDECLARED IN VARWALL
      integer irstin0, ierr
      integer intfromfile(50) ! integers read from headers

      integer use_restart
c
c
c.... determine the step number to start with
c
      open(unit=72,file='numstart.dat',status='old')
      read(72,*) irstart
      close(72)
c
      fname1='geombc.dat'
      fname1= trim(fname1)  // cname(myrank+1)
      fnamelr='restart.latest'

      itmp=1
      if (irstart .gt. 0) itmp = int(log10(float(irstart)))+1
      write (fmt1,"('(''restart.'',i',i1,',1x)')") itmp
      write (fnamer,fmt1) irstart
      fnamer = trim(fnamer) // cname(myrank+1)
      fnamelr = trim(fnamelr) // cname(myrank+1)

c
c.... open input files
c
      call openfile(  fname1,  'read?'//CHAR(0), igeom )
c
c.... try opening restart.latest.proc before trying restart.stepno.proc
c
      call openfile(  fnamelr,  'read'//CHAR(0), irstin )
      if ( irstin .eq. 0 )
     &   call openfile( fnamer, 'read'//CHAR(0), irstin )
! either one will work
c
c.... input the geometry parameters
c

      ieleven=11
      ione=1
      fname1='number of nodes?'
      call readheader(igeom,fname1,numnp,ione,'integer'//CHAR(0), iotype)
      fname1='number of modes?'
      call readheader(igeom,fname1,nshg,ione,'integer'//CHAR(0), iotype)
      fname1='number of interior elements?'
      call readheader(igeom,fname1,numel,ione,'integer'//CHAR(0), iotype)
      fname1='number of boundary elements?'
      call readheader(igeom,fname1,numelb,ione,'integer'//CHAR(0), iotype)
      fname1='maximum number of element nodes?'
      call readheader(igeom,fname1,nen,ione,'integer'//CHAR(0), iotype)
      fname1='number of interior tpblocks?'
      call readheader(igeom,fname1,nelblk,ione,'integer'//CHAR(0), iotype)
      fname1='number of boundary tpblocks?'
      call readheader(igeom,fname1,nelblb,ione,'integer'//CHAR(0), iotype)
      fname1='number of nodes with Dirichlet BCs?'
      call readheader(igeom,fname1,numpbc,ione,'integer'//CHAR(0), iotype)
      fname1='number of shape functions?'
      call readheader(igeom,fname1,ntopsh,ione,'integer'//CHAR(0), iotype)
c
c.... calculate the maximum number of boundary element nodes
c     
      nenb = 0
      do i = 1, melCat
         if (nen .eq. nenCat(i,nsd)) nenb = max(nenCat(i,nsd-1), nenb)
      enddo
c     
      if (myrank == master) then
         if (nenb .eq. 0) call error ('input   ','nen     ',nen)
      endif
c
c.... setup some useful constants
c
      I3nsd  = nsd / 3          ! nsd=3 integer flag
      E3nsd  = float(I3nsd)     ! nsd=3 real    flag
c    
      if(matflg(1,1).lt.0) then
         nflow = nsd + 1
      else
         nflow = nsd + 2
      endif 
      ndof   = nsd + 2
      nsclr=impl(1)/100
      ndof=ndof+nsclr           ! number of sclr transport equations to solve
      
      ndofBC = ndof + I3nsd     ! dimension of BC array
      ndiBCB = 2                ! dimension of iBCB array
      ndBCB  = ndof + 1         ! dimension of BCB array
c     
      nsymdf = (ndof*(ndof + 1)) / 2 ! symm. d.o.f.'s
c
c.... ----------------------> Communication tasks <--------------------
c
      if(numpe > 1) then

         fname1='size of ilwork array?'
         call readheader(igeom,fname1,nlwork,ione,'integer'//CHAR(0), iotype)

         ione=1
         fname1='ilwork?'
         call readheader(igeom,fname1,nlwork,ione,'integer'//CHAR(0), iotype)

         allocate( point2ilwork(nlwork) )
         allocate( ilworkread(nlwork) )
         call readdatablock(igeom,fname1,ilworkread,
     &                      nlwork,'integer'//CHAR(0), iotype)
         point2ilwork = ilworkread
         call ctypes (point2ilwork)
      else
           nlwork=1
           allocate( point2ilwork(1))
      endif
c     
c.... read the node coordinates
c
      itwo=2
      fname1='co-ordinates?'
      call readheader(igeom,fname1,intfromfile,itwo, 'double'//CHAR(0), iotype)
      numnp=intfromfile(1)
c      nsd=intfromfile(2)
      allocate( point2x(numnp,nsd) )
      allocate( xread(numnp,nsd) )
      ixsiz=numnp*nsd
      call readdatablock(igeom,fname1,xread,ixsiz, 'double'//CHAR(0),iotype)
      point2x = xread
c
c.... read the local to global node id mapping
c
      if(numpe>1) then
          ione=1
          itwo=2
          fname1='mode number map from partition to global?'
          call readheader(igeom,fname1,intfromfile,ione, 'integer'//CHAR(0), iotype)
          numnp=intfromfile(1)
          allocate(l2g(numnp))
          call readdatablock(igeom,fname1,l2g,numnp, 'integer'//CHAR(0),iotype)
      else
        allocate(l2g(numnp))
        do i=1,numnp
           l2g(i)=i
        enddo
      endif

c
c.... read in and block out the connectivity
c
      call genblk (IBKSIZ)
c
c.... read the boundary condition mapping array
c
      ione=1
      fname1='bc mapping array?'
      call readheader(igeom,fname1,nshg,
     &     ione,'integer'//CHAR(0), iotype)
      allocate( nBC(nshg) )

      allocate( nBCread(nshg) )
      call readdatablock(igeom,fname1,nBCread,nshg,'integer'//CHAR(0),iotype)
      nBC=nBCread
c
c.... read the temporary iBC array
c
      ione = 1
      fname1='bc codes array?'
      call readheader(igeom,fname1,numpbc,
     &     ione, 'integer'//CHAR(0), iotype)
      if ( numpbc > 0 ) then
         allocate( iBCtmp(numpbc) )
         allocate( iBCtmpread(numpbc) )
         call readdatablock(igeom,fname1,iBCtmpread,numpbc,
     &                      'integer'//CHAR(0),iotype)
         iBCtmp=iBCtmpread
      else  ! sometimes a partition has no BC's
         allocate( iBCtmp(1) )
         iBCtmp=0
      endif
c
c.... read boundary condition data
c
      ione=1
      fname1='boundary condition array?'
      call readheader(igeom,fname1,intfromfile,
     &     ione, 'integer'//CHAR(0), iotype)
c here intfromfile(1) contains (ndof+7)*numpbc
      if ( numpbc > 0 ) then
         if(intfromfile(1).ne.(ndof+7)*numpbc) then
           warning='WARNING more data in BCinp than needed: keeping 1st'
           write(*,*) warning, ndof+7
         endif
         allocate( BCinp(numpbc,ndof+7) )
         nsecondrank=intfromfile(1)/numpbc
         allocate( BCinpread(numpbc,nsecondrank) )
         iBCinpsiz=intfromfile(1)
         call readdatablock(igeom,fname1,BCinpread,iBCinpsiz,
     &                      'double'//CHAR(0),iotype)
         BCinp(:,1:(ndof+7))=BCinpread(:,1:(ndof+7))
      else  ! sometimes a partition has no BC's
         allocate( BCinp(1,ndof+7) )
         BCinp=0
      endif
c
c.... read periodic boundary conditions
c
      fname1='periodic masters array?'
      call readheader(igeom,fname1,nshg,
     &     ione, 'integer'//CHAR(0), iotype)
      allocate( point2iper(nshg) )
      allocate( iperread(nshg) )
      call readdatablock(igeom,fname1,iperread,nshg,
     &                      'integer'//CHAR(0),iotype)
      point2iper=iperread
c
c.... generate the boundary element blocks
c
      call genbkb (ibksiz)

#if(VER_VARWALL == 1)
c.... read the values of wall property variables from geombc into wallpropg
c
      if ((ideformwall.eq.1) .and. (ivarwallprop.eq.1)) then

          use_restart=1

          itwo=2
          fname1='varwallprop?'
          intfromfile = 0
          call readheader(igeom,fname1,intfromfile,itwo, 'double'//CHAR(0), iotype)

          if(intfromfile(1).gt.0) then
              use_restart=0
              numnp=intfromfile(1)
              nwallprop=intfromfile(2)
              if(nwallprop.ne.2) then
                 warning ='WARNING number of properties not equal 2'
                 write(*,*) warning
                 if(nwallprop.gt.3) then
                   PRINT *,'ERROR: Variable Wall Properties Component Number greater than 3 in geombc.dat.proc'
                   stop
                 endif
              endif

              allocate( wallpropg(numnp,nwallprop) )
              ixsiz=numnp*nwallprop
              wallpropg = 0.0D0
              call readdatablock(igeom,fname1,xread(:,1:nwallprop),ixsiz, 'double'//CHAR(0),iotype)
              wallpropg = xread(:,1:nwallprop)
          endif
      endif
#endif
c
c.... Read restart files
c
c.... read the header and check it against the run data
c

      ithree=3
c      call creadlist(irstin,ithree,nshg2,ndof2,lstep)
      fname1='solution?'
      call readheader(irstin,fname1,intfromfile,
     &     ithree,'integer'//CHAR(0), iotype)
      nshg2=intfromfile(1)
      ndof2=intfromfile(2)
      lstep=intfromfile(3)
      if(ndof2.ne.ndof) then
        warning='WARNING more data in restart than needed: keeping 1st '
        write(*,*) warning , ndof
      endif
c
      if (nshg2 .ne. nshg) 
     &     call error ('restar  ', 'nshg   ', nshg)
c
c.... read the values of primitive variables into q
c
      allocate( qold(nshg,ndof) )
      allocate( qread(nshg,ndof2) )

      iqsiz=nshg*ndof2
      call readdatablock(irstin,fname1,qread,iqsiz,
     &                      'double'//CHAR(0),iotype)
      qold(:,1:ndof)=qread(:,1:ndof)
c 
      fname1='time derivative of solution?'
      intfromfile=0
      call readheader(irstin,fname1,intfromfile,
     &     ithree,'integer'//CHAR(0), iotype)
      allocate( acold(nshg,ndof) )
      if(intfromfile(1).ne.0) then 
         nshg2=intfromfile(1)
         ndof2=intfromfile(2)
         lstep=intfromfile(3)
         
         if (nshg2 .ne. nshg) 
     &        call error ('restar  ', 'nshg   ', nshg)
c     
         allocate( acread(nshg,ndof2) )
         acread=zero

         iacsiz=nshg*ndof2
         call readdatablock(irstin,fname1,acread,iacsiz,
     &                   'double'//CHAR(0),iotype)
         acold(:,1:ndof)=acread(:,1:ndof)
         deallocate(acread)
      else
         if(myrank.eq.master) then
           write(*,*) ''
           warning='Time derivative of solution is set to zero (SAFE)'
           write(*,*) warning
         endif
         acold=zero
      endif

c      call creadlist(irstin,ithree,nshg2,ndisp,lstep)

      if (ideformwall.eq.1) then
         fname1='displacement?'
         call readheader(irstin,fname1,intfromfile,
     &        ithree,'integer'//CHAR(0), iotype)
         nshg2=intfromfile(1)
         ndisp=intfromfile(2)
         lstep=intfromfile(3)
         if(ndisp.ne.nsd) then
            warning='WARNING ndisp not equal nsd'
            write(*,*) warning , ndisp
         endif
c
         if (nshg2 .ne. nshg) 
     &        call error ('restar  ', 'nshg   ', nshg)
c
c.... read the values of primitive variables into uold
c
         allocate( uold(nshg,nsd) )
         allocate( uread(nshg,nsd) )
         
         iusiz=nshg*nsd
         call readdatablock(irstin,fname1,uread,iusiz,
     &        'double'//CHAR(0),iotype)
         uold(:,1:nsd)=uread(:,1:nsd)
       else
         allocate( uold(nshg,nsd) )
         uold(:,1:nsd) = zero
       endif

c
#if(VER_VARWALL == 1)
c.... read the values of wall property variables from restart into wallpropg
c
       if ((ideformwall.eq.1) .and. (ivarwallprop.eq.1) .and.
     &   (use_restart.eq.1)) then

         fname0='restart.0'
         fname0 = trim(fname0) // cname(myrank+1)
c
c.... open input files at t=0 to get varwallprop
c.... varwallprop is stored in restart.0.proc only
c
         call openfile( fname0,'read?'//CHAR(0), irstin0 );
         fname1='varwallprop?'

         intfromfile = 0
         call readheader(irstin0,fname1,intfromfile,
     &        ithree,'integer'//CHAR(0), iotype)

        if (intfromfile(1).gt.0) then
           nshg2 = intfromfile(1)
           nwallprop = intfromfile(2)
c          lstep=intfromfile(3) is not necessary since this is step 0
c          not real lstep
           if(nwallprop.ne.2) then
             warning ='WARNING number of properties not equal 2'
             write(*,*) warning
             if(nwallprop.gt.3) then
               PRINT *,'ERROR: Variable Wall Properties Component Number greater than 3 in restart.0.proc'
               stop
             endif
           endif

           if (nshg2 .ne. nshg)
     &        call error ('restar wallprop ', 'nshg   ', nshg)

           allocate( wallpropg(nshg,nwallprop) )
           iusiz=nshg*nwallprop
           wallpropg = 0.0D0
           call readdatablock(irstin0,fname1,uread(:,1:nwallprop),iusiz,
     &        'double'//CHAR(0),iotype)
           wallpropg=uread(:,1:nwallprop)

         endif
         call closefile( irstin0, "read" )
       endif
#endif

c
c.... close c-binary files
c
      call closefile( irstin, "read"//CHAR(0) )
      call closefile( igeom,  "read"//CHAR(0) )
c
      deallocate(xread)
      deallocate(qread)
      if ( numpbc > 0 )  then
         deallocate(bcinpread)
         deallocate(ibctmpread)
      endif
      deallocate(iperread)
      if(numpe.gt.1)
     &     deallocate(ilworkread)
      deallocate(nbcread)

      return
c
 994  call error ('input   ','opening ', igeom)
 995  call error ('input   ','opening ', igeom)
 997  call error ('input   ','end file', igeom)
 998  call error ('input   ','end file', igeom)
c
      end
