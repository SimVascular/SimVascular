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

!> This subroutine generates the problem data and calls the solution
!!  driver.

        subroutine proces
c
        use readarrays          ! used to access x, iper, ilwork

#if(VER_VARWALL == 1)
        USE pointer_data
#endif

        include "global.h"
        include "mpif.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/propar.h"
        include "common_blocks/outpar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
        include "common_blocks/nomodule.h"
c
c arrays in the following 2 lines are now dimensioned in readnblk
c        dimension x(numnp,nsd)
c        dimension iper(nshg), ilwork(nlwork)
c
C     Local variables
C
      INTEGER             ibc
C
      REAL*8                ac,          bc
      REAL*8                shgl,        shglb
      REAL*8                shp,         shpb
      REAL*8                velbar
      REAL*8                y
C
        dimension y(nshg,ndof),
     &            iBC(nshg),
     &            BC(nshg,ndofBC),
     &            ac(nshg,ndof)

#if(VER_VARWALL == 1)
c      temporary local boundary element nodal coordinates and wall properties
       real*8, allocatable, dimension(:,:,:) :: xlb
       real*8, allocatable, dimension(:,:,:) :: wallpropl
       integer iblk,iel
#endif
c
c.... shape function declarations
c
        dimension shp(MAXTOP,maxsh,MAXQPT),
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT),
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT)

        dimension velbar(nfath,nflow)
c
c stuff to interpolate profiles at inlet
c
        real*8 bcinterp(100,ndof+1),interp_mask(ndof)
        logical exlog

c
c.... generate the geometry and boundary conditions data
c

        call gendat (y,              ac,             point2x, l2g,
     &               iBC,            BC,
     &               point2iper,     point2ilwork,   shp,
     &               shgl,           shpb,           shglb
     &               )
        call setper(nshg)
        call perprep(iBC,point2iper,nshg)

c
c.... time averaged statistics
c
        if (ioform .eq. 2) then
           call initStats(point2x, iBC, point2iper, point2ilwork)
        endif
c
c.... p vs. Q boundary
c
           call initNABI( point2x, shpb )
c
c.... initialize AutoSponge
c
        if(matflg(5,1).ge.4) then ! cool case (sponge)
           call initSponge( y,point2x)
        endif
c
c.... Write Message For Simulation Type
c
        if(myrank.eq.master) then
          if(ideformwall.eq.1) then
            write(*,*) "Simulation Type: DEFORMABLE WALL"
            if(ivarwallprop.eq.1) then
              write(*,*) "Thickness Type: VARIABLE"
              write(*,*) ""
            else
              write(*,*) "Thickness Type: CONSTANT"
              write(*,*) ""
            endif
          else
            write(*,*) ""
            write(*,*) "Simulation Type: RIGID WALL"
            write(*,*) ""
          endif
        endif

#if(VER_VARWALL == 1)

        if((ideformwall.eq.1) .and. (ivarwallprop.eq.1)) then

          do iblk = 1, nelblb
c
c....set up the parameters
c
            iel    = lcblkb(1,iblk)

            npro   = lcblkb(1,iblk+1) - iel

c            allocate ( xlb(npro,nenl,nsd) )
            allocate ( wallpropl(npro,nshl,2) )

c           get wall properties for each wall node for block iblk
         call localx(wallpropg,wallpropl,  mienb(iblk)%p, 2, 'gather  ')

c           get coordinates for wall nodes in block iblk
c           call localx(point2x,  xlb,  mienb(iblk)%p,  nsd,  'gather  ')

            call local_elemwallprop(wallpropl,wallpropelem(iblk)%p)

c            deallocate(xlb)
            deallocate(wallpropl)
          enddo

          deallocate(wallpropg)
        end if
#endif

c.... close echo file

        close (iecho)

c
c
c.... call the semi-discrete predictor multi-corrector iterative driver
c
        call itrdrv (y,              ac,
     &               uold,           point2x,
     &               iBC,            BC,
     &               point2iper,     point2ilwork,   shp,
     &               shgl,           shpb,           shglb
     &               )
c
c.... return
c
c
c.... stop CPU-timer
c
CAD        call timer ('End     ')
c
c.... close echo file
c
c        close (iecho)
c
c.... end of the program
c
CAD        write(6,*) 'Life: ', second(0) - ttim(100)
        deallocate(point2iper)
        if(numpe.gt.1) deallocate(point2ilwork)
        deallocate(point2x)

        return
        end


