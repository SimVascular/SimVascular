c     UC Copyright Notice
c
c     This software is Copyright (c) 2014-2015 The Regents of the 
c     University of California. All Rights Reserved.
c
c     Permission to copy and modify this software and its documentation
c     for educational, research and non-profit purposes, without fee, 
c     and without a written agreement is hereby granted, provided that
c     the above copyright notice, this paragraph and the following three
c     paragraphs appear in all copies.
c
c     Permission to make commercial use of this software may be obtained
c     by contacting:
c
c     Technology Transfer Office
c     9500 Gilman Drive, Mail Code 0910
c     University of California
c     La Jolla, CA 92093-0910
c     (858) 534-5815
c     invent@ucsd.edu
c
c     This software program and documentation are copyrighted by The
c     Regents of the University of California. The software program and
c     documentation are supplied "as is", without any accompanying
c     services from The Regents. The Regents does not warrant that the
c     operation of the program will be uninterrupted or error-free. The
c     end-user understands that the program was developed for research
c     purposes and is advised not to rely exclusively on the program for
c     any reason.
c
c     IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY 
c     PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL 
c     DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS 
c     SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF 
c     CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
c     THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY 
c     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
c     OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE 
c     SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE 
c     UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO PROVIDE 
c     MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

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

!> This routine reads the initial values in primitive form (density,
!! velocity and temperature), satisfies the boundary conditions and 
!! converts them to Y-variables.
!!
!! input:<BR>
!! @params[in] iBC(nshg) Boundary condition code
!! @params[in] BC(nshg,ndofBC) Boundary condition constrain data
!! @params[in] x(numnp,nsd) Locations of nodes, numnp-> # of node,
!!                          where nsd-> space dimension, 1=x, 2=y, 3=z 
!!			
!! output:<BR>
!!  y      (nshg,ndof)          : initial values of Y variables

        subroutine genini (iBC, BC, y, ac, iper, ilwork,
     &               x,  lgmapping,
     &               shp,     shgl,    shpb,    shglb ) 
c
        use specialBC     ! gets itvn from here
        use convolImpFlow ! brings in ntimeptpT and other variables
        use convolRCRFlow ! brings RCR variables
#if (VER_CORONARY == 1)
        use convolCORFlow  ! brings in Coranary variables
#endif
        use LagrangeMultipliers ! brings in variables for Lagrange Multipliers
        use calcFlowPressure
        
        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/mio.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/title.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             ibc,           ilwork,    iper, lgmapping
C
      REAL*8                ac,          bc,          shgl,        shglb
      REAL*8                shp,         shpb,        x
      REAL*8                y
C
C     Local variables
C
      INTEGER             i,           ierr,	irank
      INTEGER             itempr,      j
      INTEGER             n
c
        dimension iBC(nshg),                iper(nshg),
     &            BC(nshg,ndofBC),          y(nshg,ndof),
     &            ac(nshg,ndof),       x(numnp,nsd), lgmapping(numnp),
     &            shp(MAXTOP,maxsh,MAXQPT),
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT),
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT)

        dimension ilwork(nlwork)


c
c.... -------------------------->  Restart  <---------------------------
c
c.... read q from [RESTAR.INP], reset LSTEP
c
        call restar ('in  ',  y,  ac)
c
c.... time varying boundary conditions as set from file bct.dat and impt.dat 
c     (see function for format in file bctint.f)
c
        if (itvn .gt. 0 ) then !for inlet velocities
           call initBCt( x,lgmapping, iBC, BC)
           call BCint(lstep*Delt(1),shp,shgl,shpb,shglb,x, BC, iBC)
        endif
        if (impfile .gt. 0 ) then !for impedance BC
           do irank=1, numpe
              call MPI_BARRIER (MPI_COMM_WORLD,ierr)
              if((irank-1).eq.myrank) then 
                 write(*,*) 'reading Qhistor.dat'
                 open(unit=816, file='Qhistor.dat',status='old')
                 read (816,*) ntimeptpT
                 allocate (QHistImp(ntimeptpT+1,numImpSrfs)) 
                 allocate (QHistTry(ntimeptpT,numImpSrfs)) 
                 allocate (QHistTryF(ntimeptpT,numImpSrfs)) 
                 do j=1,ntimeptpT+1
                    read(816,*) (QHistImp(j,n),n=1,numImpSrfs) !read flow history
                 enddo
                 close(816)              
                 call initImpt() !read impedance data and initialize begin/end values
                 do i=2,ntimeptpT
                    call Impint((ntimeptpT-i+1)*Delt(1),i) !return Imp values in reverse order ZN->Z0
                 enddo

                 allocate (poldImp(0:MAXSURF)) !for pressure part that depends on the history only
              else 
                 continue
              endif
           enddo
        endif
        if (ircrfile .gt. 0 ) then !for RCR BC
           call initRCRt()      !read RCR data
           dtRCR(:) = Delt(1)/(ValueListRCR(2,:)*ValueListRCR(3,:))
           !last one needed only to have array of same size as imp BC
           allocate (poldRCR(0:MAXSURF)) !for pressure part that depends on the history only
           allocate (HopRCR(0:MAXSURF)) !for H operator contribution
        endif
#if(VER_CORONARY == 1)
        if (icorfile .gt. 0 ) then !for Coronary BC
c          print *,'Inside genini and about to call initcort'
           call initCORt()

           allocate (CORArea(numCORSrfs))
           allocate (CORic(2,numCORSrfs))
           allocate (poldCOR(0:MAXSURF)) !for pressure part that depends on the history only
           allocate (plvoldCOR(0:MAXSURF))
           allocate (HopCOR(0:MAXSURF)) !for H operator contribution
c          Set to zero
           CORArea = zero
           poldCOR = zero
           plvoldCOR = zero
           HopCOR = zero

c           print *,'Print Intialization of poldCOR'
c           do loopA=1, MAXSURF
c             print *,'poldCOR(',loopA,')',poldCOR(loopA)
c           enddo
        endif
#endif
        if (numCalcSrfs .gt. 0) then !for CalcSurfaces
           allocate (CalcArea(numCalcSrfs))
           CalcArea = zero
           allocate (FlowHist(lstep+nstep(1)+1,numCalcSrfs)) !for flow history
           allocate (PressHist(lstep+nstep(1)+1,numCalcSrfs)) !for pressure history
           FlowHist = zero
           PressHist = zero
        endif
        if (iLagfile .gt. 0) then !for Lagrange multipliers
           call initLagrange()
           allocate(QLagrange(numLagrangeSrfs,2))
           allocate(PQLagrange(numLagrangeSrfs,2))
           allocate(IPLagrange(numLagrangeSrfs,6))
           allocate(NANBLagrange(6,nshg,3))
           QLagrange = zero
           PQLagrange = zero
           IPLagrange = zero
           NANBLagrange = zero
        endif
c
c
c.... satisfy the boundary conditions
c

c        call itrBC (y, ac,  iBC, BC, iper, ilwork)

        itempr=mod(impl(1),2)  ! tempr solve if impl odd
        if(itempr.eq.1) then
           isclr=0
           call itrBCSclr (y, ac,  iBC, BC, iper, ilwork)
        endif
        do isclr=1,nsclr
           call itrBCSclr (y, ac,  iBC, BC, iper, ilwork)
        enddo
c
c.... --------------------------->  Echo  <----------------------------
c
c.... echo the initial data
c
        if ((necho .lt. 0).and.(myrank.eq.master)) then
          do n = 1, nshg
            if (mod(n,50) .eq. 1) write(iecho,1000) ititle,(i,i=1,ndof)
            write (iecho,1100) n, (y(n,i),i=1,ndof)
          enddo
        endif
c
c.... return
c
        return
c
1000    format(a80,//,
     &  ' I n i t i a l   V a l u e s                        ',//,
     &  '    Node ',/,
     &  '   Number ',6x,6('dof',i1,:,10x))
1100    format(1p,2x,i5,5x,5(e12.5,2x))
c
        end

