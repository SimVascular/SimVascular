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

!> This routine computes the LHS mass matrix, the RHS residual 
!! vector, and the preconditioning matrix, for use with the GMRES
!! solver.

        subroutine ElmGMR (u,         y,         ac,        x,     
     &                     shp,       shgl,      iBC,
     &                     BC,        shpb,      shglb,
     &                     res,       iper,      ilwork,
     &                     rowp,      colm,      lhsK,      
     &                     lhsP,      rerr)
c
        use pvsQbi  ! brings in NABI
        use stats   !  
        use pointer_data  ! brings in the pointers for the blocked arrays
        use local_mass
        use LagrangeMultipliers 
c
        include "global.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                ac,          bc,          res,         shgl
      REAL*8                shglb,       shp,         shpb,        u
      REAL*8                x,           y
C
C     Local variables
C
      INTEGER             i,           iblk,        iel
      INTEGER             inum,        j

C
      REAL*8                qres
      REAL*8                rmass

C
        dimension y(nshg,ndof),         ac(nshg,ndof),
     &            u(nshg,nsd),
     &            x(numnp,nsd),               
     &            iBC(nshg),           
     &            BC(nshg,ndofBC),  
     &            res(nshg,nflow),
     &            iper(nshg)
c
        dimension shp(MAXTOP,maxsh,MAXQPT),  
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
        dimension qres(nshg,idflx),     rmass(nshg)
c
        dimension ilwork(nlwork)

        integer rowp(nshg*nnz),         colm(nshg+1)

        real*8 lhsK(9,nnz_tot), lhsP(4,nnz_tot)

        real*8, allocatable, dimension(:,:,:,:) :: xKebe, xGoC

        real*8  rerr(nshg,10)

        real*8, allocatable :: tmpshp(:,:), tmpshgl(:,:,:)
        real*8, allocatable :: tmpshpb(:,:), tmpshglb(:,:,:)

        real*8 spmasstot(20),  ebres(nshg)
c
c.... set up the timer
c

CAD        call timer ('Elm_Form')
c
c.... -------------------->   diffusive flux   <--------------------
c
c.... set up parameters
c
        ires   = 1

        if (idiff==1 .or. idiff==3 .or. isurf==1) then ! global reconstruction
                                                       ! of qdiff
c
c loop over element blocks for the global reconstruction
c of the diffusive flux vector, q, and lumped mass matrix, rmass
c
           qres = zero
           rmass = zero
        
           do iblk = 1, nelblk
              iel    = lcblk(1,iblk)
              lelCat = lcblk(2,iblk)
              lcsyst = lcblk(3,iblk)
              iorder = lcblk(4,iblk)
              nenl   = lcblk(5,iblk) ! no. of vertices per element
              nshl   = lcblk(10,iblk)
              mattyp = lcblk(7,iblk)
              ndofl  = lcblk(8,iblk)
              nsymdl = lcblk(9,iblk)
              npro   = lcblk(1,iblk+1) - iel 
              ngauss = nint(lcsyst)
c     
c.... compute and assemble diffusive flux vector residual, qres,
c     and lumped mass matrix, rmass

              call AsIq (y,                x,                       
     &                   shp(lcsyst,1:nshl,:), 
     &                   shgl(lcsyst,:,1:nshl,:),
     &                   mien(iblk)%p,     mxmudmi(iblk)%p,  
     &                   qres,             rmass )
           enddo
       
c
c.... form the diffusive flux approximation
c
           call qpbc( rmass, qres, iBC, iper, ilwork )       
c
        endif 
c
c.... -------------------->   interior elements   <--------------------
c
        res    = zero
        if (stsResFlg .ne. 1) then
           flxID = zero
        endif

        if (lhs .eq. 1) then
           lhsp   = zero
           lhsk   = zero
        endif
c
c.... loop over the element-blocks
c
        do iblk = 1, nelblk
          iblock = iblk         ! used in local mass inverse (p>2)
          iel    = lcblk(1,iblk)
          lelCat = lcblk(2,iblk)
          lcsyst = lcblk(3,iblk)
          iorder = lcblk(4,iblk)
          nenl   = lcblk(5,iblk) ! no. of vertices per element
          nshl   = lcblk(10,iblk)
          mattyp = lcblk(7,iblk)
          ndofl  = lcblk(8,iblk)
          nsymdl = lcblk(9,iblk)
          npro   = lcblk(1,iblk+1) - iel 
          inum   = iel + npro - 1
          ngauss = nint(lcsyst)
c
c.... allocate the element matrices
c
          allocate ( xKebe(npro,9,nshl,nshl) )
          allocate ( xGoC (npro,4,nshl,nshl) )
c
c..... to calculate inner product for Lagrange Multipliers
c
          if(Lagrange.gt.zero) then
             allocate(loclhsLag(npro,9,nshl,nshl,3))
          endif 
c
c.... compute and assemble the residual and tangent matrix
c
          allocate (tmpshp(nshl,MAXQPT))
          allocate (tmpshgl(nsd,nshl,MAXQPT))

          tmpshp(1:nshl,:) = shp(lcsyst,1:nshl,:)
          tmpshgl(:,1:nshl,:) = shgl(lcsyst,:,1:nshl,:)

          call AsIGMR (y,                   ac,
     &                 x,                   mxmudmi(iblk)%p,      
     &                 tmpshp, 
     &                 tmpshgl,
     &                 mien(iblk)%p,
     &                 res,
     &                 qres,                xKebe,
     &                 xGoC,                rerr)
c
c.... satisfy the BC's on the implicit LHS
c     
          if (impl(1) .ne. 9 .and. lhs .eq. 1) then
             if(ipord.eq.1) 
     &         call bc3lhs (iBC, BC, mien(iblk)%p, xKebe)  
               call fillsparseI (mien(iblk)%p, 
     &                 xKebe,            lhsK,
     &                 xGoC,             lhsP,
     &                 rowp,             colm)
          endif

          deallocate ( xKebe )
          deallocate ( xGoC  )
          deallocate ( tmpshp )
          deallocate ( tmpshgl )
c
c..... to calculate inner product for Lagrange Multipliers
c
       if(Lagrange.gt.zero) then
          deallocate(loclhsLag)
       endif 
c
c.... end of interior element loop
c
       enddo
c
c.... add in lumped mass contributions if needed
c
       if((flmpr.ne.0).or.(flmpl.ne.0)) then
          call lmassadd(ac,res,rowp,colm,lhsK,gmass)
       endif

       have_local_mass = 1
c
c.... time average statistics
c       
       if ( stsResFlg .eq. 1 ) then

          if (numpe > 1) then
             call commu (stsVec, ilwork, nResDims  , 'in ')
          endif
          do j = 1,nshg
             if (btest(iBC(j),10)) then
                i = iper(j)
                stsVec(i,:) = stsVec(i,:) + stsVec(j,:)
             endif
          enddo
c     
          do i = 1,nshg
             stsVec(i,:) = stsVec(iper(i),:)
          enddo

          if (numpe > 1) then
             call commu (stsVec, ilwork, nResDims  , 'out')
          endif
          return
          
       endif
c
c.... zero lhsLagL before adding contributions from the boundary elements
c
       if(Lagrange.gt.zero) then
          lhsLagL = zero
       endif 

c
c.... -------------------->   boundary elements   <--------------------
c
c.... loop over the boundary elements
c
        do iblk = 1, nelblb
c
c.... set up the parameters
c
          iel    = lcblkb(1,iblk)
          lelCat = lcblkb(2,iblk)
          lcsyst = lcblkb(3,iblk)
          iorder = lcblkb(4,iblk)
          nenl   = lcblkb(5,iblk)  ! no. of vertices per element
          nenbl  = lcblkb(6,iblk)  ! no. of vertices per bdry. face
          nshl   = lcblkb(9,iblk)
          nshlb  = lcblkb(10,iblk)
          mattyp = lcblkb(7,iblk)
          ndofl  = lcblkb(8,iblk)
          npro   = lcblkb(1,iblk+1) - iel 


          if(lcsyst.eq.3) lcsyst=nenbl
c
          if(lcsyst.eq.3 .or. lcsyst.eq.4) then
             ngaussb = nintb(lcsyst)
          else
             ngaussb = nintb(lcsyst)
          endif
c
c.... allocate the element matrices
c
          allocate ( xKebe(npro,9,nshl,nshl) )
          allocate ( xGoC (npro,4,nshl,nshl) )
c
c..... to calculate inner product for Lagrange Multipliers
c
          if(Lagrange.gt.zero) then
             allocate(loclhsLag(npro,9,nshlb,nshlb,3))
          endif 
c
c.... compute and assemble the residuals corresponding to the 
c     boundary integral
c
          allocate (tmpshpb(nshl,MAXQPT))
          allocate (tmpshglb(nsd,nshl,MAXQPT))
          
          tmpshpb(1:nshl,:) = shpb(lcsyst,1:nshl,:)
          tmpshglb(:,1:nshl,:) = shglb(lcsyst,:,1:nshl,:)

#if (VER_VARWALL == 1)
          if (ivarwallprop .eq. 1) then

            call AsBMFG2 (u,                       y,
     &                   ac,                      x,
     &                   tmpshpb,               tmpshglb,
     &                   mienb(iblk)%p,           mmatb(iblk)%p,
     &                   miBCB(iblk)%p,           mBCB(iblk)%p,
     &                   res,  xKebe, xGoC, wallpropelem(iblk)%p)
          else
            call AsBMFG (u,                       y,
     &                   ac,                      x,
     &                   tmpshpb,               tmpshglb,
     &                   mienb(iblk)%p,           mmatb(iblk)%p,
     &                   miBCB(iblk)%p,           mBCB(iblk)%p,
     &                   res,  xKebe, xGoC)
          endif

#elif (VER_CLOSEDLOOP == 1)
         call AsBMFG (u,                       y,
     &                ac,                      x,
     &                tmpshpb,
     &                tmpshglb,
     &                mienb(iblk)%p,           mmatb(iblk)%p,
     &                miBCB(iblk)%p,           mBCB(iblk)%p,
     &                res,  xKebe, xGoC)
#else
          call AsBMFG (u,                       y,
     &                 ac,                      x,
     &                 tmpshpb,
     &                 tmpshglb,
     &                 mienb(iblk)%p,           mmatb(iblk)%p,
     &                 miBCB(iblk)%p,           mBCB(iblk)%p,
     &                 res,                     xKebe)
#endif


c
c.... satisfy (again, for the vessel wall contributions) the BC's on the implicit LHS
c
c.... first, we need to make xGoC zero, since it doesn't have contributions from the 
c.... vessel wall elements

          xGoC = zero

          if (impl(1) .ne. 9 .and. lhs .eq. 1) then
             if(ipord.eq.1)
     &         call bc3lhs (iBC, BC,mienb(iblk)%p, xKebe)
             call fillsparseI (mienb(iblk)%p,
     &                 xKebe,           lhsK,
     &                 xGoC,            lhsP,
     &                 rowp,            colm)
          endif

          deallocate ( xKebe )
          deallocate ( xGoC )
          deallocate (tmpshpb)
          deallocate (tmpshglb)
          if(Lagrange.gt.zero) then
             deallocate(loclhsLag)
          endif
c
c.... end of boundary element loop
c
       enddo
c
       if(Lagrange.gt.zero) then
          LagSwitch = 0 
          call CalcNANBLagrange(colm, rowp, y(:,1:3))
       endif
c       
       if(ipvsq.ge.1) then
c
c....  pressure vs. resistance boundary condition sets pressure at
c      outflow to linearly increase as flow through that face increases
c      (routine is at bottom of this file)
c
          call ElmpvsQ (res,y,-1.0d0)     
       endif
           
c
c before the commu we need to rotate the residual vector for axisymmetric
c boundary conditions (so that off processor periodicity is a dof add instead
c of a dof combination).  Take care of all nodes now so periodicity, like
c commu is a simple dof add.
c
       if(iabc==1)              !are there any axisym bc's
     &       call rotabc(res, iBC,  'in ')
c
c
c.... -------------------->   communications <-------------------------
c

       if (numpe > 1) then
          call commu (res  , ilwork, nflow  , 'in ')
       endif

c
c.... ---------------------->   post processing  <----------------------
c
c.... satisfy the BCs on the residual
c
      call bc3Res (iBC,  BC,  res,  iper, ilwork)
c
c.... return
c
c      call timer ('Back    ')
      return
      end
