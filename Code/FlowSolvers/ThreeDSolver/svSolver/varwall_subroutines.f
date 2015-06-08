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

!> This routine computes and assembles the data corresponding to the
!! boundary elements.

#include "cvFlowsolverOptions.h"

#if (VER_VARWALL == 1)

      subroutine AsBMFG2 (u,       y,       ac,      x,
     &                   shpb,    shglb,
     &                   ienb,    materb,  iBCB,    BCB,
     &                   res,     xKebe,   xGoC, elem_prop)
        use LagrangeMultipliers
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

c.............................Declarations....................................
        INTEGER ienb,materb,iBCB
        REAL*8 ac,acl,BCB,dwl,res,rl,shpb,shglb,sgn,u,ul,y,yl,x,
     &  xKebe,xlb
        REAL*8 xGoC,elem_prop
c.............................................................................


        dimension y(nshg,ndofl),           x(numnp,nsd),
     &            ac(nshg,ndofl),          u(nshg,nsd),
     &            shpb(nshl,ngaussb),
     &            shglb(nsd,nshl,ngaussb),
     &            ienb(npro,nshl),         materb(npro),
     &            iBCB(npro,ndiBCB),       BCB(npro,nshlb,ndBCB),
     &            res(nshg,nflow),         dwl(npro,nenl),
     &            elem_prop(npro,2)
c
        dimension yl(npro,nshl,ndofl),     xlb(npro,nenl,nsd),
     &            rl(npro,nshl,nflow),     sgn(npro,nshl),
     &            ul(npro,nshl,nsd),       acl(npro,nshl,ndofl)
c
        dimension xKebe(npro,9,nshl,nshl),
     &            xGoC(npro,4,nshl,nshl)

c
c.... get the matrix of mode signs for the hierarchic basis functions
c
        if (ipord .gt. 1) then
           call getsgn(ienb,sgn)
        endif
c
c.... gather the variables
c
        call localy(y,      yl,     ienb,   ndofl,  'gather  ')
        call localy(ac,     acl,    ienb,   ndofl,  'gather  ')
        call localx(x,      xlb,    ienb,   nsd,    'gather  ')
        call localx(u,      ul,     ienb,   nsd,    'gather  ')
c
c.... zero the matrices if they are being recalculated
c
       if (lhs. eq. 1)  then
           xKebe = zero
        endif
       if(Lagrange.gt.zero) then
          loclhsLag = zero
       endif
c
c.... get the boundary element residuals
c
        rl  = zero
c
c.... 3D
c
        call e3b2  (ul,      yl,      acl,     iBCB,    BCB,
     &             shpb,    shglb,  xlb,     rl,
     &             sgn,     dwl,    xKebe , elem_prop)

        IF (numNormalSrfs .GT. 0) THEN
           CALL normCon (ienb, y, xKebe, xGoC, rl)
        END IF
c
c.... assemble the residual and the modified residual
c
        call local (res,    rl,     ienb,   nflow,  'scatter ')

c
c.... end
c
        return
        end

!> This routine calculates the 3D RHS residual of the fluid boundary
!! elements.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Y variables
!! @param[in] iBCB(npro,ndiBCB) boundary condition code (iBCB(:,1) is
!!                              a bit tested boundary integral flag i.e.
!!                              if set to value of BCB
!!                              if set to floating value
!! @param[in] iBCB(:,1)
!! - convective flux * 1  0  (ditto to all below)
!! - pressure   flux * 2
!! - viscous    flux * 4
!! - heat       flux * 8
!! - turbulence wall * 16
!! - scalarI   flux  * 16*2^I (where I is the scalar number)
!!
!! @param[in] iBCB(:,2) is the srfID given by the user in MGI that we will
!!                      collect integrated fluxes for.
!!
!! @param[in] BCB(npro,nshlb,ndBCB) Boundary Condition values
!! - BCB (1) mass flux
!! - BCB (2) pressure
!! - BCB (3) viscous flux in x1-direc.
!! - BCB (4) viscous flux in x2-direc.
!! - BCB (5) : viscous flux in x3-direc.
!! - BCB (6) : heat flux
!!
!!  @param[in] shpb(nen,ngaussb) Boundary element shape-functions
!!  @param[in] shglb(nsd,nen,ngaussb) Boundary element grad-shape-functions
!!  @param[in] xlb(npro,nenl,nsd) Nodal coordinates at current step
!!
!! output:<BR>
!! $param[in] rl(npro,nshl,nflow) Element residual
!!
!! Note: Always the first side of the element is on the boundary.
!!       However, note that for higher-order elements the nodes on
!!       the boundary side are not the first nshlb nodes, see the
!!       array mnodeb.


        subroutine e3b2 (ul,      yl,      acl,     iBCB,    BCB,
     &                  shpb,    shglb,   xlb,     rl,      sgn,
     &    dwl,     xKebe, elem_prop)
        use LagrangeMultipliers
c
        include "global.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"

C     Argument variables: CAREFUL COPIED FROM E3B !!!
C
      INTEGER             ibcb
C
      REAL*8                acl,         bcb,         rl,          sgn
      REAL*8                shglb,       shpb,        ul,          xkebe
      REAL*8                xlb,         yl
C
C     Local variables: CAREFUL COPIED FROM E3B !!!
C
      INTEGER             i,           iel,         iface,       ii
      INTEGER             j,           k,           l,           lnode
      INTEGER             m,           n,           nodlcl,      nodlcl2
      INTEGER             nodlclM,nodlclN
C
      REAL*8                allu,        bnorm,      g1yi
      REAL*8                g2yi,        g3yi,        pres,        rho
      REAL*8                rkwall_glob, rlkwall,     rmu,         rna
      REAL*8                rou,         shape,       shdrv,       stabk
      REAL*8                tau1n,       tau2n,       tau3n,       temp
      REAL*8                u1,          u2,          u3,          unm
      REAL*8                vdot,        wdetjb,      elem_prop

c
        dimension yl(npro,nshl,ndof),          iBCB(npro,ndiBCB),
     &            BCB(npro,nshlb,ndBCB),       shpb(nshl,ngaussb),
     &            shglb(nsd,nshl,ngaussb),
     &            xlb(npro,nenl,nsd),          ul(npro,nshl,nsd),
     &            acl(npro,nshl,ndof),
     &            rl(npro,nshl,nflow), elem_prop(npro,2)
c
        dimension g1yi(npro,ndof),             g2yi(npro,ndof),
     &            g3yi(npro,ndof),             WdetJb(npro),
     &            bnorm(npro,nsd)
c
        dimension u1(npro),                    u2(npro),
     &            u3(npro),                    rho(npro),
     &            unm(npro),                   pres(npro),
     &            vdot(npro,nsd),              rlKwall(npro,nshlb,nsd)
c
        dimension rou(npro),                   rmu(npro),
     &            temp(npro),                  allU(nsd)
c
        dimension tau1n(npro),
     &            tau2n(npro),                 tau3n(npro)
c
        dimension lnode(27),               sgn(npro,nshl),
     &            shape(npro,nshl),        shdrv(npro,nsd,nshl),
     &            rNa(npro,4)

        real*8    xmudmi(npro,ngauss),      dwl(npro,nshl)
c
      dimension xKebe(npro,9,nshl,nshl),  rKwall_glob(npro,9,nshl,nshl),
     &   stabK(npro,9,nshl,nshl)

c     REMOVED intp: CHECK !!!
c     integer   intp, num
      integer   num

c
c.... compute the nodes which lie on the boundary (hierarchic)
c
        call getbnodes(lnode)
c
c.... loop through the integration points
c
        if(lcsyst.eq.3.or.lcsyst.eq.4) then
           ngaussb = nintb(lcsyst)
        else
           ngaussb = nintb(lcsyst)
        endif

        stabK = zero

        do intp = 1, ngaussb
c
c.... get the hierarchic shape functions at this int point
c
c        call getshp(shpb,        shglb,        sgn,
c     &              shape,       shdrv)

c       SHOULD CALL THE ROUTINE WITH BOUNDARY SHAPE FUNCTIONS
c       OTHERWISE: BOUND CHECK ERROR - DES - 30JAN2014
        call getshpb(shpb,        shglb,        sgn,
     &               shape,       shdrv)

c
c     NOTE I DID NOT PASS THE lnode down.  It is not needed
c     since the shape functions are zero on the boundary
c
c     Note that xmudmi is not calculated at these quadrature
c     points so you give it a zero.  This has implications.
c     the traction calculated by this approach will include
c     molecular stresses ONLY.  This is why we will use the
c     consistent flux method to obtain the forces when doing
c     effective viscosity wall modeling.  When doing slip velocity
c     this is not a problem since the traction is given from the
c     log law relation (not the viscosity).
c
        xmudmi=zero
c
c.... get necessary fluid properties (including eddy viscosity)
c
        call getdiff(dwl, yl,     shape,     xmudmi, xlb,   rmu, rho)
c
c.... calculate the integraton variables
c
        call e3bvar2 (yl,              acl,             ul,
     &               shape,
     &               shdrv,           xlb,
     &               lnode,           WdetJb,
     &               bnorm,           pres,
     &               u1,              u2,              u3,
     &               rmu,             unm,
     &               tau1n,           tau2n,           tau3n,
     &               vdot,            rlKwall,
     &               xKebe,           rKwall_glob,  elem_prop)

c
c.... -----------------> boundary conditions <-------------------
c


        do iel = 1, npro
c
c  if we have a nonzero value then
c  calculate the fluxes through this surface
c
           iface = abs(iBCB(iel,2))
           if (iface .ne. 0 .and. ires.ne.2) then
              flxID(1,iface) =  flxID(1,iface) + WdetJb(iel)! measure area too
              flxID(2,iface) =  flxID(2,iface) - WdetJb(iel) * unm(iel)
              flxID(3,iface) = flxID(3,iface)
     &                   - ( tau1n(iel) - bnorm(iel,1)*pres(iel))
     &                   * WdetJb(iel)
              flxID(4,iface) = flxID(4,iface)
     &                   - ( tau2n(iel) - bnorm(iel,2)*pres(iel))
     &                   * WdetJb(iel)
              flxID(5,iface) = flxID(5,iface)
     &                   - ( tau3n(iel) - bnorm(iel,3)*pres(iel))
     &                   * WdetJb(iel)

           endif
c
c
c.... mass flux
c

           if (btest(iBCB(iel,1),0)) then
              unm(iel)  = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 unm(iel) = unm(iel)
     &                    + shape(iel,nodlcl) * BCB(iel,n,1)
              enddo
           endif
c
c.... pressure
c
          if (btest(iBCB(iel,1),1)) then
              pres(iel) = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 pres(iel) = pres(iel)
     &                     + shape(iel,nodlcl) * BCB(iel,n,2)
              enddo
           endif
c
c.... viscous flux
c
           if (btest(iBCB(iel,1),2)) then
              tau1n(iel) = zero
              tau2n(iel) = zero
              tau3n(iel) = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 tau1n(iel) = tau1n(iel)
     &                      + shape(iel,nodlcl)*BCB(iel,n,3)
                 tau2n(iel) = tau2n(iel)
     &                      + shape(iel,nodlcl)*BCB(iel,n,4)
                 tau3n(iel) = tau3n(iel)
     &                      + shape(iel,nodlcl)*BCB(iel,n,5)
              enddo
           endif
c
c.... turbulence wall (as a way of checking for deformable wall stiffness)
c
           if (btest(iBCB(iel,1),4)) then
              rlKwall(iel,:,:) = rlKwall(iel,:,:) / ngaussb ! divide by number of gauss points
              pres(iel) = zero                              ! to avoid the gauss point loop
              tau1n(iel) = zero                             ! and make the traction contribution
              tau2n(iel) = zero                             ! zero
              tau3n(iel) = zero
           else
              rlKwall(iel,:,:) = zero                       ! this is not a deformable element
              vdot(iel,:) = zero
              xKebe(iel,:,:,:) = zero
              rKwall_glob(iel,:,:,:) = zero                 ! no stiffness: not a wall element
           endif
c
c..... to calculate inner products for Lagrange Multipliers
c
           if(Lagrange.gt.zero) then
              do k=1, numLagrangeSrfs
                 if (iBCB(iel,2).eq.nsrflistLagrange(k)) then
                    do n = 1, nshlb
                       nodlcl = lnode(n)
                       do m=1, nsd
                          do i=1, nshlb
                             nodlcl2 = lnode(i)
                             do l=1, nsd
                                do j=1, 3
                                   num=(m-1)*nsd+l
                                   loclhsLag(iel,num,n,i,j)=
     &                                loclhsLag(iel,num,n,i,j)+
     &                                shape(iel,nodlcl)*WdetJb(iel)*
     &                                shape(iel,nodlcl2)*
     &                                LagInplaneVectors(m,j,k)*
     &                                LagInplaneVectors(l,j,k)
                                enddo
                             enddo
                          enddo
                       enddo
                    enddo
                 endif
              enddo
           endif  ! end of if(Lagrange.gt.zero)
c
        enddo                                               ! end of bc loop
c
c$$$c.... if we are computing the bdry for the consistent
c$$$c     boundary forces, we must not include the surface elements
c$$$c     in the computataion (could be done MUCH more efficiently!)--->
                                                                  !this
                                                                  !comment should read as for the consistent flux calculation rather than boundary forces
c$$$c
        if (ires .eq. 2) then
           do iel = 1, npro
              if (nsrflist(iBCB(iel,2)) .ne. 0) then
                 unm(iel) = zero
                 tau1n(iel) = zero
                 tau2n(iel) = zero
                 tau3n(iel) = zero
c                 pres(iel) = zero
c
c whatever is zeroed here will beome part of the post-processed surface
c                 "traction force"
c
c uncomment the next two lines to get all of the t vector coming from
c                 Alberto's wall motion model.
                 vdot(iel,:)=zero
                 rlKwall(iel,:,:)=zero

c
c uncomment the next 8 lines to get only the tangential part
c

c                  vn=dot_product(vdot(iel,:),bnorm(iel,:))
c                  vdot(iel,:)=vn*bnorm(iel,:)
c                  walln1=dot_product(rlkwall(iel,1,:),bnorm(iel,:))
c                  walln2=dot_product(rlkwall(iel,2,:),bnorm(iel,:))
c                  walln3=dot_product(rlkwall(iel,3,:),bnorm(iel,:))
c                  rlkwall(iel,1,:)=walln1*bnorm(iel,:)
c                  rlkwall(iel,2,:)=walln2*bnorm(iel,:)
c                  rlkwall(iel,3,:)=walln3*bnorm(iel,:)
              endif
           enddo
        endif
c
c.... assemble the contributions
c
        rNa(:,1) = -WdetJb * ( tau1n - bnorm(:,1) * pres - vdot(:,1))
        rNa(:,2) = -WdetJb * ( tau2n - bnorm(:,2) * pres - vdot(:,2))
        rNa(:,3) = -WdetJb * ( tau3n - bnorm(:,3) * pres - vdot(:,3))
        rNa(:,4) =  WdetJb * unm
c
c.... THIS IS DONE FOR ADDING STABILITY IN THE CASE OF BACK FLOW
c
        rou      = backFlowStabCoef*5D-1*rho*WdetJb*(ABS(unm) - unm)
        rNa(:,1) = rNa(:,1) + rou*u1
        rNa(:,2) = rNa(:,2) + rou*u2
        rNa(:,3) = rNa(:,3) + rou*u3

c       BUG FIX - DES - 30JAN2014
        DO n = 1, nshlb
           nodlclN = lnode(n)
           DO m = 1, nshlb
              nodlclM = lnode(m)
              temp = alfi*gami*Delt(1)*rou*shape(:,nodlclN)*
     2            shape(:,nodlclM)
              stabK(:,1,n,m) = stabK(:,1,n,m) + temp
              stabK(:,5,n,m) = stabK(:,5,n,m) + temp
              stabK(:,9,n,m) = stabK(:,9,n,m) + temp
           END DO
        END DO

c        SMALL CONTRIBUTION ACCORDING TO MAHDI - DES 30JAN2014
c        DO iel=1, npro
c           IF (rou(iel) .GT. 0D0) THEN
c              allU(1) = u1(iel)
c              allU(2) = u2(iel)
c              allU(3) = u3(iel)
c              DO n=1, nshlb
c                 DO m=1, nshlb
c                    DO i=1,nsd
c                       DO j=1,nsd
c                          ii = j + (i - 1)*nsd
c                          stabK(iel,ii,n,m) = stabK(iel,ii,n,m) -
c     2                       rho(iel)*WdetJb(iel)*alfi*gami*Delt(1)*
c     3                       shpb(iel,n)*shpb(iel,m)*allU(i)*
c     4                       bnorm(iel,j)
c                       END DO
c                    END DO
c                 END DO
c              END DO
c           END IF
c        END DO

        if(iconvflow.eq.1) then     ! conservative form was integrated
                                    ! by parts and has a convective
                                    ! boundary integral
c
c.... assemble the contributions
c
           rou=rho*unm
           rNa(:,1) = rNa(:,1) + WdetJb * rou * u1
           rNa(:,2) = rNa(:,2) + WdetJb * rou * u2
           rNa(:,3) = rNa(:,3) + WdetJb * rou * u3
        endif
c
c.... ------------------------->  Residual  <--------------------------
c
c.... add the flux to the residual
c
        do n = 1, nshlb
           nodlcl = lnode(n)

           rl(:,nodlcl,1) = rl(:,nodlcl,1) - shape(:,nodlcl) * rNa(:,1)
           rl(:,nodlcl,2) = rl(:,nodlcl,2) - shape(:,nodlcl) * rNa(:,2)
           rl(:,nodlcl,3) = rl(:,nodlcl,3) - shape(:,nodlcl) * rNa(:,3)
           rl(:,nodlcl,4) = rl(:,nodlcl,4) - shape(:,nodlcl) * rNa(:,4)

        enddo
        if(ideformwall.eq.1) then
           rl(:,1,1) = rl(:,1,1) - rlKwall(:,1,1)
           rl(:,1,2) = rl(:,1,2) - rlKwall(:,1,2)
           rl(:,1,3) = rl(:,1,3) - rlKwall(:,1,3)

           rl(:,2,1) = rl(:,2,1) - rlKwall(:,2,1)
           rl(:,2,2) = rl(:,2,2) - rlKwall(:,2,2)
           rl(:,2,3) = rl(:,2,3) - rlKwall(:,2,3)

           rl(:,3,1) = rl(:,3,1) - rlKwall(:,3,1)
           rl(:,3,2) = rl(:,3,2) - rlKwall(:,3,2)
           rl(:,3,3) = rl(:,3,3) - rlKwall(:,3,3)
        endif

c
c.... end of integration loop
c
        enddo
        if(ideformwall.eq.1) then
c
c.... -----> Wall Stiffness and Mass matrices for implicit LHS  <-----------
c
c.... Now we simply have to add the stiffness contribution in rKwall_glob to
c.... the mass contribution already contained in xKebe

c.... this line is going to destroy the mass matrix contribution


c      xKebe = zero

         xKebe(:,:,:,:) = ( xKebe(:,:,:,:)*iwallmassfactor
     &           + rKwall_glob(:,:,:,:)*iwallstiffactor )


         endif
c$$$        ttim(40) = ttim(40) + tmr()
c
c.... return
c

        xKebe = xKebe + stabK

        return
        end

!> This routine computes the variables at integration points for
!! the boundary element routine.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Primitive variables (local), ndof: 5[p,v1,v2,v3,T]+number of scalars solved
!! @param[in] acl(npro,nshl,ndof) Acceleration (local)
!! @param[in] ul(npro,nshlb,nsd) Displacement (local)
!! @param[in] shpb(nen) Boundary element shape-functions
!! @param[in] shglb(nsd,nen) Boundary element grad-shape-functions
!! @param[in] xlb(npro,nenl,nsd) Nodal coordinates at current step
!! @param[in] lnode(nenb) Local nodes on the boundary
!!
!! output:<BR>
!! @param[out] g1yi(npro,ndof) grad-v in direction 1
!! @param[out] g2yi(npro,ndof) grad-v in direction 2
!! @param[out] g3yi(npro,ndof) grad-v in direction 3
!! @param[out] WdetJb(npro) weighted Jacobian
!! @param[out] bnorm(npro,nsd) outward normal
!! @param[out] pres(npro) pressure
!! @param[out] u1(npro) x1-velocity component
!! @param[out] u2(npro) x2-velocity component
!! @param[out] u3(npro) x3-velocity component
!! @param[out] unm(npro) BC u dot n
!! @param[out] p(npro) BC pressure
!! @param[out] tau1n(npro) BC viscous flux 1
!! @param[out] tau2n(npro) BC viscous flux 2
!! @param[out] tau3n(npro) BC viscous flux 3
!! @param[out] vdot(npro,nsd) Acceleration at quadrature points
!! @param[out] rlKwall(npro,nshlb,nsd) Wall stiffness contribution to the local residual

      subroutine e3bvar2 (yl,      acl,     ul,
     &                   shpb,    shglb,
     &                   xlb,     lnode,
     &                   WdetJb,  bnorm,   pres,
     &                   u1,      u2,      u3,      rmu,
     &                   unm,     tau1n,   tau2n,   tau3n,
     &                   vdot,    rlKwall,
     &                   xKebe,   rKwall_glob, elem_prop)

      include "global.h"
      include "common_blocks/conpar.h"
      include "common_blocks/elmpar.h"
      include "common_blocks/inpdat.h"
      include "common_blocks/intpt.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/propar.h"
      include "common_blocks/shpdat.h"
      include "common_blocks/timdat.h"

C     Argument variables: CAREFULL COPIED FROM E3BVAR !!!
C
      INTEGER             lnode
C
      REAL*8                acl,         bnorm,       pres
      REAL*8                rkwall_glob, rlkwall,     rmu,         shglb
      REAL*8                shpb,        tau1n,       tau2n,       tau3n
      REAL*8                u1,          u2,          u3,          ul
      REAL*8                unm,         vdot,        wdetjb,      xkebe
      REAL*8                xlb,         yl
C
C     Local variables
C
      INTEGER             ipt2,        ipt3,        k,           n
      INTEGER             nodlcl
C
      REAL*8                b1,          b2,          b3
      REAL*8                detjacrot,   dmatrix,     dtimesb1
      REAL*8                dtimesb2,    dtimesb3,    dxdxib,    dxidxb
      REAL*8                g1yi,        g2yi,        g3yi,      gl1yi
      REAL*8                gl2yi,       gl3yi,       rkwall_glob11
      REAL*8                rkwall_glob12,            rkwall_glob13
      REAL*8                rkwall_glob21,            rkwall_glob22
      REAL*8                rkwall_glob23,            rkwall_glob31
      REAL*8                rkwall_glob32,            rkwall_glob33
      REAL*8                rkwall_local11,           rkwall_local12
      REAL*8                rkwall_local13,           rkwall_local21
      REAL*8                rkwall_local22,           rkwall_local23
      REAL*8                rkwall_local31,           rkwall_local32
      REAL*8                rkwall_local33,           rotnodallocal
      REAL*8                temp,        temp1,       temp2,      temp3
      REAL*8                tmp1,       v1,          v2
      REAL*8                v3,          x1rot,       x2rot,      x3rot
      REAL*8                elem_prop


c
      dimension yl(npro,nshl,ndof),        rmu(npro),
     &            shpb(npro,nshl),           shglb(npro,nsd,nshl),
     &            xlb(npro,nenl,nsd),
     &            lnode(27),                 g1yi(npro,ndof),
     &            g2yi(npro,ndof),           g3yi(npro,ndof),
     &            WdetJb(npro),              bnorm(npro,nsd),
     &            pres(npro),
     &            u1(npro),                  u2(npro),
     &            u3(npro),
     &            unm(npro),
     &            tau1n(npro),               tau2n(npro),
     &            tau3n(npro),
     &            acl(npro,nshl,ndof),       ul(npro,nshl,nsd),
     &            vdot(npro,nsd),           rlKwall(npro,nshlb,nsd),
     &            elem_prop(npro,2)
c
      dimension gl1yi(npro,ndof),          gl2yi(npro,ndof),
     &            gl3yi(npro,ndof),          dxdxib(npro,nsd,nsd),
     &            dxidxb(npro,nsd,nsd),      temp(npro),
     &            temp1(npro),               temp2(npro),
     &            temp3(npro),
     &            v1(npro,nsd),              v2(npro,nsd),
     &            v3(npro,nsd),
     &            rotnodallocal(npro,nsd,nsd),
     &            x1rot(npro,nsd),           x2rot(npro,nsd),
     &            x3rot(npro,nsd),           detJacrot(npro),
     &            B1(npro,5,3),              B2(npro,5,3),
     &            B3(npro,5,3),              Dmatrix(npro,5,5),
     &            DtimesB1(npro,5,3),        DtimesB2(npro,5,3),
     &            DtimesB3(npro,5,3),
     &            rKwall_local11(npro,nsd,nsd),
     &            rKwall_local12(npro,nsd,nsd),
     &            rKwall_local13(npro,nsd,nsd),
     &            rKwall_local21(npro,nsd,nsd),
     &            rKwall_local22(npro,nsd,nsd),
     &            rKwall_local23(npro,nsd,nsd),
     &            rKwall_local31(npro,nsd,nsd),
     &            rKwall_local32(npro,nsd,nsd),
     &            rKwall_local33(npro,nsd,nsd),
     &            rKwall_glob11(npro,nsd,nsd),
     &            rKwall_glob12(npro,nsd,nsd),
     &            rKwall_glob13(npro,nsd,nsd),
     &            rKwall_glob21(npro,nsd,nsd),
     &            rKwall_glob22(npro,nsd,nsd),
     &            rKwall_glob23(npro,nsd,nsd),
     &            rKwall_glob31(npro,nsd,nsd),
     &            rKwall_glob32(npro,nsd,nsd),
     &            rKwall_glob33(npro,nsd,nsd)
c
      dimension   rKwall_glob(npro,9,nshl,nshl),
     &            xKebe(npro,9,nshl,nshl)
c
      real*8      lhmFctvw, tsFctvw(npro)

      dimension   tmp1(npro)
c
      real*8    Turb(npro),                xki,
     &            xki3,                      fv1
c
      integer   e, i, j
c
      integer   aa, b


c
c.... ------------------->  integration variables  <--------------------
c
c.... compute the primitive variables at the integration point
c

      pres = zero
      u1   = zero
      u2   = zero
      u3   = zero
c

      do n = 1, nshlb
         nodlcl = lnode(n)
c
         pres = pres + shpb(:,nodlcl) * yl(:,nodlcl,1)
         u1   = u1   + shpb(:,nodlcl) * yl(:,nodlcl,2)
         u2   = u2   + shpb(:,nodlcl) * yl(:,nodlcl,3)
         u3   = u3   + shpb(:,nodlcl) * yl(:,nodlcl,4)

      enddo
c
c.... ---------------------->  Element Metrics  <-----------------------
c
c.... compute the deformation gradient
c
      dxdxib = zero
c
      do n = 1, nenl
         dxdxib(:,1,1) = dxdxib(:,1,1) + xlb(:,n,1) * shglb(:,1,n)
         dxdxib(:,1,2) = dxdxib(:,1,2) + xlb(:,n,1) * shglb(:,2,n)
         dxdxib(:,1,3) = dxdxib(:,1,3) + xlb(:,n,1) * shglb(:,3,n)
         dxdxib(:,2,1) = dxdxib(:,2,1) + xlb(:,n,2) * shglb(:,1,n)
         dxdxib(:,2,2) = dxdxib(:,2,2) + xlb(:,n,2) * shglb(:,2,n)
         dxdxib(:,2,3) = dxdxib(:,2,3) + xlb(:,n,2) * shglb(:,3,n)
         dxdxib(:,3,1) = dxdxib(:,3,1) + xlb(:,n,3) * shglb(:,1,n)
         dxdxib(:,3,2) = dxdxib(:,3,2) + xlb(:,n,3) * shglb(:,2,n)
         dxdxib(:,3,3) = dxdxib(:,3,3) + xlb(:,n,3) * shglb(:,3,n)
      enddo
c
c.... compute the normal to the boundary. This is achieved by taking
c     the cross product of two vectors in the plane of the 2-d
c     boundary face.
c
      if(lcsyst.eq.1) then      ! set to curl into element all others out
         ipt2=2
         ipt3=3
      elseif(lcsyst.eq.2) then
         ipt2=4
         ipt3=2
      elseif(lcsyst.eq.3) then
         ipt2=3
         ipt3=2
      elseif(lcsyst.eq.4) then
         ipt2=2
         ipt3=4
      elseif(lcsyst.eq.5) then
         ipt2=4
         ipt3=2
      elseif(lcsyst.eq.6) then
         ipt2=2
         ipt3=5
      endif
      v1 = xlb(:,ipt2,:) - xlb(:,1,:)
      v2 = xlb(:,ipt3,:) - xlb(:,1,:)
c
c compute cross product
c
      temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
      temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
      temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)
c
c mag is area for quads, twice area for tris
c
      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      bnorm(:,1) = temp1 * temp
      bnorm(:,2) = temp2 * temp
      bnorm(:,3) = temp3 * temp
c

      if (lcsyst .eq. 1) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 2) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 3) then
         WdetJb     = Qwtb(lcsyst,intp) / (two*temp)
      elseif (lcsyst .eq. 4) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 5) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 6) then
         WdetJb     = Qwtb(lcsyst,intp) / (two*temp)
      endif
c
c.... -------------------------->  Grad-V  <----------------------------
c
c.... compute grad-v for Navier-Stokes terms
c
      if (Navier .eq. 1) then
c
c.... compute the inverse of deformation gradient
c
         dxidxb(:,1,1) =   dxdxib(:,2,2) * dxdxib(:,3,3)
     &        - dxdxib(:,3,2) * dxdxib(:,2,3)
         dxidxb(:,1,2) =   dxdxib(:,3,2) * dxdxib(:,1,3)
     &        - dxdxib(:,1,2) * dxdxib(:,3,3)
         dxidxb(:,1,3) =   dxdxib(:,1,2) * dxdxib(:,2,3)
     &        - dxdxib(:,1,3) * dxdxib(:,2,2)
         temp          = one / ( dxidxb(:,1,1) * dxdxib(:,1,1)
     &        + dxidxb(:,1,2) * dxdxib(:,2,1)
     &        + dxidxb(:,1,3) * dxdxib(:,3,1) )
         dxidxb(:,1,1) =  dxidxb(:,1,1) * temp
         dxidxb(:,1,2) =  dxidxb(:,1,2) * temp
         dxidxb(:,1,3) =  dxidxb(:,1,3) * temp
         dxidxb(:,2,1) = (dxdxib(:,2,3) * dxdxib(:,3,1)
     &        - dxdxib(:,2,1) * dxdxib(:,3,3)) * temp
         dxidxb(:,2,2) = (dxdxib(:,1,1) * dxdxib(:,3,3)
     &        - dxdxib(:,3,1) * dxdxib(:,1,3)) * temp
         dxidxb(:,2,3) = (dxdxib(:,2,1) * dxdxib(:,1,3)
     &        - dxdxib(:,1,1) * dxdxib(:,2,3)) * temp
         dxidxb(:,3,1) = (dxdxib(:,2,1) * dxdxib(:,3,2)
     &        - dxdxib(:,2,2) * dxdxib(:,3,1)) * temp
         dxidxb(:,3,2) = (dxdxib(:,3,1) * dxdxib(:,1,2)
     &        - dxdxib(:,1,1) * dxdxib(:,3,2)) * temp
         dxidxb(:,3,3) = (dxdxib(:,1,1) * dxdxib(:,2,2)
     &        - dxdxib(:,1,2) * dxdxib(:,2,1)) * temp
c
c.... compute local-grad-Y
c
         gl1yi = zero
         gl2yi = zero
         gl3yi = zero
c
         do n = 1, nshl
            gl1yi(:,1) = gl1yi(:,1) + shglb(:,1,n) * yl(:,n,1)
            gl1yi(:,2) = gl1yi(:,2) + shglb(:,1,n) * yl(:,n,2)
            gl1yi(:,3) = gl1yi(:,3) + shglb(:,1,n) * yl(:,n,3)
            gl1yi(:,4) = gl1yi(:,4) + shglb(:,1,n) * yl(:,n,4)
c
            gl2yi(:,1) = gl2yi(:,1) + shglb(:,2,n) * yl(:,n,1)
            gl2yi(:,2) = gl2yi(:,2) + shglb(:,2,n) * yl(:,n,2)
            gl2yi(:,3) = gl2yi(:,3) + shglb(:,2,n) * yl(:,n,3)
            gl2yi(:,4) = gl2yi(:,4) + shglb(:,2,n) * yl(:,n,4)
c
            gl3yi(:,1) = gl3yi(:,1) + shglb(:,3,n) * yl(:,n,1)
            gl3yi(:,2) = gl3yi(:,2) + shglb(:,3,n) * yl(:,n,2)
            gl3yi(:,3) = gl3yi(:,3) + shglb(:,3,n) * yl(:,n,3)
            gl3yi(:,4) = gl3yi(:,4) + shglb(:,3,n) * yl(:,n,4)
         enddo
c
c.... convert local-grads to global-grads
c
         g1yi(:,2) = dxidxb(:,1,1) * gl1yi(:,2) +
     &        dxidxb(:,2,1) * gl2yi(:,2) +
     &        dxidxb(:,3,1) * gl3yi(:,2)
         g2yi(:,2) = dxidxb(:,1,2) * gl1yi(:,2) +
     &        dxidxb(:,2,2) * gl2yi(:,2) +
     &        dxidxb(:,3,2) * gl3yi(:,2)
         g3yi(:,2) = dxidxb(:,1,3) * gl1yi(:,2) +
     &        dxidxb(:,2,3) * gl2yi(:,2) +
     &        dxidxb(:,3,3) * gl3yi(:,2)
c
         g1yi(:,3) = dxidxb(:,1,1) * gl1yi(:,3) +
     &        dxidxb(:,2,1) * gl2yi(:,3) +
     &        dxidxb(:,3,1) * gl3yi(:,3)
         g2yi(:,3) = dxidxb(:,1,2) * gl1yi(:,3) +
     &        dxidxb(:,2,2) * gl2yi(:,3) +
     &        dxidxb(:,3,2) * gl3yi(:,3)
         g3yi(:,3) = dxidxb(:,1,3) * gl1yi(:,3) +
     &        dxidxb(:,2,3) * gl2yi(:,3) +
     &        dxidxb(:,3,3) * gl3yi(:,3)
c
         g1yi(:,4) = dxidxb(:,1,1) * gl1yi(:,4) +
     &        dxidxb(:,2,1) * gl2yi(:,4) +
     &        dxidxb(:,3,1) * gl3yi(:,4)
         g2yi(:,4) = dxidxb(:,1,2) * gl1yi(:,4) +
     &        dxidxb(:,2,2) * gl2yi(:,4) +
     &        dxidxb(:,3,2) * gl3yi(:,4)
         g3yi(:,4) = dxidxb(:,1,3) * gl1yi(:,4) +
     &        dxidxb(:,2,3) * gl2yi(:,4) +
     &        dxidxb(:,3,3) * gl3yi(:,4)
c
c.... end grad-v
c
      endif

c
c.... mass flux
c
      unm = bnorm(:,1) * u1 +bnorm(:,2) * u2  +bnorm(:,3) * u3
! no rho in continuity eq.


c
c.... viscous flux
c
      tau1n = bnorm(:,1) * two * rmu *  g1yi(:,2)
     &     + bnorm(:,2) *      (rmu * (g2yi(:,2) + g1yi(:,3)))
     &     + bnorm(:,3) *      (rmu * (g3yi(:,2) + g1yi(:,4)))
      tau2n = bnorm(:,1) *      (rmu * (g2yi(:,2) + g1yi(:,3)))
     &     + bnorm(:,2) * two * rmu *  g2yi(:,3)
     &     + bnorm(:,3) *      (rmu * (g3yi(:,3) + g2yi(:,4)))
      tau3n = bnorm(:,1) *      (rmu * (g3yi(:,2) + g1yi(:,4)))
     &     + bnorm(:,2) *      (rmu * (g3yi(:,3) + g2yi(:,4)))
     &     + bnorm(:,3) * two * rmu *  g3yi(:,4)
c
      temp1 = bnorm(:,1) * tau1n
     &     + bnorm(:,2) * tau2n
     &     + bnorm(:,3) * tau3n

      pres  = pres - temp1

      tau1n = tau1n - bnorm(:,1) * temp1
      tau2n = tau2n - bnorm(:,2) * temp1
      tau3n = tau3n - bnorm(:,3) * temp1

      vdot = zero
      rlKwall = zero
      if (intp.eq.ngaussb)   then    ! do this only for the last gauss point
        rKwall_glob = zero
      endif

      if(ideformwall.eq.1) then
      do n = 1, nshlb
         nodlcl = lnode(n)
c
         vdot(:,1) = vdot(:,1) + shpb(:,nodlcl) * acl(:,nodlcl,2)
         vdot(:,2) = vdot(:,2) + shpb(:,nodlcl) * acl(:,nodlcl,3)
         vdot(:,3) = vdot(:,3) + shpb(:,nodlcl) * acl(:,nodlcl,4)

      enddo


c        !  vdot = vdot * thicknessvw * rhovw
        !e3bvar2.f is for ivarwallprop=1
         do n=1,nsd
         !  elem_prop(:,1) variable thickness
         vdot(:,n) = vdot(:,n) * elem_prop(:,1)*rhovw
         enddo



c
c.... --------------------->  Stiffness matrix & residual  <-----------------
c
c.... B^t * D * B formulation for plane stress enhanced membrane
c
c
c.... rotation matrix
c
      v1 = xlb(:,ipt2,:) - xlb(:,1,:)
      temp       = one / sqrt ( v1(:,1)**2 + v1(:,2)**2 + v1(:,3)**2 )
      v1(:,1) = v1(:,1) * temp
      v1(:,2) = v1(:,2) * temp
      v1(:,3) = v1(:,3) * temp

      v2 = xlb(:,ipt3,:) - xlb(:,1,:)

c     compute cross product
      temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
      temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
      temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)

      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      v3(:,1) = temp1 * temp
      v3(:,2) = temp2 * temp
      v3(:,3) = temp3 * temp

c     cross product again for v2
      temp1 = v3(:,2) * v1(:,3) - v1(:,2) * v3(:,3)
      temp2 = v1(:,1) * v3(:,3) - v3(:,1) * v1(:,3)
      temp3 = v3(:,1) * v1(:,2) - v1(:,1) * v3(:,2)

      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      v2(:,1) = temp1 * temp
      v2(:,2) = temp2 * temp
      v2(:,3) = temp3 * temp

      do j = 1, nsd
         rotnodallocal(:,1,j) = v1(:,j)
         rotnodallocal(:,2,j) = v2(:,j)
         rotnodallocal(:,3,j) = v3(:,j)
      enddo

c
c.... rotated coordinates
c
      x1rot = zero
      x2rot = zero
      x3rot = zero

      do i = 1, nsd
         do j = 1, nsd
            x1rot(:,i) = x1rot(:,i)+rotnodallocal(:,i,j)*xlb(:,1,j)
            x2rot(:,i) = x2rot(:,i)+rotnodallocal(:,i,j)*xlb(:,ipt2,j)
            x3rot(:,i) = x3rot(:,i)+rotnodallocal(:,i,j)*xlb(:,ipt3,j)
         enddo
      enddo

c
c.... B matrices
c
      B1 = zero
      B2 = zero
      B3 = zero
      detJacrot = (x2rot(:,1)-x1rot(:,1)) * (x3rot(:,2)-x1rot(:,2)) -
     &     (x3rot(:,1)-x1rot(:,1)) * (x2rot(:,2)-x1rot(:,2))

      B1(:,1,1) = (x2rot(:,2)-x3rot(:,2))/detJacrot(:)
      B1(:,2,2) = (x3rot(:,1)-x2rot(:,1))/detJacrot(:)
      B1(:,3,1) = (x3rot(:,1)-x2rot(:,1))/detJacrot(:)
      B1(:,3,2) = (x2rot(:,2)-x3rot(:,2))/detJacrot(:)
      B1(:,4,3) = (x2rot(:,2)-x3rot(:,2))/detJacrot(:)
      B1(:,5,3) = (x3rot(:,1)-x2rot(:,1))/detJacrot(:)

      B2(:,1,1) = (x3rot(:,2)-x1rot(:,2))/detJacrot(:)
      B2(:,2,2) = (x1rot(:,1)-x3rot(:,1))/detJacrot(:)
      B2(:,3,1) = (x1rot(:,1)-x3rot(:,1))/detJacrot(:)
      B2(:,3,2) = (x3rot(:,2)-x1rot(:,2))/detJacrot(:)
      B2(:,4,3) = (x3rot(:,2)-x1rot(:,2))/detJacrot(:)
      B2(:,5,3) = (x1rot(:,1)-x3rot(:,1))/detJacrot(:)

      B3(:,1,1) = (x1rot(:,2)-x2rot(:,2))/detJacrot(:)
      B3(:,2,2) = (x2rot(:,1)-x1rot(:,1))/detJacrot(:)
      B3(:,3,1) = (x2rot(:,1)-x1rot(:,1))/detJacrot(:)
      B3(:,3,2) = (x1rot(:,2)-x2rot(:,2))/detJacrot(:)
      B3(:,4,3) = (x1rot(:,2)-x2rot(:,2))/detJacrot(:)
      B3(:,5,3) = (x2rot(:,1)-x1rot(:,1))/detJacrot(:)

C      B1 = B1 / detJacrot
C      B2 = B2 / detJacrot
C      B3 = B3 / detJacrot

c
c.... D matrix
c
      Dmatrix = zero



      temp1 = 1.0D0/(1.0d0 - rnuvw*rnuvw)
      temp2 = rnuvw * temp1
      temp3 = pt5 * (1.0d0 - rnuvw) * temp1


      ! wall_prop_elem(:,2) is variable evw
       Dmatrix(:,1,1) = elem_prop(:,2)*temp1
       Dmatrix(:,1,2) = elem_prop(:,2)*temp2
       Dmatrix(:,2,1) = elem_prop(:,2)*temp2
       Dmatrix(:,2,2) = elem_prop(:,2)*temp1

      Dmatrix(:,3,3) = elem_prop(:,2)*temp3
      Dmatrix(:,4,4) = elem_prop(:,2)*temp3*rshearconstantvw
      Dmatrix(:,5,5) = elem_prop(:,2)*temp3*rshearconstantvw


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc this is original Dmatrix for uniform thickness and evw
c      !Dmatrix = zero
c      !temp1 = evw / (1.0d0 - rnuvw*rnuvw)
c      !temp2 = rnuvw * temp1
c      !temp3 = pt5 * (1.0d0 - rnuvw) * temp1
c      !Dmatrix(:,1,1) = temp1
c      !Dmatrix(:,1,2) = temp2
c      !Dmatrix(:,2,1) = temp2
c      !Dmatrix(:,2,2) = temp1
c      !Dmatrix(:,3,3) = temp3
c      !Dmatrix(:,4,4) = temp3*rshearconstantvw
c      !Dmatrix(:,5,5) = temp3*rshearconstantvw
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c.... D * [B1|B2|B3]
c
      DtimesB1 = zero
      DtimesB2 = zero
      DtimesB3 = zero
      do i = 1, 5
         do j = 1, 3
            do k = 1, 5
               DtimesB1(:,i,j) = DtimesB1(:,i,j)
     &              + Dmatrix(:,i,k) * B1(:,k,j)
               DtimesB2(:,i,j) = DtimesB2(:,i,j)
     &              + Dmatrix(:,i,k) * B2(:,k,j)
               DtimesB3(:,i,j) = DtimesB3(:,i,j)
     &              + Dmatrix(:,i,k) * B3(:,k,j)
            enddo
         enddo
      enddo
c
c.... [B1|B2|B3]^T * D * [B1|B2|B3]
c
      rKwall_local11 = zero
      rKwall_local12 = zero
      rKwall_local13 = zero
      rKwall_local21 = zero
      rKwall_local22 = zero
      rKwall_local23 = zero
      rKwall_local31 = zero
      rKwall_local32 = zero
      rKwall_local33 = zero

      do i = 1, 3               ! i is a node index: i=1, nenbl=3
         do j = 1, 3            ! same is true for j
            do k = 1, 5
               rKwall_local11(:,i,j)  = rKwall_local11(:,i,j)
     &              + B1(:,k,i) * DtimesB1(:,k,j)
               rKwall_local12(:,i,j)  = rKwall_local12(:,i,j)
     &              + B1(:,k,i) * DtimesB2(:,k,j)
               rKwall_local13(:,i,j)  = rKwall_local13(:,i,j)
     &              + B1(:,k,i) * DtimesB3(:,k,j)
               rKwall_local21(:,i,j)  = rKwall_local21(:,i,j)
     &              + B2(:,k,i) * DtimesB1(:,k,j)
               rKwall_local22(:,i,j)  = rKwall_local22(:,i,j)
     &              + B2(:,k,i) * DtimesB2(:,k,j)
               rKwall_local23(:,i,j)  = rKwall_local23(:,i,j)
     &              + B2(:,k,i) * DtimesB3(:,k,j)
               rKwall_local31(:,i,j)  = rKwall_local31(:,i,j)
     &              + B3(:,k,i) * DtimesB1(:,k,j)
               rKwall_local32(:,i,j)  = rKwall_local32(:,i,j)
     &              + B3(:,k,i) * DtimesB2(:,k,j)
               rKwall_local33(:,i,j)  = rKwall_local33(:,i,j)
     &              + B3(:,k,i) * DtimesB3(:,k,j)
            enddo
         enddo
      enddo

c
c.... Now we need to rotate each of these submatrices to the global frame
c
      call rotatestiff(rKwall_local11, rotnodallocal, rKwall_glob11)
      call rotatestiff(rKwall_local12, rotnodallocal, rKwall_glob12)
      call rotatestiff(rKwall_local13, rotnodallocal, rKwall_glob13)
      call rotatestiff(rKwall_local21, rotnodallocal, rKwall_glob21)
      call rotatestiff(rKwall_local22, rotnodallocal, rKwall_glob22)
      call rotatestiff(rKwall_local23, rotnodallocal, rKwall_glob23)
      call rotatestiff(rKwall_local31, rotnodallocal, rKwall_glob31)
      call rotatestiff(rKwall_local32, rotnodallocal, rKwall_glob32)
      call rotatestiff(rKwall_local33, rotnodallocal, rKwall_glob33)

c     multiply the nodal matrices by the area and the thickness
      do i =1, nsd
         do j = 1, nsd
            rKwall_glob11(:,i,j) = rKwall_glob11(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob12(:,i,j) = rKwall_glob12(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob13(:,i,j) = rKwall_glob13(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob21(:,i,j) = rKwall_glob21(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob22(:,i,j) = rKwall_glob22(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob23(:,i,j) = rKwall_glob23(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob31(:,i,j) = rKwall_glob31(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob32(:,i,j) = rKwall_glob32(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)
            rKwall_glob33(:,i,j) = rKwall_glob33(:,i,j) * detJacrot(:)
     &                           * pt5 * elem_prop(:,1)


         enddo
      enddo

c
c.... Final K * u product (in global coordinates) to get the residual
c
      do i = 1, 3               ! now i is a spatial index: i=1, nsd=3
         rlKwall(:,1,1) = rlKwall(:,1,1)
     &                  + rKwall_glob11(:,1,i) * ul(:,1,i)
     &                  + rKwall_glob12(:,1,i) * ul(:,2,i)
     &                  + rKwall_glob13(:,1,i) * ul(:,3,i)
         rlKwall(:,1,2) = rlKwall(:,1,2)
     &                  + rKwall_glob11(:,2,i) * ul(:,1,i)
     &                  + rKwall_glob12(:,2,i) * ul(:,2,i)
     &                  + rKwall_glob13(:,2,i) * ul(:,3,i)
         rlKwall(:,1,3) = rlKwall(:,1,3)
     &                  + rKwall_glob11(:,3,i) * ul(:,1,i)
     &                  + rKwall_glob12(:,3,i) * ul(:,2,i)
     &                  + rKwall_glob13(:,3,i) * ul(:,3,i)
         rlKwall(:,2,1) = rlKwall(:,2,1)
     &                  + rKwall_glob21(:,1,i) * ul(:,1,i)
     &                  + rKwall_glob22(:,1,i) * ul(:,2,i)
     &                  + rKwall_glob23(:,1,i) * ul(:,3,i)
         rlKwall(:,2,2) = rlKwall(:,2,2)
     &                  + rKwall_glob21(:,2,i) * ul(:,1,i)
     &                  + rKwall_glob22(:,2,i) * ul(:,2,i)
     &                  + rKwall_glob23(:,2,i) * ul(:,3,i)
         rlKwall(:,2,3) = rlKwall(:,2,3)
     &                  + rKwall_glob21(:,3,i) * ul(:,1,i)
     &                  + rKwall_glob22(:,3,i) * ul(:,2,i)
     &                  + rKwall_glob23(:,3,i) * ul(:,3,i)
         rlKwall(:,3,1) = rlKwall(:,3,1)
     &                  + rKwall_glob31(:,1,i) * ul(:,1,i)
     &                  + rKwall_glob32(:,1,i) * ul(:,2,i)
     &                  + rKwall_glob33(:,1,i) * ul(:,3,i)
         rlKwall(:,3,2) = rlKwall(:,3,2)
     &                  + rKwall_glob31(:,2,i) * ul(:,1,i)
     &                  + rKwall_glob32(:,2,i) * ul(:,2,i)
     &                  + rKwall_glob33(:,2,i) * ul(:,3,i)
         rlKwall(:,3,3) = rlKwall(:,3,3)
     &                  + rKwall_glob31(:,3,i) * ul(:,1,i)
     &                  + rKwall_glob32(:,3,i) * ul(:,2,i)
     &                  + rKwall_glob33(:,3,i) * ul(:,3,i)
      enddo
c
c.... --------------> End of Stiffness matrix & residual  <-----------------
c

c
c.... -----> Wall Stiffness and Mass matrices for implicit LHS  <-----------
c

c....  Here we just add the mass matrix contribution.  The stiffness contribution
c....  is added in e3b

c      lhmFct = almi * (one - flmpl)      Maybe we have to define flmplW: lumped
                                        ! mass parameter for the wall
      lhmFctvw = almi * (one - flmpl)
c
c.... scale variables for efficiency
c

      tsFctvw     = lhmFctvw * WdetJb * rhovw * elem_prop(:,1)

c     tsFctvw     = lhmFctvw * WdetJb * rhovw * thicknessvw

c
c.... compute mass and convection terms
c
c.... NOTE:  the wall mass contributions should only have 3 nodal components
c.... since the fourth node is an interior node... therefore, the loops should
c.... be done from 1 to nshlb=3...

      do b = 1, nshlb
         do aa = 1, nshlb
            tmp1 = tsFctvw * shpb(:,aa) * shpb(:,b)
c
c           tmp1=alpha_m*(1-lmp)*WdetJ*N^aN^b*rho*thickness   the time term
c
            xKebe(:,1,aa,b) = xKebe(:,1,aa,b) + tmp1
            xKebe(:,5,aa,b) = xKebe(:,5,aa,b) + tmp1
            xKebe(:,9,aa,b) = xKebe(:,9,aa,b) + tmp1
         enddo
      enddo

c
c.... assemble the nodal stiffness into the element stiffness matrix rKwall_glob
c
c.... We have passed the integer intp to make this operation only once: we are
c.... not using the gauss points structure to compute the stiffness of the wall
c.... elements, so we don't want to be redundant and calculate ngaussb times the
c.... stiffness matrix which is constant for linear triangles...

c.... This is ugly, but I will fix it later...

      if (intp.eq.ngaussb)   then    ! do this only for the last gauss point
        rKwall_glob(:,1,1,1) = rKwall_glob11(:,1,1)
        rKwall_glob(:,2,1,1) = rKwall_glob11(:,1,2)
        rKwall_glob(:,3,1,1) = rKwall_glob11(:,1,3)
        rKwall_glob(:,4,1,1) = rKwall_glob11(:,2,1)
        rKwall_glob(:,5,1,1) = rKwall_glob11(:,2,2)
        rKwall_glob(:,6,1,1) = rKwall_glob11(:,2,3)
        rKwall_glob(:,7,1,1) = rKwall_glob11(:,3,1)
        rKwall_glob(:,8,1,1) = rKwall_glob11(:,3,2)
        rKwall_glob(:,9,1,1) = rKwall_glob11(:,3,3)

        rKwall_glob(:,1,1,2) = rKwall_glob12(:,1,1)
        rKwall_glob(:,2,1,2) = rKwall_glob12(:,1,2)
        rKwall_glob(:,3,1,2) = rKwall_glob12(:,1,3)
        rKwall_glob(:,4,1,2) = rKwall_glob12(:,2,1)
        rKwall_glob(:,5,1,2) = rKwall_glob12(:,2,2)
        rKwall_glob(:,6,1,2) = rKwall_glob12(:,2,3)
        rKwall_glob(:,7,1,2) = rKwall_glob12(:,3,1)
        rKwall_glob(:,8,1,2) = rKwall_glob12(:,3,2)
        rKwall_glob(:,9,1,2) = rKwall_glob12(:,3,3)

        rKwall_glob(:,1,1,3) = rKwall_glob13(:,1,1)
        rKwall_glob(:,2,1,3) = rKwall_glob13(:,1,2)
        rKwall_glob(:,3,1,3) = rKwall_glob13(:,1,3)
        rKwall_glob(:,4,1,3) = rKwall_glob13(:,2,1)
        rKwall_glob(:,5,1,3) = rKwall_glob13(:,2,2)
        rKwall_glob(:,6,1,3) = rKwall_glob13(:,2,3)
        rKwall_glob(:,7,1,3) = rKwall_glob13(:,3,1)
        rKwall_glob(:,8,1,3) = rKwall_glob13(:,3,2)
        rKwall_glob(:,9,1,3) = rKwall_glob13(:,3,3)

        rKwall_glob(:,1,2,1) = rKwall_glob21(:,1,1)
        rKwall_glob(:,2,2,1) = rKwall_glob21(:,1,2)
        rKwall_glob(:,3,2,1) = rKwall_glob21(:,1,3)
        rKwall_glob(:,4,2,1) = rKwall_glob21(:,2,1)
        rKwall_glob(:,5,2,1) = rKwall_glob21(:,2,2)
        rKwall_glob(:,6,2,1) = rKwall_glob21(:,2,3)
        rKwall_glob(:,7,2,1) = rKwall_glob21(:,3,1)
        rKwall_glob(:,8,2,1) = rKwall_glob21(:,3,2)
        rKwall_glob(:,9,2,1) = rKwall_glob21(:,3,3)

        rKwall_glob(:,1,2,2) = rKwall_glob22(:,1,1)
        rKwall_glob(:,2,2,2) = rKwall_glob22(:,1,2)
        rKwall_glob(:,3,2,2) = rKwall_glob22(:,1,3)
        rKwall_glob(:,4,2,2) = rKwall_glob22(:,2,1)
        rKwall_glob(:,5,2,2) = rKwall_glob22(:,2,2)
        rKwall_glob(:,6,2,2) = rKwall_glob22(:,2,3)
        rKwall_glob(:,7,2,2) = rKwall_glob22(:,3,1)
        rKwall_glob(:,8,2,2) = rKwall_glob22(:,3,2)
        rKwall_glob(:,9,2,2) = rKwall_glob22(:,3,3)

        rKwall_glob(:,1,2,3) = rKwall_glob23(:,1,1)
        rKwall_glob(:,2,2,3) = rKwall_glob23(:,1,2)
        rKwall_glob(:,3,2,3) = rKwall_glob23(:,1,3)
        rKwall_glob(:,4,2,3) = rKwall_glob23(:,2,1)
        rKwall_glob(:,5,2,3) = rKwall_glob23(:,2,2)
        rKwall_glob(:,6,2,3) = rKwall_glob23(:,2,3)
        rKwall_glob(:,7,2,3) = rKwall_glob23(:,3,1)
        rKwall_glob(:,8,2,3) = rKwall_glob23(:,3,2)
        rKwall_glob(:,9,2,3) = rKwall_glob23(:,3,3)

        rKwall_glob(:,1,3,1) = rKwall_glob31(:,1,1)
        rKwall_glob(:,2,3,1) = rKwall_glob31(:,1,2)
        rKwall_glob(:,3,3,1) = rKwall_glob31(:,1,3)
        rKwall_glob(:,4,3,1) = rKwall_glob31(:,2,1)
        rKwall_glob(:,5,3,1) = rKwall_glob31(:,2,2)
        rKwall_glob(:,6,3,1) = rKwall_glob31(:,2,3)
        rKwall_glob(:,7,3,1) = rKwall_glob31(:,3,1)
        rKwall_glob(:,8,3,1) = rKwall_glob31(:,3,2)
        rKwall_glob(:,9,3,1) = rKwall_glob31(:,3,3)

        rKwall_glob(:,1,3,2) = rKwall_glob32(:,1,1)
        rKwall_glob(:,2,3,2) = rKwall_glob32(:,1,2)
        rKwall_glob(:,3,3,2) = rKwall_glob32(:,1,3)
        rKwall_glob(:,4,3,2) = rKwall_glob32(:,2,1)
        rKwall_glob(:,5,3,2) = rKwall_glob32(:,2,2)
        rKwall_glob(:,6,3,2) = rKwall_glob32(:,2,3)
        rKwall_glob(:,7,3,2) = rKwall_glob32(:,3,1)
        rKwall_glob(:,8,3,2) = rKwall_glob32(:,3,2)
        rKwall_glob(:,9,3,2) = rKwall_glob32(:,3,3)

        rKwall_glob(:,1,3,3) = rKwall_glob33(:,1,1)
        rKwall_glob(:,2,3,3) = rKwall_glob33(:,1,2)
        rKwall_glob(:,3,3,3) = rKwall_glob33(:,1,3)
        rKwall_glob(:,4,3,3) = rKwall_glob33(:,2,1)
        rKwall_glob(:,5,3,3) = rKwall_glob33(:,2,2)
        rKwall_glob(:,6,3,3) = rKwall_glob33(:,2,3)
        rKwall_glob(:,7,3,3) = rKwall_glob33(:,3,1)
        rKwall_glob(:,8,3,3) = rKwall_glob33(:,3,2)
        rKwall_glob(:,9,3,3) = rKwall_glob33(:,3,3)

        rKwall_glob = rKwall_glob*betai*Delt(itseq)*Delt(itseq)*alfi

      else
c....   nothing happens
        goto 123
      endif

123   continue

      endif
c
c.... return
c
      return
      end

      SUBROUTINE local_elemwallprop(rlocal,elem_prop)
      INCLUDE "global.h"
      INCLUDE "common_blocks/propar.h"
      INCLUDE "common_blocks/shpdat.h"
      INCLUDE "common_blocks/nomodule.h"

      INTEGER i,k,j
      REAL*8 elem_prop,rlocal
      DIMENSION rlocal(npro,nshl,2), elem_prop(npro,2)
      REAL*8, DIMENSION(3,2):: f

      DO i = 1,npro
        k = 0
c        DO j=1,nshl
c         IF (rlocal(i,j,1)>0.0D0 .AND. k<4) THEN
c         some element may have all four nodes on the wall
        DO j=1,nshlb
          IF (rlocal(i,j,1)>0.0D0) THEN
            k=k+1
            f(k,:)=rlocal(i,j,:)
          END IF
        END DO

        IF(k == 3) THEN
          elem_prop(i,:)=(f(1,:)+f(2,:)+f(3,:))/3D0
        ELSE
          elem_prop(i,1) = thicknessvw
          elem_prop(i,2) = evw
        END IF
      END DO
      END SUBROUTINE

#endif
