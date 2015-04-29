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
c---------------------------------------------------------------------
c     
c     drvftools.f : Bundle of Fortran driver routines for ftools.f
c     
c     Each routine is to be called by les**.c
c     
c---------------------------------------------------------------------
c     
c----------------
c     drvLesPrepDiag
c----------------
c     
      subroutine drvlesPrepDiag ( flowDiag, ilwork,
     &                            iBC,      BC,      iper,
     &                            rowp,     colm,    
     &                            lhsK,     lhsP)
c     
      use pointer_data
      use pvsQbi
      use convolImpFlow !brings in the current part of convol coef for imp BC
      use convolRCRFlow !brings in the current part of convol coef for RCR BC
      use LagrangeMultipliers 

        include "global.h"
        include "mpif.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                bc,          flowdiag
C
C     Local variables
C
      INTEGER             i,           irankcoupled,             j
C
      REAL*8                tfact
c     
      dimension flowDiag(nshg,4), ilwork(nlwork)
      dimension iBC(nshg), iper(nshg), BC(nshg,ndofBC)
      real*8 lhsK(9,nnz_tot), lhsP(4,nnz_tot)
      integer rowp(nnz_tot),  colm(nshg+1)
      integer n, k
c
      integer sparseloc
c
c     
c.... Clear the flowdiag
c
      if((flmpl.eq.1).or.(ipord.gt.1)) then
         do n = 1, nshg
          k = sparseloc( rowp(colm(n)), colm(n+1)-colm(n), n )
     &       + colm(n)-1
c     
          flowdiag(n,1) = lhsK(1,k)
          flowdiag(n,2) = lhsK(5,k)
          flowdiag(n,3) = lhsK(9,k)
c     
          flowdiag(n,4) = lhsP(4,k)
         enddo
      else
          flowDiag = zero
          do n = 1, nshg  ! rowsum put on the diagonal instead of diag entry
           do k=colm(n),colm(n+1)-1

c
              flowdiag(n,1) = flowdiag(n,1) + abs(lhsK(1,k)) 
c     &                          + lhsK(2,k) + lhsK(3,k)
              flowdiag(n,2) = flowdiag(n,2) + abs(lhsK(5,k)) 
c     &                          + lhsK(4,k) + lhsK(6,k)
              flowdiag(n,3) = flowdiag(n,3) + abs(lhsK(9,k)) 
c     &                          + lhsK(7,k) + lhsK(8,k)
c
              flowdiag(n,4) = flowdiag(n,4) + abs(lhsP(4,k)) 
           enddo
           flowdiag(n,:)=flowdiag(n,:)*pt33
      enddo
      endif
      if(ipvsq.ge.3) then ! for first cut only do diagonal extraction
 ! this is not yet correct for multi procs I suspect if partition
 ! boundary cuts a p=QR face
         tfact=alfi * gami * Delt(1)
         do n=1,nshg
            if(numResistSrfs.gt.zero) then
               do k = 1,numResistSrfs
                  if (nsrflistResist(k).eq.ndsurf(n)) then
                     irankCoupled=k      
                     flowdiag(n,1:3) = flowdiag(n,1:3)
     &               + tfact*ValueListResist(irankCoupled)*
     &               NABI(n,:)*NABI(n,:)
                  endif
               enddo
            elseif(numImpSrfs.gt.zero) then
               do k = 1,numImpSrfs
                  if (nsrflistImp(k).eq.ndsurf(n)) then
                     irankCoupled=k      
                     flowdiag(n,1:3) = flowdiag(n,1:3)
     &               + tfact*ImpConvCoef(ntimeptpT+2,irankCoupled)*
     &               NABI(n,:)*NABI(n,:)
                  endif
               enddo
            elseif(numRCRSrfs.gt.zero) then
               do k = 1,numRCRSrfs
                  if (nsrflistRCR(k).eq.ndsurf(n)) then
                     irankCoupled=k      
                     flowdiag(n,1:3) = flowdiag(n,1:3)
     &               + tfact*RCRConvCoef(lstep+2,irankCoupled)* !check lstep+2 if restart from t.ne.0
     &               NABI(n,:)*NABI(n,:)
                  endif
               enddo
            endif
         enddo
      endif
c
c.... Now I am adding contributions from the Lagrange multipliers to preconditioning
c 
c     
      if(Lagrange .gt. zero) then
         tfact=alfi * gami * Delt(1)
         call LagAddDiag (flowDiag, tfact)
      endif

c
      if(iabc==1)    !are there any axisym bc's
     &      call rotabc(flowdiag, iBC, 'in ')
c

c     
c.... communicate : add the slaves part to the master's part of flowDiag
c     
      if (numpe > 1) then 
           call commu (flowDiag, ilwork, nflow, 'in ') 
        endif
c
c.... satisfy the boundary conditions on the diagonal
c
        call bc3diag(iBC, BC,  flowDiag)
c
c     
c.... on processor periodicity was not taken care of in the setting of the 
c     boundary conditions on the matrix.  Take care of it now.
c
        call bc3per(iBC,  flowDiag, iper, ilwork, 4)
c
c... slaves and masters have the correct values
c
c     
c.... Calculate square root
c     
        do i = 1, nshg
           do j = 1, nflow
              if (flowDiag(i,j).ne.0) 
     &             flowDiag(i,j) = 1. / sqrt(abs(flowDiag(i,j)))
           enddo
        enddo
c     
        return
        end

c     
c-------------
c     drvsclrDiag
c-------------
c     
      subroutine drvsclrDiag ( sclrDiag, ilwork, iBC, BC, iper, 
     &                         rowp,     colm,   lhsS )
c     
      use pointer_data

        include "global.h"
        include "mpif.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/workfc.h"
C
C     Local variables
C
      INTEGER             i,           k,           n
C
      integer  ilwork(nlwork),    iBC(nshg),     iper(nshg),
     &         rowp(nnz_tot),    colm(nshg+1)

      real*8   sclrDiag(nshg),    lhsS(nnz_tot), BC(nshg,ndofBC)
      integer sparseloc

      sclrDiag = zero
      do n = 1, nshg
         k = sparseloc( rowp(colm(n)), colm(n+1)-colm(n), n ) 
     &                               + colm(n)-1
c
         sclrDiag(n) = lhsS(k)
      enddo
c     
c.... communicate : add the slaves part to the master's part of sclrDiag
c     
      if (numpe > 1) then 
           call commu (sclrDiag, ilwork, 1, 'in ') 
        endif
c
c.... satisfy the boundary conditions on the diagonal
c
        call bc3SclrDiag(iBC,  sclrDiag)
c
c     
c.... on processor periodicity was not taken care of in the setting of the 
c     boundary conditions on the matrix.  Take care of it now.
c
        call bc3per(iBC,  sclrDiag, iper, ilwork, 1)
c
c... slaves and masters have the correct values
c
c     
c.... Calculate square root
c     
        do i = 1, nshg
           if (sclrDiag(i).ne.0) then
              sclrDiag(i) = 1. / sqrt(abs(sclrDiag(i)))
           endif
        enddo
c     
      return
      end

C============================================================================
C
C "fLesSparseApG":
C
C============================================================================
      subroutine fLesSparseApG(      col,      row,      pLhs,      
     &                                p,      q,      nNodes,
     &                                  nnz_tot )
c
c.... Data declaration
c
      implicit none
      integer      nNodes, nnz_tot
      integer      col(nNodes+1),      row(nnz_tot)
      real*8      pLhs(4,nnz_tot),      p(nNodes),      q(nNodes,3)
c
      real*8      pisave
      integer      i,      j,      k
c
c.... clear the vector
c
      do i = 1, nNodes
          q(i,1) = 0
          q(i,2) = 0
          q(i,3) = 0
      enddo
c
c.... Do an AP product
c
      do i = 1, nNodes
c
          pisave = p(i)
cdir$ ivdep
          do k = col(i), col(i+1)-1
            j = row(k) 
c
            q(j,1) = q(j,1) - pLhs(1,k) * pisave
            q(j,2) = q(j,2) - pLhs(2,k) * pisave
            q(j,3) = q(j,3) - pLhs(3,k) * pisave
          enddo
      enddo
c
c.... end
c
      return
      end

C============================================================================
C
C "fLesSparseApKG":
C
C============================================================================

      subroutine fLesSparseApKG( col, row, kLhs, pLhs,
     1                             p,   q, nNodes,
     2                                  nnz_tot_hide ) 
c
c.... Data declaration
c
c      implicit none
      use pvsQbi
      use LagrangeMultipliers 

        include "global.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/timdat.h"
        include "common_blocks/conpar.h"
C
C     Argument variables
C
      INTEGER             nnz_tot_hide
C
C     Local variables
C
      REAL*8                tfact
C
      integer nNodes
      integer col(nNodes+1), row(nnz_tot)
      real*8  kLhs(9,nnz_tot), pLhs(4,nnz_tot)
      real*8  p(nNodes,4), q(nNodes,3)
      real*8      tmp1,      tmp2,      tmp3,      pisave
      integer      i,      j,      k
c
c.... clear the vector
c
      do i = 1, nNodes
          q(i,1) = 0
          q(i,2) = 0
          q(i,3) = 0
      enddo
c
c.... Do an AP product
c
      do i = 1, nNodes
c
          tmp1 = 0
          tmp2 = 0
          tmp3 = 0
          pisave   = p(i,4)
cdir$ ivdep
          do k = col(i), col(i+1)-1
            j = row(k) 
            tmp1 = tmp1
     1            + kLhs(1,k) * p(j,1)
     2            + kLhs(4,k) * p(j,2)
     3            + kLhs(7,k) * p(j,3)
            tmp2 = tmp2
     1            + kLhs(2,k) * p(j,1)
     2            + kLhs(5,k) * p(j,2)
     3            + kLhs(8,k) * p(j,3)
            tmp3 = tmp3
     1            + kLhs(3,k) * p(j,1)
     2            + kLhs(6,k) * p(j,2)
     3            + kLhs(9,k) * p(j,3)
c
            q(j,1) = q(j,1) - pLhs(1,k) * pisave
            q(j,2) = q(j,2) - pLhs(2,k) * pisave
            q(j,3) = q(j,3) - pLhs(3,k) * pisave
          enddo
          q(i,1) = q(i,1) + tmp1
          q(i,2) = q(i,2) + tmp2
          q(i,3) = q(i,3) + tmp3
      enddo

      if (Lagrange .gt. 0) then
         LagSwitch = 1
         call CalcNANBLagrange(col, row, p(:,1:3))
      endif

      if(ipvsq.ge.2) then
         tfact=alfi * gami * Delt(1)
         call ElmpvsQ(q,p,tfact)
      endif
c
c.... end
c
      return
      end


C============================================================================
C
C "fLesSparseApNGt":
C
C============================================================================

      subroutine fLesSparseApNGt(      col,      row,      pLhs,      
     1                              p,   q, nNodes,
     2                                  nnz_tot   )
c
c.... Data declaration
c
      implicit none
      integer nNodes, nnz_tot
      integer col(nNodes+1), row(nnz_tot)
      real*8 pLhs(4,nnz_tot), p(nNodes,3), q(nNodes)
c
      real*8      tmp
      integer      i,      j,      k
c
c.... Do an AP product
c
      do i = nNodes, 1, -1
c
          tmp = 0
          do k = col(i), col(i+1)-1
            j = row(k)
c
            tmp = tmp
     1             + pLhs(1,k) * p(j,1)
     2             + pLhs(2,k) * p(j,2)
     3             + pLhs(3,k) * p(j,3)
          enddo
          q(i) = tmp
      enddo
c
c.... end
c
      return
      end

C============================================================================
C
C "fLesSparseApNGtC":
C
C============================================================================

      subroutine fLesSparseApNGtC(      col,      row,      pLhs,      
     1                              p,      q,      nNodes,
     2                                  nnz_tot )
c
c.... Data declaration
c
      implicit none
      integer nNodes, nnz_tot
      integer col(nNodes+1), row(20*nNodes)
      real*8 pLhs(4,nnz_tot), p(nNodes,4), q(nNodes)
c
      real*8      tmp
      integer      i,      j,      k
c
c.... Do an AP product
c
      do i = nNodes, 1, -1
c
          tmp = 0
          do k = col(i), col(i+1)-1
            j = row(k)
c
            tmp = tmp
     1                + pLhs(1,k) * p(j,1)
     2                + pLhs(2,k) * p(j,2)
     3                + pLhs(3,k) * p(j,3)
     4                + pLhs(4,k) * p(j,4)
          enddo
          q(i) = tmp
      enddo
c
c.... end
c
      return
      end

C============================================================================
C
C "fLesSparseApFull":
C
C============================================================================

      subroutine fLesSparseApFull( col, row, kLhs, pLhs,
     1                              p, q, nNodes,
     2                                  nnz_tot_hide )
c
c.... Data declaration
c
c      implicit none
      use pvsQbi
      use LagrangeMultipliers 
        
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/timdat.h"
C
C     Argument variables
C
      INTEGER             nnz_tot_hide
C
C     Local variables
      REAL*8                tfact
C
      integer nNodes
      integer col(nNodes+1), row(nnz_tot)
      real*8 kLhs(9,nnz_tot), pLhs(4,nnz_tot)
      real*8    p(nNodes,4), q(nNodes,4)
      real*8      tmp1,      tmp2,      tmp3,      tmp4,      pisave
      integer      i,      j,      k
c
c.... clear the vector
c
      do i = 1, nNodes
          q(i,1) = 0
          q(i,2) = 0
          q(i,3) = 0
      enddo
c
c.... Do an AP product
c
      do i = 1, nNodes
c
          tmp1 = 0
          tmp2 = 0
          tmp3 = 0
          tmp4 = 0
          pisave   = p(i,4)
cdir$ ivdep
          do k = col(i), col(i+1)-1
            j = row(k)
c
            tmp1 = tmp1
     1               + kLhs(1,k) * p(j,1)
     2               + kLhs(4,k) * p(j,2)
     3               + kLhs(7,k) * p(j,3)
            tmp2 = tmp2
     1               + kLhs(2,k) * p(j,1)
     2               + kLhs(5,k) * p(j,2)
     3               + kLhs(8,k) * p(j,3)
            tmp3 = tmp3
     1               + kLhs(3,k) * p(j,1)
     2               + kLhs(6,k) * p(j,2)
     3               + kLhs(9,k) * p(j,3)
c
            tmp4 = tmp4
     1               + pLhs(1,k) * p(j,1)
     2               + pLhs(2,k) * p(j,2)
     3               + pLhs(3,k) * p(j,3)
     4               + pLhs(4,k) * p(j,4)
c
            q(j,1) = q(j,1) - pLhs(1,k) * pisave
            q(j,2) = q(j,2) - pLhs(2,k) * pisave
            q(j,3) = q(j,3) - pLhs(3,k) * pisave
          enddo
          q(i,1) = q(i,1) + tmp1
          q(i,2) = q(i,2) + tmp2
          q(i,3) = q(i,3) + tmp3
          q(i,4) = tmp4
      enddo
      
      if (Lagrange .gt. 0) then
         LagSwitch = 1
         call CalcNANBLagrange(col, row, p(:,1:3))
      endif

      if(ipvsq.ge.2) then
         tfact=alfi * gami * Delt(1)
         call ElmpvsQ(q,p,tfact)
      endif
c
c.... end
c
      return
      end

C============================================================================
C
C "fLesSparseApSclr":
C
C============================================================================

      subroutine fLesSparseApSclr(      col,      row,      lhs,      
     1                              p, q, nNodes,
     &                                  nnz_tot)
c
c.... Data declaration
c
      implicit none
      integer nNodes, nnz_tot
      integer col(nNodes+1), row(nnz_tot)
      real*8 lhs(nnz_tot), p(nNodes),      q(nNodes)
c
      real*8      tmp
      integer      i,      j,      k
c
c.... Do an AP product
c
      do i = nNodes, 1, -1
c
          tmp = 0
          do k = col(i), col(i+1)-1
            tmp = tmp + lhs(k) * p(row(k))
          enddo
          q(i) = tmp
      enddo
c
c.... end
c
      return
      end

C============================================================================
      subroutine commOut(  global,  ilwork,  n, 
     &                       iper,    iBC, BC  )
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/workfc.h"

C
C     Argument variables
C
      INTEGER             n
C
C     Local variables
C
      INTEGER             i
C
      real*8  global(nshg,n), BC(nshg,ndofBC)
      integer ilwork(nlwork), iper(nshg), iBC(nshg)
c
      if ( numpe .gt. 1) then 
         call commu ( global, ilwork, n, 'out')
      endif
c
c     before doing AP product P must be made periodic
c     on processor slaves did not get updated with the 
c     commu (out) so do it here
c
        do i=1,n
           global(:,i) = global(iper(:),i)  ! iper(i)=i if non-slave so no danger
        enddo
c
c       slave has masters value, for abc we need to rotate it
c        (if this is a vector only no SCALARS)
        if((iabc==1) .and. (n.gt.1)) !are there any axisym bc's
     &     call rotabc(global, iBC,  'out')


c$$$        do j = 1,nshg
c$$$           if (btest(iBC(j),10)) then
c$$$              i = iper(j)
c$$$              res(j,:) = res(i,:) 
c$$$           endif
c$$$        enddo
      
      return 
      end

C============================================================================
      subroutine commIn(  global,  ilwork,  n, 
     &                      iper,    iBC, BC )
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/workfc.h"

C
C     Argument variables
C
      INTEGER             n
C
      real*8  global(nshg,n), BC(nshg,ndofBC)
      integer ilwork(nlwork), iper(nshg), iBC(nshg)
c
        if((iabc==1) .and. (n.gt.1)) !are there any axisym bc's
     &       call rotabc(global, iBC, 'in ')
c

      if ( numpe .gt. 1 ) then
         call commu ( global, ilwork, n, 'in ')
      endif
            
      call bc3per ( iBC, global, iper, ilwork, n)
      
      return 
      end


C============================================================================
      subroutine LagAddDiag (flowdiag, tfact)

      use LagrangeMultipliers 
      use pvsQbi

      include "global.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/timdat.h"
      include "common_blocks/conpar.h"
C
C     Local variables
C
      INTEGER             i,           k,           n
C
      real*8    flowdiag(nshg,4), tfact, tfactSurf

c
c.... Now I am adding contributions from the Lagrange multipliers to preconditioning
c 
c     
      do n=1,nshg
         do k = 1,numLagrangeSrfs
            tfactSurf = zero
            tfactSurf = tfact * LagMeanFlow(k)
                  if (nsrflistLagrange(k).eq.ndsurf(n)) then
                     do i=1, 3  
                        flowdiag(n,i)=flowdiag(n,i)+ abs( 
     &                     tfactSurf*(-Lagalpha(k,1)
     &                     +PenaltyCoeff(k,1)*Penalty(k,1))
     &                     *(NANBIJ(n,i,1)-two*NABI(n,i)*PNABI(n,i)
     &                     /LagProfileArea(k)+NABI(n,i)*NABI(n,i)*
     &                     ProfileDelta(k))+tfactSurf
     &                     *LagMeanFlow(k)*PenaltyCoeff(k,1)
     &                     *(NANBLagrange(1,n,i)-PQLagrange(k,1)*
     &                     NABI(n,i)-QLagrange(k,1)*PNABI(n,i)
     &                     /LagProfileArea(k)+QLagrange(k,1)*
     &                     NABI(n,i)*ProfileDelta(k))**2
     &                     +tfactSurf*(-Lagalpha(k,2)
     &                     +PenaltyCoeff(k,2)*Penalty(k,2))
     &                     *NANBIJ(n,i,2)+tfactSurf*(-Lagalpha(k,3)
     &                     +PenaltyCoeff(k,3)*Penalty(k,3))
     &                     *NANBIJ(n,i,3)+tfactSurf
     *                     *LagMeanFlow(k)*(PenaltyCoeff(k,2)*
     &                     NANBLagrange(2,n,i)**2+PenaltyCoeff(k,3)
     &                     *NANBLagrange(3,n,i)**2) ) 
                        flowdiag(n,i)=flowdiag(n,i)+abs(tfactSurf**2
     &                     *( (NANBLagrange(1,n,i)-
     &                     PQLagrange(k,1)*NABI(n,i)-QLagrange(k,1)
     &                     *PNABI(n,i)/LagProfileArea(k)+
     &                     QLagrange(k,1)*NABI(n,i)*ProfileDelta(k))**2+
     &                     NANBLagrange(2,n,i)**2
     &                     +NANBLagrange(3,n,i)**2 )
     &                     /ScaleFactor(k,1)/two/gami/alfi)
                     enddo
                  endif                     
            enddo
         enddo

         return
         end
