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

!> Routine to couple pressure with flow rate for each coupled surface

      subroutine ElmpvsQ (res,y,sign)     

      use pvsQbi  ! brings in NABI
      use convolImpFlow !brings in the current part of convol coef for imp BC
      use convolRCRFlow !brings in the current part of convol coef for RCR BC
#if (VER_CORONARY == 1)
      use convolCORFlow !brings in the current part of convol coef for Cor BC
#endif
      use LagrangeMultipliers !brings in the current part of coef for Lagrange Multipliers
#if (VER_CLOSEDLOOP == 1)
      USE GeneralBC ! Brings in PGen
#endif
c
      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/timdat.h"
C
C
      REAL*8  sign
      real*8  res(nshg,ndof), y(nshg,3)
      real*8  p(0:MAXSURF),   q(0:MAXSURF,3)
      integer irankCoupled, i, j, k
c
c... get p for the resistance BC
c           
      if(numResistSrfs.gt.zero) then
        call GetFlowQ(p,y,nsrflistResist,numResistSrfs)  !Q pushed into p but at this point 
                          ! p is just the full Q for each surface
        
        p=sign*p*ValueListResist ! p=QR  now we have the true pressure on each
                                             ! outflow surface.  Note sign is -1
                                             ! for RHS, +1 for LHS
c
c....  multiply it by integral NA n_i
c     
       do i = 1,nshg
          do k = 1,numResistSrfs
              irankCoupled = 0
              if (nsrflistResist(k).eq.ndsurf(i)) then 
                  irankCoupled=k
                  res(i,1:3)=res(i,1:3) + p(irankCoupled)*NABI(i,1:3)
              endif
          enddo   
       enddo
       
      endif !end of coupling for Resistance BC

#if (VER_CLOSEDLOOP == 1)
      IF (numNeumannSrfs .GT. zero) THEN
         IF (sign .LT. zero) THEN
            p = sign*PCoupled
         ELSE
            CALL GetFlowQ (p, y, nsrflistCoupled, numCoupledSrfs)
            p = sign*p*PGenDer
         END IF
         DO i = 1,nshg
            DO k = numDirichletSrfs+1, numCoupledSrfs
               IF (nsrflistCoupled(k) .EQ. ndsurf(i)) THEN
                  res(i,1:3)=res(i,1:3) + p(k)*NABI(i,1:3)
               END IF
            END DO
         END DO
      END IF
#endif
c
c... get p for the impedance BC
c     
      if(numImpSrfs.gt.zero) then
        call GetFlowQ(p,y,nsrflistImp,numImpSrfs)  !Q pushed into p but at this point 
                          ! p is just the full Q for each surface
        do j = 1,numImpSrfs
            if(sign.lt.zero) then ! RHS so -1
               p(j)= sign*(poldImp(j) + p(j)*ImpConvCoef(ntimeptpT+2,j))!pressure p=pold+ Qbeta
            elseif(sign.gt.zero) then ! LHS so sign is positive
               p(j)= sign*p(j)*ImpConvCoef(ntimeptpT+2,j)
            endif
        enddo
             
c
c....  multiply it by integral NA n_i
c
       do i = 1,nshg
          do k = 1,numImpSrfs
              irankCoupled = 0
              if (nsrflistImp(k).eq.ndsurf(i)) then 
                  irankCoupled=k
                  res(i,1:3)=res(i,1:3) + p(irankCoupled)*NABI(i,1:3)
              endif
          enddo   
       enddo
       
      endif !end of coupling for Impedance BC
c
c... get p for the RCR BC
c     
      if(numRCRSrfs.gt.zero) then
        call GetFlowQ(p,y,nsrflistRCR,numRCRSrfs)  !Q pushed into p but at this point 
                          ! p is just the full Q for each surface
        do j = 1,numRCRSrfs
            if(sign.lt.zero) then ! RHS so -1
                p(j)= sign*(poldRCR(j) + p(j)*RCRConvCoef(lstep+2,j)) !pressure p=pold+ Qbet
                p(j)= p(j) - HopRCR(j) ! H operator contribution 
            elseif(sign.gt.zero) then ! LHS so sign is positive
                p(j)= sign*p(j)*RCRConvCoef(lstep+2,j)
            endif
        enddo
             
c
c....  multiply it by integral NA n_i
c     
       do i = 1,nshg
          do k = 1,numRCRSrfs
              irankCoupled = 0
              if (nsrflistRCR(k).eq.ndsurf(i)) then 
                  irankCoupled=k
                  res(i,1:3)=res(i,1:3) + p(irankCoupled)*NABI(i,1:3)
              endif
          enddo   
       enddo       
      endif !end of coupling for RCR BC

#if (VER_CORONARY == 1)

c ===============================================================
c... Get p for the Coronary BC - ADDED FROM DIB - DES - 27JAN2014
c =============================================================== 
  
      if(numCORSrfs.gt.zero) then
        call GetFlowQ(p,y,nsrflistCOR,numCORSrfs)  !Q pushed into p but at this point 
                          ! p is just the full Q for each surface
        do j = 1,numCORSrfs

           if(sign.lt.zero) then ! RHS so -1
              p(j)= sign*(poldCOR(j) + 
     &              p(j)*CORConvCoef(lstep+2,j)) !pressure p=pold+ Qbeta
                                !check lstep - need it to be integer and value n not n+1
                 p(j)= p(j) +sign* HopCOR(j) ! H operator contribution 
c                print *, 'negative'
c                print *, 'poldCOR(j)', poldCOR(j)
c                print *, 'CORConvCoef(lstep+2,j)', CORConvCoef(lstep+2,j)
c                print *, 'HopCOR(j)', HopCOR(j)
           elseif(sign.gt.zero) then ! LHS so sign is positive
                 p(j)= sign*p(j)*CORConvCoef(lstep+2,j)
c                print *, 'positive'
           endif
c           print *, 'p(',j,')', p(j)
        enddo
c
c....  multiply it by integral NA n_i
c     
       do i = 1,nshg
          do k = 1,numCORSrfs
              irankCoupled = 0
              if (nsrflistCOR(k).eq.ndsurf(i)) then 
                 irankCoupled=k
                 res(i,1:3) = res(i,1:3) + 
     &           p(irankCoupled)*NABI(i,1:3)
              endif
          enddo   
       enddo

      endif !end of coupling for Coronary BC

c =========================================
c... END - ADDED FROM DIB - DES - 27JAN2014
c =========================================

#endif

c
c... get p for the Lagrange multipliers
c           
      if(numLagrangeSrfs .gt. zero) then
         if(sign .lt. zero) then ! RHS so -1
            p = zero
            call GetFlowQ(p, y, nsrflistLagrange,
     &         numLagrangeSrfs)  
            QLagrange(1:numLagrangeSrfs,1)=p(1:numLagrangeSrfs)
            p = zero
            call GetProfileFlowQ(p, y, nsrflistLagrange,
     &         numLagrangeSrfs)    !flow rate multiplied by a profile function 
            PQLagrange(1:numLagrangeSrfs,1)=p(1:numLagrangeSrfs)
     &         /LagProfileArea(1:numLagrangeSrfs) 
            q = zero
            call GetInnerProduct(q, y, nsrflistLagrange,
     &            numLagrangeSrfs)
            IPLagrange(1:numLagrangeSrfs,1:3)=q(1:numLagrangeSrfs,1:3)
            do k = 1,numLagrangeSrfs
               Penalty(k,1)= 
     &           abs( IPLagrange(k,1)-two*QLagrange(k,1)*PQLagrange(k,1)
     &            +QLagrange(k,1)**2*ProfileDelta(k) )*LagMeanFlow(k)
               Penalty(k,2)= abs(IPLagrange(k,2))*LagMeanFlow(k)
               Penalty(k,3)= abs(IPLagrange(k,3))*LagMeanFlow(k)
               resL(k,1)=two*ScaleFactor(k,1)*Lagalpha(k,1)-Penalty(k,1)
               resL(k,2)=two*ScaleFactor(k,2)*Lagalpha(k,2)-Penalty(k,2)
               resL(k,3)=two*ScaleFactor(k,3)*Lagalpha(k,3)-Penalty(k,3)
                  do i = 1,nshg
                     if (nsrflistLagrange(k).eq.ndsurf(i)) then 
                          res(i,1:3)=res(i,1:3)+sign*LagMeanFlow(k)
     &                        *(-Lagalpha(k,1)+PenaltyCoeff(k,1)
     &                        *Penalty(k,1))*(NANBLagrange(1,i,1:3)-
     &                        PQLagrange(k,1)*NABI(i,1:3)-QLagrange(k,1)
     &                        *PNABI(i,1:3)/LagProfileArea(k)+
     &                        QLagrange(k,1)*NABI(i,1:3)
     &                        *ProfileDelta(k))+sign*LagMeanFlow(k)*
     &                        (-Lagalpha(k,2)+PenaltyCoeff(k,2)
     &                        *Penalty(k,2))*NANBLagrange(2,i,1:3)
     &                        +sign*LagMeanFlow(k)*
     &                        (-Lagalpha(k,3)+PenaltyCoeff(k,3)
     &                        *Penalty(k,3))*NANBLagrange(3,i,1:3)   
                     endif
                  enddo
            enddo
            AddLag(:,1:3) = resL(:,1:3)
            call LagMultiplyMatrixTranspose(nsrflistLagrange,
     &         numLagrangeSrfs)
            res(:,1:3) = res(:,1:3) + LagAPproduct(:,1:3)
     &         /ScaleFactor(1,1)/alfi/gami/two
         elseif(sign .gt. zero) then ! LHS 
            p = zero
            call GetFlowQ(p, y, nsrflistLagrange,
     &         numLagrangeSrfs)  
            QLagrange(1:numLagrangeSrfs,2)=p(1:numLagrangeSrfs)
            p = zero
            call GetProfileFlowQ(p, y, nsrflistLagrange,
     &         numLagrangeSrfs)    !flow rate multiplied by a profile function 
            PQLagrange(1:numLagrangeSrfs,2)=p(1:numLagrangeSrfs)
     &         /LagProfileArea(1:numLagrangeSrfs) 
            q = zero
            call GetInnerProduct(q, y, nsrflistLagrange,
     &         numLagrangeSrfs)
            IPLagrange(1:numLagrangeSrfs,4:6)=q(1:numLagrangeSrfs,1:3)  
            do k = 1, numLagrangeSrfs
                  do i = 1,nshg
                     if (nsrflistLagrange(k).eq.ndsurf(i)) then 
                          res(i,1:3)=res(i,1:3)+sign*LagMeanFlow(k)*
     &                        (-Lagalpha(k,1)+PenaltyCoeff(k,1)
     &                        *Penalty(k,1))*(NANBLagrange(4,i,1:3)-
     &                        NABI(i,1:3)*PQLagrange(k,2)-QLagrange(k,2)
     &                        *PNABI(i,1:3)/LagProfileArea(k)+
     &                        QLagrange(k,2)*NABI(i,1:3)*
     &                        ProfileDelta(k))  
     &                        +sign*LagMeanFlow(k)*LagMeanFlow(k)
     &                        *PenaltyCoeff(k,1)*(NANBLagrange(1,i,1:3)-
     &                        NABI(i,1:3)*PQLagrange(k,1)-QLagrange(k,1)
     &                        *PNABI(i,1:3)/LagProfileArea(k)+
     &                        QLagrange(k,1)*NABI(i,1:3)*
     &                        ProfileDelta(k))*(IPLagrange(k,4)
     &                        -PQLagrange(k,1)*QLagrange(k,2)
     &                        -QLagrange(k,1)*PQLagrange(k,2)
     &                        +QLagrange(k,1)*QLagrange(k,2)
     &                        *ProfileDelta(k))+sign*LagMeanFlow(k)*
     &                        (-Lagalpha(k,2)+PenaltyCoeff(k,2)
     &                        *Penalty(k,2))*NANBLagrange(5,i,1:3)
     &                        +sign*LagMeanFlow(k)*(-Lagalpha(k,3)+
     &                        PenaltyCoeff(k,3)*Penalty(k,3))
     &                        *NANBLagrange(6,i,1:3)+sign*LagMeanFlow(k)
     &                        *LagMeanFlow(k)*PenaltyCoeff(k,2)*
     &                        NANBLagrange(2,i,1:3)*IPLagrange(k,5)+sign
     &                        *LagMeanFlow(k)**2*PenaltyCoeff(k,3)
     &                        *NANBLagrange(3,i,1:3)*IPLagrange(k,6)  
                     endif    
                  enddo
            enddo

            call LagMultiplyMatrix(y, 1, nsrflistLagrange,
     &         numLagrangeSrfs)  
            call LagMultiplyMatrixTranspose(nsrflistLagrange,
     &         numLagrangeSrfs)  
            res(:,1:3) = res(:,1:3) - LagAPproduct(:,1:3)        
     &         /ScaleFactor(1,1)/alfi/gami/two               
         endif
      endif !end of coupling for Lagrange multipliers
         
      return
      end

