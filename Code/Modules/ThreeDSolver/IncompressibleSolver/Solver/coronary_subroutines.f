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

#include "cvFlowsolverOptions.h"

#if (VER_CORONARY == 1)

c   DIB ADDITION - DES - 27JAN2014
c ================================
c   Initialize the Coronary bc:
c   Read the data in initCORt
c ================================
      subroutine initCORt()
      
      use convolCORFlow      
      include "global.h"
      include "common_blocks/timdat.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/inpdat.h"
      INTEGER k,j,n
c      print *,'I am in the initCORt subroutine'
      open(unit=815, file='cort.dat',status='old')
c         read (815,*)
         read (815,*) nptsCORmax
c	       print *, 'nptsCORmax = ', nptsCORmax
c        print *, 'nptscormax is read'
         allocate (numCORt(numCorSrfs))  
         allocate (ValuePlvist(nptsCORmax,2,numCORSrfs))
         allocate (ValueListCOR(9,numCORSrfs))
         allocate (dQinidT(numCORSrfs))
         allocate (dPinidT(numCORSrfs))
c        allocate (CORScaleFactor(numCORSrfs))
         numCORt = zero
         ValuePlvist = zero
         ValueListCOR = zero
         dQinidT = zero
         dPinidT = zero
         do k=1,numCORSrfs
            read (815,*) numDataCOR
c           print *, 'numDataCOR = ', numDataCOR
c           print *, 'numdatacor is read'
            numCORt(k) = numDataCOR
            do j=1,9
               read (815,*) ValueListCOR(j,k) ! reads q0, q1, q2, p0, p1, p2, b0, b1, b2
c              print *, 'ValueListCOR = ', ValueListCOR(j,k)
            enddo
            read (815,*) dQinidT(k)
c	          print *, 'dQinidT = ', dQinidT(k)
            read (815,*) dPinidT(k)
c           print *, 'dPinidT = ', dPinidT(k)
            do j=1,numDataCOR
               read(815,*) (ValuePlvist(j,n,k),n=1,2) ! n=1 time, 2 value
            enddo
         enddo
c        print *, 'ValuePlvist = ', ValuePlvist(1,2,1)
c        print *, 'ValuePlvist = ', ValuePlvist(2,2,1)
      close(815)
       
      allocate (dtCOR(2,numCORSrfs))
      allocate (COR(2, numCORSrfs))
      allocate (CoefCOR(5,numCORSrfs))
      allocate (DetCOR(numCORSrfs))
c     allocate (CORic(2,numCORSrfs))
c     print *, 'allocation of constants done'

      if (lstep .eq. 0) then
         nptsCOR = 0
         allocate (CORConvCoef(nstep(1)+2,numCORSrfs)) !for convolution coeff
         allocate (CORPlvConvCoef(nstep(1)+2,numCORSrfs))
         allocate (QHistCOR(nstep(1)+1,numCORSrfs)) !for flow history
         allocate (PlvHistCOR(nstep(1)+1,numCORSrfs))
         allocate (PHistCOR(nstep(1)+1,numCORSrfs)) !for pressure history
         QHistCOR = zero
         PlvHistCOR = zero
         PHistCOR = zero
c	       print *, 'allocation of conv coef and q and phist done'
      elseif (lstep .gt. 0) then   
         nptsCOR = lstep            
         allocate (CORConvCoef(lstep+nstep(1)+2,numCORSrfs)) !for convolution coeff
         allocate (CORPlvConvCoef(lstep+nstep(1)+2,numCORSrfs))
         allocate (QHistCOR(lstep+nstep(1)+1,numCORSrfs)) !for flow history
         allocate (PlvHistCOR(lstep+nstep(1)+1,numCORSrfs))
         allocate (PHistCOR(lstep+nstep(1)+1,numCORSrfs)) !for pressure history
         QHistCOR = zero
         PlvHistCOR = zero
         PHistCOR = zero
c	       print *, 'allocation of conv coef and q and phist done'
         call ReadDataFile(QHistCOR(1:lstep+1,:),lstep+1,numCORSrfs,
     &      'QHistCOR.dat',876)
         call ReadDataFile(PHistCOR(1:lstep+1,:),lstep+1,numCORSrfs,
     &      'PHistCOR.dat',877)
         call ReadDataFile(PlvHistCOR(1:lstep+1,:),lstep+1,numCORSrfs,
     &      'PlvHistCOR.dat',879)
      endif
c     print*, 'End of initcort subroutine'      
      return
      end      

!> Interpolate the data to match the process time step
      subroutine CORint(ctime,Plvist)
      
      use convolCORFlow ! brings numCORSrfs, ValuePlvist
      include "global.h"
      include "common_blocks/nomodule.h"
      
      real*8  ctime, ptime, wr
      integer nlast, nper, k, j
      real*8  Plvist(0:MAXSURF)      
c     print *, 'I am in the CORint subroutine'   
c     print *, 'numCORSrfs',numCORSrfs    
      do k =1,numCORSrfs
         nlast=numCORt(k) ! number of time series to interpolate from
         nper=ctime/ValuePlvist(nlast,1,k) ! number of periods completed to shift off
         ptime = ctime-nper*ValuePlvist(nlast,1,k) ! now time in periodic domain
            
         do j=2,nlast   !loop to find the interval that we are in
            if(ValuePlvist(j,1,k).gt.ptime) then ! this is upper bound, j-1 is lower
               wr=(ptime-ValuePlvist(j-1,1,k))
     &             / ( ValuePlvist(j,1,k)-ValuePlvist(j-1,1,k) )
               Plvist(k)= ValuePlvist(j-1,2,k)*(one-wr) 
     &                        + ValuePlvist(j,2,k)*wr
               exit
            endif
         enddo
c       print *, 'Plvist(',k,') = ', Plvist(k)
      enddo
      return
      end

c 
c ... initialize the influence of the initial conditions for the Coronary BC
c    
      subroutine calcCORic(y,srfIdList,numSrfs)
      
      use convolCORFlow  
      
      include "global.h" 
      include "common_blocks/timdat.h" 
      include "common_blocks/conpar.h" 
      include "common_blocks/inpdat.h"
      
      integer   srfIdList(0:MAXSURF), numSrfs, irankCoupled, k
      real*8    y(nshg,4)   !need velocity and pressure
      real*8    CoupleArea(0:MAXSURF)
      real*8    VelOnly(nshg,3), POnly(nshg)
      real*8    Qini(0:MAXSURF), Pini(0:MAXSURF)
      real*8    PlvistIni(0:MAXSURF)
           
      call calcCOR()
      call calcCoefCOR()  
      POnly(:)= one ! one to get area
      call integrScalar(CoupleArea,POnly,srfIdList,numSrfs) !get surf area
      CORArea(1:numSrfs)=CoupleArea(1:numSrfs)
      call CORint(Delt(1)*lstep,PlvistIni)
      CORic = zero
      if (lstep .eq. zero) then
         VelOnly(:,1:3)=y(:,1:3)
         call GetFlowQ(Qini,VelOnly,srfIdList,numSrfs) !get initial flow
         QHistCOR(1,1:numSrfs)=Qini(1:numSrfs)
         POnly(:)=y(:,4) ! pressure
         call integrScalar(Pini,POnly,srfIdList,numSrfs) !get initial pressure integral
         PHistCOR(1,1:numSrfs) = Pini(1:numSrfs)/CORArea(1:numSrfs)
         Pini(1:numSrfs)=Pini(1:numSrfs)/CORArea(1:numSrfs)
         PlvHistCOR(1,1:numSrfs)=PlvistIni(1:numSrfs)
      elseif (lstep .gt. zero) then
         Qini(1:numSrfs) = QHistCOR(1,1:numSrfs)
         Pini(1:numSrfs) = PHistCOR(1,1:numSrfs)
         PlvistIni(1:numSrfs)=PlvHistCOR(1,1:numSrfs)
      endif
      
      do k=1, numSrfs
            CORic(1,k) = (ValueListCOR(6,k)*dPinidT(k)-
     &           ValueListCOR(6,k)*COR(2,k)*Pini(k)-
     &         ValueListCOR(3,k)*dQinidT(k)-
     &         (ValueListCOR(3,k)*COR(1,k)+
     &         ValueListCOR(2,k))*Qini(k)-
     &         ValueListCOR(8,k)*PlvistIni(k))
     &         /DetCOR(k)
            CORic(2,k) = (ValueListCOR(6,k)*dPinidT(k)-
     &           ValueListCOR(6,k)*COR(1,k)*Pini(k)-
     &         ValueListCOR(3,k)*dQinidT(k)-
     &         (ValueListCOR(3,k)*COR(2,k)+
     &         ValueListCOR(2,k))*Qini(k)-
     &         ValueListCOR(8,k)*PlvistIni(k))
     &         /DetCOR(k)
       enddo       
      
      return
      end
c
c...  calculates the coefficients needed for beta calculation in the Coronary BC
c

      subroutine calcCoefCOR()

      use convolCORFlow

      include "global.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/inpdat.h"

      INTEGER k

      do k=1, numCORSrfs
          CoefCOR(1,k)= (ValueListCOR(3,k)*
     &    COR(1,k)*COR(1,k)
     &    +ValueListCOR(2,k)*COR(1,k)+
     &    ValueListCOR(1,k))/DetCOR(k)
          CoefCOR(2,k)=(ValueListCOR(3,k)*
     &    COR(2,k)*COR(2,k)
     &    +ValueListCOR(2,k)*COR(2,k)+
     &    ValueListCOR(1,k))/DetCOR(k)
          CoefCOR(3,k)=(ValueListCOR(8,k)*COR(1,k)+
     &    ValueListCOR(7,k))
     &             /DetCOR(k)
          CoefCOR(4,k)=(ValueListCOR(8,k)*COR(2,k)+
     &    ValueListCOR(7,k))
     &             /DetCOR(k)
          CoefCOR(5,k)=ValueListCOR(3,k)/ValueListCOR(6,k)
      enddo

      return
      end
c
c...  calculates dtCOR, the exponents if the exponentials for the Coronary BC
c
      subroutine calcCOR()

      use convolCORFlow ! brings ValueListCOR
 
      include "global.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/inpdat.h"

      INTEGER k

      do k=1, numCORSrfs
         DetCOR(k)=sqrt(ValueListCOR(5,k)*ValueListCOR(5,k)
     &          -four*ValueListCOR(4,k)*ValueListCOR(6,k))
         COR(2,k)=-(ValueListCOR(5,k)+DetCOR(k))/
     &   two/ValueListCOR(6,k)
         COR(1,k)= ValueListCOR(4,k)/
     &   ValueListCOR(6,k)/COR(2,k) 
         dtCOR(1,k)=Delt(1)*COR(1,k)
         dtCOR(2,k)=Delt(1)*COR(2,k)   
      enddo

      return
      end

c
c...calculate the time varying coefficients of pressure 
c...for the Coronary convolution
c
      subroutine CalcCORConvCoef (stepn, numSrfs)

      use convolCORFlow !brings in dtCOR, COR, CoefCOR

      include "global.h"
      include "common_blocks/timdat.h"

      INTEGER numSrfs, stepn, j

      CORConvCoef = zero
      if (stepn. eq. 0) then
        CORConvCoef(1,:)=real(CoefCOR(1,:)/
     &              dtCOR(1,:)/COR(1,:)/alfi*
     &              ((-one+alfi*dtCOR(1,:))*
     &              exp(dtCOR(1,:)*alfi)+one)
     &                  - CoefCOR(2,:)/
     &              dtCOR(2,:)/COR(2,:)/alfi*
     &               ((-one+alfi*dtCOR(2,:))*
     &              exp(dtCOR(2,:)*alfi)+one))
        CORConvCoef(2,:)= real(CoefCOR(5,:) +
     &                    CoefCOR(1,:)/
     &                  dtCOR(1,:)/COR(1,:)/alfi*
     &                  (exp(dtCOR(1,:)*alfi)-
     &                  (one+alfi*dtCOR(1,:)))-
     &                  CoefCOR(2,:)/dtCOR(2,:)/
     &                  COR(2,:)/alfi*
     &                  (exp(dtCOR(2,:)*alfi)-
     &                  (one+alfi*dtCOR(2,:))))
      endif
      if (stepn. ge. 1) then  
         CORConvCoef(1,:) =real(CoefCOR(1,:)/
     &                     dtCOR(1,:)/COR(1,:)*
     &                  (exp(dtCOR(1,:)*
     &                  (stepn+alfi-one))-
     &                (one-dtCOR(1,:))*
     &                exp(dtCOR(1,:)*(stepn+alfi)))
     &                 -CoefCOR(2,:)/dtCOR(2,:)/COR(2,:)*
     &                 (exp(dtCOR(2,:)*(stepn+alfi-one))-
     &                (one-dtCOR(2,:))*
     &                exp(dtCOR(2,:)*(stepn+alfi))))
         CORConvCoef(stepn+1,:) = real(CoefCOR(1,:)/
     &                        dtCOR(1,:)/COR(1,:)
     &                        /alfi*(alfi*exp(dtCOR(1,:)*
     &                        (alfi+one))-
     &                        (alfi+one)*exp(dtCOR(1,:)*
     &                        (alfi))+one)-
     &                        CoefCOR(2,:)/dtCOR(2,:)/
     &                        COR(2,:)/alfi*
     &                        (alfi*exp(dtCOR(2,:)*
     &                        (alfi+one))-
     &                        (alfi+one)*exp(dtCOR(2,:)
     &                        *(alfi))+one))
         CORConvCoef(stepn+2,:) = real(CoefCOR(5,:)+
     &                        CoefCOR(1,:)/
     &                        dtCOR(1,:)/COR(1,:)/alfi*
     &                     (exp(dtCOR(1,:)*alfi)-
     &                     one-alfi*dtCOR(1,:))-
     &                        CoefCOR(2,:)/dtCOR(2,:)
     &                        /COR(2,:)/alfi*
     &                      (exp(dtCOR(2,:)*alfi)
     &                      -one-alfi*dtCOR(2,:)))
      endif
      if (stepn. ge. 2) then
       do j=2,stepn
         CORConvCoef(j,:) = real(CoefCOR(1,:)/
     &                      dtCOR(1,:)/COR(1,:)*
     &                      (exp(dtCOR(1,:)*
     &                      (stepn+alfi-j))-
     &                      two*exp(dtCOR(1,:)*
     &                      (stepn+alfi-j+one))+
     &                     exp(dtCOR(1,:)*
     &                      (stepn+alfi-j+two)))-
     &                      CoefCOR(2,:)/
     &                      dtCOR(2,:)/COR(2,:)*
     &                      (exp(dtCOR(2,:)*
     &                      (stepn+alfi-j))-
     &                      two*exp(dtCOR(2,:)*
     &                      (stepn+alfi-j+one))+
     &                      exp(dtCOR(2,:)*
     &                      (stepn+alfi-j+two))))
       enddo
      endif
      
      return
      end

c
c...calculate the time varying coefficients of left ventricular pressure
c...for the Coronary convolution
c...need to do for t=0 and 1
c

      subroutine CalcCORPlvConvCoef (stepn, numSrfs)

      use convolCORFlow !brings in dtCOR, COR, CoefCOR

      include "global.h"
      include "common_blocks/timdat.h"

      INTEGER numSrfs, stepn, j
      
      CORPlvConvCoef = zero
      if (stepn. eq. 0) then
         CORPlvConvCoef(1,:)=real(CoefCOR(3,:)/
     &              dtCOR(1,:)/COR(1,:)/alfi*
     &             ((-one+alfi*dtCOR(1,:))*
     &             exp(dtCOR(1,:)*alfi)+one)
     &                 - CoefCOR(4,:)/
     &             dtCOR(2,:)/COR(2,:)/alfi*
     &             ((-one+alfi*dtCOR(2,:))*
     &             exp(dtCOR(2,:)*alfi)+one))
         CORPlvConvCoef(2,:)=real(CoefCOR(3,:)/
     &                dtCOR(1,:)/COR(1,:)/alfi*
     &                (exp(dtCOR(1,:)*alfi)
     &                -(one+alfi*dtCOR(1,:)))-
     &                   CoefCOR(4,:)/
     &                 dtCOR(2,:)/COR(2,:)/alfi*
     &                 (exp(dtCOR(2,:)*alfi)-
     &                 (one+alfi*dtCOR(2,:))))
      endif
      if (stepn. ge. 1) then
       CORPlvConvCoef(1,:) = real(CoefCOR(3,:)/
     &                  dtCOR(1,:)/COR(1,:)*
     &                  (exp(dtCOR(1,:)*
     &             (stepn+alfi-one))-
     &             (one-dtCOR(1,:))*
     &             exp(dtCOR(1,:)*(stepn+alfi)))
     &                -CoefCOR(4,:)/
     &                dtCOR(2,:)/COR(2,:)*
     &                 (exp(dtCOR(2,:)*
     &                 (stepn+alfi-one))-
     &               (one-dtCOR(2,:))*
     &               exp(dtCOR(2,:)*(stepn+alfi))))
      CORPlvConvCoef(stepn+1,:)=real(CoefCOR(3,:)
     &                       /dtCOR(1,:)/COR(1,:)
     &                       /alfi*(alfi*
     &                exp(dtCOR(1,:)*(alfi+one))-
     &                       (alfi+one)*
     &                exp(dtCOR(1,:)*(alfi))+one)-
     &                        CoefCOR(4,:)/
     &                dtCOR(2,:)/COR(2,:)/alfi*
     &                (alfi*exp(dtCOR(2,:)*
     &                (alfi+one))-
     &                (alfi+one)*exp(dtCOR(2,:)*
     &                (alfi))+one))
       CORPlvConvCoef(stepn+2,:)=real(CoefCOR(3,:)
     &                /dtCOR(1,:)/COR(1,:)
     &                /alfi*(exp(dtCOR(1,:)*alfi)
     &                -one-alfi*dtCOR(1,:))-
     &                        CoefCOR(4,:)/
     &                 dtCOR(2,:)/COR(2,:)/alfi*
     &                     (exp(dtCOR(2,:)*alfi)
     &                -one-alfi*dtCOR(2,:)))
      endif
      if (stepn. ge. 2) then
       do j=2,stepn
         CORPlvConvCoef(j,:) = real(CoefCOR(3,:)/
     &                 dtCOR(1,:)/COR(1,:)*
     &                      (exp(dtCOR(1,:)*
     &                 (stepn+alfi-j))-
     &                      two*exp(dtCOR(1,:)*
     &                 (stepn+alfi-j+one))+
     &                     exp(dtCOR(1,:)*
     &                 (stepn+alfi-j+two)))-
     &                      CoefCOR(4,:)/
     &                  dtCOR(2,:)/COR(2,:)*
     &                      (exp(dtCOR(2,:)*
     &                  (stepn+alfi-j))-
     &                      two*exp(dtCOR(2,:)*
     &                  (stepn+alfi-j+one))+
     &                      exp(dtCOR(2,:)*
     &                  (stepn+alfi-j+two))))
       enddo
      endif

      return
      end

c
c...calculate the time dependent H operator for the Coronary convolution
c
      subroutine CalcHopCOR (timestepCOR, stepn, srfIdList, numSrfs, y)

      use convolCORFlow !brings in HopCOR, dtCOR, COR, CoefCOR
      
      include "global.h"
      include "common_blocks/timdat.h"
      include "common_blocks/conpar.h"
      include "common_blocks/inpdat.h"

      integer   srfIdList(0:MAXSURF), numSrfs, stepn
      real*8    y(nshg,4), timestepCOR, PlvistNext(0:MAXSURF)
      real*8    CoupleArea(0:MAXSURF), POnly(nshg)
      
      HopCOR=zero
      PlvistNext=zero
      
      call CalcCORPlvConvCoef (stepn, numSrfs)
      call pHist(plvoldCOR, PlvHistCOR, CORPlvConvCoef,
     &    nstep(1)+nptsCOR,numSrfs) 
      call CORint(timestepCOR*(stepn + alfi),PlvistNext)
      POnly(:)=y(:,4) ! pressure
      call integrScalar(CoupleArea,POnly,srfIdList,numSrfs) !get initial pressure integral
c      print *, 'CorArea =', CorArea(1:numSrfs)
      PHistCOR(stepn+1,1:numSrfs) = CoupleArea(1:numSrfs)
     &   /CORArea(1:numSrfs)
      PlvHistCOR(stepn+2,1:numSrfs)=PlvistNext(1:numSrfs)
      HopCOR(1:numSrfs) =real(plvoldCOR(1:numSrfs)+ 
     &      CORic(1,1:numSrfs)*
     &      exp(dtCOR(1,1:numSrfs)*(stepn+alfi))-
     &      CORic(2,1:numSrfs)*
     &      exp(dtCOR(2,1:numSrfs)*(stepn+alfi))+ 
     &      CorPlvConvCoef(stepn+2,1:numSrfs)*
     &      PlvistNext(1:numSrfs))

      return
      end
c
c...update time history of left ventricular pressure
c
      subroutine UpdHistPlvConv(y,timestepCOR,stepn,srfIdList,numSrfs) 
      
      use convolCORFlow 
      
      include "global.h" 
      include "common_blocks/timdat.h"
      include "common_blocks/conpar.h"
      include "common_blocks/workfc.h"
      include "common_blocks/outpar.h"

      integer   srfIdList(0:MAXSURF), numSrfs, stepn
      real*8    timestepCOR, PlvistNext(0:MAXSURF)
      real*8    y(nshg,4)
      real*8    CoupleArea(0:MAXSURF), POnly(nshg)
      
      PlvistNext=zero      
      call CORint(timestepCOR*stepn,PlvistNext)
      PlvHistCOR(stepn+1,1:numSrfs)=PlvistNext(1:numSrfs)   
      POnly(:)=y(:,4) ! pressure
      call integrScalar(CoupleArea,POnly,srfIdList,numSrfs) !get initial pressure integral
      PHistCOR(stepn+1,1:numSrfs) = CoupleArea(1:numSrfs)
     &   /CORArea(1:numSrfs)

      if ((mod(lstep, ntout) .eq. 0).and.
     &   (myrank .eq. zero)) then
         call OutputDataFile(QHistCOR(1:lstep+1,:),lstep+1,numSrfs,
     &      'QHistCOR.dat',876)
         call OutputDataFile(PHistCOR(1:lstep+1,:),lstep+1,numSrfs,
     &      'PHistCOR.dat',877)
         call OutputDataFile(PlvHistCOR(1:lstep+1,:),lstep+1,numSrfs,
     &      'PlvHistCOR.dat',879)
      endif 

      return
      end

c     END - ADDED FROM DIB - DES - 10DEC2013  
#endif
