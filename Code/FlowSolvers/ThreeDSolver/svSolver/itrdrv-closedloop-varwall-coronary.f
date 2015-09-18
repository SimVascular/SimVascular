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

!> This iterative driver is the semi-discrete, predictor multi-corrector 
!! algorithm. It contains the Hulbert Generalized Alpha method which
!! is 2nd order accurate for Rho_inf from 0 to 1.  The method can be
!! made  first-order accurate by setting Rho_inf=-1. It uses CGP and
!! GMRES iterative solvers.
!!
!! working arrays:<BR>
!! - y(nshg,ndof) Y variables
!! - x(nshg,nsd) Node coordinates
!! - iBC(nshg) BC codes
!! - BC(nshg,ndofBC) BC constraint parameters
!! - iper(nshg) Periodicity table
!!

      subroutine itrdrv (y,         ac,         
     &                   uold,      x,         
     &                   iBC,       BC,         
     &                   iper,      ilwork,     shp,       
     &                   shgl,      shpb,       shglb
     &                   ) 
c
      use pvsQbi        !gives us splag (the spmass at the end of this run) 
      use specialBC !gives us itvn
      use convolImpFlow !for Imp bc
      use convolRCRFlow !for RCR bc
#if(VER_CORONARY == 1)
      use convolCORFlow !for COR bc
#endif
      use calcFlowPressure !to save history of flow and pressure of bc surfaces 
      use LagrangeMultipliers 
      use ResidualControl 
      
#if(VER_CLOSEDLOOP == 1)
      use GeneralBC
#endif
      use pointer_data
      
        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "svLS.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/incomp.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/matdat.h"
        include "common_blocks/mio.h"
        include "common_blocks/mtimer2.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/outpar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/sequence.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timpar.h"
        include "common_blocks/workfc.h"
        include "common_blocks/timdat.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/propar.h"

C
C     Local variables
C
      INTEGER             icnt,        ilss,         indx
      INTEGER             iqoldsiz,    isize,       isolsc,      isolve
      INTEGER             istepc,      istp,        itmp,        itsq
      INTEGER             iupdate,     k,           lesid
      INTEGER             lstep0
        
      INTEGER             ndofs,       nitems
      INTEGER             nkvecs,      npermdims,   npermdimss
      INTEGER             nsclrsol,    nsolflow,    nstp
      INTEGER             ntempdims,    ntmpdims
      INTEGER             ntmpdimss,   ntoutv

#if(VER_CLOSEDLOOP == 1)
      INTEGER             ii
#endif

C
      REAL*8              deltt,       dimkry,      dtglt
      REAL*8              rmub,        rmue
      REAL*8              tfact
      REAL*8              xi
C

        
        real*8    y(nshg,ndof),              ac(nshg,ndof),           
     &            yold(nshg,ndof),           acold(nshg,ndof),
     &            u(nshg,nsd),               uold(nshg,nsd),
     &            x(numnp,nsd),              solinc(nshg,ndof),
     &            BC(nshg,ndofBC),           tf(nshg,ndof)

c
        real*8    res(nshg,ndof)
c     
        real*8    shp(MAXTOP,maxsh,MAXQPT),  
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
        integer   rowp(nshg,nnz),        colm(nshg+1),
     &            iBC(nshg),             ilwork(nlwork),  
     &            iper(nshg),            ifuncs(6),
     &            deformflag(nshg)

        integer iblk, iel

        integer stopjob
        character*10 cname2
        character*5  cname
c
c.... For linear solver Library
c
        integer eqnType, prjFlag, presPrjFlag, verbose
c
        real*8, allocatable, dimension(:,:) :: aperm,  atemp, atempS
        real*8, allocatable, dimension(:,:,:) :: apermS

        real*8, allocatable, dimension(:,:) :: lhsP, lhsK, lhsS
        real*8   almit, alfit, gamit
c
        character*1024    servername
        character*20    fname1,fmt1
        character*20    fname2,fmt2,fnamer2
        integer         iarray(50) ! integers for headers
        character*20        license_f_name
        
        real*8 rerr(nshg,10), ybar(nshg,5), yerror(nshg,5),
     &          dummyVar(nshg), uhess(nshg,27), gradu(nshg,9)
     
        INTEGER idTmp, i, j
        REAL*8 correction, sumtime

      INTEGER svLS_nFaces, gnNo, nNo, faIn, facenNo
      INTEGER, ALLOCATABLE :: ltg(:), gNodes(:)
      REAL*8, ALLOCATABLE :: sV(:,:)

      CHARACTER*128 fileName
      TYPE(svLS_commuType) communicator
      TYPE(svLS_lhsType) svLS_lhs
      TYPE(svLS_lsType) svLS_ls

!--------------------------------------------------------
!     Get flag for deformable nodes
      deformflag=0
      if(ideformwall.eq.1) then
         do iblk = 1, nelblb
             iel    = lcblkb(1,iblk)
             nshl   = lcblkb(9,iblk)
             nshlb  = lcblkb(10,iblk)
             npro   = lcblkb(1,iblk+1) - iel
             call deformableNode (mienb(iblk)%p, miBCB(iblk)%p,
     &        deformflag)
         enddo
      endif

!--------------------------------------------------------------------
!     Setting up svLS
 
      IF (svLSFlag .EQ. 1) THEN
         CALL svLS_LS_CREATE(svLS_ls, svLSType, dimKry=Kspace,
     2      relTol=epstol(8), relTolIn=(/epstol(1),epstol(7)/), 
     3      maxItr=maxNSIters, 
     4      maxItrIn=(/maxMomentumIters,maxContinuityIters/))

         CALL svLS_COMMU_CREATE(communicator, MPI_COMM_WORLD)
 
         IF (numpe .GT. 1) THEN
            WRITE(fileName,*) myrank
            fileName = "ltg.dat."//ADJUSTL(TRIM(fileName))
            OPEN(1,FILE=fileName)
            READ(1,*) gnNo
            READ(1,*) nNo
            ALLOCATE(ltg(nNo))
            READ(1,*) ltg
            CLOSE(1)
         ELSE
            gnNo = nshg
            nNo = nshg
            ALLOCATE(ltg(nNo))
            DO i=1, nNo
               ltg(i) = i
            END DO
         END IF
      ELSE
!--------------------------------------------------------------------

c find the machine name so that we set the license key properly
        license_f_name='license.dat'
        call SolverLicenseServer(servername)
      END IF
c
c only master should be verbose
c

        if(numpe.gt.0 .and. myrank.ne.master)iverbose=0  
 
c
c.... initialize
c     
        ifuncs(:)  = 0              ! func. evaluation counter
        istep  = 0
        yold   = y
        acold  = ac

        rerr = zero
        ybar = y
        yerror=zero
c
c.... ---------------> initialize LesLib Library <---------------
c
c.... assign parameter values
c     
        do i = 1, 100
           numeqns(i) = i
        enddo
        nKvecs       = Kspace
        prjFlag      = iprjFlag
        presPrjFlag  = ipresPrjFlag
        verbose      = iverbose
c
c.... determine how many scalar equations we are going to need to solve
c
      nsolt=mod(impl(1),2)      ! 1 if solving temperature
      nsclrsol=nsolt+nsclr      ! total number of scalars solved At
                                ! some point we probably want to create
                                ! a map, considering stepseq(), to find
                                ! what is actually solved and only
                                ! dimension lhs to the appropriate
                                ! size. (see 1.6.1 and earlier for a
                                ! "failed" attempt at this).


      nsolflow=mod(impl(1),100)/10  ! 1 if solving flow
      
c
c.... Now, call lesNew routine to initialize
c     memory space
c
      call genadj(colm, rowp, icnt )  ! preprocess the adjacency list

      nnz_tot=icnt ! this is exactly the number of non-zero blocks on
                   ! this proc
 
#if(VER_CLOSEDLOOP == 1)
      numCoupledSrfs = numDirichletSrfs + numNeumannSrfs
      IF (numCoupledSrfs .GT. 0) THEN
         CALL initGenBC

         IF (numDirichletSrfs .GT. 0) THEN
!     Construction Dirichlet pointer            
            ALLOCATE(ptrDirichlet(itvn))
            ptrDirichlet = 0
            DO i=1, itvn
               idTmp = ndsurf(nBCt(i))
               DO j=1, numDirichletSrfs
                  IF (idTmp .EQ. nsrflistDirichlet(j)) THEN
                     ptrDirichlet(i) = j
                     EXIT
                  END IF
               END DO
            END DO
            
!     Correcting BCt array to get unit flow rate for coupled Dirichlet
!     surfaces
            CALL itrBC (y, ac, iBC, BC, iper, ilwork)
            CALL GetFlowQ (QCoupled, y, nsrflistCoupled, numCoupledSrfs)
            DO i=1,itvn
               j = ptrDirichlet(i)
               IF (j .NE. 0) THEN
                  BCt(i,1,1:3) = BCt(i,1,1:3)/QCoupled(j)
               END IF
            END DO

!     Assigning/adjusting the Dirichlet nodes velocities
            GenFlag = 'I'
            CALL calcGenBC (y, yold)
            DO i=1,itvn
               j = ptrDirichlet(i)
               IF (j .NE. 0) THEN
                  BC(nBCt(i),3:5) = BCt(i,1,1:3)*QCoupled(j)
               END IF
            END DO
            CALL itrBC (yold,  ac,  iBC,  BC,  iper, ilwork)
         END IF
      END IF
#endif

      if (nsolflow.eq.1) then
         lesId   = numeqns(1)
         eqnType = 1
         nDofs   = 4

!--------------------------------------------------------------------
!     Rest of configuration of svLS is added here, where we have LHS
!     pointers

         nPermDims = 1
         nTmpDims = 1

         allocate (lhsP(4,nnz_tot))
         allocate (lhsK(9,nnz_tot))

         IF (svLSFlag .EQ. 1) THEN
            IF  (ipvsq .GE. 2) THEN

#if((VER_CORONARY == 1)&&(VER_CLOSEDLOOP == 1))
               svLS_nFaces = 1 + numResistSrfs + numNeumannSrfs 
     2            + numImpSrfs + numRCRSrfs + numCORSrfs
#elif((VER_CORONARY == 1)&&(VER_CLOSEDLOOP == 0))
               svLS_nFaces = 1 + numResistSrfs
     2            + numImpSrfs + numRCRSrfs + numCORSrfs
#elif((VER_CORONARY == 0)&&(VER_CLOSEDLOOP == 1))
               svLS_nFaces = 1 + numResistSrfs + numNeumannSrfs 
     2            + numImpSrfs + numRCRSrfs
#else
               svLS_nFaces = 1 + numResistSrfs
     2            + numImpSrfs + numRCRSrfs
#endif

            ELSE
               svLS_nFaces = 1
            END IF

            CALL svLS_LHS_CREATE(svLS_lhs, communicator, gnNo, nNo, 
     2         nnz_tot, ltg, colm, rowp, svLS_nFaces)
            
            faIn = 1
            facenNo = 0
            DO i=1, nshg
               IF (IBITS(iBC(i),3,3) .NE. 0)  facenNo = facenNo + 1
            END DO
            ALLOCATE(gNodes(facenNo), sV(nsd,facenNo))
            sV = 0D0
            j = 0
            DO i=1, nshg
               IF (IBITS(iBC(i),3,3) .NE. 0) THEN
                  j = j + 1
                  gNodes(j) = i
                  IF (.NOT.BTEST(iBC(i),3)) sV(1,j) = 1D0
                  IF (.NOT.BTEST(iBC(i),4)) sV(2,j) = 1D0
                  IF (.NOT.BTEST(iBC(i),5)) sV(3,j) = 1D0
               END IF
            END DO
            CALL svLS_BC_CREATE(svLS_lhs, faIn, facenNo, 
     2         nsd, BC_TYPE_Dir, gNodes, sV)

            IF  (ipvsq .GE. 2) THEN
               DO k = 1, numResistSrfs
                  faIn = faIn + 1
                  CALL AddNeumannBCTosvLS(nsrflistResist(k), faIn)
               END DO

#if(VER_CLOSEDLOOP == 1)
               DO k = numDirichletSrfs+1, numCoupledSrfs
                  faIn = faIn + 1
                  CALL AddNeumannBCTosvLS(nsrflistCoupled(k), faIn)
               END DO
#endif
               DO k = 1, numImpSrfs
                  faIn = faIn + 1
                  CALL AddNeumannBCTosvLS(nsrflistImp(k), faIn)
               END DO
               DO k = 1, numRCRSrfs
                  faIn = faIn + 1
                  CALL AddNeumannBCTosvLS(nsrflistRCR(k), faIn)
               END DO
#if(VER_CORONARY == 1)
               DO k = 1, numCORSrfs
                  faIn = faIn + 1
                  CALL AddNeumannBCTosvLS(nsrflistCOR(k), faIn)
               END DO
#endif
            END IF
         ELSE
!--------------------------------------------------------------------
            call myfLesNew( lesId,   41994,
     &                 eqnType,
     &                 nDofs,          minIters,       maxIters,
     &                 nKvecs,         prjFlag,        nPrjs,
     &                 presPrjFlag,    nPresPrjs,      epstol(1),
     &                 prestol,        verbose,        statsflow,
     &                 nPermDims,      nTmpDims,      servername  )
         
         END IF
         allocate (aperm(nshg,nPermDims))
         allocate (atemp(nshg,nTmpDims))
         IF (svLSFlag .NE. 1) THEN
           call readLesRestart( lesId,  aperm, nshg, myrank, lstep,
     &                        nPermDims )
         ENDIF



      ENDIF

      if(nsclrsol.gt.0) then
       do isolsc=1,nsclrsol
         lesId       = numeqns(isolsc+1)
         eqnType     = 2
         nDofs       = 1
         presPrjflag = 0        
         nPresPrjs   = 0       
         prjFlag     = 1
         indx=isolsc+2-nsolt ! complicated to keep epstol(2) for
                             ! temperature followed by scalars
         call myfLesNew( lesId,            41994,
     &                 eqnType,
     &                 nDofs,          minIters,       maxIters,
     &                 nKvecs,         prjFlag,        nPrjs,
     &                 presPrjFlag,    nPresPrjs,      epstol(indx),
     &                 prestol,        verbose,        statssclr,
     &                 nPermDimsS,     nTmpDimsS,   servername )
       enddo
c
c  Assume all scalars have the same size needs
c
       allocate (apermS(nshg,nPermDimsS,nsclrsol))
       allocate (atempS(nshg,nTmpDimsS))  !they can all share this
       allocate (lhsS(nnz_tot,nsclrsol))
c
c actually they could even share with atemp but leave that for later
c
      else
         nPermDimsS = 0
         nTmpDimsS  = 0
      endif
c
c...  prepare lumped mass if needed
c
      if((flmpr.ne.0).or.(flmpl.ne.0)) call genlmass(x, shp,shgl)
c
c.... -----------------> End of initialization <-----------------
c
c.....open the necessary files to gather time series
c
      lstep0 = lstep+1
c
c.... satisfy the boundary conditions
c
    
c
c   NATE:  Can this function be skipped if you are using leslib????
c          call doesn't exist in Mahdi's version.
c   Ok, function call just moved way down in the source, but still
c   exists.  Is this ok??
c
c      call itrBC (y, ac,  iBC, BC, iper, ilwork)
c
c
c
c.... loop through the time sequences
c
      sumtime = 0d0

      do 3000 itsq = 1, ntseq

         itseq = itsq

c
c.... set up the time integration parameters
c         
         nstp   = nstep(itseq)
         nitr   = niter(itseq)
         LCtime = loctim(itseq)
         dtol(:)= deltol(itseq,:)

         call itrSetup ( y, acold )
c
c...initialize the coefficients for the impedance convolution,
c   which are functions of alphaf so need to do it after itrSetup

         if(numImpSrfs.gt.zero) then
            call calcImpConvCoef (numImpSrfs, ntimeptpT)
         endif
c
c...initialize the initial condition P(0)-RQ(0)-Pd(0) for RCR BC
c   need ndsurf so should be after initNABI

         if(numRCRSrfs.gt.zero) then
           call calcRCRic(y,nsrflistRCR,numRCRSrfs)
         endif
c
c...calculate area and initial pressure and flow for CalcSurfaces
c
         if(numCalcSrfs.gt.zero) then
           call calcCalcic(y,nsrflistCalc,numCalcSrfs)
         endif
c
c.... allocate LHS and RHS arrays required for the constrained surfaces
c   
         if(Lagrange.gt.zero) then
           call calcLagrangeic(nsrflistLagrange, 
     &        numLagrangeSrfs)
         endif   

#if(VER_CORONARY == 1)
c
c.... initialize the initial condition for Coronary BC
c     need ndsurf so should be after initNABI

         if(numCORSrfs.gt.zero) then
            call calcCORic(y,nsrflistCOR,numCORSrfs)
c            print *, 'initializing IC for COR BC'
         endif      
#endif

c
c.... find the last solve of the flow in the step sequence so that we will
c         know when we are at/near end of step
c
c         ilast=0
         nitr=0  ! count number of flow solves in a step (# of iterations)
         do i=1,seqsize
            if(stepseq(i).eq.0) nitr=nitr+1
         enddo
c
c.... loop through the time steps
c
         istop=0
         rmub=datmat(1,2,1)
c        DES - KEEP CONSTANT
         rmue=datmat(1,2,1) 

         do 2000 istp = 1, nstp

            xi=istp*1.0/nstp
            datmat(1,2,1)=rmub*(1.0-xi)+xi*rmue

c
c.... if we have time varying boundary conditions update the values of BC.
c     these will be for time step n+1 so use lstep+1
c     
            IF (itvn .GT. 0) THEN
               call BCint((lstep+1)*Delt(1), shp, shgl, shpb, shglb, x, 
     2            BC, iBC)
               
#if(VER_CLOSEDLOOP == 1)
               IF (numDirichletSrfs .GT. 0) THEN
                  DO i=1,itvn
                     j = ptrDirichlet(i)
                     IF (j .NE. 0) THEN
                        BC(nBCt(i),3:5) = BCt(i,1,1:3)*QCoupled(j)
            END IF
                  END DO
               END IF
#endif

            END IF
c
c ... calc the pressure contribution that depends on the history for the imp BC
c     
            if(numImpSrfs.gt.0) call pHist(poldImp,QHistImp,ImpConvCoef,
     &                                          ntimeptpT,numImpSrfs)
c
c ... calc the pressure contribution that depends on the history for the RCR BC
c     
            if(numRCRSrfs.gt.0) then 
               call CalcHopRCR (Delt(itseq), lstep, numRCRSrfs) 
               call CalcRCRConvCoef(lstep,numRCRSrfs) 
               call pHist(poldRCR,QHistRCR,RCRConvCoef,
     &            nstep+nptsRCR,numRCRSrfs)
            endif

#if(VER_CORONARY == 1)

c
c ... calc the pressure contribution that depends on the history for the Coronary BC
c
            if(numCORSrfs.gt.0) then 
               call CalcCORConvCoef(lstep,numCORSrfs) 

               call pHist(poldCOR, QHistCOR, CORConvCoef,
     &              nstep(1)+nptsCOR,numCORSrfs)

               call CalcHopCOR (Delt(itseq), lstep, 
     &              nsrflistCOR, 
     &              numCORSrfs,y)
            endif

#endif

c
c.... calculate initial residuals for the constrained surfaces
c   
            if(Lagrange.gt.zero .and. lstep .eq. zero) then
              call calcLagrangeErroric(y, colm, rowp,  
     &        nsrflistLagrange, numLagrangeSrfs)
            endif   

c
c.... -----------------------> predictor phase <-----------------------
c
            call itrPredict(yold, y,   acold,  ac ,  uold,  u)

            if(nsolt.eq.1) then
               isclr=0
               call itrBCSclr (y, ac, iBC, BC, iper, ilwork)
            endif
            do isclr=1,nsclr
               call itrBCSclr (y, ac, iBC, BC, iper, ilwork)
            enddo

            call itrBC (y, ac, iBC, BC, iper, ilwork)

            iter=0
            ilss=0  ! this is a switch thrown on first solve of LS redistance
           do istepc=1,seqsize
               icode=stepseq(istepc)
               if(mod(icode,5).eq.0) then ! this is a solve
                  isolve=icode/10
                  if(icode.eq.0) then ! flow solve (encoded as 0)
c
                     iter   = iter+1
                     ifuncs(1)  = ifuncs(1) + 1
c     
                     Force(1) = zero
                     Force(2) = zero
                     Force(3) = zero
                     HFlux    = zero
                     lhs = 1 - min(1,mod(ifuncs(1)-1,LHSupd(1))) 

#if(VER_CLOSEDLOOP == 1)
                     IF (numCoupledSrfs .GT. 0 ) THEN
                        GenFlag = 'T'
                        IF (lstep .LT. iGenInitialization) THEN
                          GenFlag = 'I'
                        ENDIF
                        CALL calcGenBC (y, yold)
                      
                        IF (PGenDerFlag .OR. ipvsq.EQ.3) THEN
                           CALL calcGenBCDerivative
                        END IF

                        IF (numDirichletSrfs .GT. 0) THEN
                           DO i=1,itvn
                              j = ptrDirichlet(i)
                              IF (j .NE. 0) THEN
                              BC(nBCt(i),3:5) = BCt(i,1,1:3)*QCoupled(j)
                              END IF
                           END DO
                           CALL itrBC (y, ac, iBC, BC, iper, ilwork)
                           
                           DO i=1,itvn
                              ii = nBCt(i)
                              IF (ptrDirichlet(i) .NE. 0) THEN
                                  ac(ii,1:3) = (gami-1d0)/gami*
     2                              acold(ii,1:3) + (y(ii,1:3) - 
     3                              yold(ii,1:3))/(Delt(itseq)*gami)
                              END IF
                           END DO
                        END IF
                     END IF
#endif

                    call SolFlow(y,           ac,        u,
     &                         yold,          acold,     uold,
     &                         x,             iBC,
     &                         BC,            res,
     &                         nPermDims,     nTmpDims,  aperm,
     &                         atemp,         iper,          
     &                         ilwork,        shp,       shgl,
     &                         shpb,          shglb,     rowp,     
     &                         colm,          lhsK,      lhsP,
     &                         solinc,        rerr,      sumtime,
     &                         svLS_lhs,     svLS_ls,  svLS_nFaces)
                  
                  else          ! scalar type solve
                     if (icode.eq.5) then ! Solve for Temperature
                                ! (encoded as (nsclr+1)*10)
                        isclr=0
                        ifuncs(2)  = ifuncs(2) + 1
                        j=1
                     else       ! solve a scalar  (encoded at isclr*10)
                        isclr=isolve  
                        ifuncs(isclr+2)  = ifuncs(isclr+2) + 1
                        j=isclr+nsolt
                     endif ! deciding between temperature and scalar

                     lhs = 1 - min(1,mod(ifuncs(isclr+2)-1,
     &                                   LHSupd(isclr+2))) 

                     call SolSclr(y,          ac,        u,
     &                         yold,          acold,     uold,
     &                         x,             iBC,
     &                         BC,            nPermDimsS,nTmpDimsS,  
     &                         apermS(1,1,j), atempS,    iper,          
     &                         ilwork,        shp,       shgl,
     &                         shpb,          shglb,     rowp,     
     &                         colm,          lhsS(1,j), 
     &                         solinc(1,isclr+5))
                        
                        
                  endif         ! end of scalar type solve

               else ! this is an update  (mod did not equal zero)
                  iupdate=icode/10  ! what to update
                  if(icode.eq.1) then !update flow  
                     call itrCorrect ( y,    ac,    u,   solinc)
                     call itrBC (y,  ac,  iBC,  BC, iper, ilwork)
                  else  ! update scalar
                     isclr=iupdate  !unless
                     if(icode.eq.6) isclr=0
                        call itrCorrectSclr (y, ac, solinc(1,isclr+5))
                     call itrBCSclr(y,ac,iBC,BC,iper,ilwork)
                     endif
                  endif         !end of switch between solve or update

                  if(rescontrol .gt. 0) then
                     if (controlResidual .lt. ResCriteria .and. 
     &                  CurrentIter .ge. MinNumIter) then
                        CurrentIter = 0
                        exit
                     endif      
                  endif                
               
               enddo            ! loop over sequence in step

#if(VER_CLOSEDLOOP == 1)
c           Updating the InitialData

            IF (numCoupledSrfs .GT. 0) THEN
               GenFlag = 'L'
               IF (lstep .GE. iGenInitialization) THEN
                  CALL calcGenBC (y, yold)
               END IF
            END IF
#endif
   
c     
c.... obtain the time average statistics
c
            if (ioform .eq. 2) then   

               call stsGetStats( y,      yold,     ac,     acold,
     &                           u,      uold,     x,
     &                           shp,    shgl,     shpb,   shglb,
     &                           iBC,    BC,       iper,   ilwork,
     &                           rowp,   colm,     lhsK,   lhsP )

            endif

c     
c  calculate wall deformation velocity before solution update
c
!            uvel=(u-uold)/Delt(1)

c
c  Find the solution at the end of the timestep and move it to old
c
            call itrUpdate( yold,  acold,   uold,  y,    ac,   u)
            call itrBC (yold, acold,  iBC,  BC,  iper,ilwork)

            istep = istep + 1
            lstep = lstep + 1
c
c ... write out the solution
c
            if (mod(lstep, ntout) .eq. 0) then
                 call restar ('out ',  yold  ,ac)
               if(ideformwall.eq.1) 
     &            call write_displ(myrank, lstep, nshg, 3, uold,
     &                              deformflag)
            endif
c 
c ... update the flow history for the impedance convolution, filter it and write it out
c    
            if(numImpSrfs.gt.zero) then
               call UpdHistConv(y,nsrflistImp,numImpSrfs) !uses Delt(1)
            endif

c 
c ... update the flow history for the RCR convolution
c    
            if(numRCRSrfs.gt.zero) then
               call UpdHistConv(y,nsrflistRCR,numRCRSrfs) !uses lstep
               call UpdRCR(y,nsrflistRCR,numRCRSrfs)
            endif

#if(VER_CORONARY == 1)
c 
c ... update the flow history for the Coronary convolution
c    
            if(numCORSrfs.gt.zero) then
               call UpdHistConv(y,nsrflistCOR,numCORSrfs) !uses lstep
               call UpdHistPlvConv(y, Delt(itseq), lstep, 
     &            nsrflistCOR, numCORSrfs) 
            endif

#endif

c 
c ... update the flow history for the CalcSurfaces
c    
            if(numCalcSrfs.gt.zero) then
               call Updcalc(y,nsrflistCalc,numCalcSrfs)
            endif
c
c.... calculate the values of constraint functions and write Lagrange Multipliers
c
           if (Lagrange .gt. 0) then
              call UpdateLagrangeCoef(y, colm, rowp, nsrflistLagrange,
     &           numLagrangeSrfs)
           endif               
c
c.... compute the consistent boundary flux
c
           if(tractionMethod.eq.0 .or. tractionMethod.eq.2) then
             if(nsrfCM.gt.0) then
                call newBflux ( yold,      acold,      uold,    x,
     &                      shp,       shgl,       shpb,   
     &                      shglb,     ilwork,     iBC,
     &                      BC,        iper)
             endif
           endif

           if(tractionMethod.eq.1 .or. tractionMethod.eq.2) then
             call Bflux ( yold,      acold,      uold,     x,
     &                      shp,       shgl,       shpb,
     &                      shglb,     ilwork,     iBC,
     &                      BC,        iper)
           endif



c....  print out results.
c
            ntoutv=max(ntout,100)   ! velb is not needed so often

c
c.... end of the NSTEP and NTSEQ loops
c
c
c.... -------------------> error calculation  <-----------------
c 
            if ((ioybar.eq.1) .or. (ioyerror.eq.1)) then
c$$$c
c$$$c compute average
c$$$c
c$$$               tfact=one/istep
c$$$               ybar =tfact*yold + (one-tfact)*ybar

c compute average
c ybar(:,1) - ybar(:,3) is average velocity components
c ybar(:,4) is average pressure
c ybar(:,5) is average speed
c averaging procedure justified only for identical time step sizes
c istep is number of time step
c
               tfact=one/istep

c ybar to contain the averaged ((u,v,w),p)-field
c and speed average, i.e sqrt(u^2+v^2+w^2)

               ybar(:,1) = tfact*yold(:,1) + (one-tfact)*ybar(:,1)
               ybar(:,2) = tfact*yold(:,2) + (one-tfact)*ybar(:,2)
               ybar(:,3) = tfact*yold(:,3) + (one-tfact)*ybar(:,3)
               ybar(:,4) = tfact*yold(:,4) + (one-tfact)*ybar(:,4)
c    
               dummyVar  = sqrt(yold(:,1)**2+yold(:,2)**2+yold(:,3)**2)

               if (istep .eq. 1) then
                  ybar(:,5) = dummyVar
               else
                  ybar(:,5) = tfact*dummyVar + (one-tfact)*ybar(:,5)
               endif
c
c compute errors
c
!               rerr(:, 7)=rerr(:, 7)+(yold(:,1)-ybar(:,1))**2
!               rerr(:, 8)=rerr(:, 8)+(yold(:,2)-ybar(:,2))**2
!               rerr(:, 9)=rerr(:, 9)+(yold(:,3)-ybar(:,3))**2
!               rerr(:,10)=rerr(:,10)+(yold(:,4)-ybar(:,4))**2
!               yerror(:,1) = sqrt((yold(:,1)-ybar(:,1))**2)
!               yerror(:,2) = sqrt((yold(:,2)-ybar(:,2))**2)
!               yerror(:,3) = sqrt((yold(:,3)-ybar(:,3))**2)
!               yerror(:,4) = sqrt((yold(:,4)-ybar(:,4))**2)
!               yerror(:,5) = sqrt((dummyVar-ybar(:,5))**2)
               yerror(:,1) = yerror(:,1)+(yold(:,1)-ybar(:,1))**2
               yerror(:,2) = yerror(:,2)+(yold(:,2)-ybar(:,2))**2
               yerror(:,3) = yerror(:,3)+(yold(:,3)-ybar(:,3))**2
               yerror(:,4) = yerror(:,4)+(yold(:,4)-ybar(:,4))**2
               yerror(:,5) = yerror(:,5)+(dummyVar-ybar(:,5))**2

            endif

            if (mod(lstep, ntout) .eq. 0) then
              if (ioybar.eq.1) then
                call append_restart(myrank, lstep, nshg, 1, ybar(:,5),
     &                             'average speed'//CHAR(0))
                call append_restart(myrank, lstep, nshg, 1, ybar(:,4),
     &                             'average pressure'//CHAR(0))
              endif

              if (ioyerror.eq.1) then
                call append_restart(myrank, lstep, nshg, 1,
     &                         yerror(:,5), 'speed error'//CHAR(0))
                call append_restart(myrank, lstep, nshg, 1,
     &                         yerror(:,4),'pressure error'//CHAR(0))
              endif

            endif
            

            if(istop.eq.1000) exit ! stop when delta small (see rstatic)
 2000    continue
 2001    continue
        

CAD         tcorecp2 = second(0)
CAD         tcorewc2 = second(-1)
         
CAD         write(6,*) 'T(core) cpu-wallclock = ',tcorecp2-tcorecp1,
CAD     &                                        tcorewc2-tcorewc1

 3000 continue

c
c.... ---------------------->  Post Processing  <----------------------
c
c.... print out the last step
c

      if ((mod(lstep, ntout) .ne. 0) .or.
     &     (nstp .eq. 0)) then
         call restar ('out ',  yold  ,ac)
         if(ideformwall.eq.1) 
     &        call write_displ(myrank, lstep, nshg, 3, u,deformflag)

          if (ioybar.eq.1) then
            call append_restart(myrank, lstep, nshg, 1, ybar(:,5),
     &                         'average speed'//CHAR(0))
            call append_restart(myrank, lstep, nshg, 1, ybar(:,4),
     &                         'average pressure'//CHAR(0))
          endif

          if (ioyerror.eq.1) then
            call append_restart(myrank, lstep, nshg, 1,
     &                     yerror(:,5), 'speed error'//CHAR(0))
            call append_restart(myrank, lstep, nshg, 1,
     &                     yerror(:,4),'pressure error'//CHAR(0))
          endif

      endif

      
         lesId   = numeqns(1)
      IF (svLSflag .NE. 1) THEN
         call saveLesRestart( lesId,  aperm , nshg, myrank, lstep,
     &                        nPermDims )
      END IF

!      if(ioybar.eq.1) then
!
!         itmp = 1
!         if (lstep .gt. 0) itmp = int(log10(float(lstep)))+1
!         write (fmt2,"('(''restart.'',i',i1,',1x)')") itmp
!         write (fname2,fmt2) lstep
!
!         fname2 = trim(fname2) // cname(myrank+1)
!c
!c.... open  files
!c
!         call openfile(  fname2,  'append?'//CHAR(0), irstin )
!
!         fnamer2 = 'ybar'
!         isize = nshg*5
!         nitems = 3
!         iarray(1) = nshg
!         iarray(2) = 5
!         iarray(3) = lstep
!         call writeheader(irstin, fnamer2,iarray, nitems, isize,
!     &        'double'//CHAR(0), iotype )
!
!         nitems = nshg*5
!         call writedatablock(irstin, fnamer2,ybar, nitems,
!     &        'double'//CHAR(0), iotype)
!
!         call closefile( irstin, "append"//CHAR(0) )
!
!      endif

 5    format(1X,F15.10,3X,F15.10,3X,F15.10,3X,F15.10)
 444  format(6(2x,e14.7))
c
c.... end
c
      if(nsolflow.eq.1) then
         deallocate (lhsK)
         deallocate (lhsP)
         IF (svLSFlag .NE. 1) THEN
         deallocate (aperm)
         deallocate (atemp)
         ENDIF
         if (Lagrange .gt. 0) then
            deallocate (lhsLagL)
         endif         
      endif
      if(nsclrsol.gt.0) then
         deallocate (lhsS)
         deallocate (apermS)
         deallocate (atempS)
      endif
      
      if(iabc==1) deallocate(acs)

      return

      CONTAINS 

c     =================== 
c     AddNeumannBCTosvLS 
c     ===================     

      SUBROUTINE AddNeumannBCTosvLS(srfID, faIn)
C
      INTEGER srfID, faIn

      INTEGER facenNo, i, j

      facenNo = 0
      DO i = 1, nshg
         IF (srfID .EQ. ndsurf(i)) THEN
            facenNo = facenNo + 1
         END IF
      END DO
      IF (ALLOCATED(gNodes)) DEALLOCATE(gNodes, sV)
      ALLOCATE(gNodes(facenNo), sV(nsd,facenNo))
      sV = 0D0
      j = 0
      DO i = 1, nshg
         IF (srfID .EQ. ndsurf(i)) THEN
            j = j + 1
            gNodes(j) = i
            sV(:,j) = NABI(i,1:3)
         END IF
      END DO
      CALL svLS_BC_CREATE(svLS_lhs, faIn, facenNo, 
     2   nsd, BC_TYPE_Neu, gNodes, sV)

      RETURN
      END SUBROUTINE AddNeumannBCTosvLS

      end
 

      subroutine deformableNode(ienb, iBCB, flg)

        include "global.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C     Local variables
C
      INTEGER             i,           j
C
c
      integer   flg(nshg),        iBCB(npro,ndiBCB),
     &          ienb(npro, nshl)

      do i=1, npro
         if (btest(iBCB(i,1),4)) then
            do j=1, nshlb
               flg(ienb(i,j))=1
            enddo
         endif
      enddo
c
      return
      end
