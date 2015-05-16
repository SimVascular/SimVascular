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

#if (VER_CLOSEDLOOP == 1)

!> This subroutine initiate the General boundary condition
 
      SUBROUTINE initGenBC

      USE GeneralBC
      INCLUDE "global.h"
      INCLUDE "mpif.h"
      INCLUDE "common_blocks/workfc.h"
      INCLUDE "common_blocks/conpar.h"
      INCLUDE "common_blocks/nomodule.h"

      REAL*8 temp(nshg)
      INTEGER loopA
      LOGICAL ierr

      ALLOCATE(nsrflistCoupled(0:MAXSURF), ECoupled(0:MAXSURF),
     2   QCoupled(0:MAXSURF), QnCoupled(0:MAXSURF), ACoupled(0:MAXSURF),
     2   PCoupled(0:MAXSURF), PnCoupled(0:MAXSURF), PGenDer(0:MAXSURF))

      IF (ipvsq .GE. 2) THEN
         PGenDerFlag = .TRUE.
      ELSE
         PGenDerFlag = .FALSE.
      END IF
      PGenDer = 0D0     

      nsrflistCoupled = nsrflistDirichlet
      nsrflistCoupled(numDirichletSrfs+1:numCoupledSrfs) = 
     2   nsrflistNeumann(1:numNeumannSrfs)
      
c     Eval Area of Coupled Surfaces
      temp = 1D0
      CALL integrScalar(ACoupled, temp, nsrflistCoupled, numCoupledSrfs)
      
      IF (myrank .EQ. master) THEN

c       CHECK AREA
        DO loopA = 1,numCoupledSrfs
          IF(ACoupled(loopA).LE.0.0D0)THEN
            PRINT *,'' 
            PRINT *,'WARNING: Zero Area for Coupled Surface ',loopA
            PRINT *,'' 
          ENDIF
        ENDDO

         IF (iGenFromFile .EQ. 1) THEN
            INQUIRE(FILE='GenBC',EXIST=ierr)
            IF (.NOT.ierr) THEN
               INQUIRE(FILE='../GenBC',EXIST=ierr)
               IF (ierr) THEN
                  CALL system('cp ../GenBC ./')
               ELSE
                  PRINT *, 'ERROR: No executable GenBC has been found'
                  PRINT *, 'Please, make sure that you had this', 
     2               ' executable file inside runnig directory'
                  STOP
               END IF
            END IF
         END IF

         INQUIRE(FILE='InitialData',EXIST=ierr)
         IF (ierr) THEN
            PRINT *, ''
            PRINT *, 'NOTE: Initializing General BC form previous simulation'
            PRINT *, ''
         ELSE
            INQUIRE(FILE='../InitialData',EXIST=ierr)
            IF (ierr) THEN
               CALL system('cp ../InitialData ./')
               PRINT *, ''
               PRINT *,'NOTE: General BC has been initialized from provided',
     2            ' file'
               PRINT *, ''
            ELSE
               PRINT *, ''
               PRINT *, 'NOTE: Self GenBC initializiation'
               PRINT *, ''
            END IF
         END IF
      END IF

      RETURN
      END SUBROUTINE initGenBC


!> Main subroutine for gathering required information and 
!! communicating with 0D to acquire necessary information

      SUBROUTINE calcGenBC (y, yold)
       
      USE GeneralBC
      INCLUDE "global.h"
      INCLUDE "mpif.h"
      INCLUDE "common_blocks/conpar.h"
      INCLUDE "common_blocks/workfc.h"
      INCLUDE "common_blocks/inpdat.h"
      INCLUDE "common_blocks/matdat.h"
      INCLUDE "common_blocks/nomodule.h"

      INTEGER MPIstat(MPI_STATUS_SIZE), i, ia

      REAL*8 y(nshg,ndof), yold(nshg,ndof), QInitial, en(nshg,ndof), 
     2       enOfEle,rho

      REAL*8 testnorm
      INTEGER ierr

c     Get Density
      rho = datmat(1,1,1)

      DO ia=1, nshg
         enOfEle = yold(ia,4)
         DO i=1, nsd
           enOfEle = enOfEle + 5D-1*rho*yold(ia,i)*yold(ia,i)
         END DO
         DO i=1, nsd
            en(ia,i) = enOfEle*yold(ia,i)
         END DO
      END DO
      
      CALL GetFlowQ (QCoupled,  y,    nsrflistCoupled, numCoupledSrfs) 
      CALL GetFlowQ (QnCoupled, yold, nsrflistCoupled, numCoupledSrfs)
      CALL GetFlowQ (ECoupled,  en,   nsrflistCoupled, numCoupledSrfs)

      CALL integrScalar (PCoupled, y(:,4),     nsrflistCoupled, 
     2   numCoupledSrfs)

      CALL integrScalar (PnCoupled, yold(:,4), nsrflistCoupled, 
     2   numCoupledSrfs)
      
      DO i=1, numCoupledSrfs
         PnCoupled(i) = PnCoupled(i)/ACoupled(i)
         PCoupled(i) = PCoupled(i)/ACoupled(i)
      END DO

      IF (myrank .EQ. master) THEN
         IF (GenFlag .EQ. 'L') THEN
            i = numCoupledSrfs
            CALL printGen (i, nsrflistCoupled(1:i), QCoupled(1:i), 
     2         PCoupled(1:i), ECoupled(1:i))
         END IF

         OPEN (1,FILE='GenBC.int', STATUS='UNKNOWN', FORM='UNFORMATTED')
         WRITE (1) GenFlag
         WRITE (1) Delt(1)
         WRITE (1) numDirichletSrfs
         WRITE (1) numNeumannSrfs
         DO i=1, numCoupledSrfs
            IF (i .LE. numDirichletSrfs) THEN
               WRITE (1) PnCoupled(i), PCoupled(i)
            ELSE
               WRITE (1) QnCoupled(i), QCoupled(i)
            END IF
         END DO
         CLOSE (1)

         
c        EXTERNAL CALL TO GENBC
         IF (iGenFromFile .EQ. 1) THEN            
            CALL system('./GenBC')
         ELSE
            CALL system('GenBC')
         END IF

         OPEN (1,FILE='GenBC.int',STATUS='OLD', FORM='UNFORMATTED')
         DO i=1, numCoupledSrfs
            IF (i .LE. numDirichletSrfs) THEN
               READ (1) QCoupled(i)
            ELSE
               READ (1) PCoupled(i)
            END IF
         END DO
         CLOSE(1)
      END IF
      
      i = MAXSURF + 1
      CALL MPI_BCAST(QCoupled, i, MPI_DOUBLE_PRECISION, master,
     2   MPI_COMM_WORLD, ierr)
      
      CALL MPI_BCAST(PCoupled, i, MPI_DOUBLE_PRECISION, master,
     2   MPI_COMM_WORLD, ierr)
      
      RETURN
      END SUBROUTINE calcGenBC

!> Main subroutine for calculating the general surface pressure 
!! derivative with respect to flow rate

      SUBROUTINE calcGenBCDerivative
       
      USE GeneralBC
      INCLUDE "global.h"
      INCLUDE "mpif.h"
      INCLUDE "common_blocks/workfc.h"
      INCLUDE "common_blocks/inpdat.h"
      INCLUDE "common_blocks/nomodule.h"

      INTEGER MPIstat(MPI_STATUS_SIZE), i0
      REAL*8 diff, PBase(numCoupledSrfs), garbage
      REAL*8, PARAMETER :: absTol = 1D-8, relTol = 1D-5
      INTEGER ierr,i,j

      IF (numNeumannSrfs .EQ. 0) RETURN
      
      PGenDerFlag = .FALSE.
      i0 = numDirichletSrfs

      diff = SUM(ABS(QCoupled(i0+1:numCoupledSrfs)))
     2   /REAL(numNeumannSrfs,8)

      IF (diff*relTol .LT. absTol) THEN
         diff = absTol
      ELSE
         diff = diff*relTol
      END IF

      IF (myrank .EQ. master) THEN
         DO j=i0, numCoupledSrfs
            OPEN (1, FILE='GenBC.int', STATUS='UNKNOWN', 
     2         FORM='UNFORMATTED')
            WRITE (1) 'D'
            WRITE (1) Delt(1)
            WRITE (1) numDirichletSrfs
            WRITE (1) numNeumannSrfs
            DO i=1, numCoupledSrfs
               IF (i .LE. i0) THEN
                  WRITE (1) PnCoupled(i), PCoupled(i)
               ELSE
                  IF (i .NE. j) THEN
                     WRITE (1) QnCoupled(i), QCoupled(i)
                  ELSE
                     WRITE (1) QnCoupled(i), QCoupled(i) + diff
                  END IF
               END IF
            END DO
            CLOSE (1)
            
            IF (iGenFromFile .EQ. 1) THEN
               CALL system('./GenBC')
            ELSE
               CALL system('GenBC')
            END IF

            OPEN (1,FILE='GenBC.int',STATUS='OLD', FORM='UNFORMATTED')
            DO i=1, numCoupledSrfs
               IF (i .LE. i0) THEN
                  READ (1) garbage
               ELSE
                  IF (i .NE. j) THEN
                     IF (j .EQ. i0) THEN
                        READ (1) PBase(i)
                     ELSE
                        READ (1) garbage
                     END IF
                  ELSE
                     READ (1) PGenDer(i)
                     PGenDer(i) = (PGenDer(i) - PBase(i))/diff
                  END IF
               END IF
            END DO
            CLOSE(1)
         END DO
      END IF

      i = MAXSURF + 1
      CALL MPI_BCAST(PGenDer, i, MPI_DOUBLE_PRECISION, master,
     2   MPI_COMM_WORLD, ierr)

      RETURN
      END SUBROUTINE calcGenBCDerivative

!> Writting results to the disk

      SUBROUTINE printGen (nSrfs, surfID, Q, P, E)
       
      IMPLICIT NONE

      LOGICAL ierr

      INTEGER nSrfs, nParam, i
      PARAMETER (nParam = 3)
      INTEGER surfID(nSrfs)

      REAL*8 Q(nSrfs), P(nSrfs), E(nSrfs), R(nParam,nSrfs)
 
      CHARACTER*32 fileNames(nParam)
      CHARACTER(len=32) :: myFMT1,myFMT2

      IF (nSrfs .EQ. 0) RETURN

      fileNames = (/'QGeneral','PGeneral','EGeneral'/)

      R(1,:) = Q
      R(2,:) = P
      R(3,:) = E

c     SET FORMATS
      WRITE(myFMT1, '("(",I0,"(E13.5))")') nSrfs
      WRITE(myFMT2, '("(",I0,"(I13))")') nSrfs

      DO i=1, nParam
         INQUIRE(FILE=TRIM(fileNames(i)), EXIST=ierr)
         IF (ierr) THEN
            OPEN (1, FILE=TRIM(fileNames(i)), STATUS='OLD', 
     2         ACCESS='APPEND')

            WRITE (1,fmt=myFMT1) R(i,:)
            CLOSE (1)
         ELSE
            OPEN (1, FILE=TRIM(fileNames(i)), STATUS='NEW')
            WRITE (1,fmt=myFMT2) surfID
            WRITE (1,fmt=myFMT1) R(i,:)
            CLOSE (1)
         END IF
      END DO

      RETURN
      END SUBROUTINE printGen


!> This subroutine applies the normal velocity constraint
!! for the selected surfaces

      SUBROUTINE normCon (iens, y, xKebe, xGoC, rl)

! This is how the xKebe is formed
!  1 2 3
!  4 5 6
!  7 8 9

      USE pvsQbi

      INCLUDE "global.h"
      INCLUDE "common_blocks/propar.h"
      INCLUDE "common_blocks/shpdat.h"
      INCLUDE "common_blocks/conpar.h"
      INCLUDE "common_blocks/elmpar.h"
      INCLUDE "common_blocks/nomodule.h"

      INTEGER iens(npro,nshape), ie, ia, ib, iAc, i, j, l, iSrf

      REAL*8 norm(nsd,nsd), ndy, y(nshg, ndofl), 
     2   xKebe(npro,9,nshl,nshl), 
     3   K(nsd,nsd), nK(nsd,nsd), nKnt(nsd,nsd), 
     4   xGoC(npro,4,nshl,nshl), G(nsd), nG(nsd),
     5   rl(npro,nshl,nflow), R(nsd), nR(nsd)

      INTENT(IN) iens, y
      INTENT(INOUT) xKebe, xGoC, rl

c      return
      DO iSrf = 1, numNormalSrfs
         DO ie = 1, npro
            DO ia = 1, nshl
               iAc = ABS(iens(ie,ia))
               IF (ndsurf(iAc) .EQ. nsrflistNormal(iSrf)) THEN
                  CALL getNorm (iAc, norm)

                  ndy = 0D0
                  DO i = 1, nsd
                     ndy = ndy + norm(1,i)*y(iA,i)
                  END DO
                  IF (.true.)then!ndy .LT. 0D0) THEN
c********************************************************************
c First modifing RHS matrix
                     DO i = 1, nsd
                        R(i) = rl(ie,ia,i)
                     END DO
c Here setting nR = n*R
                     DO i=1, nsd
                        nR(i) = 0D0
                        DO j=1,nsd
                           nR(i) = nR(i) + norm(i,j)*R(j)
                        END DO
                     END DO

c Now setting second and third row equal to zero, since the normal to the face is
c in the first direction
                     DO i=2, nsd
                        nR(i) = 0D0
                     END DO

c Rotating back the coordinate R = n^t*nR
                     DO i=1, nsd
                        R(i) = 0D0
                        DO j=1,nsd
                           R(i) = R(i) + norm(j,i)*nR(j)
                        END DO
                     END DO

c Assigning the new residual vector
                     DO i = 1, nsd
                        rl(ie,ia,i) = R(i)
                     END DO

c********************************************************************
c Now modifing LHS matrices

                     DO ib = 1, nshl
                        DO i=1, nsd
                           G(i) = xGoC(ie,i,ia,ib)
                           DO j=1,nsd
                              K(i,j) = xKebe(ie,j+nsd*(i-1),ia,ib)
                           END DO
                        END DO
c
c Here setting nK = n*K and nG = n*G
c
                        DO i=1, nsd
                           nG(i) = 0D0
                           DO j=1,nsd
                              nG(i) = nG(i) + norm(i,j)*G(j)
                              nK(i,j) = 0D0
                              DO l=1, nsd
                                 nK(i,j) = nK(i,j) + norm(i,l)*K(l,j)
                              END DO
                           END DO
                        END DO
c
c Here setting nKnt = nK*n^t
c
                        DO i=1, nsd
                           DO j=1,nsd
                              nKnt(i,j) = 0D0
                              DO l=1, nsd
                                 nKnt(i,j) = nKnt(i,j) +
     2                              nK(i,l)*norm(j,l)
                              END DO
                           END DO
                        END DO
c
c Now setting second and third row equal to zero, since the normal to the face is
c in the first direction and we dont want to constrain the velocity in normal direction, 
c first row should be conserved. Note that diagonal element is not chaged to prevent 
c singularity of stiffness matrix
c
                        DO i=2, nsd
                           IF (ia .NE. ib) THEN
                              nG(i) = 0D0
                              DO j=1,nsd
                                 nKnt(i,j) = 0D0
                              END DO
                           ELSE
                              nG(i) = 0D0
                              DO j=1,nsd
                                 IF (i.NE.j) THEN
                                    nKnt(i,j) = 0D0
                                 END IF
                              END DO
                           END IF
                        END DO

c Rotating back the coordinate nK = nKnt*n and G = nt*nG

                        DO i=1, nsd
                           G(i) = 0D0
                           DO j=1,nsd
                              G(i) = G(i) + norm(j,i)*nG(j)
                              nK(i,j) = 0D0
                              DO l=1, nsd
                                 nK(i,j) = nK(i,j) + nKnt(i,l)*norm(l,j)
                              END DO
                           END DO
                        END DO
c
c Rotating back the coordinate K = n^t*nK
c
                        DO i=1, nsd
                           DO j=1,nsd
                              K(i,j) = 0D0
                              DO l=1, nsd
                                 K(i,j) = K(i,j) + norm(l,i)*nK(l,j)
                              END DO
                           END DO
                        END DO
c
c Assigning the new stiffness matrix
c
                        DO i=1, nsd
                           xGoC(ie, i, ia, ib) = G(i)
                           DO j=1,nsd
                              xKebe(ie, j + nsd*(i-1), ia, ib) = K(i,j)
                           END DO
                        END DO
                     END DO ! loop for another local node 
                  END IF ! if clause for inward velocity
               END IF ! if clause for nodes inside the face
            END DO ! loop for the node
         END DO ! loop over the elements
      END DO ! loop over the faces

      RETURN

      END SUBROUTINE normCon

!> This subroutine is intended for calculating normal vector

      SUBROUTINE getNorm(iAc, norm)

      USE pvsQbi

      INCLUDE "global.h"

      INTEGER iAc, i
      REAL*8 normal(nsd), tang1(nsd), tang2(nsd), temp, norm(nsd, nsd)

      INTENT(IN) iAc
      INTENT(OUT) norm

      temp = 0D0
      DO i=1,nsd
         normal(i) = NABI(iAc,i)
         temp = temp + normal(i)**2D0
      END DO
      temp = SQRT(temp)
      normal = normal/temp

      IF (ABS(normal(1)) .NE. 1D0) THEN
         tang1(1) = 0D0
         tang1(2) = normal(3)
         tang1(3) = -normal(2)

         temp = 0D0
         DO i=1,nsd
            temp = temp + tang1(i)**2D0
         END DO
         temp = SQRT(temp)
         tang1 = tang1/temp
      ELSE
         tang1 = (/0D0,1D0,0D0/)
      END IF

      tang2(1) = normal(2)*tang1(3) - normal(3)*tang1(2)
      tang2(2) = normal(3)*tang1(1) - normal(1)*tang1(3)
      tang2(3) = normal(1)*tang1(2) - normal(2)*tang1(1)

      norm(1,:) = normal
      norm(2,:) = tang1
      norm(3,:) = tang2

      RETURN
      END SUBROUTINE getNorm

#endif
