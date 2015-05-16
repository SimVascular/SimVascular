!     This software is Copyright (c) 2012-2015 The Regents of the 
!     University of California. All Rights Reserved.
!
!     Permission to copy and modify this software and its documentation
!     for educational, research and non-profit purposes, without fee, 
!     and without a written agreement is hereby granted, provided that
!     the above copyright notice, this paragraph and the following three
!     paragraphs appear in all copies.
!
!     Permission to make commercial use of this software may be obtained
!     by contacting:
!
!     Technology Transfer Office
!     9500 Gilman Drive, Mail Code 0910
!     University of California
!     La Jolla, CA 92093-0910
!     (858) 534-5815
!     invent@ucsd.edu
!
!     This software program and documentation are copyrighted by The
!     Regents of the University of California. The software program and
!     documentation are supplied "as is", without any accompanying
!     services from The Regents. The Regents does not warrant that the
!     operation of the program will be uninterrupted or error-free. The
!     end-user understands that the program was developed for research
!     purposes and is advised not to rely exclusively on the program for
!     any reason.
!
!     IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY 
!     PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL 
!     DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS 
!     SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF 
!     CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
!     THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY 
!     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
!     OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE 
!     SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE 
!     UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO PROVIDE 
!     MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
      
      SUBROUTINE NSSOLVER(nFaces, gnNo, dof, nNo, nnz, mynNo, commu, 
     2   cS, face, ls, rowPtr, colPtr, Val, Ri)
      
      INCLUDE "svLS_STD.h"
      

      INTEGER, INTENT(IN) :: nFaces, gnNo, dof, nNo, nnz, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      TYPE(svLS_faceType), INTENT(INOUT) :: face(nFaces)
      TYPE(svLS_lsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: Val(dof*dof,nnz)
      REAL*8, INTENT(INOUT) :: Ri(dof,nNo)

      INTEGER i, j, k, iB, iBB, nB, nsd
      REAL*8 CPUT, NORMS, NORMV, DOTS, DOTV, eps
      REAL*8, ALLOCATABLE :: U(:,:,:), P(:,:),
     2   MU(:,:,:), MP(:,:), A(:,:), B(:), xB(:), mK(:,:), mG(:,:), 
     3   mD(:,:), mL(:), Gt(:,:), Rm(:,:), Rc(:), Rmi(:,:), Rci(:)
 
      nsd = dof - 1
      iB = ls%RI%mItr
      nB = 2*iB
      ALLOCATE(Rm(nsd,nNo), Rc(nNo), Rmi(nsd,nNo), Rci(nNo), 
     2   U(nsd,nNo,iB), P(nNo,iB), MU(nsd,nNo,nB), MP(nNo,nB), 
     3   A(nB,nB), B(nB), xB(nB))

      Rmi = Ri(1:nsd,:)
      Rci = Ri(dof,:)

      xB          = 0D0
      B           = 0D0
      Rm          = Rmi
      Rc          = Rci
      eps         = SQRT(NORMV(nsd, mynNo, commu, Rm)**2D0
     2            +      NORMS(     mynNo, commu, Rc)**2D0)
      ls%RI%iNorm = eps
      ls%RI%fNorm = eps
      ls%CG%callD = 0D0
      ls%GM%callD = 0D0
      ls%CG%itr   = 0
      ls%GM%itr   = 0
      ls%RI%callD = CPUT()
      ls%RI%suc   = .FALSE.
      eps         = MAX(ls%RI%absTol,ls%RI%relTol*eps)

      CALL DEPART
      CALL BCPRE

      DO i=1, ls%RI%mItr
         iB  = 2*i - 1
         iBB = 2*i
         ls%RI%dB = ls%RI%fNorm
         CALL GMRES(nFaces, nsd, nNo, nnz, mynNo, commu, cS, face, 
     2      ls%GM, rowPtr, colPtr, mK, Rm, U(:,:,i))  
         
         CALL SPARMULVS(nsd, nNo, nnz, commu, cS, 
     2      rowPtr, colPtr, mD, U(:,:,i), P(:,i))   
         
         P(:,i) = Rc - P(:,i)       
         CALL CGRAD(nFaces, nsd, nNo, nnz, mynNo, commu, cS, face, 
     2      ls%CG, rowPtr, colPtr, Gt, mG, mL, P(:,i)) 
         
         CALL SPARMULSV(nsd, nNo, nnz, commu, cS, 
     2      rowPtr, colPtr, mG, P(:,i), MU(:,:,iB)) 
         
         MU(:,:,iBB) = Rm - MU(:,:,iB)   
         CALL GMRES(nFaces, nsd, nNo, nnz, mynNo, commu, cS, face, 
     2      ls%GM, rowPtr, colPtr, mK, MU(:,:,iBB), U(:,:,i)) 
         
         CALL SPARMULVV(nsd, nNo, nnz, commu, cS, 
     2      rowPtr, colPtr, mK, U(:,:,i), MU(:,:,iBB))
         
         CALL ADDBCMUL(BCOP_TYPE_ADD, nFaces, nsd, nNo, mynNo, commu, 
     2      face, U(:,:,i), MU(:,:,iBB))
         
         CALL SPARMULSS(nNo, nnz, commu, cS, 
     2      rowPtr, colPtr, mL, P(:,i), MP(:,iB))
         
         CALL SPARMULVS(nsd, nNo, nnz, commu, cS, 
     2      rowPtr, colPtr, mD, U(:,:,i), MP(:,iBB))

         DO k=iB, iBB
            DO j=1, k - 1
               A(j,k) = DOTV(nsd, mynNo, commu, MU(:,:,j), MU(:,:,k))
     2                + DOTS(     mynNo, commu, MP(:,j),   MP(:,k))
               A(k,j) = A(j,k)
            END DO
            A(k,k) = NORMV(nsd, mynNo, commu, MU(:,:,k))**2D0 
     2             + NORMS(     mynNo, commu, MP(:,k))**2D0
            B(k)   = DOTV (nsd, mynNo, commu, MU(:,:,k), Rmi) 
     2             + DOTS (     mynNo, commu, MP(:,k), Rci)
         END DO

         xB = B
         CALL GE(iBB, A(1:iBB,1:iBB), xB(1:iBB))

         ls%RI%fNorm = SQRT(ls%RI%iNorm**2D0 - SUM(xB(1:iBB)*B(1:iBB)))
         IF(ls%RI%fNorm .LT. eps) THEN
            ls%RI%suc = .TRUE.
            EXIT
         END IF
 
         Rm = Rmi - xB(1)*MU(:,:,1)
         Rc = Rci - xB(1)*MP(:,1)
         DO j=2, iBB
            Rm = Rm - xB(j)*MU(:,:,j)
            Rc = Rc - xB(j)*MP(:,j)
         END DO
      END DO
      IF (i .GT. ls%RI%mItr) THEN
         ls%RI%itr = ls%RI%mItr
      ELSE
         ls%RI%itr = i
 
         Rc = Rci - xB(1)*MP(:,1)
         DO j=2, iBB
            Rc = Rc - xB(j)*MP(:,j)
         END DO
      END IF
      ls%Resc = NINT(1D2*(NORMS(mynNo, commu, Rc)/
     2   ls%RI%fNorm)**2D0)
      ls%Resm = 100 - ls%Resc
 
      Rmi = xB(2)*U(:,:,1)
      Rci = xB(1)*P(:,1)
      DO i=2, ls%RI%itr
         iB  = 2*i - 1
         iBB = 2*i

         Rmi = Rmi + xB(iBB)*U(:,:,i)
         Rci = Rci + xB(iB)*P(:,i)
      END DO

      ls%RI%callD = CPUT() - ls%RI%callD
      ls%RI%dB    = 1D1*LOG(ls%RI%fNorm/ls%RI%dB)
      
      Ri(1:nsd,:) = Rmi
      Ri(dof,:) = Rci
      
      DEALLOCATE (Rm, Rc, Rmi, Rci, U, P, MU, MP, A, B, mK, mD, mG, mL, 
     2   Gt)
 
      IF (commu%masF) CALL LOGFILE

      RETURN
      CONTAINS

!====================================================================

      SUBROUTINE DEPART

      IMPLICIT NONE

      INTEGER i, j, k, l
      REAL*8, ALLOCATABLE :: tmp(:)
      
      ALLOCATE(mK(nsd*nsd,nnz), mG(nsd,nnz), mD(nsd,nnz), mL(nnz), 
     2   Gt(nsd,nnz), tmp((nsd+1)*(nsd+1)))

      IF (nsd .EQ. 2) THEN
         DO i=1, nnz
            tmp = Val(:,i)

            mK(1,i) = tmp(1)
            mK(2,i) = tmp(2)
            mK(3,i) = tmp(4)
            mK(4,i) = tmp(5)

            mG(1,i) = tmp(3)
            mG(2,i) = tmp(6)

            mD(1,i) = tmp(7)
            mD(2,i) = tmp(8)
            
            mL(i)   = tmp(9)
         END DO
      ELSE IF(nsd .EQ. 3) THEN
         DO i=1, nnz
            tmp = Val(:,i)
   
            mK(1,i) = tmp(1)
            mK(2,i) = tmp(2)
            mK(3,i) = tmp(3)
            mK(4,i) = tmp(5)
            mK(5,i) = tmp(6)
            mK(6,i) = tmp(7)
            mK(7,i) = tmp(9)
            mK(8,i) = tmp(10)
            mK(9,i) = tmp(11)
            
            mG(1,i) = tmp(4)
            mG(2,i) = tmp(8)
            mG(3,i) = tmp(12)

            mD(1,i) = tmp(13)
            mD(2,i) = tmp(14)
            mD(3,i) = tmp(15)

            mL(i)   = tmp(16)
         END DO
      ELSE
         PRINT *, "Not defined nsd for DEPART", nsd
      END IF
 
      DO i=1, nNo
         Do j=rowPtr(1,i), rowPtr(2,i)
            k = colPtr(j)
            DO l=rowPtr(1,k), rowPtr(2,k)
               IF (colPtr(l) .EQ. i) THEN
                  Gt(:,l) = -mG(:,j)
                  EXIT
               END IF
            END DO
         END DO
      END DO

      RETURN
      END SUBROUTINE DEPART

!====================================================================
      
      SUBROUTINE BCPRE

      IMPLICIT NONE

      INTEGER faIn, i, a, Ac
      REAL*8 NORMV
      REAL*8, ALLOCATABLE :: v(:,:)

      DO faIn=1, nFaces
         IF (face(faIn)%coupledFlag) THEN
            IF (face(faIn)%sharedFlag) THEN
               IF (.NOT.ALLOCATED(v)) ALLOCATE(v(nsd,nNo))
               v = 0D0
               DO a=1, face(faIn)%nNo
                  Ac = face(faIn)%glob(a)
                  DO i=1, nsd
                     v(i,Ac) = face(faIn)%valM(i,a)
                  END DO
               END DO
               face(faIn)%nS = NORMV(nsd, mynNo, commu, v)**2D0
            ELSE
               face(faIn)%nS = 0D0
               DO a=1, face(faIn)%nNo
                  Ac = face(faIn)%glob(a)
                  DO i=1, nsd
                     face(faIn)%nS = face(faIn)%nS + 
     2                  face(faIn)%valM(i,a)**2D0
                  END DO
               END DO
            END IF
         END IF
      END DO

      RETURN
      END SUBROUTINE BCPRE

!====================================================================

      SUBROUTINE LOGFILE

      IMPLICIT NONE
 
      LOGICAL flag
      INTEGER fid, i, j
      CHARACTER*16, PARAMETER :: fName = 'svLS_NS.log'

      INQUIRE(FILE=fName, EXIST=flag)

      fid = 11232
      OPEN(fid, FILE=fName, POSITION='APPEND')
      
      IF (.NOT.flag) THEN
         i = 0
         DO j=1, nFaces
            IF (face(j)%coupledFlag) i = i + 1
         END DO
         WRITE(fid,*) gnNo, commu%nTasks, i
      END IF

      i = 0
      IF (ls%RI%suc) i = i + 100
      IF (ls%GM%suc) i = i + 10
      IF (ls%CG%suc) i = i + 1

      WRITE(fid,"(I4.3,I3,I4,I5,3I4,3ES9.2E2,3I4)") 
     2   i, ls%RI%itr, ls%GM%itr, ls%CG%itr, 
     3   NINT((ls%RI%CallD-ls%GM%CallD-ls%CG%CallD)/ls%RI%CallD*1D2),
     4   NINT(ls%GM%callD/ls%RI%CallD*1D2),
     5   NINT(ls%CG%callD/ls%RI%CallD*1D2),
     6   ls%RI%iNorm, ls%RI%fNorm/ls%RI%iNorm, ls%RI%CallD,
     7   ls%Resm, ls%Resc, NINT(ls%RI%dB) 

      CLOSE(fid)

      RETURN
      END SUBROUTINE LOGFILE

      END SUBROUTINE NSSOLVER

