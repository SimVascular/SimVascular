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
      
      SUBROUTINE CGRAD(nFaces, dof, nNo, nnz, mynNo, commu, cS, face, 
     2   ls, rowPtr, colPtr, D, G, L, R)
      
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: nFaces, dof, nNo, nnz, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      TYPE(svLS_faceType), INTENT(IN) :: face(nFaces)
      TYPE(svLS_subLsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: D(dof,nnz), G(dof,nnz), L(nnz)
      REAL*8, INTENT(INOUT) :: R(nNo)
 
      INTEGER i
      REAL*8 errO, err, alpha, eps, time
      REAL*8 CPUT, NORMS, DOTS
      REAL*8, ALLOCATABLE :: X(:), P(:), SP(:), DGP(:), GP(:,:),
     2   unCondU(:,:)

      ALLOCATE(X(nNo), P(nNo), SP(nNo), DGP(nNo), GP(dof,nNo), 
     2   unCondU(dof,nNo))
 
      time     = CPUT()
      ls%suc   = .FALSE.
      errO     = NORMS(mynNo, commu, R)
      ls%iNorm = errO
      eps      = MAX(ls%absTol,ls%relTol*errO)
      eps      = eps*eps
      X        = 0D0

      err      = errO
      err      = err*err
      P        = R
      DO i=1, ls%mItr
         IF (err .LT. eps) THEN
            ls%suc = .TRUE.
            EXIT
         END IF
         errO = err
         CALL SPARMULSV(dof, nNo, nnz, commu, cS, rowPtr, colPtr,
     2      G, P, GP)

         IF (ANY(face%coupledFlag)) THEN
            unCondU = GP
            CALL ADDBCMUL(BCOP_TYPE_PRE, nFaces, dof, nNo, mynNo, commu,
     2         face, unCondU, GP)
         END IF

         CALL SPARMULVS(dof, nNo, nnz, commu, cS, rowPtr, colPtr,
     2      D, GP, DGP)
         
         CALL SPARMULSS(     nNo, nnz, commu, cS, rowPtr, colPtr,
     2      L, P, SP)
         
         SP    = SP - DGP
         alpha = errO/DOTS(mynNo, commu, P, SP)
         X     = X + alpha*P
         R     = R - alpha*SP
         err   = NORMS(mynNo, commu, R)
         err   = err*err
         P     = R + err/errO*P
      END DO
      R        = X
      ls%fNorm = SQRT(err)
      ls%callD = CPUT() - time + ls%callD
      ls%dB    = 5D0*LOG(err/errO)
      ls%itr   = ls%itr + i - 1
      
      DEALLOCATE(X, P, SP, DGP, GP)
      
      RETURN
      END SUBROUTINE CGRAD

!====================================================================
       
      SUBROUTINE CGRADV(dof, nNo, nnz, mynNo, commu, cS, ls, 
     2   rowPtr, colPtr, K, R)
      
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: dof, nNo, nnz, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      TYPE(svLS_subLsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: K(dof*dof,nnz)
      REAL*8, INTENT(INOUT) :: R(dof,nNo)
     
      INTEGER i
      REAL*8 errO, err, alpha, eps
      REAL*8 CPUT, NORMV, DOTV
      REAL*8, ALLOCATABLE :: P(:,:), KP(:,:), X(:,:)

      ALLOCATE(P(dof,nNo), KP(dof,nNo), X(dof,nNo))
      
      ls%callD = CPUT()
      ls%suc   = .FALSE.
      err      = NORMV(dof, mynNo, commu, R)
      ls%iNorm = err
      eps      = MAX(ls%absTol,ls%relTol*err)
      eps      = eps*eps
      err      = err*err
      X        = 0D0
      P        = R

      DO i=1, ls%mItr
         IF (err .LT. eps) THEN
            ls%suc = .TRUE.
            EXIT
         END IF
         errO = err
         CALL SPARMULVV(dof, nNo, nnz, commu, cS, rowPtr, colPtr,
     2      K, P, KP)
         
         alpha = errO/DOTV(dof, mynNo, commu, P, KP)
         X     = X + alpha*P
         R     = R - alpha*KP
         err   = NORMV(dof, mynNo, commu, R)
         err   = err*err
         P = R + err/errO*P
      END DO

      R        = X
      ls%itr   = i - 1
      ls%fNorm = SQRT(err)
      ls%callD = CPUT() - ls%callD
      ls%dB    = 5D0*LOG(err/errO)
      IF (i .GT. ls%mItr) ls%itr = ls%mItr

      RETURN
      END SUBROUTINE CGRADV

!====================================================================
       
      SUBROUTINE CGRADS(nNo, nnz, mynNo, commu, cS, ls, 
     2   rowPtr, colPtr, K, R)
      
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: nNo, nnz, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      TYPE(svLS_subLsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: K(nnz)
      REAL*8, INTENT(INOUT) :: R(nNo)
     
      INTEGER i
      REAL*8 errO, err, alpha, eps
      REAL*8 CPUT, NORMS, DOTS
      REAL*8, ALLOCATABLE :: P(:), KP(:), X(:)

      ALLOCATE(P(nNo), KP(nNo), X(nNo))
      
      ls%callD = CPUT()
      ls%suc   = .FALSE.
      err      = NORMS(mynNo, commu, R)
      ls%iNorm = err
      eps      = MAX(ls%absTol,ls%relTol*err)
      eps      = eps*eps
      err      = err*err
      X        = 0D0
      P        = R

      DO i=1, ls%mItr
         IF (err .LT. eps) THEN
            ls%suc = .TRUE.
            EXIT
         END IF
         errO = err
         CALL SPARMULSS(nNo, nnz, commu, cS, rowPtr, colPtr, K, P, KP)
         alpha = errO/DOTS(mynNo, commu, P, KP)
         X     = X + alpha*P
         R     = R - alpha*KP
         err   = NORMS(mynNo, commu, R)
         err   = err*err
         P = R + err/errO*P
      END DO

      R        = X
      ls%itr   = i - 1
      ls%fNorm = SQRT(err)
      ls%callD = CPUT() - ls%callD
      ls%dB    = 5D0*LOG(err/errO)
      IF (i .GT. ls%mItr) ls%itr = ls%mItr

      RETURN
      END SUBROUTINE CGRADS
