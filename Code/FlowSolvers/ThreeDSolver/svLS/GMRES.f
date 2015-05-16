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
      
      SUBROUTINE GMRES(nFaces, dof, nNo, nnz, mynNo, commu, cS, face, 
     2   ls, rowPtr, colPtr, Val, R, X)
      
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: nFaces, dof, nNo, nnz, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      TYPE(svLS_faceType), INTENT(IN) :: face(nFaces)
      TYPE(svLS_subLsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: Val(dof*dof,nnz), R(dof,nNo)
      REAL*8, INTENT(OUT) :: X(dof,nNo)
     
      INTEGER i, j, k, l
      REAL*8 CPUT, NORMV, DOTV
      REAL*8 eps, tmp, time
      REAL*8, ALLOCATABLE :: u(:,:,:), h(:,:), unCondU(:,:), y(:), c(:),
     2   s(:), err(:)

      ALLOCATE(h(ls%sD+1,ls%sD), u(dof,nNo,ls%sD+1), unCondU(dof,nNo), 
     2   y(ls%sD), c(ls%sD), s(ls%sD), err(ls%sD+1))
       
      time   = CPUT()
      ls%suc = .FALSE.

      X = 0D0
      DO l=1, ls%mItr
         IF (l .EQ. 1) THEN
            u(:,:,1) = R
         ELSE
            ls%itr = ls%itr + 1
            CALL SPARMULVV(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2         Val, X, u(:,:,1))
         
            CALL ADDBCMUL(BCOP_TYPE_ADD, nFaces, dof, nNo, mynNo, commu,
     2         face, X, u(:,:,1))
         
            u(:,:,1) = R - u(:,:,1)
         END IF
         IF (ANY(face%coupledFlag)) THEN
            unCondU = u(:,:,1)
            CALL ADDBCMUL(BCOP_TYPE_PRE, nFaces, dof, nNo, mynNo, commu,
     2         face, unCondU, u(:,:,1))
         END IF

         err(1)   = NORMV(dof, mynNo, commu, u(:,:,1))
         IF (l .EQ. 1) THEN
            eps       = err(1)
            IF (eps .LE. ls%absTol) THEN
               ls%callD = 0D0
               ls%dB    = 0D0
               RETURN
            END IF
            ls%iNorm  = eps
            ls%fNorm  = eps
            eps       = MAX(ls%absTol,ls%relTol*eps)
         END IF
         ls%dB = ls%fNorm
         u(:,:,1) = u(:,:,1)/err(1)
         DO i=1, ls%sD
            ls%itr = ls%itr + 1
            CALL SPARMULVV(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2         Val, u(:,:,i), u(:,:,i+1))
            
            CALL ADDBCMUL(BCOP_TYPE_ADD, nFaces, dof, nNo, mynNo, commu,
     2         face, u(:,:,i), u(:,:,i+1))
            
            IF (ANY(face%coupledFlag)) THEN
               unCondU = u(:,:,i+1)
               CALL ADDBCMUL(BCOP_TYPE_PRE, nFaces, dof, nNo, mynNo, 
     2            commu, face, unCondU, u(:,:,i+1))
            END IF
            DO j=1, i
               h(j,i) = DOTV(dof, mynNo, commu, u(:,:,i+1), u(:,:,j))
               u(:,:,i+1) = u(:,:,i+1) - h(j,i)*u(:,:,j)
            END DO
            h(i+1,i)   = NORMV(dof, mynNo, commu, u(:,:,i+1))
            
            u(:,:,i+1) = u(:,:,i+1)/h(i+1,i)
            DO j=1, i-1
               tmp      =  c(j)*h(j,i) + s(j)*h(j+1,i)
               h(j+1,i) = -s(j)*h(j,i) + c(j)*h(j+1,i)
               h(j,i)   =  tmp
            END DO
            tmp      = SQRT(h(i,i)*h(i,i) + h(i+1,i)*h(i+1,i))
            c(i)     = h(i,i)/tmp
            s(i)     = h(i+1,i)/tmp
            h(i,i)   = tmp
            h(i+1,i) = 0D0
            err(i+1) = -s(i)*err(i)
            err(i)   =  c(i)*err(i)
            IF (ABS(err(i+1)) .LT. eps) THEN
               ls%suc = .TRUE.
               EXIT
            END IF
         END DO
         IF (i .GT. ls%sD) i = ls%sD

         y = err(1:i)
         DO j=i, 1, -1
            DO k=j+1, i
               y(j) = y(j) - h(j,k)*y(k)
            END DO
            y(j) = y(j)/h(j,j)
         END DO
 
         DO j=1, i
            X = X + u(:,:,j)*y(j)
         END DO
         ls%fNorm = ABS(err(i+1))
         IF (ls%suc) EXIT
      END DO

      ls%callD = CPUT() - time + ls%callD
      ls%dB    = 1D1*LOG(ls%fNorm/ls%dB)

      RETURN
      END SUBROUTINE GMRES

!====================================================================
      
      SUBROUTINE GMRESV(nFaces, dof, nNo, nnz, mynNo, commu, cS, face, 
     2   ls, rowPtr, colPtr, Val, R)
      
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: nFaces, dof, nNo, nnz, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      TYPE(svLS_faceType), INTENT(IN) :: face(nFaces)
      TYPE(svLS_subLsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: Val(dof*dof,nnz)
      REAL*8, INTENT(INOUT) :: R(dof,nNo)
 
      INTEGER i, j, k, l
      REAL*8 CPUT, NORMV, DOTV
      REAL*8 eps, tmp
      REAL*8, ALLOCATABLE :: u(:,:,:), h(:,:), X(:,:), y(:), c(:), s(:),
     2   err(:)

      ALLOCATE(h(ls%sD+1,ls%sD), u(dof,nNo,ls%sD+1), X(dof,nNo), 
     2   y(ls%sD), c(ls%sD), s(ls%sD), err(ls%sD+1))
       
      ls%callD  = CPUT()
      ls%suc    = .FALSE.
      eps       = NORMV(dof, mynNo, commu, R)
      ls%iNorm  = eps
      ls%fNorm  = eps
      eps       = MAX(ls%absTol,ls%relTol*eps)
      ls%itr    = 0
      X         = 0D0

      IF (ls%iNorm .LE. ls%absTol) THEN
         ls%callD = 0D0
         ls%dB    = 0D0
         RETURN
      END IF
      DO l=1, ls%mItr
         ls%dB = ls%fNorm
         ls%itr = ls%itr + 1
         CALL SPARMULVV(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2      Val, X, u(:,:,1))
         CALL ADDBCMUL(BCOP_TYPE_ADD, nFaces, dof, nNo, mynNo, commu,
     2      face, X, u(:,:,1))
         
         u(:,:,1) = R - u(:,:,1)
         err(1)   = NORMV(dof, mynNo, commu, u(:,:,1))
         u(:,:,1) = u(:,:,1)/err(1)
         DO i=1, ls%sD
            ls%itr = ls%itr + 1
            CALL SPARMULVV(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2         Val, u(:,:,i), u(:,:,i+1))
            CALL ADDBCMUL(BCOP_TYPE_ADD, nFaces, dof, nNo, mynNo, commu,
     2         face, u(:,:,i), u(:,:,i+1))
            
            DO j=1, i
               h(j,i) = DOTV(dof, mynNo, commu, u(:,:,i+1), u(:,:,j))
               u(:,:,i+1) = u(:,:,i+1) - h(j,i)*u(:,:,j)
            END DO
            h(i+1,i)   = NORMV(dof, mynNo, commu, u(:,:,i+1))
            u(:,:,i+1) = u(:,:,i+1)/h(i+1,i)
            DO j=1, i-1
               tmp      =  c(j)*h(j,i) + s(j)*h(j+1,i)
               h(j+1,i) = -s(j)*h(j,i) + c(j)*h(j+1,i)
               h(j,i)   =  tmp
            END DO
            tmp      = SQRT(h(i,i)*h(i,i) + h(i+1,i)*h(i+1,i))
            c(i)     = h(i,i)/tmp
            s(i)     = h(i+1,i)/tmp
            h(i,i)   = tmp
            h(i+1,i) = 0D0
            err(i+1) = -s(i)*err(i)
            err(i)   =  c(i)*err(i)
            IF (ABS(err(i+1)) .LT. eps) THEN
               ls%suc = .TRUE.
               EXIT
            END IF
         END DO
         IF (i .GT. ls%sD) i = ls%sD

         y = err(1:i)
         DO j=i, 1, -1
            DO k=j+1, i
               y(j) = y(j) - h(j,k)*y(k)
            END DO
            y(j) = y(j)/h(j,j)
         END DO
 
         DO j=1, i
            X = X + u(:,:,j)*y(j)
         END DO
         ls%fNorm = ABS(err(i+1))
         IF (ls%suc) EXIT
      END DO
      R = X
      ls%callD = CPUT() - ls%callD
      ls%dB    = 1D1*LOG(ls%fNorm/ls%dB)

      RETURN
      END SUBROUTINE GMRESV

