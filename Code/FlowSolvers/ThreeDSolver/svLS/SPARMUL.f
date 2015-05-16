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
      
      SUBROUTINE SPARMULVV(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2   K, U, KU)

      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: dof, nNo, nnz
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: K(dof*dof,nnz), U(dof,nNo)
      REAL*8, INTENT(OUT) :: KU(dof,nNo)

      INTEGER i, j, l, col, s, e

      KU = 0D0
      SELECT CASE (dof)
      CASE (1)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               KU(1,i) = KU(1,i) + K(1,j)*U(1,colPtr(j))
            END DO
         END DO
      CASE(2)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(1,i) = KU(1,i) + K(1,j)*U(1,col) + K(2,j)*U(2,col)
               KU(2,i) = KU(2,i) + K(3,j)*U(1,col) + K(4,j)*U(2,col)
            END DO
         END DO
      CASE(3)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(1,i) = KU(1,i) + K(1,j)*U(1,col) + K(2,j)*U(2,col)
     2                           + K(3,j)*U(3,col)
               KU(2,i) = KU(2,i) + K(4,j)*U(1,col) + K(5,j)*U(2,col)
     2                           + K(6,j)*U(3,col)
               KU(3,i) = KU(3,i) + K(7,j)*U(1,col) + K(8,j)*U(2,col)
     2                           + K(9,j)*U(3,col)
            END DO
         END DO
      CASE(4)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(1,i) = KU(1,i) + K(1 ,j)*U(1,col) + K(2 ,j)*U(2,col)
     2                           + K(3 ,j)*U(3,col) + K(4 ,j)*U(4,col)
               KU(2,i) = KU(2,i) + K(5 ,j)*U(1,col) + K(6 ,j)*U(2,col)
     2                           + K(7 ,j)*U(3,col) + K(8 ,j)*U(4,col)
               KU(3,i) = KU(3,i) + K(9 ,j)*U(1,col) + K(10,j)*U(2,col)
     2                           + K(11,j)*U(3,col) + K(12,j)*U(4,col)
               KU(4,i) = KU(4,i) + K(13,j)*U(1,col) + K(14,j)*U(2,col)
     2                           + K(15,j)*U(3,col) + K(16,j)*U(4,col)
            END DO
         END DO
      CASE DEFAULT
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               DO l=1, dof
                  e = l*dof
                  s = e - dof + 1
                  KU(l,i) = KU(l,i) + SUM(K(s:e,j)*U(:,col))
               END DO
            END DO
         END DO
      END SELECT

      CALL COMMUV(dof, nNo, commu, cS, KU)

      RETURN
      END SUBROUTINE SPARMULVV

!====================================================================

      SUBROUTINE SPARMULVS(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2   K, U, KU)

      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: dof, nNo, nnz
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: K(dof,nnz), U(dof,nNo)
      REAL*8, INTENT(OUT) :: KU(nNo)
      
      INTEGER i, j, col

      KU = 0D0
      SELECT CASE (dof)
      CASE (1)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               KU(i) = KU(i) + K(1,j)*U(1,colPtr(j))
            END DO
         END DO
      CASE(2)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(i) = KU(i) + K(1,j)*U(1,col) + K(2,j)*U(2,col)
            END DO
         END DO
      CASE(3)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(i) = KU(i) + K(1,j)*U(1,col) + K(2,j)*U(2,col)
     2                       + K(3,j)*U(3,col)
            END DO
         END DO
      CASE(4)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(i) = KU(i) + K(1,j)*U(1,col) + K(2,j)*U(2,col)
     2                       + K(3,j)*U(3,col) + K(4,j)*U(4,col)
            END DO
         END DO
      CASE DEFAULT
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               KU(i) = KU(i) + SUM(K(:,j)*U(:,colPtr(j)))
            END DO
         END DO
      END SELECT

      CALL COMMUS(nNo, commu, cS, KU)

      RETURN
      END SUBROUTINE SPARMULVS

!====================================================================

      SUBROUTINE SPARMULSV(dof, nNo, nnz, commu, cS, rowPtr, colPtr, 
     2   K, U, KU)

      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: dof, nNo, nnz
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: K(dof,nnz), U(nNo)
      REAL*8, INTENT(OUT) :: KU(dof,nNo)

      INTEGER i, j, col

      KU = 0D0
      SELECT CASE (dof)
      CASE (1)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               KU(1,i) = KU(1,i) + K(1,j)*U(colPtr(j))
            END DO
         END DO
      CASE(2)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(1,i) = KU(1,i) + K(1,j)*U(col)
               KU(2,i) = KU(2,i) + K(2,j)*U(col)
            END DO
         END DO
      CASE(3)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(1,i) = KU(1,i) + K(1,j)*U(col)
               KU(2,i) = KU(2,i) + K(2,j)*U(col)
               KU(3,i) = KU(3,i) + K(3,j)*U(col)
            END DO
         END DO
      CASE(4)
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               col = colPtr(j)
               KU(1,i) = KU(1,i) + K(1,j)*U(col)
               KU(2,i) = KU(2,i) + K(2,j)*U(col)
               KU(3,i) = KU(3,i) + K(3,j)*U(col)
               KU(4,i) = KU(4,i) + K(4,j)*U(col)
            END DO
         END DO
      CASE DEFAULT
         DO i=1, nNo
            DO j=rowPtr(1,i), rowPtr(2,i)
               KU(:,i) = KU(:,i) + K(:,j)*U(colPtr(j))
            END DO
         END DO
      END SELECT

      CALL COMMUV(dof, nNo, commu, cS, KU)

      RETURN
      END SUBROUTINE SPARMULSV

!====================================================================

      SUBROUTINE SPARMULSS(nNo, nnz, commu, cS, rowPtr, colPtr, 
     2   K, U, KU)

      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: nNo, nnz
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      INTEGER, INTENT(IN) :: rowPtr(2,nNo), colPtr(nnz)
      REAL*8, INTENT(IN) :: K(nnz), U(nNo)
      REAL*8, INTENT(OUT) :: KU(nNo)

      INTEGER i, j

      KU = 0D0
      DO i=1, nNo
         DO j=rowPtr(1,i), rowPtr(2,i)
            KU(i) = KU(i) + K(j)*U(colPtr(j))
         END DO
      END DO

      CALL COMMUS(nNo, commu, cS, KU)

      RETURN
      END SUBROUTINE SPARMULSS

