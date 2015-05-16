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
      
      SUBROUTINE GE (N, A, B)
      
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N
      REAL*8, INTENT(IN) :: A(N,N)
      REAL*8, INTENT(INOUT) :: B(N)

      INTEGER m, ipv, i, j
      REAL*8 pivot, saveEl
      REAL*8, ALLOCATABLE :: C(:,:)

      IF (N .EQ. 2) THEN
         pivot  = A(1,1)*A(2,2) - A(2,1)*A(1,2)
         saveEl = (B(1)*A(2,2) - B(2)*A(1,2))/pivot
         B(2)   = (B(2)*A(1,1) - B(1)*A(2,1))/pivot
         B(1)   = saveEl
         RETURN
      END IF
      ALLOCATE(C(N,N+1))

      C(:N,:N) = A
      C(:,N+1) = B

      DO m=1,N-1
         ipv = m
         pivot = ABS(C(m,m))
         DO i=m+1,N
            IF (ABS(C(i,m)) .GT. pivot) THEN
               ipv = i
               pivot = ABS(C(i,m))
            END IF
         END DO
         IF (pivot .LT. 2D0*EPSILON(pivot)) THEN
            PRINT *, 'Singular matrix'
            STOP
         END IF
         IF (ipv .NE. m) THEN
            DO j=m, N+1
               saveEl = C(m,j)
               C(m,j) = C(ipv,j)
               C(ipv,j) = saveEl
            END DO
            DO j=1, m-1
            END DO
         END IF

         DO i=m+1,N
            saveEl = C(i,m)/C(m,m)
            C(i,m) = 0D0
            DO j=m+1,N+1
               C(i,j) = C(i,j) - saveEl*C(m,j)
            END DO
         END DO
      END DO

      DO j=N,1,-1
         DO i=j+1,N
            C(j,N+1) = C(j,N+1) - C(j,i)*C(i,N+1)
         END DO
         C(j,N+1) = C(j,N+1)/C(j,j)
      END DO
      
      B = C(:,N+1)

      DEALLOCATE(C)

      RETURN
      END SUBROUTINE GE
