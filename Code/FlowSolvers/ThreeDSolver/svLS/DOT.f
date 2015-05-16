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
      
      FUNCTION DOTV(dof, nNo, commu, U, V)
 
      INCLUDE "svLS_STD.h"
      
      INTEGER, INTENT(IN) :: dof, nNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      REAL*8, INTENT(IN) :: V(dof,nNo), U(dof,nNo)
      
      INTEGER i, ierr
      REAL*8 tmp, DOTV

      DOTV = 0D0
      SELECT CASE(dof)
      CASE(1)
         DO i=1, nNo
            DOTV = DOTV + U(1,i)*V(1,i)
         END DO
      CASE(2)
         DO i=1, nNo
            DOTV = DOTV + U(1,i)*V(1,i) + U(2,i)*V(2,i)
         END DO
      CASE(3)
         DO i=1, nNo
            DOTV = DOTV + U(1,i)*V(1,i) + U(2,i)*V(2,i) +U(3,i)*V(3,i)
         END DO
      CASE(4)
         DO i=1, nNo
            DOTV = DOTV + U(1,i)*V(1,i) + U(2,i)*V(2,i) 
     2                  + U(3,i)*V(3,i) + U(4,i)*V(4,i)
         END DO
      CASE DEFAULT 
         DO i=1, nNo
            DOTV = DOTV + SUM(U(:,i)*V(:,i))
         END DO
      END SELECT

      IF (commu%nTasks .EQ. 1) RETURN

      CALL MPI_ALLREDUCE(DOTV, tmp, 1, mpreal, MPI_SUM, commu%comm, 
     2   ierr)

      DOTV = tmp

      RETURN
      END FUNCTION DOTV

!====================================================================
      
      FUNCTION DOTS(nNo, commu, U, V)
 
      INCLUDE "svLS_STD.h"
      
      INTEGER, INTENT(IN) :: nNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      REAL*8, INTENT(IN) :: V(nNo), U(nNo)
 
      INTEGER i, ierr
      REAL*8 tmp, DOTS

      DOTS = 0D0
      DO i=1, nNo
         DOTS = DOTS + U(i)*V(i)
      END DO

      IF (commu%nTasks .EQ. 1) RETURN

      CALL MPI_ALLREDUCE(DOTS, tmp, 1, mpreal, MPI_SUM, commu%comm, 
     2   ierr)

      DOTS = tmp

      RETURN
      END FUNCTION DOTS
