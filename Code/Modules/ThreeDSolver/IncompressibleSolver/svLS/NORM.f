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
      
      FUNCTION NORMV(dof, nNo, commu, U)
 
      INCLUDE "svLS_STD.h" 
       
      INTEGER, INTENT(IN) :: dof, nNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      REAL*8, INTENT(IN) :: U(dof,nNo)
      
      INTEGER i, ierr
      REAL*8 tmp, NORMV
            
      NORMV = 0D0
      SELECT CASE(dof)
      CASE(1)
         DO i=1, nNo
            NORMV = NORMV + U(1,i)*U(1,i)
         END DO
      CASE(2)
         DO i=1, nNo
            NORMV = NORMV + U(1,i)*U(1,i) + U(2,i)*U(2,i)
         END DO
      CASE(3)
         DO i=1, nNo
            NORMV = NORMV+ U(1,i)*U(1,i) + U(2,i)*U(2,i) + U(3,i)*U(3,i)
         END DO
      CASE(4)
         DO i=1, nNo
            NORMV = NORMV + U(1,i)*U(1,i) + U(2,i)*U(2,i) 
     2                    + U(3,i)*U(3,i) + U(4,i)*U(4,i)
         END DO
      CASE DEFAULT 
         DO i=1, nNo
            NORMV = NORMV + SUM(U(:,i)*U(:,i))
         END DO
      END SELECT

      IF (commu%nTasks .NE. 1) THEN
         CALL MPI_ALLREDUCE(NORMV, tmp, 1, mpreal, MPI_SUM, 
     2        commu%comm, ierr)
         NORMV = tmp
      END IF
      
      NORMV = SQRT(NORMV)

      RETURN
      END FUNCTION NORMV

!====================================================================
      
      FUNCTION NORMS(nNo, commu, U)
 
      INCLUDE "svLS_STD.h"
      
      INTEGER, INTENT(IN) :: nNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      REAL*8, INTENT(IN) :: U(nNo)
      
      INTEGER i, ierr
      REAL*8 tmp, NORMS
      
      NORMS = 0D0
      DO i=1, nNo
         NORMS = NORMS + U(i)*U(i)
      END DO

      IF (commu%nTasks .NE. 1) THEN
         CALL MPI_ALLREDUCE(NORMS, tmp, 1, mpreal, MPI_SUM, 
     2        commu%comm, ierr)
         NORMS = tmp
      END IF
      
      NORMS = SQRT(NORMS)

      RETURN
      END FUNCTION NORMS


