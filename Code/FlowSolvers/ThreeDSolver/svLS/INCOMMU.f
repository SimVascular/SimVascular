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
      
      SUBROUTINE COMMUV(dof, nNo, commu, cS, R)
 
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: dof, nNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      REAL*8, INTENT(INOUT) :: R(dof,nNo)

      INTEGER i, j, k, s, e, ierr, nTasks, tF, stat(MPI_STATUS_SIZE) 
      INTEGER comm
      REAL*8, ALLOCATABLE :: rTmp(:,:)

      IF (commu%nTasks .EQ. 1) RETURN
      
      nTasks = commu%nTasks
      tF     = commu%tF
      comm   = commu%comm
      
      IF (tF .NE. 1) THEN
         i = tF - 1
         i = cS(i)%ptr + cS(i)%n - 1
         ALLOCATE(rTmp(dof,i))
      END IF

      DO i=1, nTasks
         IF (cS(i)%tag .NE. 0) THEN
            s = cS(i)%ptr
            e = s + cS(i)%n - 1
            IF (i .LT. tF) THEN
               CALL MPI_IRECV(rTmp(:,s:e), cS(i)%n*dof, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            ELSE
               CALL MPI_ISEND(R(:,s:e), cS(i)%n*dof, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            END IF
         END IF
      END DO
         
      k = 1
      DO i=1, tF - 1
         IF (cS(i)%tag .NE. 0) THEN
            CALL MPI_WAIT(cS(i)%req, stat, ierr)
            DO j=1, cS(i)%nBl
               s = cS(i)%blPtr(j)
               e = s + cS(i)%blN(j) - 1
               R(:,s:e) = R(:,s:e) + rTmp(:,k:k+e-s)
               k = k + cS(i)%blN(j)
            END DO
         END IF
      END DO
 
      k = 1
      DO i=1, tF - 1
         DO j=1, cS(i)%nBl
            s = cS(i)%blPtr(j)
            e = s + cS(i)%blN(j) - 1
            rTmp(:,k:k+e-s) = R(:,s:e)
            k = k + cS(i)%blN(j)
         END DO
      END DO

      DO i=1, nTasks
         IF (cS(i)%tag .NE. 0) THEN
            s = cS(i)%ptr
            e = s + cS(i)%n - 1
            IF (i .GT. tF) THEN
               CALL MPI_WAIT(cS(i)%req, stat, ierr)
               CALL MPI_IRECV(R(:,s:e), cS(i)%n*dof, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            ELSE
               CALL MPI_ISEND(rTmp(:,s:e), cS(i)%n*dof, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            END IF
         END IF
      END DO
      
      DO i=1, nTasks
         IF (cS(i)%tag .NE. 0) THEN
            CALL MPI_WAIT(cS(i)%req, stat, ierr)
         END IF
      END DO

      RETURN
      END SUBROUTINE COMMUV

!====================================================================
      
      SUBROUTINE COMMUS(nNo, commu, cS, R)
 
      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: nNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_cSType), INTENT(IN) :: cS(commu%nTasks)
      REAL*8, INTENT(INOUT) :: R(nNo)
      
      INTEGER i, j, k, s, e, ierr, nTasks, tF, stat(MPI_STATUS_SIZE) 
      INTEGER comm
      REAL*8, ALLOCATABLE :: rTmp(:)

      IF (commu%nTasks .EQ. 1) RETURN
      
      nTasks = commu%nTasks
      tF     = commu%tF
      comm   = commu%comm
     
      IF (tF .NE. 1) THEN
         i = tF - 1
         i = cS(i)%ptr + cS(i)%n - 1
         ALLOCATE(rTmp(i))
      END IF

      DO i=1, nTasks
         IF (cS(i)%tag .NE. 0) THEN
            s = cS(i)%ptr
            e = s + cS(i)%n - 1
            IF (i .LT. tF) THEN
               CALL MPI_IRECV(rTmp(s:e), cS(i)%n, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            ELSE
               CALL MPI_ISEND(R(s:e), cS(i)%n, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            END IF
         END IF
      END DO
 
      k = 1
      DO i=1, tF - 1
         IF (cS(i)%tag .NE. 0) THEN
            CALL MPI_WAIT(cS(i)%req, stat, ierr)
            DO j=1, cS(i)%nBl
               s = cS(i)%blPtr(j)
               e = s + cS(i)%blN(j) - 1
               R(s:e) = R(s:e) + rTmp(k:k+e-s)
               k = k + cS(i)%blN(j)
            END DO
         END IF
      END DO
       
      k = 1
      DO i=1, tF - 1
         DO j=1, cS(i)%nBl
            s = cS(i)%blPtr(j)
            e = s + cS(i)%blN(j) - 1
            rTmp(k:k+e-s) = R(s:e)
            k = k + cS(i)%blN(j)
         END DO
      END DO
 
      DO i=1, nTasks
         IF (cS(i)%tag .NE. 0) THEN
            s = cS(i)%ptr
            e = s + cS(i)%n - 1
            IF (i .GT. tF) THEN
               CALL MPI_WAIT(cS(i)%req, stat, ierr)
               CALL MPI_IRECV(R(s:e), cS(i)%n, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            ELSE
               CALL MPI_ISEND(rTmp(s:e), cS(i)%n, mpreal, i-1, 
     2            cS(i)%tag, comm, cS(i)%req, ierr)
            END IF
         END IF
      END DO
      
      DO i=1, nTasks
         IF (cS(i)%tag .NE. 0) THEN
            CALL MPI_WAIT(cS(i)%req, stat, ierr)
         END IF
      END DO

      RETURN
      END SUBROUTINE COMMUS
      
