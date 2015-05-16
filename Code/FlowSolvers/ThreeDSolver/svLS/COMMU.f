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
      
      SUBROUTINE svLS_COMMU_CREATE(commu, commi)
      
      INCLUDE "svLS_STD.h"
   
      TYPE(svLS_commuType), INTENT(INOUT) :: commu
      INTEGER, INTENT(IN) :: commi

      INTEGER ierr
      INTEGER comm

      IF (commu%foC) THEN
         PRINT *, "COMMU is not free"
         PRINT *, "You may use svLS_COMMU_FREE to free this structure"
      END IF

      commu%foC  = .TRUE.
      commu%comm = commi
      
      comm       = commi

      CALL MPI_COMM_RANK(comm, commu%task, ierr)
      CALL MPI_COMM_SIZE(comm, commu%nTasks, ierr)

      CALL MPI_ALLREDUCE(commu%task, commu%master, 1, mpint, MPI_MIN, 
     2   comm, ierr)
      
      IF (commu%master .NE. 0) THEN
         PRINT *, "master is not zero"
         CALL MPI_FINALIZE(comm, ierr)
         STOP
      END IF

      commu%masF  = .FALSE.
      commu%tF    = commu%task + 1
      IF (commu%task .EQ. commu%master) THEN
         commu%masF = .TRUE.
      END IF

      RETURN
      END SUBROUTINE svLS_COMMU_CREATE

!====================================================================

      SUBROUTINE svLS_COMMU_FREE(commu)
      
      INCLUDE "svLS_STD.h"
   
      TYPE(svLS_commuType), INTENT(INOUT) :: commu

      IF (.NOT.commu%foC) THEN
         PRINT *, 'Cannot free commu'
         PRINT *, 'It is not created yet'
         STOP
      END IF
      commu%foC  = .FALSE.

      RETURN
      END SUBROUTINE svLS_COMMU_FREE
