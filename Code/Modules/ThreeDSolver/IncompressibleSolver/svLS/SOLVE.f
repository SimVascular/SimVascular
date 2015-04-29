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
      
      SUBROUTINE svLS_SOLVE (lhs, ls, dof, Ri, Val, incL, res)

      INCLUDE "svLS_STD.h"

      TYPE(svLS_lhsType), INTENT(INOUT) :: lhs
      TYPE(svLS_lsType), INTENT(INOUT) :: ls
      INTEGER, INTENT(IN) :: dof
      REAL*8, INTENT(INOUT) :: Ri(dof,lhs%nNo)
      REAL*8, INTENT(INOUT) :: Val(dof*dof,lhs%nnz)
      INTEGER, INTENT(IN), OPTIONAL :: incL(lhs%nFaces)
      REAL*8, INTENT(IN), OPTIONAL :: res(lhs%nFaces)

      LOGICAL flag
      INTEGER faIn, a, nNo, nnz, nFaces
      REAL*8, ALLOCATABLE :: R(:,:), W(:,:)
      
      nNo    = lhs%nNo
      nnz    = lhs%nnz
      nFaces = lhs%nFaces

      IF (lhs%nFaces .NE. 0) THEN
         lhs%face%incFlag = .TRUE.
         IF (PRESENT(incL)) THEN
            DO faIn=1, lhs%nFaces
               IF (incL(faIn) .EQ. 0) lhs%face(faIn)%incFlag = .FALSE.
            END DO
         END IF

         flag = ANY(lhs%face%bGrp.EQ.BC_TYPE_Neu)
         IF (.NOT.PRESENT(res) .AND. flag) THEN
            PRINT *, "res is required when there is a Neu surface"
         END IF
         DO faIn=1, lhs%nFaces
            lhs%face(faIn)%coupledFlag = .FALSE.
            IF (.NOT.lhs%face(faIn)%incFlag) CYCLE
            flag = lhs%face(faIn)%bGrp .EQ. BC_TYPE_Neu
            IF (flag .AND. res(faIn).NE.0D0) THEN
               lhs%face(faIn)%res = res(faIn)
               lhs%face(faIn)%coupledFlag = .TRUE.
            END IF
         END DO
      END IF
      
      ALLOCATE(R(dof,nNo), W(dof,nNo))
      DO a=1, nNo
         R(:,lhs%map(a)) = Ri(:,a)
      END DO

      CALL COMMUV(dof, nNo, lhs%commu, lhs%cS, R)
      CALL PRECOND(nFaces, dof, nNo, nnz, lhs%commu, lhs%cS, 
     2   lhs%face, lhs%rowPtr, lhs%colPtr, lhs%diagPtr, Val, R, W)
 
      SELECT CASE (ls%LS_type)
         CASE (LS_TYPE_NS)
            CALL NSSOLVER(nFaces, lhs%gnNo, dof, nNo, nnz, lhs%mynNo, 
     2         lhs%commu, lhs%cS, lhs%face, ls, lhs%rowPtr, lhs%colPtr,
     3         Val, R)
         CASE (LS_TYPE_GMRES)
            CALL GMRESV(nFaces, dof, nNo, nnz, lhs%mynNo, lhs%commu, 
     2         lhs%cS, lhs%face, ls%RI, lhs%rowPtr, lhs%colPtr, Val, R)
         CASE (LS_TYPE_CG)
            IF (dof .EQ. 1) THEN
               CALL CGRADS(nNo, nnz, lhs%mynNo, lhs%commu, lhs%cS, 
     2            ls%RI, lhs%rowPtr, lhs%colPtr, Val, R)
            ELSE
               CALL CGRADV(dof, nNo, nnz, lhs%mynNo, lhs%commu, lhs%cS, 
     2            ls%RI, lhs%rowPtr, lhs%colPtr, Val, R)
            END IF
         CASE DEFAULT
            PRINT *, 'LS_type not defined'
            STOP
      END SELECT
      R = R*W

      DO a=1, nNo
         Ri(:,a) = R(:,lhs%map(a))
      END DO

      DEALLOCATE(R, W)

      RETURN
      END SUBROUTINE svLS_SOLVE

