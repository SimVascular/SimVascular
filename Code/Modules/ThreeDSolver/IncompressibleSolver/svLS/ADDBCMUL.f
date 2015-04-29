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
      
      SUBROUTINE ADDBCMUL(op_Type, nFaces, dof, nNo, mynNo, commu, face,
     2   X, Y)

      INCLUDE "svLS_STD.h"

      INTEGER, INTENT(IN) :: op_type, nFaces, dof, nNo, mynNo
      TYPE(svLS_commuType), INTENT(IN) :: commu
      TYPE(svLS_faceType), INTENT(IN) :: face(nFaces)
      REAL*8, INTENT(IN) :: X(dof, nNo)
      REAL*8, INTENT(INOUT) :: Y(dof, nNo)

      INTEGER faIn, i, a, Ac, nsd
      REAL*8 S, DOTV
      REAL*8, ALLOCATABLE :: v(:,:), coef(:)

      ALLOCATE(coef(nFaces))

      IF (op_Type .EQ. BCOP_TYPE_ADD) THEN
         coef = face%res
      ELSE IF(op_Type .EQ. BCOP_TYPE_PRE) THEN
         coef = -face%res/(1D0 + face%res*face%nS)
      ELSE
         PRINT *, "op_Type is not defined"
         STOP
      END IF

      DO faIn=1, nFaces
         nsd = MIN(face(faIn)%dof,dof)
         IF (face(faIn)%coupledFlag) THEN
            IF (face(faIn)%sharedFlag) THEN
               IF (.NOT.ALLOCATED(v)) ALLOCATE(v(dof,nNo))
               v = 0D0
               DO a=1, face(faIn)%nNo
                  Ac = face(faIn)%glob(a)
                  DO i=1, nsd
                     v(i,Ac) = face(faIn)%valM(i,a)
                  END DO
               END DO
               S = coef(faIn)*DOTV(dof, mynNo, commu, v, X)
               DO a=1, face(faIn)%nNo
                  Ac = face(faIn)%glob(a)
                  DO i=1, nsd
                     Y(i,Ac) = Y(i,Ac) + v(i,Ac)*S
                  END DO
               END DO
            ELSE
               S = 0D0
               DO a=1, face(faIn)%nNo
                  Ac = face(faIn)%glob(a)
                  DO i=1, nsd
                     S = S + face(faIn)%valM(i,a)*X(i,Ac)
                  END DO
               END DO
               S = coef(faIn)*S
               DO a=1, face(faIn)%nNo
                  Ac = face(faIn)%glob(a)
                  DO i=1, nsd
                     Y(i,Ac) = Y(i,Ac) + face(faIn)%valM(i,a)*S
                  END DO
               END DO
            END IF
         END IF
      END DO

      RETURN
      END SUBROUTINE ADDBCMUL
