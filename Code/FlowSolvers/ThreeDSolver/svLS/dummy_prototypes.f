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
            INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: commi
      END SUBROUTINE svLS_COMMU_CREATE
 
      SUBROUTINE svLS_COMMU_FREE(commu)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_commuType), INTENT(INOUT) :: commu
      END SUBROUTINE svLS_COMMU_FREE
         
      SUBROUTINE svLS_LHS_CREATE(lhs, commu, gnNo, nNo, nnz, 
     2      gNodes, rowPtr, colPtr, nFaces)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lhsType), INTENT(INOUT) :: lhs
            TYPE(svLS_commuType), INTENT(IN) :: commu
            INTEGER, INTENT(IN) :: gnNo, nNo, nnz
            INTEGER, INTENT(IN) :: gNodes(nNo), rowPtr(nNo+1), 
     2         colPtr(nnz)
            INTEGER, INTENT(IN) :: nFaces
      END SUBROUTINE svLS_LHS_CREATE

      SUBROUTINE svLS_LHS_FREE(lhs)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lhsType), INTENT(INOUT) :: lhs
      END SUBROUTINE svLS_LHS_FREE

       SUBROUTINE svLS_LS_CREATE(ls, LS_type, relTol, absTol, maxItr,
     2      dimKry, relTolIn, absTolIn, maxItrIn)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lsType), INTENT(INOUT) :: ls
            INTEGER, INTENT(IN) :: LS_type
            REAL*8, INTENT(IN), OPTIONAL :: relTol, absTol, relTolIn(2),
     2         absTolIn(2)
            INTEGER, INTENT(IN), OPTIONAL :: maxItr, dimKry, maxItrIn(2)
       END SUBROUTINE svLS_LS_CREATE

       SUBROUTINE svLS_LS_FREE(ls)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lsType), INTENT(INOUT) :: ls
       END SUBROUTINE svLS_LS_FREE
         
       SUBROUTINE svLS_BC_CREATE(lhs, faIn, nNo, dof, BC_type, 
     2         gNodes, Val)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lhsType), INTENT(INOUT) :: lhs
            INTEGER, INTENT(IN) :: faIn, nNo, dof
            INTEGER, INTENT(IN) :: BC_type
            INTEGER, INTENT(IN) :: gNodes(nNo)
            REAL*8, INTENT(IN), OPTIONAL :: Val(dof,nNo)
       END SUBROUTINE svLS_BC_CREATE

       SUBROUTINE svLS_BC_FREE(lhs, faIn)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lhsType), INTENT(INOUT) :: lhs
            INTEGER, INTENT(IN) :: faIn
       END SUBROUTINE svLS_BC_FREE
         
       SUBROUTINE svLS_SOLVE (lhs, ls, dof, Ri, Val, res)
            INCLUDE "svLS_STD.h"
            TYPE(svLS_lhsType), INTENT(INOUT) :: lhs
            TYPE(svLS_lsType), INTENT(INOUT) :: ls
            INTEGER, INTENT(IN) :: dof
            REAL*8, INTENT(INOUT) :: Ri(dof,lhs%nNo)
            REAL*8, INTENT(IN) :: Val(dof*dof,lhs%nnz)
            REAL*8, INTENT(IN), OPTIONAL :: res(lhs%nFaces)
       END SUBROUTINE svLS_SOLVE

 



