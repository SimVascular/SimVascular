c Copyright (c) 2014-2015 The Regents of the University of California.
c All Rights Reserved.
c
c Portions of the code Copyright (c) 2009-2011 Open Source Medical
c Software Corporation, University of California, San Diego.
c
c Portions of the code Copyright (c) 2000-2007, Stanford University, 
c     Rensselaer Polytechnic Institute, Kenneth E. Jansen, 
c     Charles A. Taylor.
c  
c  See SimVascular Acknowledgements file for additional 
c  contributors to the source code.
c
c  Redistribution and use in source and binary forms, with or without 
c  modification, are permitted provided that the following conditions 
c  are met:
c
c  Redistributions of source code must retain the above copyright notice,
c  this list of conditions and the following disclaimer. 
c  Redistributions in binary form must reproduce the above copyright 
c  notice, this list of conditions and the following disclaimer in the 
c  documentation and/or other materials provided with the distribution. 
c  Neither the name of the Stanford University or Rensselaer Polytechnic
c  Institute nor the names of its contributors may be used to endorse or
c  promote products derived from this software without specific prior 
c  written permission.
c
c  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
c  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
c  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
c  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
c  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
c  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
c  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
c  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
c  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
c  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
c  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
c  DAMAGE.
c
c
        subroutine bc3Diag (iBC,  BC,  res)
c
c----------------------------------------------------------------------
c
c This routine satisfies the BC of the diagonal vector for 3D elements.
c
c input:
c  iBC   (nshg)        : Boundary Condition Code
c  BC    (nshg,ndofBC) : the boundary condition constraint parameters
c  res   (nshg,nflow)   : residual before BC is applied
c
c output:
c  res   (nshg,nflow)   : residual after satisfaction of BC
c
c----------------------------------------------------------------------
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
c
C
C     Argument variables
C
      INTEGER             ibc
C
      REAL*8                bc,          res

        dimension iBC(nshg),                BC(nshg,ndofBC),   
     &            res(nshg,4)
c 
c.... pressure 
c
        where (btest(iBC,2))
           res(:,4) = zero
        endwhere
c
c.... velocities
c
c ibits(n1,n2,n3) extracts bits n2+1 through n2+n3 (extending to the left
c as is traditional in binary) of the integer n1
c and returns the base 10 integer. In examples below x y z a b can 
c be 1 or zero without any effect.
c
c.... x1-velocity
c
c if iBC=4   bits of ibc =00000100 => ibits(4,3,3)=0
c if iBC=40  bits of ibc =00101000 => ibits(4,3,3)=5
c if iBC=40  bits of ibc =00101000 => ibits(4,3,2)=1
c
        where (ibits(iBC,3,3) .eq. 1)   ! bits of iBC= xy001zab 
c
c     notice that the extracted 3 bits form the number 1.  below
c     you will see the combinations which make up 2-7, all of the
c     possible velocity combinations
c
          res(:,2) = res(:,2) - BC(:,4) * res(:,1)
          res(:,3) = res(:,3) - BC(:,5) * res(:,1)
          res(:,1) = zero
        endwhere
c
c.... x2-velocity
c
        where (ibits(iBC,3,3) .eq. 2)   ! bits of iBC= xy010zab 
          res(:,1) = res(:,1) - BC(:,4) * res(:,2)
          res(:,3) = res(:,3) - BC(:,5) * res(:,2)
          res(:,2) = zero
        endwhere
c
c.... x1-velocity and x2-velocity
c
        where (ibits(iBC,3,3) .eq. 3)  ! bits of iBC= xy011zab 
          res(:,3) = res(:,3) - BC(:,4) * res(:,1) - BC(:,6) * res(:,2)
          res(:,1) = zero
          res(:,2) = zero
        endwhere
c
c.... x3-velocity
c
        where (ibits(iBC,3,3) .eq. 4)  ! bits of iBC= xy100zab 
          res(:,1) = res(:,1) - BC(:,4) * res(:,3)
          res(:,2) = res(:,2) - BC(:,5) * res(:,3)
          res(:,3) = zero
        endwhere
c
c.... x1-velocity and x3-velocity
c
        where (ibits(iBC,3,3) .eq. 5)  ! bits of iBC= xy101zab 
          res(:,2) = res(:,2) - BC(:,4) * res(:,1) - BC(:,6) * res(:,3)
          res(:,1) = zero
          res(:,3) = zero
        endwhere
c
c.... x2-velocity and x3-velocity
c
        where (ibits(iBC,3,3) .eq. 6)  ! bits of iBC= xy110zab 
          res(:,1) = res(:,1) - BC(:,4) * res(:,2) - BC(:,6) * res(:,3)
          res(:,2) = zero
          res(:,3) = zero
        endwhere
c
c.... x1-velocity, x2-velocity and x3-velocity
c
        where (ibits(iBC,3,3) .eq. 7)  ! bits of iBC= xy111zab 
          res(:,1) = zero
          res(:,2) = zero
          res(:,3) = zero
        endwhere
c
c.... scaled plane extraction boundary condition
c
           where (btest(iBC,11))
              res(:,1) = zero
              res(:,2) = zero
              res(:,3) = zero
           endwhere

c.... return
c
        return
        end


        subroutine bc3SclrDiag (iBC,  res)
c
c----------------------------------------------------------------------
c
c This routine satisfies the BC of the diagonal vector for 3D elements.
c
c input:
c  iBC   (nshg)        : Boundary Condition Code
c  BC    (nshg,ndofBC) : the boundary condition constraint parameters
c  res   (nshg)   : residual before BC is applied
c
c output:
c  res   (nshg)   : residual after satisfaction of BC
c  
c
c----------------------------------------------------------------------
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/sclrs.h"
C
C     Argument variables
C
      INTEGER             ibc
C
      REAL*8                res
C
C     Local variables
C
      INTEGER             ib
C
      REAL*8                bc

        dimension iBC(nshg),                BC(nshg,ndofBC),   
     &            res(nshg)

       if(isclr.eq.0) then
c 
c.... temperature
c
          where (btest(iBC,1))
             res = zero
          endwhere
       else
c 
c.... scalar
c
          ib=5+isclr
          where (btest(iBC,ib))
             res = zero
          endwhere
       endif
c
c.... return
c
        return
        end
