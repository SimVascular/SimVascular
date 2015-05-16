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
c---------------------------------------------------------------------
c
c ftools.f : Bundle of Fortran routines
c
c Each routine is to be called by drvftools.f
c
c Various operations run based on les**.c
c
c---------------------------------------------------------------------
c
c--------------
c flesPrepDiag
c--------------
c
       subroutine flesPrepDiag ( ien, xKebe, xGoc, flowDiag )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
C
C     Argument variables
C
      REAL                flowdiag,    xgoc,        xkebe
C
C     Local variables
C
      INTEGER             i,           i0,          nn2
C
      REAL                diagl
C
        dimension xKebe(npro,3*nshl,3*nshl), 
     &            xGoC(npro,4*nshl,nshl)
        dimension Diagl(npro,nshl,nflow),   flowDiag(nshg, 4)
        integer   ien(npro,nshl)
c
c
c.... monentum contribution to diagonal
c
        do i = 1, nshl
          i0 = (nsd) * (i - 1)
          Diagl(:,i,1) = xKebe(1:npro,i0+1,i0+1)
          Diagl(:,i,2) = xKebe(1:npro,i0+2,i0+2)
          Diagl(:,i,3) = xKebe(1:npro,i0+3,i0+3)
        enddo
c
c.... continuity contribution to diagonal
c
        nn2 = nshl * nsd
        do i = 1, nshl
          Diagl(:,i,4) = xGoC(1:npro,nn2+i,i)
        enddo

        call local (flowDiag,  Diagl, ien, nflow, 'scatter ')
c
        return
        end
c
c--------------------------------
c fMtxVdimVecMult 
c row and column index exchanged
c--------------------------------
c
        subroutine fMtxVdimVecMult( a, b, c, na, nb, nc, m, n )
c
c.... Data declaration
c
        implicit none
        integer na,     nb,     nc,     m,      n
        real*8  a(n,na),        b(n,nb),        c(n,nc)
c
        integer i,      j
c
c.... Do the work
c
C WIP: change to F90
C
        if ( m .eq. 1 ) then
c
            do i = 1, n
                c(i,1) = a(i,1) * b(i,1)
            enddo
c
        else if ( m .eq. 2 ) then
c
            do i = 1, n
                c(i,1) = a(i,1) * b(i,1)
                c(i,2) = a(i,2) * b(i,2)
            enddo
c
        else if ( m .eq. 3 ) then
c
            do i = 1, n
                c(i,1) = a(i,1) * b(i,1)
                c(i,2) = a(i,2) * b(i,2)
                c(i,3) = a(i,3) * b(i,3)
            enddo
c
        else if ( m .eq. 4 ) then
c
            do i = 1, n
                c(i,1) = a(i,1) * b(i,1)
                c(i,2) = a(i,2) * b(i,2)
                c(i,3) = a(i,3) * b(i,3)
                c(i,4) = a(i,4) * b(i,4)
            enddo
c
        else
c
            do i = 1, n 
                do j = 1, m 
                    c(i,j) = a(i,j) * b(i,j)
                enddo
            enddo
c
        endif
c
      return
      end
c
c---------- 
c flesZero
c----------
c
      subroutine flesZero ( a, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m)
c
        do i = 1, n
          do j = 1, m
            a(i,j) = 0.0e-0
          enddo
        enddo
c
        return 
        end
c
c--------
c flesCp
c--------
c
      subroutine flesCp ( a, b, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m), b(n,m)
c
        do i = 1, n
          do j = 1, m
            b(i,j) = a(i,j)
          enddo
        enddo
c 
        return
        end
c
c-----------
c flesScale
c-----------
c
       subroutine flesScale ( a, s, m, n )
c
        implicit none
        integer  m, n, i, j   
        real*8   a(n,m), s
c
        do i = 1, n
          do j = 1, m
            a(i,j) = a(i,j) * s
          enddo
        enddo
c
        return
        end
c
c-------------
c flesScaleCp
c-------------
c
      subroutine flesScaleCp ( a, b, s, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m), b(n,m), s
c
        do i = 1, n
          do j = 1, m
            b(i,j) = a(i,j) * s
          enddo
        enddo
c
        return 
        end
c
c---------
c flesAdd
c---------
c
      subroutine flesAdd ( a, b, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m), b(n,m)
c
        do i = 1, n
          do j = 1, m
            b(i,j) = b(i,j) + a(i,j)
          enddo
        enddo
c
        return
        end
c
c---------
c flesSub 
c---------
c
      subroutine flesSub ( a, b, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m), b(n,m)
c
        do i = 1, n
          do j = 1, m
            b(i,j) = b(i,j) - a(i,j)
          enddo
        enddo
c
        return
        end
c
c----------
c flesDot1
c----------
c
       real*8 function flesDot1 ( a, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m)
c
      flesDot1 = 0
        do i = 1, n
          do j = 1, m
            flesDot1 = flesDot1 + a(i,j) * a(i,j)
          enddo
        enddo
c
        return 
        end
c
c----------
c flesDot2
c----------
c
      real*8 function flesDot2 ( a, b, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   a(n,m), b(n,m)
c
      flesDot2 = 0
        do i = 1, n
          do j = 1, m
            flesDot2 = flesDot2 + a(i,j) * b(i,j)
          enddo
        enddo
c
        return
        end
c
c-----------
c flesDaxpy
c-----------
c
      subroutine flesDaxpy ( x, y, a, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   x(n,m), y(n,m)
        real*8   a
c
        do i = 1, n
          do j= 1, m
            y(i,j) = y(i,j) + a * x(i,j)
          enddo
        enddo
c
        return
        end
c
c-----------
c flesDxpay
c-----------
c
      subroutine flesDxpay ( x, y, a, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   x(n,m), y(n,m)
        real*8   a
c
        do i = 1, n
          do j = 1, m
            y(i,j) = a * y(i,j) + x(i,j)
          enddo
        enddo
c
        return 
        end
c
c---------
c flesInv
c---------
c
      subroutine flesInv ( x, m, n )
c
        implicit none
        integer  m, n, i, j
        real*8   x(n,m)
c
        do i = 1, n
          do j = 1, m
            if ( x(i,j) .ne. 0 ) x(i,j) = 1. / x(i,j)
          enddo
        enddo
c
        return
        end
c
c--------------------------
c fMtxBlkDot2
c row and column exchanged
c--------------------------
c
        subroutine fMtxBlkDot2( x, y, c, m, n )
c
c.... Data declaration
c
        implicit none
        integer m,      n
        real*8  x(n,m), y(n),   c(m)
c
        real*8  tmp1,   tmp2,   tmp3,   tmp4
        real*8  tmp5,   tmp6,   tmp7,   tmp8
        integer i,      j,      m1
c
c.... Determine the left overs
c
        m1 = mod(m,8) + 1

c
c.... Do the small pieces
c
        goto ( 8000, 1000, 2000, 3000, 4000, 5000, 6000, 7000 ) m1
c
1000    continue
        tmp1 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
        enddo
        c(1) = tmp1
        goto 8000
c
2000    continue
        tmp1 = 0
        tmp2 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
            tmp2 = tmp2 + x(i,2) * y(i)
        enddo
        c(1) = tmp1
        c(2) = tmp2
        goto 8000
c
3000    continue
        tmp1 = 0
        tmp2 = 0
        tmp3 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
            tmp2 = tmp2 + x(i,2) * y(i)
            tmp3 = tmp3 + x(i,3) * y(i)
        enddo
        c(1) = tmp1
        c(2) = tmp2
        c(3) = tmp3
        goto 8000
c
4000    continue
        tmp1 = 0
        tmp2 = 0
        tmp3 = 0
        tmp4 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
            tmp2 = tmp2 + x(i,2) * y(i)
            tmp3 = tmp3 + x(i,3) * y(i)
            tmp4 = tmp4 + x(i,4) * y(i)
        enddo
        c(1) = tmp1
        c(2) = tmp2
        c(3) = tmp3
        c(4) = tmp4
        goto 8000
c
5000    continue
        tmp1 = 0
        tmp2 = 0
        tmp3 = 0
        tmp4 = 0
        tmp5 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
            tmp2 = tmp2 + x(i,2) * y(i)
            tmp3 = tmp3 + x(i,3) * y(i)
            tmp4 = tmp4 + x(i,4) * y(i)
            tmp5 = tmp5 + x(i,5) * y(i)
        enddo
        c(1) = tmp1
        c(2) = tmp2
        c(3) = tmp3
        c(4) = tmp4
        c(5) = tmp5
        goto 8000
c
6000    continue
        tmp1 = 0
        tmp2 = 0
        tmp3 = 0
        tmp4 = 0
        tmp5 = 0
        tmp6 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
            tmp2 = tmp2 + x(i,2) * y(i)
            tmp3 = tmp3 + x(i,3) * y(i)
            tmp4 = tmp4 + x(i,4) * y(i)
            tmp5 = tmp5 + x(i,5) * y(i)
            tmp6 = tmp6 + x(i,6) * y(i)
        enddo
        c(1) = tmp1
        c(2) = tmp2
        c(3) = tmp3
        c(4) = tmp4
        c(5) = tmp5
        c(6) = tmp6
        goto 8000
c
7000    continue
        tmp1 = 0
        tmp2 = 0
        tmp3 = 0
        tmp4 = 0
        tmp5 = 0
        tmp6 = 0
        tmp7 = 0
        do i = 1, n
            tmp1 = tmp1 + x(i,1) * y(i)
            tmp2 = tmp2 + x(i,2) * y(i)
            tmp3 = tmp3 + x(i,3) * y(i)
            tmp4 = tmp4 + x(i,4) * y(i)
            tmp5 = tmp5 + x(i,5) * y(i)
            tmp6 = tmp6 + x(i,6) * y(i)
            tmp7 = tmp7 + x(i,7) * y(i)
        enddo
        c(1) = tmp1
        c(2) = tmp2
        c(3) = tmp3
        c(4) = tmp4
        c(5) = tmp5
        c(6) = tmp6
        c(7) = tmp7
        goto 8000
c
c.... Do the remaining part
c
8000    continue
c
        do j = m1, m, 8
            tmp1 = 0
            tmp2 = 0
            tmp3 = 0
            tmp4 = 0
            tmp5 = 0
            tmp6 = 0
            tmp7 = 0
            tmp8 = 0
            do i = 1, n
                tmp1 = tmp1 + x(i,j+0) * y(i)
                tmp2 = tmp2 + x(i,j+1) * y(i)
                tmp3 = tmp3 + x(i,j+2) * y(i)
                tmp4 = tmp4 + x(i,j+3) * y(i)
                tmp5 = tmp5 + x(i,j+4) * y(i)
                tmp6 = tmp6 + x(i,j+5) * y(i)
                tmp7 = tmp7 + x(i,j+6) * y(i)
                tmp8 = tmp8 + x(i,j+7) * y(i)
            enddo
            c(j+0) = tmp1
            c(j+1) = tmp2
            c(j+2) = tmp3
            c(j+3) = tmp4
            c(j+4) = tmp5
            c(j+5) = tmp6
            c(j+6) = tmp7
            c(j+7) = tmp8
        enddo
c
        return
        end
c
c--------------------------
c fMtxBlkDaxpy
c row and column exchanged
c--------------------------
c
        subroutine fMtxBlkDaxpy( x, y, c, m, n )
c
c.... Data declaration
c
        implicit none
        integer m,      n
        real*8  x(n,m), y(n),   c(m)
c
        real*8  tmp1,   tmp2,   tmp3,   tmp4
        real*8  tmp5,   tmp6,   tmp7,   tmp8
        integer i,      j,      m1
c
c.... Determine the left overs
c
        m1 = mod(m,8) + 1
c
c.... Do the small pieces
c
        goto ( 8000, 1000, 2000, 3000, 4000, 5000, 6000, 7000 ) m1
c
1000    continue
        tmp1 = c(1)
        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1)
        enddo
        goto 8000
c
2000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
        enddo
        goto 8000
c
3000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)

        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3)
        enddo
        goto 8000
c
4000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
        enddo
        goto 8000
c
5000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
     3           + tmp5 * x(i,5)
        enddo
        goto 8000
c
6000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        tmp6 = c(6)
        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
     3           + tmp5 * x(i,5) + tmp6 * x(i,6)
        enddo
        goto 8000
c
7000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        tmp6 = c(6)
        tmp7 = c(7)
        do i = 1, n
            y(i) = y(i)
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
     3           + tmp5 * x(i,5) + tmp6 * x(i,6)
     4           + tmp7 * x(i,7)
        enddo
        goto 8000
c
c.... Do the remaining part
c
8000    continue
c
        do j = m1, m, 8
            tmp1 = c(j+0)
            tmp2 = c(j+1)
            tmp3 = c(j+2)
            tmp4 = c(j+3)
            tmp5 = c(j+4)
            tmp6 = c(j+5)
            tmp7 = c(j+6)
            tmp8 = c(j+7)
            do i = 1, n
                y(i) = y(i)
     1               + tmp1 * x(i,j+0) + tmp2 * x(i,j+1)
     2               + tmp3 * x(i,j+2) + tmp4 * x(i,j+3)
     3               + tmp5 * x(i,j+4) + tmp6 * x(i,j+5)
     4               + tmp7 * x(i,j+6) + tmp8 * x(i,j+7)
            enddo
        enddo
c
        return 
        end
c
c--------------------------
c fMtxBlkDyeax
c row and column exchanged
c--------------------------
c
        subroutine fMtxBlkDyeax( x, y, c, m, n )
c
c.... Data declaration
c
        implicit none
        integer m,      n
        real*8  x(n,m), y(n),   c(m)
c
        real*8  tmp1,   tmp2,   tmp3,   tmp4
        real*8  tmp5,   tmp6,   tmp7,   tmp8
        integer i,      j,      m1
c
c.... Determine the left overs
c
        m1 = mod(m,8) + 1
c
c.... Do the small pieces
c
        goto ( 8000, 1000, 2000, 3000, 4000, 5000, 6000, 7000 ) m1
c
1000    continue
        tmp1 = c(1)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1)
        enddo
        goto 8001
c
2000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
        enddo
        goto 8001
c
3000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3)
        enddo
        goto 8001
c
4000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
        enddo
        goto 8001
c
5000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
     3           + tmp5 * x(i,5)
        enddo
        goto 8001
c
6000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        tmp6 = c(6)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
     3           + tmp5 * x(i,5) + tmp6 * x(i,6)
       enddo
        goto 8001
c
7000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        tmp6 = c(6)
        tmp7 = c(7)
        do i = 1, n
            y(i) =
     1           + tmp1 * x(i,1) + tmp2 * x(i,2)
     2           + tmp3 * x(i,3) + tmp4 * x(i,4)
     3           + tmp5 * x(i,5) + tmp6 * x(i,6)
     4           + tmp7 * x(i,7)
        enddo
        goto 8001
c
8000    continue
        do i = 1, n
            y(i) = 0
        enddo
        goto 8001
c
c.... Do the remaining part
c
8001    continue
c
        do j = m1, m, 8
            tmp1 = c(j+0)
            tmp2 = c(j+1)
            tmp3 = c(j+2)
            tmp4 = c(j+3)
            tmp5 = c(j+4)
            tmp6 = c(j+5)
            tmp7 = c(j+6)
            tmp8 = c(j+7)
            do i = 1, n
                y(i) = y(i)
     1               + tmp1 * x(i,j+0) + tmp2 * x(i,j+1)
     2               + tmp3 * x(i,j+2) + tmp4 * x(i,j+3)
     3               + tmp5 * x(i,j+4) + tmp6 * x(i,j+5)
     4               + tmp7 * x(i,j+6) + tmp8 * x(i,j+7)
            enddo
        enddo
c
        return 
        end
c
c--------------------------
c fMtxBlkDmaxpy
c row and column exchanged 
c--------------------------
c
       subroutine fMtxBlkDmaxpy( x, y, c, m, n )
c
c.... Data declaration
c
        implicit none
        integer m,      n
        real*8  x(n,m), y(n),   c(m)
c
        real*8  tmp1,   tmp2,   tmp3,   tmp4
        real*8  tmp5,   tmp6,   tmp7,   tmp8
        integer i,      j,      m1
c
c.... Determine the left overs
c
        m1 = mod(m,8) + 1
c
c.... Do the small pieces
c
        goto ( 8000, 1000, 2000, 3000, 4000, 5000, 6000, 7000 ) m1
c
1000    continue
        tmp1 = c(1)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1)
        enddo
        goto 8000
c
2000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1) - tmp2 * x(i,2)
        enddo
        goto 8000
c
3000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1) - tmp2 * x(i,2)
     2           - tmp3 * x(i,3)
        enddo
        goto 8000
c
4000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1) - tmp2 * x(i,2)
     2           - tmp3 * x(i,3) - tmp4 * x(i,4)
        enddo
        goto 8000
c
5000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1) - tmp2 * x(i,2)
     2           - tmp3 * x(i,3) - tmp4 * x(i,4)
     3           - tmp5 * x(i,5)
        enddo
        goto 8000
c
6000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        tmp6 = c(6)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1) - tmp2 * x(i,2)
     2           - tmp3 * x(i,3) - tmp4 * x(i,4)
     3           - tmp5 * x(i,5) - tmp6 * x(i,6)
        enddo
        goto 8000

7000    continue
        tmp1 = c(1)
        tmp2 = c(2)
        tmp3 = c(3)
        tmp4 = c(4)
        tmp5 = c(5)
        tmp6 = c(6)
        tmp7 = c(7)
        do i = 1, n
            y(i) = y(i)
     1           - tmp1 * x(i,1) - tmp2 * x(i,2)
     2           - tmp3 * x(i,3) - tmp4 * x(i,4)
     3           - tmp5 * x(i,5) - tmp6 * x(i,6)
     4           - tmp7 * x(i,7)
        enddo
        goto 8000
c
c.... Do the remaining part
c
8000    continue
c
        do j = m1, m, 8
            tmp1 = c(j+0)
            tmp2 = c(j+1)
            tmp3 = c(j+2)
            tmp4 = c(j+3)
            tmp5 = c(j+4)
            tmp6 = c(j+5)
            tmp7 = c(j+6)
            tmp8 = c(j+7)
            do i = 1, n
                y(i) = y(i)
     1               - tmp1 * x(i,j+0) - tmp2 * x(i,j+1)
     2               - tmp3 * x(i,j+2) - tmp4 * x(i,j+3)
     3               - tmp5 * x(i,j+4) - tmp6 * x(i,j+5)
     4               - tmp7 * x(i,j+6) - tmp8 * x(i,j+7)
            enddo
        enddo
c
        return
        end
c
c--------------------------
c fMtxVdimVecCp
c row and column exchanged 
c--------------------------
c
        subroutine fMtxVdimVecCp( a, b, na, nb, m, n )
c
c.... Data declaration
c
        implicit none
        integer na,     nb,     m,      n
        real*8  a(n,na),        b(n,nb)
c
        integer i,      j
c
c.... Do the work
c
        if ( m .eq. 1 ) then

            do i = 1, n
                b(i,1) = a(i,1)
            enddo

        else if ( m .eq. 2 ) then

            do i = 1, n
                b(i,1) = a(i,1)
                b(i,2) = a(i,2)
            enddo

        else if ( m .eq. 3 ) then

            do i = 1, n
                b(i,1) = a(i,1)
                b(i,2) = a(i,2)
                b(i,3) = a(i,3)
            enddo

        else if ( m .eq. 4 ) then

            do i = 1, n
                b(i,1) = a(i,1)
                b(i,2) = a(i,2)
                b(i,3) = a(i,3)
                b(i,4) = a(i,4)
            enddo

        else

            do i = 1, n
                do j = 1, m 
                    b(i,j) = a(i,j)
                enddo
            enddo

        endif
c
        return
        end
c
c--------------------------
c fMtxVdimVecDot2
c row and column exchanged
c--------------------------
c
        subroutine fMtxVdimVecDot2( a, b, c, na, nb, m, n )
c
c.... Data declaration
c
        implicit none
        integer na,     nb,     m,      n
        real*8  a(n,na),        b(n,nb),        c(m)
c
        integer i,      j
c
c.... Do the work
c
        if ( m .eq. 1 ) then

            c(1) = 0
            do i = 1, n
                c(1) = c(1) + a(i,1) * b(i,1)
            enddo

        else if ( m .eq. 2 ) then

            c(1) = 0
            c(2) = 0
            do i = 1, n
                c(1) = c(1) + a(i,1) * b(i,1)
                c(2) = c(2) + a(i,2) * b(i,2)
            enddo

        else if ( m .eq. 3 ) then

            c(1) = 0
            c(2) = 0
            c(3) = 0
            do i = 1, n
                c(1) = c(1) + a(i,1) * b(i,1)
                c(2) = c(2) + a(i,2) * b(i,2)
                c(3) = c(3) + a(i,3) * b(i,3)
            enddo

        else if ( m .eq. 4 ) then

            c(1) = 0
            c(2) = 0
            c(3) = 0
            c(4) = 0
            do i = 1, n
                c(1) = c(1) + a(i,1) * b(i,1)
                c(2) = c(2) + a(i,2) * b(i,2)
                c(3) = c(3) + a(i,3) * b(i,3)
                c(4) = c(4) + a(i,4) * b(i,4)
            enddo

        else

            do j = 1, m 
                c(j) = 0
                do i = 1, n 
                    c(j) = c(j) + a(i,j) * b(i,j)
                enddo
            enddo

        endif
c
        return
        end
c
c--------------------------
c fMtxVdimVecDaxpy
c row and column exchanged
c--------------------------
c
        subroutine fMtxVdimVecDaxpy( a, b, c, na, nb, m, n )
c
c.... Data declaration
c
        implicit none
        integer na,     nb,     m,      n
        real*8  a(n,na),        b(n,nb),        c(m)
c
        integer i,      j
c
c.... Do the work
c
        if ( m .eq. 1 ) then

            do i = 1, n
                b(i,1) = b(i,1) + c(1) * a(i,1)
            enddo

        else if ( m .eq. 2 ) then

            do i = 1, n
                b(i,1) = b(i,1) + c(1) * a(i,1)
                b(i,2) = b(i,2) + c(2) * a(i,2)
            enddo

        else if ( m .eq. 3 ) then

            do i = 1, n
                b(i,1) = b(i,1) + c(1) * a(i,1)
                b(i,2) = b(i,2) + c(2) * a(i,2)
                b(i,3) = b(i,3) + c(3) * a(i,3)
            enddo

        else if ( m .eq. 4 ) then

            do i = 1, n
                b(i,1) = b(i,1) + c(1) * a(i,1)
                b(i,2) = b(i,2) + c(2) * a(i,2)
                b(i,3) = b(i,3) + c(3) * a(i,3)
                b(i,4) = b(i,4) + c(4) * a(i,4)
            enddo

        else

            do j = 1, m 
                do i = 1, n 
                    b(i,j) = b(i,j) + c(j) * a(i,j)
                enddo
            enddo

        endif
c
        return
        end
c
c---------
c flesApG
c---------
c
      subroutine flesApG ( ien, xGoC, lesP, lesQ, nPs, nQs )
c   
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         nps,         nqs
C
      REAL                xgoc
C
C     Local variables
C
      INTEGER             i,           i0,          j
C
      REAL                ptemp,       qtemp
C
        dimension xGoC(npro,4*nshl,nshl)
        real*8 lesP(nshg,nPs), lesQ(nshg,nQs) 
        dimension ien(npro,nshl)
        dimension Ptemp(npro,nshl,nPs), Qtemp(npro,nshl,nQs)
c
c.... zero Qtemp
c
      Qtemp = zero
c
c.... localize the lesP for the EBE product
c
        call local ( lesP, Ptemp, ien, nPs, 'gather  ' )
c
c.... Now, product operation
c
          do i = 1, nshl
           i0 = (nsd) * (i - 1)  
           do j = 1, nshl
c
             Qtemp(:,i,1) = Qtemp(:,i,1) 
     &                    + xGoC(1:npro,i0+1,j) * Ptemp(:,j,nPs)
c
             Qtemp(:,i,2) = Qtemp(:,i,2)
     &                    + xGoC(1:npro,i0+2,j) * Ptemp(:,j,nPs)
c
             Qtemp(:,i,3) = Qtemp(:,i,3)
     &                    + xGoC(1:npro,i0+3,j) * Ptemp(:,j,nPs) 
c
           enddo
        enddo
c
c... assemble the result of the product
c
        call local ( lesQ, Qtemp, ien, nQs, 'scatter ' )
c
        return 
        end
c
c----------
c flesApKG
c----------
c
       subroutine flesApKG ( ien, xKebe, xGoC, lesP, lesQ, nPs, nQs )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         nps,         nqs
C
      REAL                xgoc,        xkebe
C
C     Local variables
C
      INTEGER             i,           i0,          j,           j0
C
      REAL                ptemp,       qtemp
C
        dimension xKebe(npro,3*nshl,3*nshl), 
     &       xGoC(npro,4*nshl,nshl)
        dimension ien(npro,nshl)
        real*8 lesP(nshg,nPs), lesQ(nshg,nQs)
        dimension Ptemp(npro,nshl,nPs), Qtemp(npro,nshl,nQs)       
c
c.... zero Qtemp
c
      Qtemp = zero
c
c.... localize the lesP for the EBE product
c
        call local ( lesP, Ptemp, ien, nPs, 'gather  ' )
c
c.... Now, product operation
c
c.... K contribution 
c
        do i = 1, nshl
           i0 = (nsd) * (i - 1)
          do j = 1, nshl
             j0 = (nsd) * (j - 1) 
c
            Qtemp(:,i,1) = Qtemp(:,i,1)
     &                   + xKebe(1:npro,i0+1,j0+1) * Ptemp(:,j,1)
     &                   + xKebe(1:npro,i0+1,j0+2) * Ptemp(:,j,2)    
     &                   + xKebe(1:npro,i0+1,j0+3) * Ptemp(:,j,3)
c
            Qtemp(:,i,2) = Qtemp(:,i,2)
     &                   + xKebe(1:npro,i0+2,j0+1) * Ptemp(:,j,1)
     &                   + xKebe(1:npro,i0+2,j0+2) * Ptemp(:,j,2)
     &                   + xKebe(1:npro,i0+2,j0+3) * Ptemp(:,j,3)
            Qtemp(:,i,3) = Qtemp(:,i,3)
     &                   + xKebe(1:npro,i0+3,j0+1) * Ptemp(:,j,1)
     &                   + xKebe(1:npro,i0+3,j0+2) * Ptemp(:,j,2)
     &                   + xKebe(1:npro,i0+3,j0+3) * Ptemp(:,j,3)
c
          enddo
           enddo
c
c.... G contribution
c
        do i = 1, nshl 
           i0 = (nsd) * (i - 1) 
          do j = 1, nshl
c
            Qtemp(:,i,1) = Qtemp(:,i,1)
     &                   + xGoC(1:npro,i0+1,j) * Ptemp(:,j,nPs)
            Qtemp(:,i,2) = Qtemp(:,i,2)
     &                   + xGoC(1:npro,i0+2,j) * Ptemp(:,j,nPs)
            Qtemp(:,i,3) = Qtemp(1:,i,3)
     &                   + xGoC(1:npro,i0+3,j) * Ptemp(:,j,nPs)
c
          enddo
        enddo
c
c.... assemble the result of the product
c
        call local ( lesQ, Qtemp, ien, nQs, 'scatter ' )
c
        return
        end
c
c-----------
c flesApNGt
c-----------
c
      subroutine flesApNGt ( ien, xGoC, lesP, lesQ, nPs, nQs )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         nps,         nqs
C
      REAL                xgoc
C
C     Local variables
C
      INTEGER             i,           i0,          j
C
      REAL                ptemp,       qtemp
C
        dimension ien(npro,nshl), xGoC(npro,4*nshl,nshl)
        real*8 lesP(nshg,nPs), lesQ(nshg,nQs)
        dimension Ptemp(npro,nshl,nPs), Qtemp(npro,nshl,nQs)
c
c.... zero Qtemp
c
      Qtemp = zero 
c
c.... localize the lesP for the EBE product
c
        call local ( lesP, Ptemp, ien, nPs, 'gather  ' )
c
c.... Now, product operation
c
c.... Negative G^t contribution ( not explicitly formed )
c
        do i = 1, nshl
           do j = 1, nshl
              i0 = (nsd) * (j - 1)
c
             Qtemp(:,i,nQs) = Qtemp(:,i,nQs)
     &                      - xGoC(1:npro,i0+1,i) * Ptemp(:,j,1)
     &                      - xGoC(1:npro,i0+2,i) * Ptemp(:,j,2)
     &                      - xGoC(1:npro,i0+3,i) * Ptemp(:,j,3)
c
           enddo
        enddo
c
c... assemble the result of the product
c
        call local ( lesQ, Qtemp, ien, nQs, 'scatter  ' )
c
        return
        end
c
c------------
c flesApNGtC 
c------------
c
      subroutine flesApNGtC ( ien, xGoC, lesP, lesQ, nPs, nQs )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         nps,         nqs
C
      REAL                xgoc
C
C     Local variables
C
      INTEGER             i,           i0,          j,           nnm2
C
      REAL                ptemp,       qtemp
C
        dimension ien(npro,nshl), xGoC(npro,4*nshl,nshl)
        real*8 lesP(nshg,nPs), lesQ(nshg,nQs)
        dimension Ptemp(npro,nshl,nPs), Qtemp(npro,nshl,nQs)
c
c.... zero Qtemp
c
      Qtemp = zero
c
c.... localize the lesP for the EBE product
c
      call local ( lesP, Ptemp, ien, nPs, 'gather  ')
c
c.... Now, product operation
c
c.... Negative G^t contribution ( not explicitly formed )
c
        do i = 1, nshl
           do j = 1, nshl
           i0 = (nsd) * (j - 1)
c
             Qtemp(:,i,nQs) = Qtemp(:,i,nQs)
     &                      - xGoC(1:npro,i0+1,i) * Ptemp(:,j,1)
     &                      - xGoC(1:npro,i0+2,i) * Ptemp(:,j,2)
     &                      - xGoC(1:npro,i0+3,i) * Ptemp(:,j,3)
c
           enddo
        enddo
c
c.... C contribution
c
        nnm2 = nshl * (nsd)
c
        do i = 1, nshl
           i0 = nnm2 + i
          do j = 1, nshl
c
             Qtemp(:,i,nQs) = Qtemp(:,i,nQs)
     &                      + xGoC(1:npro,i0,j) * Ptemp(:,j,nPs)
c
          enddo
        enddo
c
c... assemble the result of the product
c
        call local ( lesQ, Qtemp, ien, nQs, 'scatter  ' )
c
        return
        end
c
c------------
c flesApFull
c------------
c
      subroutine flesApFull ( ien, xKebe, xGoC, lesP, lesQ, nPs, nQs )
c   
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         nps,         nqs
C
      REAL                xgoc,        xkebe
C
C     Local variables
C
      INTEGER             i,           i0,          j,           j0
      INTEGER             nnm2
C
      REAL                ptemp,       qtemp
C
        dimension ien(npro,nshl)
        dimension xKebe(npro,3*nshl,3*nshl), 
     &       xGoC(npro,4*nshl,nshl)
        real*8 lesP(nshg,nPs), lesQ(nshg,nQs)
        dimension Ptemp(npro,nshl,nPs), Qtemp(npro,nshl,nQs)
c
c.... zero Qtemp
c
      Qtemp = zero
c
c.... localize the lesP for the EBE product
c
       call local ( lesP, Ptemp, ien, nPs, 'gather  ' )
c
c.... Now, product operation
c
c.... K * Du contribution
c
        do i = 1, nshl
           i0 = (nsd) * (i - 1)
          do j = 1, nshl
             j0 = (nsd) * (j - 1)
c
            Qtemp(:,i,1) = Qtemp(:,i,1)
     &                   + xKebe(1:npro,i0+1,j0+1) * Ptemp(:,j,1)
     &                   + xKebe(1:npro,i0+1,j0+2) * Ptemp(:,j,2)
     &                   + xKebe(1:npro,i0+1,j0+3) * Ptemp(:,j,3)
c
            Qtemp(:,i,2) = Qtemp(:,i,2)
     &                   + xKebe(1:npro,i0+2,j0+1) * Ptemp(:,j,1)
     &                   + xKebe(1:npro,i0+2,j0+2) * Ptemp(:,j,2)
     &                   + xKebe(1:npro,i0+2,j0+3) * Ptemp(:,j,3)
            Qtemp(:,i,3) = Qtemp(:,i,3)
     &                   + xKebe(1:npro,i0+3,j0+1) * Ptemp(:,j,1)
     &                   + xKebe(1:npro,i0+3,j0+2) * Ptemp(:,j,2)
     &                   + xKebe(1:npro,i0+3,j0+3) * Ptemp(:,j,3)
c
          enddo
        enddo
c
c.... G * Dp contribution
c
       do i = 1, nshl
           i0 = (nsd) * (i - 1)
          do j = 1, nshl
c
            Qtemp(:,i,1) = Qtemp(:,i,1)
     &                   + xGoC(1:npro,i0+1,j) * Ptemp(:,j,nPs)
            Qtemp(:,i,2) = Qtemp(:,i,2)
     &                   + xGoC(1:npro,i0+2,j) * Ptemp(:,j,nPs)
            Qtemp(:,i,3) = Qtemp(:,i,3)
     &                   + xGoC(1:npro,i0+3,j) * Ptemp(:,j,nPs)
c
          enddo
        enddo
c
c.... -G^t * Du contribution
c
       do i = 1, nshl
           do j = 1, nshl
              i0 = (nsd) * (j - 1)
c
             Qtemp(:,i,nQs) = Qtemp(:,i,nQs)
     &                      - xGoC(1:npro,i0+1,i) * Ptemp(:,j,1)
     &                      - xGoC(1:npro,i0+2,i) * Ptemp(:,j,2)
     &                      - xGoC(1:npro,i0+3,i) * Ptemp(:,j,3)
c
           enddo
        enddo
c
c.... C * Dp contribution
c
        nnm2 = nshl * (nsd)
c
        do i = 1, nshl
           i0 = nnm2 + i
          do j = 1, nshl
c
             Qtemp(:,i,nQs) = Qtemp(:,i,nQs)
     &                      + xGoC(1:npro,i0,j) * Ptemp(:,j,nPs)
c
          enddo
        enddo



c
c... assemble the result of the product
c
        call local ( lesQ, Qtemp, ien, nQs, 'scatter ' )
c
        return
        end
c
c-----------
c fsclrDiag
c-----------
c
      subroutine fsclrDiag ( ien, xTe, sclrDiag )
c 
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien
C
      REAL                sclrdiag,    xte
C
C     Local variables
C
      INTEGER             i
C
      REAL                diagl
C
        dimension xTe(npro,nshl,nshl)
        dimension sclrDiag(nshg,1), Diagl(npro,nshl,1)
        dimension ien(npro,nshl)
c
        do i = 1, nshl
           Diagl(:,i,1) = xTe(1:npro,i,i)
        enddo
c
        call local (sclrDiag, Diagl, ien, 1, 'scatter ')
c  
        return
        end
c
c------------
c flesApSclr
c------------
c
      subroutine flesApSclr ( ien, xTe, lesP, lesQ, nPs, nQs )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         nps,         nqs
C
      REAL                xte
C
C     Local variables
C
      INTEGER             i,           j
C
      REAL                ptemp,       qtemp
C
        dimension xTe(npro,nshl,nshl)
        dimension ien(npro,nshl)
        real*8 lesP(nshg,nPs), lesQ(nshg,nQs)
        dimension Ptemp(npro,nshl,nPs), Qtemp(npro,nshl,nQs)
c
c.... zero Qtemp
c
        Qtemp = zero
c
c.... localize the lesP for the EBE product
c
        call local ( lesP, Ptemp, ien, nPs, 'gather  ')
c
c.... Now, product operation
c
        do i = 1, nshl
          do j = 1, nshl
c
            Qtemp(:,i,nQs) = Qtemp(:,i,nQs) 
     &                     + xTe(1:npro,i,j) * Ptemp(:,j,nPs)
c
          enddo
        enddo
c
c.... assemble the result of the product
c
        call local ( lesQ, Qtemp, ien, nQs, 'scatter ' )
c
        return
        end
