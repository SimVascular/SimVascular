c Copyright (c) 2014-2015 The Regents of the University of California.
c All Rights Reserved.
c
c Portions of the code Copyright (c) 2009-2011 Open Source Medical
c Software Corporation, University of California, San Diego.
c
c Portions of the code Copyright (c) 1998-2007 Stanford University, 
c RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
c
c See SimVascular Acknowledgements file for additional
c contributors to the source code. 
c 
c Permission is hereby granted, free of charge, to any person obtaining
c a copy of this software and associated documentation files (the
c "Software"), to deal in the Software without restriction, including 
c without limitation the rights to use, copy, modify, merge, publish, 
c distribute, sublicense, and/or sell copies of the Software, and to
c permit persons to whom the Software is furnished to do so, subject
c to the following conditions:
c 
c The above copyright notice and this permission notice shall be included 
c in all copies or substantial portions of the Software.
c 
c THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
c OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
c MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
c IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
c CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
c TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
c SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
c
      subroutine lubksb(aatmp,n,np,indx,bb)
c---------------------------------------------------------------------
c
c LU back substitution routine.
c
c---------------------------------------------------------------------

c................Declarations.........................................
      IMPLICIT NONE
      INTEGER n,np,indx,index,i,ii,j,ll      
      DIMENSION indx(n),aa(50,50),bb(n),aatmp(np*np)
      REAL*8 aa,aatmp,bb,sum
c.....................................................................
c.... convert the vector to a matrix
c
c     code needs to removed for licensing reasons
c
      index = 1
      do i=1,n
         do j=1,n
            aa(i,j) = aatmp(index)
            index = index+1
         enddo
      enddo
      
      ii=0
      do i=1,n
         ll=indx(i)
         sum=bb(ll)
         bb(ll)=bb(i)
         if (ii.ne.0)then
            do j=ii,i-1
               sum=sum-aa(i,j)*bb(j)
            enddo
         else if (sum.ne.0.0) then
            ii=i
         endif
         bb(i)=sum
      enddo
      do i=n,1,-1
         sum=bb(i)
         do j=i+1,n
            sum=sum-aa(i,j)*bb(j)
         enddo
         bb(i)=sum/aa(i,i)
      enddo
      
      return
      end

            
      
