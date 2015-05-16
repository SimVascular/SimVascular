c Copyright (c)-2015 2014 The Regents of the University of California.
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
      subroutine ludcmp(aatmp,n,np,indx,d)
c---------------------------------------------------------------------
c
c     code needs to be removed for licensing reasons

c.............Declarations......................
      IMPLICIT NONE
      INTEGER np,n,indx,index,imax,i,j,k	     
      DIMENSION aatmp(np*np),aa(50,50),indx(n),vv(50)
      REAL*8 aatmp,aamax,aa,dum,d,sum,vv
c................................................
c.... convert the vector to a matrix
c
      index = 1
      do i=1,n
         do j=1,n
            aa(i,j) = aatmp(index)
            index = index+1
         enddo
      enddo

      d=1.0
      do i=1,n
         aamax=0.0
         do j=1,n
            if (abs(aa(i,j)).gt.aamax) aamax=abs(aa(i,j))
         enddo
         vv(i)=1.0/aamax
      enddo
      
      do j=1,n
         do i=1,j-1
            sum=aa(i,j)
            do k=1,i-1
               sum=sum-aa(i,k)*aa(k,j)
            enddo
            aa(i,j)=sum
         enddo
         aamax = 0.0
         do i=j,n
            sum=aa(i,j)
            do k=1,j-1
               sum=sum-aa(i,k)*aa(k,j)
            enddo
            aa(i,j)=sum
            dum=vv(i)*abs(sum)
            if (dum.ge.aamax) then
               imax=i
               aamax=dum
            endif
         enddo
         if (j.ne.imax) then
            do k=1,n
               dum=aa(imax,k)
               aa(imax,k)=aa(j,k)
               aa(j,k)=dum
            enddo
            d=-d
            vv(imax)=vv(j)
         endif
         indx(j)=imax
         if(j.ne.n)then
            dum=1.0/aa(j,j)
            do i=j+1,n
               aa(i,j)=aa(i,j)*dum
            enddo
         endif
      enddo

c
c.... convert the matrix back to a vector
c
      index = 1
      do i=1,n
         do j=1,n
            aatmp(index) = aa(i,j)
            index = index+1
         enddo
      enddo
      
      return
      end
