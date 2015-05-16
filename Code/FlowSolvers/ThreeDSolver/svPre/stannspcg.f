c Copyright (c) 2014-2015 The Regents of the University of California.
c All Rights Reserved.
c
c Portions of the code Copyright (c) 2009-2011 Open Source Medical
c Software Corporation, University of California, San Diego.
c
c Portions of the code Copyright (c) 1998-2007 Stanford University,
c Charles Taylor, Nathan Wilson.
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
c

#include "cvFlowsolverOptions.h"

      subroutine stannspcg(n,ndim,coef,jcoef1,jcoef2,rhs,u)
c
c ... array declarations.
c
c      implicit double precision (a-h, o-z)
      implicit none
c     DES - BUG FIX - 20FEB2014
c     integer n,ndim,jcoef1(1),jcoef2(1)
      integer n,ndim,jcoef1(ndim),jcoef2(ndim)
      double precision coef(1),rhs(1),u(1)
c
c      double precision, allocatable :: u(:) 
      double precision, allocatable :: wksp(:)
      double precision ubar(1), rparm(30)
      integer, allocatable :: jcoef(:,:)
      integer, allocatable :: iwksp(:) 
      integer iparm(30), p(1), ip(1)
      integer mdim,maxnz,nws,i,j,ier
c
      integer itmax,nwa,nwp,inwp,nwf,inwf,nw,inw
c
      external cg, lsp4
c
c  Dymanically allocate memory    
c
c
c      do while (0 .eq. 0)
c      enddo 
      mdim = 2
      maxnz = ndim
      allocate(jcoef(ndim,2))
c      allocate(u(n))
c  Size of workspace for cg + lsp4 
      itmax = 9999
      nwa = 3*n+2*itmax
      nwp = 2*n
      inwp = 0
      nwf = 0
      inwf = 0
      nws = 0
      nw = nwa + nws + nwp + nwf
      inw = inwp + inwf + 3*n
c
      write(*,*) 'N=',n
      write(*,*) 'ndim=',ndim
      write(*,*) 'maxnz=',maxnz
c      do i=1,ndim
c        write(*,'(A,I10,F30.5)') 'f_coef ',i,coef(i)
c      enddo
c
c  Create two-dimensional Fortran-style array jcoef
c   (symmetric format)
c
      do i = 1,ndim
         jcoef(i,1) = jcoef1(i)
         jcoef(i,2) = jcoef2(i)
      enddo
c
c      do i=1,ndim
c        do j=1,2
c          write(*,'(A,I,I,I)') 'jcoef ',i,j,jcoef(i,j)
c        enddo
c      enddo
c
      allocate(wksp(nw))
      allocate(iwksp(inw))
c
      call dfault (iparm,rparm)
c
c ... now, reset some default values.
c
c ... specify symmetric coordinate format
      iparm(12) = 4
c ... specify ntest 5 instead of default 2 
      iparm(1) = 5
c
      iparm(2) = itmax 
      iparm(3) = 3
      rparm(1) = 1.0d-08
c
c ... generate an initial guess for u and call nspcg.
c
      call vfill (n,u,0.0d0)
c
      write(*,*) 'before call nw=',nw
      write(*,*) 'before call inw=',inw   
      call nspcg (lsp4,cg,ndim,mdim,n,maxnz,coef,jcoef,p,ip,
     a            u,ubar,rhs,wksp,iwksp,nw,inw,iparm,rparm,ier)
c  Clean up 
      write(*,*) 'after call nw=',nw
      write(*,*) 'after call inw=',inw 
      deallocate(wksp)
      deallocate(iwksp)
c
      allocate(wksp(nw))
      allocate(iwksp(inw))
      write(*,*) 'before 2nd call nw=',nw
      write(*,*) 'before 2nd call inw=',inw   
      call nspcg (lsp4,cg,ndim,mdim,n,maxnz,coef,jcoef,p,ip,
     a            u,ubar,rhs,wksp,iwksp,nw,inw,iparm,rparm,ier)
c  Clean up 
      write(*,*) 'after 2nd call nw=',nw
      write(*,*) 'after 2nd call inw=',inw 
      deallocate(wksp)
      deallocate(iwksp)      
      deallocate(jcoef)
c
      return
      end
