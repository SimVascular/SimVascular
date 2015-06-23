      program main
c
c ... array declarations.
c
      implicit double precision (a-h, o-z)
      parameter (nxx = 20)
      parameter (nlen = nxx*nxx)
      parameter (nwk = 10*nlen)
      parameter (inwk = 3*nlen)
      dimension coef(nlen,3), rhs(nlen), u(nlen), wksp(nwk), ubar(1),
     a     rparm(16)
      integer jcoef(3), iwksp(inwk), iparm(25), p(1), ip(1) 
      external cg, bic2
c
      ndim = nlen
      mdim = 3
      nw = nwk
      inw = inwk
      zeta = 1.0d-5 
      nout = 6
c
c ... generate coef, jcoef, and rhs.
c
      nx = nxx
      ny = nx
      n = nx*ny
      h = 1.0d0/dble(nx + 1)
      maxnz = 3
      do i = 1,n 
         coef(i,1) = 6.0d0
         coef(i,2) = -1.0d0
         coef(i,3) = -2.0d0
         rhs(i) = 0.0d0
      enddo
      k = 0
      do j = 1,ny
         y = dble(j)*h
         do i = 1,nx
            x = dble(i)*h
            k = k + 1
            if (j .eq. 1) then
               rhs(k) = rhs(k) + 2.0d0
            endif
            if (j .eq. ny) then
               rhs(k) = rhs(k) + 2.0d0*(1.0d0 + x)
               coef(k,3) = 0.0d0
            endif
            if (i .eq. 1) then
               rhs(k) = rhs(k) + 1.0d0
            endif
            if (i .eq. nx) then
               rhs(k) = rhs(k) + 1.0d0 + y
               coef(k,2) = 0.0d0
            endif
         enddo
      enddo
      jcoef(1) = 0
      jcoef(2) = 1
      jcoef(3) = nx 
      call dfault (iparm,rparm)
c
c ... now, reset some default values.
c
      iparm(3) = 3
      iparm(4) = nout
      iparm(18) = 1 
      iparm(19) = nx
      rparm(1) = zeta
c
c ... generate an initial guess for u and call nspcg.
c
      call vfill (n,u,0.0d0)
c
      call nspcg (bic2,cg,ndim,mdim,n,maxnz,coef,jcoef,p,ip,
     a            u,ubar,rhs,wksp,iwksp,nw,inw,iparm,rparm,ier)
      write (nout,35) nw, inw 
 35   format (/1x,'nw  = ',i8/
     a         1x,'inw = ',i8)
      stop
      end 
