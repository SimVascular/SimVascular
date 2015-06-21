      program main
c
c ... array declarations.
c
      implicit double precision (a-h, o-z)
      dimension coef(120,5), rhs(100), u(100), wksp(600), ubar(1),
     a     rparm(30)
      integer jcoef(120,5), iwksp(300), iparm(30), p(100), ip(100)
      external cg, rs6
c
      ndim = 120
      mdim = 5
      nw = 600
      inw = 300
c
c ... generate coef, jcoef, and rhs.
c
      nx = 10
      ny = 10
      n = nx*ny
      h = 1.0d0/dble(nx + 1)
      maxnz = 5
      do i = 1,n 
         coef(i,1) = 6.0d0
         coef(i,2) = -1.0d0
         coef(i,3) = -2.0d0
         coef(i,4) = -1.0d0
         coef(i,5) = -2.0d0
         jcoef(i,1) = i
         jcoef(i,2) = i + 1
         jcoef(i,3) = i + nx
         jcoef(i,4) = i - 1
         jcoef(i,5) = i - nx
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
               coef(k,5) = 0.0d0
               jcoef(k,5) = 0 
            endif
            if (j .eq. ny) then
               rhs(k) = rhs(k) + 2.0d0*(1.0d0 + x)
               coef(k,3) = 0.0d0
               jcoef(k,3) = 0 
            endif
            if (i .eq. 1) then
               rhs(k) = rhs(k) + 1.0d0
               coef(k,4) = 0.0d0
               jcoef(k,4) = 0 
            endif
            if (i .eq. nx) then
               rhs(k) = rhs(k) + 1.0d0 + y
               coef(k,2) = 0.0d0
               jcoef(k,2) = 0 
            endif
         enddo
      enddo
      call dfault (iparm,rparm)
c
c ... now, reset some default values.
c
      iparm(3) = 3
      iparm(12) = 1 
      iparm(14) = 1 
      iparm(23) = 0 
c
c ... generate an initial guess for u and call nspcg.
c
      call vfill (n,u,0.0d0)
c
      call redblk (ndim,n,maxnz,coef,jcoef,p,ip,1,iwksp,ier)
      call nspcg (rs6,cg,ndim,mdim,n,maxnz,coef,jcoef,p,ip, 
     a            u,ubar,rhs,wksp,iwksp,nw,inw,iparm,rparm,ier)
      stop
      end 
