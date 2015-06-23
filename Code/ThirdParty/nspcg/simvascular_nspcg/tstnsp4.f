      program main
c
c ... array declarations.
c
      implicit double precision (a-h, o-z)
      dimension coef(120,5), rhs(100), u(100), wksp(600), ubar(1),
     a     rparm(30)
      integer jcoef(5), iwksp(300), iparm(30), p(100), ip(100)
      integer patt(2)
      external sor, sor7
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
      nz = 1
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
      iparm(14) = 1 
      iparm(19) = nx
c
c ... generate an initial guess for u and call nspcg.
c
      call vfill (n,u,0.0d0)
c
      nxp = 1
      nyp = 2
      nzp = 1
      patt(1) = 1
      patt(2) = 2
      call color (nxp,nyp,nzp,nx,ny,nz,patt,p)
      call nspcg (sor7,sor,ndim,mdim,n,maxnz,coef,jcoef,p,ip,
     a            u,ubar,rhs,wksp,iwksp,nw,inw,iparm,rparm,ier)
      stop
      end 
