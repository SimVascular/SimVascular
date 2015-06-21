      program main
c
c ... array declarations.
c
      implicit double precision (a-h, o-z)
      dimension coef(120,4), rhs(100), u(100), wksp(600), ubar(1),
     a     rparm(30)
      integer jcoef(5), iwksp(300), iparm(30), p(1), ip(1)
      external cg, mic2
c
      ndim = 120
      mdim = 4
      n = 100
      maxnz = 3
      nw = 600
      inw = 300
c
      h = 1.0d0/11.0d0
c
c ... generate laplace*s equation.
c
      call matgen (n,ndim,maxnz,jcoef,coef,rhs,h,0)
      call dfault (iparm,rparm)
c
c ... now, reset some default values.
c
      iparm(2) = 50 
      iparm(3) = 3
      rparm(1) = 1.0d-8
c
c ... generate an initial guess for u and call nspcg.
c
      call vfill (n,u,0.0d0)
c
      call nspcg (mic2,cg,ndim,mdim,n,maxnz,coef,jcoef,p,ip,
     a            u,ubar,rhs,wksp,iwksp,nw,inw,iparm,rparm,ier)
      stop
      end 
      subroutine matgen (n,ndim,maxnz,jcoef,coef,rhs,hh,isym)
c
c     matgen generates the itpack matrices jcoef, coef, and rhs
c     for a 5-point central difference approximation of the 
c     self-adjoint pde
c
c          (a*u )     +  (c*u )   + f*u  =  g
c              x x           y y
c
c     where the coefficients a, c, f, and g are functions of (x,y)
c     (supplied by the user) and the domain is the unit square
c     (0,1) x (0,1).  dirichlet boundary conditions are imposed
c     upon the boundary in the form of the function ub(x,y) also
c     supplied by the user.
c
c     parameters -- 
c
c        n       number of linear equations (output)
c        ndim    row dimension of jcoef and coef in the calling routine
c                 (input)
c        maxnz   maximum number of nonzeros per row (output)
c        jcoef   array of column indices (output) 
c        coef    array of equation coefficients (output)
c        rhs     vector of right-hand-side values (output)
c        h       mesh spacing (= hh) (input)
c
c     specifications for parameters
c
      implicit double precision (a-h, o-z)
      integer   jcoef(5)
      dimension coef(ndim,5), rhs(1)
      logical symm
c
c     statement functions
c
      a(x,y) = 1.0d0
      c(x,y) = 2.0d0
      f(x,y) = 0.0d0
      g(x,y) = 0.0d0
      ub(x,y) = 1.0d0 + x*y
c
      h = hh
      zero = 0.0d0
      one = 1.0d0
      two = 2.0d0
      tenth = 0.1d0 
      m = int ( one/h + tenth)
      mm1 = m - 1
      n = mm1*mm1
      halfh = h/two 
      symm   = isym .eq. 0
      maxnz = 5
      if (symm) maxnz = 3
c
c     loop on equations, assuming a natural ordering from left to right
c     and from down to up.
c
      neq = 0
      do j = 1,mm1
         yy = dble (j)*h
         do 40 i = 1,mm1
            xx = dble (i)*h
            neq = neq + 1
            ae = a( xx+halfh, yy)
            aw = a( xx-halfh, yy)
            cn = c( xx, yy+halfh)
            cs = c( xx, yy-halfh)
            fp = f( xx, yy)
            gp = g( xx, yy)
            cc = ae + cn + aw + cs - h*h*fp
c
c ... center point
c
            coef(neq,1) = cc
            rhs(neq) = -h*h*gp
c
c ... east point
c
            if (i .eq. mm1) go to 5
            coef(neq,2) = -ae 
            go to 10
 5          coef(neq,2) = zero
            rhs(neq) = rhs(neq) + ae*ub( one, yy) 
c
c ... north point
c
 10         if (j .eq. mm1) go to 15
            coef(neq,3) = -cn 
            go to 20
 15         coef(neq,3) = zero
            rhs(neq) = rhs(neq) + cn*ub( xx, one) 
c
c ... west point
c
 20         if (i .eq. 1) go to 25
            if (.not. symm) coef(neq,4) = -aw
            go to 30
 25         if (.not. symm) coef(neq,4) = zero
            rhs(neq) = rhs(neq) + aw*ub(zero,yy)
c
c ... south point
c
 30         if (j .eq. 1) go to 35
            if (.not. symm) coef(neq,5) = -cs
            go to 40
 35         if (.not. symm) coef(neq,5) = zero
            rhs(neq) = rhs(neq) + cs*ub(xx,zero)
c
 40      continue
      enddo
c
c ... data structure 2.
c
      jcoef(1) = 0
      jcoef(2) = 1
      jcoef(3) = mm1
      return
      end 
