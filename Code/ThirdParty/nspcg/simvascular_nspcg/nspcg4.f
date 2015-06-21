      subroutine nmcalc (coef,jcoef,wfac,jwfac,icall,subq,nn,
     a                   rhs,ubar,wksp,ier)
      implicit double precision (a-h, o-z)
c
c ... nmcalc calculates the quantities
c
c       bnorm   = sqrt (rhs,rhs)
c       bnorm1  = any other norm of rhs needed for the stopping test
c       ubarnm  = sqrt (ubar,ubar)
c
c     which are needed in the stopping tests.
c
c     the stopping tests are --
c
c    (1)  (emax/emin) * sqrt ( (r ,zt)/(rhs,inv(q)*rhs) )
c    (2)  ( 1.0/emin) * sqrt ( (zt,zt)/(u,u) )
c    (3)  (emax/emin) * sqrt ( (zt,zt)/(inv(q)*rhs,inv(q)*rhs) )
c    (4)                sqrt ( (zt,zt)/(inv(q)*rhs,inv(q)*rhs) )
c    (5)                sqrt ( (r ,r )/(rhs,rhs) )
c    (6)                sqrt ( (u-ubar,u-ubar)/(ubar,ubar) )
c    (7)  (emax/emin) * sqrt ( (r,z)/(rhs,inv(ql)*rhs) )
c    (8)  ( 1.0/emin) * sqrt ( (z,z)/(u,u) )
c    (9)  (emax/emin) * sqrt ( (z,z)/(inv(ql)*rhs,inv(ql)*rhs) )
c   (10)                sqrt ( (z,z)/(inv(ql)*rhs,inv(ql)*rhs) )
c
c ... parameters -- 
c
c        icall      key for initial or secondary call
c                    = 1   initial call 
c                    = 2   later call (needed if q is changed)
c        subq       preconditioning routine
c        n          order of system
c        rhs        right hand side
c        ubar       known solution
c        wksp       workspace vector of length n
c        ier        error code
c                    =  0  no error detected
c                    = -7  q is not positive definite
c
c ... specifications for parameters
c
      dimension rhs(1), ubar(1), wksp(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external subq 
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
c
      n = nn
      nteste = ntest
      if (ntest .gt. 6) nteste = ntest - 6
      go to (10,50,20,20,30,40), nteste 
c
c ... bnorm1: sqrt(b,q(inv)b).
c
 10   call subq (coef,jcoef,wfac,jwfac,n,rhs,wksp)
      sum = vdot (n,rhs,wksp) 
      if (sum .ge. 0.0d0) go to 15
      ier = -7
      call ershow (ier,'nmcalc')
      return
 15   bnorm1 = max ( sqrt(sum),srelpr )
      return
c
c ... bnorm1: sqrt(q(inv)b,q(inv)b).
c
 20   call subq (coef,jcoef,wfac,jwfac,n,rhs,wksp)
      sum = vdot (n,wksp,wksp)
      bnorm1 = max ( sqrt(sum),srelpr )
      return
c
c ... bnorm.
c
 30   if (icall .eq. 2) return
      sum = vdot (n,rhs,rhs)
      bnorm = max ( sqrt(sum),srelpr )
      bnorm1 = bnorm
      return
c
c ... ubarnm.
c
 40   if (icall .eq. 2) return
      sum = vdot (n,ubar,ubar)
      ubarnm = max ( sqrt(sum),srelpr )
      return
c
c ... exit.
c
 50   return
      end 
      subroutine omgchg (ssorcp,coef,jcoef,wfac,jwfac,n,p,r)
      implicit double precision (a-h, o-z)
c
c ... omgchg changes alphab and betab for a new estimate of omega.
c
c ... parameters -- 
c
c         n       order of system (= nn)
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c
c ... specifications for parameters
c
      dimension p(1), r(1), coef(1), jcoef(2), wfac(1), jwfac(1)
      external ssorcp
c
c *** begin -- package common 
c
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- package common 
c
c
c ... update alphab and betab.
c
      call ssorcp (coef,jcoef,wfac,jwfac,n,p,r,pdp,pldup)
      alphab = min (alphab, (pap/pdp) - 1.0d0)
      betab  = max (betab , pldup/pdp)
      return
      end 
      subroutine out (nn,v,iswt,noutt)
      implicit double precision (a-h, o-z)
c
c     out effects printing of residual and solution
c     vectors - called from perror1
c
c ... parameters -- 
c
c          v      vector of length n
c          iswt   labelling information 
c          nout   output device number (= noutt)
c
c ... specifications for parameters
c
      dimension v(nn)
c
         n = nn
         nout = noutt
         if (n .le. 0) return 
c
         kupper = min (n, 4) 
         if (iswt .eq. 1) write (nout,10)
 10      format (//5x,'residual vector')
         if (iswt .eq. 2) write (nout,15)
 15      format (//5x,'solution vector')
         write (nout,20) (i,i=1,kupper) 
 20      format (10x,4i15)
         write (nout,25)
 25      format (10x,65('-') /)
c
         do 35 j = 1,n,4
            kupper = min (j+3,n)
            jm1 = j - 1
            write (nout,30) jm1,(v(k),k=j,kupper) 
 30         format (4x,i5,'+  ',4d15.5) 
 35      continue
c
         return
      end 
      subroutine pbneu (suba,dsolve,coef,jcoef,wfac,jwfac,
     a                  nd,wksp,nn,r,z) 
      implicit double precision (a-h, o-z)
c
c ... pbneu computes the nd-degree block neumann polynomial 
c ... approximation to the matrix inv(a).
c     if a = d - b, where d is a dense banded matrix
c     then the output vector is --
c
c             z = (i + p + p**2 + ... + p**nd)*inv(d) * r
c
c     where   p = inv(d)*b .
c
c ... parameters -- 
c
c         suba    matrix-vector multiplication routine
c         dsolve  routine for computing inv(d)*vector
c         nd      the degree of the polynomial desired
c         wksp    workspace of length 2*n
c         n       order of system (= nn)
c         r       residual
c         z       output vector
c
c ... specifications for parameters
c
      external  suba, dsolve
      dimension r(1), z(1), wksp(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
c
      n = nn
      np1 = n + 1
      call dsolve (coef,jcoef,wfac,jwfac,n,r,z)
      if (nd .le. 0) return
c
      do 20 k = 1,nd
         call suba (coef,jcoef,wfac,jwfac,n,z,wksp)
         do 10 i = 1,n
 10      wksp(i) = r(i) - wksp(i)
         call dsolve (coef,jcoef,wfac,jwfac,n,wksp,wksp(np1))
         do 15 i = 1,n
 15      z(i) = z(i) + wksp(i+n)
 20   continue
      return
      end 
      subroutine pbpii (suba,dsolve,coef,jcoef,wfac,jwfac,
     a                  ainf,alpha,beta,nd,wksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... pbpii computes the block nd-degree least squares polynomial
c ... approximation to the matrix inv(a).  the output vector is --
c
c ...        z = inv(d)*p  (a*inv(d)) * r
c ...                    np
c
c ... parameters -- 
c
c         suba    matrix-vector multiplication routine
c         dsolve  routine to compute inv(d)*vector
c         ainf    the infinity norm of matrix inv(d)*a
c         alpha,  the least squares weighting factors
c          beta
c         nd      the degree of the polynomial desired
c         wksp    workspace of length 2*n
c         n       order of system (= nn)
c         r       residual
c         z       output vector
c
c ... specifications for parameters
c
      external  suba, dsolve
      dimension r(1), z(1), wksp(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
c
c
      n = nn
      np1 = n + 1
      al = alpha
      be = beta
c
      c1 = ((al+be+2.0d0)*(al+be+3.0d0))/(ainf*(al+2.0d0)*(al+be+2.0d0))
      call dsolve (coef,jcoef,wfac,jwfac,n,r,z)
      do 10 i = 1,n 
 10   z(i) = c1*z(i)
      if (nd .le. 0) return
c
      do 15 i = 1,n 
 15   wksp(i) = r(i)
      do 35 k = 1,nd
         fk = dble (k)
         c1 = ((2.0d0*fk+al+be+2.0d0)*(2.0d0*fk+al+be+3.0d0))/
     a         (ainf*(fk+al+2.0d0)*(fk+al+be+2.0d0))
         c2 = (fk*(fk+be)*(2.0d0*fk+al+be))/
     a        ((fk+al+1.0d0)*(fk+al+be+1.0d0)*(2.0d0*fk+al+be+2.0d0))
         call suba (coef,jcoef,wfac,jwfac,n,z,wksp(np1))
         do 20 i = 1,n
 20      wksp(n+i) = r(i) - wksp(n+i)
         do 25 i = 1,n
 25      wksp(i) = wksp(i+n) + c2*wksp(i)
         call dsolve (coef,jcoef,wfac,jwfac,n,wksp,wksp(np1))
         do 30 i = 1,n
 30      z(i) = z(i) + c1*wksp(n+i)
 35   continue
      return
      end 
      subroutine pneu (suba,coef,jcoef,wfac,jwfac,d,nd,wksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... pneu computes the nd-degree point neumann polynomial
c ... approximation to the matrix inv(a).  the output vector is --
c ...        z = p  (a)*r
c ...             np
c
c ... parameters -- 
c
c         suba    matrix-vector multiplication routine
c         d       vector of length n giving the diagonal elements
c                  of the matrix
c         nd      the degree of the polynomial desired
c         wksp    workspace of length n 
c         n       order of system (= nn)
c         r       residual
c         z       output vector
c
c ... specifications for parameters
c
      external  suba
      dimension r(1), d(1), z(1), wksp(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
c
      n = nn
      do 10 i = 1,n 
 10   z(i) = r(i)/d(i)
      if (nd .le. 0) return
c
      do 20 k = 1,nd
         call suba (coef,jcoef,wfac,jwfac,n,z,wksp)
         do 15 i = 1,n
 15      z(i) = z(i) + (r(i) - wksp(i))/d(i)
 20   continue
      return
      end 
      subroutine ppii (suba,coef,jcoef,wfac,jwfac,ainf,
     a                 alpha,beta,nd,wksp,nn,r,z) 
      implicit double precision (a-h, o-z)
c
c ... ppii computes the nd-degree least squares polynomial
c ... approximation to the matrix inv(a).  the output vector is --
c ...        z = p  (a)*r
c ...             np
c
c ... parameters -- 
c
c         suba    matrix-vector multiplication routine
c         ainf    the infinity norm of matrix a
c         alpha,  the least squares weighting factors
c          beta
c         nd      the degree of the polynomial desired
c         wksp    workspace of length 2*n
c         n       order of system (= nn)
c         r       residual
c         z       output vector
c
c ... specifications for parameters
c
      external  suba
      dimension r(1), z(1), wksp(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
c
c
      n = nn
      np1 = n + 1
      al = alpha
      be = beta
c
      c1 = ((al+be+2.0d0)*(al+be+3.0d0))/(ainf*(al+2.0d0)*(al+be+2.0d0))
      do 10 i = 1,n 
 10   z(i) = c1*r(i)
      if (nd .le. 0) return
c
      do 15 i = 1,n 
 15   wksp(i) = r(i)
      do 35 k = 1,nd
         fk = dble (k)
         c1 = ((2.0d0*fk+al+be+2.0d0)*(2.0d0*fk+al+be+3.0d0))/
     a         (ainf*(fk+al+2.0d0)*(fk+al+be+2.0d0))
         c2 = (fk*(fk+be)*(2.0d0*fk+al+be))/
     a         ((fk+al+1.0d0)*(fk+al+be+1.0d0)*(2.0d0*fk+al+be+2.0d0))
         call suba (coef,jcoef,wfac,jwfac,n,z,wksp(np1))
         do 20 i = 1,n
 20      wksp(n+i) = r(i) - wksp(n+i)
         do 25 i = 1,n
 25      wksp(i) = wksp(i+n) + c2*wksp(i)
         do 30 i = 1,n
 30      z(i) = z(i) + c1*wksp(i)
 35   continue
      return
      end 
      subroutine pbs (n,t1,t2,x)
      implicit double precision (a-h, o-z)
c
c ... pbs does a penta-diagonal back substitution (i+t1+t2)*x = y
c     where t1 and t2 are the first and second super diagonals.
c
c ... parameters -- 
c
c          n      order of the system
c          t1     vector of length n-1 containing the first super-
c                  diagonal elements
c          t2     vector of length n-2 containing the second super-
c                  diagonal elements
c          x      on input, x contains y
c                 on output, x contains the solution to
c                  (i + t1 + t2)*x = y
c
c ... specifications for parameters
c
      dimension t1(1), t2(1), x(1)
c
      x(n-1) = x(n-1) - t1(n-1)*x(n)
      do 10 i = n-2,1,-1
 10   x(i) = x(i) - t1(i)*x(i+1) - t2(i)*x(i+2)
      return
      end 
      subroutine pbsm (nn,nsize,t1,t2,x)
      implicit double precision (a-h, o-z)
c
c ... pbsm does a penta-diagonal back substitution (i+t1+t2)*x = y
c     where t1 and t2 are superdiagonals of a system composed of
c     independent subsystems of size nsize.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  order of the individual subsystems
c          t1     linear array of length n-1 containing the first
c                  super-diagonal elements of the factorizations
c          t2     linear array of length n-2 containing the second
c                  super-diagonal elements of the factorizations
c          x      on input, x contains y
c                 the solution to (i + t1 + t2)*x = y
c
c ... specifications for parameters
c
      dimension t1(nsize,1), t2(nsize,1), x(nsize,1)
c
      n = nn
      nsys = n/nsize
      do 10 j = 1,nsys
 10   x(nsize-1,j) = x(nsize-1,j) - t1(nsize-1,j)*x(nsize,j)
      do 20 i = nsize-2,1,-1
         do 15 j = 1,nsys
 15      x(i,j) = x(i,j) - t1(i,j)*x(i+1,j) - t2(i,j)*x(i+2,j)
 20   continue
      return
      end 
      subroutine pfac (nn,d,t1,t2)
      implicit double precision (a-h, o-z)
c
c ... pfac computes a factorization of a single symmetric
c     pentadiagonal matrix contained in d, t1, and t2 and
c     replaces it.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of the matrix
c          t1     vector of length n-1 containing the first 
c                  super-diagonal elements of the matrix
c          t2     vector of length n-2 containing the second
c                  super-diagonal elements of the matrix
c
c ... specifications for parameters
c
      dimension d(1), t1(1), t2(1)
c
      n = nn
      do 10 i = 1,n-2
         dii = 1.0d0/d(i)
         d(i+1) = d(i+1) - t1(i)*t1(i)*dii
         d(i+2) = d(i+2) - t2(i)*t2(i)*dii
         t1(i+1) = t1(i+1) - t1(i)*t2(i)*dii
 10   continue
      d(n) = d(n) - t1(n-1)*t1(n-1)/d(n-1)
      do 15 i = 1,n 
 15   d(i) = 1.0d0/d(i)
      do 20 i = 1,n-1
 20   t1(i) = d(i)*t1(i)
      do 25 i = 1,n-2
 25   t2(i) = d(i)*t2(i)
      return
      end 
      subroutine pfacm (nn,nsize,d,t1,t2)
      implicit double precision (a-h, o-z)
c
c ... pfacm computes factorizations of multiple independent 
c     symmetric pentadiagonal matrices contained in d, t1, and t2.
c
c ... parameters -- 
c
c          n      order of global system (= nn)
c          nsize  size of the individual subsystems
c          d      linear array of length n containing the
c                  diagonal elements of the systems
c          t1     linear array of length n-1 containing the 
c                  first super-diagonal elements of the systems
c          t2     linear array of length n-2 containing the 
c                  second super-diagonal elements of the systems
c
c ... specifications for parameters
c
      dimension d(nsize,1), t1(nsize,1), t2(nsize,1)
c
      n = nn
      nsys = n/nsize
      do 15 i = 1,nsize-2
         do 10 j = 1,nsys
            d(i+1,j) = d(i+1,j) - (t1(i,j)**2)/d(i,j)
            d(i+2,j) = d(i+2,j) - (t2(i,j)**2)/d(i,j)
            t1(i+1,j) = t1(i+1,j) - t1(i,j)*t2(i,j)/d(i,j)
 10      continue
 15   continue
      do 20 j = 1,nsys
 20   d(nsize,j) = d(nsize,j) - (t1(nsize-1,j)**2)/d(nsize-1,j)
      call vinv (n,d)
      call vexopy (n-1,t1,d,t1,3)
      call vexopy (n-2,t2,d,t2,3)
      return
      end 
      subroutine pfacn (nn,d,t1,t2,b1,b2)
      implicit double precision (a-h, o-z)
c
c ... pfacn computes a factorization of a single nonsymmetric
c     pentadiagonal matrix contained in d,t1,t2,b1, and b2
c     and replaces it.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of the matrix
c          t1     vector of length n-1 containing the first 
c                  super-diagonal elements of the matrix
c          t2     vector of length n-2 containing the second
c                  super-diagonal elements of the matrix
c          b1     vector of length n-1 containing the first 
c                  sub-diagonal elements of the matrix
c          b2     vector of length n-2 containing the second
c                  sub-diagonal elements of the matrix
c
c ... specifications for parameters
c
      dimension d(1), t1(1), t2(1), b1(1), b2(1)
c
      n = nn
      do 10 i = 1,n-2
         dii = 1.0d0/d(i)
         d(i+1) = d(i+1) - b1(i)*t1(i)*dii
         d(i+2) = d(i+2) - b2(i)*t2(i)*dii
         t1(i+1) = t1(i+1) - b1(i)*t2(i)*dii
         b1(i+1) = b1(i+1) - b2(i)*t1(i)*dii
 10   continue
      d(n) = d(n) - b1(n-1)*t1(n-1)/d(n-1)
      do 15 i = 1,n 
 15   d(i) = 1.0d0/d(i)
      do 20 i = 1,n-1
         t1(i) = d(i)*t1(i)
         b1(i) = d(i)*b1(i)
 20   continue
      do 25 i = 1,n-2
         t2(i) = d(i)*t2(i)
         b2(i) = d(i)*b2(i)
 25   continue
      return
      end 
      subroutine pfacnm (nn,nsize,d,t1,t2,b1,b2)
      implicit double precision (a-h, o-z)
c
c ... pfacnm computes factorizations of multiple independent
c     nonsymmetric pentadiagonal matrices contained in
c     d,t1,t2,b1, and b2.
c
c ... parameters -- 
c
c          n      order of global system (= nn)
c          nsize  order of single subsystem
c          d      linear array of length n containing the
c                  diagonal elements of the systems
c          t1     linear array of length n-1 containing the first
c                  super-diagonal elements of the systems
c          t2     linear array of length n-2 containing the second
c                  super-diagonal elements of the systems
c          b1     linear array of length n-1 containing the first
c                  sub-diagonal elements of the systems
c          b2     linear array of length n-2 containing the second
c                  sub-diagonal elements of the systems
c
c ... specifications for parameters
c
      dimension d(nsize,1), t1(nsize,1), b1(nsize,1), t2(nsize,1),
     a            b2(nsize,1) 
c
      n = nn
      nsys = n/nsize
      do 15 i = 1,nsize-2
         do 10 j = 1,nsys
            d(i+1,j) = d(i+1,j) - b1(i,j)*t1(i,j)/d(i,j)
            d(i+2,j) = d(i+2,j) - b2(i,j)*t2(i,j)/d(i,j)
            t1(i+1,j) = t1(i+1,j) - b1(i,j)*t2(i,j)/d(i,j)
            b1(i+1,j) = b1(i+1,j) - b2(i,j)*t1(i,j)/d(i,j)
 10      continue
 15   continue
      do 20 j = 1,nsys
 20   d(nsize,j) = d(nsize,j) - b1(nsize-1,j)*t1(nsize-1,j)/
     a               d(nsize-1,j)
      call vinv (n,d)
      call vexopy (n-1,t1,d,t1,3)
      call vexopy (n-2,t2,d,t2,3)
      call vexopy (n-1,b1,d,b1,3)
      call vexopy (n-2,b2,d,b2,3)
      return
      end 
      subroutine pfs (n,b1,b2,x)
      implicit double precision (a-h, o-z)
c
c ... pfs does a penta-diagonal forward substitution  (i+b1+b2)*x = y 
c     where b1 and b2 are the first and second sub-diagonals.
c
c ... parameters -- 
c
c          n      order of system
c          b1     vector of length n-1 containing the first 
c                  sub-diagonal elements
c          b2     vector of length n-2 containing the second
c                  sub-diagonal elements
c          x      on input, x contains y
c                 on output, x contains the solution to
c                  (i + b1 + b2)*x = y
c
c ... specifications for parameters
c
      dimension b1(1), b2(1), x(2)
c
      x(2) = x(2) - b1(1)*x(1)
      do 10 i = 3,n 
 10   x(i) = x(i) - b1(i-1)*x(i-1) - b2(i-2)*x(i-2)
      return
      end 
      subroutine pfsm (nn,nsize,b1,b2,x)
      implicit double precision (a-h, o-z)
c
c ... pfsm does a penta-diagonal forward substitution (i+b1+b2)*x = y 
c     where b1 and b2 are subdiagonals of a system composed of
c     independent subsystems of size nsize.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  order of the individual subsystems
c          b1     linear array of length n-1 containing the first
c                  sub-diagonal elements of the factorizations
c          b2     linear array of length n-2 containing the second
c                  sub-diagonal elements of the factorizations
c          x      on input, x contains y
c                 on output, x contains 
c                 the solution to (i + b1 + b2)*x = y
c
c ... specifications for parameters
c
      dimension b1(nsize,1), b2(nsize,1), x(nsize,1)
c
      n = nn
      nsys = n/nsize
      do 10 j = 1,nsys
 10   x(2,j) = x(2,j) - b1(1,j)*x(1,j)
      do 20 i = 3,nsize
         do 15 j = 1,nsys
 15      x(i,j) = x(i,j) - b1(i-1,j)*x(i-1,j) - b2(i-2,j)*x(i-2,j)
 20   continue
      return
      end 
      subroutine psoln (nn,d,t1,t2,b1,b2,y,x)
      implicit double precision (a-h, o-z)
c
c ... psoln solves the system ax = y for x, where a is a single
c     pentadiagonal system.  d, t1, t2, b1, and b2 contain
c     the main, first and second super, and first and second sub
c     diagonals, respectively, of the factorization.
c
c ... parameters -- 
c
c          n      order of system
c          d      vector of length n containing the diagonal
c                  elements of the factorization matrix
c          t1     vector of length n-1 containing the first 
c                  super-diagonal elements of the factorization
c          t2     vector of length n-2 containing the second
c                  super-diagonal elements of the factorization
c          b1     vector of length n-1 containing the first 
c                  sub-diagonal elements of the factorization
c          b2     vector of length n-2 containing the second
c                  sub-diagonal elements of the factorization
c          y      the right-hand side
c          x      the solution to ax = y
c
c ... specifications for parameters
c
      dimension d(1), t1(1), t2(1), b1(1), b2(1), x(1), y(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call pfs (n,b1,b2,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call pbs (n,t1,t2,x)
      return
      end 
      subroutine psolnm (nn,nsize,d,t1,t2,b1,b2,y,x)
      implicit double precision (a-h, o-z)
c
c ... psolnm solves the system ax = y for x, where a contains
c     multiple pentadiagonal systems.  d, t1, t2, b1, and b2 are
c     the main, first and second super, and the first and second
c     sub diagonals, respectively, of the factorization.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of an individual subsystem 
c          d      vector of length n containing the diagonal
c                  elements of the factorization matrix
c          t1     vector of length n-1 containing the first 
c                  super-diagonal elements of the factorization
c          t2     vector of length n-2 containing the second
c                  super-diagonal elements of the factorization
c          b1     vector of length n-1 containing the first 
c                  sub-diagonal elements of the factorization
c          b2     vector of length n-2 containing the second
c                  sub-diagonal elements of the factorization
c          y      the right-hand side
c          x      the solution to ax = y
c
c ... specifications for parameters
c
      dimension d(1), t1(1), t2(1), b1(1), b2(1), x(1), y(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call pfsm (n,nsize,b1,b2,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call pbsm (n,nsize,t1,t2,x)
      return
      end 
      subroutine parsi
      implicit double precision (a-h, o-z)
c
c ... parsi computes the iteration parameters.
c
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      logical           halt, maxadp, minadp, maxadd, minadd
      common / itcom2 / halt, maxadp, minadp, maxadd, minadd
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      rhol = rho
      if (is - 1) 10,15,20
 10   rho = 1.0d0
      go to 25
 15   rho = 1.0d0/(1.0d0 - sigma*sigma/2.0d0) 
      go to 25
 20   rho = 1.0d0/(1.0d0 - sigma*sigma*rhol/4.0d0)
c
c ... compute alpha, beta.
c
 25   alpha = rho*gamma
      beta = rhol*(rho - 1.0d0)/rho
      return
      end 
      subroutine permas (isym,nn,nzz,ia,ja,a,wksp,p)
      implicit double precision (a-h, o-z)
c
c ... permas takes the sparse matrix representation of the
c     matrix and permutes both rows and columns, overwriting
c     the previous structure.  (sparse data structure)
c
c ... parameters -- 
c
c          isym      switch for symmetric storage 
c                     = 0   matrix is symmetric
c                     = 1   matrix is nonsymmetric
c          n         size of system
c          nz        length of ia, ja, and a vectors
c          ia        vector of i values 
c          ja        vector of j values 
c          a         vector of matrix coefficients
c          wksp      workspace vector of length n 
c          p         permutation vector 
c
c ... it is assumed that the i-th entry of the permutation vector
c     p indicates the row the i-th row gets mapped into.  (i.e.
c     if ( p(i) = j ) row i gets mapped into row j)
c
c ... specifications for parameters
c
      dimension a(1), wksp(1) 
      integer   ia(1), ja(1), p(1)
c
      n = nn
      nz = nzz
c
c ... explicit gathers.
c
      call vgathi (nz,p,ia,ia)
      call vgathi (nz,p,ja,ja)
      do 5 i = 1,n
 5    wksp(i) = a(i)
      call vscatr (n,wksp,p,a)
      do 10 i = 1,n 
         ia(i) = i
         ja(i) = i
 10   continue
c
c ... convert to upper triangular elements for symmetric storage
c
      if (isym .eq. 1) return 
      np1 = n + 1
      do 15 i = np1,nz
         if (ia(i) .le. ja(i)) go to 15 
         idum = ia(i)
         ia(i) = ja(i)
         ja(i) = idum
 15   continue
      return
      end 
      subroutine permat (ndim,maxnz,coef,jcoef,wksp,iwksp,nn,p)
      implicit double precision (a-h, o-z)
c
c ... permat takes the sparse matrix representation of the
c     matrix and permutes both rows and columns, overwriting
c     the previous structure.  (purdue data structure)
c
c ... parameters -- 
c
c          ndim      row dimension of coef array in defining routine
c          maxnz     number of columns in coef and jcoef arrays
c          coef      array of matrix coefficients 
c          jcoef     array of matrix columns numbers
c          wksp      workspace array of length n
c          iwksp     integer workspace array of length n
c          n         order of system (= nn)
c          p         permutation vector 
c
c ... it is assumed that the i-th entry of the permutation vector
c     p indicates the row the i-th row gets mapped into.  (i.e.
c     if ( p(i) = j ) row i gets mapped into row j)
c
c ... specifications for parameters
c
      dimension coef(ndim,1), wksp(1)
      integer   jcoef(ndim,1), iwksp(1), p(1)
c
      n = nn
      if (n .le. 0) return
      do 20 j = 1,maxnz
         do 10 i = 1,n
            wksp(i) = coef(i,j)
            iwksp(i) = jcoef(i,j)
 10      continue
         call vscatr (n,wksp,p,coef(1,j))
         call vscati (n,iwksp,p,jcoef(1,j))
         call vgathi (n,p,jcoef(1,j),jcoef(1,j))
 20   continue
      return
      end 
      subroutine perror1 (suba,coef,jcoef,wfac,jwfac,nn,u,rhs,
     a                   wksp,digtt1,digtt2,idgtts)
      implicit double precision (a-h, o-z)
c
c     perror1 computes the residual, r = rhs - a*u.  the user
c     also has the option of printing the residual and/or the
c     unknown vector depending on idgts.
c
c ... parameters -- 
c
c          suba   matrix-vector multiplication routine
c          n      dimension of matrix (= nn)
c          u      latest estimate of solution
c          rhs    right hand side of matrix problem
c          wksp   workspace vector of length n
c          digit1 output - measure of accuracy of stopping test (= digtt1
c          digit2 output - measure of accuracy of solution (= digtt2) 
c          idgts   parameter controlling level of output (= idgtts)
c                    if idgts < 1 or idgts > 4, then no output.
c                            = 1, then number of digits is printed, pro-
c                                 vided level .ge. 1
c                            = 2, then solution vector is printed, pro-
c                                 vided level .ge. 1
c                            = 3, then residual vector is printed, pro-
c                                 vided level .ge. 1
c                            = 4, then both vectors are printed, pro- 
c                                 vided level .ge. 1
c
c ... specifications for parameters
c
      external suba 
      dimension rhs(1), u(1), wksp(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      logical           halt, maxadp, minadp, maxadd, minadd
      common / itcom2 / halt, maxadp, minadp, maxadd, minadd
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      idgts = idgtts
      digit1 = 0.0d0
      digit2 = 0.0d0
c
      digit1 = -dlog10 (abs (srelpr))
      if (stptst .gt. 0.0d0) digit1 = -dlog10 (abs (stptst))
         call suba (coef,jcoef,wfac,jwfac,n,u,wksp)
         call vexopy (n,wksp,rhs,wksp,2)
         rnrm = sqrt ( vdot (n,wksp,wksp) )
         sum = vdot (n,rhs,rhs)
         bnorm = max ( sqrt(sum),srelpr )
         temp = rnrm/bnorm
         if (temp .eq. 0.0d0) go to 10
         digit2 = -dlog10 (abs (temp))
         go to 15
c
 10   digit2 = -dlog10 (abs (srelpr))
c
 15      if ((idgts .lt. 1) .or. (level .le. 0)) go to 25
         write (nout,20) digit1,digit2
 20      format (/10x,'approx. no. of digits in stopping test =',
     a          f5.1,2x,'(digit1)'
     b          /10x,'approx. no. of digits in ratio test    =',
     c          f5.1,2x,'(digit2)')
c
         if (idgts .le. 1 .or. idgts .gt. 4) go to 25
         if (idgts .ge. 3) call out (n,wksp,1,nout)
         if (idgts .ne. 3) call out (n,u,2,nout)
c
 25   continue
      digtt1 = digit1
      digtt2 = digit2
      return
      end 
      subroutine pervec (nn,p,v,wksp)
      implicit double precision (a-h, o-z)
c
c ... pervec permutes a vector as dictated by the permutation
c ... vector p.  if p(i) = j, then v(j) gets v(i).
c
c ... parameters -- 
c
c          n       length of vectors p, v, and wksp  (= nn) 
c          p       integer permutation vector
c          v       vector to be permuted
c          wksp    workspace vector of length n
c
c ... specifications for parameters
c
      integer   p(1)
      dimension v(1), wksp(1) 
c
      n = nn
      if (n .le. 0) return
      do 10 i = 1,n 
         wksp(i) = v(i)
 10   continue
      call vscatr (n,wksp,p,v)
      return
      end 
      subroutine pgen (nn,p,ip,nc,ncolor)
      implicit double precision (a-h, o-z)
c
c ... pgen constructs the permutation vector p and its inverse
c ... ip for a coloring given by p.
c
c ... parameters -- 
c
c         n         order of system (= nn)
c         p         vector from prbndx upon input 
c                   permutation vector upon output
c         ip        integer workspace vector upon input
c                   inverse permutation vector upon output
c         nc        number of points for each color (output)
c         ncolor    number of colors
c
c ... specifications for parameters
c
      integer p(1), ip(1), nc(1)
c
      n = nn
c
c ... determine number of colors and number of elements for each
c     color.
c
      ncolor = 0
      do 5 i = 1,n
 5    nc(i) = 0
      do 10 i = 1,n 
         ic = p(i)
         if (ncolor .lt. ic) ncolor = ic
         nc(ic) = nc(ic) + 1
 10   continue
c
c ... construct permutation vector.
c
      ip(1) = 1
      do 15 i = 2,ncolor
         ip(i) = ip(i-1) + nc(i-1)
 15   continue
      do 20 i = 1,n 
         ic = p(i)
         p(i) = ip(ic)
         ip(ic) = ip(ic) + 1
 20   continue
c
c ... construct inverse permutation vector.
c
      do 25 i = 1,n 
         j = p(i)
         ip(j) = i
 25   continue
      return
      end 
      subroutine pjac (diag,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... pjac does the point jacobi preconditioning. 
c
c ... parameters -- 
c
c         diag    vector of length n containing the diagonal
c                  elements of the coefficient matrix
c         n       order of system (= nn)
c         r       residual
c         z       output vector
c
c ... specifications for parameters
c
      dimension r(1), z(1), diag(1)
c
      n = nn
      do 10 i = 1,n 
 10   z(i) = r(i)/diag(i)
      return
      end 
      subroutine pmdg (ndim,mdim,nn,maxnz,jcoef,coef,ncol,nc,p,ip,
     a                 maxd,maxnew,jcnew,wksp,iwksp,isym,ier)
      implicit double precision (a-h, o-z)
c
c ... pmdg permutes the matrix according to index vector p, 
c     and, if room allows, stores the permuted matrix in
c     diagonal format.  there will be enough room if the number
c     of diagonals needed does not exceed mdim.
c
c ... parameters -- 
c
c        ndim      row dimension of coef and jcoef arrays
c                   in defining routine 
c        mdim      column dimension of coef and jcoef arrays in
c                   defining routine
c        n         order of system (active row size of coef and jcoef)
c        maxnz     active column size of coef and jcoef
c        jcoef     integer array of column numbers
c        coef      floating point array of coefficients
c        ncolor    number of colors in the permutation (= ncol)
c        nc        integer vector of length ncolor giving the
c                   number of nodes for each color
c        p         permutation vector
c        ip        inverse permuation vector
c        maxd      active columns in permuted matrix
c        maxnew    integer vector giving the number of diagonals
c                   created for each color
c        jcnew     integer array of size ncolor*max(maxnew(i))
c                   giving the diagonal numbers for each color
c        wksp      floating point workspace of length n
c        iwksp     integer workspace of length 2*n
c        isym      symmetric storage switch
c                   = 0    symmetric storage
c                   = 1    nonsymmetric storage
c        ier       error flag 
c                  =  0   no errors detected
c                  = -9   mdim is less than the number of columns
c                          needed in coef to store the permuted
c                          matrix in diagonal format
c
c ... specifications for parameters
c
      integer jcoef(2), nc(1), p(1), maxnew(1), jcnew(ncol,1),
     a        iwksp(1), ip(1) 
      dimension coef(ndim,1), wksp(1)
c
c
      n = nn
      ncolor = ncol 
c
c ... fill out rest of matrix if symmetric storage is used. 
c
      if (isym .ne. 0) go to 2
      maxd = 2*maxnz - 1
      if (mdim .lt. maxd) ier = -9
      if (ier .lt. 0) return
c
      do 1 j = 2,maxnz
         ind = jcoef(j)
         len = n - ind
         jcol = maxnz + j - 1 
         jcoef(jcol) = -ind
         call vfill (ind,coef(1,jcol),0.0d0)
         call vcopy (len,coef(1,j),coef(ind+1,jcol))
 1    continue
      maxnz = maxd
c
c ... determine the number of created diagonals.
c
 2    do 5 i = 1,ncolor
         maxnew(i) = 1
         jcnew(i,1) = 0
 5    continue
      do 35 j = 2,maxnz
         ind = jcoef(j)
         do 10 i = 1,n
            iwksp(n+i) = i + ind
            if (coef(i,j) .eq. 0.0d0) iwksp(n+i) = i
 10      continue
         call vscati (n,iwksp(n+1),p,iwksp)
         call vgathi (n,p,iwksp,iwksp)
         do 15 i = 1,n
 15      iwksp(i) = iwksp(i) - i
         ist = 1
         do 30 k = 1,ncolor
            ncc = nc(k)
            ied = ist + ncc - 1
            lim = maxnew(k)
            do 25 i = ist,ied 
               id = iwksp(i)
               do 20 jj = 1,lim
                  if (jcnew(k,jj) .eq. id) go to 25
 20            continue
               lim = lim + 1
               maxnew(k) = lim
               if (lim .gt. mdim) go to 40
               jcnew(k,lim) = id
 25         continue
            ist = ist + ncc
 30      continue
 35   continue
c
c ... determine maxd.
c
 40   maxd = -1
      do 45 k = 1,ncolor
 45   maxd = max (maxd,maxnew(k))
      if (mdim .lt. maxd) ier = -9
      if (ier .lt. 0) return
c
c ... permute matrix.
c
      do 55 j = 1,maxnz
         do 50 i = 1,n
 50      wksp(i) = coef(i,j)
         call vscatr (n,wksp,p,coef(1,j))
 55   continue
c
c ... rearrange rows.
c
      ist = 1
      do 85 k = 1,ncolor
         ncc = nc(k)
         ied = ist + ncc - 1
         lim = maxnew(k)
         do 62 l = 1,lim
            jcol = jcnew(k,l) 
            iwksp(n+jcol) = l 
 62      continue
         do 80 i = ist,ied
            iip = ip(i)
            do 60 j = 2,maxnz 
 60         wksp(j) = coef(i,j)
            do 63 j = 2,maxd
 63         coef(i,j) = 0.0d0
            do 75 j = 2,maxnz 
               if (wksp(j) .eq. 0.0d0) go to 75
               icol = p(iip + jcoef(j)) - i
               l = iwksp(n+icol)
               coef(i,l) = wksp(j)
 75         continue
 80      continue
         ist = ist + ncc
 85   continue
      return
      end 
      subroutine prbndx (nn,ndim,maxnzz,jcoef,coef,p,ip,propa,nstore) 
      implicit double precision (a-h, o-z)
c
c**************************************************************
c
c     (purdue, diagonal data structures)
c     prbndx determines if the matrix has property a.
c     this algorithm assumes all neighbors of a particular node
c     are known.
c
c     the algorithm is to mark the first node as red (arbitrary).
c     all of its adjacent nodes are marked black and placed in
c     a stack.  the remainder of the code pulls the first node
c     off the top of the stack and tries to type its adjacent nodes.
c     the typing of the adjacent point is a five way case statement
c     which is well commented below (see do loop 50).
c
c     the array p is used both to keep track of the color of a node
c     (red node is positive, black is negative) but also the father
c     node that caused the color marking of that point.  since
c     complete information on the adjacency structure is hard to come 
c     by, this forms a link to enable the color change of a partial
c     tree when a recoverable color conflict occurs.
c
c     the array ip is used as a stack to point to the set of nodes
c     left to be typed that are known to be adjacent to the current
c     father node.
c
c
c*********************************************************************
c
c ... input parameters --
c
c        n      number of nodes.  (integer, scalar) (= nn)
c        ndim   row dimension of coef array
c        maxnz  maximum number of nonzeros per row
c        jcoef  integer data array
c        coef   floating point data array
c        p,ip   integer workspace vectors of length n
c        nstore data structure switch
c                = 1  purdue
c                = 2  diagonal (symmetric or nonsymmetric)
c
c ... output parameters --
c
c        p      contains information for constructing the permutation 
c               array upon output
c        propa  a logical variable which is set to .true. if the
c               matrix has property a and .false. otherwise 
c
c
c******************************************************************** 
c
c ... specifications for parameters
c
      integer   p(1), ip(1), jcoef(ndim,1)
      dimension coef(ndim,1)
      logical   propa
c
c
c ... specifications for local variables
c
      integer   first, old, young, curtyp, type
c
c-----------------------------------------------------------------------
c
         n = nn
         maxnz = maxnzz
         do 5 i = 1,n
            p(i) = 0
            ip(i) = 0
 5       continue
c
c ... handle the first set of points until some adjacent points
c ... are found
c
         first = 1
c
 10      p(first) = first
         if (maxnz .gt. 1) go to 20
c
c ... search for next entry that has not been marked
c
         if (first .eq. n) go to 65
         ibgn = first + 1
         do 15 i = ibgn,n
            if (p(i) .ne. 0) go to 15
            first = i
            go to 10
 15      continue
         go to 65
c
c ... first set of adjacent points found
c
 20      next = 1
         last = 1
         ip(1) = first
c
c ... loop over labeled points indicated in the stack stored in
c ... the array ip
c
 25      k = ip(next)
         curtyp = p(k)
         nxttyp = -curtyp
         if (maxnz .le. 0) go to 55
         do 50 j = 1,maxnz
            if (nstore .eq. 1) jcol = jcoef(k,j)
            if (nstore .ge. 2) jcol = k + jcoef(j,1)
c
c ... determine if element (k,j) is a diagonal element or zero.
c
            if (jcol .lt. 1 .or. jcol .gt. n .or. jcol .eq. k)
     a                                    go to 50
            if (coef(k,j) .eq. 0.0d0) go to 50
c
            type = p(jcol)
c
c==================================================================
c
c     the following is a five way case statement dealing with the
c     labeling of the adjacent node.
c
c ... case i.  if the adjacent node has already been labeled with
c              label equal to nxttyp, then skip to the next adjacent
c              node.
c
         if (type .eq. nxttyp) go to 50 
c
c ... case ii.  if the adjacent node has not been labeled yet label
c               it with nxttyp and enter it in the stack
c
         if (type .ne. 0) go to 30
            last = last + 1
            ip(last) = jcol
            p(jcol) = nxttyp
            go to 50
c
c ... case iii.  if the adjacent node has already been labeled with
c                opposite color and the same father seed, then there
c                is an irrecoverable color conflict.
c
 30      if (type .eq. curtyp) go to 999
c
c ... case iv.  if the adjacent node has the right color and a different
c               father node, then change all nodes of the youngest fathe
c               node to point to the oldest father seed and retain the
c               same colors.
c
         if (type * nxttyp .lt. 1) go to  40
            old   = min ( iabs(type), iabs(nxttyp) )
            young = max ( iabs(type), iabs(nxttyp) )
            do  35 l = young,n
               if (iabs(p(l)) .eq. young) p(l) = isign(old, p(l))
 35         continue
            curtyp = p(k)
            nxttyp = -curtyp
            go to 50
c
c ... case v.  if the adjacent node has the wrong color and a different
c              father node, then change all nodes of the youngest father
c              node to point to the oldest father node along with
c              changing their colors.  since until this time the
c              youngest father node tree has been independent no other
c              color conflicts will arise from this change. 
c
 40      old   = min ( iabs(type), iabs(nxttyp) )
         young = max ( iabs(type), iabs(nxttyp) )
         do  45 l = young,n
            if (iabs(p(l)) .eq. young) p(l) = isign(old, -p(l))
 45      continue
         curtyp = p(k)
         nxttyp = -curtyp
c
c
c ... end of case statement
c
c==================================================================
 50      continue
c
c ... advance to next node in the stack 
c
 55      next = next + 1
         if (next .le. last) go to 25
c
c ... all nodes in the stack have been removed
c
c ... check for nodes not labeled.  if any are found
c ... start the labeling process again at the first
c ... node found that is not labeled.
c
         ibgn = first + 1
         do 60 i = ibgn,n
            if (p(i) .ne. 0) go to 60
               first = i
               go to 10
 60      continue
c
c
c===================================================================
c
c
c ... all nodes are now typed either red or black.
c ... red-black ordering possible.
c
 65      propa = .true.
         do 70 i = 1,n
            if (p(i) .ge. 0) p(i) = 1
            if (p(i) .le. 0) p(i) = 2
 70      continue
         return
c
c ...... type conflict
c
 999  propa = .false.
      return
      end 
      subroutine prbblk (ncol,ndis,iblock,lbhb,p,ip,propa)
      implicit double precision (a-h, o-z)
c
c**************************************************************
c
c     (block structure)
c     prbblk determines if the matrix has block property a. 
c     see routine prbndx for an explanation of the algorithm
c
c**************************************************************
c
c ... input parameters --
c
c         ncolor   number of diagonal blocks
c         ndis     number of distinct diagonal blocks
c         iblock   integer array of size 3 by ndis by max(lbhb(i))
c                   giving block constants
c         lbhb     integer vector of size ndis giving the number
c                   of diagonal blocks for each distinct block size.
c         p,ip     integer workspace vectors of length ncolor
c
c ... output parameters --
c
c        p      contains information for constructing the permutation 
c               array upon output
c        propa  a logical variable which is set to .true. if the
c               matrix has block property a and .false. otherwise
c
c
c******************************************************************** 
c
c ... specifications for parameters
c
      integer   p(1), ip(1), iblock(3,ndis,1), lbhb(1)
      logical   propa
c
c ... specifications for local variables
c
      integer   first, old, young, curtyp, type
c
c
         ncolor = ncol
         ndist = ndis
         index = 1
         do 5 i = 1,ncolor
            p(i) = 0
            ip(i) = 0
 5       continue
c
c ... handle the first set of points until some adjacent points
c ... are found
c
         first = 1
c
 10      p(first) = first
         if (ndist .gt. 1) index = first
         maxnz = lbhb(index)
         if (maxnz .gt. 1) go to 20
c
c ... search for next entry that has not been marked
c
         if (first .eq. ncolor) go to 65
         do 15 i = first+1,ncolor
            if (p(i) .ne. 0) go to 15
            first = i
            go to 10
 15      continue
         go to 65
c
c ... first set of adjacent points found
c
 20      next = 1
         last = 1
         ip(1) = first
c
c ... loop over labeled points indicated in the stack stored in
c ... the array ip
c
 25      k = ip(next)
         curtyp = p(k)
         nxttyp = -curtyp
         if (ndist .gt. 1) index = k
         maxnz = lbhb(index)
         if (maxnz .le. 0) go to 55
         do 50 j = 1,maxnz
            jcol = k + iblock(1,index,j)
c
c ... determine if element (k,j) is a diagonal element or zero.
c
            if (jcol .lt. 1 .or. jcol .gt. ncolor .or.
     a          jcol .eq. k) go to 50
            if (iblock(3,index,j) .eq. 0) go to 50
c
            type = p(jcol)
c
c==================================================================
c
c     the following is a five way case statement dealing with the
c     labeling of the adjacent node.
c
c ... case i.  if the adjacent node has already been labeled with
c              label equal to nxttyp, then skip to the next adjacent
c              node.
c
         if (type .eq. nxttyp) go to 50 
c
c ... case ii.  if the adjacent node has not been labeled yet label
c               it with nxttyp and enter it in the stack
c
         if (type .ne. 0) go to 30
            last = last + 1
            ip(last) = jcol
            p(jcol) = nxttyp
            go to 50
c
c ... case iii.  if the adjacent node has already been labeled with
c                opposite color and the same father seed, then there
c                is an irrecoverable color conflict.
c
 30      if (type .eq. curtyp) go to 999
c
c ... case iv.  if the adjacent node has the right color and a different
c               father node, then change all nodes of the youngest fathe
c               node to point to the oldest father seed and retain the
c               same colors.
c
         if (type * nxttyp .lt. 1) go to  40
            old   = min ( iabs(type), iabs(nxttyp) )
            young = max ( iabs(type), iabs(nxttyp) )
            do  35 l = young,ncolor
               if (iabs(p(l)) .eq. young) p(l) = isign(old, p(l))
 35         continue
            curtyp = p(k)
            nxttyp = -curtyp
            go to 50
c
c ... case v.  if the adjacent node has the wrong color and a different
c              father node, then change all nodes of the youngest father
c              node to point to the oldest father node along with
c              changing their colors.  since until this time the
c              youngest father node tree has been independent no other
c              color conflicts will arise from this change. 
c
 40      old   = min ( iabs(type), iabs(nxttyp) )
         young = max ( iabs(type), iabs(nxttyp) )
         do  45 l = young,ncolor
            if (iabs(p(l)) .eq. young) p(l) = isign(old, -p(l))
 45      continue
         curtyp = p(k)
         nxttyp = -curtyp
c
c
c ... end of case statement
c
c==================================================================
 50      continue
c
c ... advance to next node in the stack 
c
 55      next = next + 1
         if (next .le. last) go to 25
c
c ... all nodes in the stack have been removed
c
c ... check for nodes not labeled.  if any are found
c ... start the labeling process again at the first
c ... node found that is not labeled.
c
         do 60 i = first+1,ncolor
            if (p(i) .ne. 0) go to 60
               first = i
               go to 10
 60      continue
c
c
c===================================================================
c
c
c ... all nodes are now typed either red or black.
c ... red-black ordering possible.
c
 65      propa = .true.
         do 70 i = 1,ncolor
            if (p(i) .ge. 0) p(i) = 1
            if (p(i) .le. 0) p(i) = 2
 70      continue
         return
c
c ...... type conflict
c
 999  propa = .false.
      return
      end 
      subroutine prep1 (nn,ndim,maxnzz,jcoef,coef,ier)
      implicit double precision (a-h, o-z)
c
c ... prep1 puts the diagonal elements of the matrix in column one
c     of coef  (purdue data structure)
c
c ... parameters -- 
c
c         n       dimension of matrix ( = nn)
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array (= maxnzz)
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         ier     error flag -- on return, values mean
c                      0 -- no errors detected
c                     -5 -- nonexistent diagonal element
c
c ... specifications for parameters
c
      integer   jcoef(ndim,1) 
      dimension coef(ndim,1)
c
      n = nn
      maxnz = maxnzz
c
      do 20 i = 1,n 
         do 10 j = 1,maxnz
            if (jcoef(i,j) .eq. i) go to 15
 10      continue
c
c ... no diagonal entry for row i.
c
         ier = -5
         return
c
c ... switch entries so that diagonal element is in column 1.
c
 15      if (j .eq. 1) go to 20
         save = coef(i,j)
         coef(i,j) = coef(i,1)
         jcoef(i,j) = jcoef(i,1)
         coef(i,1) = save
         jcoef(i,1) = i
 20   continue
      return
      end 
      subroutine prep2 (nn,ndim,maxnzz,jcoef,coef,wksp,ier) 
      implicit double precision (a-h, o-z)
c
c ... prep2 puts the diagonal entries of the matrix into column
c     one of coef.  (diagonal data structure)
c
c ... parameters -- 
c
c         n       dimension of matrix ( = nn)
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array (= maxnzz)
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         wksp    workspace array of size n
c         ier     error flag -- on return, values mean
c                      0 -- no errors detected
c                     -5 -- nonexistent diagonal element
c
c ... specifications for parameters
c
      integer   jcoef(2)
      dimension coef(ndim,1), wksp(1)
c
      n = nn
      maxnz = maxnzz
c
      do 10 j = 1,maxnz
         if (jcoef(j) .eq. 0) go to 15
 10   continue
c
c ... no main diagonal.
c
      ier = -5
      return
c
c ... switch diagonals so that main diagonal is in column 1.
c
 15   if (j .eq. 1) return
      do 20 i = 1,n 
         wksp(i) = coef(i,1)
         coef(i,1) = coef(i,j)
         coef(i,j) = wksp(i)
 20   continue
      jcoef(j) = jcoef(1)
      jcoef(1) = 0
      return
      end 
      subroutine prep3 (n,nz,ia,ja,a,m,np,iwksp)
      implicit double precision (a-h, o-z)
c
c ... prep3 puts the diagonal elements of the matrix in the 
c     first n locations of the data structure, adds duplicate
c     triples, and defines the partition for matrix-vector
c     products.
c
c ... parameters -- 
c
c         n       number of equations
c         nz      length of ia, ja, and a vectors 
c         ia      vector of i values
c         ja      vector of j values
c         a       vector of matrix coefficients
c         m       number of partitions (output)
c         np      on output, np contains the partition pointers.
c                  it must be at least m+1 in length.
c         iwksp   integer workspace vector of length n
c
c ... specifications for parameters
c
      integer ia(1), ja(1), iwksp(1), np(1)
      dimension a(1)
c
c ... eliminate duplicates from the vectors by adding their 
c     values in the a vector.  first, sort the vectors by
c     rows first and then by columns within each row.
c
      call vsrta1 (nz,ia,ja,a)
c
c ... add duplicates.
c
      l = 1
      do 10 k = 2,nz
         i = ia(k)
         j = ja(k)
         aval = a(k)
         if (i .eq. ia(l) .and. j .eq. ja(l)) go to 5
         l = l + 1
         ia(l) = i
         ja(l) = j
         a(l) = aval
         go to 10
 5       a(l) = a(l) + aval
 10   continue
      nz = l
c
c ... put main diagonal elements first. 
c
      do 20 k = 1,nz
 15      i = ia(k)
         j = ja(k)
         if (i .ne. j) go to 20
         if (i .eq. k) go to 20
         val = a(k) 
         ia(k) = ia(i)
         ja(k) = ja(i)
         a(k) = a(i)
         ia(i) = i
         ja(i) = i
         a(i) = val 
         go to 15
 20   continue
c
c ... define partitions.
c
      kbgn = n + 1
      krep = kbgn
      mm = 1
      np(1) = 1
 25   mm = mm + 1
      np(mm) = kbgn 
      do 30 i = 1,n 
 30   iwksp(i) = 0
      nval = 0
      if (kbgn .gt. nz) go to 50
      do 40 k = kbgn,nz
         i = ia(k)
         j = ja(k)
         if (iwksp(i) .eq. 1 .or. iwksp(i) .eq. 3 .or.
     a       iwksp(j) .ge. 2) go to 40
         nval = nval + 1
         iwksp(i) = iwksp(i) + 1
         iwksp(j) = iwksp(j) + 2
         if (k .eq. krep) go to 35
         at = a(krep)
         it = ia(krep)
         jt = ja(krep)
         a(krep) = a(k)
         ia(krep) = i
         ja(krep) = j
         a(k) = at
         ia(k) = it 
         ja(k) = jt 
 35      krep = krep + 1
         if (nval .ge. n) go to 45
 40   continue
 45   kbgn = krep
      go to 25
 50   m = mm - 1
      return
      end 
      subroutine prich (nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... prich does the richardson preconditioning.
c
c ... parameters -- 
c
c         n       order of system (= nn)
c         r       residual
c         z       output vector
c
c ... specifications for parameters
c
      dimension r(1), z(1)
c
      n = nn
      do 10 i = 1,n 
 10   z(i) = r(i)
      return
      end 
      subroutine pstops (nn,r,z,u,ubar,ier)
      implicit double precision (a-h, o-z)
c
c ... pstops performs a test to see if the iterative method has
c     converged to a solution inside the error tolerance, zeta.
c     (cg and si routines)
c
c     the stopping tests are --
c
c    (1)  (emax/emin) * sqrt ( (r ,zt)/(rhs,inv(q)*rhs) )
c    (2)  ( 1.0/emin) * sqrt ( (zt,zt)/(u,u) )
c    (3)  (emax/emin) * sqrt ( (zt,zt)/(inv(q)*rhs,inv(q)*rhs) )
c    (4)                sqrt ( (zt,zt)/(inv(q)*rhs,inv(q)*rhs) )
c    (5)                sqrt ( (r ,r )/(rhs,rhs) )
c    (6)                sqrt ( (u-ubar,u-ubar)/(ubar,ubar) )
c    (7)  (emax/emin) * sqrt ( (r,z)/(rhs,inv(ql)*rhs) )
c    (8)  ( 1.0/emin) * sqrt ( (z,z)/(u,u) )
c    (9)  (emax/emin) * sqrt ( (z,z)/(inv(ql)*rhs,inv(ql)*rhs) )
c   (10)                sqrt ( (z,z)/(inv(ql)*rhs,inv(ql)*rhs) )
c
c
c ... parameters -- 
c
c         n       order of system
c         r       residual vector
c         z       pseudo-residual vector
c         u       solution estimate
c         ier     error flag
c                  =  0   no errors detected
c                  = -7   splitting matrix is not positive definite
c
c ... specifications for parameters
c
      dimension r(1), z(1), u(1), ubar(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      logical           halt, maxadp, minadp, maxadd, minadd
      common / itcom2 / halt, maxadp, minadp, maxadd, minadd
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      common / itcom4 / srelpr, keyzer, keygs
      common / itcom9 / rdot, rzdot, rztdot, zdot, zztdot, ztdot,
     a           rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a           udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
      logical rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a        udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
c
c *** end   -- package common 
c
      logical q1
      save    q1
      n = nn
      halt = .false.
      tiny = 500.0d0*srelpr
      nteste = ntest
      if (ntest .gt. 6) nteste = nteste - 6
c
      go to (10,20,30,40,50,60), nteste 
c
c ... test 1
c
 10   if (rzdot .ge. 0.0d0) go to 15
      ier = -7
      call ershow (ier,'pstops')
      return
 15   emaxl = emax
      eminl = emin
      if (eminl .lt. tiny) eminl = tiny 
      tl = emaxl*sqrt (rzdot) 
      tr = eminl*bnorm1
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
c
c ... test 2
c
c ... special procedure for zeroth iteration
c
 20   if (in .ge. 1) go to 25 
      q1 = .false.
      udnm = 1.0d0
      stptst = sqrt (rzdot)
      if (stptst .lt. tiny) halt = .true.
      return
c
c ... in .ge. 1
c
c ... test if udnm needs to be recomputed.
c
 25   if (q1) go to 28
      if ((in .gt. 5)  .and.  (mod(in,5) .ne. 0)) go to 28
         uold = udnm
         udnm = sqrt ( vdot (n,u,u) )
         if (udnm .lt. tiny) udnm = 1.0d0 
         if ((in .gt. 5)  .and.
     a       (abs (udnm-uold) .lt. udnm*zeta)) q1 = .true.
c
c ... compute stopping test.
c
 28   eminl = emin
      if (eminl .lt. tiny) eminl = tiny 
      tl = sqrt ( vdot (n,z,z) )
      tr = udnm*eminl
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
c
c ... test 3.
c
 30   emaxl = emax
      eminl = emin
      if (eminl .lt. tiny) eminl = tiny 
      tl = emaxl*sqrt ( vdot (n,z,z) )
      tr = eminl*bnorm1
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
c
c ... test 4.
c
 40   tl = sqrt ( vdot (n,z,z) )
      tr = bnorm1
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
c
c ... test 5.
c
 50   tl = sqrt ( vdot (n,r,r) )
      tr = bnorm
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
c
c ... test 6.
c
 60   sum = 0.0d0
      do 65 i = 1,n 
 65   sum = sum + (u(i) - ubar(i))**2
      tl = sqrt (sum)
      tr = ubarnm
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
      end 
      subroutine rowise (maxnz,jcoef,irwise)
      implicit double precision (a-h, o-z)
c
c ... rowise determines whether a row-wise or diagonal-wise 
c     algorithm should be used for ic and ssor splittings with
c     diagonal storage.  this routine should be called after
c     final factorization is computed.
c
c ... parameters -- 
c
c          maxnz  number of number of diagonals stored
c          jcoef  vector of diagonal numbers for factorization
c                  array or matrix
c          irwise has a value upon output of
c                  0   if diagonal-wise algorithm should be used
c                  1   if row-wise algorithm should be used 
c
c ... specifications for parameters
c
      integer   jcoef(2)
c
c ... use a rowwise algorithm if  2 .le. /jcoef(j)/ .le. maxd
c     some j.
c
      maxd = 10
c
      irwise = 0
      do 15 j = 1,maxnz
         jcol = iabs(jcoef(j))
         if (jcol .le. 1 .or. jcol .gt. maxd) go to 15
         irwise = 1 
         return
 15   continue
      return
      end 
      subroutine rowsum (lda,n,maxnzz,a,x,isym)
      implicit double precision (a-h, o-z)
c
c ... rowsum computes the row sum of the matrix a.
c
c ... parameters -- 
c
c        lda     leading dimension of array a
c        n       active size of array a 
c        maxnz   number of columns in array a
c        a       array of size n by maxnz
c        x       vector of length n containing the row
c                 sum of a upon output
c        isym    symmetry switch
c                 = 0  matrix is a banded symmetric matrix
c                       with the diagonal in column one
c                 = 1  matrix is nonsymmetric
c
c ... specifications for parameters
c
      dimension a(lda,1), x(1)
c
      maxnz = maxnzz
      do 10 i = 1,n 
 10   x(i) = 0.0d0
      do 20 j = 1,maxnz
         do 15 i = 1,n
 15      x(i) = x(i) + a(i,j) 
 20   continue
      if (isym .eq. 1 .or. maxnz .le. 1) return
      do 30 j = 2,maxnz
         do 25 i = j,n
 25      x(i) = x(i) + a(i-j+1,j)
 30   continue
      return
      end 
      subroutine rsad (nn,nsize,nrr,ndim,maxnew,ndtt,ndbb,jcnew,
     a                 coef,c,b,dfac,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsad computes  c = (dr - t*inv(db)*b)*b
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     diagonal storage
c
c ... parameters -- 
c
c        n          order of system
c        nsize      size of an individual subsystem (if multiple
c                    systems) 
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnew     number of columns in coef array
c        ndt        number of upper diagonals in diagonal block
c        ndb        number of lower diagonals in diagonal block
c        coef       floating point data structure 
c        b          vector of length n containing bb behind br
c        c          vector of length nr containing cr
c        dfac       vector of length (1+nt+nb)*n to contain 
c                    factorization of diagonal block upon output
c        wksp       workspace vector of length nb 
c
c ... specifications for parameters
c
      integer   jcnew(2,1), maxnew(2)
      dimension coef(ndim,2), b(1), c(1), dfac(1), wksp(1)
c
      n = nn
      nr = nrr
      ndt = ndtt
      ndb = ndbb
      nrp1 = nr + 1 
      nb = n - nr
      maxd = 1 + ndt + ndb
      maxz = maxnew(1) - maxd 
      max2 = maxnew(2) - maxd 
c
c ... cr = dr*br.
c
      if (ndt+ndb .gt. 0) go to 15
      do 10 i = 1,nr
 10   c(i) = coef(i,1)*b(i)
      go to 20
 15   call bmuln (ndim,nr,ndt,ndb,coef,coef(1,2),coef(1,ndt+2),b,c)
c
c ... wksp = b*br
c
 20   if (maxz*max2 .eq. 0) return
      do 25 i = 1,nb
 25   wksp(i) = 0.0d0 
      call vaddd (ndim,2,nb,nr,max2,coef(nrp1,maxd+1),
     a            jcnew(2,maxd+1),wksp,b,-nr)
c
c ... wksp = inv(db)*wksp
c
      if (ndt+ndb .gt. 0) go to 35
      do 30 i = 1,nb
 30   wksp(i) = wksp(i)*dfac(i+nr)
      go to 40
 35   call bdsol (n,nb,nsize,ndt,ndb,dfac(nrp1),wksp,wksp,1)
c
c ... cr = cr - t*wksp
c
 40   call vsubd (ndim,2,nr,nb,maxz,coef(1,maxd+1),jcnew(1,maxd+1),
     a            c,wksp,nr)
      return
      end 
      subroutine rsap (ndimm,n,nr,maxnz,jcoef,coef,b,c,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsap computes  c = (dr - t*inv(db)*b)*b
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     purdue format 
c
c ... parameters -- 
c
c        ndim       row dimension of coef,jcoef arrays
c        n          order of total system
c        nr         order of red subsystem
c        maxnz      number of columns in coef,jcoef arrays
c        jcoef      integer array of matrix column numbers
c        coef       floating point array of matrix coefficients
c        b,c        vectors of length nr
c        wksp       workspace array of length n + nb
c
c ... specifications for parameters
c
      integer   jcoef(ndimm,2)
      dimension coef(ndimm,2), b(1), c(1), wksp(1)
c
      ndim = ndimm
      do 10 i = 1,nr
 10   c(i) = coef(i,1)*b(i)
      if (maxnz .le. 1) return
      np1 = n + 1
      nb = n - nr
      nrp1 = nr + 1 
      maxm1 = maxnz - 1
      do 15 i = 1,n 
 15   wksp(i) = 0.0d0 
      call vaddp (ndim,ndim,nb,maxm1,coef(nrp1,2),jcoef(nrp1,2),
     a            wksp(nrp1),b,wksp(np1))
      do 20 i = nrp1,n
 20   wksp(i) = wksp(i)/coef(i,1)
      call vsubp (ndim,ndim,nr,maxm1,coef(1,2),jcoef(1,2),c,wksp,wksp)
      return
      end 
      subroutine rsatd (nn,nsize,nrr,ndim,maxnew,ndtt,ndbb,jcnew,
     a                  coef,c,b,dfac,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsatd computes  c = ((dr**t) - (b**t)*(db**(-t))*(t**t))*b
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     diagonal storage
c
c ... parameters -- 
c
c        n          order of system
c        nsize      size of an individual subsystem (if multiple
c                    systems) 
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnew     number of columns in coef array
c        ndt        number of upper diagonals in diagonal block
c        ndb        number of lower diagonals in diagonal block
c        coef       floating point data structure 
c        b          vector of length n containing bb behind br
c        c          vector of length nr containing cr
c        dfac       vector of length (1+nt+nb)*n to contain 
c                    factorization of diagonal block upon output
c        wksp       workspace vector of length nb 
c
c ... specifications for parameters
c
      integer   jcnew(2,1), maxnew(2)
      dimension coef(ndim,2), b(1), c(1), dfac(1), wksp(1)
c
      n = nn
      nr = nrr
      ndt = ndtt
      ndb = ndbb
      nrp1 = nr + 1 
      nb = n - nr
      maxd = 1 + ndt + ndb
      maxz = maxnew(1) - maxd 
      max2 = maxnew(2) - maxd 
c
c ... cr = (dr**t)*br.
c
      if (ndt+ndb .gt. 0) go to 15
      do 10 i = 1,nr
 10   c(i) = coef(i,1)*b(i)
      go to 20
 15   call bmulnt (ndim,nr,ndt,ndb,coef,coef(1,2),coef(1,ndt+2),b,c)
c
c ... wksp = (t**t)*br
c
 20   if (maxz*max2 .eq. 0) return
      do 25 i = 1,nb
 25   wksp(i) = 0.0d0 
      call vadddt (ndim,2,nr,nb,maxz,coef(1,maxd+1),
     a            jcnew(1,maxd+1),wksp,b,nr)
c
c ... wksp = (db**(-t))*wksp
c
      if (ndt+ndb .gt. 0) go to 35
      do 30 i = 1,nb
 30   wksp(i) = wksp(i)*dfac(i+nr)
      go to 40
 35   call bdsolt (n,nb,nsize,ndt,ndb,dfac(nrp1),wksp,wksp) 
c
c ... cr = cr - (b**t)*wksp
c
 40   call vsubdt (ndim,2,nb,nr,max2,coef(nrp1,maxd+1),
     a            jcnew(2,maxd+1),c,wksp,-nr)
      return
      end 
      subroutine rsatp (ndimm,n,nr,maxnz,jcoef,coef,b,c,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsatp computes  c = (dr - (b**t)*inv(db)*(t**t))*b
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     purdue format 
c
c ... parameters -- 
c
c        ndim       row dimension of coef,jcoef arrays
c        n          order of total system
c        nr         order of red subsystem
c        maxnz      number of columns in coef,jcoef arrays
c        jcoef      integer array of matrix column numbers
c        coef       floating point array of matrix coefficients
c        b,c        vectors of length nr
c        wksp       workspace array of length n + nb
c
c ... specifications for parameters
c
      integer   jcoef(ndimm,2)
      dimension coef(ndimm,2), b(1), c(1), wksp(1)
c
      ndim = ndimm
      do 10 i = 1,nr
 10   c(i) = coef(i,1)*b(i)
      if (maxnz .le. 1) return
      np1 = n + 1
      nb = n - nr
      nrp1 = nr + 1 
      maxm1 = maxnz - 1
      do 15 i = 1,n 
 15   wksp(i) = 0.0d0 
      call vaddpt (ndim,ndim,nr,maxm1,coef(1,2),jcoef(1,2),wksp,b,
     a             wksp)
      do 20 i = nrp1,n
 20   wksp(i) = wksp(i)/coef(i,1)
      call vsubpt (ndim,ndim,nb,maxm1,coef(nrp1,2),jcoef(nrp1,2),c,
     a             wksp(nrp1),wksp(np1))
      return
      end 
      subroutine rsbegd (nn,nsize,nrr,ndim,maxnew,ndtt,ndbb,jcnew,
     a                   coef,c,b,dfac,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsbegd computes  cr = br - t*inv(db)*bb.
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     diagonal storage
c
c ... parameters -- 
c
c        n          order of system
c        nsize      size of an individual subsystem (if multiple
c                    systems) 
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnew     number of columns in coef array
c        ndt        number of upper diagonals in diagonal block
c        ndb        number of lower diagonals in diagonal block
c        coef       floating point data structure 
c        b          vector of length n containing bb behind br
c        c          vector of length nr containing cr
c        dfac       vector of length (1+nt+nb)*n containing 
c                    factorization of diagonal block upon input
c        wksp       workspace vector of length nb 
c
c ... specifications for parameters
c
      integer   jcnew(2,1), maxnew(2)
      dimension coef(ndim,2), b(1), c(1), dfac(1), wksp(1)
c
      n = nn
      nr = nrr
      ndt = ndtt
      ndb = ndbb
      nrp1 = nr + 1 
      nb = n - nr
      maxd = 1 + ndt + ndb
c
c ... compute cr.
c
      do 10 i = 1,nr
 10   c(i) = b(i)
      call bdsol (n,nb,nsize,ndt,ndb,dfac(nrp1),b(nrp1),wksp,1)
      maxm1 = maxnew(1) - maxd
      call vsubd (ndim,2,nr,nb,maxm1,coef(1,maxd+1),jcnew(1,maxd+1),
     a            c,wksp,nr)
      return
      end 
      subroutine rsbegp (n,nr,ndim,maxnz,jcoef,coef,c,b,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsbegp computes  cr = br - t*inv(db)*bb.
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     purdue storage
c
c ... parameters -- 
c
c        n          order of system
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnz      number of columns in coef array
c        jcoef      integer data structure
c        coef       floating point data structure 
c        b          vector of length n containing bb behind br
c        c          vector of length nr containing cr
c        wksp       workspace vector of length n
c
c ... specifications for parameters
c
      integer   jcoef(ndim,2) 
      dimension coef(ndim,2), b(1), c(1), wksp(1) 
c
      nrp1 = nr + 1 
      do 10 i = 1,nr
 10   c(i) = b(i)
      if (maxnz .le. 1) return
      do 15 i = nrp1,n
 15   wksp(i) = b(i)/coef(i,1)
      maxm1 = maxnz - 1
      call vsubp (ndim,ndim,nr,maxm1,coef(1,2),jcoef(1,2),c,
     a            wksp,wksp)
      return
      end 
      subroutine rsendd (nn,nsize,nrr,ndim,maxnew,ndtt,ndbb,jcnew,
     a                   coef,x,b,dfac) 
      implicit double precision (a-h, o-z)
c
c ... rsendd computes  xb = inv(db)*(bb - b*xr)
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     diagonal storage
c
c ... parameters -- 
c
c        n          order of system
c        nsize      size of an individual subsystem (if multiple
c                    systems) 
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnew     number of columns in coef array
c        ndt        number of upper diagonals in diagonal block
c        ndb        number of lower diagonals in diagonal block
c        coef       floating point data structure 
c        x          vector of length n containing  xr, xb
c        b          vector of length n containing bb in the last
c                    nb locations
c        dfac       vector of length (1+nt+nb)*n containing 
c                    factorization of diagonal block upon input
c
c ... specifications for parameters
c
      integer   jcnew(2,1), maxnew(2)
      dimension coef(ndim,2), x(1), b(1), dfac(1) 
c
      n = nn
      nr = nrr
      ndt = ndtt
      ndb = ndbb
      nrp1 = nr + 1 
      nb = n - nr
      maxd = 1 + ndt + ndb
c
c ... compute xb.
c
      do 10 i = nrp1,n
 10   x(i) = b(i)
      max2 = maxnew(2) - maxd 
      call vsubd (ndim,2,nb,nr,max2,coef(nrp1,maxd+1),
     a            jcnew(2,maxd+1),x(nrp1),x,-nr)
      call bdsol (n,nb,nsize,ndt,ndb,dfac(nrp1),x(nrp1),x(nrp1),1)
      return
      end 
      subroutine rsendp (n,nr,ndim,maxnz,jcoef,coef,x,b,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsendp computes  xb = inv(db)*(bb - b*xr)
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     purdue format 
c
c ... parameters -- 
c
c        n          order of matrix
c        nr         order of red subsystem
c        ndim       row dimension of ah and jah arrays
c        maxnz      number of columns in coef and jcoef arrays
c        jcoef      integer array of column numbers
c        coef       floating point array of matrix coefficients
c        x          vector of length n containing  xr, xb
c        b          vector of length n containing bb in the last
c                    nb locations
c        wksp       workspace array of length nb
c
c ... specifications for parameters
c
      integer   jcoef(ndim,2) 
      dimension coef(ndim,2), x(1), b(1), wksp(1) 
c
      nrp1 = nr + 1 
      nb = n - nr
      do 10 i = nrp1,n
 10   x(i) = b(i)
      if (maxnz .le. 1) go to 15
      maxm1 = maxnz - 1
      call vsubp (ndim,ndim,nb,maxm1,coef(nrp1,2),jcoef(nrp1,2),
     a            x(nrp1),x,wksp)
 15   do 20 i = nrp1,n
 20   x(i) = x(i)/coef(i,1)
      return
      end 
      subroutine rsmatd (ndim,nrr,nb,maxnew,jcnew,dr,ah,ak,db,
     a                   maxrss,jcrs,rs,maxlim,isym,ier)
      implicit double precision (a-h, o-z)
c
c ... rsmatd computes  rs = dr - ah*inv(db)*ak  where a has been
c     permuted to red-black form --
c
c                   * dr  ah *
c             a =   *        *
c                   * ak  db *
c
c     (diagonal storage)
c
c      dr is nr x nr        ah is nr x nb
c      ak is nb x nr        db is nb x nb
c
c ... definition of parameters --
c
c         ndim          row dimension of ah and ak arrays
c         nr            number of red points
c         nb            number of black points
c         maxnew        integer vector of length 2 indicating number
c                        of diagonals stored in ah and ak,
c                        respectively.
c         jcnew         integer array of diagonal numbers
c         dr            vector of length nr
c         ah            array of size nr by (maxnew(1)-1)
c         ak            array of size nb by (maxnew(2)-1)
c         db            vector of length nb
c         maxrs         number of columns needed to store reduced
c                        system (output)
c         jcrs          diagonal numbers for rs (output)
c         rs            array to contain reduced system
c         maxlim        maximum column width to be allowed for rs
c         isym          symmetry switch for rs matrix
c                        = 0   store only upper half of rs
c                        = 1   store all of rs
c         ier           error code
c                        =  0     no errors detected
c                        = -2     maxlim .lt. maxrs
c
c ... specifications for parameters
c
      integer maxnew(2), jcnew(2,1), jcrs(1)
      dimension db(1), ak(ndim,1), ah(ndim,1), dr(1), rs(nrr,1)
c
      nr = nrr
      maxrs = 1
      jcrs(1) = 0
      do 5 i = 1,nr 
 5    rs(i,1) = dr(i)
      maxh = maxnew(1) - 1
      maxk = maxnew(2) - 1
      do 35 lh = 1,maxh
         i = jcnew(1,lh+1) - nr
         ia1 = max (1,1-i)
         ib1 = min (nr,nb-i) 
         do 30 lk = 1,maxk
            k = jcnew(2,lk+1) + nr
            l = i + k
            if (l .lt. 0 .and. isym .eq. 0) go to 30
            do 10 ld = 1,maxrs
               if (jcrs(ld) .eq. l) go to 20
 10         continue
            if (maxrs .eq. maxlim) go to 999
            maxrs = maxrs + 1 
            ld = maxrs
            jcrs(maxrs) = l
            do 15 ii = 1,nr
 15         rs(ii,maxrs) = 0.0d0
 20         ist = max (ia1,1-l)
            ied = min (ib1,nr-l)
            do 25 m = ist,ied 
 25         rs(m,ld) = rs(m,ld) - ah(m,lh)*ak(m+i,lk)/db(m+i)
 30      continue
 35   continue
      maxrss = maxrs
      return
c
c ... error exit -- maxlim too small.
c
 999  ier = -2
      return
      end 
      subroutine rsmatp (ndim,nrr,maxnzz,jcoef,coef,maxrss,jcrs,
     a                   rs,maxlim,wksp,iwksp,ier)
      implicit double precision (a-h, o-z)
c
c ... rsmatp computes  rs = dr - ah*inv(db)*ak  where a has been
c     permuted to red-black form --
c
c                   * dr  ah *
c             a =   *        *
c                   * ak  db *
c
c     (purdue storage)
c
c      dr is nr x nr        ah is nr x nb
c      ak is nb x nr        db is nb x nb
c
c ... definition of parameters --
c
c         ndim          row dimension of coef and jcoef arrays
c         nr            number of red points
c         maxnz         number of columns in coef and jcoef 
c         jcoef         array of column indices
c         coef          array of matrix coefficients
c         maxrs         number of columns needed to store reduced
c                        system (output)
c         jcrs          column numbers for rs (output)
c         rs            array to contain reduced system
c         maxlim        maximum column width to be allowed for rs
c         wksp          workspace of length 2*nr
c         iwksp         integer workspace of length nr
c         ier           error code
c                        =  0     no errors detected
c                        = -2     maxlim .lt. maxrs
c
c ... specifications for parameters
c
      integer jcoef(ndim,1), jcrs(nrr,1), iwksp(1)
      dimension coef(ndim,1), rs(nrr,1), wksp(1)
c
      nr = nrr
      maxnz = maxnzz
      maxrs = 1
      do 5 i = 1,nr 
         rs(i,1) = coef(i,1)
         jcrs(i,1) = i
 5    continue
      do 50 j = 2,maxnz
         call vgathr (nr,coef,jcoef(1,j),wksp)
         do 10 i = 1,nr
 10      wksp(i) = coef(i,j)/wksp(i)
         do 45 jj = 2,maxnz
            call vgathr (nr,coef(1,jj),jcoef(1,j),wksp(nr+1))
            call vgathi (nr,jcoef(1,jj),jcoef(1,j),iwksp)
            do 15 i = 1,nr
 15         wksp(nr+i) = wksp(i)*wksp(nr+i)
            do 40 i = 1,nr
               jcol = iwksp(i)
               term = wksp(nr+i)
               if (jcol .gt. nr) go to 40
               do 20 jjj = 1,maxrs
                  if (jcrs(i,jjj) .ne. jcol) go to 20
                  rs(i,jjj) = rs(i,jjj) - term
                  go to 40
 20            continue
               if (maxrs .eq. 1) go to 30
               do 25 jjj = 2,maxrs
                  if (jcrs(i,jjj) .ne. i) go to 25
                  rs(i,jjj) = rs(i,jjj) - term
                  jcrs(i,jjj) = jcol
                  go to 40
 25            continue
 30            if (maxrs .eq. maxlim) go to 999
               maxrs = maxrs + 1
               do 35 ii = 1,nr
                  jcrs(ii,maxrs) = ii
                  rs(ii,maxrs) = 0.0d0
 35            continue
               rs(i,maxrs) = -term
               jcrs(i,maxrs) = jcol
 40         continue
 45      continue
 50   continue
      maxrss = maxrs
      return
c
c ... error exit -- maxlim too small.
c
 999  ier = -2 
      return
      end 
      subroutine rsrhsd (nn,nrr,ndim,maxnew,jcnew,coef,c,b,wksp)
      implicit double precision (a-h, o-z)
c
c ... rsrhsd computes  cr = br - t*inv(db)*bb.
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     diagonal storage
c
c ... parameters -- 
c
c        n          order of system
c                    systems) 
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnew     number of columns in coef array
c        coef       floating point data structure 
c        b          vector of length n containing bb behind br
c        c          vector of length nr containing cr
c        wksp       workspace vector of length nb 
c
c ... specifications for parameters
c
      integer   jcnew(2,2), maxnew(2)
      dimension coef(ndim,2), b(1), c(1), wksp(1) 
c
      n = nn
      nr = nrr
      nb = n - nr
c
c ... compute cr.
c
      do 10 i = 1,nr
 10   c(i) = b(i)
      do 15 i = 1,nb
 15   wksp(i) = b(nr+i)/coef(nr+i,1)
      maxm1 = maxnew(1) - 1
      call vsubd (ndim,2,nr,nb,maxm1,coef(1,2),jcnew(1,2),
     a            c,wksp,nr)
      return
      end 
      subroutine rsxbd (nn,nrr,ndim,maxnew,jcnew,coef,x,b)
      implicit double precision (a-h, o-z)
c
c ... rsxbd computes  xb = inv(db)*(bb - b*xr)
c
c       where      a  = ( dr   t )
c                       ( b   db )
c
c     diagonal storage
c
c ... parameters -- 
c
c        n          order of system
c                    systems) 
c        nr         order of the red subsystem
c        ndim       row dimension of coef array
c        maxnew     number of columns in coef array
c        coef       floating point data structure 
c        x          vector of length n containing  xr, xb
c        b          vector of length n containing bb in the last
c                    nb locations
c
c ... specifications for parameters
c
      integer   jcnew(2,2), maxnew(2)
      dimension coef(ndim,2), x(1), b(1)
c
      n = nn
      nr = nrr
      nrp1 = nr + 1 
      nb = n - nr
c
c ... compute xb.
c
      do 10 i = nrp1,n
 10   x(i) = b(i)
      max2 = maxnew(2) - 1
      call vsubd (ndim,2,nb,nr,max2,coef(nrp1,2), 
     a            jcnew(2,2),x(nrp1),x,-nr)
      do 15 i = nrp1,n
 15   x(i) = x(i)/coef(i,1)
      return
      end 
      subroutine sbbs (ldd,ldt,n,kblszz,nsize,lbhb,iblock,d,t,
     a                 jt,x,omega)
      implicit double precision (a-h, o-z)
c
c ... sbbs does an block ssor backward pass.
c     symmetric diagonal data structure, natural ordering.
c     block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         kblsz    block size 
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         lbhb     number of blocks per block row 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         x        input/output vector of length n
c         omega    over-relaxation factor
c
c ... specifications for parameters
c
      integer   jt(1), iblock(3,1)
      dimension d(ldd,2), t(ldt,1), x(1)
c
      kblsz = kblszz
      l = n/kblsz
      nt = iblock(3,1) - 1
      do 35 k = l,1,-1
         ist = (k - 1)*kblsz + 1
         ied = k*kblsz
         if (k .eq. l) go to 15
         jjlim = min (lbhb,l-k+2)
         do 10 jj = 3,jjlim
            jblk = iblock(1,jj)
            jst = iblock(2,jj)
            mjj = iblock(3,jj)
            inc = jblk*kblsz
            istf = ist + inc
            if (istf .gt. n) go to 10
            call vsubd (ldt,1,kblsz,kblsz,mjj,t(ist,jst),jt(jst),
     a                   x(ist),x(istf),inc)
 10      continue
 15      if (nt .ge. 1) go to 25
         do 20 i = ist,ied
 20      x(i) = omega*d(i,1)*x(i)
         go to 35
 25      call bdsol (ldd,kblsz,nsize,nt,0,d(ist,1),x(ist),x(ist),
     a               0)
         do 30 i = ist,ied
 30      x(i) = omega*x(i)
 35   continue
      return
      end 
      subroutine sbbsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... sbbsn does an block ssor backward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         x        input/output vector of length n
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,2), t(ldt,1), wksp(1), x(1) 
      logical   unif
c
      unif = iunif .eq. 1
c
      l = ncolor
      if (.not. unif) go to 10
      na = nci(1)
      nb = na
      jlim = lbhb(1)
      l = n/na
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      kk = 1
c
c ... do backward solution.
c
 10   lm1 = l - 1
      do 50 k = lm1,1,-1
         if (unif) go to 15
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         go to 20
 15      ist = (k - 1)*na + 1 
 20      ied = ist + na - 1
         do 25 i = 1,na
 25      wksp(i) = 0.0d0
         do 30 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .le. k) go to 30
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            call vaddd (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb),
     a                  wksp,x(istb),inc)
 30      continue
         if (ndt + ndb .ge. 1) go to 40 
         do 35 i = ist,ied
 35      x(i) = x(i) - omega*d(i,1)*wksp(i-ist+1) 
         go to 50
 40      call bdsol (ldd,na,nsize,ndt,ndb,d(ist,1),wksp,wksp,1)
         do 45 i = ist,ied
 45      x(i) = x(i) - omega*wksp(i-ist+1)
 50   continue
      return
      end 
      subroutine sbbsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,x,omega,iunif)
      implicit double precision (a-h, o-z)
c
c ... sbbsnt does an block ssor transpose backward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         x        input/output vector of length n
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,2), t(ldt,1), x(1)
      logical   unif
c
      unif = iunif .eq. 1
c
      l = ncolor
      if (.not. unif) go to 10
      na = nci(1)
      nb = na
      jlim = lbhb(1)
      l = n/na
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      kk = 1
c
c ... do backward solution.
c
 10   do 50 k = l,1,-1
         if (unif) go to 15
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         go to 20
 15      ist = (k - 1)*na + 1 
 20      ied = ist + na - 1
         if (ndt + ndb .ge. 1) go to 30 
         do 25 i = ist,ied
 25      x(i) = omega*d(i,1)*x(i)
         go to 40
 30      call bdsolt (ldd,na,nsize,ndt,ndb,d(ist,1),x(ist),x(ist))
         do 35 i = ist,ied
 35      x(i) = omega*x(i)
 40      do 45 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .ge. k) go to 45
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            call vsubdt (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb), 
     a                  x(istb),x(ist),inc)
 45      continue
 50   continue
      return
      end 
      subroutine sbfs (ldd,ldt,n,kblszz,nsize,lbhb,iblock,d,t,
     a                 jt,x,omega,wksp) 
      implicit double precision (a-h, o-z)
c
c ... sbfs does an block ssor forward pass.
c     symmetric diagonal data structure, natural ordering.
c     block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         kblsz    block size 
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         lbhb     number of blocks per block row 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         x        input/output vector of length n
c         omega    over-relaxation factor
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jt(1), iblock(3,1)
      dimension d(ldd,2), t(ldt,1), wksp(1), x(1) 
c
      kblsz = kblszz
      l = n/kblsz
      lm1 = l - 1
      nt = iblock(3,1) - 1
      do 35 k = 1,lm1
         ist = (k - 1)*kblsz + 1
         ied = k*kblsz
         if (nt .ge. 1) go to 15
         do 10 i = ist,ied
 10      wksp(i-ist+1) = omega*d(i,1)*x(i)
         go to 25
 15      call bdsol (ldd,kblsz,nsize,nt,0,d(ist,1),
     a                          x(ist),wksp,0)
         do 20 i = 1,kblsz
 20      wksp(i) = omega*wksp(i)
 25      jjlim = min (lbhb,l-k+2)
         do 30 jj = 3,jjlim
            jblk = iblock(1,jj)
            jst = iblock(2,jj)
            mjj = iblock(3,jj)
            inc = jblk*kblsz
            istf = ist + inc
            if (istf .gt. n) go to 30
            call vsubdt (ldt,1,kblsz,kblsz,mjj,t(ist,jst),jt(jst),
     a                   x(istf),wksp,inc)
 30      continue
 35   continue
      return
      end 
      subroutine sbfsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif)
      implicit double precision (a-h, o-z)
c
c ... sbfsn does an block ssor forward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         x        input/output vector of length n
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,2), t(ldt,1), x(1)
      logical   unif
c
      unif = iunif .eq. 1
c
      l = ncolor
      if (.not. unif) go to 10
      na = nci(1)
      nb = na
      jlim = lbhb(1)
      l = n/na
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      kk = 1
c
c ... do forward solution.
c
 10   do 45 k = 1,l 
         if (unif) go to 15
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         go to 20
 15      ist = (k - 1)*na + 1 
 20      ied = ist + na - 1
         do 25 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .ge. k) go to 25
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            call vsubd (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb),
     a                  x(ist),x(istb),inc)
 25      continue
         if (ndt + ndb .ge. 1) go to 35 
         do 30 i = ist,ied
 30      x(i) = omega*d(i,1)*x(i)
         go to 45
 35      call bdsol (ldd,na,nsize,ndt,ndb,d(ist,1),x(ist),x(ist),1)
         do 40 i = ist,ied
 40      x(i) = omega*x(i)
 45   continue
      return
      end 
      subroutine sbfsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,x,omega,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... sbfsnt does an block ssor transpose forward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         x        input/output vector of length n
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,2), t(ldt,1), wksp(1), x(1) 
      logical   unif
c
      unif = iunif .eq. 1
c
      l = ncolor
      if (.not. unif) go to 10
      na = nci(1)
      nb = na
      jlim = lbhb(1)
      l = n/na
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      kk = 1
c
c ... do forward solution.
c
 10   lm1 = l - 1
      do 50 k = 1,lm1
         if (unif) go to 15
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         go to 20
 15      ist = (k - 1)*na + 1 
 20      ied = ist + na - 1
         if (ndt + ndb .ge. 1) go to 30 
         do 25 i = ist,ied
 25      wksp(i-ist+1) = omega*d(i,1)*x(i)
         go to 40
 30      call bdsolt (ldd,na,nsize,ndt,ndb,d(ist,1),x(ist),wksp)
         do 35 i = 1,na
 35      wksp(i) = omega*wksp(i)
 40      do 45 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .le. k) go to 45
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            call vsubdt (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb), 
     a                  x(istb),wksp,inc)
 45      continue
 50   continue
      return
      end 
      subroutine sbsl (ldd,ldt,n,kblsz,nsize,lbhb,iblock,d,t,
     a                 jt,y,x,omega,wksp)
      implicit double precision (a-h, o-z)
c
c ... sbsl does an block ssor solution. 
c     symmetric diagonal data structure, natural ordering.
c     block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         kblsz    block size 
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         lbhb     number of blocks per block row 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         y        input vector for the right-hand-side
c         x        output vector for the solution to q*x = y
c         omega    over-relaxation factor
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jt(1), iblock(3,1)
      dimension d(ldd,1), t(ldt,1), wksp(1), x(1), y(1)
c
      const = 2.0d0 - omega
      do 10 i = 1,n 
 10   x(i) = const*y(i)
      call sbfs (ldd,ldt,n,kblsz,nsize,lbhb,iblock,d,t,
     a           jt,x,omega,wksp)
      call sbbs (ldd,ldt,n,kblsz,nsize,lbhb,iblock,d,t,
     a           jt,x,omega)
      return
      end 
      subroutine sbsln (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,y,x,omega,iunif,wksp) 
      implicit double precision (a-h, o-z)
c
c ... sbsln does an block ssor solution.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         y        input vector of length n containing right-hand-side
c         x        output vector containing the solution to q*x = y
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,1), t(ldt,1), wksp(1), x(1), y(1)
c
      const = 2.0d0 - omega
      do 10 i = 1,n 
 10   x(i) = const*y(i)
      call sbfsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif)
      call sbbsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif,wksp)
      return
      end 
      subroutine sbslnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,y,x,omega,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... sbslnt does an block ssor transpose solution.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         y        input vector of length n containing right-hand-side
c         x        output vector containing the solution to q*x = y
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,1), t(ldt,1), wksp(1), x(1), y(1)
c
      const = 2.0d0 - omega
      do 10 i = 1,n 
 10   x(i) = const*y(i)
      call sbfsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif,wksp)
      call sbbsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif)
      return
      end 
      subroutine sbsln1 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,y,x,omega,iunif)
      implicit double precision (a-h, o-z)
c
c ... sbsln1 does an block ssor forward solution. 
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         y        input vector of length n containing right-hand-side
c         x        output vector containing the solution to q*x = y
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,1), t(ldt,1), x(1), y(1)
c
      const = 2.0d0 - omega
      do 10 i = 1,n 
 10   x(i) = const*y(i)
      call sbfsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif)
      return
      end 
      subroutine sbsln2 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,y,x,omega,iunif,wksp) 
      implicit double precision (a-h, o-z)
c
c ... sbsln2 does an block ssor back solution.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         y        input vector of length n containing right-hand-side
c         x        output vector containing the solution to q*x = y
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,1), t(ldt,1), wksp(1), x(1), y(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call sbbsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif,wksp)
      return
      end 
      subroutine sbsln3 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,y,x,omega,iunif)
      implicit double precision (a-h, o-z)
c
c ... sbsln3 does an block ssor transpose forward solution. 
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         y        input vector of length n containing right-hand-side
c         x        output vector containing the solution to q*x = y
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,1), t(ldt,1), x(1), y(1)
c
      const = 2.0d0 - omega
      do 10 i = 1,n 
 10   x(i) = const*y(i)
      call sbbsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif)
      return
      end 
      subroutine sbsln4 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,y,x,omega,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... sbsln4 does an block ssor transpose back solution.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ssor preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0.  formed in the factorization routine.
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         y        input vector of length n containing right-hand-side
c         x        output vector containing the solution to q*x = y
c         omega    over-relaxation factor
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jt(ncolor,1), nci(1), lbhb(1),
     a          iblock(3,ncolor,2)
      dimension d(ldd,1), t(ldt,1), wksp(1), x(1), y(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call sbfsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,omega,iunif,wksp)
      return
      end 
      subroutine scal1 (nn,ndim,maxnzz,jcoef,coef,rhs,u,ubar,
     a                  diag,work,iflag,ier)
      implicit double precision (a-h, o-z)
c
c ... scal1 scales the original matrix to a unit diagonal matrix.
c     (purdue data structure) 
c     rhs and u vectors are scaled accordingly.  upon output, diag
c     contains the reciprocal square roots of the diagonal elements.
c     it is assumed that the diagonal of the matrix is in column one
c     of coef.
c
c ... parameters -- 
c
c         n       dimension of matrix
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array 
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         rhs     right hand side of matrix problem
c         u       latest estimate of solution
c         ubar    exact solution (optional)
c         diag    work array of length n (nonvolatile)
c         work    work array of length n (volatile)
c         iflag   flag for ubar
c                  = 0  do not scale ubar
c                  = 1  scale ubar
c         ier     error flag -- on return, values mean
c                      0 -- no errors detected
c                     -4 -- nonpositive diagonal element
c
c ... specifications for parameters
c
      integer   jcoef(ndim,1) 
      dimension coef(ndim,1), rhs(1), u(1), diag(1), work(1),
     a          ubar(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      maxnz = maxnzz
c
c ... check for positive diagonal entries for each row.
c
      cmin = vmin (n,coef)
      if (cmin .gt. 0.0d0) go to 10
c
c ... fatal error -- nonpositive diagonal element.
c
      ier = -4
      return
c
c ... scale matrix.  store reciprocal square roots
c ... of diagonal entries in diag.
c
 10   do 15 i = 1,n 
 15   diag(i) = sqrt (coef(i,1))
c
c ... scale rhs, u, and ubar. 
c
      do 20 i = 1,n 
 20   u(i) = diag(i)*u(i)
      if (iflag .eq. 0) go to 30
      do 25 i = 1,n 
 25   ubar(i) = diag(i)*ubar(i)
 30   do 35 i = 1,n 
 35   diag(i) = 1.0d0/diag(i)
      do 40 i = 1,n 
 40   rhs(i) = diag(i)*rhs(i) 
      if (keygs .eq. 2) go to 55
c
c ... using gathers.
c
      do 50 j = 1,maxnz
         call vgathr (n,diag,jcoef(1,j),work)
         do 45 i = 1,n
 45      coef(i,j) = diag(i)*coef(i,j)*work(i)
 50   continue
      return
c
c ... not using gathers.
c
 55   do 65 j = 1,maxnz
         do 60 i = 1,n
 60      coef(i,j) = diag(i)*coef(i,j)*diag(jcoef(i,j))
 65   continue
      return
      end 
      subroutine scal2 (nn,ndim,maxnz,jcoef,coef,rhs,u,ubar,
     a                  diag,iflag,ier) 
      implicit double precision (a-h, o-z)
c
c ... scal2 scales the original matrix to a unit diagonal matrix.
c     (diagonal data structure)
c     rhs and u vectors are scaled accordingly.  upon output, diag
c     contains the reciprocal square roots of the diagonal elements.
c     it is assumed that the diagonal of the matrix is in column one
c     of coef.
c
c ... parameters -- 
c
c         n       dimension of matrix
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array 
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         rhs     right hand side of matrix problem
c         u       latest estimate of solution
c         ubar    exact solution (optional)
c         diag    work array of length n (nonvolatile)
c         iflag   flag for ubar
c                  = 0  do not scale ubar
c                  = 1  scale ubar
c         ier     error flag -- on return, values mean
c                      0 -- no errors detected
c                     -4 -- nonpositive diagonal element
c
c ... specifications for parameters
c
      integer   jcoef(2)
      dimension coef(ndim,1), rhs(1), u(1), diag(1), ubar(1)
c
c
      n = nn
c
c ... check for positive diagonal entries for each row.
c
      cmin = vmin (n,coef)
      if (cmin .gt. 0.0d0) go to 10
c
c ... fatal error -- nonpositive diagonal element.
c
      ier = -4
      return
c
c ... scale matrix.  store reciprocal square roots
c ... of diagonal entries in diag.
c
 10   do 15 i = 1,n 
 15   diag(i) = sqrt (coef(i,1))
c
c ... scale rhs, u, and ubar. 
c
      do 20 i = 1,n 
 20   u(i) = diag(i)*u(i)
      if (iflag .eq. 0) go to 30
      do 25 i = 1,n 
 25   ubar(i) = diag(i)*ubar(i)
 30   do 35 i = 1,n 
 35   diag(i) = 1.0d0/diag(i)
      do 40 i = 1,n 
 40   rhs(i) = diag(i)*rhs(i) 
c
c ... scale matrix. 
c
      do 60 j = 1,maxnz
         ind = jcoef(j)
         len = n - iabs(ind)
         if (ind .lt. 0) go to 50
         do 45 i = 1,len
 45      coef(i,j) = diag(i)*coef(i,j)*diag(i+ind)
         go to 60
 50      do 55 i = 1,len
 55      coef(i-ind,j) = diag(i-ind)*coef(i-ind,j)*diag(i)
 60   continue
      return
      end 
      subroutine scal3 (nn,nz,ia,ja,a,rhs,u,ubar,diag,
     a                  work,iflag,ier) 
      implicit double precision (a-h, o-z)
c
c ... scal3 scales the original matrix to a unit diagonal matrix.
c     (sparse data structure) 
c     rhs and u vectors are scaled accordingly.  upon output, diag
c     contains the reciprocal square roots of the diagonal elements.
c     it is assumed that the diagonal of the matrix is in the
c     n first locations of a. 
c
c ... parameters -- 
c
c         n       dimension of matrix
c         nz      length of ia, ja, and a vectors 
c         a       vector containing matrix coefficients
c         ia      vector of i values
c         ja      vector of j values
c         rhs     right hand side of matrix problem
c         u       latest estimate of solution
c         ubar    exact solution (optional)
c         diag    vector of length n containing the reciprocal
c                  square roots of the diagonal elements upon
c                  output
c         work    workspace vector of length n
c         iflag   flag for ubar
c                  = 0  do not scale ubar
c                  = 1  scale ubar
c         ier     error flag -- on return, values mean
c                      0 -- no errors detected
c                     -4 -- nonpositive diagonal element
c
c ... specifications for parameters
c
      integer   ia(1), ja(1)
      dimension a(1), rhs(1), u(1), diag(1), work(1),
     a          ubar(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
c
c ... check for positive diagonal entries for each row.
c
      cmin = vmin (n,a)
      if (cmin .gt. 0.0d0) go to 10
c
c ... fatal error -- nonpositive diagonal element.
c
      ier = -4
      return
c
c ... scale matrix.  store reciprocal square roots
c ... of diagonal entries in diag.
c
 10   do 15 i = 1,n 
 15   diag(i) = sqrt (a(i))
c
c ... scale rhs, u, and ubar. 
c
      do 20 i = 1,n 
 20   u(i) = diag(i)*u(i)
      if (iflag .eq. 0) go to 30
      do 25 i = 1,n 
 25   ubar(i) = diag(i)*ubar(i)
 30   do 35 i = 1,n 
 35   diag(i) = 1.0d0/diag(i)
      do 40 i = 1,n 
 40   rhs(i) = diag(i)*rhs(i) 
      if (keygs .eq. 2) go to 60
c
c ... using gathers.
c
      ist = 1
 45   ied = min (ist-1+n,nz) 
      if (ied .lt. ist) return
         len = ied - ist + 1
         call vgathr (len,diag,ia(ist),work)
         do 50 i = ist,ied
 50      a(i) = a(i)*work(i-ist+1)
         call vgathr (len,diag,ja(ist),work)
         do 55 i = ist,ied
 55      a(i) = a(i)*work(i-ist+1)
      ist = ied + 1 
      go to 45
c
c ... not using gathers.
c
 60   do 65 i = 1,nz
 65   a(i) = a(i)*diag(ia(i))*diag(ja(i))
      return
      end 
      subroutine sorstp (n,u,ubar,dnrm,ccon)
      implicit double precision (a-h, o-z)
c
c ... sorstp performs a test to see if the sor
c     method has converged to a solution inside the error
c     tolerance, zeta.
c
c ... parameters -- 
c
c          n      order of system
c          u      present solution estimate
c          ubar   exact solution
c          dnrm   inner product of pseudo-residuals at preceding
c                    iteration
c          con    stopping test parameter (= ccon)
c
c ... specifications for parameters
c
      dimension u(1), ubar(1) 
      logical q1
      save    q1
c
c *** begin -- itpack common
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      logical           halt, maxadp, minadp, maxadd, minadd
      common / itcom2 / halt, maxadp, minadp, maxadd, minadd
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
c
c *** end   -- itpack common
c
      con = ccon
      halt = .false.
      if (ntest .eq. 6) go to 25
c
c ... special procedure for zeroth iteration.
c
      if (in .ge. 1) go to 5
      q1 = .false.
      udnm = 1.0d0
      stptst = 1000.0d0
      return
c
c ... test if udnm needs to be recomputed
c
 5    if (q1) go to 15
      if ((in .gt. 5)  .and.  (mod(in,5) .ne. 0)) go to 15
      uold = udnm
      udnm = 0.0d0
      do 10 i = 1,n 
 10   udnm = udnm + u(i)*u(i) 
      if (udnm .eq. 0.0d0) udnm = 1.0d0
      if ((in .gt. 5) .and.
     a       (abs (udnm-uold) .le. udnm*zeta)) q1 = .true.
c
c ... compute stopping test
c
 15   tr = sqrt (udnm)
      tl = 1.0d0
      if (con .eq. 1.0d0)  go to 20
      tl = sqrt (dnrm)
      tr = tr*(1.0d0 - con)
 20   stptst = tl/tr
      if (tl .ge. tr*zeta) return
      halt = .true. 
      return
c
c ... second test.
c
 25   if (in .eq. 0) ubarnm = sqrt (vdot(n,ubar,ubar))
      sum = 0.0d0
      do 30 i = 1,n 
 30   sum = sum + (u(i) - ubar(i))**2
      tl = sqrt (sum)
      tr = ubarnm
      stptst = tl/tr
      if (tl .lt. tr*zeta) halt = .true.
      return
      end 
      subroutine sords (ndim,nn,maxtt,jt,d,t,omegaa,irwise, 
     a                  u,rhs,unew,iwksp)
      implicit double precision (a-h, o-z)
c
c ... sords does an sor solve (natural ordering,
c     symmetric diagonal storage).
c
c        unew = inv(d + w*l)*((1-w)*d*un + w*(rhs - u*un))
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        u      current solution vector 
c        rhs    right hand side
c        unew   updated solution vector 
c        iwksp  integer workspace of length maxt
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), u(1), unew(1), rhs(1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      omega = omegaa
c
c ... rhs = (1-w)*d*un + w*(rhs - u*un) 
c
      call vsubd (ndim,1,n,n,maxt,t,jt,rhs,u,0)
      con = 1.0d0 - omega
      do 10 i = 1,n 
 10   rhs(i) = con*d(i)*u(i) + omega*rhs(i)
c
c ... rhs = inv(i+w*l*inv(d))*rhs
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 50
c
c ... diagonal-wise algorithm.
c
      do 15 i = 1,maxt
 15   iwksp(i) = jt(i) + 1
c
c ... determine nc, imin.
c
 20   nc = n
      do 25 i = 1,maxt
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 25
         nc = nterm 
         imin = i
 25   continue
      if (nc .ge. n) go to 70 
      ndel = jt(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 40
c
c ... special case for first minor subdiagonal.
c
      nc1 = n
      do 30 i = 1,maxt
         if (i .eq. imin) go to 30
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 30   continue
      iwksp(imin) = nc1 + 1
      do 35 j = ibeg,nc1
 35   rhs(j) = rhs(j) - omega*t(j-1,imin)*rhs(j-1)/d(j-1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 40   iwksp(imin) = iwksp(imin) + ndel
      iend = min (ibeg+ndel-1,n)
      do 45 i = ibeg,iend
 45   rhs(i) = rhs(i) - omega*t(i-ndel,imin)*rhs(i-ndel)/d(i-ndel)
      go to 20
c
c ... rowwise algorithm.
c
 50   do 65 i = 1,n 
         do 55 j = 1,maxt
 55      iwksp(j) = min (n,i+jt(j))
         term = omega*rhs(i)/d(i)
         do 60 j = 1,maxt
 60      rhs(iwksp(j)) = rhs(iwksp(j)) - t(i,j)*term
 65   continue
c
c ... unew = inv(d)*rhs
c
 70   do 75 i = 1,n 
 75   unew(i) = rhs(i)/d(i)
      return
      end 
      subroutine sordn (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,omegaa,
     a                  irwise,u,rhs,unew,iwksp)
      implicit double precision (a-h, o-z)
c
c ... sordn does an sor solve (natural ordering,
c     nonsymmetric diagonal storage).
c
c        unew = inv(d + w*l)*((1-w)*d*un + w*(rhs - u*un))
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        u      current solution vector 
c        rhs    right hand side
c        unew   updated solution vector 
c        iwksp  integer workspace of length maxt
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), b(ndim,1), u(1), unew(1), rhs(1)
      integer   jt(1), jb(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      omega = omegaa
c
c ... rhs = (1-w)*d*un + w*(rhs - u*un) 
c
      call vsubd (ndim,1,n,n,maxt,t,jt,rhs,u,0)
      con = 1.0d0 - omega
      do 10 i = 1,n 
 10   rhs(i) = con*d(i)*u(i) + omega*rhs(i)
c
c ... rhs = inv(i+w*l*inv(d))*rhs
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 50
c
c ... diagonal-wise algorithm.
c
      do 15 i = 1,maxb
 15   iwksp(i) = 1 - jb(i)
c
c ... determine nc, imin.
c
 20   nc = n
      do 25 i = 1,maxb
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 25
         nc = nterm 
         imin = i
 25   continue
      if (nc .ge. n) go to 70 
      ndel = -jb(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 40
c
c ... special case for first minor subdiagonal.
c
      nc1 = n
      do 30 i = 1,maxb
         if (i .eq. imin) go to 30
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 30   continue
      iwksp(imin) = nc1 + 1
      do 35 j = ibeg,nc1
 35   rhs(j) = rhs(j) - omega*b(j,imin)*rhs(j-1)/d(j-1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 40   iwksp(imin) = iwksp(imin) + ndel
      iend = min (ibeg+ndel-1,n)
      do 45 i = ibeg,iend
 45   rhs(i) = rhs(i) - omega*b(i,imin)*rhs(i-ndel)/d(i-ndel)
      go to 20
c
c ... rowwise algorithm.
c
 50   do 65 i = 1,n 
         do 55 j = 1,maxb
 55      iwksp(j) = max (1,i+jb(j))
         sum = 0.0d0
         do 60 j = 1,maxb
 60      sum = sum + b(i,j)*rhs(iwksp(j))/d(iwksp(j))
         rhs(i) = rhs(i) - omega*sum
 65   continue
c
c ... unew = inv(d)*rhs
c
 70   do 75 i = 1,n 
 75   unew(i) = rhs(i)/d(i)
      return
      end 
      subroutine sorp (ndim,nn,maxt,maxb,jt,jb,d,t,b,omega,u,
     a                 rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... sorp does an sor solve
c     (natural ordering, purdue storage).
c
c        unew = inv((1/w)*d + l)*(((1-w)/w)*d*un + (rhs - u*un))
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  over-relaxation factor
c        u      current solution vector 
c        rhs    right hand side
c        unew   updated solution vector 
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), b(ndim,1), u(1), rhs(1), unew(1)
      integer   jt(ndim,1), jb(ndim,1)
      n = nn
c
c ... rhs = ((1-w)/w)*d*un + (rhs - u*un)
c
      call vsubp (ndim,ndim,n,maxt,t,jt,rhs,u,unew)
      con = (1.0d0 - omega)/omega
      do 10 i = 1,n 
 10   unew(i) = con*d(i)*u(i) + rhs(i)
c
c ... unew = inv((1/w)*d + l)*rhs
c
      if (maxb .ge. 1) go to 20
      do 15 i = 1,n 
 15   unew(i) = omega*unew(i)/d(i)
      return
 20   do 30 i = 1,n 
         sum = unew(i)
         do 25 j = 1,maxb
            sum = sum - b(i,j)*unew(jb(i,j))
 25      continue
         unew(i) = omega*sum/d(i)
 30   continue
      return
      end 
      subroutine sorcp (ndimm,n,jc,d,c,ncol,nc,nt,nb,omega, 
     a                  u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... sorcp does an sor solve.
c     (purdue storage, multicolor)
c
c        unew = inv((1/w)*d + l)*(((1-w)/w)*d*un + (rhs - u*un))
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          u      current solution
c          rhs    right-hand-side
c          unew   updated solution
c
c ... specifications for parameters
c
      integer   jc(ndimm,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimm,1), u(1), rhs(1), unew(1)
c
      ndim = ndimm
      ncolor = ncol 
c
c ... rhs = ((1-w)/w)*d*un + (rhs - u*un)
c
      ist =  1
      do 10 icol = 1,ncolor
         npt = nc(icol)
         j2 = nt(icol)
         call vsubp (ndim,ndim,npt,j2,c(ist,1),jc(ist,1),rhs(ist),u,
     a               unew)
         ist = ist + npt
 10   continue
      con = (1.0d0 - omega)/omega
      do 15 i = 1,n 
 15   unew(i) = con*d(i)*u(i) + rhs(i)
c
c ... unew = inv((1/w)*d + l)*rhs
c
      ist = 1
      do 25 icol = 1,ncolor
         npt = nc(icol)
         ied = ist + npt - 1
         j1 = nt(icol) + 1
         mj = nb(icol)
         call vsubp (ndim,ndim,npt,mj,c(ist,j1),jc(ist,j1),unew(ist), 
     a               unew,rhs)
         do 20 i = ist,ied
 20      unew(i) = omega*unew(i)/d(i)
         ist = ist + npt
 25   continue
      return
      end 
      subroutine sordb (ldf,ndim,nsize,kblszz,iblock,lbhb,
     a                  dfac,coef,jcoef,nn,omega,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... sordb does an sor pass
c     (symmetric block diagonal format, constant block size)
c
c        unew = inv((1/w)*d + l)*(((1-w)/w)*d*un + (rhs - u*un))
c
c ... parameters -- 
c
c         ldf      row dimension of dfac
c         ndim     row dimension of coef array
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         lbhb     column size of iblock
c         dfac     array for diagonal block factorization
c         coef     array for matrix coefficients
c         jcoef    vector for diagonal numbers
c         n        size of system
c         omega    relaxation parameter 
c         u        current solution estimate
c         rhs      right-hand-side
c         unew     updated solution estimate
c
c ... specifications for parameters
c
      integer   jcoef(2), iblock(3,1)
      dimension dfac(ldf,1), coef(ndim,2), u(1), rhs(1), unew(1)
c
      n = nn
      kblsz = kblszz
c
c ... rhs = ((1-w)/w)*d*un + (rhs - u*un)
c
      nwdiag = iblock (3,1)
      nt = nwdiag - 1
      maxt = 0
      if (lbhb .lt. 3) go to 15
      do 10 j = 3,lbhb
         maxt = maxt + iblock(3,j)
 10   continue
 15   jbgn = nwdiag + 1
      call vsubd (ndim,1,n,n,maxt,coef(1,jbgn),jcoef(jbgn),rhs,
     a            u,0)
      call bmul (ndim,n,nt,coef,coef(1,2),u,unew) 
      con = (1.0d0 - omega)/omega
      do 20 i = 1,n 
 20   unew(i) = con*unew(i) + rhs(i)
c
c ... unew = inv((1/w)*d + l)*rhs
c
      l = n/kblsz
      do 50 k = 1,l 
         ist = (k - 1)*kblsz + 1
         ied = k*kblsz
         if (nt .ge. 1) go to 30
         do 25 i = ist,ied
 25      unew(i) = omega*dfac(i,1)*unew(i)
         go to 40
 30      call bdsol (ldf,kblsz,nsize,nt,0,dfac(ist,1),
     a                          unew(ist),unew(ist),0)
         do 35 i = ist,ied
 35      unew(i) = omega*unew(i)
 40      if (k .eq. l) go to 50
         jjlim = min (lbhb,l-k+2)
         do 45 jj = 3,jjlim
            jblk = iblock(1,jj)
            jst = iblock(2,jj) + nwdiag 
            mjj = iblock(3,jj)
            inc = jblk*kblsz
            istf = ist + inc
            if (istf .gt. n) go to 45
            call vsubdt (ndim,1,kblsz,kblsz,mjj,coef(ist,jst),
     a                   jcoef(jst),unew(istf),unew(ist),inc)
 45      continue
 50   continue
      return
      end 
      subroutine sordnb (ldf,ndim,nsize,kblszz,iblock,lbhbb,
     a                   dfac,coef,jcoef,nn,omega,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... sordnb does an sor pass 
c     (nonsymmetric block diagonal format, constant block size)
c
c        unew = inv((1/w)*d + l)*(((1-w)/w)*d*un + (rhs - u*un))
c
c ... parameters -- 
c
c         ldf      row dimension of dfac
c         ndim     row dimension of coef array
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         lbhb     column size of iblock
c         dfac     array for diagonal block factorization
c         coef     array for matrix coefficients
c         jcoef    vector for diagonal numbers
c         n        size of system
c         omega    relaxation parameter 
c         u        current solution estimate
c         rhs      right-hand-side
c         unew     updated solution estimate
c
c ... specifications for parameters
c
      integer   jcoef(2), iblock(3,2)
      dimension dfac(ldf,1), coef(ndim,2), u(1), rhs(1), unew(1)
c
      n = nn
      kblsz = kblszz
      lbhb = lbhbb
c
c ... rhs = ((1-w)/w)*d*un + (rhs - u*un)
c
      nt = iblock (3,1) - 1
      nb = iblock (3,2)
      nwdiag = nt + nb + 1
      maxt = 0
      if (lbhb .lt. 3) go to 15
      do 10 j = 3,lbhb
         ind = iblock(1,j)
         if (ind .gt. 0) maxt = maxt + iblock(3,j)
 10   continue
 15   jbgn = nwdiag + 1
      call vsubd (ndim,1,n,n,maxt,coef(1,jbgn),jcoef(jbgn),rhs,
     a            u,0)
      ind = nt + 2
      call bmuln (ndim,n,nt,nb,coef,coef(1,2),coef(1,ind),u,unew)
      con = (1.0d0 - omega)/omega
      do 20 i = 1,n 
 20   unew(i) = con*unew(i) + rhs(i)
c
c ... unew = inv((1/w)*d + l)*rhs
c
      l = n/kblsz
      do 45 k = 1,l 
         ist = (k - 1)*kblsz + 1
         ied = k*kblsz
         do 25 j = 3,lbhb
            jcol = k + iblock(1,j)
            if (jcol .ge. k .or. jcol .le. 0) go to 25
            jstb = iblock(2,j) + nwdiag 
            mb = iblock(3,j)
            inc = (jcol - k)*kblsz
            istb = ist + inc
            call vsubd (ndim,1,kblsz,kblsz,mb,coef(ist,jstb),
     a                  jcoef(jstb),unew(ist),unew(istb),inc)
 25      continue
         if (nt + nb .ge. 1) go to 35
         do 30 i = ist,ied
 30      unew(i) = omega*dfac(i,1)*unew(i)
         go to 45
 35      call bdsol (ldf,kblsz,nsize,nt,nb,dfac(ist,1),unew(ist),
     a               unew(ist),1)
         do 40 i = ist,ied
 40      unew(i) = omega*unew(i)
 45   continue
      return
      end 
      subroutine sordmb (ldf,ndim,nsize,iblock,lbhb,ncol,nc,ipt,
     a                   dfac,coef,jcnew,nn,omega,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... sordmb does an sor pass 
c     (nonsymmetric block diagonal format, nonconstant block size)
c
c        unew = inv((1/w)*d + l)*(((1-w)/w)*d*un + (rhs - u*un))
c
c ... parameters -- 
c
c         ldf      row dimension of dfac array
c         ndim     row dimension of coef array
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c         ncolor   number of distinct block sizes 
c         nc       integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c         ipt      integer pointer vector of length ncolor+1
c                   giving the starting locations of new block
c                   rows
c         dfac     array for diagonal block factorization
c         coef     array of matrix coefficients
c         jcnew    integer array of row dimension ncolor giving the
c                   diagonal numbers for each block
c         n        size of system
c         omega    relaxation parameter 
c         u        current solution estimate
c         rhs      right-hand-side
c         unew     updated solution estimate
c
c ... specifications for parameters
c
      integer   jcnew(ncol,1), iblock(3,ncol,2), lbhb(1), nc(1),
     a          ipt(1)
      dimension dfac(ldf,1), coef(ndim,2), u(1), rhs(1), unew(1)
c
      n = nn
      ncolor = ncol 
c
c ... rhs = ((1-w)/w)*d*un + (rhs - u*un)
c
      ndt = iblock (3,1,1) - 1
      ndb = iblock (3,1,2)
      nwdiag = ndt + ndb + 1
      do 15 k = 1,ncolor
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nc(k) 
         do 10 j = 3,jlim
            jcol = k + iblock(1,k,j)
            if (jcol .le. k .or. jcol .gt. ncolor) go to 10 
            jstb = iblock(2,k,j) + nwdiag
            mb = iblock(3,k,j)
            inc = ipt(jcol) - ipt(k)
            nb = nc(jcol)
            istb = ist + inc
            call vsubd (ndim,ncolor,na,nb,mb,coef(ist,jstb),
     a                  jcnew(k,jstb),rhs(ist),u(istb),inc) 
 10      continue
 15   continue
      ind = ndt + 2 
      call bmuln (ndim,n,ndt,ndb,coef,coef(1,2),coef(1,ind),u,unew)
      con = (1.0d0 - omega)/omega
      do 20 i = 1,n 
 20   unew(i) = con*unew(i) + rhs(i)
c
c ... unew = inv((1/w)*d + l)*rhs
c
      do 45 k = 1,ncolor
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nc(k) 
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         ied = ist + na - 1
         do 25 j = 3,jlim
            jcol = k + iblock(1,k,j)
            if (jcol .ge. k .or. jcol .le. 0) go to 25
            jstb = iblock(2,k,j) + nwdiag
            mb = iblock(3,k,j)
            inc = ipt(jcol) - ipt(k)
            nb = nc(jcol)
            istb = ist + inc
            call vsubd (ndim,ncolor,na,nb,mb,coef(ist,jstb),
     a                  jcnew(k,jstb),unew(ist),unew(istb),inc)
 25      continue
         if (ndt + ndb .ge. 1) go to 35 
         do 30 i = ist,ied
 30      unew(i) = omega*dfac(i,1)*unew(i)
         go to 45
 35      call bdsol (ldf,na,nsize,ndt,ndb,dfac(ist,1),unew(ist),
     a               unew(ist),1)
         do 40 i = ist,ied
 40      unew(i) = omega*unew(i)
 45   continue
      return
      end 
      subroutine srbs (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... srbs does an sor back solve (natural ordering,
c     diagonal storage).
c
c        (i + omega*inv(d)*t)*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        x      on input, x contains y
c               on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      if (maxt .le. 0) return 
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 60
c
c ... diagonal-wise algorithm.
c
      do 20 i = 1,maxt
 20   iwksp(i) = n - jt(i)
c
c ... determine nc, imax.
c
 25   nc = 1
      do 30 i = 1,maxt
         nterm = iwksp(i) + 1 
         if (nterm .le. nc) go to 30
         nc = nterm 
         imax = i
 30   continue
      if (nc .le. 1) return
      ndel = jt(imax)
      iend = nc - 1 
      if (ndel .gt. 1) go to 50
c
c ... special case for first super diagonal.
c
      nc1 = 1
      do 40 i = 1,maxt
         if (i .eq. imax) go to 40
         if (iwksp(i) .gt. nc1) nc1 = iwksp(i)
 40   continue
      iwksp(imax) = nc1 - 1
      do 45 k = iend,nc1,-1
 45   x(k) = x(k) - omega*t(k,imax)*x(k+1)/d(k)
      go to 25
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imax) = iwksp(imax) - ndel
      ibeg = max (iend - ndel,0) + 1
      do 55 i = ibeg,iend
 55   x(i) = x(i) - omega*t(i,imax)*x(i+ndel)/d(i)
      go to 25
c
c ... rowwise algorithm.
c
 60   do 75 i = n,1,-1
         do 65 j = 1,maxt
 65      iwksp(j) = min (n,i+jt(j))
         sum = 0.0d0
         do 70 j = 1,maxt
 70      sum = sum + t(i,j)*x(iwksp(j)) 
         x(i) = x(i) - omega*sum/d(i)
 75   continue
      return
      end 
      subroutine srbst (ndim,nn,maxbb,jb,d,b,omega,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... srbst does an sor transpose back solve (natural ordering,
c     diagonal storage).
c
c        (i + omega*inv(d)*(b**t))*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of b array
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxb
c        x      on input, x contains y
c               on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), b(ndim,1)
      integer   jb(1), iwksp(1)
c
      n = nn
      maxb = maxbb
      if (maxb .lt. 1) return 
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 70
c
c ... diagonal-wise algorithm.
c
      do 15 i = 1,maxb
 15   iwksp(i) = n + jb(i)
c
c ... determine nc, imax.
c
 20   nc = 1
      do 25 i = 1,maxb
         nterm = iwksp(i) + 1 
         if (nterm .le. nc) go to 25
         nc = nterm 
         imax = i
 25   continue
      if (nc .le. 1) return
      ndel = -jb(imax)
      iend = nc - 1 
      if (ndel .gt. 1) go to 50
c
c ... special case for first sub diagonal.
c
      nc1 = 1
      do 30 i = 1,maxb
         if (i .eq. imax) go to 30
         if (iwksp(i) .gt. nc1) nc1 = iwksp(i)
 30   continue
      iwksp(imax) = nc1 - 1
      do 45 k = iend,nc1,-1
 45   x(k) = x(k) - omega*b(k+1,imax)*x(k+1)/d(k) 
      go to 20
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imax) = iwksp(imax) - ndel
      ibeg = max (iend - ndel,0) + 1
      do 65 i = ibeg,iend
 65   x(i) = x(i) - omega*b(i+ndel,imax)*x(i+ndel)/d(i)
      go to 20
c
c ... rowwise algorithm.
c
 70   do 85 i = n,2,-1
         do 75 j = 1,maxb
 75      iwksp(j) = max (1,i+jb(j))
         term = omega*x(i)
         do 80 j = 1,maxb
 80      x(iwksp(j)) = x(iwksp(j)) - b(i,j)*term/d(iwksp(j))
 85   continue
      return
      end 
      subroutine srfs (ndim,nn,maxbb,jb,d,b,omega,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... srfs does an sor forward solve (natural ordering,
c     diagonal storage).
c
c        (i + omega*b*inv(d))*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of b array
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxb
c        x      on input, x contains y
c               on output, x is the solution to forward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), b(ndim,1)
      integer   jb(1), iwksp(1)
c
c
      n = nn
      maxb = maxbb
      if (maxb .le. 0) return 
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 60
c
c ... diagonal-wise algorithm.
c
      do 20 i = 1,maxb
 20   iwksp(i) = 1 - jb(i)
c
c ... determine nc, imin.
c
 25   nc = n
      do 30 i = 1,maxb
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 30
         nc = nterm 
         imin = i
 30   continue
      if (nc .ge. n) return
      ndel = -jb(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 50
c
c ... special case for first minor subdiagonal.
c
      nc1 = n
      do 40 i = 1,maxb
         if (i .eq. imin) go to 40
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 40   continue
      iwksp(imin) = nc1 + 1
      do 45 j = ibeg,nc1
 45   x(j) = x(j) - omega*b(j,imin)*x(j-1)/d(j-1) 
      go to 25
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imin) = iwksp(imin) + ndel
      iend = min (ibeg+ndel-1,n)
      do 55 i = ibeg,iend
 55   x(i) = x(i) - omega*b(i,imin)*x(i-ndel)/d(i-ndel)
      go to 25
c
c ... rowwise algorithm.
c
 60   do 75 i = 1,n 
         do 65 j = 1,maxb
 65      iwksp(j) = max (1,i+jb(j))
         sum = 0.0d0
         do 70 j = 1,maxb
 70      sum = sum + b(i,j)*x(iwksp(j))/d(iwksp(j))
         x(i) = x(i) - omega*sum
 75   continue
      return
      end 
      subroutine srfst (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... srfst does an sor transpose forward solve (natural ordering,
c     diagonal storage).
c
c        (i + omega*(t**t)*inv(d))*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        x      on input, x contains y
c               on output, x is the solution to forward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
      n = nn
      maxt = maxtt
      if (maxt .lt. 1) return 
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 70
c
c ... diagonal-wise algorithm.
c
      do 15 i = 1,maxt
 15   iwksp(i) = jt(i) + 1
c
c ... determine nc, imin.
c
 20   nc = n
      do 25 i = 1,maxt
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 25
         nc = nterm 
         imin = i
 25   continue
      if (nc .ge. n) return
      ndel = jt(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 50
c
c ... special case for first minor subdiagonal.
c
      nc1 = n
      do 30 i = 1,maxt
         if (i .eq. imin) go to 30
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 30   continue
      iwksp(imin) = nc1 + 1
      do 45 j = ibeg,nc1
 45   x(j) = x(j) - omega*t(j-1,imin)*x(j-1)/d(j-1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imin) = iwksp(imin) + ndel
      iend = min (ibeg+ndel-1,n)
      do 65 i = ibeg,iend
 65   x(i) = x(i) - omega*t(i-ndel,imin)*x(i-ndel)/d(i-ndel)
      go to 20
c
c ... rowwise algorithm.
c
 70   do 85 i = 1,n 
         do 75 j = 1,maxt
 75      iwksp(j) = min (n,i+jt(j))
         term = omega*x(i)/d(i)
         do 80 j = 1,maxt
 80      x(iwksp(j)) = x(iwksp(j)) - t(i,j)*term
 85   continue
      return
      end 
      subroutine srbsp (ndim,nn,maxt,jt,d,t,omega,x)
      implicit double precision (a-h, o-z)
c
c ... srbsp does an sor backward solve (natural ordering,
c     purdue storage).
c        ((1/omega)*d + t)*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        omega  relaxation factor
c        x      on input, x contains y
c               on output, x is the solution to backward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), t(ndim,1)
      integer   jt(ndim,1)
c
      n = nn
      if (maxt .ge. 1) go to 15
      do 10 i = 1,n 
 10   x(i) = omega*x(i)/d(i)
      return
 15   do 30 i = n,1,-1
         sum = x(i) 
         do 25 j = 1,maxt
            sum = sum - t(i,j)*x(jt(i,j))
 25      continue
         x(i) = omega*sum/d(i)
 30   continue
      return
      end 
      subroutine srbstp (ndim,nn,maxb,jb,d,b,omega,x)
      implicit double precision (a-h, o-z)
c
c ... srbstp does an sor transpose back solve
c     (natural ordering, purdue storage).
c        ((1/omega)*d + (b**t))*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of b array
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  over-relaxation factor
c        x      on input, x contains y
c        x      on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), b(ndim,1)
      integer   jb(ndim,1)
c
      n = nn
      if (maxb .ge. 1) go to 15
      do 10 i = 1,n 
 10   x(i) = omega*x(i)/d(i)
      return
 15   do 30 i = n,1,-1
         x(i) = omega*x(i)/d(i)
         term = x(i)
         do 25 j = 1,maxb
            x(jb(i,j)) = x(jb(i,j)) - b(i,j)*term 
 25      continue
 30   continue
      return
      end 
      subroutine srfsp (ndim,nn,maxb,jb,d,b,omega,x)
      implicit double precision (a-h, o-z)
c
c ... srfsp does an sor forward solve (natural ordering,
c     purdue storage).
c        ((1/omega)*d + b)*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of b array
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  relaxation factor
c        x      on input, x contains y
c               on output, x is the solution to forward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), b(ndim,1)
      integer   jb(ndim,1)
c
      n = nn
      if (maxb .ge. 1) go to 15
      do 10 i = 1,n 
 10   x(i) = omega*x(i)/d(i)
      return
 15   do 30 i = 1,n 
         sum = x(i) 
         do 25 j = 1,maxb
            sum = sum - b(i,j)*x(jb(i,j))
 25      continue
         x(i) = omega*sum/d(i)
 30   continue
      return
      end 
      subroutine srfstp (ndim,n,maxt,jt,d,t,omega,x)
      implicit double precision (a-h, o-z)
c
c ... srfstp does an sor transpose forward solve
c     (natural ordering, purdue storage).
c        ((1/omega)*d + (t**t))*x = y
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        omega  over-relaxation factor
c        x      on input, x contains y
c               on output, x is the solution to forward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), t(ndim,1)
      integer   jt(ndim,1)
c
      if (maxt .ge. 1) go to 15
      do 10 i = 1,n 
 10   x(i) = omega*x(i)/d(i)
      return
 15   do 30 i = 1,n 
         x(i) = omega*x(i)/d(i)
         term = x(i)
         do 25 j = 1,maxt
            x(jt(i,j)) = x(jt(i,j)) - t(i,j)*term 
 25      continue
 30   continue
      return
      end 
      subroutine srs (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srs does an ssor solution (natural ordering,
c     symmetric diagonal storage).
c
c        con*(i + w*(t**t)*inv(d))*d*(i + w*inv(d)*t)*x = y 
c         con = 1/(w*(2-w))   and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      fac = omega*(2.0d0 - omega)
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfst (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*x(i)/d(i)
      call srbs (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srs1 (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srs1 does an ssor forward solve (natural ordering,
c     symmetric diagonal storage).
c
c        con*(i + w*(t**t)*inv(d))*d*x = y
c         con = 1/(w*(2-w))   and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      fac = omega*(2.0d0 - omega)
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfst (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*x(i)/d(i)
      return
      end 
      subroutine srs2 (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srs2 does an ssor back solve (natural ordering,
c     symmetric diagonal storage).
c
c        (i + w*inv(d)*t)*x = y
c            w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srbs (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srs3 (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srs3 does an ssor transpose forward solve (natural ordering,
c     symmetric diagonal storage).
c
c        con*d*(i + w*inv(d)*t)*x = y
c         con = 1/(w*(2-w))   and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      fac = omega*(2.0d0 - omega)
      do 10 i = 1,n 
 10   x(i) = fac*y(i)/d(i)
      call srbs (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srs4 (ndim,nn,maxtt,jt,d,t,omega,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srs4 does an ssor transpose back solve (natural ordering,
c     symmetric diagonal storage).
c
c        (i + w*(t**t)*inv(d))*x = y
c            w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfst (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srsn (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,omega,
     a                 irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsn does an ssor solution (natural ordering,
c     nonsymmetric diagonal storage).
c
c        con*(i + w*b*inv(d))*d*(i + w*inv(d)*t)*x = y
c         where  con = 1/(w*(2-w))  and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1), b(ndim,1)
      integer   jt(1), jb(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      fac = omega*(2.0d0 - omega)
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfs (ndim,n,maxb,jb,d,b,omega,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*x(i)/d(i)
      call srbs (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srsnt (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,omega,
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsnt does a transpose ssor solution (natural ordering,
c     nonsymmetric diagonal storage).
c
c       con*(i + w*(t**t)*inv(d))*d*(i + w*inv(d)*(b**t))*x = y
c        con = 1/(w*(2-w))  and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1), b(ndim,1)
      integer   jt(1), jb(1), iwksp(1)
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      fac = omega*(2.0d0 - omega)
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfst (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*x(i)/d(i)
      call srbst (ndim,n,maxb,jb,d,b,omega,irwise,iwksp,x)
      return
      end 
      subroutine srsn1 (ndim,n,maxb,jb,d,b,omega, 
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsn1 does an ssor forward pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c        con*(i + w*b*inv(d))*d*(i + w*inv(d)*t)*x = y
c         where  con = 1/(w*(2-w))  and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), b(ndim,1)
      integer   jb(1), iwksp(1)
c
c
      fac = omega*(2.0d0 - omega)
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfs (ndim,n,maxb,jb,d,b,omega,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*x(i)/d(i)
      return
      end 
      subroutine srsn2 (ndim,n,maxt,jt,d,t,omega, 
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsn2 does an ssor backward pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c        con*(i + w*b*inv(d))*d*(i + w*inv(d)*t)*x = y
c         where  con = 1/(w*(2-w))  and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srbs (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srsn3 (ndim,n,maxb,jb,d,b,omega, 
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsn3 does a transpose ssor back pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c       con*(i + w*(t**t)*inv(d))*d*(i + w*inv(d)*(b**t))*x = y
c        con = 1/(w*(2-w))  and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the sub-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), b(ndim,1)
      integer   jb(1), iwksp(1)
c
c
      fac = omega*(2.0d0 - omega)
      do 15 i = 1,n 
 15   x(i) = fac*y(i)/d(i)
      call srbst (ndim,n,maxb,jb,d,b,omega,irwise,iwksp,x)
      return
      end 
      subroutine srsn4 (ndim,n,maxt,jt,d,t,omega, 
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsn4 does a transpose ssor forward pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c       con*(i + w*(t**t)*inv(d))*d*(i + w*inv(d)*(b**t))*x = y
c        con = 1/(w*(2-w))  and  w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the super-
c                diagonals of the matrix
c        omega  over-relaxation factor
c        irwise rowwise algorithm switch
c                = 0  use diagonal algorithm
c                = 1  use row-wise algorithm
c        iwksp  integer workspace of length maxt
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(1), iwksp(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfst (ndim,n,maxt,jt,d,t,omega,irwise,iwksp,x)
      return
      end 
      subroutine srsp (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,omega,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsp does an ssor solution (natural ordering,
c     purdue storage).
c        con*((1/w)*d + b)*inv(d)*((1/w)*d + t)*x = y
c        where con = w/(2-w) and w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t,b arrays
c        n      order of system
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  relaxation factor
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1), b(ndim,1)
      integer   jt(ndim,1), jb(ndim,1)
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      fac = (2.0d0 - omega)/omega
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfsp (ndim,n,maxb,jb,d,b,omega,x)
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*x(i)
      call srbsp (ndim,n,maxt,jt,d,t,omega,x)
      return
      end 
      subroutine srsp1 (ndim,n,maxb,jb,d,b,omega,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsp1 does an ssor forward solve (natural ordering,
c     purdue storage).
c
c ... parameters -- 
c
c        ndim   row dimension of t,b arrays
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  relaxation factor
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), b(ndim,1)
      integer   jb(ndim,1)
c
c
      fac = (2.0d0 - omega)/omega
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfsp (ndim,n,maxb,jb,d,b,omega,x)
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*x(i)
      return
      end 
      subroutine srsp2 (ndim,n,maxt,jt,d,t,omega,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsp2 does an ssor back solve (natural ordering,
c     purdue storage).
c
c ... parameters -- 
c
c        ndim   row dimension of t,b arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        omega  relaxation factor
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(ndim,1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srbsp (ndim,n,maxt,jt,d,t,omega,x)
      return
      end 
      subroutine srsp3 (ndim,n,maxb,jb,d,b,omega,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsp3 does an ssor transpose back solve (natural ordering,
c     purdue storage).
c
c ... parameters -- 
c
c        ndim   row dimension of t,b arrays
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  relaxation factor
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), b(ndim,1)
      integer   jb(ndim,1)
c
c
      fac = (2.0d0 - omega)/omega
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*y(i)
      call srbstp (ndim,n,maxb,jb,d,b,omega,x)
      return
      end 
      subroutine srsp4 (ndim,n,maxt,jt,d,t,omega,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsp4 does an ssor transpose forward solve (natural ordering,
c     purdue storage).
c
c ... parameters -- 
c
c        ndim   row dimension of t,b arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        omega  relaxation factor
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1)
      integer   jt(ndim,1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfstp (ndim,n,maxt,jt,d,t,omega,x)
      return
      end 
      subroutine srsntp (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,omega,y,x)
      implicit double precision (a-h, o-z)
c
c ... srsntp does an ssor transpose solution (natural ordering,
c     purdue storage).
c        con*((1/w)*d + (t**t))*inv(d)*((1/w)*d + (b**t))*x = y
c        where con = w/(2-w) and w = omega
c
c ... parameters -- 
c
c        ndim   row dimension of t,b arrays
c        n      order of system
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the matrix
c        t      array of active size n by maxt giving the upper
c                triangle of the matrix 
c        b      array of active size n by maxb giving the lower
c                triangle of the matrix 
c        omega  relaxation factor
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndim,1), b(ndim,1)
      integer   jt(ndim,1), jb(ndim,1)
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      fac = (2.0d0 - omega)/omega
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfstp (ndim,n,maxt,jt,d,t,omega,x)
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*x(i)
      call srbstp (ndim,n,maxb,jb,d,b,omega,x)
      return
      end 
      subroutine ssorad (ssorcp,coef,jcoef,wfac,jwfac,n,p,z,r,icode)
      implicit double precision (a-h, o-z)
c
c ... ssorad does the ssor adaptive process.
c
c ... parameters -- 
c
c         n       order of system
c         p,z,r   vectors from acceleration algorithm
c         icode   key for restarting iteration
c                  = 0    omega unchanged (no restart)
c                  = 1    new omega       (restart needed)
c
c ... specifications for parameters
c
      dimension p(1), z(1), r(1), coef(1), jcoef(2), wfac(1),
     a          jwfac(1)
      external ssorcp
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      logical           halt, maxadp, minadp, maxadd, minadd
      common / itcom2 / halt, maxadp, minadp, maxadd, minadd
      common / itcom3 / alpha, beta, zeta, emax, emin, pap, 
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- package common 
c
c
c------------------------------------------------------------------
c  parameter estimation formulas
c------------------------------------------------------------------
c
      alp (w,beta,s) = ((1.0d0 + beta*w*w)*s - w*(2.0d0 - w)) / 
     a                            (w*(2.0d0 - w - s))
c
      omg (alpha,beta) = 2.0d0/(1.0d0 + sqrt (1.0d0 + 2.0d0*alpha + 
     a                         4.0d0*beta))
c
      se (w,alpha,beta) = ((1.0d0 + alpha)*w*(2.0d0 - w)) /
     a                       (1.0d0 + alpha*w + beta*w*w)
c
      cond (w,alpha,beta) = 1.0d0/se(w,alpha,beta)
c
      rc (w,alpha,beta) = dlog ((sqrt (cond(w,alpha,beta))+1.0d0) /
     a                          (sqrt (cond(w,alpha,beta))-1.0d0))
c
c------------------------------------------------------------------
c
      icode = 0
      if (is .ge. 6  .and.  (.not. minadp)) go to 5
      tmo = 2.0d0 - omega
      if (emin .lt. tmo) alphab = min (alphab, alp(omega,betab,emin))
 5    if ((.not. omgadp) .or. (.not. minadp) .or. (is .le. 5)) return 
      omegab = max (1.0d0, omg (alphab,betab))
      if (rc(omega,alphab,betab) .gt. fff*rc(omegab,alphab,betab))
     a            return
      if (iacel .eq. 2) pap = vdot (n,p,z)
      call omgchg (ssorcp,coef,jcoef,wfac,jwfac,n,p,r)
      omega = max (1.0d0,omg(alphab,betab))
      icode = 1
      if (level .ge. 2) write (nout,10) in, alphab, betab, omega
 10   format (/1x,15x,36hparameters were changed at iteration,i7/
     a        1x,20x,19halphab             ,f15.9/
     a        1x,20x,19hbetab              ,f15.9/
     a        1x,20x,19homega              ,f15.9/)
      return
      end 
      subroutine ssord (ndim,maxt,jt,d,t,nn,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssord computes  pdp = (p,d*p)  and
c                     pldup = (p,l*inv(d)*u*p)
c
c     for symmetric diagonal storage format.
c
c ... parameters -- 
c
c         ndim    row dimension of coef array in defining routine
c         maxt    number of diagonals in t
c         jt      diagonal numbers for upper triangular part
c         d       diagonal
c         t       upper triangular diagonals
c         n       order of system
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c         pdp     (p,d*p)
c         pldup   (p,l*d*u*p) 
c
c ... specifications for parameters
c
      integer   jt(1)
      dimension d(1), t(ndim,1), p(1), r(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*d(i)*p(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p) = (u*p,inv(d)*u*p)
c
      pldup = 0.0d0
      if (maxt .le. 0) return 
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      call vaddd (ndim,1,n,n,maxt,t,jt,r,p,0)
      sum = 0.0d0
      do 20 i = 1,n 
 20   sum = sum + r(i)*r(i)/d(i)
      pldup = sum
      return
      end 
      subroutine ssordn (ndim,maxt,maxb,jt,jb,d,t,b,nn,p,r, 
     a                   wksp,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssordn computes  pdp = (p,d*p)  and
c                      pldup = (p,l*inv(d)*u*p)
c
c     for nonsymmetric diagonal storage format.
c
c ... parameters -- 
c
c         ndim    row dimension of coef array in defining routine
c         maxt    number of diagonals in t
c         maxb    number of diagonals in b
c         jt      diagonal numbers for upper triangular part
c         jb      diagonal numbers for lower triangular part
c         d       diagonal
c         t       upper triangular diagonals
c         b       lower triangular diagonals
c         n       order of system
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c         wksp    workspace vector of length n
c         pdp     (p,d*p)
c         pldup   (p,l*d*u*p) 
c
c ... specifications for parameters
c
      integer   jt(1), jb(1)
      dimension d(1), t(ndim,1), b(ndim,1), p(1), r(1), wksp(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*d(i)*p(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p)
c
      pldup = 0.0d0
      if (maxt .le. 0 .or. maxb .le. 0) return
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      call vaddd (ndim,1,n,n,maxt,t,jt,r,p,0)
      do 20 i = 1,n 
 20   r(i) = r(i)/d(i)
      do 25 i = 1,n 
 25   wksp(i) = 0.0d0 
      call vaddd (ndim,1,n,n,maxb,b,jb,wksp,r,0)
      sum = 0.0d0
      do 30 i = 1,n 
 30   sum = sum + p(i)*wksp(i)
      pldup = sum
      return
      end 
      subroutine ssorp (ndim,maxt,jt,d,t,nn,p,r,wksp,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssorp computes  pdp = (p,d*p)  and
c                     pldup = (p,l*inv(d)*u*p)
c
c     for symmetric purdue storage format.
c
c ... parameters -- 
c
c         ndim    row dimension of coef array in defining routine
c         maxt    number of columns in t
c         jt      column numbers for upper triangular part
c         d       diagonal
c         t       upper triangular part of a
c         n       order of system
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c         wksp    workspace vector of length n
c                  (keygs = 1 only)
c         pdp     (p,d*p)
c         pldup   (p,l*d*u*p) 
c
c ... specifications for parameters
c
      integer   jt(ndim,1)
      dimension d(1), t(ndim,1), p(1), r(1), wksp(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*d(i)*p(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p) = (u*p,inv(d)*u*p)
c
      pldup = 0.0d0
      if (maxt .le. 0) return 
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      call vaddp (ndim,ndim,n,maxt,t,jt,r,p,wksp) 
      sum = 0.0d0
      do 20 i = 1,n 
 20   sum = sum + r(i)*r(i)/d(i)
      pldup = sum
      return
      end 
      subroutine ssorpn (ndimm,maxt,maxb,jt,jb,d,t,b,nn,p,r,
     a                   wksp,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssorpn computes  pdp = (p,d*p)  and
c                      pldup = (p,l*inv(d)*u*p)
c
c     for nonsymmetric purdue storage format.
c
c ... parameters -- 
c
c         ndim    row dimension of coef array in defining routine
c         maxt    number of columns in t
c         maxb    number of columns in b
c         jt      column numbers for upper triangular part
c         jb      column numbers for lower triangular part
c         d       diagonal
c         t       upper triangular part 
c         b       lower triangular part 
c         n       order of system
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c         wksp    workspace vector of length n
c                  2*n if keygs = 1
c         pdp     (p,d*p)
c         pldup   (p,l*d*u*p) 
c
c ... specifications for parameters
c
      integer   jt(ndimm,1), jb(ndimm,1)
      dimension d(1), t(ndimm,1), b(ndimm,1), p(1), r(1), wksp(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      ndim = ndimm
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*d(i)*p(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p)
c
      pldup = 0.0d0
      if (maxt .le. 0 .or. maxb .le. 0) return
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      call vaddp (ndim,ndim,n,maxt,t,jt,r,p,wksp) 
      do 20 i = 1,n 
 20   r(i) = r(i)/d(i)
      do 25 i = 1,n 
 25   wksp(i) = 0.0d0 
      np1 = n + 1
      call vaddp (ndim,ndim,n,maxb,b,jb,wksp,r,wksp(np1))
      sum = 0.0d0
      do 30 i = 1,n 
 30   sum = sum + p(i)*wksp(i)
      pldup = sum
      return
      end 
      subroutine ssrcd (ldf,ndim,maxnz,nsize,iblock,dfac,coef,
     a                  jcoef,nn,p,r,wksp,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssrcd computes  pdp = (p,d*p)  and
c                     pldup = (p,l*inv(d)*u*p)
c
c     for symmetric block diagonal storage format.
c
c ... parameters -- 
c
c         ldf      row dimension of dfac
c         ndim     row dimension of coef array
c         maxnz    number of diagonals stored in coef
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         dfac     array for diagonal block factorization
c         coef     array for matrix coefficients
c         jcoef    vector for diagonal numbers
c         n        size of system
c         p        vector from acceleration algorithm
c         r        workspace vector from acceleration algorithm
c         wksp     workspace vector of length n
c         pdp      (p,d*p)
c         pldup    (p,l*d*u*p)
c
c ... specifications for parameters
c
      integer   jcoef(2), iblock(3,1)
      dimension dfac(ldf,1), coef(ndim,2), p(1), r(1), wksp(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      nwdiag = iblock (3,1)
      nt = nwdiag - 1
      call bmul (ndim,n,nt,coef,coef(1,2),p,r)
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*r(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p) = (u*p,inv(d)*u*p)
c
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      jbgn = nwdiag + 1
      mdiag = maxnz - nwdiag
      call vaddd (ndim,1,n,n,mdiag,coef(1,jbgn),jcoef(jbgn),
     a            r,p,0)
      call bdsol (ldf,n,nsize,nt,0,dfac,r,wksp,0) 
      sum = 0.0d0
      do 25 i = 1,n 
 25   sum = sum + r(i)*wksp(i)
      pldup = sum
      return
      end 
      subroutine ssrcdm (ldf,ndim,lbhb,nsize,ncol,nci,ipt,
     a                   iblock,dfac,coef,jcnew,nn,p,r,wksp,
     a                   pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssrcdm computes  pdp = (p,d*p)  and
c                      pldup = (p,l*inv(d)*u*p)
c
c     for nonsymmetric block diagonal storage format.
c     (nonconstant block size)
c
c ... parameters -- 
c
c         ldf      row dimension of dfac array
c         ndim     row dimension of coef array
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         ncolor   number of distinct block sizes 
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c         ipt      integer pointer vector of length ncolor+1
c                   giving the starting locations of new block
c                   rows
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         dfac     array for diagonal block factorization
c         coef     array of matrix coefficients
c         jcnew    integer array of row dimension ncolor giving the
c                   diagonal numbers for each block
c         n        size of system
c         p        vector from acceleration algorithm
c         r        workspace vector from acceleration algorithm
c         wksp     workspace vector of length n
c         pdp      (p,d*p)
c         pldup    (p,l*d*u*p)
c
c ... specifications for parameters
c
      integer   jcnew(ncol,1), iblock(3,ncol,2), lbhb(1),
     a          nci(1), ipt(1)
      dimension dfac(ldf,1), coef(ndim,2), p(1), r(1), wksp(1)
c
c ... define constants ndt, ndb.
c
      n = nn
      ncolor = ncol 
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      nwdiag = ndt + ndb + 1
c
c ... compute pdp = (p,d*p).
c
      ind = ndt + 2 
      call bmuln (ndim,n,ndt,ndb,coef,coef(1,2),coef(1,ind),p,r)
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*r(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p)
c
      do 15 i = 1,n 
         r(i) = 0.0d0 
         wksp(i) = 0.0d0
 15   continue
      do 25 k = 1,ncolor
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         do 20 j = 3,jlim
            jcol = k + iblock(1,k,j)
            if (jcol .le. k) go to 20
            jstb = iblock(2,k,j) + nwdiag
            mb = iblock(3,k,j)
            inc = ipt(jcol) - ipt(k)
            nb = nci(jcol)
            istb = ist + inc
            call vaddd (ndim,ncolor,na,nb,mb,coef(ist,jstb),
     a                  jcnew(k,jstb),r(ist),p(istb),inc)
 20      continue
 25   continue
      call bdsol (ldf,n,nsize,ndt,ndb,dfac,r,r,1) 
      do 35 k = 1,ncolor
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         do 30 j = 3,jlim
            jcol = k + iblock(1,k,j)
            if (jcol .ge. k) go to 30
            jstb = iblock(2,k,j) + nwdiag
            mb = iblock(3,k,j)
            inc = ipt(jcol) - ipt(k)
            nb = nci(jcol)
            istb = ist + inc
            call vaddd (ndim,ncolor,na,nb,mb,coef(ist,jstb),
     a                  jcnew(k,jstb),wksp(ist),r(istb),inc)
 30      continue
 35   continue
      sum = 0.0d0
      do 40 i = 1,n 
 40   sum = sum + p(i)*wksp(i)
      pldup = sum
      return
      end 
      subroutine ssrcdn (ldf,ndim,lbhb,nsize,iblock,dfac,coef,
     a                   jcoef,nn,p,r,wksp,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssrcdn computes  pdp = (p,d*p)  and
c                      pldup = (p,l*inv(d)*u*p)
c
c     for nonsymmetric block diagonal storage format.
c     (constant block size)
c
c ... parameters -- 
c
c         ldf      row dimension of dfac
c         ndim     row dimension of coef array
c         lbhb     number of blocks per block row 
c         nsize    size of an individual subsystem within a 
c                   diagonal block
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         dfac     array for diagonal block factorization
c         coef     array for matrix coefficients
c         jcoef    vector for diagonal numbers
c         n        size of system
c         p        vector from acceleration algorithm
c         r        workspace vector from acceleration algorithm
c         wksp     workspace vector of length n
c         pdp      (p,d*p)
c         pldup    (p,l*d*u*p)
c
c ... specifications for parameters
c
      integer   jcoef(2), iblock(3,2)
      dimension dfac(ldf,1), coef(ndim,2), p(1), r(1), wksp(1)
c
c ... compute nt, nb, maxt, maxb
c
      n = nn
      nt = iblock(3,1) - 1
      nb = iblock(3,2)
      maxt = 0
      maxb = 0
      if (lbhb .lt. 3) go to 15
      do 10 j = 3,lbhb
         ind = iblock(1,j)
         if (ind .gt. 0) maxt = maxt + iblock(3,j)
         if (ind .lt. 0) maxb = maxb + iblock(3,j)
 10   continue
c
c ... compute pdp = (p,d*p).
c
 15   ind = nt + 2
      call bmuln (ndim,n,nt,nb,coef,coef(1,2),coef(1,ind),p,r)
      sum = 0.0d0
      do 20 i = 1,n 
 20   sum = sum + p(i)*r(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p)
c
      do 25 i = 1,n 
         wksp(i) = 0.0d0
         r(i) = 0.0d0 
 25   continue
      ind = nt + nb + 2
      indd = ind + maxt
      call vaddd (ndim,1,n,n,maxt,coef(1,ind),jcoef(ind),
     a            r,p,0)
      call bdsol (ldf,n,nsize,nt,nb,dfac,r,r,1)
      call vaddd (ndim,1,n,n,maxb,coef(1,indd),jcoef(indd), 
     a           wksp,r,0)
      sum = 0.0d0
      do 30 i = 1,n 
 30   sum = sum + p(i)*wksp(i)
      pldup = sum
      return
      end 
      subroutine srbscp (ndim,n,jc,d,c,ncolor,nc,nt,omega,
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... srbscp does a back sor solve.
c     (purdue storage, multicolor)
c
c     ((1/w)*d + t)*x = y
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          x      on input, x contains y
c                 on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1)
      dimension d(1), c(ndim,1), x(1), wksp(1)
c
      ied = n
      do 20 icol = ncolor,1,-1
         npt = nc(icol)
         ist = ied - npt + 1
         j2 = nt(icol)
         call vsubp (ndim,ndim,npt,j2,c(ist,1),jc(ist,1),x(ist),x,wksp)
         do 15 i = ist,ied
 15      x(i) = omega*x(i)/d(i)
         ied = ied - npt
 20   continue
      return
      end 
      subroutine srbsct (ndim,n,jc,d,c,ncolor,nc,nt,nb,omega,
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... srbsct does a transpose back sor solve.
c     (purdue storage, multicolor)
c
c     ((1/w)*d + (b**t))*x = y
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length max(nc(i))
c          x      on input, x contains y
c                 on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndim,1), x(1), wksp(1)
c
      ied = n
      do 20 icol = ncolor,1,-1
         npt = nc(icol)
         ist = ied - npt + 1
         do 15 i = ist,ied
 15      x(i) = omega*x(i)/d(i)
         j1 = nt(icol) + 1
         mj = nb(icol)
         call vsubpt (ndim,ndim,npt,mj,c(ist,j1),jc(ist,j1),x,x(ist), 
     a                wksp)
         ied = ied - npt
 20   continue
      return
      end 
      subroutine srfscp (ndim,jc,d,c,ncolor,nc,nt,nb,omega, 
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... srfscp does a forward sor solve.
c     (purdue storage, multicolor)
c
c     ((1/w)*d + b)*x = y
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          x      on input, x contains y
c                 on output, x is the solution to the forward solve
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndim,1), x(1), wksp(1)
c
      ist = 1
      do 20 icol = 1,ncolor
         npt = nc(icol)
         ied = ist + npt - 1
         j1 = nt(icol) + 1
         mj = nb(icol)
         call vsubp (ndim,ndim,npt,mj,c(ist,j1),jc(ist,j1),x(ist),x,
     a               wksp)
         do 15 i = ist,ied
 15      x(i) = omega*x(i)/d(i)
         ist = ist + npt
 20   continue
      return
      end 
      subroutine srfsct (ndim,jc,d,c,ncolor,nc,nt,omega,
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... srfsct does a transpose forward sor solve.
c     (purdue storage, multicolor)
c
c     ((1/w)*d + (t**t))*x = y
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length max(nc(i))
c          x      on input, x contains y
c                 on output, x is the solution to the forward solve
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1)
      dimension d(1), c(ndim,1), x(1), wksp(1)
c
      ist =  1
      do 20 icol = 1,ncolor
         npt = nc(icol)
         ied = ist + npt - 1
         do 15 i = ist,ied
 15      x(i) = omega*x(i)/d(i)
         j2 = nt(icol)
         call vsubpt (ndim,ndim,npt,j2,c(ist,1),jc(ist,1),x,x(ist),
     a                wksp)
         ist = ist + npt
 20   continue
      return
      end 
      subroutine srscp (ndim,nn,jc,d,c,ncolor,nc,nt,nb,omega,
     a                  wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srscp does an ssor solve.
c     (purdue storage, multicolor)
c        con*((1/w)*d + b)*inv(d)*((1/w)*d + t)*x = y
c        where con = w/(2-w) and w = omega
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndim,1), x(1), y(1), wksp(1)
c
c
      n = nn
      fac = (2.0d0 - omega)/omega
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfscp (ndim,jc,d,c,ncolor,nc,nt,nb,omega,wksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*x(i)
      call srbscp (ndim,n,jc,d,c,ncolor,nc,nt,omega,wksp,x) 
      return
      end 
      subroutine srscpt (ndim,nn,jc,d,c,ncolor,nc,nt,nb,omega,
     a                   wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srscpt does an transpose ssor solve.
c     (purdue storage, multicolor)
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length max(nc(i))
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndim,1), x(1), y(1), wksp(1)
c
c
      n = nn
      fac = (2.0d0 - omega)/omega
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfsct (ndim,jc,d,c,ncolor,nc,nt,omega,wksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*x(i)
      call srbsct (ndim,n,jc,d,c,ncolor,nc,nt,nb,omega,wksp,x)
      return
      end 
      subroutine ssrcp (ndim,jc,d,c,nn,ncolor,nc,nt,p,r,wksp,
     a                  pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssrcp computes  pdp = (p,d*p)  and
c                     pldup = (p,l*inv(d)*u*p)
c
c     for symmetric multicolor purdue storage format.
c
c ... parameters -- 
c
c         ndim    row dimension of c,jc arrays
c         jc      integer array giving the column indices of the
c                  corresponding elements in c
c         d       vector of length n giving the diagonal elements
c                  of the matrix
c         c       array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c         n       order of system
c         ncolor  number of colors used 
c         nc      integer vector of length ncolor giving the number
c                  of nodes for each color
c         nt      integer vector of length ncolor giving the number
c                  of upper columns for each color
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c         wksp    workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c         pdp     (p,d*p)
c         pldup   (p,l*d*u*p) 
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1)
      dimension d(1), c(ndim,1), p(1), r(1), wksp(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*d(i)*p(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p) = (u*p,inv(d)*u*p)
c
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      ist = 1
      do 20 icol = 1,ncolor
         npt = nc(icol)
         mj = nt(icol)
         call vaddp (ndim,ndim,npt,mj,c(ist,1),jc(ist,1),r(ist),p,wksp)
         ist = ist + npt
 20   continue
      sum = 0.0d0
      do 25 i = 1,n 
 25   sum = sum + r(i)*r(i)/d(i)
      pldup = sum
      return
      end 
      subroutine ssrcpn (ndimm,jc,d,c,nn,ncol,nc,nt,nb,p,r,wksp,
     a                   pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... ssrcpn computes  pdp = (p,d*p)  and
c                      pldup = (p,l*inv(d)*u*p)
c
c     for nonsymmetric multicolor purdue storage format.
c
c ... parameters -- 
c
c         ndim    row dimension of c,jc arrays
c         jc      integer array giving the column indices of the
c                  corresponding elements in c
c         d       vector of length n giving the diagonal elements
c                  of the matrix
c         c       array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c         n       order of system
c         ncolor  number of colors used 
c         nc      integer vector of length ncolor giving the number
c                  of nodes for each color
c         nt      integer vector of length ncolor giving the number
c                  of upper columns for each color
c         nb      integer vector of length ncolor giving the number
c                  of lower columns for each color
c         p       vector from acceleration algorithm
c         r       workspace vector from acceleration algorithm
c         wksp    workspace vector of length
c                  n + max(nc(i))     if keygs = 1
c                  n                  if keygs = 2
c         pdp     (p,d*p)
c         pldup   (p,l*d*u*p) 
c
c ... specifications for parameters
c
      integer   jc(ndimm,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimm,1), p(1), r(1), wksp(1)
c
c ... compute pdp = (p,d*p).
c
      n = nn
      ndim = ndimm
      ncolor = ncol 
      sum = 0.0d0
      do 10 i = 1,n 
 10   sum = sum + p(i)*d(i)*p(i)
      pdp = sum
c
c ... compute pldup = (p,l*inv(d)*u*p) = (u*p,inv(d)*u*p)
c
      np1 = n + 1
      do 15 i = 1,n 
 15   r(i) = 0.0d0
      ist = 1
      do 20 icol = 1,ncolor
         npt = nc(icol)
         mj = nt(icol)
         call vaddp (ndim,ndim,npt,mj,c(ist,1),jc(ist,1),r(ist),p,wksp)
         ist = ist + npt
 20   continue
      do 25 i = 1,n 
 25   r(i) = r(i)/d(i)
      do 30 i = 1,n 
 30   wksp(i) = 0.0d0 
      ist = 1
      do 35 icol = 1,ncolor
         npt = nc(icol)
         j1 = nt(icol) + 1
         mj = nb(icol)
         call vaddp (ndim,ndim,npt,mj,c(ist,j1),jc(ist,j1),wksp(ist), 
     a               r,wksp(np1))
         ist = ist + npt
 35   continue
      sum = 0.0d0
      do 40 i = 1,n 
 40   sum = sum + p(i)*wksp(i)
      pldup = sum
      return
      end 
      subroutine srscp1 (ndim,nn,jc,d,c,ncolor,nc,nt,nb,omega,
     a                  wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srscp1 does an ssor forward solve.
c     (purdue storage, multicolor)
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndim,1), x(1), y(1), wksp(1)
c
c
      n = nn
      fac = (2.0d0 - omega)/omega
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfscp (ndim,jc,d,c,ncolor,nc,nt,nb,omega,wksp,x)
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*x(i)
      return
      end 
      subroutine srscp2 (ndim,n,jc,d,c,ncolor,nc,nt,omega,
     a                  wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srscp2 does an ssor back solve.
c     (purdue storage, multicolor)
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1)
      dimension d(1), c(ndim,1), x(1), y(1), wksp(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srbscp (ndim,n,jc,d,c,ncolor,nc,nt,omega,wksp,x) 
      return
      end 
      subroutine srscp3 (ndim,n,jc,d,c,ncolor,nc,nt,nb,omega,
     a                   wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srscp3 does an transpose ssor back solve.
c     (purdue storage, multicolor)
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length max(nc(i))
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndim,1), x(1), y(1), wksp(1)
c
c
      fac = (2.0d0 - omega)/omega
c
      do 15 i = 1,n 
 15   x(i) = fac*d(i)*y(i)
      call srbsct (ndim,n,jc,d,c,ncolor,nc,nt,nb,omega,wksp,x)
      return
      end 
      subroutine srscp4 (ndim,n,jc,d,c,ncolor,nc,nt,omega,
     a                   wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... srscp4 does an transpose ssor forward solve.
c     (purdue storage, multicolor)
c
c ... parameters -- 
c
c          ndim   row dimension of c,jc arrays
c          n      order of system (= nn)
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c          nc     integer vector of length ncolor giving the number
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          omega  over-relaxation factor
c          wksp   workspace vector of length max(nc(i))
c
c ... specifications for parameters
c
      integer   jc(ndim,1), nc(1), nt(1)
      dimension d(1), c(ndim,1), x(1), y(1), wksp(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call srfsct (ndim,jc,d,c,ncolor,nc,nt,omega,wksp,x)
      return
      end 
      double precision function tau (ii)
      implicit double precision (a-h, o-z)
c
c ... tau sets tau(ii) for the sor method.
c
c ... parameters -- 
c
c          ii     number of times parameters have been changed
c
c ... specifications for parameters
c
c
      dimension t(9)
c
      data  t(1), t(2), t(3), t(4), t(5), t(6),  t(7),  t(8), t(9)
     a    / 1.5d0, 1.8d0, 1.85d0, 1.9d0, 1.94d0, 1.96d0, 1.975d0, 
     a      1.985d0, 1.992d0 /
c
      tau = t(9)
      if (ii .le. 8) tau = t(ii)
      return
      end 
      subroutine tbs (n,t,x)
      implicit double precision (a-h, o-z)
c
c ... tbs does a back substitution  (i + t)*x = y  where t is the
c     first super-diagonal.
c
c ... parameters -- 
c
c          n      order of the system
c          t      vector of length n-1 containing the super-
c                  diagonal elements
c          x      on input, x contains y
c                 on output, x contains the solution to (i - t)*x = y 
c
c ... specifications for parameters
c
      dimension t(1), x(1)
c
      do 10 i = n-1,1,-1
 10   x(i) = x(i) - t(i)*x(i+1)
      return
      end 
      subroutine tbsm (nn,nsize,t,x)
      implicit double precision (a-h, o-z)
c
c ... tbsm does a back substitution  (i + t)*x = y  where t 
c     is a super diagonal composed of independent subsystems of
c     size nsize.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  order of the individual subsystems
c          t      linear array of length n-1 containing the super-
c                  diagonal elements of the factorizations
c          x      on input, x contains y
c                 the solution to (i + t)*x = y
c
c ... specifications for parameters
c
      dimension t(nsize,1), x(nsize,1)
c
      n = nn
      nsys = n/nsize
      do 15 i = nsize-1,1,-1
         do 10 j = 1,nsys
 10      x(i,j) = x(i,j) - t(i,j)*x(i+1,j)
 15   continue
      return
      end 
      subroutine tfac (nn,d,t)
      implicit double precision (a-h, o-z)
c
c ... tfac computes a factorization of a single symmetric
c     tridiagonal matrix contained in d and t and replaces it.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of the matrix
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the matrix
c
c ... specifications for parameters
c
      dimension d(1), t(1)
c
      n = nn
      nm1 = n - 1
      do 10 i = 2,n 
 10   d(i) = d(i) - (t(i-1)*t(i-1))/d(i-1)
      do 15 i = 1,n 
 15   d(i) = 1.0d0/d(i)
      do 20 i = 1,nm1
 20   t(i) = d(i)*t(i)
      return
      end 
      subroutine tfacm (nn,nsize,d,t)
      implicit double precision (a-h, o-z)
c
c ... tfacm computes factorizations of multiple independent 
c     symmetric tridiagonal matrices contained in d and t.
c
c ... parameters -- 
c
c          n      order of global system (= nn)
c          nsize  size of the individual subsystems
c          d      linear array of length n containing the
c                  diagonal elements of the systems
c          t      linear array of length n-1 containing the 
c                  super-diagonal elements of the systems
c
c ... specifications for parameters
c
      dimension d(nsize,1), t(nsize,1)
c
      n = nn
      nm1 = n - 1
      nsys = n/nsize
      do 10 i = 2,nsize
         do 5 j = 1,nsys
 5       d(i,j) = d(i,j) - (t(i-1,j)**2)/d(i-1,j) 
 10   continue
      call vinv (n,d)
      call vexopy (nm1,t,d,t,3)
      return
      end 
      subroutine tfacn (nn,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... tfacn computes a factorization of a single nonsymmetric
c     tridiagonal matrix contained in d, t, and b and
c     replaces it.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of the matrix
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the matrix
c          b      vector of length n-1 containing the sub-
c                  diagonal elements of the matrix
c
c ... specifications for parameters
c
      dimension d(1), t(1), b(1)
c
      n = nn
      nm1 = n - 1
      do 10 i = 2,n 
 10   d(i) = d(i) - b(i-1)*t(i-1)/d(i-1)
      do 15 i = 1,n 
 15   d(i) = 1.0d0/d(i)
      do 20 i = 1,nm1
         t(i) = d(i)*t(i)
         b(i) = d(i)*b(i)
 20   continue
      return
      end 
      subroutine tfacnm (nn,nsize,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... tfacnm computes factorizations of multiple independent
c     nonsymmetric tridiagonal matrices contained in
c     d, t, and b.
c
c ... parameters -- 
c
c          n      order of global system (= nn)
c          nsize  order of single subsystem
c          d      linear array of length n containing the
c                  diagonal elements of the systems
c          t      linear array of length n-1 containing the 
c                  super-diagonal elements of the systems
c          b      linear array of length n-1 containing the 
c                  sub-diagonal elements of the systems
c
c ... specifications for parameters
c
      dimension d(nsize,1), t(nsize,1), b(nsize,1)
c
      n = nn
      nm1 = n - 1
      nsys = n/nsize
      do 10 i = 2,nsize
         do 5 j = 1,nsys
 5       d(i,j) = d(i,j) - b(i-1,j)*t(i-1,j)/d(i-1,j)
 10   continue
      call vinv (n,d)
      call vexopy (nm1,t,d,t,3)
      call vexopy (nm1,b,d,b,3)
      return
      end 
      subroutine tfs (n,b,x)
      implicit double precision (a-h, o-z)
c
c ... tfs does a forward substitution  (i + b)*x = y,
c     where b is the first sub-diagonal.
c
c ... parameters -- 
c
c          n      order of system
c          b      vector of length n-1 containing the sub-
c                  diagonal elements
c          x      on input, x contains y
c                 on output, x contains the solution to (i - b)*x = y 
c
c ... specifications for parameters
c
      dimension b(1), x(1)
c
      do 10 i = 2,n 
 10   x(i) = x(i) - b(i-1)*x(i-1)
      return
      end 
      subroutine tfsm (nn,nsize,b,x)
      implicit double precision (a-h, o-z)
c
c ... tfsm does a forward substitution  (i + b)*x = y  where b
c     is a sub-diagonal composed of independent subsystems of
c     size nsize.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  order of the individual subsystems
c          b      linear array of length n-1 containing the sub-
c                  diagonal elements of the factorizations
c          x      on input, x contains y
c                 on output, x contains the solution to (i + b)*x = y 
c
c ... specifications for parameters
c
      dimension b(nsize,1), x(nsize,1)
c
      n = nn
      nsys = n/nsize
      do 20 i = 2,nsize
         do 15 j = 1,nsys
 15      x(i,j) = x(i,j) - b(i-1,j)*x(i-1,j)
 20   continue
      return
      end 
      subroutine tinv (nn,d,t)
      implicit double precision (a-h, o-z)
c
c ... tinv computes an approximate inverse to a single tridiagonal
c     symmetric matrix.  d and u must contain upon input the
c     output from a factorization routine.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of the factorization
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the factorization
c
c ... specifications for parameters
c
      dimension d(1), t(1)
c
      n = nn
      nm1 = n - 1
c
      do 10 i = nm1,1,-1
 10   d(i) = d(i) + t(i)*t(i)*d(i+1)
      do 15 i = 1,nm1
 15   t(i) = -d(i+1)*t(i)
      return
      end 
      subroutine tinvm (nn,nsize,d,t)
      implicit double precision (a-h, o-z)
c
c ... tinvm computes an approximate inverse to multiple tridiagonal
c     symmetric matrices.  d and t must contain upon input the
c     output from a factorization routine.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          nsize  size of a single subsystem
c          d      vector of length n containing the diagonal
c                  elements of the factorization
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the factorization
c
c ... specifications for parameters
c
      dimension d(nsize,1), t(nsize,1)
c
      n = nn
      nm1 = n - 1
      nsys = n/nsize
      nsm1 = nsize - 1
c
      do 20 i = nsm1,1,-1
         do 15 l = 1,nsys
 15      d(i,l) = d(i,l) + t(i,l)*t(i,l)*d(i+1,l) 
 20   continue
      call vemxty (nm1,t,d(2,1),t)
      return
      end 
      subroutine tinvn (nn,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... tinvn computes an approximate inverse to a single tridiagonal
c     nonsymmetric matrix.  d, b, and t must contain upon
c     input the output from a factorization routine.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of the factorization
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the factorization
c          b      vector of length n-1 containing the sub-
c                  diagonal elements of the factorization
c
c ... specifications for parameters
c
      dimension d(1), t(1), b(1)
c
      n = nn
      nm1 = n - 1
c
      do 10 i = nm1,1,-1
 10   d(i) = d(i) + b(i)*t(i)*d(i+1)
      do 20 i = 1,nm1
         t(i) = -d(i+1)*t(i)
         b(i) = -d(i+1)*b(i)
 20   continue
      return
      end 
      subroutine tinvnm (nn,nsize,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... tinvnm computes an approximate inverse to multiple tridiagonal
c     nonsymmetric matrices.  d, t, and b must contain upon 
c     input the output from a factorization routine.
c
c ... parameters -- 
c
c          n      order of system (= nn)
c          nsize  size of a single subsystem
c          d      vector of length n containing the diagonal
c                  elements of the factorization
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the factorization
c          b      vector of length n-1 containing the sub-
c                  diagonal elements of the factorization
c
c ... specifications for parameters
c
      dimension d(nsize,1), t(nsize,1), b(nsize,1)
c
      n = nn
      nm1 = n - 1
      nsys = n/nsize
      nsm1 = nsize - 1
c
      do 20 i = nsm1,1,-1
         do 15 l = 1,nsys
 15      d(i,l) = d(i,l) + b(i,l)*t(i,l)*d(i+1,l) 
 20   continue
      call vemxty (nm1,t,d(2,1),t)
      call vemxty (nm1,b,d(2,1),b)
      return
      end 
      subroutine tsoln (nn,d,t,b,y,x)
      implicit double precision (a-h, o-z)
c
c ... tsoln solves the system ax = y for x, where a is a single
c     tridiagonal system.  d, t, and b contain
c     the main diagonal, the first super-diagonal, and the first
c     sub-diagonal, respectively of the factorization.
c
c ... parameters -- 
c
c          n      order of system
c          d      vector of length n containing the diagonal
c                  elements of the factorization matrix
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the factorization
c          b      vector of length n-1 containing the sub-
c                  diagonal elements of the factorization
c          y      the right-hand side
c          x      the solution to ax = y
c
c ... specifications for parameters
c
      dimension d(1), t(1), b(1), x(1), y(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call tfs (n,b,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call tbs (n,t,x)
      return
      end 
      subroutine tsolnm (nn,nsize,d,t,b,y,x)
      implicit double precision (a-h, o-z)
c
c ... tsolnm solves the system ax = y for x, where a contains
c     multiple tridiagonal systems.  d, t, and b contain
c     the main diagonal, the first super-diagonal, and the first
c     sub-diagonal, respectively of the factorization.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of an individual subsystem 
c          d      vector of length n containing the diagonal
c                  elements of the factorization matrix
c          t      vector of length n-1 containing the super-
c                  diagonal elements of the factorization
c          b      vector of length n-1 containing the sub-
c                  diagonal elements of the factorization
c          y      the right-hand side
c          x      the solution to ax = y
c
c ... specifications for parameters
c
      dimension d(1), t(1), b(1), x(1), y(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call tfsm (n,nsize,b,x) 
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call tbsm (n,nsize,t,x) 
      return
      end 
      subroutine tsum (nn,lda,ldb,ldc,ma,mbb,mc,mdd,incb,incc,
     a                 incdd,ja,jb,jc,jd,a,b,c,rows,cols,wksp,
     a                 icodee,omegaa)
      implicit double precision (a-h, o-z)
c
c ... tsum computes the row and column sum of (c**t)*a*b restricted
c ... to the sparsity pattern of jd.  a is assumed to be symmetric.
c
c ... parameters -- 
c
c         n             orders of arrays a,b,c,d
c         lda,ldb,ldc   row dimensions of arrays a,b,c
c         ma,mb,mc,md   columns (diagonals) in arrays a,b,c,d
c         incb,incc,    offsets for diagonal numbers of b,c,d arrays
c           incd
c         ja,jb,jc,jd   diagonal index arrays for a,b,c,d
c         a,b,c         arrays of dimension n x (ma,mb,md)
c         rows          row sum of d = (c**t)*a*b upon output
c         cols          column sum of d upon output
c         wksp          workspace array of length n
c         icode         key
c                        = 0  if c .ne. b
c                        = 1  if c .eq. b
c         omega         relaxation factor between 0 and 1
c
c ... specifications for parameters
c
      integer   ja(1), jb(1), jc(1), jd(1)
      dimension a(lda,1), b(ldb,1), c(ldc,1), wksp(1), rows(1), cols(1)
c
      n = nn
      mb = mbb
      md = mdd
      incd = incdd
      icode = icodee
      omega = omegaa
      do 95 lc = 1,mc
         i = jc(lc) - incc
         ia1 = max (1,i+1)
         ib1 = min (n,n+i)
         do 90 la = 1,ma
            j = ja(la)
            l1 = -i + j
            ia2 = max (ia1,1-l1)
            ib2 = min (ib1,n-l1)
            do 45 lb = 1,mb
               k = jb(lb) - incb
               l = l1 + k
               do 10 ld = 1,md
                  if (jd(ld)-incd .eq. l) go to 15
 10            continue
               go to 45
 15            ist = max (ia2,1-l)
               ied = min (ib2,n-l)
               do 20 kk = ist,ied
 20            wksp(kk-ist+1) = c(kk-i,lc)*a(kk-i,la)*b(kk+l1,lb)
               do 25 kk = ist,ied
 25            rows(kk) = rows(kk) + omega*wksp(kk-ist+1)
               if (l .eq. 0  .or.  icode .ne. 1) go to 35
                     do 30 kk = ist,ied 
 30                  rows(kk+l) = rows(kk+l) + omega*wksp(kk-ist+1)
 35            if (icode .eq. 1) go to 45
               do 40 kk = ist,ied
 40            cols(kk+l) = cols(kk+l) + omega*wksp(kk-ist+1)
 45         continue
            if (j .eq. 0) go to 90
            l1 = -i - j
            ia2 = max (ia1,1-l1)
            ib2 = min (ib1,n-l1)
            do 85 lb = 1,mb
               k = jb(lb) - incb
               l = l1 + k
               do 50 ld = 1,md
                  if (jd(ld)-incd .eq. l) go to 55
 50            continue
               go to 85
 55            ist = max (ia2,1-l)
               ied = min (ib2,n-l)
               do 60 kk = ist,ied
 60            wksp(kk-ist+1) = c(kk-i,lc)*a(kk+l1,la)*b(kk+l1,lb)
               do 65 kk = ist,ied
 65            rows(kk) = rows(kk) + omega*wksp(kk-ist+1)
               if (l .eq. 0  .or.  icode .ne. 1) go to 75
                     do 70 kk = ist,ied 
 70                  rows(kk+l) = rows(kk+l) + omega*wksp(kk-ist+1)
 75            if (icode .eq. 1) go to 85
               do 80 kk = ist,ied
 80            cols(kk+l) = cols(kk+l) + omega*wksp(kk-ist+1)
 85         continue
 90      continue
 95   continue
      return
      end 
      subroutine tsumn (nn,np,nq,lda,ldb,ldc,ldj,ma,mb,mc,md,
     a                  incb,incc,incd,ja,jb,jc,jd,a,b,c,
     a                  rows,omega)
      implicit double precision (a-h, o-z)
c
c ... tsumn computes the row sum of c*a*b restricted
c ... to the sparsity pattern of jd.
c
c      c is np x nn        b is nn x nq 
c      a is nn x nn        d is np x nq 
c
c ... definition of parameters --
c
c         nn,np,nq      orders of arrays
c         lda,ldb,ldc   row dimensions of arrays a,b,c
c         ldj           row dimension of ja,jb,jc,jd vectors
c         ma,mb,mc,md   columns (diagonals) in arrays a,b,c,d
c         incb,incc,    offsets for diagonal numbers of b,c,d arrays
c           incd
c         ja,jb,jc,jd   diagonal index arrays for a,b,c,d
c         a,b,c         arrays of dimension n x (ma,mb,md)
c         rows          row sum of d = c*a*b upon output
c         omega         relaxation factor between 0 and 1
c
c ... specifications for parameters
c
      integer   ja(ldj,1), jb(ldj,1), jc(ldj,1), jd(ldj,1)
      dimension a(lda,1), b(ldb,1), c(ldc,1), rows(1)
c
      n = nn
      do 40 lc = 1,mc
         i = jc(1,lc) - incc
         ia1 = max (1,1-i)
         ib1 = min (np,n-i)
         do 35 la = 1,ma
            j = ja(1,la)
            l1 = i + j
            ia2 = max (ia1,1-l1)
            ib2 = min (ib1,n-l1)
            do 30 lb = 1,mb
               k = jb(1,lb) - incb
               l = l1 + k
               do 15 ld = 1,md
                  if (jd(1,ld)-incd .eq. l) go to 20
 15            continue
               go to 30
 20            ist = max (ia2,1-l)
               ied = min (ib2,nq-l)
               do 25 m = ist,ied
 25            rows(m) = rows(m) + omega*c(m,lc)*a(m+i,la)*b(m+l1,lb) 
 30         continue
 35      continue
 40   continue
      return
      end 
      subroutine t1prod (lda,ldb,ldc,ldd,ldj,nn,np,nq,ma,mb,mc,md,
     a                   incb,incc,incd,ja,jb,jc,jd,a,b,c,d)
      implicit double precision (a-h, o-z)
c
c ... t1prod computes  d = d - c*a*b
c ...
c ... but restricted to the sparsity pattern of d.  a is assumed to
c ... be in nonsymmetric storage mode.
c
c      c is np x nn        b is nn x nq 
c      a is nn x nn        d is np x nq 
c
c ... definition of parameters --
c
c         lda,ldb,      row dimension of arrays a,b,c,d
c          ldc,ldd
c         ldj           row dimension of arrays ja,jb,jc,jd 
c         nn,np,nq      orders of arrays
c         ma,mb,mc,md   columns (diagonals) in arrays a,b,c,d
c         incb,incc,    offsets for diagonal numbers of b,c,d arrays
c           incd
c         ja,jb,jc,jd   diagonal index arrays for a,b,c,d
c         a,b,c,d       arrays of dimension n x (ma,mb,mc, or md)
c
c ... specifications for parameters
c
      integer ja(ldj,1), jb(ldj,1), jc(ldj,1), jd(ldj,1)
      dimension a(lda,1), b(ldb,1), c(ldc,1), d(ldd,1)
c
      n = nn
      do 40 lc = 1,mc
         i = jc(1,lc) - incc
         ia1 = max (1,1-i)
         ib1 = min (np,n-i)
         do 35 la = 1,ma
            j = ja(1,la)
            l1 = i + j
            ia2 = max (ia1,1-l1)
            ib2 = min (ib1,n-l1)
            do 30 lb = 1,mb
               k = jb(1,lb) - incb
               l = l1 + k
               do 15 ld = 1,md
                  if (jd(1,ld)-incd .eq. l) go to 20
 15            continue
               go to 30
 20            ist = max (ia2,1-l)
               ied = min (ib2,nq-l)
               do 25 m = ist,ied
 25            d(m,ld) = d(m,ld) - c(m,lc)*a(m+i,la)*b(m+l1,lb)
 30         continue
 35      continue
 40   continue
      return
      end 
      subroutine t2prod (nn,nda,ndb,ndc,ndd,ma,mbb,mc,mdd,incb,incc,
     a                   incd,ja,jb,jc,jd,a,b,c,d)
      implicit double precision (a-h, o-z)
c
c ... t2prod computes  d = d - (c**t)*a*b  restricted to the
c ... sparsity pattern of d.  a is assumed to be symmetric. 
c
c ... parameters -- 
c
c         n             orders of arrays a,b,c,d
c         nda,ndb,ndc,  row dimensions of arrays a,b,c,d
c          ndd
c         ma,mb,mc,md   columns (diagonals) in arrays a,b,c,d
c         incb,incc,    offsets for diagonal numbers of b,c,d arrays
c           incd
c         ja,jb,jc,jd   diagonal index arrays for a,b,c,d
c         a,b,c,d       arrays of dimension n x (ma,mb,mc, or md)
c
c ... specifications for parameters
c
      integer   ja(1), jb(1), jc(1), jd(1)
      dimension a(nda,1), b(ndb,1), c(ndc,1), d(ndd,1)
c
      n = nn
      mb = mbb
      md = mdd
      do 65 lc = 1,mc
         i = jc(lc) - incc
         ia1 = max (1,i+1)
         ib1 = min (n,n+i)
         do 60 la = 1,ma
            j = ja(la)
            l1 = -i + j
            ia2 = max (ia1,1-l1)
            ib2 = min (ib1,n-l1)
            do 30 lb = 1,mb
               k = jb(lb) - incb
               l = l1 + k
               do 15 ld = 1,md
                  if (jd(ld)-incd .eq. l) go to 20
 15            continue
               go to 30
 20            ist = max (ia2,1-l)
               ied = min (ib2,n-l)
               do 25 ir = ist,ied
 25            d(ir,ld) = d(ir,ld) - c(ir-i,lc)*a(ir-i,la)*b(ir+l1,lb)
 30         continue
            if (j .eq. 0) go to 60
            l1 = -i - j
            ia2 = max (ia1,1-l1)
            ib2 = min (ib1,n-l1)
            do 55 lb = 1,mb
               k = jb(lb) - incb
               l = l1 + k
               do 40 ld = 1,md
                  if (jd(ld)-incd .eq. l) go to 45
 40            continue
               go to 55
 45            ist = max (ia2,1-l)
               ied = min (ib2,n-l)
               do 50 ir = ist,ied
 50            d(ir,ld) = d(ir,ld) - c(ir-i,lc)*a(ir+l1,la)*b(ir+l1,lb)
 55         continue
 60      continue
 65   continue
      return
      end 
      subroutine unpmdg (ndim,nn,maxnz,jcoef,coef,ncol,nc,p,ip,
     a                   maxd,maxnew,jcnew,wksp,iwksp,isym) 
      implicit double precision (a-h, o-z)
c
c ... unpmdg reverses the permutation done by pmdiag.  it
c     permutates the matrix according to index vector ip.
c     the permuted matrix is stored in diagonal format.
c
c ... parameters -- 
c
c        ndim      row dimension of coef and jcoef arrays
c                   in defining routine 
c        n         order of system (active row size of coef and jcoef)
c        maxnz     active column size of coef and jcoef
c        jcoef     integer array of column numbers
c        coef      floating point array of coefficients
c        ncolor    number of colors in the permutation (= ncol)
c        nc        integer vector of length ncolor giving the
c                   number of nodes for each color
c        p         permutation vector
c        ip        inverse permuation vector
c        maxd      active column size of permuted matrix
c        jcnew     integer array of size ncolor*max(maxnew(i))
c                   giving the diagonal numbers for each color
c        wksp      floating point workspace of length n
c        iwksp     integer workspace of length 2*n
c        isym      symmetric storage switch
c                   = 0    symmetric storage
c                   = 1    nonsymmetric storage
c
c ... specifications for parameters
c
      integer jcoef(2), nc(1), p(1), jcnew(ncol,1), maxnew(1),
     a        iwksp(1), ip(1) 
      dimension coef(ndim,1), wksp(1)
c
c
      n = nn
      ncolor = ncol 
c
c ... set up pointer vector.
c
      do 10 j = 1,maxnz
         jcol = jcoef(j)
         iwksp(n+jcol) = j
 10   continue
c
c ... permute rows of matrix first.
c
      do 15 j = 1,maxd
         do 12 i = 1,n
 12      wksp(i) = coef(i,j)
         call vscatr (n,wksp,ip,coef(1,j))
 15   continue
c
c ... rearrange rows.
c
      ist = 1
      do 35 k = 1,ncolor
         ncc = nc(k)
         ied = ist + ncc - 1
         lim = maxnew(k)
         do 30 i = ist,ied
            iip = ip(i)
            do 20 j = 2,maxd
               wksp(j) = coef(iip,j)
               coef(iip,j) = 0.0d0
 20         continue
            do 25 j = 2,lim
               if (wksp(j) .eq. 0.0d0) go to 25
               jcol = ip(i + jcnew(k,j)) - iip
               l = iwksp(n+jcol)
               coef(iip,l) = wksp(j)
 25         continue
 30      continue
         ist = ist + ncc
 35   continue
c
c ... zero out lower triangular matrix if symmeteric storage used.
c
      if (isym .ne. 0) return 
      maxold = (maxnz + 1)/2
      mp1 = maxold + 1
      do 45 j = mp1,maxnz
         do 40 i = 1,n
 40      coef(i,j) = 0.0d0
         jcoef(j) = 0
 45   continue
      maxnz = maxold
      return
      end 
      subroutine uscal1 (nn,ndim,maxnzz,jcoef,coef,rhs,u,ubar,
     a                   diag,work,iflag)
      implicit double precision (a-h, o-z)
c
c ... uscal1 reverses the scaling done in routine scal1.  diag must
c     contain upon input the output from scal1.
c     (purdue data structure) 
c
c ... parameters -- 
c
c         n       dimension of matrix
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array 
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         rhs     right hand side of matrix problem
c         u       latest estimate of solution
c         ubar    exact solution (optional)
c         diag    vector of the same name from scal1 routine
c         work    work array of length n (volatile)
c         iflag   flag for ubar
c                  = 0  do not unscale ubar
c                  = 1  unscale ubar
c
c ... specifications for parameters
c
      integer   jcoef(ndim,1) 
      dimension coef(ndim,1), rhs(1), u(1), diag(1), work(1),
     a          ubar(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      maxnz = maxnzz
c
c ... unscale u and rhs arrays.
c
      do 10 i = 1,n 
 10   u(i) = diag(i)*u(i)
      if (iflag .eq. 0) go to 20
      do 15 i = 1,n 
 15   ubar(i) = diag(i)*ubar(i)
 20   do 25 i = 1,n 
 25   diag(i) = 1.0d0/diag(i)
      do 30 i = 1,n 
 30   rhs(i) = diag(i)*rhs(i) 
c
c ... unscale matrix.
c
      if (keygs .eq. 2) go to 45
c
c ... using gathers.
c
      do 40 j = 1,maxnz
         call vgathr (n,diag,jcoef(1,j),work)
         do 35 i = 1,n
 35      coef(i,j) = diag(i)*coef(i,j)*work(i)
 40   continue
      return
c
c ... not using gathers.
c
 45   do 55 j = 1,maxnz
         do 50 i = 1,n
 50      coef(i,j) = diag(i)*coef(i,j)*diag(jcoef(i,j))
 55   continue
      return
      end 
      subroutine uscal2 (nn,ndim,maxnz,jcoef,coef,rhs,u,ubar,
     a                   diag,iflag)
      implicit double precision (a-h, o-z)
c
c ... uscal2 reverses the scaling done in routine scal2.  diag must
c     contain upon input the output from scal2.
c     (diagonal data structure)
c
c ... parameters -- 
c
c         n       dimension of matrix
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array 
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         rhs     right hand side of matrix problem
c         u       latest estimate of solution
c         ubar    exact solution (optional)
c         diag    vector of the same name from scal2 routine
c         iflag   flag for ubar
c                  = 0  do not unscale ubar
c                  = 1  unscale ubar
c
c ... specifications for parameters
c
      integer   jcoef(2)
      dimension coef(ndim,1), rhs(1), u(1), diag(1), ubar(1)
c
c
      n = nn
c
c ... unscale u and rhs arrays.
c
      do 10 i = 1,n 
 10   u(i) = diag(i)*u(i)
      if (iflag .eq. 0) go to 20
      do 15 i = 1,n 
 15   ubar(i) = diag(i)*ubar(i)
 20   do 25 i = 1,n 
 25   diag(i) = 1.0d0/diag(i)
      do 30 i = 1,n 
 30   rhs(i) = diag(i)*rhs(i) 
c
c ... unscale matrix.
c
      do 50 j = 1,maxnz
         ind = jcoef(j)
         len = n - iabs(ind)
         if (ind .lt. 0) go to 40
         do 35 i = 1,len
 35      coef(i,j) = diag(i)*coef(i,j)*diag(i+ind)
         go to 50
 40      do 45 i = 1,len
 45      coef(i-ind,j) = diag(i)*coef(i-ind,j)*diag(i-ind)
 50   continue
      return
      end 
      subroutine uscal3 (nn,nz,ia,ja,a,rhs,u,ubar,diag,work,iflag)
      implicit double precision (a-h, o-z)
c
c ... uscal3 reverses the scaling done in routine scal3.  diag must
c     contain upon input the output from scal3.
c     (sparse data structure) 
c
c ... parameters -- 
c
c         n       dimension of matrix
c         nz      length of the vectors a, ia, and ja
c         a       vector of matrix coefficients
c         ia      vector of i values
c         ja      vector of j values
c         rhs     right hand side of matrix problem
c         u       latest estimate of solution
c         ubar    exact solution (optional)
c         diag    vector of the same name from scal3 routine
c         work    work array of length n (volatile)
c         iflag   flag for ubar
c                  = 0  do not unscale ubar
c                  = 1  unscale ubar
c
c ... specifications for parameters
c
      integer   ia(1), ja(1)
      dimension a(1), rhs(1), u(1), diag(1), work(1),
     a          ubar(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
c
c ... unscale u and rhs arrays.
c
      do 10 i = 1,n 
 10   u(i) = diag(i)*u(i)
      if (iflag .eq. 0) go to 20
      do 15 i = 1,n 
 15   ubar(i) = diag(i)*ubar(i)
 20   do 25 i = 1,n 
 25   diag(i) = 1.0d0/diag(i)
      do 30 i = 1,n 
 30   rhs(i) = diag(i)*rhs(i) 
c
c ... unscale matrix.
c
      if (keygs .eq. 2) go to 50
c
c ... using gathers.
c
      ist = 1
 35   ied = min (ist-1+n,nz) 
      if (ied .lt. ist) return
         len = ied - ist + 1
         call vgathr (len,diag,ia(ist),work)
         do 40 i = ist,ied
 40      a(i) = a(i)*work(i-ist+1)
         call vgathr (len,diag,ja(ist),work)
         do 45 i = ist,ied
 45      a(i) = a(i)*work(i-ist+1)
      ist = ied + 1 
      go to 35
c
c ... not using gathers.
c
 50   do 55 i = 1,nz
 55   a(i) = a(i)*diag(ia(i))*diag(ja(i))
      return
      end 
      subroutine vaddd (lda,ldja,nn,m,mdiagg,a,ja,y,x,jofff)
      implicit double precision (a-h, o-z)
c
c ... vaddd computes  y = y + a*x
c
c     (diagonal storage)
c
c ... parameters -- 
c
c         lda      leading dimension of a array
c         ldja     leading dimension of ja array
c         n        active row size of matrix
c         m        active column size of matrix
c         mdiag    number of diagonals in a
c         a        array of matrix diagonals
c         ja       array of matrix diagonal numbers
c         y,x      vectors of length n
c         joff     offset for diagonal numbers
c
c ... specifications for parameters
c
      dimension a(lda,3), x(1), y(1)
      integer   ja(ldja,3)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      mdiag = mdiagg
      joff = jofff
      if (mdiag .lt. 1) return
      if (keyzer .eq. 1) go to 20
      do 15 j = 1,mdiag
         ind = ja(1,j) - joff 
         ist = max (1,1-ind) 
         ied = min (n,m-ind) 
         do 10 i = ist,ied
 10      y(i) = y(i) + a(i,j)*x(i+ind)
 15   continue
      return
c
c ... unrolled version (requires memory to be zeroed out).
c
 20   l = mod (mdiag,4)
      if (l .eq. 0) go to 60
c
c ... initial short computations
c
      go to (25,35,45), l
 25   do 30 i = 1,n 
 30   y(i) = y(i) + a(i,1)*x(i+ja(1,1)-joff)
      go to 55
 35   do 40 i = 1,n 
 40   y(i) = y(i) + a(i,1)*x(i+ja(1,1)-joff) + a(i,2)*x(i+ja(1,2)-joff)
      go to 55
 45   do 50 i = 1,n 
 50   y(i) = y(i) + a(i,1)*x(i+ja(1,1)-joff) + a(i,2)*x(i+ja(1,2)-joff)
     a            + a(i,3)*x(i+ja(1,3)-joff)
 55   if (mdiag .le. 4) return
c
c ... loop unrolling to a level of 4.
c
 60   lp1 = l + 1
      do 70 j = lp1,mdiag,4
         do 65 i = 1,n
 65      y(i) = y(i) + a(i,j  )*x(i+ja(1,j  )-joff)
     a               + a(i,j+1)*x(i+ja(1,j+1)-joff)
     a               + a(i,j+2)*x(i+ja(1,j+2)-joff)
     a               + a(i,j+3)*x(i+ja(1,j+3)-joff)
 70   continue
      return
      end 
      subroutine vadddt (lda,ldja,nn,m,mdiagg,a,ja,y,x,jofff)
      implicit double precision (a-h, o-z)
c
c ... vadddt computes  y = y + (a**t)*x 
c
c     (diagonal storage)
c
c ... parameters -- 
c
c         lda      leading dimension of a array
c         ldja     leading dimension of ja array
c         n        active row size of matrix
c         m        active column size of matrix
c         mdiag    number of diagonals in a
c         a        array of matrix diagonals
c         ja       array of matrix diagonal numbers
c         y,x      vectors of length n
c         joff     offset for diagonal numbers
c
c ... specifications for parameters
c
      dimension a(lda,3), x(1), y(1)
      integer   ja(ldja,3)
c
      n = nn
      mdiag = mdiagg
      joff = jofff
      if (mdiag .lt. 1) return
      do 15 j = 1,mdiag
         ind = ja(1,j) - joff 
         ist = max (1,1-ind) 
         ied = min (n,m-ind) 
         do 10 i = ist,ied
 10      y(i+ind) = y(i+ind) + a(i,j)*x(i)
 15   continue
      return
      end 
      subroutine vaddp (ndimr,ndimi,nn,mm,a,ja,y,x,wksp)
      implicit double precision (a-h, o-z)
c
c ... vaddp does  y = y + a*x  (purdue format)
c
c ... parameters -- 
c
c       ndimr     row dimension of a array
c       ndimi     row dimension of ja array
c       n         order of system
c       m         number of columns in a and ja arrays
c       a         floating point array of active size n by m
c       ja        integer array of active size n by m
c       y         accumulation vector
c       x         right-hand-side vector
c       wksp      workspace vector of length n  (keygs = 1 only)
c
c ... specifications for parameters
c
      dimension a(ndimr,3), ja(ndimi,3), x(1), y(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      m = mm
      if (m .le. 0) return
      if (keygs .eq. 1) go to 100
c
c ... implicit gathers.
c
      l = mod (m,4) 
      if (l .eq. 0) go to 45
c
c ... initial short computations
c
      go to (10,20,30), l
 10   do 15 i = 1,n 
 15   y(i) = y(i) + a(i,1)*x(ja(i,1))
      go to 40
 20   do 25 i = 1,n 
 25   y(i) = y(i) + a(i,1)*x(ja(i,1)) + a(i,2)*x(ja(i,2))
      go to 40
 30   do 35 i = 1,n 
 35   y(i) = y(i) + a(i,1)*x(ja(i,1)) + a(i,2)*x(ja(i,2))
     a            + a(i,3)*x(ja(i,3))
 40   if (m .le. 4) return
c
c ... loop unrolling to a level of 4.
c
 45   lp1 = l + 1
      do 55 j = lp1,m,4
         do 50 i = 1,n
 50      y(i) = y(i) + a(i,j)*x(ja(i,j)) + a(i,j+1)*x(ja(i,j+1))
     a               + a(i,j+2)*x(ja(i,j+2)) + a(i,j+3)*x(ja(i,j+3))
 55   continue
      return
c
c ... explicit gathers.
c
 100  do 110 j = 1,m
         call vgathr (n,x,ja(1,j),wksp) 
         do 105 i = 1,n
 105     y(i) = y(i) + a(i,j)*wksp(i)
 110  continue
      return
      end 
      subroutine vaddpt (ndimr,ndimi,n,m,a,ja,y,x,wksp)
      implicit double precision (a-h, o-z)
c
c ... vaddpt does  y = y + (a**t)*x  (purdue format)
c
c ... parameters -- 
c
c       ndimr     row dimension of a array
c       ndimi     row dimension of ja array
c       n         order of system
c       m         number of columns in a and ja arrays
c       a         floating point array of active size n by m
c       ja        integer array of active size n by m
c       y         accumulation vector
c       x         right-hand-side vector
c       wksp      workspace vector of length n
c
c ... specifications for parameters
c
      dimension a(ndimr,3), ja(ndimi,3), x(1), y(1), wksp(1)
c
      if (m .le. 0) return
c
      do 20 j = 1,m 
         do 15 i = 1,n
            y(ja(i,j)) = y(ja(i,j)) + a(i,j)*x(i)
 15      continue
 20   continue
      return
      end 
      subroutine vadds (mm,np,ia,ja,a,y,x,wksp)
      implicit double precision (a-h, o-z)
c
c ... vadds does  y = y + a*x  (sparse format)
c
c ... parameters -- 
c
c       m         number of partitions
c       np        partition pointers
c       ia        vector of i values
c       ja        vector of j values
c       a         vector of coefficients
c       y         accumulation vector
c       x         right-hand-side vector
c       wksp      workspace vector of length 2*n  (keygs = 1 only)
c
c ... specifications for parameters
c
      dimension np(1), a(1), ia(1), ja(1), x(1), y(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      m = mm
      if (m .le. 0) return
      if (keygs .eq. 1) go to 20
c
c ... implicit gathers.
c
      do 15 k = 1,m 
         ist = np(k)
         ied = np(k+1) - 1
cdir$ ivdep
         do 10 i = ist,ied
 10      y(ia(i)) = y(ia(i)) + a(i)*x(ja(i))
 15   continue
      return
c
c ... explicit gathers.
c
 20   do 30 k = 1,m 
         ist = np(k)
         nel = np(k+1) - ist
         call vgathr (nel,x,ja(ist),wksp)
         call vgathr (nel,y,ia(ist),wksp(nel+1))
         do 25 i = 1,nel
 25      wksp(i) = wksp(nel+i) + a(ist+i-1)*wksp(i)
         call vscatr (nel,wksp,ia(ist),y)
 30   continue
      return
      end 
      subroutine vcopy (n,x,y)
      implicit double precision (a-h, o-z)
c
c ... vcopy copies vector x to vector y.
c
c ... parameters -- 
c
c          n       length of vectors
c          x,y     vectors of length n
c
c ... specifications for parameters
c
      dimension x(1), y(1)
c
      if (n .le. 0) return
      do 10 i = 1,n 
 10   y(i) = x(i)
      return
      end 
      double precision function vdot (n,x,y)
      implicit double precision (a-h, o-z)
c
c ... vdot computes the dot product of vectors x and y.
c
c ... parameters -- 
c
c          n       length of vectors
c          x,y     vectors of length n
c
c ... specifications for parameters
c
      dimension x(1), y(1)
c
c
      vdot = 0.0d0
      if (n .le. 0) return
      do i = 1,n 
         vdot = vdot + x(i)*y(i) 
      enddo
      return
      end 
      subroutine vexopy (nn,v,x,y,icode)
      implicit double precision (a-h, o-z)
c
c ... vexopy computes  v = x  op  y  where v, x, and y are vectors
c ... and op is one of the operations  + - * / .
c
c ... parameters -- 
c
c          n       length of vectors  (= nn)
c          v,x,y   vectors of length n
c          icode   key indicating operation
c            = 1      for addition
c            = 2      for subtraction
c            = 3      for multiplication
c            = 4      for division
c
c ... specifications for parameters
c
      dimension v(1), x(1), y(1)
c
      n = nn
      if (n .le. 0) return
      go to (10,20,30,40), icode
c
c ... compute   v = x + y
c
 10   do 15 i = 1,n 
 15   v(i) = x(i) + y(i)
      return
c
c ... compute   v = x - y
c
 20   do 25 i = 1,n 
 25   v(i) = x(i) - y(i)
      return
c
c ... compute   v = x * y
c
 30   do 35 i = 1,n 
 35   v(i) = x(i)*y(i)
      return
c
c ... compute   v = x / y
c
 40   do 45 i = 1,n 
 45   v(i) = x(i)/y(i)
      return
      end 
      subroutine vfill (n,v,val)
      implicit double precision (a-h, o-z)
c
c     vfill fills a vector, v, with a constant value, val.
c
c ... parameters -- 
c
c          n      integer length of vector v
c          v      vector
c          val    constant that fills first n locations of v
c
c ... specifications for parameters
c
      dimension v(n)
c
      if (n .le. 0) return
      do 10 i = 1,n 
 10   v(i) = val
      return
      end 
      subroutine vgathi (n,ja,ia,jb)
      implicit double precision (a-h, o-z)
c
c ... vgathi gathers elements from array ja according to index
c ... list ia and places them into consecutive locations in 
c ... array jb.
c
c ... parameters -- 
c
c          n        order of arrays ia and jb
c          ja       integer array of source elements
c          ia       integer array of length n giving desired
c                      elements of array ja
c          jb       integer target array of length n
c
c ... specifications for parameters
c
      integer   ia(1), ja(1), jb(1)
c
      if (n .le. 0) return
      do 10 i = 1,n 
 10   jb(i) = ja(ia(i))
c
c205  jb(1;n) = q8vgathr (ja(1;n),ia(1;n);jb(1;n))
cray1 call gather (n,jb,ja,ia)
c
      return
      end 
      subroutine vgathr (n,a,ia,b)
      implicit double precision (a-h, o-z)
c
c ... vgathr gathers elements from array a according to index
c ... list ia and places them into consecutive locations in 
c ... array b.
c
c ... parameters -- 
c
c          n       order of arrays ia and b
c          a       array of source elements
c          ia      integer array of length n giving desired 
c                     elements of array a
c          b       target array of length n
c
c ... specifications for parameters
c
      integer   ia(1)
      dimension a(1), b(1)
c
      if (n .le. 0) return
      do 10 i = 1,n 
 10   b(i) = a(ia(i))
c
c205  b(1;n) = q8vgathr (a(1;n),ia(1;n);b(1;n))
cray1 call gather (n,b,a,ia)
c
      return
      end 
      subroutine vinv (nn,v)
      implicit double precision (a-h, o-z)
c
c ... vinv computes   v = 1/v 
c
c ... parameters -- 
c
c        n       length of vector (= nn)
c        v       input/output vector of length n. 
c
c ... specifications for parameters
c
      dimension v(1)
c
      n = nn
      if (n .le. 0) return
      do 10 i = 1,n 
 10   v(i) = 1.0d0 / v(i)
      return
      end 
      double precision function vmax (n,v)
      implicit double precision (a-h, o-z)
c
c ... vmax determaxes the maximum algebraic element of vector v.
c
c ... parameters -- 
c
c        n     length of vector
c        v     floating point vector of length n
c
c ... specifications for parameters
c
      dimension v(1)
c
      vmax = v(1)
      if (n .le. 1) return
      do i = 2,n 
         if (v(i) .gt. vmax) vmax = v(i)
      enddo
      return
      end 
      double precision function vmin (n,v)
      implicit double precision (a-h, o-z)
c
c ... vmin determines the minimum algebraic element of vector v.
c
c ... parameters -- 
c
c        n     length of vector
c        v     floating point vector of length n
c
c ... specifications for parameters
c
      dimension v(1)
c
      vmin = v(1)
      if (n .le. 1) return
      do i = 2,n 
         if (v(i) .lt. vmin) vmin = v(i)
      enddo
      return
      end 
      subroutine vscati (n,ja,ia,jb)
      implicit double precision (a-h, o-z)
c
c ... vscati scatters elements from consecutive locations in array
c ... ja to positions in array jb according to index list ia.
c
c ... parameters -- 
c
c         n       order of arrays ia and ja
c         ja      integer array of source elements
c         ia      integer array of length n giving new locations
c                   in array jb.
c         jb      integer target array
c
c ... specifications for parameters
c
      integer   ia(1), ja(1), jb(1)
c
      if (n .le. 0) return
      do 10 i = 1,n 
 10   jb(ia(i)) = ja(i)
c
c205  jb(1;n) = q8vscatr (ja(1;n),ia(1;n);jb(1;n))
cray1 call scatter (n,jb,ia,ja)
c
      return
      end 
      subroutine vscatr (n,a,ia,b)
      implicit double precision (a-h, o-z)
c
c ... vscatr scatters elements from consecutive locations in array a
c ... to positions in array b according to index list ia.
c
c ... parameters -- 
c
c          n       order of arrays ia and a
c          a       array of source elements
c          ia      integer array of length n giving new locations
c                    in array b
c          b       target array
c
c ... specifications for parameters
c
      integer   ia(1)
      dimension a(1), b(1)
c
      if (n .le. 0) return
      do 10 i = 1,n 
 10   b(ia(i)) = a(i)
c
c205  b(1;n) = q8vscatr (a(1;n),ia(1;n);b(1;n))
cray1 call scatter (n,b,ia,a) 
c
      return
      end 
      subroutine vsubd (lda,ldja,nn,m,mdiagg,a,ja,y,x,jofff)
      implicit double precision (a-h, o-z)
c
c ... vsubd computes  y = y - a*x
c
c     (diagonal storage)
c
c ... parameters -- 
c
c         lda      leading dimension of a array
c         ldja     leading dimension of ja array
c         n        active row size of matrix
c         m        active column size of matrix
c         mdiag    number of diagonals in a
c         a        array of matrix diagonals
c         ja       array of matrix diagonal numbers
c         y,x      vectors of length n
c         joff     offset for diagonal numbers
c
c ... specifications for parameters
c
      dimension a(lda,3), x(1), y(1)
      integer   ja(ldja,3)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      mdiag = mdiagg
      joff = jofff
      if (mdiag .lt. 1) return
      if (keyzer .eq. 1) go to 20
      do 15 j = 1,mdiag
         ind = ja(1,j) - joff 
         ist = max (1,1-ind) 
         ied = min (n,m-ind) 
         do 10 i = ist,ied
 10      y(i) = y(i) - a(i,j)*x(i+ind)
 15   continue
      return
c
c ... unrolled version (requires memory to be zeroed out).
c
 20   l = mod (mdiag,4)
      if (l .eq. 0) go to 60
c
c ... initial short computations
c
      go to (25,35,45), l
 25   do 30 i = 1,n 
 30   y(i) = y(i) - a(i,1)*x(i+ja(1,1)-joff)
      go to 55
 35   do 40 i = 1,n 
 40   y(i) = y(i) - a(i,1)*x(i+ja(1,1)-joff) - a(i,2)*x(i+ja(1,2)-joff)
      go to 55
 45   do 50 i = 1,n 
 50   y(i) = y(i) - a(i,1)*x(i+ja(1,1)-joff) - a(i,2)*x(i+ja(1,2)-joff)
     a            - a(i,3)*x(i+ja(1,3)-joff)
 55   if (mdiag .le. 4) return
c
c ... loop unrolling to a level of 4.
c
 60   lp1 = l + 1
      do 70 j = lp1,mdiag,4
         do 65 i = 1,n
 65      y(i) = y(i) - a(i,j  )*x(i+ja(1,j  )-joff)
     a               - a(i,j+1)*x(i+ja(1,j+1)-joff)
     a               - a(i,j+2)*x(i+ja(1,j+2)-joff)
     a               - a(i,j+3)*x(i+ja(1,j+3)-joff)
 70   continue
      return
      end 
      subroutine vsubdt (lda,ldja,nn,m,mdiagg,a,ja,y,x,jofff)
      implicit double precision (a-h, o-z)
c
c ... vsubdt computes  y = y - (a**t)*x 
c
c     (diagonal storage)
c
c ... parameters -- 
c
c         lda      leading dimension of a array
c         ldja     leading dimension of ja array
c         n        active row size of matrix
c         m        active column size of matrix
c         mdiag    number of diagonals in a
c         a        array of matrix diagonals
c         ja       array of matrix diagonal numbers
c         y,x      vectors of length n
c         joff     offset for diagonal numbers
c
c ... specifications for parameters
c
      dimension a(lda,3), x(1), y(1)
      integer   ja(ldja,3)
c
      n = nn
      mdiag = mdiagg
      joff = jofff
      if (mdiag .lt. 1) return
      do 15 j = 1,mdiag
         ind = ja(1,j) - joff 
         ist = max (1,1-ind) 
         ied = min (n,m-ind) 
         do 10 i = ist,ied
 10      y(i+ind) = y(i+ind) - a(i,j)*x(i)
 15   continue
      return
      end 
      subroutine vsubp (ndimr,ndimi,nn,mm,a,ja,y,x,wksp)
      implicit double precision (a-h, o-z)
c
c ... vsubp does  y = y - a*x  (purdue format)
c
c ... parameters -- 
c
c       ndimr     row dimension of a array
c       ndimi     row dimension of ja array
c       n         order of system
c       m         number of columns in a and ja arrays
c       a         floating point array of active size n by m
c       ja        integer array of active size n by m
c       y         accumulation vector
c       x         right-hand-side vector
c       wksp      workspace vector of length n  (keygs = 1 only)
c
c ... specifications for parameters
c
      dimension a(ndimr,3), ja(ndimi,3), x(1), y(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      n = nn
      m = mm
      if (m .le. 0) return
      if (keygs .eq. 1) go to 100
c
c ... implicit gathers.
c
      l = mod (m,4) 
      if (l .eq. 0) go to 45
c
c ... initial short computations
c
      go to (10,20,30), l
 10   do 15 i = 1,n 
 15   y(i) = y(i) - a(i,1)*x(ja(i,1))
      go to 40
 20   do 25 i = 1,n 
 25   y(i) = y(i) - a(i,1)*x(ja(i,1)) - a(i,2)*x(ja(i,2))
      go to 40
 30   do 35 i = 1,n 
 35   y(i) = y(i) - a(i,1)*x(ja(i,1)) - a(i,2)*x(ja(i,2))
     a            - a(i,3)*x(ja(i,3))
 40   if (m .le. 4) return
c
c ... loop unrolling to a level of 4.
c
 45   lp1 = l + 1
      do 55 j = lp1,m,4
         do 50 i = 1,n
 50      y(i) = y(i) - a(i,j)*x(ja(i,j)) - a(i,j+1)*x(ja(i,j+1))
     a               - a(i,j+2)*x(ja(i,j+2)) - a(i,j+3)*x(ja(i,j+3))
 55   continue
      return
c
c ... explicit gathers.
c
 100  do 110 j = 1,m
         call vgathr (n,x,ja(1,j),wksp) 
         do 105 i = 1,n
 105     y(i) = y(i) - a(i,j)*wksp(i)
 110  continue
      return
      end 
      subroutine vsubpt (ndimr,ndimi,n,m,a,ja,y,x,wksp)
      implicit double precision (a-h, o-z)
c
c ... vsubpt does  y = y - (a**t)*x  (purdue format)
c
c ... parameters -- 
c
c       ndimr     row dimension of a array
c       ndimi     row dimension of ja array
c       n         order of system
c       m         number of columns in a and ja arrays
c       a         floating point array of active size n by m
c       ja        integer array of active size n by m
c       y         accumulation vector
c       x         right-hand-side vector
c       wksp      workspace vector of length n
c
c ... specifications for parameters
c
      dimension a(ndimr,3), ja(ndimi,3), x(1), y(1), wksp(1)
c
      if (m .le. 0) return
c
      do 20 j = 1,m 
         do 15 i = 1,n
            y(ja(i,j)) = y(ja(i,j)) - a(i,j)*x(i)
 15      continue
 20   continue
      return
      end 
      subroutine vsubs (mm,np,ia,ja,a,y,x,wksp)
      implicit double precision (a-h, o-z)
c
c ... vsubs does  y = y - a*x  (sparse format)
c
c ... parameters -- 
c
c       m         number of partitions
c       np        partition pointers
c       ia        vector of i values
c       ja        vector of j values
c       a         vector of coefficients
c       y         accumulation vector
c       x         right-hand-side vector
c       wksp      workspace vector of length 2*n  (keygs = 1 only)
c
c ... specifications for parameters
c
      dimension np(1), a(1), ia(1), ja(1), x(1), y(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      m = mm
      if (m .le. 0) return
      if (keygs .eq. 1) go to 20
c
c ... implicit gathers.
c
      do 15 k = 1,m 
         ist = np(k)
         ied = np(k+1) - 1
cdir$ ivdep
         do 10 i = ist,ied
 10      y(ia(i)) = y(ia(i)) - a(i)*x(ja(i))
 15   continue
      return
c
c ... explicit gathers.
c
 20   do 30 k = 1,m 
         ist = np(k)
         nel = np(k+1) - ist
         call vgathr (nel,x,ja(ist),wksp)
         call vgathr (nel,y,ia(ist),wksp(nel+1))
         do 25 i = 1,nel
 25      wksp(i) = wksp(nel+i) - a(ist+i-1)*wksp(i)
         call vscatr (nel,wksp,ia(ist),y)
 30   continue
      return
      end 
      subroutine vtriad (n,c,a,con,b,icode)
      implicit double precision (a-h, o-z)
c
c ... vtriad computes    c = a + con*b    if  icode = 1
c                        c = con*b        if  icode = 2
c
c ... parameters -- 
c
c        n         length of vectors
c        c,a,b     vectors of length n
c        con       multiplicative constant
c        icode     switch
c
c ... specifications for parameters
c
      dimension a(1), b(1), c(1)
c
      if (n .le. 0) return
      if (icode .eq. 2) go to 15
c
c ... compute    c = a + con*b
c
      do 10 i = 1,n 
 10   c(i) = a(i) + con*b(i)
      return
c
c ... compute    c = con*b
c
 15   do 20 i = 1,n 
 20   c(i) = con*b(i)
      return
      end 
      subroutine vsqrt (n,v,w)
      implicit double precision (a-h, o-z)
c
c routine to compute square root of a matrix,   w = sqrt(v) 
c
      dimension v(1), w(1)
c
      do 1 i=1,n
 1    w(i) = sqrt(v(i))
      return
      end 
      subroutine vifill (n, iv, ival)
      implicit double precision (a-h, o-z)
c
c routine to fill an integer vector with a value. 
c
      integer iv(1) 
      if (n .le. 0) return
      do 1 i=1,n
 1    iv(i) = ival
      return
      end 
      subroutine vicopy (n,iv1,iv2)
      implicit double precision (a-h, o-z)
c
c routine to copy one integer vector to another.
c
      integer iv1(1), iv2(1)
      if (n .le. 0) return
      do 1 i=1,n
 1    iv2(i) = iv1(i)
      return
      end 
      subroutine vemxty (nn,v,x,y)
      implicit double precision (a-h, o-z)
c
c ... vemxty computes  v = -x * y  where v, x, and y are vectors
c
c ... parameters -- 
c
c          n       length of vectors  (= nn)
c          v,x,y   vectors of length n
c
c ... specifications for parameters
c
      dimension v(1), x(1), y(1)
c
      n = nn
      if (n .le. 0) return
      do 10 i = 1,n 
 10   v(i) = -x(i)*y(i)
      return
      end 
      subroutine vsrta1 (nz,ia,ja,a)
      implicit double precision (a-h, o-z)
c
c   imsl routine name   - vsrta
c
c-----------------------------------------------------------------------
c
c   purpose             - sorting of the sparse data structure by
c                          rows first and then by columns within
c                          rows
c
c   usage               - call vsrta1 (nz,ia,ja,a)
c
c   arguments    ia     - on input, ia contains the i values of
c                          the array to be sorted.
c                         on output, ia contains the i values of
c                          the sorted array.
c                ja     - on input, ja contains the j values of
c                          the array to be sorted.
c                         on output, ja contains the j values of
c                          the sorted array.
c                a      - on input, a contains the coefficients
c                          of the array to be sorted.
c                         on output, a contains the coefficients
c                          of the sorted array.
c                nz     - input variable containing the number of
c                           elements in the array to be sorted.
c
c   precision/hardware  - single/all
c
c   reqd. imsl routines - none required 
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c   copyright           - 1978 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code.  no other warranty, 
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c                                  specifications for arguments
      integer            ia(nz), ja(nz) 
      dimension          a(nz)
c                                  specifications for local variables 
      integer            iu(21),il(21)
c
      logical lt, le, eq
      lt (i1,j1,i2,j2) = i1 .lt. i2 .or. (i1 .eq. i2 .and. j1 .lt. j2)
      le (i1,j1,i2,j2) = i1 .lt. i2 .or. (i1 .eq. i2 .and. j1 .le. j2)
      eq (i1,j1,i2,j2) = i1 .eq. i2 .and. j1 .eq. j2
c
c                                  first executable statement
      m = 1
      i = 1
      j = nz
      r = 0.375d0
      if (nz .le. 0) return
   10 if (i .eq. j) go to 55
      if (r .gt. 0.5898437d0) go to 20
      r = r + 3.90625d-2
      go to 25
   20 r = r - 0.21875d0
   25 k = i
c                                  select a central element of the
c                                  array and save it in location t
      ij = int ( dble(i) + dble(j-i)*r )
      t = a(ij)
      it = ia(ij)
      jt = ja(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if ( le(ia(i),ja(i),it,jt) ) go to 30
      ia(ij) = ia(i)
      ia(i) = it
      it = ia(ij)
      ja(ij) = ja(i)
      ja(i) = jt
      jt = ja(ij)
      a(ij) = a(i)
      a(i) = t
      t = a(ij)
   30 l = j
c                                  if last element of array is less than
c                                  t, interchange with t
      if (.not. lt(ia(j),ja(j),it,jt) ) go to 40
      ia(ij) = ia(j)
      ia(j) = it
      it = ia(ij)
      ja(ij) = ja(j)
      ja(j) = jt
      jt = ja(ij)
      a(ij) = a(j)
      a(j) = t
      t = a(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if ( le(ia(i),ja(i),it,jt) ) go to 40
      ia(ij) = ia(i)
      ia(i) = it
      it = ia(ij)
      ja(ij) = ja(i)
      ja(i) = jt
      jt = ja(ij)
      a(ij) = a(i)
      a(i) = t
      t = a(ij)
      go to 40
   35 if ( eq(ia(l),ja(l),ia(k),ja(k)) ) go to 40 
      itt = ia(l)
      ia(l) = ia(k) 
      ia(k) = itt
      jtt = ja(l)
      ja(l) = ja(k) 
      ja(k) = jtt
      tt = a(l)
      a(l) = a(k)
      a(k) = tt
c                                  find an element in the second half of
c                                  the array which is smaller than t
   40 l = l - 1
      if (.not. le (ia(l),ja(l),it,jt) ) go to 40 
c                                  find an element in the first half of
c                                  the array which is greater than t
   45 k = k + 1
      if ( lt (ia(k),ja(k),it,jt) ) go to 45
c                                  interchange these elements
      if (k .le. l) go to 35
c                                  save upper and lower subscripts of 
c                                  the array yet to be sorted
      if (l-i .le. j-k) go to 50
      il(m) = i
      iu(m) = l
      i = k
      m = m + 1
      go to 60
   50 il(m) = k
      iu(m) = j
      j = l
      m = m + 1
      go to 60
c                                  begin again on another portion of
c                                  the unsorted array
   55 m = m - 1
      if (m .eq. 0) return
      i = il(m)
      j = iu(m)
   60 if (j-i .ge. 11) go to 25
      if (i .eq. 1) go to 10
      i = i - 1
   65 i = i + 1
      if (i .eq. j) go to 55
      it = ia(i+1)
      jt = ja(i+1)
      t = a(i+1)
      if ( le (ia(i),ja(i),it,jt) ) go to 65
      k = i
   70 ia(k+1) = ia(k)
      ja(k+1) = ja(k)
      a(k+1) = a(k) 
      k = k - 1
      if ( lt (it,jt,ia(k),ja(k)) ) go to 70
      ia(k+1) = it
      ja(k+1) = jt
      a(k+1) = t
      go to 65
      end 
      subroutine zbrent (n,tri,eps,nsig,aa,bb,maxfnn,ier)
      implicit double precision (a-h, o-z)
c   modified imsl routine name   - zbrent
c
c-----------------------------------------------------------------------
c
c   computer            - cdc/single
c
c   latest revision     - january 1, 1978
c
c   purpose             - zero of a function which changes sign in a
c                           given interval (brent algorithm)
c
c   usage               - call zbrent (f,eps,nsig,a,b,maxfn,ier)
c
c   arguments    tri    - a tridiagonal matrix of order n
c                eps    - first convergence criterion (input).  a root,
c                           b, is accepted if abs(f(b)) is less than or
c                           equal to eps.  eps may be set to zero.
c                nsig   - second convergence criterion (input).  a root,
c                           b, is accepted if the current approximation
c                           agrees with the true solution to nsig
c                           significant digits.
c                a,b    - on input, the user must supply two points, a
c                           and b, such that f(a) and f(b) are opposite
c                           in sign. (= aa, bb)
c                           on output, both a and b are altered.  b
c                           will contain the best approximation to the
c                           root of f. see remark 1.
c                maxfn  - on input, maxfn should contain an upper bound
c                           on the number of function evaluations
c                           required for convergence.  on output, maxfn
c                           will contain the actual number of function
c                           evaluations used. (= maxfnn)
c                ier    - error parameter. (output)
c                         terminal error
c                           ier = 3 indicates the algorithm failed to 
c                             converge in maxfn evaluations.
c                           ier = 4 indicates f(a) and f(b) have the
c                             same sign.
c
c   precision/hardware  - single and double/h32
c                       - single/h36,h48,h60
c
c
c   notation            - information on special notation and
c                           conventions is available in the manual
c                           introduction or through imsl routine uhelp
c
c   remarks  1.  let f(x) be the characteristic function of the matrix
c                tri evaluated at x. function determ evaluates f(x).
c                on exit from zbrent, when ier=0, a and b satisfy the 
c                following,
c                f(a)*f(b) .le. 0,
c                abs(f(b)) .le. abs(f(a)), and
c                either abs(f(b)) .le. eps or
c                abs(a-b) .le. max(abs(b),0.1)*10.0**(-nsig).
c                the presence of 0.1 in this error criterion causes
c                leading zeroes to the right of the decimal point to be
c                counted as significant digits. scaling may be required
c                in order to accurately determine a zero of small
c                magnitude.
c            2.  zbrent is guaranteed to reach convergence within
c                k = (dlog((b-a)/d)+1.0)**2 function evaluations where
c                  d=min(over x in (a,b) of
c                    max(abs(x),0.1)*10.0**(-nsig)).
c                this is an upper bound on the number of evaluations. 
c                rarely does the actual number of evaluations used by 
c                zbrent exceed sqrt(k). d can be computed as follows, 
c                  p = min (abs(a),abs(b))
c                  p = max (0.1,p)
c                  if ((a-0.1)*(b-0.1).lt.0.0) p = 0.1
c                  d = p*10.0**(-nsig)
c
c   copyright           - 1977 by imsl, inc. all rights reserved.
c
c   warranty            - imsl warrants only that imsl testing has been
c                           applied to this code. no other warranty,
c                           expressed or implied, is applicable.
c
c-----------------------------------------------------------------------
c
c ... specifications for parameters
c
      dimension          tri(2,1)
c
c
c ... local package references --
c
c          determ
c                                  first executable statement
      a = aa
      b = bb
      maxfn = maxfnn
      t = 0.1d0**nsig 
      ic = 2
      fa = determ(n,tri,a)
      fb = determ(n,tri,b)
      s = b
c                                  test for same sign
      if (fa*fb .gt. 0.0d0) go to 50
    5 c = a
      fc = fa
      d = b - c
      e = d
   10 if (abs (fc) .ge. abs (fb)) go to 15
      a = b
      b = c
      c = a
      fa = fb
      fb = fc
      fc = fa
   15 continue
      tol = t * max (abs (b),0.1d0)
      rm = (c - b)/2.0d0
c                                  test for first convergence criteria
      if (abs (fb) .le. eps) go to 40
c                                  test for second convergence criteria
      if (abs (c-b) .le. tol) go to 40
c                                  check evaluation counter 
      if (ic .ge. maxfn) go to 45
c                                  is bisection forced
      if (abs (e) .lt. tol) go to 30
      if (abs (fa) .le. abs (fb)) go to 30
      s = fb/fa
      if (a .ne. c) go to 20
c                                  linear interpolation
      p = (c - b)*s 
      q = 1.0d0 - s
      go to 25
c                                  inverse quadratic interpolation
   20 q = fa/fc
      r = fb/fc
      rone = r - 1.0d0
      p = s*((c - b)*q*(q - r) - (b - a)*rone)
      q = (q - 1.0d0)*rone*(s - 1.0d0)
   25 if (p .gt. 0.0d0) q = -q
      if (p .lt. 0.0d0) p = -p
      s = e
      e = d
c                                  if abs(p/q).ge.75*abs(c-b) then
c                                     force bisection
      if (p + p .ge. 3.0d0*rm*q) go to 30 
c                                  if abs(p/q).ge.0.5d0*abs(s) then force
c                                     bisection. s = the value of p/q 
c                                     on the step before the last one 
      if (p + p .ge. abs (s*q)) go to 30
      d = p/q
      go to 35
c                                  bisection
   30 e = rm
      d = e
c                                  increment b
   35 a = b
      fa = fb
      temp = d
      if (abs (temp) .le. tol/2.0d0) temp = sign (tol/2.0d0,rm) 
      b = b + temp
      s = b
      fb = determ(n,tri,s)
      ic = ic + 1
      if (fb*fc .le. 0.0d0) go to 10
      go to 5
c                                  convergence of b
   40 a = c
      maxfn = ic
      go to 9000
c                                  maxfn evaluations
   45 ier = 3
      a = c
      maxfn = ic
      call ershow (ier,'zbrent')
      go to 9000
c                                  terminal error - f(a) and f(b) have
c                                  the same sign
   50 ier = 4
      maxfn = ic
      call ershow (ier,'zbrent')
 9000 continue
      aa = a
      bb = b
      maxfnn = maxfn
      return
      end 
