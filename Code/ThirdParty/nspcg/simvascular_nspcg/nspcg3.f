      subroutine adjust (n,ndim,maxnzz,jcoef,key) 
      implicit double precision (a-h, o-z)
c
c ... adjust makes adjustments to the jcoef array.
c
c ... parameters -- 
c
c          n      dimension of the matrix.
c          ndim   row dimension of jcoef array in defining routine
c          maxnz  number of columns in jcoef array
c          jcoef  integer matrix representation array
c          key    indicator flag
c                  = 1   remove zeros from jcoef array
c                  = 2   restore zeros to jcoef array
c
c ... specifications for parameters
c
      integer   jcoef(ndim,1) 
c
      maxnz = maxnzz
      if (maxnz .lt. 2) return
      if (key .eq. 2) go to 20
c
c ... change zero elements of jcoef array.
c
      do 15 j = 2,maxnz
         do 10 i = 1,n
 10      if (jcoef(i,j) .le. 0) jcoef(i,j) = i
 15   continue
      return
c
c ... put original zeros back in jcoef array.
c
 20   do 30 j = 2,maxnz
         do 25 i = 1,n
 25      if (jcoef(i,j) .eq. i) jcoef(i,j) = 0
 30   continue
      return
      end 
      subroutine adinfn (nn,ndim,maxnzz,jcoef,coef,nstore,ainf,wksp)
      implicit double precision (a-h, o-z)
c
c ... adinfn computes an upper bound on the spectral radius of
c     inv(d)*a.
c
c ... parameters -- 
c
c         n       order of system (= nn)
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array (= maxnzz)
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         nstore  matrix storage mode
c                  = 2   symmetric diagonal format
c                  = 3   nonsymmetric diagonal format
c         ainf    upper bound estimate upon output
c         wksp    workspace vector of length n
c
c ... specifications for parameters
c
      integer   jcoef(2)
      dimension coef(ndim,1), wksp(1)
c
      n = nn
      maxnz = maxnzz
      if (ainf .gt. 0.0d0) return
      do 10 i = 1,n 
 10   wksp(i) = coef(i,1)
      do 25 jd = 1,maxnz
         do 20 j = 1,maxnz
            if (jcoef(j) .ne. jd) go to 20
            do 15 i = 1,n
 15         wksp(i) = wksp(i) - abs (coef(i,j))
            if (nstore .eq. 3) go to 25 
            len = n - jd
            do 18 i = 1,len
 18         wksp(i+jd) = wksp(i+jd) - abs (coef(i,j))
            go to 25
 20      continue
         go to 30
 25   continue
 30   if (nstore .eq. 2) go to 50
      do 45 jd = 1,maxnz
         do 40 j = 1,maxnz
            if (jcoef(j) .ne. -jd) go to 40
            do 35 i = 1,n
 35         wksp(i) = wksp(i) - abs (coef(i,j))
            go to 45
 40      continue
         go to 50
 45   continue
c
c ... factor.
c
 50   t1 = vmin (n,wksp)
      if (t1 .le. 0.0d0) t1 = 1.0d0
      call ainfn (n,ndim,maxnz,jcoef,coef,nstore,ainf,wksp) 
      ainf = ainf/t1
      return
      end 
      subroutine ainfn (nn,ndim,maxnzz,jcoef,coef,nstore,ainf,
     a                  wksp) 
      implicit double precision (a-h, o-z)
c
c ... ainfn calculates the infinity norm of the matrix a.
c
c ... parameters -- 
c
c         n       order of system (= nn)
c         ndim    row dimension of coef array in defining routine
c         maxnz   number of columns in coef array (= maxnzz)
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         nstore  matrix storage mode
c                  = 1   purdue format
c                  = 2   symmetric diagonal format
c                  = 3   nonsymmetric diagonal format
c                  = 4   symmetric sparse format
c                  = 5   nonsymmetric sparse format
c         ainf    the infinity norm of the matrix, //a//, upon
c                  output
c         wksp    workspace vector of length n
c
c ... specifications for parameters
c
      integer   jcoef(ndim,2) 
      dimension coef(ndim,1), wksp(1)
c
      n = nn
      maxnz = maxnzz
      if (ainf .gt. 0.0d0) return
      go to (10,30,55,75,75), nstore
c
c ... ellpack data structure. 
c
 10   do 15 i = 1,n 
 15   wksp(i) = abs (coef(i,1))
      if (maxnz .le. 1) go to 995
      do 25 j = 2,maxnz
         do 20 i = 1,n
 20      wksp(i) = wksp(i) + abs (coef(i,j))
 25   continue
      go to 995
c
c ... symmetric diagonal data structure.
c
 30   do 35 i = 1,n 
 35   wksp(i) = abs (coef(i,1))
      if (maxnz .le. 1) go to 995
      do 50 j = 2,maxnz
         ind = jcoef(j,1)
         len = n - ind
         do 40 i = 1,len
 40      wksp(i) = wksp(i) + abs (coef(i,j))
         do 45 i = 1,len
 45      wksp(i+ind) = wksp(i+ind) + abs (coef(i,j))
 50   continue
      go to 995
c
c ... nonsymmetric diagonal data structure.
c
 55   do 60 i = 1,n 
 60   wksp(i) = abs (coef(i,1))
      if (maxnz .le. 1) go to 995
      do 70 j = 2,maxnz
         ind = jcoef(j,1)
         len = n - iabs(ind)
         ist1 = max (1,1 - ind)
         ist2 = min (n,n - ind)
         do 65 i = ist1,ist2
 65      wksp(i) = wksp(i) + abs (coef(i,j))
 70   continue
      go to 995
c
c ... sparse structure.
c
 75   do 80 i = 1,n 
 80   wksp(i) = abs (coef(i,1))
      if (maxnz .le. n) go to 995
      np1 = n + 1
      do 85 k = np1,maxnz
 85   wksp(jcoef(k,1)) = wksp(jcoef(k,1)) + abs (coef(k,1)) 
      if (nstore .eq. 5) go to 995
      do 90 k = np1,maxnz
 90   wksp(jcoef(k,2)) = wksp(jcoef(k,2)) + abs (coef(k,1)) 
c
c ... determine ainf = max (wksp(i)).
c
 995  ainf = vmax (n,wksp)
      return
      end 
      subroutine bdfac (lda,nn,nsizee,nt,nb,a,isym)
      implicit double precision (a-h, o-z)
c
c ... bdfac computes the factorization of a dense banded matrix.
c
c ... parameters -- 
c
c        lda    leading dimension of array a
c        n      active size of array a
c        nsize  size of an individual subsystem (if multiple systems) 
c                nsize = n upon input if not a multiple system
c        nt     number of diagonals needed to store the super-
c                diagonals
c        nb     number of diagonals needed to store the sub-
c                diagonals
c        a      array
c        isym   symmetry switch
c                = 0   matrix is symmetric
c                = 1   matrix is nonsymmetric
c
c ... specifications for parameters
c
      dimension a(lda,5)
      data lenv / 10 /
c
      n = nn
      maxt = nt
      nsize = nsizee
      nsys = n/nsize
c
c ... branch on symmetry.
c
      if (isym .eq. 1) go to 30
c
c ... symmetric case.
c
c ... diagonal case (maxt = 0).
c
      if (maxt .ne. 0) go to 15
      do 10 i = 1,n 
 10   a(i,1) = 1.0d0/a(i,1)
      return
c
c ... tridiagonal case (maxt = 1).
c
 15   if (maxt .ne. 1) go to 20
      if (nsys .le. lenv) call tfac (n,a,a(1,2))
      if (nsys .gt. lenv) call tfacm (n,nsize,a,a(1,2))
      return
c
c ... pentadiagonal case (maxt = 2).
c
 20   if (maxt .ne. 2) go to 25
      if (nsys .le. lenv) call pfac (n,a,a(1,2),a(1,3))
      if (nsys .gt. lenv) call pfacm (n,nsize,a,a(1,2),a(1,3))
      return
c
c ... banded case (maxt .gt. 2).
c
 25   if (nsys .le. lenv) call bfac (lda,n,maxt,a,a(1,2))
      if (nsys .gt. lenv) call bfacm (n,nsize,nsys,maxt,a,a(1,2))
      return
c
c ... nonsymmetric case.
c
 30   maxb = nb
c
c ... diagonal case (maxt = maxb = 0).
c
      if (maxt .ne. 0 .or. maxb .ne. 0) go to 40
      do 35 i = 1,n 
 35   a(i,1) = 1.0d0/a(i,1)
      return
c
c ... tridiagonal case (maxt = maxb = 1).
c
 40   if (maxt .ne. 1 .or. maxb .ne. 1) go to 45
      if (nsys .le. lenv) call tfacn (n,a,a(1,2),a(2,3))
      if (nsys .gt. lenv) call tfacnm (n,nsize,a,a(1,2),a(2,3))
      return
c
c ... pentadiagonal case (maxt = maxb = 2).
c
 45   if (maxt .ne. 2 .or. maxb .ne. 2) go to 50
      if (nsys .le. lenv) call pfacn (n,a,a(1,2),a(1,3),a(2,4),
     a                                  a(3,5))
      if (nsys .gt. lenv) call pfacnm (n,nsize,a,a(1,2),a(1,3),
     a                                   a(2,4),a(3,5))
      return
c
c ... all other cases.
c
 50   if (nsys .le. lenv) call bfacn (lda,n,maxt,maxb,a,a(1,2),
     a                                a(1,maxt+2))
      if (nsys .gt. lenv) call bfacnm (n,nsize,nsys,maxt,maxb,a,a(1,2),
     a                                 a(1,maxt+2))
      return
      end 
      subroutine bdinv (lda,nn,nsizee,nt,nb,fac,isym)
      implicit double precision (a-h, o-z)
c
c ... bdinv computes the inverse of a dense banded matrix.
c
c ... parameters -- 
c
c        lda    leading dimension of factorization matrix fac
c        n      active size of factorization matrix fac
c        nsize  size of an individual subsystem (if multiple systems) 
c                nsize = n upon input if not a multiple system
c        nt     number of diagonals needed to store the super-
c                diagonals
c        nb     number of diagonals needed to store the sub-
c                diagonals
c        fac    array containing factorization upon input
c        isym   symmetry switch
c                = 0   matrix is symmetric
c                = 1   matrix is nonsymmetric
c
c ... specifications for parameters
c
      dimension fac(lda,3)
      data lenv / 10 /
c
      n = nn
      maxt = nt
      nsize = nsizee
      nsys = n/nsize
c
c ... branch on symmetry.
c
      if (isym .eq. 1) go to 30
c
c ... symmetric case.
c
      if (maxt - 1) 10,20,25
c
c ... diagonal case (maxt = 0).
c
 10   return
c
c ... tridiagonal case (maxt = 1).
c
 20   if (nsys .le. lenv) call tinv (n,fac,fac(1,2))
      if (nsys .gt. lenv) call tinvm (n,nsize,fac,fac(1,2)) 
      return
c
c ... banded case (maxt .ge. 2).
c
 25   call binv (lda,n,maxt+1,fac)
      return
c
c ... nonsymmetric case.
c
 30   maxb = nb
c
c ... diagonal case (maxt = maxb = 0).
c
      if (maxt .ne. 0 .or. maxb .ne. 0) go to 40
      return
c
c ... tridiagonal case (maxt = maxb = 1).
c
 40   if (maxt .ne. 1 .or. maxb .ne. 1) go to 45
      if (nsys .le. lenv) call tinvn (n,fac,fac(1,2),fac(2,3))
      if (nsys .gt. lenv) call tinvnm (n,nsize,fac,fac(1,2),fac(2,3)) 
      return
c
c ... all other cases.
c
 45   call binvn (lda,n,maxt,maxb,fac,fac(1,2),fac(1,maxt+2))
      return
      end 
      subroutine bdsol (lda,nn,nsizee,nt,nb,fac,y,x,isym)
      implicit double precision (a-h, o-z)
c
c ... bdsol computes the solution to a dense banded matrix. 
c     thus, bdsol finds the solution to   a*x = y,  where fac
c     contains the factorization of the a matrix. 
c
c ... parameters -- 
c
c        lda    leading dimension of array fac
c        n      active size of array fac
c        nsize  size of an individual subsystem (if multiple systems) 
c                nsize = n upon input if not a multiple system
c        nt     number of diagonals needed to store the super-
c                diagonals of the factorization
c        nb     number of diagonals needed to store the sub-
c                diagonals of the factorization
c        fac    array containing the factorization of the matrix
c        y      upon input, y conains the right hand side
c        x      upon output, x contains the solution to  a*x = y
c        isym   symmetry switch
c                = 0   matrix is symmetric
c                = 1   matrix is nonsymmetric
c
c ... specifications for parameters
c
      dimension fac(lda,5), x(1), y(1)
      data lenv / 10 /
c
      n = nn
      maxt = nt
      nsize = nsizee
      nsys = n/nsize
c
c ... branch on symmetry.
c
      if (isym .eq. 1) go to 30
c
c ... symmetric case.
c
c ... diagonal case (maxt = 0).
c
      if (maxt .ne. 0) go to 15
      do 10 i = 1,n 
 10   x(i) = fac(i,1)*y(i)
      return
c
c ... tridiagonal case (maxt = 1).
c
 15   if (maxt .ne. 1) go to 20
      if (nsys .le. lenv) call tsoln (n,fac,fac(1,2),fac(1,2),y,x)
      if (nsys .gt. lenv) call tsolnm (n,nsize,fac,fac(1,2),
     a                                 fac(1,2),y,x)
      return
c
c ... pentadiagonal case (maxt = 2).
c
 20   if (maxt .ne. 2) go to 25
      if (nsys .le. lenv) call psoln (n,fac,fac(1,2),fac(1,3),
     a                                fac(1,2),fac(1,3),y,x)
      if (nsys .gt. lenv) call psolnm (n,nsize,fac,fac(1,2),fac(1,3), 
     a                                 fac(1,2),fac(1,3),y,x)
      return
c
c ... banded case (maxt .ge. 3).
c
 25   if (nsys .le. lenv) call bsol (lda,n,maxt,fac,fac(1,2),y,x)
      if (nsys .gt. lenv) call bsolm (n,nsize,maxt,fac,fac(1,2),y,x)
      return
c
c ... nonsymmetric case.
c
 30   maxb = nb
c
c ... diagonal case (maxt = maxb = 0).
c
      if (maxt .ne. 0 .or. maxb .ne. 0) go to 40
      do 35 i = 1,n 
 35   x(i) = fac(i,1)*y(i)
      return
c
c ... tridiagonal case (maxt = maxb = 1).
c
 40   if (maxt .ne. 1 .or. maxb .ne. 1) go to 45
      if (nsys .le. lenv) call tsoln (n,fac,fac(1,2),fac(2,3),y,x)
      if (nsys .gt. lenv) call tsolnm (n,nsize,fac,fac(1,2),fac(2,3), 
     a                                 y,x)
      return
c
c ... pentadiagonal case (maxt = maxb = 2).
c
 45   if (maxt .ne. 2 .or. maxb .ne. 2) go to 50
      if (nsys .le. lenv) call psoln (n,fac,fac(1,2),fac(1,3),
     a                                fac(2,4),fac(3,5),y,x)
      if (nsys .gt. lenv) call psolnm (n,nsize,fac,fac(1,2),fac(1,3), 
     a                                 fac(2,4),fac(3,5),y,x)
      return
c
c ... all other cases.
c
 50   if (nsys .le. lenv) call bsoln (lda,n,maxt,maxb,fac,fac(1,2),
     a                                fac(1,maxt+2),y,x)
      if (nsys .gt. lenv) call bsolnm (n,nsize,maxt,maxb,fac,
     a                            fac(1,2),fac(1,maxt+2),y,x)
      return
      end 
      subroutine bdsolt (lda,nn,nsizee,nt,nb,fac,y,x)
      implicit double precision (a-h, o-z)
c
c ... bdsolt computes the transpose solution to a nonsymmetric
c     dense banded matrix.
c     thus, bdsolt finds the solution to   (a**t)*x = y,  where fac
c     contains the factorization of the a matrix. 
c
c ... parameters -- 
c
c        lda    leading dimension of array fac
c        n      active size of array fac
c        nsize  size of an individual subsystem (if multiple systems) 
c                nsize = n upon input if not a multiple system
c        nt     number of diagonals needed to store the super-
c                diagonals of the factorization
c        nb     number of diagonals needed to store the sub-
c                diagonals of the factorization
c        fac    array containing the factorization of the matrix
c        y      upon input, y conains the right hand side
c        x      upon output, x contains the solution to  a*x = y
c
c ... specifications for parameters
c
      dimension fac(lda,5), x(1), y(1)
      data lenv / 10 /
c
      n = nn
      maxt = nt
      maxb = nb
      nsize = nsizee
      nsys = n/nsize
c
c ... nonsymmetric case.
c
c ... diagonal case (maxt = maxb = 0).
c
      if (maxt .ne. 0 .or. maxb .ne. 0) go to 15
      do 10 i = 1,n 
 10   x(i) = fac(i,1)*y(i)
      return
c
c ... tridiagonal case (maxt = maxb = 1).
c
 15   if (maxt .ne. 1 .or. maxb .ne. 1) go to 20
      if (nsys .le. lenv) call tsoln (n,fac,fac(2,3),fac(1,2),y,x)
      if (nsys .gt. lenv) call tsolnm (n,nsize,fac,fac(2,3),fac(1,2), 
     a                                 y,x)
      return
c
c ... pentadiagonal case (maxt = maxb = 2).
c
 20   if (maxt .ne. 2 .or. maxb .ne. 2) go to 25
      if (nsys .le. lenv) call psoln (n,fac,fac(2,4),fac(3,5),
     a                                fac(1,2),fac(1,3),y,x)
      if (nsys .gt. lenv) call psolnm (n,nsize,fac,fac(2,4),fac(3,5), 
     a                                 fac(1,2),fac(1,3),y,x)
      return
c
c ... all other cases.
c
 25   if (nsys .le. lenv) call bsolnt (lda,n,maxt,maxb,fac,fac(1,2),
     a                                fac(1,maxt+2),y,x)
      if (nsys .gt. lenv) call bsontm (n,nsize,maxt,maxb,fac,
     a                            fac(1,2),fac(1,maxt+2),y,x)
      return
      end 
      subroutine bbs (ndim,nn,maxt,t,x) 
      implicit double precision (a-h, o-z)
c
c ... bbs does a banded back substitution  (i + t)*x = y.
c     t is a rectangular matrix of adjacent super-diagonals.
c
c ... parameters -- 
c
c          ndim   row dimension of t array in defining routine
c          n      order of system
c          maxt   number of columns in t array
c          t      array of active size n by maxt giving the super-
c                  diagonals in the order 1,2,3,...
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(ndim,1), x(1)
c
      n = nn
      do 20 i = n-1,1,-1
         sum = x(i) 
         lim = min (maxt,n-i)
         do 15 j = 1,lim
            sum = sum - t(i,j)*x(i+j)
 15      continue
         x(i) = sum 
 20   continue
      return
      end 
      subroutine bbsm (nsize,nsys,maxt,t,x)
      implicit double precision (a-h, o-z)
c
c ... bbsm does a back solve  (i + t)*x = y  where t is an array
c     containing superdiagonals in order 1,2,... .
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of a single subsystem
c          nsys   number of independent subsystems
c          maxt   number of columns in t array
c          t      array of active size n by maxt containing 
c                  the super-diagonal elements of the factorization
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(nsize,nsys,1), x(nsize,1)
c
      do 25 i = nsize-1,1,-1
         lim = min (nsize-i, maxt)
         do 20 j = 1,lim
            ij = i + j
            do 15 l = 1,nsys
 15         x(i,l) = x(i,l) - t(i,l,j)*x(ij,l)
 20      continue
 25   continue
      return
      end 
      subroutine bbst (ndim,nn,maxb,b,x)
      implicit double precision (a-h, o-z)
c
c ... bbst does a backward substitution  (i + (b**t))*x = y 
c     where the array b represents sub-diagonals.  b corresponds
c     to a banded system.
c
c ... parameters -- 
c
c          ndim   row dimension of b in defining routine
c          n      order of system (= nn)
c          maxb   number of diagonals stored in b 
c          b      array of active size n x maxb giving the
c                  sub-diagonals in the order -1,-2,... .
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension b(ndim,1), x(1)
c
      n = nn
      do 25 i = n,2,-1
         term = x(i)
         lim = min (i-1,maxb)
         do 20 j = 1,lim
            x(i-j) = x(i-j) - b(i,j)*term
 20      continue
 25   continue
      return
      end 
      subroutine bbstm (nsize,nsys,maxb,b,x)
      implicit double precision (a-h, o-z)
c
c ... bbstm does the backward solve (i + (b**t))*x = y  where b
c     contains subdiagonals for multiple banded systems.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  the size of an individual subsystem
c          nsys   the number of subsystems
c          maxb   number of columns in b array
c          b      array of active size n by maxb containing 
c                  sub-diagonals in the order -1,-2,-3,...
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension b(nsize,nsys,1), x(nsize,1)
c
      do 25 i = nsize,2,-1
         lim = min (i-1,maxb)
         do 20 j = 1,lim
            do 15 l = 1,nsys
 15         x(i-j,l) = x(i-j,l) - b(i,l,j)*x(i,l) 
 20      continue
 25   continue
      return
      end 
      subroutine bfac (ndim,nn,maxt,d,t)
      implicit double precision (a-h, o-z)
c
c ... bfac computes a factorization to a single banded
c     symmetric matrix represented by d and t and replaces it.
c
c ... parameters -- 
c
c          ndim   row dimension of t array in defining routine
c          n      order of system (= nn)
c          maxt   number of columns in t array
c          d      vector containing the diagonal elements of a
c          t      array of active size n by maxt containing the
c                  super-diagonals in the order 1,2,3,...
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1)
c
      n = nn
      nm1 = n - 1
      do 20 k = 1,nm1
         pivot = d(k)
         lim = min (n-k,maxt)
         do 15 j1 = 1,lim
            term = t(k,j1)/pivot
            jcol1 = k + j1
            d(jcol1) = d(jcol1) - term*t(k,j1)
            if (j1 .eq. lim) go to 15
            j1p1 = j1 + 1
            do 10 j2 = j1p1,lim
               jcol2 = j2 - j1
               t(jcol1,jcol2) = t(jcol1,jcol2) - term*t(k,j2)
 10         continue
 15      continue
 20   continue
      do 25 i = 1,n 
 25   d(i) = 1.0d0/d(i)
      do 35 j = 1,maxt
         len = n - j
         do 30 i = 1,len
 30      t(i,j) = d(i)*t(i,j) 
 35   continue
      return
      end 
      subroutine bfacm (n,nsize,nsys,maxt,d,t)
      implicit double precision (a-h, o-z)
c
c ... bfacm computes factorizations to multiple banded
c     symmetric matrices represented by d and t and replaces it.
c
c ... parameters -- 
c
c          n      order of global system (= nn)
c          nsize  order of a single system
c          nsys   number of independent subsystems
c          maxt   number of columns in t array
c          d      vector of length n containing the diagonal
c                  elements of a
c          t      array of active size n by maxt containing the
c                  super-diagonals in the order 1,2,3,...
c
c ... specifications for parameters
c
      dimension d(nsize,1), t(nsize,nsys,1)
c
      nsm1 = nsize - 1
      do 30 k = 1,nsm1
         lim = min (nsize-k,maxt)
         do 25 j1 = 1,lim
            jcol1 = k + j1
            do 10 l = 1,nsys
 10         d(jcol1,l) = d(jcol1,l) - (t(k,l,j1)**2)/d(k,l) 
            if (j1 .eq. lim) go to 25
            j1p1 = j1 + 1
            do 20 j2 = j1p1,lim
               jcol2 = j2 - j1
               do 15 l = 1,nsys
                  t(jcol1,l,jcol2) = t(jcol1,l,jcol2)
     a               - t(k,l,j1)*t(k,l,j2)/d(k,l) 
 15            continue
 20         continue
 25      continue
 30   continue
      call vinv (n,d)
      do 35 jj = 1,maxt
         len = n - jj
         call vexopy (len,t(1,1,jj),d,t(1,1,jj),3)
 35   continue
      return
      end 
      subroutine bfacn (ndim,nn,maxt,maxb,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... bfacn computes a factorization to a single banded
c     nonsymmetric matrix represented by d, t, and b and
c     replaces it.
c
c ... parameters -- 
c
c          ndim   row dimension of t and b in defining routine
c          n      order of system (= nn)
c          maxt   number of diagonals stored in t 
c          maxb   number of diagonals stored in b 
c          d      vector of length n containing the diagonal
c                  elements of a
c          t      array of active size n x maxt giving the
c                  super-diagonals in the order 1,2,3,...
c          b      array of active size n x maxb giving the
c                  sub-diagonals in the order -1,-2,-3,...
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), b(ndim,1)
c
      n = nn
      nm1 = n - 1
      do 35 k = 1,nm1
         pivot = d(k)
         liml = min (maxb,n-k)
         limu = min (maxt,n-k)
         do 30 ip = 1,liml
            i = k + ip
            term = b(i,ip)/pivot
            do 25 jp = 1,limu 
               term1 = term*t(k,jp)
               l = jp - ip
               if (l) 10,15,20
 10            b(i,-l) = b(i,-l) - term1
               go to 25
 15            d(i) = d(i) - term1
               go to 25
 20            t(i,l) = t(i,l) - term1
 25         continue
 30      continue
 35   continue
c
      do 40 i = 1,n 
 40   d(i) = 1.0d0/d(i)
      do 50 j = 1,maxt
         len = n - j
         do 45 i = 1,len
 45      t(i,j) = d(i)*t(i,j) 
 50   continue
      do 60 j = 1,maxb
         len = n - j
         do 55 i = 1,len
 55      b(i+j,j) = d(i)*b(i+j,j)
 60   continue
      return
      end 
      subroutine bfacnm (nn,nsize,nsys,maxt,maxb,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... bfacnm computes a factorization to multiple banded
c     nonsymmetric matrices represented by d, t, and b and
c     replaces it.
c
c ... parameters -- 
c
c          nsize  size of a subsystem
c          nsys   number of independent subsystems
c          maxt   number of diagonals stored in t 
c          maxb   number of diagonals stored in b 
c          n      order of system (= nn)
c          d      vector of length n containing the diagonal
c                  elements of a
c          t      array of active size n x maxt giving the
c                  super-diagonals in the order 1,2,3,...
c          b      array of active size n x maxb giving the
c                  sub-diagonals in the order -1,-2,-3,...
c
c ... specifications for parameters
c
      dimension d(nsize,1), t(nsize,nsys,1), b(nsize,nsys,1)
c
      n = nn
      nsm1 = nsize - 1
      do 50 k = 1,nsm1
         liml = min (maxb,nsize-k)
         limu = min (maxt,nsize-k)
         do 45 ip = 1,liml
            i = k + ip
            do 40 jp = 1,limu 
               l = jp - ip
               if (l) 10,20,30
 10            do 15 m = 1,nsys
 15            b(i,m,-l) = b(i,m,-l) - b(i,m,ip)*t(k,m,jp)/d(k,m)
               go to 40
 20            do 25 m = 1,nsys
 25            d(i,m) = d(i,m) - b(i,m,ip)*t(k,m,jp)/d(k,m) 
               go to 40
 30            do 35 m = 1,nsys
 35            t(i,m,l) = t(i,m,l) - b(i,m,ip)*t(k,m,jp)/d(k,m)
 40         continue
 45      continue
 50   continue
c
      call vinv (n,d)
      do 55 j = 1,maxt
         len = n - j
         call vexopy (len,t(1,1,j),d,t(1,1,j),3)
 55   continue
      do 60 j = 1,maxb
         len = n - j
         call vexopy (len,b(j+1,1,j),d,b(j+1,1,j),3)
 60   continue
      return
      end 
      subroutine bfs (ndim,nn,maxb,b,x) 
      implicit double precision (a-h, o-z)
c
c ... bfs does a forward substitution  (i + b)*x = y  where the
c     array b represents sub-diagonals.  b corresponds to a 
c     banded system.
c
c ... parameters -- 
c
c          ndim   row dimension of b in defining routine
c          n      order of system (= nn)
c          maxb   number of diagonals stored in b 
c          b      array of active size n x maxb giving the
c                  sub-diagonals in the order -1,-2,-3,... .
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension b(ndim,1), x(1)
c
      n = nn
      do 15 i = 2,n 
         lim = min (i-1,maxb)
         sum = x(i) 
         do 10 j = 1,lim
            sum = sum - b(i,j)*x(i-j)
 10      continue
         x(i) = sum 
 15   continue
      return
      end 
      subroutine bfsm (nsize,nsys,maxb,b,x)
      implicit double precision (a-h, o-z)
c
c ... bfsm does the forward solve (i + b)*x = y  where b contains
c     subdiagonals for multiple banded systems.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  the size of an individual subsystem
c          nsys   the number of subsystems
c          maxb   number of columns in b array
c          b      array of active size n by maxb containing 
c                  sub-diagonals in the order -1,-2,-3,... .
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension b(nsize,nsys,1), x(nsize,1)
c
      do 20 i = 2,nsize
         lim = min (i-1,maxb)
         do 15 j = 1,lim
            do 10 l = 1,nsys
 10         x(i,l) = x(i,l) - b(i,l,j)*x(i-j,l)
 15      continue
 20   continue
      return
      end 
      subroutine bfst (ndim,nn,maxt,t,x)
      implicit double precision (a-h, o-z)
c
c ... bfst does a banded forward substitution  (i + (t**t))*x = y.
c     t is a rectangular matrix of adjacent super-diagonals.
c
c ... parameters -- 
c
c          ndim   row dimension of t array in defining routine
c          n      order of system
c          maxt   number of columns in t array
c          t      array of active size n by maxt giving the super-
c                  diagonals in the order 1,2,3,...
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(ndim,1), x(1)
c
      n = nn
      nm1 = n - 1
      do 20 i = 1,nm1
         term = x(i)
         lim = min (maxt,n-i)
         do 15 j = 1,lim
            x(i+j) = x(i+j) - t(i,j)*term
 15      continue
 20   continue
      return
      end 
      subroutine bfstm (nsize,nsys,maxt,t,x)
      implicit double precision (a-h, o-z)
c
c ... bfstm does a forward solve  (i + (t**t))*x = y  where t is
c     an array containing superdiagonals in order 1,2,... . 
c     (multiple systems)
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of a single subsystem
c          nsys   number of independent subsystems
c          maxt   number of columns in t array
c          t      array of active size n by maxt containing 
c                  the super-diagonal elements of the factorization
c          x      on input, x contains y
c                 vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(nsize,nsys,1), x(nsize,1)
c
      nsm1 = nsize - 1
      do 20 i = 1,nsm1
         lim = min (maxt,nsize-i)
         do 15 j = 1,lim
            ij = i + j
            do 10 l = 1,nsys
 10         x(ij,l) = x(ij,l) - t(i,l,j)*x(i,l)
 15      continue
 20   continue
      return
      end 
      subroutine binv (ndim,nn,maxnz,fact)
      implicit double precision (a-h, o-z)
c
c ... binv computes an approximate inverse to a single banded
c     symmetric matrix.  fact must contain upon input the output
c     from a factorization routine.
c
c ... parameters -- 
c
c          ndim   row dimension of fact in the defining routine
c          n      order of system (= nn)
c          maxnz  bandwidth of the factorization and inverse
c          fact   array containing factorization diagonals
c                  in the order 0,1,2,3,...
c
c ... specifications for parameters
c
      dimension fact(ndim,2)
c
      n = nn
      nm1 = n - 1
c
c ... general banded matrix.
c
      do 25 ik = 1,nm1
         k = n - ik 
         lim = min (ik+1,maxnz)
         sum1= 0.0d0
         do 15 i = 2,lim
            t1 = fact(k,i)
            sum2= 0.0d0
            do 10 j = 2,lim
               m1 = min (i,j)
               m2 = max (i,j)
               l1 = k + m1 - 1
               l2 = m2 - m1 + 1
               sum2 = sum2 - fact(k,j)*fact(l1,l2)
 10         continue
            fact(n,i) = sum2
            sum1 = sum1 - t1*sum2
 15      continue
         fact(k,1) = fact(k,1) + sum1
         do 20 i = 2,lim
 20      fact(k,i) = fact(n,i)
 25   continue
      do 30 i = 2,maxnz
 30   fact(n,i)= 0.0d0
      return
      end 
      subroutine binvn (ndim,nn,maxt,maxb,d,t,b)
      implicit double precision (a-h, o-z)
c
c ... binvn computes an approximate inverse to a single banded
c     nonsymmetric matrix.  d, t, and b must contain upon input
c     the output from a factorization routine.
c
c ... parameters -- 
c
c          ndim   row dimension of t and b in the defining routine
c          n      order of system (= nn)
c          maxt   number of columns in t
c          maxb   number of columns in b
c          d      vector of length n containing the diagonal
c                  elements of the factorization
c          t      array of active size n by maxt containing 
c                  the superdiagonals of the factorization
c                  in the order 1,2,3,...
c          b      array of active size n by maxb containing 
c                  the subdiagonals of the factorization
c                   in the order -1,-2,-3,....
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), b(ndim,1)
c
      n = nn
      nm1 = n - 1
c
c ... general banded matrix.
c
      do 75 ik = 1,nm1
         k = n - ik 
c
c ... copy kth row and column into wksp.
c
         limr = min (maxt,ik)
         limc = min (maxb,ik)
         do 10 j = 1,limr
 10      t(n,j) = t(k,j)
         do 15 j = 1,limc
 15      b(1,j) = b(k+j,j)
c
c ... do computations for kth row.
c
         do 40 j = 1,limr
            sum= 0.0d0
            lim = min (limr,limc+j)
            do 35 i = 1,lim
               kpi = k + i
               l = i - j
               if (l) 20,25,30
 20            sum = sum - t(n,i)*t(kpi,-l)
               go to 35
 25            sum = sum - t(n,i)*d(kpi)
               go to 35
 30            sum = sum - t(n,i)*b(kpi,l)
 35         continue
            t(k,j) = sum
 40      continue
c
c ... do computations for kth column.
c
         do 65 j = 1,limc
            sum= 0.0d0
            lim = min (limc,limr+j)
            kpj = k + j
            do 60 i = 1,lim
               kpi = k + i
               l = i - j
               if (l) 45,50,55
 45            sum = sum - b(1,i)*b(kpj,-l)
               go to 60
 50            sum = sum - b(1,i)*d(kpi)
               go to 60
 55            sum = sum - b(1,i)*t(kpj,l)
 60         continue
            b(kpj,j) = sum
 65      continue
c
c ... compute kth diagonal element.
c
         sum = d(k) 
         lim = min (limr,limc)
         do 70 j = 1,lim
 70      sum = sum - t(n,j)*b(k+j,j)
         d(k) = sum 
 75   continue
c
c ... zero out workspace rows.
c
      do 80 j = 1,maxt
 80   t(n,j)= 0.0d0
      do 85 j = 1,maxb
 85   b(1,j)= 0.0d0
      return
      end 
      subroutine bmul (ndim,n,maxt,d,t,x,y)
      implicit double precision (a-h, o-z)
c
c ... bmul computes y = a*x, where x and y are vectors and
c ... a is a banded symmetric matrix.
c
c ... parameters -- 
c
c         ndim          row dimension of array t
c         n             order of matrix 
c         maxt          number of columns in t
c         d             vector of length n giving the
c                        diagonal elements of a
c         t             array of size n by maxt giving the
c                        superdiagonals of a in the order
c                        1,2,....
c         x,y           vectors of order n
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), x(1), y(1)
c
      do 10 i = 1,n 
 10   y(i) = d(i)*x(i)
      if (maxt .le. 0) return 
      do 25 la = 1,maxt
         len = n - la
         do 15 i = 1,len
 15      y(i) = y(i) + t(i,la)*x(i+la)
         do 20 i = 1,len
 20      y(i+la) = y(i+la) + t(i,la)*x(i)
 25   continue
      return
      end 
      subroutine bmuln (ndim,n,maxt,maxb,d,t,b,x,y)
      implicit double precision (a-h, o-z)
c
c ... bmuln computes y = a*x, where x and y are vectors and 
c ... d, t, and b represent a stored in nonsymmetric band
c ... storage format.
c
c ... parameters -- 
c
c         ndim          row dimension of arrays t and b
c         n             order of array a
c         maxt          number of columns in t array
c         maxb          number of columns in b array
c         d             vector of length n giving the diagonal
c                        elements of a
c         t             array of active size n by maxt giving
c                        the super-diagonals of a in the order
c                        1,2,3,...
c         b             array of active size n by maxb giving
c                        the sub-diagonals of a in the order
c                        -1,-2,-3,....
c         x,y           vectors of order n
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), b(ndim,1), x(1), y(1)
c
      do 10 i = 1,n 
 10   y(i) = d(i)*x(i)
      if (maxt .lt. 1) go to 25
      do 20 j = 1,maxt
         len = n - j
         do 15 i = 1,len
 15      y(i) = y(i) + t(i,j)*x(i+j)
 20   continue
 25   if (maxb .lt. 1) return 
      do 35 j = 1,maxb
         len = n - j
         do 30 i = 1,len
 30      y(i+j) = y(i+j) + b(i+j,j)*x(i)
 35   continue
      return
      end 
      subroutine bmulnt (ndim,n,maxt,maxb,d,t,b,x,y)
      implicit double precision (a-h, o-z)
c
c ... bmulnt computes y = (a**t)*x, where x and y are vectors and
c ... d, t, and b represent a stored in nonsymmetric band
c ... storage format.
c
c ... parameters -- 
c
c         ndim          row dimension of arrays t and b
c         n             order of array a
c         maxt          number of columns in t array
c         maxb          number of columns in b array
c         d             vector of length n giving the diagonal
c                        elements of a
c         t             array of active size n by maxt giving
c                        the super-diagonals of a in the order
c                        1,2,3,...
c         b             array of active size n by maxb giving
c                        the sub-diagonals of a in the order
c                        -1,-2,-3,...
c         x,y           vectors of order n
c
c ... specifications for parameters
c
      dimension d(1), t(ndim,1), b(ndim,1), x(1), y(1)
c
      do 10 i = 1,n 
 10   y(i) = d(i)*x(i)
      if (maxt .lt. 1) go to 25
      do 20 j = 1,maxt
         len = n - j
         do 15 i = 1,len
 15      y(i+j) = y(i+j) + t(i,j)*x(i)
 20   continue
 25   if (maxb .lt. 1) return 
      do 35 j = 1,maxb
         len = n - j
         do 30 i = 1,len
 30      y(i) = y(i) + b(i+j,j)*x(i+j)
 35   continue
      return
      end 
      subroutine bsol (ndim,nn,maxt,d,t,y,x)
      implicit double precision (a-h, o-z)
c
c ... bsol solves a*x = y for a banded and symmetric matrix a. d and
c     t must contain upon input the factorization arrays from bfac.
c
c ... parameters -- 
c
c          ndim   row dimension of t array in defining routine
c          n      order of system
c          maxt   number of columns in t array
c          d      vector of length n containing the diagonal
c                  pivots of the factorization
c          t      array of active size n by maxt giving the super-
c                  diagonals of the factorization in the order
c                  1,2,3,...
c          y      right-hand-side vector
c          x      vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(ndim,1), x(1), y(1), d(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call bfst (ndim,n,maxt,t,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call bbs (ndim,n,maxt,t,x)
      return
      end 
      subroutine bsolm (nn,nsize,maxt,d,t,y,x)
      implicit double precision (a-h, o-z)
c
c ... bsolm solves the system  ax = y  for x, where a is multiple
c     symmetric banded matrices whose factorizations are contained in 
c     d and t.
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of a single subsystem
c          maxt   number of columns in t array
c          d      vector of length n containing the diagonal
c                  elements of the factorization
c          t      array of active size n by maxt containing 
c                  the super-diagonal elements of the factorization
c                  in the order 1,2,3,...
c          y      right-hand-side vector
c          x      vector containing solution upon output
c
c ... specifications for parameters
c
      dimension d(1), t(1), y(1), x(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      nsys = n/nsize
      call bfstm (nsize,nsys,maxt,t,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call bbsm (nsize,nsys,maxt,t,x)
      return
      end 
      subroutine bsoln (ndim,nn,maxt,maxb,d,t,b,y,x)
      implicit double precision (a-h, o-z)
c
c ... bsoln solves a*x = y for a banded and nonsymmetric matrix a.
c     d, t, and b must contain upon input the factorization arrays
c     from bfacn.
c
c ... parameters -- 
c
c          ndim   row dimension of t array in defining routine
c          n      order of system
c          maxt   number of columns in t array
c          maxb   number of columns in b array
c          d      vector of length n containing the diagonal
c                  pivots of the factorization
c          t      array of active size n by maxt giving the super-
c                  diagonals of the factorization in the order
c                  1,2,3,...
c          b      array of active size n by maxb giving the sub-
c                  diagonals of the factorization in the order
c                  -1,-2,-3,...
c          y      right-hand-side vector
c          x      vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(ndim,1), x(1), y(1), d(1), b(ndim,1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call bfs (ndim,n,maxb,b,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call bbs (ndim,n,maxt,t,x)
      return
      end 
      subroutine bsolnm (nn,nsize,maxt,maxb,d,t,b,y,x)
      implicit double precision (a-h, o-z)
c
c ... bsolnm solves a*x = y for a banded and nonsymmetric matrix a.
c     d, t, and b must contain upon input the factorization arrays
c     from bfacnm.  (multiple systems)
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of an individual subsystem 
c          maxt   number of columns in t array
c          maxb   number of columns in b array
c          d      vector of length n containing the diagonal
c                  pivots of the factorization
c          t      array of active size n by maxt giving the super-
c                  diagonals of the factorization in the order
c                  1,2,3,...
c          b      array of active size n by maxb giving the sub-
c                  diagonals of the factorization in the order
c                  -1,-2,-3,...
c          y      right-hand-side vector
c          x      vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(1), x(1), y(1), d(1), b(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      nsys = n/nsize
      call bfsm (nsize,nsys,maxb,b,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call bbsm (nsize,nsys,maxt,t,x)
      return
      end 
      subroutine bsolnt (ndim,nn,maxt,maxb,d,t,b,y,x)
      implicit double precision (a-h, o-z)
c
c ... bsolnt solves (a**t)*x = y for a banded and nonsymmetric
c     matrix a.  d, t, and b must contain upon input the
c     factorization arrays from bfacn.
c
c ... parameters -- 
c
c          ndim   row dimension of t array in defining routine
c          n      order of system
c          maxt   number of columns in t array
c          maxb   number of columns in b array
c          d      vector of length n containing the diagonal
c                  pivots of the factorization
c          t      array of active size n by maxt giving the super-
c                  diagonals of the factorization in the order
c                  1,2,3,...
c          b      array of active size n by maxb giving the sub-
c                  diagonals of the factorization in the order
c                  -1,-2,-3,...
c          y      right-hand-side vector
c          x      vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(ndim,1), x(1), y(1), d(1), b(ndim,1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call bfst (ndim,n,maxt,t,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call bbst (ndim,n,maxb,b,x)
      return
      end 
      subroutine bsontm (nn,nsize,maxt,maxb,d,t,b,y,x)
      implicit double precision (a-h, o-z)
c
c ... bsontm solves (a**t)*x = y for a banded and nonsymmetric
c     matrix a.  d, t, and b must contain upon input the
c     factorization arrays from bfacnm.  (multiple systems) 
c
c ... parameters -- 
c
c          n      order of system
c          nsize  size of an individual subsystem 
c          maxt   number of columns in t array
c          maxb   number of columns in b array
c          d      vector of length n containing the diagonal
c                  pivots of the factorization
c          t      array of active size n by maxt giving the super-
c                  diagonals of the factorization in the order
c                  1,2,3,...
c          b      array of active size n by maxb giving the sub-
c                  diagonals of the factorization in the order
c                  -1,-2,-3,...
c          y      right-hand-side vector
c          x      vector containing solution upon output
c
c ... specifications for parameters
c
      dimension t(1), x(1), y(1), d(1), b(1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      nsys = n/nsize
      call bfstm (nsize,nsys,maxt,t,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call bbstm (nsize,nsys,maxb,b,x)
      return
      end 
      subroutine bicol (n,nz,ia,ja,count,father,oppos,propa)
      implicit double precision (a-h, o-z)
c
c ... bicolor determines whether or not the matrix represented
c     in the sparse (ia,ja) format is bi-colorable.
c     the algorithm used is the union-find algorithm.
c
c ... parameters -- 
c
c        n      number of vertices
c        nz     number of edges (length of ia and ja vectors)
c        ia     integer vector of i values
c        ja     integer vector of j values
c        count  integer workspace vectors of length n each
c        father upon output, count gives the color of each node
c        oppos
c        propa  logical variable indicating on output whether
c                matrix has property a
c
c ... specification of parameters
c
      logical propa 
      integer ia(1), ja(1), count(1), father(1), oppos(1)
      integer v, w, w0, a, b, c, d
c
      do 10 i = 1,n 
         count(i) = 1
         father(i) = 0
         oppos(i) = 0
 10   continue
      do 60 k = 1,nz
         if (ia(k) .eq. ja(k)) go to 60 
c
c ... a = find (ia(k)).
c
         v = ia(k)
 15      if (father(v) .eq. 0) go to 20 
         v = father(v)
         go to 15
 20      w = ia(k)
 25      if (father(w) .eq. 0) go to 30 
         w0 = w
         w = father(w)
         father(w0) = v
         go to 25
 30      a = v
c
c ... b = find (ja(k)).
c
         v = ja(k)
 35      if (father(v) .eq. 0) go to 40 
         v = father(v)
         go to 35
 40      w = ja(k)
 45      if (father(w) .eq. 0) go to 50 
         w0 = w
         w = father(w)
         father(w0) = v
         go to 45
 50      b = v
c
c ... test for a = b.
c
         if (a .ne. b) go to 55
         propa = .false.
         return
c
c ... do unioning.
c
 55      if (oppos(a) .eq. b) go to 60
         if (oppos(b) .eq. 0) then
            c = a
         else
c
c ... c = merge (a,oppos(b)). 
c
            i = a
            j = oppos(b)
            if (count(i) .ge. count(j)) then
               father(j) = i
               count(i) = count(i) + count(j)
               c = i
            else
               father(i) = j
               count(j) = count(i) + count(j)
               c = j
            endif
         endif
         if (oppos(a) .eq. 0) then
            d = b
         else
c
c ... d = merge (b,oppos(a)). 
c
            i = b
            j = oppos(a)
            if (count(i) .ge. count(j)) then
               father(j) = i
               count(i) = count(i) + count(j)
               d = i
            else
               father(i) = j
               count(j) = count(i) + count(j)
               d = j
            endif
         endif
         oppos(c) = d
         oppos(d) = c
 60   continue
c
c ... do coloring.
c
      do 65 i = 1,n 
 65   count(i) = 0
      do 90 i = 1,n 
c
c ... a = find(i).
c
         v = i
 70      if (father(v) .eq. 0) go to 75 
         v = father(v)
         go to 70
 75      w = i
 80      if (father(w) .eq. 0) go to 85 
         w0 = w
         w = father(w)
         father(w0) = v
         go to 80
 85      a = v
         if (count(a) .eq. 0) then
            count(a) = 1
            count(i) = 1
            j = oppos(a)
            if (j .ne. 0) count(j) = 2
         else
            count(i) = count(a)
         endif
 90   continue
      propa = .true.
      return
      end 
      subroutine chgcon (tri,ier)
      implicit double precision (a-h, o-z)
c
c ... chgcon computes the new estimates for the largest and 
c     smallest eigenvalues (emax and emin) for conjugate gradient
c     acceleration. 
c
c ... parameters -- 
c
c          tri    tridiagonal matrix associated with the eigenvalues
c                    of the conjugate gradient polynomial
c          ier    error code
c
c ... specifications for parameters
c
      dimension tri(2,2)
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
c     description of variables in common blocks in main routine
c
      save tl1,tl2,bl1,bl2
      ip = is
      if (ip - 1) 10,20,30
c
c ... ip = 0
c
 10   end = 1.0d0/alpha
      tri(1,1) = end
      tri(2,1)= 0.0d0 
      if (maxadp) emax = end
      if (minadp) emin = end
      return
c
c ... ip = 1
c
 20   t1 = 1.0d0/alpha + beta/alphao
      t2 = beta/(alphao**2)
      tri(1,2) = t1 
      tri(2,2) = t2 
      tsqr = sqrt (t2)
      tl1 = tri(1,1) + tsqr
      tl2 = t1 + tsqr
      bl1 = tri(1,1) - tsqr
      bl2 = t1 - tsqr
      t3 = tri(1,1) + t1
      t4 = sqrt ( (t1-tri(1,1))**2 + 4.0d0*t2 )
      if (maxadp) emax = (t3 + t4)/2.0d0
      if (minadp) emin = (t3 - t4)/2.0d0
      return
c
c ... ip .ge. 2
c
 30   t1 = 1.0d0/alpha + beta/alphao
      t2 = beta/(alphao**2)
      tsqr = sqrt (t2)
      tri(1,ip+1) = t1
      tri(2,ip+1) = t2
      if (.not. maxadp) go to 40
c
c ... compute new estimate of emax.
c
      tl1 = max (tl1,tl2+tsqr)
      tl2 = t1 + tsqr
      emaxo = emax
      end = max (tl1,tl2)
      e1 = eigvss (ip+1,tri,emaxo,end,2,ier)
      if (ier .ne. 3  .and.  ier .ne. 4) go to 35 
c
c ... poor estimate for emax.  therefore need to stop adaptive
c     procedure and keep old value of emax.
c
      maxadp = .false.
      if (level .ge. 2) write (nout,31) ier,in,emaxo
 31   format (/5x,'estimation of maximum eigenvalue emax halted'
     a        /5x,'routine zbrent returned ier = ',i5
     b        /5x,'adaptive procedure turned off at iteration ',i5
     c        /5x,'final estimate of maximum eigenvalue =',d15.7/)
      go to 40
c
c ... valid emax estimate.  check for small relative change in emax.
c
 35   emax = e1
      if (abs (emax - emaxo) .lt. emax*zeta) maxadp = .false.
c
c ... compute new estimate of emin.
c
 40   if (.not. minadp) return
      bl1 = min (bl1,bl2-tsqr)
      bl2 = t1 - tsqr
      start = max ( 0.0d0, min (bl1,bl2) )
      emino = emin
      e1 = eigvss (ip+1,tri,start,emino,1,ier)
      if (ier .ne. 3  .and.  ier .ne. 4) go to 45 
c
c ... poor estimate for emin.  therefore need to stop adaptive
c     procedure and keep old value of emin.
c
      minadp = .false.
      if (level .ge. 2) write (nout,41) ier,in,emino
 41   format (/5x,'estimation of minimum eigenvalue emin halted'
     a        /5x,'routine zbrent returned ier = ',i5
     b        /5x,'adaptive procedure turned off at iteration ',i5
     c        /5x,'final estimate of minimum eigenvalue =',d15.7/)
      return
c
c ... valid emin estimate.  check for small relative change in emin.
c
 45   emin = e1
      if (abs (emin - emino) .lt. emin*zeta) minadp = .false.
      return
      end 
      subroutine chgsi (suba,coef,jcoef,wfac,jwfac,nn,z,wksp,
     a                  icode,ier)
      implicit double precision (a-h, o-z)
c
c ... chgsi adapts on the iteration parameters.
c
c ... parameters -- 
c
c         n         order of system (= nn)
c         z         current pseudo-residual vector
c         wksp      workspace vector of length n
c         icode     output indicator of parameter changes
c                    = 0    estimates of emax, emin not changed
c                    = 1    estimates of emax, emin changed 
c         ier       error code
c
c ... specifications for parameters
c
      external suba 
      integer jcoef(2), jwfac(1)
      dimension z(1), wksp(1), coef(1), wfac(1)
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
      n = nn
c
      istar = 3
      icode = 0
      if (is .eq. 0) return
      rnrm = sqrt (rzdot)
      rnrmq = sqrt (dkq)
      rnrm1 = sqrt (dkm1)
      qa = rnrm/rnrmq
      t1 = rr**is
      qt = 2.0d0*sqrt (t1)/(1.0d0 + t1)
      if (qa .le. qt**ff) return
      if (qa .le. 1.0d0  .and.  is .le. istar) return
      icode = 1
c
c ... compute rayleigh quotient.
c ...        rq = (z,a*z)/(r,z)
c
      call suba (coef,jcoef,wfac,jwfac,n,z,wksp)
      top= 0.0d0
      do 10 i = 1,n 
 10   top = top + z(i)*wksp(i)
      if (top .ge. 0.0d0) go to 15
      ier = -6
      call ershow (ier,'chgsi')
      return
 15   rq = top/rzdot
      kode = 0
      if (rq .gt. rqmax) kode = 1
      rqmin = min (rq,rqmin)
      rqmax = max (rq,rqmax)
      yy = (1.0d0+t1)*(qa+sqrt (qa*qa-qt*qt))/2.0d0
      xx = yy**(1.0d0/dble (is))
      if (qa .gt. 1.0d0) go to 25
      if (kode .eq. 1) go to 25
c
c ... emin adjustment.
c
      eminp = (emax+emin)*(1.0d0-xx)*(xx-rr)/(2.0d0*xx*(rr+1.0d0))
      if (minadp) emin = min (emin,eminp,rqmin) 
      if (maxadp) emax = max (emax,rqmax)
      if (level .ge. 2) write (nout,20) in,rq,eminp,emin,emax
 20   format (/1x,15x,'parameters were changed at iteration',i7/
     a        1x,20x,'rayleigh quotient  ',f15.9/ 
     a        1x,20x,'young estimate     ',f15.9/ 
     a        1x,20x,'emin               ',f15.9/ 
     a        1x,20x,'emax               ',f15.9/)
      return
c
c ... emax adjustment.
c
 25   emaxp = (emax+emin)*(1.0d0+xx)*(xx+rr)/(2.0d0*xx*(rr+1.0d0))
      uu = ((1.0d0+t1)/(1.0d0+rr**(is-1))) * (rnrm/rnrm1)
      emaxpp = (emax+emin)*(1.0d0+uu)*(uu+rr)/(2.0d0*uu*(rr+1.0d0))
      if (maxadp) emax = max (emax,1.1d0*emaxp,1.1d0*emaxpp,1.1d0*rqmax)
      if (minadp) emin = rqmin
      if (level .ge. 2) write (nout,30) in,rq,emaxp,emaxpp,emin,emax
 30   format (/1x,15x,'parameters were changed at iteration',i7/
     a        1x,20x,'rayleigh quotient  ',f15.9/ 
     a        1x,20x,'young estimate     ',f15.9/ 
     a        1x,20x,'hageman estimate   ',f15.9/ 
     a        1x,20x,'emin               ',f15.9/ 
     a        1x,20x,'emax               ',f15.9/)
      return
      end 
      subroutine color (nxp,nyp,nzp,nx,ny,nz,pp,p)
      implicit double precision (a-h, o-z)
c
c ... routine color reproduces a color pattern given by array
c     pp of dimensions nxp x nyp x nzp into the grid color
c     array p of dimensions nx x ny x nz.
c
c ... parameters -- 
c
c       nxp,    integer variables giving the x, y, and z dimensions
c        nyp,    of the pattern array, respectively.
c        nzp
c       nx,ny,  integer variables giving the x, y, and z dimensions
c        nz      of the grid, respectively.
c       pp      integer vector of length  nxp*nyp*nzp
c                giving the color pattern to be repeated
c       p       integer vector of length  nxg*nyg*nzg
c                which contains upon output the grid coloring
c
c ... specifications for parameters
c
      integer pp (nxp,nyp,nzp), p (nx,ny,nz)
c
      do 30 k = 1,nz
         kp = mod (k - 1, nzp) + 1
         do 20 j = 1,ny
            jp = mod (j - 1, nyp) + 1
            do 10 i = 1,nx
               ip = mod (i - 1, nxp) + 1
               p (i,j,k) = pp (ip,jp,kp)
 10         continue
 20      continue
 30   continue
      return
      end 
      subroutine defcon (ndim,nn,maxnz,jcoef,coef,kblsz,iblock,lbhb)
      implicit double precision (a-h, o-z)
c
c ... define defines block constants for block-structured matrices.
c     (diagonal data structure, constant block size)
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         nn       size of system
c         maxnz    number of diagonals in coef
c         jcoef    integer vector of size maxnz giving the diagonal
c                   numbers
c         coef     matrix representation array
c         kblsz    constant block size
c         iblock   integer array of size 3 by lbhb
c                   giving block constants upon output
c         lbhb     integer giving the number of diagonal blocks
c                   upon output.
c
c ... specifications for parameters
c
      integer   jcoef(2), iblock(3,3)
      dimension coef(ndim,1)
c
      n = nn
      ipt = 2
      iblock(1,1) = 0
      iblock(1,2) = 0
      iblock(2,1) = 1
      iblock(3,1) = 0
      iblock(3,2) = 0
      do 25 j = 1,maxnz
         jd = jcoef(j)
         do 10 i = 1,n
            if (coef(i,j) .ne. 0.0d0) go to 15
 10      continue
         go to 25
 15      jcol = i + jd
c
c ... find block for jcol.
c
         ib = (i-1)/kblsz + 1 
         jb = (jcol-1)/kblsz + 1
         id = jb - ib
         if (id .eq. iblock(1,ipt)) go to 20
         ipt = ipt + 1
         iblock(1,ipt) = id
         iblock(3,ipt) = 0
 20      iblock(3,ipt) = iblock(3,ipt) + 1
 25   continue
      lbhb = ipt
c
c ... split zero diagonal block into super and sub diagonals.
c
      jlim = iblock(3,2)
      do 30 j = 1,jlim
         jd = jcoef(j)
         if (jd .lt. 0) go to 35
         iblock(3,1) = iblock(3,1) + 1
         iblock(3,2) = iblock(3,2) - 1
 30   continue
      j = jlim + 1
 35   iblock(2,2) = j
c
c ... form starting positions.
c
      if (lbhb .le. 2) return 
      iblock(2,3) = 1
      if (lbhb .le. 3) return 
      do 40 j = 4,lbhb
 40   iblock(2,j) = iblock(2,j-1) + iblock(3,j-1) 
      return
      end 
      subroutine define (ndim,maxnew,jcnew,coef,ncol,nc,
     a                   iblock,lbhb)
      implicit double precision (a-h, o-z)
c
c ... define defines block constants for block-structured matrices.
c     (diagonal data structure, nonconstant block size)
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         maxnew   integer vector giving the number of diagonals
c                   for each distinct block size. 
c         jcnew    integer array of size ncolor*max(maxnew(i))
c                   giving the diagonal numbers for each distinct
c                   block size.
c         coef     matrix representation array
c         ncolor   number of distinct block sizes 
c         nc       integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants upon output
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size
c                   upon output.
c
c ... specifications for parameters
c
      integer   maxnew(ncol), jcnew(ncol,1), nc(ncol), lbhb(ncol),
     a          iblock(3,ncol,3)
      dimension coef(ndim,1)
c
      ncolor = ncol 
      ist = 1
      do 60 k = 1,ncolor
         ncc = nc(k)
         maxnz = maxnew(k)
         ied = ist + ncc - 1
         ipt = 2
         iblock(1,k,1) = 0
         iblock(1,k,2) = 0
         iblock(2,k,1) = 1
         iblock(3,k,1) = 0
         iblock(3,k,2) = 0
         do 35 j = 1,maxnz
            jd = jcnew(k,j)
            do 10 i = ist,ied 
               if (coef(i,j) .ne. 0.0d0) go to 15
 10         continue
            go to 35
 15         jcol = i + jd
c
c ... find block for jcol.
c
            ib = k
            js = 0
            do 20 ij = 1,ncolor
               js = js + nc(ij)
               if (js .ge. jcol) go to 25
 20         continue
 25         jb = ij 
            id = jb - ib
            if (id .eq. iblock(1,k,ipt)) go to 30 
            ipt = ipt + 1
            iblock(1,k,ipt) = id
            iblock(3,k,ipt) = 0
 30         iblock(3,k,ipt) = iblock(3,k,ipt) + 1 
 35      continue
         lbhb(k) = ipt
c
c ... split zero diagonal block into super and sub diagonals.
c
         jlim = iblock(3,k,2) 
         do 40 j = 1,jlim
            jd = jcnew(k,j)
            if (jd .lt. 0) go to 45
            iblock(3,k,1) = iblock(3,k,1) + 1
            iblock(3,k,2) = iblock(3,k,2) - 1
 40      continue
         j = jlim + 1
 45      iblock(2,k,2) = j
c
c ... form starting positions.
c
         jlim = lbhb(k)
         if (jlim .le. 2) go to 55
         iblock(2,k,3) = 1
         if (jlim .le. 3) go to 55
         do 50 j = 4,jlim
 50      iblock(2,k,j) = iblock(2,k,j-1) + iblock(3,k,j-1)
 55      ist = ied + 1
 60   continue
      return
      end 
      double precision function determ (n,tri,xlmda)
      implicit double precision (a-h, o-z)
c
c     determ computes the determinant of a symmetric
c     tridiagonal matrix given by tri. det(tri - xlmda*i) = 0
c
c ... parameters -- 
c
c          n      order of tridiagonal system
c          tri    symmetric tridiagonal matrix of order n
c          xlmda  argument for characteristic equation
c
c ... specifications for parameters
c
      dimension tri(2,1)
c
      nm1 = n - 1
      d2 = tri(1,n) - xlmda
      d1 = d2 * (tri(1,nm1) - xlmda) - tri(2,n)
         if (n .eq. 2) go to 20
c
c ... beginning of loop
c
      do 10 l = nm1,2,-1
         d3 = d2
         d2 = d1
         d1 = (tri(1,l-1) - xlmda) * d2 - d3 * tri(2,l)
   10 continue
c
c ... determinant computed
c
   20 determ = d1
c
      return
      end 
      subroutine detsym (ndim,maxnzz,coef,jcoef,nn,isymm)
      implicit double precision (a-h, o-z)
c
c ... detsym determines if the matrix is symmetric.
c     (purdue storage format) 
c
c ... parameters -- 
c
c         ndim     row dimension of coef in defining routine
c         maxnz    number of columns in coef
c         coef     array of matrix nonzeros
c         jcoef    array of matrix column numbers 
c         n        order of matrix (= nn)
c         isymm    symmetry switch.  upon output, 
c                   isymm = 0  if matrix is symmetric
c                         = 1  if matrix is nonsymmetric
c
c ... specifications for parameters
c
      dimension coef(ndim,2)
      integer   jcoef(ndim,2) 
c
      n = nn
      maxnz = maxnzz
      isymm = 0
      if (maxnz .le. 1) return
      do 20 i = 1,n 
         do 15 j = 2,maxnz
            jcol = jcoef(i,j) 
            if (jcol .eq. i) go to 15
            val = coef(i,j)
            do 10 jj = 2,maxnz
               jcol1 = jcoef(jcol,jj)
               if (jcol1 .ne. i) go to 10
               val1 = coef(jcol,jj)
               if (val1 .eq. val) go to 15
               isymm = 1
               return
 10         continue
            isymm = 1
            return
 15      continue
 20   continue
      return
      end 
      subroutine echall (n,iparm,rparm,icall,icallr,ier)
      implicit double precision (a-h, o-z)
c
c ... echall initializes the package common blocks from the 
c ... information contained in iparm and rparm.  echall also
c ... prints the values of all parameters in iparm and rparm.
c
c ... parameters -- 
c
c          iparm
c           and
c          rparm  arrays of parameters specifying options and
c                    tolerances
c          icall  indicator of which parameters are being printed
c                    icall = 1,  initial parameters
c                          = 2,  final parameters 
c          icallr  indicator of calling routine
c                          = 1,  called from nspcg
c                          = 2,  called from accelerator
c
c ... specifications for parameters
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
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / itcom8 / ainf
c
c *** end   -- package common 
c
      logical erflag
      integer   iparm(25)
      dimension rparm(16)
      character*6 inames(25), rnames(16)
      data naiprm, narprm / 11,12 /
      data inames / 'ntest', 'itmax', 'level', 'nout', 'idgts',
     a              'maxadp', 'minadp', 'iomgad', 'ns1', 'ns2', 'ns3',
     a              'nstore', 'iscale', 'iperm', 'ifact', 'lvfill',
     a              'ltrunc', 'ipropa', 'kblsz', 'nbl2d', 'ifctv',
     a              'iqlr', 'isymm', 'ielim', 'ndeg' /
      data rnames / 'zeta', 'emax', 'emin', 'ff', 'fff', 'timit',
     a              'digit1', 'digit2', 'omega', 'alphab', 'betab',
     a              'specr', 'timfac', 'timtot', 'tol', 'ainf' /
c
      if (icall .ne. 1) go to 20
c
c handle accelerator parameters ...
c
      ntest  = iparm(1)
      itmax  = iparm(2)
      level  = iparm(3)
      nout   = iparm(4)
      idgts  = iparm(5)
      maxad  = iparm(6)
      maxadd = (maxad .eq. 1) 
      minad  = iparm(7)
      minadd = (minad .eq. 1) 
      maxadp = maxadd
      minadp = minadd
      iomgad = iparm(8)
      omgadp = (iomgad .eq. 1)
      ns1    = iparm(9)
      ns2    = iparm(10)
      ns3    = iparm(11)
      iqlr   = iparm(22)
      iplr   = iqlr 
c
      zeta   = rparm(1)
      emax   = rparm(2)
      emin   = rparm(3)
      ff     = rparm(4)
      fff    = rparm(5)
      timit  = rparm(6)
      digit1 = rparm(7)
      digit2 = rparm(8)
      omega  = rparm(9)
      alphab = rparm(10)
      betab  = rparm(11)
      specr  = rparm(12)
c
      erflag = .false.
      erflag = erflag .or. ntest .lt. 1 .or. ntest .gt. 10
      erflag = erflag .or. itmax .le. 0 
      erflag = erflag .or. maxad .lt. 0 .or. maxad .gt. 1
      erflag = erflag .or. minad .lt. 0 .or. minad .gt. 1
      erflag = erflag .or. ns1 .lt. 0
      erflag = erflag .or. ns2 .lt. 0
      erflag = erflag .or. emax .lt. 0.0d0
      erflag = erflag .or. emin .lt. 0.0d0
      erflag = erflag .or. ff .le. 0.0d0 .or. ff .gt. 1.0d0
      if (erflag) go to 999
c
c ... test if eps is too small
c
      temp = 500.0d0*srelpr
      if (zeta .ge. temp) go to 150
      ier = 2
      call ershow (ier,'echall')
      zeta = temp
      rparm(1) = temp
c
c ... verify n
c
 150  if (n .gt. 0 ) go to 200
      ier = -1
      call ershow (ier,'echall')
      return
c
c now handle preconditioner parameters ...
c
 200  if (icallr .eq. 2) go to 50
      nstore = iparm(12)
      iscale = iparm(13)
      iperm  = iparm(14)
      ifact  = iparm(15)
      lvfill = iparm(16)
      ltrunc = iparm(17)
      ipropa = iparm(18)
      nbl1d  = iparm(19)
      nbl2d  = iparm(20)
      ifctv  = iparm(21)
      iqlr   = iparm(22)
      isymm  = iparm(23)
      ndeg   = iparm(25)
      ainf   = rparm(16)
c
      if (nbl1d .eq. -1) nbl1d = n
      if (nbl2d .eq. -1) nbl2d = n
      kblsz = nbl1d 
      erflag = .false.
      erflag = erflag .or. iqlr .lt. 0 .or. iqlr .gt. 3
      erflag = erflag .or. ipropa .lt. 0 .or. ipropa .gt. 3 
      if (erflag) go to 999
c
c
c ... initialize rest of common variables
c
 50   halt   = .false.
      stptst= 0.0d0
      udnm   = 1.0d0
      in     = 0
c
c prepare to do output ...
c
      if (level .le. 2) return
      write (nout,15)
 15   format (/5x,'initial iterative parameters') 
      go to 30
c
 20   if (level .le. 2) return
      write (nout,25)
 25   format (/5x,'final iterative parameters')
c
 30   if (icallr .eq. 2) go to 305
      write (nout,301)
 301  format (5x,'preprocessor and preconditioner parameters')
      ibip = naiprm + 1
      ieip = 25
      ibrp = narprm + 1
      ierp = 16
      go to 300
 305  write (nout,302)
 302  format (5x,'general and acceleration parameters')
      ibip = 1
      ieip = naiprm 
      ibrp = 1
      ierp = narprm 
c
 300  write (nout,35) (i,iparm(i),inames(i),i=ibip,ieip)
 35   format (10x,'iparm(',i2,') =',i15,4x,'(',a6,')'  )
      write (nout,40) (i,rparm(i),rnames(i),i=ibrp,ierp)
 40   format (10x,'rparm(',i2,') =',d15.8,4x,'(',a6,')'  )
      return
c
c error returns ... 
c
c inadmissible option ...
 999  ier = -10
      call ershow (ier,'echall')
      return
      end 
      double precision function eigvss (n,tri,start,end,icode,ier) 
      implicit double precision (a-h, o-z)
c
c ... eigvss computes a selected eigenvalue of a symmetric
c     tridiagonal matrix for conjugate gradient acceleration.
c     modified imsl routine zbrent used.
c
c ... parameters -- 
c
c          n      order of tridiagonal system
c          tri    symmetric tridiagonal matrix of order n
c          start  initial lower bound of interval containing root
c          end    initial upper bound of interval containing root
c          icode  operation key
c                   = 1   minimum eigenvalue sought
c                   = 2   maximum eigenvalue sought
c          ier    error flag
c
c ... specifications for parameters
c
      dimension tri(2,1)
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
      eigvss= 0.0d0
      itmp = int (-dlog10 (abs (zeta))) 
      nsig = max (itmp,4)
      maxfn = max (itmax,50) 
      eps= 0.0d0
      a = start
      b = end
      call zbrent (n,tri,eps,nsig,a,b,maxfn,ier)
      if (icode .eq. 1) eigvss = max (a,b)
      if (icode .eq. 2) eigvss = min (a,b)
c
      return
      end 
      subroutine elim1 (nn,ndim,maxnzz,jcoef,coef,rhs,wksp,toll)
      implicit double precision (a-h, o-z)
c
c ... elim1 removes rows of the matrix for which the ratio of the
c     sum of off-diagonal elements to the diagonal element is
c     small (less than tol) in absolute value.
c     this is to take care of matrices arising from finite
c     element discretizations of partial differential equations
c     with dirichlet boundary conditions implemented by penalty
c     methods.  any such rows and corresponding columns are then
c     eliminated (set to the identity after correcting the rhs).
c     purdue format.
c
c ... parameter list --
c
c         n       dimension of matrix ( = nn)
c         ndim    row dimension of arrays jcoef and coef in the
c                    calling program
c         maxnz   maximum number of nonzero entries per row (=maxnzz) 
c         jcoef   integer array of matrix representation
c         coef    array of sparse matrix representation
c         rhs     right hand side of matrix problem
c         wksp    wksp array of length n
c         tol     tolerance factor  (= toll)
c
c ... specifications for arguments
c
      integer   jcoef(ndim,1) 
      dimension coef(ndim,1), rhs(1), wksp(1)
c
      n = nn
      maxnz = maxnzz
      tol = toll
      if (n .le. 0  .or.  maxnz .lt. 2) return
c
c ... find maximum off-diagonal elements in absolute value. 
c
      do 10 i = 1,n 
 10   wksp(i)= 0.0d0
      do 20 j = 2,maxnz
         do 15 i = 1,n
 15      wksp(i) = wksp(i) + abs (coef(i,j))
 20   continue
      do 25 i = 1,n 
 25   wksp(i) = wksp(i) / abs(coef(i,1))
c
c ... eliminate desired rows and columns.
c
      do 35 i = 1,n 
         if (wksp(i) .gt. tol) go to 35 
         rhs(i) = rhs(i)/coef(i,1)
         coef(i,1) = 1.0d0
         do 30 j = 2,maxnz
            coef(i,j)= 0.0d0
            jcoef(i,j) = i
 30      continue
 35   continue
      do 45 j = 2,maxnz
         do 40 i = 1,n
            jcol = jcoef(i,j) 
            if (wksp(jcol) .gt. tol) go to 40
            rhs(i) = rhs(i) - coef(i,j)*rhs(jcol) 
            coef(i,j)= 0.0d0
            jcoef(i,j) = i
 40      continue
 45   continue
      return
      end 
      subroutine elim2 (nn,ndim,maxnzz,jcoef,coef,rhs,wksp,toll)
      implicit double precision (a-h, o-z)
c
c ... elim2 removes rows of the matrix for which the ratio of the
c     sum of off-diagonal elements to the diagonal element is
c     small (less than tol) in absolute value.
c     this is to take care of matrices arising from finite
c     element discretizations of partial differential equations
c     with dirichlet boundary conditions implemented by penalty
c     methods.  any such rows and corresponding columns are then
c     eliminated (set to the identity after correcting the rhs).
c     symmetric diagonal format.
c
c ... parameter list --
c
c         n       dimension of matrix ( = nn)
c         ndim    row dimension of array coef in the
c                    calling program
c         maxnz   number of diagonals stored
c         jcoef   integer vector of diagonal numbers
c         coef    array of sparse matrix representation
c         rhs     right hand side of matrix problem
c         wksp    wksp array of length n
c         tol     tolerance factor  (= toll)
c
c ... specifications for arguments
c
      integer   jcoef(1)
      dimension coef(ndim,1), rhs(1), wksp(1)
c
      n = nn
      maxnz = maxnzz
      tol = toll
      if (n .le. 0  .or.  maxnz .lt. 2) return
c
c ... find maximum off-diagonal elements in absolute value. 
c
      do 10 i = 1,n 
 10   wksp(i)= 0.0d0
      do 25 j = 2,maxnz
         ind = jcoef(j)
         len = n - ind
         do 15 i = 1,len
 15      wksp(i) = wksp(i) + abs (coef(i,j))
         do 20 i = 1,len
 20      wksp(i+ind) = wksp(i+ind) + abs (coef(i,j))
 25   continue
      do 30 i = 1,n 
 30   wksp(i) = wksp(i) / abs(coef(i,1))
c
c ... eliminate desired rows and columns.
c
      do 50 i = 1,n 
         if (wksp(i) .gt. tol) go to 50 
         rhs(i) = rhs(i)/coef(i,1)
         coef(i,1) = 1.0d0
         do 40 j = 2,maxnz
            jcol = jcoef(j)
            iback = i - jcol
            iforw = i + jcol
            if (iforw .gt. n) go to 35
            if (wksp(iforw) .le. tol) go to 35
            rhs(iforw) = rhs(iforw) - coef(i,j)*rhs(i)
 35         if (iback .lt. 1) go to 40
            rhs(iback) = rhs(iback) - coef(iback,j)*rhs(i)
            coef(iback,j)= 0.0d0
 40      continue
         do 45 j = 2,maxnz
 45      coef(i,j)= 0.0d0
 50   continue
      return
      end 
      subroutine elim3 (nn,ndim,maxnzz,jcoef,coef,rhs,wksp,toll)
      implicit double precision (a-h, o-z)
c
c ... elim3 removes rows of the matrix for which the ratio of the
c     sum of off-diagonal elements to the diagonal element is
c     small (less than tol) in absolute value.
c     this is to take care of matrices arising from finite
c     element discretizations of partial differential equations
c     with dirichlet boundary conditions implemented by penalty
c     methods.  any such rows and corresponding columns are then
c     eliminated (set to the identity after correcting the rhs).
c     nonsymmetric diagonal format.
c
c ... parameter list --
c
c         n       dimension of matrix ( = nn)
c         ndim    row dimension of array coef in the
c                    calling program
c         maxnz   number of diagonals stored
c         jcoef   integer vector of diagonal numbers
c         coef    array of sparse matrix representation
c         rhs     right hand side of matrix problem
c         wksp    wksp array of length n
c         tol     tolerance factor  (= toll)
c
c ... specifications for arguments
c
      integer   jcoef(1)
      dimension coef(ndim,1), rhs(1), wksp(1)
c
      n = nn
      maxnz = maxnzz
      tol = toll
      if (n .le. 0  .or.  maxnz .lt. 2) return
c
c ... find maximum off-diagonal elements in absolute value. 
c
      do 10 i = 1,n 
 10   wksp(i)= 0.0d0
      do 20 j = 2,maxnz
         ind = jcoef(j)
         ist1 = max (1,1 - ind)
         ist2 = min (n,n - ind)
         do 15 i = ist1,ist2
 15      wksp(i) = wksp(i) + abs (coef(i,j))
 20   continue
      do 25 i = 1,n 
 25   wksp(i) = wksp(i) / abs(coef(i,1))
c
c ... eliminate desired rows and columns.
c
      do 35 i = 1,n 
         if (wksp(i) .gt. tol) go to 35 
         rhs(i) = rhs(i)/coef(i,1)
         coef(i,1) = 1.0d0
         do 30 j = 2,maxnz
 30      coef(i,j)= 0.0d0
 35   continue
      do 45 i = 1,n 
         if (wksp(i) .gt. tol) go to 45 
         do 40 j = 2,maxnz
            inew = i - jcoef(j)
            if (inew .lt. 1 .or. inew .gt. n) go to 40
            rhs(inew) = rhs(inew) - coef(inew,j)*rhs(i)
            coef(inew,j)= 0.0d0 
 40      continue
 45   continue
      return
      end 
      subroutine elim4 (mm,np,ia,ja,a,rhs,wksp,toll)
      implicit double precision (a-h, o-z)
c
c ... elim4 removes rows of the matrix for which the ratio of the
c     sum of off-diagonal elements to the diagonal element is
c     small (less than tol) in absolute value.
c     this is to take care of matrices arising from finite
c     element discretizations of partial differential equations
c     with dirichlet boundary conditions implemented by penalty
c     methods.  any such rows and corresponding columns are then
c     eliminated (set to the identity after correcting the rhs).
c     symmetric sparse format.
c
c ... parameter list --
c
c         m       number of partitions
c         np      pointer vector to partitions
c         ia      vector of i values
c         ja      vector of j values
c         a       vector of coefficients
c         rhs     right hand side of matrix problem
c         wksp    wksp vector of length n (2n if keygs = 1) 
c         tol     tolerance factor  (= toll)
c
c ... specifications for arguments
c
      integer ia(1), ja(1), np(2)
      dimension a(1), rhs(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      m = mm
      n = np(2) - 1 
      nz = np(m+1) - 1
      tol = toll
      np1 = n + 1
c
c ... find sum of absolute values of off-diagonal coefficients.
c
      do 10 i = 1,n 
 10   wksp(i)= 0.0d0
      if (keygs .eq. 1) go to 30
      do 25 k = 2,m 
         ist = np(k)
         ied = np(k+1) - 1
cdir$ ivdep
         do 15 i = ist,ied
 15      wksp(ia(i)) = wksp(ia(i)) + abs(a(i))
cdir$ ivdep
         do 20 i = ist,ied
 20      wksp(ja(i)) = wksp(ja(i)) + abs(a(i))
 25   continue
      go to 50
 30   do 45 k = 2,m 
         ist = np(k)
         ied = np(k+1) - 1
         len = ied - ist + 1
         call vgathr (len,wksp,ia(ist),wksp(n+1)) 
         do 35 i = ist,ied
 35      wksp(i-ist+1+n) = wksp(i-ist+1+n) + abs(a(i))
         call vscatr (len,wksp(n+1),ia(ist),wksp) 
         call vgathr (len,wksp,ja(ist),wksp(n+1)) 
         do 40 i = ist,ied
 40      wksp(i-ist+1+n) = wksp(i-ist+1+n) + abs(a(i))
         call vscatr (len,wksp(n+1),ja(ist),wksp) 
 45   continue
 50   do 55 i = 1,n 
 55   wksp(i) = wksp(i) / abs(a(i))
c
c ... eliminate desired rows and columns.
c
      do 70 l = 1,n 
         if (wksp(l) .gt. tol) go to 70 
         rhs(l) = rhs(l)/a(l) 
         a(l) = 1.0d0 
         do 60 k = np1,nz
            i = ia(k)
            j = ja(k)
            if (i .eq. l .and. wksp(j) .gt. tol)
     a              rhs(j) = rhs(j) - a(k)*rhs(i) 
            if (j .ne. l) go to 60
            rhs(i) = rhs(i) - a(k)*rhs(j)
            a(k) = 0.0d0
 60      continue
         do 65 k = np1,nz
            if (ia(k) .eq. l) a(k) = 0.0d0
 65      continue
 70   continue
      return
      end 
      subroutine elim5 (mm,np,ia,ja,a,rhs,wksp,toll)
      implicit double precision (a-h, o-z)
c
c ... elim5 removes rows of the matrix for which the ratio of the
c     sum of off-diagonal elements to the diagonal element is
c     small (less than tol) in absolute value.
c     this is to take care of matrices arising from finite
c     element discretizations of partial differential equations
c     with dirichlet boundary conditions implemented by penalty
c     methods.  any such rows and corresponding columns are then
c     eliminated (set to the identity after correcting the rhs).
c     nonsymmetric sparse format.
c
c ... parameter list --
c
c         m       number of partitions
c         np      pointer vector to partitions
c         ia      vector of i values
c         ja      vector of j values
c         a       vector of coefficients
c         rhs     right hand side of matrix problem
c         wksp    wksp vector of length n (2n if keygs = 1) 
c         tol     tolerance factor  (= toll)
c
c ... specifications for arguments
c
      integer ia(1), ja(1), np(2)
      dimension a(1), rhs(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom4 / srelpr, keyzer, keygs
c
c *** end   -- package common 
c
      m = mm
      n = np(2) - 1 
      nz = np(m+1) - 1
      tol = toll
c
c ... find sum of absolute values of off-diagonal coefficients.
c
      do 10 i = 1,n 
 10   wksp(i)= 0.0d0
      if (keygs .eq. 1) go to 25
      do 20 k = 2,m 
         ist = np(k)
         ied = np(k+1) - 1
cdir$ ivdep
         do 15 i = ist,ied
 15      wksp(ia(i)) = wksp(ia(i)) + abs(a(i))
 20   continue
      go to 40
 25   do 35 k = 2,m 
         ist = np(k)
         ied = np(k+1) - 1
         len = ied - ist + 1
         call vgathr (len,wksp,ia(ist),wksp(n+1)) 
         do 30 i = ist,ied
 30      wksp(i-ist+1+n) = wksp(i-ist+1+n) + abs(a(i))
         call vscatr (len,wksp(n+1),ia(ist),wksp) 
 35   continue
 40   do 45 i = 1,n 
 45   wksp(i) = wksp(i) / abs(a(i))
c
c ... eliminate desired rows and columns.
c
      do 50 i = 1,n 
         if (wksp(i) .gt. tol) go to 50 
         rhs(i) = rhs(i)/a(i) 
         a(i) = 1.0d0 
 50   continue
      np1 = n + 1
      do 55 k = np1,nz
         if (wksp(ia(k)) .le. tol) a(k) = 0.0d0
 55   continue
      do 60 k = np1,nz
         j = ja(k)
         if (wksp(j) .gt. tol) go to 60 
         i = ia(k)
         rhs(i) = rhs(i) - a(k)*rhs(j)
         a(k) = 0.0d0 
 60   continue
      return
      end 
      subroutine ershow (ierr,iname)
      implicit double precision (a-h, o-z)
c
c ... ershow prints an appropriate error message for the error
c     numbered ier. 
c
c ... parameters -- 
c
c        ier     error number (input)
c                 .gt. 0     for warning errors
c                 .lt. 0     for fatal errors
c        iname   routine name in which error occurred
c
c ... specifications for parameters
c
      character*10 iname
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
      character*80  fmess(20), wmess(6) 
      data fmess(1)  / 'nonpositive matrix size n' /
      data fmess(2)  / 'insufficient floating point workspace' /
      data fmess(3)  / 'insufficient integer workspace' /
      data fmess(4)  / 'nonpositive diagonal element' /
      data fmess(5)  / 'nonexistent diagonal element' /
      data fmess(6)  / 'a is not positive definite' /
      data fmess(7)  / 'q is not positive definite' /
      data fmess(8)  / 'unable to permute matrix as requested' /
      data fmess(9)  / 'mdim not large enough to expand matrix' /
      data fmess(10) / 'inadmissible parameter encountered' /
      data fmess(11) / 'incorrect storage mode for block method' /
      data fmess(12) / 'zero pivot encountered during factorization' /
      data fmess(13) / 'breakdown in direction vector calculation' /
      data fmess(14) / 'breakdown in attempt to perform rotation' /
      data fmess(15) / 'breakdown in iterate calculation' / 
      data fmess(16) / 'unimplemented combination of parameters' /
      data fmess(17) / 'error in computing preconditioning polynomial' /
      data fmess(18) / 'unable to perform eigenvalue estimation' /
      data fmess(19) / 'iterative method has gone to sleep' /
      data fmess(20) / 'unknown error' /
      data wmess(1)  / 'failure to converge in itmax iterations' /
      data wmess(2)  / 'zeta too small' /
      data wmess(3)  / 'no convergence in maxfn iterations in zbrent' /
      data wmess(4)  / 'f(a) and f(b) have the same sign in zbrent' / 
      data wmess(5)  / 'negative pivot encountered in factorization' /
      data wmess(6)  / 'unknown warning' /
c
      ier = ierr
      if (ier .eq. 0) return
      if (ier .lt. 0  .and.  level .lt. 0) return 
      if (ier .gt. 0  .and.  level .lt. 1) return 
      if (ier .lt. -19) ier = -20
      if (ier .gt.   5) ier =   6
      if (ier .lt. 0) write (nout,10)
 10   format (//1x,60('*') /
     a          1x,18('*'),' f a t a l    e r r o r ',18('*') /
     a          1x,60('*') /) 
      if (ier .gt. 0) write (nout,20)
 20   format (//1x,60('*') /
     a          1x,16('*'),' w a r n i n g    e r r o r ',16('*') /
     a          1x,60('*') /) 
      write (nout,23) iname
 23   format (1x,'routine ',a10)
      inum = iabs(ier)
      if (ier .gt. 0) go to 30
c
c ... print out fatal errors. 
c
      write (nout,25) fmess(inum)
 25   format (1x,a80)
      go to 999
c
c ... print out warning errors.
c
 30   write (nout,25) wmess(inum)
      if (inum .ne. 2) go to 999
      temp = 500.0d0*srelpr
      write (nout,35) zeta, srelpr, temp
 35   format (1x,'rparm(1) =',d10.3,' (zeta)'
     a     / 1x, 'a value this small may hinder convergence'
     a     / 1x, 'since machine precision srelpr = ',d10.3
     a     / 1x, 'zeta reset to ',d10.3)
c
c ... print ending line.
c
 999  write (nout,1000)
 1000 format (/1x,60('*')/)
      return
      end 
      subroutine filln (maxnz,jcoef)
      implicit double precision (a-h, o-z)
c
c ... filln determines the fill-in diagonals for nonsymmetric
c     diagonal storage.
c
c ... parameters -- 
c
c        maxnz   upon input, the number of diagonals
c                upon output, the number of diagonals with fill-in
c        jcoef   upon input, the diagonal numbers 
c                upon output, the diagonal numbers with fill-in
c
c ... specifications for parameters
c
      integer jcoef(2)
c
      maxn = maxnz
      do 20 j1 = 1,maxnz
         do 15 j2 = 1,maxnz
            jd = jcoef(j1) + jcoef(j2)
            if (jcoef(j1)*jcoef(j2) .ge. 0) go to 15
            do 10 j3 = 1,maxn 
               if (jcoef(j3) .eq. jd) go to 15
 10         continue
            maxn = maxn + 1
            jcoef(maxn) = jd
 15      continue
 20   continue
      maxnz = maxn
      return
      end 
      subroutine fills (maxt,jt)
      implicit double precision (a-h, o-z)
c
c ... fills determines the fill-in diagonals for symmetric
c     diagonal storage.
c
c ... parameters -- 
c
c        maxt    upon input, the number of diagonals in the 
c                 upper triangle
c                upon output, the number of diagonals in the
c                 upper triangle with fill-in
c        jt      upon input, the diagonal numbers in the upper
c                 triangle
c                upon output, the diagonal numbers in the upper
c                 triangle with fill-in 
c
c ... specifications for parameters
c
      integer jt(1) 
c
      maxn = maxt
      do 20 j1 = 1,maxt
         do 15 j2 = 1,maxt
            jd = jt(j1) - jt(j2)
            if (jd .le. 0) go to 15
            do 10 j3 = 1,maxn 
               if (jt(j3) .eq. jd) go to 15
 10         continue
            maxn = maxn + 1
            jt(maxn) = jd
 15      continue
 20   continue
      maxt = maxn
      return
      end 
      subroutine fillnp (ndim,nn,maxcc,jc,c,mwidth,ier)
      implicit double precision (a-h, o-z)
c
c ... fillnp determines the fill-in structure.
c     (purdue storage, nonsymmetric matrix)
c
c ... parameters -- 
c
c          ndim   row dimension of jc and c arrays
c          n      order of system (= nn)
c          maxc   upon input, maxc is the number of columns in
c                  the c array
c                 upon output, maxc is the number of columns in
c                  the c array with fill-in
c          jc     integer array of active size n by maxc giving the
c                  column numbers of the corresponding elements in c
c          c      array of active size n by maxc giving the 
c                  coefficients of the off-diagonal elements
c          mwidth maximum column width to be allowed for fill-in
c          ier    error code
c                  =    0  no errors detected
c                  =   -2  mwidth too small to accomodate fill-in
c
c ... specifications for parameters
c
      integer   jc(ndim,1)
      dimension c(ndim,1)
c
c
      n = nn
      maxc = maxcc
      maxu = maxc
c
      if (maxc .lt. 1) return 
      nm1 = n - 1
      do 45 k = 1,nm1
         kp1 = k + 1
         do 40 j1 = 1,maxc
         do 35 i = kp1,n
            if (jc(i,j1) .ne. k) go to 35
            do 30 j2 = 1,maxc 
               j = jc(k,j2)
               if (j .le. k .or. j .eq. i) go to 30
               do 10 j3 = 1,maxu
                  if (j .eq. iabs(jc(i,j3))) go to 30
 10            continue
               do 15 j3 = 1,maxu
                  if (jc(i,j3) .ne. i) go to 15
                  jc(i,j3) = -j
                  go to 30
 15            continue
               maxu = maxu + 1
               if (maxu .le. mwidth) go to 20
               ier = -2
               return
 20            do 25 ii = 1,n 
                  jc(ii,maxu) = ii
                  c(ii,maxu)= 0.0d0
 25            continue
               jc(i,maxu) = -j
 30         continue
 35      continue
 40      continue
 45   continue
c
c ... decode new elements of jt, jb.
c
      do 55 j = 1,maxu
         do 50 i = 1,n
 50      jc(i,j) = iabs(jc(i,j))
 55   continue
      maxcc = maxu
      return
      end 
      subroutine fillsp (ndim,nn,maxtt,jt,t,mwidth,ier)
      implicit double precision (a-h, o-z)
c
c ... fillsp determines the fill-in structure.
c     (purdue storage, symmetric matrix)
c
c ... parameters -- 
c
c          ndim   row dimension of t and jt arrays
c          n      order of system (= nn)
c          maxt   upon input, maxt is the number of columns in
c                  the t array
c                 upon output, maxt is the number of columns in
c                  the t array with fill-in
c          jt     integer array of active size n by maxt giving the
c                  column numbers of the corresponding elements in t
c          t      array of active size n by maxt giving the 
c                  coefficients of the upper triangle of the matrix
c          mwidth maximum column width of jt and t to be allowed
c          ier    error code
c                  =   0     no error detected
c                  =  -2     mwidth too small to store factor
c
c ... specifications for parameters
c
      dimension t(ndim,1)
      integer   jt(ndim,1)
c
c
      n = nn
      maxt = maxtt
      maxu = maxt
      ier = 0
c
      if (maxt .lt. 1) return 
      nm1 = n - 1
      do 40 k = 1,nm1
         do 35 j1 = 1,maxt
            jcol1 = jt(k,j1)
            if (jcol1 .le. 0 .or. jcol1 .eq. k) go to 35
            do 30 j2 = 1,maxt 
               jcol2 = jt(k,j2)
               if (jcol2 .le. 0 .or. jcol2 .eq. k) go to 30 
               if (jcol2 .le. jcol1) go to 30
               do 10 j3 = 1,maxu
                  if (jcol2 .eq. iabs(jt(jcol1,j3))) go to 30
 10            continue
               do 15 j3 = 1,maxu
                  if (jt(jcol1,j3) .ne. jcol1) go to 15
                  jt(jcol1,j3) = -jcol2 
                  go to 30
 15            continue
               maxu = maxu + 1
               if (maxu .le. mwidth) go to 20
               ier = -2
               return
 20            do 25 i = 1,n
                  jt(i,maxu) = i
                  t(i,maxu) = 0.0d0
 25            continue
               jt(jcol1,maxu) = -jcol2
 30         continue
 35      continue
 40   continue
c
c ... decode new elements of jt.
c
      do 50 j = 1,maxu
         do 45 i = 1,n
 45      jt(i,j) = iabs(jt(i,j))
 50   continue
      maxtt = maxu
      return
      end 
      subroutine ibfcn1 (lddd,ldtt,n,jd,jt,d,t,ncol,nci,
     a                   iblock,lbhb,iunif,ipropa,ipt,
     a                   omega,wksp,ier)
      implicit double precision (a-h, o-z)
c
c ... ibfcn1 does an incomplete block factorization of the matrix
c     contained in d and t (version 1, unmodified).
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic (version 1) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer array of size ncolor by whatever 
c                   giving the diagonal block diagonal numbers for
c                   each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0 
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jd(ncol,1), jt(ncol,1), nci(1), lbhb(1),
     a          iblock(3,ncol,2)
      dimension d(lddd,1), t(ldtt,1), wksp(1)
      logical   unif, propa
c
      ldd = lddd
      ldt = ldtt
      ncolor = ncol 
      unif = iunif .eq. 1
      propa = ipropa .eq. 1
c
c ... define various constants.
c
      if (unif) go to 15
      klim = ncolor 
      go to 20
 15   kblsz = nci(1)
      na = kblsz
      nb = kblsz
      nc = kblsz
      ii = 1
      kk = 1
      jlim = lbhb(1)
      llim = jlim
      klim = n/kblsz
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      ma = ndt + ndb + 1
c
c ... start factorization.
c
 20   do 95 k = 1,klim
         if (unif) go to 25
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         ma = ndt + ndb + 1
         go to 30
 25      ist = (k - 1)*kblsz + 1
 30      call bdfac (ldd,na,na,ndt,ndb,d(ist,1),1)
         call mcopy (ldd,na,na,ma,d(ist,1),wksp)
         call bdinv (na,na,na,ndt,ndb,wksp,1)
         if (k .eq. klim .or. jlim .le. 2) go to 95
         do 90 i = k+1,klim
            if (unif) go to 35
            ii = i
            llim = lbhb(i)
 35         if (llim .le. 2) go to 90
            do 40 l = 3,llim
               jcol = i + iblock(1,ii,l)
               if (jcol .eq. k) go to 45
 40         continue
            go to 90
 45         mc = iblock(3,ii,l)
            if (unif) go to 50
            nc = ipt(i+1) - ipt(i)
            incc = ipt(k) - ipt(i)
            go to 55
 50         incc = (k - i)*kblsz
 55         istc = ist - incc 
            jstc = iblock(2,ii,l)
            do 85 j = 3,jlim
               jcol = k + iblock(1,kk,j)
               if (jcol .le. k) go to 85
               jdiff = jcol - i
               if (jdiff .ne. 0 .and. propa) go to 85
               do 60 m = 1,llim
                  if (iblock(1,ii,m) .eq. jdiff) go to 65
 60            continue
               go to 85
 65            mb = iblock(3,kk,j)
               istb = ist
               jstb = iblock(2,kk,j)
               if (unif) go to 70
               nb = ipt(jcol+1) - ipt(jcol)
               incb = ipt(jcol) - ipt(k)
               go to 75
 70            incb = (jcol - k)*kblsz
 75            incd = incc + incb
               istd = istc
               jstd = iblock(2,ii,m)
               md = iblock(3,ii,m)
               if (m .eq. 1) go to 80
               call t1prod (na,ldt,ldt,ldt,ncolor,na,nc,nb, 
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jt(ii,jstd),wksp,t(istb,jstb),
     a                      t(istc,jstc),t(istd,jstd))
               go to 85
 80            md = md + iblock(3,ii,2) 
               call t1prod (na,ldt,ldt,ldd,ncolor,na,nc,nb, 
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jd(ii,jstd),wksp,t(istb,jstb),
     a                      t(istc,jstc),d(istd,jstd))
 85         continue
 90      continue
 95   continue
      return
      end 
      subroutine ibfcn2 (lddd,ldtt,n,jd,jt,d,t,ncol,nci,
     a                   iblock,lbhb,iunif,ipropa,ipt,
     a                   omega,wksp,ier)
      implicit double precision (a-h, o-z)
c
c ... ibfcn2 does an incomplete block factorization of the matrix
c     contained in d and t (version 2, unmodified).
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic (version 2) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer array of size ncolor by whatever 
c                   giving the diagonal block diagonal numbers for
c                   each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0 
c
c ... specifications for parameters
c
      integer   ipt(1), jd(ncol,1), jt(ncol,1), nci(1), lbhb(1),
     a          iblock(3,ncol,2)
      dimension d(lddd,1), t(ldtt,1), wksp(1)
      logical   unif, propa
c
      ldd = lddd
      ldt = ldtt
      ncolor = ncol 
      unif = iunif .eq. 1
      propa = ipropa .eq. 1
c
c ... define various constants.
c
      if (unif) go to 15
      klim = ncolor 
      go to 20
 15   kblsz = nci(1)
      na = kblsz
      nb = kblsz
      nc = kblsz
      ii = 1
      kk = 1
      jlim = lbhb(1)
      llim = jlim
      klim = n/kblsz
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      ma = ndt + ndb + 1
c
c ... start factorization.
c
 20   do 95 k = 1,klim
         if (unif) go to 25
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         ma = ndt + ndb + 1
         go to 30
 25      ist = (k - 1)*kblsz + 1
 30      call bdfac (ldd,na,na,ndt,ndb,d(ist,1),1)
         call bdinv (ldd,na,na,ndt,ndb,d(ist,1),1)
         if (k .eq. klim .or. jlim .le. 2) go to 95
         do 90 i = k+1,klim
            if (unif) go to 35
            ii = i
            llim = lbhb(i)
 35         if (llim .le. 2) go to 90
            do 40 l = 3,llim
               jcol = i + iblock(1,ii,l)
               if (jcol .eq. k) go to 45
 40         continue
            go to 90
 45         mc = iblock(3,ii,l)
            if (unif) go to 50
            nc = ipt(i+1) - ipt(i)
            incc = ipt(k) - ipt(i)
            go to 55
 50         incc = (k - i)*kblsz
 55         istc = ist - incc 
            jstc = iblock(2,ii,l)
            do 85 j = 3,jlim
               jcol = k + iblock(1,kk,j)
               if (jcol .le. k) go to 85
               jdiff = jcol - i
               if (jdiff .ne. 0 .and. propa) go to 85
               do 60 m = 1,llim
                  if (iblock(1,ii,m) .eq. jdiff) go to 65
 60            continue
               go to 85
 65            mb = iblock(3,kk,j)
               istb = ist
               jstb = iblock(2,kk,j)
               if (unif) go to 70
               nb = ipt(jcol+1) - ipt(jcol)
               incb = ipt(jcol) - ipt(k)
               go to 75
 70            incb = (jcol - k)*kblsz
 75            incd = incc + incb
               istd = istc
               jstd = iblock(2,ii,m)
               md = iblock(3,ii,m)
               if (m .eq. 1) go to 80
               call t1prod (ldd,ldt,ldt,ldt,ncolor,na,nc,nb,
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jt(ii,jstd),d(ist,1),t(istb,jstb),
     a                      t(istc,jstc),t(istd,jstd))
               go to 85
 80            md = md + iblock(3,ii,2) 
               call t1prod (ldd,ldt,ldt,ldd,ncolor,na,nc,nb,
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jd(ii,jstd),d(ist,1),t(istb,jstb),
     a                      t(istc,jstc),d(istd,jstd))
 85         continue
 90      continue
 95   continue
      return
      end 
      subroutine ibfcn3 (lddd,ldtt,n,jd,jt,d,t,ncol,nci,
     a                   iblock,lbhb,iunif,ipropa,ipt,omega,wksp,
     a                   ier) 
      implicit double precision (a-h, o-z)
c
c ... ibfcn3 does an incomplete block factorization of the matrix
c     contained in d and t (version 1, modified). 
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic (version 1) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer array of size ncolor by whatever 
c                   giving the diagonal block diagonal numbers for
c                   each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0 
c         omega    relaxation factor between 0 and 1.
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jd(ncol,1), jt(ncol,1), nci(1), lbhb(1),
     a          iblock(3,ncol,2)
      dimension d(lddd,1), t(ldtt,1), wksp(1)
      logical   unif, propa
c
      ldd = lddd
      ldt = ldtt
      ncolor = ncol 
      unif = iunif .eq. 1
      propa = ipropa .eq. 1
c
c ... define various constants.
c
      if (unif) go to 15
      klim = ncolor 
      go to 20
 15   kblsz = nci(1)
      na = kblsz
      nb = kblsz
      nc = kblsz
      ii = 1
      kk = 1
      jlim = lbhb(1)
      llim = jlim
      klim = n/kblsz
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      ma = ndt + ndb + 1
c
c ... start factorization.
c
 20   do 100 k = 1,klim
         if (unif) go to 25
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         ma = ndt + ndb + 1
         go to 30
 25      ist = (k - 1)*kblsz + 1
 30      call bdfac (ldd,na,na,ndt,ndb,d(ist,1),1)
         call mcopy (ldd,na,na,ma,d(ist,1),wksp)
         call bdinv (na,na,na,ndt,ndb,wksp,1)
         ip1 = na*ma + 1
         ip2 = ip1 + na - 1
         if (k .eq. klim .or. jlim .le. 2) go to 100
         do 95 i = k+1,klim
            if (unif) go to 35
            ii = i
            llim = lbhb(i)
 35         if (llim .le. 2) go to 95
            do 40 l = 3,llim
               jcol = i + iblock(1,ii,l)
               if (jcol .eq. k) go to 45
 40         continue
            go to 95
 45         mc = iblock(3,ii,l)
            if (unif) go to 50
            nc = ipt(i+1) - ipt(i)
            incc = ipt(k) - ipt(i)
            go to 55
 50         incc = (k - i)*kblsz
 55         istc = ist - incc 
            jstc = iblock(2,ii,l)
            do 90 j = 3,jlim
               jcol = k + iblock(1,kk,j)
               if (jcol .le. k) go to 90
               mb = iblock(3,kk,j)
               istb = ist
               jstb = iblock(2,kk,j)
               if (unif) go to 60
               nb = ipt(jcol+1) - ipt(jcol)
               incb = ipt(jcol) - ipt(k)
               go to 65
 60            incb = (jcol - k)*kblsz
 65            incd = incc + incb
               istd = istc
               jdiff = jcol - i
               if (jdiff .ne. 0 .and. propa) go to 85
               do 70 m = 1,llim
                  if (iblock(1,ii,m) .eq. jdiff) go to 75
 70            continue
               go to 85
 75            jstd = iblock(2,ii,m)
               md = iblock(3,ii,m)
               if (m .eq. 1) go to 80
               call t1prod (na,ldt,ldt,ldt,ncolor,na,nc,nb, 
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jt(ii,jstd),wksp,t(istb,jstb),
     a                      t(istc,jstc),t(istd,jstd))
               call tsumn
     a               (na,nc,nb,na,ldt,ldt,ncolor,ma,mb,mc,md,incb,
     a                incc,incd,jd(kk,1),jt(kk,jstb),jt(ii,jstc),
     a                jt(ii,jstd),wksp,t(istb,jstb),t(istc,jstc),
     a                d(istd,1),omega)
               go to 85
 80            md = md + iblock(3,ii,2) 
               call t1prod (na,ldt,ldt,ldd,ncolor,na,nc,nb, 
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jd(ii,jstd),wksp,t(istb,jstb),
     a                      t(istc,jstc),d(istd,jstd))
               call tsumn
     a               (na,nc,nb,na,ldt,ldt,ncolor,ma,mb,mc,md,incb,
     a                incc,incd,jd(kk,1),jt(kk,jstb),jt(ii,jstc),
     a                jd(ii,jstd),wksp,t(istb,jstb),t(istc,jstc),
     a                d(istd,1),omega)
 85            call rowsum (ldt,na,mb,t(istb,jstb),wksp(ip1),1)
               do 87 iii = ip1,ip2
 87            wksp(iii) = omega*wksp(iii)
               call bdsol (ldd,na,na,ndt,ndb,d(ist,1),wksp(ip1),
     a                     wksp(ip1),1) 
               call vsubd (ldt,ncolor,nc,na,mc,t(istc,jstc),
     a                      jt(ii,jstc),d(istd,1),wksp(ip1),incc)
 90         continue
 95      continue
 100  continue
      return
      end 
      subroutine ibfcn4 (lddd,ldtt,n,jd,jt,d,t,ncol,nci,
     a                   iblock,lbhb,iunif,ipropa,ipt,omega,wksp,
     a                   ier) 
      implicit double precision (a-h, o-z)
c
c ... ibfcn4 does an incomplete block factorization of the matrix
c     contained in d and t (version 2, modified). 
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic (version 2) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer array of size ncolor by whatever 
c                   giving the diagonal block diagonal numbers for
c                   each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         jt       integer array of size ncolor by whatever 
c                   giving the off-diagonal block diagonal numbers
c                   for each distinct block size.  jd is 1 by whatever
c                   if iunif = 1.
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         ncolor   number of distinct block sizes 
c                   ncolor = 1 if iunif = 1.
c         nci      integer vector of length ncolor, giving the number 
c                   of nodes for each distinct block size.
c                   if iunif = 1, nci(1) is the constant block size.
c         iblock   integer array of size 3 by ncolor by max(lbhb(i))
c                   giving block constants
c         lbhb     integer vector of size ncolor giving the number
c                   of diagonal blocks for each distinct block size.
c                   if iunif = 1, lbhb is of length 1.
c         iunif    uniform block size switch
c                   = 0   diagonal blocks are not of uniform size
c                   = 1   diagonal blocks are of uniform size
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         ipt      integer pointer vector of length ncolor+1 if
c                   iunif = 0 
c         omega    relaxation factor between 0 and 1.
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   ipt(1), jd(ncol,1), jt(ncol,1), nci(1), lbhb(1),
     a          iblock(3,ncol,2)
      dimension d(lddd,2), t(ldtt,1), wksp(1)
      logical   unif, propa
c
      ldd = lddd
      ldt = ldtt
      ncolor = ncol 
      unif = iunif .eq. 1
      propa = ipropa .eq. 1
c
c ... define various constants.
c
      ip1 = n + 1
      if (unif) go to 15
      klim = ncolor 
      do 13 k = 1,ncolor
         ist = ipt(k) + 1
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         ma = ndt + ndb + 1
         call rowsum (ldd,na,ma,d(ist,1),wksp(ist),1)
 13   continue
      go to 20
 15   kblsz = nci(1)
      na = kblsz
      nb = kblsz
      nc = kblsz
      ii = 1
      kk = 1
      jlim = lbhb(1)
      llim = jlim
      klim = n/kblsz
      ndt = iblock(3,1,1) - 1 
      ndb = iblock(3,1,2)
      ma = ndt + ndb + 1
      call rowsum (ldd,n,ma,d,wksp,1)
c
c ... start factorization.
c
 20   do 100 k = 1,klim
         if (unif) go to 25
         kk = k
         ist = ipt(k) + 1
         jlim = lbhb(k)
         na = nci(k)
         ndt = iblock(3,k,1) - 1
         ndb = iblock(3,k,2)
         ma = ndt + ndb + 1
         go to 30
 25      ist = (k - 1)*kblsz + 1
 30      isu = ist + na - 1
         call bdfac (ldd,na,na,ndt,ndb,d(ist,1),1)
         call bdinv (ldd,na,na,ndt,ndb,d(ist,1),1)
         call bmuln (ldd,na,ndt,ndb,d(ist,1),d(ist,2),d(ist,ndt+2),
     a               wksp(ist),wksp(ip1))
         do 31 iii = ist,isu
            if (wksp(iii) .ne. 0.0d0) go to 31
            ier = -12
            call ershow (ier,'ibfcn4')
            return
 31      continue
         do 33 iii = ist,isu
 33      d(iii,1) = d(iii,1) + omega*(1.0d0 - wksp(iii-ist+ip1))/
     a                              wksp(iii)
         ip2 = ip1 + na
         if (k .eq. klim .or. jlim .le. 2) go to 100
         do 95 i = k+1,klim
            if (unif) go to 35
            ii = i
            llim = lbhb(i)
 35         if (llim .le. 2) go to 95
            do 40 l = 3,llim
               jcol = i + iblock(1,ii,l)
               if (jcol .eq. k) go to 45
 40         continue
            go to 95
 45         mc = iblock(3,ii,l)
            if (unif) go to 50
            nc = ipt(i+1) - ipt(i)
            incc = ipt(k) - ipt(i)
            go to 55
 50         incc = (k - i)*kblsz
 55         istc = ist - incc 
            jstc = iblock(2,ii,l)
            do 90 j = 3,jlim
               jcol = k + iblock(1,kk,j)
               if (jcol .le. k) go to 90
               mb = iblock(3,kk,j)
               istb = ist
               jstb = iblock(2,kk,j)
               if (unif) go to 60
               nb = ipt(jcol+1) - ipt(jcol)
               incb = ipt(jcol) - ipt(k)
               go to 65
 60            incb = (jcol - k)*kblsz
 65            incd = incc + incb
               istd = istc
               jdiff = jcol - i
               if (jdiff .ne. 0 .and. propa) go to 85
               do 70 m = 1,llim
                  if (iblock(1,ii,m) .eq. jdiff) go to 75
 70            continue
               go to 85
 75            jstd = iblock(2,ii,m)
               md = iblock(3,ii,m)
               if (m .eq. 1) go to 80
               call t1prod (ldd,ldt,ldt,ldt,ncolor,na,nc,nb,
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jt(ii,jstd),d(ist,1),t(istb,jstb),
     a                      t(istc,jstc),t(istd,jstd))
               call tsumn
     a               (na,nc,nb,ldd,ldt,ldt,ncolor,ma,mb,mc,md,incb,
     a                incc,incd,jd(kk,1),jt(kk,jstb),jt(ii,jstc),
     a                jt(ii,jstd),d(ist,1),t(istb,jstb),t(istc,jstc), 
     a                wksp(istd),1.0d0)
               go to 85
 80            md = md + iblock(3,ii,2) 
               call t1prod (ldd,ldt,ldt,ldd,ncolor,na,nc,nb,
     a                      ma,mb,mc,md,incb,incc,incd,jd(kk,1),
     a                      jt(kk,jstb),jt(ii,jstc),
     a                      jd(ii,jstd),d(ist,1),t(istb,jstb),
     a                      t(istc,jstc),d(istd,jstd))
 85            call rowsum (ldt,na,mb,t(istb,jstb),wksp(ip1),1)
               call bmuln (ldd,na,ndt,ndb,d(ist,1),d(ist,2),
     a                     d(ist,ndt+2),wksp(ip1),wksp(ip2))
               call vsubd (ldt,ncolor,nc,na,mc,t(istc,jstc),
     a                      jt(ii,jstc),wksp(istd),wksp(ip2),incc)
 90         continue
 95      continue
 100  continue
      return
      end 
      subroutine ibfcs1 (lddd,ldtt,nn,jd,jt,d,t,kblszz,
     a                   iblock,lbhb,ipropa,omega,wksp,ier) 
      implicit double precision (a-h, o-z)
c
c ... ibfcs1 does an incomplete block factorization of the matrix
c     contained in d and t (version 1, unmodified).
c     symmetric diagonal data structure, natural ordering.
c     block ic (version 1) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer vector giving the diagonal numbers
c                   for the diagonal block
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         kblsz    block size 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         lbhb     number of blocks per block row 
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jd(1), jt(1), iblock(3,3)
      dimension d(lddd,1), t(ldtt,1), wksp(1)
      logical   propa
c
      n = nn
      ldd = lddd
      ldt = ldtt
      na = kblszz
      propa = ipropa .eq. 1
      klim = n/na
      ma = iblock(3,1)
      ndt = ma - 1
c
c ... block tridiagonal case. 
c
      if (lbhb .gt. 3) go to 25
      jblkb = iblock(1,3)
      mb = iblock(3,3)
      incb = jblkb*na
      do 20 k = 1,klim
         ist = (k - 1)*na + 1 
         istd = ist + incb
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         if (istd .gt. n) go to 20
         call mcopy (ldd,na,na,ma,d(ist,1),wksp)
         call bdinv (na,na,na,ndt,0,wksp,0)
         call t2prod (na,na,ldt,ldt,ldd,ma,mb,mb,ma,
     a                incb,incb,0,jd,jt,jt,jd,wksp,t(ist,1),t(ist,1), 
     a                d(istd,1))
 20   continue
      return
c
c ... general block structure.
c
 25   do 50 k = 1,klim
         ist = (k - 1)*na + 1 
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         if (k .eq. klim) go to 50
         call mcopy (ldd,na,na,ma,d(ist,1),wksp)
         call bdinv (na,na,na,ndt,0,wksp,0)
         jjlim = min (lbhb,klim-k+2)
         do 45 jjc = 3,jjlim
            jblkc = iblock(1,jjc)
            jstc = iblock(2,jjc)
            mc = iblock(3,jjc)
            incc = jblkc*na
            istd = ist + incc 
            if (istd .gt. n) go to 45
            do 40 jjb = 3,jjlim
               jblkb = iblock(1,jjb)
               jstb = iblock(2,jjb)
               mb = iblock(3,jjb)
               incb = jblkb*na
               jdiff = jblkb - jblkc
               if (jdiff .lt. 0) go to 40
               if (jdiff .ne. 0 .and. propa) go to 40
               do 30 jjd = 1,jjlim
                  if (jdiff .eq. iblock(1,jjd)) go to 35
 30            continue
               go to 40
 35            jblkd = iblock(1,jjd)
               jstd = iblock(2,jjd)
               md = iblock(3,jjd)
               incd = jblkd*na
               if (jjd .ne. 1) call t2prod
     a               (na,na,ldt,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jt(jstd),wksp,t(ist,jstb),t(ist,jstc),
     a                t(istd,jstd))
               if (jjd .eq. 1) call t2prod
     a               (na,na,ldt,ldt,ldd,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jd,wksp,t(ist,jstb),t(ist,jstc),
     a                d(istd,1))
 40         continue
 45      continue
 50   continue
      return
      end 
      subroutine ibfcs2 (lddd,ldtt,nn,jd,jt,d,t,kblszz,
     a                   iblock,lbhb,ipropa,omega,wksp,ier) 
      implicit double precision (a-h, o-z)
c
c ... ibfcs2 does an incomplete block factorization of the matrix
c     contained in d and t (version 2, unmodified).
c     symmetric diagonal data structure, natural ordering.
c     block ic (version 2) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer vector giving the diagonal numbers
c                   for the diagonal block
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         kblsz    block size 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         lbhb     number of blocks per block row 
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c
c ... specifications for parameters
c
      integer   jd(1), jt(1), iblock(3,3)
      dimension d(lddd,1), t(ldtt,1), wksp(1)
      logical   propa
c
      n = nn
      ldd = lddd
      ldt = ldtt
      na = kblszz
      propa = ipropa .eq. 1
      klim = n/na
      ma = iblock(3,1)
      ndt = ma - 1
c
c ... block tridiagonal case. 
c
      if (lbhb .gt. 3) go to 25
      jblkb = iblock(1,3)
      mb = iblock(3,3)
      incb = jblkb*na
      do 20 k = 1,klim
         ist = (k - 1)*na + 1 
         istd = ist + incb
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         call bdinv (ldd,na,na,ndt,0,d(ist,1),0)
         if (istd .gt. n) go to 20
         call t2prod (na,ldd,ldt,ldt,ldd,ma,mb,mb,ma,
     a                incb,incb,0,jd,jt,jt,jd,d(ist,1),t(ist,1),
     a                t(ist,1),d(istd,1))
 20   continue
      return
c
c ... general block structure.
c
 25   do 50 k = 1,klim
         ist = (k - 1)*na + 1 
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         call bdinv (ldd,na,na,ndt,0,d(ist,1),0)
         if (k .eq. klim) go to 50
         jjlim = min (lbhb,klim-k+2)
         do 45 jjc = 3,jjlim
            jblkc = iblock(1,jjc)
            jstc = iblock(2,jjc)
            mc = iblock(3,jjc)
            incc = jblkc*na
            istd = ist + incc 
            if (istd .gt. n) go to 45
            do 40 jjb = 3,jjlim
               jblkb = iblock(1,jjb)
               jstb = iblock(2,jjb)
               mb = iblock(3,jjb)
               incb = jblkb*na
               jdiff = jblkb - jblkc
               if (jdiff .lt. 0) go to 40
               if (jdiff .ne. 0 .and. propa) go to 40
               do 30 jjd = 1,jjlim
                  if (jdiff .eq. iblock(1,jjd)) go to 35
 30            continue
               go to 40
 35            jblkd = iblock(1,jjd)
               jstd = iblock(2,jjd)
               md = iblock(3,jjd)
               incd = jblkd*na
               if (jjd .ne. 1) call t2prod
     a               (na,ldd,ldt,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jt(jstd),d(ist,1),t(ist,jstb),t(ist,jstc),
     a                t(istd,jstd))
               if (jjd .eq. 1) call t2prod
     a               (na,ldd,ldt,ldt,ldd,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jd,d(ist,1),t(ist,jstb),t(ist,jstc),
     a                d(istd,1))
 40         continue
 45      continue
 50   continue
      return
      end 
      subroutine ibfcs3 (lddd,ldtt,nn,jd,jt,d,t,kblszz,
     a                   iblock,lbhb,ipropa,omegaa,wksp,ier)
      implicit double precision (a-h, o-z)
c
c ... ibfcs3 does an incomplete block factorization of the matrix
c     contained in d and t (version 1, modified). 
c     symmetric diagonal data structure, natural ordering.
c     block ic (version 1) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer vector giving the diagonal numbers
c                   for the diagonal block
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         kblsz    block size 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         lbhb     number of blocks per block row 
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         omega    relaxation factor between 0. and 1.
c                   = 0   no modification
c                   = 1   full modification
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jd(1), jt(1), iblock(3,3)
      dimension d(lddd,1), t(ldtt,1), wksp(1)
      logical   propa
c
      n = nn
      ldd = lddd
      ldt = ldtt
      na = kblszz
      omega = omegaa
      propa = ipropa .eq. 1
      klim = n/na
      ma = iblock(3,1)
      ndt = ma - 1
c
c ... block tridiagonal case. 
c
      if (lbhb .gt. 3) go to 25
      ip1 = na*ma + 1
      ip2 = ip1 + na - 1
      jblkb = iblock(1,3)
      mb = iblock(3,3)
      incb = jblkb*na
      do 20 k = 1,klim
         ist = (k - 1)*na + 1 
         istd = ist + incb
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         if (istd .gt. n) go to 20
         call mcopy (ldd,na,na,ma,d(ist,1),wksp)
         call bdinv (na,na,na,ndt,0,wksp,0)
         call t2prod (na,na,ldt,ldt,ldd,ma,mb,mb,ma,
     a                incb,incb,0,jd,jt,jt,jd,wksp,t(ist,1),
     a                t(ist,1),d(istd,1))
         call tsum (na,na,ldt,ldt,ma,mb,mb,ma,incb,incb,
     a              0,jd,jt,jt,jd,wksp,t(ist,1),t(ist,1),
     a              d(istd,1),d(istd,1),wksp(ip1),1,omega)
         call rowsum (ldt,na,mb,t(ist,1),wksp(ip1),1)
         do 15 iii = ip1,ip2
 15      wksp(iii) = omega*wksp(iii)
         call bdsol (ldd,na,na,ndt,0,d(ist,1),wksp(ip1),
     a               wksp(ip1),0)
         call vsubdt (ldt,1,na,na,mb,t(ist,1),jt,d(istd,1), 
     a                wksp(ip1),incb)
 20   continue
      return
c
c ... general block structure.
c
 25   ip1 = na*ma + 1
      ip2 = ip1 + na - 1
      do 60 k = 1,klim
         ist = (k - 1)*na + 1 
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         if (k .eq. klim) go to 60
         call mcopy (ldd,na,na,ma,d(ist,1),wksp)
         call bdinv (na,na,na,ndt,0,wksp,0)
         jjlim = min (lbhb,klim-k+2)
         do 55 jjc = 3,jjlim
            jblkc = iblock(1,jjc)
            jstc = iblock(2,jjc)
            mc = iblock(3,jjc)
            incc = jblkc*na
            istd = ist + incc 
            if (istd .gt. n) go to 55
            do 50 jjb = 3,jjlim
               jblkb = iblock(1,jjb)
               jstb = iblock(2,jjb)
               mb = iblock(3,jjb)
               incb = jblkb*na
               istdd = ist + incb
               if (istdd .gt. n) go to 50
               jdiff = jblkb - jblkc
               if (jdiff .lt. 0) go to 50
               if (jdiff .ne. 0 .and. propa) go to 40
               do 30 jjd = 1,jjlim
                  if (jdiff .eq. iblock(1,jjd)) go to 35
 30            continue
               go to 40
 35            jblkd = iblock(1,jjd)
               jstd = iblock(2,jjd)
               md = iblock(3,jjd)
               incd = jblkd*na
               if (jjd .ne. 1) call t2prod
     a               (na,na,ldt,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jt(jstd),wksp,t(ist,jstb),t(ist,jstc),
     a                t(istd,jstd))
               if (jjd .eq. 1) call t2prod
     a               (na,na,ldt,ldt,ldd,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jd,wksp,t(ist,jstb),t(ist,jstc),
     a                d(istd,1))
               if (jjd .ne. 1) call tsum
     a               (na,na,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jt(jstd),wksp,t(ist,jstb),t(ist,jstc),
     a                d(istd,1),d(istdd,1),wksp(ip1),0,omega)
               if (jjd .eq. 1) call tsum
     a               (na,na,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jd,wksp,t(ist,jstb),t(ist,jstc),
     a                d(istd,1),d(istdd,1),wksp(ip1),1,omega)
c
 40            call rowsum (ldt,na,mb,t(ist,jstb),wksp(ip1),1)
               do 42 iii = ip1,ip2
 42            wksp(iii) = omega*wksp(iii)
               call bdsol (ldd,na,na,ndt,0,d(ist,1),wksp(ip1),
     a                     wksp(ip1),0) 
               call vsubdt (ldt,1,na,na,mc,t(ist,jstc),jt(jstc),
     a                      d(istd,1),wksp(ip1),incc)
               if (jdiff .eq. 0) go to 50
               call rowsum (ldt,na,mc,t(ist,jstc),wksp(ip1),1)
               do 45 iii = ip1,ip2
 45            wksp(iii) = omega*wksp(iii)
               call bdsol (ldd,na,na,ndt,0,d(ist,1),wksp(ip1),
     a                     wksp(ip1),0) 
               call vsubdt (ldt,1,na,na,mb,t(ist,jstb),jt(jstb),
     a                      d(istdd,1),wksp(ip1),incb)
 50         continue
 55      continue
 60   continue
      return
      end 
      subroutine ibfcs4 (lddd,ldtt,nn,jd,jt,d,t,kblszz,
     a                   iblock,lbhb,ipropa,omegaa,wksp,ier)
      implicit double precision (a-h, o-z)
c
c ... ibfcs4 does an incomplete block factorization of the matrix
c     contained in d and t (version 2, modified). 
c     symmetric diagonal data structure, natural ordering.
c     block ic (version 2) preconditioning.
c
c ... parameters -- 
c
c         ldd      row dimension of d array
c         ldt      row dimension of t array
c         n        size of system
c         jd       integer vector giving the diagonal numbers
c                   for the diagonal block
c         jt       integer vector giving the diagonal numbers
c                   for the off-diagonal blocks
c         d        array for diagonal block
c         t        array for off-diagonal blocks
c         kblsz    block size 
c         iblock   integer array of size 3 by lbhb
c                   giving block constants
c         lbhb     number of blocks per block row 
c         ipropa   property a switch
c                   = 0   matrix does not have block property a
c                   = 1   matrix has block property a
c         omega    relaxation factor between 0. and 1.
c                   = 0   no modification
c                   = 1   full modification
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jd(1), jt(1), iblock(3,3)
      dimension d(lddd,2), t(ldtt,1), wksp(1)
      logical   propa
c
      n = nn
      ldd = lddd
      ldt = ldtt
      na = kblszz
      omega = omegaa
      propa = ipropa .eq. 1
      klim = n/na
      ma = iblock(3,1)
      ndt = ma - 1
c
c ... block tridiagonal case. 
c
      if (lbhb .gt. 3) go to 25
      ip1 = n + 1
      ip2 = ip1 + na
      jblkb = iblock(1,3)
      mb = iblock(3,3)
      incb = jblkb*na
      call rowsum (ldd,n,ma,d,wksp,0)
      do 20 k = 1,klim
         ist = (k - 1)*na + 1 
         isu = k*na 
         istd = ist + incb
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         call bdinv (ldd,na,na,ndt,0,d(ist,1),0)
         call bmul (ldd,na,ndt,d(ist,1),d(ist,2),wksp(ist),wksp(ip1)) 
         do 10 ii = ist,isu
            if (wksp(ii) .ne. 0.0d0) go to 10
            ier = -12
            call ershow (ier,'ibfcs4')
            return
 10      continue
         do 15 ii = ist,isu
 15      d(ii,1) = d(ii,1) + omega*(1.0d0 - wksp(ii-ist+ip1))/
     a                              wksp(ii)
         if (istd .gt. n) go to 20
         call t2prod (na,ldd,ldt,ldt,ldd,ma,mb,mb,ma,
     a                incb,incb,0,jd,jt,jt,jd,d(ist,1),t(ist,1),
     a                t(ist,1),d(istd,1))
         call rowsum (ldt,na,mb,t(ist,1),wksp(ip1),1)
         call bmul (ldd,na,ndt,d(ist,1),d(ist,2),wksp(ip1), 
     a              wksp(ip2))
         call vsubdt (ldt,1,na,na,mb,t(ist,1),jt,wksp(istd),
     a                wksp(ip2),incb)
 20   continue
      return
c
c ... general block structure.
c
 25   ip1 = n + 1
      ip2 = ip1 + na
      call rowsum (ldd,n,ma,d,wksp,0)
      do 60 k = 1,klim
         ist = (k - 1)*na + 1 
         isu = k*na 
         call bdfac (ldd,na,na,ndt,0,d(ist,1),0)
         call bdinv (ldd,na,na,ndt,0,d(ist,1),0)
         call bmul (ldd,na,ndt,d(ist,1),d(ist,2),wksp(ist),wksp(ip1)) 
         do 26 ii = ist,isu
            if (wksp(ii) .ne. 0.0d0) go to 26
            ier = -12
            call ershow (ier,'ibfcs4')
            return
 26      continue
         do 27 ii = ist,isu
 27      d(ii,1) = d(ii,1) + omega*(1.0d0 - wksp(ii-ist+ip1))/
     a                              wksp(ii)
         if (k .eq. klim) go to 60
         jjlim = min (lbhb,klim-k+2)
         do 55 jjc = 3,jjlim
            jblkc = iblock(1,jjc)
            jstc = iblock(2,jjc)
            mc = iblock(3,jjc)
            incc = jblkc*na
            istd = ist + incc 
            if (istd .gt. n) go to 55
            do 50 jjb = 3,jjlim
               jblkb = iblock(1,jjb)
               jstb = iblock(2,jjb)
               mb = iblock(3,jjb)
               incb = jblkb*na
               istdd = ist + incb
               if (istdd .gt. n) go to 50
               jdiff = jblkb - jblkc
               if (jdiff .lt. 0) go to 50
               if (jdiff .ne. 0 .and. propa) go to 40
               do 30 jjd = 1,jjlim
                  if (jdiff .eq. iblock(1,jjd)) go to 35
 30            continue
               go to 40
 35            jblkd = iblock(1,jjd)
               jstd = iblock(2,jjd)
               md = iblock(3,jjd)
               incd = jblkd*na
               if (jjd .ne. 1) call t2prod
     a               (na,ldd,ldt,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jt(jstd),d(ist,1),t(ist,jstb),t(ist,jstc),
     a                t(istd,jstd))
               if (jjd .eq. 1) call t2prod
     a               (na,ldd,ldt,ldt,ldd,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jd,d(ist,1),t(ist,jstb),t(ist,jstc),
     a                d(istd,1))
               if (jjd .ne. 1) call tsum
     a               (na,ldd,ldt,ldt,ma,mb,mc,md,incb,
     a                incc,incd,jd,jt(jstb),jt(jstc),
     a                jt(jstd),d(ist,1),t(ist,jstb),t(ist,jstc),
     a                wksp(istd),wksp(istdd),wksp(ip1),0,1.0d0)
c
 40            call rowsum (ldt,na,mb,t(ist,jstb),wksp(ip1),1)
               call bmul (ldd,na,ndt,d(ist,1),d(ist,2),wksp(ip1),
     a                    wksp(ip2))
               call vsubdt (ldt,1,na,na,mc,t(ist,jstc),jt(jstc),
     a                      wksp(istd),wksp(ip2),incc)
               if (jdiff .eq. 0) go to 50
               call rowsum (ldt,na,mc,t(ist,jstc),wksp(ip1),1)
               call bmul (ldd,na,ndt,d(ist,1),d(ist,2),wksp(ip1),
     a                    wksp(ip2))
               call vsubdt (ldt,1,na,na,mb,t(ist,jstb),jt(jstb),
     a                      wksp(istdd),wksp(ip2),incb)
 50         continue
 55      continue
 60   continue
      return
      end 
      subroutine ibbs (ldd,ldt,n,kblszz,nsize,lbhb,iblock,d,t,
     a                 jt,x,ivers,wksp) 
      implicit double precision (a-h, o-z)
c
c ... ibbs does an incomplete block backward pass.
c     symmetric diagonal data structure, natural ordering.
c     block ic preconditioning.
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
c         ivers    key for version of factorization
c                   = 1   version 1
c                   = 2   version 2
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jt(1), iblock(3,1)
      dimension d(ldd,2), t(ldt,1), wksp(1), x(1) 
      logical   vers2
c
      kblsz = kblszz
      l = n/kblsz
      nt = iblock(3,1) - 1
      vers2 = ivers .eq. 2
      do 40 k = l,1,-1
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
 20      x(i) = d(i,1)*x(i)
         go to 40
 25      if (vers2) go to 30
         call bdsol (ldd,kblsz,nsize,nt,0,d(ist,1),x(ist),x(ist),
     a               0)
         go to 40
 30      call bmul (ldd,kblsz,nt,d(ist,1),d(ist,2),x(ist),wksp)
         do 35 i = ist,ied
 35      x(i) = wksp(i-ist+1) 
 40   continue
      return
      end 
      subroutine ibbsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibbsn does an incomplete block backward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      logical   unif, vers2
c
      vers2 = ivers .eq. 2
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
         do 22 i = 1,na
 22      wksp(i) = 0.0d0
         do 25 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .le. k) go to 25
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            if (istb .gt. n) go to 25
            call vaddd (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb),
     a                  wksp,x(istb),inc)
 25      continue
         if (ndt + ndb .ge. 1) go to 35 
         do 30 i = ist,ied
 30      x(i) = x(i) - d(i,1)*wksp(i-ist+1)
         go to 50
 35      if (vers2) go to 40
         call bdsol (ldd,na,nsize,ndt,ndb,d(ist,1),wksp,wksp,1)
         do 37 i = ist,ied
 37      x(i) = x(i) - wksp(i-ist+1)
         go to 50
 40      nap1 = na + 1
         call bmuln (ldd,na,ndt,ndb,d(ist,1),d(ist,2),d(ist,ndt+2),
     a               wksp,wksp(nap1))
         do 45 i = ist,ied
 45      x(i) = x(i) - wksp(i-ist+nap1) 
 50   continue
      return
      end 
      subroutine ibbsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibbsnt does an incomplete block transpose backward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      logical   unif, vers1
c
      vers1 = ivers .eq. 1
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
 10   do 45 k = l,1,-1
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
 25      x(i) = d(i,1)*x(i)
         go to 35
 30      if (vers1) call bdsolt
     a              (ldd,na,nsize,ndt,ndb,d(ist,1),x(ist),x(ist))
         if (vers1) go to 35
         call bmulnt
     a              (ldd,na,ndt,ndb,d(ist,1),d(ist,2),d(ist,ndt+2),
     a               x(ist),wksp)
         do 32 i = ist,ied
 32      x(i) = wksp(i-ist+1) 
 35      do 40 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .ge. k) go to 40
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            if (istb .lt. 1) go to 40
            call vsubdt (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb), 
     a                  x(istb),x(ist),inc)
 40      continue
 45   continue
      return
      end 
      subroutine ibfs (ldd,ldt,n,kblszz,nsize,lbhb,iblock,d,t,
     a                 jt,x,ivers,wksp) 
      implicit double precision (a-h, o-z)
c
c ... ibfs does an incomplete block forward pass. 
c     symmetric diagonal data structure, natural ordering.
c     block ic preconditioning.
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
c         ivers    key for version of factorization
c                   = 1   version 1
c                   = 2   version 2
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jt(1), iblock(3,1)
      dimension d(ldd,2), t(ldt,1), wksp(1), x(1) 
      logical   vers1, vers2
c
      kblsz = kblszz
      l = n/kblsz
      lm1 = l - 1
      nt = iblock(3,1) - 1
      vers1 = ivers .eq. 1
      vers2 = ivers .eq. 2
      do 30 k = 1,lm1
         ist = (k - 1)*kblsz + 1
         ied = k*kblsz
         if (nt .ge. 1) go to 15
         do 10 i = ist,ied
 10      wksp(i-ist+1) = d(i,1)*x(i)
         go to 20
 15      if (vers1) call bdsol (ldd,kblsz,nsize,nt,0,d(ist,1),
     a                          x(ist),wksp,0)
         if (vers2) call bmul (ldd,kblsz,nt,d(ist,1),d(ist,2),
     a                         x(ist),wksp)
 20      jjlim = min (lbhb,l-k+2)
         do 25 jj = 3,jjlim
            jblk = iblock(1,jj)
            jst = iblock(2,jj)
            mjj = iblock(3,jj)
            inc = jblk*kblsz
            istf = ist + inc
            if (istf .gt. n) go to 25
            call vsubdt (ldt,1,kblsz,kblsz,mjj,t(ist,jst),jt(jst),
     a                   x(istf),wksp,inc)
 25      continue
 30   continue
      return
      end 
      subroutine ibfsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibfsn does an incomplete block forward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      logical   unif, vers2
c
      vers2 = ivers .eq. 2
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
 10   do 50 k = 1,l 
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
            if (istb .lt. 1) go to 25
            call vsubd (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb),
     a                  x(ist),x(istb),inc)
 25      continue
         if (ndt + ndb .ge. 1) go to 35 
         do 30 i = ist,ied
 30      x(i) = d(i,1)*x(i)
         go to 50
 35      if (vers2) go to 40
         call bdsol (ldd,na,nsize,ndt,ndb,d(ist,1),x(ist),x(ist),1)
         go to 50
 40      call bmuln (ldd,na,ndt,ndb,d(ist,1),d(ist,2),d(ist,ndt+2),
     a               x(ist),wksp)
         do 45 i = ist,ied
 45      x(i) = wksp(i-ist+1) 
 50   continue
      return
      end 
      subroutine ibfsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibfsnt does an incomplete block transpose forward solve.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      logical   unif, vers1, vers2
c
      vers1 = ivers .eq. 1
      vers2 = ivers .eq. 2
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
      do 45 k = 1,lm1
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
 25      wksp(i-ist+1) = d(i,1)*x(i)
         go to 35
 30      if (vers1) call bdsolt
     a              (ldd,na,nsize,ndt,ndb,d(ist,1),x(ist),wksp)
         if (vers2) call bmulnt
     a              (ldd,na,ndt,ndb,d(ist,1),d(ist,2),d(ist,ndt+2),
     a               x(ist),wksp)
 35      do 40 j = 3,jlim
            jcol = k + iblock(1,kk,j)
            if (jcol .le. k) go to 40
            jstb = iblock(2,kk,j)
            mb = iblock(3,kk,j)
            if (unif) inc = (jcol - k)*na
            if (.not. unif) inc = ipt(jcol) - ipt(k)
            if (.not. unif) nb = nci(jcol)
            istb = ist + inc
            if (istb .gt. n) go to 40
            call vsubdt (ldt,ncolor,na,nb,mb,t(ist,jstb),jt(kk,jstb), 
     a                  x(istb),wksp,inc)
 40      continue
 45   continue
      return
      end 
      subroutine ibsl (ldd,ldt,n,kblsz,nsize,lbhb,iblock,d,t,
     a                 jt,y,x,ivers,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibsl does an incomplete block solution.
c     symmetric diagonal data structure, natural ordering.
c     block ic preconditioning.
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
c         ivers    key for version of factorization
c                   = 1   version 1
c                   = 2   version 2
c         wksp     floating point workspace vector
c
c ... specifications for parameters
c
      integer   jt(1), iblock(3,1)
      dimension d(ldd,1), t(ldt,1), wksp(1), x(1), y(1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call ibfs (ldd,ldt,n,kblsz,nsize,lbhb,iblock,d,t,
     a           jt,x,ivers,wksp)
      call ibbs (ldd,ldt,n,kblsz,nsize,lbhb,iblock,d,t,
     a           jt,x,ivers,wksp)
      return
      end 
      subroutine ibsln (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,y,x,ivers,iunif,wksp) 
      implicit double precision (a-h, o-z)
c
c ... ibsln does an incomplete block solution.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      call ibfsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      call ibbsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      return
      end 
      subroutine ibslnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,y,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibslnt does an incomplete block transpose solution.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      call ibfsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      call ibbsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      return
      end 
      subroutine ibsln1 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,y,x,ivers,iunif,wksp) 
      implicit double precision (a-h, o-z)
c
c ... ibsln1 does an incomplete block forward pass.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      call ibfsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      return
      end 
      subroutine ibsln2 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,y,x,ivers,iunif,wksp) 
      implicit double precision (a-h, o-z)
c
c ... ibsln2 does an incomplete block backward pass.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      call ibbsn (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      return
      end 
      subroutine ibsln3 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,y,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibsln3 does an incomplete block transpose back solve. 
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      call ibbsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      return
      end 
      subroutine ibsln4 (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                   iblock,d,t,jt,y,x,ivers,iunif,wksp)
      implicit double precision (a-h, o-z)
c
c ... ibsln4 does an incomplete block transpose forward pass.
c     nonsymmetric diagonal data structure, natural or multi-color
c     orderings, block ic preconditioning.
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
c         ivers    key for version number
c                   = 1  version 1
c                   = 2  version 2
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
      call ibfsnt (ldd,ldt,n,nsize,ncolor,nci,ipt,lbhb,
     a                  iblock,d,t,jt,x,ivers,iunif,wksp)
      return
      end 
      subroutine icf (ndim,nn,maxtt,jt,d,t,meth,
     a                 ipropa,omega,wksp,iwksp,iflag)
      implicit double precision (a-h, o-z)
c
c ... icf computes an incomplete factorization of the matrix
c     stored in d and t and replaces it.
c     (symmetric diagonal storage)
c
c ... parameters -- 
c
c          ndim   row dimension of t array
c          n      order of system (= nn)
c          maxt   number of columns in t array
c          jt     integer vector giving the diagonal indices of
c                  the corresponding columns in t 
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          t      array of active size n by maxt giving the 
c                  super-diagonals of the matrix
c          meth   point factorization wanted
c                  = 1   ic
c                  = 2   mic
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          omega  modification factor between 0.0 and 1.0
c                  = 0     no modification
c                  = 1     full modification
c          wksp   workspace vector of length n
c          iwksp  integer workspace of length maxt**2
c          iflag  indicator of factorization stability
c                    iflag = 0    no errors detected
c                          = 1    zero pivot encountered
c                                 (unsuccessful factorization)
c                          = 2    negative pivot encountered
c                                 (successful factorization)
c
c ... specifications for parameters
c
      integer   jt(1), iwksp(1)
      dimension d(1), t(ndim,1), wksp(1)
      logical   propa
c
c
      n = nn
      maxt = maxtt
      iflag = 0
      propa = ipropa .eq. 1
      if (maxt .lt. 1) go to 500
      nm1 = n - 1
      if (meth .ne. 1 .or. .not. propa) go to 20
c
c ... ic, propa = t.
c
      do 15 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 10 j = 1,maxt
            kf = k + jt(j)
            if (kf .le. n) d(kf) = d(kf) - t(k,j)**2/pivot
 10      continue
 15   continue
      if (d(n) .eq. 0.0d0) go to 995
      go to 500
 20   if (meth .ne. 2 .or. .not. propa) go to 50
c
c ... mic, propa = t.
c
      do 25 i = 1,n 
 25   wksp(i) = 0.0d0 
      do 35 j = 1,maxt
         do 30 i = 1,n
 30      wksp(i) = wksp(i) + t(i,j)
 35   continue
      do 45 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 40 i = 1,maxt
            kf = k + jt(i)
            if (kf .gt. n) go to 40
            term = t(k,i)/pivot
            d(kf) = d(kf) - term*(omega*wksp(k)-(omega-1.0d0)*t(k,i))
 40      continue
 45   continue
      if (d(n) .eq. 0.0d0) go to 995
      go to 500
c
c ... ic, mic for propa = f.
c
 50   nbig = maxt + 1
      do 70 i = 1,maxt
         do 65 j = i,maxt
            if (j .eq. i) go to 65
            iloc = (j - 1)*maxt + i
            id = iabs (jt(j) - jt(i))
            do 60 k = 1,maxt
               if (jt(k) .ne. id) go to 60
               iwksp(iloc) = k
               go to 65
 60         continue
            iwksp(iloc) = nbig
 65      continue
 70   continue
      do 100 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 95 i = 1,maxt
            kf = k + jt(i)
            if (kf .gt. n) go to 95
            do 75 j = i,maxt
 75         wksp(j) = t(k,i)*t(k,j)/pivot
            d(kf) = d(kf) - wksp(i)
            do 90 j = i,maxt
               if (j .eq. i) go to 90
               kg = k + jt(j) 
               if (kg .gt. n) go to 90
               iloc = (j-1)*maxt+i
               id = iwksp(iloc)
               if (id .eq. nbig) go to 85
               kff = min (kf,kg)
               t(kff,id) = t(kff,id) - wksp(j)
               go to 90
 85            if (meth .eq. 1) go to 90
               d(kf) = d(kf) - omega*wksp(j)
               d(kg) = d(kg) - omega*wksp(j)
 90         continue
 95      continue
 100  continue
      if (d(n) .eq. 0.0d0) go to 995
c
c ... store reciprocals of pivots.
c
 500  do 505 i = 1,n
 505  d(i) = 1.0d0/d(i)
      if (maxt .lt. 1 .or. propa) go to 990
      do 515 j = 1,maxt
         len = n - jt(j)
         do 510 i = 1,len
 510     t(i,j) = d(i)*t(i,j) 
 515  continue
c
c ... check for negative pivots.
c
 990  if (vmin(n,d) .lt. 0.0d0) iflag = 2 
      return
c
c ... error - matrix cannot be factored since a pivot is zero
c
 995  iflag = 1
      return
      end 
      subroutine icfn (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,meth,
     a                 ipropa,omega,wksp,iwksp,iflag)
      implicit double precision (a-h, o-z)
c
c ... icfn computes an incomplete factorization of the matrix
c     stored in d, t, and b and replaces it.
c     (nonsymmetric diagonal storage)
c
c ... parameters -- 
c
c          ndim   row dimension of t,b arrays
c          n      order of system (= nn)
c          maxt   number of columns in t array
c          maxb   number of columns in b array
c          jt     integer vector giving the diagonal indices of
c                  the corresponding columns in t 
c          jb     integer vector giving the diagonal indices of
c                  the corresponding columns in b 
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          t      array of active size n by maxt giving the 
c                  super-diagonals of the matrix
c          b      array of active size n by maxb giving the 
c                  sub-diagonals of the matrix
c          meth   point factorization wanted
c                  = 1   ic
c                  = 2   mic
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          omega  modification factor between 0.0 and 1.0
c                  = 0     no modification
c                  = 1     full modification
c          wksp   workspace vector of length n
c          iwksp  integer workspace of length maxb*maxt
c          iflag  indicator of factorization stability
c                    iflag = 0    no errors detected
c                          = 1    zero pivot encountered
c                                 (unsuccessful factorization)
c                          = 2    negative pivot encountered
c                                 (successful factorization)
c
c ... specifications for parameters
c
      integer   jt(1), jb(1), iwksp(1)
      dimension d(1), t(ndim,1), b(ndim,1), wksp(1)
      logical   propa
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      iflag = 0
      propa = ipropa .eq. 1
      if (maxt .lt. 1  .or.  maxb .lt. 1) go to 500
      nm1 = n - 1
      if (meth .ne. 1 .or. .not. propa) go to 30
c
c ... ic, propa = t.
c
      nval = 0
      do 15 j = 1,maxb
         i1 = -jb(j)
         do 10 i = 1,maxt
            i2 = jt(i)
            if (i1 .ne. i2) go to 10
            nval = nval + 1
            iwksp(3*nval-2) = j
            iwksp(3*nval-1) = i
            iwksp(3*nval) = i2
            go to 15
 10      continue
 15   continue
      if (nval .eq. 0) go to 500
      do 25 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 20 j = 1,nval
            kf = k + iwksp(3*j)
            if (kf .gt. n) go to 20
            i1 = iwksp(3*j-2) 
            i2 = iwksp(3*j-1) 
            d(kf) = d(kf) - b(kf,i1)*t(k,i2)/pivot
 20      continue
 25   continue
      if (d(n) .eq. 0.0d0) go to 995
      go to 500
 30   if (meth .ne. 2 .or. .not. propa) go to 70
c
c ... mic, propa = t.
c
      do 35 i = 1,n 
 35   wksp(i) = 0.0d0 
      do 45 j = 1,maxt
         do 40 i = 1,n
 40      wksp(i) = wksp(i) + t(i,j)
 45   continue
      do 55 i = 1,maxb
         i1 = -jb(i)
         do 50 j = 1,maxt
            i2 = jt(j)
            if (i1 .ne. i2) go to 50
            iwksp(i) = j
            go to 55
 50      continue
         iwksp(i) = 0
 55   continue
      do 65 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 60 i = 1,maxb
            kf = k - jb(i)
            if (kf .gt. n) go to 60
            term = b(kf,i)/pivot
            t1 = 0.0d0
            i1 = iwksp(i)
            if (i1 .ne. 0) t1 = t(k,i1) 
            d(kf) = d(kf) - term*(omega*wksp(k)-(omega-1.0d0)*t1)
 60      continue
 65   continue
      if (d(n) .eq. 0.0d0) go to 995
      go to 500
c
c ... ic, mic for propa = f.
c
 70   nbig = maxt + maxb
      do 105 i = 1,maxb
         do 100 j = 1,maxt
            iloc = (j - 1)*maxb + i
            id = jt(j) + jb(i)
            if (id) 75,85,90
 75         do 80 k = 1,maxb
               if (jb(k) .ne. id) go to 80
               iwksp(iloc) = -k
               go to 100
 80         continue
            iwksp(iloc) = nbig
            go to 100
 85         iwksp(iloc) = 0
            go to 100
 90         do 95 k = 1,maxt
               if (jt(k) .ne. id) go to 95
               iwksp(iloc) = k
               go to 100
 95         continue
            iwksp(iloc) = nbig
 100     continue
 105  continue
      do 140 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 135 i = 1,maxb
            kf = k - jb(i)
            if (kf .gt. n) go to 135
            do 110 j = 1,maxt 
 110        wksp(j) = b(kf,i)*t(k,j)/pivot
            do 130 j = 1,maxt 
               iloc = (j-1)*maxb+i
               id = iwksp(iloc)
               if (id) 115,120,125
 115           mid = -id
               b(kf,mid) = b(kf,mid) - wksp(j)
               go to 130
 120           d(kf) = d(kf) - wksp(j)
               go to 130
 125           if (id .ne. nbig) t(kf,id) = t(kf,id) - wksp(j)
               if (id .eq. nbig .and. meth .eq. 2)
     a                 d(kf) = d(kf) - omega*wksp(j)
 130        continue
 135     continue
 140  continue
      if (d(n) .eq. 0.0d0) go to 995
c
c ... store reciprocals of pivots.
c
 500  do 505 i = 1,n
 505  d(i) = 1.0d0/d(i)
      if (maxt .lt. 1 .or. propa) go to 520
      do 515 j = 1,maxt
         len = n - jt(j)
         do 510 i = 1,len
 510     t(i,j) = d(i)*t(i,j) 
 515  continue
 520  if (maxb .lt. 1 .or. propa) go to 990
      do 530 j = 1,maxb
         ind = jb(j)
         len = n + ind
         do 525 i = 1,len
 525     b(i-ind,j) = d(i)*b(i-ind,j)
 530  continue
c
c ... check for negative pivots.
c
 990  if (vmin(n,d) .lt. 0.0d0) iflag = 2 
      return
c
c ... error - matrix cannot be factored since a pivot is zero
c
 995  iflag = 1
      return
      end 
      subroutine icfv (ndim,nn,maxtt,jt,d,t,meth, 
     a                 ipropa,omega,wksp,iwksp,iflag)
      implicit double precision (a-h, o-z)
c
c ... icfv computes an incomplete factorization of the matrix
c     stored in d and t and replaces it.
c     (symmetric diagonal storage, vectorized version)
c
c ... parameters -- 
c
c          ndim   row dimension of t array
c          n      order of system (= nn)
c          maxt   number of columns in t array
c          jt     integer vector giving the diagonal indices of
c                  the corresponding columns in t 
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          t      array of active size n by maxt giving the 
c                  super-diagonals of the matrix
c          meth   point factorization wanted
c                  = 1   ic
c                  = 2   mic
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          omega  modification factor between 0.0 and 1.0
c                  = 0     no modification
c                  = 1     full modification
c          wksp   workspace vector of length n
c          iwksp  integer workspace of length maxt**2
c          iflag  indicator of factorization stability
c                    iflag = 0    no errors detected
c                          = 1    zero pivot encountered
c                                 (unsuccessful factorization)
c                          = 2    negative pivot encountered
c                                 (successful factorization)
c
c ... specifications for parameters
c
      integer   jt(1), iwksp(1)
      dimension d(1), t(ndim,1), wksp(1)
      logical propa 
c
c
      n = nn
      maxt = maxtt
      iflag = 0
      propa = ipropa .eq. 1
      if (maxt .lt. 1) go to 500
      if (meth .ne. 1 .or. .not. propa) go to 45
c
c ... ic, propa = t.
c
      do 10 i = 1,maxt
 10   iwksp(i) = jt(i) + 1
c
c ... determine nc, imin.
c
 15   nc = n
      do 20 i = 1,maxt
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 20
         nc = nterm 
         imin = i
 20   continue
      if (nc .ge. n) go to 500
      ndel = jt(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 35
c
c ... special case for first super-diagonal.
c
      nc1 = n
      do 25 i = 1,maxt
         if (i .eq. imin) go to 25
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 25   continue
      iwksp(imin) = nc1 + 1
      do 30 j = ibeg,nc1
 30   d(j) = d(j) - (t(j-1,imin)**2)/d(j-1)
      go to 15
c
c ... far diagonals.
c
 35   iwksp(imin) = iwksp(imin) + ndel
      ied = min (ibeg+ndel-1,n)
cdir$ ivdep
      do 40 i = ibeg,ied
 40   d(i) = d(i) - (t(i-ndel,imin)**2)/d(i-ndel) 
      go to 15
 45   if (meth .ne. 2 .or. .not. propa) go to 100 
c
c ... mic, propa = t.
c
      do 50 i = 1,n 
 50   wksp(i) = 0.0d0 
      do 60 j = 1,maxt
         do 55 i = 1,n
 55      wksp(i) = wksp(i) + t(i,j)
 60   continue
      do 65 i = 1,maxt
 65   iwksp(i) = jt(i) + 1
c
c ... determine nc, imin.
c
 70   nc = n
      do 75 i = 1,maxt
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 75
         nc = nterm 
         imin = i
 75   continue
      if (nc .ge. n) go to 500
      ndel = jt(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 90
c
c ... special case for first super-diagonal.
c
      nc1 = n
      do 80 i = 1,maxt
         if (i .eq. imin) go to 80
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 80   continue
      iwksp(imin) = nc1 + 1
      do 85 j = ibeg,nc1
 85   d(j) = d(j) - t(j-1,imin)*(omega*wksp(j-1)- 
     a             (omega-1.0d0)*t(j-1,imin))/d(j-1)
      go to 70
c
c ... far diagonals.
c
 90   iwksp(imin) = iwksp(imin) + ndel
      ied = min (ibeg+ndel-1,n)
cdir$ ivdep
      do 95 i = ibeg,ied
 95   d(i) = d(i) - t(i-ndel,imin)*(omega*wksp(i-ndel)-
     a             (omega-1.0d0)*t(i-ndel,imin))/d(i-ndel)
      go to 70
c
c ... set up pointers for propa = f case.
c
 100  nbig = maxt + 1
      do 115 i = 1,maxt
         do 110 j = 1,maxt
            iloc = j*maxt + i 
            id = iabs (jt(j) - jt(i))
            do 105 k = 1,maxt 
               if (jt(k) .ne. id) go to 105
               iwksp(iloc) = k
               go to 110
 105        continue
            iwksp(iloc) = nbig
 110     continue
 115  continue
c
c ... ic, mic for propa = f.
c
      do 120 i = 1,maxt
 120  iwksp(i) = jt(i) + 1
c
c ... determine nc, imin.
c
 125  nc = n
      do 130 i = 1,maxt
         nterm = iwksp(i) - 1 
         if (nterm .ge. nc) go to 130
         nc = nterm 
         imin = i
 130  continue
      if (nc .ge. n) go to 500
      ndel = jt(imin)
      iwksp(imin) = iwksp(imin) + ndel
      ibeg = nc + 1 
      ied = min (ibeg+ndel-1,n)
cdir$ ivdep
      do 135 i = ibeg,ied
 135  d(i) = d(i) - (t(i-ndel,imin)**2)/d(i-ndel) 
      do 160 j = 1,maxt
         jcol = jt(j)
         if (jcol .le. ndel) go to 160
         iloc = j*maxt + imin 
         id = iwksp(iloc)
         ied1 = min (ied,n-jcol+ndel)
         if (id .eq. nbig) go to 145
cdir$ ivdep
         do 140 i = ibeg,ied1 
 140     t(i,id) = t(i,id) - t(i-ndel,imin)*t(i-ndel,j)/d(i-ndel)
         go to 160
 145     if (meth .eq. 1) go to 160
         do 150 i = ibeg,ied1 
 150     wksp(i) = omega*t(i-ndel,imin)*t(i-ndel,j)/d(i-ndel)
         ish = jcol - ndel
         do 155 i = ibeg,ied1 
            d(i) = d(i) - wksp(i)
            d(i+ish) = d(i+ish) - wksp(i)
 155     continue
 160  continue
      go to 125
c
c ... store reciprocals of pivots.
c
 500  do 505 i = 1,n
         if (d(i) .eq. 0.0d0) go to 995
 505  continue
      do 510 i = 1,n
 510  d(i) = 1.0d0/d(i)
      if (maxt .lt. 1 .or. propa) go to 990
      do 520 j = 1,maxt
         len = n - jt(j)
         do 515 i = 1,len
 515     t(i,j) = d(i)*t(i,j) 
 520  continue
c
c ... check for negative pivots.
c
 990  if (vmin(n,d) .lt. 0.0d0) iflag = 2 
      return
c
c ... error - matrix cannot be factored since a pivot is zero
c
 995  iflag = 1
      return
      end 
      subroutine icfnp (ndimr,ndimi,nn,maxtt,maxbb,jt,jb,d,t,b,meth,
     a                  ipropa,omega,iflag)
      implicit double precision (a-h, o-z)
c
c ... icfnp computes an incomplete factorization of the matrix
c     stored in d, t, and b and replaces it.
c     (purdue storage, nonsymmetric matrix)
c
c ... parameters -- 
c
c          ndimr  row dimension of t and b arrays 
c          ndimi  row dimension of jt and jb arrays
c          n      order of system (= nn)
c          maxt   number of columns in t,jt arrays
c          maxb   number of columns in b,jb arrays
c          jt     integer array giving the column indices of the
c                  corresponding elements in t
c          jb     integer array giving the column indices of the
c                  corresponding elements in b
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          t      array of active size n by maxt giving the 
c                  upper triangle of the matrix
c          b      array of active size n by maxb giving the 
c                  lower triangle of the matrix
c          meth   point factorization wanted
c                  = 1   ic
c                  = 2   mic
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          omega  modification factor between 0.0 and 1.0
c                  = 0     no modification
c                  = 1     full modification
c          iflag  indicator of factorization stability
c                    iflag = 0    no errors detected
c                          = 1    zero pivot encountered
c                                 (unsuccessful factorization)
c                          = 2    negative pivot encountered
c                                 (successful factorization)
c
c ... specifications for parameters
c
      integer   jt(ndimi,1), jb(ndimi,1)
      dimension d(1), t(ndimr,1), b(ndimr,1)
      logical   propa
c
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      iflag = 0
      propa = ipropa .eq. 1
c
      if (maxt .lt. 1  .or.  maxb .lt. 1) go to 50
      nm1 = n - 1
      do 45 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         kp1 = k + 1
         do 40 j1 = 1,maxb
         do 35 i = kp1,n
            jcol1 = jb(i,j1)
            if (jcol1 .ne. k) go to 35
            term1 = b(i,j1)/pivot
            do 30 j2 = 1,maxt 
               j = jt(k,j2)
               if (j .le. k) go to 30
               term2 = term1*t(k,j2)
               jdiff = j - i
               if (jdiff .eq. 0) go to 27
               if (propa) go to 25
               if (jdiff .gt. 0) go to 15
               do 10 j3 = 1,maxb
                  if (jb(i,j3) .ne. j) go to 10
                  b(i,j3) = b(i,j3) - term2
                  go to 30
 10            continue
               go to 25
 15            do 20 j3 = 1,maxt
                  if (jt(i,j3) .ne. j) go to 20
                  t(i,j3) = t(i,j3) - term2
                  go to 30
 20            continue
 25            if (meth .eq. 1) go to 30
 27            d(i) = d(i) - omega*term2
 30         continue
 35      continue
 40      continue
 45   continue
      if (d(n) .eq. 0.0d0) go to 995
c
c ... store reciprocals of pivots.
c
 50   do 55 i = 1,n 
 55   d(i) = 1.0d0/d(i)
      if (maxt .lt. 1 .or. propa) go to 70
      do 65 j = 1,maxt
         do 60 i = 1,n
 60      t(i,j) = d(i)*t(i,j) 
 65   continue
 70   if (maxb .lt. 1 .or. propa) go to 990
      do 80 j = 1,maxb
         do 75 i = 1,n
 75      b(i,j) = b(i,j)*d(jb(i,j))
 80   continue
c
c ... check for negative pivots.
c
 990  if (vmin(n,d) .lt. 0.0d0) iflag = 2 
      return
c
c ... error - matrix cannot be factored since a pivot is zero
c
 995  iflag = 1
      return
      end 
      subroutine icfp (ndimr,ndimi,nn,maxtt,jt,d,t,meth,ipropa,omega, 
     a                 wksp,iflag)
      implicit double precision (a-h, o-z)
c
c ... icfp computes an incomplete factorization of the matrix
c     stored in d and t and replaces it.
c     (purdue storage, symmetric matrix)
c
c ... parameters -- 
c
c          ndimr  row dimension of t array
c          ndimi  row dimension of jt array
c          n      order of system (= nn)
c          maxt   number of columns in t array
c          jt     integer array of active size n by maxt giving the
c                  column numbers of the corresponding elements in t
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          t      array of active size n by maxt giving the 
c                  coefficients of the upper triangle of the matrix
c          meth   point factorization wanted
c                  = 1   ic
c                  = 2   mic
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          omega  modification factor between 0.0 and 1.0
c                  = 0     no modification
c                  = 1     full modification
c          wksp   workspace array of length n
c          iflag  indicator of factorization stability
c                    iflag = 0    no errors detected
c                          = 1    zero pivot encountered
c                                 (unsuccessful factorization)
c                          = 2    negative pivot encountered
c                                 (successful factorization)
c
c ... specifications for parameters
c
      dimension d(1), t(ndimr,1), wksp(1)
      integer   jt(ndimi,1)
      logical   propa
c
c
      n = nn
      maxt = maxtt
      iflag = 0
      propa = ipropa .eq. 1
      if (maxt .lt. 1) go to 500
      nm1 = n - 1
      if (meth .ne. 1 .or. .not. propa) go to 20
c
c ... ic, propa = t.
c
      do 15 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 10 j = 1,maxt
            jcol = jt(k,j)
            d(jcol) = d(jcol) - t(k,j)**2/pivot
 10      continue
 15   continue
      if (d(n) .eq. 0.0d0) go to 995
      go to 500
 20   if (meth .ne. 2 .or. .not. propa) go to 50
c
c ... mic, propa = t.
c
      do 25 i = 1,n 
 25   wksp(i) = 0.0d0 
      do 35 j = 1,maxt
         do 30 i = 1,n
 30      wksp(i) = wksp(i) + t(i,j)
 35   continue
      do 45 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 40 i = 1,maxt
            jcol = jt(k,i)
            if (jcol .eq. k) go to 40
            term = t(k,i)/pivot
            d(jcol) = d(jcol) - term*(omega*wksp(k)
     a            -(omega-1.0d0)*t(k,i))
 40      continue
 45   continue
      if (d(n) .eq. 0.0d0) go to 995
      go to 500
c
c ... ic, mic for propa = f.
c
 50   do 70 k = 1,nm1
         pivot = d(k)
         if (pivot .eq. 0.0d0) go to 995
         do 65 j1 = 1,maxt
            jcol1 = jt(k,j1)
            if (jcol1 .eq. k) go to 65
            d(jcol1) = d(jcol1) - (t(k,j1)**2)/pivot
            term1 = t(k,j1)/pivot
            do 60 j2 = 1,maxt 
               jcol2 = jt(k,j2)
               if (jcol2 .le. jcol1) go to 60
               if (jcol2 .eq. k) go to 60
               term2 = term1*t(k,j2)
               do 55 j3 = 1,maxt
                  if (jcol2 .ne. jt(jcol1,j3)) go to 55
                  t(jcol1,j3) = t(jcol1,j3) - term2
                  go to 60
 55            continue
               if (meth .eq. 1) go to 60
               d(jcol1) = d(jcol1) - omega*term2
               d(jcol2) = d(jcol2) - omega*term2
 60         continue
 65      continue
 70   continue
      if (d(n) .eq. 0.0d0) go to 995
c
c ... store reciprocals of pivots and scale t.
c
 500  do 510 i = 1,n
 510  d(i) = 1.0d0/d(i)
      if (maxt .lt. 1 .or. propa) go to 990
      do 520 j = 1,maxt
         do 515 i = 1,n
 515     t(i,j) = d(i)*t(i,j) 
 520   continue
c
c ... check for negative pivots.
c
 990  if (vmin(n,d) .lt. 0.0d0) iflag = 2 
      return
c
c ... error - matrix cannot be factored since a pivot is zero
c
 995  iflag = 1
      return
      end 
      subroutine icfcp (ndimr,ndimi,nn,maxcc,jc,d,c,ncolor,nt,nb,
     a                  meth,ipropa,ipt,omega,iflag)
      implicit double precision (a-h, o-z)
c
c ... icfcp computes an incomplete factorization of the matrix
c     stored in d and c and replaces it.
c     (purdue storage, multicolor)
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
c          n      order of system (= nn)
c          maxc   number of columns in c array
c          jc     integer array giving the column indices of the
c                  corresponding elements in c
c          d      vector of length n giving the diagonal elements
c                  of the matrix
c          c      array of active size n by maxc giving the 
c                  off diagonal elements of the matrix.
c                  thus, a = d + c
c          ncolor number of colors used 
c                  of nodes for each color
c          nt     integer vector of length ncolor giving the number
c                  of upper columns for each color
c          nb     integer vector of length ncolor giving the number
c                  of lower columns for each color
c          meth   point factorization wanted
c                  = 1   ic
c                  = 2   mic
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          ipt    integer pointer vector of length ncolor + 1
c          omega  modification factor between 0.0 and 1.0
c                  = 0     no modification
c                  = 1     full modification
c          iflag  indicator of factorization stability
c                    iflag = 0    no errors detected
c                          = 1    zero pivot encountered
c                                 (unsuccessful factorization)
c                          = 2    negative pivot encountered
c                                 (successful factorization)
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nt(1), nb(1), ipt(1) 
      dimension d(1), c(ndimr,1)
      logical   propa
c
c
      n = nn
      maxc = maxcc
      ncol = ncolor 
      iflag = 0
      propa = ipropa .eq. 1
      if (maxc .lt. 1) go to 75
c
c ... do factorization.
c
      do 65 icol = 1,ncol-1
         k1 = ipt(icol) + 1
         k2 = ipt(icol+1)
         j22 = nt(icol)
         if (j22 .le. 0) go to 65
         do 60 k = k1,k2
            pivot = d(k)
            if (pivot .eq. 0.0d0) go to 995
            do 55 l1 = icol+1,ncol
               i1 = ipt(l1) + 1
               i2 = ipt(l1+1) 
               j11 = nt(l1) + 1
               j12 = nt(l1) + nb(l1)
               j32 = nt(l1)
               if (j11 .gt. j12) go to 55
               do 50 j1 = j11,j12
               do 45 i = i1,i2
                  jcol1 = jc(i,j1)
                  if (jcol1 .ne. k) go to 45
                  term1 = c(i,j1)/pivot 
                  do 40 j2 = 1,j22
                     j = jc(k,j2)
                     if (j .le. k) go to 40
                     term2 = term1*c(k,j2)
                     if (j .eq. i) go to 35
                     if (propa) go to 30
                     if (j .gt. i) go to 20
                     do 15 j3 = j11,j12 
                        if (jc(i,j3) .ne. j) go to 15
                        c(i,j3) = c(i,j3) - term2 
                        go to 40
 15                  continue 
                     go to 30 
 20                  if (j32 .le. 0) go to 30
                     do 25 j3 = 1,j32
                        if (jc(i,j3) .ne. j) go to 25
                        c(i,j3) = c(i,j3) - term2 
                        go to 40
 25                  continue 
 30                  if (meth .eq. 1) go to 40
 35                  d(i) = d(i) - omega*term2
 40               continue
 45            continue
 50            continue
 55         continue
 60      continue
 65   continue
      k1 = ipt(ncol) + 1
      k2 = ipt(ncol+1)
      do 70 k = k1,k2
         if (d(k) .eq. 0.0d0) go to 995
 70   continue
c
c ... store reciprocals of pivots.
c
 75   do 80 i = 1,n 
 80   d(i) = 1.0d0/d(i)
      if (maxc .lt. 1 .or. propa) go to 990
      do 105 icol = 1,ncol
         nt2 = nt(icol)
         i1 = ipt(icol) + 1
         i2 = ipt(icol+1)
         do 100 j = 1,maxc
            if (j .gt. nt2) go to 90
            do 85 i = i1,i2
 85         c(i,j) = d(i)*c(i,j)
            go to 100
 90         do 95 i = i1,i2
 95         c(i,j) = c(i,j)*d(jc(i,j))
 100     continue
 105  continue
c
c ... check for negative pivots.
c
 990  if (vmin(n,d) .lt. 0.0d0) iflag = 2 
      return
c
c ... error - matrix cannot be factored since a pivot is zero
c
 995  iflag = 1
      return
      end 
      subroutine ics (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... ics does an ic solution (natural ordering,
c     symmetric diagonal storage).
c
c        (i + (t**t))*inv(d)*(i + t)*x = y            propa = .false. 
c        (i + (t**t)*d)*inv(d)*(i + d*t)*x = y        propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfst (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x) 
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call icbs (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x)
      return
      end 
      subroutine ics1 (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... ics1 does an ic forward solution (natural ordering,
c     symmetric diagonal storage).
c
c        (i + (t**t))*inv(d)*x = y            propa = .false.
c        (i + (t**t)*d)*inv(d)*x = y          propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfst (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x) 
      do 15 i = 1,n 
 15   x(i) = sqrt(abs(d(i)))*x(i)
      return
      end 
      subroutine ics2 (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... ics2 does an ic back solution (natural ordering,
c     symmetric diagonal storage).
c
c        (i + t)*x = y            propa = .false. 
c        (i + d*t)*x = y          propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      call icbs (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x)
      return
      end 
      subroutine ics3 (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... ics3 does an ic transpose backward solution (natural ordering,
c     symmetric diagonal storage).
c
c        inv(d)*(i + t)*x = y                 propa = .false.
c        inv(d)*(i + d*t)*x = y               propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      do 15 i = 1,n 
 15   x(i) = sqrt(abs(d(i)))*y(i)
      call icbs (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x)
      return
      end 
      subroutine ics4 (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... ics4 does an ic transpose forward solution (natural ordering,
c     symmetric diagonal storage).
c
c        (i + (t**t))*x = y            propa = .false.
c        (i + (t**t)*d)*x = y          propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfst (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x) 
      do 15 i = 1,n 
 15   x(i) = x(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      return
      end 
      subroutine icsn (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,ipropa,
     a                 irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsn does an ic solution (natural ordering, 
c     nonsymmetric diagonal storage).
c
c        (i + b)*inv(d)*(i + t)*x = y            propa = .false.
c        (i + b*d)*inv(d)*(i + d*t)*x = y        propa = .true.
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
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        b      array of active size n by maxb giving the sub-
c                diagonals of the factorization if not property a
c                or the sub-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      maxb = maxbb
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfs (ndim,n,maxb,jb,d,b,ipropa,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call icbs (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x)
      return
      end 
      subroutine icsnt (ndim,nn,maxtt,maxbb,jt,jb,d,t,b,ipropa,
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsnt does an transpose ic solution (natural ordering,
c     nonsymmetric diagonal storage).
c
c       (i + (t**t))*inv(d)*(i + (b**t))*x = y        propa = .false. 
c       (i + (t**t)*d)*inv(d)*(i + d*(b**t))*x = y    propa = .true.
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
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        b      array of active size n by maxb giving the sub-
c                diagonals of the factorization if not property a
c                or the sub-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      n = nn
      maxt = maxtt
      maxb = maxbb
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfst (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x) 
      do 15 i = 1,n 
 15   x(i) = d(i)*x(i)
      call icbst (ndim,n,maxb,jb,d,b,ipropa,irwise,iwksp,x) 
      return
      end 
      subroutine icsn1 (ndim,n,maxb,jb,d,b,ipropa,
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsn1 does an ic forward pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c        (i + b)*inv(d)*(i + t)*x = y            propa = .false.
c        (i + b*d)*inv(d)*(i + d*t)*x = y        propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the sub-
c                diagonals of the factorization if not property a
c                or the sub-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfs (ndim,n,maxb,jb,d,b,ipropa,irwise,iwksp,x)
      do 15 i = 1,n 
 15   x(i) = sqrt(abs(d(i)))*x(i)
      return
      end 
      subroutine icsn2 (ndim,n,maxt,jt,d,t,ipropa,
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsn2 does an ic back pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c        (i + b)*inv(d)*(i + t)*x = y            propa = .false.
c        (i + b*d)*inv(d)*(i + d*t)*x = y        propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
 10   x(i) = y(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      call icbs (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x)
      return
      end 
      subroutine icsn3 (ndim,n,maxb,jb,d,b,ipropa,
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsn3 does an ic transpose back pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c        (i + b)*inv(d)*(i + t)*x = y            propa = .false.
c        (i + b*d)*inv(d)*(i + d*t)*x = y        propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the sub-
c                diagonals of the factorization if not property a
c                or the sub-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      do 15 i = 1,n 
 15   x(i) = sqrt(abs(d(i)))*y(i)
      call icbst (ndim,n,maxb,jb,d,b,ipropa,irwise,iwksp,x) 
      return
      end 
      subroutine icsn4 (ndim,n,maxt,jt,d,t,ipropa,
     a                  irwise,iwksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsn4 does an ic transpose forward pass (natural ordering,
c     nonsymmetric diagonal storage).
c
c        (i + b)*inv(d)*(i + t)*x = y            propa = .false.
c        (i + b*d)*inv(d)*(i + d*t)*x = y        propa = .true.
c
c ... parameters -- 
c
c        ndim   row dimension of t and b arrays
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      call icfst (ndim,n,maxt,jt,d,t,ipropa,irwise,iwksp,x) 
      do 15 i = 1,n 
 15   x(i) = x(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      return
      end 
      subroutine icsnp (ndimr,ndimi,nn,maxtt,maxbb,jt,jb,d,t,b,
     a                  ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsnp does an ic solution (natural ordering,
c     purdue storage, nonsymmetric matrix).
c
c        (i + b)*d*(i + t)*x = y                  if ipropa = 0
c        (d + b)*inv(d)*(d + t)*x = y             if ipropa = 1
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        b      array of active size n by maxb giving the lower
c                triangle of the factorization if ipropa = 0
c                or the lower triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1), b(ndimr,1)
      integer   jt(ndimi,1), jb(ndimi,1)
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfsp (ndimr,ndimi,n,maxb,jb,d,b,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*d(i)
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = x(i)/d(i)
 30   continue
      call icbsp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      return
      end 
      subroutine icsntp (ndimr,ndimi,nn,maxtt,maxbb,jt,jb,d,t,b,
     a                   ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsntp does an transpose ic solution (natural ordering,
c     purdue storage, nonsymmetric matrix).
c
c        (i + (t**t))*d*(i + (b**t))*x = y        if ipropa = 0
c        (d + (t**t))*inv(d)*(d + (b**t))*x = y   if ipropa = 1
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        maxb   number of columns in b array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        b      array of active size n by maxb giving the lower
c                triangle of the factorization if ipropa = 0
c                or the lower triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1), b(ndimr,1)
      integer   jt(ndimi,1), jb(ndimi,1)
c
      n = nn
      maxt = maxtt
      maxb = maxbb
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfstp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*d(i)
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = x(i)/d(i)
 30   continue
      call icbstp (ndimr,ndimi,n,maxb,jb,d,b,ipropa,x)
      return
      end 
      subroutine icsnp1 (ndimr,ndimi,nn,maxb,jb,d,b,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsnp1 does an ic forward solution (natural ordering, 
c     purdue storage, nonsymmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the lower
c                triangle of the factorization if ipropa = 0
c                or the lower triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), b(ndimr,1)
      integer   jb(ndimi,1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfsp (ndimr,ndimi,n,maxb,jb,d,b,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*sqrt(abs(d(i)))
      return
 20   do 25 i = 1,n 
 25   x(i) = x(i)/sqrt(abs(d(i)))
      return
      end 
      subroutine icsnp2 (ndimr,ndimi,n,maxt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsnp2 does an ic back solution (natural ordering,
c     purdue storage, nonsymmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = y(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = y(i)/(sign(1.0d0,d(i))*sqrt(abs(d(i))))
 30   continue
      call icbsp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      return
      end 
      subroutine icsnp3 (ndimr,ndimi,n,maxb,jb,d,b,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsnp3 does an transpose ic forward solution (natural ordering, 
c     purdue storage, nonsymmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the lower
c                triangle of the factorization if ipropa = 0
c                or the lower triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), b(ndimr,1)
      integer   jb(ndimi,1)
c
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = y(i)*sqrt(abs(d(i)))
      go to 30
 20   do 25 i = 1,n 
 25   x(i)  = y(i)/sqrt(abs(d(i)))
 30   continue
      call icbstp (ndimr,ndimi,n,maxb,jb,d,b,ipropa,x)
      return
      end 
      subroutine icsnp4 (ndimr,ndimi,n,maxt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsnp4 does an transpose ic back solution (natural ordering,
c     purdue storage, nonsymmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfstp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      return
 20   do 25 i = 1,n 
 25   x(i) = x(i)/(sign(1.0d0,d(i))*sqrt(abs(d(i))))
      return
      end 
      subroutine icsp (ndimr,ndimi,nn,maxtt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsp does an ic solution (natural ordering, 
c     purdue storage, symmetric matrix).
c
c        (i + (t**t))*d*(i + t)*x = y             if ipropa = 0
c        (d + (t**t))*inv(d)*(d + t)*x = y        if ipropa = 1
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      n = nn
      maxt = maxtt
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfstp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*d(i)
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = x(i)/d(i)
 30   continue
      call icbsp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      return
      end 
      subroutine icsp1 (ndimr,ndimi,nn,maxt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsp1 does an ic forward solution (natural ordering,
c     purdue storage, symmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      n = nn
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfstp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*sqrt(abs(d(i)))
      return
 20   do 25 i = 1,n 
 25   x(i) = x(i)/sqrt(abs(d(i)))
      return
      end 
      subroutine icsp2 (ndimr,ndimi,n,maxt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsp2 does an ic back solution (natural ordering,
c     purdue storage, symmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = y(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = y(i)/(sign(1.0d0,d(i))*sqrt(abs(d(i))))
 30   continue
      call icbsp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      return
      end 
      subroutine icsp3 (ndimr,ndimi,n,maxt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsp3 does an ic transpose forward solution (natural ordering,
c     purdue storage, symmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = y(i)*sqrt(abs(d(i)))
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = y(i)/sqrt(abs(d(i)))
 30   continue
      call icbsp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      return
      end 
      subroutine icsp4 (ndimr,ndimi,n,maxt,jt,d,t,ipropa,y,x)
      implicit double precision (a-h, o-z)
c
c ... icsp4 does an ic transpose back solution (natural ordering,
c     purdue storage, symmetric matrix).
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        y      right-hand-side vector
c        x      on output, x is the solution
c
c ... specifications for parameters
c
      dimension y(1), x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfstp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      return
 20   do 25 i = 1,n 
 25   x(i) = x(i)/(sign(1.0d0,d(i))*sqrt(abs(d(i))))
      return
      end 
      subroutine icscp (ndimr,ndimi,nn,jc,d,c,ncolor,nc,nt,nb,ipropa, 
     a                  wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icscp does an ic solve. 
c     (purdue storage, multicolor)
c
c      (i + b)*d*(i + t)*x = y         if ipropa = 0
c      (d + b)*inv(d)*(d + t)*x = y    if ipropa = 1
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          y      on input, y is the right-hand-side vector 
c          x      on output, x is the solution to the forward solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimr,1), x(1), y(1), wksp(1)
c
      n = nn
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfscp (ndimr,ndimi,jc,d,c,ncolor,nc,nt,nb,ipropa,wksp,
     a             x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*d(i)
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = x(i)/d(i)
 30   continue
      call icbscp (ndimr,ndimi,n,jc,d,c,ncolor,nc,nt,ipropa,wksp,
     a             x)
      return
      end 
      subroutine icscpt (ndimr,ndimi,nn,jc,d,c,ncolor,nc,nt,nb,ipropa,
     a                   wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icscpt does an transpose ic solve.
c     (purdue storage, multicolor)
c
c     (i + (t**t))*d*(i + (b**t))*x = y       if ipropa = 0 
c     (d + (t**t))*inv(d)*(d + (b**t))*x = y  if ipropa = 1 
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length max(nc(i))
c          y      on input, y is the right-hand-side vector 
c          x      on output, x is the solution vector
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimr,1), x(1), y(1), wksp(1)
c
      n = nn
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfsct (ndimr,ndimi,jc,d,c,ncolor,nc,nt,ipropa,wksp,
     a             x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*d(i)
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = x(i)/d(i)
 30   continue
      call icbsct (ndimr,ndimi,n,jc,d,c,ncolor,nc,nt,nb,ipropa,wksp,
     a             x)
      return
      end 
      subroutine icscp1 (ndimr,ndimi,nn,jc,d,c,ncolor,nc,nt,nb,ipropa,
     a                  wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icscp1 does an ic forward solve.
c     (purdue storage, multicolor)
c
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          y      on input, y is the right-hand-side vector 
c          x      on output, x is the solution to the forward solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimr,1), x(1), y(1), wksp(1)
c
      n = nn
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfscp (ndimr,ndimi,jc,d,c,ncolor,nc,nt,nb,ipropa,wksp,
     a             x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*sqrt(abs(d(i)))
      return
 20   do 25 i = 1,n 
 25   x(i) = x(i)/sqrt(abs(d(i)))
      return
      end 
      subroutine icscp2 (ndimr,ndimi,nn,jc,d,c,ncolor,nc,nt,ipropa,
     a                  wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icscp2 does an ic back solve.
c     (purdue storage, multicolor)
c
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          y      on input, y is the right-hand-side vector 
c          x      on output, x is the solution to the forward solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1)
      dimension d(1), c(ndimr,1), x(1), y(1), wksp(1)
c
      n = nn
c
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = y(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = y(i)/(sign(1.0d0,d(i))*sqrt(abs(d(i))))
 30   continue
      call icbscp (ndimr,ndimi,n,jc,d,c,ncolor,nc,nt,ipropa,wksp,
     a             x)
      return
      end 
      subroutine icscp3 (ndimr,ndimi,nn,jc,d,c,ncolor,nc,nt,nb,ipropa,
     a                   wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icscp3 does an transpose ic forward solve.
c     (purdue storage, multicolor)
c
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length max(nc(i))
c          y      on input, y is the right-hand-side vector 
c          x      on output, x is the solution vector
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimr,1), x(1), y(1), wksp(1)
c
      n = nn
c
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = y(i)*sqrt(abs(d(i)))
      go to 30
 20   do 25 i = 1,n 
 25   x(i) = y(i)/sqrt(abs(d(i)))
 30   continue
      call icbsct (ndimr,ndimi,n,jc,d,c,ncolor,nc,nt,nb,ipropa,wksp,
     a             x)
      return
      end 
      subroutine icscp4 (ndimr,ndimi,nn,jc,d,c,ncolor,nc,nt,ipropa,
     a                   wksp,y,x)
      implicit double precision (a-h, o-z)
c
c ... icscp4 does an transpose ic back solve.
c     (purdue storage, multicolor)
c
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length max(nc(i))
c          y      on input, y is the right-hand-side vector 
c          x      on output, x is the solution vector
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1)
      dimension d(1), c(ndimr,1), x(1), y(1), wksp(1)
c
      n = nn
c
      do 10 i = 1,n 
 10   x(i) = y(i)
      call icfsct (ndimr,ndimi,jc,d,c,ncolor,nc,nt,ipropa,wksp,
     a             x)
      if (ipropa .eq. 1) go to 20
      do 15 i = 1,n 
 15   x(i) = x(i)*sign(1.0d0,d(i))*sqrt(abs(d(i)))
      return
 20   do 25 i = 1,n 
 25   x(i) = x(i)/(sign(1.0d0,d(i))*sqrt(abs(d(i))))
      return
      end 
      subroutine icbs (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... icbs does an ic back solve (natural ordering,
c     diagonal storage).
c        (i + t)*x = y    if not property a
c        (i + d*t)*x = y  if property a 
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      logical   propa
c
      n = nn
      maxt = maxtt
      nm1 = n - 1
      propa = ipropa .eq. 1
      if (maxt .lt. 1) return 
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 70
c
c ... diagonal-wise algorithm.
c
      do 15 i = 1,maxt
 15   iwksp(i) = n - jt(i)
c
c ... determine nc, imax.
c
 20   nc = 1
      do 25 i = 1,maxt
         nterm = iwksp(i) + 1 
         if (nterm .le. nc) go to 25
         nc = nterm 
         imax = i
 25   continue
      if (nc .le. 1) return
      ndel = jt(imax)
      iend = nc - 1 
      if (ndel .gt. 1) go to 50
c
c ... special case for first super diagonal.
c
      nc1 = 1
      do 30 i = 1,maxt
         if (i .eq. imax) go to 30
         if (iwksp(i) .gt. nc1) nc1 = iwksp(i)
 30   continue
      iwksp(imax) = nc1 - 1
      if (propa) go to 40
      do 35 k = iend,nc1,-1
 35   x(k) = x(k) - t(k,imax)*x(k+1)
      go to 20
 40   do 45 k = iend,nc1,-1
 45   x(k) = x(k) - d(k)*t(k,imax)*x(k+1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imax) = iwksp(imax) - ndel
      ibeg = max (iend - ndel,0) + 1
      if (propa) go to 60
cdir$ ivdep
      do 55 i = ibeg,iend
 55   x(i) = x(i) - t(i,imax)*x(i+ndel) 
      go to 20
cdir$ ivdep
 60   do 65 i = ibeg,iend
 65   x(i) = x(i) - d(i)*t(i,imax)*x(i+ndel)
      go to 20
c
c ... rowwise algorithm.
c
 70   do 85 i = nm1,1,-1
         do 75 j = 1,maxt
 75      iwksp(j) = min (n,i+jt(j))
         sum = 0.0d0
         do 80 j = 1,maxt
 80      sum = sum + t(i,j)*x(iwksp(j)) 
         if (propa) sum = d(i)*sum
         x(i) = x(i) - sum
 85   continue
      return
      end 
      subroutine icbst (ndim,nn,maxbb,jb,d,b,ipropa,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... icbst does an ic back solve (natural ordering,
c     diagonal storage).
c        (i + (b**t))*x = y    if not property a
c        (i + d*(b**t))*x = y  if property a
c
c ... parameters -- 
c
c        ndim   row dimension of b array
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the sub-
c                diagonals of the factorization if not property a
c                or the sub-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      logical   propa
c
      n = nn
      maxb = maxbb
      propa = ipropa .eq. 1
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
      if (propa) go to 40
      do 35 k = iend,nc1,-1
 35   x(k) = x(k) - b(k+1,imax)*x(k+1)
      go to 20
 40   do 45 k = iend,nc1,-1
 45   x(k) = x(k) - d(k)*b(k+1,imax)*x(k+1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imax) = iwksp(imax) - ndel
      ibeg = max (iend - ndel,0) + 1
      if (propa) go to 60
cdir$ ivdep
      do 55 i = ibeg,iend
 55   x(i) = x(i) - b(i+ndel,imax)*x(i+ndel)
      go to 20
cdir$ ivdep
 60   do 65 i = ibeg,iend
 65   x(i) = x(i) - d(i)*b(i+ndel,imax)*x(i+ndel) 
      go to 20
c
c ... rowwise algorithm.
c
 70   if (propa) go to 90
      do 85 i = n,2,-1
         do 75 j = 1,maxb
 75      iwksp(j) = max (1,i+jb(j))
         term = x(i)
         do 80 j = 1,maxb
 80      x(iwksp(j)) = x(iwksp(j)) - b(i,j)*term
 85   continue
      return
 90   do 105 i = n,2,-1
         do 95 j = 1,maxb
 95      iwksp(j) = max (1,i+jb(j))
         term = x(i)
         do 100 j = 1,maxb
 100     x(iwksp(j)) = x(iwksp(j)) - d(iwksp(j))*b(i,j)*term
 105  continue
      return
      end 
      subroutine icfs (ndim,nn,maxbb,jb,d,b,ipropa,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... icfs does an ic forward solve (natural ordering,
c     diagonal storage).
c        (i + b)*x = y    if not property a
c        (i + b*d)*x = y  if property a 
c
c ... parameters -- 
c
c        ndim   row dimension of b array
c        n      order of system (= nn)
c        maxb   number of columns in b array
c        jb     integer vector of length maxb giving the diagonal
c                indices of the corresponding columns in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      logical   propa
c
      n = nn
      maxb = maxbb
      propa = ipropa .eq. 1
      if (maxb .lt. 1) return 
c
c ... select rowwise or diagonal-wise algorithm.
c
      if (irwise .eq. 1) go to 70
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
      if (nc .ge. n) return
      ndel = -jb(imin)
      ibeg = nc + 1 
      if (ndel .gt. 1) go to 50
c
c ... special case for first minor subdiagonal.
c
      nc1 = n
      do 30 i = 1,maxb
         if (i .eq. imin) go to 30
         if (iwksp(i) .lt. nc1) nc1 = iwksp(i)
 30   continue
      iwksp(imin) = nc1 + 1
      if (propa) go to 40
      do 35 j = ibeg,nc1
 35   x(j) = x(j) - b(j,imin)*x(j-1)
      go to 20
 40   do 45 j = ibeg,nc1
 45   x(j) = x(j) - d(j-1)*b(j,imin)*x(j-1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imin) = iwksp(imin) + ndel
      iend = min (ibeg+ndel-1,n)
      if (propa) go to 60
cdir$ ivdep
      do 55 i = ibeg,iend
 55   x(i) = x(i) - b(i,imin)*x(i-ndel) 
      go to 20
cdir$ ivdep
 60   do 65 i = ibeg,iend
 65   x(i) = x(i) - d(i-ndel)*b(i,imin)*x(i-ndel) 
      go to 20
c
c ... rowwise algorithm.
c
 70   if (propa) go to 90
      do 85 i = 2,n 
         do 75 j = 1,maxb
 75      iwksp(j) = max (1,i+jb(j))
         sum = x(i) 
         do 80 j = 1,maxb
 80      sum = sum - b(i,j)*x(iwksp(j)) 
         x(i) = sum 
 85   continue
      return
 90   do 105 i = 2,n
         do 95 j = 1,maxb
 95      iwksp(j) = max (1,i+jb(j))
         sum = x(i) 
         do 100 j = 1,maxb
 100     sum = sum - d(iwksp(j))*b(i,j)*x(iwksp(j))
         x(i) = sum 
 105  continue
      return
      end 
      subroutine icfst (ndim,nn,maxtt,jt,d,t,ipropa,irwise,iwksp,x)
      implicit double precision (a-h, o-z)
c
c ... icfst does an ic forward solve (natural ordering,
c     diagonal storage).
c        (i + (t**t))*x = y    if not property a
c        (i + (t**t)*d)*x = y  if property a
c
c ... parameters -- 
c
c        ndim   row dimension of t array
c        n      order of system (= nn)
c        maxt   number of columns in t array
c        jt     integer vector of length maxt giving the diagonal
c                indices of the corresponding columns in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the super-
c                diagonals of the factorization if not property a
c                or the super-diagonals of the matrix if property a
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
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
      logical   propa
c
      n = nn
      maxt = maxtt
      nm1 = n - 1
      propa = ipropa .eq. 1
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
      if (propa) go to 40
      do 35 j = ibeg,nc1
 35   x(j) = x(j) - t(j-1,imin)*x(j-1)
      go to 20
 40   do 45 j = ibeg,nc1
 45   x(j) = x(j) - d(j-1)*t(j-1,imin)*x(j-1)
      go to 20
c
c ... far diagonals  (do vector computations).
c
 50   iwksp(imin) = iwksp(imin) + ndel
      iend = min (ibeg+ndel-1,n)
      if (propa) go to 60
cdir$ ivdep
      do 55 i = ibeg,iend
 55   x(i) = x(i) - t(i-ndel,imin)*x(i-ndel)
      go to 20
cdir$ ivdep
 60   do 65 i = ibeg,iend
 65   x(i) = x(i) - d(i-ndel)*t(i-ndel,imin)*x(i-ndel)
      go to 20
c
c ... rowwise algorithm.
c
 70   do 85 i = 1,nm1
         do 75 j = 1,maxt
 75      iwksp(j) = min (n,i+jt(j))
         term = x(i)
         if (propa) term = term*d(i)
         do 80 j = 1,maxt
 80      x(iwksp(j)) = x(iwksp(j)) - t(i,j)*term
 85   continue
      return
      end 
      subroutine icbsp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x) 
      implicit double precision (a-h, o-z)
c
c ... icbsp does an ic back solve (natural ordering,
c     purdue storage).
c
c        (i + t)*x = y    if ipropa = 0 
c        (d + t)*x = y    if ipropa = 1 
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        x      on input, x contains y
c               on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
      logical   propa
c
      propa = ipropa .eq. 1
      if (maxt .ge. 1) go to 15
      if (.not. propa) return 
      do 10 i = 1,n 
 10   x(i) = x(i)*d(i)
      return
 15   do 25 i = n,1,-1
         sum = x(i) 
         do 20 j = 1,maxt
            sum = sum - t(i,j)*x(jt(i,j))
 20      continue
         if (propa) sum = sum*d(i)
         x(i) = sum 
 25   continue
      return
      end 
      subroutine icbstp (ndimr,ndimi,n,maxb,jb,d,b,ipropa,x)
      implicit double precision (a-h, o-z)
c
c ... icbstp does an transpose ic back solve (natural ordering,
c     purdue storage).
c
c        (i + (b**t))*x = y    if ipropa = 0
c        (d + (b**t))*x = y    if ipropa = 1
c
c ... parameters -- 
c
c        n      order of system
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the lower
c                triangle of the factorization if ipropa = 0
c                or the lower triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        x      on input, x contains y
c               on output, x is the solution to back-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), b(ndimr,1)
      integer   jb(ndimi,1)
      logical   propa
c
      propa = ipropa .eq. 1
      if (maxb .ge. 1) go to 15
      if (.not. propa) return 
      do 10 i = 1,n 
 10   x(i) = x(i)*d(i)
      return
 15   do 25 i = n,1,-1
         if (propa) x(i) = x(i)*d(i)
         term = x(i)
         do 20 j = 1,maxb
            x(jb(i,j)) = x(jb(i,j)) - b(i,j)*term 
 20      continue
 25   continue
      return
      end 
      subroutine icfsp (ndimr,ndimi,n,maxb,jb,d,b,ipropa,x) 
      implicit double precision (a-h, o-z)
c
c ... icfsp does an ic forward solve (natural ordering,
c     purdue storage).
c
c        (i + b)*x = y    if ipropa = 0 
c        (d + b)*x = y    if ipropa = 1 
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxb   number of columns in b array
c        jb     integer array giving the column numbers of the
c                corresponding elements in b
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        b      array of active size n by maxb giving the lower
c                triangle of the factorization if ipropa = 0
c                or the lower triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        x      on input, x contains y
c               on output, x is the solution to forward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), b(ndimr,1)
      integer   jb(ndimi,1)
      logical   propa
c
      propa = ipropa .eq. 1
      if (maxb .ge. 1) go to 15
      if (.not. propa) return 
      do 10 i = 1,n 
 10   x(i) = x(i)*d(i)
      return
 15   do 25 i = 1,n 
         sum = x(i) 
         do 20 j = 1,maxb
            sum = sum - b(i,j)*x(jb(i,j))
 20      continue
         if (propa) sum = sum*d(i)
         x(i) = sum 
 25   continue
      return
      end 
      subroutine icfstp (ndimr,ndimi,n,maxt,jt,d,t,ipropa,x)
      implicit double precision (a-h, o-z)
c
c ... icfstp does an transpose ic forward solve (natural ordering,
c     purdue storage).
c
c        (i + (t**t))*x = y    if ipropa = 0
c        (d + (t**t))*x = y    if ipropa = 1
c
c ... parameters -- 
c
c        ndimr  row dimension of floating point arrays
c        ndimi  row dimension of integer arrays
c        n      order of system
c        maxt   number of columns in t array
c        jt     integer array giving the column numbers of the
c                corresponding elements in t
c        d      vector of length n giving the diagonal elements
c                of the factorization
c        t      array of active size n by maxt giving the upper
c                triangle of the factorization if ipropa = 0
c                or the upper triangle of the matrix if ipropa = 1
c        ipropa property a switch
c                = 0  matrix does not have property a
c                = 1  matrix does have property a 
c        x      on input, x contains y
c               on output, x is the solution to forward-solve
c
c ... specifications for parameters
c
      dimension x(1), d(1), t(ndimr,1)
      integer   jt(ndimi,1)
      logical   propa
c
      propa = ipropa .eq. 1
      if (maxt .ge. 1) go to 15
      if (.not. propa) return 
      do 10 i = 1,n 
 10   x(i) = x(i)*d(i)
      return
 15   do 25 i = 1,n 
         if (propa) x(i) = x(i)*d(i)
         term = x(i)
         do 20 j = 1,maxt
            x(jt(i,j)) = x(jt(i,j)) - t(i,j)*term 
 20      continue
 25   continue
      return
      end 
      subroutine icbscp (ndimr,ndimi,n,jc,d,c,ncolor,nc,nt,ipropa,
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... icbscp does a back ic solve.
c     (purdue storage, multicolor)
c
c       (i + t)*x = y    if ipropa = 0
c       (d + t)*x = y    if ipropa = 1
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          x      on input, x contains y
c                 on output, x is the solution to the back solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1)
      dimension d(1), c(ndimr,1), x(1), wksp(1)
      logical   propa
c
      propa = ipropa .eq. 1
c
      ied = n
      do 25 icol = ncolor,1,-1
         npt = nc(icol)
         ist = ied - npt + 1
         j2 = nt(icol)
         call vsubp (ndimr,ndimi,npt,j2,c(ist,1),jc(ist,1),x(ist),x,
     a               wksp)
         if (.not. propa) go to 20
         do 15 i = ist,ied
 15      x(i) = x(i)*d(i)
 20      ied = ied - npt
 25   continue
      return
      end 
      subroutine icbsct (ndimr,ndimi,n,jc,d,c,ncolor,nc,nt,nb,ipropa, 
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... icbsct does a transpose back ic solve.
c     (purdue storage, multicolor)
c
c     (i + (b**t))*x = y    if ipropa = 0
c     (d + (b**t))*x = y    if ipropa = 1
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length max(nc(i))
c          x      on input, x contains y
c                 on output, x is the solution to the back solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimr,1), x(1), wksp(1)
      logical   propa
c
      propa = ipropa .eq. 1
c
      ied = n
      do 25 icol = ncolor,1,-1
         npt = nc(icol)
         ist = ied - npt + 1
         if (.not. propa) go to 20
         do 15 i = ist,ied
 15      x(i) = x(i)*d(i)
 20      j1 = nt(icol) + 1
         mj = nb(icol)
         call vsubpt (ndimr,ndimi,npt,mj,c(ist,j1),jc(ist,j1),x,x(ist),
     a                wksp)
         ied = ied - npt
 25   continue
      return
      end 
      subroutine icfscp (ndimr,ndimi,jc,d,c,ncolor,nc,nt,nb,ipropa,
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... icfscp does a forward ic solve.
c     (purdue storage, multicolor)
c
c       (i + b)*x = y    if ipropa = 0
c       (d + b)*x = y    if ipropa = 1
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length
c                  max(nc(i))     if keygs = 1
c                  0              if keygs = 2
c          x      on input, x contains y
c                 on output, x is the solution to the back solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1), nb(1)
      dimension d(1), c(ndimr,1), x(1), wksp(1)
      logical   propa
c
      propa = ipropa .eq. 1
c
      ist = 1
      do 25 icol = 1,ncolor
         npt = nc(icol)
         ied = ist + npt - 1
         j1 = nt(icol) + 1
         mj = nb(icol)
         call vsubp (ndimr,ndimi,npt,mj,c(ist,j1),jc(ist,j1),x(ist),x,
     a               wksp)
         if (.not. propa) go to 20
         do 15 i = ist,ied
 15      x(i) = x(i)*d(i)
 20      ist = ist + npt
 25   continue
      return
      end 
      subroutine icfsct (ndimr,ndimi,jc,d,c,ncolor,nc,nt,ipropa,
     a                   wksp,x)
      implicit double precision (a-h, o-z)
c
c ... icfsct does a transpose forward ic solve.
c     (purdue storage, multicolor)
c
c     (i + (t**t))*x = y    if ipropa = 0
c     (d + (t**t))*x = y    if ipropa = 1
c
c ... parameters -- 
c
c          ndimr  row dimension of c array
c          ndimi  row dimension of jc array
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
c          ipropa property a flag
c                  = 0   matrix does not have property a
c                  = 1   matrix has property a
c          wksp   workspace vector of length max(nc(i))
c          x      on input, x contains y
c                 on output, x is the solution to the forward solve
c
c ... specifications for parameters
c
      integer   jc(ndimi,1), nc(1), nt(1)
      dimension d(1), c(ndimr,1), x(1), wksp(1)
      logical   propa
c
      propa = ipropa .eq. 1
c
      ist =  1
      do 25 icol = 1,ncolor
         npt = nc(icol)
         ied = ist + npt - 1
         if (.not. propa) go to 20
         do 15 i = ist,ied
 15      x(i) = x(i)*d(i)
 20      j2 = nt(icol)
         call vsubpt (ndimr,ndimi,npt,j2,c(ist,1),jc(ist,1),x,x(ist), 
     a                wksp)
         ist = ist + npt
 25   continue
      return
      end 
      integer function ipstr (omega)
      implicit double precision (a-h, o-z)
c
c     ipstr finds the smallest integer, ipstr, greater than 5 such
c     that    ipstr * (omega-1)**(ipstr-1) .le. 0.50. ipstr will be
c          set in loop.
c
c ... parameters -- 
c
c          omega  relaxation factor for sor method
c
c ... specifications for parameters
c
c
      wm1 = omega - 1.0d0
      factor = wm1**5
c
      do ip = 6,940
         if ( dble (ip)*factor .le. 0.5d0 ) go to 15
         factor = factor*wm1
      enddo
      ip = 940
   15 continue
      ipstr = ip
      return
      end 
      subroutine iptgen (ncolor,ipt,nc) 
      implicit double precision (a-h, o-z)
c
c ... iptgen generates ipt, the pointer vector to block rows,
c     for block structured matrices with nonconstant block size.
c
c ... parameters -- 
c
c     ncolor   the number of colors (block rows)
c     ipt      upon input, an integer vector of length ncolor+1
c              upon output, the pointer vector
c     nc       integer vector of length ncolor giving the
c               number of nodes for each color
c
c ... specifications for parameters
c
      integer ipt(1), nc(1)
c
      ipt(1) = 0
      do 10 k = 1,ncolor
         ipt(k+1) = ipt(k) + nc(k)
 10   continue
      return
      end 
      subroutine iterm (nn,u) 
      implicit double precision (a-h, o-z)
c
c     iterm produces the iteration summary line at the end
c     of each iteration. if level .ge. 4, the latest approximation
c     to the solution will be printed.
c
c ... parameters -- 
c
c          n      order of system  (= nn)
c          u      solution estimate
c
c ... specifications for parameters
c
      dimension u(1)
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
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- package common 
c
      n = nn
c
c ... print various parameters after each iteration
c
      if (in .gt. 0) go to 15 
c
c ... print header
c
      if (iacel .ne. 3) write (nout,10) 
 10   format (/5x,'intermediate output after each iteration'
     a       /' iteration',11x,'convergence ',
     b      5x,'emax',9x,'emin' /7x,'n',7x,'s',8x,'test' /) 
      if (iacel .eq. 3) write (nout,12) 
 12   format (////5x,'intermediate output after each iteration'
     a       //' number of',11x,'convergence',5x, 
     b       'emax',8x,'omega',7x,'spectral' /' iterations',
     c       13x,'test',34x,'radius' //)
c
c ... print summary line
c
 15   if (iacel .ne. 3) write (nout,20) in,is,stptst,emax,emin
 20   format (3x,i5,3x,i5,3x,3d13.5)
      if (iacel .eq. 3) write (nout,22) in,is,stptst,emax,omega,specr 
 22   format (3x,i5,3x,i5,3x,5d13.5)
      if (level .ge. 4) go to 25
      return
c
 25   write (nout,30) in
 30   format (/1x,2x,'estimate of solution at iteration ',i5)
      write (nout,35) (u(i),i=1,n)
 35   format (1x,5g16.7)
      write (nout,40)
 40   format (//)
c
      return
      end 
      subroutine mcopy (lda,ldb,n,m,a,b)
      implicit double precision (a-h, o-z)
c
c ... mcopy copies an array a into array b.
c
c ... parameters -- 
c
c        lda     leading dimension of array a
c        ldb     leading dimension of array b
c        n       number of rows in a to be copied 
c        m       number of columns in a to be copied
c        a,b     arrays
c
c ... specifications for parameters
c
      dimension a(lda,1), b(ldb,1)
c
      do 15 j = 1,m 
         do 10 i = 1,n
 10      b(i,j) = a(i,j)
 15   continue
      return
      end 
      subroutine move1 (ndim,mdim,nn,maxnzz,jcoef,coef,nt,nb,ier)
      implicit double precision (a-h, o-z)
c
c ... move1 moves the data structure to the form d/t/b, where
c     d is the main diagonal, the t columns contain only upper
c     triangular elements and the b columns contain only lower
c     triangular elements.  thus the upper and lower triangle
c     elements are segregated into separate columns of coef,
c     with the upper elements coming first.
c     (purdue data structure, natural ordering, with point
c     ic or point ssor preconditionings)
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         mdim     column dimension of coef array in defining routine 
c         n        order of system (= nn)
c         maxnz    number of columns in coef array (= maxnzz)
c         jcoef    integer matrix representation array
c         coef     matrix representation array
c         nt       number of columns needed to store t, the upper
c                   triangular part of coef
c         nb       number of columns needed to store b, the lower
c                   triangular part of coef
c         ier      error code 
c                  =  0  no errors detected
c                  = -9  mdim .lt. 1+nt+nb.  hence insufficient room
c                        to store adjusted matrix 
c
c ... specifications for parameters
c
      integer   jcoef(ndim,1) 
      dimension coef(ndim,1)
c
c *** begin -- package common 
c
c
c *** end   -- package common 
c
      n = nn
      maxnz = maxnzz
c
c ... determine maximum number of nonzeros per row in t and b.
c
      ntt = 0
      nbb = 0
      if (maxnz .le. 1) go to 999
      do 25 i = 1,n 
         ntrow = 0
         nbrow = 0
         do 20 j = 2,maxnz
            if (jcoef(i,j) - i) 10,20,15
 10         nbrow = nbrow + 1 
            go to 20
 15         ntrow = ntrow + 1 
 20      continue
         if (ntrow .gt. ntt) ntt = ntrow
         if (nbrow .gt. nbb) nbb = nbrow
 25   continue
c
c ... shuffle matrix so that t is first.
c
      ndtb = ntt + nbb + 1
      if (ndtb .le. mdim) go to 30
c
c ... error -- mdim is too small.
c
      ier = -9
      go to 999
c
c ... permute elements of each row.
c
 30   if (ntt*nbb .eq. 0) go to 999
      if (ndtb .le. maxnz) go to 40
      maxz = maxnz + 1
      do 35 j = maxz,ndtb
      do 35 i = 1,n 
         coef(i,j) = 0.0d0
         jcoef(i,j) = i
 35   continue
      maxnz = ndtb
 40   nt2 = ntt + 1 
      nb1 = nt2 + 1 
      do 65 i = 1,n 
         jbc = nt2
         do 50 jtc = 2,nt2
            if (jcoef(i,jtc) .ge. i) go to 50
 45         jbc = jbc + 1
            if (jcoef(i,jbc) .lt. i) go to 45
            jtemp = jcoef(i,jtc)
            jcoef(i,jtc) = jcoef(i,jbc) 
            jcoef(i,jbc) = jtemp
            temp = coef(i,jtc)
            coef(i,jtc) = coef(i,jbc)
            coef(i,jbc) = temp
 50      continue
         jtc = 1
         do 60 jbc = nb1,maxnz
            if (jcoef(i,jbc) .le. i) go to 60
 55         jtc = jtc + 1
            if (jcoef(i,jtc) .gt. i) go to 55
            jtemp = jcoef(i,jtc)
            jcoef(i,jtc) = jcoef(i,jbc) 
            jcoef(i,jbc) = jtemp
            temp = coef(i,jtc)
            coef(i,jtc) = coef(i,jbc)
            coef(i,jbc) = temp
 60      continue
 65   continue
c
c ... exit.
c
 999  nt = ntt
      nb = nbb
      maxnzz = maxnz
      return
      end 
      subroutine move2 (ndim,nn,maxnzz,jcoef,coef,work,iwork,
     a                  nt,nb)
      implicit double precision (a-h, o-z)
c
c ... move2 moves the data structure to the form d/t/b, where
c     d is the main diagonal, the t columns contain only upper
c     triangular elements and the b columns contain only lower
c     triangular elements.  thus the upper and lower triangle
c     elements are segregated into separate columns of coef,
c     with the upper elements coming first.
c     (diagonal data structure, natural ordering, with point
c     ic or point ssor preconditionings)
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         n        order of system (= nn)
c         maxnz    number of columns in coef array (= maxnzz)
c         jcoef    integer matrix representation array
c         coef     matrix representation array
c         work     floating point workspace array of length n
c         iwork    integer work array of length maxnz
c         nt       number of columns needed to store t, the upper
c                   triangular part of coef
c         nb       number of columns needed to store b, the lower
c                   triangular part of coef
c
c ... specifications for parameters
c
      integer   jcoef(2), iwork(1)
      dimension coef(ndim,1), work(1)
c
      n = nn
      maxnz = maxnzz
      ntt = 0
      nbb = 0
      if (maxnz .le. 1) go to 999
c
c ... compute nbb and ntt.
c
      do 10 j = 1,maxnz
         ndiag = jcoef(j)
         if (ndiag .gt. 0) ntt = ntt + 1
         if (ndiag .lt. 0) nbb = nbb + 1
 10   continue
c
c ... compute pointers into sorted jcoef.
c
c ... code jcoef.
c
      do 15 j = 1,maxnz
         if (jcoef(j) .lt. 0) jcoef(j) = n - jcoef(j)
 15   continue
      iwork(1) = 1
      do 30 j = 2,maxnz
         iaux = jcoef(j)
         do 20 k = 1,j-1
            i = j - k
            ktemp = iwork(i)
            if (iaux .gt. jcoef(ktemp)) go to 25
            iwork(i+1) = iwork(i)
 20      continue
         i = 0
 25      iwork(i+1) = j
 30   continue
c
c ... decode jcoef. 
c
      do 35 j = 1,maxnz
         if (jcoef(j) .gt. n) jcoef(j) = n - jcoef(j)
 35   continue
c
c ... sort coef and jcoef.
c
      do 40 i = 1,maxnz
         if (iwork(i) .eq. i) iwork(i) = 0
 40   continue
      do 65 ii = 1,maxnz
         k = iwork(ii)
         if (k .eq. 0) go to 65
         i = ii
 45      jtemp = jcoef(i)
         jcoef(i) = jcoef(k)
         jcoef(k) = jtemp
         do 50 l = 1,n
            work(l) = coef(l,i)
            coef(l,i) = coef(l,k)
            coef(l,k) = work(l)
 50      continue
         iwork(i) = 0
         do 55 j = ii,maxnz
            if (iwork(j) .eq. i) go to 60
 55      continue
         go to 65
 60      i = j
         if (i .ne. k) go to 45
         iwork(k) = 0
 65   continue
c
c ... exit.
c
 999  nt = ntt
      nb = nbb
      return
      end 
      subroutine move3 (ndim,mdim,nn,maxnzz,jcoef,coef,nt,nb,
     a                  ncolor,nc,ier)
      implicit double precision (a-h, o-z)
c
c ... move3 moves the data structure to the form d/t/b, where
c     d is the main diagonal, the t columns contain only upper
c     triangular elements and the b columns contain only lower
c     triangular elements.  thus the upper and lower triangle
c     elements are segregated into separate columns of coef,
c     with the upper elements coming first.
c     the above segregation is done for each color.
c     (purdue data structure, multi-color ordering, with point
c     ic or point ssor preconditionings)
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         mdim     column dimension of coef array in defining routine 
c         n        order of system (= nn)
c         maxnz    number of columns in coef array (= maxnzz)
c         jcoef    integer matrix representation array
c         coef     matrix representation array
c         nt       integer vector of length ncolor.  for each color,
c                   the number of columns needed to store t, the upper
c                   triangular part of the matrix for those rows.
c         nb       integer vector of length ncolor.  for each color,
c                   the number of columns needed to store b, the lower
c                   triangular part of the matrix for those rows.
c         ncolor   number of colors
c         nc       integer vector of length ncolor, giving the number 
c                   of nodes for each color.
c         ier      error code 
c                  =  0  no errors detected
c                  = -9  mdim .lt. 1+nt+nb.  hence insufficient room
c                        to store adjusted matrix 
c
c ... specifications for parameters
c
      integer   jcoef(ndim,1), nt(1), nb(1), nc(1)
      dimension coef(ndim,1)
c
c
      n = nn
      maxnz = maxnzz
c
      ist = 1
      do 85 icol = 1,ncolor
         ncol = nc(icol)
         ied = ist + ncol - 1 
c
c ... determine maximum number of nonzeros per row in t and b.
c
         ntt = 0
         nbb = 0
         if (maxnz .le. 1) go to 80
         do 25 i = ist,ied
            ntrow = 0
            nbrow = 0
            do 20 j = 2,maxnz 
               if (jcoef(i,j) - i) 10,20,15
 10            nbrow = nbrow + 1
               go to 20
 15            ntrow = ntrow + 1
 20         continue
            if (ntrow .gt. ntt) ntt = ntrow
            if (nbrow .gt. nbb) nbb = nbrow
 25      continue
c
c ... shuffle matrix so that t is first.
c
         ndtb = ntt + nbb + 1 
         if (ndtb .le. mdim) go to 30
c
c ... error -- mdim is too small.
c
         ier = -9
         go to 999
c
c ... permute elements of each row.
c
 30      if (ndtb .le. maxnz) go to 40
         maxz = maxnz + 1
         do 35 j = maxz,ndtb
         do 35 i = 1,n
            coef(i,j) = 0.0d0
            jcoef(i,j) = i
 35      continue
         maxnz = ndtb
 40      nt2 = ntt + 1
         nb1 = ntt + 2
         nz1 = 2 + ntt + nbb
         do 75 i = ist,ied
            jbc = nt2
            do 50 jtc = 2,nt2 
               if (jtc .gt. nt2) go to 50
               if (jcoef(i,jtc) .ge. i) go to 50
 45            jbc = jbc + 1
               if (jcoef(i,jbc) .lt. i) go to 45
               jtemp = jcoef(i,jtc)
               jcoef(i,jtc) = jcoef(i,jbc)
               jcoef(i,jbc) = jtemp
               temp = coef(i,jtc)
               coef(i,jtc) = coef(i,jbc)
               coef(i,jbc) = temp
 50         continue
            jtc = 1 
            do 60 jbc = nb1,maxnz
               if (jbc .gt. maxnz) go to 60
               if (jcoef(i,jbc) .le. i) go to 60
 55            jtc = jtc + 1
               if (jcoef(i,jtc) .gt. i) go to 55
               jtemp = jcoef(i,jtc)
               jcoef(i,jtc) = jcoef(i,jbc)
               jcoef(i,jbc) = jtemp
               temp = coef(i,jtc)
               coef(i,jtc) = coef(i,jbc)
               coef(i,jbc) = temp
 60         continue
            jbc = nt2
            do 70 jzc = nz1,maxnz
               if (jzc .gt. maxnz) go to 70
               if (jcoef(i,jzc) .ge. i) go to 70
 65            jbc = jbc + 1
               if (jcoef(i,jbc) .lt. i) go to 65
               jtemp = jcoef(i,jzc)
               jcoef(i,jzc) = jcoef(i,jbc)
               jcoef(i,jbc) = jtemp
               temp = coef(i,jzc)
               coef(i,jzc) = coef(i,jbc)
               coef(i,jbc) = temp
 70         continue
 75      continue
c
 80      nt(icol) = ntt
         nb(icol) = nbb
         ist = ist + ncol
 85   continue
c
c ... exit.
c
 999  maxnzz = maxnz
      return
      end 
      subroutine move4 (ndim,nn,maxnew,jcnew,coef,ncol,nc,
     a                  work,iwork)
      implicit double precision (a-h, o-z)
c
c ... move4 moves the data structure to the form dc/tc/bc, where
c     dc is the main diagonal block, tc is the upper triangular
c     block matrices, and db is the lower triangular block
c     matrices.
c     the above segregation is done for each color.
c     (diagonal data structure, multi-color ordering, with
c     ic or ssor preconditionings (point or block))
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         n        order of system (= nn)
c         maxnew   integer vector giving the number of diagonals
c                   created for each color
c         jcnew    integer array of size ncolor*max(maxnew(i))
c                   giving the diagonal numbers for each color
c         coef     matrix representation array
c         ncolor   number of colors
c         nc       integer vector of length ncolor, giving the number 
c                   of nodes for each color.
c         work     floating point workspace array of length max (nc(i))
c         iwork    integer work array of length max (maxnew(i))
c
c ... specifications for parameters
c
      integer   maxnew(1), jcnew(ncol,1), nc(1), iwork(1)
      dimension coef(ndim,1), work(1)
c
      n = nn
      ncolor = ncol 
      ist = 1
      do 70 icol = 1,ncolor
         ncc = nc(icol)
         ied = ist + ncc - 1
c
c ... compute pointers into sorted jcnew.
c
c ... code jcnew.
c
         maxnz = maxnew(icol) 
         do 15 j = 1,maxnz
            do 5 i = ist,ied
               if (coef(i,j) .ne. 0.0d0) go to 10
 5          continue
            go to 15
 10         jd = jcnew(icol,j)
            jcol = i + jd
            if (jcol .lt. i .and. jcol .ge. ist)
     a                         jcnew(icol,j) = n - jd
            if (jcol .gt. ied) jcnew(icol,j) = 2*n + jd
            if (jcol .lt. ist) jcnew(icol,j) = 3*n - jd
 15      continue
         iwork(1) = 1
         do 30 j = 2,maxnz
            iaux = jcnew(icol,j)
            do 20 k = 1,j-1
               i = j - k
               ktemp = iwork(i)
               if (iaux .gt. jcnew(icol,ktemp)) go to 25
               iwork(i+1) = iwork(i)
 20         continue
            i = 0
 25         iwork(i+1) = j
 30      continue
c
c ... decode jcnew. 
c
         do 35 j = 1,maxnz
            jd = jcnew(icol,j)
            if (jd .gt. n .and. jd .lt. 2*n) jcnew(icol,j) = n - jd
            if (jd .gt. 2*n .and. jd .lt. 3*n)
     a                           jcnew(icol,j) = jd - 2*n
            if (jd .gt. 3*n) jcnew(icol,j) = 3*n - jd
 35      continue
c
c ... sort coef and jcnew.
c
         do 40 i = 1,maxnz
            if (iwork(i) .eq. i) iwork(i) = 0
 40      continue
         do 65 ii = 1,maxnz
            k = iwork(ii)
            if (k .eq. 0) go to 65
            i = ii
 45         jtemp = jcnew(icol,i)
            jcnew(icol,i) = jcnew(icol,k)
            jcnew(icol,k) = jtemp
            do 50 l = ist,ied 
               work(l-ist+1) = coef(l,i)
               coef(l,i) = coef(l,k)
               coef(l,k) = work(l-ist+1)
 50         continue
            iwork(i) = 0
            do 55 j = ii,maxnz
               if (iwork(j) .eq. i) go to 60
 55         continue
            go to 65
 60         i = j
            if (i .ne. k) go to 45
            iwork(k) = 0
 65      continue
         ist = ist + ncc
 70   continue
c
c ... exit.
c
      return
      end 
      subroutine move5 (ndim,n,maxnz,jcoef,coef)
      implicit double precision (a-h, o-z)
c
c ... move5 moves the data structure to the form dc/tc/bc, where
c     dc is the main diagonal block, tc is the upper triangular
c     block matrices, and db is the lower triangular block
c     matrices.
c     (diagonal data structure, with constant block size)
c
c ... parameters -- 
c
c         ndim     row dimension of coef array in defining routine
c         n        order of system
c         maxnz    number of diagonals stored
c         jcoef    integer vector of length maxnz giving the
c                   diagonal numbers
c         coef     matrix representation array
c
c ... specifications for parameters
c
      dimension coef(ndim,maxnz), jcoef(maxnz)
c
c ... move dc to the first columns.
c
      jsh = 1
      jcol = 1
      jget = 0
 5    do 10 j = 1,maxnz
         jd = jcoef(j)
         if (jd .eq. jget) go to 15
 10   continue
      if (jsh .lt. 0) go to 30
      jsh = -1
      jget = -1
      go to 5
 15   if (j .eq. jcol) go to 25
      do 20 i = 1,n 
         temp = coef(i,j)
         coef(i,j) = coef(i,jcol)
         coef(i,jcol) = temp
 20   continue
      jcoef(j) = jcoef(jcol)
      jcoef(jcol) = jd
 25   jcol = jcol + 1
      jget = jget + jsh
      go to 5
c
c ... move tc, bc to the next columns.
c
 30   if (jcol .gt. maxnz) return
      do 35 j = jcol,maxnz
         jd = jcoef(j)
         if (jd .lt. 0) jcoef(j) = n - jd
 35   continue
      jcolsv = jcol 
 40   jsml = jcol
      do 45 j = jcol,maxnz
         jd = jcoef(j)
         if (jd .lt. jcoef(jsml)) jsml = j
 45   continue
      if (jsml .eq. jcol) go to 55
      do 50 i = 1,n 
         temp = coef(i,jsml)
         coef(i,jsml) = coef(i,jcol)
         coef(i,jcol) = temp
 50   continue
      jtemp = jcoef(jsml)
      jcoef(jsml) = jcoef(jcol)
      jcoef(jcol) = jtemp
 55   jcol = jcol + 1
      if (jcol .le. maxnz) go to 40
c
c ... uncode jcoef. 
c
      do 60 j = jcolsv,maxnz
         jd = jcoef(j)
         if (jd .gt. n) jcoef(j) = n - jd
 60   continue
      return
      end 
      subroutine muldc (ndim,nn,coef,ncolor,nc,maxnew,jcnew,x,y)
      implicit double precision (a-h, o-z)
c
c ... muldc computes  y = a*x  for a matrix permuted to an
c     ncolor x ncolor block matrix stored in diagonal format.
c
c ... parameters -- 
c
c        ndim      row dimension of coef array
c        n         order of system
c        coef      floating point array of coefficients
c        ncolor    number of colors in the permutation (= ncol)
c        nc        integer vector of length ncolor giving the
c                   number of nodes for each color
c        maxnew    integer vector giving the number of diagonals
c                   created for each color
c        jcnew     integer array of size ncolor*max(maxnew(i))
c                   giving the diagonal numbers for each color
c        x         vector of length n to be multiplied by
c        y         vector of length n to contain result vector
c
c ... specifications for parameters
c
      integer nc(1), maxnew(1), jcnew(ncolor,2)
      dimension coef(ndim,2), x(1), y(1)
c
      n = nn
      do 10 i =1,n
 10   y(i) = coef(i,1)*x(i)
      i1 = 1
      joff = 0
      do 15 k = 1,ncolor
         ncc = nc(k)
         jlim = maxnew(k) - 1 
         call vaddd (ndim,ncolor,ncc,n,jlim,coef(i1,2),jcnew(k,2),
     a               y(i1),x,joff)
         i1 = i1 + ncc
         joff = joff - ncc
 15   continue
      return
      end 
      subroutine muldct (ndim,nn,coef,ncolor,nc,maxnew,jcnew,x,y)
      implicit double precision (a-h, o-z)
c
c ... muldct computes  y = (a**t)*x  for a matrix permuted to an
c     ncolor x ncolor block matrix stored in diagonal format.
c
c ... parameters -- 
c
c        ndim      row dimension of coef array
c        n         order of system
c        coef      floating point array of coefficients
c        ncolor    number of colors in the permutation (= ncol)
c        nc        integer vector of length ncolor giving the
c                   number of nodes for each color
c        maxnew    integer vector giving the number of diagonals
c                   created for each color
c        jcnew     integer array of size ncolor*max(maxnew(i))
c                   giving the diagonal numbers for each color
c        x         vector of length n to be multiplied by
c        y         vector of length n to contain result vector
c
c ... specifications for parameters
c
      integer nc(1), maxnew(1), jcnew(ncolor,2)
      dimension coef(ndim,2), x(1), y(1)
c
      n = nn
      do 10 i =1,n
 10   y(i) = coef(i,1)*x(i)
      i1 = 1
      joff = 0
      do 15 k = 1,ncolor
         ncc = nc(k)
         jlim = maxnew(k) - 1 
         call vadddt (ndim,ncolor,ncc,n,jlim,coef(i1,2),jcnew(k,2),
     a                y,x(i1),joff)
         i1 = i1 + ncc
         joff = joff - ncc
 15   continue
      return
      end 
      subroutine mult1 (ndim,maxnz,coef,jcoef,wksp,nn,x,y)
      implicit double precision (a-h, o-z)
c
c ... mult1 computes y = a*x, a matrix-vector product.
c     the diagonal is assumed to be in column one.
c     (purdue storage format) 
c
c ... parameters -- 
c
c         ndim     row dimension of coef in defining routine
c         maxnz    number of columns in coef
c         coef     array of matrix nonzeros
c         jcoef    array of matrix column numbers 
c         wksp     workspace array of length n
c         n        order of matrix (= nn)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension coef(ndim,2), x(1), y(1), wksp(1) 
      integer   jcoef(ndim,2) 
c
      n = nn
      maxm1 = maxnz - 1
      do 10 i = 1,n 
 10   y(i) = coef(i,1)*x(i)
      call vaddp (ndim,ndim,n,maxm1,coef(1,2),jcoef(1,2),y,x,wksp)
      return
      end 
      subroutine mult2n (ndim,maxnz,coef,jcoef,nn,x,y)
      implicit double precision (a-h, o-z)
c
c ... mult2n computes y = a*x, a matrix-vector product.
c     the diagonal is assumed to be in column one.  all diagonals of
c     the matrix must be stored.
c     (nonsymmetric diagonal storage format)
c
c ... parameters -- 
c
c         ndim     row dimension of coef in defining routine
c         maxnz    number of columns in coef
c         coef     array of matrix diagonals
c         jcoef    array of matrix diagonal numbers
c         n        dimension of matrix (= nn)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension coef(ndim,2), x(1), y(1)
      integer   jcoef(2)
c
      n = nn
      do 10 i = 1,n 
 10   y(i) = coef(i,1)*x(i)
      if (maxnz .le. 1) return
      maxm1 = maxnz - 1
      call vaddd (ndim,1,n,n,maxm1,coef(1,2),jcoef(2),y,x,0)
      return
      end 
      subroutine mult2s (ndim,maxnz,coef,jcoef,nn,x,y)
      implicit double precision (a-h, o-z)
c
c ... mult2s computes y = a*x, a matrix-vector product.
c     the diagonal is assumed to be in column 1.  only the upper
c     diagonals and the main diagonal are assumed stored.
c     (symmetric diagonal storage format)
c
c ... parameters -- 
c
c         ndim     row dimension of coef in defining routine
c         maxnz    number of columns in coef
c         coef     array of matrix diagonals
c         jcoef    array of matrix diagonal numbers
c         n        dimension of matrix (= nn)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension coef(ndim,1), x(1), y(1)
      integer   jcoef(2)
c
      n = nn
      do 10 i = 1,n 
 10   y(i) = coef(i,1)*x(i)
      if (maxnz .le. 1) return
c
      do 25 j = 2,maxnz
         ind = jcoef(j)
         len = n - ind
         do 15 i = 1,len
 15      y(i) = y(i) + coef(i,j)*x(i+ind)
         do 20 i = 1,len
 20      y(i+ind) = y(i+ind) + coef(i,j)*x(i)
 25   continue
      return
      end 
      subroutine mul1t (ndim,maxnz,coef,jcoef,wksp,nn,x,y)
      implicit double precision (a-h, o-z)
c
c ... mul1t computes y = (a**t)*x, a matrix-vector product. 
c     the diagonal is assumed to be in column one.
c     (purdue storage format) 
c
c ... parameters -- 
c
c         ndim     row dimension of coef in defining routine
c         maxnz    number of columns in coef
c         coef     array of matrix nonzeros
c         jcoef    array of matrix column numbers 
c         wksp     workspace array of length n
c         n        dimension of matrix (= nn)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension coef(ndim,2), x(1), y(1), wksp(1) 
      integer   jcoef(ndim,2) 
c
      n = nn
      do 10 i = 1,n 
 10   y(i) = coef(i,1)*x(i)
      if (maxnz .le. 1) return
      maxm1 = maxnz - 1
      call vaddpt (ndim,ndim,n,maxm1,coef(1,2),jcoef(1,2),y,x,wksp)
      return
      end 
      subroutine mul2nt (ndim,maxnz,coef,jcoef,nn,x,y)
      implicit double precision (a-h, o-z)
c
c ... mul2nt computes y = (a**t)*x, a matrix-vector product.
c     the diagonal is assumed to be in column one.  all diagonals of
c     the matrix must be stored.
c     (nonsymmetric diagonal storage format)
c
c ... parameters -- 
c
c         ndim     row dimension of coef in defining routine
c         maxnz    number of columns in coef
c         coef     array of matrix diagonals
c         jcoef    array of matrix diagonal numbers
c         n        dimension of matrix (= nn)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension coef(ndim,2), x(1), y(1)
      integer   jcoef(2)
c
      n = nn
      do 10 i = 1,n 
 10   y(i) = coef(i,1)*x(i)
      if (maxnz .le. 1) return
      maxm1 = maxnz - 1
      call vadddt (ndim,1,n,n,maxm1,coef(1,2),jcoef(2),y,x,0)
      return
      end 
      subroutine mult3 (mm,np,a,ia,ja,wksp,x,y)
      implicit double precision (a-h, o-z)
c
c ... mult3 computes y = a*x, a matrix-vector product.
c     the diagonal is assumed to be in the first partition. 
c     (symmetric sparse storage format) 
c
c ... parameters -- 
c
c         m        number of partitions 
c         np       integer vector of length m+1 giving partition
c                    pointers 
c         a        floating point vector giving matrix coefficients
c         ia       integer vector giving i values 
c         ja       integer vector giving j values 
c         wksp     workspace vector of length 2*n (keygs = 1 only)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension a(1), x(1), y(1), wksp(1)
      integer   np(2), ia(1), ja(1)
c
      m = mm
      ied = np(2) - 1
      do 10 i = 1,ied
 10   y(i) = a(i)*x(i)
      mm1 = m - 1
      call vadds (mm1,np(2),ia,ja,a,y,x,wksp)
      call vadds (mm1,np(2),ja,ia,a,y,x,wksp)
      return
      end 
      subroutine mult3n (mm,np,a,ia,ja,wksp,x,y)
      implicit double precision (a-h, o-z)
c
c ... mult3n computes y = a*x, a matrix-vector product.
c     the diagonal is assumed to be in the first partition. 
c     (nonsymmetric sparse storage format)
c
c ... parameters -- 
c
c         m        number of partitions 
c         np       integer vector of length m+1 giving partition
c                    pointers 
c         a        floating point vector giving matrix coefficients
c         ia       integer vector giving i values 
c         ja       integer vector giving j values 
c         wksp     workspace vector of length 2*n (keygs = 1 only)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension a(1), x(1), y(1), wksp(1)
      integer   np(2), ia(1), ja(1)
c
      m = mm
      ied = np(2) - 1
      do 10 i = 1,ied
 10   y(i) = a(i)*x(i)
      mm1 = m - 1
      call vadds (mm1,np(2),ia,ja,a,y,x,wksp)
      return
      end 
      subroutine mul3nt (mm,np,a,ia,ja,wksp,x,y)
      implicit double precision (a-h, o-z)
c
c ... mul3nt computes y = (a**t)*x, a matrix-vector product.
c     the diagonal is assumed to be in the first partition. 
c     (nonsymmetric sparse storage format)
c
c ... parameters -- 
c
c         m        number of partitions 
c         np       integer vector of length m+1 giving partition
c                    pointers 
c         a        floating point vector giving matrix coefficients
c         ia       integer vector giving i values 
c         ja       integer vector giving j values 
c         wksp     workspace vector of length 2*n (keygs = 1 only)
c         x        multiplying vector of length n 
c         y        product vector of length n
c
c ... specifications for parameters
c
      dimension a(1), x(1), y(1), wksp(1)
      integer   np(2), ia(1), ja(1)
c
      m = mm
      ied = np(2) - 1
      do 10 i = 1,ied
 10   y(i) = a(i)*x(i)
      mm1 = m - 1
      call vadds (mm1,np(2),ja,ia,a,y,x,wksp)
      return
      end 
