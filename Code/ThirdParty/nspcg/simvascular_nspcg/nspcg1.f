      subroutine nspcg (precon,accel,ndimm,mdimm,nn,maxnzz,coef,
     a                  jcoef,p,ip,u,ubar,rhs,wksp,iwksp,nw,inw,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... nspcg is the driver for the nspcg package.
c
c ... parameters -- 
c
c       precon    preconditioning module
c       accel     acceleration module
c       coef      floating point matrix data array
c       jcoef     integer matrix data array
c       n         input integer.  order of the system (= nn)
c       u         input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       wksp      floating point workspace vector of length nw
c       iwksp     integer workspace vector of length inw
c       nw        length of wksp upon input, amount used upon
c                  output
c       inw       length of iwksp upon input, amount used upon
c                  output
c       iparm     integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method. 
c       rparm     floating point vector of length 30.  allows user to 
c                 specify some floating point parameters which affect 
c                 the method. 
c       ier       output integer.  error flag.
c
c ... specifications for parameters
c
      external  accel, precon 
      integer   iparm(30), jcoef(2), p(1), ip(1), iwksp(1)
      dimension coef(1), rhs(1), u(1), ubar(1), rparm(30), wksp(1)
c
c *** begin -- package common 
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      ier = 0
      ndim = ndimm
      mdim = mdimm
      n = nn
      maxnz = maxnzz
      lenr = nw
      leni = inw
      irmax = 0
      iimax = 0
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,1,ier)
      if (ier .lt. 0) return
      timfac = 0.0d0
      call pointr (1,wksp,iwksp,ier)
c
c ... call preparatory routines.
c
c ... remove zeros from jcoef for purdue data structure.
c
      if (nstore .eq. 1) call adjust (n,ndim,maxnz,jcoef,1) 
      call prep (coef,jcoef,wksp(irpnt),iwksp(iipnt),n,nstore,ier)
      if (ier .lt. 0) then
         call ershow (ier,'nspcg')
         go to 20
      endif
c
c ... eliminate penalty-method dirichlet points, if requested.
c
      ielim = iparm(24)
      tol = rparm(15)
      if (ielim .eq. 1) call elim (n,jcoef,coef,rhs,wksp,iwksp,
     a                             tol) 
c
c ... determine symmetry of matrix.
c
      if (nstore .eq. 1 .and. isymm .eq. 2) call detsym
     a                (ndim,maxnz,coef,jcoef,n,isymm)
c
c ... scale matrix. 
c
      call scale (coef,jcoef,wksp,1,n,u,ubar,rhs,ier)
      if (ier .lt. 0) go to 20
c
c ... permute matrix.
c
      call permut (coef,jcoef,p,ip,wksp,iwksp,1,n,u,ubar,rhs,ier)
      if (ier .lt. 0) go to 15
c
c ... call iterative routine. 
c
      call precon (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a             iparm,rparm,ier)
c
c ... unpermute matrix.
c
      call permut (coef,jcoef,p,ip,wksp,iwksp,2,n,u,ubar,rhs,ier)
c
c ... unscale matrix.
c
 15   call scale (coef,jcoef,wksp,2,n,u,ubar,rhs,ier)
c
c ... restore zeros to jcoef for purdue data structure.
c
 20   if (nstore .eq. 1) call adjust (n,ndim,maxnz,jcoef,2) 
      t2 = timer (dummy)
      timtot = t2 - t1
      iparm(18) = ipropa
      iparm(23) = isymm
      rparm(13) = timfac
      rparm(14) = timtot
      call echall (n,iparm,rparm,2,1,ier)
c
      call pointr (2,wksp,iwksp,ier)
      nw = irmax
      inw = iimax
      maxnzz = maxnz
      return
      end 
      subroutine rsnsp (precon,accel,ndimm,mdimm,nn,maxnzz,coef,
     a                  jcoef,p,ip,u,ubar,rhs,wksp,iwksp,nw,inw,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rsnsp is the driver for the nspcg package for methods 
c     applied to the explicitly computed reduced system.
c
c ... parameters -- 
c
c       precon    preconditioning module
c       accel     acceleration module
c       coef      floating point matrix data array
c       jcoef     integer matrix data array
c       n         input integer.  order of the system (= nn)
c       u         input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       wksp      floating point workspace of length nw
c       iwksp     integer workspace of length inw 
c       nw        length of wksp upon input, amount used upon
c                  output
c       inw       length of iwksp upon input, amount used upon
c                  output
c       iparm     integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method. 
c       rparm     floating point vector of length 30.  allows user to 
c                 specify some floating point parameters which affect 
c                 the method. 
c       ier       output integer.  error flag.
c
c ... specifications for parameters
c
      external  accel, precon 
      integer   iparm(30), jcoef(2), p(1), ip(1), iwksp(1)
      dimension coef(1), rhs(1), u(1), ubar(1), rparm(30), wksp(1)
c
c *** begin -- package common 
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      ier = 0
      ndim = ndimm
      mdim = mdimm
      n = nn
      maxnz = maxnzz
      lenr = nw
      leni = inw
      irmax = 0
      iimax = 0
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,1,ier)
      timfac = 0.0d0
      call pointr (1,wksp,iwksp,ier)
c
c ... call preparatory routines.
c
c ... remove zeros from jcoef for purdue data structure.
c
      if (nstore .eq. 1) call adjust (n,ndim,maxnz,jcoef,1) 
      call prep (coef,jcoef,wksp(irpnt),iwksp(iipnt),n,nstore,ier)
      if (ier .lt. 0) then
         call ershow (ier,'rsnsp')
         go to 20
      endif
c
c ... eliminate penalty-method dirichlet points, if requested.
c
      ielim = iparm(24)
      tol = rparm(15)
      if (ielim .eq. 1) call elim (n,jcoef,coef,rhs,wksp,iwksp,
     a                             tol) 
c
c ... determine symmetry of matrix.
c
      if (nstore .eq. 1 .and. isymm .eq. 2) call detsym
     a                (ndim,maxnz,coef,jcoef,n,isymm)
c
c ... form reduced system matrix.
c
      call rsprep (coef,jcoef,wksp,iwksp,n,rhs,u,ubar,
     a             p,ip,nr,irs,ijcrs,irsrhs,ier)
c
c ... scale matrix. 
c
      call scale (wksp(irs),iwksp(ijcrs),wksp,1,nr,u,ubar,
     a            wksp(irsrhs),ier)
      if (ier .lt. 0) go to 20
c
c ... call iterative routine. 
c
      call precon (accel,wksp(irs),iwksp(ijcrs),nr,u,ubar,
     a             wksp(irsrhs),wksp,iwksp,iparm,rparm,ier) 
c
c ... unscale matrix.
c
      call scale (wksp(irs),iwksp(ijcrs),wksp,2,nr,u,ubar,
     a            wksp(irsrhs),ier)
c
c ... restore to original system.
c
      call rspost (coef,jcoef,wksp,iwksp,n,rhs,u,ubar,
     a             p,ip,nr,irs,ijcrs,ier)
c
c ... restore zeros to jcoef for purdue data structure.
c
 20   if (nstore .eq. 1) call adjust (n,ndim,maxnz,jcoef,2) 
      t2 = timer (dummy)
      timtot = t2 - t1
      iparm(18) = ipropa
      iparm(23) = isymm
      rparm(13) = timfac
      rparm(14) = timtot
      call echall (n,iparm,rparm,2,1,ier)
c
      call pointr (2,wksp,iwksp,ier)
      nw = irmax
      inw = iimax
      maxnzz = maxnz
      return
      end 
      subroutine prep (coef,jcoef,wksp,iwksp,nn,nstore,ier) 
      implicit double precision (a-h, o-z)
c
c ... prep puts the diagonal entries of the matrix into column
c     one of coef.
c
c ... parameters -- 
c
c         n       dimension of matrix
c         jcoef   integer matrix representation array
c         coef    matrix representation array
c         wksp    workspace array of size n
c         iwksp   integer workspace
c         ier     error flag -- on return, values mean
c                      0 -- no errors detected
c                     -5 -- nonexistent diagonal element
c
c ... specifications for parameters
c
      integer   jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cmpart / mpstrt, mpart
c
      n = nn
      go to (5,10,10,15,15), nstore
 5    call prep1 (n,ndim,maxnz,jcoef,coef,ier)
      return
 10   call prep2 (n,ndim,maxnz,jcoef,coef,wksp,ier)
      return
 15   call needw ('prep',1,iipnt,2*n+1,ier)
      if (ier .lt. 0) return
      call prep3 (n,maxnz,jcoef,jcoef(ndim+1),coef,mpart,
     a            iwksp,iwksp(n+2))
      mpstrt = iipnt
      iipnt = iipnt + mpart + 1
      return
      end 
      subroutine pointr (icall,wksp,iwksp,ier)
      implicit double precision (a-h, o-z)
c
c ... pointr adjusts pointers according to ifact. 
c
c ... parameters -- 
c
c       icall     indicates beginning or ending call
c                  = 1  for beginning
c                  = 2  for ending
c       wksp      floating point workspace vector 
c       iwksp     integer workspace vector
c
c ... specifications for parameters
c
      integer   iwksp(1)
      dimension wksp(1)
c
c *** begin -- package common 
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      if (icall .eq. 2) go to 15
c
c ... initialize pointers.
c
      if (ifact .eq. 0) return
      iipnt = 1
      irpnt = 1
      nfactr = 0
      nfacti = 0
      ifactr = 1
      ifacti = 1
      return
c
c ... reset pointers for return
c
 15   if (ier .lt. 0) return
      if (nfacti .eq. 0) go to 20
      call vicopy (nfacti,iwksp(ifacti),iwksp)
      iipnt = nfacti + 1
      ifacti = 1
 20   if (nfactr .eq. 0) return
      call vcopy (nfactr,wksp(ifactr),wksp)
      iwkpt2 = iwkpt2 - ifactr + 1
      irpnt = nfactr + 1
      ifactr = 1
      return
      end 
      subroutine needw (subnam,isw,istart,length,ier)
      implicit double precision (a-h, o-z)
c
c ... needw determines if enough integer or floating point
c     workspace is available. 
c
c ... parameters -- 
c
c        subnam  name of calling routine
c        isw     switch for floating point or integer workspace check
c                 = 0     floating point
c                 = 1     integer
c        istart  starting address
c        length  length desired
c        ier     error indicator (output)
c                 = -2    insufficient floating point workspace
c                 = -3    insufficient integer workspace
c
c ... specifications for parameters
c
      character*10 subnam
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      newlen = istart + length - 1
      if (isw .eq. 1) go to 10
      if (lenr .ge. newlen) go to 5
      ier = -2
      call ershow (ier,subnam)
 5    irmax = max (irmax,newlen)
      return
 10   if (leni .ge. newlen) go to 15
      ier = -3
      call ershow (ier,subnam)
 15   iimax = max (iimax,newlen)
      return
      end 
      subroutine scale (coef,jcoef,wksp,icall,n,u,ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... scale scales the matrix, u, ubar, and rhs.
c
c ... parameters -- 
c
c       icall     key to indicate whether scaling (icall=1) or
c                  unscaling (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -4   nonpositive diagonal element
c
c ... specifications for parameters
c
      integer jcoef(2)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c ... data common blocks
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      if (iscale .ne. 1) return
      go to (5,10,10,15,15), nstore
 5    call scalep (coef,jcoef,wksp,icall,n,u,ubar,rhs,ier)
      return
 10   call scaled (coef,jcoef,wksp,icall,n,u,ubar,rhs,ier)
      return
 15   call scales (coef,jcoef,wksp,icall,n,u,ubar,rhs,ier)
      return
      end 
      subroutine permut (coef,jcoef,p,ip,wksp,iwksp,icall,n,u,
     a                   ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... permut permutes the matrix, u, ubar, and rhs.
c
c ... parameters -- 
c
c       icall     key to indicate whether permuting (icall=1) or
c                  unpermuting (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -2   insufficient floating point space to permute system
c                  =  -3   insufficient integer space to permute
c                          system
c
c ... specifications for parameters
c
      integer jcoef(2), p(1), ip(1), iwksp(1)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c ... data common blocks
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      if (iperm .ne. 1) return
      go to (5,10,10,15,15), nstore
 5    call permp (coef,jcoef,p,ip,wksp,iwksp,
     a            icall,n,u,ubar,rhs,ier)
      return
 10   call permd (coef,jcoef,p,ip,wksp,iwksp,
     a            icall,n,u,ubar,rhs,ier)
      return
 15   call perms (coef,jcoef,p,ip,wksp,iwksp,
     a            icall,n,u,ubar,rhs,ier)
      return
      end 
      subroutine elim (n,jcoef,coef,rhs,wksp,iwksp,toll)
      implicit double precision (a-h, o-z)
c
c ... elim removes rows of the matrix for which the ratio of the
c     sum of off-diagonal elements to the diagonal element is
c     small (less than tol) in absolute value.
c     this is to take care of matrices arising from finite
c     element discretizations of partial differential equations
c     with dirichlet boundary conditions implemented by penalty
c     methods.  any such rows and corresponding columns are then
c     eliminated (set to the identity after correcting the rhs).
c
c ... parameter list --
c
c         n       dimension of matrix
c         jcoef   integer array of matrix representation
c         coef    array of sparse matrix representation
c         rhs     right hand side of matrix problem
c         wksp    wksp array of length n
c         tol     tolerance factor  (= toll)
c
c ... specifications for arguments
c
      common / cmpart / mpstrt, mpart
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer   jcoef(2), iwksp(1)
      dimension coef(1), rhs(1), wksp(1)
c
      tol = toll
      go to (5,10,15,20,25), nstore
 5    call elim1 (n,ndim,maxnz,jcoef,coef,rhs,wksp(irpnt),tol)
      return
 10   call elim2 (n,ndim,maxnz,jcoef,coef,rhs,wksp(irpnt),tol)
      return
 15   call elim3 (n,ndim,maxnz,jcoef,coef,rhs,wksp(irpnt),tol)
      return
 20   call elim4 (mpart,iwksp(mpstrt),jcoef,jcoef(ndim+1),
     a            coef,rhs,wksp(irpnt),tol)
      return
 25   call elim5 (mpart,iwksp(mpstrt),jcoef,jcoef(ndim+1),
     a            coef,rhs,wksp(irpnt),tol)
      return
      end 
      subroutine scaled (coef,jcoef,wksp,icall,nn,u,ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... scaled scales the matrix, u, ubar, and rhs. 
c     (symmetric or nonsymmetric diagonal format) 
c
c ... parameters -- 
c
c       icall     key to indicate whether scaling (icall=1) or
c                  unscaling (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -4   nonpositive diagonal element
c
c ... specifications for parameters
c
      integer jcoef(2)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      iflag = 0
      if (ntest .eq. 6) iflag = 1
      if (icall .eq. 2) go to 20
c
c ... scale system. 
c
c ... check for sufficient room.
c
      call needw ('scaled',0,irpnt,n,ier)
      if (ier .lt. 0) return
      iptscl = irpnt
      irpnt = irpnt + n
      call scal2 (n,ndim,maxnz,jcoef,coef,rhs,u,ubar,
     a            wksp(iptscl),iflag,ier)
      if (ier .lt. 0) call ershow (ier,'scaled')
      return
c
c ... unscale system.
c
 20   call uscal2 (n,ndim,maxnz,jcoef,coef,rhs,u,ubar,
     a             wksp(iptscl),iflag)
      return
      end 
      subroutine scalep (coef,jcoef,wksp,icall,nn,u,ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... scalep scales the matrix, u, ubar, and rhs. 
c     (purdue format)
c
c ... parameters -- 
c
c       icall     key to indicate whether scaling (icall=1) or
c                  unscaling (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -4   nonpositive diagonal element
c
c ... specifications for parameters
c
      integer jcoef(2)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      iflag = 0
      if (ntest .eq. 6) iflag = 1
      if (icall .eq. 2) go to 20
c
c ... scale system. 
c
c ... check for sufficient room.
c
      call needw ('scalep',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      iptscl = irpnt
      irpnt = irpnt + n
      call scal1 (n,ndim,maxnz,jcoef,coef,rhs,u,ubar,
     a            wksp(iptscl),wksp(irpnt),iflag,ier)
      if (ier .lt. 0) call ershow (ier,'scalep')
      return
c
c ... unscale system.
c
 20   call uscal1 (n,ndim,maxnz,jcoef,coef,rhs,u,ubar,
     a             wksp(iptscl),wksp(irpnt),iflag)
      return
      end 
      subroutine scales (coef,jcoef,wksp,icall,nn,u,ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... scales scales the matrix, u, ubar, and rhs. 
c     (sparse format)
c
c ... parameters -- 
c
c       icall     key to indicate whether scaling (icall=1) or
c                  unscaling (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -4   nonpositive diagonal element
c
c ... specifications for parameters
c
      integer jcoef(2)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      iflag = 0
      if (ntest .eq. 6) iflag = 1
      if (icall .eq. 2) go to 10
c
c ... scale system. 
c
c ... check for sufficient room.
c
      call needw ('scales',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      iptscl = irpnt
      irpnt = irpnt + n
      call scal3 (n,maxnz,jcoef,jcoef(ndim+1),coef,rhs,u,ubar,
     a            wksp(iptscl),wksp(irpnt),iflag,ier)
      if (ier .lt. 0) call ershow (ier,'scales')
      return
c
c ... unscale system.
c
 10   call uscal3 (n,maxnz,jcoef,jcoef(ndim+1),coef,rhs,u,ubar,
     a             wksp(iptscl),wksp(irpnt),iflag)
      return
      end 
      subroutine permd (coef,jcoef,p,ip,wksp,iwksp,icall,nn,u,
     a                  ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... permd permutes the matrix, u, ubar, and rhs.
c     (diagonal format)
c
c ... parameters -- 
c
c       icall     key to indicate whether permuting (icall=1) or
c                  unpermuting (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -2   insufficient floating point space to permute system
c                  =  -3   insufficient integer space to permute
c                          system
c
c ... specifications for parameters
c
      integer jcoef(2), p(1), ip(1), iwksp(1)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
c
      n = nn
      if (icall .eq. 2) go to 65
c
c ... permute system.
c
c ... check for sufficient storage to permute matrix
c
      call needw ('permd',0,irpnt,n,ier)
      if (ier .lt. 0) return
      nc = iipnt
      call needw ('permd',1,nc,n,ier)
      if (ier .lt. 0) return
      call pgen (n,p,ip,iwksp(nc),ncolor)
      ipt = nc + ncolor
      ncmax = 0
      do 16 i = nc,ipt-1
         if (ncmax .lt. iwksp(i)) ncmax = iwksp(i)
 16   continue
      call needw ('permd',1,ipt,ncolor+1,ier)
      if (ier .lt. 0) return
      call iptgen (ncolor,iwksp(ipt),iwksp(nc))
      maxnew = ipt + ncolor + 1
      jcnew = maxnew + ncolor 
      lbhb = jcnew + ncolor*mdim
      call needw ('permd',1,maxnew,ncolor+ncolor*mdim+n,ier)
      if (ier .lt. 0) return
      isym = nstore - 2
      call pmdg (ndim,mdim,n,maxnz,jcoef,coef,ncolor,iwksp(nc),
     a           p,ip,maxd,iwksp(maxnew),
     a           iwksp(jcnew),wksp(irpnt),iwksp(lbhb),isym,ier)
      if (ier .lt. 0) then
         call ershow (ier,'permd')
         return
      endif
      lbhb = jcnew + ncolor*maxd
      iblock = lbhb + ncolor
      call move4 (ndim,n,iwksp(maxnew),iwksp(jcnew),coef,ncolor,
     a            iwksp(nc),wksp(irpnt),iwksp(lbhb))
      call needw ('permd',1,lbhb,ncolor+3*ncolor*(maxd+1),ier)
      if (ier .lt. 0) return
      call define (ndim,iwksp(maxnew),iwksp(jcnew),coef,ncolor,
     a             iwksp(nc),iwksp(iblock),iwksp(lbhb))
      lbhbm = iwksp(lbhb)
      do 45 j = 1,ncolor-1
         if (lbhbm .lt. iwksp(lbhb+j)) lbhbm = iwksp(lbhb+j)
 45   continue
      is1 = iblock + 3*ncolor*lbhbm
      is2 = is1 + ncolor
      call needw ('permd',1,is1,2*ncolor,ier)
      if (ier .lt. 0) return
      call prbblk (ncolor,ncolor,iwksp(iblock),iwksp(lbhb), 
     a             iwksp(is1),iwksp(is2),propa)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
      iipnt = is1
      call pervec (n,p,rhs,wksp(irpnt)) 
      call pervec (n,p,u,wksp(irpnt))
      if (ntest .eq. 6) call pervec (n,p,ubar,wksp(irpnt))
      return
c
c ... unpermute system.
c
 65   call needw ('permd',1,iipnt,2*n,ier)
      if (ier .lt. 0) return
      isym = nstore - 2
      call unpmdg (ndim,n,maxnz,jcoef,coef,ncolor,iwksp(nc),
     a             p,ip,maxd,iwksp(maxnew),
     a             iwksp(jcnew),wksp(irpnt),iwksp(iipnt),isym)
      call pervec (n,ip,rhs,wksp(irpnt))
      call pervec (n,ip,u,wksp(irpnt))
      if (ntest .eq. 6) call pervec (n,ip,ubar,wksp(irpnt)) 
      return
      end 
      subroutine permp (coef,jcoef,p,ip,wksp,iwksp,icall,nn,u,
     a                  ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... permp permutes the matrix, u, ubar, and rhs.
c     (purdue format)
c
c ... parameters -- 
c
c       icall     key to indicate whether permuting (icall=1) or
c                  unpermuting (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -2   insufficient floating point space to permute
c                          system
c                  =  -3   insufficient integer space to permute
c                          system
c
c ... specifications for parameters
c
      integer jcoef(2), p(1), ip(1), iwksp(1)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
c
      n = nn
      if (icall .eq. 2) go to 40
c
c ... permute system.
c
c ... check for sufficient storage to permute matrix
c
      call needw ('permp',0,irpnt,n,ier)
      if (ier .lt. 0) return
      nc = iipnt
      call needw ('permp',1,nc,n,ier)
      if (ier .lt. 0) return
      call pgen (n,p,ip,iwksp(nc),ncolor)
      ipt = nc + ncolor
      ncmax = 0
      do 20 i = nc,ipt-1
         if (ncmax .lt. iwksp(i)) ncmax = iwksp(i)
 20   continue
      call needw ('permp',1,ipt,ncolor+1,ier)
      if (ier .lt. 0) return
      call iptgen (ncolor,iwksp(ipt),iwksp(nc))
      iipnt = iipnt + 2*ncolor + 1
      call needw ('permp',1,iipnt,n,ier)
      if (ier .lt. 0) return
      call permat (ndim,maxnz,coef,jcoef,
     a                    wksp(irpnt),iwksp(iipnt),n,p)
      call needw ('permp',1,iipnt,2*ncolor,ier)
      if (ier .lt. 0) return
      ndt = iipnt
      ndb = iipnt + ncolor
      call move3 (ndim,mdim,n,maxnz,jcoef,coef,iwksp(ndt),
     a            iwksp(ndb),ncolor,iwksp(nc),ier)
      iipnt = iipnt + 2*ncolor
      if (ier .lt. 0) then
         call ershow (ier,'permp')
         return
      endif
      call pervec (n,p,rhs,wksp(irpnt)) 
      call pervec (n,p,u,wksp(irpnt))
      if (ntest .eq. 6) call pervec (n,p,ubar,wksp(irpnt))
      return
c
c ... unpermute system.
c
 40   call needw ('permp',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call needw ('permp',1,iipnt,n,ier)
      if (ier .lt. 0) return
      call permat (ndim,maxnz,coef,jcoef,
     a                    wksp(irpnt),iwksp(iipnt),n,ip)
      call pervec (n,ip,rhs,wksp(irpnt))
      call pervec (n,ip,u,wksp(irpnt))
      if (ntest .eq. 6) call pervec (n,ip,ubar,wksp(irpnt)) 
      return
      end 
      subroutine perms (coef,jcoef,p,ip,wksp,iwksp,icall,nn,u,
     a                  ubar,rhs,ier)
      implicit double precision (a-h, o-z)
c
c ... perms permutes the matrix, u, ubar, and rhs.
c     (sparse format)
c
c ... parameters -- 
c
c       icall     key to indicate whether permuting (icall=1) or
c                  unpermuting (icall=2) is to be done
c       n         order of system
c       u         current solution estimate
c       ubar      input vector containing the true solution 
c                  (optional) 
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       ier       error flag
c                  =   0   no errors detected
c                  =  -2   insufficient floating point space to permute
c                          system
c                  =  -3   insufficient integer space to permute
c                          system
c
c ... specifications for parameters
c
      integer jcoef(2), p(1), ip(1), iwksp(1)
      dimension rhs(1), u(1), ubar(1), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
c *** end   -- package common 
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      n = nn
      isym = 0
      if (nstore .eq. 5) isym = 1
      if (icall .eq. 2) go to 10
c
c ... permute system.
c
c ... check for sufficient storage to permute matrix
c
      call needw ('perms',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call needw ('perms',1,iipnt,n,ier)
      if (ier .lt. 0) return
      call pgen (n,p,ip,iwksp(iipnt),ncolor)
      call permas (isym,n,maxnz,jcoef,jcoef(ndim+1),
     a             coef,wksp(irpnt),p)
      call pervec (n,p,rhs,wksp(irpnt)) 
      call pervec (n,p,u,wksp(irpnt))
      if (ntest .eq. 6) call pervec (n,p,ubar,wksp(irpnt))
      return
c
c ... unpermute system.
c
 10   call needw ('perms',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call needw ('perms',1,iipnt,n,ier)
      if (ier .lt. 0) return
      call permas (isym,n,maxnz,jcoef,jcoef(ndim+1),
     a             coef,wksp(irpnt),ip) 
      call pervec (n,ip,rhs,wksp(irpnt))
      call pervec (n,ip,u,wksp(irpnt))
      if (ntest .eq. 6) call pervec (n,ip,ubar,wksp(irpnt)) 
      return
      end 
      subroutine rsprep (coef,jcoef,wksp,iwksp,nn,rhs,u,ubar,
     a                   p,ip,nrr,irs,ijcrs,irsrhs,ier)
      implicit double precision (a-h, o-z)
c
c ... rsprep is the preprocessor for methods using the
c     explicitly-computed reduced system.
c
c ... parameters -- 
c
c       coef      floating point matrix data array
c       jcoef     integer matrix data array
c       n         input integer.  order of the system (= nn)
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       u         current solution estimate
c       ubar      exact solution vector (if known)
c       nr        order of the reduced system upon output
c       irs       pointer into wksp for reduced system matrix
c       ijcrs     pointer into wksp for reduced system integer
c                  array
c       irsrhs    pointer into wksp for reduced system rhs
c       ier       output integer.  error flag.
c
c ... specifications for parameters
c
      integer jcoef(2), iwksp(1), p(1), ip(1)
      dimension coef(1), rhs(1), u(1), ubar(1), wksp(1)
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / rscons / ndimrs, mdimrs, maxzrs
c
      n = nn
c
c ... permute matrix.
c
      call permut (coef,jcoef,p,ip,wksp,iwksp,1,
     a             n,u,ubar,rhs,ier)
      if (ier .lt. 0) return
c
c ... form reduced system matrix.
c
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      irs = irpnt
      ijcrs = iipnt 
      length = lenr - irpnt + 1
      call vfill (length,wksp(irpnt),0.0d0)
      if (nstore .ge. 2) go to 30
c
c ... purdue storage.
c
      call needw ('rsprep',0,irpnt,3*nr,ier)
      if (ier .lt. 0) return
      call needw ('rsprep',1,iipnt,2*nr,ier)
      if (ier .lt. 0) return
      lim1 = (lenr - 2*nr - irpnt + 1)/nr
      lim2 = (leni - nr - iipnt + 1)/nr 
      maxlim = min(lim1,lim2)
      ip1 = irpnt + nr*maxlim 
      ip2 = iipnt + nr*maxlim 
      call rsmatp (ndim,nr,maxnz,jcoef,coef,maxrs,iwksp(ijcrs),
     a             wksp(irs),maxlim,wksp(ip1),iwksp(ip2),ier)
      if (ier .lt. 0) then
         call ershow (ier,'rsprep')
         return
      endif
      irpnt = irpnt + nr*maxrs
      iipnt = iipnt + nr*maxrs
      go to 45
c
c ... diagonal storage.
c
 30   call needw ('rsprep',0,irpnt,nr,ier)
      if (ier .lt. 0) return
      call needw ('rsprep',1,iipnt,nr,ier)
      if (ier .lt. 0) return
      maxlim = length/nr
      isym = 0
      if (nstore .eq. 3) isym = 1
      call rsmatd (ndim,nr,nb,iwksp(maxnew),iwksp(jcnew),coef,
     a             coef(ndim+1),coef(nr+ndim+1),coef(nr+1),maxrs,
     a             iwksp(ijcrs),wksp(irs),maxlim,isym,ier)
      if (ier .lt. 0) then
         call ershow (ier,'rsprep')
         return
      endif
      irpnt = irpnt + nr*maxrs
      iipnt = iipnt + maxrs
c
c ... form reduced system rhs.
c
 45   irsrhs = irpnt
      ip1 = irpnt + nr
      call needw ('rsprep',0,irpnt,n+nr,ier)
      if (ier .lt. 0) return
      if (nstore .eq. 1) call rsbegp (n,nr,ndim,maxnz,jcoef,
     a                     coef,wksp(irsrhs),rhs,wksp(ip1)) 
      if (nstore .ge. 2) call rsrhsd (n,nr,ndim,iwksp(maxnew),
     a                    iwksp(jcnew),coef,wksp(irsrhs),rhs,
     a                    wksp(ip1))
      irpnt = irpnt + nr
c
c ... update constants.
c
      ndimrs = ndim 
      mdimrs = mdim 
      maxzrs = maxnz
      ndim = nr
      mdim = maxrs
      maxnz = maxrs 
      nrr = nr
      return
      end 
      subroutine rspost (coef,jcoef,wksp,iwksp,nn,rhs,u,ubar,
     a                   p,ip,nrr,irs,ijcrs,ier)
      implicit double precision (a-h, o-z)
c
c ... rspost is the postprocessor for methods using the
c     explicitly-computed reduced system.
c
c ... parameters -- 
c
c       coef      floating point matrix data array
c       jcoef     integer matrix data array
c       n         input integer.  order of the system (= nn)
c       rhs       input vector.  contains the right hand side
c                 of the matrix problem.
c       u         current solution estimate
c       ubar      exact solution vector (if known)
c       nr        order of the reduced system upon input
c       irs       pointer into wksp for reduced system matrix
c       ijcrs     pointer into wksp for reduced system integer
c                  array
c       ier       output integer.  error flag.
c
c ... specifications for parameters
c
      integer jcoef(2), iwksp(1), p(1), ip(1)
      dimension coef(1), rhs(1), u(1), ubar(1), wksp(1)
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / rscons / ndimrs, mdimrs, maxzrs
c
      n = nn
      nr = nrr
      nb = n - nr
c
c ... update constants.
c
      ndim = ndimrs 
      mdim = mdimrs 
      maxnz = maxzrs
      irpnt = irs
      iipnt = ijcrs 
c
c ... compute xb.
c
      call needw ('rspost',0,irpnt,nb,ier)
      if (ier .lt. 0) return
      if (nstore .eq. 1) call rsendp (n,nr,ndim,maxnz,jcoef,
     a                     coef,u,rhs,wksp(irpnt))
      if (nstore .ge. 2) call rsxbd (n,nr,ndim,iwksp(maxnew),
     a                    iwksp(jcnew),coef,u,rhs)
c
c ... unpermute matrix.
c
      call permut (coef,jcoef,p,ip,wksp,iwksp,2,
     a             n,u,ubar,rhs,ier)
      if (ier .lt. 0) return
      return
      end 
      subroutine redblk (ndim,n,maxnz,coef,jcoef,p,ip,nstore,
     a                   iwksp,ier)
      implicit double precision (a-h, o-z)
c
c ... redblk determines if the matrix has property a.
c
c ... parameters -- 
c
c        n        problem size
c        nstore   storage mode
c                  = 1  purdue format
c                  = 2  symmetric diagonal format 
c                  = 3  nonsymmetric diagonal format
c                  = 4  symmetric sparse format
c                  = 5  nonsymmetric sparse format
c        iwksp    integer workspace vector of length n
c        ier      error code
c                  =  0   no errors detected
c                  = -8   matrix does not have property a
c
c ... common blocks 
c
      integer jcoef(2), p(1), ip(1), iwksp(1)
      dimension coef(1)
      logical           propal
c
      go to (5,5,5,10,10), nstore
 5    call prbndx (n,ndim,maxnz,jcoef,coef,p,ip,propal,nstore)
      go to 15
 10   call bicol (n,maxnz,jcoef,jcoef(ndim+1),p,ip,iwksp,propal)
 15   if (propal) ier = 0
      if (.not. propal) ier = -8
      if (propal) return
      call ershow (ier,'redblk')
      return
      end 
      subroutine noadp (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... noadp is a dummy routine to do no adaption. 
c
c ... specifications for parameters
c
c
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      return
      end 
      subroutine copy (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... copy does a vector copy (null preconditioner)
c
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      do 10 i = 1,n 
 10   z(i) = r(i)
      return
      end 
      subroutine split (accel,suba,subat,subq,subqt,subql,subqlt,
     a                  subqr,subqrt,subadp,
     a                  coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... split determines how to apply the splitting based on
c     iqlr.
c
      external accel, suba, subat, subq, subqt, subql, subqlt,
     a         subqr, subqrt, subadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
c *** begin -- package common 
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
c *** end   -- package common 
c
      if (iqlr .eq. 0) then
         call accel (suba,subat,copy,copy,copy,copy,subadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a            iparm,rparm,jer)
      endif
      if (iqlr .eq. 1) then
         call accel (suba,subat,subq,subqt,copy,copy,subadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a            iparm,rparm,jer)
      endif
      if (iqlr .eq. 2) then
         call accel (suba,subat,copy,copy,subq,subqt,subadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a            iparm,rparm,jer)
      endif
      if (iqlr .eq. 3) then
         call accel (suba,subat,subql,subqlt,subqr,subqrt,
     a            subadp,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a            iparm,rparm,jer)
      endif
      if (jer .ne. 0) ier = jer
      return
      end 
      subroutine rich1 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rich1 drives the richardson preconditioner. 
c
      external accel, suba8, suba9, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom4 / srelpr, keyzer, keygs
c
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + n
      call split (accel,suba8,suba9,copy,copy,copy,copy,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,
     a            ier)
      if (keygs .eq. 1) irpnt = irpnt - n
      return
      end 
      subroutine jac1 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... jac1 drives the jacobi preconditioner.
c
      external accel, suba8, suba9, subq1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom4 / srelpr, keyzer, keygs
c
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + n
      call split (accel,suba8,suba9,subq1,subq1,subq1,subq1,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      if (keygs .eq. 1) irpnt = irpnt - n
      return
      end 
      subroutine sor1 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... sor1 drives the point sor method. 
c
      external accel, suba8, suba9, subq78, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call move1 (ndim,mdim,n,maxnz,jcoef,coef,maxt,maxb,ier)
      if (ier .lt. 0) then
         call ershow (ier,'sor1')
         return
      endif
      call split (accel,suba8,suba9,subq78,subq78,subq78,subq78,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine ssor1 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ssor1 drives the point ssor method.
c
      external accel, suba8, suba9, subq79, subq80, subq81, subq82,
     a         subq83, subq84, subq85
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      n = nn
      iwkpt1 = irpnt
      irpnt = irpnt + n
      if (isymm .ne. 0) irpnt = irpnt + n
      call move1 (ndim,mdim,n,maxnz,jcoef,coef,maxt,maxb,ier)
      if (ier .lt. 0) then
         call ershow (ier,'ssor1')
         return
      endif
      call split (accel,suba8,suba9,subq79,subq80,subq81,subq82,
     a            subq83,subq84,subq85, 
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      if (isymm .ne. 0) irpnt = irpnt - n
      irpnt = irpnt - n
      return
      end 
      subroutine ic1 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ic1 drives the ic preconditioner. 
c
      external accel, suba8, suba9, subq86, subq87, subq88, 
     a         subq89, subq90, subq91, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c
      n = nn
      if (ifact .eq. 0 .and. lvfill .gt. 0) go to 20
      call move1 (ndim,mdim,n,maxnz,jcoef,coef,maxt,maxb,ier)
      if (ier .lt. 0) then
         call ershow (ier,'ic1') 
         return
      endif
 20   t1 = timer (dummy)
      if (ifact .eq. 1) call pfact1 (coef,jcoef,wksp,iwksp,n,1,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba8,suba9,subq86,subq87,subq88,subq89,
     a            subq90,subq91,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine mic1 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... mic1 drives the mic preconditioner.
c
      external accel, suba8, suba9, subq86, subq87, subq88, 
     a         subq89, subq90, subq91, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c
      n = nn
      if (ifact .eq. 0 .and. lvfill .gt. 0) go to 20
      call move1 (ndim,mdim,n,maxnz,jcoef,coef,maxt,maxb,ier)
      if (ier .lt. 0) then
         call ershow (ier,'mic1')
         return
      endif
 20   t1 = timer (dummy)
      if (ifact .eq. 1) call pfact1 (coef,jcoef,wksp,iwksp,n,2,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba8,suba9,subq86,subq87,subq88,subq89,
     a            subq90,subq91,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine lsp1 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... lsp1 drives the least squares polynomial preconditioner.
c
      external accel, suba8, suba9, subq92, subq93, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom4 / srelpr, keyzer, keygs
c
      n = nn
      call needw ('lsp1',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      call ainfn (n,ndim,maxnz,jcoef,coef,1,ainf,wksp(irpnt))
      iwkpt2 = irpnt
      irpnt = irpnt + 2*n
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + n
      call split (accel,suba8,suba9,subq92,subq93,subq92,subq93,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      if (keygs .eq. 1) irpnt = irpnt - n
      return
      end 
      subroutine neu1 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... neu1 drives the neumann polynomial preconditioner.
c
      external accel, suba8, suba9, subq94, subq95, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom4 / srelpr, keyzer, keygs
c
      n = nn
      call needw ('neu1',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      iwkpt2 = irpnt
      irpnt = irpnt + n
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + n
      call split (accel,suba8,suba9,subq94,subq95,subq94,subq95,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      if (keygs .eq. 1) irpnt = irpnt - n
      return
      end 
      subroutine rich2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rich2 drives the richardson preconditioner. 
c
      external accel, suba1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      call split (accel,suba1,suba1,copy,copy,copy,copy,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine jac2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... jac2 drives the jacobi preconditioner.
c
      external accel, suba1, subq1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      call split (accel,suba1,suba1,subq1,subq1,subq1,subq1,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine ljac2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ljac2 drives the line jacobi preconditioner.
c
      external accel, suba1, subq2, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq2,subq2,subq2,subq2,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine ljacx2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ljacx2 drives the line jacobi preconditioner.
c
      external accel, suba1, subq4, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call linv (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq4,subq4,subq4,subq4,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine sor2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... sor2 drives the point sor method. 
c
      external accel, suba1, subq6, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call rowise (maxnz,jcoef,irwise)
      call needw ('sor2',1,iipnt,maxnz,ier)
      if (ier .lt. 0) return
      iwkpt1 = iipnt
      iipnt = iipnt + maxnz
      call split (accel,suba1,suba1,subq6,subq6,subq6,subq6,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - maxnz
      return
      end 
      subroutine ssor2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ssor2 drives the point ssor method.
c
      external accel, suba1, subq7, subq8, subq9, subq10,
     a         subq11, subq12 
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call rowise (maxnz,jcoef,irwise)
      call needw ('ssor2',1,iipnt,maxnz,ier)
      if (ier .lt. 0) return
      iwkpt1 = iipnt
      iipnt = iipnt + maxnz
      call split (accel,suba1,suba1,subq7,subq7,subq8,subq9,
     a            subq10,subq11,subq12, 
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - maxnz
      return
      end 
      subroutine ic2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ic2 drives the symmetric ic preconditioner. 
c
      external accel, suba1, subq13, subq14, subq15, subq16,
     a         subq17, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call pfact2 (coef,jcoef,wksp,iwksp,n,1,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      leniw = max (maxnz,nfacti)
      iwkpt1 = iipnt
      iipnt = iipnt + leniw
      call split (accel,suba1,suba1,subq13,subq13,subq14,subq15,
     a            subq16,subq17,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - leniw
      return
      end 
      subroutine mic2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... mic2 drives the symmetric mic preconditioner.
c
      external accel, suba1, subq13, subq14, subq15, subq16,
     a         subq17, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call pfact2 (coef,jcoef,wksp,iwksp,n,2,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      leniw = max (maxnz,nfacti)
      iwkpt1 = iipnt
      iipnt = iipnt + leniw
      call split (accel,suba1,suba1,subq13,subq13,subq14,subq15,
     a            subq16,subq17,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - leniw
      return
      end 
      subroutine lsp2 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... lsp2 drives the least squares polynomial preconditioner.
c
      external accel, suba1, subq18, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      call needw ('lsp2',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      call ainfn (n,ndim,maxnz,jcoef,coef,2,ainf,wksp(irpnt))
      iwkpt1 = irpnt
      irpnt = irpnt + 2*n
      call split (accel,suba1,suba1,subq18,subq18,subq18,subq18,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      return
      end 
      subroutine neu2 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... neu2 drives the neumann polynomial preconditioner.
c
      external accel, suba1, subq19, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      call needw ('neu2',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba1,suba1,subq19,subq19,subq19,subq19,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine lsor2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... lsor2 drives the line sor method. 
c
      external accel, suba1, subq20, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq20,subq20,subq20,subq20,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine lssor2 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... lssor2 drives the line ssor method.
c
      external accel, suba1, subq21, subq22, copy 
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      n = nn
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      iwkpt1 = irpnt
      irpnt = irpnt + n
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq21,subq21,subq21,subq21,
     a            copy,copy,subq22,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine bic2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... bic2 drives the block factorization (version 1) method.
c
      external accel, suba1, subq25, copy, noadp
      external ibfcs1
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacs (1,ibfcs1,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      iwkpt1 = irpnt
      irpnt = irpnt + kblsz
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq25,subq25,subq25,subq25,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - kblsz
      return
      end 
      subroutine mbic2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... mbic2 drives the block factorization (version 1, modified)
c     method.
c
      external accel, suba1, subq25, copy, noadp
      external ibfcs3
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacs (2,ibfcs3,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      iwkpt1 = irpnt
      irpnt = irpnt + kblsz
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq25,subq25,subq25,subq25,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - kblsz
      return
      end 
      subroutine bicx2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... bicx2 drives the block factorization (version 2) method.
c
      external accel, suba1, subq25, copy, noadp
      external ibfcs2
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacs (3,ibfcs2,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      iwkpt1 = irpnt
      irpnt = irpnt + kblsz
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq25,subq25,subq25,subq25,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - kblsz
      return
      end 
      subroutine mbicx2 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... mbicx2 drives the block factorization (version 2, modified)
c     method.
c
      external accel, suba1, subq25, copy, noadp
      external ibfcs4
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacs (4,ibfcs4,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      iwkpt1 = irpnt
      irpnt = irpnt + kblsz
      if (ier .lt. 0) return
      call split (accel,suba1,suba1,subq25,subq25,subq25,subq25,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - kblsz
      return
      end 
      subroutine llsp2 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... llsp2 drives the line least squares polynomial preconditioner.
c
      external accel, suba1, subq23, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      n = nn
      call needw ('llsp2',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call adinfn (n,ndim,maxnz,jcoef,coef,2,ainf,wksp(irpnt))
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call needw ('llsp2',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*n
      call split (accel,suba1,suba1,subq23,subq23,subq23,subq23,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      return
      end 
      subroutine lneu2 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... lneu2 drives the line neumann polynomial preconditioner.
c
      external accel, suba1, subq24, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      n = nn
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call needw ('lneu2',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*n
      call split (accel,suba1,suba1,subq24,subq24,subq24,subq24,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      return
      end 
      subroutine rich3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rich3 drives the richardson preconditioner. 
c
      external accel, suba4, suba5, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      call split (accel,suba4,suba5,copy,copy,copy,copy,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine jac3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... jac3 drives the jacobi preconditioner.
c
      external accel, suba4, suba5, subq1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      call split (accel,suba4,suba5,subq1,subq1,subq1,subq1,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine ljac3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ljac3 drives the line jacobi preconditioner.
c
      external accel, suba4, suba5, subq2, subq3, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba4,suba5,subq2,subq3,subq2,subq3,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine ljacx3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ljacx3 drives the line jacobi preconditioner.
c
      external accel, suba4, suba5, subq4, subq5, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call linv (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba4,suba5,subq4,subq5,subq4,subq5,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine sor3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... sor3 drives the point sor method. 
c
      external accel, suba4, suba5, subq40, noadp, copy
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call rowise (maxnz,jcoef,irwise)
      call needw ('sor3',1,iipnt,maxnz,ier)
      if (ier .lt. 0) return
      call needw ('sor3',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      call move2 (ndim,n,maxnz,jcoef,coef,wksp(irpnt),
     a            iwksp(iipnt),maxt,maxb)
      iwkpt1 = iipnt
      iipnt = iipnt + maxnz
      call split (accel,suba4,suba5,subq40,subq40,subq40,subq40,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - maxnz
      return
      end 
      subroutine ssor3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ssor3 drives the point ssor method.
c
      external accel, suba4, suba5, subq41, subq42, subq43, subq44,
     a         subq45, subq46, subq47
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      call rowise (maxnz,jcoef,irwise)
      call needw ('ssor3',1,iipnt,maxnz,ier)
      if (ier .lt. 0) return
      call needw ('ssor3',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call move2 (ndim,n,maxnz,jcoef,coef,wksp(irpnt),
     a            iwksp(iipnt),maxt,maxb)
      iwkpt1 = irpnt
      irpnt = irpnt + n
      iwkpt2 = iipnt
      iipnt = iipnt + maxnz
      call split (accel,suba4,suba5,subq41,subq42,subq43,subq44,
     a            subq45,subq46,subq47, 
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      iipnt = iipnt - maxnz
      return
      end 
      subroutine ic3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ic3 drives the nonsymmetric ic preconditioner.
c
      external accel, suba4, suba5, subq48, subq49, subq50, 
     a         subq51, subq52, subq53, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
c
      n = nn
      call needw ('ic3',1,iipnt,maxnz,ier)
      if (ier .lt. 0) return
      call needw ('ic3',0,irpnt,n,ier)
      if (ier .lt. 0) return
      if (ifact .eq. 0 .and. lvfill .gt. 0) go to 20
      call move2 (ndim,n,maxnz,jcoef,coef,wksp(irpnt),
     a            iwksp(iipnt),maxt,maxb)
 20   t1 = timer (dummy)
      if (ifact .eq. 1) call pfact3 (coef,jcoef,wksp,iwksp,n,1,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      leniw = max (maxnz,nfacti)
      iwkpt1 = iipnt
      iipnt = iipnt + leniw
      call split (accel,suba4,suba5,subq48,subq49,subq50,subq51,
     a            subq52,subq53,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - leniw
      return
      end 
      subroutine mic3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... mic3 drives the nonsymmetric mic preconditioner.
c
      external accel, suba4, suba5, subq48, subq49, subq50, 
     a         subq51, subq52, subq53, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
c
      n = nn
      call needw ('mic3',1,iipnt,maxnz,ier)
      if (ier .lt. 0) return
      call needw ('mic3',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      if (ifact .eq. 0 .and. lvfill .gt. 0) go to 20
      call move2 (ndim,n,maxnz,jcoef,coef,wksp(irpnt),
     a            iwksp(iipnt),maxt,maxb)
 20   t1 = timer (dummy)
      if (ifact .eq. 1) call pfact3 (coef,jcoef,wksp,iwksp,n,2,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      leniw = max (maxnz,nfacti)
      iwkpt1 = iipnt
      iipnt = iipnt + leniw
      call split (accel,suba4,suba5,subq48,subq49,subq50,subq51,
     a            subq52,subq53,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      iipnt = iipnt - leniw
      return
      end 
      subroutine lsp3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... lsp3 drives the least squares polynomial preconditioner.
c
      external accel, suba4, suba5, subq54, subq55, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      call needw ('lsp3',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      call ainfn (n,ndim,maxnz,jcoef,coef,3,ainf,wksp(irpnt))
      iwkpt1 = irpnt
      irpnt = irpnt + 2*n
      call split (accel,suba4,suba5,subq54,subq55,subq54,subq55,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      return
      end 
      subroutine neu3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... neu3 drives the neumann polynomial preconditioner.
c
      external accel, suba4, suba5, subq56, subq57, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      call needw ('neu3',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba4,suba5,subq56,subq57,subq56,subq57,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine lsor3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... lsor3 drives the line sor method. 
c
      external accel, suba4, suba5, subq58, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba4,suba5,subq58,subq58,subq58,subq58,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine lssor3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... lssor3 drives the line ssor method.
c
      external accel, suba4, suba5, subq59, subq60, subq61, 
     a         subq62, subq63, subq64, subq65
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      n = nn
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      iwkpt1 = irpnt
      irpnt = irpnt + n
      if (ier .lt. 0) return
      call split (accel,suba4,suba5,subq59,subq60,subq61,subq62,
     a            subq63,subq64,subq65, 
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine bic3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... bic3 drives the block factorization (version 1) method.
c
      external accel, suba4, suba5, subq70, subq71, subq72, 
     a         subq73, subq74, subq75, noadp
      external ibfcn1
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmz (1,ibfcn1,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*kblsz 
      call split (accel,suba4,suba5,subq70,subq71,subq72,subq73,
     a            subq74,subq75,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*kblsz 
      return
      end 
      subroutine mbic3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... mbic3 drives the block factorization (version 1, modified)
c     method.
c
      external accel, suba4, suba5, subq70, subq71, subq72, 
     a         subq73, subq74, subq75, noadp
      external ibfcn3
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmz (2,ibfcn3,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*kblsz 
      call split (accel,suba4,suba5,subq70,subq71,subq72,subq73,
     a            subq74,subq75,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*kblsz 
      return
      end 
      subroutine bicx3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... bicx3 drives the block factorization (version 2)
c     method.
c
      external accel, suba4, suba5, subq70, subq71, subq72, 
     a         subq73, subq74, subq75, noadp
      external ibfcn2
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmz (3,ibfcn2,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*kblsz 
      call split (accel,suba4,suba5,subq70,subq71,subq72,subq73,
     a            subq74,subq75,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*kblsz 
      return
      end 
      subroutine mbicx3 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... mbicx3 drives the block factorization (version 2, modified)
c     method.
c
      external accel, suba4, suba5, subq70, subq71, subq72, 
     a         subq73, subq74, subq75, noadp
      external ibfcn4
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      call blkdef (coef,jcoef,wksp,iwksp,n,ier)
      if (ier .lt. 0) return
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmz (4,ibfcn4,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*kblsz 
      call split (accel,suba4,suba5,subq70,subq71,subq72,subq73,
     a            subq74,subq75,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*kblsz 
      return
      end 
      subroutine llsp3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... llsp3 drives the line least squares polynomial preconditioner.
c
      external accel, suba4, suba5, subq66, subq67, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      n = nn
      call needw ('llsp3',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call adinfn (n,ndim,maxnz,jcoef,coef,3,ainf,wksp(irpnt))
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call needw ('llsp3',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*n
      call split (accel,suba4,suba5,subq66,subq67,subq66,subq67,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      return
      end 
      subroutine lneu3 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... lneu3 drives the line neumann polynomial preconditioner.
c
      external accel, suba4, suba5, subq68, subq69, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      n = nn
      t1 = timer (dummy)
      if (ifact .eq. 1) call lfact (coef,jcoef,wksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call needw ('lneu3',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*n
      call split (accel,suba4,suba5,subq68,subq69,subq68,subq69,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      return
      end 
      subroutine rich4 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rich4 drives the richardson preconditioner. 
c
      external accel, suba12, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom4 / srelpr, keyzer, keygs
c
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba12,suba12,copy,copy,copy,copy,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine jac4 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... jac4 drives the jacobi preconditioner.
c
      external accel, suba12, subq1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom4 / srelpr, keyzer, keygs
c
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba12,suba12,subq1,subq1,subq1,subq1,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine lsp4 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... lsp4 drives the least squares polynomial preconditioner.
c
      external accel, suba12, sub110, copy, noadp 
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom4 / srelpr, keyzer, keygs
c
      n = nn
      call needw ('lsp4',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      call ainfn (n,ndim,maxnz,jcoef,coef,4,ainf,wksp(irpnt))
      iwkpt2 = irpnt
      irpnt = irpnt + 2*n
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba12,suba12,sub110,sub110,sub110,sub110,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine neu4 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... neu4 drives the neumann polynomial preconditioner.
c
      external accel, suba12, sub111, copy, noadp 
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom4 / srelpr, keyzer, keygs
c
      n = nn
      call needw ('neu4',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      iwkpt2 = irpnt
      irpnt = irpnt + n
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba12,suba12,sub111,sub111,sub111,sub111,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine rich5 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rich5 drives the richardson preconditioner. 
c
      external accel, suba13, suba14, copy, noadp 
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom4 / srelpr, keyzer, keygs
c
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba13,suba14,copy,copy,copy,copy,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine jac5 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... jac5 drives the jacobi preconditioner.
c
      external accel, suba13, suba14, subq1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom4 / srelpr, keyzer, keygs
c
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba13,suba14,subq1,subq1,subq1,subq1,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine lsp5 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... lsp5 drives the least squares polynomial preconditioner.
c
      external accel, suba13, suba14, sub112, sub113, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom8 / ainf
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom4 / srelpr, keyzer, keygs
c
      n = nn
      call needw ('lsp5',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      call ainfn (n,ndim,maxnz,jcoef,coef,5,ainf,wksp(irpnt))
      iwkpt2 = irpnt
      irpnt = irpnt + 2*n
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba13,suba14,sub112,sub113,sub112,sub113,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*n
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine neu5 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... neu5 drives the neumann polynomial preconditioner.
c
      external accel, suba13, suba14, sub114, sub115, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom4 / srelpr, keyzer, keygs
c
      n = nn
      call needw ('neu5',0,irpnt,n,ier) 
      if (ier .lt. 0) return
      iwkpt2 = irpnt
      irpnt = irpnt + n
      iwkpt1 = irpnt
      if (keygs .eq. 1) irpnt = irpnt + 2*n
      call split (accel,suba13,suba14,sub114,sub115,sub114,sub115,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      if (keygs .eq. 1) irpnt = irpnt - 2*n
      return
      end 
      subroutine sor6 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... sor6 drives the multi-color sor method.
c
      external accel, suba8, subq96, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba8,suba8,subq96,subq96,subq96,subq96,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine ssor6 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ssor6 drives the multi-color ssor method.
c
      external accel, suba8, suba9, subq97, subq98, subq99, sub100,
     a         sub101, sub102, sub103
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
c
      iwkpt1 = irpnt
      irpnt = irpnt + n + ncmax
      call split (accel,suba8,suba9,subq97,subq98,subq99,sub100,
     a            sub101,sub102,sub103, 
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n - ncmax
      return
      end 
      subroutine ic6 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ic6 drives the ic preconditioner. 
c     (multi-color ordering)
c
      external accel, suba8, suba9, sub104, sub105, sub106, 
     a         sub107, sub108, sub109, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c
      n = nn
      t1 = timer (dummy)
      if (ifact .eq. 1) call pfactc (coef,jcoef,wksp,iwksp,n,1,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba8,suba9,sub104,sub105,sub106,sub107,
     a            sub108,sub109,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine mic6 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... mic6 drives the mic preconditioner.
c     (multi-color ordering)
c
      external accel, suba8, suba9, sub104, sub105, sub106, 
     a         sub107, sub108, sub109, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
c
      n = nn
      t1 = timer (dummy)
      if (ifact .eq. 1) call pfactc (coef,jcoef,wksp,iwksp,n,2,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba8,suba9,sub104,sub105,sub106,sub107,
     a            sub108,sub109,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine rs6 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rs6 drives the reduced system method (purdue storage
c     with red-black coloring).
c
      external accel, suba10, suba11, subq1, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
c
c ... compute red-black rhs.
c
      nr = iwksp(nc)
      nb = n - nr
      call needw ('rs6',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      irhs = irpnt
      irpnt = irpnt + nr
      call vfill (2*n,wksp(irhs),0.0d0)
      call rsbegp (n,nr,ndim,maxnz,jcoef,coef,wksp(irhs),rhs,
     a             wksp(irpnt))
      iwkpt1 = irpnt
      irpnt = irpnt + n + nb
      call split (accel,suba10,suba11,subq1,subq1,subq1,subq1,
     a            copy,copy,noadp,
     a            coef,jcoef,nr,u,ubar,wksp(irhs),wksp,iwksp,
     a            iparm,rparm,ier)
      call rsendp (n,nr,ndim,maxnz,jcoef,coef,u,rhs,wksp(iwkpt1))
      irpnt = irpnt - 2*n
      return
      end 
      subroutine sor7 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier) 
      implicit double precision (a-h, o-z)
c
c ... sor7 drives the multi-color sor method.
c
      external accel, suba2, subq26, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call mfact (coef,jcoef,wksp,iwksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      call split (accel,suba2,suba2,subq26,subq26,subq26,subq26,
     a            copy,copy,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      return
      end 
      subroutine ssor7 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... ssor7 drives the multi-color ssor method.
c
      external accel, suba2, suba3, subq27, subq28, subq29, 
     a         subq30, subq31, subq32, subq33
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      n = nn
      t1 = timer (dummy)
      if (ifact .eq. 1) call mfact (coef,jcoef,wksp,iwksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + n
      call split (accel,suba2,suba3,subq27,subq28,subq29,subq30,
     a            subq31,subq32,subq33, 
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - n
      return
      end 
      subroutine bic7 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... bic7 drives the block factorization (version 1) method.
c     (multi-color ordering)
c
      external accel, suba2, suba3, subq34, subq35, subq36, 
     a         subq37, subq38, subq39, noadp
      external ibfcn1
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmy (1,ibfcn1,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*ncmax 
      call split (accel,suba2,suba3,subq34,subq35,subq36,subq37,
     a            subq38,subq39,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*ncmax 
      return
      end 
      subroutine mbic7 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... mbic7 drives the block factorization (version 1, modified)
c     method (multi-color ordering)
c
      external accel, suba2, suba3, subq34, subq35, subq36, 
     a         subq37, subq38, subq39, noadp
      external ibfcn3
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmy (2,ibfcn3,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*ncmax 
      call split (accel,suba2,suba3,subq34,subq35,subq36,subq37,
     a            subq38,subq39,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*ncmax 
      return
      end 
      subroutine bicx7 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                  iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... bicx7 drives the block factorization (version 2)
c     method (multi-color ordering)
c
      external accel, suba2, suba3, subq34, subq35, subq36, 
     a         subq37, subq38, subq39, noadp
      external ibfcn2
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmy (3,ibfcn2,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*ncmax 
      call split (accel,suba2,suba3,subq34,subq35,subq36,subq37,
     a            subq38,subq39,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*ncmax 
      return
      end 
      subroutine mbicx7 (accel,coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                   iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... mbicx7 drives the block factorization (version 2, modified)
c     method (multi-color ordering)
c
      external accel, suba2, suba3, subq34, subq35, subq36, 
     a         subq37, subq38, subq39, noadp
      external ibfcn4
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
c
      t1 = timer (dummy)
      if (ifact .eq. 1) call bfacmy (4,ibfcn4,coef,jcoef,wksp,iwksp,
     a                              n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
      iwkpt1 = irpnt
      irpnt = irpnt + 2*ncmax 
      call split (accel,suba2,suba3,subq34,subq35,subq36,subq37,
     a            subq38,subq39,noadp,
     a            coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier) 
      irpnt = irpnt - 2*ncmax 
      return
      end 
      subroutine rs7 (accel,coef,jcoef,nn,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c ... rs7 drives the reduced system method (diagonal storage
c     with red-black coloring).
c
      external accel, suba6, suba7, subq76, subq77, copy, noadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
c
      n = nn
      t1 = timer (dummy)
      if (ifact .eq. 1) call mfact (coef,jcoef,wksp,iwksp,n,ier)
      t2 = timer (dummy)
      timfac = t2 - t1
      if (ier .lt. 0) return
c
c ... compute red-black rhs.
c
      nr = iwksp(nc)
      nb = n - nr
      call needw ('rs7',0,irpnt,n,ier)
      if (ier .lt. 0) return
      irhs = irpnt
      irpnt = irpnt + nr
      call rsbegd (n,n,nr,ndim,iwksp(maxnew),ndt,ndb,iwksp(jcnew),
     a             coef,wksp(irhs),rhs,wksp(ifactr),wksp(irpnt))
      iwkpt1 = irpnt
      irpnt = irpnt + nb
      call split (accel,suba6,suba7,subq76,subq77,subq76,subq77,
     a            copy,copy,noadp,
     a            coef,jcoef,nr,u,ubar,wksp(irhs),wksp,iwksp,
     a            iparm,rparm,ier)
      call rsendd (n,n,nr,ndim,iwksp(maxnew),ndt,ndb,iwksp(jcnew),
     a             coef,u,rhs,wksp(ifactr))
      irpnt = irpnt - n
      return
      end 
      subroutine suba1 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba1 calls mult2s.
c
      common / dscons / ndim, mdim, maxnz
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mult2s (ndim,maxnz,coef,jcoef,n,x,y)
      return
      end 
      subroutine suba2 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba2 calls muldc.
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call muldc (ndim,n,coef,ncolor,iwksp(nc),iwksp(maxnew),
     a            iwksp(jcnew),x,y)
      return
      end 
      subroutine suba3 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba3 calls muldct.
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call muldct (ndim,n,coef,ncolor,iwksp(nc),iwksp(maxnew),
     a             iwksp(jcnew),x,y)
      return
      end 
      subroutine suba4 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba4 calls mult2n.
c
      common / dscons / ndim, mdim, maxnz
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mult2n (ndim,maxnz,coef,jcoef,n,x,y)
      return
      end 
      subroutine suba5 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba5 calls mul2nt.
c
      common / dscons / ndim, mdim, maxnz
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mul2nt (ndim,maxnz,coef,jcoef,n,x,y)
      return
      end 
      subroutine suba6 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba6 calls rsad.
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      nbig = nr + nb
      call rsad (nbig,n,n,ndim,iwksp(maxnew),ndt,ndb,
     a           iwksp(jcnew),coef,y,x,wksp(ifactr),wksp(iwkpt1))
      return
      end 
      subroutine suba7 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba7 calls rsatd.
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      nbig = nr + nb
      call rsatd (nbig,n,n,ndim,iwksp(maxnew),ndt,ndb,
     a           iwksp(jcnew),coef,y,x,wksp(ifactr),wksp(iwkpt1))
      return
      end 
      subroutine suba8 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba8 calls mult1.
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mult1 (ndim,maxnz,coef,jcoef,wksp(iwkpt1),n,x,y) 
      return
      end 
      subroutine suba9 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba9 calls mul1t.
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mul1t (ndim,maxnz,coef,jcoef,wksp(iwkpt1),n,x,y) 
      return
      end 
      subroutine suba10 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba10 calls rsap.
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      nbig = nr + nb
      call rsap (ndim,nbig,n,maxnz,jcoef,coef,x,y,wksp(iwkpt1))
      return
      end 
      subroutine suba11 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba11 calls rsatp.
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      nbig = nr + nb
      if (isymm .eq. 0) call rsap (ndim,nbig,n,maxnz,jcoef,coef,
     a                                   x,y,wksp(iwkpt1))
      if (isymm .eq. 1) call rsatp (ndim,nbig,n,maxnz,jcoef,coef,
     a                                   x,y,wksp(iwkpt1))
      return
      end 
      subroutine suba12 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba12 calls mult3.
c
      common / dscons / ndim, mdim, maxnz
      common / cmpart / mpstrt, mpart
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mult3 (mpart,iwksp(mpstrt),coef,jcoef,jcoef(ndim+1),
     a            wksp(iwkpt1),x,y)
      return
      end 
      subroutine suba13 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba13 calls mult3n.
c
      common / dscons / ndim, mdim, maxnz
      common / cmpart / mpstrt, mpart
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mult3n (mpart,iwksp(mpstrt),coef,jcoef,jcoef(ndim+1),
     a             wksp(iwkpt1),x,y)
      return
      end 
      subroutine suba14 (coef,jcoef,wksp,iwksp,n,x,y)
      implicit double precision (a-h, o-z)
c
c ... suba14 calls mul3nt.
c
      common / dscons / ndim, mdim, maxnz
      common / cmpart / mpstrt, mpart
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension x(1), y(1), coef(1), wksp(1)
c
      call mul3nt (mpart,iwksp(mpstrt),coef,jcoef,jcoef(ndim+1),
     a             wksp(iwkpt1),x,y)
      return
      end 
      subroutine subq1 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq1 calls pjac, for jacobi preconditioning.
c
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      call pjac (coef,n,r,z)
      return
      end 
      subroutine subq2 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq2 calls bdsol, for line jacobi preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (nstore .eq. 2) isym = 0
      if (nstore .eq. 3) isym = 1
      call bdsol (n,n,kblsz,ndt,ndb,wksp(ifactr),r,z,isym)
      return
      end 
      subroutine subq3 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq3 calls bdsolt, for line jacobi preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      call bdsolt (n,n,kblsz,ndt,ndb,wksp(ifactr),r,z)
      return
      end 
      subroutine subq4 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq4 call bmul or bmuln, for line jacobi preconditioning
c     (approximate inverse)
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (nstore .eq. 2) isym = 0
      if (nstore .eq. 3) isym = 1
      ift = ifactr + n
      ifb = ifactr + n*(ndt + 1)
      if (isym .eq. 0) call bmul (n,n,ndt,wksp(ifactr),wksp(ift),r,z) 
      if (isym .eq. 1) call bmuln (n,n,ndt,ndb,wksp(ifactr),
     a                             wksp(ift),wksp(ifb),r,z) 
      return
      end 
      subroutine subq5 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq5 call bmul or bmulnt, for line jacobi preconditioning
c     (approximate inverse)
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (nstore .eq. 2) isym = 0
      if (nstore .eq. 3) isym = 1
      ift = ifactr + n
      ifb = ifactr + n*(ndt + 1)
      if (isym .eq. 0) call bmul (n,n,ndt,wksp(ifactr),wksp(ift),r,z) 
      if (isym .eq. 1) call bmulnt (n,n,ndt,ndb,wksp(ifactr),
     a                             wksp(ift),wksp(ifb),r,z) 
      return
      end 
      subroutine subq6 (coef,jcoef,wksp,iwksp,n,u,rhs,unew) 
      implicit double precision (a-h, o-z)
c
c ... subq6 calls the basic sor iterative step
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call sords (ndim,n,maxt,jcoef(2),coef,coef(ndim+1),omega,
     a            irwise,u,rhs,unew,iwksp(iwkpt1))
      return
      end 
      subroutine subq7 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq7 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call srs (ndim,n,maxt,jcoef(2),coef,coef(ndim+1),omega,
     a          irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq8 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq8 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call srs1 (ndim,n,maxt,jcoef(2),coef,coef(ndim+1),omega,
     a          irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq9 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq9 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call srs3 (ndim,n,maxt,jcoef(2),coef,coef(ndim+1),omega,
     a          irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq10 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq10 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call srs2 (ndim,n,maxt,jcoef(2),coef,coef(ndim+1),omega,
     a          irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq11 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq11 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call srs4 (ndim,n,maxt,jcoef(2),coef,coef(ndim+1),omega,
     a          irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq12 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... subq12 calls the ssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      maxt = maxnz - 1
      call ssord (ndim,maxt,jcoef(2),coef,coef(ndim+1),n,p,r,
     a            pdp,pldup)
      return
      end 
      subroutine subq13 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq13 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call ics (ndim,n,maxt,jcoef(2),wksp(ifactr),
     a                     coef(ndim+1),1,irwise,iwksp(iwkpt1),r,z)
      if (.not. propa) call ics (n,n,maxt,iwksp(ifacti+1),
     a                           wksp(ifactr),wksp(ifactr+n),
     a                           0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq14 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq14 calls ics1, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call ics1 (ndim,n,maxt,jcoef(2),wksp(ifactr),
     a                     coef(ndim+1),1,irwise,iwksp(iwkpt1),r,z)
      if (.not. propa) call ics1 (n,n,maxt,iwksp(ifacti+1), 
     a                           wksp(ifactr),wksp(ifactr+n),
     a                           0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq15 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq15 calls ics3, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call ics3 (ndim,n,maxt,jcoef(2),wksp(ifactr),
     a                     coef(ndim+1),1,irwise,iwksp(iwkpt1),r,z)
      if (.not. propa) call ics3 (n,n,maxt,iwksp(ifacti+1), 
     a                           wksp(ifactr),wksp(ifactr+n),
     a                           0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq16 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq16 calls ics2, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call ics2 (ndim,n,maxt,jcoef(2),wksp(ifactr),
     a                     coef(ndim+1),1,irwise,iwksp(iwkpt1),r,z)
      if (.not. propa) call ics2 (n,n,maxt,iwksp(ifacti+1), 
     a                           wksp(ifactr),wksp(ifactr+n),
     a                           0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq17 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq17 calls ics4, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call ics4 (ndim,n,maxt,jcoef(2),wksp(ifactr),
     a                     coef(ndim+1),1,irwise,iwksp(iwkpt1),r,z)
      if (.not. propa) call ics4 (n,n,maxt,iwksp(ifacti+1), 
     a                           wksp(ifactr),wksp(ifactr+n),
     a                           0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq18 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq18 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba1
c
      call ppii (suba1,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt1),n,r,z) 
      return
      end 
      subroutine subq19 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq19 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba1
c
      call pneu (suba1,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq20 (coef,jcoef,wksp,iwksp,n,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... subq20 calls the basic lsor iterative step
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      call sordb (n,ndim,kblsz,kblsz,iwksp(ifacti),lbhb,
     a            wksp(ifactr),coef,jcoef,n,omega,u,rhs,unew)
      return
      end 
      subroutine subq21 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq21 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim*iwksp(ifacti+2) + 1
      ipt2 = iwksp(ifacti+2) + 1
      call sbsl (n,ndim,n,kblsz,kblsz,lbhb,iwksp(ifacti),
     a           wksp(ifactr),coef(ipt1),jcoef(ipt2),r,z,
     a           omega,wksp(iwkpt1))
      return
      end 
      subroutine subq22 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... subq22 calls the lssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      call ssrcd (n,ndim,maxnz,kblsz,iwksp(ifacti),wksp(ifactr),
     a            coef,jcoef,n,p,r,wksp(iwkpt1),pdp,pldup)
      return
      end 
      subroutine subq23 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq23 calls pbpii, for line lspoly preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba1, subq2
c
      call pbpii (suba1,subq2,coef,jcoef,wksp,iwksp,ainf,
     a            0.0d0,0.0d0,ndeg,wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq24 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq24 calls pbneu, for line neumann polynomial
c     preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba1, subq2
c
      call pbneu (suba1,subq2,coef,jcoef,wksp,iwksp,ndeg,
     a            wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq25 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq25 calls ibsl, for bic preconditioning. 
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      ipt2 = ifactr + n*iwksp(ifacti+2) 
      if (lvfill .gt. 0) go to 10
      nwdiag = iwksp(ifacti+2) - ltrunc 
      if (propa) call ibsl
     a           (n,ndim,n,kblsz,kblsz,lbhb,iwksp(ifacti),
     a            wksp(ifactr),coef(ndim*nwdiag+1),
     a            jcoef(nwdiag+1),r,z,ivers,wksp(iwkpt1))
      if (.not. propa) call ibsl
     a           (n,n,n,kblsz,kblsz,lbhb,iwksp(ifacti),
     a            wksp(ifactr),wksp(ipt2),
     a            jcoef(nwdiag+1),r,z,ivers,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + iwksp(ifacti+2)
      call ibsl (n,n,n,kblsz,kblsz,lbhb,iwksp(ifacti),
     a           wksp(ifactr),wksp(ipt2),iwksp(ipt1),r,z,
     a           ivers,wksp(iwkpt1))
      return
      end 
      subroutine subq26 (coef,jcoef,wksp,iwksp,n,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... subq26 calls the basic multi-color sor iterative step 
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      call sordmb (n,ndim,n,iwksp(iblock),iwksp(lbhb),ncolor,
     a             iwksp(nc),iwksp(ipt),wksp(ifactr),coef,
     a             iwksp(jcnew),n,omega,u,rhs,unew)
      return
      end 
      subroutine subq27 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq27 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
      ipt1 = ndim*nwdiag + 1
      ipt2 = ncolor*nwdiag + jcnew
      call sbsln (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ipt1),iwksp(ipt2),r,z,omega,0,wksp(iwkpt1))
      return
      end 
      subroutine subq28 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq28 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
      ipt1 = ndim*nwdiag + 1
      ipt2 = ncolor*nwdiag + jcnew
      call sbslnt (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ipt1),iwksp(ipt2),r,z,omega,0,wksp(iwkpt1))
      return
      end 
      subroutine subq29 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq29 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
      ipt1 = ndim*nwdiag + 1
      ipt2 = ncolor*nwdiag + jcnew
      call sbsln1 (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ipt1),iwksp(ipt2),r,z,omega,0)
      return
      end 
      subroutine subq30 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq30 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
      ipt1 = ndim*nwdiag + 1
      ipt2 = ncolor*nwdiag + jcnew
      call sbsln3 (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ipt1),iwksp(ipt2),r,z,omega,0)
      return
      end 
      subroutine subq31 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq31 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
      ipt1 = ndim*nwdiag + 1
      ipt2 = ncolor*nwdiag + jcnew
      call sbsln2 (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ipt1),iwksp(ipt2),r,z,omega,0,wksp(iwkpt1))
      return
      end 
      subroutine subq32 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq32 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
      ipt1 = ndim*nwdiag + 1
      ipt2 = ncolor*nwdiag + jcnew
      call sbsln4 (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ipt1),iwksp(ipt2),r,z,omega,0,wksp(iwkpt1))
      return
      end 
      subroutine subq33 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... subq33 calls the mssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      call ssrcdm (n,ndim,iwksp(lbhb),n,ncolor,iwksp(nc),
     a             iwksp(ipt),iwksp(iblock),wksp(ifactr),
     a             coef,iwksp(jcnew),n,p,r,wksp(iwkpt1),
     a             pdp,pldup) 
      return
      end 
      subroutine subq34 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq34 calls ibsln, for multi-color bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
     a         - 2*ltrunc
      if (propa) call ibsln
     a           (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ndim*nwdiag+1),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      if (.not. propa) call ibsln
     a           (n,n,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            wksp(iwkpt2),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      return
      end 
      subroutine subq35 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq35 calls ibslnt, for multi-color bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
     a         - 2*ltrunc
      if (propa) call ibslnt
     a           (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ndim*nwdiag+1),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      if (.not. propa) call ibslnt
     a           (n,n,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            wksp(iwkpt2),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      return
      end 
      subroutine subq36 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq36 calls ibsln1, for multi-color bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
     a         - 2*ltrunc
      if (propa) call ibsln1
     a           (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ndim*nwdiag+1),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      if (.not. propa) call ibsln1
     a           (n,n,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            wksp(iwkpt2),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      return
      end 
      subroutine subq37 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq37 calls ibsln3, for multi-color bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
     a         - 2*ltrunc
      if (propa) call ibsln3
     a           (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ndim*nwdiag+1),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      if (.not. propa) call ibsln3
     a           (n,n,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            wksp(iwkpt2),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      return
      end 
      subroutine subq38 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq38 calls ibsln2, for multi-color bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
     a         - 2*ltrunc
      if (propa) call ibsln2
     a           (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ndim*nwdiag+1),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      if (.not. propa) call ibsln2
     a           (n,n,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            wksp(iwkpt2),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      return
      end 
      subroutine subq39 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq39 calls ibsln4, for multi-color bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nwdiag = iwksp(iblock+2) + iwksp(iblock+3*ncolor+2)
     a         - 2*ltrunc
      if (propa) call ibsln4
     a           (n,ndim,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            coef(ndim*nwdiag+1),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      if (.not. propa) call ibsln4
     a           (n,n,n,n,ncolor,iwksp(nc),iwksp(ipt),
     a            iwksp(lbhb),iwksp(iblock),wksp(ifactr),
     a            wksp(iwkpt2),iwksp(jcnew+nwdiag*ncolor),
     a            r,z,ivers,0,wksp(iwkpt1))
      return
      end 
      subroutine subq40 (coef,jcoef,wksp,iwksp,n,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... subq40 calls the basic sor iterative step
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      maxtp1 = maxt + 1
      call sordn (ndim,n,maxt,maxb,jcoef(2),jcoef(maxt+2),coef,
     a            coef(ndim+1),coef(maxtp1*ndim+1),omega,
     a            irwise,u,rhs,unew,iwksp(iwkpt1))
      return
      end 
      subroutine subq41 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq41 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxtp1 = maxt + 1
      call srsn (ndim,n,maxt,maxb,jcoef(2),jcoef(maxt+2),coef,
     a           coef(ndim+1),coef(ndim*maxtp1+1),omega,irwise,
     a           iwksp(iwkpt2),r,z)
      return
      end 
      subroutine subq42 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq42 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxtp1 = maxt + 1
      call srsnt (ndim,n,maxt,maxb,jcoef(2),jcoef(maxt+2),coef,
     a           coef(ndim+1),coef(ndim*maxtp1+1),omega,irwise,
     a           iwksp(iwkpt2),r,z)
      return
      end 
      subroutine subq43 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq43 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxtp1 = maxt + 1
      call srsn1 (ndim,n,maxb,jcoef(maxt+2),coef, 
     a           coef(ndim*maxtp1+1),omega,irwise,
     a           iwksp(iwkpt2),r,z)
      return
      end 
      subroutine subq44 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq44 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      maxtp1 = maxt + 1
      call srsn3 (ndim,n,maxb,jcoef(maxt+2),coef, 
     a           coef(ndim*maxtp1+1),omega,irwise,
     a           iwksp(iwkpt2),r,z)
      return
      end 
      subroutine subq45 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq45 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      call srsn2 (ndim,n,maxt,jcoef(2),coef,
     a           coef(ndim+1),omega,irwise,
     a           iwksp(iwkpt2),r,z)
      return
      end 
      subroutine subq46 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq46 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      call srsn4 (ndim,n,maxt,jcoef(2),coef,
     a           coef(ndim+1),omega,irwise,
     a           iwksp(iwkpt2),r,z)
      return
      end 
      subroutine subq47 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... subq47 calls the ssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      maxtp1 = maxt + 1
      call ssordn (ndim,maxt,maxb,jcoef(2),jcoef(maxt+2),coef,
     a             coef(ndim+1),coef(ndim*maxtp1+1),n,p,r,
     a             wksp(iwkpt1),pdp,pldup)
      return
      end 
      subroutine subq48 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq48 calls icsn, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      maxtp1 = maxt + 1
      if (propa) call icsn (ndim,n,maxt,maxb,jcoef(2),jcoef(maxt+2),
     a                      wksp(ifactr),coef(ndim+1),
     a                      coef(ndim*maxtp1+1),1,irwise,
     a                      iwksp(iwkpt1),r,z)
      if (.not. propa) call icsn (n,n,maxt,maxb,iwksp(ifacti+1),
     a                 iwksp(ifacti+maxt+1),wksp(ifactr),
     a                 wksp(ifactr+n),wksp(ifactr+n*maxtp1),
     a                 0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq49 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq49 calls icsnt, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      maxtp1 = maxt + 1
      if (propa) call icsnt (ndim,n,maxt,maxb,jcoef(2),jcoef(maxt+2), 
     a                      wksp(ifactr),coef(ndim+1),
     a                      coef(ndim*maxtp1+1),1,irwise,
     a                      iwksp(iwkpt1),r,z)
      if (.not. propa) call icsnt (n,n,maxt,maxb,iwksp(ifacti+1),
     a                 iwksp(ifacti+maxt+1),wksp(ifactr),
     a                 wksp(ifactr+n),wksp(ifactr+n*maxtp1),
     a                 0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq50 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq50 calls icsn1, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      maxtp1 = maxt + 1
      if (propa) call icsn1 (ndim,n,maxb,jcoef(maxt+2),
     a                      wksp(ifactr),
     a                      coef(ndim*maxtp1+1),1,irwise,
     a                      iwksp(iwkpt1),r,z)
      if (.not. propa) call icsn1 (n,n,maxb,
     a                 iwksp(ifacti+maxt+1),wksp(ifactr),
     a                 wksp(ifactr+n*maxtp1),
     a                 0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq51 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq51 calls icsn3, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      maxtp1 = maxt + 1
      if (propa) call icsn3 (ndim,n,maxb,jcoef(maxt+2),
     a                      wksp(ifactr),
     a                      coef(ndim*maxtp1+1),1,irwise,
     a                      iwksp(iwkpt1),r,z)
      if (.not. propa) call icsn3 (n,n,maxb,
     a                 iwksp(ifacti+maxt+1),wksp(ifactr),
     a                 wksp(ifactr+n*maxtp1),
     a                 0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq52 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq52 calls icsn2, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call icsn2 (ndim,n,maxt,jcoef(2),
     a                      wksp(ifactr),coef(ndim+1),
     a                      1,irwise,
     a                      iwksp(iwkpt1),r,z)
      if (.not. propa) call icsn2 (n,n,maxt,iwksp(ifacti+1),
     a                 wksp(ifactr),
     a                 wksp(ifactr+n),
     a                 0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq53 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq53 calls icsn4, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      if (propa) call icsn4 (ndim,n,maxt,jcoef(2),
     a                      wksp(ifactr),coef(ndim+1),
     a                      1,irwise,
     a                      iwksp(iwkpt1),r,z)
      if (.not. propa) call icsn4 (n,n,maxt,iwksp(ifacti+1),
     a                 wksp(ifactr),
     a                 wksp(ifactr+n),
     a                 0,irwise,iwksp(iwkpt1),r,z)
      return
      end 
      subroutine subq54 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq54 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba4
c
      call ppii (suba4,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt1),n,r,z) 
      return
      end 
      subroutine subq55 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq55 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba5
c
      call ppii (suba5,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt1),n,r,z) 
      return
      end 
      subroutine subq56 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq56 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba4
c
      call pneu (suba4,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq57 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq57 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba5
c
      call pneu (suba5,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq58 (coef,jcoef,wksp,iwksp,n,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... subq58 calls the basic lsor iterative step
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      call sordnb (n,ndim,kblsz,kblsz,iwksp(ifacti),lbhb,
     a             wksp(ifactr),coef,jcoef,n,omega,u,rhs,unew)
      return
      end 
      subroutine subq59 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq59 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      nwdiag = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt1 = ndim*nwdiag + 1
      ipt2 = nwdiag + 1
      call sbsln (n,ndim,n,kblsz,1,idumb(1),idumb(2),idumb(3),
     a            iwksp(ifacti),wksp(ifactr),coef(ipt1),
     a            jcoef(ipt2),r,z,omega,1,wksp(iwkpt1))
      return
      end 
      subroutine subq60 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq60 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      nwdiag = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt1 = ndim*nwdiag + 1
      ipt2 = nwdiag + 1
      call sbslnt (n,ndim,n,kblsz,1,idumb(1),idumb(2),idumb(3),
     a             iwksp(ifacti),wksp(ifactr),coef(ipt1),
     a             jcoef(ipt2),r,z,omega,1,wksp(iwkpt1))
      return
      end 
      subroutine subq61 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq61 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      nwdiag = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt1 = ndim*nwdiag + 1
      ipt2 = nwdiag + 1
      call sbsln1 (n,ndim,n,kblsz,1,idumb(1),idumb(2),idumb(3),
     a             iwksp(ifacti),wksp(ifactr),coef(ipt1),
     a             jcoef(ipt2),r,z,omega,1)
      return
      end 
      subroutine subq62 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq62 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      nwdiag = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt1 = ndim*nwdiag + 1
      ipt2 = nwdiag + 1
      call sbsln3 (n,ndim,n,kblsz,1,idumb(1),idumb(2),idumb(3),
     a             iwksp(ifacti),wksp(ifactr),coef(ipt1),
     a             jcoef(ipt2),r,z,omega,1)
      return
      end 
      subroutine subq63 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq63 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      nwdiag = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt1 = ndim*nwdiag + 1
      ipt2 = nwdiag + 1
      call sbsln2 (n,ndim,n,kblsz,1,idumb(1),idumb(2),idumb(3),
     a             iwksp(ifacti),wksp(ifactr),coef(ipt1),
     a             jcoef(ipt2),r,z,omega,1,wksp(iwkpt1))
      return
      end 
      subroutine subq64 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq64 calls the lssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      nwdiag = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt1 = ndim*nwdiag + 1
      ipt2 = nwdiag + 1
      call sbsln4 (n,ndim,n,kblsz,1,idumb(1),idumb(2),idumb(3),
     a             iwksp(ifacti),wksp(ifactr),coef(ipt1),
     a             jcoef(ipt2),r,z,omega,1,wksp(iwkpt1))
      return
      end 
      subroutine subq65 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... subq65 calls the lssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      call ssrcdn (n,ndim,lbhb,kblsz,iwksp(ifacti),wksp(ifactr),
     a             coef,jcoef,n,p,r,wksp(iwkpt1),pdp,pldup) 
      return
      end 
      subroutine subq66 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq66 calls pbpii, for line lspoly preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba4, subq2
c
      call pbpii (suba4,subq2,coef,jcoef,wksp,iwksp,ainf,
     a            0.0d0,0.0d0,ndeg,wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq67 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq67 calls pbpii, for line lspoly preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba5, subq3
c
      call pbpii (suba5,subq3,coef,jcoef,wksp,iwksp,ainf,
     a            0.0d0,0.0d0,ndeg,wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq68 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq68 calls pbneu, for line neumann polynomial
c     preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba4, subq2
c
      call pbneu (suba4,subq2,coef,jcoef,wksp,iwksp,ndeg,
     a            wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq69 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq69 calls pbneu, for line neumann polynomial
c     preconditioning.
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba5, subq3
c
      call pbneu (suba5,subq3,coef,jcoef,wksp,iwksp,ndeg,
     a            wksp(iwkpt1),n,r,z)
      return
      end 
      subroutine subq70 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq70 calls ibsln, for bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
c
      n = nn
      nwnew = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt2 = ifactr + n*nwnew 
      if (lvfill .gt. 0) go to 10
      nwdiag = nwnew - 2*ltrunc
      if (propa) call ibsln
     a           (n,ndim,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            coef(ndim*nwdiag+1),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      if (.not. propa) call ibsln
     a           (n,n,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            wksp(ipt2),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + nwnew
      call ibsln  (n,n,n,kblsz,1,idumb(1),idumb(2),
     a             idumb(3),iwksp(ifacti),wksp(ifactr),
     a             wksp(ipt2),iwksp(ipt1),
     a             r,z,ivers,1,wksp(iwkpt1))
      return
      end 
      subroutine subq71 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq71 calls ibslnt, for bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
c
      n = nn
      nwnew = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt2 = ifactr + n*nwnew 
      if (lvfill .gt. 0) go to 10
      nwdiag = nwnew - 2*ltrunc
      if (propa) call ibslnt
     a           (n,ndim,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            coef(ndim*nwdiag+1),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      if (.not. propa) call ibslnt
     a           (n,n,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            wksp(iwkpt2),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + nwnew
      call ibslnt (n,n,n,kblsz,1,idumb(1),idumb(2),
     a             idumb(3),iwksp(ifacti),wksp(ifactr),
     a             wksp(ipt2),iwksp(ipt1),
     a             r,z,ivers,1,wksp(iwkpt1))
      return
      end 
      subroutine subq72 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq72 calls ibsln1, for bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
c
      n = nn
      nwnew = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt2 = ifactr + n*nwnew 
      if (lvfill .gt. 0) go to 10
      nwdiag = nwnew - 2*ltrunc
      if (propa) call ibsln1
     a           (n,ndim,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            coef(ndim*nwdiag+1),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      if (.not. propa) call ibsln1
     a           (n,n,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            wksp(iwkpt2),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + nwnew
      call ibsln1 (n,n,n,kblsz,1,idumb(1),idumb(2),
     a             idumb(3),iwksp(ifacti),wksp(ifactr),
     a             wksp(ipt2),iwksp(ipt1),
     a             r,z,ivers,1,wksp(iwkpt1))
      return
      end 
      subroutine subq73 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq73 calls ibsln3, for bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
c
      n = nn
      nwnew = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt2 = ifactr + n*nwnew 
      if (lvfill .gt. 0) go to 10
      nwdiag = nwnew - 2*ltrunc
      if (propa) call ibsln3
     a           (n,ndim,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            coef(ndim*nwdiag+1),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      if (.not. propa) call ibsln3
     a           (n,n,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            wksp(iwkpt2),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + nwnew
      call ibsln3 (n,n,n,kblsz,1,idumb(1),idumb(2),
     a             idumb(3),iwksp(ifacti),wksp(ifactr),
     a             wksp(ipt2),iwksp(ipt1),
     a             r,z,ivers,1,wksp(iwkpt1))
      return
      end 
      subroutine subq74 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq74 calls ibsln2, for bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
c
      n = nn
      nwnew = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt2 = ifactr + n*nwnew 
      if (lvfill .gt. 0) go to 10
      nwdiag = nwnew - 2*ltrunc
      if (propa) call ibsln2
     a           (n,ndim,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            coef(ndim*nwdiag+1),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      if (.not. propa) call ibsln2
     a           (n,n,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            wksp(iwkpt2),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + nwnew
      call ibsln2 (n,n,n,kblsz,1,idumb(1),idumb(2),
     a             idumb(3),iwksp(ifacti),wksp(ifactr),
     a             wksp(ipt2),iwksp(ipt1),
     a             r,z,ivers,1,wksp(iwkpt1))
      return
      end 
      subroutine subq75 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq75 calls ibsln4, for bic preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      dimension r(1), z(1), coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
c
      n = nn
      nwnew = iwksp(ifacti+2) + iwksp(ifacti+5)
      ipt2 = ifactr + n*nwnew 
      if (lvfill .gt. 0) go to 10
      nwdiag = nwnew - 2*ltrunc
      if (propa) call ibsln4
     a           (n,ndim,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            coef(ndim*nwdiag+1),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      if (.not. propa) call ibsln4
     a           (n,n,n,kblsz,1,idumb(1),idumb(2),
     a            idumb(3),iwksp(ifacti),wksp(ifactr),
     a            wksp(iwkpt2),jcoef(nwdiag+1),
     a            r,z,ivers,1,wksp(iwkpt1))
      return
 10   ipt1 = ifacti + 3*lbhb + nwnew
      call ibsln4 (n,n,n,kblsz,1,idumb(1),idumb(2),
     a             idumb(3),iwksp(ifacti),wksp(ifactr),
     a             wksp(ipt2),iwksp(ipt1),
     a             r,z,ivers,1,wksp(iwkpt1))
      return
      end 
      subroutine subq76 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq76 calls bdsol, for rs preconditioning. 
c
c
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      nbig = nr + nb
      call bdsol (nbig,n,n,ndt,ndb,wksp(ifactr),r,z,1)
      return
      end 
      subroutine subq77 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq77 calls bdsolt, for rs preconditioning.
c
c
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      n = nn
      nr = iwksp(nc)
      nb = iwksp(nc+1)
      nbig = nr + nb
      call bdsolt (nbig,n,n,ndt,ndb,wksp(ifactr),r,z)
      return
      end 
      subroutine subq78 (coef,jcoef,wksp,iwksp,n,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... subq78 calls the basic sor iterative step
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      ip1 = ndim + 1
      ip2 = ndim*(maxt + 1) + 1
      call sorp (ndim,n,maxt,maxb,jcoef(ip1),jcoef(ip2),coef,
     a           coef(ip1),coef(ip2),omega,u,rhs,unew)
      return
      end 
      subroutine subq79 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq79 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ip1 = ndim + 1
      ip2 = ndim*(maxt + 1) + 1
      call srsp (ndim,n,maxt,maxb,jcoef(ip1),jcoef(ip2),coef,
     a           coef(ip1),coef(ip2),omega,r,z)
      return
      end 
      subroutine subq80 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq80 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ip1 = ndim + 1
      ip2 = ndim*(maxt + 1) + 1
      call srsntp (ndim,n,maxt,maxb,jcoef(ip1),jcoef(ip2),coef,
     a             coef(ip1),coef(ip2),omega,r,z) 
      return
      end 
      subroutine subq81 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq81 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ip2 = ndim*(maxt + 1) + 1
      call srsp1 (ndim,n,maxb,jcoef(ip2),coef,coef(ip2),omega,r,z)
      return
      end 
      subroutine subq82 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq82 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ip2 = ndim*(maxt + 1) + 1
      call srsp3 (ndim,n,maxb,jcoef(ip2),coef,coef(ip2),omega,r,z)
      return
      end 
      subroutine subq83 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq83 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ip1 = ndim + 1
      call srsp2 (ndim,n,maxt,jcoef(ip1),coef,coef(ip1),omega,r,z)
      return
      end 
      subroutine subq84 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq84 calls the ssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ip1 = ndim + 1
      call srsp4 (ndim,n,maxt,jcoef(ip1),coef,coef(ip1),omega,r,z)
      return
      end 
      subroutine subq85 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... subq85 calls the ssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      ip1 = ndim + 1
      ip2 = ndim*(maxt + 1) + 1
      if (isymm .eq. 0) call ssorp (ndim,maxt,jcoef(ip1),coef,
     a                              coef(ip1),n,p,r,wksp(iwkpt1),
     a                              pdp,pldup)
      if (isymm .ne. 0) call ssorpn (ndim,maxt,maxb,jcoef(ip1),
     a                               jcoef(ip2),coef,coef(ip1),
     a                               coef(ip2),n,p,r,wksp(iwkpt1),
     a                               pdp,pldup)
      return
      end 
      subroutine subq86 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq86 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      logical symm
c
      n = nn
      symm = isymm .eq. 0
      if (.not. propa) go to 10
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         if (symm) call icsp (ndim,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),coef(ip1),1,r,z) 
         if (.not. symm) call icsnp (ndim,ndim,n,maxt,maxb, 
     a                        jcoef(ip1),jcoef(ip2),wksp(ifactr),
     a                        coef(ip1),coef(ip2),1,r,z)
         return
 10   if (lvfill .gt. 0) go to 15
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp (n,ndim,n,maxt,maxb,
     a                        jcoef(ip1),jcoef(ip2),wksp(ifactr),
     a                        wksp(ip3),wksp(ip4),0,r,z)
         return
 15   continue
         ip1 = ifacti + n
         ip2 = ifacti + n*(maxt + 1)
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp (n,n,n,maxt,iwksp(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp (n,n,n,maxt,maxb,
     a                        iwksp(ip1),iwksp(ip2),wksp(ifactr),
     a                        wksp(ip3),wksp(ip4),0,r,z)
         return
      end 
      subroutine subq87 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq87 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      logical symm
c
      n = nn
      symm = isymm .eq. 0
      if (.not. propa) go to 10
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         if (symm) call icsp (ndim,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),coef(ip1),1,r,z) 
         if (.not. symm) call icsntp (ndim,ndim,n,maxt,maxb,
     a                        jcoef(ip1),jcoef(ip2),wksp(ifactr),
     a                        coef(ip1),coef(ip2),1,r,z)
         return
 10   if (lvfill .gt. 0) go to 15
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsntp (n,ndim,n,maxt,maxb,
     a                        jcoef(ip1),jcoef(ip2),wksp(ifactr),
     a                        wksp(ip3),wksp(ip4),0,r,z)
         return
 15   continue
         ip1 = ifacti + n
         ip2 = ifacti + n*(maxt + 1)
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp (n,n,n,maxt,iwksp(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsntp (n,n,n,maxt,maxb,
     a                        iwksp(ip1),iwksp(ip2),wksp(ifactr),
     a                        wksp(ip3),wksp(ip4),0,r,z)
         return
      end 
      subroutine subq88 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq88 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      logical symm
c
      n = nn
      symm = isymm .eq. 0
      if (.not. propa) go to 10
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         if (symm) call icsp1 (ndim,ndim,n,maxt,jcoef(ip1), 
     a                        wksp(ifactr),coef(ip1),1,r,z) 
         if (.not. symm) call icsnp1 (ndim,ndim,n,maxb,
     a                        jcoef(ip2),wksp(ifactr),
     a                        coef(ip2),1,r,z)
         return
 10   if (lvfill .gt. 0) go to 15
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp1 (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp1 (n,ndim,n,maxb,
     a                        jcoef(ip2),wksp(ifactr),
     a                        wksp(ip4),0,r,z)
         return
 15   continue
         ip1 = ifacti + n
         ip2 = ifacti + n*(maxt + 1)
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp1 (n,n,n,maxt,iwksp(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp1 (n,n,n,maxb, 
     a                        iwksp(ip2),wksp(ifactr),
     a                        wksp(ip4),0,r,z)
         return
      end 
      subroutine subq89 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq89 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      logical symm
c
      n = nn
      symm = isymm .eq. 0
      if (.not. propa) go to 10
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         if (symm) call icsp3 (ndim,ndim,n,maxt,jcoef(ip1), 
     a                        wksp(ifactr),coef(ip1),1,r,z) 
         if (.not. symm) call icsnp3 (ndim,ndim,n,maxb,
     a                        jcoef(ip2),wksp(ifactr),
     a                        coef(ip2),1,r,z)
         return
 10   if (lvfill .gt. 0) go to 15
         ip1 = ndim + 1
         ip2 = ndim*(maxt + 1) + 1
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp3 (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp3 (n,ndim,n,maxb,
     a                        jcoef(ip2),wksp(ifactr),
     a                        wksp(ip4),0,r,z)
         return
 15   continue
         ip1 = ifacti + n
         ip2 = ifacti + n*(maxt + 1)
         ip3 = ifactr + n
         ip4 = n*(maxt + 1)+ ifactr
         if (symm) call icsp3 (n,n,n,maxt,iwksp(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp3 (n,n,n,maxb, 
     a                        iwksp(ip2),wksp(ifactr),
     a                        wksp(ip4),0,r,z)
         return
      end 
      subroutine subq90 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq90 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      logical symm
c
      n = nn
      symm = isymm .eq. 0
      if (.not. propa) go to 10
         ip1 = ndim + 1
         if (symm) call icsp2 (ndim,ndim,n,maxt,jcoef(ip1), 
     a                        wksp(ifactr),coef(ip1),1,r,z) 
         if (.not. symm) call icsnp2 (ndim,ndim,n,maxt,
     a                        jcoef(ip1),wksp(ifactr),
     a                        coef(ip1),1,r,z)
         return
 10   if (lvfill .gt. 0) go to 15
         ip1 = ndim + 1
         ip3 = ifactr + n
         if (symm) call icsp2 (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp2 (n,ndim,n,maxt,
     a                        jcoef(ip1),wksp(ifactr),
     a                        wksp(ip3),0,r,z)
         return
 15   continue
         ip1 = ifacti + n
         ip3 = ifactr + n
         if (symm) call icsp2 (n,n,n,maxt,iwksp(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp2 (n,n,n,maxt, 
     a                        iwksp(ip1),wksp(ifactr),
     a                        wksp(ip3),0,r,z)
         return
      end 
      subroutine subq91 (coef,jcoef,wksp,iwksp,nn,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq91 calls ics, for ic(s) preconditioning.
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      logical symm
c
      n = nn
      symm = isymm .eq. 0
      if (.not. propa) go to 10
         ip1 = ndim + 1
         if (symm) call icsp4 (ndim,ndim,n,maxt,jcoef(ip1), 
     a                        wksp(ifactr),coef(ip1),1,r,z) 
         if (.not. symm) call icsnp4 (ndim,ndim,n,maxt,
     a                        jcoef(ip1),wksp(ifactr),
     a                        coef(ip1),1,r,z)
         return
 10   if (lvfill .gt. 0) go to 15
         ip1 = ndim + 1
         ip3 = ifactr + n
         if (symm) call icsp4 (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp4 (n,ndim,n,maxt,
     a                        jcoef(ip1),wksp(ifactr),
     a                        wksp(ip3),0,r,z)
         return
 15   continue
         ip1 = ifacti + n
         ip3 = ifactr + n
         if (symm) call icsp4 (n,n,n,maxt,iwksp(ip1),
     a                        wksp(ifactr),wksp(ip3),0,r,z) 
         if (.not. symm) call icsnp4 (n,n,n,maxt, 
     a                        iwksp(ip1),wksp(ifactr),
     a                        wksp(ip3),0,r,z)
         return
      end 
      subroutine subq92 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq92 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba8
c
      call ppii (suba8,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt2),n,r,z) 
      return
      end 
      subroutine subq93 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq93 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba9
c
      call ppii (suba9,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt2),n,r,z) 
      return
      end 
      subroutine subq94 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq94 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba8
c
      call pneu (suba8,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt2),n,r,z)
      return
      end 
      subroutine subq95 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq95 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba9
c
      call pneu (suba9,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt2),n,r,z)
      return
      end 
      subroutine subq96 (coef,jcoef,wksp,iwksp,n,u,rhs,unew)
      implicit double precision (a-h, o-z)
c
c ... subq96 calls the basic multi-color sor iterative step 
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension u(1), rhs(1), unew(1), coef(1), wksp(1)
c
      call sorcp (ndim,n,jcoef(ndim+1),coef,coef(ndim+1),ncolor,
     a            iwksp(nc),iwksp(ndt),iwksp(ndb),omega,u,rhs,
     a            unew)
      return
      end 
      subroutine subq97 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq97 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      call srscp (ndim,n,jcoef(ipt1),coef,coef(ipt1),ncolor,
     a            iwksp(nc),iwksp(ndt),iwksp(ndb),omega,
     a            wksp(iwkpt1),r,z)
      return
      end 
      subroutine subq98 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq98 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      call srscpt (ndim,n,jcoef(ipt1),coef,coef(ipt1),ncolor,
     a             iwksp(nc),iwksp(ndt),iwksp(ndb),omega,
     a             wksp(iwkpt1),r,z)
      return
      end 
      subroutine subq99 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... subq99 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      call srscp1 (ndim,n,jcoef(ipt1),coef,coef(ipt1),ncolor,
     a             iwksp(nc),iwksp(ndt),iwksp(ndb),omega,
     a             wksp(iwkpt1),r,z)
      return
      end 
      subroutine sub100 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub100 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      call srscp3 (ndim,n,jcoef(ipt1),coef,coef(ipt1),ncolor,
     a             iwksp(nc),iwksp(ndt),iwksp(ndb),omega,
     a             wksp(iwkpt1),r,z)
      return
      end 
      subroutine sub101 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub101 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      call srscp2 (ndim,n,jcoef(ipt1),coef,coef(ipt1),ncolor,
     a             iwksp(nc),iwksp(ndt),omega,
     a             wksp(iwkpt1),r,z)
      return
      end 
      subroutine sub102 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub102 calls the mssor preconditioner.
c
c
c *** begin -- itpack common
c
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      call srscp4 (ndim,n,jcoef(ipt1),coef,coef(ipt1),ncolor,
     a             iwksp(nc),iwksp(ndt),omega,
     a             wksp(iwkpt1),r,z)
      return
      end 
      subroutine sub103 (coef,jcoef,wksp,iwksp,n,p,r,pdp,pldup)
      implicit double precision (a-h, o-z)
c
c ... sub103 calls the mssor adaption routine.
c
c
      common / dscons / ndim, mdim, maxnz
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
c
      integer jcoef(2), iwksp(1)
      dimension p(1), r(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      if (isymm .eq. 0) call ssrcp (ndim,jcoef(ipt1),coef,coef(ipt1), 
     a                              n,ncolor,iwksp(nc),iwksp(ndt),p,
     a                              r,wksp(iwkpt1),pdp,pldup)
      if (isymm .eq. 1) call ssrcpn (ndim,jcoef(ipt1),coef,coef(ipt1),
     a                               n,ncolor,iwksp(nc),iwksp(ndt),
     a                               iwksp(ndb),p,r,wksp(iwkpt1),
     a                               pdp,pldup)
      return
      end 
      subroutine sub104 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub104 calls the ic preconditioner.
c     (multicolor purdue)
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      ipt2 = ifactr + n
      if (propa) call icscp (ndim,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       coef(ipt1),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),1,wksp(iwkpt1),r,z) 
      if (.not. propa) call icscp (n,ndim,n,jcoef(ipt1),wksp(ifactr), 
     a                       wksp(ipt2),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),0,wksp(iwkpt1),r,z) 
      return
      end 
      subroutine sub105 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub105 calls the ic preconditioner.
c     (multicolor purdue)
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      ipt2 = ifactr + n
      if (propa) call icscpt (ndim,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       coef(ipt1),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),1,wksp(iwkpt1),r,z) 
      if (.not. propa) call icscpt (n,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       wksp(ipt2),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),0,wksp(iwkpt1),r,z) 
      return
      end 
      subroutine sub106 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub106 calls the ic preconditioner.
c     (multicolor purdue)
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      ipt2 = ifactr + n
      if (propa) call icscp1 (ndim,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       coef(ipt1),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),1,wksp(iwkpt1),r,z) 
      if (.not. propa) call icscp1 (n,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       wksp(ipt2),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),0,wksp(iwkpt1),r,z) 
      return
      end 
      subroutine sub107 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub107 calls the ic preconditioner.
c     (multicolor purdue)
c
c
      common / dscons / ndim, mdim, maxnz
      logical           propa 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      ipt2 = ifactr + n
      if (propa) call icscp3 (ndim,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       coef(ipt1),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),1,wksp(iwkpt1),r,z) 
      if (.not. propa) call icscp3 (n,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       wksp(ipt2),ncolor,iwksp(nc),iwksp(ndt),
     a                       iwksp(ndb),0,wksp(iwkpt1),r,z) 
      return
      end 
      subroutine sub108 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub104 calls the ic preconditioner.
c     (multicolor purdue)
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      ipt2 = ifactr + n
      if (propa) call icscp2 (ndim,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       coef(ipt1),ncolor,iwksp(nc),iwksp(ndt),
     a                       1,wksp(iwkpt1),r,z)
      if (.not. propa) call icscp2 (n,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       wksp(ipt2),ncolor,iwksp(nc),iwksp(ndt),
     a                       0,wksp(iwkpt1),r,z)
      return
      end 
      subroutine sub109 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub104 calls the ic preconditioner.
c     (multicolor purdue)
c
c
      common / dscons / ndim, mdim, maxnz
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
c
      ipt1 = ndim + 1
      ipt2 = ifactr + n
      if (propa) call icscp4 (ndim,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       coef(ipt1),ncolor,iwksp(nc),iwksp(ndt),
     a                       1,wksp(iwkpt1),r,z)
      if (.not. propa) call icscp4 (n,ndim,n,jcoef(ipt1),wksp(ifactr),
     a                       wksp(ipt2),ncolor,iwksp(nc),iwksp(ndt),
     a                       0,wksp(iwkpt1),r,z)
      return
      end 
      subroutine sub110 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub110 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba12
c
      call ppii (suba12,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt2),n,r,z) 
      return
      end 
      subroutine sub111 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub111 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba12
c
      call pneu (suba12,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt2),n,r,z)
      return
      end 
      subroutine sub112 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub112 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba13
c
      call ppii (suba13,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt2),n,r,z) 
      return
      end 
      subroutine sub113 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub113 calls ppii, for lspoly preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      common / itcom8 / ainf
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba14
c
      call ppii (suba14,coef,jcoef,wksp,iwksp,ainf,
     a           0.0d0,0.0d0,ndeg,wksp(iwkpt2),n,r,z) 
      return
      end 
      subroutine sub114 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub114 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba13
c
      call pneu (suba13,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt2),n,r,z)
      return
      end 
      subroutine sub115 (coef,jcoef,wksp,iwksp,n,r,z)
      implicit double precision (a-h, o-z)
c
c ... sub115 calls pneu, for neumann polynomial preconditioning.
c
c
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      integer jcoef(2), iwksp(1)
      dimension r(1), z(1), coef(1), wksp(1)
      external suba14
c
      call pneu (suba14,coef,jcoef,wksp,iwksp,coef,ndeg,
     a           wksp(iwkpt2),n,r,z)
      return
      end 
      subroutine lfact (coef,jcoef,wksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... lfact computes a line factorization.
c
c ... parameters -- 
c
c        n        problem size
c        nfactr   factorization size
c
c ... common blocks 
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2)
      dimension coef(1), wksp(1)
c
c ... check for sufficient workspace to store factor.
c
      n = nn
      if (nstore .eq. 2) isym = 0
      if (nstore .eq. 3) isym = 1
      ndt = 0
      ndb = 0
      do 20 jd = 1,maxnz
         do 15 j = 1,maxnz
            if (jcoef(j) .ne. jd) go to 15
            ndt = ndt + 1
            go to 20
 15      continue
         go to 25
 20   continue
 25   if (isym .eq. 0) go to 40
      do 35 jd = 1,maxnz
         do 30 j = 1,maxnz
            if (jcoef(j) .ne. -jd) go to 30
            ndb = ndb + 1
            go to 35
 30      continue
         go to 40
 35   continue
 40   nfactr = (ndt + ndb + 1)*n
      call needw ('lfact',0,irpnt,nfactr,ier)
      if (ier .lt. 0) return
c
      ifactr = irpnt
      call vcopy (n,coef,wksp(ifactr))
      ndt = 0
      do 55 jd = 1,maxnz
         do 50 j = 1,maxnz
            if (jcoef(j) .ne. jd) go to 50
            ndt = ndt + 1
            ipt1 = (j - 1)*ndim + 1
            ipt2 = ndt*n + ifactr
            call vcopy (n,coef(ipt1),wksp(ipt2))
            go to 55
 50      continue
         go to 60
 55   continue
 60   ndb = 0
      if (isym .eq. 0) go to 75
      do 70 jd = 1,maxnz
         do 65 j = 1,maxnz
            if (jcoef(j) .ne. -jd) go to 65
            ndb = ndb + 1
            ipt1 = (j - 1)*ndim + 1
            ipt2 = (ndt + ndb)*n + ifactr
            call vcopy (n,coef(ipt1),wksp(ipt2))
            go to 70
 65      continue
         go to 75
 70   continue
c
c ... factor.
c
 75   call bdfac (n,n,kblsz,ndt,ndb,wksp(ifactr),isym)
      irpnt = irpnt + nfactr
      return
      end 
      subroutine linv (coef,jcoef,wksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... linv computes a line approximate inverse.
c
c ... parameters -- 
c
c        n        problem size
c        nfactr   factorization size
c
c ... common blocks 
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      integer jcoef(2)
      dimension coef(1), wksp(1)
c
c ... check for sufficient workspace to store factor.
c
      n = nn
      if (nstore .eq. 2) isym = 0
      if (nstore .eq. 3) isym = 1
      ndt = 0
      ndb = 0
      do 20 jd = 1,maxnz
         do 15 j = 1,maxnz
            if (jcoef(j) .ne. jd) go to 15
            ndt = ndt + 1
            go to 20
 15      continue
         go to 25
 20   continue
 25   if (isym .eq. 0) go to 40
      do 35 jd = 1,maxnz
         do 30 j = 1,maxnz
            if (jcoef(j) .ne. -jd) go to 30
            ndb = ndb + 1
            go to 35
 30      continue
         go to 40
 35   continue
c
 40   ndt = ndt + ltrunc
      if (isym .eq. 1) ndb = ndb + ltrunc
      nfactr = (ndt + ndb + 1)*n
      call needw ('linv',0,irpnt,nfactr,ier)
      if (ier .lt. 0) return
c
      ifactr = irpnt
      call vfill (nfactr,wksp(ifactr),0.0d0)
      call vcopy (n,coef,wksp(ifactr))
      it = 0
      do 55 jd = 1,maxnz
         do 50 j = 1,maxnz
            if (jcoef(j) .ne. jd) go to 50
            it = it + 1
            ipt1 = (j - 1)*ndim + 1
            ipt2 = it*n + ifactr
            call vcopy (n,coef(ipt1),wksp(ipt2))
            go to 55
 50      continue
         go to 60
 55   continue
 60   if (isym .eq. 0) go to 75
      it = ndt
      do 70 jd = 1,maxnz
         do 65 j = 1,maxnz
            if (jcoef(j) .ne. -jd) go to 65
            it = it + 1
            ipt1 = (j - 1)*ndim + 1
            ipt2 = it*n + ifactr
            call vcopy (n,coef(ipt1),wksp(ipt2))
            go to 70
 65      continue
         go to 75
 70   continue
c
c ... factor and invert.
c
 75   call bdfac (n,n,kblsz,ndt,ndb,wksp(ifactr),isym)
      call bdinv (n,n,kblsz,ndt,ndb,wksp(ifactr),isym)
      irpnt = irpnt + nfactr
      return
      end 
      subroutine mfact (coef,jcoef,wksp,iwksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... mfact computes a line factorization of a multi-color matrix.
c
c ... parameters -- 
c
c        n        problem size
c        nfactr   factorization size
c
c ... common blocks 
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
c
c ... check for sufficient workspace to store factor.
c
      n = nn
      ndt = iwksp(iblock+2) - 1
      ndb = iwksp(iblock+ncolor*3+2)
      nwdiag = ndt + ndb + 1
      nfactr = n*nwdiag
      call needw ('mfact',0,irpnt,nfactr,ier)
      if (ier .lt. 0) return
c
      ifactr = irpnt
      do 15 j = 1,nwdiag
         ipt1 = (j - 1)*ndim + 1
         ipt2 = (j - 1)*n + ifactr
         call vcopy (n,coef(ipt1),wksp(ipt2))
 15   continue
c
c ... factor.
c
      call bdfac (n,n,n,ndt,ndb,wksp(ifactr),1)
      irpnt = irpnt + nfactr
      return
      end 
      subroutine pfact1 (coef,jcoef,wksp,iwksp,nn,methh,ier)
      implicit double precision (a-h, o-z)
c
c ... pfact1 computes a point incomplete factorization.
c
c ... parameters
c
c       n       order of system
c       meth    method of factorization 
c                = 1   ic   (unmodified)
c                = 2   mic  (modified)
c       nfactr  amount of floating point workspace needed for factorization
c       nfacti  amount of integer workspace needed for factorization
c       ier     error flag
c
c ... specifications for parameters
c
c
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
      n = nn
      meth = methh
c
c ... if requested, find out if matrix has property a.
c
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
      if (lvfill .gt. 0) propa = .false.
      if (lvfill .gt. 0) go to 55
      if (ipropa .ne. 2) go to 15
      call needw ('pfact1',1,iipnt,2*n,ier)
      if (ier .lt. 0) return
      call prbndx (n,ndim,maxnz,jcoef,coef,iwksp(iipnt),
     a             iwksp(iipnt+n),propa,1)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
c
 15   if (.not. propa) go to 35
c
c ... propa = .true.
c
      ifactr = irpnt
      nfactr = n
      nfacti = 0
      call needw ('pfact1',0,irpnt,nfactr+n,ier)
      if (ier .lt. 0) return
      call vcopy (n,coef,wksp(ifactr))
      irpnt = irpnt + nfactr
      ip1 = ndim + 1
      ip2 = ndim*(maxt + 1) + 1
      if (isymm .eq. 0) call icfp (ndim,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),coef(ip1),meth,1,omega,
     a                        wksp(irpnt),iflag)
      if (isymm .ne. 0) call icfnp (ndim,ndim,n,maxt,maxb,jcoef(ip1), 
     a                        jcoef(ip2),wksp(ifactr),coef(ip1),
     a                        coef(ip2),meth,1,omega,iflag) 
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact1')
      return
c
c ... propa = .false., lvfill = 0.
c
 35   ifactr = irpnt
      jmax = maxt + 1
      if (isymm .ne. 0) jmax = 1 + maxt + maxb
      nfactr = n*jmax
      nfacti = 0
      call needw ('pfact1',0,irpnt,nfactr+n,ier)
      if (ier .lt. 0) return
      call vfill (nfactr,wksp(ifactr),0.0d0)
      do 45 j = 1,jmax
         ip1 = ndim*(j - 1) + 1
         ip2 = n*(j - 1) + ifactr
         call vcopy (n,coef(ip1),wksp(ip2))
 45   continue
      irpnt = irpnt + nfactr
      ip1 = ndim + 1
      ip2 = ndim*(maxt + 1) + 1
      ip3 = ifactr + n
      ip4 = n*(maxt + 1) + ifactr
      if (isymm .eq. 0) call icfp (n,ndim,n,maxt,jcoef(ip1),
     a                        wksp(ifactr),wksp(ip3),meth,0,omega,
     a                        wksp(irpnt),iflag)
      if (isymm .ne. 0) call icfnp (n,ndim,n,maxt,maxb,jcoef(ip1),
     a                        jcoef(ip2),wksp(ifactr),wksp(ip3),
     a                        wksp(ip4),meth,0,omega,iflag) 
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact1')
      return
c
c ... propa = .false., lvfill .gt. 0
c
 55   len = n*(maxt + 1)
      if (isymm .ne. 0) len = n*(1 + maxt + maxb) 
      call needw ('pfact1',1,iipnt,len,ier)
      if (ier .lt. 0) return
      call needw ('pfact1',0,irpnt,len,ier)
      if (ier .lt. 0) return
      jmax = maxt + 1
      if (isymm .ne. 0) jmax = 1 + maxt + maxb
      do 70 j = 1,jmax
         ipt1 = (j - 1)*ndim + 1
         ipt2 = (j - 1)*n + iipnt
         call vicopy (n,jcoef(ipt1),iwksp(ipt2))
         ipt2 = (j - 1)*n + irpnt
         call vcopy (n,coef(ipt1),wksp(ipt2))
 70   continue
      mw1 = (leni - (iipnt + n) + 1)/n
      mw2 = (lenr - (irpnt + n) + 1)/n
      mwidth = min (mw1,mw2) 
      maxc = maxt + maxb
      do 75 i = 1,lvfill
         if (isymm .eq. 0) call fillsp (n,n,maxt,iwksp(iipnt+n),
     a                              wksp(irpnt+n),mwidth,ier)
         if (isymm .ne. 0) call fillnp (n,n,maxc,iwksp(iipnt+n),
     a                              wksp(irpnt+n),mwidth,ier)
         if (ier .lt. 0) then
            call ershow (ier,'pfact1')
            return
         endif
 75   continue
      maxcp1 = maxc + 1
      if (isymm .ne. 0) call move1 (n,mwidth+1,n,maxcp1,
     a                     iwksp(iipnt),wksp(irpnt),maxt,maxb,ier)
      if (ier .lt. 0) then
         call ershow (ier,'pfact1')
         return
      endif
      if (isymm .eq. 0) nfactr = n*(maxt + 1)
      if (isymm .ne. 0) nfactr = n*(maxt + maxb + 1)
      nfacti = nfactr
      call needw ('pfact1',0,irpnt,nfactr+n,ier)
      if (ier .lt. 0) return
      call needw ('pfact1',1,iipnt,nfacti,ier)
      if (ier .lt. 0) return
c
      ifactr = irpnt
      ifacti = iipnt
      irpnt = irpnt + nfactr
      iipnt = iipnt + nfacti
      ip1 = ifacti + n
      ip2 = ifacti + n*(maxt + 1)
      ip3 = ifactr + n
      ip4 = ifactr + n*(maxt + 1)
      if (isymm .eq. 0) call icfp (n,n,n,maxt,iwksp(ip1),
     a                          wksp(ifactr),wksp(ip3),
     a                          meth,0,omega,wksp(irpnt),iflag)
      if (isymm .ne. 0) call icfnp (n,n,n,maxt,maxb,iwksp(ip1),
     a                      iwksp(ip2),wksp(ifactr),wksp(ip3),
     a                      wksp(ip4),meth,0,omega,iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact1')
      return
      end 
      subroutine pfact2 (coef,jcoef,wksp,iwksp,nn,methh,ier)
      implicit double precision (a-h, o-z)
c
c ... pfact2 computes a point incomplete factorization.
c
c ... parameters
c
c       n       order of system
c       meth    method of factorization 
c                = 1   ic   (unmodified)
c                = 2   mic  (modified)
c       nfactr  amount of floating point workspace needed for factorization
c       ier     error flag
c
c ... specifications for parameters
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
c
      n = nn
      meth = methh
c
c ... if requested, find out if matrix has property a.
c
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
      if (lvfill .gt. 0) propa = .false.
      if (lvfill .gt. 0) go to 20
      if (ipropa .ne. 2) go to 15
      call needw ('pfact2',1,iipnt,2*n,ier)
      if (ier .lt. 0) return
      call prbndx (n,ndim,maxnz,jcoef,coef,iwksp(iipnt),
     a             iwksp(iipnt+n),propa,2)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
c
 15   if (.not. propa) go to 20
c
c ... propa = .true.
c
      maxt = maxnz - 1
      maxb = 0
      ifactr = irpnt
      nfactr = n
      nfacti = 0
      call needw ('pfact2',0,irpnt,nfactr+n,ier)
      if (ier .lt. 0) return
      call rowise (maxnz,jcoef,irwise)
      call needw ('pfact2',1,iipnt,maxnz+maxt**2,ier)
      if (ier .lt. 0) return
      call vfill (n,wksp(ifactr),0.0d0)
      call vcopy (n,coef,wksp(ifactr))
      irpnt = irpnt + nfactr
      if (ifctv .eq. 0) call icf
     a          (ndim,n,maxt,jcoef(2),wksp(ifactr),coef(ndim+1),
     a          meth,1,omega,wksp(irpnt),iwksp(iipnt),iflag)
      if (ifctv .eq. 1) call icfv
     a          (ndim,n,maxt,jcoef(2),wksp(ifactr),coef(ndim+1),
     a          meth,1,omega,wksp(irpnt),iwksp(iipnt),iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact2')
      return
c
c ... propa = .false.
c
 20   call vicopy (maxnz,jcoef,iwksp(iipnt))
      maxt = maxnz - 1
      maxb = 0
      if (lvfill .eq. 0) go to 26
      do 25 i = 1,lvfill
 25   call fills (maxt,iwksp(iipnt+1))
 26   nfactr = n*(maxt + 1)
      nfacti = maxt + 1
      call needw ('pfact2',1,iipnt,maxt**2,ier)
      if (ier .lt. 0) return
      call needw ('pfact2',0,irpnt,nfactr+n,ier)
      if (ier .lt. 0) return
c
      ifactr = irpnt
      ifacti = iipnt
      call vfill (nfactr,wksp(ifactr),0.0d0)
      do 40 j = 1,maxnz
         ip1 = ndim*(j - 1) + 1
         ip2 = n*(j - 1) + ifactr
         call vcopy (n,coef(ip1),wksp(ip2))
 40   continue
      irpnt = irpnt + nfactr
      iipnt = iipnt + maxt + 1
      call rowise (maxt+1,iwksp(ifacti),irwise)
      call needw ('pfact2',1,iipnt,maxt,ier)
      if (ier .lt. 0) return
      if (ifctv .eq. 0) call icf
     a       (n,n,maxt,iwksp(ifacti+1),wksp(ifactr),wksp(ifactr+n),
     a       meth,0,omega,wksp(irpnt),iwksp(iipnt),iflag)
      if (ifctv .eq. 1) call icfv
     a       (n,n,maxt,iwksp(ifacti+1),wksp(ifactr),wksp(ifactr+n),
     a       meth,0,omega,wksp(irpnt),iwksp(iipnt),iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact2')
      return
      end 
      subroutine pfact3 (coef,jcoef,wksp,iwksp,nn,meth,ier) 
      implicit double precision (a-h, o-z)
c
c ... pfact3 computes a point incomplete factorization.
c
c ... parameters
c
c       n       order of system
c       meth    method of factorization 
c                = 1   ic   (unmodified)
c                = 2   mic  (modified)
c       nfactr  amount of floating point workspace needed for factorization
c       ier     error flag
c
c ... specifications for parameters
c
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
c
      n = nn
c
c ... if requested, find out if matrix has property a.
c
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
      if (lvfill .gt. 0) propa = .false.
      if (lvfill .gt. 0) go to 20
      if (ipropa .ne. 2) go to 15
      call needw ('pfact3',1,iipnt,2*n,ier)
      if (ier .lt. 0) return
      call prbndx (n,ndim,maxnz,jcoef,coef,iwksp(iipnt),
     a             iwksp(iipnt+n),propa,3)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
c
 15   if (.not. propa) go to 20
c
c ... propa = .true.
c
      ifactr = irpnt
      nfactr = n
      nfacti = 0
      call needw ('pfact3',0,irpnt,nfactr+n,ier)
      if (ier .lt. 0) return
      call rowise (maxnz,jcoef,irwise)
      call needw ('pfact3',1,iipnt,maxt*maxb,ier) 
      if (ier .lt. 0) return
      call vfill (n,wksp(ifactr),0.0d0)
      call vcopy (n,coef,wksp(ifactr))
      irpnt = irpnt + nfactr
      maxtp1 = maxt + 1
      call icfn (ndim,n,maxt,maxb,jcoef(2),jcoef(maxt+2),
     a           wksp(ifactr),coef(ndim+1),coef(ndim*maxtp1+1),
     a           meth,1,omega,wksp(irpnt),iwksp(iipnt),iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact3')
      return
c
c ... propa = .false.
c
 20   call vicopy (maxnz,jcoef,iwksp(iipnt))
      maxz = maxnz
      if (lvfill .eq. 0) go to 26
      do 25 i = 1,lvfill
 25   call filln (maxz,iwksp(iipnt))
 26   nfactr = n*maxz
      nfacti = maxz 
      call needw ('pfact3',1,iipnt,maxz,ier)
      if (ier .lt. 0) return
      call needw ('pfact3',0,irpnt,nfactr,ier)
      if (ier .lt. 0) return
c
      ifactr = irpnt
      ifacti = iipnt
      call vfill (nfactr,wksp(ifactr),0.0d0)
      do 40 j = 1,maxnz
         ip1 = ndim*(j - 1) + 1
         ip2 = n*(j - 1) + ifactr
         call vcopy (n,coef(ip1),wksp(ip2))
 40   continue
      irpnt = irpnt + nfactr
      iipnt = iipnt + maxz
      call rowise (maxz,iwksp(ifacti),irwise)
      call needw ('pfact3',0,irpnt,n,ier)
      if (ier .lt. 0) return
      call move2 (n,n,maxz,iwksp(ifacti),wksp(ifactr),
     a            wksp(irpnt),iwksp(iipnt),maxt,maxb)
      call needw ('pfact3',1,iipnt,maxt*maxb,ier) 
      if (ier .lt. 0) return
      ipt1 = ifacti + maxt + 1
      ipt2 = ifactr + n*(maxt + 1)
      call icfn (n,n,maxt,maxb,iwksp(ifacti+1),iwksp(ipt1), 
     a           wksp(ifactr),wksp(ifactr+n),wksp(ipt2),
     a           meth,0,omega,wksp(irpnt),iwksp(iipnt),iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfact3')
      return
      end 
      subroutine pfactc (coef,jcoef,wksp,iwksp,nn,methh,ier)
      implicit double precision (a-h, o-z)
c
c ... pfactc computes a point incomplete factorization.
c     (multicolor ordering)
c
c ... parameters
c
c       n       order of system
c       meth    method of factorization 
c                = 1   ic   (unmodified)
c                = 2   mic  (modified)
c       ier     error flag
c
c ... specifications for parameters
c
c
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
      n = nn
      meth = methh
c
c ... if requested, find out if matrix has property a.
c
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
      if (ipropa .ne. 2) go to 15
      call needw ('pfactc',1,iipnt,2*n,ier)
      if (ier .lt. 0) return
      call prbndx (n,ndim,maxnz,jcoef,coef,iwksp(iipnt),
     a             iwksp(iipnt+n),propa,1)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
c
 15   if (.not. propa) go to 30
c
c ... propa = .true.
c
      ifactr = irpnt
      nfactr = n
      nfacti = 0
      call needw ('pfactc',0,irpnt,nfactr,ier)
      if (ier .lt. 0) return
      call vcopy (n,coef,wksp(ifactr))
      irpnt = irpnt + nfactr
      ip1 = ndim + 1
      maxc = maxnz - 1
      call icfcp (ndim,ndim,n,maxc,jcoef(ip1),wksp(ifactr), 
     a            coef(ip1),ncolor,iwksp(ndt),iwksp(ndb),
     a            meth,1,iwksp(ipt),omega,iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfactc')
      return
c
c ... propa = .false.
c
 30   ifactr = irpnt
      nfactr = n*maxnz
      nfacti = 0
      call needw ('pfactc',0,irpnt,nfactr,ier)
      if (ier .lt. 0) return
      call vfill (nfactr,wksp(ifactr),0.0d0)
      do 45 j = 1,maxnz
         ip1 = ndim*(j - 1) + 1
         ip2 = n*(j - 1) + ifactr
         call vcopy (n,coef(ip1),wksp(ip2))
 45   continue
      irpnt = irpnt + nfactr
      ip1 = ndim + 1
      ip3 = ifactr + n
      maxc = maxnz - 1
      call icfcp (n,ndim,n,maxc,jcoef(ip1),wksp(ifactr),wksp(ip3),
     a            ncolor,iwksp(ndt),iwksp(ndb),meth,
     a            0,iwksp(ipt),omega,iflag)
      if (iflag .eq. 1) ier = -12
      if (iflag .eq. 2) ier = 5
      if (iflag .eq. 0) return
      call ershow (ier,'pfactc')
      return
      end 
      subroutine bfacmz (methf,factor,coef,jcoef,wksp,iwksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... bfacmz computes a block factorization.
c     (nonsymmetric diagonal) 
c
c ... parameters
c
c       n       order of system
c       nfactr  amount of floating point workspace needed for factorization
c       ier     error flag
c
c ... specifications for parameters
c
c
      external factor
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      dimension coef(1), wksp(1)
      integer idumb(3), jcoef(2), iwksp(1)
c
      n = nn
      if (methf .le. 2) ivers = 1
      if (methf .gt. 2) ivers = 2
c
c ... if requested, find out if matrix has block property a.
c
      ncol = n/kblsz
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
      if (lvfill .gt. 0) propa = .false.
      if (lvfill .gt. 0) go to 15
      if (ipropa .ne. 2) go to 15
      call needw ('bfacmz',1,iipnt,2*ncol,ier)
      if (ier .lt. 0) return
      iwksp(iipnt) = lbhb
      call prbblk (ncol,1,iwksp(iblock),iwksp(iipnt),
     a             iwksp(iipnt+1),iwksp(iipnt+ncol+1),propa)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
c
c ... calculate fill-in and factor.
c
 15   call fillbn (n,coef,jcoef,iwksp(iblock),wksp,iwksp,ier)
      if (ier .lt. 0) return
      nwnew = iwksp(iblock+2) + iwksp(iblock+5)
      nwdiag = nwnew - 2*ltrunc
      if (methf .eq. 1) nwkp = kblsz*nwnew
      if (methf .eq. 2) nwkp = kblsz*(nwnew + 1)
      if (methf .eq. 3) nwkp = 0
      if (methf .eq. 4) nwkp = n + 2*kblsz
      call needw ('fillbn',0,irpnt,nwkp,ier)
      if (ier .lt. 0) return
      ipt1 = iblock + 3*lbhb
      ipt2 = ipt1 + nwnew
      idumb(1) = kblsz
      idumb(2) = 1
      idumb(3) = lbhb
      if (propa) then
         call factor (n,ndim,n,iwksp(iipnt),jcoef(nwdiag+1),
     a                wksp(ifactr),coef(ndim*nwdiag+1),1,
     a                idumb(1),iwksp(iblock),idumb(3),1,1,
     a                idumb(2),omega,wksp(irpnt),ier)
      endif
      if (.not. propa .and. lvfill .eq. 0) then
         call factor (n,n,n,iwksp(iipnt),jcoef(nwdiag+1),
     a                wksp(ifactr),wksp(iwkpt2),1,
     a                idumb(1),iwksp(iblock),idumb(3),1,0,
     a                idumb(2),omega,wksp(irpnt),ier)
      endif
      if (lvfill .gt. 0) then 
         call factor (n,n,n,iwksp(ipt1),iwksp(ipt2),
     a                wksp(ifactr),wksp(iwkpt2),1,
     a                idumb(1),iwksp(iblock),idumb(3),1,0,
     a                idumb(2),omega,wksp(irpnt),ier)
      endif
      return
      end 
      subroutine bfacs (methf,factor,coef,jcoef,wksp,iwksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... bfacs computes a block factorization.
c     (symmetric diagonal)
c
c ... parameters
c
c       n       order of system
c       nfactr  amount of floating point workspace needed for factorization
c       ier     error flag
c
c ... specifications for parameters
c
c
      external factor
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
c
      n = nn
      if (methf .le. 2) ivers = 1
      if (methf .gt. 2) ivers = 2
c
c ... if requested, find out if matrix has block property a.
c
      ncol = n/kblsz
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
      if (lvfill .gt. 0) propa = .false.
      if (lvfill .gt. 0) go to 15
      if (ipropa .ne. 2) go to 15
      call needw ('bfacs',1,iipnt,2*ncol,ier)
      if (ier .lt. 0) return
      iwksp(iipnt) = lbhb
      call prbblk (ncol,1,iwksp(iblock),iwksp(iipnt),
     a             iwksp(iipnt+1),iwksp(iipnt+ncol+1),propa)
      if (propa) ipropa = 1
      if (.not. propa) ipropa = 0
c
c ... calculate fill-in and factor.
c
 15   call fillb (n,coef,jcoef,iwksp(iblock),wksp,iwksp,ier)
      if (ier .lt. 0) return
      nwnew = iwksp(iblock+2) 
      nwdiag = nwnew - ltrunc 
      if (methf .eq. 1) nwkp = kblsz*nwnew
      if (methf .eq. 2) nwkp = kblsz*(nwnew + 1)
      if (methf .eq. 3) nwkp = 0
      if (methf .eq. 4) nwkp = n + 2*kblsz
      call needw ('fillb',0,irpnt,nwkp,ier)
      if (ier .lt. 0) return
      ipt1 = iblock + 3*lbhb
      ipt2 = ipt1 + nwnew
      if (propa) then
         call factor (n,ndim,n,iwksp(iipnt),jcoef(nwdiag+1),
     a                wksp(ifactr),coef(ndim*nwdiag+1),kblsz,
     a                iwksp(iblock),lbhb,1,omega,wksp(irpnt),ier)
      endif
      if (.not. propa .and. lvfill .eq. 0) then
         call factor (n,n,n,iwksp(iipnt),jcoef(nwdiag+1),
     a                wksp(ifactr),wksp(iwkpt2),kblsz,
     a                iwksp(iblock),lbhb,0,omega,wksp(irpnt),ier)
      endif
      if (lvfill .gt. 0) then 
         call factor (n,n,n,iwksp(ipt1),iwksp(ipt2),wksp(ifactr),
     a                wksp(iwkpt2),kblsz,iwksp(iblock),lbhb,0,
     a                omega,wksp(irpnt),ier)
      endif
      return
      end 
      subroutine fillb (nn,coef,jcoef,iblock,wksp,iwksp,ier)
      implicit double precision (a-h, o-z)
c
c ... fillb calculates block fill-in for block factorization methods. 
c     (symmetric diagonal storage)
c
c ... parameters -- 
c
c       n       order of system
c       coef    floating point matrix coefficient array
c       jcoef   integer matrix coefficient array
c       iblock  array for block information
c       wksp    floating point workspace array
c       iwksp   integer workspace array 
c       ier     error flag
c
c ... specifications for parameters
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblk, ncmax
      dimension coef(1), wksp(1)
      integer   jcoef(1), iblock(3,3), iwksp(1)
c
      n = nn
c
c ... determine block fill-in pattern.
c
      if (lvfill .gt. 0) then 
         lbhbsa = lbhb
         do 25 lv = 1,lvfill
            lbhbl = lbhb
            do 20 j1 = 3,lbhb 
               do 15 j2 = 3,lbhb
                  jd = iblock(1,j1) - iblock(1,j2)
                  if (jd .le. 0) go to 15
                  do 10 j3 = 3,lbhbl
                     if (iblock(1,j3) .eq. jd) go to 15
 10               continue
                  lbhbl = lbhbl + 1
                  iblock(1,lbhbl) = jd
                  iblock(3,lbhbl) = 0
 15            continue
 20         continue
            lbhb = lbhbl
 25      continue
      endif
c
c ... compute constants and check for sufficient workspace. 
c
      call needw ('fillb',1,iblk,3*lbhb,ier)
      if (ier .lt. 0) return
      nwdiag = iblock(3,1)
      nwnew = nwdiag + ltrunc 
      iipnt = iblk + 3*lbhb
      ifactr = irpnt
      nwk = 3*lbhb + maxnz + ltrunc + (lbhb-2)*(2*nwnew-1)
      call needw ('fillb',1,iblk,nwk,ier)
      if (ier .lt. 0) return
      do 30 j = 1,nwnew
 30   iwksp(iipnt+j-1) = j - 1
      iblock(3,1) = nwnew
c
c ... determine diagonal numbers in filled-in block matrix. 
c
      if (lvfill .gt. 0) then 
         jmax = 3
         do 32 j = 3,lbhbsa
            if (iblock(1,j) .gt. iblock(1,jmax)) jmax = j
 32      continue
         jnext = iipnt + nwnew
         do 50 jjc = 3,lbhb
            if (jjc .le. lbhbsa) then
               jstc = iblock(2,jjc)
               mc = iblock(3,jjc)
               j1 = jnext
               do 35 j = 1,mc 
                  iwksp(jnext) = jcoef(nwdiag+jstc+j-1)
                  jnext = jnext + 1
 35            continue
               j2 = jnext - 1 
            endif
            if (jjc .eq. jmax) go to 50 
            jblkc = iblock(1,jjc)
            inc = jblkc*kblsz 
            lim1 = inc - (nwnew - 1)
            lim2 = inc + (nwnew - 1)
            do 45 j = lim1,lim2
               if (jjc .le. lbhbsa) then
                  do 40 jj = j1,j2
                     if (iwksp(jj) .eq. j) go to 45
 40               continue
               endif
               iwksp(jnext) = j
               jnext = jnext + 1
               iblock(3,jjc) = iblock(3,jjc) + 1
 45         continue
 50      continue
         if (lbhb .ge. 4) then
            do 52 jjc = 4,lbhb
 52         iblock(2,jjc) = iblock(2,jjc-1) + iblock(3,jjc-1)
         endif
      endif
c
c ... copy matrix into wksp.
c
      if (propa) then
         nfactr = n*nwnew
         nfacti = 3*lbhb
      endif
      if (.not. propa .and. lvfill .eq. 0) then
         nfactr = n*(maxnz + ltrunc)
         nfacti = 3*lbhb
      endif
      if (lvfill .gt. 0) then 
         ndg = 0
         do 55 j = 1,lbhb
 55      ndg = ndg + iblock(3,j)
         nfactr = n*ndg
         nfacti = ndg + 3*lbhb
      endif
      call needw ('fillb',0,ifactr,nfactr,ier)
      if (ier .lt. 0) return
      call needw ('fillb',1,ifacti,nfacti,ier)
      if (ier .lt. 0) return
      call vfill (nfactr,wksp(ifactr),0.0d0)
      ipt1 = 1
      ipt2 = ifactr 
      do 60 j = 1,nwdiag
         call vcopy (n,coef(ipt1),wksp(ipt2))
         ipt1 = ipt1 + ndim
         ipt2 = ipt2 + n
 60   continue
      iwkpt2 = ifactr + n*nwnew
      ipt2 = iwkpt2 
      if (.not. propa .and. lvfill .eq. 0) then
         do 62 j = nwdiag+1,maxnz
            call vcopy (n,coef(ipt1),wksp(ipt2))
            ipt1 = ipt1 + ndim
            ipt2 = ipt2 + n
 62      continue
      endif
      if (lvfill .gt. 0) then 
         j1 = iipnt + nwnew
         j2 = iipnt + ndg - 1 
         do 70 j = nwdiag+1,maxnz
            jcol = jcoef(j)
            ipt1 = (j - 1)*ndim + 1
            do 65 jj = j1,j2
               if (iwksp(jj) .ne. jcol) go to 65
               ipt2 = iwkpt2 + (jj-j1)*n
               call vcopy (n,coef(ipt1),wksp(ipt2))
               go to 70
 65         continue
 70      continue
      endif
      irpnt = ifactr + nfactr 
      iipnt = ifacti + nfacti 
      return
      end 
      subroutine fillbn (nn,coef,jcoef,iblock,wksp,iwksp,ier)
      implicit double precision (a-h, o-z)
c
c ... fillbn calculates block fill-in for block factorization methods.
c     (nonsymmetric diagonal storage)
c
c ... parameters -- 
c
c       n       order of system
c       coef    floating point matrix coefficient array
c       jcoef   integer matrix coefficient array
c       iblock  array for block information
c       wksp    floating point workspace array
c       iwksp   integer workspace array 
c       ier     error flag
c
c ... specifications for parameters
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblk, ncmax
      dimension coef(1), wksp(1)
      integer   jcoef(1), iblock(3,3), iwksp(1)
c
      n = nn
c
c ... determine block fill-in pattern.
c
      if (lvfill .gt. 0) then 
         lbhbsa = lbhb
         do 25 lv = 1,lvfill
            lbhbl = lbhb
            do 20 j1 = 3,lbhb 
               do 15 j2 = 3,lbhb
                  jd = iblock(1,j1) + iblock(1,j2)
                  if (iblock(1,j1)*iblock(1,j2) .ge. 0) go to 15
                  do 10 j3 = 1,lbhbl
                     if (iblock(1,j3) .eq. jd) go to 15
 10               continue
                  lbhbl = lbhbl + 1
                  iblock(1,lbhbl) = jd
                  iblock(3,lbhbl) = 0
 15            continue
 20         continue
            lbhb = lbhbl
 25      continue
      endif
c
c ... compute constants and check for sufficient workspace. 
c
      call needw ('fillbn',1,iblk,3*lbhb,ier)
      if (ier .lt. 0) return
      ndt = iblock(3,1) - 1
      ndb = iblock(3,2)
      nwdiag = ndt + ndb + 1
      nwnew = nwdiag + 2*ltrunc
      iipnt = iblk + 3*lbhb
      ifactr = irpnt
      nwk = 3*lbhb + maxnz + 2*ltrunc + (lbhb-2)*nwnew
      call needw ('fillbn',1,iblk,nwk,ier)
      if (ier .lt. 0) return
      do 30 j = 1,ndt+ltrunc+1
 30   iwksp(iipnt+j-1) = j - 1
      do 31 j = ndt+ltrunc+2,nwnew
 31   iwksp(iipnt+j-1) = -(j - ndt - ltrunc - 1)
      iblock(3,1) = ndt + ltrunc + 1
      iblock(3,2) = ndb + ltrunc
      iblock(2,2) = iblock(2,1) + iblock(3,1)
c
c ... determine diagonal numbers in filled-in block matrix. 
c
      if (lvfill .gt. 0) then 
         jmax = 3
         jmin = 3
         do 32 j = 3,lbhbsa
            if (iblock(1,j) .gt. iblock(1,jmax)) jmax = j
            if (iblock(1,j) .lt. iblock(1,jmin)) jmin = j
 32      continue
         jnext = iipnt + nwnew
         do 50 jjc = 3,lbhb
            if (jjc .le. lbhbsa) then
               jstc = iblock(2,jjc)
               mc = iblock(3,jjc)
               j1 = jnext
               do 35 j = 1,mc 
                  iwksp(jnext) = jcoef(nwdiag+jstc+j-1)
                  jnext = jnext + 1
 35            continue
               j2 = jnext - 1 
            endif
            if (jjc .eq. jmax .or. jjc .eq. jmin) go to 50
            jblkc = iblock(1,jjc)
            inc = jblkc*kblsz 
            lim1 = inc - (ndb + ltrunc) 
            lim2 = inc + (ndt + ltrunc) 
            do 45 j = lim1,lim2
               if (jjc .le. lbhbsa) then
                  do 40 jj = j1,j2
                     if (iwksp(jj) .eq. j) go to 45
 40               continue
               endif
               iwksp(jnext) = j
               jnext = jnext + 1
               iblock(3,jjc) = iblock(3,jjc) + 1
 45         continue
 50      continue
         if (lbhb .ge. 4) then
            do 52 jjc = 4,lbhb
 52         iblock(2,jjc) = iblock(2,jjc-1) + iblock(3,jjc-1)
         endif
      endif
c
c ... copy matrix into wksp.
c
      if (propa) then
         nfactr = n*nwnew
         nfacti = 3*lbhb
      endif
      if (.not. propa .and. lvfill .eq. 0) then
         nfactr = n*(maxnz + 2*ltrunc)
         nfacti = 3*lbhb
      endif
      if (lvfill .gt. 0) then 
         ndg = 0
         do 55 j = 1,lbhb
 55      ndg = ndg + iblock(3,j)
         nfactr = n*ndg
         nfacti = ndg + 3*lbhb
      endif
      call needw ('fillbn',0,ifactr,nfactr,ier)
      if (ier .lt. 0) return
      call needw ('fillbn',1,ifacti,nfacti,ier)
      if (ier .lt. 0) return
      call vfill (nfactr,wksp(ifactr),0.0d0)
      ipt1 = 1
      ipt2 = ifactr 
      do 60 j = 1,ndt+1
         call vcopy (n,coef(ipt1),wksp(ipt2))
         ipt1 = ipt1 + ndim
         ipt2 = ipt2 + n
 60   continue
      ipt2 = ipt2 + n*ltrunc
      do 61 j = ndt+2,nwdiag
         call vcopy (n,coef(ipt1),wksp(ipt2))
         ipt1 = ipt1 + ndim
         ipt2 = ipt2 + n
 61   continue
      iwkpt2 = ifactr + n*nwnew
      ipt2 = iwkpt2 
      if (.not. propa .and. lvfill .eq. 0) then
         do 62 j = nwdiag+1,maxnz
            call vcopy (n,coef(ipt1),wksp(ipt2))
            ipt1 = ipt1 + ndim
            ipt2 = ipt2 + n
 62      continue
      endif
      if (lvfill .gt. 0) then 
         j1 = iipnt + nwnew
         j2 = iipnt + ndg - 1 
         do 70 j = nwdiag+1,maxnz
            jcol = jcoef(j)
            ipt1 = (j - 1)*ndim + 1
            do 65 jj = j1,j2
               if (iwksp(jj) .ne. jcol) go to 65
               ipt2 = iwkpt2 + (jj-j1)*n
               call vcopy (n,coef(ipt1),wksp(ipt2))
               go to 70
 65         continue
 70      continue
      endif
      irpnt = ifactr + nfactr 
      iipnt = ifacti + nfacti 
      return
      end 
      subroutine bfacmy (methf,factor,coef,jcoef,wksp,iwksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... bfacmy computes a block factorization.
c     (multicolor nonsymmetric diagonal)
c
c ... parameters
c
c       n       order of system
c       nfactr  amount of floating point workspace needed for factorization
c       ier     error flag
c
c ... specifications for parameters
c
c
      external factor
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
      dimension coef(1), wksp(1)
      integer jcoef(2), iwksp(1)
c
      n = nn
      if (methf .le. 2) ivers = 1
      if (methf .gt. 2) ivers = 2
c
c ... calculate constants.
c
      if (ipropa .eq. 0) propa = .false.
      if (ipropa .eq. 1) propa = .true. 
c
c ... calculate fill-in and factor.
c
      call fillbc (n,ncolor,coef,jcoef,iwksp(iblock),wksp,iwksp,ier)
      if (ier .lt. 0) return
      nwdiag = ndt + ndb + 1
      nwnew = nwdiag + 2*ltrunc
      if (methf .eq. 1) nwkp = ncmax*nwnew
      if (methf .eq. 2) nwkp = ncmax*(nwnew + 1)
      if (methf .eq. 3) nwkp = 0
      if (methf .eq. 4) nwkp = n + 2*ncmax
      call needw ('bfacmy',0,irpnt,nwkp,ier)
      if (ier .lt. 0) return
      if (propa) then
         call factor (n,ndim,n,iwksp(iipnt),
     a                iwksp(jcnew+ncolor*nwdiag), 
     a                wksp(ifactr),coef(ndim*nwdiag+1),ncolor,
     a                iwksp(nc),iwksp(iblock),iwksp(lbhb),0,1,
     a                iwksp(ipt),omega,wksp(irpnt),ier)
      endif
      if (.not. propa) then
         call factor (n,n,n,iwksp(iipnt),
     a                iwksp(jcnew+ncolor*nwdiag), 
     a                wksp(ifactr),wksp(iwkpt2),ncolor,
     a                iwksp(nc),iwksp(iblock),iwksp(lbhb),0,0,
     a                iwksp(ipt),omega,wksp(irpnt),ier)
      endif
      return
      end 
      subroutine fillbc (nn,ncolor,coef,jcoef,iblock,wksp,iwksp,ier)
      implicit double precision (a-h, o-z)
c
c ... fillbc sets ups wksp for block factorization methods. 
c     (multicolor nonsymmetric diagonal)
c
c ... parameters -- 
c
c       n       order of system
c       coef    floating point matrix coefficient array
c       jcoef   integer matrix coefficient array
c       iblock  array for block information
c       wksp    floating point workspace array
c       iwksp   integer workspace array 
c       ier     error flag
c
c ... specifications for parameters
c
      common / dscons / ndim, mdim, maxnz
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      common / intern / ndt, ndb, maxt, maxb, ivers, irwise 
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / point  / iptscl, iwkpt1, iwkpt2, iwkpt3
      logical           propa 
      common / cblock / propa, ncol, maxd, nc, ipt, maxnew, 
     a                  jcnew, lbhb, iblk, ncmax
      dimension coef(1), wksp(1)
      integer   jcoef(1), iblock(3,ncolor,3), iwksp(1)
c
      n = nn
c
c ... compute constants and check for sufficient workspace. 
c
      ndt = 0
      ndb = 0
      do 10 j = 1,ncolor
         ndt = max (ndt,iblock(3,j,1)-1)
         ndb = max (ndb,iblock(3,j,2)) 
 10   continue
      nwdiag = ndt + ndb + 1
      nwnew = nwdiag + 2*ltrunc
      ifactr = irpnt
c
c ... copy matrix into wksp.
c
      if (propa) nfactr = n*nwnew
      if (.not. propa) nfactr = n*nwnew + n*(maxd-nwdiag)
      call needw ('fillbc',0,ifactr,nfactr,ier)
      if (ier .lt. 0) return
      call needw ('fillbc',1,iipnt,nwnew*ncolor,ier)
      if (ier .lt. 0) return
      call vfill (nfactr,wksp(ifactr),0.0d0)
      ipt1 = 1
      ipt2 = ifactr 
      do 15 j = 1,ndt+1
         call vcopy (n,coef(ipt1),wksp(ipt2))
         ipt1 = ipt1 + ndim
         ipt2 = ipt2 + n
 15   continue
      ipt2 = ipt2 + n*ltrunc
      do 20 j = ndt+2,nwdiag
         call vcopy (n,coef(ipt1),wksp(ipt2))
         ipt1 = ipt1 + ndim
         ipt2 = ipt2 + n
 20   continue
      iwkpt2 = ifactr + n*nwnew
      ipt2 = iwkpt2 
      if (.not. propa) then
         do 25 j = nwdiag+1,maxd
            call vcopy (n,coef(ipt1),wksp(ipt2))
            ipt1 = ipt1 + ndim
            ipt2 = ipt2 + n
 25      continue
      endif
      irpnt = ifactr + nfactr 
      do 40 ico = 1,ncolor
         do 30 j = 1,ndt+ltrunc+1
 30      iwksp(iipnt+(j-1)*ncolor+ico-1) = j - 1
         do 35 j = ndt+ltrunc+2,nwnew
 35      iwksp(iipnt+(j-1)*ncolor+ico-1) = -(j - ndt - ltrunc - 1)
 40   continue
      do 45 ico = 1,ncolor
         iblock(3,ico,1) = ndt + ltrunc + 1
         iblock(3,ico,2) = ndb + ltrunc 
         iblock(2,ico,2) = iblock(2,ico,1) + iblock(3,ico,1)
 45   continue
      return
      end 
      subroutine blkdef (coef,jcoef,wksp,iwksp,nn,ier)
      implicit double precision (a-h, o-z)
c
c ... blkdef defines various block constants for a constant 
c     block size matrix.
c
c ... parameters -- 
c
c        n        problem size
c
c ... common blocks 
c
      common / dscons / ndim, mdim, maxnz
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / cfactr / nfactr, nfacti, ifactr, ifacti, timfac
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg, 
     a                  ipropa, isymm, ifctv
      logical           propa 
      common / cblock / propa, ncolor, maxd, nc, ipt, maxnew,
     a                  jcnew, lbhb, iblock, ncmax
      integer jcoef(2), iwksp(1)
      dimension coef(1), wksp(1)
c
      n = nn
c
      call needw ('blkdef',1,iipnt,3*(maxnz+1),ier)
      if (ier .lt. 0) return
      call move5 (ndim,n,maxnz,jcoef,coef)
      if (ifact .eq. 0) return
      ifacti = iipnt
      iblock = ifacti
      call defcon (ndim,n,maxnz,jcoef,coef,kblsz,iwksp(ifacti),
     a             lbhb)
      nfacti = 3*lbhb
      iipnt = ifacti + 3*lbhb 
      return
      end 
