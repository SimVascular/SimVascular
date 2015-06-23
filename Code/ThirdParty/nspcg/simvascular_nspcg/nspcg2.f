      subroutine cg (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a               coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a               iparm,rparm,ier)
      implicit double precision (a-h, o-z)
      external suba, subat, subql, subqlt, subqr, subqrt, subadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      ier = 0
      call needw ('cg',0,irpnt,3*n+2*itmax,ier)
      if (ier .lt. 0) return
      nw = lenr - irpnt + 1
      call cgw (suba,subql,coef,jcoef,wksp,iwksp,
     a          n,u,ubar,rhs,wksp(irpnt),nw,iparm,rparm,ier)
      irmax = irpnt + nw - 1
      return
      end
      subroutine si (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a               coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a               iparm,rparm,ier)
      implicit double precision (a-h, o-z)
      external suba, subat, subql, subqlt, subqr, subqrt, subadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      ier = 0
      call needw ('si',0,irpnt,4*n,ier)
      if (ier .lt. 0) return
      nw = lenr - irpnt + 1
      call siw (suba,subql,coef,jcoef,wksp,iwksp,
     a          n,u,ubar,rhs,wksp(irpnt),nw,iparm,rparm,ier)
      irmax = irpnt + nw - 1
      return
      end
      subroutine srcg (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a                 coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier)
      implicit double precision (a-h, o-z)
      external suba, subat, subql, subqlt, subqr, subqrt, subadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      ier = 0
      call needw ('srcg',0,irpnt,3*n+2*itmax,ier)
      if (ier .lt. 0) return
      nw = lenr - irpnt + 1
      call srcgw (suba,subql,subadp,coef,jcoef,wksp,iwksp,
     a            n,u,ubar,rhs,wksp(irpnt),nw,iparm,rparm,ier)
      irmax = irpnt + nw - 1
      return
      end
      subroutine srsi (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a                 coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                 iparm,rparm,ier)
      implicit double precision (a-h, o-z)
      external suba, subat, subql, subqlt, subqr, subqrt, subadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      ier = 0
      call needw ('srsi',0,irpnt,4*n,ier)
      if (ier .lt. 0) return
      nw = lenr - irpnt + 1
      call srsiw (suba,subql,subadp,coef,jcoef,wksp,iwksp,
     a            n,u,ubar,rhs,wksp(irpnt),nw,iparm,rparm,ier)
      irmax = irpnt + nw - 1
      return
      end
      subroutine sor (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a                coef,jcoef,n,u,ubar,rhs,wksp,iwksp,
     a                iparm,rparm,ier)
      implicit double precision (a-h, o-z)
      external suba, subat, subql, subqlt, subqr, subqrt, subadp
      integer   iparm(30), jcoef(2), iwksp(1)
      dimension rhs(1), u(1), ubar(1), rparm(30), coef(1), wksp(1)
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      ier = 0
      call needw ('sor',0,irpnt,2*n,ier)
      if (ier .lt. 0) return
      nw = lenr - irpnt + 1
      call sorw (suba,subql,coef,jcoef,wksp,iwksp,
     a           n,u,ubar,rhs,wksp(irpnt),nw,iparm,rparm,ier)
      irmax = irpnt + nw - 1
      return
      end
      subroutine basic (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a        coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call basicw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine me (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a        coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call mew (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine odir (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call odirw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine omin (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call ominw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine ores (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call oresw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine iom (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call iomw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine gmres (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a         coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call gmresw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine cgnr (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a      coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call cgnrw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine lsqr (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call lsqrw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine usymlq (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call uslqw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine usymqr (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call usqrw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine landir (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call ldirw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine lanmin (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call lminw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine lanres (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call lresw (suba,subat,subql,subqlt,subqr,subqrt,
     a            coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine cgcr (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wk,iwk,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c this routine implements the constrained residual method of
c j. r. wallis, coupled with truncated/restarted orthomin.  for
c further information about the algorithm, see "constrained residual
c acceleration of conjugate residual methods", by j. r. wallis,
c r. p. kendall and t. e. little of j. s. nolen and assocs. inc.;
c report spe 13536, society of petroleum engineers, 1985.
c
c right preconditioning only is allowed in this algorithm.
c
c unfortunately, this routine is limited -- all blocks must be the
c same size.  but the idea can be easily generalized.
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wk(1), iwk(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
      external nullpl, cgcrpr
      logical ipl, ipr, iql, iqr
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz, lvfill, ltrunc, ndeg,
     a                  ipropa, isymm, ifctv
c
c ... data common blocks
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / ccgcr  / nblk, nband, ictac, ieta, ivcgcr
c
c time to proceed ...
c
      if (nstore .ne. 2 .and. nstore .ne. 3) go to 998
c
      irpsav = irpnt
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iql) go to 998
c
      ipl = .false.
      ipr = .true.
      iplr = 0
      if (ipl) iplr = iplr + 1
      if (ipr) iplr = iplr + 2
c
c form the c**(t)*a*c matrix
c
 1    if (nbl1d .le. 0 .or. nbl2d .le. 0) go to 998
      nbl0d = 1
      if (mod(nbl2d,nbl1d) .ne. 0 .or. mod(nbl1d,nbl0d) .ne. 0) 
     a          go to 998
      nblk = n / nbl2d
      if (nblk .eq. 1) nblk = n / nbl1d
      ictac = irpnt
      nwgb = lenr - ictac + 1
      ierpp = 0
      call getblk (coef,jcoef,n,nblk,nband,wk(ictac),nwgb,ierpp)
      irmax = max (irmax,ictac-1+nwgb)
      if (ierpp .lt. 0) go to 999
      irpnt = ictac + nblk*nband
c
c perform first-iterate calculations
c
      ieta = irpnt
      ivcgcr = ieta + nblk
      iv2 = ivcgcr + n
      irmax = max(irmax,iv2-1+n)
      if (irmax .gt. lenr) go to 997
c
      call suba (coef,jcoef,wk,iwk,n,u,wk(ivcgcr))
      call vexopy (n,wk(ivcgcr),rhs,wk(ivcgcr),2)
      call tmult (n,nblk,nband,wk(ictac),wk(ieta),wk(ivcgcr),
     a            wk(ivcgcr))
      call vexopy (n,u,u,wk(ivcgcr),1)
c
c pass it on to orthomin ...
c
      irpnt = iv2
      nw = lenr - irpnt + 1
      call omingw (suba,subql,subqr,nullpl,cgcrpr,coef,jcoef,
     a            wk,iwk,n,u,ubar,rhs,wk(irpnt),nw,iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
c
      irpnt = irpsav
      return
c
c error returns ...
c
c insuff. floating point workspace ...
 997  ier = -2
      call ershow (ier,'cgcr')
      return
c
c unimplemented option ...
 998  ier = -16
      call ershow (ier,'cgcr')
      return
c
c generic handler ...
 999  ier = ierpp
      return
      end
      subroutine bcgs (suba,subat,subql,subqlt,subqr,subqrt,subadp,
     a coef,jcoef,n,u,ubar,rhs,wksp,iwksp,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
      dimension u(1), ubar(1), rhs(1), coef(1), jcoef(2),
     a          wksp(1), iwksp(1)
      dimension iparm(30), rparm(30)
      external suba,  subql,  subqr
      external subat, subqlt, subqrt
      external subadp
c
c ... data common blocks
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
c
      nw = lenr - irpnt + 1
      call bcgsw (suba,subql,subqr,coef,jcoef,
     a            wksp,iwksp,n,u,ubar,rhs,wksp(irpnt),nw,
     a            iparm,rparm,ier)
      irmax = max (irmax,irpnt-1+nw)
      iimax = max (iimax,iipnt-1)
      return
      end
      subroutine cgw (suba,subq,coef,jcoef,wfac,jwfac,nn,u,ubar,rhs,
     a                wksp,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c     cgw drives the conjugate gradient algorithm.
c
c ... parameters --
c
c          suba   matrix-vector multiplication routine
c          subq   preconditioning routine
c          n      input integer.  order of the system (= nn)
c          u      input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c          ubar   input vector containing the true solution
c                  (optional)
c          rhs    input vector.  contains the right hand side
c                 of the matrix problem.
c          wksp   vector used for working space.
c          nw     length of wksp array.  if this length is less than
c                  the amount needed, nw will give the needed amount
c                  upon output.
c          iparm  integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method.
c          rparm  floating point vector of length 30.  allows user to
c                 specify some floating point parameters which affect
c                 the method.
c          ier    output integer.  error flag.
c
c ... specifications for parameters
c
      external  suba, subq
      integer   iparm(30), jcoef(2), jwfac(1)
      dimension rhs(1), u(1), ubar(1), wksp(1), rparm(30), coef(1),
     a          wfac(1)
c
c *** begin -- package common
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom3 / alpha, beta, zeta, emax, emin, pap,
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- package common
c
c ... initialize common blocks
c
      ier = 0
      n = nn
      t1 = timer (dummy)
      iacel = 1
      timit = 0.0d0
      digit1 = 0.0d0
      digit2 = 0.0d0
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 35
      if (level .ge. 2) write (nout,10)
 10   format (1x,'cg')
c
c ... compute workspace base addresses and check for sufficient
c ... workspace.
c
      iw1 = 1
      iw2 = iw1 + n
      iw3 = iw2 + n
      iw4 = iw3 + n
      nwksp = 3*n + 2*itmax
      if (nw .ge. nwksp) go to 15
      ier = -2
      call ershow (ier,'cgw')
      go to 30
 15   continue
      call nmcalc (coef,jcoef,wfac,jwfac,1,subq,n,rhs,ubar,wksp,ier)
      if (ier .lt. 0) go to 30
c
c ... zero out workspace
c
      call vfill (nwksp,wksp,0.0d0)
c
c ... iteration sequence
c
      call itcg (suba,subq,coef,jcoef,wfac,jwfac,n,u,ubar,rhs,
     a           wksp(iw1),wksp(iw2),wksp(iw3),wksp(iw4),ier)
c
      if (ier .lt. 0  .or.  ier .eq. 1) go to 25
c
c ... method has converged
c
      if (level .ge. 1) write (nout,20) in
 20   format (/1x,'cg  has converged in ',i5,' iterations' )
c
c ... optional error analysis
c
 25   if (idgts .lt. 0) go to 30
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wksp,digit1,
     a             digit2,idgts)
c
c ... set return parameters in iparm and rparm
c
 30   t2 = timer (dummy)
      nw = 3*n + 2*in
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
      rparm(9) = omega
      rparm(10) = alphab
      rparm(11) = betab
      rparm(12) = specr
c
 35   continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
c
      return
      end
      subroutine siw (suba,subq,coef,jcoef,wfac,jwfac,nn,u,ubar,rhs,
     a                wksp,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c     siw drives the chebyshev acceleration algorithm.
c
c ... parameters --
c
c          suba   matrix-vector multiplication routine
c          subq   preconditioning routine
c          n      input integer.  order of the system (= nn)
c          u      input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c          ubar   input vector containing the true solution
c                  (optional)
c          rhs    input vector.  contains the right hand side
c                 of the matrix problem.
c          wksp   vector used for working space.
c          nw     length of wksp array.  if this length is less than
c                  the amount needed, nw will give the needed amount
c                  upon output.
c          iparm  integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method.
c          rparm  floating point vector of length 30.  allows user to
c                 specify some floating point parameters which affect
c                 the method.
c          ier    output integer.  error flag.
c
c ... specifications for parameters
c
      external  suba, subq
      integer   iparm(30), jcoef(2), jwfac(1)
      dimension rhs(1), u(1), ubar(1), wksp(1), rparm(30), coef(1),
     a          wfac(1)
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
c ... initialize common blocks
c
      ier = 0
      n = nn
      t1 = timer (dummy)
      iacel = 2
      timit = 0.0d0
      digit1 = 0.0d0
      digit2 = 0.0d0
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 35
      if (level .ge. 2) write (nout,10)
 10   format (1x,'si')
c
c ... compute workspace base addresses and check for sufficient
c ... workspace.
c
      iw1 = 1
      iw2 = iw1 + n
      iw3 = iw2 + n
      iw4 = iw3 + n
      nwksp = 4*n
      if (nw .ge. nwksp) go to 15
      ier = -2
      call ershow (ier,'siw')
      go to 30
 15   continue
      call nmcalc (coef,jcoef,wfac,jwfac,1,subq,n,rhs,ubar,wksp,ier)
      if (ier .lt. 0) go to 30
c
c ... compute an initial rayleigh quotient and adjust emax, emin.
c
      call vfill (n,wksp,1.0d0)
      call subq (coef,jcoef,wfac,jwfac,n,wksp,wksp(iw2))
      call suba (coef,jcoef,wfac,jwfac,n,wksp(iw2),wksp(iw3))
      rq = vdot (n,wksp(iw2),wksp(iw3)) /
     a                    vdot (n,wksp(iw2),wksp)
      rqmax = rq
      rqmin = rq
      if (maxadd) emax = max (emax,rqmax)
      if (minadd) emin = min (emin,rqmin)
      if (minadd) emin = max (emin,0.0d0)
c
c ... zero out workspace
c
      call vfill (nwksp,wksp,0.0d0)
c
c ... iteration sequence
c
      call itsi (suba,subq,coef,jcoef,wfac,jwfac,n,u,ubar,rhs,
     a           wksp(iw1),wksp(iw2),wksp(iw3),wksp(iw4),ier)
c
      if (ier .lt. 0  .or.  ier .eq. 1) go to 25
c
c ... method has converged
c
      if (level .ge. 1) write (nout,20) in
 20   format (/1x,'si  has converged in ',i5,' iterations ')
c
c ... optional error analysis
c
 25   if (idgts .lt. 0) go to 30
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wksp,digit1,
     a             digit2,idgts)
c
c ... set return parameters in iparm and rparm
c
 30   t2 = timer (dummy)
      nw = 4*n
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
      rparm(9) = omega
      rparm(10) = alphab
      rparm(11) = betab
      rparm(12) = specr
c
 35   continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
c
      return
      end
      subroutine srcgw (suba,subq,subadp,coef,jcoef,wfac,jwfac,
     a                  nn,u,ubar,rhs,wksp,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c     srcgw drives the ssor conjugate gradient algorithm.
c
c ... parameters --
c
c          suba   matrix-vector multiplication routine
c          subq   preconditioning routine
c          subadp adpation routine
c          n      input integer.  order of the system (= nn)
c          u      input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c          ubar   input vector containing the true solution
c                  (optional)
c          rhs    input vector.  contains the right hand side
c                 of the matrix problem.
c          wksp   vector used for working space.
c          nw     length of wksp array.  if this length is less than
c                  the amount needed, nw will give the needed amount
c                  upon output.
c          iparm  integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method.
c          rparm  floating point vector of length 30.  allows user to
c                 specify some floating point parameters which affect
c                 the method.
c          ier    output integer.  error flag.
c
c ... specifications for parameters
c
      external  suba, subq, subadp
      integer   iparm(30), jcoef(2), jwfac(1)
      dimension rhs(1), u(1), ubar(1), wksp(1), rparm(30), coef(1),
     a          wfac(1)
c
c *** begin -- package common
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom3 / alpha, beta, zeta, emax, emin, pap,
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- package common
c
c ... initialize common blocks
c
      ier = 0
      n = nn
      t1 = timer (dummy)
      iacel = 1
      timit = 0.0d0
      digit1 = 0.0d0
      digit2 = 0.0d0
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 35
      if (level .ge. 2) write (nout,10)
 10   format (1x,'srcg')
c
c ... compute workspace base addresses and check for sufficient
c ... workspace.
c
      iw1 = 1
      iw2 = iw1 + n
      iw3 = iw2 + n
      iw4 = iw3 + n
      nwksp = 3*n + 2*itmax
      if (nw .ge. nwksp) go to 15
      ier = -2
      call ershow (ier,'srcgw')
      go to 30
 15   continue
c
c ... zero out workspace
c
      call vfill (nwksp,wksp,0.0d0)
c
c ... iteration sequence
c
      call itsrcg (suba,subq,subadp,coef,jcoef,wfac,jwfac,n,u,ubar,
     a             rhs,wksp(iw1),wksp(iw2),wksp(iw3),wksp(iw4),ier)
c
      if (ier .lt. 0  .or.  ier .eq. 1) go to 25
c
c ... method has converged
c
      if (level .ge. 1) write (nout,20) in
 20   format (/1x,'srcg has converged in ',i5,' iterations' )
c
c ... optional error analysis
c
 25   if (idgts .lt. 0) go to 30
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wksp,digit1,
     a             digit2,idgts)
c
c ... set return parameters in iparm and rparm
c
 30   t2 = timer (dummy)
      timit = t2 - t1
      nw = 3*n + 2*in
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
      rparm(9) = omega
      rparm(10) = alphab
      rparm(11) = betab
      rparm(12) = specr
c
 35   continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
c
      return
      end
      subroutine srsiw (suba,subq,subadp,coef,jcoef,wfac,jwfac,
     a                  nn,u,ubar,rhs,wksp,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c     srsiw drives the ssor chebyshev acceleration algorithm.
c
c ... parameters --
c
c          suba   matrix-vector multiplication routine
c          subq   preconditioning routine
c          subadp adpation routine
c          n      input integer.  order of the system (= nn)
c          u      input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c          ubar   input vector containing the true solution
c                  (optional)
c          rhs    input vector.  contains the right hand side
c                 of the matrix problem.
c          wksp   vector used for working space.
c          nw     length of wksp array.  if this length is less than
c                  the amount needed, nw will give the needed amount
c                  upon output.
c          iparm  integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method.
c          rparm  floating point vector of length 30.  allows user to
c                 specify some floating point parameters which affect
c                 the method.
c          ier    output integer.  error flag.
c
c ... specifications for parameters
c
      external  suba, subq, subadp
      integer   iparm(30), jcoef(2), jwfac(1)
      dimension rhs(1), u(1), ubar(1), wksp(1), rparm(30), coef(1),
     a          wfac(1)
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
c ... initialize common blocks
c
      ier = 0
      n = nn
      t1 = timer (dummy)
      iacel = 2
      timit = 0.0d0
      digit1 = 0.0d0
      digit2 = 0.0d0
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 35
      if (level .ge. 2) write (nout,10)
 10   format (1x,'srsi')
c
c ... compute workspace base addresses and check for sufficient
c ... workspace.
c
      iw1 = 1
      iw2 = iw1 + n
      iw3 = iw2 + n
      iw4 = iw3 + n
      nwksp = 4*n
      if (nw .ge. nwksp) go to 15
      ier = -2
      call ershow (ier,'srsiw')
      go to 30
 15   continue
c
c ... compute an initial rayleigh quotient and adjust emax, emin.
c
      call vfill (n,wksp,1.0d0)
      call subq (coef,jcoef,wfac,jwfac,n,wksp,wksp(iw2))
      call suba (coef,jcoef,wfac,jwfac,n,wksp(iw2),wksp(iw3))
      rq = vdot (n,wksp(iw2),wksp(iw3)) /
     a                    vdot (n,wksp(iw2),wksp)
      rqmax = 1.0d0
      rqmin = rq
c
c ... adjust emax, emin.
c
      emax = 1.0d0
      maxadd = .false.
      if (minadd) emin = min (emin,rqmin)
      if (minadd) emin = max (emin,0.0d0)
c
c ... zero out workspace
c
      call vfill (nwksp,wksp,0.0d0)
c
c ... iteration sequence
c
      call itsrsi (suba,subq,subadp,coef,jcoef,wfac,jwfac,n,u,ubar,
     a             rhs,wksp(iw1),wksp(iw2),wksp(iw3),wksp(iw4),ier)
c
      if (ier .lt. 0  .or.  ier .eq. 1) go to 25
c
c ... method has converged
c
      if (level .ge. 1) write (nout,20) in
 20   format (/1x,'srsi  has converged in ',i5,' iterations ')
c
c ... optional error analysis
c
 25   if (idgts .lt. 0) go to 30
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wksp,digit1,
     a             digit2,idgts)
c
c ... set return parameters in iparm and rparm
c
 30   t2 = timer (dummy)
      timit = t2 - t1
      nw = 4*n
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
      rparm(9) = omega
      rparm(10) = alphab
      rparm(11) = betab
      rparm(12) = specr
c
 35   continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
c
      return
      end
      subroutine sorw (suba,subq,coef,jcoef,wfac,jwfac,nn,
     a                 u,ubar,rhs,wksp,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c     sorw drives the successive over-relaxation algorithm.
c
c ... parameters --
c
c          suba   matrix-vector multiplication routine
c          subq   routine to do an sor pass
c          n      input integer.  order of the system (= nn)
c          u      input/output vector.  on input, u contains the
c                 initial guess to the solution.  on output, it
c                 contains the latest estimate to the solution.
c          ubar   input vector containing the true solution
c                  (optional)
c          rhs    input vector.  contains the right hand side
c                 of the matrix problem.
c          wksp   vector used for working space.
c          nw     length of wksp array.  if this length is less than
c                  the amount needed, nw will give the needed amount
c                  upon output.
c          iparm  integer vector of length 30.  allows user to
c                 specify some integer parameters which affect
c                 the method.
c          rparm  floating point vector of length 30.  allows user to
c                 specify some floating point parameters which affect
c                 the method.
c          ier    output integer.  error flag.
c
c ... specifications for parameters
c
      external  suba, subq
      integer   iparm(30), jcoef(2), jwfac(1)
      dimension rhs(1), u(1), ubar(1), wksp(1), rparm(30), coef(1),
     a          wfac(1)
c
c *** begin -- package common
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom3 / alpha, beta, zeta, emax, emin, pap,
     b                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     b                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     b                  bnorm, bnorm1
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- package common
c
c ... initialize common blocks
c
      ier = 0
      n = nn
      t1 = timer (dummy)
      iacel = 3
      timit = 0.0d0
      digit1 = 0.0d0
      digit2 = 0.0d0
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 35
      if (level .ge. 2) write (nout,10)
 10   format (1x,'sor')
c
c ... compute workspace base addresses and check for sufficient
c ... workspace.
c
      nwksp = 2*n
      if (nw .ge. nwksp) go to 15
      ier = -2
      call ershow (ier,'sorw')
      go to 30
c
c ... zero out workspace
c
 15   call vfill (nwksp,wksp,0.0d0)
c
c ... iteration sequence
c
      call itsor (subq,coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wksp,ier)
c
      if (ier .lt. 0  .or.  ier .eq. 1) go to 25
c
c ... method has converged
c
      if (level .ge. 1) write (nout,20) in
 20   format (/1x,'sor  has converged in ',i5,' iterations' )
c
c ... optional error analysis
c
 25   if (idgts .lt. 0) go to 30
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wksp,digit1,
     a             digit2,idgts)
c
c ... set return parameters in iparm and rparm
c
 30   t2 = timer (dummy)
      timit = t2 - t1
      nw = 2*n
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
      rparm(9) = omega
      rparm(10) = alphab
      rparm(11) = betab
      rparm(12) = specr
c
 35   continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
c
      return
      end
      subroutine itcg (suba,subq,coef,jcoef,wfac,jwfac,nn,u,ubar,
     a                 rhs,r,p,z,tri,ier)
      implicit double precision (a-h, o-z)
c
c     itcg does the conjugate gradient iterations.
c
c ... parameters --
c
c         suba      matrix-vector multiplication routine
c         subq      preconditioning routine
c         n         order of system (= nn)
c         u         current solution
c         ubar      known solution (optional)
c         rhs       right hand side vector
c         r,p,z     workspace vectors of length n each
c         tri       tridiagonal matrix associated with the
c                    eigenvalues of the tridiagonal matrix.
c         ier       error code
c
c ... specifications for parameters
c
      external suba, subq
      integer jcoef(2), jwfac(1)
      dimension coef(1), wfac(1)
      dimension u(1), ubar(1), rhs(1), r(1), p(1), z(1), tri(1)
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
      common / itcom9 / rdot, rzdot, rztdot, zdot, zztdot, ztdot,
     a           rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a           udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
      logical rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a        udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
c
c *** end   -- package common
c
      n = nn
      in = 0
      is = 0
      rzdot = 0.0d0
      alpha = 0.0d0
      beta = 0.0d0
      alphao = 0.0d0
      maxadp = maxadd
      minadp = minadd
c
c     compute r = residual
c
      call suba (coef,jcoef,wfac,jwfac,n,u,r)
      do 10 i = 1,n
 10   r(i) = rhs(i) - r(i)
      go to 25
c
c***** begin iteration loop *****
c
 15   do 20 i = 1,n
 20   r(i) = r(i) - alpha*z(i)
c
c ... do preconditioning step -- solve q*z = r for z.
c
 25   call subq (coef,jcoef,wfac,jwfac,n,r,z)
c
c ... compute rzdot = (r,z)
c
      dkm1 = rzdot
      rzdot = 0.0d0
      do 30 i = 1,n
 30   rzdot = rzdot + r(i)*z(i)
      if (rzdot .gt. 0.0d0) go to 35
      ier = -7
      call ershow (ier,'itcg')
      return
c
c ... determine whether or not to stop.
c
 35   call pstops (n,r,z,u,ubar,ier)
      if (level .ge. 2) call iterm (n,u)
      if (halt  .or.  ier .lt. 0) return
      if (in .lt. itmax) go to 40
      ier = 1
      call ershow (ier,'itcg')
      zeta = stptst
      return
c
c ... compute   beta = rzdot/dkm1
c
 40   if (in .eq. 0) go to 45
      beta = rzdot/dkm1
c
c ... compute   p = z + beta*p
c
 45   do 50 i = 1,n
 50   p(i) = z(i) + beta*p(i)
c
c ... compute   alpha = rzdot / (p,a*p)
c
      call suba (coef,jcoef,wfac,jwfac,n,p,z)
      alphao = alpha
      pap = 0.0d0
      do 55 i = 1,n
 55   pap = pap + p(i)*z(i)
      alpha = rzdot / pap
      if (pap .gt. 0.0d0) go to 60
      ier = -6
      call ershow (ier,'itcg')
      return
c
c ... compute latest eigenvalue estimates.
c
 60   if (maxadp .or. minadp) call chgcon (tri,ier)
c
c ... compute new solution   u = u + alpha*p
c
      do 65 i = 1,n
 65   u(i) = u(i) + alpha*p(i)
      in = in + 1
      is = is + 1
      go to 15
      end
      subroutine itsi (suba,subq,coef,jcoef,wfac,jwfac,nn,u,ubar,
     a                 rhs,r,p,z,wksp,ier)
      implicit double precision (a-h, o-z)
c
c     itsi does the semi-iterative iterations.
c
c ... parameters --
c
c         suba      matrix-vector multiplication routine
c         subq      preconditioning routine
c         n         order of system (= nn)
c         u         current solution
c         ubar      known solution (optional)
c         rhs       right hand side vector
c         r,p,z,    workspace vectors of length n each
c         wksp      volatile workspace
c         ier       error code
c
c ... specifications for parameters
c
      external suba, subq
      integer jcoef(2), jwfac(1)
      dimension coef(1), wfac(1)
      dimension u(1), ubar(1), rhs(1), r(1), p(1), z(1), wksp(1)
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
      common / itcom9 / rdot, rzdot, rztdot, zdot, zztdot, ztdot,
     a           rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a           udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
      logical rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a        udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
c
c *** end   -- package common
c
      n = nn
      in = 0
c
c ... new chebychev sequence.
c
 10   is = 0
      alpha = 0.0d0
      beta = 0.0d0
      rho = 1.0d0
      rzdot = 0.0d0
      gamma = 2.0d0/(emax + emin)
      sigma = (emax - emin)/(emax + emin)
      term = sqrt (1.0d0 - sigma*sigma)
      rr = (1.0d0 - term)/(1.0d0 + term)
      maxadp = maxadd
      minadp = minadd
c
c     compute r = residual
c
      call suba (coef,jcoef,wfac,jwfac,n,u,r)
      do 15 i = 1,n
 15   r(i) = rhs(i) - r(i)
      go to 30
c
c***** begin iteration loop *****
c
 20   do 25 i = 1,n
 25   r(i) = r(i) - alpha*z(i)
c
c ... do preconditioning step -- solve q*z = r for z.
c
 30   call subq (coef,jcoef,wfac,jwfac,n,r,z)
c
c ... compute rzdot = (r,z)
c
      dkm1 = rzdot
      rzdot = 0.0d0
      do 35 i = 1,n
 35   rzdot = rzdot + r(i)*z(i)
      if (is .eq. 0) dkq = rzdot
      if (rzdot .ge. 0.0d0) go to 40
      ier = -7
      call ershow (ier,'itsi')
      return
c
c ... determine whether or not to stop.
c
 40   call pstops (n,r,z,u,ubar,ier)
      if (level .ge. 2) call iterm (n,u)
      if (halt  .or.  ier .lt. 0) return
      if (in .lt. itmax) go to 45
      ier = 1
      call ershow (ier,'itsi')
      zeta = stptst
      return
c
c ... compute iteration parameters.
c
 45   call parsi
c
c ... compute   p = z + beta*p
c ...           u = u + alpha*p
c
      do 50 i = 1,n
         p(i) = z(i) + beta*p(i)
         u(i) = u(i) + alpha*p(i)
 50   continue
c
c ... adapt on emin and emax
c
      in = in + 1
      if (.not. maxadp  .and.  .not. minadp) go to 55
      call chgsi (suba,coef,jcoef,wfac,jwfac,n,z,wksp,icode,ier)
      if (ier .lt. 0) return
c
c ... check if new estimates of emax, emin are to be used.
c
      if (icode .eq. 1) go to 10
c
c ... estimates of emax, emin are still good.
c
 55   is = is + 1
      call suba (coef,jcoef,wfac,jwfac,n,p,z)
      go to 20
      end
      subroutine itsrcg (suba,subq,subadp,coef,jcoef,wfac,jwfac,
     a                   nn,u,ubar,rhs,r,p,z,tri,ier)
      implicit double precision (a-h, o-z)
c
c     itsrcg does the ssor conjugate gradient iterations.
c
c ... parameters --
c
c         suba      matrix-vector multiplication routine
c         subq      preconditioning routine
c         subadp    adpation routine
c         n         order of system (= nn)
c         u         current solution
c         ubar      known solution (optional)
c         rhs       right hand side vector
c         r,p,z     workspace vectors of length n each
c         tri       tridiagonal matrix associated with the
c                    eigenvalues of the tridiagonal matrix.
c         ier       error code
c
c ... specifications for parameters
c
      external suba, subq, subadp
      integer jcoef(2), jwfac(1)
      dimension coef(1), wfac(1)
      dimension u(1), ubar(1), rhs(1), r(1), p(1), z(1), tri(1)
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
      common / itcom9 / rdot, rzdot, rztdot, zdot, zztdot, ztdot,
     a           rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a           udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
      logical rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a        udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
c
c *** end   -- package common
c
      n = nn
      in = 0
      isw = 1
 5    is = 0
      rzdot = 0.0d0
      alpha = 0.0d0
      beta = 0.0d0
      alphao = 0.0d0
      maxadp = maxadd
      minadp = minadd
c
c     recompute bnorm
c
      call nmcalc (coef,jcoef,wfac,jwfac,isw,subq,n,rhs,ubar,r,ier)
      if (ier .lt. 0) return
      isw = 2
c
c     compute r = residual
c
      call suba (coef,jcoef,wfac,jwfac,n,u,r)
      do 10 i = 1,n
 10   r(i) = rhs(i) - r(i)
      go to 25
c
c***** begin iteration loop *****
c
 15   do 20 i = 1,n
 20   r(i) = r(i) - alpha*z(i)
c
c ... do preconditioning step -- solve q*z = r for z.
c
 25   call subq (coef,jcoef,wfac,jwfac,n,r,z)
c
c ... compute rzdot = (r,z)
c
      dkm1 = rzdot
      rzdot = 0.0d0
      do 30 i = 1,n
 30   rzdot = rzdot + r(i)*z(i)
      if (rzdot .ge. 0.0d0) go to 35
      ier = -7
      call ershow (ier,'itsrcg')
      return
c
c ... determine whether or not to stop.
c
 35   call pstops (n,r,z,u,ubar,ier)
      if (level .ge. 2) call iterm (n,u)
      if (halt  .or.  ier .lt. 0) return
      if (in .lt. itmax) go to 40
      ier = 1
      call ershow (ier,'itsrcg')
      zeta = stptst
      return
c
c ... compute   beta = rzdot/dkm1
c
 40   if (is .eq. 0) go to 45
      beta = rzdot/dkm1
c
c ... compute   p = z + beta*p
c
 45   do 50 i = 1,n
 50   p(i) = z(i) + beta*p(i)
c
c ... compute   alpha = rzdot / (p,a*p)
c
      call suba (coef,jcoef,wfac,jwfac,n,p,z)
      alphao = alpha
      pap = 0.0d0
      do 55 i = 1,n
 55   pap = pap + p(i)*z(i)
      alpha = rzdot / pap
      if (pap .ge. 0.0d0) go to 60
      ier = -6
      call ershow (ier,'itsrcg')
      return
c
c ... compute latest eigenvalue estimates.
c
 60   if (minadp) call chgcon (tri,ier)
c
c ... compute new solution   u = u + alpha*p
c
      do 65 i = 1,n
 65   u(i) = u(i) + alpha*p(i)
      is = is + 1
      in = in + 1
      call ssorad (subadp,coef,jcoef,wfac,jwfac,n,p,z,r,icode)
      if (icode .eq. 0) go to 15
      go to 5
      end
      subroutine itsrsi (suba,subq,subadp,coef,jcoef,wfac,jwfac,
     a                   nn,u,ubar,rhs,r,p,z,wksp,ier)
      implicit double precision (a-h, o-z)
c
c     itsrsi does the ssor semi-iterative iterations.
c
c ... parameters --
c
c         suba      matrix-vector multiplication routine
c         subq      preconditioning routine
c         subadp    adpation routine
c         n         order of system (= nn)
c         u         current solution
c         ubar      known solution (optional)
c         rhs       right hand side vector
c         r,p,z,    workspace vectors of length n each
c         wksp      volatile workspace
c         ier       error code
c
c ... specifications for parameters
c
      external suba, subq, subadp
      integer jcoef(2), jwfac(1)
      dimension coef(1), wfac(1)
      dimension u(1), ubar(1), rhs(1), r(1), p(1), z(1), wksp(1)
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
      in = 0
      isw = 1
c
c     recompute bnorm
c
 5    call nmcalc (coef,jcoef,wfac,jwfac,isw,subq,n,rhs,ubar,r,ier)
      if (ier .lt. 0) return
      isw = 2
c
c ... update rayleigh quotient .
c
      if (in .eq. 0) go to 10
      call subq (coef,jcoef,wfac,jwfac,n,p,z)
      call suba (coef,jcoef,wfac,jwfac,n,z,r)
      rq = vdot (n,z,r) / vdot (n,z,p)
      rqmin = rq
      if (minadd) emin = rqmin
c
c ... new chebychev sequence.
c
 10   is = 0
      alpha = 0.0d0
      beta = 0.0d0
      rho = 1.0d0
      rzdot = 0.0d0
      gamma = 2.0d0/(emax + emin)
      sigma = (emax - emin)/(emax + emin)
      term = sqrt (1.0d0 - sigma*sigma)
      rr = (1.0d0 - term)/(1.0d0 + term)
      minadp = minadd
c
c     compute r = residual
c
      call suba (coef,jcoef,wfac,jwfac,n,u,r)
      do 15 i = 1,n
 15   r(i) = rhs(i) - r(i)
      go to 30
c
c***** begin iteration loop *****
c
 20   do 25 i = 1,n
 25   r(i) = r(i) - alpha*z(i)
c
c ... do preconditioning step -- solve q*z = r for z.
c
 30   call subq (coef,jcoef,wfac,jwfac,n,r,z)
c
c ... compute rzdot = (r,z)
c
      dkm1 = rzdot
      rzdot = 0.0d0
      do 35 i = 1,n
 35   rzdot = rzdot + r(i)*z(i)
      if (is .eq. 0) dkq = rzdot
      if (rzdot .ge. 0.0d0) go to 40
      ier = -7
      call ershow (ier,'itsrsi')
      return
c
c ... determine whether or not to stop.
c
 40   call pstops (n,r,z,u,ubar,ier)
      if (level .ge. 2) call iterm (n,u)
      if (halt  .or.  ier .lt. 0) return
      if (in .lt. itmax) go to 45
      ier = 1
      call ershow (ier,'itsrsi')
      zeta = stptst
      return
c
c ... compute iteration parameters.
c
 45   call parsi
c
c ... compute   p = z + beta*p
c ...           u = u + alpha*p
c
      do 50 i = 1,n
         p(i) = z(i) + beta*p(i)
         u(i) = u(i) + alpha*p(i)
 50   continue
c
c ... adapt on emin and emax
c
      in = in + 1
      if (.not. minadp) go to 55
      call chgsi (suba,coef,jcoef,wfac,jwfac,n,z,wksp,icode,ier)
      if (ier .lt. 0) return
c
c ... check if new estimates of emax, emin are to be used.
c
      if (icode .eq. 1) go to 10
c
c ... estimates of emax, emin are still good.
c
 55   is = is + 1
      call suba (coef,jcoef,wfac,jwfac,n,p,z)
      call ssorad (subadp,coef,jcoef,wfac,jwfac,n,p,z,r,icode)
      if (icode .eq. 0) go to 20
      go to 5
      end
      subroutine itsor (subq,coef,jcoef,wfac,jwfac,nn,u,ubar,
     a                  rhs,wksp,ier)
      implicit double precision (a-h, o-z)
c
c ... itsor does the sor iterations
c
c ... parameters --
c
c          subq   routine to do an sor pass
c          n      size of system
c          rhs    right hand side
c          u      solution vector
c          ubar   known solution (optional)
c          wksp   workspace vector of length 2*n
c
c ... specifications for parameters
c
      integer jcoef(2), jwfac(1)
      dimension coef(1), wfac(1)
      dimension rhs(1), u(1), ubar(1), wksp(1)
      external subq
      logical  change
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
      logical           omgadp
      common / itcom5 / omega, alphab, betab, fff, specr, omgadp
c
c *** end   -- itpack common
c
c
c ... set initial parameters not already set
c
      n = nn
      in = 0
      is = 0
      ip = 0
      iss = 0
      iphat = 2
      delnnm = 0.0d0
      delsnm = 0.0d0
      call sorstp (n,u,ubar,0.0d0,0.0d0)
      change = omgadp
      ib2 = n + 1
      if (.not. omgadp) go to 10
      omegap = omega
      omega = 1.0d0
      ipstar = 4
      if (omegap .le. 1.0d0) change = .false.
c
c ... start iterating.
c
 10   do 55 iter = 1,itmax+1
c
c ... output intermediate information
c
         if (level .ge. 2) call iterm (n,u)
         if (halt) return
         if (.not. change) go to 15
         change = .false.
         is = is + 1
         ip = 0
         iss = 0
         omega = min (omegap,tau(is))
         iphat = max ( 3 , int ( (omega-1.0d0)/(2.0d0-omega) ) )
         ipstar = ipstr (omega)
c
c ... compute u (in + 1) and norm of del(s,p)
c
 15      delsnm = delnnm
         spcrm1 = specr
         do 20 i = 1,n
 20      wksp(i) = rhs(i)
         call subq (coef,jcoef,wfac,jwfac,n,u,wksp,wksp(ib2))
         do 25 i = 1,n
 25      wksp(i) = u(i) - wksp(n+i)
         sum = 0.0d0
         do 28 i = 1,n
 28      sum = sum + wksp(i)*wksp(i)
         delnnm = sqrt (sum)
         do 30 i = 1,n
 30      u(i) = wksp(i+n)
         if (delnnm .eq. 0.0d0) go to 35
         if (in .ne. 0) specr = delnnm / delsnm
         if (ip .lt. iphat) go to 50
c
c ... stopping test, set h
c
         if (specr .ge. 1.0d0) go to 50
         if (.not. (specr .gt. (omega - 1.0d0))) go to 35
         h = specr
         go to 40
 35      iss = iss + 1
         h = omega - 1.0d0
c
c ... perform stopping test.
c
 40      continue
         dnrm = delnnm**2
         call sorstp (n,u,ubar,dnrm,h)
         if (halt) go to 50
c
c ... method has not converged yet, test for changing omega
c
         if (.not. omgadp) go to 50
         if (ip .lt. ipstar)  go to 50
         if (omega .gt. 1.0d0) go to 45
         emax = sqrt (abs (specr))
         omegap = 2.0d0 / (1.0d0 + sqrt (abs (1.0d0 - specr)))
         change = .true.
         go to 50
 45      if (iss .ne. 0) go to 50
         if (specr .le. (omega - 1.0d0)**fff) go to 50
         if ((specr + 0.00005d0) .le. spcrm1) go to 50
c
c ... change parameters
c
         emax = (specr + omega - 1.0d0) / (sqrt (abs (specr))*omega)
         omegap = 2.0d0 / (1.0d0 + sqrt (abs (1.0d0 - emax*emax)))
         change = .true.
c
 50      ip = ip + 1
         in = in + 1
 55   continue
      ier = 1
      in = in - 1
      call ershow (ier,'itsor')
      zeta = stptst
      return
      end
      subroutine basicw (suba,subql,subqr,
     a             coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,
     a             iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the basic (unaccelerated) iterative method,
c with preconditioning.  that is, it applies the fixed point method
c to the preconditioned system.
c two-sided preconditioning is efficiently implemented.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      logical iql, iqr
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
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
c preliminary calculations
c
      iacel = 0
      ier = 0
      nwusd = 0
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 996
      if (level .ge. 2) write (nout,496)
496   format (' basic')
c use knowledge about spectrum to optimally extrapolate ...
      extrap = (emax+emin)/2.0d0
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
c
c initialize the stopping test ...
c
      call inithv (0)
      zthave = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,
     a            coef,jcoef,wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 735
c
c bust up workspace ...
c
      izt = 1
      iv1 = izt + n
      iwfree = iv1 + n
      if (iqlr .eq. 0) iwfree = iv1
      nwusd = max (nwusd,iwfree-1)
c
c check the memory usage ...
c
      if (nwusd .gt. nw) go to 999
c
c do preliminary calculations ...
c
      in = 0
      is = 0
      go to (151,152,153,154),iqlr + 1
c
 151  call suba (coef,jcoef,wfac,jwfac,n,u,wk(izt))
      call vexopy (n,wk(izt),rhs,wk(izt),2)
      go to 10
c
 152  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(izt))
      go to 10
c
 153  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(izt))
      go to 10
c
 154  call suba (coef,jcoef,wfac,jwfac,n,u,wk(izt))
      call vexopy (n,wk(izt),rhs,wk(izt),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(izt),wk(iv1))
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(izt))
      go to 10
c
c-----------------------begin iteration loop----------------------
c
c determine whether or not to stop --
c
 10   call inithv (1)
      nwpstp = nw - (iwfree-1)
      call pstop (1,suba,subql,subqr,
     a   coef,jcoef,wfac,jwfac,n,u,ubar,rhs,xxx,xxx,wk(izt),
     a   wk(iwfree),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iwfree-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c form iterate ...
c
      call vtriad (n,u,u,1.0d0/extrap,wk(izt),1)
c
c form residuals, as necessary ...
c
      go to (161,162,163,164),iqlr + 1
c
 161  call suba (coef,jcoef,wfac,jwfac,n,u,wk(izt))
      call vexopy (n,wk(izt),rhs,wk(izt),2)
      go to 110
c
 162  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(izt))
      go to 110
c
 163  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(izt))
      go to 110
c
 164  call suba (coef,jcoef,wfac,jwfac,n,u,wk(izt))
      call vexopy (n,wk(izt),rhs,wk(izt),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(izt),wk(iv1))
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(izt))
      go to 110
c
c proceed to next iteration
c
 110  in = in + 1
      is = is + 1
      go to 10
c
c--------------------------------finish up-------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'basicw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' basic method converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,
     a             digit1,digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 996  call ershow (ier,'basicw')
      go to 735
c
c insuff. floating point wksp ...
 999  ier = -2
      call ershow (ier,'basicw')
      go to 735
c
      end
      subroutine mew (suba,subql,subqr,
     a               coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,
     a               iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c this routine runs the minimal error algorithm of fridman.
c the reference is: v. m. fridman, "the method of minimum iterations
c ...", ussr computational math. and math. phys., vol. 2, 1962,
c pp. 362-3.
c
c two-sided preconditioning is implemented.  the iteration matrix
c should be symmetric for this algorithm to work.
c
c we have introduced periodic scaling of the direction vectors, to
c prevent overflow.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c the following indexing functions are used to access the old
c direction vectors --
c
      indp(i)  = ip  + mod(i,2)*n
      indpt(i) = ipt + mod(i,2)*n
c
c various preliminary calculations.
c
      dot = 0.0d0
      nwusd = 0
      ier = 0
      iacel = 4
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      if (level .ge. 2) write (nout,496)
496   format (' me')
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave  = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,
     a            coef,jcoef,wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c memory allocation, etc.
c
c nomenclature -- r  -- residual of the original system.
c                 z  -- inv(ql)*r
c                 zt -- inv(qr)*z
c
      ip = 1
      ipt = ip + 2*n
      iz = ipt + 2*n
      ir = iz + n
      iv1 = ir + n
      if (.not. rcalp) iv1 = ir
      izt = iv1 + n
      iv2 = izt + n
      if (.not. ztcalp) iv2 = izt
      iqlap = iv1
      iqrlap = iv2
      iwfree = iv2 + n
c
c note that memory usage has been overlapped whenever possible,
c in order to save space.
c
      nwusd = max (nwusd,iwfree-1)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      rhave = rcalp
      zthave = ztcalp
c
c perform first-iterate calculations
c
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(ir))
      call vexopy (n,wk(ir),rhs,wk(ir),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(ir),wk(iz))
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iz),wk(izt))
c
c---------------------------- begin iteration loop ----------------------------
c
c determine whether or not to stop --
c note that we have already done the calculations necessary so that suba
c and subql are not actually used by pstop.
c
 10   call inithv (1)
      nwpstp = nw - (iwfree-1)
      call pstop (1,suba,subql,subqr,
     a   coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk(ir),wk(iz),wk(izt),
     a   wk(iwfree),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iwfree-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c compute p(n), the direction vector, and inv(qr)*p(n) (=pt(n)).
c
      scal = 1.0d0
c
c first, case of in .eq. 0
c
      if (in .ne. 0) go to 100
      toplam = vdot (n,wk(iz),wk(iz))
      call suba (coef,jcoef,wfac,jwfac,n,wk(izt),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(indp(in)))
      call subqr (coef,jcoef,wfac,jwfac,n,wk(indp(in)),wk(indpt(in)))
      go to 120
c
c case in .gt. 0
c
 100  toplam = vdot (n,wk(indp(in-1)),wk(iz))
      bet1 = - vdot (n,wk(indp(in-1)),wk(iqlap)) / dot
      if (in .ne. 1) go to 110
c
c case in .eq. 1
c
      call vtriad (n,wk(indp(in)),wk(iqlap),bet1,wk(indp(in-1)),1)
      call vtriad (n,wk(indpt(in)),wk(iqrlap),bet1,wk(indpt(in-1)),1)
      go to 120
c
c case in .gt. 1
c
 110  bet2 = - vdot (n,wk(indp(in-2)),wk(iqlap)) / dotold
      call vtriad (n,wk(indp(in)), wk(iqlap), bet2,wk(indp(in-2)), 1)
      call vtriad (n,wk(indpt(in)),wk(iqrlap),bet2,wk(indpt(in-2)),1)
      call vtriad (n,wk(indp(in)), wk(indp(in)), bet1,wk(indp(in-1)), 1)
      call vtriad (n,wk(indpt(in)),wk(indpt(in)),bet1,wk(indpt(in-1)),1)
c
c at this point, we are finished forming the latest direction vector.
c we proceed to calculate lambda and update the solution and the
c residual.
c
 120  dotold = dot
      dot = vdot (n,wk(indp(in)),wk(indp(in)))
c     if (dot .lt. srelpr) go to 998
c
c scale direction vector if necessary ...
      if (dot.lt.srelpr**2 .or. dot.gt.(1.0d0/srelpr**2)) then
        scal = sqrt(dot)
        call vtriad (n,wk(indp(in)), xxx,1.0d0/scal,wk(indp(in)), 2)
        call vtriad (n,wk(indpt(in)),xxx,1.0d0/scal,wk(indpt(in)),2)
        dot = 1.0d0
      end if
c
 124  vlamda = toplam / dot / scal
c
c u --
c
      call vtriad (n,u,u,vlamda,wk(indpt(in)),1)
c
c r --
c
      call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(in)),wk(iv2))
      if (rhave) call vtriad (n,wk(ir),wk(ir),-vlamda,wk(iv2),1)
c
c z --
c
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iqlap))
      call vtriad (n,wk(iz),wk(iz),-vlamda,wk(iqlap),1)
c
c zt --
c
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iqlap),wk(iqrlap))
      if (zthave) call vtriad (n,wk(izt),wk(izt),-vlamda,wk(iqrlap),1)
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      go to 10
c
c-------------------------------finish up-----------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'mew')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' me converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 997  call ershow (ier,'mew')
      go to 735
c
 998  ier = -15
      call ershow (ier,'mew')
      go to 725
c
 999  ier = -2
      call ershow (ier,'mew')
      go to 735
c
      end
      subroutine cgnrw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the conjugate gradient algorithm on the normal equations.
c in this variant, the residual of the original system is minimized
c per iteration.  currently, only left preconditioning is implemented.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 5
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' cgnr')
      maxadp = maxadd
      minadp = minadd
      alphao = 0.0d0
      alpha = 0.0d0
      beta = 0.0d0
c
c initialize the stopping test ...
c
      call inithv (0)
      zthave = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
      itri = 1
      ip = itri
      if ( .not. (maxadd .or. minadd) ) go to 850
      ip = itri + 2*itmax
      call vfill (2*itmax,wk(itri),0.0d0)
 850  ir = ip + n
      iv1 = ir + n
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2-1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(ir))
c
c--------------------------begin iteration loop------------------------
c
c determine whether or not to stop --
c
 10   call inithv (1)
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,wk(ir),
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
      if (in .ne. 0) go to 110
c
c perform first-iterate calculations
c
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(ir),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(ip))
      ard = vdot (n,wk(ip),wk(ip))
      go to 111
c
c perform subsequent-iterate calculations
c
 110  ardold = ard
c     if (abs(ardold) .lt. srelpr) go to 996
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(ir),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      ard = vdot (n,wk(iv2),wk(iv2))
      an = ard/ardold
      call vtriad (n,wk(ip),wk(iv2),an,wk(ip),1)
      beta = an
c
c proceed to form the iterate.
c
 111  call suba (coef,jcoef,wfac,jwfac,n,wk(ip),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      pap = vdot (n,wk(iv2),wk(iv2))
      if (abs(pap) .lt. srelpr**2) go to 998
      vlamda = ard/pap
c
      call vtriad (n,u,u,vlamda,wk(ip),1)
      call vtriad (n,wk(ir),wk(ir),-vlamda,wk(iv2),1)
c
c update eigenvalue estimates
c
      alphao = alpha
      alpha = vlamda
      if (maxadp .or. minadp) call chgcon (wk(itri),ier)
      if (ier .lt. 0) go to 725
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      go to 10
c
c-------------------------------finish up---------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'cgnrw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' cgnr converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'cgnrw')
      return
c
 996  ier = -13
      call ershow (ier,'cgnrw')
      go to 725
c
 997  call ershow (ier,'cgnrw')
      go to 735
c
 998  ier = -15
      call ershow (ier,'cgnrw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'cgnrw')
      go to 735
c
      end
      subroutine lsqrw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the lsqr algorithm.  the algorithm is taken from
c the article 'lsqr -- an algorithm for sparse linear equations
c and sparse least squares.'
c by c. c. paige amd m. a. saunders, in acm transactions on
c mathematical software, vol. 8, no. 1, march 1982, pp. 43-71.
c the iterates produced are the same as those of cgnr, in exact
c arithmetic, but this should be more stable.  only left
c preconditioning is currently implemented.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      integer vect1, vect2, os
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 6
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 996
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' lsqr')
c
c initialize the stopping test ...
c
      call inithv (0)
      zdhav = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,
     a            coef,jcoef,wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 735
c
c ... associated integer variables.
c
      iu = 1
      iv = iu + n
      iw = iv + n
      iv1 = iw + n
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2-1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
c
c now, perform first-iterate calculations
c
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      beta = sqrt(vdot (n,wk(iv2),wk(iv2)))
      if (abs(beta) .lt. srelpr) go to 997
      call vtriad (n,wk(iu),xxx,1.0d0/beta,wk(iv2),2)
c
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(iu),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      alpha = sqrt(vdot (n,wk(iv2),wk(iv2)))
      if (abs(alpha) .lt. srelpr) go to 997
      call vtriad (n,wk(iv),xxx,1.0d0/alpha,wk(iv2),2)
c
      call vcopy (n,wk(iv),wk(iw))
      phibar = beta
      rhobar = alpha
      zdot = phibar**2
c if u(0) is zero, then the norm of u(n) can be calculated for free.
c otherwise, i don't know.
c
c---------------------------begin iteration loop---------------------
c
c determine whether or not to stop --
c
 10   call inithv (1)
      zdhav = .true.
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c ... compute the lanczos vectors.
c
      call suba (coef,jcoef,wfac,jwfac,n,wk(iv),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      call vtriad (n,wk(iu),wk(iv2),-alpha,wk(iu),1)
      beta = sqrt(vdot (n,wk(iu),wk(iu)))
      if (abs(beta) .lt. srelpr) go to 997
      call vtriad (n,wk(iu),xxx,1.0d0/beta,wk(iu),2)
c
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(iu),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      call vtriad (n,wk(iv),wk(iv2),-beta,wk(iv),1)
      alpha = sqrt(vdot (n,wk(iv),wk(iv)))
      if (abs(alpha) .lt. srelpr) go to 997
      call vtriad (n,wk(iv),xxx,1.0d0/alpha,wk(iv),2)
c
c continue by calculating various scalars.
c
      rho = sqrt(rhobar**2+beta**2)
      if (rho .lt. srelpr) go to 998
      c = rhobar/rho
      s = beta/rho
      theta = s*alpha
      rhobar = -c*alpha
      phi = c*phibar
      phibar = s*phibar
c
c now generate the new u and w vectors.
c
      call vtriad (n,u,u,phi/rho,wk(iw),1)
      call vtriad (n,wk(iw),wk(iv),-theta/rho,wk(iw),1)
c
c proceed to next iteration
c
      zdot = phibar**2
      in = in + 1
      is = is + 1
      go to 10
c
c-----------------------------finish up-------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'lsqrw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' lsqr converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'lsqrw')
      return
c
 996  call ershow (ier,'lsqrw')
      go to 735
c
 997  ier = -13
      call ershow (ier,'lsqrw')
      go to 725
c
 998  ier = -14
      call ershow (ier,'lsqrw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'lsqrw')
      go to 735
c
      end
      subroutine odirw (suba,subql,subqr,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c this routine implements orthodir with truncation and
c restarting and with 2-sided preconditioning.  the effective value
c of the z matrix is (inv(ql)*a*inv(qr))**t.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      logical iql, iqr
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
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
c the following indexing functions are used to access the old
c direction vectors and dot products --
c
      indpt(i) = ipt + mod(i,nv)*n
      indqap(i) = iqapt + mod(i,nv)*n
      inddot(i) = idot + mod(i,nv)
c
c various preliminary calculations.
c
c
      nwusd = 0
      ier = 0
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      write (nout,496)
496   format (' orthodir')
      iacel = 7
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave  = .true.
      zthave = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 997
c
c memory allocation, etc.
c
      nv = max (1,min(ns1,ns2-1))
      ipt = 1
      iqapt = ipt + nv*n
      idot = iqapt + nv*n
      iz = idot + nv
      izt = iz + n
      if (.not. iqr) izt = iz
      isv = izt + n
      iv1 = isv + n
      iv2 = iv1 + n
c
      if (iql) nwusd = max (nwusd,iv2-1+n)
      if (.not. iql) nwusd = max (nwusd,iv1-1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
c
c perform first-iterate calculations
c
      if (iql) go to 122
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iz))
      call vexopy (n,wk(iz),rhs,wk(iz),2)
      go to 121
 122  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iz))
 121  if (iqr) call subqr (coef,jcoef,wfac,jwfac,n,wk(iz),wk(izt))
c     if (.not. iqr) zdot = vdot (n,wk(iz),wk(iz))
c
c ========================= begin iteration loop ====================
c
c
c determine whether or not to stop ...
c
 10   call inithv (1)
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,wk(iz),wk(izt),
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c proceed to calculate the direction vectors.
c
c first, case of no old p vectors.
c
      np = min(mod(in,ns2),ns1)
      if (np .ne. 0) go to 100
c
      if (is .eq. 0) call vcopy (n,wk(izt),wk(indpt(in)))
      if (is .ne. 0) call vcopy (n,wk(isv),wk(indpt(in)))
      if (iql) go to 123
      call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(in)),wk(indqap(in)))
      go to 120
 123  call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(in)),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(indqap(in)))
      go to 120
c
c case of at least one old p vector.
c this case is handled in a tricky way, to optimize the workspace.
c
 100  if (iql) go to 124
      call suba (coef,jcoef,wfac,jwfac,n,wk(isv),wk(iv1))
      go to 125
 124  call suba (coef,jcoef,wfac,jwfac,n,wk(isv),wk(iv2))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iv1))
c
 125  top = vdot (n,wk(indqap(in-np)),wk(iv1))
      bet = - top / wk(inddot(in-np))
      call vtriad (n,wk(indpt(in)),wk(isv),bet,wk(indpt(in-np)),1)
      call vtriad (n,wk(indqap(in)),wk(iv1),bet,wk(indqap(in-np)),1)
      ibegin = in - np + 1
      iend = in - 1
      if (ibegin .gt. iend) go to 613
      do 612 i = ibegin,iend
      top = vdot (n,wk(indqap(i)),wk(iv1))
      bet = - top / wk(inddot(i))
      call vtriad (n,wk(indpt(in)),wk(indpt(in)),bet,wk(indpt(i)),1)
 612  call vtriad (n,wk(indqap(in)),wk(indqap(in)),bet,wk(indqap(i)),1)
 613  continue
c
c periodically scale the direction vector, to prevent overflow ...
c
 120  continue
      dot = vdot (n,wk(indqap(in)),wk(indqap(in)))
      if (dot.lt.srelpr**2 .or. dot.gt.(1.0d0/srelpr**2)) then
        call vtriad (n,wk(indpt(in)), xxx,1.0d0/dot,wk(indpt(in)), 2)
        call vtriad (n,wk(indqap(in)),xxx,1.0d0/dot,wk(indqap(in)),2)
        dot = 1.0d0
      end if
c
c at this point, we are finished forming the latest direction vector.
c we proceed to calculate lambda and update the solution and
c the residuals.
c
 129  continue
c     if (abs(dot) .lt. srelpr) go to 998
      wk(inddot(in)) = dot
      top = vdot (n,wk(indqap(in)),wk(iz))
      vlamda = top / dot
c the following commented-out line is unstable.  but it can be fixed.
c     if (.not. iqr) zdot = zdot - 2*vlamda*top + vlamda**2*dot
c
c u --
c
      call vtriad (n,u,u,vlamda,wk(indpt(in)),1)
c
c z --
c
      call vtriad (n,wk(iz),wk(iz),-vlamda,wk(indqap(in)),1)
c
c zt --
c
      call subqr (coef,jcoef,wfac,jwfac,n,wk(indqap(in)),wk(isv))
      if (iqr) call vtriad (n,wk(izt),wk(izt),-vlamda,wk(isv),1)
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      if (is .eq. ns2) is = 0
      go to 10
c
c-------------------------------finish up----------------------------
c
 900  if (.not. halt) go to 996
      if (level .ge. 1) write (nout,720) in
 720  format (/' orthodir converged in ',i5,' iterations.')
c
 725  if (idgts .ge. 0)
     a    call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,
     a                 digit1,digit2,idgts)
c pack revised parms into iparm, rparm ...
      t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c
c error returns
c
c no convergence ...
 996  ier = 1
      call ershow (ier,'odirw')
      zeta = stptst
      go to 725
c
c generic error handler ...
 997  call ershow (ier,'odirw')
      go to 735
c
c breakdown ...
 998  ier = -15
      call ershow (ier,'odirw')
      go to 725
c
c insufficient floating point wksp ...
 999  ier = -2
      call ershow (ier,'odirw')
      go to 735
      end
      subroutine ominw (suba,subql,subqr,
     a      coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c this routine implements the truncated/restarted orthomin algorithm.
c eigenvalue estimation is implemented.
c note that this also implements the gcr algorithm.
c
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subql, subqr
      external nullpl, nullpr
      dimension iparm(30), rparm(30)
c
      ier = 0
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) return
c
c pass on to workhorse routine ...
c
      call omingw (suba,subql,subqr,nullpl,nullpr,
     a      coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      return
      end
      subroutine omingw (suba,subql,subqr,precl,precr,
     a      coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c this is a generalized version of the omingw routine which allows a
c more general computational form for the preconditioning.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      logical ipl, ipr
      external suba, subql, subqr, precl, precr
      dimension iparm(30), rparm(30)
      logical ztget, havest, hadest, evest
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
c the following indexing functions are used to access the old
c direction vectors and dot products --
c
      indpt(i) = ipt + mod(i,nv)*n
      indqap(i) = iqapt + mod(i,nv)*n
      inddot(i) = idot + mod(i,nv+1)
      indhes(i,j) = ihess + (i-1) + (j-1)*nhess
      inapar(i) = iapar + mod(i,nv)
      indlam(i) = ilam + mod(i,nv+1)
c
c various preliminary calculations.
c
      t1 = timer (dummy)
c
      ipl = iplr .eq. 1 .or. iplr .eq. 3
      ipr = iplr .eq. 2 .or. iplr .eq. 3
c
      iacel = 8
      nwusd = 0
      if (level .ge. 1) write (nout,497)
497   format (' omin')
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave   = .true.
      zthave  = .true.
      nwpstp =  nw
      call pstopg (0,suba,subql,subqr,precl,precr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 997
      ztget = ztcalp
      zthave = ztget
c
c memory allocation, etc.
c
      numbig = 1000
      methev = 1
      if (iabs(ns3) .ge. numbig) then
        if (ns3 .gt. 0) ns3 = ns3 - numbig
        if (ns3 .lt. 0) ns3 = ns3 + numbig
        methev = 2
      end if
c
      evest = ns3 .ne. 0 .and. (maxadd.or.minadd)
      nhess = 2 + min(itmax,ns2)
      nv = max (1,min(ns1,ns2-1))
      ipt = 1
      iqapt = ipt + nv*n
      idot = iqapt + nv*n
      iapar = idot + (nv+1)
      ihess = iapar + nv
      ilam = ihess + nhess*(nv+2)
      if (.not. evest) ilam = ihess
      iz = ilam + (nv+1)
      izt = iz + n
      if (.not. ipr) izt = iz
      iv1 = izt + n
      iv2 = iv1 + n
      ir = iz
      if (ipl) ir = iv1
c
      nwtmp = iv1 - 1 + n
      if (ipl) nwtmp = iv2 - 1 + n
      nwusd = max (nwusd,nwtmp)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
c
c perform first-iterate calculations
c
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(ir))
      call vexopy (n,wk(ir),rhs,wk(ir),2)
      if (ipl) call precl (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                     wk(ir),wk(iz))
      hadest = .false.
c
c ------------------------- begin iteration loop ----------------------
c
c determine whether or not to stop ...
c
 10   if (.not. ztget) go to 710
      if (ipr) call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                     wk(iz),wk(izt))
c
 710  call inithv (1)
      nwpstp = nw - (iv1-1)
      call pstopg (1,suba,subql,subqr,precl,precr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,wk(iz),wk(izt),
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
      if (zthave) go to 711
      if (ipr) call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                     wk(iz),wk(izt))
c
c------------------proceed to calculate the direction vectors----------------
c
c first, case of no old p vectors.
c
 711  np = min(mod(in,ns2),ns1)
      if (np .ne. 0) go to 100
c
      call vcopy (n,wk(izt),wk(indpt(in)))
      if (.not. ipl) then
        call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(in)),wk(indqap(in)))
      else
        call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(in)),wk(iv1))
        call precl (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a              wk(iv1),wk(indqap(in)))
      end if
      go to 120
c
c case of at least one old p vector.
c this case is handled in a tricky way, to optimize the workspace.
c
 100  if (.not. ipl) then
        call suba (coef,jcoef,wfac,jwfac,n,wk(izt),wk(iv1))
      else
        call suba (coef,jcoef,wfac,jwfac,n,wk(izt),wk(iv2))
        call precl (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a              wk(iv2),wk(iv1))
      end if
c
      top = vdot (n,wk(indqap(in-np)),wk(iv1))
      wk(inapar(in-np)) = top
      bet = - top / wk(inddot(in-np))
      call vtriad (n,wk(indpt(in)),wk(izt),bet,wk(indpt(in-np)),1)
      call vtriad (n,wk(indqap(in)),wk(iv1),bet,wk(indqap(in-np)),1)
c
      do 612 i = in-np+1, in-1
      top = vdot (n,wk(indqap(i)),wk(iv1))
      wk(inapar(i)) = top
      bet = - top / wk(inddot(i))
      call vtriad (n,wk(indpt(in)), wk(indpt(in)), bet,wk(indpt(i)), 1)
 612  call vtriad (n,wk(indqap(in)),wk(indqap(in)),bet,wk(indqap(i)),1)
c
c at this point, we are finished forming the latest direction vector.
c we proceed to calculate lambda and update the solution and
c the residuals.
c
120   continue
      apap = vdot (n,wk(indqap(in)),wk(indqap(in)))
c     if (abs(apap) .lt. srelpr**2) go to 998
      if (abs(apap) .eq. 0.0d0) go to 998
      wk(inddot(in)) = apap
      top = vdot (n,wk(indqap(in)),wk(iz))
      vlamda = top / apap
c     if (.not. ipr) zzdot = zzdot - 2*vlamda*top + vlamda**2*apap
c
c u --
      call vtriad (n,u,u,vlamda,wk(indpt(in)),1)
c
c z --
      call vtriad (n,wk(iz),wk(iz),-vlamda,wk(indqap(in)),1)
c
c----------------------------hess matrix update---------------------------
c
c there are two schemes here, based on two different ways of projecting
c the iteration matrix.
c
c update hessenberg matrix: scheme 1
c
      if (.not. evest) go to 955
      wk(indlam(in)) = vlamda
      if (is .eq. 0) call vfill (nhess*(nv+2),wk(ihess),0.0d0)
      if (methev .ne. 1) go to 746
c
      do 954 i=in-np,in
      if (i .eq. in) apar = apap
      if (i .ne. in) apar = wk(inapar(i))
      wk(indhes(i+1+(is-in),in-i+2)) = wk(indhes(i+1+(is-in),in-i+2))
     a  + apar/wk(indlam(in)) / sqrt(wk(inddot(in))*wk(inddot(i)))
      if (is .ne. 0)
     a wk(indhes(i+1+(is-in),in-i+1)) = wk(indhes(i+1+(is-in),in-i+1))
     a  - apar/wk(indlam(in-1)) / sqrt(wk(inddot(in-1))*wk(inddot(i)))
 954  continue
      iesize = is
      go to 747
c
c update hessenberg matrix: scheme 2
c
 746  iesize = is + 1
      wk(indhes(is+2,1)) = -1.0d0 / vlamda
      wk(indhes(is+1,2)) =  1.0d0 / vlamda
      if (np .eq. 0) go to 749
      do 748 i=in-np,in-1
      id = in - i + 1
      wk(indhes(is+3-id,id  )) = wk(indhes(is+3-id,id  ))
     a                      - wk(inapar(i))/wk(inddot(i))/wk(indlam(i))
 748  wk(indhes(is+2-id,id+1)) = wk(indhes(is+2-id,id+1))
     a                      + wk(inapar(i))/wk(inddot(i))/wk(indlam(i))
 749  continue
c
c estimate eigenvalues ...
c
 747  nwhe = nw - (iv1-1)
      call hesest (wk(ihess),nhess,nv+2,iesize,ns3,havest,
     a             emaxnw,eminnw,wk(iv1),nwhe,ier)
      nwusd = max (nwusd,iv1-1+nwhe)
      if (ier .ne. 0) go to 995
      if (.not. havest) go to 955
      if (hadest) go to 956
      if (maxadd) emax = emaxnw
      if (minadd) emin = eminnw
      hadest = .true.
      go to 955
 956  if (maxadd) emax = max (emax,emaxnw)
      if (minadd) emin = min (emin,eminnw)
c
c---------------------------proceed to next iteration------------------------
c
 955  in = in + 1
      is = is + 1
      if (is .eq. ns2) is = 0
      go to 10
c
c-------------------------------finish up------------------------------------
c
 900  if (.not. halt) go to 996
      if (level .ge. 1) write (nout,720) in
 720  format (/' orthomin converged in ',i5,' iterations.')
c
 725  if (idgts .ge. 0)
     a    call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a                 digit2,idgts)
c pack revised parms into iparm, rparm ...
      t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c
c--------------------------------error returns-----------------------------
c
c unimplemented option ...
 995  ier = -16
      call ershow (ier,'omingw')
      go to 725
c
c no convergence ...
 996  ier = 1
      call ershow (ier,'omingw')
      zeta = stptst
      go to 725
c
c generic error handler ...
 997  call ershow (ier,'omingw')
      go to 735
c
c breakdown ...
 998  ier = -15
      call ershow (ier,'omingw')
      go to 725
c
c insufficient floating point wksp ...
 999  ier = -2
      call ershow (ier,'omingw')
      go to 735
      end
      subroutine oresw (suba,subql,subqr,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c this routine implements orthores with truncation and
c restarting and with 2-sided preconditioning.  the value of z is
c the identity.  the code is optimal in speed and workspace
c requirements, for general a, ql and qr.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c the following indexing functions are used to access the old
c direction vectors and dot products --
c
      indu(i) = iu + mod(i,nv)*n
      indz(i) = iz + mod(i,nv)*n
      inddot(i) = idot + mod(i,nv)
c
c various preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 9
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      if (level .ge. 2) write (nout,496)
496   format (' orthores')
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave  = .true.
      zthave = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c memory allocation, etc.
c
c nomenclature -- r  -- residual of the original system.
c                 z  -- inv(ql)*r
c                 zt -- inv(qr)*z
c
      nv = max (1,min(ns1+1,ns2))
      iu = 1
      iz = iu + nv*n
      idot = iz + nv*n
      iv1 = idot + nv
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2-1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
c
c perform first-iterate calculations.
c note -- we will use the vector 'u' to store ztilde. the u vectors
c will be stored in the table.  wk(iv1) will hold r.
c
      call vcopy (n,u,wk(indu(0)))
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(indz(0)))
      call subqr (coef,jcoef,wfac,jwfac,n,wk(indz(0)),u)
      wk(inddot(0)) = vdot (n,wk(indz(0)),wk(indz(0)))
c
c-------------------------begin iteration loop-----------------------
c
c determine whether or not to stop --
c
 10   call inithv (1)
      nwpstp = nw - (iv2-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a     wfac,jwfac,n,wk(indu(in)),ubar,rhs,xxx,wk(indz(in)),u,
     a     wk(iv2),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv2-1)
      if (level .ge. 2) call iterm (n,wk(indu(in)))
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c proceed to calculate the iterates.
c
      np = min(mod(in,ns2)+1,ns1+1)
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      top = vdot (n,wk(indz(in+1-np)),wk(iv2))
      sig = top / wk(inddot(in+1-np))
      call vtriad (n,wk(indz(in+1)),wk(iv2),-sig,wk(indz(in+1-np)),1)
      call vtriad (n,wk(indu(in+1)),u,sig,wk(indu(in+1-np)),1)
      sigsum = sig
      ibegin = in - np + 2
      iend = in
      if (ibegin .gt. iend) go to 613
      do 612 i = ibegin,iend
      top = vdot (n,wk(indz(i)),wk(iv2))
      den = wk(inddot(i))
      if (abs(den) .lt. srelpr) go to 998
      sig = top / den
      call vtriad (n,wk(indz(in+1)),wk(indz(in+1)),-sig,wk(indz(i)),1)
      call vtriad (n,wk(indu(in+1)),wk(indu(in+1)),sig,wk(indu(i)),1)
 612  sigsum = sigsum + sig
 613  continue
      if (abs(sigsum) .lt. srelpr) go to 998
      vlamda = 1.0d0/sigsum
      call vtriad (n,wk(indz(in+1)),xxx,-vlamda,wk(indz(in+1)),2)
      call vtriad (n,wk(indu(in+1)),xxx,vlamda,wk(indu(in+1)),2)
      wk(inddot(in+1)) = vdot (n,wk(indz(in+1)),wk(indz(in+1)))
c
      call subqr (coef,jcoef,wfac,jwfac,n,wk(indz(in+1)),u)
c
c proceed to next iteration
c
      in = in + 1
      go to 10
c
c-----------------------------finish up----------------------------
c
 900  call vcopy (n,wk(indu(in)),u)
      if (halt) go to 715
      ier = 1
      call ershow (ier,'oresw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' orthores converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 997  call ershow (ier,'oresw')
      go to 735
c
 998  ier = -15
      call ershow (ier,'oresw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'oresw')
      go to 735
c
      end
      subroutine iomw (suba,subql,subqr,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the (truncated) iom algorithm.  the reference is
c youcef saad, "krylov subspace methods ...", mathematics of
c computation, vol. 37, july 1981, pp. 105f.
c
c in the symmetric case this algorithm reduces to the symmlq
c algorithm of paige and saunders, except paige and saunders have
c implemented a trick to avoid breakdown before convergence.  this
c trick is not implemented here.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      integer idotw, vect1, vect2, dots1, dots2, os
      logical uneed
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
      dimension gdum(1), wkxxx(1)
      logical iql, iqr
      logical exact, gamize
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
c next, the indexing functions.
c
      indv1(i) = vect1 + mod(i,nv)*n
      indbe2(i) = ibeta2 + mod(i,os)
      indc(i) = icos + mod(i,os)
      inds(i) = isin + mod(i,os)
      indu(i) = iu + mod(i,os+1)
      indw(i) = iw + n*mod(i,os)
c
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 10
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 996
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      gamize = .true.
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' iom')
c the following flag tells us whether the truncating actually
c throws out important information.  it should actually be set to
c true if the matrix is symmetric.
      exact = .false.
c
c initialize the stopping test ...
c
      call inithv (0)
      zdhav = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c ... associated integer variables.
c
      os = iabs(ns1)
      iv = 1
      nv = os
      idotw = 1
      iw = 1
      vect1 = iw + iv*n*os
      vect2 = vect1
      dots1 = vect2 + iv*n*nv
      dots2 = dots1
      ibeta1 = dots2 + idotw*os
      ibeta2 = ibeta1
      icos = ibeta2 + os
      isin = icos + os
      iu = isin + os
      iv1 = iu + os+1
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2-1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      uneed = rcalp .or. zcalp .or. ztcalp .or. udhav
     a   .or. ntest .eq. 6 .or. level .ge. 3
c
c--------------------------begin iteration loop---------------------
c
c perform first-iterate calculations ...
c
 10   if (is .ne. 0) go to 100
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      call pvec (n,nv,iv,1,os,idotw,is,1,1,wk(vect1),wk(dots1),0,
     a          wk(ibeta1),gdum,gamize,wk(iv2),wkxxx,ier)
      gamma1 = gdum(1)
      if (ier .lt. 0) go to 997
      gamma2 = gamma1
      vnorm1 = 1.0d0/gamma1
      vnorm2 = 1.0d0/gamma2
      zdot = vnorm1**2
      ucnp1= 0.0d0
c
 100  call inithv (1)
      zdhav = .true.
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c
c ... compute q(n+1), etc -- the direction vectors
c
      call suba (coef,jcoef,wfac,jwfac,n,wk(indv1(is)),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      call pvec (n,nv,iv,1,os,idotw,is+1,1,1,wk(vect1),wk(dots1),0,
     a          wk(ibeta1),gdum,gamize,wk(iv2),wkxxx,ier)
      gamma1 = gdum(1)
      if (ier .lt. 0) go to 997
      gamma2 = gamma1
c
c ... now record norms.
c
      vn1old = vnorm1
      vnorm1 = 1.0d0/gamma1
      vn2old = vnorm2
      vnorm2 = 1.0d0/gamma2
c
c ... now update the factorization
c
      ucnbar = ucnp1
      ibgn = max (0,is+1-os)
      do 1 i = ibgn,is
 1    wk(indu(i+1)) = -wk(indbe2(i))
      if (ibgn .gt. 0) wk(indu(ibgn))= 0.0d0
      call qrupd (is+1,os+1,os,wk(icos),wk(isin),ucnbar,ucn,wk(iu),
     a  vn2old,ier)
      if (ier .lt. 0) go to 998
      ucnp1 = wk(indu(is+1))
c
c ... update the old w vector.
c
      if (is .ne. 0)
     a   call vtriad (n,wk(indw(is-1)),xxx,ucnbar/ucn,wk(indw(is-1)),2)
c
c ... now generate the new w vector.
c
      if (abs(ucnp1) .lt. srelpr) go to 998
      call vcopy (n,wk(indv1(is)),wk(iv1))
      ibgn = max (1,is-os+1)
      iend = is
      if (iend .lt. ibgn) go to 200
      do 201 i = ibgn,iend
 201  call vtriad (n,wk(iv1),wk(iv1),-wk(indu(i)),wk(indw(i-1)),1)
 200  continue
      call vtriad (n,wk(indw(is)),xxx,1.0d0/ucnp1,wk(iv1),2)
      if (is .ne. 0) go to 205
c
c ... update iterate u(0).
c
      zold= 0.0d0
      zbar = vn1old
      if (uneed) call vtriad (n,u,u,zbar,wk(indw(0)),1)
      go to 210
c
c ... update subsequent iterates u(n).
c
 205  zold = wk(indc(is))*zbar
      zbold = zbar
      zbar =-wk(inds(is))*zbar
      factor = zold
      if (uneed) factor = factor - zbold*ucn/ucnbar
      call vtriad (n,u,u,factor,wk(indw(is-1)),1)
      if (uneed) call vtriad (n,u,u,zbar,wk(indw(is)),1)
c to avoid breakdown for the symmetric indefinite case, we'd only add
c in w(is-1) here, i believe.
 210  continue
      zdot = (zbar/ucnp1*vnorm1)**2
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      go to 10
c
c-----------------------------finish up------------------------------
c
 900  if (.not. uneed) call vtriad (n,u,u,zbar,wk(indw(is-1)),1)
      if (halt) go to 715
      ier = 1
      call ershow (ier,'iomw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' iom converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'iomw')
      return
c
 996  call ershow (ier,'iomw')
      go to 735
c
 997  ier = -13
      call ershow (ier,'iomw')
      go to 725
c
 998  ier = -14
      call ershow (ier,'iomw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'iomw')
      go to 735
c
      end
      subroutine gmresw (suba,subql,subqr,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the truncated/restarted gmres algorithm.  a detailed
c description of this useful algorithm may be found in the paper,
c "gmres: a generalized minimal residual algorithm for solving
c nonsymmetric linear systems", youcef saad and martin h. schultz,
c siam j. sci. stat. comput., v. 7, no. 3, july 1986.
c
c further scoop on how to set up qr factorizations can be obtained in
c "practical use of some krylov subspace methods for solving
c indefinite and unsymmetric linear systems", youcef saad, siam j. sci.
c stat. comput., v. 5, no. 1, march 1984.
c
c the advantage of this algorithm over its competitors orthomin and gcr
c is that work and storage are saved by avoiding the computation of
c certain vectors.
c
c this routine now handles right and 2-sided preconditioning.  the main
c thing to note about this is that a new table of basis vecttors is now
c necessary, to use to update the solution.
c
c this routine also avoids explicit scaling of the p and w vectors.
c
c for the pure restarted case, we actually compute the final arnoldi
c vector, rather than just estimating its norm.  this is a diversion
c from the saad/schultz paper.  this was done because in some cases it
c was found that the norm estimation was subject to significant
c numerical error.
c
c modified feb. 1990 to make the restarted method more efficient.
c specifically, new formulas were installed for the scalar part of
c the computation to give an optimal asymptotic dependence on ns2.
c
c----------------------------------------------------------------------
c
      dimension coef(*), jcoef(*), wfac(*), jwfac(*)
      dimension u(*), ubar(*), rhs(*), wk(*)
      logical uneed, zneed
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
      logical iql, iqr
      logical trunc, exact, rstrt, rstrtd, zhvold
      logical havest, hadest, evadpt
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
c-------------------------- indexing functions --------------------------
c
c the following function accesses the arnoldi vectors.
      indp(i) = ip + mod(i,nv)*n
c
c the following accesses q-r times the arnoldi vectors
      indpt(i) = ipt + mod(i,nvt)*n
c
c fudge factor for the arnoldi vectors.  p(actual) = p(stored)*pfudge.
c (we do the same trick with a*p.)
      indpf(i) = ipf + mod(i,nv)
c
c the following accesses the w-vectors.
      indw(i) = iw + n*mod(i,nv)
c
c fudge factors for the w vectors ...
c (similarly, the vector "xi" is fudged.)
      indwf(i) = iwf + mod(i,nv)
c
c the following accesses the hessenberg matrix -- stored by diagonals ...
      indhes(i,j) = ihess + (i-1) + (j-i+1)*nhess
c
c the following are the cosines and sines of the rotations.
      indc(i) = icos + mod(i,nrot)
      inds(i) = isin + mod(i,nrot)
c
c the following accesses the u matrix -- stored by columns ...
      indu(i,j) = iu + j-i+1 + mod(j-1,nuc)*nbwuh
c
c the following accesses the z-vector ...
      indzc(i) = izc + mod(i-1,nzc)
c
c----------------------- preliminary calculations ----------------------
c
      nwusd = 0
      ier = 0
      iacel = 11
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 996
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      iadpt = ns3
      evadpt = (maxadd .or. minadd) .and. iadpt .ne. 0
      trunc = ns1 .lt. (ns2-1)
      exact = .not. trunc
      if (ns1 .lt. 2) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' gmres')
c
c--------------------------initialize the stopping test------------------
c
      call inithv (0)
      zdhav = .not. (trunc .and. .not.exact)
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c uneed tells us whether u must be computed explicitly per iteration.
c similarly for zneed.
      uneed = rcalp .or. udhav
     a   .or. ntest .eq. 6 .or. level .ge. 3
      zneed = zcalp
      hadest = .false.
c
c------------------------associated integer variables---------------------
c
c---effective ns2---
      ns2e = min(ns2,itmax)
c---length of diags of hess matrix---
      nhess = ns2e + 2
c---bandwidth of the hess matrix---
      nbwh = min(ns1+1,ns2e+1)
c---bandwidth of u-or-h---
      nbwuh = min(ns1+2, ns2e+1)
c---number columns stored of the u matrix---
      if (     trunc) nuc = 1
      if (.not.trunc) nuc = ns2e
c---size of arnoldi-vector tables---
      nv = min(ns1,ns2e+1)
      nvt = nv
      if (iqr .and. .not.trunc) nvt = nv - 1
      if (iqr .and.      trunc) nvt = 1
c---number of givens rotations to store---
      nrot = min(ns1,ns2e)
c---number of elts of z-vector to store---
      if (     trunc) nzc = 2
      if (.not.trunc) nzc = ns2e + 1
c
c------------------------------memory layout---------------------------
c
      ihess = 1
                       ipt = ihess + nhess*nbwh
      if (.not.evadpt) ipt = ihess
                    ip = ipt + n*nvt
      if (.not.iqr) ip = ipt
      izc = ip + n*nv
      icos = izc + nzc
      isin = icos + nrot
      iy = isin + nrot
                                 iu = iy + ns2e
      if (trunc .or. .not.uneed) iu = iy
      ipz = iu + nbwuh*nuc
                 ipf = ipz + ns2e+1
      if (trunc) ipf = ipz
      iz = ipf + nv
      iw = iz + n
                       iwf = iw + n*nv
                       ixi = iwf + nv
                       iv1 = ixi + n
      if (.not. trunc) iv1 = iw
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2+n-1)
c
c --- check the memory usage ---
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      rstrtd = .true.
c
c--------------------------------------------------------------------
c---------------------------begin iteration loop---------------------
c--------------------------------------------------------------------
c
c handle first iteration after restart ...
c
 10   call inithv (1)
      zdhav = .not.(trunc.and..not.exact) .and. in .ne. 0
      if (.not. rstrtd) go to 100
c    ---get resid---
      if (.not. zhave) then
        if (iql) then
          call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
          call vexopy (n,wk(iv1),rhs,wk(iv1),2)
          call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iz))
        else
          call suba (coef,jcoef,wfac,jwfac,n,u,wk(iz))
          call vexopy (n,wk(iz),rhs,wk(iz),2)
        end if
        zhave = .true.
      end if
c    ---get resid norm---
      if (.not. zdhav) then
        zdot = vdot (n,wk(iz),wk(iz))
        zdhav = .true.
      end if
      if (zdot .lt. 0.0d0) go to 994
      vnorm = sqrt(zdot)
      if (vnorm .lt. srelpr**2) go to 997
      call vcopy (n,wk(iz),wk(indp(is)))
      wk(indpf(is)) = 1.0d0/vnorm
      wk(indzc(is+1)) = vnorm
c
c --- perform stopping test ---
c
 100  nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,wk(iz),xxx,
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c rstrt tells us whether this is the last step before restarting ...
      rstrt = (is+1 .eq. ns2)
      if (evadpt .and. is .eq. 0) 
     &         call vfill (nhess*nbwh,wk(ihess),0.0d0)
c
c-----------------------compute the new arnoldi vector----------------
c
c pn(is+1)*p(is+1) = a*p(is) + sum (i=0 to is) (beta(is+1,i)*p(i)),
c
c---get a times old vec---
      if (iqr) call subqr (coef,jcoef,wfac,jwfac,n,wk(indp (is)),
     a                                             wk(indpt(is)))
      if (iql) then
        call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(is)),wk(iv1))
        call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      else
        call suba (coef,jcoef,wfac,jwfac,n,wk(indpt(is)),wk(iv2))
      end if
      apf = wk(indpf(is))
c---compute arnoldi vector---
      ibeg = max (is+1-ns1,0)
      iend = is
      if (ibeg .gt. 0) wk(indu(ibeg,is+1)) = 0.0d0
      pfnew = apf
      do 199 i = ibeg,iend
        h = vdot (n,wk(indp(i)),wk(iv2)) * wk(indpf(i))*apf
        wk(indu(i+1,is+1)) = h
        if (evadpt) wk(indhes(i+1,is+1)) = h
        if (i .eq. ibeg) call vtriad (n,wk(indp(is+1)),wk(iv2),
     a                           -h*wk(indpf(i))/pfnew,wk(indp(i)),1)
        if (i .ne. ibeg) call vtriad (n,wk(indp(is+1)),wk(indp(is+1)),
     a                           -h*wk(indpf(i))/pfnew,wk(indp(i)),1)
 199  continue
      wk(indpf(is+1)) = pfnew
c---get norm---
      dot = vdot (n,wk(indp(is+1)),wk(indp(is+1))) * pfnew**2
      vnorm = sqrt(dot)
      if (vnorm .lt. srelpr**2) go to 192
      wk(indu(is+2,is+1)) = vnorm
      if (evadpt) wk(indhes(is+2,is+1)) = vnorm
c---scale---
      wk(indpf(is+1)) = wk(indpf(is+1))/vnorm
      if (abs(wk(indpf(is+1))) .lt. srelpr .or.
     a    abs(wk(indpf(is+1))) .gt. 1.0d0/srelpr) then
        call vtriad (n,wk(indp(is+1)),xxx,wk(indpf(is+1)),
     a                 wk(indp(is+1)),2)
        wk(indpf(is+1)) = 1.0d0
      end if
c
c-----------------------update the qr factorization-------------------
c
 192  continue
c---apply old rotations---
      ibgn = max(0,is-ns1)
      iuold = indu(ibgn+1,is+1)
      do 7977 i = ibgn, is-1
      iunew = indu(i+2,is+1)
      ut = wk(iuold)
      h  = wk(iunew)
      ctmp = wk(indc(i+1))
      stmp = wk(inds(i+1))
      wk(iuold) =  ctmp*ut + stmp*h
      wk(iunew) = -stmp*ut + ctmp*h
 7977 iuold = iunew
      iunew = indu(is+2,is+1)
c---calc new rotation---
      v1 = wk(iuold)
      v2 = wk(iunew)
      denom = sqrt (v1**2 + v2**2)
      if (denom .lt. srelpr) go to 998
      wk(indc(is+1)) = v1/denom
      wk(inds(is+1)) = v2/denom
c---apply new rotation---
      wk(iuold) = denom
      wk(iunew) = 0.0d0
c
c--------------------------compute w, if needed------------------------
c
      uc = wk(indu(is+1,is+1))
      if (abs(uc) .lt. srelpr**2) go to 998
      if (.not.trunc) go to 572
c
c ... case of explicit w calc ...
c
      if (is .eq. 0) then
        call vcopy (n,wk(indpt(is)),wk(indw(1)))
        wk(indwf(is+1)) = wk(indpf(is))/uc
c       call vtriad (n,wk(indw(is+1)),xxx,1.0d0/uc,wk(indpt(is)),2)
        go to 572
      end if
      wfnew = wk(indpf(is))
      ibeg = max (1,is+1-ns1)
      iend = is
      do 574 i = ibeg, iend
      if (i .eq. ibeg) call vtriad (n,wk(indw(is+1)),wk(indpt(is)),
     a   -wk(indu(i,is+1))*wk(indwf(i))/wfnew,wk(indw(i)),1)
      if (i .ne. ibeg) call vtriad (n,wk(indw(is+1)),wk(indw(is+1)),
     a   -wk(indu(i,is+1))*wk(indwf(i))/wfnew,wk(indw(i)),1)
 574  continue
      wk(indwf(is+1)) = wfnew/uc
      if (abs(wk(indwf(is+1))) .lt. srelpr .or.
     a    abs(wk(indwf(is+1))) .gt. 1.0d0/srelpr) then
        call vtriad (n,wk(indw(is+1)),xxx,wk(indwf(is+1)),
     a                 wk(indw(is+1)),2)
        wk(indwf(is+1)) = 1.0d0
      end if
 572  continue
c
c---get new zc entries---
      wk(indzc(is+2)) = -wk(inds(is+1))*wk(indzc(is+1))
      wk(indzc(is+1)) =  wk(indc(is+1))*wk(indzc(is+1))
c
c------------------- u-vector computation section ---------------------
c
      if (trunc) then
c
c---truncated case---
        call vtriad (n,u,u,wk(indzc(is+1))*wk(indwf(is+1)),
     a               wk(indw(is+1)),1)
      else
c
c---non-truncated case---
        if (.not.(uneed .or. rstrt)) go to 410
        iynew = iv1
        nwusd = max (nwusd,iynew+ns2e-1)
        if (nwusd .gt. nw) go to 999
c     ---do back solve on u-matrix---
        nm = is + 1
        do 623 i = nm, 1, -1
        sum = wk(indzc(i))
        do 624 j = i+1, nm
 624    sum = sum - wk(iynew-1+j)*wk(indu(i,j))
 623    wk(iynew-1+i) = sum/wk(indu(i,i))
c     ---form iterate---
        do 625 i = 0, nm-1
        val = wk(iynew+i)
        if (uneed .and. i.ne.nm-1) val = val - wk(iy+i)
 625    call vtriad (n,u,u,val*wk(indpf(i)),wk(indpt(i)),1)
        if (uneed) call vcopy (nm,wk(iynew),wk(iy))
      endif
 410  continue
c
c--------------------- residual computation section -------------------
c
      zhvold = zhave
      zhave = .false.
      if (trunc) go to 671
c
c---non-truncated case---
c
c do it if resid needed by pstop or if restarting.
      if (zneed .or. rstrt) then
        ipznew = iv1
        nwusd = max (nwusd,ipznew+ns2e)
        if (nwusd .gt. nw) go to 999
        call vcopy (is+1,wk(izc),wk(ipznew))
        wk(ipznew+is+1) = 0.0d0
c     ---apply rotations---
        do 644 i = is+1, 1, -1
          v1 = wk(indc(i))*wk(ipznew+i-1) - wk(inds(i))*wk(ipznew+i)
          v2 = wk(inds(i))*wk(ipznew+i-1) + wk(indc(i))*wk(ipznew+i)
          wk(ipznew+i-1) = v1
 644      wk(ipznew+i)   = v2
c     ---form resid---
        do 645 i = 0, is+1
        val = wk(ipznew+i)
        if (zhvold .and. i.ne.is+1) val = val - wk(ipz)
 645    call vtriad (n,wk(iz),wk(iz),-val*wk(indpf(i)),
     a               wk(indp(i)),1)
        call vcopy (is+2,wk(ipznew),wk(ipz))
        zhave = .true.
      end if
      go to 425
c
c---truncated case---
c
c do it if pstop needs it or if we may restart later.
 671  if ( zneed .or. (itmax.gt.ns2) ) then
c     ---update xi---
        if (is .eq. 0) then
          call vcopy (n,wk(indp(is)),wk(ixi))
          xif = wk(indpf(is))
        else
          xif = xif*(-wk(inds(is)))
          call vtriad (n,wk(ixi),wk(ixi),
     a           wk(indc(is))*wk(indpf(is))/xif,wk(indp(is)),1)
        end if
        if (abs(xif).lt.srelpr .or.
     a      abs(xif) .gt. 1.0d0/srelpr) then
          call vtriad (n,wk(ixi),xxx,xif,wk(ixi),2)
          xif = 1.0d0
        end if
c     ---form resid---
        call vtriad (n,wk(iz),wk(iz),
     a                -wk(indzc(is+1))*wk(indc(is+1))*xif,wk(ixi),1)
        call vtriad (n,wk(iz),wk(iz),
     a                -wk(indzc(is+1))*wk(inds(is+1))*wk(indpf(is+1)),
     a                 wk(indp(is+1)),1)
        zhave = .true.
      endif
 425  continue
c
c---get resid norm---
c
      if (exact) then
        zdot = wk(indzc(is+2))**2
      end if
c
c--------------------------------ev est-------------------------------
c
      if (evadpt) then
        nwhe = nw - (iv1-1)
        call hesest (wk(ihess),nhess,nv+2,is+1,iadpt,havest,
     a               emaxnw,eminnw,wk(iv1),nwhe,ier)
        nwusd = max (nwusd,iv1-1+nwhe)
        if (ier .ne. 0) go to 996
        if (.not. havest) go to 874
        if (hadest) go to 876
        if (maxadd) emax = emaxnw
        if (minadd) emin = eminnw
        hadest = .true.
        go to 874
 876    if (maxadd) emax = max (emax,emaxnw)
        if (minadd) emin = min (emin,eminnw)
      end if
c
c-------------------------finish up the iteration----------------------
c
 874  in = in + 1
      is = is + 1
      if (rstrt) is = 0
      rstrtd = rstrt
      go to 10
c
c----------------------------------------------------------------------
c------------------------------wrap it up------------------------------
c----------------------------------------------------------------------
c
c---form u, if not up-to-date---
c
 900  if (uneed .or. rstrtd .or. trunc) go to 901
        iynew = iv1
        nwusd = max (nwusd,iynew+ns2e-1)
        if (nwusd .gt. nw) go to 999
c     ---do back solve on u-matrix---
        nm = is
        do 663 i = nm, 1, -1
        sum = wk(indzc(i))
        do 664 j = i+1, nm
 664    sum = sum - wk(iynew-1+j)*wk(indu(i,j))
 663    wk(iynew-1+i) = sum/wk(indu(i,i))
c     ---form iterate---
        do 665 i = 0, nm-1
        val = wk(iynew+i)
 665    call vtriad (n,u,u,val*wk(indpf(i)),wk(indpt(i)),1)
c
c----------------------------------------------------------------------
c-----------------------------head out of here-------------------------
c----------------------------------------------------------------------
c
 901  continue
      if (halt) go to 715
      ier = 1
      call ershow (ier,'gmresw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' gmres converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c----------------------------error returns-----------------------------
c
 994  ier = -15
      call ershow (ier,'gmresw')
      return
c
 995  ier = -16
      call ershow (ier,'gmresw')
      return
c
 996  call ershow (ier,'gmresw')
      go to 735
c
 997  ier = -13
      call ershow (ier,'gmresw')
      go to 725
c
 998  ier = -14
      call ershow (ier,'gmresw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'gmresw')
      go to 735
c
      end
      subroutine uslqw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the usymlq algorithm.  see: m. a. saunders, h. d. simon
c and e. l. yip, "two conjugate-gradient-type methods for sparse
c unsymmetric linear equations, report eta-tr-18, boeing computer
c services, seattle, washington, 1984, to appear in siam journal on
c numerical analysis.
c
c note -- this routine is still not quite optimal.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      integer vect1, vect2, os
      logical uneed
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c next, the indexing functions.
c
      indv1(i) = vect1 + mod(i,nv)*n
      indv2(i) = vect2 + mod(i,nv)*n
      indbe1(i) = ibeta1 + mod(i,os)
      indbe2(i) = ibeta2 + mod(i,os)
      indc(i) = icos + mod(i,os)
      inds(i) = isin + mod(i,os)
      indu(i) = iu + mod(i,os+1)
      indw(i) = iw + n*mod(i,os)
c
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 12
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 996
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' usymlq')
c
c initialize the stopping test ...
c
      call inithv (0)
      zdhav = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c ... associated integer variables.
c
      os = 2
      iv =  1
      nv = os
      iw =  1
      vect1 = iw + iv*n*os
      vect2 = vect1 + iv*n*nv
      ibeta1 = vect2 + iv*n*nv
      ibeta2 = ibeta1 + os
      icos = ibeta2 + os
      isin = icos + os
      iu = isin + os
      iv1 = iu + os + 1
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2-1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      uneed = rcalp .or. zcalp .or. ztcalp .or. udhav
     a   .or. ntest .eq. 6 .or. level .ge. 3
c
c------------------------begin iteration loop-----------------------
c
c perform first-iterate calculations ...
c
 10   if (in .ne. 0) go to 100
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      vnorm1 = sqrt(vdot(n,wk(iv2),wk(iv2)))
      vnorm2 = vnorm1
      if (abs(vnorm1) .lt. srelpr) go to 997
      gamma1 = 1.0d0/vnorm1
      gamma2 = 1.0d0/vnorm2
      call vtriad (n,wk(indv1(0)),xxx,gamma1,wk(iv2),2)
      call vcopy (n,wk(indv1(0)),wk(indv2(0)))
      zdot = vnorm1**2
      ucnp1= 0.0d0
c
c determine whether or not to stop --
c
 100  call inithv (1)
      zdhav = .true.
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c ... compute q(n+1), etc -- the direction vectors
c
      call suba (coef,jcoef,wfac,jwfac,n,wk(indv1(in)),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      an = vdot (n,wk(indv2(in)),wk(iv2))
      if (in .ne. 0) go to  110
      call vtriad (n,wk(indv2(in+1)),wk(iv2),-an,wk(indv2(in)), 1)
      wk(indbe2(in)) = -an
      go to 111
 110  call vtriad (n,wk(indv2(in+1)),xxx,-vnorm1,wk(indv2(in-1)),2)
      call vtriad (n,wk(indv2(in+1)),wk(indv2(in+1)),1.0d0,wk(iv2),1)
      call vtriad (n,wk(indv2(in+1)),wk(indv2(in+1)),-an,
     a              wk(indv2(in)),1)
      wk(indbe2(in)) = -an
      wk(indbe2(in-1)) = -vnorm1
 111  vn2old = vnorm2
      vnorm2 = sqrt(vdot (n,wk(indv2(in+1)),wk(indv2(in+1))))
      if (abs(vnorm2) .lt. srelpr) go to 997
      gamma2 = 1.0d0/vnorm2
      call vtriad (n,wk(indv2(in+1)),xxx,gamma2,wk(indv2(in+1)),2)
c
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(indv2(in)),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      if (in .ne. 0) go to 810
      call vtriad (n,wk(indv1(in+1)),wk(iv2),-an,wk(indv1(in)),1)
      wk(indbe1(in)) = -an
      go to 811
 810  call vtriad (n,wk(indv1(in+1)),xxx,-vn2old,wk(indv1(in-1)),2)
      call vtriad (n,wk(indv1(in+1)),wk(indv1(in+1)),1.0d0,wk(iv2),1)
      call vtriad (n,wk(indv1(in+1)),wk(indv1(in+1)),-an,wk(indv1(in)),
     a             1)
      wk(indbe1(in)) = -an
      wk(indbe1(in-1)) = -vn2old
 811  vn1old= vnorm1
      vnorm1 = sqrt(vdot (n,wk(indv1(in+1)),wk(indv1(in+1))))
      if (abs(vnorm1) .lt. srelpr) go to 997
      gamma1 = 1.0d0/vnorm1
      call vtriad (n,wk(indv1(in+1)),xxx,gamma1,wk(indv1(in+1)),2)
c
c ... now update the factorization
      ucnbar = ucnp1
      ibgn = max (0,in+1-os)
      do 1 i = ibgn,in
 1    wk(indu(i+1)) = -wk(indbe2(i))
      if (ibgn .gt. 0) wk(indu(ibgn))= 0.0d0
      call qrupd (in+1,os+1,os,wk(icos),wk(isin),ucnbar,ucn,wk(iu),
     a  vn2old,ier)
      if (ier .ne. 0) go to 998
      ucnp1 = wk(indu(in+1))
c
c ... update the old w vector.
c
      if (in .ne. 0)
     a   call vtriad (n,wk(indw(in-1)),xxx,ucnbar/ucn,wk(indw(in-1)),2)
c
c ... now generate the new w vector.
c
      if (abs(ucnp1) .lt. srelpr) go to 998
      call vcopy (n,wk(indv1(in)),wk(iv1))
      ibgn = max (1,in-os+1)
      iend = in
      if (iend .lt. ibgn) go to 200
      do 201 i = ibgn,iend
 201  call vtriad (n,wk(iv1),wk(iv1),-wk(indu(i)),wk(indw(i-1)),1)
 200  continue
      call vtriad (n,wk(indw(in)),xxx,1.0d0/ucnp1,wk(iv1),2)
      if (in .ne. 0) go to 205
c
c ... update iterate u(0).
      zold= 0.0d0
      zbar = vn1old
      if (uneed) call vtriad (n,u,u,zbar,wk(indw(0)), 1)
      go to 210
c
c ... update subsequent iterates u(n).
c
 205  zold = wk(indc(in))*zbar
      zbold = zbar
      zbar =-wk(inds(in))*zbar
      factor = zold
      if (uneed) factor = factor - zbold*ucn/ucnbar
      call vtriad (n,u,u,factor,wk(indw(in-1)), 1)
      if (uneed) call vtriad (n,u,u,zbar,wk(indw(in)), 1)
 210  continue
      zdot = (zbar/ucnp1*vnorm1)**2
c
c proceed to next iteration
c
      in = in +  1
      is = is + 1
      go to  10
c
c------------------------------finish up-------------------------------
c
 900  if (.not. uneed) call vtriad (n,u,u,zbar,wk(indw(in-1)),1)
      if (halt) go to 715
      ier = 1
      call ershow (ier,'uslqw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' usymlq converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
c unimplemented option
 995  ier = -16
      call ershow (ier,'uslqw')
      return
 996  call ershow (ier,'uslqw')
      go to 735
c
 997  ier = -13
      call ershow (ier,'uslqw')
      go to 725
c
 998  ier = -14
      call ershow (ier,'uslqw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'uslqw')
      go to 735
c
      end
      subroutine usqrw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the usymqr algorithm. same reference as usymlq
c algorithm.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      integer vect1, vect2, os
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c next, the indexing functions.
c
      indv 1(i)= vect1 + mod(i,nv)*n
      indv2(i) = vect2 + mod(i,nv)*n
      indbe 1(i)= ibeta1 + mod(i,os)
      indbe2(i) = ibeta2 + mod(i,os)
      indc(i) = icos + mod(i,os+ 1)
      inds(i) = isin + mod(i,os+ 1)
      indu(i) = iu + mod(i,os+2)
      indw(i) = iw + n*mod(i,os)
c
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel =  13
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 996
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' usymqr')
c
c initialize the stopping test ...
c
      call inithv (0)
      zdhav = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c ... associated integer variables.
c
      os = 2
      iv =  1
      nv = os
      iw =  1
      vect1 = iw + iv*n*os
      vect2 = vect1 + iv*n*nv
      ibeta1 = vect2 + iv*n*nv
      ibeta2 = ibeta1 + os
      icos = ibeta2 + os
      isin = icos + os+ 1
      iu = isin + os+ 1
      iv1 = iu + os+2
      iv2 = iv1 + n
      nwusd = max (nwusd,iv2- 1+n)
c
c check the memory usage --
c
      if (nwusd .gt. nw) go to 999
c
c
c now, perform first-iterate calculations
      in = 0
      is = 0
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      vnorm1 = sqrt(vdot (n,wk(iv2),wk(iv2)))
      vnorm2 = vnorm1
      if (abs(vnorm1) .lt. srelpr) go to 997
      gamma1 = 1.0d0/vnorm1
      gamma2 = 1.0d0/vnorm2
      call vtriad (n,wk(indv1(0)),xxx,gamma1,wk(iv2),2)
      call vcopy (n,wk(indv1(0)),wk(indv2(0)))
      zdot = vnorm1**2
      znext = vnorm1
c
c------------------------begin iteration loop------------------------
c
c determine whether or not to stop --
c
 10   call inithv (1)
      zdhav = .true.
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c
c ... compute q(n+1), etc -- the direction vectors
      call suba (coef,jcoef,wfac,jwfac,n,wk(indv1(in)),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      an = vdot (n,wk(indv2(in)),wk(iv2))
      if (in .ne. 0) go to 110
      call vtriad (n,wk(indv2(in+1)),wk(iv2),-an,wk(indv2(in)),1)
      wk(indbe2(in)) = -an
      go to 111
 110  call vtriad (n,wk(indv2(in+1)),xxx,-vnorm1,wk(indv2(in-1)),2)
      call vtriad (n,wk(indv2(in+1)),wk(indv2(in+1)),1.0d0,wk(iv2),1)
      call vtriad (n,wk(indv2(in+1)),wk(indv2(in+1)),-an,wk(indv2(in)),
     a             1)
      wk(indbe2(in)) = -an
      wk(indbe2(in-1)) = -vnorm1
 111  vn2old = vnorm2
      vnorm2 = sqrt(vdot (n,wk(indv2(in+1)),wk(indv2(in+1))))
      if (abs(vnorm2) .lt. srelpr) go to 997
      gamma2 = 1.0d0/vnorm2
      call vtriad (n,wk(indv2(in+1)),xxx,gamma2,wk(indv2(in+1)),2)
c
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(indv2(in)),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      if (in .ne. 0) go to 810
      call vtriad (n,wk(indv1(in+1)),wk(iv2),-an,wk(indv1(in)),1)
      wk(indbe1(in)) = -an
      go to 811
 810  call vtriad (n,wk(indv1(in+1)),xxx,-vn2old,wk(indv1(in-1)),2)
      call vtriad (n,wk(indv1(in+1)),wk(indv1(in+1)),1.0d0,wk(iv2),1)
      call vtriad (n,wk(indv1(in+1)),wk(indv1(in+1)),-an,wk(indv1(in)),
     a             1)
      wk(indbe1(in)) = -an
      wk(indbe1(in-1)) = -vn2old
 811  vnorm1 = sqrt(vdot (n,wk(indv1(in+1)),wk(indv1(in+1))))
      if (abs(vnorm1) .lt. srelpr) go to 997
      gamma1 = 1.0d0/vnorm1
      call vtriad (n,wk(indv1(in+1)),xxx,gamma1,wk(indv1(in+1)),2)
c
c ... now update the factorization
      ibgn = max (0,in+1-os)
      do 1 i = ibgn,in
 1    wk(indu(i+1)) = -wk(indbe2(i))
      if (ibgn .gt. 0) wk(indu(ibgn))= 0.0d0
      wk(indu(in+2)) = vnorm2
      call qrupd (in+2,os+2,os+1,wk(icos),wk(isin),wk(indu(in+1)),x,
     a  wk(iu),vnorm2,ier)
      if (ier .lt. 0) go to 998
c
c ... now generate the new w vector.
      uc = wk(indu(in+1))
      if (abs(uc) .lt. srelpr) go to 998
      call vcopy (n,wk(indv1(in)),wk(iv1))
      ibgn = max (1,in-os+1)
      iend = in
      if (iend .lt. ibgn) go to 200
      do 201 i = ibgn,iend
 201  call vtriad (n,wk(iv1),wk(iv1),-wk(indu(i)),wk(indw(i-1)),1)
 200  continue
      call vtriad (n,wk(indw(in)),xxx,1.0d0/uc,wk(iv1),2)
c
c ... update iterates u(n).
      z = wk(indc(in+1))*znext
      znext = -wk(inds(in+1))*znext
      call vtriad (n,u,u,z,wk(indw(in)),1)
      zdot = znext**2
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      go to 10
c
c-----------------------------finish up----------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'usqrw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' usymqr converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'usqrw')
      return
c
 996  call ershow (ier,'usqrw')
      go to 735
c
 997  ier = -13
      call ershow (ier,'usqrw')
      go to 725
c
 998  ier = -14
      call ershow (ier,'usqrw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'usqrw')
      go to 735
c
      end
      subroutine ldirw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the lanczos/orthodir algorithm. see jea and young, in
c linear algebra and its applications, vol 52/3, 1983, pp399f.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c indexing functions.
c
      indq(i) = iq + n*mod(i,2)
      indqt(i) = iqt + n*mod(i,2)
c
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 14
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' landir')
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave  = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
      iq = 1
      iqt = iq + 2*n
      ir = iqt + 2*n
      iv1 = ir + n
      iv2 = iv1 + n
      iv3 = iv2 + n
      nwusd = max (nwusd,iv3-1+n)
c
c check the memory usage.
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(ir))
c
c begin iteration loop
c
c determine whether or not to stop.
c
 10   call inithv (1)
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,wk(ir),xxx,
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
      if (in .ne. 0) go to 110
c
c perform first-iterate calculations
c
      call vcopy (n,wk(ir),wk(indq(in)))
      call vcopy (n,wk(indq(in)),wk(indqt(in)))
      qaq= 0.0d0
      go to 115
c
c proceed to calculate the direction vectors, for in .gt. 0.
c
 110  call subqlt (coef,jcoef,wfac,jwfac,n,wk(indqt(in-1)),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv3))
      aqaq = vdot(n,wk(iv2),wk(iv3))
      an = aqaq / qaq
      if (in .ne. 1) go to 150
      call vtriad (n,wk(indq(in)),wk(iv2),-an,wk(indq(in-1)),1)
      call vtriad (n,wk(indqt(in)),wk(iv3),-an,wk(indqt(in-1)),1)
      go to 115
 150  bn = qaq / qaqold
      call vtriad (n,wk(indq(in)),wk(iv2),-bn,wk(indq(in-2)),1)
      call vtriad (n,wk(indq(in)),wk(indq(in)),-an,wk(indq(in-1)),1)
      call vtriad (n,wk(indqt(in)),wk(iv3),-bn,wk(indqt(in-2)),1)
      call vtriad (n,wk(indqt(in)),wk(indqt(in)),-an,wk(indqt(in-1)),1)
c
c proceed to form the iterate.
c
 115  call suba (coef,jcoef,wfac,jwfac,n,wk(indq(in)),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      qaqold = qaq
      qaq = vdot(n,wk(indqt(in)),wk(iv2))
      if (abs(qaq) .lt. srelpr) go to 998
      qr = vdot(n,wk(indqt(in)),wk(ir))
      vlamda = qr / qaq
      call vtriad (n,u,u,vlamda,wk(indq(in)),1)
      call vtriad (n,wk(ir),wk(ir),-vlamda,wk(iv2),1)
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      go to 10
c
c-----------------------------finish up-----------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'ldirw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' lanczos/orthodir converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'ldirw')
      return
c
 997  call ershow (ier,'ldirw')
      go to 735
c
 998  ier = -15
      call ershow (ier,'ldirw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'ldirw')
      go to 735
c
      end
      subroutine lminw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the lanczos/orthomin algorithm.
c here, zhat and phat will refer to the "dummy" system of the
c lanczos method.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      integer vect1, vect2, os
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c
      nwusd = 0
      ier = 0
      iacel = 15
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      if (level .ge. 2) write (nout,496)
496   format (' lanmin')
c
c initialize the stopping test ...
c
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      call inithv (0)
      zhave  = .true.
      zthave = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c allocate memory -- overlap wherever possible ...
      ip = 1
      ipt = ip + n
      if (.not. iqr) ipt = ip
      iphat = ipt + n
      iz = iphat + n
      izt = iz + n
      if (.not. iqr) izt = iz
      izhat = izt + n
      iv1 = izhat + n
      iv2 = iv1 + n
      if (iqlr .eq. 0) nwusd = max (nwusd,iv1-1+n)
      if (iqlr .ne. 0) nwusd = max (nwusd,iv2-1+n)
c
c check the memory usage.
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      if (.not. iql) go to 121
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iz))
      go to 122
 121  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iz))
      call vexopy (n,wk(iz),rhs,wk(iz),2)
 122  if (iqr) call subqr (coef,jcoef,wfac,jwfac,n,wk(iz),wk(izt))
c
c============================begin iteration loop======================
c
c determine whether or not to stop.
c
 10   call inithv (1)
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,wk(iz),wk(izt),
     a            wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
      if (in .ne. 0) go to 110
c
c perform first-iterate calculations
c
      call vcopy (n,wk(iz),wk(izhat))
      rd = vdot (n,wk(iz),wk(izhat))
      call vcopy (n,wk(iz),wk(ip))
      call vcopy (n,wk(izhat),wk(iphat))
      if (iqr) call vcopy (n,wk(izt),wk(ipt))
      go to 111
c
c perform subsequent-iterate calculations
c
 110  rdold = rd
c     if (abs(rdold) .lt. srelpr) go to 996
      if (abs(rdold) .eq. 0.0d0) go to 996
c
c form the old zhat ...
      go to (131,132,133,134), iqlr + 1
 131  call subat (coef,jcoef,wfac,jwfac,n,wk(iphat),wk(iv1))
      go to 135
 132  call subqlt (coef,jcoef,wfac,jwfac,n,wk(iphat),wk(iv2))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iv1))
      go to 135
 133  call subat (coef,jcoef,wfac,jwfac,n,wk(iphat),wk(iv2))
      call subqrt (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iv1))
      go to 135
 134  call subqlt (coef,jcoef,wfac,jwfac,n,wk(iphat),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      call subqrt (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iv1))
 135  call vtriad (n,wk(izhat),wk(izhat),-vlamda,wk(iv1),1)
c
c form the direction vectors ...
      rd = vdot (n,wk(iz),wk(izhat))
      an = rd/rdold
      call vtriad (n,wk(ip),wk(iz),an,wk(ip),1)
      call vtriad (n,wk(iphat),wk(izhat),an,wk(iphat),1)
      if (iqr) call vtriad (n,wk(ipt),wk(izt),an,wk(ipt),1)
c
c============================form the iterate========================
c
 111  if (iql) go to 141
      call suba (coef,jcoef,wfac,jwfac,n,wk(ipt),wk(iv1))
      go to 142
 141  call suba (coef,jcoef,wfac,jwfac,n,wk(ipt),wk(iv2))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iv1))
 142  pap = vdot (n,wk(iphat),wk(iv1))
c     if (abs(pap) .lt. srelpr**2) go to 998
      if (abs(pap) .eq. 0.0d0) go to 998
      vlamda = rd/pap
c
c
      call vtriad (n,u,u,vlamda,wk(ipt),1)
      call vtriad (n,wk(iz),wk(iz),-vlamda,wk(iv1),1)
      if (.not. iqr) go to 151
      call subqr (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iv2))
      call vtriad (n,wk(izt),wk(izt),-vlamda,wk(iv2),1)
c
c proceed to next iteration
c
 151  in = in + 1
      is = is + 1
      go to 10
c
c------------------------------finish up-----------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'lminw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' lanczos/orthomin converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 996  ier = -13
      call ershow (ier,'lminw')
      go to 725
c
 997  call ershow (ier,'lminw')
      go to 735
c
 998  ier = -15
      call ershow (ier,'lminw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'lminw')
      go to 735
c
      end
      subroutine lresw (suba,subat,subql,subqlt,subqr,subqrt,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the lanczos/orthores algorithm.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subat, subql, subqlt, subqr, subqrt
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
c indexing functions.
c
      indu(i) = iu + n*mod(i,nv)
      indr(i) = ir + n*mod(i,nv)
      indrt(i) = irt + n*mod(i,nv)
c
c preliminary calculations.
c
      nwusd = 0
      ier = 0
      iacel = 16
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
      if (level .ge. 2) write (nout,496)
496   format (' lanres')
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave  = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
      nv = 2
      iu = 1
      ir = iu + nv*n
      irt = ir + nv*n
      iv1 = irt + nv*n
      nwusd = max (nwusd,iv1-1+n)
c
c check the memory usage.
c
      if (nwusd .gt. nw) go to 999
c
c note -- we will use the vector 'u' for scratch storage, to save space.
c
      call vcopy (n,u,wk(indu(0)))
      in = 0
      is = 0
      call suba (coef,jcoef,wfac,jwfac,n,wk(indu(in)),wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(indr(in)))
      call vcopy (n,wk(indr(in)),wk(indrt(in)))
c
c--------------------------begin iteration loop-----------------------
c
c determine whether or not to stop.
c
 10   call inithv (1)
      nwpstp = nw - (iv1-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a     wfac,jwfac,n,wk(indu(in)),ubar,rhs,xxx,wk(indr(in)),xxx,
     a     wk(iv1),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv1-1)
      if (level .ge. 2) call iterm (n,wk(indu(in)))
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
c proceed to calculate the parameters.
c first, gamma.
c
      rd = vdot (n,wk(indr(in)),wk(indrt(in)))
      call suba (coef,jcoef,wfac,jwfac,n,wk(indr(in)),wk(iv1))
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),u)
      rar = vdot (n,u,wk(indrt(in)))
      if (abs(rar) .lt. srelpr) go to 998
      gam = rd / rar
c
c now, rho.
c
      if (in .ne. 0) go to 118
      rho = 1.0d0
      go to 119
 118  if (abs(gamold) .lt. srelpr) go to 998
      if (abs(rdold) .lt. srelpr) go to 998
      if (abs(rho) .lt. srelpr) go to 998
      rhoinv = 1.0d0 - (gam/gamold)*(rd/rdold)/rho
      if (abs(rhoinv) .lt. srelpr) go to 998
      rho = 1.0d0 / rhoinv
c
c now work on updating u, r, rt.
c first, the first iteration.
c
 119  if (in .ne. 0) go to 150
      call vtriad (n,wk(indu(in+1)),wk(indu(in)),gam,wk(indr(in)),1)
      call vtriad (n,wk(indu(in+1)),xxx,rho,wk(indu(in+1)),2)
      call vtriad (n,wk(indr(in+1)),wk(indr(in)),-gam,u,1)
      call vtriad (n,wk(indr(in+1)),xxx,rho,wk(indr(in+1)),2)
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(indrt(in)),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),u)
      call vtriad (n,wk(indrt(in+1)),wk(indrt(in)),-gam,u,1)
      call vtriad (n,wk(indrt(in+1)),xxx,rho,wk(indrt(in+1)),2)
      go to 151
c
c now work on subsequent iterations.
c
 150  call vtriad (n,wk(indu(in+1)),xxx,1.0d0-rho,wk(indu(in-1)),2)
      call vtriad (n,wk(indu(in+1)),wk(indu(in+1)),rho,wk(indu(in)),1)
      call vtriad (n,wk(indu(in+1)),wk(indu(in+1)),rho*gam,
     a            wk(indr(in)),1)
      call vtriad (n,wk(indr(in+1)),xxx,1.0d0-rho,wk(indr(in-1)),2)
      call vtriad (n,wk(indr(in+1)),wk(indr(in+1)),rho,wk(indr(in)),1)
      call vtriad (n,wk(indr(in+1)),wk(indr(in+1)),-rho*gam,u,1)
      call subqlt (coef,jcoef,wfac,jwfac,n,wk(indrt(in)),wk(iv1))
      call subat (coef,jcoef,wfac,jwfac,n,wk(iv1),u)
      call vtriad (n,wk(indrt(in+1)),xxx,1.0d0-rho,wk(indrt(in-1)),
     a             2)
      call vtriad (n,wk(indrt(in+1)),wk(indrt(in+1)),rho,wk(indrt(in)),
     a             1)
      call vtriad (n,wk(indrt(in+1)),wk(indrt(in+1)),-rho*gam,u,1)
c
c proceed to next iteration
c
 151  gamold = gam
      rdold = rd
      in = in + 1
      is = is + 1
      go to 10
c
c-------------------------------finish up----------------------------
c
 900  call vcopy (n,wk(indu(in)),u)
      if (halt) go to 715
      ier = 1
      call ershow (ier,'lresw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' lanczos/orthores converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'lresw')
      return
c
 997  call ershow (ier,'lresw')
      go to 735
c
 998  ier = -15
      call ershow (ier,'lresw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'lresw')
      go to 735
c
      end
      subroutine bcgsw (suba,subql,subqr,
     a coef,jcoef,wfac,jwfac,n,u,ubar,rhs,wk,nw,iparm,rparm,ier)
      implicit double precision (a-h, o-z)
c
c code to run the biconjugate-gradient-squared algorithm.
c the algorithm is taken from "preconditioned biconjugate gradient
c methods for numerical reservoir simulation", by p. joly and r.
c eymard, to appear in journal of computational physics.  the original
c reference  is p. sonneveld, "cgs, a fast lanczos-type solver for
c unsymmetric linear systems," report 84-16, delft university of
c technology, dept. of mathematics and informatics.
c
      dimension u(1), ubar(1), rhs(1), wk(1), coef(1), jcoef(2),
     a          wfac(1), jwfac(1)
      external suba, subql, subqr
      dimension iparm(30), rparm(30)
      logical iql, iqr
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
      nwusd = 0
      ier = 0
      iacel = 15
      t1 = timer (dummy)
      call echall (n,iparm,rparm,1,2,ier)
      if (ier .lt. 0) go to 997
      if (level .ge. 2) write (nout,496)
496   format (' bcgs')
      iql = iqlr .eq. 1 .or. iqlr .eq. 3
      iqr = iqlr .eq. 2 .or. iqlr .eq. 3
      if (iqr) go to 995
c
c initialize the stopping test ...
c
      call inithv (0)
      zhave  = .true.
      nwpstp =  nw
      call pstop (0,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,xxx,xxx,
     a            wk,nwpstp,ier)
      nwusd = max (nwusd,nwpstp)
      if (ier .lt. 0) go to 730
c
c allocate memory -- overlap wherever possible ...
      ir0 = 1
      ip = ir0 + n
      ipt = ip + n
      if (.not. iqr) ipt = ip
      iq = ipt + n
      iz = iq + n
      izt = iz + n
      if (.not. iqr) izt = iz
      iv1 = izt + n
      iv2 = iv1 + n
      iv3 = iv2 + n
      nwusd = max (nwusd,iv3-1+n)
      ipaaq  = iv1
      ippaaq = iv2
c
c check the memory usage.
c
      if (nwusd .gt. nw) go to 999
c
      in = 0
      is = 0
      if (.not. iql) go to 121
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(iv1))
      call vexopy (n,wk(iv1),rhs,wk(iv1),2)
      call subql (coef,jcoef,wfac,jwfac,n,wk(iv1),wk(iz))
      go to 122
 121  call suba (coef,jcoef,wfac,jwfac,n,u,wk(iz))
      call vexopy (n,wk(iz),rhs,wk(iz),2)
 122  if (iqr) call subqr (coef,jcoef,wfac,jwfac,n,wk(iz),wk(izt))
c
c=======================begin iteration loop=======================
c
c determine whether or not to stop.
c
 10   call inithv (1)
      nwpstp = nw - (iv2-1)
      call pstop (1,suba,subql,subqr,coef,jcoef,
     a            wfac,jwfac,n,u,ubar,rhs,xxx,wk(iz),wk(izt),
     a            wk(iv2),nwpstp,ier)
      nwusd = max (nwusd,nwpstp+iv2-1)
      if (level .ge. 2) call iterm (n,u)
      if (halt .or. in .ge. itmax .or. ier .lt. 0) go to 900
c
      if (in .ne. 0) go to 110
c
c perform first-iterate calculations
c
      call vcopy (n,wk(iz),wk(ir0))
      call vcopy (n,wk(iz),wk(ip))
      call vcopy (n,wk(iz),wk(iq))
      r0r = vdot (n,wk(iz),wk(ir0))
      go to 111
c
c perform subsequent-iterate calculations
c
 110  r0rold = r0r
      r0r = vdot (n,wk(ir0),wk(iz))
      if (abs(r0rold) .lt. srelpr**2) go to 996
      beta = r0r/r0rold
c
c form direction vectors ...
c
      call vtriad (n,wk(ip),wk(iz),beta,wk(ipaaq),1)
      call vtriad (n,wk(iv2),wk(ipaaq),beta,wk(iq),1)
      call vtriad (n,wk(iq),wk(ip),beta,wk(iv2),1)
c
c==========================form the iterate==========================
c
c at this point we have the vectors p and q and the new dot(r,r0) ...
c now form aq ...
c
 111  iaq = iv1
      if (.not.iql) then
        call suba (coef,jcoef,wfac,jwfac,n,wk(iq),wk(iaq))
      else
        call suba (coef,jcoef,wfac,jwfac,n,wk(iq),wk(iv2))
        call subql (coef,jcoef,wfac,jwfac,n,wk(iv2),wk(iaq))
      end if
c dot(r0,aq) ...
      r0aq = vdot (n,wk(ir0),wk(iaq))
      if (abs(r0aq) .lt. srelpr**2) go to 998
      alpha = r0r / r0aq
c p-alpha*aq, p+p-alpha*aq ...
      call vtriad (n,wk(ipaaq), wk(ip),-alpha,wk(iaq),1)
      call vexopy (n,wk(ippaaq),wk(ip),wk(ipaaq),1)
c
c---get u---
      call vtriad (n,u,u,alpha,wk(ippaaq),1)
c
c---get resid---
      if (.not.iql) then
        call suba (coef,jcoef,wfac,jwfac,n,wk(ippaaq),wk(iv3))
        call vtriad (n,wk(iz),wk(iz),-alpha,wk(iv3),1)
      else
        call suba  (coef,jcoef,wfac,jwfac,n,wk(ippaaq),wk(iv3))
        call subql (coef,jcoef,wfac,jwfac,n,wk(iv3),wk(iv2))
        call vtriad (n,wk(iz),wk(iz),-alpha,wk(iv2),1)
      end if
c
c proceed to next iteration
c
      in = in + 1
      is = is + 1
      go to 10
c
c----------------------------------finish up----------------------------------
c
 900  if (halt) go to 715
      ier = 1
      call ershow (ier,'bcgsw')
      zeta = stptst
      go to 725
 715  continue
      if (level .ge. 1) write (nout,720) in
 720  format (/' bcgs converged in ',i5,' iterations.')
c
 725  continue
      if (idgts .lt. 0) go to 730
      call perror1 (suba,coef,jcoef,wfac,jwfac,n,u,rhs,wk,digit1,
     a             digit2,idgts)
 730  t2 = timer (dummy)
      timit = t2 - t1
      iparm(2) = in
      rparm(1) = zeta
      rparm(2) = emax
      rparm(3) = emin
      rparm(6) = timit
      rparm(7) = digit1
      rparm(8) = digit2
 735  continue
      if (level .ge. 3) call echall (n,iparm,rparm,2,2,ier)
      nw = nwusd
      return
c
c error returns
c
 995  ier = -16
      call ershow (ier,'bcgsw')
      go to 725
c
 996  ier = -13
      call ershow (ier,'bcgsw')
      go to 725
c
 997  call ershow (ier,'bcgsw')
      go to 735
c
 998  ier = -15
      call ershow (ier,'bcgsw')
      go to 725
c
 999  ier = -2
      call ershow (ier,'bcgsw')
      go to 735
c
      end
      subroutine nullpl (coef,jcoef,wk,iwk,n,subql,suba,subqr,u,v)
      implicit double precision (a-h, o-z)
c
c routine to just apply the left preconditioner ...
c
      dimension u(1), v(1), coef(1), jcoef(2), wk(1), iwk(1)
      external subql, suba, subqr
c
      call subql (coef,jcoef,wk,iwk,n,u,v)
      return
      end
      subroutine nullpr (coef,jcoef,wk,iwk,n,subql,suba,subqr,u,v)
      implicit double precision (a-h, o-z)
c
c routine to just apply the right preconditioner ...
c
      dimension u(1), v(1), coef(1), jcoef(2), wk(1), iwk(1)
      external subql, suba, subqr
c
      call subqr (coef,jcoef,wk,iwk,n,u,v)
      return
      end
      subroutine cgcrpr (coef,jcoef,wk,iwk,n,subql,suba,subqr,u,v)
      implicit double precision (a-h, o-z)
c
c right preconditioner routine to use with cgcr method.
c
      dimension u(1), v(1), coef(1), jcoef(2), wk(1), iwk(1)
c
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
c
      common / cwkcon / lenr, irpnt, irmax, leni, iipnt, iimax
      common / ccgcr  / nblk, nband, ictac, ieta, ivcgcr
c
      external subql, suba, subqr
c
c could bypass next line if subqr is just a copy ...
      call subqr (coef,jcoef,wk,iwk,n,u,v)
      call suba (coef,jcoef,wk,iwk,n,v,wk(ivcgcr))
      call tmult (n,nblk,nband,wk(ictac),wk(ieta),wk(ivcgcr),
     a            wk(ivcgcr))
      call vexopy (n,v,v,wk(ivcgcr),2)
c
      return
      end
      subroutine getblk (coef,jcoef,n,nblk,nband,ctac,nw,ier)
      implicit double precision (a-h, o-z)
c
c this utility routine for the cgcr algorithm computes the matrix
c (c**t)*a*c and factors it.  here, each column of c is zero
c everywhere except it is all 1's on one of its blocks.
c
      dimension ctac(nblk,1), coef(1), jcoef(2)
      logical symm
c
      common / itcom4 / srelpr, keyzer, keygs
      common / itcom6 / method, iscale, iperm, nstore,
     a                  ifact, kblsz ,lvfill, ltrunc, ndeg,
     a                  ipropa, isymm, ifctv
c
c ... data common blocks
c
      common / dscons / ndim, mdim, maxnz
c
c
      nband = 0
c
c*************************** find the bandwidth **********************
c
c
      idmin = 0
      idmax = 0
      do 10 i=1,maxnz
      idiag = jcoef(i)
      idmin = min (idmin,idiag)
 10   idmax = max (idmax,idiag)
      if (nstore .eq. 2) idmin = - idmax
      ihalf = max (-idmin,idmax)
      nbsiz = n / nblk
      nhband = (ihalf+nbsiz-1)/nbsiz
      nband = 1 + 2*nhband
c
c*******************************************************************
c
c now form the matrix.  basically what we need to do here is to
c add up all the elements in each block of the a-matrix.
c
      if (nblk*nband .gt. nw) go to 999
      nw = nblk*nband
c
      call vfill (nblk*nband,ctac,0.0d0)
c
c loop over the diagonals ...
      do 1 i=1,maxnz
      idiag = jcoef(i)
      ibeg = max (1,1-idiag)
      iend = min (n,n-idiag)
      ibbeg = 1 + (ibeg-1)/nbsiz
      ibend = 1 + (iend-1)/nbsiz
      ibase = (i-1)*ndim
c
      symm = nstore .eq. 2 .and. idiag .ne. 0
      idm1 = idiag - 1
      iomid = -idm1
      nmid = n - idiag
      nhbp1 = nhband + 1
c loop over the rows of ctac ...
      do 2 j=ibbeg,ibend
      ibeg = max (1+(j-1)*nbsiz,iomid)
      iend = min(j*nbsiz,nmid)
c     ic1 = (ibeg+idiag-1)/nbsiz + 1
c     ic2 = (iend+idiag-1)/nbsiz + 1
c     id1 = ic1 - j + nhband + 1
c     id2 = ic2 - j + nhband + 1
      itemp1 = (ibeg+idm1)/nbsiz
      itemp2 = (iend+idm1)/nbsiz
      id1 = itemp1 + 2 - j + nhband
      id2 = itemp2 + 2 - j + nhband
      j1s = j + id1 - nhbp1
      j2s = j + id2 - nhbp1
      id1s = nband - id1 + 1
      id2s = nband - id2 + 1
      if (id1 .ne. id2) go to 3
c     ctac(j,id1) = ctac(j,id1)
c    a              + vadd(iend-ibeg+1,coef(ibase+ibeg))
      do 41 ii=ibeg,iend
      if (symm) ctac(j1s,id1s) = ctac(j1s,id1s) + coef(ibase+ii)
 41   ctac(j,id1) = ctac(j,id1) + coef(ibase+ii)
      go to 2
c3    imid = 1 + (ic2-1)*nbsiz - idiag
 3    imid = iomid + itemp2*nbsiz
c     ctac(j,id1) = ctac(j,id1)
c    a              + vadd(imid-ibeg  ,coef(ibase+ibeg))
      do 42 ii=ibeg,imid-1
      if (symm) ctac(j1s,id1s) = ctac(j1s,id1s) + coef(ibase+ii)
 42   ctac(j,id1) = ctac(j,id1) + coef(ibase+ii)
c     ctac(j,id2) = ctac(j,id2)
c    a              + vadd(iend-imid+1,coef(ibase+imid))
      do 43 ii=imid,iend
      if (symm) ctac(j2s,id2s) = ctac(j2s,id2s) + coef(ibase+ii)
 43   ctac(j,id2) = ctac(j,id2) + coef(ibase+ii)
c
 2    continue
c
 1    continue
c
c**************************** do lu factorization ********************
c
      do 31 i=1,nblk-1
      denom = ctac(i,nhbp1)
      if (abs(denom) .lt. srelpr) go to 998
      xpivot = 1.0d0 / denom
      nsubmt = min(nhband,nblk-i)
      do 30 j=1,nsubmt
      ipj = i + j
      ind2 = nhbp1 - j
      do 30 k=1,nsubmt
c30   ctac(i+j,nhband-j+1+k) = ctac(i+j,nhband-j+1+k)
c    a  - xpivot*ctac(i+j,nhband-j+1)*ctac(i,nhband+1+k)
      ind = nhbp1 - j + k
 30   ctac(ipj,ind) = ctac(ipj,ind)
     a  - xpivot*ctac(ipj,ind2)*ctac(i,nhbp1+k)
      do 32 j=1,nsubmt
      ipj = i + j
      ind1 = nhbp1 - j
      ind2 = nhbp1 + j
c     ctac(i+j,nhband+1-j) = ctac(i+j,nhband+1-j)*xpivot
c32   ctac(i  ,nhband+1+j) = ctac(i  ,nhband+1+j)*xpivot
      ctac(ipj,ind1) = ctac(ipj,ind1)*xpivot
 32   ctac(i  ,ind2) = ctac(i  ,ind2)*xpivot
 31   continue
      return
c
c
c error returns
c
c breakdown ...
 998  ier = -6
      call ershow (ier,'getblk')
      return
c
c insuff. memory ...
 999  ier = -2
      call ershow (ier,'getblk')
      nw = nblk*nband
      return
      end
      subroutine tmult (n,nblk,nband,ctac,eta,u,v)
      implicit double precision (a-h, o-z)
c
c this utility routine for the cgcr algorithm computes the product of
c the t-matrix with a vector.  here, t = c*((c**t)*a*c)**(-1) * c**t,
c a projection.
c
      dimension ctac(nblk,nband), eta(1), u(1), v(1)
c
      nbsiz = n / nblk
      nhband = (nband-1)/2
      nhbp1 = nhband + 1
c
c form the eta vector - aggregation step.
c
      do 1 i=0,nblk-1
c1    eta(i) = vadd (nbsiz,u(1+i*nbsiz))
      ip1 = i + 1
      eta(ip1) = 0.0d0
      do 1 j=1,nbsiz
 1    eta(ip1) = eta(ip1) + u(i*nbsiz+j)
c
c perform the forward solve.
c
      if (nhband .eq. 0) go to 40
      do 2 irow=2,nblk
      ibeg = max (1,irow-nhband)
      iend = irow - 1
      ind = nhbp1 - irow
      do 3 icol = ibeg,iend
 3    eta(irow) = eta(irow) - eta(icol)*ctac(irow,ind+icol)
 2    continue
c
c perform the diagonal solve.
c
 40   do 4 i=1,nblk
 4    eta(i) = eta(i) / ctac(i,nhbp1)
c
c perform the back solve.
c
      if (nhband .eq. 0) go to 41
      do 5 i=1,nblk-1
      irow = nblk - i
      ibeg = irow + 1
      iend = min (irow+nhband,nblk)
      ind = nhbp1 - irow
      do 6 icol = ibeg,iend
 6    eta(irow) = eta(irow) - eta(icol)*ctac(irow,ind+icol)
 5    continue
c
c form the vector t*u - disaggregation step.
c
 41   do 7 i=0,nblk-1
      val = eta(i+1)
c7    call vfill (nbsiz,v(1+i*nbsiz),eta(i+1))
      do 7 j=1,nbsiz
 7    v(i*nbsiz+j) = val
c
      return
      end
      double precision function vadd (n,v)
      implicit double precision (a-h, o-z)
c
c adds up all elements of a vector
c
      dimension v(1)
c
      sum = 0.0d0
      do i=1,n
         sum = sum + v(i)
      enddo
      vadd = sum
      return
      end
      subroutine hesest (hess,nhess,nd,esize,imode,havest,
     a                   emax,emin,wk,nw,ier)
      implicit double precision (a-h, o-z)
c
c routine to calculate the moduli of the extremal eigenvalues of a
c banded hessenberg matrix.
c
c hess      - the hessenberg matrix, stored by diagonals
c nhess, nd - dimensions of array hess
c esize     - indicator of how many rows/cols of hess have been
c             filled out so far
c imode     - style of eigenvalue estimation:
c   abs(imode)  - use this size of principal submatrix to do estimate
c   sign(imode) - use either leading or trailing principal submatrix
c
c
      dimension hess(nhess,nd), wk(1)
      logical havest
      integer esize
c
      havest = .false.
      if (imode .gt. 0 .and. esize .gt. imode) return
c
c memory allocation
c
      ndim = min(esize,iabs(imode))
      if (ndim .le. 0) return
      imat = 1
      ireal = imat + ndim*ndim
      iimag = ireal + ndim
c
      nwusd = iimag - 1 + ndim
      if (nwusd .gt. nw) go to 999
      nw = nwusd
c
c make the hess matrix into a full matrix
c
      if (imode .lt. 0) go to 1
      ibeg = 1
      iend = esize
      go to 2
 1    ibeg = max (1,esize-iabs(imode)+1)
      iend = esize
 2    call vfill (ndim*ndim,wk(imat),0.0d0)
      do 3 i=ibeg,iend
      jbeg = max (ibeg,i-1)
      jend = min (ibeg-1+ndim,i+nd-2)
      do 3 j=jbeg,jend
 3    wk(imat+(i-ibeg)+(j-ibeg)*ndim) = hess(i,j-i+2)
c
c call to eispack to calculate eigenvalues
c
      ierr = 0
      call hqr (ndim,ndim,1,ndim,wk(imat),wk(ireal),wk(iimag),ierr)
      if (ierr .ne. 0) go to 998
c
c find eigenvalues with largest and smallest modulus
c
      emax = wk(ireal)**2 + wk(iimag)**2
      emin = emax
      if (ndim .eq. 1) go to 5
      do 6 i=2,ndim
      vmod = wk(ireal-1+i)**2 + wk(iimag-1+i)**2
      emax = max (emax,vmod)
 6    emin = min (emin,vmod)
c
 5    emax = sqrt (emax)
      emin = sqrt (emin)
      havest = .true.
      return
c
c
c error returns ...
c
c error in call to eispack
 998  ier = -18
      call ershow (ier,'hesest')
      return
c
c insuff. floating point workspace
 999  ier = -2
      nw = nwusd
      call ershow (ier,'hesest')
      return
      end
      subroutine pvec (n,nv,iv,s,s1,idotw,it,il,ir,vect,
     a                 dots,ndc,betas,gamma,gamize,svec,wk,ier)
      implicit double precision (a-h, o-z)
c
c this routine performs generalized gram-schmidt on a collection
c of vectors.
c it is used to update the table of direction vectors for
c generalized conjugate gradient methods per-iteration.
c note that this routine was intended to be rather general,
c including block conjugate gradient methods.
c
c params --
c n      - size of the vectors
c nv     - the size of the p-vector table.
c          ie., the table contains p(it-1), p(it-2),...,p(it-nv).
c iv     - number of p-vector-like objects we are dragging along.
c          eg., if iv=3, then we may be computing p, ap and q(inv)ap.
c s      - the block size for block conjugate gradient methods.
c s1     - indicates how many of the old p-vectors are to be used to
c          orthogonalize the new p-vector.
c idotw  - indicates the bandwidth of the matrix used to calculate
c          the betas.
c          generally equals s1, but if the h-matrix is symmetric
c          then = 1.
c it     - iteration number.  this routine calculates p(it).
c il,ir  - integers between 1 and iv.  indicate whether p, ap or
c          q(inv)ap
c          is to be used to calculate the inner product for
c          orthogonality.
c vect   - the p-vector table.
c dots   - workspace for the dot products.
c ndc    - the number of dot products that have already been
c          computed by formit.
c betas  - workspace for the betas.
c gamma  - an s by s matrix containing the coefficients from applying
c          gram schmidt to p(it).
c gamize - flag to indicate whether gram schmidt is to be applied
c          after p(it) is calculated.
c svec   - input packet of vectors to the p-vector calculation
c          process.
c wk     - workspace. must be of size s.
c ier    - error code
c
c array structure and indexing functions --
c
c vect(n,s,nv,iv)    jv
c svec(n,s,iv)       isv
c dots(s,s,idotw,s1) id
c betas(s,s,s1)      ib
c gamma(s,s)         -
c
      integer   s1, s, idotw
      dimension vect(1), svec(1)
      dimension dots(1), betas(1)
      dimension gamma(s,s)
      dimension wk(1)
      logical   gamize
      common / itcom4 / srelpr, keyzer, keygs
c
c define the necessary indexing functions.
c
      jv(i,j,k,l) = 1 + (i-1) + n*((j-1) + s*(mod(k,nv) + nv*(l-1)))
      isv(i,j,k) = 1 + (i-1) + n*((j-1) + s*(k-1))
      id(i,j,k,l) = 1 + (i-1) + s*((j-1) + s*((k-l) + idotw*mod(k,s1)))
      ib(i,j,k) = 1 + (i-1) + s*((j-1) + s*mod(k,s1))
c
      ier = 0
c
c ... handle first iteration.
c
      if (it .eq. 0  .or.  s1 .le. 0) go to 1000
c
c ... now handle general iteration.
c
c ... first, calculate dot products (p(it-1),p(i)).
c
      ibgn = max (it-idotw,0)
      iend = it - 1 - ndc
      if (ibgn .gt. iend) go to 10
      do 2 i = ibgn,iend
      do 2 j = 1,s
      do 2 k = 1,s
 2    dots(id(j,k,it-1,i)) = vdot (n,vect(jv(1,j,it-1,il)),
     a                               vect(jv(1,k,i,ir)))
c
c ... next, form all the new betas.
c
 10   ibgn = max (it-s1,0)
      iend = it - 1
      do 3 i = ibgn,iend
      do 34 l = 1,s
      do 35 k = 1,s
      wk(k) = -vdot (n,vect(jv(1,k,i,il)),svec(isv(1,l,ir)))
      jbgn = max (i-idotw+1,it-s1,0)
      jend = i - 1
      if (jend .lt. jbgn) go to 35
      do 4 j = jbgn,jend
      do 4 m = 1,s
 4    wk(k) = wk(k) - dots(id(k,m,i,j))*betas(ib(m,l,j))
 35   continue
      call vcopy (s*s,dots(id(1,1,i,i)),gamma)
      call gauss (s,s,gamma,wk(1),betas(ib(1,l,i)),ier)
      if (ier .ne. 0) go to 999
 34   continue
 3    continue
c
c ... now, get new p vectors.
c
      do 37 m = 1,iv
      do 37 i = ibgn,iend
      do 37 l = 1,s
      do 6 k = 1,s
 6    call vtriad (n,svec(isv(1,l,m)),svec(isv(1,l,m)),
     a             betas(ib(k,l,i)),vect(jv(1,k,i,m)),1)
 37   continue
c
c ... copy new vectors into the table.
c
 1000 do 168 m = 1,iv
 168  call vcopy (n*s,svec(isv(1,1,m)),vect(jv(1,1,it,m)))
c
c ... now calculate gamma and orthogonalize the new block of p-vectors
c
      call vfill (s*s,gamma,0.0d0)
      do 881 i = 1,s
 881  gamma(i,i) = 1.0d0
      if (.not. gamize) return
      do 879 i = 1,s
      if (i .eq. 1) go to 882
      do 883 j = 1,i-1
 883  wk(j) = vdot (n,vect(jv(1,1,j,it)),vect(jv(1,1,i,it)))
      do 884 j = 1,i-1
      do 885 m = 1,iv
 885  call vtriad (n,vect(jv(1,i,it,m)),vect(jv(1,i,it,m)),
     a        -wk(j),vect(jv(1,j,it,m)),1)
      do 886 k = j,i-1
 886  gamma(j,i) = gamma(j,i) - gamma(j,k)*wk(k)
 884  continue
 882  vnorm = sqrt(vdot(n,vect(jv(1,i,it,1)),vect(jv(1,i,it,1))))
      if (abs(vnorm) .lt. srelpr**2) go to 999
      do 888 m = 1,iv
 888  call vtriad (n,vect(jv(1,i,it,m)),xxx,1.0d0/vnorm,
     a             vect(jv(1,i,it,m)),2)
      do 887 j = 1,i
 887  gamma(j,i) = gamma(j,i)/vnorm
 879  continue
      return
c
c ... error return.
c
 999  ier = -100
      return
      end
      subroutine gauss (ndim,n,a,rhs,u,ier)
      implicit double precision (a-h, o-z)
c
c gaussian elimination routine.
c
      dimension a(ndim,ndim), rhs(ndim), u(ndim)
      common / itcom4 / srelpr, keyzer, keygs
      ier = 0
      if (n .eq. 1) go to 190
      do 1 i = 1,n-1
      if (abs(a(i,i)) .lt. srelpr**2) go to 999
      do 10 j = i+1,n
      fact = a(j,i)/a(i,i)
      a(j,i) = 0.0d0
      do 2 k = i+1,n
 2    a(j,k) = a(j,k) - fact*a(i,k)
      rhs(j) = rhs(j) - fact*rhs(i)
 10   continue
 1    continue
c
 190  do 3 i = 1,n
      k = n - i + 1
      if (abs(a(k,k)) .lt. srelpr**2) go to 999
      u(k) = rhs(k)
      if (i .eq. 1) go to 44
      do 4 j = k+1,n
      u(k) = u(k) - u(j)*a(k,j)
 4    continue
 44   u(k) = u(k)/a(k,k)
 3    continue
      return
 999  ier = -100
      return
      end
      subroutine qrupd (ndim,nnz,nind,c,s,ucnbar,ucn,u,b,ier)
      implicit double precision (a-h, o-z)
c
c this routine updates the qr factorization of the banded upper
c hessenberg matrix used by various conjugate gradient variants.
c
c parameters --
c ndim   - the current size of the hessenberg matrix
c nnz    - the actual number of nonzeros in the band of the
c          hessenberg matrix.  obviously, must be .le. than nind.
c nind   - the bandwidth of the hessenberg matrix, as stored
c c,s    - arrays which hold the cosines and sines of all the
c          rotations that have been performed so far
c u      - the new rightmost column of the hessenberg matrix,
c          which is to be rotated
c b      - the element of the hessenberg matrix to be zapped
c          by the new rotation
c ucnbar - the element of the hessenberg matrix that b is to be
c ucn    - rotated into the new value of ucnbar, after the rotation
c
      dimension c(1), s(1), u(1)
c
c note -- due to the fortran implementation on the cyber 205, it is
c necessary to make ucnbar an array rather than a scalar.
c
      dimension ucnbar(1)
c
c ... define the usual indexing functions.
c
      indv(i) = 1 + mod(i,nind)
      indu(i) = 1 + mod(i,nind+1)
c
c ... indu is used to index u.
c
      if (ndim .le. 1) return
c
c ... apply all the old rotations to the column.
c
      jbgn = max(1,ndim-nnz+1)
      jend = ndim - 2
      if (jend .lt. jbgn) go to 3
      do 2 j = jbgn,jend
      u1 = c(indv(j))*u(indu(j)) + s(indv(j))*u(indu(j+1))
      u2 =-s(indv(j))*u(indu(j)) + c(indv(j))*u(indu(j+1))
      u(indu(j)) = u1
      u(indu(j+1)) = u2
 2    continue
 3    continue
c
c ... now proceed to form the new  2-by-2 rotation matrix.
c
      ucnb = ucnbar(1)
      denom = sqrt(ucnb*ucnb+b*b)
      if (abs(ucnb) .ge. 1.0d-40) denom = sign(denom,ucnb)
      if (abs(denom) .lt. 1.0d-40) go to 999
      c(indv(ndim-1)) = ucnb/denom
      s(indv(ndim-1)) = b/denom
c
c ... now apply the new rotation.
c
      u1 = c(indv(ndim-1))*u(indu(ndim-1))+s(indv(ndim-1))*u(indu(ndim))
      u2 =-s(indv(ndim-1))*u(indu(ndim-1))+c(indv(ndim-1))*u(indu(ndim))
      u(indu(ndim-1)) = u1
      u(indu(ndim)) = u2
      ucn = c(indv(ndim-1))*ucnb + s(indv(ndim-1))*b
      return
 999  ier = -14
      return
      end
      subroutine pstop (ncall,suba,subql,subqr,
     a                  coef,jcoef,wfac,jwfac,n,u,ubar,rhs,r,
     a                  z,zt,wk,nw,ier)
      implicit double precision (a-h, o-z)
c
      dimension zt(1), z(1), r(1), u(1), ubar(1), rhs(1), wk(1)
      dimension coef(1), jcoef(2), wfac(1), jwfac(1)
      external suba, subql, subqr, nullpl, nullpr
c
      call pstopg (ncall,suba,subql,subqr,nullpl,nullpr,
     a     coef,jcoef,wfac,jwfac,n,u,ubar,rhs,r,z,zt,wk,nw,ier)
      return
      end
      subroutine pstopg (ncall,suba,subql,subqr,precl,precr,
     a                   coef,jcoef,wfac,jwfac,n,u,ubar,rhs,r,
     a                   z,zt,wk,nw,ier)
      implicit double precision (a-h, o-z)
c
c ... pstop computes one of stopping tests to determine if the
c     iterative method has converged to a solution within the
c     error tolerance, zeta.  the stopping tests are --
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
c ... here, emax and emin are estimates of the 2-norm of the iteration
c     matrix and its inverse.
c
c key parameters --
c
c ncall: = 0 for first call to pstop by accelerator
c        < 0 for recalc of bnorms, in the case that a new prec has
c            been calc'ed
c        > 0 for a routine call to check the stopping test
c
c iplr : = 0 the left and right preconditioning matrices are the
c            identity
c        = 1 the right prec is the identity
c        = 2 the left prec is the identity
c        = 3 neither the left nor the right prec matrix is the
c            identity
c
c r: the residual of the original system, if rhave = .true.
c z :  ql**(-1) r, if zhave = .true.
c zt : qr**(-1) z, if zthave = .true.
c
c
c this routine is admittedly quite overdesigned.  the idea was to have
c a general routine which would calculate the needed inner products
c with the absolute least amount of work, by determining which inner
c products already exist.
c
      dimension zt(1), z(1), r(1), u(1), ubar(1), rhs(1), wk(1)
      dimension coef(1), jcoef(2), wfac(1), jwfac(1)
      external suba, subql, subqr, precl, precr
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
      logical init, nufact
      logical ipl, ipr
      logical risz, riszt, ziszt
      logical rhav, zhav, zthav, rcalc, zcalc, ztcalc
      logical udhv, rdhv, rzhv, rzthv, zdhv, zzthv, ztdhv
      logical udcal, rdcal, rzcal, rztcal, zdcal, zztcal, ztdcal
c
      dimension idlarr(10), idrarr(10), needbn(10)
      data idlarr /1,3,3,3,1,0,1,2,2,2/
      data idrarr /3,3,3,3,1,0,2,2,2,2/
      data needbn /1,0,1,1,1,0,1,0,1,1/
c
c
      nwusd = 0
      halt = .false.
      tiny = 500.0d0*srelpr
c
c get flags to tell us if there is any prec on the left or right ...
      ipl = iplr .eq. 1 .or. iplr .eq. 3
      ipr = iplr .eq. 2 .or. iplr .eq. 3
c find equivalences between r, z, zt ...
      risz = .not. ipl
      ziszt = .not. ipr
      riszt = risz .and. ziszt
c decode ntest ...
      idl = idlarr(ntest)
      idr = idrarr(ntest)
      idot = 1 + (idl-1) + (idr-1)*3
c
      init = ncall .eq. 0
      nufact = ncall .lt. 0
      if (.not. (init .or. nufact)) go to 900
c
c======================================================================
c========================== initialization section ====================
      iv1 = 1
      iv2 = iv1 + n
c
c ... compute bnorms, as necessary.
c
      if (needbn(ntest) .eq. 0) go to 750
      idle = idl
      if (idle .eq. 3 .and. ziszt) idle = 2
      if (idle .eq. 2 .and. risz)  idle = 1
      idre = idr
      if (idre .eq. 3 .and. ziszt) idre = 2
      if (idre .eq. 2 .and. risz)  idre = 1
      idp  = 0
      idlp = 0
      idrp = 0
      nwusd = 0
      if (nwusd .gt. nw) go to 999
c calc ql(inv)*rhs, if necess ...
      if (max(idle,idre) .gt. 1 .and. ipl) then
        nwusd = nwusd + n
        if (nwusd .gt. nw) go to 999
        idp = idp + 1
        call precl (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,rhs,
     a              wk(1+n*(idp-1)))
        if (idle .gt. 1) idlp = idlp + 1
        if (idre .gt. 1) idrp = idrp + 1
      end if
c calc qr(inv)*ql(inv)*rhs, if necess ...
      if (max(idle,idre) .gt. 2) then
        nwusd = nwusd + n
        if (nwusd .gt. nw) go to 999
        idp = idp + 1
        if (idp .eq. 1)
     a  call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a              rhs,wk(1+n*(idp-1)))
        if (idp .eq. 2)
     a  call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                          wk(1+n*(idp-2)),wk(1+n*(idp-1)))
        if (idle .gt. 2) idlp = idlp + 1
        if (idre .gt. 2) idrp = idrp + 1
      end if
c get needed dot ...
      if (init .or. (idlp .ne. 0 .or. idrp .ne. 0)) then
        bnorm1 = seldot(n,1+idlp,rhs,wk(1),wk(1+n),wk(1+2*n),
     a                    1+idrp,rhs,wk(1),wk(1+n),wk(1+2*n))
        if (bnorm1 .lt. 0.0d0) go to 998
        bnorm1 = max (srelpr,sqrt(bnorm1))
      end if
      if (idlp .eq. 0 .or. idrp .eq. 0) bnorm = bnorm1
c
c ... get ubar norm, as necessary ...
c
 750  if (nufact) go to 900
      ubarnm = srelpr
      if (ntest .eq. 6) ubarnm = sqrt(vdot (n,ubar,ubar))
c
c ... end of initialization phase ...
c===============================================================================
c===============================================================================
c
c ... now begin the actual stopping test section ...
c
c notes on the strategy of this routine:
c     basically, what we're after in order to perform the stopping
c tests is certain dot products.  the needed dot products may already
c be available from the accelerator (in variables rrot, etc., as
c indicated in flags rdhav, etc.) otherwise, it will be necessary to
c compute these from the appropriate vectors.  these vectors in turn
c may already exist (in variables r, z, zt, as indicated b
c rhave, zhave, zthave), or it may be necessary to compute them.
c if they are computed by pstop, then the workspace is used to store
c them. furthermore, there are dependencies between the vectors: zt
c requires z, z requires r.  add to this the further complication that
c it may be possible to c optimize: if there is no left preconditioner,
c then r equals z, and so forth.
c     this routine attempts to get the necessary data to do the
c stopping test in the most optimal way.
c     a few notes on the semantics of variables.  the flag rhave tells
c whether the variable named r actually contains the residual; the flag
c rhav tells whether the residual exists somewhere - whether in r, z,
c zt or workspace.  if c nonzero, the variable ir tells where in the
c workspace the residual is (if it is in the workspace).  now, the
c variable rdhav tells whether rdot actually contains the dot product
c of r with itself.  unlike r and rhave, rdot and rdhav will actually
c be updated by pstop if they are calculated herein, or if rdot can be
c found from some other dot.
c     the variable rcalp indicates whether r was somewhere in workspace
c after pstop did its work.  the accelerator would like to know this,
c since it may want to circumvent letting pstop do a vector calculation
c if it can do it more efficiently.
c     for the initialization call (ncall=0), there is a dry run of
c the stopping test.  that is, the flags rhave, rcalp, rrhave, etc.
c are set to what they would be set in an actual call, but no actual
c vector calculations are done.  this is necessary so that the
c accelerator can plan ahead and take action to circumvent pstop doing
c lengthly calculations - e.g., calculating the residual using an a
c mult when the accelerator could do it simply by doing a saxpy.
c
c
 900  continue
c
c make temporaries for dot haves (modify the actual dot haves only
c if ncall>0)
      udhv = udhav
      rdhv = rdhav
      rzhv = rzhav
      rzthv = rzthav
      zdhv = zdhav
      zzthv = zzthav
      ztdhv = ztdhav
c
c evaluate vector haves ...
      rhav  = rhave  .or. (zhave.and.risz)  .or. (zthave.and.riszt)
      zhav  = zhave  .or. (rhave.and.risz)  .or. (zthave.and.ziszt)
      zthav = zthave .or. (rhave.and.riszt) .or. (zhave .and.ziszt)
c
c take note that there are no vectors in the workspace ...
      ir = 0
      iz = 0
      izt = 0
c
      iwfree = 1
c
c ********** calculate r **********
c
c find dot needs ...
 102  assign 105 to lbldn
      go to 1100
c calculate whatever dots we can ...
 105  assign 110 to lbldc
      go to 1300
c find vector needs ...
 110  assign 115 to lblvn
      go to 1200
c get r ...
 115  if (.not. rcalc) go to 120
      ir = iwfree
      iwfree = iwfree + n
      nwusd = iwfree-1
      if (init .or. nufact) go to 116
      if (nwusd .gt. nw) go to 999
      call suba (coef,jcoef,wfac,jwfac,n,u,wk(ir))
      call vexopy (n,wk(ir),rhs,wk(ir),2)
 116  rhav = .true.
c revise vector haves ...
      if (.not. risz) go to 111
      iz = ir
      zhav = .true.
 111  if (.not. riszt) go to 120
      izt = ir
      zthav = .true.
c
c ********** calculate z **********
c
c calculate dots ...
 120  assign 125 to lbldc
      go to 1300
c revise vector needs ...
 125  assign 126 to lblvn
      go to 1200
c get z ...
 126  if (.not. zcalc) go to 130
      iz = iwfree
      iwfree = iwfree + n
      nwusd = iwfree-1
      if (init .or. nufact) go to 127
      if (nwusd .gt. nw) go to 999
      if (rhave) call precl (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                       r,wk(iz))
      if (ir .ne. 0) call precl (coef,jcoef,wfac,jwfac,n,subql,suba,
     a                           subqr,wk(ir),wk(iz))
 127  zhav = .true.
c revise vector haves ...
      if (.not. risz) go to 121
      ir = iz
      rhav = .true.
 121  if (.not. ziszt) go to 130
      izt = iz
      zthav = .true.
c
c ********** calculate zt **********
c
c calculate dots ...
 130  assign 135 to lbldc
      go to 1300
c  revise vector needs ..
 135  assign 136 to lblvn
      go to 1200
c get zt ...
 136  if (.not. ztcalc) go to 150
      izt = iwfree
      iwfree = iwfree + n
      nwusd = iwfree-1
      if (init .or. nufact) go to 137
      if (nwusd .gt. nw) go to 999
      if (zhave)  call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                        z,wk(izt))
      if ((.not. zhave) .and. (rhave .and. risz))
     a    call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                r,wk(izt))
      if (iz .ne. 0)
     a    call precr (coef,jcoef,wfac,jwfac,n,subql,suba,subqr,
     a                wk(iz),wk(izt))
 137  zthav = .true.
c revise vector haves ...
      if (.not. riszt) go to 131
      ir = izt
      rhav = .true.
 131  if (.not. ziszt) go to 150
      iz = izt
      zhav = .true.
c
c***** take care of details before going on to perform the stopping
c      test
c
c calculate whatever dots we can ...
 150  assign 151 to lbldc
      go to 1300
c save vector calculation needs ...
 151  rcalp  = ir  .ne. 0
      zcalp  = iz  .ne. 0
      ztcalp = izt .ne. 0
c head home, if ncall .le. 0 ...
      if (init .or. nufact) go to 950
c
c save dot have temporaries, if ncall>0 ...
      udhav  = udhv
      rdhav  = rdhv
      rzhav  = rzhv
      rzthav = rzthv
      zdhav  = zdhv
      zzthav = zzthv
      ztdhav = ztdhv
c
c get (u-ubar,u-ubar)
      if (ntest .ne. 6) go to 45
      uedot= 0.0d0
      do 40 i = 1,n
 40   uedot = uedot + (u(i) - ubar(i))**2
c
c===============================================================================
c====================== stopping test computation section ======================
c
c at this point, all the needed dot products have been computed, and
c we are to actually perform the stopping test.
c
 45   go to (51,52,53,54,55,56,57,58,59,60), ntest
c
c ... test 1
c
 51   if (rztdot .lt. -srelpr) go to 998
      top = emax * sqrt (abs(rztdot))
      bottom = emin * bnorm1
      go to 80
c
c ... test 2
c
 52   top = sqrt (abs(ztdot))
      bottom = emin * udnm
      go to 80
c
c ... test 3
c
 53   top = emax * sqrt (abs(ztdot))
      bottom = emin * bnorm1
      go to 80
c
c ... test 4
c
 54   top = sqrt (abs(ztdot))
      bottom = bnorm1
      go to 80
c
c ... test 5
c
 55   top = sqrt (abs(rdot))
      bottom = bnorm1
      go to 80
c
c ... test 6
c
 56   top = sqrt (abs(uedot))
      bottom = ubarnm
      go to 80
c
c ... test 7
c
 57   if (rzdot .lt. -srelpr) go to 998
      top = emax * sqrt (abs(rzdot))
      bottom = emin * bnorm1
      go to 80
c
c ... test 8
c
 58   top = sqrt (abs(zdot))
      bottom = emin * udnm
      go to 80
c
c ... test 9
c
 59   top = emax * sqrt (abs(zdot))
      bottom = emin * bnorm1
      go to 80
c
c ... test 10
c
 60   top = sqrt (abs(zdot))
      bottom = bnorm1
      go to 80
c
 80   if (bottom .lt. tiny) bottom = tiny
      stptst = top/bottom
      call ckconv (ier)
      if (ier .lt. 0) go to 950
      halt = .false.
      if (top .lt. bottom*zeta) halt = .true.
c
c done with the stopping test, head home.
      go to 950
c
c===============================================================================
c*********************** section to calculate dot-needs ************************
c
c here, we consider which dot products the stopping test needs, and
c see whether the needed dot products are currently nonexistent and
c thus must be calculated.
c
 1100 continue
c
c spread any dot information to other dots, as possible ...
      if (risz) then
        if (rdhv) then
          rzdot = rdot
          rzhv = .true.
          zdot = rdot
          zdhv = .true.
        end if
        if (rzhv) then
          rdot = rzdot
          rdhv = .true.
          zdot = rzdot
          zdhv = .true.
        end if
        if (zdhv) then
          rzdot = zdot
          rzhv = .true.
          rdot = zdot
          rdhv = .true.
        end if
      end if
c
      if (ziszt) then
        if (zdhv) then
          zztdot = zdot
          zzthv = .true.
          ztdot = zdot
          ztdhv = .true.
        end if
        if (zzthv) then
          zdot = zztdot
          zdhv = .true.
          ztdot = zztdot
          ztdhv = .true.
        end if
        if (ztdhv) then
          zdot = ztdot
          zdhv = .true.
          zztdot = ztdot
          zzthv = .true.
        end if
      end if
c
      if (riszt) then
        if (rdhv) then
          rztdot = rdot
          rzthv = .true.
          ztdot = rdot
          ztdhv = .true.
        end if
        if (rzthv) then
          rdot = rztdot
          rdhv = .true.
          ztdot = rztdot
          ztdhv = .true.
        end if
        if (ztdhv) then
          rztdot = ztdot
          rzthv = .true.
          rdot = ztdot
          rdhv = .true.
        end if
      end if
c
c figure out which dots actually need to be calculated ...
 1103 udcal = (needbn(ntest) .eq. 0 .and. ntest .ne. 6) .and. .not.udhv
      rdcal  =  idot .eq. 1                   .and. .not.rdhv
      rzcal  = (idot .eq. 2 .or. idot .eq. 4) .and. .not.rzhv
      rztcal = (idot .eq. 3 .or. idot .eq. 7) .and. .not.rzthv
      zdcal  =  idot .eq. 5                   .and. .not.zdhv
      zztcal = (idot .eq. 6 .or. idot .eq. 8) .and. .not.zzthv
      ztdcal =  idot .eq. 9                   .and. .not.ztdhv
      go to lbldn
c
c===============================================================================
c********************* section to calculate vector-needs ***********************
c
c here, we see which vectors have to be calculated in order to
c satisfy the dot calculation needs.
c
 1200 continue
      ztcalc = (rztcal.or.zztcal.or.ztdcal)
     a         .and. .not.zthav
      zcalc  = (rzcal .or.zdcal .or.zztcal .or. ztcalc)
     a         .and. .not.zhav
      rcalc  = (rdcal .or.rzcal .or.rztcal .or. zcalc)
     a         .and. .not.rhav
      go to lblvn
c
c===============================================================================
c********************* dot product calculation section *************************
c
c here, we calculate whatever dot products can be calculated from the
c currently existing vectors.
c
c first locate where the needed vectors are ...
 1300 if (rhave) locr = 1
      if (zhave .and. risz) locr = 2
      if (zthave .and. riszt) locr = 3
      if (ir .ne. 0) locr = 4
c
      if (rhave .and. risz) locz = 1
      if (zhave) locz = 2
      if (zthave .and. ziszt) locz = 3
      if (iz .ne. 0) locz = 4
c
      if (rhave .and. riszt) loczt = 1
      if (zhave .and. ziszt) loczt = 2
      if (zthave) loczt = 3
      if (izt .ne. 0) loczt = 4
c
c now calculate whatever dot products we can ...
c
c** get udnm ...
      if (.not. udcal) go to 1350
      if ((in .gt. 5) .and. (mod(in,5) .ne. 0)) go to 1350
      uold = udnm
      if (init .or. nufact) go to 1349
      udnm = sqrt ( abs ( vdot (n,u,u) ) )
c     if ((in .gt. 5) .and. (abs (udnm-uold) .lt. udnm*zeta))
c    a           is3 = 1
      if (udnm .lt. srelpr) udnm = 1.0d0
 1349 udhv = .true.
      assign 1350 to lbldn
      go to 1100
c
c** get rdot ...
 1350 if (.not. (rdcal .and. rhav)) go to 1360
      if (init .or. nufact) go to 1359
      rdot = seldot (n,locr,r,z,zt,wk(ir),locr,r,z,zt,wk(ir))
 1359 rdhv = .true.
      assign 1360 to lbldn
      go to 1100
c
c** get rzdot ...
 1360 if (.not. (rzcal .and. rhav .and. zhav)) go to 1370
      if (init .or. nufact) go to 1369
      rzdot = seldot (n,locr,r,z,zt,wk(ir),locz,r,z,zt,wk(iz))
 1369 rzhv = .true.
      assign 1370 to lbldn
      go to 1100
c
c** get rztdot ...
 1370 if (.not. (rztcal .and. rhav .and. zthav)) go to 1380
      if (init .or. nufact) go to 1379
      rztdot = seldot (n,locr,r,z,zt,wk(ir),loczt,r,z,zt,wk(izt))
 1379 rzthv = .true.
      assign 1380 to lbldn
      go to 1100
c
c** get zdot ...
 1380 if (.not. (zdcal .and. zhav)) go to 1390
      if (init .or. nufact) go to 1389
      zdot = seldot (n,locz,r,z,zt,wk(iz),locz,r,z,zt,wk(iz))
 1389 zdhv = .true.
      assign 1390 to lbldn
      go to 1100
c
c** get zztdot ...
 1390 if (.not. (zztcal .and. zhav .and. zthav)) go to 1400
      if (init .or. nufact) go to 1399
      zztdot = seldot (n,locz,r,z,zt,wk(iz),loczt,r,z,zt,wk(izt))
 1399 zzthv = .true.
      assign 1400 to lbldn
      go to 1100
c
c** get ztdot ...
 1400 if (.not. (ztdcal .and. zthav)) go to 1410
      if (init .or. nufact) go to 1409
      ztdot = seldot (n,loczt,r,z,zt,wk(izt),loczt,r,z,zt,wk(izt))
 1409 ztdhv = .true.
      assign 1410 to lbldn
      go to 1100
c
 1410 continue
      go to lbldc
c
c===============================================================================
 950  nw = nwusd
      return
c================================= error returns ===============================
c
c splitting matrix is not positive definite
c
 998  ier = -7
      call ershow (ier,'pstop')
      go to 950
c
c insuff. floating point wksp
c
 999  ier = -2
      call ershow (ier,'pstop')
      go to 950
      end
      double precision function seldot (n,iu,u1,u2,u3,u4,iv,v1,v2,v3,v4)
      implicit double precision (a-h, o-z)
c
c this routine computes a dot product from a selected pair of vectors
c
      dimension u1(1), u2(1), u3(1), v1(1), v2(1), v3(1)
      dimension u4(1), v4(1)
c
      ind = 1 + (iv-1) + 4*(iu-1)
      if (ind .eq. 1)  seldot = vdot (n,u1,v1)
      if (ind .eq. 2)  seldot = vdot (n,u1,v2)
      if (ind .eq. 3)  seldot = vdot (n,u1,v3)
      if (ind .eq. 4)  seldot = vdot (n,u1,v4)
      if (ind .eq. 5)  seldot = vdot (n,u2,v1)
      if (ind .eq. 6)  seldot = vdot (n,u2,v2)
      if (ind .eq. 7)  seldot = vdot (n,u2,v3)
      if (ind .eq. 8)  seldot = vdot (n,u2,v4)
      if (ind .eq. 9)  seldot = vdot (n,u3,v1)
      if (ind .eq. 10) seldot = vdot (n,u3,v2)
      if (ind .eq. 11) seldot = vdot (n,u3,v3)
      if (ind .eq. 12) seldot = vdot (n,u3,v4)
      if (ind .eq. 13) seldot = vdot (n,u4,v1)
      if (ind .eq. 14) seldot = vdot (n,u4,v2)
      if (ind .eq. 15) seldot = vdot (n,u4,v3)
      if (ind .eq. 16) seldot = vdot (n,u4,v4)
      return
      end
      subroutine ckconv (ier)
      implicit double precision (a-h, o-z)
c
c routine to determine whether iterative method has stagnated,
c or other unfortunate situation.
c
      parameter (nst=20)
      parameter (eps=1.0d-7)
      common / itcom1 / in, itmax, level, nout, ns1, ns2, ns3,
     a      iplr, iqlr, ntest, is, iacel, idgts, nbl1d, nbl2d
      common / itcom3 / alpha, beta, zeta, emax, emin, pap,
     a                  alphao, gamma, sigma, rr, rho, dkq, dkm1,
     a                  ff, rqmin, rqmax, stptst, udnm, ubarnm,
     a                  bnorm, bnorm1
c
      dimension stold(nst)
      save stold, ist
      ind(i) = 1 + mod(i,nst)
c
      if (in .le. 0) ist = 0
c
      ist = ist + 1
      stold(ind(ist)) = stptst
      if (ist .lt. nst) go to 900
c     do 2 i = 1, nst-1
      do 2 i = nst-1, 1, -1
c     val = abs(stold(ind(ist-i))-stptst)/stptst
      val = abs(stold(ind(ist-i))-stptst)
      if (val .gt. eps*stptst) go to 900
 2    continue
      ier = -19
      call ershow (ier,'ckconv')
      return
c
 900  return
      end
      subroutine inithv (icall)
      implicit double precision (a-h, o-z)
c
c routine to initialize dot and vector haves to false.
c
      common / itcom9 / rdot, rzdot, rztdot, zdot, zztdot, ztdot,
     a           rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a           udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
      logical rhave, zhave, zthave, rcalp, zcalp, ztcalp,
     a        udhav, rdhav, rzhav, rzthav, zdhav, zzthav, ztdhav
c
      udhav  = .false.
      rdhav  = .false.
      rzhav  = .false.
      rzthav = .false.
      zdhav  = .false.
      zzthav = .false.
      ztdhav = .false.
      if (icall .eq. 1) return
      rhave  = .false.
      zhave  = .false.
      zthave = .false.
c
      return
      end
      subroutine hqr(nm,n,low,igh,h,wr,wi,ierr)
      implicit double precision (a-h, o-z)
c
      integer en,enm2
      dimension h(nm,n),wr(n),wi(n)
      double precision norm
      logical notlas
c
c     this routine is a translation of the algol procedure hqr,
c     num. math. 14, 219-231(1970) by martin, peters, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971).
c
c     this routine finds the eigenvalues of a floating point
c     upper hessenberg matrix by the qr method.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        low and igh are integers determined by the balancing
c          routine  balanc.  if  balanc  has not been used,
c          set low=1, igh=n.
c
c        h contains the upper hessenberg matrix.  information about
c          the transformations used in the reduction to hessenberg
c          form by  elmhes  or  orthes, if performed, is stored
c          in the remaining triangle under the hessenberg matrix.
c
c     on output
c
c        h has been destroyed.  therefore, it must be saved
c          before calling  hqr  if subsequent calculation and
c          back transformation of eigenvectors is to be performed.
c
c        wr and wi contain the real and imaginary parts,
c          respectively, of the eigenvalues.  the eigenvalues
c          are unordered except that complex conjugate pairs
c          of values appear consecutively with the eigenvalue
c          having the positive imaginary part first.  if an
c          error exit is made, the eigenvalues should be correct
c          for indices ierr+1,...,n.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the limit of 30*n iterations is exhausted
c                     while the j-th eigenvalue is being sought.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      norm = 0.0d0
      k = 1
c     .......... store roots isolated by balanc
c                and compute matrix norm ..........
      do 50 i = 1, n
c
         do 40 j = k, n
   40    norm = norm + abs(h(i,j))
c
         k = i
         if (i .ge. low .and. i .le. igh) go to 50
         wr(i) = h(i,i)
         wi(i) = 0.0d0
   50 continue
c
      en = igh
      t = 0.0d0
      itn = 30*n
c     .......... search for next eigenvalues ..........
   60 if (en .lt. low) go to 1001
      its = 0
      na = en - 1
      enm2 = na - 1
c     .......... look for single small sub-diagonal element
c                for l=en step -1 until low do -- ..........
   70 do 80 ll = low, en
         l = en + low - ll
         if (l .eq. low) go to 100
         s = abs(h(l-1,l-1)) + abs(h(l,l))
         if (s .eq. 0.0d0) s = norm
         tst1 = s
         tst2 = tst1 + abs(h(l,l-1))
         if (tst2 .eq. tst1) go to 100
   80 continue
c     .......... form shift ..........
  100 x = h(en,en)
      if (l .eq. en) go to 270
      y = h(na,na)
      w = h(en,na) * h(na,en)
      if (l .eq. na) go to 280
      if (itn .eq. 0) go to 1000
      if (its .ne. 10 .and. its .ne. 20) go to 130
c     .......... form exceptional shift ..........
      t = t + x
c
      do 120 i = low, en
  120 h(i,i) = h(i,i) - x
c
      s = abs(h(en,na)) + abs(h(na,enm2))
      x = 0.75d0 * s
      y = x
      w = -0.4375d0 * s * s
  130 its = its + 1
      itn = itn - 1
c     .......... look for two consecutive small
c                sub-diagonal elements.
c                for m=en-2 step -1 until l do -- ..........
      do 140 mm = l, enm2
         m = enm2 + l - mm
         zz = h(m,m)
         r = x - zz
         s = y - zz
         p = (r * s - w) / h(m+1,m) + h(m,m+1)
         q = h(m+1,m+1) - zz - r - s
         r = h(m+2,m+1)
         s = abs(p) + abs(q) + abs(r)
         p = p / s
         q = q / s
         r = r / s
         if (m .eq. l) go to 150
         tst1 = abs(p)*(abs(h(m-1,m-1)) + abs(zz) + abs(h(m+1,m+1)))
         tst2 = tst1 + abs(h(m,m-1))*(abs(q) + abs(r))
         if (tst2 .eq. tst1) go to 150
  140 continue
c
  150 mp2 = m + 2
c
      do 160 i = mp2, en
         h(i,i-2) = 0.0d0
         if (i .eq. mp2) go to 160
         h(i,i-3) = 0.0d0
  160 continue
c     .......... double qr step involving rows l to en and
c                columns m to en ..........
      do 260 k = m, na
         notlas = k .ne. na
         if (k .eq. m) go to 170
         p = h(k,k-1)
         q = h(k+1,k-1)
         r = 0.0d0
         if (notlas) r = h(k+2,k-1)
         x = abs(p) + abs(q) + abs(r)
         if (x .eq. 0.0d0) go to 260
         p = p / x
         q = q / x
         r = r / x
  170    s = sign(sqrt(p*p+q*q+r*r),p)
         if (k .eq. m) go to 180
         h(k,k-1) = -s * x
         go to 190
  180    if (l .ne. m) h(k,k-1) = -h(k,k-1)
  190    p = p + s
         x = p / s
         y = q / s
         zz = r / s
         q = q / p
         r = r / p
         if (notlas) go to 225
c     .......... row modification ..........
         do 200 j = k, n
            p = h(k,j) + q * h(k+1,j)
            h(k,j) = h(k,j) - p * x
            h(k+1,j) = h(k+1,j) - p * y
  200    continue
c
         j = min(en,k+3)
c     .......... column modification ..........
         do 210 i = 1, j
            p = x * h(i,k) + y * h(i,k+1)
            h(i,k) = h(i,k) - p
            h(i,k+1) = h(i,k+1) - p * q
  210    continue
         go to 255
  225    continue
c     .......... row modification ..........
         do 230 j = k, n
            p = h(k,j) + q * h(k+1,j) + r * h(k+2,j)
            h(k,j) = h(k,j) - p * x
            h(k+1,j) = h(k+1,j) - p * y
            h(k+2,j) = h(k+2,j) - p * zz
  230    continue
c
         j = min(en,k+3)
c     .......... column modification ..........
         do 240 i = 1, j
            p = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2)
            h(i,k) = h(i,k) - p
            h(i,k+1) = h(i,k+1) - p * q
            h(i,k+2) = h(i,k+2) - p * r
  240    continue
  255    continue
c
  260 continue
c
      go to 70
c     .......... one root found ..........
  270 wr(en) = x + t
      wi(en) = 0.0d0
      en = na
      go to 60
c     .......... two roots found ..........
  280 p = (y - x) / 2.0d0
      q = p * p + w
      zz = sqrt(abs(q))
      x = x + t
      if (q .lt. 0.0d0) go to 320
c     .......... real pair ..........
      zz = p + sign(zz,p)
      wr(na) = x + zz
      wr(en) = wr(na)
      if (zz .ne. 0.0d0) wr(en) = x - w / zz
      wi(na) = 0.0d0
      wi(en) = 0.0d0
      go to 330
c     .......... complex pair ..........
  320 wr(na) = x + p
      wr(en) = x + p
      wi(na) = zz
      wi(en) = -zz
  330 en = enm2
      go to 60
c     .......... set error -- all eigenvalues have not
c                converged after 30*n iterations ..........
 1000 ierr = en
 1001 return
      end
