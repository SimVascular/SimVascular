c Copyright (c) 2014-2015 The Regents of the University of California.
c All Rights Reserved.
c
c Portions of the code Copyright (c) 2009-2011 Open Source Medical
c Software Corporation, University of California, San Diego.
c
c Portions of the code Copyright (c) 2000-2007, Stanford University, 
c     Rensselaer Polytechnic Institute, Kenneth E. Jansen, 
c     Charles A. Taylor.
c  
c  See SimVascular Acknowledgements file for additional 
c  contributors to the source code.
c
c  Redistribution and use in source and binary forms, with or without 
c  modification, are permitted provided that the following conditions 
c  are met:
c
c  Redistributions of source code must retain the above copyright notice,
c  this list of conditions and the following disclaimer. 
c  Redistributions in binary form must reproduce the above copyright 
c  notice, this list of conditions and the following disclaimer in the 
c  documentation and/or other materials provided with the distribution. 
c  Neither the name of the Stanford University or Rensselaer Polytechnic
c  Institute nor the names of its contributors may be used to endorse or
c  promote products derived from this software without specific prior 
c  written permission.
c
c  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
c  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
c  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
c  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
c  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
c  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
c  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
c  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
c  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
c  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
c  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
c  DAMAGE.
c
c

#include "cvFlowsolverOptions.h"

      subroutine Asadj (row_fill_list,iens,adjcnt)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        
c..........DECLARATION.................................................
        INTEGER row_fill_list(nshg,6*nnz),
     &          ien(npro,nshl),
     &          adjcnt(nshg), ndlist(nshl)

        INTEGER iens(npro,nshl),i,j,ibroke,jlngth,jnd

        INTEGER k,knd,l
c......................................................................
c prefer to show explicit absolute value needed for cubic modes and
c higher rather than inline abs on pointer as in past versions
c iens is the signed ien array ien is unsigned
c
      ien = abs(iens)

        do i=1,npro
           do j=1,nshl
              ndlist(j)=ien(i,j)
           enddo
           do j=1,nshl
              jnd=ndlist(j)
              jlngth=adjcnt(jnd) ! current length of j's list
              do k=1,nshl 
                 knd=ndlist(k)
                 ibroke=zero
                 do l= 1,jlngth
                    if(row_fill_list(jnd,l).eq. knd) then
                       ibroke=1
                       exit
                    endif
                 enddo
                 
c
c  to get here k was not in  j's list so add it
c
                 if(ibroke.eq.0) then
                    jlngth=jlngth+1 ! lenthen list
                    if(jlngth.gt.6*nnz) then
                       write(*,*) 'increase overflow factor in genadj'
                       stop
                    endif
                    row_fill_list(jnd,jlngth)=knd ! add unique entry to list
                 endif
              enddo ! finished checking all the k's for this j
              adjcnt(jnd)=jlngth  ! update the counter
           enddo                  ! done with j's
        enddo                   ! done with elements in this block
c
c
c.... end
c
        return
        end

!> This routine computes and assembles the data corresponding to the
!! boundary elements.

      subroutine AsBFlx (u,           y,           ac,      
     &                   x,           shpb,    
     &                   shglb,       ienb,        iBCB,    
     &                   BCB,         invflx,      flxres,
     &                   flxLHS,      flxnrm,      xKebe )

        use LagrangeMultipliers 

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

c..........................Declaration................................
       INTEGER i,ienb,iBCB,invflx,lnflx,lnode,n
       REAL*8 ac,BCB,flxres,flxLHS,flxnrm,shpb,shglb,u,y,x,xKebe
       REAL*8  yl,xlb,rl,sgn,flhsl,fnrml,ul,acl,dwl
c.....................................................................       
c
        dimension y(nshg,ndofl),           x(numnp,nsd),
     &            ac(nshg,ndofl),          u(nshg,nsd),
     &            shpb(nshl,ngaussb),
     &            shglb(nsd,nshl,ngaussb),         
     &            ienb(npro,nshl),         
     &            iBCB(npro,ndiBCB),       BCB(npro,nshlb,ndBCB),
     &            invflx(nshg),            flxres(nshg,nflow),
     &            flxLHS(nshg,1),          flxnrm(nshg,nsd)
c
        dimension yl(npro,nshl,ndofl),     xlb(npro,nenl,nsd),
     &            rl(npro,nshl,nflow),     sgn(npro,nshl),
     &            flhsl(npro,nshl,1),      fnrml(npro,nshl,nsd),
     &            lnflx(npro),             lnode(27),
     &            ul(npro,nshl,nsd),       acl(npro,nshl,ndofl),
     &			  dwl(npro,nshl)
        
        dimension xKebe(npro,9,nshl,nshl) 

c
c.... compute the nodes which lie on the boundary (hierarchic)
c
        call getbnodes(lnode)
c
c.... get the matrix of mode signs for the hierarchic basis functions
c
        if (ipord .gt. 1) then
           call getsgn(ienb,sgn)
        endif
c     
c.... gather the variables
c
        call localy(y,      yl,     ienb,   ndofl,  'gather  ')
        call localy(ac,     acl,    ienb,   ndofl,  'gather  ')
        call localx(x,      xlb,    ienb,   nsd,    'gather  ')
        call localx(u,      ul,     ienb,   nsd,    'gather  ')

        rl    = zero
        flhsl = zero
        fnrml = zero
c
        ires = 2
c
c..... to calculate inner product for Lagrange Multipliers
c
        if(Lagrange.gt.zero) then
           allocate(loclhsLag(npro,9,nshlb,nshlb,3))
           loclhsLag = zero
        endif           
c        
        call e3b  (ul,      yl,      acl,     iBCB,    BCB,     
     &             shpb,    shglb,
     &             xlb,     rl,      sgn,     dwl,     xKebe)
        ires = 1
c
c.... assemble the residuals
c
        call local (flxres, rl,     ienb,   nflow,  'scatter ')
c
c.... compute the LHS for the flux computation (should only be done
c     once)
c
        call f3lhs (shpb,       shglb,      xlb,
     &              flhsl,      fnrml,      sgn )

c     
c.... reset the non-contributing element values
c
        lnflx = 0
        do n = 1, nshlb
          lnflx = lnflx + min(1, invflx(ienb(:,lnode(n))))
        enddo
c
        do n = 1, nshl
          where (lnflx .ne. nshlb)   flhsl(:,n,1) = zero
          do i = 1, nsd
            where (lnflx .ne. nshlb) fnrml(:,n,i) = zero
          enddo
        enddo
c
c.... assemble the boundary LHS and normal
c
        call local (flxLHS, flhsl,  ienb,   1,      'scatter ')
        call local (flxnrm, fnrml,  ienb,   nsd,    'scatter ')
c
        if(Lagrange.gt.zero) then
           deallocate(loclhsLag)
        endif
c     
c.... end
c
        return
        end

!> This routine computes and assembles the data corresponding to the
!! interior elements for the global reconstruction of the diffusive
!! flux vector.
!! @param[in] y(numnp,ndof) Y variables
!! @param[in] x(numnp,nsd) nodal coordinates
!! @param[in] shp(nen,nintg) element shape-functions
!! @param[in] shgl(nsd,nen,nintg) element local shape-function gradients
!! @param[in] ien(npro) nodal connectivity array
!! @param[out] qres(numnp,nsd,nsd) residual vector for diffusive flux
!! @param[out] rmass(numnp) lumped mass matrix

        subroutine AsIq (y,       x,       shp,
     &                   shgl,    ien,     xmudmi,
     &                   qres,    rmass    )

      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
 
c
C     Argument variables
C
      INTEGER             ien
C
      REAL*8                qres,        rmass,       shgl,        shp
      REAL*8                x,           xmudmi,      y
C
C     Local variables
C
      INTEGER             i
C
      REAL*8                dwl,         ql
      REAL*8                rmassl,      sgn
      REAL*8                xl,          yl

        dimension y(nshg,ndof),               x(numnp,nsd),            
     &            shp(nshl,ngauss),         shgl(nsd,nshl,ngauss),
     &            ien(npro,nshl),      dwl(npro,nenl),
     &            qres(nshg,idflx),    rmass(nshg)
c
        dimension yl(npro,nshl,ndof),          xl(npro,nenl,nsd),         
     &            ql(npro,nshl,idflx),  rmassl(npro,nshl),
     &            xmudmi(npro,ngauss)
c
        dimension sgn(npro,nshl)
c
c.... create the matrix of mode signs for the hierarchic basis 
c     functions. 
c
        do i=1,nshl
           where ( ien(:,i) < 0 )
              sgn(:,i) = -one
           elsewhere
              sgn(:,i) = one
           endwhere
        enddo

c
c.... gather the variables
c

        call localy(y,      yl,     ien,    ndof,   'gather  ')
        call localx (x,      xl,     ien,    nsd,    'gather  ')
c
c.... get the element residuals 
c
        ql     = zero
        rmassl = zero

        call e3q  (yl,         dwl,      shp,      shgl,    
     &             xl,         ql,       rmassl,
     &             xmudmi,     sgn  )

c
c.... assemble the diffusive flux residual 
c
        call local (qres,   ql,  ien,  idflx,  'scatter ')
        call local (rmass,  rmassl, ien,  1,          'scatter ')
c
c.... end
c
        return
        end


!< This routine computes and assembles the data corresponding to the
!! interior elements for the global reconstruction of the diffusive
!! flux vector.

        subroutine AsIqSclr (y,       x,       shp,
     &                       shgl,    ien,     qres,    
     &                       rmass    )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
 
c
C     Argument variables
C
      INTEGER             ien
C
      REAL*8                qres,        rmass,       shgl,        shp
      REAL*8                x,           y
C
C     Local variables
C
      REAL*8                dwl,         ql,          rmassl
      REAL*8                sgn,         xl,          yl


        dimension y(nshg,ndof),             x(numnp,nsd),            
     &            shp(nshl,ngauss),         shgl(nsd,nshl,ngauss),
     &            ien(npro,nshl),      dwl(npro,nenl),
     &            qres(nshg,nsd),           rmass(nshg)
c
        dimension yl(npro,nshl,ndof),       xl(npro,nenl,nsd),         
     &            ql(npro,nshl,nsd),        rmassl(npro,nshl)
c
        dimension sgn(npro,nshl)

        if (ipord .gt. 1) then
           call getsgn(ien,sgn)
        endif
c
c.... gather the variables
c
        call localy(y,      yl,     ien,    ndof,   'gather  ')
        call localx (x,      xl,     ien,    nsd,    'gather  ')
c
c.... get the element residuals 
c
        ql     = zero
        rmassl = zero

        call e3qSclr  (yl,      dwl,    shp,    shgl,    
     &                 xl,      ql,     rmassl, 
     &                 sgn             )

c
c.... assemble the temperature diffusive flux residual 
c
        call local (qres,   ql,  ien,  nsd,  'scatter ')
        call local (rmass,  rmassl, ien,  1, 'scatter ')
c
c.... end
c
        return
        end

!> This routine satisfies the BC of LHS mass matrix for all  
!! elements in this block.
!! @param[in] iBC(nshg) boundary condition code
!! @param[in] BC(nshg,ndofBC) Dirichlet BC constraint parameters
!! @param[in] ien(npro,nshape) ien array for this element
!! @param[in] xKebe(npro,9,nshl,nshl) element consistent mass matrix before BC
!! @param[out] xKebe(npro,9,nshl,nshl) LHS mass matrix after BC is satisfied

      subroutine bc3LHS (iBC,  BC,  iens,  xKebe )

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        
C     Argument variables
C
      INTEGER             ibc
C
      REAL*8                bc,          xkebe
C
C     Local variables
C
      INTEGER             i,           iadj1,       iadj2,       iadj3
      INTEGER             iel,         ien,         in,          inod
      INTEGER             ire21,       ire22,       ire23,       irem1
      INTEGER             irem2,       irem3
C
c
      dimension iBC(nshg),      ien(npro,nshape),
     &          BC(nshg,ndofBC), xKebe(npro,9,nshl,nshl)
      integer iens(npro,nshl)
c
c prefer to show explicit absolute value needed for cubic modes and
c higher rather than inline abs on pointer as in past versions
c iens is the signed ien array ien is unsigned
c
      ien=abs(iens)
c
c.... loop over elements
c
c        return
        do iel = 1, npro
c
c.... loop over number of shape functions for this element
c
           do inod = 1, nshl
c
c.... set up parameters
c
              in  = abs(ien(iel,inod))
              if (ibits(iBC(in),3,3) .eq. 0) goto 5000 ! NO velocity BC's
              if (ibits(iBC(in),3,3) .eq. 7) goto 5000 ! 3 components ok

c.... 1 or 2 component velocities
c
c
c.... x1-velocity
c
              if ( ibits(iBC(in),3,3) .eq. 1) then
c
 ! we want to project out the x1 component of the velocity from the tangent  
 ! matix which is, mathematically, M^e = S^T M^e S. We will do the M^e S
 ! product first. It has the effect of
 ! subtracting the column of the block-9 matrix from each column of the block-9
 ! matrix  that is going to survive (weighted by the coefficient in the
 ! BC array associated with that row) FOR EACH column  of the
 ! nshl by nshl matrix FOR EACH element.  THEN the transpose of the
 ! operation is carried out (replace the word "column" by row
 ! EVERYWHERE). The following code has been set up so that we only have to 
 ! give the starting position in each case since we know the block-9 matrix is
 ! ordered like this   
 !  1 2 3
 !  4 5 6
 !  7 8 9

c
c  adjusting the second column for the eventual removal of the first
c  column of the block-9 submatrix
c
                 irem1=1
                 irem2=irem1+3
                 irem3=irem2+3

                 iadj1=2
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem1,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem2,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem3,i,inod) 

                 enddo
 ! block status ' denotes colunn 1 projected off.
 !  1 2' 3
 !  4 5' 6
 !  7 8' 9
c
c  adjusting the third column for the eventual removal of the first
c  column of the block-9 submatrix
c
                 iadj1=3
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem1,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem2,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem3,i,inod) 
 ! block status
 !  1 2' 3'
 !  4 5' 6'
 !  7 8' 9'
                 enddo
                 do i=1,nshl
c
c done with the first  columns_block-9 for columns AND rows of nshl
c
                    xKebe(iel,irem1,i,inod) = zero 
                    xKebe(iel,irem2,i,inod) = zero 
                    xKebe(iel,irem3,i,inod) = zero 


 ! block status
 !  0 2' 3'
 !  0 5' 6'
 !  0 8' 9'

                 enddo
c
c  now adjust the second row_block-9 for EACH row nshl for EACH element 
c

                 iadj1=4
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 irem1=1
                 irem2=irem1+1
                 irem3=irem2+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem3,inod,i) 

                 enddo
 ! block status
 !  0 2' 3'
 !  0 5'' 6''
 !  0 8' 9'


                 iadj1=7
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem3,inod,i) 

 ! block status
 !  0 2' 3'
 !  0 5'' 6''
 !  0 8'' 9''
                 enddo
                 do i=1,nshl

c
c eliminate the first row of block-9 for all rows
c 
                    xKebe(iel,irem1,inod,i) = zero 
                    xKebe(iel,irem2,inod,i) = zero 
                    xKebe(iel,irem3,inod,i) = zero 

                 enddo

 ! block status
 !  0 0   0
 !  0 5'' 6''
 !  0 8'' 9''

  ! Be aware that this simple status of the block does not reflect that when 
  ! we eliminated columns we did it for columns in nshl as well for the given
  ! inod. Conversely when we eliminated rows in the block we did so for ALL
  !  rows in nshl as can be seen by the transpose of i and inod.

                 xKebe(iel,1,inod,inod)=one
 ! block status
 !  1 0   0
 !  0 5'' 6''
 !  0 8'' 9''
              endif
c
c.... x2-velocity
c
              if ( ibits(iBC(in),3,3) .eq. 2) then
c
! See comment above. Now we are eliminating the 2nd column then row of
 ! the block-9 matrix
 !  1 2 3
 !  4 5 6
 !  7 8 9
c
c  adjusting the first column for the eventual removal of the second
c  column of the block-9 submatrix
c
                 irem1=2
                 irem2=irem1+3
                 irem3=irem2+3

                 iadj1=1
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem1,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem2,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem3,i,inod) 

                 enddo
c
c  adjusting the third column for the eventual removal of the second
c  column of the block-9 submatrix
c
                 iadj1=3
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem1,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem2,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem3,i,inod) 

                 enddo
                 do i=1,nshl
c
c done with the second  columns_block-9 for columns
c

                    xKebe(iel,irem1,i,inod) = zero 
                    xKebe(iel,irem2,i,inod) = zero 
                    xKebe(iel,irem3,i,inod) = zero 

                 enddo
c
c  now adjust the 1st row_block-9 for EACH row nshl for EACH element 
c

                 iadj1=1
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 irem1=4
                 irem2=irem1+1
                 irem3=irem2+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem3,inod,i) 

                 enddo
                 iadj1=7
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem3,inod,i) 
                 enddo
                 do i=1,nshl

c
c eliminate the second row of block-9 for all rows 
c 
                    xKebe(iel,irem1,inod,i) = zero 
                    xKebe(iel,irem2,inod,i) = zero 
                    xKebe(iel,irem3,inod,i) = zero 
                 enddo
                 xKebe(iel,5,inod,inod)=one
              endif
c
c.... x3-velocity
c
              if ( ibits(iBC(in),3,3) .eq. 4) then
c
! See comment above. Now we are eliminating the 3rd column then row of
 ! the block-9 matrix
 !  1 2 3
 !  4 5 6
 !  7 8 9
c
c  adjusting the 1st column for the eventual removal of the 3rd
c  column of the block-9 submatrix
c
                 irem1=3
                 irem2=irem1+3
                 irem3=irem2+3

                 iadj1=1
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem1,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem2,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem3,i,inod) 

                 enddo
c
c  adjusting the second column for the eventual removal of the 3rd
c  column of the block-9 submatrix
c
                 iadj1=2
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem1,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem2,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,5) * xKebe(iel,irem3,i,inod) 
                 enddo
                 do i=1,nshl

c
c done with the 3rd columns_block-9 for columns 
c

                    xKebe(iel,irem1,i,inod) = zero 
                    xKebe(iel,irem2,i,inod) = zero 
                    xKebe(iel,irem3,i,inod) = zero 

                 enddo
c
c  now adjust the 1st row_block-9 for EACH row nshl for EACH element 
c

                 iadj1=1
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 irem1=7
                 irem2=irem1+1
                 irem3=irem2+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem3,inod,i) 

                 enddo
                 iadj1=4
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,5) * xKebe(iel,irem3,inod,i) 

                 enddo
                 do i=1,nshl
                    xKebe(iel,irem1,inod,i) = zero 
                    xKebe(iel,irem2,inod,i) = zero 
                    xKebe(iel,irem3,inod,i) = zero 

                 enddo
                 xKebe(iel,9,inod,inod)=one
              endif
c     
c.... x1-velocity and x2-velocity
c
              if ( ibits(iBC(in),3,3) .eq. 3 ) then
c
! See comment above. Now we are eliminating the 2nd and 1st column then
 ! same rows of
 ! the block-9 matrix
 !  1 2 3
 !  4 5 6
 !  7 8 9
c
c  adjusting the 3rd column for the eventual removal of the first and second
c  column of the block-9 submatrix
c
                 irem1=1
                 irem2=irem1+3
                 irem3=irem2+3

                 ire21=2
                 ire22=ire21+3
                 ire23=ire22+3

                 iadj1=3
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem1,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire21,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem2,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire22,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem3,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire23,i,inod) 

 ! Status of the block-9 matrix
 !  1 2 3'
 !  4 5 6'
 !  7 8 9'
                 enddo
                 do i=1,nshl
c
c done with the first and second columns_block-9 for columns AND rows of nshl
c

                    xKebe(iel,irem1,i,inod) = zero 
                    xKebe(iel,irem2,i,inod) = zero 
                    xKebe(iel,irem3,i,inod) = zero 

                    xKebe(iel,ire21,i,inod) = zero 
                    xKebe(iel,ire22,i,inod) = zero 
                    xKebe(iel,ire23,i,inod) = zero 

 ! Status of the block-9 matrix
 !  0 0 3'
 !  0 0 6'
 !  0 0 9'

                 enddo
c
c  now adjust the 3rd row_block-9 for EACH row nshl for EACH element 
c

                 iadj1=7
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 irem1=1
                 irem2=irem1+1
                 irem3=irem2+1
                 ire21=4
                 ire22=ire21+1
                 ire23=ire22+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem1,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire21,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem2,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire22,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem3,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire23,inod,i) 


 ! Status of the block-9 matrix
 !  0 0 3'
 !  0 0 6'
 !  0 0 9''
                 enddo
                 do i=1,nshl
                    xKebe(iel,irem1,inod,i) = zero 
                    xKebe(iel,irem2,inod,i) = zero 
                    xKebe(iel,irem3,inod,i) = zero 

                    xKebe(iel,ire21,inod,i) = zero 
                    xKebe(iel,ire22,inod,i) = zero 
                    xKebe(iel,ire23,inod,i) = zero 

 ! Status of the block-9 matrix
 !  0 0 0
 !  0 0 0
 !  0 0 9''

                 enddo
                 xKebe(iel,1,inod,inod)=one
                 xKebe(iel,5,inod,inod)=one
              endif
c     
c.... x1-velocity and x3-velocity
c
              if ( ibits(iBC(in),3,3) .eq. 5 ) then
c
! See comment above. Now we are eliminating the 1 and 3 column then
 ! same rows of
 ! the block-9 matrix
 !  1 2 3
 !  4 5 6
 !  7 8 9
c
c  adjusting the 3rd column for the eventual removal of the first and second
c  column of the block-9 submatrix
c
                 irem1=1
                 irem2=irem1+3
                 irem3=irem2+3

                 ire21=3
                 ire22=ire21+3
                 ire23=ire22+3

                 iadj1=2
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem1,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire21,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem2,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire22,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem3,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire23,i,inod) 

                 enddo
                 do i=1,nshl
c
c done with the first and third columns_block-9 for columns AND rows of nshl
c
                    xKebe(iel,irem1,i,inod) = zero 
                    xKebe(iel,irem2,i,inod) = zero 
                    xKebe(iel,irem3,i,inod) = zero 

                    xKebe(iel,ire21,i,inod) = zero 
                    xKebe(iel,ire22,i,inod) = zero 
                    xKebe(iel,ire23,i,inod) = zero 
                 enddo
c
c  now adjust the 2nd row_block-9 for EACH row nshl for EACH element 
c

                 iadj1=4
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 irem1=1
                 irem2=irem1+1
                 irem3=irem2+1
                 ire21=7
                 ire22=ire21+1
                 ire23=ire22+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem1,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire21,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem2,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire22,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem3,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire23,inod,i) 

                 enddo
                 do i=1,nshl
                    xKebe(iel,irem1,inod,i) = zero 
                    xKebe(iel,irem2,inod,i) = zero 
                    xKebe(iel,irem3,inod,i) = zero 

                    xKebe(iel,ire21,inod,i) = zero 
                    xKebe(iel,ire22,inod,i) = zero 
                    xKebe(iel,ire23,inod,i) = zero 

                 enddo
                 xKebe(iel,1,inod,inod)=one
                 xKebe(iel,9,inod,inod)=one
              endif
c     
c.... x2-velocity and x3-velocity
c
              if ( ibits(iBC(in),3,3) .eq. 6 ) then
c
! See comment above. Now we are eliminating the 2nd and 3rd column then
 ! same rows of
 ! the block-9 matrix
 !  1 2 3
 !  4 5 6
 !  7 8 9
c
c  adjusting the 3rd column for the eventual removal of the first and second
c  column of the block-9 submatrix
c
                 irem1=2
                 irem2=irem1+3
                 irem3=irem2+3

                 ire21=3
                 ire22=ire21+3
                 ire23=ire22+3

                 iadj1=1
                 iadj2=iadj1+3
                 iadj3=iadj2+3
                 do i = 1, nshl
                    xKebe(iel,iadj1,i,inod) = xKebe(iel,iadj1,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem1,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire21,i,inod) 
                    xKebe(iel,iadj2,i,inod) = xKebe(iel,iadj2,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem2,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire22,i,inod) 
                    xKebe(iel,iadj3,i,inod) = xKebe(iel,iadj3,i,inod) 
     &                           - BC(in,4) * xKebe(iel,irem3,i,inod) 
     &                           - BC(in,6) * xKebe(iel,ire23,i,inod) 
                 enddo
                 do i=1,nshl

c
c done with the first and second columns_block-9 for columns AND rows of nshl
c
                    xKebe(iel,irem1,i,inod) = zero 
                    xKebe(iel,irem2,i,inod) = zero 
                    xKebe(iel,irem3,i,inod) = zero 

                    xKebe(iel,ire21,i,inod) = zero 
                    xKebe(iel,ire22,i,inod) = zero 
                    xKebe(iel,ire23,i,inod) = zero 

                 enddo
c
c  now adjust the 3rd row_block-9 for EACH row nshl for EACH element 
c

                 iadj1=7
                 iadj2=iadj1+1
                 iadj3=iadj2+1
                 irem1=1
                 irem2=irem1+1
                 irem3=irem2+1
                 ire21=4
                 ire22=ire21+1
                 ire23=ire22+1
                 do i = 1, nshl
                    xKebe(iel,iadj1,inod,i) = xKebe(iel,iadj1,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem1,inod,i) 
                    xKebe(iel,iadj2,inod,i) = xKebe(iel,iadj2,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem2,inod,i) 
                    xKebe(iel,iadj3,inod,i) = xKebe(iel,iadj3,inod,i) 
     &                           - BC(in,4) * xKebe(iel,irem3,inod,i) 
     &                           - BC(in,6) * xKebe(iel,ire23,inod,i) 

                 enddo
                 do i=1,nshl
                    xKebe(iel,irem1,inod,i) = zero 
                    xKebe(iel,irem2,inod,i) = zero 
                    xKebe(iel,irem3,inod,i) = zero 

c 
                    xKebe(iel,ire21,inod,i) = zero 
                    xKebe(iel,ire22,inod,i) = zero 
                    xKebe(iel,ire23,inod,i) = zero 

                 enddo
                 xKebe(iel,5,inod,inod)=one
                 xKebe(iel,9,inod,inod)=one
              endif
      
 5000         continue
        
c        
c.... end loop over shape functions (nodes)
c        
           enddo
c
c.... end loop over elements
c     
        enddo
c
c These elements should assemble to a matrix with the rows and columns 
c associated with the Dirichlet nodes zeroed out.  Note that BC3 Diag
c
c     
c.... return
c
        return
        end

!> This routine satisfies the BC of the periodic nodes after Ap product
!! @param[in] iBC(nshg) Boundary Condition Code
!! @param[in] iper(nshg) Partners of periodic nodes
!! @param[in] res(nshg,nQs) Residual before BC is applied
!! @param[out] res(nshg,nQs) Residual after satisfaction of BC

        subroutine bc3per (iBC,  res, iper, ilwork,nQs)
        use periodicity  ! this gives you rcount(1:nshg) (real*8)
        
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/workfc.h"
        
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper,        nqs
C
      REAL*8                res
C
C     Local variables
C
      INTEGER             i,           iacc,        is,          isgbeg
      INTEGER             isgend,      itask,       itkbeg,      j
      INTEGER             lenseg,      numseg,      numtask
C
        dimension iBC(nshg),
     &            res(nshg,nQs),           ilwork(nlwork),
     &            iper(nshg)
c
c.... local periodic (and axisymmetric) boundary conditions (no communications)
c
           do j = 1,nshg
              if ((btest(iBC(j),10)) .or. (btest(iBC(j),12))) then
                 i = iper(j)
                 res(i,:) = res(i,:) + res(j,:)
                 res(j,:) = zero
              endif
           enddo


        if(numpe.gt.1) then
c
c.... nodes treated on another processor are eliminated
c     
           numtask = ilwork(1)
           itkbeg = 1
           
           do itask = 1, numtask
              
              iacc   = ilwork (itkbeg + 2)
              numseg = ilwork (itkbeg + 4)
              
              if (iacc .eq. 0) then
                 do is = 1,numseg
                    isgbeg = ilwork (itkbeg + 3 + 2*is)
                    lenseg = ilwork (itkbeg + 4 + 2*is)
                    isgend = isgbeg + lenseg - 1
                    res(isgbeg:isgend,:) = zero
                 enddo
              endif
              
              itkbeg = itkbeg + 4 + 2*numseg
              
           enddo
        endif
c
c.... return
c
        return
        end

!> This routine satisfies the BC of the residual vector for 3D elements.
!!
!! @param[in] iBC(nshg) Boundary Condition Code
!! @param[in] BC(nshg,ndofBC) The boundary condition constraint parameters
!! @param[in] res(nshg,nflow) Residual before BC is applied
!! @param[out] res(nshg,nflow) Residual after satisfaction of BC

        subroutine bc3Res ( iBC,  BC,  res, iper, ilwork)
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                bc,          res
C
        dimension iBC(nshg),
     &            BC(nshg,ndofBC),   
     &            res(nshg,nflow),           ilwork(nlwork),
     &            iper(nshg)
c
c.... local periodic boundary conditions (no communications)
c
        call bc3per(iBC,  res, iper, ilwork, nflow)
c 
c.... pressure 
c
        where (btest(iBC,2))
           res(:,4) = zero
        endwhere
c
c.... velocities
c
c ibits(n1,n2,n3) extracts bits n2+1 through n2+n3 (extending to the left
c as is traditional in binary) of the integer n1
c and returns the base 10 integer. In examples below x y z a b can 
c be 1 or zero without any effect.
c
c.... x1-velocity
c
c if iBC=4   bits of ibc =00000100 => ibits(4,3,3)=0
c if iBC=40  bits of ibc =00101000 => ibits(40,3,3)=5
c if iBC=40  bits of ibc =00101000 => ibits(40,3,2)=1
c
        where (ibits(iBC,3,3) .eq. 1)   ! bits of iBC= xy001zab 
c
c     notice that the extracted 3 bits form the number 1.  below
c     you will see the combinations which make up 2-7, all of the
c     possible velocity combinations
c
          res(:,2) = res(:,2) - BC(:,4) * res(:,1)
          res(:,3) = res(:,3) - BC(:,5) * res(:,1)
          res(:,1) = zero
        endwhere
c
c.... x2-velocity
c
        where (ibits(iBC,3,3) .eq. 2)   ! bits of iBC= xy010zab 
          res(:,1) = res(:,1) - BC(:,4) * res(:,2)
          res(:,3) = res(:,3) - BC(:,5) * res(:,2)
          res(:,2) = zero
        endwhere
c
c.... x1-velocity and x2-velocity
c
        where (ibits(iBC,3,3) .eq. 3)  ! bits of iBC= xy011zab 
          res(:,3) = res(:,3) - BC(:,4) * res(:,1) - BC(:,6) * res(:,2)
          res(:,1) = zero
          res(:,2) = zero
        endwhere
c
c.... x3-velocity
c
        where (ibits(iBC,3,3) .eq. 4)  ! bits of iBC= xy100zab 
          res(:,1) = res(:,1) - BC(:,4) * res(:,3)
          res(:,2) = res(:,2) - BC(:,5) * res(:,3)
          res(:,3) = zero
        endwhere
c
c.... x1-velocity and x3-velocity
c
        where (ibits(iBC,3,3) .eq. 5)  ! bits of iBC= xy101zab 
          res(:,2) = res(:,2) - BC(:,4) * res(:,1) - BC(:,6) * res(:,3)
          res(:,1) = zero
          res(:,3) = zero
        endwhere
c
c.... x2-velocity and x3-velocity
c
        where (ibits(iBC,3,3) .eq. 6)  ! bits of iBC= xy110zab 
          res(:,1) = res(:,1) - BC(:,4) * res(:,2) - BC(:,6) * res(:,3)
          res(:,2) = zero
          res(:,3) = zero
        endwhere
c
c.... x1-velocity, x2-velocity and x3-velocity
c
        where (ibits(iBC,3,3) .eq. 7)  ! bits of iBC= xy111zab 
          res(:,1) = zero
          res(:,2) = zero
          res(:,3) = zero
        endwhere
c
c.... scaled plane extraction boundary condition
c
        
        where (btest(iBC,11))
          res(:,1) = zero
          res(:,2) = zero
          res(:,3) = zero
c         NATHAN TURBULENCE REMOVAL
c         DES - if pressures are interpolated, res should be zero
c         res(:,4) = zero
        endwhere
        
c
c.... return
c
        return
        end


!> Boundary conditions on scalar residual

        subroutine bc3ResSclr (iBC,  res, iper, ilwork)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/sclrs.h"

C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                res
C
C     Local variables
C
      INTEGER             is

c
        dimension iBC(nshg),
     &            res(nshg),                ilwork(nlwork),
     &            iper(nshg)


        if(isclr.eq.0) then
c     
c.... temperature
c     
           where (btest(iBC,1)) res(:) = zero
        else
c
c.... turbulence or scalar
c
           is=isclr+5
           where (btest(iBC,is)) res(:) = zero
        endif
c
c.... local periodic boundary conditions (no communications)
c
        call bc3per(iBC,  res, iper, ilwork, 1)
c
c.... return
c
        return
        end

!> This routine :
!!   1. computes the boundary fluxes
!!   2. prints the results in the file [FLUX.lstep]
!!
!! output:<br>
!!  in file flux.<lstep>.n (similar to restart file): <BR>
!!  - machin  nshg  lstep <BR>
!!  - normal_1 ... normal_nsd <B>outward normal direction</B> <BR>
!!  - tau_1n   ... tau_nsd n <B>boundary viscous flux</B> <BR>

      subroutine Bflux ( y,          ac,        u,      x,
     &                   shp,       shgl,       shpb,   
     &                   shglb,     ilwork,     iBC,
     &                   BC,        iper  )
      
      use pointer_data
      use LagrangeMultipliers 
      
        include "global.h"
        include "mpif.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/mio.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/outpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h" 
        include "common_blocks/workfc.h"

C     Argument variables
C
      INTEGER             ibc,         iper
C
      REAL*8                bc
C
C     Local variables
C
      INTEGER             i,           iblk,        iel
      INTEGER             itmp,        j,           nprold
      INTEGER             n,           npro2
      INTEGER             ntoutv
C
      REAL*8               tn

C
      character*5  cname
      
      real*8    y(nshg,ndof),             ac(nshg,ndof),
     &          u(nshg,nsd),              x(numnp,nsd)
      dimension iBC(nshg),           
     &            BC(nshg,ndofBC),  
     &            iper(nshg)
     
      real*8    shp(MAXTOP,maxsh,MAXQPT),  
     &          shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &          shpb(MAXTOP,maxsh,MAXQPT),
     &          shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c    
c
      real*8    flxres(nshg,nflow),
     &          flxLHS(nshg,1),           flxnrm(nshg,nsd),
     &          temp(nshg),               rtmp(nshg,ndof),
     &          flxTot(nflow),            wallssVec(nshg,ndof)

      real*8    qres(nshg,nsd*nsd)

c
      integer   ilwork(nlwork),
     &          invflx(nshg),             nodflx(nshg)             
c
      character*20 fname1,  fmt1, fmt2, fnamer
      character*25 fname2
      integer  isize, nitems
      integer iarray(50)  ! integers read from headers

      real*8, allocatable, dimension(:,:,:,:) :: xKebe, xGoC
      integer, allocatable, dimension(:,:)    :: ien2
      integer, allocatable, dimension(:)      :: map
      real*8, allocatable, dimension(:,:)     :: xmu2
c
c....  calculate the flux nodes
c
      numflx  = 0
      invflx  = 0
      nodflx  = 0
      do iblk = 1, nelblb 
         iel    = lcblkb(1,iblk)
         lcsyst = lcblkb(3,iblk)
         nenl   = lcblkb(5,iblk) 
         nenbl  = lcblkb(6,iblk) 
         ndofl  = lcblkb(8,iblk)
         nshl   = lcblkb(9,iblk)
         nshlb  = lcblkb(10,iblk)
         npro   = lcblkb(1,iblk+1) - iel 
         call flxNode (mienb(iblk)%p,   miBCB(iblk)%p,  invflx) 
      enddo 
c      do i = 1, nshg
      do i = 1, numnp
         if (invflx(i) .ne. 0) then
            numflx = numflx + 1
            nodflx(numflx) = i
         endif
      enddo
c     
c.... -------------------->   interior elements   <--------------------
c     
c.... initialize the arrays
c     
      flxres = zero
      flxLHS = zero
      flxnrm = zero
      if (numflx .ne. 0)  then !we have flux nodes
         qres   = zero
c     
c.... loop over the element-blocks
c
         lhs    = 0

         ires=2  ! shield e3ql from an unmapped lmassinv

         do iblk = 1, nelblk
c     
c.... set up the parameters
c     
            iel    = lcblk(1,iblk)
            nenl   = lcblk(5,iblk) ! no. of vertices per element
            nshl   = lcblk(10,iblk)
            ndofl  = lcblk(8,iblk)
            lcsyst = lcblk(3,iblk)
            npro   = lcblk(1,iblk+1) - iel 
            ngauss = nint(lcsyst)       
            allocate ( ien2(npro,nshl) )
            allocate ( xmu2(npro,maxsh))
            allocate ( map(npro) )
c
c.... get the elements touching the boundary
c         
            call mapConn( mien(iblk)%p,    ien2,    invflx,
     &                    map,             nshl,    npro,    
     &                    npro2,           nshg )

            nprold = npro
            npro = npro2
         
            if (npro .ne. 0) then

               call mapArray( mxmudmi(iblk)%p, xmu2,    map,
     &                        maxsh,           nprold)
c
c.... allocate the element matrices (though they're not needed)
c
               allocate ( xKebe(npro,9,nshl,nshl) )
               allocate ( xGoC (npro,4,nshl,nshl) )
            if(Lagrange.gt.zero) then
               allocate(loclhsLag(npro,9,nshlb,nshlb,3))
            endif 
c     
c.... compute and assemble the residuals
c     
               call AsIGMR (y,                    ac,
     &                      x,                    xmu2(1:npro,:),
     &                      shp(lcsyst,1:nshl,:),
     &                      shgl(lcsyst,:,1:nshl,:),
     &                      ien2(1:npro,:),       
     &                      flxres,               qres,
     &                      xKebe,                xGoC,
     &                      rtmp)
c     
               deallocate ( xKebe )
               deallocate ( xGoC  )
            if(Lagrange.gt.zero) then
               deallocate(loclhsLag)
            endif
         endif
            deallocate ( ien2  )
            deallocate ( xmu2  )
            deallocate ( map   )
c     
         enddo ! iblk = 1, nelblk
c     
c.... -------------------->   boundary elements   <--------------------
c     
         do iblk = 1, nelblb
c     
c.... set up the parameters
c
            iel    = lcblkb(1,iblk)
            lcsyst = lcblkb(3,iblk)
            nenl   = lcblkb(5,iblk)
            nshl   = lcblkb(9,iblk)
            nenbl  = lcblkb(6,iblk)
            nshlb  = lcblkb(10,iblk)
            npro   = lcblkb(1,iblk+1) - iel 
 
            if(lcsyst.eq.3) lcsyst=nenbl
c     
            if(lcsyst.eq.3 .or. lcsyst.eq.4) then
               ngaussb = nintb(lcsyst)
            else
               ngaussb = nintb(lcsyst)
            endif
c
c.... allocate the element matrices (though they're not needed)
c
            allocate ( xKebe(npro,9,nshl,nshl) )  
 
c.... compute and assemble the residuals
c
            call AsBFlx (u,                      y,
     &                   ac,                     x,
     &                   shpb(lcsyst,1:nshl,:),
     &                   shglb(lcsyst,:,1:nshl,:),
     &                   mienb(iblk)%p,
     &                   miBCB(iblk)%p,           mBCB(iblk)%p,
     &                   invflx,                  flxres,
     &                   flxLHS,                  flxnrm,
     &                   xKebe  )
c     
               deallocate ( xKebe )
c     
c.... end of boundary element loop
c
         enddo !iblk = 1, nelblb

c     DIFFERENT WITH MAHDI VERSION - MAKE SURE IS OK - DES
      endif  ! make sure the zero numflux processors still commu

c.... Communication needed before we take care of periodicity and
c     division of RHS by LHS ???
c...  Note that the domains that do not have flux nodes,
c     have zero flxres, flxLHS, and flxnrm vectors
c
      if ( numpe > 1 ) then
         call commu (flxres, ilwork, nflow, 'in ')
         call commu (flxLHS, ilwork, 1   , 'in ')
         call commu (flxnrm, ilwork, nsd , 'in ')
      endif
c
c  take care of periodic boundary conditions
c
      do j= 1,nshg
         if ((btest(iBC(j),10))) then
            i = iper(j)
            flxLHS(i,1) = flxLHS(i,1) + flxLHS(j,1)
            flxres(i,:) =  flxres(i,:) + flxres(j,:)
         endif
      enddo
c
      do j= 1,nshg
         if ((btest(iBC(j),10))) then
            i = iper(j)
            flxLHS(j,1) = flxLHS(i,1)
            flxres(j,:) = flxres(i,:)
         endif
      enddo
c
c        call bc3per(iBC,  flxres, iper, ilwork, nflow)

c
c.... integrated fluxes (aerodynamic forces update)
c
      flxTot = zero
      do n = 1, numflx
         flxTot = flxTot + flxres(nodflx(n),:)
      enddo
      Force(1) = flxTot(1)
      Force(2) = flxTot(2)
      Force(3) = flxTot(3)

c     ENDIF IN MAHDI VERSION - MAKE SURE IS OK - DES

c
c.... only need to commu if we are going to print surface flux since
c     the force calculation just sums flxres (and each on processor node
c     has his "piece" of the sum already).
c
      ntoutv=ntout
      if ( (mod(lstep, ntoutv) .eq. 0) 
     &     .or.  (istep .eq. nstep(itseq)) ) then

c
c  need to zero the slaves to prevent counting twice
c  (actually unnecessary since flxres of boundary nodes will be counted n
c  times while flxlhs will be counted n times-> the ratio is still
c  correct
c      
         wallssVec=rtmp

         if (numflx .eq. 0) then   !no flux nodes
            rtmp=zero
            wallssVec = zero
         else
c     
c.... ---------------------------->  Solve  <---------------------------
c
c.... compute the viscous and heat fluxes
c     
c
c.... ---------------------------->  Print  <---------------------------
c
c.... nodal fluxes
c
            do i = 1, 3
               where ( (invflx .ne. 0) .and. (flxLHS(:,1) .ne. zero) )
     &              flxres(:,i) = flxres(:,i) / flxLHS(:,1)
            enddo
c     
c.... normalize the outward normal
c     
            temp = sqrt( flxnrm(:,1)**2 
     &                 + flxnrm(:,2)**2 
     &                 + flxnrm(:,3)**2 )
            where ( (invflx .ne. 0) .and. (temp .ne. zero) )
               flxnrm(:,1) = flxnrm(:,1) / temp
               flxnrm(:,2) = flxnrm(:,2) / temp
               flxnrm(:,3) = flxnrm(:,3) / temp
            endwhere
c        NO ENDIF IN MAHDI VERSION - MAKE SURE IT IS OK - DES
         endif !no flux nodes
         
c     
c.... ---------------------------->  Communications <-------------------
c
         if(numpe > 1) then
            call commu (flxres, ilwork, nflow, 'out')
            call commu (flxLHS, ilwork, 1   , 'out')
            call commu (flxnrm, ilwork, nsd , 'out')
         endif
c
         rtmp = zero
         wallssVec  = zero

         do i=1, numnp
            if (invflx(i) .ne. 0) then
               rtmp(i,2:4) = flxres(i,1:3) !viscous flux
c     calculate the WSS
               tn = flxres(i,1) * flxnrm(i,1)
     &            + flxres(i,2) * flxnrm(i,2)
     &            + flxres(i,3) * flxnrm(i,3)

                wallssVec(i,1) = flxres(i,1) - tn * flxnrm(i,1)
                wallssVec(i,2) = flxres(i,2) - tn * flxnrm(i,2)
                wallssVec(i,3) = flxres(i,3) - tn * flxnrm(i,3)
            endif
         enddo

c        ENDIF IN MAHDI VERSION - MAKE SURE IS OK - DES

         itmp = 1
         if (lstep .gt. 0) itmp = int(log10(float(lstep)))+1
         write (fmt1,"('(''flux.'',i',i1,',1x)')") itmp
         write (fname1,fmt1) lstep
      
         fname1 = trim(fname1) // cname(myrank+1)
   
c         open (unit=iflux, file=fname1, status='unknown', 
c     &         form='formatted',err=997)

c      write (iflux) machin, nshg, lstep
c      write (iflux) rtmp(:,1:6)
c
c.... output the results
c     
c         do n = 1, numflx
c            k = nodflx(n)
c            write (iflux,2000) k, (x(k,i), i=1,3), 
c     &           (flxnrm(k,i),  i=1,3),
c     &           (flxres(k,i),  i=1,3)
c         enddo
c         close (iflux)

c... output the results in the new format in restart.step#.proc# file

         itmp = 1
         if (lstep .gt. 0) itmp = int(log10(float(lstep)))+1
         write (fmt2,"('(''restart.'',i',i1,',1x)')") itmp
         write (fname2,fmt2) lstep

         fname2 = trim(fname2) // cname(myrank+1)
c
c.... open input files
c
         call openfile(  fname2,  'append?'//CHAR(0), irstin )
         
         fnamer = 'rin plane traction'
         isize = nshg*ndof
         nitems = 3
         iarray(1) = nshg
         iarray(2) = ndof
         iarray(3) = lstep
         call writeheader(irstin, fnamer,iarray, nitems, isize, 
     &        'double'//CHAR(0), iotype )
    
c         fnamer = 'boundary flux'        
         nitems = nshg*ndof
         call writedatablock(irstin, fnamer,rtmp(:,2:4), nitems,
     &        'double'//CHAR(0), iotype)
        
         call closefile( irstin, "append"//CHAR(0) )
c         call Write_boundaryflux(myrank,lstep,nshg,ndof,rtmp(:,1:ndof))

c     wallss vectors into the restart file(s)
c         if( iowflux .eq. 1) then
            call openfile(  fname2,  'append?'//CHAR(0), irstin )
            
            fnamer = 'rwall shear stresses'
            isize = nshg*ndof
            
            nitems = 3
            iarray(1) = nshg
            iarray(2) = ndof
            iarray(3) = lstep
            call writeheader(irstin, fnamer,iarray, nitems, isize, 
     &           'double'//CHAR(0), iotype )

c     fnamer = 'boundary flux'
            nitems = nshg*ndof

         
c     wall shear stresses vectors  are in wallssVec
            call writedatablock(irstin, fnamer,wallssVec, nitems, 
     &           'double'//CHAR(0), iotype)
            
            call closefile( irstin, "append"//CHAR(0) )         
c         endif! iowflux

      endif
c     
      return
c
c.... file error handling
c
997     call error ('bflux   ','opening ', iflux)
c
c$$$1000    format(' ',a80,/,1x,i10,1p,3e20.7)
 2000   format(i6,9(2x,E12.5e2))
c$$$2001    format(1p,1x,i6,3e15.7)
c
c.... end
c
        end

c     only for stresses
      subroutine newBflux ( y,          ac,        u,       x,
     &                   shp,       shgl,       shpb,
     &                   shglb,     ilwork,     iBC,
     &                   BC,        iper  )

      use pointer_data
      use LagrangeMultipliers

        include "global.h"
        include "mpif.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/mio.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/outpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"

C     Argument variables
C
      INTEGER             ibc,         iper
C
      REAL*8                bc
C
C     Local variables
C
      INTEGER             i,           iblk,        iel
      INTEGER             itmp,        j,           nprold
      INTEGER             n,           npro2
      INTEGER             ntoutv
C
      REAL*8               tn

C
      character*5  cname

      real*8    y(nshg,ndof),             ac(nshg,ndof),
     &          u(nshg,nsd),              x(numnp,nsd)
      dimension iBC(nshg),
     &            BC(nshg,ndofBC),
     &            iper(nshg)

      real*8    shp(MAXTOP,maxsh,MAXQPT),
     &          shgl(MAXTOP,nsd,maxsh,MAXQPT),
     &          shpb(MAXTOP,maxsh,MAXQPT),
     &          shglb(MAXTOP,nsd,maxsh,MAXQPT)
c
c
      real*8    wallssVec(nshg,nsd),
     &          sA(nshg,1),               sTF(nshg,nsd),
     &          sSF(nshg,nsd),            walltsVec(nshg,nsd),
     &          xnew(numnp,nsd),          vnew(nshg,nsd)

      real*8    qres(nshg,nsd*nsd)

c
      integer   ilwork(nlwork),
     &          invflx(nshg),             nodflx(nshg)
c
      character*20 fname1,  fmt1, fmt2, fnamer
      character*25 fname2
      integer  isize, nitems
      integer iarray(50)  ! integers read from headers

      real*8, allocatable, dimension(:,:,:,:) :: xKebe, xGoC
      integer, allocatable, dimension(:,:)    :: ien2
      integer, allocatable, dimension(:)      :: map
      real*8, allocatable, dimension(:,:)     :: xmu2

      ntoutv=ntout
      if ( (mod(lstep, ntoutv) .eq. 0)
     &     .or.  (istep .eq. nstep(itseq)) ) then
c
c....  calculate the flux nodes
c
          numflx  = 0
          invflx  = 0
          nodflx  = 0
          do iblk = 1, nelblb
             iel    = lcblkb(1,iblk)
             lcsyst = lcblkb(3,iblk)
             nenl   = lcblkb(5,iblk)
             nenbl  = lcblkb(6,iblk)
             ndofl  = lcblkb(8,iblk)
             nshl   = lcblkb(9,iblk)
             nshlb  = lcblkb(10,iblk)
             npro   = lcblkb(1,iblk+1) - iel
             call flxNode (mienb(iblk)%p,   miBCB(iblk)%p,  invflx)
          enddo

          xnew(:,1:nsd)=x(:,1:nsd)
c      do i = 1, nshg
          do i = 1, numnp
             if (invflx(i) .ne. 0) then
                numflx = numflx + 1
                nodflx(numflx) = i
                if(ideformwall.eq.1) then
                  if(applyWallDeformation.eq.1) then
                    xnew(i,:)=x(i,:)+u(i,:)
                  endif
                endif
             endif
          enddo

         walltsVec = zero
         wallssVec = zero
         sA=zero
         sTF=zero
         sSF=zero

         if( numflx .ne. 0) then

             vnew=y(:,1:nsd)

            do iblk = 1, nelblb
               iel    = lcblkb(1,iblk)
               nshl   = lcblkb(9,iblk)
               npro   = lcblkb(1,iblk+1) - iel
               call calculateWallStresses(sA,sTF,sSF,xnew,vnew,
     &          mienb(iblk)%p,miBCB(iblk)%p)
            enddo
         endif

         if(numpe > 1) then
             call commu (sA, ilwork, 1   , 'in ')
             call commu (sTF, ilwork, nsd, 'in ')
             call commu (sSF, ilwork, nsd, 'in ')
         endif

         do j= 1,nshg
           if ((btest(iBC(j),10))) then
            i = iper(j)
            sA(i,1) = sA(i,1) + sA(j,1)
            sTF(i,:) =  sTF(i,:) + sTF(j,:)
            sSF(i,:) =  sSF(i,:) + sSF(j,:)
           endif
         enddo

         do j= 1,nshg
           if ((btest(iBC(j),10))) then
            i = iper(j)
            sA(j,1) = sA(i,1)
            sTF(j,:) = sTF(i,:)
            sSF(j,:) = sSF(i,:)
           endif
         enddo

         if(numpe > 1) then
             call commu (sA, ilwork, 1   , 'out')
             call commu (sTF, ilwork, nsd, 'out')
             call commu (sSF, ilwork, nsd, 'out')
         endif

         do i = 1, numnp
             if (invflx(i) .ne. 0 .AND. sA(i,1).ne.zero) then
                walltsVec(i,:) = sTF(i,:)/sA(i,1)
                wallssVec(i,:) = sSF(i,:)/sA(i,1)
             endif
         enddo

         itmp = 1
         if (lstep .gt. 0) itmp = int(log10(float(lstep)))+1
         write (fmt2,"('(''restart.'',i',i1,',1x)')") itmp
         write (fname2,fmt2) lstep

         fname2 = trim(fname2) // cname(myrank+1)
c
c.... open input files
c
         call openfile(  fname2,  'append?'//CHAR(0), irstin )

c     write velocity based in-plane traction
         fnamer = 'vin plane traction'

         isize = nshg*nsd
         nitems = 3
         iarray(1) = nshg
         iarray(2) = nsd
         iarray(3) = lstep
         call writeheader(irstin, fnamer,iarray, nitems, isize,
     &        'double'//CHAR(0), iotype )

         nitems = nshg*nsd
         call writedatablock(irstin, fnamer,walltsVec, nitems,
     &        'double'//CHAR(0), iotype)

c     write velocity-based wall shear stress
        fnamer = 'vwall shear stresses'

        isize = nshg*nsd
        nitems = 3
        iarray(1) = nshg
        iarray(2) = nsd
        iarray(3) = lstep
        call writeheader(irstin, fnamer,iarray, nitems, isize,
     &           'double'//CHAR(0), iotype )

        nitems = nshg*nsd
        call writedatablock(irstin, fnamer,wallssVec, nitems,
     &           'double'//CHAR(0), iotype)

        call closefile( irstin, "append"//CHAR(0) )

      endif

      return
c
c.... file error handling
c
      call error ('newbflux   ','opening ', iflux)

      end

!> This routine flags the flux nodes

      subroutine flxNode(ienb, iBCB, flg)

        include "global.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C     Local variables
C
      INTEGER             i,           j,           nodelcl
C
c
      integer   flg(nshg),        iBCB(npro,ndiBCB),     
     &          ienb(npro, nshl), lnode(27)

c
c.... compute the nodes which lie on the boundary (hierarchic)
c
      call getbnodes(lnode)

      do i=1, npro 
         if (nsrflist(iBCB(i,2)).eq.1) then
            do j=1, nshlb
               nodelcl = lnode(j)
               flg(abs(ienb(i,nodelcl)))=flg(abs(ienb(i,nodelcl)))+1  
            enddo
         endif
      enddo
c
      return
      end

!> This routine calculate wall stresses on the flux nodes

      subroutine calculateWallStresses(sA, sTF, sSF, x, vv, ienb, iBCB)

        include "global.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/matdat.h"

      integer   ienb(npro, nshl), iBCB(npro,ndiBCB)
      real*8    sA(nshg,1), sTF(nshg,nsd), sSF(nshg,nsd),
     &          vv(nshg,nsd),x(numnp,nsd)

      INTEGER       i,  j, k, n1, n2
      integer    nid(nshl)
      real*8     beArea, eJac,normM, temp, coef
      real*8     beNorm(nsd), eNaNbx(nshl,nsd), ux(nsd,nsd)
      real*8     V(nsd,nsd-1), normV(nsd)
      real*8     SJ, Xa, Xb, Xc, Ya, Yb, Yc, Za, Zb, Zc
      REAL*8     Tdn(nsd), ndTdn, taue(nsd),rmu

      coef=24D0
      rmu = datmat(1,2,1)

      do n1=1, npro

        if (nsrflist(iBCB(n1,2)).ne.1) then
          cycle
        end if

        nid(1:nshl)=ienb(n1,:)

c     for beNorm
        DO j=1,nsd-1
           DO i=1,nsd
              V(i,j) = x(nid(j+1),i)-x(nid(1),i)
           END DO
        END DO

         normV(1) = V(2,1)*V(3,2) - V(3,1)*V(2,2);
         normV(2) = V(3,1)*V(1,2) - V(1,1)*V(3,2);
         normV(3) = V(1,1)*V(2,2) - V(2,1)*V(1,2);

        normM = zero
        DO i=1,nsd
           normM = normM + normV(i)**2
        END DO
        normM = SQRT(normM);
        beNorm(1:nsd) = normV/normM;

        temp = zero
        DO i=1, nsd
           temp = temp + beNorm(i)*(x(nid(nshl),i)-x(nid(1),i))
        END DO
        IF (temp .GT. 0D0) THEN
           beNorm = -beNorm
        END IF

c     for beArea
         Xa = x(nid(1),1) - x(nid(3),1);
         Xb = x(nid(2),1) - x(nid(3),1);

         Ya = x(nid(1),2) - x(nid(3),2);
         Yb = x(nid(2),2) - x(nid(3),2);

         Za = x(nid(1),3) - x(nid(3),3);
         Zb = x(nid(2),3) - x(nid(3),3);

         beArea = SQRT((Xa*Yb - Xb*Ya)**2D0
     &   + (Ya*Zb - Yb*Za)**2D0 + (Za*Xb - Zb*Xa)**2D0)

         Xa = x(nid(1),1) - x(nid(4),1);
         Xb = x(nid(2),1) - x(nid(4),1);
         Xc = x(nid(3),1) - x(nid(4),1);

         Ya = x(nid(1),2) - x(nid(4),2);
         Yb = x(nid(2),2) - x(nid(4),2);
         Yc = x(nid(3),2) - x(nid(4),2);

         Za = x(nid(1),3) - x(nid(4),3);
         Zb = x(nid(2),3) - x(nid(4),3);
         Zc = x(nid(3),3) - x(nid(4),3);

         eJac = Xa*Yb*Zc + Xb*Yc*Za + Xc*Ya*Zb -
     &      (Xc*Yb*Za + Xb*Ya*Zc + Xa*Yc*Zb)

         SJ = ABS(eJac)/eJac/24D0
         eJac = ABS(eJac)

         eNaNbx(1,1) = SJ*(Yb*Zc - Yc*Zb);
         eNaNbx(2,1) = SJ*(Yc*Za - Ya*Zc);
         eNaNbx(3,1) = SJ*(Ya*Zb - Yb*Za);

         eNaNbx(1,2) = SJ*(Xc*Zb - Xb*Zc);
         eNaNbx(2,2) = SJ*(Xa*Zc - Xc*Za);
         eNaNbx(3,2) = SJ*(Xb*Za - Xa*Zb);

         eNaNbx(1,3) = SJ*(Xb*Yc - Xc*Yb);
         eNaNbx(2,3) = SJ*(Xc*Ya - Xa*Yc);
         eNaNbx(3,3) = SJ*(Xa*Yb - Xb*Ya);

          DO i=1, nsd
             eNaNbx(nshl,i) = zero;
             DO n2=1, nshl-1
                eNaNbx(nshl,i) = eNaNbx(nshl,i) - eNaNbx(n2,i);
             END DO
          END DO

        ux = zero
        DO n2=1, nshl
           DO i=1, nsd
              DO j=1, nsd
                 ux(i,j) = ux(i,j)
     &                  + coef*vv(nid(n2),i)*eNaNbx(n2,j)/eJac
              END DO
           END DO
        END DO

        DO i=1,nsd
           Tdn(i) = zero
           DO j=1, nsd
              Tdn(i) = Tdn(i) + rmu*(ux(i,j) + ux(j,i))*beNorm(j)
           END DO
        END DO

        ndTdn = zero
        DO i=1,nsd
           ndTdn = ndTdn + Tdn(i)*beNorm(i)
        END DO

        taue = Tdn - ndTdn*beNorm

        DO n2=1,3
           sA(nid(n2),1) = sA(nid(n2),1) + beArea
           DO i=1,nsd
              sTF(nid(n2),i) = sTF(nid(n2),i) - beArea*Tdn(i)
              sSF(nid(n2),i) = sSF(nid(n2),i) - beArea*taue(i)
           END DO
        END DO

      enddo

      return
      end

!> Create a condensed connectivity array based on the nodes in
!! mask.
      
      subroutine mapConn( ien,      ien2,    mask,
     &                    map,      nshl,    npro,    
     &                    npro2,    nshg )
      
      integer ien(npro,nshl),ien2(npro,nshl),mask(nshg),
     &        map(npro)

      integer nshl, nshg, npro, npro2, i, iel

c
c.... first build the map
c      
      map = 0
      do i = 1, nshl
         do iel = 1, npro
            map(iel) = map(iel) + mask( abs(ien(iel,i)) )
         enddo
      enddo
      
      npro2 = 0
      do iel = 1, npro
         if ( map(iel) .gt. 0 ) then
            npro2 = npro2 + 1
            map(iel) = npro2
         else
            map(iel) = npro
         endif
      enddo
c
c.... create the condensed connectivity array
c
      if ( npro2 .gt. 0 ) then
         do i = 1, nshl
            do iel = 1, npro
               ien2(map(iel),i) = ien(iel,i)
            enddo
         enddo
      endif
      
      return 
      end

!> Maps array x into array x2 based on the given map
               
      subroutine mapArray(x,x2,map,nshl,nprold)

      real*8   x(nprold,nshl),    x2(nprold,nshl)      
      integer  map(nprold)
      integer   nprold, nshl,i
c
c.... map the array
c
      do i = 1, nshl
         x2(map(:),i) = x(:,i)
      enddo

      return 
      end

        block data endata
c
c----------------------------------------------------------------------
c 
c  Almost all data statements are stored in this block data.
c
c----------------------------------------------------------------------
c
        include "global.h"
        include "common_blocks/mbndnod.h"
        include "common_blocks/melmcat.h"
        include "common_blocks/mintpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/mioname.h"
        include "common_blocks/mmatpar.h"
        include "common_blocks/msympar.h"
        include "common_blocks/mtimer1.h"
        include "common_blocks/mtimer2.h"
        include "common_blocks/point.h"
        include "common_blocks/resdat.h"
        include "common_blocks/workfc.h"

C     Local variables
C
      INTEGER             i,           j,           k
C
c
c----------------------------------------------------------------------
c
c.... quadrature point data
c
c----------------------------------------------------------------------

      data master / 0 /
c
c.... boundary nodes of boundary elements
c.... common /bndnod/ mnodeb(9,8,3)
c
        data (((mnodeb(i,j,k), i=1,9), j=1,8), k=1,2)
     &              /  1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  0,  0,   0,  0,  0,   0,  0,  0,     ! 1D
     &                 1,  2,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  0,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  5,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  4,   0,  0,  0,   0,  0,  0,
     &                 1,  2,  4,   0,  0,  0,   0,  0,  0,
     &                 1,  2,  4,   0,  0,  0,   0,  0,  0   /  ! 2D
        data (((mnodeb(i,j,k), i=1,9), j=1,8), k=3,3)
     &              /  1,  2,  3,   4,  0,  0,   0,  0,  0, 
     &                 1,  2,  3,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  3,   0,  0,  0,   0,  0,  0, 
     &                 1,  2,  5,   4,  0,  0,   0,  0,  0, 
     &                 1,  2,  3,   4,  9, 10,  11, 12, 21,
     &                 1,  2,  3,   5,  6,  9,   0,  0,  0,
     &                 1,  2,  3,   7,  9,  8,   0,  0,  0,
     &                 1,  2,  5,   4,  7, 10,  13, 14, 16   /  ! 3D  
c
c----------------------------------------------------------------------
c
c.... element category information
c.... common /elmcat/ mcsyst, melCat, nenCat(8,3), nfaCat(8,3)
c
        data mcsyst, melCat
     &     /   4,      8    /           ! caution: see below
c
        data nenCat /  2,  2,  2,  2,    3,  3,  3,  3,         ! 1D
     &                 4,  3,  3,  4,    9,  6,  6,  9,         ! 2D
     &                 8,  4,  6,  6,   27, 10, 18, 18    /     ! 3D
c
        data nfaCat /  2,  2,  2,  2,    2,  2,  2,  2,         ! 1D
     &                 4,  3,  3,  4,    4,  3,  3,  4,         ! 2D
     &                 6,  4,  5,  5,    6,  4,  5,  5    /     ! 3D
c
c melCat affects: nenCat, nfaCat, mnodeb
c
c----------------------------------------------------------------------
c
c.... maximum number of quadrature points per nsd
c.... common /intpar/ intmax
c
        data intmax /  3  /
c
c----------------------------------------------------------------------
c
c.... io channels
c.... common /io    / iin,    igeom,  ipar,   ibndc,  imat,   iecho,
c....                 iout,   ichmou, irstin, irstou, ihist,  iflux,
c....                 ierror, itable, iforce, igraph, itime
c
        data    iin,    igeom,  ipar,   ibndc,  imat,   iecho,
     &          iout,   ichmou, irstin, irstou, ihist,  iflux,
     &          ierror, itable, iforce, igraph, itime
     &  /       10,     11,     12,     13,     14,     15,
     &          16,     17,     18,     19,     20,     21,
     &          22,     23,     24,     25,     26      /
c
c----------------------------------------------------------------------
c
c.... io file names
c.... common /ioname/ fin,    fgeom,  fpar,   fbndc,  fmat,   fecho,
c....                 frstin, frstou, fhist,  ferror, ftable, fforce,
c....                 ftime
c
        data    fin,    fgeom,  fpar,   fbndc,  fmat,   fecho,
     &          frstin, frstou, fhist,  ferror, ftable, fforce, fgraph,
     &          ftime
     &  /       'input.dat',            'geombc.dat',
     &          'partition.dat',        'bc.dat',
     &          'material.dat',         'echo.dat',
     &          'restart',           'restart',
     &          'histor.dat',           'error.dat',
     &          'table.dat',            'forces.dat',
     &          'graph.dat',            'time.out'       /
c
c----------------------------------------------------------------------
c
c.... run parameters 
c.... common /matpar/ ithm,   pr,     Planck, Stefan, Nh,     Rh,
c                     Rgas,   gamma,  gamma1, s0,     const,  xN2,
c                     xO2,    yN2,    yO2,    Msh,    cpsh,   s0sh,
c                     h0sh,   Rs,     cps,    cvs,    h0s,    Trot,
c                     sigs,   Tvib,   g0s,    dofs
c
c        data    pr
c     &  /       7.20000000000000d-1      /
c
        data    Planck,               Stefan,
     &          Nh,                   Rh,
     &          gamma,                gamma1
     &  /       6.62617600000000d-34, 5.66970000000000d-08,
     &          6.02204500000000d+23, 8.31441000000000d+0,
     &          1.40000000000000d+0,  0.40000000000000d+0    /
c
        data    xN2,                  xO2
     &  /       0.79000000000000d+0,  0.21000000000000d+0     /
c
        data    Msh 
     &  /       2.80000000000000d-2,  3.20000000000000d-2,
     &          3.00000000000000d-2,  1.40000000000000d-2,
     &          1.60000000000000d-2    /
c
        data    h0sh
     &  /       0.00000000000000d+0,  0.00000000000000d+0,
     &          8.97750000000000d+4,  4.70820000000000d+5,
     &          2.46790000000000d+5    /
c
        data    Trot
     &  /       2.87000000000000d+0,  2.08000000000000d+0,
     &          2.45000000000000d+0,  0.00000000000000d+0,
     &          0.00000000000000d+0    /
c
        data    sigs
     &  /       2.00000000000000d+0,  2.00000000000000d+0,
     &          1.00000000000000d+0,  0.00000000000000d+0,
     &          0.00000000000000d+0    /
c
        data    Tvib
     &  /       3.39350000000000d+3,  2.27356000000000d+3,
     &          2.73887000000000d+3,  0.00000000000000d+0,
     &          0.00000000000000d+0    /
c
        data    g0s
     &  /       1.00000000000000d+0,  3.00000000000000d+0,
     &          4.00000000000000d+0,  4.00000000000000d+0,
     &          9.00000000000000d+0    /
c
        data    dofs
     &  /       5.00000000000000d+0,  5.00000000000000d+0,
     &          5.00000000000000d+0,  3.00000000000000d+0,
     &          3.00000000000000d+0    /
c
c----------------------------------------------------------------------
c
c.... dynamic storage pointer management data
c.... common /point / mbeg,   mend,   mprec
c
        data    mbeg,    mend,    mprec
     &  /       1,      100000,     2   /
c
c----------------------------------------------------------------------
c
c.... residual statistics data
c.... common /resdat/ resfrt
c
        data    resfrt
     &  /       0.00000000000000d+0     /
c
c----------------------------------------------------------------------
c
c.... symmetric storage pointers
c.... common /sympar/ indsym(5,5)
c
        data indsym /    1,  2,  4,  7, 11,
     &                   2,  3,  5,  8, 12,
     &                   4,  5,  6,  9, 13,
     &                   7,  8,  9, 10, 14,
     &                  11, 12, 13, 14, 15   /
c
c----------------------------------------------------------------------
c
c.... timer parameters
c.... common /timer1/ ccode(13)
c.... common /timer2/ icd
c
        data    ccode
     &  /       'Input   ', 'PrProces', 'Rezoning', 'Elm_Form',
     &          'Solver  ', 'Bnd_Flux', 'Output  ', 'Mapping ',
     &          'Gather  ', 'Scatter ', 'Begin   ', 'End     ',
     &          'Back    ' /
c
        data    icd
     &  /       11         /
c
c----------------------------------------------------------------------
c
c.... end
c
        end

      function cname (i)
C     Argument variables
C
      INTEGER             i
C
C     Local variables
C
      INTEGER             i0,          i1,          i2,          i3
      INTEGER             ic0,         ii
C
      logical beg
      CHARACTER*5 cname,cc

      ic0 = ICHAR("0")
      cc = " "
      ii = i

      i0 = mod (ii,10)
      ii = (ii - i0) / 10
      i1 = mod (ii,10)
      ii = (ii - i1) / 10
      i2 = mod (ii,10)
      ii = (ii - i2) / 10
      i3 = mod (ii,10)

      beg = .false.

      IF (i3 .ne. 0) then
        beg = .true.
        cc  = CHAR(ic0 + i3)
      ENDIF
      IF (i2 .ne. 0 .or. beg) then
        beg = .true.
        cc = TRIM(cc)//CHAR(ic0 + i2)
      ENDIF
      IF (i1 .ne. 0 .or. beg) then
        beg = .true.
        cc = TRIM(cc)//CHAR(ic0 + i1)
      ENDIF

      cc = TRIM(cc)//CHAR(ic0 + i0)
      cname = "." // cc

      return
      end

      function cname2 (i)
C     Argument variables
C
      INTEGER             i
C
C     Local variables
C
      INTEGER             ic0,         ii,          k
C
      logical      beg
      character*10 cname2,cc
      integer      il(0:8)

      ic0 = ICHAR("0")
      cc = " "
      ii = i

      il(0) = mod(ii,10)
      do k = 1,8
        ii = (ii - il(k-1)) / 10
        il(k) = mod (ii,10)
      enddo

      beg = .false.

      do k = 8,1,-1
        if (il(k) .ne. 0 .or. beg) then
          beg = .true.
          cc  = TRIM(cc) // CHAR(ic0 + il(k))
        endif
      enddo

      cc = TRIM(cc)//CHAR(ic0 + il(0))
      cname2 = "." // cc

      return
      end

!> This subroutine is responsible for interprocessor communication of
!! the residual and solution vectors.
!!
!! @param[in] global(nshg,n) Global vector to be communicated. Note that
!!                           this vector is local to the processor, (i.e.
!!                           not distributed across processors)
!! @param[in] ilwork(nlwork) This is the local interprocessor work array.
!!                           This array is local to the processor, (i.e.
!!                           each processor has a unique ilwork array.
!! @param[in] n              Second dimension of the array to be communicated
!! @param[in] code           = 'in' for communicating with the residual
!!                           = 'out' for cummunicating the solution
!!
!!
!! The array ilwork describes the details of the communications. 
!! Each communication step (call of this routine) consists of a 
!! sequence of "tasks", where a task is defined as a communication 
!! between two processors where data is exchanged. This would imply 
!! that for a given processor, there will be as many tasks as there
!! are processors with which it must communicate. Details of the 
!! ilwork array appear below.

      subroutine commu (global, ilwork,  n,  code)

        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/workfc.h"

C     Argument variables
C
      INTEGER             ilwork,      n
C
      REAL*8                global
C
C     Local variables
C
      INTEGER             iacc,        idl,         idof,        ierr
      INTEGER             iother,      is,          isgbeg,      isgend
      INTEGER             itag,        itask,       itemp,       itkbeg
      INTEGER             j,           jdl,         kdof,        lenseg
      INTEGER             lfront,      m
      INTEGER             numseg,      numtask
C
      REAL*8                rtemp
C
      integer status(MPI_STATUS_SIZE)
      integer stat(MPI_STATUS_SIZE, 2*numpe), req(2*numpe)

      dimension global(nshg,n),
     &          rtemp(maxfront*n,numpe),
     &          ilwork(nlwork)
 
      character*3 code


      if (code .ne. 'in ' .and. code .ne. 'out') 
     &  call error ('commu   ','code    ',0)

      if     (n .eq. 1)      then        ! like a scalar
        kdof = 1
      elseif (n .eq. nsd)    then        ! like the normal vectors
        kdof = 2
      elseif (n .eq. ndof)   then        ! res, y, ac, krylov vectors....
        kdof = 3
      elseif (n .eq. nflow*nflow) then     ! bdiag
        kdof = 4
      elseif (n .eq. (nflow-1)*nsd) then  ! qres
        kdof = 5
      elseif (n .eq. nflow) then
        kdof = 6
      elseif (n .eq. 24 ) then
        kdof = 7
      elseif (n .eq. 9) then
        kdof = 8
      elseif (n .eq. 11 ) then
        kdof = 9
      elseif (n .eq. 7 ) then
        kdof = 10
      elseif (n .eq. 33 ) then
         kdof = 11 
      elseif (n .eq. 22 ) then
         kdof = 12
      elseif (n .eq. 16 ) then
         kdof = 13
      elseif (n .eq. 10 ) then
         kdof = 14
       elseif (n .eq. nflow*nsd ) then   !surface tension + qres
         kdof = 15 
      else
        call error ('commu   ','n       ',n)
      endif

c... Note that when adding another kdof to the above set, we must
c... also make changes in ctypes.f and auxmpi.h

c---------------------------------------------------------------------
c  ilwork(1): number of tasks
c
c  The following information is contained in ilwork for each task:
c     itag: tag of the communication
c     iacc: == 0 if task is a send
c           == 1 if task is a recieve
c     iother: rank of processor with which this communication occurs
c     numseg: number of data "segments" to be sent or recieved. A 
c             segment is defined as a continuous section of the global
c             vector to be communicated, (i.e. a group of nodes (or,
c             rather, "shape function coefficients") which occur 
c             sequentially in the array global(nshg,n)).
c     isbeg:  location of the first segment in the array owned by the
c             current processor.
c
c The two types of communication are 'in', where the residual is being
c communicated, and 'out', where the solution is being communicated.
c Note that when the type is 'out', senders recieve and recievers send.
c
c The following comment pertains to a communication of type 'in':
c
c     If the task is a send, then all of the numseg segments are
c     sent with a single call to MPI_SEND. Where these segments live in 
c     the array is built into the array sevsegtype, which is a common 
c     array constructed in the subroutine "ctypes.f". In other words,
c     sevsegtype is a data type that describes the indices of the blocks
c     to be sent, in terms of there beginning index, and the length of 
c     each segment. Using this, we can make a single send to take care of
c     all the segments for this task. 
c      
c     If the task is a recieve, then once the vector is recieved, the
c     recieved segments must be added to the correct locations in the
c     current array. These locations are described in ilwork as the
c     beginning position, then the length of the segment.
c     
c---------------------------------------------------------------------
      numtask = ilwork(1)
      
      itkbeg = 1
      m = 0
      idl=0

      DO itask = 1, numtask
        m      = m + 1
        itag   = ilwork (itkbeg + 1)
        iacc   = ilwork (itkbeg + 2)
        iother = ilwork (itkbeg + 3)
        numseg = ilwork (itkbeg + 4)
        isgbeg = ilwork (itkbeg + 5)
c
c.... if iacc == 0, then this task is a send.
c     slave
c
        if (iacc .EQ. 0) then  
c
c.... residual communication
c
          if (code .eq. 'in ')
     &      call MPI_ISEND(global(isgbeg, 1), 1, sevsegtype(itask,kdof), 
     &                     iother, itag, MPI_COMM_WORLD, req(m), ierr)
c
c.... solution communication
c
          if (code .eq. 'out') then
            call MPI_IRECV(global(isgbeg, 1), 1, sevsegtype(itask,kdof), 
     &                     iother, itag, MPI_COMM_WORLD, req(m), ierr)
c            call MPI_RECV(global(isgbeg,1), 1, sevsegtype(itask,kdof),
c     &                    iother, itag, MPI_COMM_WORLD, status, ierr)
          endif
c
c.... if iacc == 1, then this task is a recieve.
c     master
c
        else
          if (code .eq. 'in ') then
c
c.... determine the number of total number of nodes involved in this
c     communication (lfront), including all segments
c
            lfront = 0
            do is = 1,numseg
              lenseg = ilwork (itkbeg + 4 + 2*is)
              lfront = lfront + lenseg
            enddo
c
c.... recieve all segments for this task in a single step
c
            idl=idl+1 ! stands for i Do Later, the number to fix later
            call MPI_IRECV(rtemp(1,idl), lfront*n, MPI_DOUBLE_PRECISION, 
     &                     iother, itag, MPI_COMM_WORLD, req(m), ierr)
          endif
          if (code .eq. 'out') then
            call MPI_ISEND(global(isgbeg, 1), 1, sevsegtype(itask,kdof), 
     &                     iother, itag, MPI_COMM_WORLD, req(m), ierr)
          endif
        endif

        itkbeg = itkbeg + 4 + 2*numseg

      enddo   !! end tasks loop

      call MPI_WAITALL(m, req, stat, ierr)

c
c     Stuff added below is a delayed assembly of that which was communicated
c     above but due to the switch to non-blocking receivves could not be
c     assembled until after the waitall.  Only necessary for commu "in"
c

      if(code .eq. 'in ') then
         itkbeg=1
         jdl=0
         do j=1,numtask         ! time to do all the segments that needed to be
                                ! assembled into the global vector

            iacc   = ilwork (itkbeg + 2)
            numseg = ilwork (itkbeg + 4)
            isgbeg = ilwork (itkbeg + 5)
            if(iacc.eq.1) then
               jdl=jdl+1  ! keep track of order of rtemp's
c
c... add the recieved data to the global array on the current processor.
c    Note that this involves splitting up the chunk of recieved data
c    into its correct segment locations for the current processor.
c
               itemp = 1
               do idof = 1,n
                  do is = 1,numseg
                 isgbeg = ilwork (itkbeg + 3 + 2*is)
                 lenseg = ilwork (itkbeg + 4 + 2*is)
                 isgend = isgbeg + lenseg - 1
                 global(isgbeg:isgend,idof) = global(isgbeg:isgend,idof)
     &                                + rtemp (itemp:itemp+lenseg-1,jdl)
                 itemp = itemp + lenseg
                  enddo
               enddo
            endif ! end of receive (iacc=1)
            itkbeg = itkbeg + 4 + 2*numseg
         enddo
      endif  ! commu "in"
      return
      end

      subroutine ctypes (ilwork)

        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/newdim.h"
        
C     Parameter variables
C
      INTEGER             maxseg
C
C     Argument variables
C
      INTEGER             ilwork
C
C     Local variables
C
      INTEGER             iacc,        ierr,        ioffset,     is
      INTEGER             isbegin,     itask,       itkbeg,      kdof
      INTEGER             lenseg,      lfront,      lstride
      INTEGER             numseg,      numtask
C
      parameter (maxseg = 10000)
      integer  sizeofdouble

      dimension ilwork(nlwork)
      dimension isbegin(maxseg), lenseg(maxseg), ioffset(maxseg)

      CALL MPI_TYPE_EXTENT (MPI_DOUBLE_PRECISION,sizeofdouble,ierr)
      lstride = nshg * sizeofdouble
c
c.... maxfront is a common variable being set in this routine
c
      maxfront = 0  
      numtask = ilwork (1)
      itkbeg  = 1

      if (numtask .gt. maxtask) 
     &  call error('ctypes  ','numtask ',numtask)

      nshg0 = nshg

      do itask = 1,numtask
c
c.... iacc = 0 ==> this task is a send
c          = 1 ==> this task is a recieve
c
        iacc   = ilwork (itkbeg + 2)
c
c.... numseg : number of data segments to be communicated
c
        numseg = ilwork (itkbeg + 4)
c
c.... adjust the number of the other processor, since processors
c     are numbered here starting from 0, not 1.
c
        ilwork (itkbeg + 3) = ilwork (itkbeg + 3) - 1 
        if (numseg .gt. maxseg) 
     &    call error('ctypes  ','numseg  ',numseg )
c
c.... lfront = total number of nodes involved in this task
c     
        lfront = 0
        do is = 1,numseg
c
c.... isbegin(is): starting node number for each segment
c
          isbegin (is) = ilwork (itkbeg + 3 + 2*is)
c
c.... lenseg(is): length of each segment (number of nodes)
c
          lenseg  (is) = ilwork (itkbeg + 4 + 2*is)
c
c.... increment the total node counter
c
          lfront       = lfront + lenseg(is)
c
c.... nshg0: number of nodes to be assembled on this processor,
c             i.e. subtract the number of nodes which will be 
c             sent to another processor.
c
        if (iacc .eq. 0) nshg0 = nshg0 - lenseg(is)
        enddo
c
c.... maxfront: number of nodes which will be communicated, including
c               all segments. Note that after the loop over tasks
c               is complete, maxfront will contain the maximum number
c               of nodes for any of the tasks.
c
        maxfront = MAX(maxfront,lfront)
c
c.... ioffset: array offset from the first node in the first segment
c
        ioffset(1:numseg) = isbegin(1:numseg) - isbegin(1)
c
c.... now set up the MPI data types which will be used in commu.f.
c     These data types represent the indexed sets that will be sent
c     and recieved.
c 
c
c.... the following call to MPI_TYPE_INDEXED will create a new data
c     type which will represent the blocks of data we wish to transfer
c     for this task. A handle to the new type is returned 
c     (sevsegtype(itask,1)). This data type describes the blocks of
c     data to be transferred in terms of segments.
c     Input to this routine:
c          numseg: number of segments in this task
c          lenseg: length of each segment (number of nodes)
c          ioffset: where to begin each block with respect to the
c                   first segment
c          MPI_DOUBLE_PRECISION: type to set for each of the blocks
c
        call MPI_TYPE_INDEXED (numseg, lenseg, ioffset,
     &                  MPI_DOUBLE_PRECISION, sevsegtype(itask,1), ierr)
c
c.... now create a new data type for each of the types of arrays we 
c     may wish to communicate with. For example ndof will be used when
c     communicating the residual vector. Each one of these is derived
c     from the first data type defined above, sevsegtype(itask,1).
c
        call MPI_TYPE_HVECTOR(nsd,    1, lstride, sevsegtype(itask,1),
     &                                        sevsegtype(itask,2), ierr)
c
        call MPI_TYPE_HVECTOR(ndof,   1, lstride, sevsegtype(itask,1),
     &                                        sevsegtype(itask,3), ierr)
c
        call MPI_TYPE_HVECTOR(nflow*nflow,1, lstride,
     &                    sevsegtype(itask,1),sevsegtype(itask,4), ierr)
        call MPI_TYPE_HVECTOR((nflow-1)*nsd,1,lstride,
     &                    sevsegtype(itask,1),sevsegtype(itask,5), ierr)
       call MPI_TYPE_HVECTOR(nflow,1,lstride,sevsegtype(itask,1),
     &                                        sevsegtype(itask,6), ierr)
       call MPI_TYPE_HVECTOR(24,1,lstride,sevsegtype(itask,1),
     &                                        sevsegtype(itask,7), ierr)
       call MPI_TYPE_HVECTOR(9,1,lstride,sevsegtype(itask,1),
     &                                        sevsegtype(itask,8), ierr)
       call MPI_TYPE_HVECTOR(11,1,lstride,sevsegtype(itask,1),
     &                                        sevsegtype(itask,9), ierr)
       call MPI_TYPE_HVECTOR(7,1,lstride,sevsegtype(itask,1),
     &                                   sevsegtype(itask,10), ierr)
       call MPI_TYPE_HVECTOR(33,1,lstride,sevsegtype(itask,1),
     &                                   sevsegtype(itask,11), ierr)
       call MPI_TYPE_HVECTOR(22,1,lstride,sevsegtype(itask,1),
     &                                   sevsegtype(itask,12), ierr)
       call MPI_TYPE_HVECTOR(16,1,lstride,sevsegtype(itask,1),
     &                                   sevsegtype(itask,13), ierr)
       call MPI_TYPE_HVECTOR(10,1,lstride,sevsegtype(itask,1),
     &                                   sevsegtype(itask,14), ierr)
       call MPI_TYPE_HVECTOR(nflow*nsd,1,lstride,sevsegtype(itask,1),
     &                                   sevsegtype(itask,15), ierr)
c
c
c.... now this must be done to make MPI recognize each of the data
c     types that were just defined
c
        do kdof = 1,15
          call MPI_TYPE_COMMIT (sevsegtype(itask,kdof), ierr)
        enddo
c
c.... set the counter to the index in ilwork where the next task
c     begins
c

        itkbeg = itkbeg + 4 + 2*numseg
c
c.... end loop over tasks
c
      enddo
      return
      end

      subroutine initDtN
      
      use dtnmod
      
      include "global.h"
      include "common_blocks/conpar.h"
      
      allocate (ifeature(nshg))
      end


      subroutine DtN(iBC,BC,y)

      use dtnmod
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/sclrs.h"

C     Local variables
C
      INTEGER             i,           itype,       j

      real*8 BC(nshg,ndofBC),y(nshg,ndof),tmp(nsclr)
      integer iBC(nshg)

      do i=1,nshg
         itype=ifeature(i)
         if(btest(iBC(i),13)) then
            do j=1,nsclr
               tmp(j)=y(i,5+j)
            end do
            call Dirichlet2Neumann(nsclr, itype, tmp)
c
c  put the value in the position a Dirichlet value would be in BC.
c  later we will localize this value to the BCB array.  
c  this is not dangerous because we should NEVER need to set Dirichlet
c  on the same node as a DtN condition
c
            do j=1,nsclr
               BC(i,6+j)=-tmp(j)
            end do
         endif
      end do
      return
      end

      subroutine dtnl(iBC,BC,ienb,iBCB,BCB)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"

C     Local variables
C
      INTEGER             i,           j,           k,           nstart
C
      integer ienb(npro,nshl), iBC(nshg),iBCB(npro,ndiBCB)
      real*8  BCB(npro,nshlb,ndBCB), tmpBCB(npro,nshlb,nsclr),
     &        BC(nshg,ndofBC),        tmpBC(nshg,nsclr)

      nstart=ndofBC-nsclr
c      tmpBC=zero
c      do i=1,nshg
c         if(btest(iBC(i),13)) then
            do j=1,nsclr
c               tmpBC(i,j)=BC(i,nstart+j)
               tmpBC(:,j)=BC(:,nstart+j)
            enddo
c         endif
c      enddo
      
      call localb(tmpBC,tmpBCB,ienb,nsclr,'gather  ')
      
      do i=1,npro
         do j=1,nsclr
            if(iBCB(i,2).lt.0) then  !this is a face with dtn
               do k=1,nshlb
                  BCB(i,k,6+j)=tmpBCB(i,k,j)
               enddo
            endif
         enddo
      enddo
      return
      end

c
c This routine just calls the appropriate version of D2N for the number 
c of scalars used
c
      subroutine Dirichlet2Neumann(nscalar, itype, tmp)
      integer nscalar, itype
      real*8 tmp(nscalar),foo
      
c Just short circuit the routine for a little bit.
c      tmp(1)=0.0d0
c      return
      if(nscalar .eq. 1) then
c         write(*,*) 'Entering D2N1'
c          foo= rand(0)
         call Dirichlet2Neumann_1(nscalar,itype,tmp)
c         write(*,*) 'Returning from D2N after DTN1'
c         return
      elseif(nscalar.eq.2) then
         call Dirichlet2Neumann_2(nscalar,itype,tmp)
      else
         write(*,*) 'FATAL ERROR: cannont handle ',nscalar,' scalars'
         stop
      endif
            
      return
      end

!> This is an interface routine, designed to call return a value for
!! the flux to a point on the wafer due to electrochemical deposition
!! to Ken Jansen's PHASTA given a boundary conditions and an index for
!! a particular feature.
!!
!! There is an inherent assumption that we are going to be doing
!! electroplating. This routine sets up the filenames and the 
!! top-of-the-domain boundary conditions.

      subroutine Dirichlet2Neumann_2(nscalar, itype, tmp)

      implicit none

      integer maxdata,maxtypes
      parameter(maxdata=100,maxtypes=5)

      integer itype, nscalar
      real*8 tmp(nscalar)
c For each table up to maxtypes, we have 4 pieces of data--two independent,
c two dependent--for each point, up to maxdata+1.
      real*8 table(4,0:maxdata,0:maxdata,maxtypes)
      save table

      integer i,j,n
      logical readfile(maxtypes)
      save readfile
      data (readfile(i),i=1,maxtypes) / maxtypes*.false./

      real*8  dx(2,maxtypes)
      integer numdata(2,maxtypes)
      save dx
      save numdata
      
      real*8  x,y, z(3,2)
c We can only deal with two parameter models for now.
      if(nscalar .ne. 2) then
         write(*,*) 'Sorry, Dirichlet2Neumann handles 2 scalars.'
         write(*,*) 'You asked for ', nscalar
         write(*,*) 'STOPPING...'
         stop
      endif

c If we haven't read in our parameters for this featuretype yet...

      if( .not. readfile(itype)) then
         readfile(itype) = .true.
         call readtable_2(itype,table,numdata,dx,
     &        maxdata,maxtypes)
      endif

      x = tmp(1)
      y = tmp(2)
      
      if(.false.) then
         if( x .gt. table(1,0,0,itype) .or. 
     &        x .lt. table(1,numdata(1,itype)-1,0,itype) ) then
            write(*,*) 'Sorry, concentration 1 asked for: ', x
            write(*,*) '  is out of the table bounds.'
            write(*,*)  '#1  [ ',table(1,0,0,itype), ' , ',
     &           table(1,numdata(1,itype)-1,0,itype), ' ] ',
     &           numdata(1,itype)-1
            
            write(*,*) '  STOPPING...'
            stop
         endif
         if( y .gt. table(2,0,0,itype) .or. 
     &        y .lt. table(2,0,numdata(2,itype)-1,itype) ) then
            write(*,*) 'Sorry, concentration 2 asked for: ', y
            write(*,*) '  is out of the table bounds.'
            write(*,*)  '#2   [ ',table(2,0,0,itype), ' , ',
     &           table(2,0,numdata(2,itype)-1,itype), ' ] ',
     &           numdata(2,itype)-1
            write(*,*) '  STOPPING...'
            stop
         endif
      endif

      i = int ( (x - table(1,0,0,itype) ) / dx(1,itype))
      j = int ( (y - table(2,0,0,itype) ) / dx(2,itype))
c      write(*,*) 'i,j,x,y: ',i,j,x,y
      if(i .lt. 0) then
         i = 0
c         x = table(1,0,0,itype)
c         write(*,*) 'Reseting i low: ',i,j,x,y
      endif
      if(j .lt. 0) then
         j = 0
         y = table(2,0,0,itype)
c         write(*,*) 'Reseting j low: ',i,j,x,y
      endif
      if(i .ge. numdata(1,itype)) then
         i = numdata(1,itype)-2
c         x = table(1,i+1,0,itype)
c         write(*,*) 'Reseting i high: ',i,j,x,y
      endif
      if(j .ge. numdata(2,itype)) then
         j = numdata(2,itype)-2
         y = table(1,0,j+1,itype)
c         write(*,*) 'Reseting j high: ',i,j,x,y
      endif

      do n=3,4
         
         z(1,1) = table(n,i,j,itype)
         z(3,1) = table(n,i+1,j,itype)
         z(1,2) = table(n,i,j+1,itype)
         z(3,2) = table(n,i+1,j+1,itype)

         z(2,1) = (z(3,1) - z(1,1)) / dx(1,itype)
     &        *(x-table(1,i,j,itype)) + z(1,1)
         z(2,2) = (z(3,2) - z(1,2)) / dx(1,itype)
     &        *(x-table(1,i,j,itype)) + z(1,2)
         tmp(n-2) = (z(2,2) - z(2,1))/dx(2,itype)
     &        *(y-table(2,i,j,itype)) + z(2,1)
      
      enddo
c      write(*,*) 'Interpolation from ',x,y,' to:', tmp(1),tmp(2)
      return

      end

!> Read a table of ordered quadruplets and place them into the slot in
!! TABLE that is assosciated with ISLOT. Store the number of 
!! data in NUMDATA and the spacing in DX.  The file to be read 
!! is 'TABLE.[islot]' The data but be in a rectangular regular grid.
!!
!! AUTHOR: Max Bloomfield, May 2000

      subroutine readtable_2(islot,table,numdata,dx,maxdata,maxslots)

      implicit none
c
      integer islot
      integer maxslots,numdata(2,maxslots), maxdata
c
      real*8 table(4,0:maxdata,0:maxdata,maxslots), dx(2,maxslots)
      real*8 x1,x2,y1,y2,x2old
c
      character*250 linein,filename
c
      integer i,j,k
      logical verbose

      verbose = .true.

      i=0
      j=0
      write(filename,1066) islot
 1066 format('TABLE.',i1)

      open(file=filename,unit=1066)

      write(*,*) 'Opening ', filename

 1    continue 
      read (unit=1066,fmt='(a)',end=999) linein

      if (linein(1:1).eq.'#') then
         write (*,'(a)') linein
         goto 1
      endif
c
      if (i.gt.maxdata**2+maxdata+1) then
         write(*,*) 
     &        'reached the maximum number of data points allowed'
         write(*,*) 'FATAL ERROR: stopping'
         stop
      endif
c
      read (linein,*) x1,x2,y1,y2
      if(i .gt. 0 .and. x2 .ne. x2old) then
c        increment the outer index in this nested loop. That is, go on
c        to the next "row" (not in fortran speak, but in table speak.)
         j = j + 1
         i=0
      endif
         
      table(1,i,j,islot) = x1*1.d-0
      table(2,i,j,islot) = x2*1.d-0
      table(3,i,j,islot) = y1*1.d-0
      table(4,i,j,islot) = y2*1.d-0
c      
      i=i+1
      x2old = x2

      goto 1
c
 999  continue
c
      numdata(1,islot) = I
      numdata(2,islot) = j+1
c      
      dx(1,islot) = table(1,2,1,islot) - table(1,1,1,islot)
      dx(2,islot) = table(2,1,2,islot) - table(2,1,1,islot)

      if(verbose) then
         write(*,*) 'Table is ',i,' by ',j+1
         
         write(*,*) 'there are ',i*(j+1),' flux data points'
         write(*,*) 'closing unit 1066'
         close(1066)
c     
         write(*,*) 'The abscissa are ',
     &        dx(1,islot),' and ',dx(2,islot),' apart.'
         
         write(*,*) 'the flux data are '
         do i=0,numdata(1,islot)-1
            do j=0,numdata(2,islot)-1
               write(*,*) i,j,(table(k,i,j,islot), k=1,4)
            end do
         end do
         
      endif
      return
      end

!> This is an interface routine, designed to call return a value for
!! the flux to a point on the wafer due to electrochemical deposition
!! to Ken Jansen's PHASTA given a boundary conditions and an index for
!! a particular feature.
!!
!! There is an inherent assumption that we are going to be doing
!! electroplating. This routine sets up the filenames and the 
!! top-of-the-domain boundary conditions.

      subroutine Dirichlet2Neumann_1(nscalar, itype, tmp)

      implicit none

      integer maxdata,maxtypes
      parameter(maxdata=200,maxtypes=2)

      integer itype, nscalar
      real*8 tmp(nscalar)
      real*8 table(0:maxdata,2,maxtypes)
      save table

      integer i
      logical readfile(maxtypes)
      save readfile
      data (readfile(i),i=1,maxtypes) / maxtypes*.false./

      real*8  dx(maxtypes)
      save dx
      integer numdata(maxtypes)
      save numdata
      
      real*8 dt, conc_BC, flux_BC
c We can only deal with one parameter models for now.

      if(nscalar .ne. 1) then
         write(*,*) 'Sorry, Dirichlet2Neumann can only handle 1 scalar.'
         write(*,*) 'You asked for ', nscalar
         write(*,*) 'STOPPING...'
         stop
      endif

c If we haven't read in our parameters for this featuretype yet...

      if( .not. readfile(itype)) then
         readfile(itype) = .true.
         call readtable_1(itype,table,numdata(itype),dx(itype),
     &        maxdata,maxtypes)
c         write(*,*) 'back from readtable'
         if(dx(itype) .eq. 0.0d0) then
            write(*,*) 'DX for table ',itype,' is zero (I think)'
            stop
         endif
      endif
c      write(*,*) 'returning from D2N'

      conc_BC = tmp(1)
      
c      if( conc_BC .lt. table(0,1,itype) .or. 
c     &    conc_BC .gt. table(numdata(itype),1,itype) ) then
c         write(*,*) 'Sorry, concentration asked for: ', conc_BC
c         write(*,*) '  is out of the table bounds.'
c         write(*,*) '[',table(0,1,itype),',
c     &        ',table(numdata(itype),1,itype),']'
c         write(*,*) '  STOPPING...'
c         stop
c      endif

      i = int ( (conc_BC - table(0,1,itype) ) / dx(itype))

      if( conc_BC .lt. table(0,1,itype))then
         i = 0
         conc_BC =  table(i,1,itype)
      elseif( conc_BC .gt. table(numdata(itype),1,itype)) then
         i = numdata(itype)
         conc_BC =  table(i,1,itype)
      endif
         

      dt = conc_BC - table(i,1,itype)
      flux_BC = dt * (table(i+1,2,itype) - table(i,2,itype)) +
     &     table(i,2,itype)


      tmp(1) = flux_BC
      

      end

!> Read a table of ordered pairs and place them into the slot in
!! TABLE that is assosciated with ISLOT. Store the number of 
!! data in NUMDATA and the spacing in DX.  The file to be read 
!! is 'TABLE.[islot]'
!!
!! AUTHOR: Max Bloomfield, May 2000

      subroutine readtable_1(islot,table,numdata,dx,maxdata,maxslots)
      implicit none
c
      integer islot
      integer numdata, maxdata, maxslots
c
      real*8 table(0:maxdata,2,maxslots),dx
c
      character*80 linein,filename
c
      integer i,j
      logical verbose
      verbose = .true.

      i=-1
      
      write(filename,1066) islot
 1066 format('TABLE.',i1)
      open(file=filename,unit=1066)
      if(verbose) write(*,*) 'Opening ', filename

 1    continue 
      read (unit=1066,fmt='(a)',end=999) linein

      if (linein(1:1).eq.'#') then
         write (*,'(a)') linein
         goto 1
      endif
c
      i=i+1
      if (i.ge.maxdata) then
         write(*,*) 
     &        'reached the maximum number of data points allowed'
         write(*,*) 'FATAL ERROR: stopping'
         stop
      endif
c
      read (linein,*) table(i,1,islot), table(i,2,islot)
      table(i,1,islot)= table(i,1,islot)*1.0d-0
      table(i,2,islot)= table(i,2,islot)*1.0d-0
c      
      goto 1
c
 999  continue
c
      numdata = i
      dx = table(1,1,islot)-table(0,1,islot)
c      
      if(verbose) then
         write(*,*) 'there are ',numdata,' flux data points'
         write(*,*) 'closing unit 1066'
         close(1066)
c     
         write(*,*) 'the flux data are '
         do 101 j=0,i
            write(*,*) j,table(j,1,islot), table(j,2,islot)
 101     continue
      endif
      return
      end

!> This routine calculates the residual and tangent matrix for the 
!! UBar formulation of the incompressible Navier Stokes equations.
!!
!! input:    e    a   1..5   when we think of U^e_a  and U is 5 variables <BR>
!! @param[in] yl(npro,nshl,ndof) Y variables (not U)
!! @param[in] acl(npro,nshl,ndof) Y acceleration (Y_{,t})
!! @param[in] shp(nen,ngauss) Element shape-functions  N_a
!! @param[in] shgl(nsd,nen,ngauss) Element local-shape-functions N_{a,xi}
!! @param[in] wght(ngauss) Element weight (for quadrature)
!! @param[in] xl(npro,nenl,nsd) Nodal coordinates at current step (x^e_a)
!! @param[in] ql(npro,nshl,nsd*nsd) Diffusive flux vector (don't worry)
!! @param[in] rlsl(npro,nshl,6) Resolved Leonard stresses
!!
!! output: <BR>
!! @param[out] rl(npro,nshl,nflow) Element RHS residual    (G^e_a)
!! @param[out] rml(npro,nshl,nflow) Element modified residual  (G^e_a tilde)
!! @param[out] xKebe(npro,9,nshl,nshl) Element LHS tangent mass matrix
!! @param[out] xGoC(npro,4,nshl,nshl) Element LHS tangent mass matrix
!!
!! Note: This routine will calculate the element matrices for the
!!        Hulbert's generalized alpha method integrator

        subroutine e3 (yl,      acl,     dwl,     shp,
     &                 shgl,    xl,      rl,      ql,
     &                 xKebe,   xGoC,    xmudmi,  sgn, 
     &                 rerrl, rlsl)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

C     Argument variables
C
      REAL*8                acl,         dwl,         ql,          rl
      REAL*8                rlsl,        sgn,         shgl,        shp
      REAL*8                xgoc,        xkebe,       xl,      xmudmi
      REAL*8                yl
C
C     Local variables
C
      INTEGER             iaa,         ib
C
      REAL*8                aci,         dxidx,       g1yi,        g2yi
      REAL*8                g3yi,        pres,        rho,         rlsli
      REAL*8                rlui,        rmu,         shdrv,       shg
      REAL*8                shpfun,      src,         taubar,      tauc
      REAL*8                taum,        u1,          u2,          u3
      REAL*8                ubar,        wdetj
C
c
        dimension yl(npro,nshl,ndof),
     &            acl(npro,nshl,ndof),       
     &            shp(nshl,ngauss),       shgl(nsd,nshl,ngauss),
     &            xl(npro,nenl,nsd),      dwl(npro,nenl),
     &            rl(npro,nshl,nflow),     ql(npro,nshl,idflx)
c      

      	dimension xKebe(npro,9,nshl,nshl), xGoC(npro,4,nshl,nshl)
c
c.... local declarations
c
        dimension g1yi(npro,ndof),        g2yi(npro,ndof),
     &            g3yi(npro,ndof),        shg(npro,nshl,nsd),
     &            aci(npro,3),            dxidx(npro,nsd,nsd),       
     &            WdetJ(npro),            rho(npro),
     &            pres(npro),             u1(npro),
     &            u2(npro),               u3(npro),
     &            rLui(npro,nsd),         uBar(npro,nsd),
     &            xmudmi(npro,ngauss),     sgn(npro,nshl), 
     &            shpfun(npro,nshl),      shdrv(npro,nsd,nshl),
     &            rmu(npro),              tauC(npro),
     &            tauM(npro),             tauBar(npro),
     &            src(npro,3)

        dimension rlsl(npro,nshl,6),      rlsli(npro,6)

        real*8    rerrl(npro,nshl,6)
        integer   aa

c
c     DES - TO REMOVE !!!     
c.... local reconstruction of diffusive flux vector for quadratics
c     or greater but NOT for bflux since local mass was not mapped
c
c        if ( idiff==2 .and. ires .eq. 1 ) then
c           call e3ql (yl,        dwl,       shp,       shgl, 
c     &                xl,        ql,        xmudmi, 
c     &                sgn)
c        endif
c
c.... loop through the integration points
c
        do intp = 1, ngauss

        if (Qwt(lcsyst,intp) .eq. zero) cycle          ! precaution
c
c.... get the hierarchic shape functions at this int point
c
        call getshp(shp,          shgl,      sgn, 
     &              shpfun,       shdrv)
c
c.... get necessary fluid properties (including eddy viscosity)
c
        call getdiff(dwl,  yl,     shpfun,     xmudmi, xl,   rmu, rho)
c
c.... calculate the integration variables
c
        call e3ivar (yl,          acl,       shpfun,
     &               shdrv,       xl,
     &               aci,         g1yi,      g2yi,    
     &               g3yi,        shg,       dxidx,   
     &               WdetJ,       rho,       pres, 
     &               u1,          u2,        u3,              
     &               ql,          rLui,      src,
     &               rerrl,       rlsl,      rlsli,
     &               dwl) 
c
c.... compute the stabilization terms
c
        call e3stab (rho,          u1,       u2,
     &               u3,           dxidx,    rLui,   
     &               rmu,          tauC,     tauM,   
     &               tauBar,       uBar )  
c
c.... compute the residual contribution at this integration point
c
        call e3Res ( u1,        u2,         u3,
     &               uBar,      aci,        WdetJ,
     &               g1yi,      g2yi,       g3yi,
     &               rLui,      rmu,        rho,
     &               tauC,      tauM,       tauBar,
     &               shpfun,    shg,        src,
     &               rl,        pres,       acl,
     &               rlsli)
c
c.... compute the tangent matrix contribution
c
        if (lhs .eq. 1) then
           call e3LHS ( u1,        u2,         u3,
     &                  uBar,      WdetJ,      rho,
     &                  rLui,      rmu,
     &                  tauC,      tauM,       tauBar,
     &                  shpfun,    shg,        xKebe,
     &                  xGoC )
        endif

c
c.... end of integration loop
c
      enddo

c
c.... symmetrize C
c
      if (lhs .eq. 1) then
         do ib = 1, nshl
            do iaa = 1, ib-1
               xGoC(:,4,iaa,ib) = xGoC(:,4,ib,iaa)
            enddo
         enddo
      endif
c
c.... return
c
      return
      end

!> This routine calculates the residual and tangent matrix for the 
!! advection - diffusion equation for scalar.

      subroutine e3Sclr (yl,      acl,     shp,
     &                     shgl,    xl,      dwl,
     &                     rl,      ql,      xSebe,   
     &                     sgn,     xmudmi)
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/solpar.h"
c
      real*8    yl(npro,nshl,ndof),     acl(npro,nshl,ndof),       
     &            shp(nshl,ngauss),       shgl(nsd,nshl,ngauss),
     &            xl(npro,nenl,nsd),      rl(npro,nshl),          
     &            ql(npro,nshl,nsd),      xSebe(npro,nshl,nshl),
     &            dwl(npro,nshl)
c
c.... local declarations
c
      real*8    gradS(npro,nsd),        shg(npro,nshl,nsd),
     &            Sdot(npro),             Sclr(npro),
     &            dxidx(npro,nsd,nsd),    WdetJ(npro),      
     &            u1(npro),     u2(npro), u3(npro),
     &            sgn(npro,nshl),         shpfun(npro,nshl),       
     &            shdrv(npro,nsd,nshl),   rLS(npro),
     &            tauS(npro),             diffus(npro),
     &            srcL(npro),             srcR(npro),
     &            gGradS(npro,nsd),       dcFct(npro),
     &            giju(npro,6)
c
c.... Source terms sometimes take the form (beta_i)*(phi,_i).  Since
c     the convective term has (u_i)*(phi,_i), it is useful to treat
c     beta_i as a "correction" to the velocity.  In calculating the
c     stabilization terms, the new "modified" velocity (u_i-beta_i) is 
c     then used in place of the pure velocity for stabilization terms,
c     and the source term sneaks into the RHS and LHS.
      real*8    uMod(npro,nsd), srcRat(npro), xmudmi(npro,ngauss)
c
      integer   aa, b
c     DES - TO REMOVE !!!     
c.... local reconstruction of diffusive flux vector
c
c        if ( idiff==2 ) then
c           call e3qlSclr (yl, dwl, shp, shgl, xl, ql, sgn)
c        endif
c
c.... loop through the integration points
c
        do intp = 1, ngauss

        if (Qwt(lcsyst,intp) .eq. zero) cycle          ! precaution
c
c.... get the hierarchic shape functions at this int point
c
        call getshp(shp,          shgl,      sgn, 
     &              shpfun,        shdrv)
c
c.... get necessary fluid properties
c
        call getdiffsclr(shpfun,dwl,yl,diffus)
c
c.... calculate the integration variables
c
        call e3ivarSclr(yl,          acl,       shpfun,
     &                  shdrv,       xl,        xmudmi,
     &                  Sclr,        Sdot,      gradS,
     &                  shg,         dxidx,     WdetJ,       
     &                  u1,          u2,        u3,              
     &                  ql,          rLS,       SrcR,
     &                  SrcL,        uMod,      dwl,
     &                  diffus,      srcRat)


c
c.... compute the stabilization terms
c
        call e3StabSclr (uMod,    dxidx,   tauS, 
     &                   diffus,  srcR,    giju,
     &                   srcRat)
c
c... computing the DC factor for the discontinuity capturing
c
        if (idcsclr(1) .ne. 0) then
           if ((idcsclr(2).eq.1 .and. isclr.eq.1) .or. 
     &          (idcsclr(2).eq.2 .and. isclr.eq.2)) then ! scalar with dc
c
              call e3dcSclr ( gradS,    giju,     gGradS,
     &                        rLS,      tauS,     srcR,
     &                        dcFct)
           endif
        endif                   !end of idcsclr
c
c.... compute the residual contribution at this integration point
c
        call e3ResSclr ( uMod,      gGradS,
     &                   Sclr,      Sdot,       gradS,  
     &                   WdetJ,     rLS,        tauS,
     &                   shpfun,    shg,        srcR,
     &                   diffus, 
     &                   rl )
c
c.... compute the tangent matrix contribution
c
        if (lhs .eq. 1) then
           call e3LHSSclr ( uMod,      giju,       dcFct,
     &                      Sclr,      Sdot,       gradS,  
     &                      WdetJ,     rLS,        tauS,
     &                      shpfun,    shg,        srcL,
     &                      diffus,
     &                      xSebe )

        endif

c
c.... end of integration loop
c
      enddo

c
c.... return
c
      return
      end

!> This routine calculates the 3D RHS residual of the fluid boundary 
!! elements.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Y variables
!! @param[in] iBCB(npro,ndiBCB) boundary condition code (iBCB(:,1) is
!!                              a bit tested boundary integral flag i.e.
!!                              if set to value of BCB
!!                              if set to floating value
!! @param[in] iBCB(:,1) 
!! - convective flux * 1  0  (ditto to all below)
!! - pressure   flux * 2
!! - viscous    flux * 4
!! - heat       flux * 8
!! - turbulence wall * 16 
!! - scalarI   flux  * 16*2^I (where I is the scalar number)
!!
!! @param[in] iBCB(:,2) is the srfID given by the user in MGI that we will
!!                      collect integrated fluxes for.
!!
!! @param[in] BCB(npro,nshlb,ndBCB) Boundary Condition values
!! - BCB (1) mass flux 
!! - BCB (2) pressure 
!! - BCB (3) viscous flux in x1-direc.
!! - BCB (4) viscous flux in x2-direc.
!! - BCB (5) : viscous flux in x3-direc.
!! - BCB (6) : heat flux
!!
!!  @param[in] shpb(nen,ngaussb) Boundary element shape-functions
!!  @param[in] shglb(nsd,nen,ngaussb) Boundary element grad-shape-functions
!!  @param[in] xlb(npro,nenl,nsd) Nodal coordinates at current step
!!
!! output:<BR>
!! $param[in] rl(npro,nshl,nflow) Element residual
!!
!! Note: Always the first side of the element is on the boundary.  
!!       However, note that for higher-order elements the nodes on 
!!       the boundary side are not the first nshlb nodes, see the 
!!       array mnodeb.


        subroutine e3b (ul,      yl,      acl,     iBCB,    BCB,     
     &                  shpb,    shglb,
     &                  xlb,     rl,      sgn,     dwl,     xKebe)
        use LagrangeMultipliers 
c
        include "global.h"        
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"
  
C     Argument variables
C
      INTEGER             ibcb
C
      REAL*8                acl,         bcb,         rl,          sgn
      REAL*8                shglb,       shpb,        ul,          xkebe
      REAL*8                xlb,         yl
C
C     Local variables
C
      INTEGER             i,           iel,         iface,       ii
      INTEGER             j,           k,           l,           lnode
      INTEGER             m,           n,           nodlcl,      nodlcl2
      INTEGER             nodlclM,nodlclN
C
      REAL*8                allu,        bnorm,      g1yi
      REAL*8                g2yi,        g3yi,        pres,        rho
      REAL*8                rkwall_glob, rlkwall,     rmu,         rna
      REAL*8                rou,         shape,       shdrv,       stabk
      REAL*8                tau1n,       tau2n,       tau3n,       temp
      REAL*8                u1,          u2,          u3,          unm
      REAL*8                vdot,        wdetjb

c
        dimension yl(npro,nshl,ndof),          iBCB(npro,ndiBCB),
     &            BCB(npro,nshlb,ndBCB),       shpb(nshl,ngaussb),
     &            shglb(nsd,nshl,ngaussb),           
     &            xlb(npro,nenl,nsd),          ul(npro,nshl,nsd),
     &            acl(npro,nshl,ndof),
     &            rl(npro,nshl,nflow)
c
        dimension g1yi(npro,ndof),             g2yi(npro,ndof),
     &            g3yi(npro,ndof),             WdetJb(npro),
     &            bnorm(npro,nsd)
c
        dimension u1(npro),                    u2(npro),
     &            u3(npro),                    rho(npro),
     &            unm(npro),                   pres(npro),
     &            vdot(npro,nsd),              rlKwall(npro,nshlb,nsd)
c
        dimension rou(npro),                   rmu(npro),
     &            temp(npro),                  allU(nsd)
c
        dimension tau1n(npro),
     &            tau2n(npro),                 tau3n(npro)
c
        dimension lnode(27),               sgn(npro,nshl),
     &            shape(npro,nshl),        shdrv(npro,nsd,nshl),
     &            rNa(npro,4)

        real*8    xmudmi(npro,ngauss),      dwl(npro,nshl)
c
      dimension xKebe(npro,9,nshl,nshl),  rKwall_glob(npro,9,nshl,nshl),
     &   stabK(npro,9,nshl,nshl)

      integer   num

c
c.... compute the nodes which lie on the boundary (hierarchic)
c
        call getbnodes(lnode)
c
c.... loop through the integration points
c
        if(lcsyst.eq.3.or.lcsyst.eq.4) then
           ngaussb = nintb(lcsyst)
        else
           ngaussb = nintb(lcsyst)
        endif
        
        stabK = zero

        do intp = 1, ngaussb
c
c.... get the hierarchic shape functions at this int point
c
c        call getshp(shpb,        shglb,        sgn, 
c     &              shape,       shdrv)

c       SHOULD CALL THE ROUTINE WITH BOUNDARY SHAPE FUNCTIONS
c       OTHERWISE: BOUND CHECK ERROR - DES - 30JAN2014
        call getshpb(shpb,        shglb,        sgn, 
     &               shape,       shdrv)

c
c     NOTE I DID NOT PASS THE lnode down.  It is not needed
c     since the shape functions are zero on the boundary
c
c     Note that xmudmi is not calculated at these quadrature 
c     points so you give it a zero.  This has implications.
c     the traction calculated by this approach will include 
c     molecular stresses ONLY.  This is why we will use the 
c     consistent flux method to obtain the forces when doing
c     effective viscosity wall modeling.  When doing slip velocity
c     this is not a problem since the traction is given from the
c     log law relation (not the viscosity).
c
        xmudmi=zero
c
c.... get necessary fluid properties (including eddy viscosity)
c
        call getdiff(dwl, yl, shape, xmudmi, xlb, rmu, rho)
c
c.... calculate the integraton variables
c
        call e3bvar (yl,              acl,             ul,              
     &               shape,
     &               shdrv,           xlb,
     &               lnode,           WdetJb,
     &               bnorm,           pres,            
     &               u1,              u2,              u3,
     &               rmu,             unm,
     &               tau1n,           tau2n,           tau3n,
     &               vdot,            rlKwall,         
     &               xKebe,           rKwall_glob)
        
c        
c.... -----------------> boundary conditions <-------------------
c


        do iel = 1, npro
c
c  if we have a nonzero value then
c  calculate the fluxes through this surface 
c
           iface = abs(iBCB(iel,2))
           if (iface .ne. 0 .and. ires.ne.2) then
              flxID(1,iface) =  flxID(1,iface) + WdetJb(iel)! measure area too
              flxID(2,iface) =  flxID(2,iface) - WdetJb(iel) * unm(iel)
              flxID(3,iface) = flxID(3,iface)
     &                   - ( tau1n(iel) - bnorm(iel,1)*pres(iel))
     &                   * WdetJb(iel) 
              flxID(4,iface) = flxID(4,iface)
     &                   - ( tau2n(iel) - bnorm(iel,2)*pres(iel))
     &                   * WdetJb(iel) 
              flxID(5,iface) = flxID(5,iface)
     &                   - ( tau3n(iel) - bnorm(iel,3)*pres(iel))
     &                   * WdetJb(iel) 

           endif
c
c
c.... mass flux
c

           if (btest(iBCB(iel,1),0)) then
              unm(iel)  = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 unm(iel) = unm(iel) 
     &                    + shape(iel,nodlcl) * BCB(iel,n,1)
              enddo
           endif
c
c.... pressure
c
          if (btest(iBCB(iel,1),1)) then
              pres(iel) = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 pres(iel) = pres(iel) 
     &                     + shape(iel,nodlcl) * BCB(iel,n,2)
              enddo
           endif
c
c.... viscous flux
c        
           if (btest(iBCB(iel,1),2)) then
              tau1n(iel) = zero
              tau2n(iel) = zero
              tau3n(iel) = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 tau1n(iel) = tau1n(iel) 
     &                      + shape(iel,nodlcl)*BCB(iel,n,3)
                 tau2n(iel) = tau2n(iel) 
     &                      + shape(iel,nodlcl)*BCB(iel,n,4)
                 tau3n(iel) = tau3n(iel) 
     &                      + shape(iel,nodlcl)*BCB(iel,n,5)
              enddo
           endif
c
c.... turbulence wall (as a way of checking for deformable wall stiffness)
c
           if (btest(iBCB(iel,1),4)) then
              rlKwall(iel,:,:) = rlKwall(iel,:,:) / ngaussb ! divide by number of gauss points 
              pres(iel) = zero                              ! to avoid the gauss point loop
              tau1n(iel) = zero                             ! and make the traction contribution
              tau2n(iel) = zero                             ! zero
              tau3n(iel) = zero                              
           else
              rlKwall(iel,:,:) = zero                       ! this is not a deformable element
              vdot(iel,:) = zero
              xKebe(iel,:,:,:) = zero
              rKwall_glob(iel,:,:,:) = zero                 ! no stiffness: not a wall element
           endif
c
c..... to calculate inner products for Lagrange Multipliers
c
           if(Lagrange.gt.zero) then
              do k=1, numLagrangeSrfs
                 if (iBCB(iel,2).eq.nsrflistLagrange(k)) then
                    do n = 1, nshlb
                       nodlcl = lnode(n)
                       do m=1, nsd
                          do i=1, nshlb
                             nodlcl2 = lnode(i)
                             do l=1, nsd
                                do j=1, 3
                                   num=(m-1)*nsd+l
                                   loclhsLag(iel,num,n,i,j)=
     &                                loclhsLag(iel,num,n,i,j)+
     &                                shape(iel,nodlcl)*WdetJb(iel)*
     &                                shape(iel,nodlcl2)*
     &                                LagInplaneVectors(m,j,k)*
     &                                LagInplaneVectors(l,j,k)
                                enddo 
                             enddo
                          enddo
                       enddo
                    enddo
                 endif
              enddo
           endif  ! end of if(Lagrange.gt.zero)
c
        enddo                                               ! end of bc loop
c
c$$$c.... if we are computing the bdry for the consistent
c$$$c     boundary forces, we must not include the surface elements
c$$$c     in the computataion (could be done MUCH more efficiently!)--->
                                                                  !this
                                                                  !comment should read as for the consistent flux calculation rather than boundary forces
c$$$c
        if (ires .eq. 2) then
           do iel = 1, npro 
              if (nsrflist(iBCB(iel,2)) .ne. 0) then
                 unm(iel) = zero
                 tau1n(iel) = zero
                 tau2n(iel) = zero
                 tau3n(iel) = zero
c                 pres(iel) = zero
c
c whatever is zeroed here will beome part of the post-processed surface
c                 "traction force"
c
c uncomment the next two lines to get all of the t vector coming from
c                 Alberto's wall motion model.
                 vdot(iel,:)=zero
                 rlKwall(iel,:,:)=zero

c
c uncomment the next 8 lines to get only the tangential part
c

c                  vn=dot_product(vdot(iel,:),bnorm(iel,:))
c                  vdot(iel,:)=vn*bnorm(iel,:)
c                  walln1=dot_product(rlkwall(iel,1,:),bnorm(iel,:))
c                  walln2=dot_product(rlkwall(iel,2,:),bnorm(iel,:))
c                  walln3=dot_product(rlkwall(iel,3,:),bnorm(iel,:))
c                  rlkwall(iel,1,:)=walln1*bnorm(iel,:)
c                  rlkwall(iel,2,:)=walln2*bnorm(iel,:)
c                  rlkwall(iel,3,:)=walln3*bnorm(iel,:)
              endif
           enddo
        endif
c
c.... assemble the contributions
c
        rNa(:,1) = -WdetJb * ( tau1n - bnorm(:,1) * pres - vdot(:,1))
        rNa(:,2) = -WdetJb * ( tau2n - bnorm(:,2) * pres - vdot(:,2))
        rNa(:,3) = -WdetJb * ( tau3n - bnorm(:,3) * pres - vdot(:,3))
        rNa(:,4) =  WdetJb * unm
c
c.... THIS IS DONE FOR ADDING STABILITY IN THE CASE OF BACK FLOW 
c
        rou      = backFlowStabCoef*5D-1*rho*WdetJb*(ABS(unm) - unm)
        rNa(:,1) = rNa(:,1) + rou*u1 
        rNa(:,2) = rNa(:,2) + rou*u2
        rNa(:,3) = rNa(:,3) + rou*u3

c       BUG FIX - DES - 30JAN2014
        DO n = 1, nshlb
           nodlclN = lnode(n)
           DO m = 1, nshlb
              nodlclM = lnode(m)
              temp = alfi*gami*Delt(1)*rou*shape(:,nodlclN)*
     2            shape(:,nodlclM)
              stabK(:,1,n,m) = stabK(:,1,n,m) + temp
              stabK(:,5,n,m) = stabK(:,5,n,m) + temp
              stabK(:,9,n,m) = stabK(:,9,n,m) + temp
           END DO
        END DO

c        SMALL CONTRIBUTION ACCORDING TO MAHDI - DES 30JAN2014
c        DO iel=1, npro
c           IF (rou(iel) .GT. 0D0) THEN
c              allU(1) = u1(iel)
c              allU(2) = u2(iel)
c              allU(3) = u3(iel)
c              DO n=1, nshlb
c                 nodlclN = lnode(n)
c                 DO m=1, nshlb
c                    nodlclM = lnode(m)
c                    DO i=1,nsd
c                       DO j=1,nsd
c                          ii = j + (i - 1)*nsd
c                          stabK(iel,ii,n,m) = stabK(iel,ii,n,m) -
c     2                       rho(iel)*WdetJb(iel)*alfi*gami*Delt(1)*
c     3                       shape(iel,nodlclN)*shape(iel,nodlclM)*allU(i)*
c     4                       bnorm(iel,j)
c                       END DO
c                    END DO
c                 END DO
c              END DO
c           END IF
c        END DO

        if(iconvflow.eq.1) then     ! conservative form was integrated
                                    ! by parts and has a convective 
                                    ! boundary integral
c
c.... assemble the contributions
c
           rou=rho*unm
           rNa(:,1) = rNa(:,1) + WdetJb * rou * u1 
           rNa(:,2) = rNa(:,2) + WdetJb * rou * u2
           rNa(:,3) = rNa(:,3) + WdetJb * rou * u3
        endif
c
c.... ------------------------->  Residual  <--------------------------
c
c.... add the flux to the residual
c
        do n = 1, nshlb
           nodlcl = lnode(n)

           rl(:,nodlcl,1) = rl(:,nodlcl,1) - shape(:,nodlcl) * rNa(:,1)
           rl(:,nodlcl,2) = rl(:,nodlcl,2) - shape(:,nodlcl) * rNa(:,2)
           rl(:,nodlcl,3) = rl(:,nodlcl,3) - shape(:,nodlcl) * rNa(:,3)
           rl(:,nodlcl,4) = rl(:,nodlcl,4) - shape(:,nodlcl) * rNa(:,4)

        enddo
        if(ideformwall.eq.1) then
           rl(:,1,1) = rl(:,1,1) - rlKwall(:,1,1)
           rl(:,1,2) = rl(:,1,2) - rlKwall(:,1,2)
           rl(:,1,3) = rl(:,1,3) - rlKwall(:,1,3)
           
           rl(:,2,1) = rl(:,2,1) - rlKwall(:,2,1)
           rl(:,2,2) = rl(:,2,2) - rlKwall(:,2,2)
           rl(:,2,3) = rl(:,2,3) - rlKwall(:,2,3)
        
           rl(:,3,1) = rl(:,3,1) - rlKwall(:,3,1)
           rl(:,3,2) = rl(:,3,2) - rlKwall(:,3,2)
           rl(:,3,3) = rl(:,3,3) - rlKwall(:,3,3)
        endif 

        enddo
        if(ideformwall.eq.1) then
c     
c.... -----> Wall Stiffness and Mass matrices for implicit LHS  <-----------
c     
c.... Now we simply have to add the stiffness contribution in rKwall_glob to 
c.... the mass contribution already contained in xKebe

c.... this line is going to destroy the mass matrix contribution


c      xKebe = zero

         xKebe(:,:,:,:) = ( xKebe(:,:,:,:)*iwallmassfactor
     &           + rKwall_glob(:,:,:,:)*iwallstiffactor )


         endif
c$$$        ttim(40) = ttim(40) + tmr()
c
c.... return
c

        xKebe = xKebe + stabK 

        return
        end


c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c*********************************************************************
c*********************************************************************


        subroutine e3bSclr (yl,      iBCB,    BCB,     shpb,    shglb,
     &                      xlb,     rl,      sgn,     dwl)

        include "global.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"

C     Argument variables
C
      INTEGER             ibcb
C
      REAL*8                bcb,         rl,          sgn,         shglb
      REAL*8                shpb,        xlb,         yl
C
C     Local variables
C
      INTEGER             ib,          ibb,         iel,         iface
      INTEGER             iwalljump,   lnode,       n,           nodlcl
C
      REAL*8                flux,        rna,         shape,       shdrv
C
c
        dimension yl(npro,nshl,ndof),          iBCB(npro,ndiBCB),
     &            BCB(npro,nshlb,ndBCB),       shpb(nshl,*),
     &            shglb(nsd,nshl,*),           
     &            xlb(npro,nenl,nsd),          
     &            rl(npro,nshl)
c
        real*8    WdetJb(npro),                bnorm(npro,nsd)
c
        dimension lnode(27),                   sgn(npro,nshl),
     &            shape(npro,nshl),            shdrv(npro,nsd,nshl),
     &            rNa(npro),                   flux(npro)
        real*8    dwl(npro,nshl)

c
c.... compute the nodes which lie on the boundary (hierarchic)
c
        call getbnodes(lnode)
c
c.... loop through the integration points
c
        if(lcsyst.eq.3.or.lcsyst.eq.4) then
           ngaussb = nintb(lcsyst)
        else
           ngaussb = nintb(lcsyst)
        endif
        do intp = 1, ngaussb
c
c.... get the hierarchic shape functions at this int point
c
c       BUG FIX - DES - 30JAN2014
        call getshpb(shpb,        shglb,        sgn, 
     &              shape,       shdrv)
c
c.... calculate the integraton variables
c
        call e3bvarSclr (yl,          shdrv,   xlb,
     &                   shape,       WdetJb,  bnorm,
     &                   flux,        dwl )
c        
c.... -----------------> boundary conditions <-------------------
c

c
c.... heat or scalar  flux
c     
        if(isclr.eq.0) then 
           iwalljump=0
        else
           iwalljump=1  !turb wall between heat and scalar flux..jump over
        endif
        ib=4+isclr+iwalljump
        ibb=6+isclr
        do iel=1, npro
c
c  if we have a nonzero value then
c  calculate the fluxes through this surface 
c
           if (iBCB(iel,2) .ne. 0 .and. ires.ne.2) then
              iface = abs(iBCB(iel,2))
              flxID(ibb,iface) =  flxID(ibb,iface) 
     &                          - WdetJb(iel) * flux(iel)
           endif

           if (btest(iBCB(iel,1),ib-1)) then
              flux(iel) = zero
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 flux(iel) = flux(iel) 
     &                     + shape(iel,nodlcl) * BCB(iel,n,ibb)
              enddo           
           endif
        enddo
c
c.... assemble the contributions
c
        rNa(:) = -WdetJb * flux
c
c.... ------------------------->  Residual  <--------------------------
c
c.... add the flux to the residual
c
        do n = 1, nshlb
           nodlcl = lnode(n)
 
           rl(:,nodlcl) = rl(:,nodlcl) - shape(:,nodlcl) * rNa(:)
        enddo
c
c.... -------------------->  Aerodynamic Forces  <---------------------
c
        if ((ires .ne. 2) .and. (iter .eq. nitr)) then
c
c.... compute the forces on the body
c
           if(isclr.eq.0)   HFlux    = sum(flux)
c
        endif
c
c.... end of integration loop
c
        enddo

c
c.... return
c
        return
        end

!> This routine computes the variables at integration points for 
!! the boundary element routine.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Primitive variables (local), ndof: 5[p,v1,v2,v3,T]+number of scalars solved 
!! @param[in] acl(npro,nshl,ndof) Acceleration (local)
!! @param[in] ul(npro,nshlb,nsd) Displacement (local)
!! @param[in] shpb(nen) Boundary element shape-functions
!! @param[in] shglb(nsd,nen) Boundary element grad-shape-functions
!! @param[in] xlb(npro,nenl,nsd) Nodal coordinates at current step
!! @param[in] lnode(nenb) Local nodes on the boundary
!!
!! output:<BR>
!! @param[out] g1yi(npro,ndof) grad-v in direction 1
!! @param[out] g2yi(npro,ndof) grad-v in direction 2
!! @param[out] g3yi(npro,ndof) grad-v in direction 3
!! @param[out] WdetJb(npro) weighted Jacobian
!! @param[out] bnorm(npro,nsd) outward normal
!! @param[out] pres(npro) pressure
!! @param[out] u1(npro) x1-velocity component
!! @param[out] u2(npro) x2-velocity component
!! @param[out] u3(npro) x3-velocity component
!! @param[out] unm(npro) BC u dot n
!! @param[out] p(npro) BC pressure
!! @param[out] tau1n(npro) BC viscous flux 1
!! @param[out] tau2n(npro) BC viscous flux 2
!! @param[out] tau3n(npro) BC viscous flux 3
!! @param[out] vdot(npro,nsd) Acceleration at quadrature points
!! @param[out] rlKwall(npro,nshlb,nsd) Wall stiffness contribution to the local residual 

      subroutine e3bvar (yl,      acl,     ul,
     &                   shpb,    shglb,
     &                   xlb,     lnode,  
     &                   WdetJb,  bnorm,   pres,    
     &                   u1,      u2,      u3,      rmu,  
     &                   unm,     tau1n,   tau2n,   tau3n,
     &                   vdot,    rlKwall,         
     &                   xKebe,   rKwall_glob)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"

C     Argument variables
C
      INTEGER             lnode
C
      REAL*8                acl,         bnorm,       pres
      REAL*8                rkwall_glob, rlkwall,     rmu,         shglb
      REAL*8                shpb,        tau1n,       tau2n,       tau3n
      REAL*8                u1,          u2,          u3,          ul
      REAL*8                unm,         vdot,        wdetjb,      xkebe
      REAL*8                xlb,         yl
C
C     Local variables
C
      INTEGER             ipt2,        ipt3,        k,           n
      INTEGER             nodlcl
C
      REAL*8                b1,          b2,          b3
      REAL*8                detjacrot,   dmatrix,     dtimesb1
      REAL*8                dtimesb2,    dtimesb3,    dxdxib,    dxidxb
      REAL*8                g1yi,        g2yi,        g3yi,      gl1yi
      REAL*8                gl2yi,       gl3yi,       rkwall_glob11
      REAL*8                rkwall_glob12,            rkwall_glob13
      REAL*8                rkwall_glob21,            rkwall_glob22
      REAL*8                rkwall_glob23,            rkwall_glob31
      REAL*8                rkwall_glob32,            rkwall_glob33
      REAL*8                rkwall_local11,           rkwall_local12
      REAL*8                rkwall_local13,           rkwall_local21
      REAL*8                rkwall_local22,           rkwall_local23
      REAL*8                rkwall_local31,           rkwall_local32
      REAL*8                rkwall_local33,           rotnodallocal
      REAL*8                temp,        temp1,       temp2,      temp3
      REAL*8                tmp1,       v1,          v2
      REAL*8                v3,          x1rot,       x2rot,      x3rot
C
c
      dimension yl(npro,nshl,ndof),        rmu(npro),
     &            shpb(npro,nshl),           shglb(npro,nsd,nshl),
     &            xlb(npro,nenl,nsd),        
     &            lnode(27),                 g1yi(npro,ndof),
     &            g2yi(npro,ndof),           g3yi(npro,ndof),
     &            WdetJb(npro),              bnorm(npro,nsd),
     &            pres(npro),                
     &            u1(npro),                  u2(npro),
     &            u3(npro),
     &            unm(npro),                 
     &            tau1n(npro),               tau2n(npro),
     &            tau3n(npro),
     &            acl(npro,nshl,ndof),       ul(npro,nshl,nsd),
     &            vdot(npro,nsd),            rlKwall(npro,nshlb,nsd)
c
      dimension gl1yi(npro,ndof),          gl2yi(npro,ndof),
     &            gl3yi(npro,ndof),          dxdxib(npro,nsd,nsd),
     &            dxidxb(npro,nsd,nsd),      temp(npro),
     &            temp1(npro),               temp2(npro),
     &            temp3(npro),
     &            v1(npro,nsd),              v2(npro,nsd),
     &            v3(npro,nsd),              
     &            rotnodallocal(npro,nsd,nsd),
     &            x1rot(npro,nsd),           x2rot(npro,nsd),
     &            x3rot(npro,nsd),           detJacrot(npro),
     &            B1(npro,5,3),              B2(npro,5,3),
     &            B3(npro,5,3),              Dmatrix(npro,5,5),
     &            DtimesB1(npro,5,3),        DtimesB2(npro,5,3),
     &            DtimesB3(npro,5,3),
     &            rKwall_local11(npro,nsd,nsd),
     &            rKwall_local12(npro,nsd,nsd),
     &            rKwall_local13(npro,nsd,nsd),
     &            rKwall_local21(npro,nsd,nsd),
     &            rKwall_local22(npro,nsd,nsd),
     &            rKwall_local23(npro,nsd,nsd),
     &            rKwall_local31(npro,nsd,nsd),
     &            rKwall_local32(npro,nsd,nsd),
     &            rKwall_local33(npro,nsd,nsd),
     &            rKwall_glob11(npro,nsd,nsd),
     &            rKwall_glob12(npro,nsd,nsd),
     &            rKwall_glob13(npro,nsd,nsd),
     &            rKwall_glob21(npro,nsd,nsd),
     &            rKwall_glob22(npro,nsd,nsd),
     &            rKwall_glob23(npro,nsd,nsd),
     &            rKwall_glob31(npro,nsd,nsd),
     &            rKwall_glob32(npro,nsd,nsd),
     &            rKwall_glob33(npro,nsd,nsd)
c     
      dimension   rKwall_glob(npro,9,nshl,nshl),
     &            xKebe(npro,9,nshl,nshl)
c     
      real*8      lhmFctvw, tsFctvw(npro)

      dimension   tmp1(npro)       
c     
      real*8    Turb(npro),                xki,
     &            xki3,                      fv1
c        
      integer   e, i, j
c      
      integer   aa, b


c
c.... ------------------->  integration variables  <--------------------
c
c.... compute the primitive variables at the integration point
c
      pres = zero
      u1   = zero
      u2   = zero
      u3   = zero
c             
      do n = 1, nshlb
         nodlcl = lnode(n)
c     
         pres = pres + shpb(:,nodlcl) * yl(:,nodlcl,1)
         u1   = u1   + shpb(:,nodlcl) * yl(:,nodlcl,2)
         u2   = u2   + shpb(:,nodlcl) * yl(:,nodlcl,3)
         u3   = u3   + shpb(:,nodlcl) * yl(:,nodlcl,4)

      enddo
c
c.... ---------------------->  Element Metrics  <-----------------------
c
c.... compute the deformation gradient
c
      dxdxib = zero
c
      do n = 1, nenl
         dxdxib(:,1,1) = dxdxib(:,1,1) + xlb(:,n,1) * shglb(:,1,n)
         dxdxib(:,1,2) = dxdxib(:,1,2) + xlb(:,n,1) * shglb(:,2,n)
         dxdxib(:,1,3) = dxdxib(:,1,3) + xlb(:,n,1) * shglb(:,3,n)
         dxdxib(:,2,1) = dxdxib(:,2,1) + xlb(:,n,2) * shglb(:,1,n)
         dxdxib(:,2,2) = dxdxib(:,2,2) + xlb(:,n,2) * shglb(:,2,n)
         dxdxib(:,2,3) = dxdxib(:,2,3) + xlb(:,n,2) * shglb(:,3,n)
         dxdxib(:,3,1) = dxdxib(:,3,1) + xlb(:,n,3) * shglb(:,1,n)
         dxdxib(:,3,2) = dxdxib(:,3,2) + xlb(:,n,3) * shglb(:,2,n)
         dxdxib(:,3,3) = dxdxib(:,3,3) + xlb(:,n,3) * shglb(:,3,n)
      enddo
c
c.... compute the normal to the boundary. This is achieved by taking
c     the cross product of two vectors in the plane of the 2-d 
c     boundary face.
c
      if(lcsyst.eq.1) then      ! set to curl into element all others out
         ipt2=2
         ipt3=3
      elseif(lcsyst.eq.2) then
         ipt2=4
         ipt3=2
      elseif(lcsyst.eq.3) then
         ipt2=3
         ipt3=2
      elseif(lcsyst.eq.4) then
         ipt2=2
         ipt3=4
      elseif(lcsyst.eq.5) then
         ipt2=4
         ipt3=2
      elseif(lcsyst.eq.6) then
         ipt2=2
         ipt3=5
      endif
      v1 = xlb(:,ipt2,:) - xlb(:,1,:)
      v2 = xlb(:,ipt3,:) - xlb(:,1,:)
c
c compute cross product
c
      temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
      temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
      temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)
c     
c mag is area for quads, twice area for tris
c 
      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      bnorm(:,1) = temp1 * temp
      bnorm(:,2) = temp2 * temp
      bnorm(:,3) = temp3 * temp
c        
      if (lcsyst .eq. 1) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 2) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 3) then
         WdetJb     = Qwtb(lcsyst,intp) / (two*temp)
      elseif (lcsyst .eq. 4) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 5) then
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
      elseif (lcsyst .eq. 6) then
         WdetJb     = Qwtb(lcsyst,intp) / (two*temp)
      endif
c
c.... -------------------------->  Grad-V  <----------------------------
c
c.... compute grad-v for Navier-Stokes terms
c
      if (Navier .eq. 1) then
c
c.... compute the inverse of deformation gradient
c
         dxidxb(:,1,1) =   dxdxib(:,2,2) * dxdxib(:,3,3) 
     &        - dxdxib(:,3,2) * dxdxib(:,2,3)
         dxidxb(:,1,2) =   dxdxib(:,3,2) * dxdxib(:,1,3) 
     &        - dxdxib(:,1,2) * dxdxib(:,3,3)
         dxidxb(:,1,3) =   dxdxib(:,1,2) * dxdxib(:,2,3) 
     &        - dxdxib(:,1,3) * dxdxib(:,2,2)
         temp          = one / ( dxidxb(:,1,1) * dxdxib(:,1,1) 
     &        + dxidxb(:,1,2) * dxdxib(:,2,1)  
     &        + dxidxb(:,1,3) * dxdxib(:,3,1) )
         dxidxb(:,1,1) =  dxidxb(:,1,1) * temp
         dxidxb(:,1,2) =  dxidxb(:,1,2) * temp
         dxidxb(:,1,3) =  dxidxb(:,1,3) * temp
         dxidxb(:,2,1) = (dxdxib(:,2,3) * dxdxib(:,3,1) 
     &        - dxdxib(:,2,1) * dxdxib(:,3,3)) * temp
         dxidxb(:,2,2) = (dxdxib(:,1,1) * dxdxib(:,3,3) 
     &        - dxdxib(:,3,1) * dxdxib(:,1,3)) * temp
         dxidxb(:,2,3) = (dxdxib(:,2,1) * dxdxib(:,1,3) 
     &        - dxdxib(:,1,1) * dxdxib(:,2,3)) * temp
         dxidxb(:,3,1) = (dxdxib(:,2,1) * dxdxib(:,3,2) 
     &        - dxdxib(:,2,2) * dxdxib(:,3,1)) * temp
         dxidxb(:,3,2) = (dxdxib(:,3,1) * dxdxib(:,1,2) 
     &        - dxdxib(:,1,1) * dxdxib(:,3,2)) * temp
         dxidxb(:,3,3) = (dxdxib(:,1,1) * dxdxib(:,2,2) 
     &        - dxdxib(:,1,2) * dxdxib(:,2,1)) * temp
c
c.... compute local-grad-Y
c
         gl1yi = zero
         gl2yi = zero
         gl3yi = zero
c     
         do n = 1, nshl
            gl1yi(:,1) = gl1yi(:,1) + shglb(:,1,n) * yl(:,n,1)
            gl1yi(:,2) = gl1yi(:,2) + shglb(:,1,n) * yl(:,n,2)
            gl1yi(:,3) = gl1yi(:,3) + shglb(:,1,n) * yl(:,n,3)
            gl1yi(:,4) = gl1yi(:,4) + shglb(:,1,n) * yl(:,n,4)
c     
            gl2yi(:,1) = gl2yi(:,1) + shglb(:,2,n) * yl(:,n,1)
            gl2yi(:,2) = gl2yi(:,2) + shglb(:,2,n) * yl(:,n,2)
            gl2yi(:,3) = gl2yi(:,3) + shglb(:,2,n) * yl(:,n,3)
            gl2yi(:,4) = gl2yi(:,4) + shglb(:,2,n) * yl(:,n,4)
c     
            gl3yi(:,1) = gl3yi(:,1) + shglb(:,3,n) * yl(:,n,1)
            gl3yi(:,2) = gl3yi(:,2) + shglb(:,3,n) * yl(:,n,2)
            gl3yi(:,3) = gl3yi(:,3) + shglb(:,3,n) * yl(:,n,3)
            gl3yi(:,4) = gl3yi(:,4) + shglb(:,3,n) * yl(:,n,4)
         enddo
c     
c.... convert local-grads to global-grads
c     
         g1yi(:,2) = dxidxb(:,1,1) * gl1yi(:,2) + 
     &        dxidxb(:,2,1) * gl2yi(:,2) +
     &        dxidxb(:,3,1) * gl3yi(:,2)
         g2yi(:,2) = dxidxb(:,1,2) * gl1yi(:,2) + 
     &        dxidxb(:,2,2) * gl2yi(:,2) +
     &        dxidxb(:,3,2) * gl3yi(:,2)
         g3yi(:,2) = dxidxb(:,1,3) * gl1yi(:,2) + 
     &        dxidxb(:,2,3) * gl2yi(:,2) +
     &        dxidxb(:,3,3) * gl3yi(:,2)
c     
         g1yi(:,3) = dxidxb(:,1,1) * gl1yi(:,3) + 
     &        dxidxb(:,2,1) * gl2yi(:,3) +
     &        dxidxb(:,3,1) * gl3yi(:,3)
         g2yi(:,3) = dxidxb(:,1,2) * gl1yi(:,3) + 
     &        dxidxb(:,2,2) * gl2yi(:,3) +
     &        dxidxb(:,3,2) * gl3yi(:,3)
         g3yi(:,3) = dxidxb(:,1,3) * gl1yi(:,3) + 
     &        dxidxb(:,2,3) * gl2yi(:,3) +
     &        dxidxb(:,3,3) * gl3yi(:,3)
c     
         g1yi(:,4) = dxidxb(:,1,1) * gl1yi(:,4) + 
     &        dxidxb(:,2,1) * gl2yi(:,4) +
     &        dxidxb(:,3,1) * gl3yi(:,4)
         g2yi(:,4) = dxidxb(:,1,2) * gl1yi(:,4) + 
     &        dxidxb(:,2,2) * gl2yi(:,4) +
     &        dxidxb(:,3,2) * gl3yi(:,4)
         g3yi(:,4) = dxidxb(:,1,3) * gl1yi(:,4) + 
     &        dxidxb(:,2,3) * gl2yi(:,4) +
     &        dxidxb(:,3,3) * gl3yi(:,4)
c     
c.... end grad-v
c     
      endif
c     
c.... mass flux
c     
      unm = bnorm(:,1) * u1 +bnorm(:,2) * u2  +bnorm(:,3) * u3
! no rho in continuity eq.


c
c.... viscous flux
c
      tau1n = bnorm(:,1) * two * rmu *  g1yi(:,2)  
     &     + bnorm(:,2) *      (rmu * (g2yi(:,2) + g1yi(:,3)))
     &     + bnorm(:,3) *      (rmu * (g3yi(:,2) + g1yi(:,4)))
      tau2n = bnorm(:,1) *      (rmu * (g2yi(:,2) + g1yi(:,3)))
     &     + bnorm(:,2) * two * rmu *  g2yi(:,3) 
     &     + bnorm(:,3) *      (rmu * (g3yi(:,3) + g2yi(:,4)))
      tau3n = bnorm(:,1) *      (rmu * (g3yi(:,2) + g1yi(:,4)))
     &     + bnorm(:,2) *      (rmu * (g3yi(:,3) + g2yi(:,4)))
     &     + bnorm(:,3) * two * rmu *  g3yi(:,4) 
c     
      temp1 = bnorm(:,1) * tau1n
     &     + bnorm(:,2) * tau2n
     &     + bnorm(:,3) * tau3n
      
      pres  = pres - temp1
      
      tau1n = tau1n - bnorm(:,1) * temp1
      tau2n = tau2n - bnorm(:,2) * temp1
      tau3n = tau3n - bnorm(:,3) * temp1

      vdot = zero
      rlKwall = zero
      if (intp.eq.ngaussb)   then    ! do this only for the last gauss point
        rKwall_glob = zero
      endif

      if(ideformwall.eq.1) then
      do n = 1, nshlb
         nodlcl = lnode(n)
c     
         vdot(:,1) = vdot(:,1) + shpb(:,nodlcl) * acl(:,nodlcl,2)
         vdot(:,2) = vdot(:,2) + shpb(:,nodlcl) * acl(:,nodlcl,3)
         vdot(:,3) = vdot(:,3) + shpb(:,nodlcl) * acl(:,nodlcl,4)

      enddo
      vdot = vdot * thicknessvw * rhovw
c     
c.... --------------------->  Stiffness matrix & residual  <-----------------
c     
c.... B^t * D * B formulation for plane stress enhanced membrane
c
c
c.... rotation matrix
c     
      v1 = xlb(:,ipt2,:) - xlb(:,1,:)
      temp       = one / sqrt ( v1(:,1)**2 + v1(:,2)**2 + v1(:,3)**2 )
      v1(:,1) = v1(:,1) * temp
      v1(:,2) = v1(:,2) * temp
      v1(:,3) = v1(:,3) * temp
      
      v2 = xlb(:,ipt3,:) - xlb(:,1,:)
      
c     compute cross product
      temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
      temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
      temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)
      
      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      v3(:,1) = temp1 * temp
      v3(:,2) = temp2 * temp
      v3(:,3) = temp3 * temp
      
c     cross product again for v2
      temp1 = v3(:,2) * v1(:,3) - v1(:,2) * v3(:,3)
      temp2 = v1(:,1) * v3(:,3) - v3(:,1) * v1(:,3)
      temp3 = v3(:,1) * v1(:,2) - v1(:,1) * v3(:,2)
      
      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      v2(:,1) = temp1 * temp
      v2(:,2) = temp2 * temp
      v2(:,3) = temp3 * temp
      
      do j = 1, nsd
         rotnodallocal(:,1,j) = v1(:,j)
         rotnodallocal(:,2,j) = v2(:,j)
         rotnodallocal(:,3,j) = v3(:,j)
      enddo      
c     
c.... rotated coordinates
c
      x1rot = zero
      x2rot = zero
      x3rot = zero
      
      do i = 1, nsd
         do j = 1, nsd 
            x1rot(:,i) = x1rot(:,i)+rotnodallocal(:,i,j)*xlb(:,1,j)
            x2rot(:,i) = x2rot(:,i)+rotnodallocal(:,i,j)*xlb(:,ipt2,j)
            x3rot(:,i) = x3rot(:,i)+rotnodallocal(:,i,j)*xlb(:,ipt3,j)
         enddo
      enddo      
c     
c.... B matrices
c     
      B1 = zero
      B2 = zero
      B3 = zero
      detJacrot = (x2rot(:,1)-x1rot(:,1)) * (x3rot(:,2)-x1rot(:,2)) - 
     &     (x3rot(:,1)-x1rot(:,1)) * (x2rot(:,2)-x1rot(:,2))
      
      B1(:,1,1) = (x2rot(:,2)-x3rot(:,2))/detJacrot(:)
      B1(:,2,2) = (x3rot(:,1)-x2rot(:,1))/detJacrot(:)
      B1(:,3,1) = (x3rot(:,1)-x2rot(:,1))/detJacrot(:)
      B1(:,3,2) = (x2rot(:,2)-x3rot(:,2))/detJacrot(:)
      B1(:,4,3) = (x2rot(:,2)-x3rot(:,2))/detJacrot(:)
      B1(:,5,3) = (x3rot(:,1)-x2rot(:,1))/detJacrot(:)
      
      B2(:,1,1) = (x3rot(:,2)-x1rot(:,2))/detJacrot(:)
      B2(:,2,2) = (x1rot(:,1)-x3rot(:,1))/detJacrot(:)
      B2(:,3,1) = (x1rot(:,1)-x3rot(:,1))/detJacrot(:)
      B2(:,3,2) = (x3rot(:,2)-x1rot(:,2))/detJacrot(:)
      B2(:,4,3) = (x3rot(:,2)-x1rot(:,2))/detJacrot(:)
      B2(:,5,3) = (x1rot(:,1)-x3rot(:,1))/detJacrot(:)
      
      B3(:,1,1) = (x1rot(:,2)-x2rot(:,2))/detJacrot(:)
      B3(:,2,2) = (x2rot(:,1)-x1rot(:,1))/detJacrot(:)
      B3(:,3,1) = (x2rot(:,1)-x1rot(:,1))/detJacrot(:)
      B3(:,3,2) = (x1rot(:,2)-x2rot(:,2))/detJacrot(:)
      B3(:,4,3) = (x1rot(:,2)-x2rot(:,2))/detJacrot(:)
      B3(:,5,3) = (x2rot(:,1)-x1rot(:,1))/detJacrot(:)
      
C      B1 = B1 / detJacrot
C      B2 = B2 / detJacrot
C      B3 = B3 / detJacrot
      
c     
c.... D matrix
c     
      Dmatrix = zero
      temp1 = evw / (1.0d0 - rnuvw*rnuvw)
      temp2 = rnuvw * temp1
      temp3 = pt5 * (1.0d0 - rnuvw) * temp1
      Dmatrix(:,1,1) = temp1
      Dmatrix(:,1,2) = temp2
      Dmatrix(:,2,1) = temp2
      Dmatrix(:,2,2) = temp1
      Dmatrix(:,3,3) = temp3
      Dmatrix(:,4,4) = temp3*rshearconstantvw
      Dmatrix(:,5,5) = temp3*rshearconstantvw
c     
c.... D * [B1|B2|B3]
c     
      DtimesB1 = zero
      DtimesB2 = zero
      DtimesB3 = zero
      do i = 1, 5
         do j = 1, 3
            do k = 1, 5
               DtimesB1(:,i,j) = DtimesB1(:,i,j) 
     &              + Dmatrix(:,i,k) * B1(:,k,j)
               DtimesB2(:,i,j) = DtimesB2(:,i,j) 
     &              + Dmatrix(:,i,k) * B2(:,k,j)
               DtimesB3(:,i,j) = DtimesB3(:,i,j) 
     &              + Dmatrix(:,i,k) * B3(:,k,j)
            enddo
         enddo
      enddo
c     
c.... [B1|B2|B3]^T * D * [B1|B2|B3]
c     
      rKwall_local11 = zero
      rKwall_local12 = zero
      rKwall_local13 = zero
      rKwall_local21 = zero
      rKwall_local22 = zero
      rKwall_local23 = zero
      rKwall_local31 = zero
      rKwall_local32 = zero
      rKwall_local33 = zero
      
      do i = 1, 3               ! i is a node index: i=1, nenbl=3
         do j = 1, 3            ! same is true for j
            do k = 1, 5
               rKwall_local11(:,i,j)  = rKwall_local11(:,i,j)
     &              + B1(:,k,i) * DtimesB1(:,k,j)
               rKwall_local12(:,i,j)  = rKwall_local12(:,i,j)
     &              + B1(:,k,i) * DtimesB2(:,k,j)
               rKwall_local13(:,i,j)  = rKwall_local13(:,i,j)
     &              + B1(:,k,i) * DtimesB3(:,k,j)
               rKwall_local21(:,i,j)  = rKwall_local21(:,i,j)
     &              + B2(:,k,i) * DtimesB1(:,k,j)
               rKwall_local22(:,i,j)  = rKwall_local22(:,i,j)
     &              + B2(:,k,i) * DtimesB2(:,k,j)
               rKwall_local23(:,i,j)  = rKwall_local23(:,i,j)
     &              + B2(:,k,i) * DtimesB3(:,k,j)
               rKwall_local31(:,i,j)  = rKwall_local31(:,i,j)
     &              + B3(:,k,i) * DtimesB1(:,k,j)
               rKwall_local32(:,i,j)  = rKwall_local32(:,i,j)
     &              + B3(:,k,i) * DtimesB2(:,k,j)
               rKwall_local33(:,i,j)  = rKwall_local33(:,i,j)
     &              + B3(:,k,i) * DtimesB3(:,k,j)
            enddo
         enddo
      enddo
      
c     
c.... Now we need to rotate each of these submatrices to the global frame
c     
      call rotatestiff(rKwall_local11, rotnodallocal, rKwall_glob11)
      call rotatestiff(rKwall_local12, rotnodallocal, rKwall_glob12)
      call rotatestiff(rKwall_local13, rotnodallocal, rKwall_glob13)
      call rotatestiff(rKwall_local21, rotnodallocal, rKwall_glob21)
      call rotatestiff(rKwall_local22, rotnodallocal, rKwall_glob22)
      call rotatestiff(rKwall_local23, rotnodallocal, rKwall_glob23)
      call rotatestiff(rKwall_local31, rotnodallocal, rKwall_glob31)
      call rotatestiff(rKwall_local32, rotnodallocal, rKwall_glob32)
      call rotatestiff(rKwall_local33, rotnodallocal, rKwall_glob33)

c     multiply the nodal matrices by the area and the thickness
      do i =1, nsd
         do j = 1, nsd
            rKwall_glob11(:,i,j) = rKwall_glob11(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob12(:,i,j) = rKwall_glob12(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob13(:,i,j) = rKwall_glob13(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob21(:,i,j) = rKwall_glob21(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob22(:,i,j) = rKwall_glob22(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob23(:,i,j) = rKwall_glob23(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob31(:,i,j) = rKwall_glob31(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob32(:,i,j) = rKwall_glob32(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
            rKwall_glob33(:,i,j) = rKwall_glob33(:,i,j) * detJacrot(:) 
     &                           * pt5 * thicknessvw
         enddo
      enddo

c     
c.... Final K * u product (in global coordinates) to get the residual
c
      do i = 1, 3               ! now i is a spatial index: i=1, nsd=3
         rlKwall(:,1,1) = rlKwall(:,1,1) 
     &                  + rKwall_glob11(:,1,i) * ul(:,1,i) 
     &                  + rKwall_glob12(:,1,i) * ul(:,2,i) 
     &                  + rKwall_glob13(:,1,i) * ul(:,3,i) 
         rlKwall(:,1,2) = rlKwall(:,1,2)
     &                  + rKwall_glob11(:,2,i) * ul(:,1,i) 
     &                  + rKwall_glob12(:,2,i) * ul(:,2,i) 
     &                  + rKwall_glob13(:,2,i) * ul(:,3,i) 
         rlKwall(:,1,3) = rlKwall(:,1,3) 
     &                  + rKwall_glob11(:,3,i) * ul(:,1,i) 
     &                  + rKwall_glob12(:,3,i) * ul(:,2,i) 
     &                  + rKwall_glob13(:,3,i) * ul(:,3,i) 
         rlKwall(:,2,1) = rlKwall(:,2,1) 
     &                  + rKwall_glob21(:,1,i) * ul(:,1,i) 
     &                  + rKwall_glob22(:,1,i) * ul(:,2,i) 
     &                  + rKwall_glob23(:,1,i) * ul(:,3,i) 
         rlKwall(:,2,2) = rlKwall(:,2,2)
     &                  + rKwall_glob21(:,2,i) * ul(:,1,i) 
     &                  + rKwall_glob22(:,2,i) * ul(:,2,i) 
     &                  + rKwall_glob23(:,2,i) * ul(:,3,i) 
         rlKwall(:,2,3) = rlKwall(:,2,3) 
     &                  + rKwall_glob21(:,3,i) * ul(:,1,i) 
     &                  + rKwall_glob22(:,3,i) * ul(:,2,i) 
     &                  + rKwall_glob23(:,3,i) * ul(:,3,i)
         rlKwall(:,3,1) = rlKwall(:,3,1) 
     &                  + rKwall_glob31(:,1,i) * ul(:,1,i) 
     &                  + rKwall_glob32(:,1,i) * ul(:,2,i) 
     &                  + rKwall_glob33(:,1,i) * ul(:,3,i) 
         rlKwall(:,3,2) = rlKwall(:,3,2)
     &                  + rKwall_glob31(:,2,i) * ul(:,1,i) 
     &                  + rKwall_glob32(:,2,i) * ul(:,2,i) 
     &                  + rKwall_glob33(:,2,i) * ul(:,3,i) 
         rlKwall(:,3,3) = rlKwall(:,3,3) 
     &                  + rKwall_glob31(:,3,i) * ul(:,1,i) 
     &                  + rKwall_glob32(:,3,i) * ul(:,2,i) 
     &                  + rKwall_glob33(:,3,i) * ul(:,3,i)
      enddo
c     
c.... --------------> End of Stiffness matrix & residual  <-----------------
c     

c     
c.... -----> Wall Stiffness and Mass matrices for implicit LHS  <-----------
c     

c....  Here we just add the mass matrix contribution.  The stiffness contribution 
c....  is added in e3b

c      lhmFct = almi * (one - flmpl)      Maybe we have to define flmplW: lumped
                                        ! mass parameter for the wall
      lhmFctvw = almi * (one - flmpl)                                  
c
c.... scale variables for efficiency
c
      tsFctvw     = lhmFctvw * WdetJb * rhovw * thicknessvw     
c
c.... compute mass and convection terms
c
c.... NOTE:  the wall mass contributions should only have 3 nodal components 
c.... since the fourth node is an interior node... therefore, the loops should
c.... be done from 1 to nshlb=3...

      do b = 1, nshlb
         do aa = 1, nshlb
            tmp1 = tsFctvw * shpb(:,aa) * shpb(:,b)
c
c           tmp1=alpha_m*(1-lmp)*WdetJ*N^aN^b*rho*thickness   the time term 
c            
            xKebe(:,1,aa,b) = xKebe(:,1,aa,b) + tmp1
            xKebe(:,5,aa,b) = xKebe(:,5,aa,b) + tmp1
            xKebe(:,9,aa,b) = xKebe(:,9,aa,b) + tmp1
         enddo
      enddo

c
c.... assemble the nodal stiffness into the element stiffness matrix rKwall_glob
c
c.... We have passed the integer intp to make this operation only once: we are 
c.... not using the gauss points structure to compute the stiffness of the wall 
c.... elements, so we don't want to be redundant and calculate ngaussb times the 
c.... stiffness matrix which is constant for linear triangles...

c.... This is ugly, but I will fix it later...

      if (intp.eq.ngaussb)   then    ! do this only for the last gauss point
        rKwall_glob(:,1,1,1) = rKwall_glob11(:,1,1)
        rKwall_glob(:,2,1,1) = rKwall_glob11(:,1,2)
        rKwall_glob(:,3,1,1) = rKwall_glob11(:,1,3)
        rKwall_glob(:,4,1,1) = rKwall_glob11(:,2,1)
        rKwall_glob(:,5,1,1) = rKwall_glob11(:,2,2)
        rKwall_glob(:,6,1,1) = rKwall_glob11(:,2,3)
        rKwall_glob(:,7,1,1) = rKwall_glob11(:,3,1)
        rKwall_glob(:,8,1,1) = rKwall_glob11(:,3,2)
        rKwall_glob(:,9,1,1) = rKwall_glob11(:,3,3)
      
        rKwall_glob(:,1,1,2) = rKwall_glob12(:,1,1)
        rKwall_glob(:,2,1,2) = rKwall_glob12(:,1,2)
        rKwall_glob(:,3,1,2) = rKwall_glob12(:,1,3)
        rKwall_glob(:,4,1,2) = rKwall_glob12(:,2,1)
        rKwall_glob(:,5,1,2) = rKwall_glob12(:,2,2)
        rKwall_glob(:,6,1,2) = rKwall_glob12(:,2,3)
        rKwall_glob(:,7,1,2) = rKwall_glob12(:,3,1)
        rKwall_glob(:,8,1,2) = rKwall_glob12(:,3,2)
        rKwall_glob(:,9,1,2) = rKwall_glob12(:,3,3)
      
        rKwall_glob(:,1,1,3) = rKwall_glob13(:,1,1)
        rKwall_glob(:,2,1,3) = rKwall_glob13(:,1,2)
        rKwall_glob(:,3,1,3) = rKwall_glob13(:,1,3)
        rKwall_glob(:,4,1,3) = rKwall_glob13(:,2,1)
        rKwall_glob(:,5,1,3) = rKwall_glob13(:,2,2)
        rKwall_glob(:,6,1,3) = rKwall_glob13(:,2,3)
        rKwall_glob(:,7,1,3) = rKwall_glob13(:,3,1)
        rKwall_glob(:,8,1,3) = rKwall_glob13(:,3,2)
        rKwall_glob(:,9,1,3) = rKwall_glob13(:,3,3)
      
        rKwall_glob(:,1,2,1) = rKwall_glob21(:,1,1)
        rKwall_glob(:,2,2,1) = rKwall_glob21(:,1,2)
        rKwall_glob(:,3,2,1) = rKwall_glob21(:,1,3)
        rKwall_glob(:,4,2,1) = rKwall_glob21(:,2,1)
        rKwall_glob(:,5,2,1) = rKwall_glob21(:,2,2)
        rKwall_glob(:,6,2,1) = rKwall_glob21(:,2,3)
        rKwall_glob(:,7,2,1) = rKwall_glob21(:,3,1)
        rKwall_glob(:,8,2,1) = rKwall_glob21(:,3,2)
        rKwall_glob(:,9,2,1) = rKwall_glob21(:,3,3)
      
        rKwall_glob(:,1,2,2) = rKwall_glob22(:,1,1)
        rKwall_glob(:,2,2,2) = rKwall_glob22(:,1,2)
        rKwall_glob(:,3,2,2) = rKwall_glob22(:,1,3)
        rKwall_glob(:,4,2,2) = rKwall_glob22(:,2,1)
        rKwall_glob(:,5,2,2) = rKwall_glob22(:,2,2)
        rKwall_glob(:,6,2,2) = rKwall_glob22(:,2,3)
        rKwall_glob(:,7,2,2) = rKwall_glob22(:,3,1)
        rKwall_glob(:,8,2,2) = rKwall_glob22(:,3,2)
        rKwall_glob(:,9,2,2) = rKwall_glob22(:,3,3)      

        rKwall_glob(:,1,2,3) = rKwall_glob23(:,1,1)
        rKwall_glob(:,2,2,3) = rKwall_glob23(:,1,2)
        rKwall_glob(:,3,2,3) = rKwall_glob23(:,1,3)
        rKwall_glob(:,4,2,3) = rKwall_glob23(:,2,1)
        rKwall_glob(:,5,2,3) = rKwall_glob23(:,2,2)
        rKwall_glob(:,6,2,3) = rKwall_glob23(:,2,3)
        rKwall_glob(:,7,2,3) = rKwall_glob23(:,3,1)
        rKwall_glob(:,8,2,3) = rKwall_glob23(:,3,2)
        rKwall_glob(:,9,2,3) = rKwall_glob23(:,3,3)
      
        rKwall_glob(:,1,3,1) = rKwall_glob31(:,1,1)
        rKwall_glob(:,2,3,1) = rKwall_glob31(:,1,2)
        rKwall_glob(:,3,3,1) = rKwall_glob31(:,1,3)
        rKwall_glob(:,4,3,1) = rKwall_glob31(:,2,1)
        rKwall_glob(:,5,3,1) = rKwall_glob31(:,2,2)
        rKwall_glob(:,6,3,1) = rKwall_glob31(:,2,3)
        rKwall_glob(:,7,3,1) = rKwall_glob31(:,3,1)
        rKwall_glob(:,8,3,1) = rKwall_glob31(:,3,2)
        rKwall_glob(:,9,3,1) = rKwall_glob31(:,3,3)

        rKwall_glob(:,1,3,2) = rKwall_glob32(:,1,1)
        rKwall_glob(:,2,3,2) = rKwall_glob32(:,1,2)
        rKwall_glob(:,3,3,2) = rKwall_glob32(:,1,3)
        rKwall_glob(:,4,3,2) = rKwall_glob32(:,2,1)
        rKwall_glob(:,5,3,2) = rKwall_glob32(:,2,2)
        rKwall_glob(:,6,3,2) = rKwall_glob32(:,2,3)
        rKwall_glob(:,7,3,2) = rKwall_glob32(:,3,1)
        rKwall_glob(:,8,3,2) = rKwall_glob32(:,3,2)
        rKwall_glob(:,9,3,2) = rKwall_glob32(:,3,3)
      
        rKwall_glob(:,1,3,3) = rKwall_glob33(:,1,1)
        rKwall_glob(:,2,3,3) = rKwall_glob33(:,1,2)
        rKwall_glob(:,3,3,3) = rKwall_glob33(:,1,3)
        rKwall_glob(:,4,3,3) = rKwall_glob33(:,2,1)
        rKwall_glob(:,5,3,3) = rKwall_glob33(:,2,2)
        rKwall_glob(:,6,3,3) = rKwall_glob33(:,2,3)
        rKwall_glob(:,7,3,3) = rKwall_glob33(:,3,1)
        rKwall_glob(:,8,3,3) = rKwall_glob33(:,3,2)
        rKwall_glob(:,9,3,3) = rKwall_glob33(:,3,3)

        rKwall_glob = rKwall_glob*betai*Delt(itseq)*Delt(itseq)*alfi
      
      else
c....   nothing happens
        goto 123
      endif      

123   continue

      endif
c     
c.... return
c     
      return
      end
      
!> Variables for boundary elements

        subroutine e3bvarSclr (yl,        shdrv,    xlb,
     &                         shape,     WdetJb,   bnorm,
     &                         flux,      dwl )

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"

C     Argument variables
C
      REAL*8                bnorm,       flux,        shape,     shdrv
      REAL*8                wdetjb,      xlb,         yl
C
C     Local variables
C
      INTEGER             isc,         n
C
      REAL*8                dxdxib,      dxidxb,      grads,     gradsl
      REAL*8                temp,        temp1,       temp2,     temp3
      REAL*8                v1,          v2
C
c
        dimension yl(npro,nshl,ndof),        shdrv(npro,nsd,nshl),
     &            xlb(npro,nenl,nsd),        shape(npro,nshl),
     &            WdetJb(npro),              bnorm(npro,nsd),
     &            flux(npro)
c
        dimension dxdxib(npro,nsd,nsd),
     &            dxidxb(npro,nsd,nsd),      temp(npro),
     &            temp1(npro),               temp2(npro),
     &            temp3(npro),
     &            v1(npro,nsd),              v2(npro,nsd),
     &            gradSl(npro,nsd),          gradS(npro,nsd)

        real*8    diffus(npro),              dwl(npro,nshl)
        
        call getdiffsclr(shape,dwl,yl,diffus)
c
c.... ---------------------->  Element Metrics  <-----------------------
c
c.... compute the deformation gradient
c
        dxdxib = zero
c
        do n = 1, nenl
           dxdxib(:,1,1) = dxdxib(:,1,1) + xlb(:,n,1) * shdrv(:,1,n)
           dxdxib(:,1,2) = dxdxib(:,1,2) + xlb(:,n,1) * shdrv(:,2,n)
           dxdxib(:,1,3) = dxdxib(:,1,3) + xlb(:,n,1) * shdrv(:,3,n)
           dxdxib(:,2,1) = dxdxib(:,2,1) + xlb(:,n,2) * shdrv(:,1,n)
           dxdxib(:,2,2) = dxdxib(:,2,2) + xlb(:,n,2) * shdrv(:,2,n)
           dxdxib(:,2,3) = dxdxib(:,2,3) + xlb(:,n,2) * shdrv(:,3,n)
           dxdxib(:,3,1) = dxdxib(:,3,1) + xlb(:,n,3) * shdrv(:,1,n)
           dxdxib(:,3,2) = dxdxib(:,3,2) + xlb(:,n,3) * shdrv(:,2,n)
           dxdxib(:,3,3) = dxdxib(:,3,3) + xlb(:,n,3) * shdrv(:,3,n)
        enddo
c     
c.... compute the normal to the boundary. This is achieved by taking
c     the cross product of two vectors in the plane of the 2-d 
c     boundary face.
c
        v1 = xlb(:,2,:) - xlb(:,1,:)
        v2 = xlb(:,3,:) - xlb(:,1,:)
        
c     
c.....The following are done in order to correct temp1..3  
c     based on the results from compressible code.  This is done only 
c     for wedges, depending on the bounary face.(tri or quad)  
c     
        if (lcsyst .eq. 4) then
           temp1 = dxdxib(:,2,1) * dxdxib(:,3,3) -
     &             dxdxib(:,2,3) * dxdxib(:,3,1)
           temp2 = dxdxib(:,3,1) * dxdxib(:,1,3) -
     &             dxdxib(:,3,3) * dxdxib(:,1,1)
           temp3 = dxdxib(:,1,1) * dxdxib(:,2,3) -
     &             dxdxib(:,1,3) * dxdxib(:,2,1)
             
        elseif (lcsyst .eq. 1) then
           temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
           temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
           temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)
        else 
           temp1 = - v1(:,2) * v2(:,3) + v2(:,2) * v1(:,3)
           temp2 = - v2(:,1) * v1(:,3) + v1(:,1) * v2(:,3)
           temp3 = - v1(:,1) * v2(:,2) + v2(:,1) * v1(:,2)
        endif
c
        temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
        bnorm(:,1) = temp1 * temp
        bnorm(:,2) = temp2 * temp
        bnorm(:,3) = temp3 * temp
c
     
        if (lcsyst .eq. 3) then
           WdetJb     = (1 - Qwtb(lcsyst,intp)) / (four*temp)
        elseif (lcsyst .eq. 4) then
           WdetJb     = Qwtb(lcsyst,intp) / temp
        else
           WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
        endif      
c
c.... -------------------------->  Grad-V  <----------------------------
c
c.... compute grad-v for Navier-Stokes terms
c
        if (Navier .eq. 1) then
c
c.... compute the inverse of deformation gradient
c
          dxidxb(:,1,1) =   dxdxib(:,2,2) * dxdxib(:,3,3) 
     &                    - dxdxib(:,3,2) * dxdxib(:,2,3)
          dxidxb(:,1,2) =   dxdxib(:,3,2) * dxdxib(:,1,3) 
     &                    - dxdxib(:,1,2) * dxdxib(:,3,3)
          dxidxb(:,1,3) =   dxdxib(:,1,2) * dxdxib(:,2,3) 
     &                    - dxdxib(:,1,3) * dxdxib(:,2,2)
          temp          = one / ( dxidxb(:,1,1) * dxdxib(:,1,1) 
     &                          + dxidxb(:,1,2) * dxdxib(:,2,1)  
     &                          + dxidxb(:,1,3) * dxdxib(:,3,1) )
          dxidxb(:,1,1) =  dxidxb(:,1,1) * temp
          dxidxb(:,1,2) =  dxidxb(:,1,2) * temp
          dxidxb(:,1,3) =  dxidxb(:,1,3) * temp
          dxidxb(:,2,1) = (dxdxib(:,2,3) * dxdxib(:,3,1) 
     &                   - dxdxib(:,2,1) * dxdxib(:,3,3)) * temp
          dxidxb(:,2,2) = (dxdxib(:,1,1) * dxdxib(:,3,3) 
     &                   - dxdxib(:,3,1) * dxdxib(:,1,3)) * temp
          dxidxb(:,2,3) = (dxdxib(:,2,1) * dxdxib(:,1,3) 
     &                   - dxdxib(:,1,1) * dxdxib(:,2,3)) * temp
          dxidxb(:,3,1) = (dxdxib(:,2,1) * dxdxib(:,3,2) 
     &                   - dxdxib(:,2,2) * dxdxib(:,3,1)) * temp
          dxidxb(:,3,2) = (dxdxib(:,3,1) * dxdxib(:,1,2) 
     &                   - dxdxib(:,1,1) * dxdxib(:,3,2)) * temp
          dxidxb(:,3,3) = (dxdxib(:,1,1) * dxdxib(:,2,2) 
     &                   - dxdxib(:,1,2) * dxdxib(:,2,1)) * temp
c
c.... compute local-grad-Y
c
c
          gradSl = zero
          isc=5+isclr
          do n = 1, nshl
            gradSl(:,1) = gradSl(:,1) + shdrv(:,1,n) * yl(:,n,isc)
            gradSl(:,2) = gradSl(:,2) + shdrv(:,2,n) * yl(:,n,isc)
            gradSl(:,3) = gradSl(:,3) + shdrv(:,3,n) * yl(:,n,isc)
          enddo
c
c.... convert local-grads to global-grads
c
          gradS(:,1) = dxidxb(:,1,1) * gradSl(:,1) + 
     &                 dxidxb(:,2,1) * gradSl(:,2) + 
     &                 dxidxb(:,3,1) * gradSl(:,3)  

c
          gradS(:,2) = dxidxb(:,1,2) * gradSl(:,1) +
     &                 dxidxb(:,2,2) * gradSl(:,2) +
     &                 dxidxb(:,3,2) * gradSl(:,3) 

          gradS(:,3) = dxidxb(:,1,3) * gradSl(:,1) +
     &                 dxidxb(:,2,3) * gradSl(:,2) +
     &                 dxidxb(:,3,3) * gradSl(:,3) 
c
c.... end grad-T
c
        endif

        flux = diffus * ( gradS(:,1) * bnorm(:,1)
     &                  + gradS(:,2) * bnorm(:,2)
     &                  + gradS(:,3) * bnorm(:,3) )
c
c.... return
c
        return
        end

!> Rotates the local nodal stiffnesses to the goblal frame

      subroutine rotatestiff(rKlocal, rotation, 
     &                       rKglobal)

      include "global.h"
      include "common_blocks/propar.h"
      
C     Argument variables
C
      REAL*8                rkglobal,    rklocal,     rotation
C
C     Local variables
C
      INTEGER             i,           j,           k
C
      REAL*8                tempm
C
      dimension rKlocal(npro,nsd,nsd), rotation(npro,nsd,nsd),
     &          rKglobal(npro,nsd,nsd)

      dimension tempm(npro,nsd,nsd)

      tempm = zero
      do i = 1, 3
         do j = 1, 3
            do k = 1, 3
               tempm(:,i,j) = tempm(:,i,j) 
     &              + rKlocal(:,i,k) * rotation(:,k,j)
            enddo
         enddo
      enddo
      
      rKglobal = zero
      do i = 1, 3
         do j = 1, 3
            do k = 1, 3
               rKglobal(:,i,j) = rKglobal(:,i,j) 
     &              + rotation(:,k,i) * tempm(:,k,j)
            enddo
         enddo
      enddo

      return
      end

!> This routine calculates the contribution of the Discontinuity-
!! Capturing operator to RHS and preconditioner for the scalar solve.
!!
!! input:<BR>
!! @param[in] g1yti(nflow,npro) Grad-y in direction 1
!! @param[in] g2yti(nflow,npro) Grad-y in direction 2
!! @param[in] g3yti(nflow,npro) Grad-y in direction 3
!! @param[in] A0(nsymdf,npro) A0 matrix (Symm. storage)
!! @param[in] raLS(npro) Square of LS residual (A0inv norm)
!! @param[in] rtLS(npro) Square of LS residual (Tau norm)
!! @param[in] giju(6,npro) Metric matrix
!! @param[in] DC(ngauss,npro) Discontinuity-capturing factor
!! @param[in] intp Integration point number
!!
!! output:<BR>
!! @param[out] ri(nflow*(nsd+1),npro) Partial residual
!! @param[out] rmi(nflow*(nsd+1),npro) Partial modified residual
!! @param[out] stiff(nsymdf,6,npro) Diffusivity matrix
!! @param[out] DC(npro) Discontinuity-capturing factor

        subroutine e3dcSclr ( gradS,    giju,     gGradS,
     &                        rLS,      tauS,     srcR,
     &                        dcFct)
        include "global.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/solpar.h"
        include "common_blocks/precis.h"
c
C     Argument variables
C
      REAL*8                dcfct,       ggrads,      giju,       grads
      REAL*8                rls,         srcr,        taus
C
C     Local variables
C
      REAL*8                fact
C
        dimension gradS(npro,nsd),            gGradS(npro,nsd),
     &            rLS(npro),                  tauS(npro),
     &            giju(npro,6),               dcFct(npro),
     &            srcR(npro)
c
c.... Form GijUp gradS and  gradS . GijUp gradS (store in dcFct)
c
      
          gGradS(:,1) = GijU(:,1) * gradS(:,1)
     1                  + GijU(:,4) * gradS(:,2)
     2                  + GijU(:,6) * gradS(:,3)
          gGradS(:,2) = GijU(:,4) * gradS(:,1)
     1                  + GijU(:,2) * gradS(:,2)
     2                  + GijU(:,5) * gradS(:,3)
          gGradS(:,3) = GijU(:,6) * gradS(:,1)
     1                  + GijU(:,5) * gradS(:,2)
     2                  + GijU(:,3) * gradS(:,3)
c
          dcFct(:)    = gradS(:,1) * gGradS(:,1)
     1                    + gradS(:,2) * gGradS(:,2)
     2                    + gradS(:,3) * gGradS(:,3)
     3                    + epsM
      
          dcFct(:) = 1.0/ dcFct(:)
c
c.... Form pdeRes 2-norm / gradT 2-norm
c

          dcFct  = dcFct * (rLS - srcR) ** 2 
c
c.... ------------------------->  DC factor  <------------------------
c
c.... DC-mallet
c
          if (idcsclr(1) .eq. 1) then
c       
             fact = one
             if (ipord .eq. 2)  fact = 0.9
             if (ipord .eq. 3) fact = 0.75
c       
c$$$  dcFct(:)=dim((fact*sqrt(dcFct(:))),(tauS(:)*dcFct(:))) !not work
                                                          !with all compilers
             dcFct(:)=max(0.0,(fact*sqrt(dcFct(:)))-(tauS(:)*dcFct(:)))
c
          endif
c       
c       
c....   DC-quadratic
c       
          if (idcsclr(1) .eq. 2) then
c       
             dcFct(:) = two * tauS(:) * dcFct(:)
c       
          endif
c       
c....   DC-min
c       
          if (idcsclr(1) .eq. 3) then
c       
             fact = one
             if (ipord .eq. 2)  fact = 0.9
c       
          dcFct(:) = min( max(0.0, (fact * sqrt(dcFct(:)) -
     &                   tauS(:)*dcFct(:)) ), two * tauS(:) * dcFct(:))
c       
          endif
c
c.... Scale the gGradT for residual formation
c      
          gGradS(:,1) = dcFct(:) * gGradS(:,1)
          gGradS(:,2) = dcFct(:) * gGradS(:,2)
          gGradS(:,3) = dcFct(:) * gGradS(:,3)
      


      return
      end

!> This routine computes the variables at integration point.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Primitive variables
!! @param[in] acl(npro,nshl,ndof) Prim.var. accel. 
!! @param[in] shp(nen) Element shape-functions
!! @param[in] shgl(nsd,nen) Element local-grad-shape-functions
!! @param[in] xl(npro,nenl,nsd) Nodal coordinates at current step
!! @param[in] ql(npro,nshl,nsd*nsd) Diffusive flux vector
!! @param[in] rlsl(npro,nshl,6) Resolved Leonard stresses
!!
!! output:<BR>
!! @param[out] aci(npro,3) Primvar accel. variables 
!! @param[out] g1yi(npro,ndof) Grad-y in direction 1
!! @param[out] g2yi(npro,ndof) Grad-y in direction 2
!! @param[out] g3yi(npro,ndof) Grad-y in direction 3
!! @param[out] shg(npro,nshl,nsd) Element global grad-shape-functions
!! @param[out] dxidx(npro,nsd,nsd) Inverse of deformation gradient
!! @param[out] WdetJ(npro) Weighted Jacobian
!! @param[out] rho(npro) Density
!! @param[out] pres(npro) Pressure
!! @param[out] u1(npro) X1-velocity component
!! @param[out] u2(npro) X2-velocity component
!! @param[out] u3(npro) X3-velocity component
!! @param[out] rLui(npro,nsd) Xi-momentum residual
!! @param[out] src(npro,nsd) Body force term (not density weighted)
!! @param[out] rlsli(npro,6) Resolved Leonard stresses at quad pt
!!
!! locally calculated and used<BR>
!!  divqi(npro,nsd+isurf) Divergence of reconstructed quantity

      subroutine e3ivar (yl,          acl,       shpfun,
     &                   shgl,        xl,       
     &                   aci,         g1yi,      g2yi,    
     &                   g3yi,        shg,       dxidx,   
     &                   WdetJ,       rho,       pres, 
     &                   u1,          u2,        u3,              
     &                   ql,          rLui,      src,
     &                   rerrl,       rlsl,      rlsli,
     &                   dwl)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
  
 
c
C     Argument variables
C
      REAL*8                aci,         acl,         dwl,        dxidx
      REAL*8                g1yi,        g2yi,        g3yi,       pres
      REAL*8                ql,          rho,         rlsl,       rlsli
      REAL*8                rlui,        shg,         shgl,       shpfun
      REAL*8                src,         u1,          u2,         u3
      REAL*8                wdetj,       xl,          yl
C
C     Local variables
C
      INTEGER             i,           ia,          idflow,      n
C
      REAL*8                dist2w,      divqi,       gradh
      REAL*8                gyti,        sclr,        sforce,     temp
      REAL*8                tmp,         weber,       xx
C
c  passed arrays
c
      dimension yl(npro,nshl,ndof),        dwl(npro,nenl),       
     &            acl(npro,nshl,ndof),       shpfun(npro,nshl),
     &            shgl(npro,nsd,nshl),       xl(npro,nenl,nsd),
     &            aci(npro,nsd),             g1yi(npro,ndof),
     &            g2yi(npro,ndof),           g3yi(npro,ndof),
     &            shg(npro,nshl,nsd),        dxidx(npro,nsd,nsd),
     &            WdetJ(npro),               
     &            rho(npro),                 pres(npro),
     &            u1(npro),                  u2(npro),
     &            u3(npro),                  divqi(npro,nflow-1+isurf),
     &            ql(npro,nshl,idflx),       rLui(npro,nsd),
     &            src(npro,nsd), Temp(npro),xx(npro,nsd)
c
        dimension tmp(npro),      dist2w(npro)
c
        dimension rlsl(npro,nshl,6),         rlsli(npro,6)
c
        real*8    rerrl(npro,nshl,6), omega(3), divu(npro)
        dimension gyti(npro,nsd),            gradh(npro,nsd),
     &            sforce(npro,3),            weber(npro),
     &            Sclr(npro)
c
c.... ------------->  Primitive variables at int. point  <--------------
c
c.... compute primitive variables
c
       pres = zero
       u1   = zero
       u2   = zero
       u3   = zero
c
       do n = 1, nshl 
          pres = pres + shpfun(:,n) * yl(:,n,1)
          u1   = u1   + shpfun(:,n) * yl(:,n,2)
          u2   = u2   + shpfun(:,n) * yl(:,n,3)
          u3   = u3   + shpfun(:,n) * yl(:,n,4)
       enddo
       if(matflg(5,1).eq.2) then ! boussinesq body force
          Temp = zero
          do n = 1, nshl
             Temp = Temp + shpfun(:,n) * yl(:,n,5)
          enddo
       endif
       if(matflg(5,1).eq.3.or.matflg(6,1).eq.1) then
c         user-specified body force or coriolis force specified
          xx = zero
          do n  = 1,nenl
             xx(:,1) = xx(:,1)  + shpfun(:,n) * xl(:,n,1)
             xx(:,2) = xx(:,2)  + shpfun(:,n) * xl(:,n,2)
             xx(:,3) = xx(:,3)  + shpfun(:,n) * xl(:,n,3)
          enddo
       endif
c
c
c.... Resolved Leonhard stress set to zero - no turbulence - DES
       rlsli = zero
c
c.... ----------------------->  accel. at int. point  <----------------------
c
       aci = zero
       do n = 1, nshl
          aci(:,1) = aci(:,1) + shpfun(:,n) * acl(:,n,2)
          aci(:,2) = aci(:,2) + shpfun(:,n) * acl(:,n,3)
          aci(:,3) = aci(:,3) + shpfun(:,n) * acl(:,n,4)
       enddo
c
c.... --------------------->  Element Metrics  <-----------------------
c
       call e3metric( xl,         shgl,       dxidx,  
     &                shg,        WdetJ)
c
c.... compute the global gradient of u and P
c
c
       g1yi = zero
       g2yi = zero
       g3yi = zero
       do n = 1, nshl
          g1yi(:,1) = g1yi(:,1) + shg(:,n,1) * yl(:,n,1)
          g1yi(:,2) = g1yi(:,2) + shg(:,n,1) * yl(:,n,2)
          g1yi(:,3) = g1yi(:,3) + shg(:,n,1) * yl(:,n,3)
          g1yi(:,4) = g1yi(:,4) + shg(:,n,1) * yl(:,n,4)
c
          g2yi(:,1) = g2yi(:,1) + shg(:,n,2) * yl(:,n,1)
          g2yi(:,2) = g2yi(:,2) + shg(:,n,2) * yl(:,n,2)
          g2yi(:,3) = g2yi(:,3) + shg(:,n,2) * yl(:,n,3)
          g2yi(:,4) = g2yi(:,4) + shg(:,n,2) * yl(:,n,4)
c
          g3yi(:,1) = g3yi(:,1) + shg(:,n,3) * yl(:,n,1)
          g3yi(:,2) = g3yi(:,2) + shg(:,n,3) * yl(:,n,2)
          g3yi(:,3) = g3yi(:,3) + shg(:,n,3) * yl(:,n,3)
          g3yi(:,4) = g3yi(:,4) + shg(:,n,3) * yl(:,n,4)
       enddo

       divqi = zero
       idflow = 3
       if ( idiff >= 1 .or. isurf==1 ) then
c     
c.... compute divergence of diffusive flux vector, qi,i
c     
          if(idiff >= 1) then
             do n=1, nshl
                divqi(:,1) = divqi(:,1) + shg(:,n,1)*ql(:,n,1 ) 
     &                                  + shg(:,n,2)*ql(:,n,4 )
     &                                  + shg(:,n,3)*ql(:,n,7 )

                divqi(:,2) = divqi(:,2) + shg(:,n,1)*ql(:,n,2 ) 
     &                                  + shg(:,n,2)*ql(:,n,5 )
     &                                  + shg(:,n,3)*ql(:,n,8)

                divqi(:,3) = divqi(:,3) + shg(:,n,1)*ql(:,n,3 ) 
     &                                  + shg(:,n,2)*ql(:,n,6 )
     &                                  + shg(:,n,3)*ql(:,n,9 )

          enddo

          endif                 !end of idiff
c     
          if (isurf .eq. 1) then   
c     .... divergence of normal calculation (curvature)
             do n=1, nshl
                divqi(:,idflow+1) = divqi(:,idflow+1) 
     &               + shg(:,n,1)*ql(:,n,idflx-2)
     &               + shg(:,n,2)*ql(:,n,idflx-1)
     &               + shg(:,n,3)*ql(:,n,idflx)
             enddo 
c     .... initialization of some variables
             Sclr = zero
             gradh= zero
             gyti = zero
             sforce=zero
             do i = 1, npro
                do n = 1, nshl      
                   Sclr(i) = Sclr(i) + shpfun(i,n) * yl(i,n,6) !scalar
c     
c     .... compute the global gradient of Scalar variable
c     
                   gyti(i,1) = gyti(i,1) + shg(i,n,1) * yl(i,n,6) 
                   gyti(i,2) = gyti(i,2) + shg(i,n,2) * yl(i,n,6)
                   gyti(i,3) = gyti(i,3) + shg(i,n,3) * yl(i,n,6)
c     
                enddo

c NMW - 2014-03-25: Not really sure what this is doing.
c                   deleted since we don't have epsilon_ls anymore, but
c                   not sure if this is correct.
c                if (abs (sclr(i)) .le. epsilon_ls) then
c                   gradh(i,1) = 0.5/epsilon_ls * (1.0 
c     &                  + cos(pi*Sclr(i)/epsilon_ls)) * gyti(i,1)
c                   gradh(i,2) = 0.5/epsilon_ls * (1.0 
c     &                  + cos(pi*Sclr(i)/epsilon_ls)) * gyti(i,2) 
c                   gradh(i,3) = 0.5/epsilon_ls * (1.0 
c     &                  + cos(pi*Sclr(i)/epsilon_ls)) * gyti(i,3)
c                endif
             enddo              !end of the loop over npro
c     
c .. surface tension force calculation
c .. divide by density now as it gets multiplied in e3res.f, as surface
c    tension force is already in the form of force per unit volume
c     
             weber(:) = Bo
             sforce(:,1) = -(1.0/weber(:)) * divqi(:,idflow+1) !x-direction
     &            *gradh(:,1) /rho(:)
             sforce(:,2) = -(1.0/weber(:)) * divqi(:,idflow+1) !y-direction
     &            *gradh(:,2) /rho(:)
             sforce(:,3) = -(1.0/weber(:)) * divqi(:,idflow+1) !z-direction
     &            *gradh(:,3) /rho(:)          
c
          endif        ! end of the surface tension force calculation
       endif           ! diffusive flux computation
c
c Calculate strong form of pde as well as the source term
c      
       call e3resStrongPDE(
     &      aci,  u1,   u2,   u3,   Temp, rho,  xx,
     &            g1yi, g2yi, g3yi,
     &      rLui, src, divqi)
c
c.... take care of the surface tension force term here
c
       if (isurf .eq. 1) then  ! note multiplied by density in e3res.f 
          src(:,1) = src(:,1) + sforce(:,1)
          src(:,2) = src(:,2) + sforce(:,2)
          src(:,3) = src(:,3) + sforce(:,3)
       endif       

       return
       end

!> Calculate the variables for the scalar advection-diffusion
!! equation.

      subroutine e3ivarSclr (yl,          acl,       shpfun,
     &                      shgl,        xl,        xmudmi,
     &                      Sclr,        Sdot,      gradS,  
     &                      shg,         dxidx,     WdetJ,
     &                      u1,          u2,        u3,              
     &                      ql,          rLS ,       SrcR,
     &                      SrcL,        uMod,      dwl,
     &                      diffus,      srcRat)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
c
C     Argument variables
C
      REAL*8                acl,         diffus,      dwl,         dxidx
      REAL*8                grads,       ql,          rls,         sclr
      REAL*8                sdot,        shg,         shgl,        shpfun
      REAL*8                srcl,        srcr,        srcrat,      u1
      REAL*8                u2,          u3,          umod,        wdetj
      REAL*8                xl,          yl
C
C     Local variables
C
      INTEGER             id,          n
C
      REAL*8                divqi,       divs,        temp,        tmp
      REAL*8                x,           xx
C
c  passed arrays
c
      dimension yl(npro,nshl,ndof),        acl(npro,nshl,ndof), 
     &          Sclr(npro),                Sdot(npro),
     &          gradS(npro,nsd),           shpfun(npro,nshl),
     &          shgl(npro,nsd,nshl),       xl(npro,nenl,nsd),
     &          shg(npro,nshl,nsd),        dxidx(npro,nsd,nsd),
     &          WdetJ(npro),              
     &          u1(npro),                  u2(npro),
     &          u3(npro),                  divS(npro),
     &          ql(npro,nshl,nsd),         rLS(npro),
     &          SrcR(npro),                 SrcL(npro),
     &          dwl(npro,nshl),            diffus(npro),
     &          umod(npro,nsd), Temp(npro),xx(npro,nsd),
     &          divqi(npro)   
c
      dimension tmp(npro), srcRat(npro)
      real*8 rLui(npro,nsd),     aci(npro,nsd),
     &       g1yi(npro,nflow),   g2yi(npro,nflow),
     &       g3yi(npro,nflow),
     &       src(npro,nsd),      rho(npro),
     &       rmu(npro)
      real*8 uBar(npro,nsd), xmudmi(npro,ngauss)

c
c.... ------------->  Primitive variables at int. point  <--------------
c
c.... compute primitive variables
c
      u1   = zero
      u2   = zero
      u3   = zero
      Sclr = zero
c
      id=isclr+5
      do n = 1, nshl 
         u1   = u1   + shpfun(:,n) * yl(:,n,2)
         u2   = u2   + shpfun(:,n) * yl(:,n,3)
         u3   = u3   + shpfun(:,n) * yl(:,n,4)
         Sclr = Sclr + shpfun(:,n) * yl(:,n,id)
      enddo
c
c
c.... ----------------------->  dS/dt at int. point  <----------------------
c
      Sdot = zero
      do n = 1, nshl
         Sdot = Sdot + shpfun(:,n) * acl(:,n,id)
      enddo
c
c.... --------------------->  Element Metrics  <-----------------------
c

      call e3metric( xl,         shgl,        dxidx,  
     &               shg,        WdetJ)

c
c.... compute the global gradient of u and P
c
c
       gradS = zero
       do n = 1, nshl
          gradS(:,1) = gradS(:,1) + shg(:,n,1) * yl(:,n,id)
          gradS(:,2) = gradS(:,2) + shg(:,n,2) * yl(:,n,id)
          gradS(:,3) = gradS(:,3) + shg(:,n,3) * yl(:,n,id)
       enddo

       divS = zero
       if ( idiff >= 1 ) then
c
c.... compute divergence of diffusive flux vector, qi,i
c
          do n=1, nshl
             divS(:) = divS(:) + shg(:,n,1)*ql(:,n,1 ) 
     &                         + shg(:,n,2)*ql(:,n,2 ) 
     &                         + shg(:,n,3)*ql(:,n,3 ) 
          enddo
       endif                    ! diffusive flux computation

       if(consrv_sclr_conv_vel) then
c         Calculate uBar = u - TauM*L, where TauM is the momentum
c         stabilization factor and L is the momentum residual

          if(matflg(5,1).eq.2) then ! boussinesq body force
             Temp = zero
             do n = 1, nshl
                Temp = Temp + shpfun(:,n) * yl(:,n,5)
             enddo
          endif
          if(matflg(5,1).eq.3.or.matflg(6,1).eq.1) then
c     user-specified body force or coriolis force specified
             xx = zero
             do n  = 1,nenl
                xx(:,1) = xx(:,1)  + shpfun(:,n) * xl(:,n,1)
                xx(:,2) = xx(:,2)  + shpfun(:,n) * xl(:,n,2)
                xx(:,3) = xx(:,3)  + shpfun(:,n) * xl(:,n,3)
             enddo
          endif
          aci = zero
          do n = 1, nshl
             aci(:,1) = aci(:,1) + shpfun(:,n) * acl(:,n,2)
             aci(:,2) = aci(:,2) + shpfun(:,n) * acl(:,n,3)
             aci(:,3) = aci(:,3) + shpfun(:,n) * acl(:,n,4)
          enddo
          g1yi = zero
          g2yi = zero
          g3yi = zero
          do n = 1, nshl
             g1yi(:,1) = g1yi(:,1) + shg(:,n,1) * yl(:,n,1)
             g1yi(:,2) = g1yi(:,2) + shg(:,n,1) * yl(:,n,2)
             g1yi(:,3) = g1yi(:,3) + shg(:,n,1) * yl(:,n,3)
             g1yi(:,4) = g1yi(:,4) + shg(:,n,1) * yl(:,n,4)
c     
             g2yi(:,1) = g2yi(:,1) + shg(:,n,2) * yl(:,n,1)
             g2yi(:,2) = g2yi(:,2) + shg(:,n,2) * yl(:,n,2)
             g2yi(:,3) = g2yi(:,3) + shg(:,n,2) * yl(:,n,3)
             g2yi(:,4) = g2yi(:,4) + shg(:,n,2) * yl(:,n,4)
c     
             g3yi(:,1) = g3yi(:,1) + shg(:,n,3) * yl(:,n,1)
             g3yi(:,2) = g3yi(:,2) + shg(:,n,3) * yl(:,n,2)
             g3yi(:,3) = g3yi(:,3) + shg(:,n,3) * yl(:,n,3)
             g3yi(:,4) = g3yi(:,4) + shg(:,n,3) * yl(:,n,4)
          enddo
c          
          
          rho  = datmat(1,1,1)
          rmu = datmat(1,2,1)
                    
          divqi=zero  ! until we reconstruct q_flow for scalar solve
          call e3resStrongPDE(
     &         aci,  u1,   u2,   u3,   Temp, rho,  xx,
     &               g1yi, g2yi, g3yi,
     &         rLui, src, divqi)
          src(:,1)=u1           !
          src(:,2)=u2           ! store u in src memory
          src(:,3)=u3           !
c         e3uBar calculates Tau_M and assembles uBar
          call getdiff(dwl, yl, shpfun, xmudmi, xl, rmu, rho)
          call e3uBar(rho, src, dxidx, rLui, rmu, uBar)
          u1=ubar(:,1)          ! the entire scalar residual
          u2=ubar(:,2)          ! is based on the modified
          u3=ubar(:,3)          ! velocity for conservation
       endif
c
c.... Initialize uMod, the modified velocity uMod
c      We initialize it to u_i and then calculate
c      the correction in e3sourcesclr
c

       umod(:,1) = u1
       umod(:,2) = u2
       umod(:,3) = u3
c     
c.... no source terms
c
        srcRat = zero
        srcR   = zero
        srcL   = zero
c
c.... -------------------> Scalar residual  <-----------------
c

         rLS(:) = ( Sdot(:) +  (u1*gradS(:,1) + 
     &                              u2*gradS(:,2) +
     &                              u3*gradS(:,3)) )
     &        - divS(:)           

c
c.... return
c
       return
       end

!> This routine computes the left hand side tangent matrix at an 
!! integration point.
!!
!!  input:<BR>
!!  @param[in] u1(npro) X1-velocity
!!  @param[in] u2(npro) X2-velocity
!!  @param[in] u3(npro) X3-velocity
!!  @param[in] uBar(npro,3) U - tauM * Li
!!  @param[in] WdetJ(npro) Weighted jacobian determinant
!!  @param[in] rLui(npro,3) Total residual of NS equations
!!  @param[in] rmu(npro) Fluid viscosity
!!  @param[in] rho(npro) Fluid density
!!  @param[in] tauC(npro) Continuity tau
!!  @param[in] tauM(npro) Momentum tau
!!  @param[in] tauBar(npro) Additional tau
!!  @param[in] shpfun(npro,nshl) Element shpfun functions
!!  @param[in] shg(npro,nshl,3) Global grad of element shape functions
!!
!!  output:<BR>
!!  @param[out] xKebe(npro,9,nshl,nshl) Left hand side
!!  @param[out] xGoC(npro,4,nshl,nshl) Left hand side

      subroutine e3LHS ( u1,        u2,         u3,
     &                   uBar,      WdetJ,      rho,
     &                   rLui,      rmu,       
     &                   tauC,      tauM,       tauBar,
     &                   shpfun,    shg,        xKebe,
     &                   xGoC )

        include "global.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"
C    
C	 Argument variables
C
      REAL*8                rho,         rlui,        rmu,         shg
      REAL*8                shpfun,      taubar,      tauc,        taum
      REAL*8                u1,          u2,          u3,          ubar
      REAL*8                wdetj,       xgoc,        xkebe
C
C     Local variables
C
      REAL*8                t1,          t2,          t3,          tlw
      REAL*8                tmp,         tmp1,        tmp2
      dimension u1(npro),         u2(npro),       u3(npro),
     &          uBar(npro,3),     WdetJ(npro),    rho(npro),
     &          rLui(npro,3),     rmu(npro),   
     &          tauC(npro),       tauM(npro),     tauBar(npro),
     &          shpfun(npro,nshl),shg(npro,nshl,3)
      
      dimension xKebe(npro,9,nshl,nshl), xGoC(npro,4,nshl,nshl)
c
c.... local declarations
c
      dimension t1(npro,3),       t2(npro,3),      t3(npro,3),
     &          tmp1(npro),       tmp2(npro),    
     &          tmp(npro),        tlW(npro)

      integer   aa, b
      
      real*8    lhmFct, lhsFct,           tsFct(npro)
      
      lhsFct = alfi * gami * Delt(itseq)
      lhmFct = almi * (one - flmpl) 
c
c.... scale variables for efficiency
c
      tlW      = lhsFct * WdetJ     
      tmp1      = tlW * rho
      tauM      = tlW * tauM 
      tauC      = tlW * tauC 
      rmu       = tlW * rmu 
      tsFct     = lhmFct * WdetJ * rho
      if(iconvflow.eq.2) then  ! 2 is ubar form 3 is cons form but ubar tang. 
         tauBar    = lhsFct * WdetJ * tauBar 
         uBar(:,1) = tmp1 * uBar(:,1)
         uBar(:,2) = tmp1 * uBar(:,2)
         uBar(:,3) = tmp1 * uBar(:,3)
      else
         tauBar = zero  !lazy tangent...if effective code it
         uBar(:,1) = tmp1 * u1(:)
         uBar(:,2) = tmp1 * u2(:)
         uBar(:,3) = tmp1 * u3(:)
      endif

c
c.... compute mass and convection terms
c
      do b = 1, nshl
         t1(:,1) = uBar(:,1) * shg(:,b,1)
     &           + uBar(:,2) * shg(:,b,2)
     &           + uBar(:,3) * shg(:,b,3)
c
c t1=ubar_k N^b,k*rho*alpha_f*gamma*deltat*WdetJ  
c
         do aa = 1, nshl
            tmp1 = tsFct * shpfun(:,aa) * shpfun(:,b)
            tmp2 = tmp1 + t1(:,1) * shpfun(:,aa)
c
c tmp1=alpha_m*(1-lmp)*WdetJ*N^aN^b*rho   the time term CORRECT
c tmp2=tmp1+N^a*ubar_k N^b,k*rho*alpha_f*gamma*deltat*WdetJ   the 
c    second term is convective term CORRECT
c            
            xKebe(:,1,aa,b) = xKebe(:,1,aa,b) + tmp2
            xKebe(:,5,aa,b) = xKebe(:,5,aa,b) + tmp2
            xKebe(:,9,aa,b) = xKebe(:,9,aa,b) + tmp2
         enddo
      enddo
c
c.... compute the rest of K (symmetric terms)
c      
      do b = 1, nshl
         
         t1(:,1) = tauC * shg(:,b,1)
         t1(:,2) = tauC * shg(:,b,2)
         t1(:,3) = tauC * shg(:,b,3)

c t1 is tauC*N^b_i,j*alpha_f*gamma*deltat*WdetJ
         
         t2(:,1) = rmu  * shg(:,b,1)
         t2(:,2) = rmu  * shg(:,b,2)
         t2(:,3) = rmu  * shg(:,b,3)
c t2 is mu*N^b_j,k*alpha_f*gamma*deltat*WdetJ
      
         tmp1 = tauM   * ( u1 * shg(:,b,1)  
     &                   + u2 * shg(:,b,2) 
     &                   + u3 * shg(:,b,3) )*rho
c tmp1 is tauM*(rho u_m N^b_j,m)*alpha_f*gamma*deltat*WdetJ

         tmp2 = tauBar * ( rLui(:,1) * shg(:,b,1)
     &                   + rLui(:,2) * shg(:,b,2)
     &                   + rLui(:,3) * shg(:,b,3) )
c tmp2 is taubar*(L_m N^b_j,m)*alpha_f*gamma*deltat*WdetJ
         t3(:,1) = t2(:,1) + tmp1 * u1 + tmp2 * rLui(:,1)
         t3(:,2) = t2(:,2) + tmp1 * u2 + tmp2 * rLui(:,2)
         t3(:,3) = t2(:,3) + tmp1 * u3 + tmp2 * rLui(:,3)

c t3 is   (mu*N^b_j,k + u_k tauM*(rho u_m N^b_j,m)+ L_k*taubar*(L_mN^b_j,m ) 
c   *alpha_f*gamma*deltat*WdetJ     which isline 2 page 40 of whiting
c   ALMOST (waiting to get hit with N^a_{i,k}
c mu correct NOW (wrong before) and rho weight on tauM term
c
c.... first do the (nodal) diagonal blocks         
c
         aa  = b
         
         tmp = t3(:,1) * shg(:,aa,1)
     &       + t3(:,2) * shg(:,aa,2)
     &       + t3(:,3) * shg(:,aa,3)
c previous command is the N^a_{i,k} dot product with t3 defined above

         xKebe(:,1,aa,b) = xKebe(:,1,aa,b) + tmp
     &                      + t1(:,1) * shg(:,aa,1)
     &                      + t2(:,1) * shg(:,aa,1)
         xKebe(:,5,aa,b) = xKebe(:,5,aa,b) + tmp
     &                      + t1(:,2) * shg(:,aa,2)
     &                      + t2(:,2) * shg(:,aa,2)
         xKebe(:,9,aa,b) = xKebe(:,9,aa,b) + tmp
     &                      + t1(:,3) * shg(:,aa,3)
     &                      + t2(:,3) * shg(:,aa,3)
c
         tmp1               = t1(:,1) * shg(:,aa,2)
     &                      + t2(:,2) * shg(:,aa,1)
         xKebe(:,2,aa,b) = xKebe(:,2,aa,b) + tmp1 
         xKebe(:,4,b,aa) = xKebe(:,4,b,aa) + tmp1 
c
         tmp1               = t1(:,1) * shg(:,aa,3)
     &                      + t2(:,3) * shg(:,aa,1)
         xKebe(:,3,aa,b) = xKebe(:,3,aa,b) + tmp1 
         xKebe(:,7,b,aa) = xKebe(:,7,b,aa) + tmp1 
c
         tmp1               = t1(:,2) * shg(:,aa,3)
     &                      + t2(:,3) * shg(:,aa,2)
         xKebe(:,6,aa,b) = xKebe(:,6,aa,b) + tmp1 
         xKebe(:,8,b,aa) = xKebe(:,8,b,aa) + tmp1 
c
c.... now the off-diagonal (nodal) blocks
c
         do aa = b+1, nshl
            tmp             = t3(:,1) * shg(:,aa,1)
     &                      + t3(:,2) * shg(:,aa,2)
     &                      + t3(:,3) * shg(:,aa,3)
c
            tmp1            = tmp
     &                      + t1(:,1) * shg(:,aa,1)
     &                      + t2(:,1) * shg(:,aa,1)
            xKebe(:,1,aa,b) = xKebe(:,1,aa,b) + tmp1
            xKebe(:,1,b,aa) = xKebe(:,1,b,aa) + tmp1
c
            tmp1            = tmp
     &                      + t1(:,2) * shg(:,aa,2)
     &                      + t2(:,2) * shg(:,aa,2)
            xKebe(:,5,aa,b) = xKebe(:,5,aa,b) + tmp1
            xKebe(:,5,b,aa) = xKebe(:,5,b,aa) + tmp1
c
            tmp1            = tmp
     &                      + t1(:,3) * shg(:,aa,3)
     &                      + t2(:,3) * shg(:,aa,3)
            xKebe(:,9,aa,b) = xKebe(:,9,aa,b) + tmp1
            xKebe(:,9,b,aa) = xKebe(:,9,b,aa) + tmp1
c
c.... ( i != j )
c
            tmp1               = t1(:,1) * shg(:,aa,2)
     &                         + t2(:,2) * shg(:,aa,1)
            xKebe(:,2,aa,b) = xKebe(:,2,aa,b) + tmp1
            xKebe(:,4,b,aa) = xKebe(:,4,b,aa) + tmp1
c
            tmp1               = t1(:,1) * shg(:,aa,3)
     &                         + t2(:,3) * shg(:,aa,1)
            xKebe(:,3,aa,b) = xKebe(:,3,aa,b) + tmp1
            xKebe(:,7,b,aa) = xKebe(:,7,b,aa) + tmp1
c
            tmp1               = t1(:,2) * shg(:,aa,1)
     &                         + t2(:,1) * shg(:,aa,2)
            xKebe(:,4,aa,b) = xKebe(:,4,aa,b) + tmp1
            xKebe(:,2,b,aa) = xKebe(:,2,b,aa) + tmp1
c
            tmp1               = t1(:,2) * shg(:,aa,3)
     &                         + t2(:,3) * shg(:,aa,2)
            xKebe(:,6,aa,b) = xKebe(:,6,aa,b) + tmp1
            xKebe(:,8,b,aa) = xKebe(:,8,b,aa) + tmp1
c
            tmp1               = t1(:,3) * shg(:,aa,1)
     &                         + t2(:,1) * shg(:,aa,3)
            xKebe(:,7,aa,b) = xKebe(:,7,aa,b) + tmp1
            xKebe(:,3,b,aa) = xKebe(:,3,b,aa) + tmp1
c
            tmp1               = t1(:,3) * shg(:,aa,2)
     &                         + t2(:,2) * shg(:,aa,3)
            xKebe(:,8,aa,b) = xKebe(:,8,aa,b) + tmp1
            xKebe(:,6,b,aa) = xKebe(:,6,b,aa) + tmp1
c
         enddo
      enddo
c
c.... compute G   Nai Nbp,j
c
      
      do b = 1, nshl
         t1(:,1) = tlW * shg(:,b,1)
         t1(:,2) = tlW * shg(:,b,2)
         t1(:,3) = tlW * shg(:,b,3)
         do aa = 1, nshl
            xGoC(:,1,aa,b) = xGoC(:,1,aa,b) + t1(:,1) * shpfun(:,aa)  
            xGoC(:,2,aa,b) = xGoC(:,2,aa,b) + t1(:,2) * shpfun(:,aa)  
            xGoC(:,3,aa,b) = xGoC(:,3,aa,b) + t1(:,3) * shpfun(:,aa)  
         enddo
      enddo
c
c.... compute C
c we divide by rho because the L on the weight space is density divided
c      form
c
      tauM=tauM/rho
      do b = 1, nshl
         t1(:,1) = tauM * shg(:,b,1)
         t1(:,2) = tauM * shg(:,b,2)
         t1(:,3) = tauM * shg(:,b,3)
         do aa = b, nshl
            xGoC(:,4,aa,b) = xGoC(:,4,aa,b) 
     &                      + t1(:,1) * shg(:,aa,1)
     &                      + t1(:,2) * shg(:,aa,2)
     &                      + t1(:,3) * shg(:,aa,3)
         enddo
      enddo
      
c
c.... return
c
      return
      end

!> Calculate the tangent matrix for the advection-diffusion equation

      subroutine e3LHSSclr ( uMod,      giju,       dcFct,
     &                       Sclr,      Sdot,       gradS,  
     &                       WdetJ,     rLS,        tauS,
     &                       shpfun,    shg,        src,
     &                       diffus,
     &                       xSebe )

c
        include "global.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"
C
C     Argument variables
C
      REAL*8                dcfct,       giju
C
C     Local variables
C
      REAL*8                t1,          tmp,         tmp1,        tmp2
C
      real*8    uMod(npro,nsd),
     &          Sclr(npro),       Sdot(npro),   gradS(npro,nsd),
     &          WdetJ(npro),      rLS(npro),        rho(npro), 
     &          tauS(npro),       shpfun(npro,nshl),  
     &          src(npro),        shg(npro,nshl,3),
     &			xSebe(npro,nshl,nshl)
      
      real*8    diffus(npro),  cp,  kptmp(npro),tauSo(npro)

c
c.... local declarations
c
      dimension t1(npro,3),       tmp1(npro),       tmp2(npro),
     &          tmp(npro),        dcFct(npro),      giju(npro,6)

      integer   aa, b
      
      real*8    lhsFct,           tsFct(npro)
      
      lhsFct = alfi * gami * Delt(itseq)
c
c.... scale variables for efficiency
c     
      tauSo     = tauS
      tauS      = lhsFct * WdetJ * tauS 
      kptmp     = lhsFct * WdetJ * diffus
      tsFct     = almi   * WdetJ * (one - flmpl)
      src       = src    * WdetJ * lhsFct
c
c.... compute mass and convection terms
c
      do b = 1, nshl
         t1(:,1) = WdetJ * ( uMod(:,1) * shg(:,b,1)
     &                     + uMod(:,2) * shg(:,b,2)
     &                     + uMod(:,3) * shg(:,b,3) )
         t1(:,2) = t1(:,1) * tauSo
         do aa = 1, nshl
            tmp1 = shpfun(:,aa) * shpfun(:,b)
            tmp2 = shpfun(:,aa) * lhsFct
            xSebe(:,aa,b) = xSebe(:,aa,b) + tmp1 * (tsFct + src)
     &                                    + tmp2 * t1(:,1)
c
c.... compute mass term for stab u_j N_{a,j} tau N_b (note that a and b
c            flipped on both sides below)
c
            xSebe(:,b,aa) = xSebe(:,b,aa) + t1(:,2)*shpfun(:,aa)
         enddo
      enddo
c
c.... compute the rest of S (symmetric terms)
c      
      do b = 1, nshl
         tmp     = tauS(:) 
     &             * ( uMod(:,1) * shg(:,b,1)
     &               + uMod(:,2) * shg(:,b,2)
     &               + uMod(:,3) * shg(:,b,3) )

         t1(:,1) = kptmp * shg(:,b,1) + uMod(:,1) * tmp
         t1(:,2) = kptmp * shg(:,b,2) + uMod(:,2) * tmp
         t1(:,3) = kptmp * shg(:,b,3) + uMod(:,3) * tmp
         if (idcsclr(1) .ne. 0) then
            if ((idcsclr(2).eq.1 .and. isclr.eq.1) .or. 
     &           (idcsclr(2).eq.2 .and. isclr.eq.2)) then ! scalar with dc
c
               tmp = WdetJ * dcFct * lhsFct
c
               giju(:,1)	= tmp * giju(:,1)
               giju(:,2)	= tmp * giju(:,2)
               giju(:,3)	= tmp * giju(:,3)
               giju(:,4)	= tmp * giju(:,4)
               giju(:,5)	= tmp * giju(:,5)
               giju(:,6)	= tmp * giju(:,6)
c       
               t1(:,1) = t1(:,1) + giju(:,1) * shg(:,b,1) 
     2                           + giju(:,4) * shg(:,b,2) 
     3			         + giju(:,6) * shg(:,b,3)
               t1(:,2) = t1(:,2) + giju(:,4) * shg(:,b,1) 
     2                           + giju(:,2) * shg(:,b,2) 
     3			         + giju(:,5) * shg(:,b,3)
               t1(:,3) = t1(:,3) + giju(:,6) * shg(:,b,1) 
     2                           + giju(:,5) * shg(:,b,2) 
     3			         + giju(:,3) * shg(:,b,3)
            endif
         endif                  !end of idcsclr
c
c.... first do the (nodal) diagonal blocks         
c
         aa  = b
         
         xSebe(:,aa,b) = xSebe(:,aa,b) + t1(:,1) * shg(:,aa,1)
     &                                 + t1(:,2) * shg(:,aa,2)
     &                                 + t1(:,3) * shg(:,aa,3)

c
c.... now the off-diagonal (nodal) blocks
c
         do aa = b+1, nshl
            tmp             = t1(:,1) * shg(:,aa,1)
     &                      + t1(:,2) * shg(:,aa,2)
     &                      + t1(:,3) * shg(:,aa,3)
c
            xSebe(:,aa,b) = xSebe(:,aa,b) + tmp
            xSebe(:,b,aa) = xSebe(:,b,aa) + tmp
c
         enddo
      enddo
      
c
c.... return
c
      return
      end

!> Compute the metrics of the mapping from global to local 
!! coordinates and the jacobian of the mapping (weighted by 
!! the quadrature weight

      subroutine e3metric(  xl,      shgl,     dxidx,
     &                      shg,     WdetJ)

        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Local variables
C
      INTEGER             n
C      
      real*8     xl(npro,nenl,nsd),    shgl(npro,nsd,nshl),
     &           dxidx(npro,nsd,nsd),  shg(npro,nshl,nsd), 
     &           WdetJ(npro)

      real*8     dxdxi(npro,nsd,nsd),  tmp(npro)

c
c.... compute the deformation gradient
c
      dxdxi = zero
c
       do n = 1, nenl
          dxdxi(:,1,1) = dxdxi(:,1,1) + xl(:,n,1) * shgl(:,1,n)
          dxdxi(:,1,2) = dxdxi(:,1,2) + xl(:,n,1) * shgl(:,2,n)
          dxdxi(:,1,3) = dxdxi(:,1,3) + xl(:,n,1) * shgl(:,3,n)
          dxdxi(:,2,1) = dxdxi(:,2,1) + xl(:,n,2) * shgl(:,1,n)
          dxdxi(:,2,2) = dxdxi(:,2,2) + xl(:,n,2) * shgl(:,2,n)
          dxdxi(:,2,3) = dxdxi(:,2,3) + xl(:,n,2) * shgl(:,3,n)
          dxdxi(:,3,1) = dxdxi(:,3,1) + xl(:,n,3) * shgl(:,1,n)
          dxdxi(:,3,2) = dxdxi(:,3,2) + xl(:,n,3) * shgl(:,2,n)
          dxdxi(:,3,3) = dxdxi(:,3,3) + xl(:,n,3) * shgl(:,3,n)
       enddo
c
c.... compute the inverse of deformation gradient
c
       dxidx(:,1,1) =   dxdxi(:,2,2) * dxdxi(:,3,3) 
     &                - dxdxi(:,3,2) * dxdxi(:,2,3)
       dxidx(:,1,2) =   dxdxi(:,3,2) * dxdxi(:,1,3) 
     &                - dxdxi(:,1,2) * dxdxi(:,3,3)
       dxidx(:,1,3) =  dxdxi(:,1,2) * dxdxi(:,2,3) 
     &                - dxdxi(:,1,3) * dxdxi(:,2,2)
       tmp          = one / ( dxidx(:,1,1) * dxdxi(:,1,1) 
     &                       + dxidx(:,1,2) * dxdxi(:,2,1)  
     &                       + dxidx(:,1,3) * dxdxi(:,3,1) )
       dxidx(:,1,1) = dxidx(:,1,1) * tmp
       dxidx(:,1,2) = dxidx(:,1,2) * tmp
       dxidx(:,1,3) = dxidx(:,1,3) * tmp
       dxidx(:,2,1) = (dxdxi(:,2,3) * dxdxi(:,3,1) 
     &                - dxdxi(:,2,1) * dxdxi(:,3,3)) * tmp
       dxidx(:,2,2) = (dxdxi(:,1,1) * dxdxi(:,3,3) 
     &                - dxdxi(:,3,1) * dxdxi(:,1,3)) * tmp
       dxidx(:,2,3) = (dxdxi(:,2,1) * dxdxi(:,1,3) 
     &                - dxdxi(:,1,1) * dxdxi(:,2,3)) * tmp
       dxidx(:,3,1) = (dxdxi(:,2,1) * dxdxi(:,3,2) 
     &                - dxdxi(:,2,2) * dxdxi(:,3,1)) * tmp
       dxidx(:,3,2) = (dxdxi(:,3,1) * dxdxi(:,1,2) 
     &                - dxdxi(:,1,1) * dxdxi(:,3,2)) * tmp
       dxidx(:,3,3) = (dxdxi(:,1,1) * dxdxi(:,2,2) 
     &                - dxdxi(:,1,2) * dxdxi(:,2,1)) * tmp
c
       WdetJ = Qwt(lcsyst,intp) / tmp
c
c.... compute the global gradient of shape-functions
c
       do n = 1, nshl
          shg(:,n,1) = shgl(:,1,n) * dxidx(:,1,1) + 
     &                 shgl(:,2,n) * dxidx(:,2,1) +
     &                 shgl(:,3,n) * dxidx(:,3,1)
          shg(:,n,2) = shgl(:,1,n) * dxidx(:,1,2) + 
     &                 shgl(:,2,n) * dxidx(:,2,2) +
     &                 shgl(:,3,n) * dxidx(:,3,2) 
          shg(:,n,3) = shgl(:,1,n) * dxidx(:,1,3) + 
     &                 shgl(:,2,n) * dxidx(:,2,3) +
     &                 shgl(:,3,n) * dxidx(:,3,3) 
       enddo

       return
       end

!> This routine computes the element contribution to the 
!! diffusive flux vector and the lumped mass matrix.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Y variables
!! @param[in] shp(nen,ngauss) Element shape-functions
!! @param[in] shgl(nsd,nen,ngauss) Element local-grad-shape-functions
!! @param[in] xl(npro,nshl,nsd) Nodal coordinates at current step
!!  
!! output:<BR>
!! @param[out] ql(npro,nshl,idflx) Element RHS diffusion residual 
!! @param[out] rmassl(npro,nshl) Element lumped mass matrix

        subroutine e3q (yl,      dwl,     shp,     shgl,
     &                  xl,      ql,      rmassl, 
     &                  xmudmi,  sgn )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      REAL*8                dwl,         ql,          rmassl,      sgn
      REAL*8                shgl,        shp,         xl,          xmudmi
      REAL*8                yl
C
C     Local variables
C
      INTEGER             i,           n
C
      REAL*8                alph1,       alph2,       dxidx,       g1yi
      REAL*8                g1yti,       g2yi,        g2yti,       g3yi
      REAL*8                g3yti,       qdi,         rmu,         shape
      REAL*8                shdrv,       shg,         shpsum,      wdetj
c
        dimension yl(npro,nshl,ndof),     dwl(npro,nenl),
     &            shp(nshl,ngauss),      shgl(nsd,nshl,ngauss),
     &            xl(npro,nenl,nsd),
     &            ql(npro,nshl,idflx),  rmassl(npro,nshl),
     &            xmudmi(npro,ngauss)
c
c local arrays
c
        dimension g1yi(npro,nflow),           g2yi(npro,nflow),
     &            g3yi(npro,nflow),           shg(npro,nshl,nsd),
     &            dxidx(npro,nsd,nsd),       WdetJ(npro),
     &            rmu(npro) 
c
        dimension qdi(npro,idflx),alph1(npro),alph2(npro)
c
        dimension sgn(npro,nshl),          shape(npro,nshl),
     &            shdrv(npro,nsd,nshl),    shpsum(npro)

        real*8 tmp(npro)
c
c.... for surface tension
c     
        dimension g1yti(npro),          g2yti(npro),
     &            g3yti(npro)
        integer idflow
c
c.... loop through the integration points
c
        
        
        alph1 = 0.d0
        alph2 = 0.d0
        
        do intp = 1, ngauss
        if (Qwt(lcsyst,intp) .eq. zero) cycle          ! precaution
c     
        call getshp(shp,          shgl,      sgn, 
     &              shape,        shdrv)
        
c
c.... initialize
c
        qdi = zero
c
c
c.... calculate the integration variables necessary for the
c     formation of q
c

        call e3qvar   (yl,        shdrv,   
     &                 xl,           g1yi,
     &                 g2yi,      g3yi,         shg,
     &                 dxidx,     WdetJ )      
c  
        idflow = 9   ! we ALWAYS save space for tau_{ij} in q_i 
                     ! even if idiff is not greater than 1

        if(idiff >= 1) then   !so taking care of all the idiff=1,3
c
c.... compute diffusive fluxes 
c
c.... compute the viscosity
c
        call getdiff(dwl, yl, shape, xmudmi, xl, rmu, tmp)
c
c.... diffusive flux in x1-direction
c
        qdi(:,1) =  two * rmu *  g1yi(:,2)
        qdi(:,4) =        rmu * (g1yi(:,3) + g2yi(:,2))
        qdi(:,7) =        rmu * (g1yi(:,4) + g3yi(:,2))
c     
c.... diffusive flux in x2-direction
c
        qdi(:,2) =        rmu * (g1yi(:,3) + g2yi(:,2))
        qdi(:,5) =  two * rmu *  g2yi(:,3)
        qdi(:,8) =        rmu * (g2yi(:,4) + g3yi(:,3))
c     
c.... diffusive flux in x3-direction
c
        qdi(:,3) =        rmu * (g1yi(:,4) + g3yi(:,2))
        qdi(:,6)=         rmu * (g2yi(:,4) + g3yi(:,3))
        qdi(:,9)=  two * rmu *  g3yi(:,4)
c
c
c.... assemble contribution of qdi to ql,i.e., contribution to 
c     each element node
c     
        do i=1,nshl
           ql(:,i,1 ) = ql(:,i,1 )+ shape(:,i)*WdetJ*qdi(:,1 )
           ql(:,i,2 ) = ql(:,i,2 )+ shape(:,i)*WdetJ*qdi(:,2 )
           ql(:,i,3 ) = ql(:,i,3 )+ shape(:,i)*WdetJ*qdi(:,3 )

           ql(:,i,4 ) = ql(:,i,4 )+ shape(:,i)*WdetJ*qdi(:,4 )
           ql(:,i,5 ) = ql(:,i,5 )+ shape(:,i)*WdetJ*qdi(:,5 )
           ql(:,i,6 ) = ql(:,i,6 )+ shape(:,i)*WdetJ*qdi(:,6 )

           ql(:,i,7 ) = ql(:,i,7 )+ shape(:,i)*WdetJ*qdi(:,7 )
           ql(:,i,8 ) = ql(:,i,8 )+ shape(:,i)*WdetJ*qdi(:,8 )
           ql(:,i,9 ) = ql(:,i,9 )+ shape(:,i)*WdetJ*qdi(:,9 )

        enddo
c
c.... compute and assemble the element contribution to the lumped
c     mass matrix
c
c
c.... row sum technique
c
        if ( idiff == 1 ) then
           do i=1,nshl
              rmassl(:,i) = rmassl(:,i) + shape(:,i)*WdetJ
           enddo
        endif
c
c.... "special lumping technique" (Hughes p. 445)
c
        if ( idiff == 3 ) then
           shpsum = zero
           do i=1,nshl
              shpsum = shpsum + shape(:,i)*shape(:,i)
              rmassl(:,i)=rmassl(:,i)+shape(:,i)*shape(:,i)*WdetJ
           enddo
           alph1 = alph1+WdetJ
           alph2 = alph2+shpsum*WdetJ
        endif
      endif                     ! end of idiff=1 .or. 3 
c
      if(isurf .eq. 1) then
c
c.... initialize
c
        g1yti   = zero
        g2yti   = zero
        g3yti   = zero
c
c.... calculate the integration variables necessary for the
c     formation of q
c
c.... compute the global gradient of Yt-variables, assuming 6th entry as 
c.... the phase indicator function 
c
c  Yt_{,x_i}=SUM_{a=1}^nshl (N_{a,x_i}(int) Yta)
c
        do n = 1, nshl
          g1yti(:)  = g1yti(:)  + shg(:,n,1) * yl(:,n,6)
          g2yti(:)  = g2yti(:)  + shg(:,n,2) * yl(:,n,6)
          g3yti(:)  = g3yti(:)  + shg(:,n,3) * yl(:,n,6)
        enddo
c
c    computing N_{b}*N_{a,x_i)*yta*WdetJ
c
        do i=1,nshl
           ql(:,i,idflow+1)  = ql(:,i,idflow+1)  
     &                       + shape(:,i)*WdetJ*g1yti
           ql(:,i,idflow+2)  = ql(:,i,idflow+2)  
     &                       + shape(:,i)*WdetJ*g2yti
           ql(:,i,idflow+3)  = ql(:,i,idflow+3)  
     &                       + shape(:,i)*WdetJ*g3yti
           rmassl(:,i) = rmassl(:,i) + shape(:,i)*WdetJ
        enddo
      endif  !end of the isurf  
c
c.... end of the loop over integration points
c
      enddo
c
c.... normalize the mass matrix for idiff == 3
c
      if ( idiff == 3 ) then
         do i=1,nshl
            rmassl(:,i) = rmassl(:,i)*alph1/alph2
         enddo
      endif
      

c
c.... return
c
       return
       end

!> This routine computes the element contribution to the 
!! diffusive flux vector and the lumped mass matrix.

        subroutine e3qSclr (yl,      dwl,     shp,     shgl,
     &                      xl,      ql,      rmassl, 
     &                      sgn )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      REAL*8                dwl,         ql,          rmassl,      sgn
      REAL*8                shgl,        shp,         xl,          yl
C
C     Local variables
C
      INTEGER             i
C
      REAL*8                alph1,       alph2,       dxidx,       gradt
      REAL*8                qdi,         shape,       shdrv,       shpsum
      REAL*8                wdetj
c
        dimension yl(npro,nshl,ndof),    dwl(npro,nshl),
     &            shp(nshl,ngauss),      shgl(nsd,nshl,ngauss),
     &            xl(npro,nenl,nsd),
     &            ql(npro,nshl,nsd),     rmassl(npro,nshl)
c
c local arrays
c
        dimension gradT(npro,nsd),     
     &            dxidx(npro,nsd,nsd),       WdetJ(npro)
c
        dimension qdi(npro,nsd),alph1(npro),alph2(npro)
c
        dimension sgn(npro,nshl),          shape(npro,nshl),
     &            shdrv(npro,nsd,nshl),    shpsum(npro)

        real*8 diffus(npro)
c
c.... loop through the integration points
c
        
        
        alph1 = 0.d0
        alph2 = 0.d0
        
        do intp = 1, ngauss
        if (Qwt(lcsyst,intp) .eq. zero) cycle          ! precaution
c     
        call getshp(shp,          shgl,      sgn, 
     &              shape,        shdrv)
        
c
c.... initialize
c
        qdi = zero
c
c
c.... calculate the integration variables necessary for the
c     formation of q 
c
        call e3qvarSclr   (yl,        shdrv,   
     &                     xl,        gradT,
     &                     dxidx,     WdetJ )        

c
c.... compute diffusive flux vector at this integration point
c
        call getdiffsclr(shape, dwl, yl, diffus)

c
c.... diffusive flux 
c
        qdi(:,1) =  diffus * gradT(:,1)
        qdi(:,2) =  diffus * gradT(:,2)
        qdi(:,3) =  diffus * gradT(:,3)
c
c
c.... assemble contribution of qdi to ql,i.e., contribution to 
c     each element node
c     
        do i=1,nshl
           ql(:,i,1 ) = ql(:,i,1 )+ shape(:,i)*WdetJ*qdi(:,1 )
           ql(:,i,2 ) = ql(:,i,2 )+ shape(:,i)*WdetJ*qdi(:,2 )
           ql(:,i,3 ) = ql(:,i,3 )+ shape(:,i)*WdetJ*qdi(:,3 )

        enddo
c
c.... compute and assemble the element contribution to the lumped
c     mass matrix
c
c
c.... row sum technique
c
        if ( idiff == 1 ) then
           do i=1,nshl
              rmassl(:,i) = rmassl(:,i) + shape(:,i)*WdetJ
           enddo
        endif
c
c.... "special lumping technique" (Hughes p. 445)
c
        if ( idiff == 3 ) then
           shpsum = zero
           do i=1,nshl
              shpsum = shpsum + shape(:,i)*shape(:,i)
              rmassl(:,i)=rmassl(:,i)+shape(:,i)*shape(:,i)*WdetJ
           enddo
           alph1 = alph1+WdetJ
           alph2 = alph2+shpsum*WdetJ
        endif
c
c.... end of the loop over integration points
c
      enddo
c
c.... normalize the mass matrix for idiff == 3
c
      if ( idiff == 3 ) then
         do i=1,nshl
            rmassl(:,i) = rmassl(:,i)*alph1/alph2
         enddo
      endif
c
c.... return
c
       return
       end

!> This routine computes the local diffusive flux vector using a 
!! local projection algorithm
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Y variables
!! @param[in] shp(nen,ngauss) Element shape-functions
!! @param[in] shgl(nsd,nen,ngauss) Element local-grad-shape-functions
!! @param[in] xl(npro,nshape,nsd) Nodal coordinates at current step
!! @param[in] sgn(npro,nshl) Signs for reversed shape functions
!!  
!! output:<BR>
!! @param[out]  ql(npro,nshl,nsd*nsd) Element RHS diffusion residual 

      subroutine e3ql (yl,      dwl,     shp,     shgl,
     &                 xl,      ql,      xmudmi,
     &                 sgn )
      use local_mass
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
C     Argument variables
C
      REAL*8                dwl,         ql,          sgn,         shgl
      REAL*8                shp,         xl,          xmudmi,      yl
C
C     Local variables
C
      INTEGER             i,        iel,         indx
      INTEGER             j
C
      REAL*8                dxidx,       g1yi,        g2yi,        g3yi
      REAL*8                qdi,         qrl,	d
      REAL*8                rmass,       rminv,       rmu,         shape
      REAL*8                shdrv,       shg
      REAL*8                wdetj
C
      dimension yl(npro,nshl,ndof),        dwl(npro,nshl),
     &          shp(nshl,ngauss),          shgl(nsd,nshl,ngauss),
     &          xl(npro,nenl,nsd),         sgn(npro,nshl),
     &          ql(npro,nshl,idflx), xmudmi(npro,ngauss)
c
c local arrays
c
      dimension g1yi(npro,ndof),           g2yi(npro,ndof),
     &          g3yi(npro,ndof),           shg(npro,nshl,nsd),
     &          dxidx(npro,nsd,nsd),       WdetJ(npro),
     &          rmu(npro),
     &          rminv(npro,nshl,nshl),
     &          qrl(npro,nshl,nsd*nsd)
c
      dimension qdi(npro,nsd*nsd),    shape(npro,nshl),
     &          shdrv(npro,nsd,nshl),      indx(nshl),
     &          rmass(npro,nshl,nshl)


        real*8 tmp(npro)
c
c.... loop through the integration points
c
      rminv = zero
      rmass = zero
      qrl   = zero
        
      do intp = 1, ngauss

         call getshp(shp, shgl, sgn, shape, shdrv)

         qdi = zero
c
c.... calculate the integration variables 
c    
c
         call e3qvar   (yl,           shdrv,   
     &                  xl,           g1yi,
     &                  g2yi,         g3yi,         shg,
     &                  dxidx,        WdetJ )

         call getdiff(dwl,  yl, shape, xmudmi, xl,rmu, tmp)
c
c.... diffusive flux in x1-direction
c
         qdi(:,1) =  two * rmu *  g1yi(:,2)
         qdi(:,4) =        rmu * (g1yi(:,3) + g2yi(:,2))
         qdi(:,7) =        rmu * (g1yi(:,4) + g3yi(:,2))
c
c.... diffusive flux in x2-direction
c
         qdi(:,2) =        rmu * (g1yi(:,3) + g2yi(:,2))
         qdi(:,5) =  two * rmu *  g2yi(:,3)
         qdi(:,8) =        rmu * (g2yi(:,4) + g3yi(:,3))
c     
c.... diffusive flux in x3-direction
c
         qdi(:,3) =        rmu * (g1yi(:,4) + g3yi(:,2))
         qdi(:,6)=        rmu * (g2yi(:,4) + g3yi(:,3))
         qdi(:,9)=  two * rmu *  g3yi(:,4)
c
c
c.... assemble contribution of qdi to qrl,i.e., contribution to 
c     each element shape function
c
         tmp = Qwt(lcsyst,intp)
         if (lcsyst .eq. 1) then 
            tmp = tmp*(three/four)
         endif
c
c reconsider this when hierarchic wedges come into code WDGCHECK
c
        
         do i=1,nshl
            qrl(:,i,1 ) = qrl(:,i,1 )+ shape(:,i)*tmp*qdi(:,1 )
            qrl(:,i,2 ) = qrl(:,i,2 )+ shape(:,i)*tmp*qdi(:,2 )
            qrl(:,i,3 ) = qrl(:,i,3 )+ shape(:,i)*tmp*qdi(:,3 )
            
            qrl(:,i,4 ) = qrl(:,i,4 )+ shape(:,i)*tmp*qdi(:,4 )
            qrl(:,i,5 ) = qrl(:,i,5 )+ shape(:,i)*tmp*qdi(:,5 )
            qrl(:,i,6 ) = qrl(:,i,6 )+ shape(:,i)*tmp*qdi(:,6 )

            qrl(:,i,7 ) = qrl(:,i,7 )+ shape(:,i)*tmp*qdi(:,7 )
            qrl(:,i,8 ) = qrl(:,i,8 )+ shape(:,i)*tmp*qdi(:,8 )
            qrl(:,i,9 ) = qrl(:,i,9 )+ shape(:,i)*tmp*qdi(:,9 )
         enddo
c
c.... add contribution to local mass matrix
c

         if (have_local_mass .eq. 0) then
            do i=1,nshl
               do j=1,nshl
                  rmass(:,i,j) = rmass(:,i,j)+shape(:,i)*shape(:,j)*tmp
              enddo
           enddo
        endif
c
c.... end of the loop over integration points
c
      enddo

c
c.... find the inverse of the local mass matrix for each element


         if (have_local_mass .eq. 0) then
            allocate (lmassinv(iblock)%p(npro,nshl,nshl))

            do iel=1,npro
               do i=1,nshl      ! form the identy matrix
                  do j=1,nshl
                     lmassinv(iblock)%p(iel,i,j) = 0.0
                  enddo
                  lmassinv(iblock)%p(iel,i,i)=1.0
               enddo
c     
c.... LU factor the mass matrix
c
c               call ludcmp(rmass(iel,:,:),nshl,nshl,indx,d)
c     
c.... back substitute with the identy matrix to find the
c     matrix inverse
c          
               do j=1,nshl
c                  call lubksb(rmass(iel,:,:),nshl,nshl,indx,
c     &                        lmassinv(iblock)%p(iel,:,j))
               enddo
            enddo
            rminv(:,:,:) = lmassinv(iblock)%p(:,:,:)
         else
            rminv(:,:,:) = lmassinv(iblock)%p(:,:,:)
         endif
c
c.... find the modal coefficients of ql by multiplying by the inverse of
c     the local mass matrix
c
      do iel=1,npro
        do j=1,9
c         do j=1, 3*nsd
            ql(iel,:,j) = matmul( rminv(iel,:,:),qrl(iel,:,j) )
         enddo
      enddo
c
c.... return
c
      return
      end

!> This routine computes the local diffusive flux vector using a 
!! local projection algorithm: 
!! diffus * phi,i

      subroutine e3qlSclr (yl,      dwl,     shp,     shgl,
     &                     xl,      ql,      sgn )

      use local_mass
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
C     Argument variables
C
      REAL*8                dwl,         ql,          sgn,         shgl
      REAL*8                shp,         xl,          yl
C
C     Local variables
C
      INTEGER             i,          iel,         indx
      INTEGER             j
C
      REAL*8                diffus,      dxidx,       eviscv,      gradt
      REAL*8                qdi,         qrl,	d
      REAL*8                rmass,       rminv,       shape,       shdrv
      REAL*8                tmp,          wdetj
C
      dimension yl(npro,nshl,ndof),        dwl(npro,nshl),
     &          shp(nshl,ngauss),          shgl(nsd,nshl,ngauss),
     &          xl(npro,nenl,nsd),         sgn(npro,nshl),
     &          ql(npro,nshl,nsd)
c
c local arrays
c
      dimension dxidx(npro,nsd,nsd),       WdetJ(npro),
     &          diffus(npro),
     &          rminv(npro,nshl,nshl),
     &          qrl(npro,nshl,nsd)
c
      dimension qdi(npro,nsd),    shape(npro,nshl),
     &          shdrv(npro,nsd,nshl),      indx(nshl),
     &          rmass(npro,nshl,nshl),     gradT(npro,nsd),
     &          eviscv(npro)

c
c.... loop through the integration points
c
      rminv = zero
      rmass = zero
      qrl   = zero
        
      do intp = 1, ngauss

         call getshp(shp, shgl, sgn, shape, shdrv)

         qdi = zero
c
c.... calculate the integration variables 
c    
c
         call e3qvarSclr  (yl,           shdrv,        xl,           
     &                     gradT,        dxidx,        WdetJ )
c
c....  call function to sort out diffusivity (at end of this file)
c
         call getdiffsclr(dwl,shape,yl, diffus)
c
c.... diffusive flux in x1-direction
c
         qdi(:,1) =  diffus * gradT(:,1)
         qdi(:,2) =  diffus * gradT(:,2)
         qdi(:,3) =  diffus * gradT(:,3)

c
c.... assemble contribution of qdi to qrl,i.e., contribution to 
c     each element shape function
c
         tmp = Qwt(lcsyst,intp)
         if (lcsyst .eq. 1) then 
            tmp = tmp*(three/four)
         endif
        
         do i=1,nshl
            qrl(:,i,1 ) = qrl(:,i,1 )+ shape(:,i)*tmp*qdi(:,1 )
            qrl(:,i,2 ) = qrl(:,i,2 )+ shape(:,i)*tmp*qdi(:,2 )
            qrl(:,i,3 ) = qrl(:,i,3 )+ shape(:,i)*tmp*qdi(:,3 )
         enddo
c
c.... add contribution to local mass matrix
c
         if (have_local_mass .eq. 0) then
            do i=1,nshl
               do j=1,nshl
                  rmass(:,i,j)=rmass(:,i,j)+shape(:,i)*shape(:,j)*tmp
               enddo
            enddo
         endif

c.... end of the loop over integration points
c
      enddo

c
c.... find the inverse of the local mass matrix for each element
c
       qrl   = qrl/6.d0
c
c.... Assuming that lmassinv was already computed for flow equations
c     
       rmass = rmass/6.0
c
c.... for cubics, it cannot be precomputed, so compute and
c     save it the first time it is needed
c
         if (have_local_mass .eq. 0) then
            allocate (lmassinv(iblock)%p(npro,nshl,nshl))

            do iel=1,npro
               do i=1,nshl      ! form the identy matrix
                  do j=1,nshl
                     lmassinv(iblock)%p(iel,i,j) = 0.0
                  enddo
                  lmassinv(iblock)%p(iel,i,i)=1.0
               enddo
c     
c.... LU factor the mass matrix
c
c               call ludcmp(rmass(iel,:,:),nshl,nshl,indx,d)
c     
c.... back substitute with the identy matrix to find the
c     matrix inverse
c          
               do j=1,nshl
c                  call lubksb(rmass(iel,:,:),nshl,nshl,indx,
c     &                        lmassinv(iblock)%p(iel,:,j))
               enddo
            enddo
            rminv(:,:,:) = lmassinv(iblock)%p(:,:,:)
         else
            rminv(:,:,:) = lmassinv(iblock)%p(:,:,:)
         endif
c
c.... find the modal coefficients of ql by multiplying by the inverse of
c     the local mass matrix
c
      do iel=1,npro
         do j=1,nsd
            ql(iel,:,j) = matmul( rminv(iel,:,:),qrl(iel,:,j) )
         enddo
      enddo
c
c.... return
c
      return
      end

!> This routine computes the variables at integration point
!!  necessary for the computation of the diffusive flux vector.
!!
!! input:<BR>
!! @param[in] yl(npro,nshl,ndof) Primitive variables
!! @param[in] shgl(npro,nsd,nshl) Element local-grad-shape-functions
!! @param[in] xl(npro,nenl,nsd) Nodal coordinates at current step
!!
!! output:<BR>
!! @param[out]  g1yi(npro,ndof) Grad-y in direction 1
!! @param[out]  g2yi(npro,ndof) Grad-y in direction 2
!! @param[out]  g3yi(npro,ndof) Grad-y in direction 3
!! @param[out]  shg(npro,nshl,nsd) Element global grad-shape-functions
!! @param[out]  dxidx(npro,nsd,nsd) Inverse of deformation gradient
!! @param[out]  WdetJ(npro) Weighted Jacobian
!! @param[out]  u1(npro) X1-velocity component
!! @param[out]  u2(npro) X2-velocity component
!! @param[out]  u3(npro) X3-velocity component

        subroutine e3qvar (yl,          shgl,    
     &                     xl,          g1yi,
     &                     g2yi,        g3yi,        shg,
     &                     dxidx,       WdetJ )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
C     Argument variables
C
      REAL*8                dxidx,       g1yi,        g2yi,        g3yi
      REAL*8                shg,         shgl,        wdetj,       xl
      REAL*8                yl
C
C     Local variables
C
      INTEGER             n
C
      REAL*8                dxdxi,       tmp
C
c  passed arrays
c
        dimension yl(npro,nshl,ndof), 
     &            shgl(npro,nsd,nshl), xl(npro,nenl,nsd),
     &            g1yi(npro,nflow),       g2yi(npro,nflow),
     &            g3yi(npro,nflow),       shg(npro,nshl,nsd), 
     &            dxidx(npro,nsd,nsd),   WdetJ(npro)
c
c  local arrays
c
        dimension tmp(npro),           dxdxi(npro,nsd,nsd)

c
c.... compute the deformation gradient
c
        dxdxi = zero
c
          do n = 1, nenl
            dxdxi(:,1,1) = dxdxi(:,1,1) + xl(:,n,1) * shgl(:,1,n)
            dxdxi(:,1,2) = dxdxi(:,1,2) + xl(:,n,1) * shgl(:,2,n)
            dxdxi(:,1,3) = dxdxi(:,1,3) + xl(:,n,1) * shgl(:,3,n)
            dxdxi(:,2,1) = dxdxi(:,2,1) + xl(:,n,2) * shgl(:,1,n)
            dxdxi(:,2,2) = dxdxi(:,2,2) + xl(:,n,2) * shgl(:,2,n)
            dxdxi(:,2,3) = dxdxi(:,2,3) + xl(:,n,2) * shgl(:,3,n)
            dxdxi(:,3,1) = dxdxi(:,3,1) + xl(:,n,3) * shgl(:,1,n)
            dxdxi(:,3,2) = dxdxi(:,3,2) + xl(:,n,3) * shgl(:,2,n)
            dxdxi(:,3,3) = dxdxi(:,3,3) + xl(:,n,3) * shgl(:,3,n)
          enddo
c
c.... compute the inverse of deformation gradient
c
        dxidx(:,1,1) =   dxdxi(:,2,2) * dxdxi(:,3,3) 
     &                 - dxdxi(:,3,2) * dxdxi(:,2,3)
        dxidx(:,1,2) =   dxdxi(:,3,2) * dxdxi(:,1,3) 
     &                 - dxdxi(:,1,2) * dxdxi(:,3,3)
        dxidx(:,1,3) =   dxdxi(:,1,2) * dxdxi(:,2,3) 
     &                 - dxdxi(:,1,3) * dxdxi(:,2,2)
        tmp          = one / ( dxidx(:,1,1) * dxdxi(:,1,1) 
     &                       + dxidx(:,1,2) * dxdxi(:,2,1)  
     &                       + dxidx(:,1,3) * dxdxi(:,3,1) )
        dxidx(:,1,1) = dxidx(:,1,1) * tmp
        dxidx(:,1,2) = dxidx(:,1,2) * tmp
        dxidx(:,1,3) = dxidx(:,1,3) * tmp
        dxidx(:,2,1) = (dxdxi(:,2,3) * dxdxi(:,3,1) 
     &                - dxdxi(:,2,1) * dxdxi(:,3,3)) * tmp
        dxidx(:,2,2) = (dxdxi(:,1,1) * dxdxi(:,3,3) 
     &                - dxdxi(:,3,1) * dxdxi(:,1,3)) * tmp
        dxidx(:,2,3) = (dxdxi(:,2,1) * dxdxi(:,1,3) 
     &                - dxdxi(:,1,1) * dxdxi(:,2,3)) * tmp
        dxidx(:,3,1) = (dxdxi(:,2,1) * dxdxi(:,3,2) 
     &                - dxdxi(:,2,2) * dxdxi(:,3,1)) * tmp
        dxidx(:,3,2) = (dxdxi(:,3,1) * dxdxi(:,1,2) 
     &                - dxdxi(:,1,1) * dxdxi(:,3,2)) * tmp
        dxidx(:,3,3) = (dxdxi(:,1,1) * dxdxi(:,2,2) 
     &                - dxdxi(:,1,2) * dxdxi(:,2,1)) * tmp
c
        WdetJ = Qwt(lcsyst,intp)/ tmp

c
c.... --------------------->  Global Gradients  <-----------------------
c
        g1yi = zero
        g2yi = zero
        g3yi = zero
c
c
        do n = 1, nshl
c
c.... compute the global gradient of shape-function
c
c            ! N_{a,x_i}= N_{a,xi_i} xi_{i,x_j}
c
          shg(:,n,1) = shgl(:,1,n) * dxidx(:,1,1) + 
     &                 shgl(:,2,n) * dxidx(:,2,1) +
     &                 shgl(:,3,n) * dxidx(:,3,1)
          shg(:,n,2) = shgl(:,1,n) * dxidx(:,1,2) + 
     &                 shgl(:,2,n) * dxidx(:,2,2) +
     &                 shgl(:,3,n) * dxidx(:,3,2) 
          shg(:,n,3) = shgl(:,1,n) * dxidx(:,1,3) + 
     &                 shgl(:,2,n) * dxidx(:,2,3) +
     &                 shgl(:,3,n) * dxidx(:,3,3) 
c
c.... compute the global gradient of Y-variables
c
c
c  Y_{,x_i}=SUM_{a=1}^nenl (N_{a,x_i}(int) Ya)
c
          g1yi(:,2) = g1yi(:,2) + shg(:,n,1) * yl(:,n,2)
          g1yi(:,3) = g1yi(:,3) + shg(:,n,1) * yl(:,n,3)
          g1yi(:,4) = g1yi(:,4) + shg(:,n,1) * yl(:,n,4)
c
          g2yi(:,2) = g2yi(:,2) + shg(:,n,2) * yl(:,n,2)
          g2yi(:,3) = g2yi(:,3) + shg(:,n,2) * yl(:,n,3)
          g2yi(:,4) = g2yi(:,4) + shg(:,n,2) * yl(:,n,4)
c
          g3yi(:,2) = g3yi(:,2) + shg(:,n,3) * yl(:,n,2)
          g3yi(:,3) = g3yi(:,3) + shg(:,n,3) * yl(:,n,3)
          g3yi(:,4) = g3yi(:,4) + shg(:,n,3) * yl(:,n,4)

       enddo

c
c.... return
c

       return
       end

!> Compute the variables for the local scalar diffusion

      subroutine e3qvarSclr  (yl,       shgl,         xl, 
     &                        gradT,    dxidx,        WdetJ )

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
c
C     Local variables
C
      INTEGER             id,          n
C
c  passed arrays
c
      real*8   yl(npro,nshl,ndof),    shp(npro,nshl),
     &         shgl(npro,nsd,nshl),   xl(npro,nenl,nsd),
     &         dxidx(npro,nsd,nsd),   WdetJ(npro),
     &         gradT(npro,nsd)
c
c  local arrays
c
      real*8   shg(npro,nshl,nsd)


      call e3metric( xl,         shgl,       dxidx,  
     &               shg,        WdetJ)

      gradT = zero
      id=5+isclr
c
c  later, when there are more models than SA we will need a 
c  more general function to calculate evisc at a quadrature point
c
      do n = 1, nshl
         gradT(:,1) = gradT(:,1) + shg(:,n,1) * yl(:,n,id)
         gradT(:,2) = gradT(:,2) + shg(:,n,2) * yl(:,n,id)
         gradT(:,3) = gradT(:,3) + shg(:,n,3) * yl(:,n,id)
      enddo
c
c.... return
c

       return
       end

!> This routine computes the residual vector at an
!! integration point.
!!
!!  input:<BR>
!!  @param[in] u1(npro) X1-velocity
!!  @param[in] u2(npro) X2-velocity
!!  @param[in] u3(npro) X3-velocity
!!  @param[in] uBar(npro,3) u - tauM * Li
!!  @param[in] aci(npro,3) Acceleration
!!  @param[in] rlsli(npro,6) Resolved Leonard stresses
!!  @param[in] WdetJ(npro) Weighted jacobian determinant
!!  @param[in] g1yi(npro,ndof) X1-gradient of variables
!!  @param[in] g2yi(npro,ndof) X2-gradient of variables
!!  @param[in] g3yi(npro,ndof) X3-gradient of variables
!!  @param[in] rLui(npro,3) Total residual of NS equations
!!  @param[in] rmu(npro) Fluid viscosity
!!  @param[in] rho(npro) Density
!!  @param[in] tauC(npro) Continuity tau
!!  @param[in] tauM(npro) Momentum tau
!!  @param[in] tauBar(npro) Additional tau
!!  @param[in] shpfun(npro,nshl) Element shape functions
!!  @param[in] shg(npro,nshl,nsd) Global grad of element shape functions
!!  @param[in] src(npro,nsd) Body force term
!!
!!  output:<BR>
!!  @param[out] rl(npro,nshl,nflow)

      subroutine e3Res ( u1,        u2,         u3,
     &                   uBar,      aci,        WdetJ,
     &                   g1yi,      g2yi,       g3yi,
     &                   rLui,      rmu,        rho,
     &                   tauC,      tauM,       tauBar,
     &                   shpfun,    shg,        src,
     &                   rl,        pres,       acl,
     &                   rlsli)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"
 
C
C     Argument variables
C
      REAL*8                aci,         acl,         g1yi,        g2yi
      REAL*8                g3yi,        pres,        rho,         rl
      REAL*8                rlsli,       rlui,        rmu,         shg
      REAL*8                shpfun,      src,         taubar,      tauc
      REAL*8                taum,        u1,          u2,          u3
      REAL*8                ubar,        wdetj
C
C     Local variables
C
      REAL*8                tmps
C
      dimension u1(npro),         u2(npro),         u3(npro),
     &          uBar(npro,nsd),   aci(npro,nsd),    WdetJ(npro),
     &          g1yi(npro,nflow), g2yi(npro,nflow), g3yi(npro,nflow),
     &          rLui(npro,nsd),   rmu(npro),        rho(npro),
     &          tauC(npro),       tauM(npro),       tauBar(npro),
     &          shpfun(npro,nshl),shg(npro,nshl,nsd), src(npro,nsd),
     &          pres(npro)
      
      dimension rl(npro,nshl,nflow),
     &          acl(npro,nshl,ndof),
     &          rlsli(npro,6)
c
c.... local declarations
c
      real*8    tmp1(npro),   tmp2(npro),          tmp3(npro), 
     &          tmp(npro),    rGNa(npro,nsd,nsd),  rNa(npro,nsd),
     &          locmass(npro,nshl),omega(3)

      integer aa
c     
c.... initialize multipliers for Na and Na_{,i}
c
      rNa  = zero
      rGNa = zero
c
c.... compute the Na multiplier
c
      tmps     = one-flmpr  ! consistant mass factor

c
c no density yet...it comes later
c
      rNa(:,1) = aci(:,1)  * tmps
     &         - src(:,1)
      rNa(:,2) = aci(:,2)  * tmps
     &         - src(:,2)
      rNa(:,3) = aci(:,3)  * tmps
     &         - src(:,3)

c
c.... rotating frame terms if needed
c

      if(matflg(6,1).eq.1) then ! rotation

         omega(1)=datmat(1,6,1)
         omega(2)=datmat(2,6,1)
         omega(3)=datmat(3,6,1)
c
c no density yet...it comes later
c
         
         rNa(:,1) = rNa(:,1) + (omega(2)-omega(3)) * tauM * rLui(:,1)
         rNa(:,2) = rNa(:,2) + (omega(3)-omega(1)) * tauM * rLui(:,2)
         rNa(:,3) = rNa(:,3) + (omega(1)-omega(2)) * tauM * rLui(:,3)
      endif


c
c.... compute the Na,i multiplier
c
      tmp  = -pres + tauC * (g1yi(:,2) + g2yi(:,3) + g3yi(:,4))
      tmp1 =  rmu * ( g2yi(:,2) + g1yi(:,3) )
      tmp2 =  rmu * ( g3yi(:,3) + g2yi(:,4) )
      tmp3 =  rmu * ( g1yi(:,4) + g3yi(:,2) )


      if(iconvflow.eq.2) then  ! advective form (NO IBP either)
c
c no density yet...it comes later
c
         rNa(:,1) = rNa(:,1) 
     &            + ubar(:,1) * g1yi(:,2)
     &            + ubar(:,2) * g2yi(:,2)
     &            + ubar(:,3) * g3yi(:,2)
         rNa(:,2) = rNa(:,2)
     &            + ubar(:,1) * g1yi(:,3)
     &            + ubar(:,2) * g2yi(:,3)
     &            + ubar(:,3) * g3yi(:,3)
         rNa(:,3) = rNa(:,3)
     &            + ubar(:,1) * g1yi(:,4)
     &            + ubar(:,2) * g2yi(:,4)
     &            + ubar(:,3) * g3yi(:,4)

         rGNa(:,1,1) = two * rmu * g1yi(:,2) + tmp
         rGNa(:,1,2) = tmp1
         rGNa(:,1,3) = tmp3
         rGNa(:,2,1) = tmp1
         rGNa(:,2,2) = two * rmu * g2yi(:,3) + tmp
         rGNa(:,2,3) = tmp2
         rGNa(:,3,1) = tmp3
         rGNa(:,3,2) = tmp2
         rGNa(:,3,3) = two * rmu * g3yi(:,4) + tmp
      else   ! conservative form (with IBP)

c                                            IBP conservative convection
c                                                      |||||
c                                                      vvvvv
         rGNa(:,1,1) = two * rmu * g1yi(:,2) + tmp - u1(:)*u1(:)*rho(:)
         rGNa(:,1,2) = tmp1                        - u1(:)*u2(:)*rho(:)
         rGNa(:,1,3) = tmp3                        - u1(:)*u3(:)*rho(:)
         rGNa(:,2,1) = tmp1                        - u1(:)*u2(:)*rho(:)
         rGNa(:,2,2) = two * rmu * g2yi(:,3) + tmp - u2(:)*u2(:)*rho(:)
         rGNa(:,2,3) = tmp2                        - u3(:)*u2(:)*rho(:)
         rGNa(:,3,1) = tmp3                        - u1(:)*u3(:)*rho(:)
         rGNa(:,3,2) = tmp2                        - u3(:)*u2(:)*rho(:)
         rGNa(:,3,3) = two * rmu * g3yi(:,4) + tmp - u3(:)*u3(:)*rho(:)
      endif

      tmp1        = tauM * rLui(:,1) 
      tmp2        = tauM * rLui(:,2) 
      tmp3        = tauM * rLui(:,3)
      
      rGNa(:,1,1) = rGNa(:,1,1) + tmp1 * u1 
      rGNa(:,1,2) = rGNa(:,1,2) + tmp1 * u2
      rGNa(:,1,3) = rGNa(:,1,3) + tmp1 * u3
      rGNa(:,2,1) = rGNa(:,2,1) + tmp2 * u1
      rGNa(:,2,2) = rGNa(:,2,2) + tmp2 * u2
      rGNa(:,2,3) = rGNa(:,2,3) + tmp2 * u3
      rGNa(:,3,1) = rGNa(:,3,1) + tmp3 * u1
      rGNa(:,3,2) = rGNa(:,3,2) + tmp3 * u2
      rGNa(:,3,3) = rGNa(:,3,3) + tmp3 * u3

      if(iconvflow.eq.1) then  
c
c... get the u_j w_{i,i} term in there to match A_j^T w_{i,j} tau L_i
c    to match the SUPG of incompressible limit
c 
         rGNa(:,1,1) = rGNa(:,1,1) + tmp1 * u1
         rGNa(:,1,2) = rGNa(:,1,2) + tmp2 * u1
         rGNa(:,1,3) = rGNa(:,1,3) + tmp3 * u1
         rGNa(:,2,1) = rGNa(:,2,1) + tmp1 * u2
         rGNa(:,2,2) = rGNa(:,2,2) + tmp2 * u2
         rGNa(:,2,3) = rGNa(:,2,3) + tmp3 * u2
         rGNa(:,3,1) = rGNa(:,3,1) + tmp1 * u3
         rGNa(:,3,2) = rGNa(:,3,2) + tmp2 * u3
         rGNa(:,3,3) = rGNa(:,3,3) + tmp3 * u3
      endif

      if(iconvflow.eq.2) then  ! advective form has a taubar term to restore con
         tmp1 = tauBar
     &     * ( rLui(:,1) * g1yi(:,2)
     &       + rLui(:,2) * g2yi(:,2)
     &       + rLui(:,3) * g3yi(:,2) )
         tmp2 = tauBar
     &     * ( rLui(:,1) * g1yi(:,3)
     &       + rLui(:,2) * g2yi(:,3)
     &       + rLui(:,3) * g3yi(:,3) )
         tmp3 = tauBar
     &     * ( rLui(:,1) * g1yi(:,4)
     &       + rLui(:,2) * g2yi(:,4)
     &       + rLui(:,3) * g3yi(:,4) )

         rGNa(:,1,1) = rGNa(:,1,1) + tmp1 * rLui(:,1)
         rGNa(:,1,2) = rGNa(:,1,2) + tmp1 * rLui(:,2)
         rGNa(:,1,3) = rGNa(:,1,3) + tmp1 * rLui(:,3)
         rGNa(:,2,1) = rGNa(:,2,1) + tmp2 * rLui(:,1)
         rGNa(:,2,2) = rGNa(:,2,2) + tmp2 * rLui(:,2)
         rGNa(:,2,3) = rGNa(:,2,3) + tmp2 * rLui(:,3)
         rGNa(:,3,1) = rGNa(:,3,1) + tmp3 * rLui(:,1)
         rGNa(:,3,2) = rGNa(:,3,2) + tmp3 * rLui(:,2)
         rGNa(:,3,3) = rGNa(:,3,3) + tmp3 * rLui(:,3)
      endif   ! end of advective form
c
c.... everything that gets multiplied by rNa was supposed
c     to have density multiplying it.  Do it now.

      rNa(:,1) = rNa(:,1) * rho
      rNa(:,2) = rNa(:,2) * rho
      rNa(:,3) = rNa(:,3) * rho

c
c
c.... multiply the residual pieces by the weight space
c
      do aa = 1,nshl
c
c.... continuity
c
         rl(:,aa,4) = rl(:,aa,4) + WdetJ
     &              * ( shg(:,aa,1) * uBar(:,1) 
     &                + shg(:,aa,2) * uBar(:,2) 
     &                + shg(:,aa,3) * uBar(:,3) )
c
c.... momentum
c
         rl(:,aa,1) = rl(:,aa,1) - WdetJ
     &              * ( shpfun(:,aa) * rNa(:,1)
     &                + shg(:,aa,1) * rGNa(:,1,1)
     &                + shg(:,aa,2) * rGNa(:,1,2)
     &                + shg(:,aa,3) * rGNa(:,1,3) )
         rl(:,aa,2) = rl(:,aa,2) - WdetJ
     &              * ( shpfun(:,aa) * rNa(:,2)
     &                + shg(:,aa,1) * rGNa(:,2,1)
     &                + shg(:,aa,2) * rGNa(:,2,2)
     &                + shg(:,aa,3) * rGNa(:,2,3) )
         rl(:,aa,3) = rl(:,aa,3) - WdetJ
     &              * ( shpfun(:,aa) * rNa(:,3)
     &                + shg(:,aa,1) * rGNa(:,3,1)
     &                + shg(:,aa,2) * rGNa(:,3,2)
     &                + shg(:,aa,3) * rGNa(:,3,3) )
      
      enddo                 
c
c.... return
c
      return
      end

!> Calculate the residual for the advection-diffusion equation

      subroutine e3ResSclr ( uMod,              gGradS,
     &                       Sclr,		Sdot,	gradS,  
     &                       WdetJ,		rLS,	tauS,
     &                       shpfun,            shg,    src,
     &                       diffus,
     &                       rl )
c
        include "global.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      REAL*8                tmps
C
      real*8    uMod(npro,nsd),   gGradS(npro, nsd),
     &          Sclr(npro),       Sdot(npro),	gradS(npro,nsd),
     &          WdetJ(npro),      rLS(npro),	rho(npro),
     &          tauS(npro),       shpfun(npro,nshl), src(npro), 
     &          shg(npro,nshl,3), rl(npro,nshl)
      
      real*8    diffus(npro)
c
c.... local declarations
c
      real*8    rGNa(npro,nsd),   rNa(npro),  rcp(npro), tmp(npro)

      integer   aa
c     
c.... initialize multipliers for Na and Na_{,i}
c
      rNa  = zero
      rGNa = zero
c
c.... Na multiplier
c
      tmps     = one-flmpr  ! consistant mass factor
      rcp = one ! rho * cp
      

         rNa = rcp*(tmps*Sdot + uMod(:,1) * gradS(:,1)
     &                         + uMod(:,2) * gradS(:,2)
     &                         + uMod(:,3) * gradS(:,3) ) 
     &        - src



      tmp = rcp * tauS * (rLS -src)
c
c.... Na,i multiplier
c
      rGNa(:,1) = diffus * gradS(:,1) + uMod(:,1) * tmp
      rGNa(:,2) = diffus * gradS(:,2) + uMod(:,2) * tmp
      rGNa(:,3) = diffus * gradS(:,3) + uMod(:,3) * tmp
c
      if (idcsclr(1) .ne. 0) then
         if ((idcsclr(2).eq.1 .and. isclr.eq.1) .or. 
     &        (idcsclr(2).eq.2 .and. isclr.eq.2)) then ! scalar with dc
c
c.... add the contribution of DC to residual
c
            rGNa(:,1) = rGNa(:,1) + gGradS(:,1) ! gGradS is 
            rGNa(:,2) = rGNa(:,2) + gGradS(:,2) ! g^{ij}*Y_{j}*dcFct
            rGNa(:,3) = rGNa(:,3) + gGradS(:,3) ! calculated in e3dc.f
c
         endif
      endif                     ! end of idcsclr
c
c.... multiply the residual pieces by the weight space
c
      do aa = 1,nshl
c
         rl(:,aa) = rl(:,aa)	- WdetJ
     &                        * ( shpfun(:,aa) * rNa(:)
     &                        + shg(:,aa,1) * rGNa(:,1)
     &                        + shg(:,aa,2) * rGNa(:,2)
     &                        + shg(:,aa,3) * rGNa(:,3) )

      enddo
c
c.... return
c
      return
      end

!> Calculate the strong PDE residual.

      subroutine e3resStrongPDE(
     &     aci,  u1,   u2,   u3,   Temp, rho,  xx,
     &     g1yi, g2yi, g3yi,
     &     rLui, src, divqi)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/solpar.h"
C
C     Local variables
C
      REAL*8                bfx,         bfy,         bfz,         tref
C

c     INPUTS
      double precision  aci(npro,nsd)
      double precision   xx(npro,nsd)
      double precision g1yi(npro,nflow) 
      double precision g2yi(npro,nflow)
      double precision g3yi(npro,nflow)
      double precision u1(npro)
      double precision u2(npro)
      double precision u3(npro)
      double precision Temp(npro)
      double precision rho(npro)

c     OUTPUTS
      double precision rLui(npro,nsd)
      double precision src(npro,nsd)

c     LOCALS
      double precision divu(npro)
      double precision divqi(npro,nsd)
      double precision omega(nsd)

c.... compute source term
      src = zero
      if(matflg(5,1) .ge. 1) then
c        body force contribution to src
         bfx      = datmat(1,5,1) ! Boussinesq, g*alfap
         bfy      = datmat(2,5,1)
         bfz      = datmat(3,5,1)
         select case ( matflg(5,1) )
         case ( 1 )             ! standard linear body force
            src(:,1) = bfx
            src(:,2) = bfy
            src(:,3) = bfz
         case ( 2 )             ! boussinesq body force
            Tref = datmat(2,2,1)
            src(:,1) = bfx * (Temp(:)-Tref)
            src(:,2) = bfy * (Temp(:)-Tref)
            src(:,3) = bfz * (Temp(:)-Tref)
         case ( 3 )             ! user specified f(x,y,z)
            call e3source(xx, src)
         end select
      endif
c     
      if(matflg(6,1).eq.1) then
c        coriolis force contribution to src
         omega(1)=datmat(1,6,1)
         omega(2)=datmat(2,6,1)
         omega(3)=datmat(3,6,1)
c  note that we calculate f as if it contains the usual source
c  plus the Coriolis and the centrifugal forces taken to the rhs (sign change)
c  as long as we are doing SUPG with no accounting for these terms in the
c  LHS this is the only change (which will find its way to the RHS momentum
c  equation (both Galerkin and SUPG parts)).
c
c  uncomment later if you want rotation always about z axis
c                 orig_src - om x om x r       - two om x u
c
c$$$          src(:,1)=src(:,1)+omega(3)*omega(3)*xx(:,1)+two*omega(3)*u2
c$$$          src(:,2)=src(:,2)+omega(3)*omega(3)*xx(:,2)-two*omega(3)*u1
c
c more general for testing
c
         src(:,1)=src(:,1)
     &        -omega(2)*(omega(1)*xx(:,2)-omega(2)*xx(:,1))
     &        -omega(3)*(omega(1)*xx(:,3)-omega(3)*xx(:,1))
     &        -two*(omega(2)*u3-omega(3)*u2)
         src(:,2)=src(:,2)
     &        -omega(1)*(omega(2)*xx(:,1)-omega(1)*xx(:,2))
     &        -omega(3)*(omega(2)*xx(:,3)-omega(3)*xx(:,2))
     &        -two*(omega(3)*u1-omega(1)*u3)
         src(:,3)=src(:,3)
     &        -omega(1)*(omega(3)*xx(:,1)-omega(1)*xx(:,3))
     &        -omega(2)*(omega(3)*xx(:,2)-omega(2)*xx(:,3))
     &        -two*(omega(1)*u2-omega(2)*u1)
      endif
c     calculate momentum residual
      rLui(:,1) =(aci(:,1) + u1 * g1yi(:,2)
     &     + u2 * g2yi(:,2)
     &     + u3 * g3yi(:,2) - src(:,1) ) * rho
     &     + g1yi(:,1)
     &        - divqi(:,1)
      rLui(:,2) =(aci(:,2) + u1 * g1yi(:,3)
     &     + u2 * g2yi(:,3)
     &     + u3 * g3yi(:,3) - src(:,2) ) * rho
     &     + g2yi(:,1)
     &        - divqi(:,2)
      rLui(:,3) =(aci(:,3) + u1 * g1yi(:,4)
     &     + u2 * g2yi(:,4)
     &     + u3 * g3yi(:,4) - src(:,3) ) * rho
     &     + g3yi(:,1)
     &        - divqi(:,3)
      if(iconvflow.eq.1) then
         divu(:)  = (g1yi(:,2) + g2yi(:,3) + g3yi(:,4))*rho
         rLui(:,1)=rlui(:,1)+u1(:)*divu(:)
         rLui(:,2)=rlui(:,2)+u2(:)*divu(:)
         rLui(:,3)=rlui(:,3)+u3(:)*divu(:)
      endif
c
      return
      end subroutine e3resStrongPDE

!> This routine computes the body force term.
!!  Currently this computes a swirl body with the axis alligned with 
!!  the z-coordinate

      subroutine e3source(xx, src)

        include "global.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
C
C     Local variables
C
      INTEGER             iel
C
      REAL*8                bigr,        re,          t13,         t2
      REAL*8                t20,         t21,         t26,         t3
      REAL*8                t30,         t37,         t4,          t54
      REAL*8                t7,          t8,          x,           y
      REAL*8                z
C
      
      real*8   xx(npro,nsd), src(npro,nsd)

      real*8   nu

      real*8   r, Stheta, dpdz, rP5
      if(datmat(2,5,1).eq. 0) then  ! calc swirl 
c
c  This is the body force which will drive a swirl in a pipe flow
c     
         bigR    = 0.5d0
         dpdz    = datmat(1,5,1)
         do iel = 1, npro
            
            r   = sqrt( xx(iel,1)**2 + xx(iel,2)**2)
            rP5 = (r/bigR)**5
            
            Stheta = dpdz * sin(0.5*pi*rP5)
            
            src(iel,1) = -xx(iel,2)/r * Stheta
            src(iel,2) =  xx(iel,1)/r * Stheta
            src(iel,3) =  dpdz
         enddo
      else  ! contrived test problem
   
      do iel = 1, npro
            x = xx(iel,1)
            y = xx(iel,2)
            z = xx(iel,3)
            Re = 1.0/datmat(1,2,1)
c
c  The following are MAPLE generated forcing functions for
c  a Lid Driven cavity flow with an analytic solution
c
            t2 = x**2
            t3 = t2**2
            t4 = t3*x
            t7 = t2*x
            t8 = 8*t7
            t13 = y**2
            t20 = t13**2
            t21 = t20-t13
            t26 = t3**2
            t30 = t3*t2
            t37 = t13*y
            t54 = -8/Re*(24.E0/5.E0*t4-12*t3+t8+2*(4*t7-6*t2+2*x)*(12
     &           *t13-2)+(24*x-12)*t21)-64*(t26/2-2*t3*t7+3*t30-2*t4+t3
     &           /2)*(-24*t20*y+8*t37-4*y)+64*t21*(4*t37-2*y)*(-4*t30+12
     &           *t4-14*t3+t8-2*t2)

            src(iel,1) = 0.0
            src(iel,2) = t54
            src(iel,3) = 0.0
         enddo
   
      endif
      return
      end

!> This routine computes the diagonal Tau for least-squares operator.  
!!
!! input:<BR>
!! @param[in] u1(npro) X1-velocity component
!! @param[in] u2(npro) X2-velocity component
!! @param[in] u3(npro) X3-velocity component
!! @param[in] dxidx(npro,nsd,nsd) Inverse of deformation gradient
!! @param[in] rLui(npro,nsd) Least-squares residual vector
!!
!! output:<BR>
!! @param[out] tauC(npro) Continuity tau
!! @param[out] tauM(npro) Momentum tau
!! @param[out] tauBar(npro) Additional tau
!! @param[out] uBar(npro,nsd) Modified velocity

      subroutine e3stab (rho,          u1,       u2,
     &                   u3,           dxidx,    rLui,   
     &                   rmu,          tauC,     tauM,   
     &                   tauBar,       uBar )  
c
        include "global.h"
        include "common_blocks/genpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/outpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/timdat.h"
C
C     Argument variables
C
      REAL*8                dxidx,       rho,         rlui,        rmu
      REAL*8                taubar,      tauc,        taum,        u1
      REAL*8                u2,          u3,          ubar
C
C     Local variables
C
      REAL*8                dt,          dts,         dtsfsq,      dtsi
      REAL*8                fact,        ff,          fff
      REAL*8                gijd,        omegasq,     rhoinv,      rnu
      REAL*8                unorm,       velsq
C
        dimension rho(npro),                 u1(npro),
     &            u2(npro),                  u3(npro),
     &            dxidx(npro,nsd,nsd), 
     &            rLui(npro,nsd),
     &            tauC(npro),    tauM(npro), tauBar(npro),
     &            rmu(npro),     uBar(npro,3), unorm(npro)

c
        dimension gijd(npro,6),       fact(npro), rnu(npro),
     &       rhoinv(npro)
c
c
c.... get the metric tensor
c      
      call e3gijd( dxidx, gijd )
c
c... higher order element diffusive correction
c
      if (ipord == 1) then
         fff = 36.0d0
      else if (ipord == 2) then
         fff = 60.0d0
c     fff = 36.0d0
      else if (ipord == 3) then
         fff = 128.0d0
c     fff = 144.0d0
      endif

      omegasq=zero
      if(matflg(6,1).eq.1) omegasq = datmat(1,6,1)**2
     .                              +datmat(2,6,1)**2
     .                              +datmat(3,6,1)**2
      rhoinv=one/rho
      rnu=rmu*rhoinv

      if(itau.eq.0)  then  ! original tau
c
c...  momentum tau
c 
         dts=  Dtgl*dtsfct	! Dtgl = (time step)^-1
         tauM = ( (two*dts)**2
     3		      + ( u1 * ( gijd(:,1) * u1
     4			             + gijd(:,4) * u2
     5			             + gijd(:,6) * u3 )
     6		        + u2 * ( gijd(:,4) * u1
     7			             + gijd(:,2) * u2
     8			             + gijd(:,5) * u3 )
     9		        + u3 * ( gijd(:,6) * u1
     a			             + gijd(:,5) * u2
     1			             + gijd(:,3) * u3 ) ) )
     2		    + fff * rnu** 2
     3		    * ( gijd(:,1) ** 2
     4		      + gijd(:,2) ** 2
     5		      + gijd(:,3) ** 2
     6		      + 2.
     7		      * ( gijd(:,4) ** 2
     8		        + gijd(:,5) ** 2
     9		        + gijd(:,6) ** 2 ) 
     b              +omegasq)
        
         fact = sqrt(tauM)
         dtsi=one/dts
         ff=taucfct/dtsfct
         tauC =rho* pt125*fact/(gijd(:,1)+gijd(:,2)+gijd(:,3))*ff
         tauM = one/fact
      else if(itau.eq.1)  then  ! new tau

c
c  determinant of gijd
c
         fact = gijd(:,1) * gijd(:,2) * gijd(:,3)
     &        - gijd(:,2) * gijd(:,6) * gijd(:,6)
     &        - gijd(:,1) * gijd(:,5) * gijd(:,5)
     &        - gijd(:,3) * gijd(:,4) * gijd(:,4)
     &        + gijd(:,6) * gijd(:,4) * gijd(:,5) * two
        
c
c put 1/2u*h1 = sqrt(u_i g^{ij} u_j) into tau_M  note inverse is calculated
c on the fly here from cofactors over the determinent dotted from left and 
c right with u
c
         
         tauM = 
     1       u1 * ( (gijd(:,2)*gijd(:,3)-gijd(:,5)*gijd(:,5))  * u1
     2     +  two * (gijd(:,5)*gijd(:,6)-gijd(:,4)*gijd(:,3))  * u2 
     3     +  two * (gijd(:,4)*gijd(:,5)-gijd(:,6)*gijd(:,2))  * u3)
     1     + u2 * ( (gijd(:,1)*gijd(:,3)-gijd(:,6)*gijd(:,6))  * u2
     3     +  two * (gijd(:,4)*gijd(:,6)-gijd(:,1)*gijd(:,5))  * u3)
     1     + u3 * ( (gijd(:,1)*gijd(:,2)-gijd(:,4)*gijd(:,4))  * u3)
         tauM=fact/taum  ! here we have (u_i g^{ij} u^j)^{-1} approx 4/u^2h^2
c
c  we can calculate tauC more efficiently now
c
         tauC=tauM*(one+tauM*rmu*rmu)
         tauC=one/tauC
         tauC=taucfct*sqrt(tauC)
c
c
c...  momentum tau
c
c
c     this tau needs a u/h instead of a u*h so we contract with g_{ij} as
c     follows  (i.e. u_i g_{ij} u_j approx u^2/(h^2)/4) 
c
         fact = 
     3          u1 * ( gijd(:,1) * u1
     4               + gijd(:,4) * u2
     5               + gijd(:,6) * u3 )
     6        + u2 * ( gijd(:,4) * u1
     7               + gijd(:,2) * u2
     8               + gijd(:,5) * u3 )
     9        + u3 * ( gijd(:,6) * u1
     a               + gijd(:,5) * u2
     1               + gijd(:,3) * u3 ) 
c 
c first limit dt effect on tau from causing trouble if user drops CFL below
c .05 (this could cause loss of spatial stability)
c
         velsq=vel*vel
         unorm = (u1*u1+u2*u2+u3*u3)/velsq
         dtsfsq=dtsfct*dtsfct
         dt=one/Dtgl
         taubar=  dtsfsq/( dt*dt + .01*unorm/fact)  ! never gets above (C_1 20*u_inf/h)^2
c
c  this means tau will never get below h/(20*C_1*u) no matter what time step 
c  you choose.  The 0.01 constant comes from minCFL=.05=> .05*.05*4 (where the 
c  4 comes from the bi-unit mapping). If you want to limit sooner the formula
c  would be  ".01-factor"=minCFL^2*4
c

         tauM = rho ** 2
     1		    * ( four*taubar + fact
     2		    + fff * rmu** 2
     3		    * ( gijd(:,1) ** 2
     4		      + gijd(:,2) ** 2
     5		      + gijd(:,3) ** 2
     6		      + 2.
     7		      * ( gijd(:,4) ** 2
     8		        + gijd(:,5) ** 2
     9		        + gijd(:,6) ** 2 ) ) 
     b              +omegasq)
         fact=sqrt(tauM)
cdebugcheck         tauBar = pt125*fact/(gijd(:,1)+gijd(:,2)+gijd(:,3)) !*dtsi
      
        tauM=one/fact           ! turn it right side up.
      else if(itau.eq.2)  then  ! new tau different continuity h

         unorm = (u1*u1+u2*u2+u3*u3)
         
         tauM=(gijd(:,1)+gijd(:,2)+gijd(:,3))/unorm ! here we have  4/u^2h^2
c
c  we can calculate tauC more efficiently now
c
         tauC=tauM*(one+tauM*rmu*rmu)
         tauC=one/tauC
         tauC=sqrt(tauC)*taucfct
c
c
c...  momentum tau
c
c
c     this tau needs a u/h instead of a u*h so we contract with g_{ij} as
c     follows  (i.e. u_i g_{ij} u_j approx u^2/(h^2)/4) 
c
         fact = 
     3          u1 * ( gijd(:,1) * u1
     4               + gijd(:,4) * u2
     5               + gijd(:,6) * u3 )
     6        + u2 * ( gijd(:,4) * u1
     7               + gijd(:,2) * u2
     8               + gijd(:,5) * u3 )
     9        + u3 * ( gijd(:,6) * u1
     a               + gijd(:,5) * u2
     1               + gijd(:,3) * u3 ) 
c 
c first limit dt effect on tau from causing trouble if user drops CFL below
c .05 (this could cause loss of spatial stability)
c
         velsq=vel*vel
         dtsfsq=dtsfct*dtsfct
         dt=one/Dtgl
         unorm=unorm/velsq
         taubar=  dtsfsq/( dt*dt + .01*unorm/fact)  ! never gets above (C_1 20*u_inf/h)^2
c
c  this means tau will never get below h/(20*C_1*u) no matter what time step 
c  you choose.  The 0.01 constant comes from minCFL=.05=> .05*.05*4 (where the 
c  4 comes from the bi-unit mapping). If you want to limit sooner the formula
c  would be  ".01-factor"=minCFL^2*4
c

         tauM = rho ** 2
     1		    * ( four*taubar + fact
     2		    + fff * rmu** 2
     3		    * ( gijd(:,1) ** 2
     4		      + gijd(:,2) ** 2
     5		      + gijd(:,3) ** 2
     6		      + 2.
     7		      * ( gijd(:,4) ** 2
     8		        + gijd(:,5) ** 2
     9		        + gijd(:,6) ** 2 ) ) 
     b              +omegasq)
         fact=sqrt(tauM)
c         tauBar = pt125*fact/(gijd(:,1)+gijd(:,2)+gijd(:,3)) !*dtsi
      
        tauM=one/fact           ! turn it right side up.
      else if(itau.eq.3)  then  ! compressible tau

c
c  determinant of gijd
c
         fact = gijd(:,1) * gijd(:,2) * gijd(:,3)
     &        - gijd(:,2) * gijd(:,6) * gijd(:,6)
     &        - gijd(:,1) * gijd(:,5) * gijd(:,5)
     &        - gijd(:,3) * gijd(:,4) * gijd(:,4)
     &        + gijd(:,6) * gijd(:,4) * gijd(:,5) * two
        
c
c put 1/2u*h1 = sqrt(u_i g^{ij} u_j) into tau_M  note inverse is calculated
c on the fly here from cofactors over the determinent dotted from left and 
c right with u
c
         
         tauM = 
     1       u1 * ( (gijd(:,2)*gijd(:,3)-gijd(:,5)*gijd(:,5))  * u1
     2     +  two * (gijd(:,5)*gijd(:,6)-gijd(:,4)*gijd(:,3))  * u2 
     3     +  two * (gijd(:,4)*gijd(:,5)-gijd(:,6)*gijd(:,2))  * u3)
     1     + u2 * ( (gijd(:,1)*gijd(:,3)-gijd(:,6)*gijd(:,6))  * u2
     3     +  two * (gijd(:,4)*gijd(:,6)-gijd(:,1)*gijd(:,5))  * u3)
     1     + u3 * ( (gijd(:,1)*gijd(:,2)-gijd(:,4)*gijd(:,4))  * u3)
c
c  we can calculate tauC more efficiently now
c
         tauM=sqrt(tauM/fact)*two
         tauC=pt5*tauM*min(one,pt5*tauM/rmu)*taucfct
c
c
c...  momentum tau
c
c
c     this tau needs a u/h instead of a u*h so we contract with g_{ij} as
c     follows  (i.e. u_i g_{ij} u_j approx u^2/(h^2)/4) 
c
         fact = 
     3          u1 * ( gijd(:,1) * u1
     4               + gijd(:,4) * u2
     5               + gijd(:,6) * u3 )
     6        + u2 * ( gijd(:,4) * u1
     7               + gijd(:,2) * u2
     8               + gijd(:,5) * u3 )
     9        + u3 * ( gijd(:,6) * u1
     a               + gijd(:,5) * u2
     1               + gijd(:,3) * u3 ) 
         fact=one/sqrt(fact)

         unorm = (u1*u1+u2*u2+u3*u3)

         dts= one/( Dtgl*dtsfct)
         tauM =min(dts,min(fact,fact*fact*unorm*pt33/rmu))
      endif
c
c.... calculate tauBar
c
      tauBar = rLui(:,1) * ( gijd(:,1) * rLui(:,1)
     &                       + gijd(:,4) * rLui(:,2)
     &                       + gijd(:,6) * rLui(:,3) )
     &         + rLui(:,2) * ( gijd(:,4) * rLui(:,1)
     &                       + gijd(:,2) * rLui(:,2)
     &                       + gijd(:,5) * rLui(:,3) ) 
     &         + rLui(:,3) * ( gijd(:,6) * rLui(:,1)
     &                       + gijd(:,5) * rLui(:,2)
     &                       + gijd(:,3) * rLui(:,3) )
      where ( tauBar .ne. 0.0 ) 
         tauBar = tauM / sqrt(tauBar)
      endwhere

c
c.... compute the modified velocity, uBar
c
        uBar(:,1) = u1 - tauM * rLui(:,1)*rhoinv
        uBar(:,2) = u2 - tauM * rLui(:,2)*rhoinv
        uBar(:,3) = u3 - tauM * rLui(:,3)*rhoinv
c     
c.... return
c
        return
        end

!> Compute Momentum tau

      subroutine e3uBar (rho,          ui,         dxidx,     
     &                   rLui,         rmu,        uBar )         

        include "global.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      REAL*8                dts,         fff
C
      real*8     rho(npro),            ui(npro,nsd),
     &           dxidx(npro,nsd,nsd),  rLui(npro,nsd),
     &           rmu(npro),            uBar(npro,nsd)

      real*8     gijd(npro,6),         tauM(npro)

c
c.... get the metric tensor
c      
      call e3gijd( dxidx, gijd )
c
c.... higher order element diffusive correction
c
      if (ipord == 1) then
         fff = 36.0d0
      else if (ipord == 2) then
         fff = 60.0d0
      else if (ipord == 3) then
         fff = 128.0d0
      endif

      dts  =  (Dtgl*dtsfct)
      tauM = rho ** 2
     1		    * ( (two*dts)**2
     3		      + ( ui(:,1) * ( gijd(:,1) * ui(:,1)
     4			            + gijd(:,4) * ui(:,2)
     5			            + gijd(:,6) * ui(:,3) )
     6		        + ui(:,2) * ( gijd(:,4) * ui(:,1)
     7			            + gijd(:,2) * ui(:,2)
     8			            + gijd(:,5) * ui(:,3) )
     9		        + ui(:,3) * ( gijd(:,6) * ui(:,1)
     a			            + gijd(:,5) * ui(:,2)
     1			            + gijd(:,3) * ui(:,3) ) ) )
     2		    + fff * rmu** 2
     3		    * ( gijd(:,1) ** 2
     4		      + gijd(:,2) ** 2
     5		      + gijd(:,3) ** 2
     6		      + 2.
     7		      * ( gijd(:,4) ** 2
     8		        + gijd(:,5) ** 2
     9		        + gijd(:,6) ** 2 ) )
        
      tauM = one/sqrt(tauM)
c
c.... compute the modified velocity, uBar
c
      uBar(:,1) = ui(:,1) - tauM * rLui(:,1)
      uBar(:,2) = ui(:,2) - tauM * rLui(:,2)
      uBar(:,3) = ui(:,3) - tauM * rLui(:,3)

      return
      end

!> Get the metric tensor g_{ij}=xi_{k,i} xi_{k,j}.  

      subroutine e3gijd( dxidx,  gijd )
      
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
C
C     Local variables
C
      REAL*8                c1,          c2
C     
      real*8  dxidx(npro,nsd,nsd),  gijd(npro,6),
     &        tmp1(npro),           tmp2(npro),
     &        tmp3(npro)
c
c  form metric tensor g_{ij}=xi_{k,i} xi_{k,j}.  It is a symmetric
c  tensor so we only form 6 components and use symmetric matrix numbering.
c
      if (lcsyst .ge. 2) then  ! note this makes wedges like hexs..should
c                                be corrected later

         gijd(:,1) = dxidx(:,1,1) * dxidx(:,1,1)
     &             + dxidx(:,2,1) * dxidx(:,2,1)
     &             + dxidx(:,3,1) * dxidx(:,3,1)
c
         gijd(:,4) = dxidx(:,1,1) * dxidx(:,1,2)
     &             + dxidx(:,2,1) * dxidx(:,2,2)
     &             + dxidx(:,3,1) * dxidx(:,3,2)
c
         gijd(:,2) = dxidx(:,1,2) * dxidx(:,1,2)
     &             + dxidx(:,2,2) * dxidx(:,2,2)
     &             + dxidx(:,3,2) * dxidx(:,3,2)
c
         gijd(:,5) = dxidx(:,1,2) * dxidx(:,1,3)
     &             + dxidx(:,2,2) * dxidx(:,2,3)
     &             + dxidx(:,3,2) * dxidx(:,3,3)
c
         gijd(:,6) = dxidx(:,1,1) * dxidx(:,1,3)
     &             + dxidx(:,2,1) * dxidx(:,2,3)
     &             + dxidx(:,3,1) * dxidx(:,3,3)
c
         gijd(:,3) = dxidx(:,1,3) * dxidx(:,1,3)
     &             + dxidx(:,2,3) * dxidx(:,2,3)
     &             + dxidx(:,3,3) * dxidx(:,3,3)
c
      else   if (lcsyst .eq. 1) then
c
c  There is an invariance problem with tets 
c  It is fixed by the following modifications to gijd 
c

         c1 = 1.259921049894873D+00
         c2 = 6.299605249474365D-01
c
         tmp1(:) = c1 * dxidx(:,1,1)+c2 *(dxidx(:,2,1)+dxidx(:,3,1))
         tmp2(:) = c1 * dxidx(:,2,1)+c2 *(dxidx(:,1,1)+dxidx(:,3,1))
         tmp3(:) = c1 * dxidx(:,3,1)+c2 *(dxidx(:,1,1)+dxidx(:,2,1))
         gijd(:,1) = dxidx(:,1,1) * tmp1
     1              + dxidx(:,2,1) * tmp2
     2              + dxidx(:,3,1) * tmp3
c
         tmp1(:) = c1 * dxidx(:,1,2)+c2 *(dxidx(:,2,2)+dxidx(:,3,2))
         tmp2(:) = c1 * dxidx(:,2,2)+c2 *(dxidx(:,1,2)+dxidx(:,3,2))
         tmp3(:) = c1 * dxidx(:,3,2)+c2 *(dxidx(:,1,2)+dxidx(:,2,2))
         gijd(:,2) = dxidx(:,1,2) * tmp1
     1             + dxidx(:,2,2) * tmp2
     2             + dxidx(:,3,2) * tmp3
c
         gijd(:,4) = dxidx(:,1,1) * tmp1
     1             + dxidx(:,2,1) * tmp2
     2             + dxidx(:,3,1) * tmp3
c
         tmp1(:) = c1 * dxidx(:,1,3)+c2 *(dxidx(:,2,3)+dxidx(:,3,3))
         tmp2(:) = c1 * dxidx(:,2,3)+c2 *(dxidx(:,1,3)+dxidx(:,3,3))
         tmp3(:) = c1 * dxidx(:,3,3)+c2 *(dxidx(:,1,3)+dxidx(:,2,3))
         gijd(:,3) = dxidx(:,1,3) * tmp1
     1             + dxidx(:,2,3) * tmp2
     2             + dxidx(:,3,3) * tmp3
c
         gijd(:,5) = dxidx(:,1,2) * tmp1
     1             + dxidx(:,2,2) * tmp2
     2             + dxidx(:,3,2) * tmp3
c
         gijd(:,6) = dxidx(:,1,1) * tmp1
     1             + dxidx(:,2,1) * tmp2
     2             + dxidx(:,3,1) * tmp3
c
      else
         write(*,*) 'lcsyst eq',lcsyst,'not supported'
         stop
      endif

      return
      end

!> Calculate the stabilization for the advection-diffusion equation

      subroutine e3StabSclr (uMod,  dxidx,  tauT, diffus, srcP, giju,
     &                       srcRat )
c
c
        include "global.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timdat.h"
 
C
C     Local variables
C
      REAL*8                dts
C
        real*8    rho(npro),                 uMod(npro,nsd),
     &            dxidx(npro,nsd,nsd),       diffus(npro),
     &            tauT(npro),                srcP(npro)

c
        real*8    gijd(npro,6),       giju(npro,6),   
     &            tmp1(npro),         tmp2(npro),
     &            tmp3(npro),         fact(npro),
     &            srcRat(npro)

        real*8     fff
        if(ivart.eq.1) then
           tauT=zero
           return
        endif
c
c.... get the metric tensor
c      
      call e3gijd( dxidx, gijd )
c
c...  momentum tau
c 
c
c... higher order element diffusive correction
c
        if (ipord == 1) then
           fff = 9.0d0
        else if (ipord == 2) then
           fff = 36.0d0
        else if (ipord == 3) then
           fff = 64.0d0
        endif

        dts=  (Dtgl*dtsfct)
c       DES - MODIFIED AFTER NATHAN TURBULENCE CLEANUP
        srcRat=srcP
        tauT = 
     1	       (two*dts)**2 
     2       + srcRat ** 2
     3	     + uMod(:,1) * ( gijd(:,1) * uMod(:,1)
     4	                   + gijd(:,4) * uMod(:,2)
     5	                   + gijd(:,6) * uMod(:,3) )
     6	     + uMod(:,2) * ( gijd(:,4) * uMod(:,1)
     7	                   + gijd(:,2) * uMod(:,2)
     8	                   + gijd(:,5) * uMod(:,3) )
     9	     + uMod(:,3) * ( gijd(:,6) * uMod(:,1)
     a	                   + gijd(:,5) * uMod(:,2)
     1	                   + gijd(:,3) * uMod(:,3) )
     2	     + fff * diffus(:)** 2
     3	           * ( gijd(:,1) ** 2
     4		     + gijd(:,2) ** 2
     5		     + gijd(:,3) ** 2
     6		     + 2.
     7		      * ( gijd(:,4) ** 2
     8		        + gijd(:,5) ** 2
     9		        + gijd(:,6) ** 2 ) )
        
        tauT = one/sqrt(tauT)
c
        if(idcsclr(1) .ne. 0) then 
           if ((idcsclr(2).eq.1 .and. isclr.eq.1) .or. 
     &          (idcsclr(2).eq.2 .and. isclr.eq.2)) then ! scalar with dc
c     
c     determinant of gijd
c     
              fact = one/(gijd(:,1) * gijd(:,2) * gijd(:,3)
     &             - gijd(:,2) * gijd(:,6) * gijd(:,6)
     &             - gijd(:,1) * gijd(:,5) * gijd(:,5)
     &             - gijd(:,3) * gijd(:,4) * gijd(:,4)
     &             + gijd(:,6) * gijd(:,4) * gijd(:,5) * two)
c
c ... note between compressible and incompressible 5 and 6 of giju 
c     are switched        
c
              giju(:,1) = fact * (gijd(:,2)*gijd(:,3) 
     &                  - gijd(:,5)**2)
              giju(:,2) = fact * (gijd(:,1)*gijd(:,3) 
     &                  - gijd(:,6)**2)
              giju(:,3) = fact * (gijd(:,1)*gijd(:,2)
     &                  - gijd(:,4)**2)
              giju(:,4) = fact * (gijd(:,5)*gijd(:,6)
     &                  - gijd(:,4)*gijd(:,3) )
              giju(:,5) = fact * (gijd(:,4)*gijd(:,6)
     &                  - gijd(:,1)*gijd(:,5) )
              giju(:,6) = fact * (gijd(:,4)*gijd(:,5)
     &                  - gijd(:,6)*gijd(:,2) )

c
           endif
        endif                   ! end of idcsclr.ne.0
c     
c.... return
c
        return
        end

!> Compute the terms needed for the left hand side matrices 
!! needed for the conservative projection       

      subroutine e3StsLhs( xl,  lStsVec )

      use     stats
      
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C      
      integer i
      real*8  lDir(npro,nshl,3), lStsVec(npro,nshl,nResDims),
     &        xl(npro,nenl,3)

      call e3StsDir( xl,  lDir )
      
      do i = 1, nshl
         lStsVec(:,i,1) = lDir(:,i,1) * lDir(:,i,1)
         lStsVec(:,i,2) = lDir(:,i,2) * lDir(:,i,2)
         lStsVec(:,i,3) = lDir(:,i,3) * lDir(:,i,3)

         lStsVec(:,i,4) = lDir(:,i,1) * lDir(:,i,2)
         lStsVec(:,i,5) = lDir(:,i,2) * lDir(:,i,3)
         lStsVec(:,i,6) = lDir(:,i,3) * lDir(:,i,1)

         lStsVec(:,i,7) = 0.0
         lStsVec(:,i,8) = 0.0
         lStsVec(:,i,9) = 0.0
         lStsVec(:,i,10) = 0.0
         lStsVec(:,i,11) = 0.0
      enddo
      
      return
      end

!> Compute the residual terms for the consistent projection

      subroutine e3StsRes( xl, rl, lStsVec )

      use     stats
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Local variables
C
      INTEGER             i

C      
      real*8  xl(npro,nenl,3),  rl(npro,nshl,ndof)
      real*8  lDir(npro,nshl,3), lStsVec(npro,nshl,nResDims)
      
      call e3StsDir( xl,  lDir )
      
      do i = 1, nshl
         lStsVec(:,i,1) = lDir(:,i,1) * rl(:,i,4)
         lStsVec(:,i,2) = lDir(:,i,2) * rl(:,i,4)
         lStsVec(:,i,3) = lDir(:,i,3) * rl(:,i,4)
      
         lStsVec(:,i,4) = lDir(:,i,1) * rl(:,i,1)
         lStsVec(:,i,5) = lDir(:,i,2) * rl(:,i,2)
         lStsVec(:,i,6) = lDir(:,i,3) * rl(:,i,3)
      
         lStsVec(:,i,7) = lDir(:,i,1) * rl(:,i,2)
     &                  + lDir(:,i,2) * rl(:,i,1)
         lStsVec(:,i,8) = lDir(:,i,2) * rl(:,i,3)
     &                  + lDir(:,i,3) * rl(:,i,2)
         lStsVec(:,i,9) = lDir(:,i,3) * rl(:,i,1)
     &        + lDir(:,i,1) * rl(:,i,3)
         lStsVec(:,i,10) = 0
         lStsVec(:,i,11) = 0
      enddo
      

      return
      end

!> Compute the normal to each of the nodes

      subroutine e3StsDir( xl,  lDir )

        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Local variables
C
      REAL*8                fct,         x12,         x13,         x14
      REAL*8                x16,         x18,         x23,         x24
      REAL*8                x25,         x27,         x31,         x34
      REAL*8                x36,         x38,         x42,         x45
      REAL*8                x47,         x52,         x54,         x57
      REAL*8                x61,         x63,         x68,         x72
      REAL*8                x74,         x75,         x81,         x83
      REAL*8                x86,         y12,         y13,         y14
      REAL*8                y16,         y18,         y23,         y24
      REAL*8                y25,         y27,         y31,         y34
      REAL*8                y36,         y38,         y42,         y45
      REAL*8                y47,         y52,         y54,         y57
      REAL*8                y61,         y63,         y68,         y72
      REAL*8                y74,         y75,         y81,         y83
      REAL*8                y86,         z12,         z13,         z14
      REAL*8                z16,         z18,         z23,         z24
      REAL*8                z25,         z27,         z31,         z34
      REAL*8                z36,         z38,         z42,         z45
      REAL*8                z47,         z52,         z54,         z57
      REAL*8                z61,         z63,         z68,         z72
      REAL*8                z74,         z75,         z81,         z83
      REAL*8                z86
C      
      real*8  xl(npro,nenl,3), lDir(npro,nshl,3)
      integer e

c
c.... linear tets
c
      if (nshl .eq. 4 ) then
         fct = 1.d0 / 6.d0
         do e = 1, npro

            x12         = xl(e,2,1) - xl(e,1,1)
            x13         = xl(e,3,1) - xl(e,1,1)
            x14         = xl(e,4,1) - xl(e,1,1)
            x23         = xl(e,3,1) - xl(e,2,1)
            x24         = xl(e,4,1) - xl(e,2,1)
            x34         = xl(e,4,1) - xl(e,3,1)

            y12         = xl(e,2,2) - xl(e,1,2)
            y13         = xl(e,3,2) - xl(e,1,2)
            y14         = xl(e,4,2) - xl(e,1,2)
            y23         = xl(e,3,2) - xl(e,2,2)
            y24         = xl(e,4,2) - xl(e,2,2)
            y34         = xl(e,4,2) - xl(e,3,2)

            z12         = xl(e,2,3) - xl(e,1,3)
            z13         = xl(e,3,3) - xl(e,1,3)
            z14         = xl(e,4,3) - xl(e,1,3)
            z23         = xl(e,3,3) - xl(e,2,3)
            z24         = xl(e,4,3) - xl(e,2,3)
            z34         = xl(e,4,3) - xl(e,3,3)
            
c
c.. The calculation of the direction of a vertex is based on the average of
c.. the normals of the neighbor faces(3); And the calculation of the direction
c.. of the edge is based on the neighbor faces(2);
c
          lDir(e,1,1) = fct * (y14*(z12 - z13) + y12*(z13 - z14) 
     &                         + y13*(-z12 + z14))
          lDir(e,1,2) = fct * ( x14*(-z12 + z13) + x13*(z12 - z14) 
     &                          + x12*(-z13 + z14))
          lDir(e,1,3) = fct * ( x14*(y12 - y13)
     &                          + x12*(y13 - y14) + x13*(-y12 + y14))
c
          lDir(e,2,1) = fct * (-(y13*z12) + y14*z12 + y12*z13
     &                          - y12*z14 + y24*z23 - y23*z24)
            lDir(e,2,2) = fct * (x13*z12 - x14*z12-x12*z13 + x12*z14
     &                          - x24*z23 + x23*z24 )
            lDir(e,2,3) = fct * (-(x13*y12) + x14*y12 + x12*y13
     &                         - x12*y14 + x24*y23 - x23*y24)
c
            lDir(e,3,1) = fct * (y12*z13 - y14*z13 + y13*(-z12 + z14)
     &                         + y24*z23 - y23*z24)
            lDir(e,3,2) = fct * (-(x12*z13) + x14*z13 + x13*(z12 - z14)
     &                         - x24*z23 + x23*z24)
            lDir(e,3,3) = fct * (x12*y13 - x14*y13 + x13*(-y12 + y14)
     &                         + x24*y23 - x23*y24)
c
            lDir(e,4,1) = fct * (y14*(z12 - z13) - y12*z14 + y13*z14
     &                         + y24*z23 - y23*z24)
            lDir(e,4,2) = fct * (x14*(-z12 + z13) + x12*z14 - x13*z14
     &                         - x24*z23 + x23*z24)
            lDir(e,4,3) = fct * (x14*(y12 - y13) - x12*y14 + x13*y14
     &                         + x24*y23 - x23*y24)
c
         enddo
c
c.... quadratic tets
c
      else if (nshl .eq. 10 ) then
         fct = 1.d0 / 6.d0
         do e = 1, npro

            x12         = xl(e,2,1) - xl(e,1,1)
            x13         = xl(e,3,1) - xl(e,1,1)
            x14         = xl(e,4,1) - xl(e,1,1)
            x23         = xl(e,3,1) - xl(e,2,1)
            x24         = xl(e,4,1) - xl(e,2,1)
            x34         = xl(e,4,1) - xl(e,3,1)

            y12         = xl(e,2,2) - xl(e,1,2)
            y13         = xl(e,3,2) - xl(e,1,2)
            y14         = xl(e,4,2) - xl(e,1,2)
            y23         = xl(e,3,2) - xl(e,2,2)
            y24         = xl(e,4,2) - xl(e,2,2)
            y34         = xl(e,4,2) - xl(e,3,2)

            z12         = xl(e,2,3) - xl(e,1,3)
            z13         = xl(e,3,3) - xl(e,1,3)
            z14         = xl(e,4,3) - xl(e,1,3)
            z23         = xl(e,3,3) - xl(e,2,3)
            z24         = xl(e,4,3) - xl(e,2,3)
            z34         = xl(e,4,3) - xl(e,3,3)
            
c
c.... vertex modes
c            
          lDir(e,1,1) = fct * (y14*(z12 - z13) + y12*(z13 - z14) 
     &                         + y13*(-z12 + z14))
          lDir(e,1,2) = fct * ( x14*(-z12 + z13) + x13*(z12 - z14) 
     &                          + x12*(-z13 + z14))
          lDir(e,1,3) = fct * ( x14*(y12 - y13)
     &                          + x12*(y13 - y14) + x13*(-y12 + y14))
c
          lDir(e,2,1) = fct * (-(y13*z12) + y14*z12 + y12*z13
     &                          - y12*z14 + y24*z23 - y23*z24)
            lDir(e,2,2) = fct * (x13*z12 - x14*z12-x12*z13 + x12*z14
     &                          - x24*z23 + x23*z24 )
            lDir(e,2,3) = fct * (-(x13*y12) + x14*y12 + x12*y13
     &                         - x12*y14 + x24*y23 - x23*y24)
c
            lDir(e,3,1) = fct * (y12*z13 - y14*z13 + y13*(-z12 + z14)
     &                         + y24*z23 - y23*z24)
            lDir(e,3,2) = fct * (-(x12*z13) + x14*z13 + x13*(z12 - z14)
     &                         - x24*z23 + x23*z24)
            lDir(e,3,3) = fct * (x12*y13 - x14*y13 + x13*(-y12 + y14)
     &                         + x24*y23 - x23*y24)
c
            lDir(e,4,1) = fct * (y14*(z12 - z13) - y12*z14 + y13*z14
     &                         + y24*z23 - y23*z24)
            lDir(e,4,2) = fct * (x14*(-z12 + z13) + x12*z14 - x13*z14
     &                         - x24*z23 + x23*z24)
            lDir(e,4,3) = fct * (x14*(y12 - y13) - x12*y14 + x13*y14
     &                         + x24*y23 - x23*y24)
c
c.... edge modes (quadratic)
c
            lDir(e,5,1) = pt25*(-(y13*z12) + y14*z12 + y12*(z13-z14))
            lDir(e,5,2) = pt25*(x13*z12 - x14*z12 + x12*(-z13 + z14))
            lDir(e,5,3) = pt25*(-(x13*y12) + x14*y12 + x12*(y13 - y14))

            lDir(e,6,1) = pt25*(-(y13*z12) + y12*z13 + y24*z23-y23*z24)
            lDir(e,6,2) = pt25*(x13*z12 - x12*z13 - x24*z23 + x23*z24)
            lDir(e,6,3) = pt25*(-(x13*y12) + x12*y13 + x24*y23-x23*y24)

            lDir(e,7,1) = pt25*((y12 - y14)*z13 + y13*(-z12 + z14))
            lDir(e,7,2) = pt25*((-x12 + x14)*z13 + x13*(z12 - z14))
            lDir(e,7,3) = pt25*((x12 - x14)*y13 + x13*(-y12 + y14))

            lDir(e,8,1) = pt25*(y14*(z12 - z13) + (-y12 + y13)*z14)
            lDir(e,8,2) = pt25*(x14*(-z12 + z13) + (x12 - x13)*z14)
            lDir(e,8,3) = pt25*(x14*(y12 - y13) + (-x12 + x13)*y14)

            lDir(e,9,1) = pt25*(y14*z12 - y12*z14 + y24*z23 - y23*z24)
            lDir(e,9,2) = pt25*(-(x14*z12) + x12*z14 - x24*z23+x23*z24)
            lDir(e,9,3) = pt25*(x14*y12 - x12*y14 + x24*y23 - x23*y24)

            lDir(e,10,1) = pt25*(-(y14*z13) + y13*z14+y24*z23-y23*z24)
            lDir(e,10,2) = pt25*(x14*z13 - x13*z14-x24*z23 + x23*z24)
            lDir(e,10,3) = pt25*(-(x14*y13) + x13*y14+x24*y23-x23*y24)


         enddo
c
c.... cubic tets
c
      else if (nshl .eq. 20 ) then
         fct = 1.d0 / 6.d0
         do e = 1, npro

            x12         = xl(e,2,1) - xl(e,1,1)
            x13         = xl(e,3,1) - xl(e,1,1)
            x14         = xl(e,4,1) - xl(e,1,1)
            x23         = xl(e,3,1) - xl(e,2,1)
            x24         = xl(e,4,1) - xl(e,2,1)
c$$$            x34         = xl(e,4,1) - xl(e,3,1)

            y12         = xl(e,2,2) - xl(e,1,2)
            y13         = xl(e,3,2) - xl(e,1,2)
            y14         = xl(e,4,2) - xl(e,1,2)
            y23         = xl(e,3,2) - xl(e,2,2)
            y24         = xl(e,4,2) - xl(e,2,2)
c$$$            y34         = xl(e,4,2) - xl(e,3,2)

            z12         = xl(e,2,3) - xl(e,1,3)
            z13         = xl(e,3,3) - xl(e,1,3)
            z14         = xl(e,4,3) - xl(e,1,3)
            z23         = xl(e,3,3) - xl(e,2,3)
            z24         = xl(e,4,3) - xl(e,2,3)
c$$$            z34         = xl(e,4,3) - xl(e,3,3)
            
c
c.... vertex modes
c            
          lDir(e,1,1) = fct * (y14*(z12 - z13) + y12*(z13 - z14) 
     &                         + y13*(-z12 + z14))
          lDir(e,1,2) = fct * ( x14*(-z12 + z13) + x13*(z12 - z14) 
     &                          + x12*(-z13 + z14))
          lDir(e,1,3) = fct * ( x14*(y12 - y13)
     &                          + x12*(y13 - y14) + x13*(-y12 + y14))
c
          lDir(e,2,1) = fct * (-(y13*z12) + y14*z12 + y12*z13
     &                          - y12*z14 + y24*z23 - y23*z24)
            lDir(e,2,2) = fct * (x13*z12 - x14*z12-x12*z13 + x12*z14
     &                          - x24*z23 + x23*z24 )
            lDir(e,2,3) = fct * (-(x13*y12) + x14*y12 + x12*y13
     &                         - x12*y14 + x24*y23 - x23*y24)
c
            lDir(e,3,1) = fct * (y12*z13 - y14*z13 + y13*(-z12 + z14)
     &                         + y24*z23 - y23*z24)
            lDir(e,3,2) = fct * (-(x12*z13) + x14*z13 + x13*(z12 - z14)
     &                         - x24*z23 + x23*z24)
            lDir(e,3,3) = fct * (x12*y13 - x14*y13 + x13*(-y12 + y14)
     &                         + x24*y23 - x23*y24)
c
            lDir(e,4,1) = fct * (y14*(z12 - z13) - y12*z14 + y13*z14
     &                         + y24*z23 - y23*z24)
            lDir(e,4,2) = fct * (x14*(-z12 + z13) + x12*z14 - x13*z14
     &                         - x24*z23 + x23*z24)
            lDir(e,4,3) = fct * (x14*(y12 - y13) - x12*y14 + x13*y14
     &                         + x24*y23 - x23*y24)
c
c.... edge modes (quadratic and cubic)
c
            lDir(e,5,1) = pt25*(-(y13*z12) + y14*z12 + y12*(z13-z14))
            lDir(e,5,2) = pt25*(x13*z12 - x14*z12 + x12*(-z13 + z14))
            lDir(e,5,3) = pt25*(-(x13*y12) + x14*y12 + x12*(y13 - y14))
            lDir(e,6,1) = lDir(e,5,1)
            lDir(e,6,2) = lDir(e,5,2)
            lDir(e,6,3) = lDir(e,5,3)

            lDir(e,7,1) = pt25*(-(y13*z12) + y12*z13 + y24*z23-y23*z24)
            lDir(e,7,2) = pt25*(x13*z12 - x12*z13 - x24*z23 + x23*z24)
            lDir(e,7,3) = pt25*(-(x13*y12) + x12*y13 + x24*y23-x23*y24)
            lDir(e,8,1) = lDir(e,7,1)
            lDir(e,8,2) = lDir(e,7,2)
            lDir(e,8,3) = lDir(e,7,3)

            lDir(e,9,1) = pt25*((y12 - y14)*z13 + y13*(-z12 + z14))
            lDir(e,9,2) = pt25*((-x12 + x14)*z13 + x13*(z12 - z14))
            lDir(e,9,3) = pt25*((x12 - x14)*y13 + x13*(-y12 + y14))
            lDir(e,10,1) = lDir(e,9,1)
            lDir(e,10,2) = lDir(e,9,2)
            lDir(e,10,3) = lDir(e,9,3)

            lDir(e,11,1) = pt25*(y14*(z12 - z13) + (-y12 + y13)*z14)
            lDir(e,11,2) = pt25*(x14*(-z12 + z13) + (x12 - x13)*z14)
            lDir(e,11,3) = pt25*(x14*(y12 - y13) + (-x12 + x13)*y14)
            lDir(e,12,1) = lDir(e,11,1)
            lDir(e,12,2) = lDir(e,11,2)
            lDir(e,12,3) = lDir(e,11,3)


            lDir(e,13,1) = pt25*(y14*z12 - y12*z14 + y24*z23 - y23*z24)
            lDir(e,13,2) = pt25*(-(x14*z12) + x12*z14-x24*z23+x23*z24)
            lDir(e,13,3) = pt25*(x14*y12 - x12*y14 + x24*y23 - x23*y24)
            lDir(e,14,1) = lDir(e,13,1)
            lDir(e,14,2) = lDir(e,13,2)
            lDir(e,14,3) = lDir(e,13,3)

            lDir(e,15,1) = pt25*(-(y14*z13) + y13*z14+y24*z23-y23*z24)
            lDir(e,15,2) = pt25*(x14*z13 - x13*z14-x24*z23 + x23*z24)
            lDir(e,15,3) = pt25*(-(x14*y13) + x13*y14+x24*y23-x23*y24)
            lDir(e,16,1) = lDir(e,15,1)
            lDir(e,16,2) = lDir(e,15,2)
            lDir(e,16,3) = lDir(e,15,3)
c
c.... face modes (cubic)
c
            lDir(e,17,1) = pt5*(-(y13*z12) + y12*z13)
            lDir(e,17,2) = pt5*(x13*z12 - x12*z13)
            lDir(e,17,3) = pt5*(-(x13*y12) + x12*y13)

            lDir(e,18,1) = pt5*(y14*z12 - y12*z14)
            lDir(e,18,2) = pt5*(-(x14*z12) + x12*z14)
            lDir(e,18,3) = pt5*(x14*y12 - x12*y14)

            lDir(e,19,1) = pt5*(y24*z23 - y23*z24)
            lDir(e,19,2) = pt5*(-(x24*z23) + x23*z24)
            lDir(e,19,3) = pt5*(x24*y23 - x23*y24)

            lDir(e,20,1) = pt5*(-(y14*z13) + y13*z14)
            lDir(e,20,2) = pt5*(x14*z13 - x13*z14)
            lDir(e,20,3) = pt5*(-(x14*y13) + x13*y14)

         enddo
c
c.... hexes
c     
      else if (nenl .eq. 8) then
         fct = 1.d0 / 12.d0
         do e = 1, npro
          x13     = xl(e,1,1) - xl(e,3,1)
          x16     = xl(e,1,1) - xl(e,6,1)
          x18     = xl(e,1,1) - xl(e,8,1)
          x24     = xl(e,2,1) - xl(e,4,1)
          x25     = xl(e,2,1) - xl(e,5,1)
          x27     = xl(e,2,1) - xl(e,7,1)
          x36     = xl(e,3,1) - xl(e,6,1)
          x38     = xl(e,3,1) - xl(e,8,1)
          x45     = xl(e,4,1) - xl(e,5,1)
          x47     = xl(e,4,1) - xl(e,7,1)
          x57     = xl(e,5,1) - xl(e,7,1)
          x68     = xl(e,6,1) - xl(e,8,1)
c
          y13     = xl(e,1,2) - xl(e,3,2)
          y16     = xl(e,1,2) - xl(e,6,2)
          y18     = xl(e,1,2) - xl(e,8,2)
          y24     = xl(e,2,2) - xl(e,4,2)
          y25     = xl(e,2,2) - xl(e,5,2)
          y27     = xl(e,2,2) - xl(e,7,2)
          y36     = xl(e,3,2) - xl(e,6,2)
          y38     = xl(e,3,2) - xl(e,8,2)
          y45     = xl(e,4,2) - xl(e,5,2)
          y47     = xl(e,4,2) - xl(e,7,2)
          y57     = xl(e,5,2) - xl(e,7,2)
          y68     = xl(e,6,2) - xl(e,8,2)
c
          z13     = xl(e,1,3) - xl(e,3,3)
          z16     = xl(e,1,3) - xl(e,6,3)
          z18     = xl(e,1,3) - xl(e,8,3)
          z24     = xl(e,2,3) - xl(e,4,3)
          z25     = xl(e,2,3) - xl(e,5,3)
          z27     = xl(e,2,3) - xl(e,7,3)
          z36     = xl(e,3,3) - xl(e,6,3)
          z38     = xl(e,3,3) - xl(e,8,3)
          z45     = xl(e,4,3) - xl(e,5,3)
          z47     = xl(e,4,3) - xl(e,7,3)
          z57     = xl(e,5,3) - xl(e,7,3)
          z68     = xl(e,6,3) - xl(e,8,3)
c
      x31= -x13
      x61= -x16
      x81= -x18
      x42= -x24
      x52= -x25
      x72= -x27
      x63= -x36
      x83= -x38
      x54= -x45
      x74= -x47
      x75= -x57
      x86= -x68
      y31= -y13
      y61= -y16
      y81= -y18
      y42= -y24
      y52= -y25
      y72= -y27
      y63= -y36
      y83= -y38
      y54= -y45
      y74= -y47
      y75= -y57
      y86= -y68
      z31= -z13
      z61= -z16
      z81= -z18
      z42= -z24
      z52= -z25
      z72= -z27
      z63= -z36
      z83= -z38
      z54= -z45
      z74= -z47
      z75= -z57
      z86= -z68

          lDir(e,1,1) = fct * (-y24 * z45 + y36 * z24 - y68 * z45 
     1                + z24 * y45 - z36 * y24 + z68 * y45 )
          lDir(e,2,1) = fct * (-y16 * z63 + y54 * z16 - y47 * z63 
     1                + z16 * y63 - z54 * y16 + z47 * y63 )
          lDir(e,3,1) = fct * (-y42 * z27 + y18 * z42 - y86 * z27 
     1                + z42 * y27 - z18 * y42 + z86 * y27 )
          lDir(e,4,1) = fct * (-y38 * z81 + y72 * z38 - y25 * z81 
     1                + z38 * y81 - z72 * y38 + z25 * y81 )
          lDir(e,5,1) = fct * (-y61 * z18 + y27 * z61 - y74 * z18 
     1                + z61 * y18 - z27 * y61 + z74 * y18 )
          lDir(e,6,1) = fct * (-y57 * z72 + y81 * z57 - y13 * z72 
     1                + z57 * y72 - z81 * y57 + z13 * y72 )
          lDir(e,7,1) = fct * (-y83 * z36 + y45 * z83 - y52 * z36 
     1                + z83 * y36 - z45 * y83 + z52 * y36 )
          lDir(e,8,1) = fct * (-y75 * z54 + y63 * z75 - y31 * z54 
     1                + z75 * y54 - z63 * y75 + z31 * y54 )
c
          lDir(e,1,2) = fct * (-z24 * x45 + z36 * x24 - z68 * x45 
     1                + x24 * z45 - x36 * z24 + x68 * z45 )
          lDir(e,2,2) = fct * (-z16 * x63 + z54 * x16 - z47 * x63 
     1                + x16 * z63 - x54 * z16 + x47 * z63 )
          lDir(e,3,2) = fct * (-z42 * x27 + z18 * x42 - z86 * x27 
     1                + x42 * z27 - x18 * z42 + x86 * z27 )
          lDir(e,4,2) = fct * (-z38 * x81 + z72 * x38 - z25 * x81 
     1                + x38 * z81 - x72 * z38 + x25 * z81 )
          lDir(e,5,2) = fct * (-z61 * x18 + z27 * x61 - z74 * x18 
     1                + x61 * z18 - x27 * z61 + x74 * z18 )
          lDir(e,6,2) = fct * (-z57 * x72 + z81 * x57 - z13 * x72 
     1                + x57 * z72 - x81 * z57 + x13 * z72 )
          lDir(e,7,2) = fct * (-z83 * x36 + z45 * x83 - z52 * x36 
     1                + x83 * z36 - x45 * z83 + x52 * z36 )
          lDir(e,8,2) = fct * (-z75 * x54 + z63 * x75 - z31 * x54 
     1                + x75 * z54 - x63 * z75 + x31 * z54 )
c
          lDir(e,1,3) = fct * (-x24 * y45 + x36 * y24 - x68 * y45 
     1                + y24 * x45 - y36 * x24 + y68 * x45 )
          lDir(e,2,3) = fct * (-x16 * y63 + x54 * y16 - x47 * y63 
     1                + y16 * x63 - y54 * x16 + y47 * x63 )
          lDir(e,3,3) = fct * (-x42 * y27 + x18 * y42 - x86 * y27 
     1                + y42 * x27 - y18 * x42 + y86 * x27 )
          lDir(e,4,3) = fct * (-x38 * y81 + x72 * y38 - x25 * y81 
     1                + y38 * x81 - y72 * x38 + y25 * x81 )
          lDir(e,5,3) = fct * (-x61 * y18 + x27 * y61 - x74 * y18 
     1                + y61 * x18 - y27 * x61 + y74 * x18 )
          lDir(e,6,3) = fct * (-x57 * y72 + x81 * y57 - x13 * y72 
     1                + y57 * x72 - y81 * x57 + y13 * x72 )
          lDir(e,7,3) = fct * (-x83 * y36 + x45 * y83 - x52 * y36 
     1                + y83 * x36 - y45 * x83 + y52 * x36 )
          lDir(e,8,3) = fct * (-x75 * y54 + x63 * y75 - x31 * y54 
     1                + y75 * x54 - y63 * x75 + y31 * x54 )
c
         enddo
      else
         write(*,*) 'Error in e3sts: elt type not impl.'
         stop
      endif
      
      return
      end
         
!> Compute the necessary terms for the statistics projection
!! matrices.

      subroutine elmStatsLhs( x,  iBC,   iper,  ilwork )

      use     stats
      use     pointer_data
      
        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/workfc.h"
C
C     Local variables
C
      INTEGER             i,           iblk,        iel,         j
C
      real*8  x(numnp,3)
      integer iBC(nshg), iper(nshg), ilwork(nlwork)
      
      real*8, allocatable :: xl(:,:,:)
      real*8, allocatable :: lStsVec(:,:,:)

c
c.... loop over element blocks
c
      stsVec = zero
      
      do iblk = 1, nelblk
         iel    = lcblk(1,iblk)
         lcsyst = lcblk(3,iblk)
         nenl   = lcblk(5,iblk) ! no. of vertices per element
         nshl   = lcblk(10,iblk)
         ndofl  = lcblk(8,iblk)
         npro   = lcblk(1,iblk+1) - iel 

         allocate ( xl(npro,nenl,3)             )
         allocate ( lStsVec(npro,nshl,nResDims) )
c
c.... localize needed data
c
         call localx ( x,    xl,  mien(iblk)%p, nsd,   'gather  ' )
c
c.... form the Lhs
c
         call e3StsLhs( xl, lStsVec )
c
c.... assemble
c
         call local (stsVec, lStsVec, mien(iblk)%p,
     &               nResDims, 'scatter ' ) 

         deallocate ( xl       )
         deallocate ( lStsVec  )
c
c.... end loop over element blocks
c
      enddo

      if (numpe > 1) then
        call commu (stsVec, ilwork, nResDims  , 'in ')
      endif
c
c.... local periodic boundary conditions (no communications)
c
      do j = 1,nshg
         if (btest(iBC(j),10)) then
            i = iper(j)
            stsVec(i,:) = stsVec(i,:) + stsVec(j,:)
         endif
      enddo
c
      do i = 1,nshg
         stsVec(i,:) = stsVec(iper(i),:)
      enddo
      if (numpe > 1) then
        call commu (stsVec, ilwork, nResDims  , 'out')
      endif

      return
      end
      
!> Assemble the residual for the statistics

      subroutine elmStatsRes( y,        ac,    x,      shp,     shgl, 
     &                        shpb,     shglb,       iBC,     BC, 
     &                        iper,     ilwork,      rowp,    colm,
     &                        lhsK,     lhsP )
      
      use     stats
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
  
C
C     Local variables
C
      INTEGER ierrcalctmp
C
      REAL*8                rjunk
C      
      real*8  y(nshg,ndof),             ac(nshg,ndof), x(numnp,nsd),
     &        shp(MAXTOP,maxsh,MAXQPT),  shgl(MAXTOP,nsd,maxsh,MAXQPT),
     &        shpb(MAXTOP,maxsh,MAXQPT),
     &        shglb(MAXTOP,nsd,maxsh,MAXQPT),
     &        BC(nshg,ndofBC),          lhsK(9,nnz_tot),
     &        lhsP(4,nnz_tot),         res(nshg,ndof)

      integer iBC(nshg),                iper(nshg),
     &        ilwork(nlwork),           rowp(nshg,nnz),
     &        colm(nshg+1)
      

      lhs    = 0
      stsVec = zero
      
      stsResFlg = 1

      call ElmGMR (y,         ac,         x,
     &             shp,       shgl,       iBC,       
     &             BC,        shpb,       shglb,
     &             res,       iper,       ilwork,   
     &             rowp,      colm,       lhsK,      
     &             lhsP,      rjunk   )

      stsResFlg = 0

      return 
      end

!> This utility routine prints out the error and stops the program.
!!
!! input:
!! @param[in] routin name of the routine where the error occurred
!! @param[in] variab an 8-character error message
!! @param[in] num any integer number associated with the error

        subroutine error (routin, variab, num)
c
        include "global.h"
        include "mpif.h"
        include "common_blocks/mio.h"
        include "common_blocks/mioname.h"
        include "common_blocks/timer4.h"
        include "common_blocks/title.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER num
C
C     Local variables
C
      INTEGER ierchk
C
c
      character*8 routin, variab
c
        data ierchk /0/
c
c.... check for redundant error
c
        if (ierchk .eq. 1) stop
        ierchk = 1
c
c.... open file
c
        open (unit=ierror, file=ferror, status='unknown')
c
c.... print the error
c
        write (*,1000) title, routin, variab, num
        if (num .ne. 0) write (ierror,1000) title, routin, variab, num
        if (num .eq. 0) write (ierror,1000) title, routin, variab
c
c.... halt the process
c
        close (ierror)


        WRITE(6,'(A,G14.6)') 'Life: ',death - birth
        if (numpe > 1) then
           call MPI_ABORT(MPI_COMM_WORLD)
        endif
        
 
1000    format(' ',a80,//,
     &         ' ****** Error occurred in routine <',a8,'>',/,
     &          '  Error code :',a8,:,' : ',i8,//)
        end

      subroutine errsmooth(rerr,   x,     iper,   ilwork, 
     &                     shp,    shgl,  iBC)
c
        use pointer_data
c
        include "global.h"
        include "mpif.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/workfc.h"
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                rerr,        shgl,        shp,         x
C
C     Local variables
C
      INTEGER             i,           iblk,        iel,         j
C
      REAL*8                rerrsm,      rmass,       shglb,       shpb

C
c
        dimension shp(MAXTOP,maxsh,MAXQPT),  
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
        dimension rerrsm(nshg, 10), 
     &            rerr(nshg,10), 
     &            x(numnp,nsd), 
     &            rmass(nshg)
c
        dimension ilwork(nlwork), iBC(nshg), iper(nshg)

        real*8, allocatable :: tmpshp(:,:), tmpshgl(:,:,:)
        real*8, allocatable :: tmpshpb(:,:), tmpshglb(:,:,:)

c
c loop over element blocks for the global reconstruction
c of the smoothed error and lumped mass matrix, rmass
c
        rerrsm = zero
        rmass = zero
        
        do iblk = 1, nelblk
c
c.... set up the parameters
c
          nenl   = lcblk(5,iblk)   ! no. of vertices per element
          iel    = lcblk(1,iblk)
          lelCat = lcblk(2,iblk)
          lcsyst = lcblk(3,iblk)
          iorder = lcblk(4,iblk)
          nenl   = lcblk(5,iblk)   ! no. of vertices per element
          nshl   = lcblk(10,iblk)
          mattyp = lcblk(7,iblk)
          ndofl  = lcblk(8,iblk)
          nsymdl = lcblk(9,iblk)
          npro   = lcblk(1,iblk+1) - iel
          ngauss = nint(lcsyst)
c
c.... compute and assemble diffusive flux vector residual, qres,
c     and lumped mass matrix, rmass

          allocate (tmpshp(nshl,MAXQPT))
          allocate (tmpshgl(nsd,nshl,MAXQPT))

          tmpshp(1:nshl,:) = shp(lcsyst,1:nshl,:)
          tmpshgl(:,1:nshl,:) = shgl(lcsyst,:,1:nshl,:)

          call smooth (rerr,  
     &                 x,                       
     &                 tmpshp,              
     &                 tmpshgl,
     &                 mien(iblk)%p,
     &                 rerrsm,                   
     &                 rmass)

          deallocate ( tmpshp )
          deallocate ( tmpshgl ) 
       enddo
c
       if (numpe > 1) then
          call commu (rerrsm , ilwork,  10   , 'in ')
          call commu (rmass  , ilwork,  1    , 'in ')
       endif       
c
c.... take care of periodic boundary conditions
c
        do j= 1,nshg
          if ((btest(iBC(j),10))) then
            i = iper(j)
            rmass(i) = rmass(i) + rmass(j)
            rerrsm(i,:) = rerrsm(i,:) + rerrsm(j,:)
          endif
        enddo

        do j= 1,nshg
          if ((btest(iBC(j),10))) then
            i = iper(j)
            rmass(j) = rmass(i)
            rerrsm(j,:) = rerrsm(i,:)
          endif
        enddo
c
c.... invert the diagonal mass matrix and find q
c
        rmass = one/rmass
       
       do i=1, 10
          rerrsm(:,i) = rmass*rerrsm(:,i)
       enddo
       if(numpe > 1) then
          call commu (rerrsm, ilwork, 10, 'out')    
       endif
c
c      copy the smoothed error overwriting the original error.
c

       rerr = rerrsm 

       return
       end

!> This routine computes and assembles the data corresponding to the
!! interior elements for the global reconstruction of the diffusive
!! flux vector.
!!
!! input:<BR>
!! @param[in] y(nshg,ndof) Y variables
!! @param[in] x(numnp,nsd) Nodal coordinates
!! @param[in] shp(nshape,ngauss) Element shape-functions
!! @param[in] shgl(nsd,nshape,ngauss) Element local shape-function gradients
!! @param[in] ien(npro) Nodal connectivity array
!!
!! output:<BR>
!! @param[out] qres(nshg,nflow-1,nsd) Residual vector for diffusive flux
!! @param[out] rmass(nshg) Lumped mass matrix

        subroutine smooth (rerr,       x,       shp,
     &                     shgl,       ien,          
     &                     rerrsm,     rmass    )
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

c
C     Argument variables
C
      INTEGER             ien
C
      REAL*8                rerr,        rerrsm,      rmass,       shgl
      REAL*8                shp,         x
C
C     Local variables
C
      INTEGER             i,           j,           n
C
      REAL*8                dxidx,       error,       rerrl,    rerrsml
      REAL*8                rmassl,      sgn,         shape,      shdrv
      REAL*8                shg,         wdetj,       xl
C
        dimension rerr(nshg,10),               x(numnp,nsd),     
     &            shp(nshl,maxsh),  
     &            shgl(nsd,nshl,maxsh),
     &            ien(npro,nshl),
     &            rerrsm(nshg,10),    rmass(nshg)
c
c.... element level declarations
c
        dimension rerrl(npro,nshl,10),        xl(npro,nenl,nsd),         
     &            rerrsml(npro,nshl,10),       rmassl(npro,nshl)
c
        dimension sgn(npro,nshl),          shape(npro,nshl),
     &            shdrv(npro,nsd,nshl),    WdetJ(npro),
     &            dxidx(npro,nsd,nsd),     shg(npro,nshl,nsd)
c
        dimension error(npro,10)
c
c.... create the matrix of mode signs for the hierarchic basis 
c     functions. 
c
        if (ipord .gt. 1) then
           call getsgn(ien,sgn)
        endif
c
c.... gather the variables
c

        call local(rerr,   rerrl,  ien,    10,   'gather  ')
        call localx(x,      xl,     ien,    nsd,    'gather  ')
c
c.... get the element residuals 
c
        rerrsml     = zero
        rmassl      = zero

c
c.... loop through the integration points
c
        
                
        do intp = 1, ngauss
        if (Qwt(lcsyst,intp) .eq. zero) cycle          ! precaution
c
c.... create a matrix of shape functions (and derivatives) for each
c     element at this quadrature point. These arrays will contain 
c     the correct signs for the hierarchic basis
c
        call getshp(shp,          shgl,      sgn, 
     &              shape,        shdrv)
c
        call e3metric( xl,         shdrv,        dxidx,  
     &                 shg,        WdetJ)
        error=zero
        do n = 1, nshl
           do i=1,10
              error(:,i)=error(:,i) + shape(:,n) * rerrl(:,n,i)
           enddo
        enddo
        do i=1,nshl
           do j=1,10
              rerrsml(:,i,j)  = rerrsml(:,i,j)  
     &                       + shape(:,i)*WdetJ*error(:,j)
           enddo

           rmassl(:,i) = rmassl(:,i) + shape(:,i)*WdetJ
        enddo
 
c.... end of the loop over integration points
c
      enddo
c
c.... assemble the diffusive flux residual 
c
        call local (rerrsm,   rerrsml,  ien,  10,'scatter ')
        call local (rmass,   rmassl,  ien,  1,  'scatter ')
c

      return
      end

!> This subroutine computes the element LHS matrix and the normal 
!! to the boundary for computation of (output) boundary fluxes. 
!! 
!! input:<BR>
!! @param[in] shpb(nen,nintg) Boundary element shape-functions
!! @param[in] shglb(nsd,nen,nintg) Boundary element grad-shape-functions
!! @param[in] wghtb(nintg) Boundary element weight
!! @param[in] xlb(npro,nenl,nsd) Nodal coordinates
!! @param[in] sgn(npro,nshl) Mode signs for hierarchic basis
!!
!! output:<BR>
!! @param[out] flhsl(npro,nenl,1) Element lumped lhs on flux boundary
!! @param[out] fnrml(npro,nenl,nsd) RHS of LS projection of normal to flux boundary
!!
!! Note: Special lumping technique is used to compute the LHS. 
!!       See T.J.R. Hughes, "The Finite Element Method: Linear 
!!       Static and Dynamic Finite Element Analysis", page 445.  
!!
!! Note: Least-squares projection is used to compute the normal to
!!       the boundary at the nodes.  This routine provides the element
!!       contribution to the RHS of the projection linear system.

      subroutine f3lhs (shpb,   shglb,  xlb,    flhsl,
     &                  fnrml,  sgn )
c
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
C     Argument variables
C
      REAL*8                flhsl,       fnrml,       sgn,        shglb
      REAL*8                shpb,        xlb
C
C     Local variables
C
      INTEGER             n
C
      REAL*8                bnorm,       fmstot,      shape,      shdrv
      REAL*8                temp,        temp1,       temp2,      temp3
      REAL*8                v1,          v2,          wdetjb
C
      dimension shpb(nshl,ngaussb),        shglb(nsd,nshl,ngaussb),
     &          xlb(npro,nenl,nsd),
     &          flhsl(npro,nshl,1),        fnrml(npro,nshl,nsd)
c
      dimension WdetJb(npro),
     &          bnorm(npro,nsd),           fmstot(npro),
     &          temp(npro),                temp1(npro),
     &          temp2(npro),               temp3(npro)

      dimension sgn(npro,nshl),            shape(npro,nshl),
     &          shdrv(npro,nsd,nshl),
     &          v1(npro,nsd),              v2(npro,nsd)
c
c.... integrate the lumped LHS matrix and normal
c
      fmstot = zero
c
c
c.... compute the normal to the boundary 
c

      v1 = xlb(:,2,:) - xlb(:,1,:)
      v2 = xlb(:,3,:) - xlb(:,1,:)
      
      if (lcsyst .eq. 1) then
         temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
         temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
         temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)
      else 
         temp1 = - v1(:,2) * v2(:,3) + v2(:,2) * v1(:,3)
         temp2 = - v2(:,1) * v1(:,3) + v1(:,1) * v2(:,3)
         temp3 = - v1(:,1) * v2(:,2) + v2(:,1) * v1(:,2)
      endif
c     
      temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
      bnorm(:,1) = temp1 * temp
      bnorm(:,2) = temp2 * temp
      bnorm(:,3) = temp3 * temp
      
      do intp = 1, ngaussb
c
c.... get the hierarchic shape functions at this int point
c
c        BUG FIX - DES - 30JAN2014 was getshp
         call getshpb(shpb,        shglb,        sgn, 
     &               shape,       shdrv)
c
         WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
c
c.... compute the lumped LHS and normal
c          
         do n = 1, nenl ! when changed to nshl solution degraded ipord 10
            flhsl(:,n,1) = flhsl(:,n,1) + WdetJb * shape(:,n)

c for curved geometries the below construct for the normals has to be used
            fnrml(:,n,1) = fnrml(:,n,1) + WdetJb * bnorm(:,1)
     &                                           * shape(:,n)
            fnrml(:,n,2) = fnrml(:,n,2) + WdetJb * bnorm(:,2)
     &                                           * shape(:,n)
            fnrml(:,n,3) = fnrml(:,n,3) + WdetJb * bnorm(:,3)
     &                                           * shape(:,n)
          enddo
c
c  To best represent this case it should be assigned to the vertex 
c  modes and higher entities should get zero as is done below
c
          fmstot = fmstot + WdetJb
c
        enddo
        
c$$$        do i=1,nenl
c$$$           fnrml(:,i,:)=bnorm(:,:)
c$$$        enddo
        if(ipord.gt.1)  fnrml(:,nenl:nshl,:)=zero
c
c.... scale the LHS matrix contribution
c
        temp = zero
        do n = 1, nshl
           temp = temp + flhsl(:,n,1)
        enddo
c
        do n = 1, nshl
           flhsl(:,n,1) = flhsl(:,n,1) * fmstot / temp
        enddo
c
c.... return
c
        return
        end

      subroutine fillsparseI( iens, xKebe, lhsK,
     &                               xGoC,      lhsP,
     1                         row, col)
c
c
c
      use LagrangeMultipliers 
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
      real*8 xKebe(npro,9,nshl,nshl), xGoC(npro,4,nshl,nshl)
      integer ien(npro,nshl), col(nshg+1), row(nshg*nnz)
      real*8 lhsK(9,nnz_tot), lhsP(4,nnz_tot)
c
      integer aa, b, c, e, i, k, n,  j,  l
c
      integer sparseloc

      integer iens(npro,nshl)
c
c prefer to show explicit absolute value needed for cubic modes and
c higher rather than inline abs on pointer as in past versions
c iens is the signed ien array ien is unsigned
c
      ien=abs(iens)
c       
c.... Accumulate the lhs
c
      do e = 1, npro ! loop over the elements
          do aa = 1, nshl ! loop over the local equation numbers
            i = ien(e,aa) ! finds the global equation number or
                        ! block-row of our matrix
            c = col(i)    ! starting point to look for the matching column
            n = col(i+1) - c  !length of the list of entries in rowp
            do b = 1, nshl ! local variable number tangent respect
                         ! to
c function that searches row until it finds the match that gives the
c               global equation number

                k = sparseloc( row(c), n, ien(e,b) ) + c-1
c
c                                             *         *
c                   dimension egmass(npro,ndof,nenl,ndof,nenl)
c
c compressible      lhsT(1:5,1:5,k)=lhsT(1:5,1:5,k)+egmass(e,1:5,aa,1:5,b)
c
                lhsK(1,k) = lhsK(1,k) + xKebe(e,1,aa,b)
                lhsK(2,k) = lhsK(2,k) + xKebe(e,2,aa,b)
                lhsK(3,k) = lhsK(3,k) + xKebe(e,3,aa,b)
                lhsK(4,k) = lhsK(4,k) + xKebe(e,4,aa,b)
                lhsK(5,k) = lhsK(5,k) + xKebe(e,5,aa,b)
                lhsK(6,k) = lhsK(6,k) + xKebe(e,6,aa,b)
                lhsK(7,k) = lhsK(7,k) + xKebe(e,7,aa,b)
                lhsK(8,k) = lhsK(8,k) + xKebe(e,8,aa,b)
                lhsK(9,k) = lhsK(9,k) + xKebe(e,9,aa,b)
c
                lhsP(1,k) = lhsP(1,k) + xGoC(e,1,aa,b)
                lhsP(2,k) = lhsP(2,k) + xGoC(e,2,aa,b)
                lhsP(3,k) = lhsP(3,k) + xGoC(e,3,aa,b)
                lhsP(4,k) = lhsP(4,k) + xGoC(e,4,aa,b)
            enddo
          enddo
      enddo
c
c..... lshLagL is required to calculate the LHS and RHS of Lagrange Multipliers
c..... It should be assembled before calling CalcNANBLagrange
c
      if(Lagrange.gt.zero) then
         do e = 1, npro ! loop over the elements
           do aa = 1, nshlb ! loop over the local equation numbers
                      i = ien(e,aa) 
                      c = col(i)    
                      n = col(i+1) - c  
                      do b = 1, nshlb
                         k = sparseloc( row(c), n, ien(e,b) ) + c-1
c
                             lhsLagL(1,k,:)=lhsLagL(1,k,:)+
     &                  loclhsLag(e,1,aa,b,:)
                             lhsLagL(2,k,:)=lhsLagL(2,k,:)+
     &                  loclhsLag(e,2,aa,b,:)
                         lhsLagL(3,k,:)=lhsLagL(3,k,:)+
     &                  loclhsLag(e,3,aa,b,:)
                             lhsLagL(4,k,:)=lhsLagL(4,k,:)+
     &                  loclhsLag(e,4,aa,b,:)
                         lhsLagL(5,k,:)=lhsLagL(5,k,:)+
     &                  loclhsLag(e,5,aa,b,:)
                         lhsLagL(6,k,:)=lhsLagL(6,k,:)+
     &                  loclhsLag(e,6,aa,b,:)
                         lhsLagL(7,k,:)=lhsLagL(7,k,:)+
     &                  loclhsLag(e,7,aa,b,:)
                         lhsLagL(8,k,:)=lhsLagL(8,k,:)+
     &                  loclhsLag(e,8,aa,b,:)
                         lhsLagL(9,k,:)=lhsLagL(9,k,:)+
     &                  loclhsLag(e,9,aa,b,:)
                  enddo
            enddo
          enddo
      endif
c
c.... end
c
      return
      end



      subroutine fillsparseSclr(   iens,      xSebe, lhsS,
     1                              row,      col)
c
c
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

      real*8 xSebe(npro,nshl,nshl)
      integer ien(npro,nshl), col(nshg+1), row(nshg*nnz)
      real*8  lhsS(nnz_tot) 
c
      integer      aa,      b,      c,      e,      i,      k,      n
c
      integer sparseloc

      integer iens(npro,nshl)
c
c prefer to show explicit absolute value needed for cubic modes and
c higher rather than inline abs on pointer as in past versions
c iens is the signed ien array ien is unsigned
c
      ien=abs(iens)
c
c.... Accumulate the lhs
c
      do e = 1, npro
          do aa = 1, nshl
            i = ien(e,aa)
            c = col(i)
            n = col(i+1) - c
            do b = 1, nshl
                k = sparseloc( row(c), n, ien(e,b) ) + c-1
c
                lhsS(k) = lhsS(k) + xSebe(e,aa,b)
            enddo
          enddo
      enddo
c
c.... end
c
      return
      end

!> This function finds the location of the non-zero elements
!! of the LHS matrix in the sparsely stored matrix 
!! lhsK(nflow*nflow,nnz*numnp)
!!
!! Nahid Razmara, Spring 2000.       (Sparse Matrix)

      integer function sparseloc( list, n, target )


      integer      list(n),      n,      target
      integer      rowvl,      rowvh,      rowv

c
c.... Initialize
c
      rowvl = 1
      rowvh = n + 1
c
c.... do a binary search
c
100   if ( rowvh-rowvl .gt. 1 ) then
          rowv = ( rowvh + rowvl ) / 2
          if ( list(rowv) .gt. target ) then
            rowvh = rowv
          else
            rowvl = rowv
          endif
          goto 100
      endif
c
c.... return
c
      sparseloc = rowvl
c
      return
      end

      subroutine genadj (colm,         rowp, icnt )
c     
      use pointer_data
c     
        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/workfc.h"
c   
C
C     Argument variables
C
      INTEGER             icnt
C
C     Local variables
C
      INTEGER             i,           ibig,        iblk,        iel
      INTEGER             imin,        j,           maxfill
      INTEGER             ncol,        nnonzero,    nnza
C  
      integer rowp(nshg*nnz),         colm(nshg+1)
      integer adjcnt(nshg),    row_fill_list(nshg,6*nnz), mloc(1)
c                                          change ^ if overflow
c                                   also change overflow check in asadj TWICE
      integer tmprdim(1)
      real*8, allocatable, dimension(:) :: tmpr

      adjcnt=0
      
      do iblk = 1, nelblk
c     
c.... set up the parameters
c     
         iel    = lcblk(1,iblk)
         lelCat = lcblk(2,iblk)
         lcsyst = lcblk(3,iblk)
         iorder = lcblk(4,iblk)
         nenl   = lcblk(5,iblk) ! no. of vertices per element
         nshl   = lcblk(10,iblk)
         npro   = lcblk(1,iblk+1) - iel 
         
c     
c.... compute sparse matrix data structures
c     
         call Asadj (row_fill_list,                       
     &               mien(iblk)%p,  adjcnt )
         
      enddo
      
      call sumgatInt ( adjcnt, nshg, nnonzero)
      if ( myrank .eq. master) then
         write (*,*) 'Number of global nonzeros ',nnonzero
      endif

c     
c     build the colm array
c     
      colm(1)=1
      do i=1,nshg
         colm(i+1)=colm(i)+adjcnt(i)
      enddo
c     
c     sort the rowp into increasing order
c     
      ibig=10*nshg
      icnt=0
      tmprdim=maxval(adjcnt)
      allocate (tmpr(tmprdim(1)))
      do i=1,nshg
         ncol=adjcnt(i)
         tmpr(1:ncol)=row_fill_list(i,1:ncol)
         do j=1,ncol
            icnt=icnt+1
            imin=minval(tmpr(1:ncol))
            mloc=minloc(tmpr(1:ncol))
            rowp(icnt)=imin
            tmpr(mloc(1))=ibig
         enddo
      enddo

c     NEED TO CLARIFY THIS !!!
      maxfill=tmprdim(1)
      write(*,*) 'maxfill=',maxfill
      nnza=icnt/nshg +1
      if(icnt.gt.nnz*nshg) then
         write(*,*) 'increase nnz in genmat to',nnza
         stop
      else
         write(*,*) 'nnz ok  nnz=',nnz,' actually needed',nnza   
         write(*,*) myrank,' is my rank and my nnz_tot is: ',icnt   
      endif
      return
      end

!> This routine generates the essential prescribed boundary conditions.
!!
!! input:<BR>
!! @param[in] iBC(nshg) Boundary condition code
!! @param[in] nBC(nshg) Boundary condition mapping array
!!
!! output:<BR>
!! @param[out] BC(nshg,ndofBC) The constraint data for prescribed BC 
!!
!! <B>Note</B>: genBC1 reduces the input data for the velocity. In the
!!       case of varying velocity direction in the generation, the 
!!       results may not be correct. (since a linearity assumption is 
!!       made in the generation).

      subroutine genBC (iBC,  BC,   x,   ilwork, iper)
c
      use readarrays            ! used to access BCinp, nBC
      use specialBC ! filling acs here

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/title.h"

C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8              bc,          x
C
C     Local variables
C
      INTEGER             i,           j,           n,           nn
      INTEGER             nsurf
C
      REAL*8              bctmp

C
      dimension iBC(nshg),                nsurf(nshg),
     &            BC(nshg,ndofBC),
     &            x(numnp,nsd),           ilwork(nlwork),
     &            iper(nshg)
c
c BCinp for each point has:
c   D T P c11 c12 c13 M1 c21 c22 c23 M2 theta S1 S2 S3...
c   1 2 3 4   5   6   7  8   9   10  11 12    13 14 15...
c Remember, ndof=nsd+2+nsclr
c
c Arrays in the following 1 line are now dimensioned in readnblk
c        dimension BCinp(numpbc,ndof+7)
c  
      dimension BCtmp(nshg,ndof+7)
c
c ndof+7= 3(thermos) + (nsd-1)*(nsd+1) + nscalars + 1 (theta)
c                       #vect *(vec dir +mag)
c
c.... --------------------------->  Input  <---------------------------
c
c.... convert boundary condition data
c
      BCtmp = zero
c
      if(numpbc.ne.0) then  
         do i = 1, ndof+7
            where (nBC(:) .ne. 0) BCtmp(:,i) = BCinp(nBC(:),i)
         enddo
         deallocate(BCinp)
      endif
            
c
      if(any(BCtmp(:,12).ne.0)) then
         iabc=1
         allocate (acs(nshg,2))
         where (btest(iBC,10))
            acs(:,1) = cos(BCtmp(:,12)) 
            acs(:,2) = sin(BCtmp(:,12)) 
         endwhere
      endif
           
c
c.... ------------------------>  Conversion  <-------------------------
c
c.... convert the input boundary conditions to condensed version
c
      BC = zero
c
      call genBC1 (BCtmp,  iBC,  BC)
c
c.... --------------------------->  Echo  <----------------------------
c
c.... echo the input data
c
      if (necho .lt. 3) then
         nn = 0
         do n = 1, nshg
            if (nBC(n) .ne. 0) then
               nn = nn + 1
               if(mod(nn,50).eq.1) 
     &              write(iecho,1000)ititle,(j,j=1,ndofBC)
               write (iecho,1100) n, (BC(n,i),i=1,ndofBC)
            endif
         enddo
      endif
c     
c.... return
c
      return
c
 1000 format(a80,//,
     &' P r e s c r i b e d   B o u n d a r y   C o n d i t i o n s',//,
     &  '    Node  ',/,
     &  '   Number ',5x,6('BC',i1,:,10x))
 1100 format(1p,2x,i5,3x,6(e12.5,1x))
c
      end

!> This subroutine adjusts the boundary conditions to accommodate for 
!! the velocity constraints in the non-axes directions. It copies the
!! reduced constraint parameters in BC.
!!
!! input:<BR>
!! @param[in] BCtmp(nshg,6+5*I3nsd) input BC parameters (density, temperature,
!!                                  pressure, (nsd-1)(nsd+1) velocity params,
!!                                  upto 4 scalar params)
!! @param[in] iBC(nshg) Boundary condition code
!!
!! output:
!! @param[out] BC(nshg,ndofBC) The constraint eq's parameters
!!

        subroutine genBC1 (BCtmp,  iBC,  BC)
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/sclrs.h"
        
C
C     Argument variables
C
      INTEGER             ibc
C
      REAL*8                bc,          bctmp
C
C     Local variables
C
      INTEGER             i
C
      REAL*8                tmp,         tmpbc
C
        dimension BCtmp(nshg,ndof+7),    iBC(nshg),
     &            BC(nshg,ndofBC),tmpbc(4)
c
        dimension tmp(nshg)
c
c.... scalars
c
        do isclr=1,nsclr
           where (btest(iBC,5+isclr)) BC(:,6+isclr) = BCtmp(:,12+isclr)
        enddo
c
c.... set up the thermodynamic properties
c
        where (btest(iBC,0)) BC(:,1) = BCtmp(:,1) ! density
        where (btest(iBC,1)) BC(:,2) = BCtmp(:,2) ! temperature
        where (btest(iBC,2)) BC(:,1) = BCtmp(:,3) ! pressure
c
c.... if the velocity in the x1-direction is specified
c
        where (ibits(iBC,3,3) .eq. 1)
          tmp     = BCtmp(:,4)**2 + BCtmp(:,5)**2 + BCtmp(:,6)**2
          BC(:,3) = tmp * BCtmp(:,7) / BCtmp(:,4)
          BC(:,4) =       BCtmp(:,5) / BCtmp(:,4)
          BC(:,5) =       BCtmp(:,6) / BCtmp(:,4)
        endwhere
c
c.... if the velocity in the x2-direction is specified
c
        where (ibits(iBC,3,3) .eq. 2)
          tmp     = BCtmp(:,4)**2 + BCtmp(:,5)**2 + BCtmp(:,6)**2
          BC(:,3) = tmp * BCtmp(:,7) / BCtmp(:,5)
          BC(:,4) =       BCtmp(:,4) / BCtmp(:,5)
          BC(:,5) =       BCtmp(:,6) / BCtmp(:,5)
        endwhere
c
c.... if the two velocities are specified (x1 & x2-direction)
c
c
c  Protect against user flipping the order of x1 and x2 in 
c  the vector 1 and vector 2.  Without this it will blow up.
c
        do i=1,nshg
          if(ibits(iBC(i),3,3) .eq. 3 .and. 
     &       (BCtmp(i,4).eq.0 .or. BCtmp(i,9).eq.0)) then !flip them
              tmpbc(1:4)=BCtmp(i,4:7)
              BCtmp(i,4:7)=BCtmp(i,8:11)
              BCtmp(i,8:11)=tmpbc(1:4)
          endif
        enddo
        where (ibits(iBC,3,3) .eq. 3)
          tmp         = sqrt (BCtmp(:, 4)**2 + BCtmp(:, 5)**2
     &                                       + BCtmp(:, 6)**2)
          BCtmp(:, 4) = BCtmp(:, 4) / tmp
          BCtmp(:, 5) = BCtmp(:, 5) / tmp
          BCtmp(:, 6) = BCtmp(:, 6) / tmp
          BCtmp(:, 7) = BCtmp(:, 7) * tmp
c
          tmp         = sqrt (BCtmp(:, 8)**2 + BCtmp(:, 9)**2
     &                                       + BCtmp(:,10)**2)
          BCtmp(:, 8) = BCtmp(:, 8) / tmp
          BCtmp(:, 9) = BCtmp(:, 9) / tmp
          BCtmp(:,10) = BCtmp(:,10) / tmp
          BCtmp(:,11) = BCtmp(:,11) * tmp
c
          BCtmp(:, 4) = BCtmp(:, 9) * BCtmp(:, 4)
     &                - BCtmp(:, 5) * BCtmp(:, 8)
          BCtmp(:, 6) = BCtmp(:, 9) * BCtmp(:, 6)
     &                - BCtmp(:, 5) * BCtmp(:,10)
          BCtmp(:, 7) = BCtmp(:, 9) * BCtmp(:, 7)
     &                - BCtmp(:, 5) * BCtmp(:,11)
          BC(:,3)     = BCtmp(:, 7) / BCtmp(:, 4)
          BC(:,4)     = BCtmp(:, 6) / BCtmp(:, 4)
c
          BCtmp(:, 9) = BCtmp(:, 4) * BCtmp(:, 9) 
          BCtmp(:,10) = BCtmp(:, 4) * BCtmp(:,10)
     &                - BCtmp(:, 8) * BCtmp(:, 6)
          BCtmp(:,11) = BCtmp(:, 4) * BCtmp(:,11)
     &                - BCtmp(:, 8) * BCtmp(:, 7)
          BC(:,5)     = BCtmp(:,11) / BCtmp(:, 9)
          BC(:,6)     = BCtmp(:,10) / BCtmp(:, 9)
        endwhere
c
c.... if the velocity in the x3-direction is specified
c
        if (nsd .eq. 3) then
        where (ibits(iBC,3,3) .eq. 4)
          tmp     = BCtmp(:,4)**2 + BCtmp(:,5)**2 + BCtmp(:,6)**2
          BC(:,3) = tmp * BCtmp(:,7) / BCtmp(:,6)
          BC(:,4) =       BCtmp(:,4) / BCtmp(:,6)
          BC(:,5) =       BCtmp(:,5) / BCtmp(:,6)
        endwhere
        endif
c
c.... if two velocities are specified (x1 & x3-direction)
c
        if (nsd .eq. 3) then
c
c  Protect against user flipping the order of x1 and x3 in 
c  the vector 1 and vector 2.  Without this it will blow up.
c
        do i=1,nshg
          if(ibits(iBC(i),3,3) .eq. 5 .and.
     &       (BCtmp(i,4).eq.0 .or. BCtmp(i,10).eq.0)) then !flip them
              tmpbc(1:4)=BCtmp(i,4:7)
              BCtmp(i,4:7)=BCtmp(i,8:11)
              BCtmp(i,8:11)=tmpbc(1:4)
           endif
        enddo
        where (ibits(iBC,3,3) .eq. 5)
          tmp         = sqrt (BCtmp(:, 4)**2 + BCtmp(:, 5)**2
     &                                       + BCtmp(:, 6)**2)
          BCtmp(:, 4) = BCtmp(:, 4) / tmp
          BCtmp(:, 5) = BCtmp(:, 5) / tmp
          BCtmp(:, 6) = BCtmp(:, 6) / tmp
          BCtmp(:, 7) = BCtmp(:, 7) * tmp
c
          tmp         = sqrt (BCtmp(:, 8)**2 + BCtmp(:, 9)**2
     &                                       + BCtmp(:,10)**2)
          BCtmp(:, 8) = BCtmp(:, 8) / tmp
          BCtmp(:, 9) = BCtmp(:, 9) / tmp
          BCtmp(:,10) = BCtmp(:,10) / tmp
          BCtmp(:,11) = BCtmp(:,11) * tmp
c
          BCtmp(:, 4) = BCtmp(:,10) * BCtmp(:, 4)
     &                - BCtmp(:, 6) * BCtmp(:, 8)
          BCtmp(:, 5) = BCtmp(:,10) * BCtmp(:, 5)
     &                - BCtmp(:, 6) * BCtmp(:, 9)
          BCtmp(:, 7) = BCtmp(:,10) * BCtmp(:, 7)
     &                - BCtmp(:, 6) * BCtmp(:,11)
          BC(:,3)     = BCtmp(:, 7) / BCtmp(:, 4)
          BC(:,4)     = BCtmp(:, 5) / BCtmp(:, 4)
c
          BCtmp(:, 9) = BCtmp(:, 4) * BCtmp(:, 9)
     &                - BCtmp(:, 8) * BCtmp(:, 5)
          BCtmp(:,10) = BCtmp(:, 4) * BCtmp(:,10)
          BCtmp(:,11) = BCtmp(:, 4) * BCtmp(:,11)
     &                - BCtmp(:, 8) * BCtmp(:, 7)
          BC(:,5)     = BCtmp(:,11) / BCtmp(:,10)
          BC(:,6)     = BCtmp(:, 9) / BCtmp(:,10)
        endwhere
        endif
c
c.... if two velocities are specified (x2 & x3-direction)
c
        if (nsd .eq. 3) then
c
c  Protect against user flipping the order of x2 and x3 in 
c  the vector 1 and vector 2.  Without this it will blow up.
c
        do i=1,nshg
          if(ibits(iBC(i),3,3) .eq. 6 .and. (
     &       BCtmp(i,5).eq.0 .or. BCtmp(i,10).eq.0)) then !flip them
              tmpbc(1:4)=BCtmp(i,4:7)
              BCtmp(i,4:7)=BCtmp(i,8:11)
              BCtmp(i,8:11)=tmpbc(1:4)
           endif
        enddo
        where (ibits(iBC,3,3) .eq. 6)
          tmp         = sqrt (BCtmp(:, 4)**2 + BCtmp(:, 5)**2
     &                                       + BCtmp(:, 6)**2)
          BCtmp(:, 4) = BCtmp(:, 4) / tmp
          BCtmp(:, 5) = BCtmp(:, 5) / tmp
          BCtmp(:, 6) = BCtmp(:, 6) / tmp
          BCtmp(:, 7) = BCtmp(:, 7) * tmp
c
          tmp         = sqrt (BCtmp(:, 8)**2 + BCtmp(:, 9)**2
     &                                       + BCtmp(:,10)**2)
          BCtmp(:, 8) = BCtmp(:, 8) / tmp
          BCtmp(:, 9) = BCtmp(:, 9) / tmp
          BCtmp(:,10) = BCtmp(:,10) / tmp
          BCtmp(:,11) = BCtmp(:,11) * tmp
c
          BCtmp(:, 4) = BCtmp(:,10) * BCtmp(:, 4)
     &                - BCtmp(:, 6) * BCtmp(:, 8)
          BCtmp(:, 5) = BCtmp(:,10) * BCtmp(:, 5)
     &                - BCtmp(:, 6) * BCtmp(:, 9)
          BCtmp(:, 7) = BCtmp(:,10) * BCtmp(:, 7)
     &                - BCtmp(:, 6) * BCtmp(:,11)
          BC(:,3)     = BCtmp(:, 7) / BCtmp(:, 5)
          BC(:,4)     = BCtmp(:, 4) / BCtmp(:, 5)
c
          BCtmp(:, 8) = BCtmp(:, 5) * BCtmp(:, 8)
     &                - BCtmp(:, 9) * BCtmp(:, 4) 
          BCtmp(:,10) = BCtmp(:, 5) * BCtmp(:,10)
          BCtmp(:,11) = BCtmp(:, 5) * BCtmp(:,11)
     &                - BCtmp(:, 9) * BCtmp(:, 7)
          BC(:,5)     = BCtmp(:,11) / BCtmp(:,10)
          BC(:,6)     = BCtmp(:, 8) / BCtmp(:,10)
        endwhere
        endif
c
c.... if all velocities are specified
c
        if (nsd .eq. 3) then
        where (ibits(iBC,3,3) .eq. 7)
          BC(:,3) = BCtmp(:,7) * BCtmp(:,4)
          BC(:,4) = BCtmp(:,7) * BCtmp(:,5)
          BC(:,5) = BCtmp(:,7) * BCtmp(:,6)
        endwhere
        endif
c
c.... end
c
        return
        end

!> This routine reads the interior elements and generates the
!! appropriate blocks.

        subroutine genblk (IBKSZ)
c
        use pointer_data
c
        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/outpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
C
C     Argument variables
C
      INTEGER             ibksz
C
C     Local variables
C
      INTEGER             iblk,        iel,         iientpsiz,   ijunk
      INTEGER             ipordl,      iseven,      itpblk,      ierr
      INTEGER             n,           n1
      INTEGER             n2,          neltp

C
        integer, allocatable :: ientp(:,:)
        integer mater(ibksz)
        integer intfromfile(50) ! integers read from headers
        character*255 fname1
c
        iel=1
        itpblk=nelblk
        nelblk=0
        mattyp = 0
        ndofl = ndof
        nsymdl = nsymdf
        do iblk = 1, itpblk
c
c           read(igeom) neltp,nenl,ipordl,nshl, ijunk, ijunk, lcsyst
           iseven=7
c           call creadlist(igeom,iseven,
c     &          neltp,nenl,ipordl,nshl, ijunk, ijunk, lcsyst)
           iseven=7
           fname1='connectivity interior?'
           call readheader(igeom,fname1,intfromfile,iseven,
     &                     'integer'//CHAR(0), iotype)
           neltp  = intfromfile(1)
           nenl   = intfromfile(2)
           ipordl = intfromfile(3)
           nshl   = intfromfile(4)
           ijunk  = intfromfile(5)
           ijunk  = intfromfile(6)
           lcsyst = intfromfile(7)
           allocate (ientp(neltp,nshl))
c           read(igeom) ientp
           iientpsiz=neltp*nshl
           call readdatablock(igeom,fname1,ientp,iientpsiz,
     &                     'integer'//CHAR(0), iotype)

           do n=1,neltp,ibksz 
              nelblk=nelblk+1
              npro= min(IBKSZ, neltp - n + 1)
c
              lcblk(1,nelblk)  = iel
c              lcblk(2,nelblk)  = iopen ! available for later use
              lcblk(3,nelblk)  = lcsyst
              lcblk(4,nelblk)  = ipordl
              lcblk(5,nelblk)  = nenl
              lcblk(6,nelblk)  = nfacel
              lcblk(7,nelblk)  = mattyp
              lcblk(8,nelblk)  = ndofl
              lcblk(9,nelblk)  = nsymdl 
              lcblk(10,nelblk) = nshl ! # of shape functions per elt

c             ELEMENT BLOCK ALLOCATION EXCEEDED
              IF(nelblk>MAXBLK) THEN
                PRINT *,''
                PRINT *,'Fatal: Total Element Block Allocation '//
     &           'Exceeded.'
                PRINT *,'Use more processors for this problem or '//
     &           'modify MAXBLK in the code.'
                PRINT *,''
                CALL MPI_FINALIZE(ierr)
                STOP
              ENDIF

c
c.... allocate memory for stack arrays
c
              allocate (mmat(nelblk)%p(npro))
c
              allocate (mien(nelblk)%p(npro,nshl))
              allocate (mxmudmi(nelblk)%p(npro,maxsh))
c
c.... save the element block
c
              n1 = n
              n2 = n+npro-1
              mater = 1   ! all one material for now
              call gensav (ientp(n1:n2,1:nshl),
     &                     mater,mien(nelblk)%p,
     &                     mmat(nelblk)%p)
              iel=iel+npro
c
           enddo
           deallocate(ientp)
        enddo
        lcblk(1,nelblk+1) = iel
c
c.... return
c
CAD        call timer ('Back    ')
c
        return
c
1000    format(a80,//,
     &  ' N o d a l   C o n n e c t i v i t y',//,
     &  '   Elem  ',/,
     &  '  Number  ',7x,27('Node',i2,:,2x))
1100    format(2x,i5,6x,27i8)
        end

!> This routine inputs the geometry and the boundary conditions.

        subroutine gendat (y,       ac,       x, lgmapping,  iBC,   BC,
     &                     iper,    ilwork,
     &                     shp,     shgl,    shpb,    shglb
     &                    ) 
      
        use readarrays          ! used to acess nBC
        use dtnmod
        use pointer_data

        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/mio.h"
        include "common_blocks/newdim.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/title.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             ibc,      ilwork,      iper, lgmapping
C
      REAL*8                ac,          bc,          shgl,        shglb
      REAL*8                shp,         shpb,        x
      REAL*8                y
C
C     Local variables
C
      INTEGER             i,           iblk,        iel,         ignd
      INTEGER             j,           n,        nodflx

c
c arrays in the following line are now dimensioned in readnblk
c        dimension nBC(nshg)
c
        dimension y(nshg,ndof),      ac(nshg,ndof),
     &            x(numnp,nsd),      iBC(nshg),
     &            BC(nshg,ndofBC),
     &            nodflx(numflx),    ilwork(nlwork),
     &            iper(nshg),    lgmapping(numnp)
c
c.... shape function declarations
c     
        dimension shp(MAXTOP,maxsh,MAXQPT),  
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
c.... ---------------------------->  Nodes  <--------------------------
c
c.... compute length scales
c
        call xyzbound(x)
c
c.... echo the coordinates
c
        if ((necho .lt. 2).and.(myrank.eq.master)) then
          do n = 1, numnp
            if (mod(n,50) .eq. 1) write (iecho,1000) ititle,(i,i=1,nsd)
            write (iecho,1100) n, (x(n,i),i=1,nsd)
          enddo
        endif
c
c.... prepare periodic boundary conditions
c
        do i = 1,nshg
          if (iper(i) .ne. 0) then
            nshg0 = nshg0 - 1
          else
            iper(i) = i
          endif
        enddo
c
c.... ---------------------->  Interior Elements  <--------------------
c
        ibound = 0
c
c.... generate the interior nodal mapping
c
        call genshp ( shp, shgl, nshape, nelblk)
c
c.... --------------------->  Boundary Conditions  <-------------------
c
c.... read and generate the boundary condition codes (iBC array)
c
        call geniBC (iBC)
c
c.... read and generate the essential boundary conditions (BC array)
c
        call genBC  (iBC,   BC,   point2x,
     &               point2ilwork, point2iper)
        deallocate(nBC)
c
c.... ---------------------->  Boundary Elements  <--------------------
c
        ibound = 1
        call gtnods
c
c  We now take care of Direchlet to Neumann BC's.  It had to move here
c  so that the IBC array was of size nshg and ready to be marked.
c

        if(nsclr.gt.0) then 
           call initDtN         ! Dirichlet to Neumann module: 
                                     ! initialize this once only
           do iblk = 1, nelblb  ! number of blocks
              iel    = lcblkb(1,iblk)
              npro   = lcblkb(1,iblk+1) - iel
c
c  for the DtN BC we need to mark all of the nodes that are involved.
c
              do i=1,npro
c
c if this element has the BCB AND it has not been found yet then mark it
c
                 if(miBCB(iblk)%p(i,2).lt.0) then  
                    idtn = 1    !set the flag for dtn bc's
                    do j=1,nshapeb
                       do isclr=1,nsclr
                          ignd=mienb(iblk)%p(i,j)
                             ifeature(ignd) = abs(miBCB(iblk)%p(i,2))       
                             iBC(ignd)=ior(iBC(ignd),2**13)
                                ! must mark this as a Neumann BC now
                             miBCB(iblk)%p(i,1)=
     &                       ior(miBCB(iblk)%p(i,1),2**(4+isclr))
                       end do
                    end do
                 endif
              end do
           end do
        endif
c
c.... generate the boundary element shape functions
c
        call genshpb ( shpb, shglb, nshapeb, nelblb)
c
c.... --------------------->  Initial Conditions  <--------------------
c
c.... generate the initial conditions and initialize time varying BC
c
        call genini (iBC,      BC,         y, 
     &               ac,       iper, 
     &               ilwork,  
     &               x,  lgmapping,
     &               shp,     shgl,    shpb,    shglb) 
c
c.... close the geometry, boundary condition and material files
c
        close (igeom)
        close (ibndc)
        if (mexist) close (imat)
c
c.... return
c
        return
c
c.... end of file error handling
c
999     call error ('gendat  ','end file',igeom)
c
1000    format(a80,//,
     &  ' N o d a l   C o o r d i n a t e s                  ',//,
     &  '    Node     ',12x,3('x',i1,:,17x))
1100    format(1p,2x,i5,13x,3(1e12.5,7x))
2000    format(a80,//,
     &  ' B o u n d a r y   F l u x   N o d e s              '//,
     &  '   index          Node          ')
2100    format(1x,i5,5x,i10)
c
        end

!> Compute the bounding box for the 
!! computational domain

        subroutine xyzbound(x)

        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      REAL*8                x
C
C     Local variables
C
      INTEGER             ierr

C
      REAL*8                xlngth,      xmax,        ylngth,      ymax
      REAL*8                zlngth,      zmax
C
        dimension x(numnp,3)

        real*8   Forout(3), Forin(3)

        xlngth=maxval(x(:,1))
        ylngth=maxval(x(:,2))
        zlngth=maxval(x(:,3))
        if(numpe. gt. 1) then
           Forin=(/xlngth,ylngth,zlngth/)
           call MPI_ALLREDUCE (Forin, Forout, 3,
     &       MPI_DOUBLE_PRECISION,MPI_MAX, MPI_COMM_WORLD,ierr)
           xmax = Forout(1)
           ymax = Forout(2)
           zmax = Forout(3)
        else
           xmax = xlngth
           ymax = ylngth
           zmax = zlngth
        endif
        xlngth=minval(x(:,1))
        ylngth=minval(x(:,2))
        zlngth=minval(x(:,3))
        if(numpe .gt. 1) then
           Forin=(/xlngth,ylngth,zlngth/)
           call MPI_ALLREDUCE (Forin, Forout, 3,
     &       MPI_DOUBLE_PRECISION,MPI_MIN, MPI_COMM_WORLD,ierr)
        else
           Forout(1) = xlngth
           Forout(2) = ylngth
           Forout(3) = zlngth
        endif

        xlngth = xmax-Forout(1)
        ylngth = ymax-Forout(2)
        zlngth = zmax-Forout(3)

        if(myrank.eq.master) then
           print 108,  xlngth,ylngth,zlngth
        endif
 108    format(' Domain size (x,y,z):',2x,3f15.10)
        return
        end

!> This routine reads the boundary condition codes.
!!
!! output:<BR>
!! @params[out] iBC(nshg) Boundary Condition code
!!
!!         = 1 * iBC_1 + 2 * iBC_2 + 4 * iBC_3
!!              density   temperature   pressure
!!
!!    if nsd = 3:
!!
!!        +  8 * iBC_4 +  16 * iBC_5 +  32 * iBC_6
!!           x1-velocity   x2-velocity   x3-velocity
!!
!!        + 64 * iBC_7 + 128 * iBC_8 + 256 * iBC_9 + 512 * iBC_10
!!          sclr1         sclr2        sclr3         sclr4
!!
!!        + 1024 * iBC_11  + 2048* iBC_12 
!!          perioidicity     spebc          
!!
!!  @params[out] nBC(nshg) Boundary Condition mapping array

        subroutine geniBC (iBC)
c
        use readarrays          ! used to access iBCtmp

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/title.h"
C
C     Argument variables
C
      INTEGER             ibc
C
C     Local variables
C
      INTEGER             i,           itemp,       j,           n
      INTEGER             nb,          nn
c
c Arrays in the following 1 line are now dimensioned in readnblk
c        dimension iBCtmp(numpbc)
c
        dimension iBC(nshg)
        dimension itemp(6)
c
c.... set the iBC array
c
        iBC = 0
c
        if(numpbc.eq.0) return  ! sometimes there are no BC's on a partition
        where (nBC(:) .ne. 0) iBC(:) = iBCtmp(nBC(:))
c
c.... echo the input iBC array only if other than zero
c
        if (necho .lt. 3) then
          nn = 0
          do n = 1, nshg
            if (nBC(n) .ne. 0) then
              nb = nBC(n)
              nn = nn + 1
              if (mod(nn,50).eq.1) write(iecho,1000)ititle,(j,j=1,ndof)
              itemp(   1) = mod(iBCtmp(nb)   ,2) - mod(iBCtmp(nb)/ 4,2)
              itemp(   2) = mod(iBCtmp(nb)/ 8,2)
              itemp(   3) = mod(iBCtmp(nb)/16,2)
              itemp(   4) = mod(iBCtmp(nb)/32,2)
              itemp(ndof) = mod(iBCtmp(nb)/ 2,2)
              write(iecho,1100) n,(itemp(i),i=1,ndof)
            endif
          enddo
        endif
        deallocate(iBCtmp)
c
c.... return
c
        return
c
c.... end of file error handling
c
999     call error ('geniBC  ','end file',ibndc)
c
1000    format(a80,//,
     &  ' N o d a l   B o u n d a r y   C o n d i t i o n   C o d e',//,
     &  '    Node   ',13x,6('dof',i1,:,6x))
1100    format(2x,i5,10x,5i10)
c
        end

!> This subroutine inputs the integration information.

      subroutine genint
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/shpdat.h"
C
C     Local variables
C
      INTEGER             nerr
C
      real*8, allocatable :: tmpQpt (:,:), tmpQwt (:) 
      real*8, allocatable :: tmpQptb(:,:), tmpQwtb(:) 

c
c.... compute the shape function parameters
c
        
c
c.... linear tets only
c

      if (nen.ne.4) then
        call error ('genint  ','tetsonly', nen)
      endif

c 
c.... get quadrature data for interior and boundary elements
c
c
c  Tets
c

c 2014-01-01:  does the original genint.f contain a bug?
c it seems like this routine would do the wrong thing for
c all cases where nen.ne.4
c due to misplaced endif -NMW
             
c      if (nen.eq.4) then 
         nshape  = (ipord+1)*(ipord+2)*(ipord+3)/6
         nshapeb = (ipord+1)*(ipord+2)/2
c      endif

      select case (intg(1,1))
      case (1)
         nint(1) = 1
      case (2)
         nint(1) = 4
      case (3)
         nint(1) = 16
      case (4)
         nint(1) = 29
      end select
      select case (intg(2,1))
      case (1)
         nintb(1) = 1
      case (2)
         nintb(1) = 3
      case (3)
         nintb(1) = 6
      case (4)
         nintb(1) = 12
      end select
      
      allocate (tmpQpt (4,nint(1)))           
      allocate (tmpQwt (nint(1)))           
      allocate (tmpQptb(4,nintb(1)))           
      allocate (tmpQwtb(nintb(1)))           
      
      call symtet(nint(1),tmpQpt,tmpQwt,nerr) ! interior elements
      Qpt(1,1:4,1:nint(1)) = tmpQpt(1:4,1:nint(1))
      Qwt(1,1:nint(1))     = tmpQwt(1:nint(1))
      
      
      call symtri(nintb(1),tmpQptb,tmpQwtb,nerr) ! boundary elements
      Qptb(1,1:4,1:nintb(1)) = tmpQptb(1:4,1:nintb(1))
      Qwtb(1,1:nintb(1))     = tmpQwtb(1:nintb(1))
      
      deallocate (tmpQpt)
      deallocate (tmpQwt)
      deallocate (tmpQptb)
      deallocate (tmpQwtb)
        
c
c.... adjust quadrature weights to be consistent with the
c     design of tau. 
c
      Qwt(1,:) = (four/three)*Qwt(1,:)
      Qwtb(1,:) = two*Qwtb(1,:)
        
      return
      end

        subroutine genlmass (x, shp,shgl)
c
        use pointer_data
c
        include "global.h"
        include "mpif.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Local variables
C
      INTEGER             iblk,        iel,         inum
C
        real*8 x(numnp,nsd)
c
        real*8 shp(MAXTOP,maxsh,MAXQPT),   
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT) 
c
        real*8, allocatable :: tmpshp(:,:), tmpshgl(:,:,:)
        
c
c gmass came in via pointer_data and will 
c be available wherever it is included  (allocate it now).
c

        allocate (gmass(nshg))  
        gmass=zero
c
c.... loop over the element-blocks
c
        do iblk = 1, nelblk
          iel    = lcblk(1,iblk)
          lelCat = lcblk(2,iblk)
          lcsyst = lcblk(3,iblk)
          iorder = lcblk(4,iblk)
          nenl   = lcblk(5,iblk) ! no. of vertices per element
          nshl   = lcblk(10,iblk)
          mattyp = lcblk(7,iblk)
          npro   = lcblk(1,iblk+1) - iel 
          inum   = iel + npro - 1
          ngauss = nint(lcsyst)
c
c
c.... compute and assemble the residual and tangent matrix
c
          allocate (tmpshp(nshl,MAXQPT))
          allocate (tmpshgl(nsd,nshl,MAXQPT))
          tmpshp(1:nshl,:) = shp(lcsyst,1:nshl,:)
          tmpshgl(:,1:nshl,:) = shgl(lcsyst,:,1:nshl,:)
         

          call AsImass (x,       tmpshp,     
     &                  tmpshgl, mien(iblk)%p,
     &                  gmass)

          deallocate ( tmpshp )
          deallocate ( tmpshgl )
c
c.... end of interior element loop
c
       enddo

      return
      end

!> This routine computes and assembles the mass corresponding to the
!! each node.

        subroutine AsImass (x,      shp,
     &                     shgl,    ien,     
     &                     gmass)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c
        real*8 x(numnp,nsd),              
     &         shp(nshl,maxsh),       shgl(nsd,nshl,ngauss),
     &         gmass(nshg)

        integer ien(npro,nshl)

c
        real*8    xl(npro,nenl,nsd),    WdetJ(npro), 
     &            sgn(npro,nshl),       shape(npro,nshl),          
     &            locmass(npro,nshl),   shg(npro,nshl,nsd),
     &            fmstot(npro),         temp(npro),
     &            dxidx(npro,nsd,nsd),  shdrv(npro,nsd,nshl)

        integer aa
c        
c
c
c.... gather the variables
c
c
c.... get the matrix of mode signs for the hierarchic basis functions. 
c
        if (ipord .gt. 1) then
           call getsgn(ien,sgn)
        endif
        
        call localx(x,      xl,     ien,    nsd,    'gather  ')
c
c.... zero the matrices if they are being recalculated
c

        locmass=zero
        fmstot=zero

        do intp = 1, ngauss

           if (Qwt(lcsyst,intp) .eq. zero) cycle ! precaution
c
c.... get the hierarchic shape functions at this int point
c
           call getshp(shp,          shgl,      sgn, 
     &                 shape,        shdrv)

c
c.... --------------------->  Element Metrics  <-----------------------
c
           call e3metric( xl,         shdrv,       dxidx,  
     &                    shg,        WdetJ)

c
c  get this quad points contribution to the integral of the square of  the 
c  shape function
c
           do aa = 1,nshl
              locmass(:,aa)= locmass(:,aa) 
     &             + shape(:,aa)*shape(:,aa)*WdetJ
           enddo
c
c also accumulate this quad points contribution to the integral of the element
c volume (integral Na^2 d Omega)
c 
           fmstot= fmstot + WdetJ ! intregral  d Omega
c
c.... end of integration loop
c
        enddo
c
c.... lumped mass if needed   Note that the locmass factors accumulated
c     over integration points and weighted with WdetJ already.
c

c.... scale the LHS matrix contribution with special lumping weighting
c
c  The first term we collect is the trace of integral Na^2 d Omega
c
        temp = zero
        do aa = 1, nshl
           temp = temp + locmass(:,aa) !reusing temp to save memory
        enddo

c
c scale the diagonal so that the trace will still yield Omega^e (the volume
c of the element)
c
        do aa = 1, nshl
           locmass(:,aa) = locmass(:,aa) * fmstot / temp
        enddo
c
c.... assemble the residual
c
        call local (gmass,    locmass,     ien,    1,  'scatter ')

c
c.... end
c
        return
        end

      subroutine lmassadd ( ac,       res,
     &                      rowp,     colm,    
     &                      lhsK,     gmass)
c     
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/timdat.h"
c     
      real*8 ac(nshg,ndof), res(nshg,4), tmp,tmp1
      real*8 lhsK(9,nnz_tot), gmass(nshg), rho(nshg)
      integer rowp(nnz*nshg),  colm(nshg+1)
      integer n, k
c
      integer sparseloc
c
c
      rho=datmat(1,1,1)  ! needs to be generalized for VOF or level set
      tmp1=flmpl*almi
      if((flmpl.ne.0).and.(lhs.eq.1)) then
c
c.... Add lmass to diag of lhsK
c
         do n = 1, nshg
          k = sparseloc( rowp(colm(n)), colm(n+1)-colm(n), n )
     &       + colm(n)-1
            tmp=gmass(n)*tmp1*rho(n)
          lhsK(1,k) = lhsK(1,k) + tmp
          lhsK(5,k) = lhsK(5,k) + tmp
          lhsK(9,k) = lhsK(9,k) + tmp
         enddo
      endif

      tmp1=flmpr

      if(flmpr.ne.0) then
         rho=rho*gmass*tmp1  ! reuse rho
         res(:,1)=res(:,1)-ac(:,1)*rho(:)
         res(:,2)=res(:,2)-ac(:,2)*rho(:)
         res(:,3)=res(:,3)-ac(:,3)*rho(:)
      endif
     
      return
      end

      subroutine lmassaddSclr ( ac,       res,
     &                          rowp,     colm,    
     &                          lhsS,     gmass)
c     
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/timdat.h"
c     
      real*8 ac(nshg),       res(nshg), tmp, tmp1
      real*8 lhsS(nnz_tot), gmass(nshg), rho(nshg)
      integer rowp(nnz*nshg),  colm(nshg+1)
      integer n, k
c
      integer sparseloc
c
c
      rho=datmat(1,1,1)  ! needs to be generalized for VOF or level set
      tmp1=flmpl*almi
      if((flmpl.ne.0).and.(lhs.eq.1)) then
c
c.... Add lmass to diag of lhsK
c
         do n = 1, nshg
          k = sparseloc( rowp(colm(n)), colm(n+1)-colm(n), n )
     &       + colm(n)-1
            tmp=gmass(n)*tmp1*rho(n)
          lhsS(k) = lhsS(k) + tmp
         enddo
      endif

      tmp1=flmpr
      if(flmpr.ne.0) then
         rho=rho*gmass*tmp1  ! reuse rho
         res(:)=res(:)-ac(:)*rho(:)
      endif
     
      return
      end

!> This routine saves the element block data.
!!
!! input:<BR>
!! @param[in] ientmp(npro,nshl) Nodal connectivity 
!! @param[in] mattmp(npro) Material type flag
!!
!! output:<BR>
!! @param[out] ien(npro,nshl) Nodal connectivity
!! @param[out] mater(npro) Material type flag
!!

        subroutine gensav (ientmp, mattmp, ien,    mater)
c
        include "global.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         ientmp,      mater,       mattmp
C
C     Local variables
C
      INTEGER             i
C
        dimension   ientmp(npro,nshl),
     &              mattmp(npro),           ien(npro,nshl),
     &              mater(npro)
c
c.... save the element data
c
        do i = 1, nshl
          ien(:,i) = ientmp(:,i)
        enddo
c
        mater = mattmp
c
c.... end
c
        return
        end

!> This subroutine generates shape functions for tetra elements

        subroutine genshp (shp,    shgl, nshp, nblk)  
c
        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             nblk,        nshp
C
      REAL*8                shgl,        shp
C
C     Local variables
C
      INTEGER             i,           iblk
C
        dimension shp(MAXTOP,maxsh,MAXQPT), 
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT)
c
c.... loop through element blocks
c
        maxnint=1
          do iblk = 1, nblk
c
c.... get coord. system and element type 
c
            lcsyst = lcblk(3,iblk)
            nshl   = lcblk(10,iblk)
c
c.... generate the shape-functions in local coordinates
c
            select case ( lcsyst )
            case ( 1 )          ! tets
               maxnint=max(maxnint,nint(lcsyst))
            do i=1,nint(lcsyst)  
               call shpTet(ipord,Qpt(1,1:3,i),shp(1,:,i),shgl(1,:,:,i))
            enddo
            shgl(1,:,1:nshl,1:nint(lcsyst)) = 
     &      shgl(1,:,1:nshl,1:nint(lcsyst))/two
c     
c            case ( 2 )          ! hexes
c     

c
c            case ( 3 )          ! wedges
c

c
c            case ( 5 )          ! pyramids
c            

c
c.... nonexistent element
c
            case default
c
            call error ('genshp  ', 'elem Cat', lelCat)
c
            end select
c
c.... end of generation
c
          enddo
c
c.... return
c
        return
        end

!> This subroutine generates shape functions for tetra elements

      subroutine genshpb (shpb,    shglb, nshpb, nblk)  
        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             nblk,        nshpb
C
      REAL*8                shglb,       shpb
C
C     Local variables
C
      INTEGER             i,           iblk
c
      dimension shpb(MAXTOP,maxsh,MAXQPT), 
     &          shglb(MAXTOP,nsd,maxsh,MAXQPT)
c
c.... loop through element blocks
c
      do iblk = 1, nblk
c
c.... get coord. system and element type 
c

         lcsyst = lcblkb(3,iblk)

c.... generate the shape-functions in local coordinates
c
         select case ( lcsyst )
         case ( 1 )             ! tets
            nshl=lcblkb(9,iblk)
            do i=1,nintb(lcsyst)  
               call shpTet(ipord,Qptb(1,1:3,i),shpb(1,:,i),
     &              shglb(1,:,:,i))
            enddo
            shglb(1,:,1:nshl,1:nintb(lcsyst)) = 
     &           shglb(1,:,1:nshl,1:nintb(lcsyst))/two
c     
c         case ( 2 )             ! hexes
c     

c     
c         case ( 3 )             ! wedges with tri bd face
c            

c     
c         case ( 4 )             ! wedges with quad bd face
c     

c
c         case ( 5 )             ! pyramids with quad bd face
c     

c
c         case ( 6 )             ! pyramids with quad bd face
c     

c
c.... nonexistent element
c
         case default
c
            call error ('genshp  ', 'elem Cat', lelCat)
c
         end select
c
c.... end of generation
c
      enddo
c
c.... return
c
      return
      end

!> This routine saves the boundary element block.
!!
!! input:<BR>
!! @param[in] ientmp(npro,nshl) Boundary nodal connectivity
!! @param[in] iBCtmp(npro,ndiBCB) Boundary condition codes
!! @param[in] BCBtmp(npro,nshlb,ndBCB) Boundary condition values
!! @param[in] mattmp(npro) Material type flag
!!
!! output:<BR>
!! @param[out] ienb(npro,nshl) Boundary nodal connectivity
!! @param[out] iBCB(npro,ndiBCB) Boundary condition codes
!! @param[out] BCB(npro,nshlb,ndBCB) Boundary condition values
!! @param[out] materb(npro) Material type flag
!!

        subroutine gensvb (ientmp, iBCBtmp, BCBtmp, mattmp,
     &                     ienb,   iBCB,    BCB,    materb)
c
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ibcb,        ibcbtmp,     ienb,        ientmp
      INTEGER             materb,      mattmp
C
      REAL*8                bcb,         bcbtmp
C
C     Local variables
C
      INTEGER             i,           j
C
        dimension   ientmp(npro,nshl),
     &              iBCBtmp(npro,ndiBCB),    BCBtmp(npro,ndBCB)

        dimension   mattmp(npro),           ienb(npro,nshl),
     &              iBCB(npro,ndiBCB),      BCB(npro,nshlb,ndBCB),
     &              materb(npro)
c
c.... generate the boundary element mapping
c
        do i = 1, nshl
          ienb(:,i) = ientmp(:,i)
        enddo
c
c.... save the boundary element data
c
        iBCB   = iBCBtmp
        do i = 1, nenbl ! This is NOT NSHLB as we are just copying the
                        ! piecewise constant data given by NSpre and
                        ! higher order coefficients must be zero
           do j = 1, ndBCB
              BCB(:,i,j)   = BCBtmp(:,j)
           end do
        end do
        do i = nenbl+1, nshlb
           do j = 1, ndBCB
              BCB(:,i,j)   = zero
           end do
        end do

        materb = mattmp
c
c.... return
c
        return
        end

!> Compute and add the contribution of the turbulent
!! eddy viscosity to the molecular viscosity.

      subroutine getDiff( dwl,yl, shape, xmudmi, xl, rmu,  rho)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
 
C
C     Local variables
C
      INTEGER             i,           isc
C
      REAL*8                sigmainv
C
      real*8  yl(npro,nshl,ndof), rmu(npro), xmudmi(npro,ngauss),
     &        shape(npro,nshl),   rho(npro),
     &        dwl(npro,nshl),     sclr(npro),
     &        xl(npro,nenl,nsd)
      integer n, e

      real*8  kay, epsilon
     &        h_param, prop_blend(npro),test_it(npro)
c
c    
c.... get the material properties (2 fluid models will need to determine
c     the "interpolated in phase space" properties....constant for now.
c     two options exist in the interpolation 1) smooth (recommended) 
c     interpolation of nodal data, 2) discontinuous "sampling" phase at 
c     quadrature points.
c
CAD
CAD    prop_blend is a smoothing function to avoid possible large density 
CAD   gradients, e.g., water and air flows where density ratios can approach
CAD   1000.
CAD
CAD    epsilon_ls is an adjustment to control the width of the band over which
CAD    the properties are blended. 

c      DES - ONLY ONE FLUID PROPERTY CONSIDERED    
       rho  = datmat(1,1,1)	! single fluid model, i.e., only 1 density
       rmu = datmat(1,2,1)
c      DES ------------------------------------

CAD	At this point we have a rho that is bounded by the two values for
CAD 	density 1, datmat(1,1,1), the fluid,  and density 2, datmat(1,1,2)
CAD     the gas

c
c  The above approach evaluates all intermediate quantities at the 
c  quadrature point, then combines them to form the needed quantities there.
c  1 alternative is calculating all quanties (only rho here) at the nodes and 
c  then interpolating the result to the quadrature points.  If this is done,
c  do not forget to do the same thing for rou in e3b!!!
c  ^^^^^^^^^^
c  ||||||||||
c  WARNING
c
c.... dynamic model
c      
c
      return
      end
       
      subroutine getdiffsclr(shape, dwl, yl, diffus)

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/matdat.h"
C
C      
      real*8   diffus(npro), rho(npro)
      real*8   yl(npro,nshl,ndof), dwl(npro,nshl), shape(npro,nshl)
      integer n, e
      rho(:)  = datmat(1,1,1)	! single fluid model, i.e., only 1 density
      if(isclr.eq.0) then  ! solving the temperature equation
         diffus(:) = datmat(1,4,1)
      else                      ! solving scalar advection diffusion equations
         diffus = scdiff(isclr)
      endif
c
      return
      end

      function ev2sa(xmut,rm,cv1)
      implicit none
      real*8 err,ev2sa,rm,cv1,f,dfds,rat,efac
      real*8 pt5,kappa,B,xmut,chi3,denom,cv1_3
      integer iter
      pt5=0.5
      err=1.0d-6
      ev2sa=rm*cv1*1.2599       ! inflection point chi=cv1*cuberoot(2)
      kappa=0.4
c$$$        B=5.5
      efac=0.1108               ! exp(-kappa*B)
      do iter=1,50
         chi3=ev2sa/rm
         chi3=chi3*chi3*chi3
         cv1_3=cv1**3
         denom=chi3+cv1_3
         
         f=ev2sa*chi3/denom - xmut
         dfds=chi3*(chi3+4.0*cv1_3)/(denom**2)
         rat=-f/dfds
         ev2sa=ev2sa+rat
         if(abs(rat).le.err) goto 20
      enddo
      write(*,*)'ev2sa failed to converge'
      write(*,*) 'dfds,        rat,        ev2sa,        mu'
      write(*,*) dfds,rat,ev2sa,rm
 20   continue
      return
      end  

!> This subroutine compute the total number of 
!! nodes in the model

      subroutine gtnods

      include "global.h"
      include "mpif.h"
      include "auxmpi.h"
      include "common_blocks/newdim.h"
      include "common_blocks/workfc.h"
      include "common_blocks/conpar.h"
c
c     Local variables
c
      INTEGER ierr,irecvcount,numvec
c
      DIMENSION irecvcount(numpe), numvec(numpe)
c
      if(numpe > 1) then
        irecvcount = 1
        numvec = nshg0
        call MPI_REDUCE_SCATTER (numvec, nshgt, irecvcount,
     &       MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
      else
        nshgt=nshg
      endif
c
      if (myrank .eq. master) then
        write(6,*) 'Total number of nodes = ',nshgt
      endif
c     
      return
      end

!> This file contains functions for dealing with higher order shape 
!! functions at the element level.
!! Returns the matrix of mode signs used for negating higher order
!! basis functions. Connectivity array is assumed to have negative
!! signs on all modes to be negated.

      subroutine getsgn(ien, sgn)
c
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien
C
      REAL*8                sgn
C
C     Local variables
C
      INTEGER             i
C
      dimension ien(npro,nshl),  sgn(npro,nshl)
      
      do i=nenl+1,nshl
         where ( ien(:,i) < 0 )
            sgn(:,i) = -one
         elsewhere
            sgn(:,i) = one
         endwhere
      enddo
      
      return 
      end
 
!> Returns the matrix of element shape functions with the higher
!! order modes correctly negated at the current quadrature point.
     
      subroutine getshp(shp, shgl, sgn, shape, shdrv)
c
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      REAL*8                sgn,         shape,       shdrv,       shgl
      REAL*8                shp
C
C     Local variables
C
      INTEGER             i,           j
C      
      dimension shp(nshl,ngauss),   shgl(nsd,nshl,ngauss),
     &          sgn(npro,nshl),     shape(npro,nshl),
     &          shdrv(npro,nsd,nshl)
      
      
      do i=1,nenl
         shape(:,i) = shp(i,intp)
         do j=1,3
            shdrv(:,j,i) = shgl(j,i,intp)
         enddo
      enddo
      if ( ipord > 1 ) then
         do i=nenl+1,nshl
            shape(:,i) = sgn(:,i) * shp(i,intp)
            do j=1,3
               shdrv(:,j,i) = shgl(j,i,intp)*sgn(:,i) 
            enddo
         enddo
      endif
      
      return 
      end

!> Returns the matrix of element shape functions with the higher
!! order modes correctly negated at the current quadrature point.
     
      subroutine getshpb(shp, shgl, sgn, shape, shdrv)
c
        include "global.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/elmpar.h"
C
C     Argument variables
C
      REAL*8                sgn,         shape,       shdrv,       shgl
      REAL*8                shp
C
C     Local variables
C
      INTEGER             i,           j
C
      dimension shp(nshl,ngaussb),  shgl(nsd,nshl,ngaussb),
     &          sgn(npro,nshl),     shape(npro,nshl),
     &          shdrv(npro,nsd,nshl)
      
      
      do i=1,nenl
         shape(:,i) = shp(i,intp)
         do j=1,3
            shdrv(:,j,i) = shgl(j,i,intp)
         enddo
      enddo
      if ( ipord > 1 ) then
         do i=nenl+1,nshl
            shape(:,i) = sgn(:,i) * shp(i,intp)
            do j=1,3
               shdrv(:,j,i) = shgl(j,i,intp)*sgn(:,i) 
            enddo
         enddo
      endif
      
      return 
      end
 
!> Compute the higher order modes that lie on the boundary of the 
!! element.
     
      subroutine getbnodes(lnode)
c
        include "global.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"

C
C     Argument variables
C
      INTEGER             lnode
C
C     Local variables
C
      INTEGER             n,           nem,         nfm
C      
      dimension lnode(27)

c
c.... boundary triangle of tet element
c
      if (lcsyst .eq. 1) then
         do n = 1, nenbl
            lnode(n) = n
         enddo
         if( ipord>1 ) then
            nem = ipord-1
            do n=1,3*nem
               lnode(nenbl+n) = nenbl+1+n
            enddo
         endif
         if( ipord>2 ) then
            nfm = (ipord-2)*(ipord-1)/2
            do n=1,nfm
               lnode(3+3*nem+n) = 4+6*nem+n
            enddo
         endif       
c     
c.... other element types deleted or need to be implemented
c
      else
         write (*,*) 'Boundary element not implemented for lcyst='
     &        ,lcsyst
         stop
      endif
      
      return 
      end
      
!> Evaluate coefficient vector at its interpolation points

      subroutine evalAtInterp( ycoeff,  yvals,  x,   nvars, npts )

      use     pointer_data

        include "global.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

C
C     Local variables
C
      INTEGER             i,           iblk,        iel,         j

C      
      integer nvars, npts, nHits(nshg)
      
      real*8  ycoeff(nshg,ndof),   yvals(nshg,nvars),
     &        shp(nshl,npts),      shgl(nsd,nshl,npts),
     &        intpnt(3,npts),      x(numnp,nsd)
      
      real*8, allocatable :: ycl(:,:,:)
      real*8, allocatable :: xl(:,:,:)
      real*8, allocatable :: yvl(:,:,:)
      real*8, allocatable :: sgn(:,:)

      yvals = zero
c
c.... generate the shape functions at the interpolation points
c
      call getIntPnts(intpnt,npts)
      do i=1,npts
         call shpTet(ipord,intpnt(:,i),shp(:,i),shgl(:,:,i))
      enddo
c
c.... loop over element blocks
c
      nHits = 0
      do iblk = 1, nelblk
         iel    = lcblk(1,iblk)
         lcsyst = lcblk(3,iblk)
         nenl   = lcblk(5,iblk) ! no. of vertices per element
         nshl   = lcblk(10,iblk)
         ndofl  = lcblk(8,iblk)
         npro   = lcblk(1,iblk+1) - iel 

         allocate ( ycl(npro,nshl,ndof ) )
         allocate ( yvl(npro,nshl,nvars) )
         allocate ( xl(npro,nenl,nsd   ) )
         allocate ( sgn(npro,nshl)       )
         
         call getsgn(mien(iblk)%p,sgn)
         
         call localy( ycoeff, ycl, mien(iblk)%p, ndof,  'gather  ')
         call localx( x,      xl,  mien(iblk)%p, nsd,   'gather  ')

         call eval  ( xl,       ycl,      yvl,      
     &                shp,      shgl,     sgn,      
     &                nvars,    npts    )

c
c.... average coefficients since stresses may be discontinuous
c         
         call localSum( yvals,    yvl,    mien(iblk)%p,  
     &                  nHits,    nVars)  
         
         
         deallocate ( ycl )
         deallocate ( yvl )
         deallocate ( sgn )
         deallocate ( xl  )
c
      enddo

c
c.... average the global values
c
      do i = 1, nshg
         do j = 1, nvars
            yvals(i,j) = yvals(i,j)/nHits(i) !(real(nHits(i),8))
         enddo
      enddo
      
      return
      end

!> Evaluate in element coordinate system

      subroutine eval( xl,      ycl,     yvl,     
     &                 shp,     shgl,    sgn,
     &                 nvars,   npts ) 
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/matdat.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             npts
C
C     Local variables
C
      INTEGER             i,           j,           n
C
      REAL*8                rmu
C      
      integer nvars
c
      real*8  ycl(npro,nshl,ndof),   yvl(npro,nshl,nvars),
     &        sgn(npro,nshl),        shape(npro,nshl),
     &        shdrv(npro,nsd,nshl),  shp(nshl,npts),
     &        shgl(nsd,nshl,npts),   xl(npro,nenl,nsd),
     &        shg(npro,nshl,nsd),    gradV(npro,nsd,nsd),
     &        dxidx(npro,nsd,nsd),   tmp(npro), wtmp
      
      yvl = zero
c
c.... loop over interpolation points
c
      do intp = 1, npts
         call getshp(shp,          shgl,      sgn, 
     &               shape,        shdrv)
      
c
c.... pressure and velocity
c
         do i = 1, nshl
            do j = 1, 4
               yvl(:,intp,j) = yvl(:,intp,j) + shape(:,i) * ycl(:,i,j)
            enddo
         enddo
c
c.... viscous stress
c
         call e3metric( xl,         shdrv,      dxidx,  
     &                  shg,        tmp)

         gradV = zero
         do n = 1, nshl
            gradV(:,1,1) = gradV(:,1,1) + shg(:,n,1) * ycl(:,n,2)
            gradV(:,2,1) = gradV(:,2,1) + shg(:,n,1) * ycl(:,n,3)
            gradV(:,3,1) = gradV(:,3,1) + shg(:,n,1) * ycl(:,n,4)
c     
            gradV(:,1,2) = gradV(:,1,2) + shg(:,n,2) * ycl(:,n,2)
            gradV(:,2,2) = gradV(:,2,2) + shg(:,n,2) * ycl(:,n,3)
            gradV(:,3,2) = gradV(:,3,2) + shg(:,n,2) * ycl(:,n,4)
c     
            gradV(:,1,3) = gradV(:,1,3) + shg(:,n,3) * ycl(:,n,2)
            gradV(:,2,3) = gradV(:,2,3) + shg(:,n,3) * ycl(:,n,3)
            gradV(:,3,3) = gradV(:,3,3) + shg(:,n,3) * ycl(:,n,4)
         enddo

         rmu = datmat(1,2,1)
            
         yvl(:,intp,6 ) = two * rmu * gradV(:,1,1)
         yvl(:,intp,7 ) = two * rmu * gradV(:,2,2)
         yvl(:,intp,8 ) = two * rmu * gradV(:,3,3)

         yvl(:,intp,9 ) = rmu * ( gradV(:,1,2) + gradV(:,2,1) )
         yvl(:,intp,10) = rmu * ( gradV(:,1,3) + gradV(:,3,1) )
         yvl(:,intp,11) = rmu * ( gradV(:,2,3) + gradV(:,3,2) )

c
c.... loop over interpolation points
c         
      enddo
      
      return
      end

!> This routine inputs all the necessary data, allocates required array 
!! storage, and sets up the appropriate parameters for the processing.

        subroutine input(npe,mrank)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/extrat.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intdat.h"
        include "common_blocks/itrpnt.h"
        include "common_blocks/mio.h"
        include "common_blocks/mioname.h"
        include "common_blocks/mmatpar.h"
        include "common_blocks/outpar.h"
        include "common_blocks/precis.h"
        include "common_blocks/solpar.h"
        include "common_blocks/timpar.h"
        include "common_blocks/title.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             mrank, 	mpoint,      npe
C
C     Local variables
C
      INTEGER             i,           intsec
C
      REAL*8                boltzm
C
        external endata
c
        character*8  date
        character*80 card
        integer TMRC

        numpe=npe
        myrank=mrank
        intsec=TMRC()
        ttim(100) = intsec

        epsM = sqrt(epsilon(one))
c
c.... read in and block all data
c

        call readnblk()

c
c.... open the echo file (echo closed at exit)
c
        if (myrank == master) 
     &  open (unit=iecho, file=fecho, status='unknown',   err=996)

c
c.... -------------------->  Control Parameters  <---------------------
c
c.... echo the global information
c

        title = 'Default Ensa Case'     
        call date_and_time (date)
        title  = title(1:69) // ' ' // date(7:8) // '/' // date(5:6)
     &                                           // '/' // date(3:4)
        ititle = char(12) // title(1:78)

        if (myrank == master) then
          write (iecho,1100) ititle, numpe,  numnp,  numel,  numelb,
     &                               nen,    nfaces, nsd,    numflx
          write (iecho,1200)         iALE,   navier,
     &                               necho
c
c.... check the input parameters
c
          if (iALE .lt. 0 .or. iALE .gt. 1)
     &                     call error ('input   ','iALE    ',iALE)
c
          if (navier .lt. 0 .or. navier .gt. 1)
     &                     call error ('input   ','navier  ',navier)
c
          if (necho  .lt. 0 .or. necho  .gt. 3)
     &                     call error ('input   ','necho   ',necho)
        endif

        if (myrank == master) then
          write (iecho,1300) ititle, ntseq, ivart, iDC,
     &                             Kspace, nGMRES
c
c.... check the input parameters
c
          if (ntseq  .gt. 100) call error ('input   ','ntseq   ',ntseq)
c
          if (ivart  .lt. 1 .or. ivart .gt. 3) 
     &                       call error ('input   ','ivart   ',ivart)
c
          if (iDC    .lt. 0 .or. iDC   .gt. 4) 
     &                       call error ('input   ','iDC     ',iDC)
c
          if (Kspace .lt. 1)   call error ('input   ','Kspace  ',Kspace)
c
          if (nGMRES .lt. 1)   call error ('input   ','nGMRES  ',nGMRES)
        endif
c
c.... allocate memory for the Q-R algorithm of GMRES 
c
        mHBrg = mpoint ('H-Berg  ', Kspace+1,Kspace,  0)
        meBrg = mpoint ('e-Berg  ', Kspace+1,0,       0)
        myBrg = mpoint ('y-Berg  ', Kspace,  0,       0)
        mRcos = mpoint ('Rcos-QR ', Kspace,  0,       0)
        mRsin = mpoint ('Rsin-QR ', Kspace,  0,       0)
c
c.... ----------------->  Time Sequence Parameters  <-----------------
c
c.... echo the solver information
c
        iprev = 0
        do i = 1, ntseq
          if (mod(i,50).eq.1 .and. myrank .eq. master) 
     &      write(iecho,1400) ititle

          if (myrank .eq. master) 
     &      write (iecho,1500)      i, nstep(i),  niter(i),  impl(i),
     &                                 LHSupd(i), epstol(i)
c
          if ((iALE .eq. 1) .or. (niter(i) .gt. 1)) iprev = 1
        enddo
c
c.... echo the spatial and time integration information
c
        do i = 1, ntseq
          if (mod(i,50).eq.1 .and. myrank .eq. master) 
     &      write(iecho,1600) ititle
          if (myrank .eq. master) 
     &      write (iecho,1700)      i, intg(1,i), intg(2,i), rhoinf(i),
     &                                 loctim(i), Delt(i),   CFLfl(i),
     &                                 CFLsl(i)
c
        enddo
c
        if (myrank .eq. master) 
     &    write (iecho,1800) ititle, ntout,  ioform, ro,     vel,    
     &                               temper, press,  entrop
        
        if (myrank .eq. master) then
           write (*,*) 'Element block size = ',ibksiz
        endif

c
c.... generate the spatial integration rules
c
        call genint

        ichem = 0
c
c.... estimate number of nonzero global entries:
c....       nnonzero ~ nnz * nshg
c
        if (ipord .eq. 1) then
           nnz = 35
        else if (ipord .eq. 2) then
           nnz = 85
        else  !assumed cubic
           nnz = 300
        endif


c
c.... compute fluid thermodynamic properties
c
        Boltzm = Rh / Nh
c
        do i = 1, 5
          Rs(i)   = Rh / Msh(i)
          h0s(i)  = h0sh(i) / Msh(i)
          cpsh(i) = ( pt5 * dofs(i) + one ) * Rh
          cps(i)  = ( pt5 * dofs(i) + one ) * Rs(i)
          cvs(i)  = pt5 * dofs(i) * Rs(i)
        enddo
c
        do i = 1, 5
          s0sh(i) = Rh * ( pt5*( log( (two*pi*Msh(i)/(Nh*Planck**2))**3
     &                  * Boltzm**5 ) + five ) + log(g0s(i)) )
        enddo
c
        do i = 1, 3
          s0sh(i) = s0sh(i) + Rh * ( one - log(sigs(i)*Trot(i)) )
        enddo
c
        Rgas  = one / ( xN2 / Rs(1) + xO2 / Rs(2) ) 
c        Rgas  = 0.4*716.5
        yN2   = xN2 * Rgas / Rs(1)
        yO2   = xO2 * Rgas / Rs(2)
c
        s0    =     yN2 * s0sh(1) / Msh(1) + yO2 * s0sh(2) / Msh(2)
        const = - ( yN2 * Rs(1) * log(xN2) + yO2 * Rs(2) * log(xO2) )
c
c.... stop CPU-timer
c
c        call timer ('Back    ')

        return
c
c.... end of file error handling
c
992     call error ('input   ','opening ', imat)
993     call error ('input   ','opening ', iin)
996     call error ('input   ','opening ', iecho)
999     call error ('input   ','end file', iin)
c
1000    format(a69)
1100    format(a80,//,
     &  ' M a i n   C o n t r o l   P a r a m e t e r s        '   //,
     &  ' number of processing elements . . . . . . . (numpe )=',i10//,
     &  ' number of mesh nodes  . . . . . . . . . . . (numnp )=',i10//,
     &  ' number of elements  . . . . . . . . . . . . (numel )=',i10//,
     &  ' number of boundary elements . . . . . . . . (numelb)=',i10//,
     &  ' number of element nodes . . . . . . . . . . (nen   )=',i10//,
     &  ' number of element faces . . . . . . . . . . (nfaces)=',i10//,
     &  ' number of space dimensions  . . . . . . . . (nsd   )=',i10//,
     &  ' number of boundary flux nodes . . . . . . . (numflx)=',i10/)
1200    format(
     &  ' frame of reference  . . . . . . . . . . . . (iALE  )=',i10//,
     &  '    eq. 0, Eulerian                                   ',  / ,
     &  '    eq. 1, arbitrary Lagrangian-Eulerian              ',  //,
     &  ' equation type . . . . . . . . . . . . . . . (navier)=',i10//,
     &  '    eq. 0, Euler (inviscid)                           ',  / ,
     &  '    eq. 1, Navier-Stokes (viscous)                    ',  //,
     &  ' input echo parameter  . . . . . . . . . . . (necho )=',i10)
1300    format(a80,//,
     &  ' S o l u t i o n   P a r a m e t e r s                '   //,
     &  ' number of time sequences  . . . . . . . . . (ntseq )=',i10//,
     &  ' variational formulation . . . . . . . . . . (ivart )=',i10//,
     &  '    eq. 1, Galerkin                                   ',  / ,
     &  '    eq. 2, Galerkin/least-squares                     ',  / ,
     &  '    eq. 3, plus discontinuity-capturing operator      ',  //,
     &  ' discontinuity-capturing type  . . . . . . . (iDC   )=',i10//,
     &  '    eq. 1, DC-mallet                                  ',  / ,
     &  '    eq. 2, quadratic DC                               ',  / ,
     &  '    eq. 3, smallest of the previous two DCs           ',  //,
     &  ' dimension of Krylov space . . . . . . . . . (kspace)=',i10//,
     &  ' maximum number of GMRES cycles  . . . . . . (ngmres)=',i10)
1400    format(a80,//,
     &  ' S o l v e r   I n f o r m a t i o n                    ',//,
     &  ' Seq num    Nstep    Niter    Impl      Nupdate',
     &  '     Eps_Tol')
1500    format(i6,i10,i9,i8,i11,2x,e15.5)
1600    format(a80,//,
     &  ' S p a t i a l   a n d   T i m e   I n t e g r a t i o n',//,
     &  ' Seq num  Elem Int.  Bound Int.  Level  LCtime',
     &  '    Delt       CFLfld    CFLsld')
1700    format(i6,i8,i12,e13.4,1p,i8,1p,e13.4,0p,2f10.4)
1800    format(a80,//,
     &  ' O u t p u t   I n f o r m a t i o n                  ',1p,//,
     &  ' number of time steps per output . . . . . . (ntout )=',i10//,
     &  ' I/O format  . . . . . . . . . . . . . . . . (ioform)=',i10//,
     &  '    eq. 0, ASCII                                      ',  / ,
     &  '    eq. 1, binary                                     ',  //,
     &' scaling factor for density  . . . . . . . . (ro    )=',e15.5//,
     &' scaling factor for velocity . . . . . . . . (vel   )=',e15.5//,
     &' scaling factor for temperature. . . . . . . (temper)=',e15.5//,
     &' scaling factor for pressure . . . . . . . . (press )=',e15.5//,
     &' scaling factor for entropy  . . . . . . . . (entrop)=',e15.5)
c

        end

!> This program satisfies the boundary conditions on the Y-variables.
!!
!! input:<BR>
!! @param[in] y(nshg,nflow) y variables 
!! @param[in] iBC(nshg) Boundary Condition Code
!! @param[in] BC(nshg,ndofBC) Boundary condition constraint parameters
!!
!! output:<BR>
!! @param[out] y(nshg,nflow) Adjusted V value(s) corresponding to a constraint d.o.f.
!!

        subroutine itrBC (y,ac, iBC, BC, iper, ilwork)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/workfc.h"
c
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                ac,          bc,          y
C
C     Local variables
C
      INTEGER             i
C
        dimension y(nshg,nflow),             iBC(nshg),
     &            ac(nshg,nflow),            BC(nshg,ndofBC)

        dimension ilwork(nlwork),           iper(nshg)

c
c.... ------------------------->  Velocity  <--------------------------
c.... 3D
c
c.... x1-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 1)
            y(:,1) =  BC(:,3)  - BC(:,4) * y(:,2)
     &                         - BC(:,5) * y(:,3)
          endwhere
c
c.... x2-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 2)
            y(:,2) = BC(:,3)  - BC(:,4) * y(:,1)
     &                        - BC(:,5) * y(:,3)
          endwhere
c
c.... x1-velocity and x2-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 3)
            y(:,1) =  BC(:,3)  - BC(:,4) * y(:,3)
            y(:,2) =  BC(:,5)  - BC(:,6) * y(:,3)
          endwhere
c
c.... x3-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 4)
            y(:,3) = BC(:,3) - BC(:,4) * y(:,1)
     &                       - BC(:,5) * y(:,2)
          endwhere
c
c.... x1-velocity and x3-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 5)
            y(:,1) = BC(:,3) - BC(:,4) * y(:,2)
            y(:,3) = BC(:,5) - BC(:,6) * y(:,2)
          endwhere
c
c.... x2-velocity and x3-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 6)
            y(:,2) = BC(:,3)  - BC(:,4) * y(:,1)
            y(:,3) = BC(:,5)  - BC(:,6) * y(:,1)
          endwhere
c
c.... x1-velocity, x2-velocity and x3-velocity, 3D
c
          where (ibits(iBC,3,3) .eq. 7)
            y(:,1) =  BC(:,3)
            y(:,2) =  BC(:,4)
            y(:,3) =  BC(:,5) 
          endwhere
c
c       endif
c
c.... end of velocity
c
c.... ------------------------->  Pressure  <--------------------------
c
        if (any(btest(iBC,2))) then
c
c.... pressure
c
          where (btest(iBC,2))
            y(:,4) = BC(:,1)  ! pressure here
          endwhere
c
        endif
c
c.... local periodic (and axisymmetric) boundary conditions (no communications)
c 
      do i = 1,nflow
           y(:,i) = y(iper(:),i)
           ac(:,i) = ac(iper(:),i)
      enddo
c
c.... communications
c 
        if (numpe > 1) then
           call commu (y, ilwork, nflow, 'out')
           call commu (ac, ilwork, nflow, 'out')
        endif
c
c       slave has masters value, for abc we need to rotate it
c
        if(iabc==1) then        !are there any axisym bc's
           call rotabc(y, iBC, 'out')
           call rotabc(ac, iBC, 'out')
        endif
     
c
c.... return
c
        return
        end

!> This routine satisfies the boundary conditions on the isclr

        subroutine itrBCSclr (y, ac, iBC, BC, iper, ilwork)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/workfc.h"
c
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                ac,          bc,          y
C
C     Local variables
C
      INTEGER             i,           ib,          ibb,         id
C
      REAL*8              T
C
        dimension y(nshg,ndof),             iBC(nshg),
     &            ac(nshg,ndof),            BC(nshg,ndofBC)

        dimension ilwork(nlwork),            iper(nshg)
        dimension T(nshg)

        if(isclr.eq.0) then ! this is temperature
           ib=1
           ibb=2
           id=5
        else
           ib=5+isclr
           ibb=ib+1
           id=ib
        endif

c
c
c.... ------------------------>  Scalar  <------------------------
c
c
        where (btest(iBC,ib))
          y(:,id) =  BC(:,ibb)
        endwhere
c
c.... local periodic (and axisymmetric) boundary conditions (no communications)
c 
      do i = 1,nshg
          y(i,id) = y(iper(i),id)
          ac(i,id) = ac(iper(i),id)
      enddo
c
c.... communications
c 
        if (numpe > 1) then
           T=y(:,id)
           call commu (T, ilwork, 1, 'out')
           y(:,id)=T
           T=ac(:,id)
           call commu (T, ilwork, 1, 'out')
           ac(:,id)=T
        endif
     
        return
        end

!> Initialize the predictor multicorrector (set up parameters)

      subroutine itrSetup ( y,  acold ) 
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/timdat.h"
C    
      real*8     y(nshg,ndof),     acold(nshg,ndof)
      
c
c  Define the Hulbert parameters
c  second order if between (and including) 0 and 1 otherwise backward Euler
c
      if( rhoinf(itseq).lt.0.or.rhoinf(itseq).gt.1) then ! backward Euler
         almi   = one
         alfi   = one
         gami   = one
         ipred  = 1
      else           !second order family
         almi   = (three-rhoinf(itseq))/(one+rhoinf(itseq))/two
         alfi   = one/(one+rhoinf(itseq))
         gami   = pt5+almi-alfi
         if(ideformwall.eq.1) then
            betai=1.0
         else
            betai=0.0
         endif
      endif
c     
c.... set the jacobian type
c     
      Jactyp=0
      if (impl(itseq) .eq. 3) then
         Jactyp = 1
         impl(itseq) = 2
      endif
c     
c.... same_Dy predictor special case
c     
      if (ipred.eq.4 .and. itseq .eq. 1 ) then
         y=y-(one-alfi)*Delt(1)*acold
         if ( rhoinf(itseq) .eq. 0.0 ) then
            ipred = 3
         endif
      endif
c
c.... set the global time increment and the CFL data
c
      Dtgl   = one / Delt(itseq)  ! caution: inverse of time step
      CFLfld = CFLfl(itseq)
      CFLsld = CFLsl(itseq)
      
      return
      end

!> Predict solution at time n+1

      subroutine itrPredict (yold,  y,  acold,   ac,   uold,   u)
      
      use LagrangeMultipliers 

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      REAL*8                fct,         fct1,        fct2,        fct3
C   
      real*8        y(nshg,ndof),               ac(nshg,ndof),
     &              u(nshg,nsd),                yold(nshg,ndof),
     &              acold(nshg,ndof),           uold(nshg,nsd)

      
      if ( ipred.eq.1) then     ! yn+1_pred=yn
         fct = (gami-one)/gami
         y  = yold
         ac = acold * fct
         if(ideformwall.eq.1) 
     &          u(:,1:3) = uold(:,1:3) + Delt(itseq)*yold(:,1:3) + 
     &              pt5*((gami-two*betai)/gami)*
     &              Delt(itseq)*Delt(itseq)*acold(:,1:3)
      endif
c     
      if ( ipred.eq.2) then     ! an+1_pred=0
         y  = yold + (one - gami)/Dtgl * acold
         ac = 0.0
      endif
c     
      if(ipred.eq.3 ) then      ! an+1_pred=an
         y  = yold+alfi*Delt(itseq)*acold
         ac = acold
      endif
c     
      if ( ipred.eq.4 ) then    ! protect from DC=4 rho=0, same dV
         fct1 = alfi/(one-alfi)
         fct2 = one-almi/gami
         fct3 = almi/gami/alfi*Dtgl
         y    = yold+fct1*(yold-y)
         ac   = acold*fct2+(y-yold)*fct3
      endif
c     
      if (Lagrange .gt. 0) then
         Lag = Lagold
      endif
      
      return
      end

!> Correct solution at time n+1

      subroutine itrCorrect ( y,     ac,   u,   solinc )
      
      use LagrangeMultipliers 

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      REAL*8                fct1,        fct2
C   
      real*8      y(nshg,ndof),               ac(nshg,ndof),  
     &            u(nshg,nsd),                solinc(nshg,4)
      
      fct1 = gami*Delt(itseq)
      fct2 = gami*alfi*Delt(itseq)
      
      y(:,1:3)  = y(:,1:3)  + fct1 * solinc(:,1:3)
      y(:,4  )  = y(:,4  )  + fct2 * solinc(:,4  )
      ac(:,1:3) = ac(:,1:3) + solinc(:,1:3)

      if(ideformwall.eq.1) 
     &   u(:,1:3)  = u(:,1:3)  + 
     &            Delt(itseq)*Delt(itseq)*betai*solinc(:,1:3)
c     
      if (Lagrange .gt. 0) then
         Lag(:,1:3) = Lag(:,1:3) + fct2 * Lagincr(:,1:3) 
      endif
      return
      end

!> Correct solution at time n+1

      subroutine itrCorrectSclr ( y,     ac,   solinc )
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      INTEGER             is
C
      REAL*8                fct1
C
      real*8      y(nshg,ndof),               ac(nshg,ndof),  
     &            solinc(nshg)
      
      fct1 = gami*Delt(itseq)
      is=5+isclr
      y(:,is)  = y(:,is)  + fct1 * solinc(:)
      ac(:,is) = ac(:,is) + solinc(:)
c     
      return
      end

!> Compute solution and acceleration at n+alpha

      subroutine itrYAlpha ( uold,        yold,        acold,        
     &                       u,           y,           ac,
     &                       uAlpha,      yAlpha,      acAlpha )

c      use readarrays       !reads in uold and acold     
      use LagrangeMultipliers 

      include "global.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/conpar.h"
      include "common_blocks/timdat.h"

      real*8        yold(nshg,ndof),            acold(nshg,ndof),
     &              y(nshg,ndof),               ac(nshg,ndof),
     &              yAlpha(nshg,ndof),          acAlpha(nshg,ndof),
     &              u(nshg,nsd),                uold(nshg,nsd),
     &              uAlpha(nshg,nsd)

      acAlpha(:,4) = zero  !pressure acceleration is never used but....

      yAlpha (:,1:3) = yold(:,1:3) 
     &                  + alfi * (y(:,1:3) - yold(:,1:3))

      acAlpha(:,1:3) = acold(:,1:3)
     &                  + almi * (ac(:,1:3) - acold(:,1:3))

      yAlpha (:,4  ) = y(:,4)

      if(ideformwall.eq.1) uAlpha (:,1:3) = uold(:,1:3) 
     &                  + alfi * (u(:,1:3) - uold(:,1:3))
      
      if(ndof.ge.5) then
c
c  Now take care of temperature, turbulence, what have you
c
      

         yAlpha (:,5:ndof  ) = yold(:,5:ndof) 
     &                       + alfi * (y(:,5:ndof) - yold(:,5:ndof))
         acAlpha(:,5:ndof  ) = acold(:,5:ndof) 
     &                       + almi * (ac(:,5:ndof) - acold(:,5:ndof))

      endif

      if (Lagrange .gt. 0) then
         Lagalpha(:,1:3) = Lag(:,1:3)
      endif
      return
      end

!> Update solution at end of time step

      subroutine itrUpdate( yold,          acold,        uold,
     &                      y,             ac,           u )

c      use readarrays            !reads in uold and acold
      use LagrangeMultipliers 

        include "global.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/conpar.h"

      real*8        yold(nshg,ndof),            acold(nshg,ndof),
     &              y(nshg,ndof),               ac(nshg,ndof),
     &              u(nshg,nsd),                uold(nshg,nsd)

      yold  = y

      acold = ac

      if(ideformwall.eq.1)  uold  = u

      if (Lagrange .gt. 0) then
         Lagold = Lag
      endif

      return
      end

!> This subroutine performs a vector gather/scatter operation.
!!
!! input:<BR>
!! @param[in] global(nshg,n) Global array
!! @param[in] rlocal(npro,nshl,n) Local array
!! @param[in] ien(npro,nshl) Nodal connectivity
!! @param[in] n Number of d.o.f.'s to be copied
!! @param[in] code The transfer code
!! - .eq. 'gather  ', from global to local
!! - .eq. 'scatter ', add  local to global 
!! - .eq. 'globaliz', from local to global

        subroutine local (global, rlocal, ientmp, n, code)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/mtimer2.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ientmp,      n
C
      REAL*8                global,      rlocal
C
C     Local variables
C
      INTEGER             i,           ien,         j,           nel
C
        dimension global(nshg,n),           rlocal(npro,nshl,n),
     &            ien(npro,nshl),           ientmp(npro,nshl)
c
        character*8 code
        
c
c.... cubic basis has negatives in ien
c
        if (ipord > 2) then
           ien = abs(ientmp)
        else
           ien = ientmp
        endif
c
c.... ------------------------>  'localization  '  <--------------------
c
        if (code .eq. 'gather  ') then
c
c.... gather the data
c

          do j = 1, n
            do i = 1, nshl
              rlocal(:,i,j) = global(ien(:,i),j)
            enddo
          enddo


c
c.... transfer count
c
          gbytes = gbytes + n*nshl*npro
c
c.... return
c
          return
        endif
c
c.... ------------------------->  'assembling '  <----------------------
c
        if (code .eq. 'scatter ') then
c
c.... scatter the data (possible collisions)
c
          do j = 1, n
            do i = 1, nshl
              do nel = 1,npro
                global(ien(nel,i),j) = global(ien(nel,i),j) 
     &                               + rlocal(nel,i,j)
              enddo
            enddo
          enddo

c
c.... transfer and flop counts
c
          sbytes = sbytes + n*nshl*npro
          flops  = flops  + n*nshl*npro
c
c.... return
c
          return
        endif
c
c.... ------------------------->  'globalizing '  <----------------------
c
        if (code .eq. 'globaliz') then
c
c.... scatter the data (possible collisions)
c
          do j = 1, n
            do i = 1, nshl
              do nel = 1,npro
                global(ien(nel,i),j) = rlocal(nel,i,j)
              enddo
            enddo
          enddo
c
c.... return
c
          return
        endif
c
c.... --------------------------->  error  <---------------------------
c
        call error ('local   ', code, 0)
c
c.... end
c
        end
c
!> This subroutine performs a vector gather/scatter operation for the
!! nodal coordinates array.
!!
!! input:<BR>
!! @param[in] global(numnp,n) Global array
!! @param[in] rlocal(npro,nenl,n) Local array
!! @param[in] ien(npro,nshl) Nodal connectivity
!! @param[in] n Number of d.o.f.'s to be copied
!! @param[in] code The transfer code
!! - .eq. 'gather  ', from global to local
!! - .eq. 'scatter ', add  local to global 
!!

        subroutine localx (global, rlocal, ien, n, code)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/mtimer2.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ien,         n
C
      REAL*8                global,      rlocal
C
C     Local variables
C
      INTEGER             i,           j,           nel
C
        dimension global(numnp,n),           rlocal(npro,nenl,n),
     &            ien(npro,nshl)
c
        character*8 code
c
c.... ------------------------>  'localization  '  <--------------------
c
        if (code .eq. 'gather  ') then
c
c.... gather the data
c
          do j = 1, n
            do i = 1, nenl
              rlocal(:,i,j) = global(ien(:,i),j)
            enddo
          enddo


c
c.... transfer count
c
          gbytes = gbytes + n*nenl*npro
c
c.... return
c
          return
        endif
c
c.... ------------------------->  'assembling '  <----------------------
c
        if (code .eq. 'scatter ') then
c
c.... scatter the data (possible collisions)
c

          do j = 1, n
            do i = 1, nenl
              do nel = 1,npro
                global(ien(nel,i),j) = global(ien(nel,i),j) 
     &                               + rlocal(nel,i,j)
              enddo
            enddo
          enddo


c
c.... transfer and flop counts
c
          sbytes = sbytes + n*nenl*npro
          flops  = flops  + n*nenl*npro
c
c.... return
c
          return
        endif
c
c.... --------------------------->  error  <---------------------------
c
        call error ('local   ', code, 0)
c
c.... end
c
        end
c

!> Sum the data from the local array to the global degrees of
!!  freedom and keep track of the number of locals contributing
!!  to each global dof. This may be used to find the average.

        subroutine localSum (global, rlocal, ientmp, nHits, n)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ientmp,      n,           nhits
C
      REAL*8                global,      rlocal
C
C     Local variables
C
      INTEGER             i,           idg,         ien,         j
      INTEGER             nel
C
        dimension global(nshg,n),           rlocal(npro,nshl,n),
     &            ien(npro,nshl),           ientmp(npro,nshl),
     &            nHits(nshg)
c
c.... cubic basis has negatives in ien
c
        if (ipord > 2) then
           ien = abs(ientmp)
        else
           ien = ientmp
        endif
c
c.... ------------------------->  'assembling '  <----------------------
c
        do j = 1, n
           do i = 1, nshl
              do nel = 1,npro
                 idg = ien(nel,i)
                 global(idg,j) = global(idg,j) + rlocal(nel,i,j)
              enddo
           enddo
        enddo
        do i = 1, nshl
           do nel = 1,npro
              idg = ien(nel,i)
              nHits(idg) = nHits(idg) + 1
           enddo
        enddo
c
c.... end
c
        end

!> This subroutine performs a vector gather/scatter operation on boundary only.
!!
!! input:<BR>
!! @param[in] global(nshg,n) Global array
!! @param[in] rlocal(npro,nshl,n) Local array
!! @param[in] ien(npro,nshl) Nodal connectivity
!! @param[in] n Number of d.o.f.'s to be copied
!! @param[in] code The transfer code
!! - .eq. 'gather  ', from global to local
!! - .eq. 'scatter ', add  local to global 
!! - .eq. 'globaliz', from local to global
 
      subroutine localb (global, rlocal, ientmp, n, code)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/mtimer2.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ientmp,      n
C
      REAL*8                global,      rlocal
C
C     Local variables
C
      INTEGER             i,           ien,         j,           nel
C
        dimension global(nshg,n),           rlocal(npro,nshlb,n),
     &            ien(npro,nshl),           ientmp(npro,nshl)
c
        character*8 code
        
c
c.... cubic basis has negatives in ien
c
        if (ipord > 2) then
           ien = abs(ientmp)
        else
           ien = ientmp
        endif
c
c.... ------------------------>  'localization  '  <--------------------
c
        if (code .eq. 'gather  ') then
c
c.... set timer
c
cad          call timer ('Gather  ')
c
c.... gather the data
c

          do j = 1, n
            do i = 1, nshlb
              rlocal(:,i,j) = global(ien(:,i),j)
            enddo
          enddo


c
c.... transfer count
c
          gbytes = gbytes + n*nshl*npro
c
c.... return
c
cad          call timer ('Back    ')
          return
        endif
c
c.... ------------------------->  'assembling '  <----------------------
c
        if (code .eq. 'scatter ') then
c
c.... set timer
c
cad          call timer ('Scatter ')
c
c.... scatter the data (possible collisions)
c
          do j = 1, n
            do i = 1, nshlb
              do nel = 1,npro
                global(ien(nel,i),j) = global(ien(nel,i),j) 
     &                               + rlocal(nel,i,j)
              enddo
            enddo
          enddo

c
c.... transfer and flop counts
c
          sbytes = sbytes + n*nshlb*npro
          flops  = flops  + n*nshlb*npro
c
c.... return
c
CAD          call timer ('Back    ')
          return
        endif
c
c.... ------------------------->  'globalizing '  <----------------------
c
        if (code .eq. 'globaliz') then
c
c.... scatter the data (possible collisions)
c
          do j = 1, n
            do i = 1, nshlb
              do nel = 1,npro
                global(ien(nel,i),j) = rlocal(nel,i,j)
              enddo
            enddo
          enddo
c
c.... return
c
cad          call timer ('Back    ')
          return
        endif
c
c.... --------------------------->  error  <---------------------------
c
        call error ('local   ', code, 0)
c
c.... end
c
        end
c

!> This subroutine performs a vector gather/scatter operation.
!!
!! input:<BR>
!! @param[in] global(nshg,n) Global array
!! @param[in] rlocal(npro,nshl,n) Local array
!! @param[in] ien(npro,nshl) Nodal connectivity
!! @param[in] n Number of d.o.f.'s to be copied
!! @param[in] code The transfer code
!! - .eq. 'gather  ', from global to local
!! - .eq. 'scatter ', add  local to global 
!! - .eq. 'globaliz', from local to global
!!

        subroutine localy (global, rlocal, ientmp, n, code)
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/mtimer2.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
C
C     Argument variables
C
      INTEGER             ientmp,      n
C
      REAL*8                global,      rlocal
C
C     Local variables
C
      INTEGER             i,           ien,         j
C
        dimension global(nshg,n),           rlocal(npro,nshl,n),
     &            ien(npro,nshl),           ientmp(npro,nshl)
c
        character*8 code
        
c
c.... cubic basis has negatives in ien
c
        if (ipord > 2) then
           ien = abs(ientmp)
        else
           ien = ientmp
        endif
c
c.... ------------------------>  'localization  '  <--------------------
c
        if (code .eq. 'gather  ') then
c
c.... set timer
c
c          call timer ('Gather  ')
c
c.... gather the data to the current block 
c

CAD      rlocal = yl={P, u, v, w, T, scalar1, ...}
CAD	 global = y = {u, v, w, P, T, scalar1, ...}

CAD      Put u,v,w in the slots 2,3,4 of yl 

          do j = 1, 3
            do i = 1, nshl
              rlocal(:,i,j+1) = global(ien(:,i),j)
            enddo
          enddo

CAD      Put Pressure in the first slot of yl

          do i = 1, nshl
             rlocal(:,i,1) = global(ien(:,i),4)
          enddo

CAD      Fill in the remaining slots with T, and additional scalars
          
          if(n.gt.4) then
             do j = 5, n
                do i = 1, nshl
                   rlocal(:,i,j) = global(ien(:,i),j)
                enddo
             enddo
          endif
c
c.... transfer count
c
          gbytes = gbytes + n*nshl*npro
c
c.... return
c
c          call timer ('Back    ')
          return
        endif
c
c.... ------------------------->  'assembling '  <----------------------
c
        if (code .eq. 'scatter ') then
           write(*,*) 'do not use localy here'
        endif
c
c.... ------------------------->  'globalizing '  <----------------------
c
        if (code .eq. 'globaliz') then
           write(*,*) 'do not use localy here'
        endif
c
c.... --------------------------->  error  <---------------------------
c
        call error ('local   ', code, 0)
c
c.... end
c
        end

c     
c--------------
c     drvAllreduce
c--------------
c     
      subroutine drvAllreduce ( eachproc, result, m )
c     
      include "global.h"
      include "mpif.h"
      include "common_blocks/workfc.h"
c   
C     Argument variables
C
      INTEGER             m
C
      REAL*8                eachproc,    result
C
C     Local variables
C
      INTEGER             ierr

C  
      dimension eachproc(m), result(m)
c     
      if (numpe > 1) then
         call MPI_ALLREDUCE ( eachproc, result, m, 
     &        MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr )
      else
         result = eachproc
      endif
c     
      return
      end

!> Sum real*8 array over all processors

      subroutine sumgat (u, n, summed)

      include "global.h"
      include "mpif.h"
      include "auxmpi.h"
      include "common_blocks/conpar.h"
      include "common_blocks/fronts.h"
      include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             n
C
      REAL*8                summed,      u
C
C     Local variables
C
      INTEGER             ierr,        ilwork,      irecvcount
C
      REAL*8                sumvec
C
      dimension u(nshg,n), ilwork(nlwork) 
      dimension sumvec(numpe), irecvcount(numpe)

      summed = sum(u)

      if (numpe > 1) then
         irecvcount = 1
         sumvec = summed
         call MPI_REDUCE_SCATTER (sumvec, summed, irecvcount, 
     &        MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)

      endif

      return
      end

!> Sum real*8 array of length nnp over all processors

      subroutine sumgatN (u, n, summed, nnp)

      include "global.h"
      include "mpif.h"
      include "auxmpi.h"
      include "common_blocks/fronts.h"
      include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             n,           nnp
C
      REAL*8                summed,      u
C
C     Local variables
C
      INTEGER             ierr,        ilwork,      irecvcount
C
      REAL*8                sumvec
C
      dimension u(nnp,n), ilwork(nlwork) 
      dimension sumvec(numpe), irecvcount(numpe)

c protect against underflow
c     summed = sum(u)
      summed = sum(u) + 1.e-20

      if (numpe > 1) then
         irecvcount = 1
         sumvec = summed
         call MPI_REDUCE_SCATTER (sumvec, summed, irecvcount, 
     &        MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)

      endif

      return
      end

!> Sum integer array over all processors

      subroutine sumgatInt (u, n, summed )

      include "global.h"
      include "mpif.h"
      include "auxmpi.h"
      include "common_blocks/workfc.h"

C
C     Argument variables
C
      INTEGER             n
C
C     Local variables
C
      INTEGER             ierr
C
      integer u(n), summed
      integer sumvec(numpe), irecvcount(numpe)

c$$$      ttim(62) = ttim(62) - tmr()

      summed = sum(u)

      if (numpe > 1) then
         irecvcount = 1
         sumvec = summed
         call MPI_REDUCE_SCATTER (sumvec, summed, irecvcount, 
     &        MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)

      endif
c$$$      ttim(62) = ttim(62) + tmr()

      return
      end

!> This function dynamically allocates memory for the arrays.
!!
!! input:<BR>
!! @param[in] name Name of the array
!! @param[in] ndim1 first dimension of the array 
!! @param[in] ndim2 second dimension of the array  
!! @param[in] ndim3 third dimension of the array
!!
!! output:<BR>
!! @param[out] mpoint Memory location
!!

        function mpoint (name,  ndim1,  ndim2,  ndim3)
c
        include "global.h"
        include "common_blocks/point.h"
c
C     Argument variables
C
      INTEGER            mpoint,  ndim1,       ndim2,       ndim3
C
C     Local variables
C
      INTEGER             idim1,       idim2,       idim3
C
        character*8 name
c
c.... calculate the array size
c
        idim1  = ndim1
        idim2  = ndim2 * min(idim1, 1)
        idim3  = ndim3 * min(idim2, 1)
c
c.... store the array information
c
        mpoint = mbeg
c
c.... set the memory pointer 
c
        mbeg   = mpoint + max(1,idim1) * max(1,idim2) * max(1,idim3)
c
c.... if past the end of total memory allocated, set the error message
c
        if (mbeg .gt. mend) call error ('mpoint  ', name, mbeg-mend)
c
c.... return
c
        return
        end

      subroutine setper (nshg)
       
      use periodicity
C
C     Argument variables
C
      INTEGER             nshg
C
       allocate (rcount(nshg))
       
       return
       end

       subroutine perprep (iBC, iper,nshg)
       
       use periodicity
C       
C     Argument variables
C
      INTEGER             ibc,         iper,        nshg
C
C     Local variables
C
      INTEGER             i,           j,           k
C
      REAL*8                one
C
       dimension iBC(nshg),
     &           iper(nshg)       
  
c
c..... calculate the inverse of the number of slaves + 1
c
       one=1.00000000000
       rcount=one
       do j = 1,nshg
          if (btest(iBC(j),10)) then
             i = iper(j)
             rcount(i) = rcount(i) + one
          endif
       enddo
       do k=1,nshg
          if(rcount(k).ne.one) then
             rcount(k)=one/rcount(k)
          endif
       enddo
      
c
c.... return
c
       return
       end

!> Natural pressure boundary condition can be calculated with p, the pressure,
!!  related (in some prescribed manner) to Q, the flow rate, through the same 
!!  boundary.  To do this efficiently requires us to precompute the integral 
!!  of N_A over the boundary for each node A and store it in a vector of length
!!  nshg (a bit wasteful since only the nodes on the boundary will be non zero
!!  in this vector but it is probably slower to index it than to multiply and 
!!  add the extra zeros....check later).

      subroutine initNABI( x, shpb )
      
      use     pointer_data
      use     pvsQbi

      include "global.h"
      include "common_blocks/blkdat.h"
      include "common_blocks/conpar.h"
      include "common_blocks/elmpar.h"
      include "common_blocks/intpt.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/propar.h"
      include "common_blocks/shpdat.h"
C
C     Argument variables
C
      REAL*8                shpb
C
C     Local variables
C
      INTEGER             iblk,        iel
C      
      real*8   x(numnp,nsd)
c
c use is like
c 
c      NABI=pvsQbi -> NABI
c
        dimension   shpb(MAXTOP,maxsh,MAXQPT)
        real*8, allocatable :: tmpshpb(:,:)
        allocate ( NABI(nshg,3) ) 
        allocate ( NASC(nshg)   )
        allocate ( ndsurf(nshg) ) 

c
c....  calculate NABI
c
      NABI=zero
      NASC=zero
      ndsurf=0
        if (Lagrange .gt. 0) then
           allocate ( PNABI(nshg,3)  )
           allocate ( NANBIJ(nshg,3,3)  )
           PNABI = zero
           NANBIJ = zero
        endif
c
c.... -------------------->   boundary elements   <--------------------
c
c.... set up parameters
c
c        intrul = intg   (2,itseq)
c        intind = intptb (intrul)
c
c.... loop over the boundary elements
c
        do iblk = 1, nelblb
c
c.... set up the parameters
c
          iel    = lcblkb(1,iblk)
          lelCat = lcblkb(2,iblk)
          lcsyst = lcblkb(3,iblk)
          iorder = lcblkb(4,iblk)
          nenl   = lcblkb(5,iblk)  ! no. of vertices per element
          nenbl  = lcblkb(6,iblk)  ! no. of vertices per bdry. face
          nshl   = lcblkb(9,iblk)
          nshlb  = lcblkb(10,iblk)
          mattyp = lcblkb(7,iblk)
          ndofl  = lcblkb(8,iblk)
          npro   = lcblkb(1,iblk+1) - iel 


          if(lcsyst.eq.3) lcsyst=nenbl
c
          if(lcsyst.eq.3 .or. lcsyst.eq.4) then
             ngaussb = nintb(lcsyst)
          else
             ngaussb = nintb(lcsyst)
          endif
          
c
c.... compute and assemble the residuals corresponding to the 
c     boundary integral
c
          allocate (tmpshpb(nshl,MAXQPT))
          
          tmpshpb(1:nshl,:) = shpb(lcsyst,1:nshl,:)

          call AsBNABI (                       x,
     &                 tmpshpb,
     &                 mienb(iblk)%p,
     &                 miBCB(iblk)%p)

          call AsBNASC(                       x,
     &                 tmpshpb,
     &                 mienb(iblk)%p,
     &                 miBCB(iblk)%p)
          
          if (Lagrange .gt. 0) then 
             call AsBPNABI(             x,
     &                 tmpshpb,
     &                 mienb(iblk)%p,
     &                 miBCB(iblk)%p) 
          endif    

          deallocate (tmpshpb)

      enddo 

c
c     note that NABI has NOT been communicated.  It
C     is the on processor contribution to this vector.  It will used to
C     build the on processor contribution to res that will then be made
C     complete via a call to commu.  Similarly the LHS usage will create
C     the on-processor contribution to the lhsK. Same for NASC
c
      return
      end

!> This routine satisfies the periodic boundary conditions
!! on the diffusive flux residual and mass matrix
!!
!! input:<BR>
!! @param[in] rmass(nshg) Mass matrix
!! @param[in] qres(nshg,(nflow-1)*nsd) Diffusive flux vector
!! 
!! output:<BR>
!! @param[out] qres 
!! @param[out] rmass 
!!

      subroutine qpbc( rmass, qres, iBC, iper, ilwork )
c
      include "global.h"
      include "common_blocks/conpar.h"
      include "common_blocks/genpar.h"
      include "common_blocks/workfc.h"
      include "common_blocks/fronts.h"
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                qres,        rmass
C
C     Local variables
C
      INTEGER             i,           idflow,      istrt,       j
C
      REAL*8                tmp,         tmpvec,      uv
C      
      dimension rmass(nshg), qres(nshg,idflx),
     &          iBC(nshg), iper(nshg),uv(nshg,2),
     &          tmpvec(nshg,4), tmp(nshg),
     &          ilwork(nlwork)
c
      if(iabc==1) then   !are there any axisym bc's
      do i=1,idflx/nsd
         do j=1,2
            istrt=j+(i-1)*(nflow-1)
            uv(:,j)=qres(:,istrt)
         enddo
         call rotabc(uv, iBC, 'in ')
         do j=1,2
            istrt=j+(i-1)*(nflow-1)
            qres(:,istrt)=uv(:,j)
         enddo
      enddo
      endif
c
c
c.... compute qi for node A, i.e., qres <-- qres/rmass
c
       if (numpe > 1) then
          call commu (qres  , ilwork, idflx  , 'in ')
          call commu (rmass , ilwork,  1            , 'in ')
       endif
c
c  take care of periodic boundary conditions
c  but not on surface tension terms in qres(:,10-12)
c  that are used to compute normal vector
c
        idflow = (nflow-1)*nsd
        do j= 1,nshg
          if ((btest(iBC(j),10))) then
            i = iper(j)
            rmass(i) = rmass(i) + rmass(j)
c            qres(i,:) = qres(i,:) + qres(j,:)
            qres(i,1:idflow) = qres(i,1:idflow) + qres(j,1:idflow)
          endif
        enddo

        do j= 1,nshg
          if ((btest(iBC(j),10))) then
            i = iper(j)
            rmass(j) = rmass(i)
c            qres(j,:) = qres(i,:)
            qres(j,1:idflow) = qres(i,1:idflow)
          endif
        enddo
c
c.... invert the diagonal mass matrix and find q
c
        rmass = one/rmass
       
       do i=1, idflx
          qres(:,i) = rmass*qres(:,i)
       enddo
       if (isurf .eq. 1) then
          idflow=(nflow-1)*nsd
c
c.... calculation of the unit normal vector
c
           tmp =  sqrt(qres(:,idflow+1)**2
     &               + qres(:,idflow+2)**2
     &               + qres(:,idflow+3)**2)
           do i = 1, nshg
             if (tmp(i) .lt. 0.0001) tmp(i) = 0.0001
           end do
           tmp = one/tmp

          do i=1, nsd
             qres(:,idflow+i) = tmp*qres(:,idflow+i)
          enddo 
       endif

       if(numpe > 1) then
          call commu (qres, ilwork, idflx, 'out')    
       endif

       if(iabc==1) then         !are there any axisym bc's
c
c       slave has masters value, for abc we need to rotate it
c
          do i=1,idflx/nsd
             do j=1,2
                istrt=j+(i-1)*(nflow-1)
                uv(:,j)=qres(:,istrt)
             enddo
             call rotabc(uv, iBC, 'out')
             do j=1,2
                istrt=j+(i-1)*(nflow-1)
                qres(:,istrt)=uv(:,j)
             enddo
          enddo
       endif
       
c
c.... return
c    
        return
        end

!> This routine satisfies the periodic boundary conditions
!! on the diffusive flux residual and mass matrix
!!
!! input:<BR>
!! @param[in] rmass(nshg) Mass matrix
!! @param[in] qres(nshg, nsd) Diffusive flux vector
!! 
!! output:<BR>
!! @param[out] qres 
!! @param[out] rmass 
!!

      subroutine qpbcSclr( rmass, qres, iBC, iper, ilwork )
c
      include "global.h"
      include "common_blocks/conpar.h"
      include "common_blocks/genpar.h"
      include "common_blocks/workfc.h"
      include "common_blocks/fronts.h"
C
C     Argument variables
C
      INTEGER             ibc,         ilwork,      iper
C
      REAL*8                qres,        rmass
C
C     Local variables
C
      INTEGER             i,           j
C      
      dimension rmass(nshg), qres(nshg,nsd),
     &          iBC(nshg), iper(nshg),
     &          ilwork(nlwork)

      if(iabc==1) !are there any axisym bc's
     &         call rotabc(qres, iBC,  'in ')

c
c.... compute qi for node A, i.e., qres <-- qres/rmass
c
       if (numpe > 1) then
          call commu (qres  , ilwork, nsd  , 'in ')
          call commu (rmass , ilwork,  1   , 'in ')
       endif

c
c  take care of periodic boundary conditions
c
        do j= 1,nshg
          if (btest(iBC(j),10)) then
            i = iper(j)
            rmass(i) = rmass(i) + rmass(j)
            qres(i,:) = qres(i,:) + qres(j,:)
          endif
        enddo

        do j= 1,nshg
          if (btest(iBC(j),10)) then
            i = iper(j)
            rmass(j) = rmass(i)
            qres(j,:) = qres(i,:)
          endif
        enddo
c
c.... invert the diagonal mass matrix and find q
c
        rmass = one/rmass
       
       do i=1, nsd
          qres(:,i) = rmass*qres(:,i)
       enddo

       if(numpe > 1) then
          call commu (qres, ilwork, nsd, 'out')    
       endif

      if(iabc==1) !are there any axisym bc's
     &         call rotabc(qres, iBC, 'out')

c
c.... return
c    
        return
        end

!> This routine is the restart option.
!!
!! input:<BR>
!! @param[in] code Restart option on the primitive variables
!! - .eq. 'in  ', read from [restar.inp]
!! - .eq. 'out ', write to  [restar.out]
!!
!! input or output:<BR>
!! - q(nshg,ndof) The variables to be read/written

        subroutine restar (code,  q, ac)
c
        use readarrays          ! used to access qold, acold

        include "global.h"
        include "mpif.h"
        include "common_blocks/conpar.h"
        include "common_blocks/mio.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      REAL*8                ac,          q
C
C     Local variables
C
      INTEGER             iqoldsiz
C
        character*4 code
        character*8 mach2
        character*20 fname1,  fmt1
        character*5  cname
        integer ioerr

c
        dimension q(nshg,ndof),ac(nshg,ndof)
c
c.... -------------------------->  'in  '  <---------------------------
c
        if (code .eq. 'in  ') then
c
c Incompressible orders velocity, pressure, temperature unlike compressible
c which is what we have our files set up for
c
          q(:,1:3)=qold(:,2:4)
          q(:,4)=qold(:,1)
          if(ndof.gt.4)  q(:,5:ndof)=qold(:,5:ndof)

          ac(:,1:3)=acold(:,2:4)
          ac(:,4)=acold(:,1)
          if(ndof.gt.4)  ac(:,5:ndof)=acold(:,5:ndof)

          deallocate(qold)
          deallocate(acold)
          return
        endif
c
c.... -------------------------->  'out '  <---------------------------
c
        if (code .eq. 'out ') then

           allocate( qold(nshg,ndof) )
           allocate( acold(nshg,ndof) )
           acold=0
c
           qold(:,2:4) = q(:,1:3)
           qold(:,1)   = q(:,4)
           if(ndof.gt.4) qold(:,5:ndof)   = q(:,5:ndof)
c 
           acold(:,2:4) = ac(:,1:3)
           acold(:,1)   = ac(:,4)
           if(ndof.gt.4) acold(:,5:ndof)   = ac(:,5:ndof)
c
           iqoldsiz=nshg*ndof
           call write_restart(myrank, lstep, nshg, ndof, 
     &          qold, acold)

          if (myrank.eq.master) then 
            open(unit=72,file='numstart.dat',status='old',iostat=ioerr)
            if (ioerr.eq.0) then
              write(72,*) lstep
            else
              write(*,*) 'IO_ERROR file: numstart.dat code: ',ioerr
            endif
            close(72)
          endif
           deallocate(qold)
           deallocate(acold)
           return
        endif
c
c.... ---------------------->  Error Handling  <-----------------------
c
c.... Error handling
c
        call error ('restar  ',code//'    ',0)
c
c.... file error handling
c
995     call error ('restar  ','opening ', irstin)
996     call error ('restar  ','opening ', irstou)
c
c.... end
c
        end

!> This subroutine is responsible for rotating 
!! the residual and solution vectors for axisymmetric BC's.
!!
!! input:<BR>
!! @param[in] global(nshg,n) Global vector to be rotated.
!! @param[in] code 
!! - = 'in' for rotating with the residual
!! - = 'out' for rotating the solution 
!!
!! Note that the cos and sin of the rotation angles are preprocessed and
!! stored in acs(1 and 2) respectively.
!!

      subroutine rotabc (global, iBC, code)
c
      use specialBC  ! gives us acs, contains (:,1)=cos(theta) (:,2)=sin(theta)

      include "global.h"
      include "common_blocks/conpar.h"
c
c     Argument variables
c
      INTEGER             ibc
c
      REAL*8                global
c
c     Local variables
c
      REAL*8                 tmp
c 
      dimension global(nshg,2),             iBC(nshg),
     &          tmp(nshg)
 
      character*3 code

      if (code .eq. 'in ') then
         where( btest(iBC,10))
            tmp         =  global(:,1)*acs(:,1) - global(:,2)*acs(:,2)
            global(:,2) =  global(:,1)*acs(:,2) + global(:,2)*acs(:,1)
            global(:,1) = tmp
         endwhere
      else  if (code .eq. 'out') then
         where( btest(iBC,10))
            tmp         =  global(:,1)*acs(:,1) + global(:,2)*acs(:,2)
            global(:,2) = -global(:,1)*acs(:,2) + global(:,2)*acs(:,1)
            global(:,1) = tmp
         endwhere
      else 
         call error ('rotabc  ','code    ',0)
      endif

      return
      end

!> This subroutine calculates the statistics of the residual.
!!
!! input:<BR>
!! @param[in] res(nshg,nflow) Preconditioned residual
!!
!! output:<BR>
!! - The time step, cpu-time and entropy-norm of the residual
!! are printed in the file HISTOR.DAT.
!!

        subroutine rstatic (res, y, Dy)
c
        use ResidualControl 
        
        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/extrat.h"
        include "common_blocks/incomp.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/mio.h"
        include "common_blocks/mioname.h"
        include "common_blocks/newdim.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/resdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      REAL*8                res
C
C     Local variables
C
      INTEGER             i,           ierr,        intsec
      INTEGER             irecvcount,  jresmx,      jtotrs
      INTEGER             mproc,       nrsmax
C
      REAL*8                cputme
      REAL*8                rdy1,        rdy2,        rdy4
      REAL*8                rdy5,        resmax,      resmaxl,   resnrm
      REAL*8                resvec,      rmaxdyp,     rmaxdyu,   rtmp
      REAL*8                rvec,        totres
C
        dimension res(nshg,nflow),    mproc(1),  rvec(numpe)
        dimension rtmp(nshg),        nrsmax(1)
        dimension irecvcount(numpe), resvec(numpe)

        real*8    y(nshg,ndof),    Dy(nshg,4)
        integer tmrc
        integer ioerr
c
c.... compute max delta y
c
        rdy1 = zero
        rdy2 = zero
        rdy4 = zero
        rdy5 = zero
        call sumgatN( abs(gami*Delt(itseq) 
     &                * Dy(1:numnp,1:3)),3,rdy1, numnp)
        call sumgatN( abs( y(1:numnp,1:3)),3,rdy2,numnp)
        call sumgatN( abs(gami*alfi*Delt(itseq)
     &                * Dy(1:numnp,4)),1,rdy4,numnp)
        call sumgatN( abs( y(1:numnp,4)),  1,rdy5,numnp)
        rmaxdyU = rdy1/rdy2
        rmaxdyP = rdy4/rdy5
      
c
c..... Signal to quit if delta is very small. look in itrdrv.f for the
c      completion of the hack.
c
        if( rmaxdyU .lt. dtol(1) .and. rmaxdyP .lt. dtol(2)) then
           istop = 1000
        endif

        if (numpe == 1) nshgt=nshg   ! global = this processor
c
c.... ----------------------->  Convergence  <-------------------------
c
c.... compute the maximum residual and the corresponding node number
c
        rtmp = zero
        if (impl(itseq) .ge. 9) then
          do i = 1, nflow
            rtmp = rtmp + res(:,i)**2    ! only add continuity and momentum
          enddo
        endif

        call sumgat (rtmp, 1, resnrm)
        
        resmaxl = maxval(rtmp)

        irecvcount = 1
        resvec = resmaxl
        if (numpe > 1) then
           call MPI_REDUCE_SCATTER (resvec, resmax, irecvcount,
     &          MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
           if (resmax .eq. resvec(1) ) then
              mproc(1) = myrank
              nrsmax   = maxloc(rtmp)
           else
              nrsmax(1) = -1
              mproc(1)  = -1
           endif
c          BUG FIX - DES - 30JAN2014
           resvec = nrsmax(1)
           call MPI_REDUCE_SCATTER (resvec, rvec, irecvcount,
     &          MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
           nrsmax = rvec(1)
c          BUG FIX - DES - 30JAN2014
           resvec = mproc(1)
           call MPI_REDUCE_SCATTER (resvec, rvec, irecvcount,
     &          MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, ierr)
           mproc = rvec(1)
      else
          resmax   = resmaxl
          nrsmax   = maxloc(rtmp)
          mproc(1) = 0
      endif
c
c.... correct the residuals
c
        if (loctim(itseq) .eq. 0) then
          resnrm = resnrm 
          resmax = resmax
        else
          resnrm = resnrm
          resmax = resmax
        endif
c
c.... approximate the number of entries
c
        totres = resnrm / float(nshgt)
        totres = sqrt(totres)
        resmax = sqrt(resmax)
        if (resfrt .eq. zero) resfrt = totres
        jtotrs = int  ( 10.d0 * log10 ( totres / resfrt ) )
        jresmx = int  ( 10.d0 * log10 ( resmax / totres ) )
        
        if(rescontrol .gt. 0) then
           controlResidual = totres
           CurrentIter = CurrentIter + 1
        endif
c     
c.... get the CPU-time
c
CAD        cputme = (second(0) - ttim(100))
        intsec=TMRC()
        cputme = (intsec - ttim(100))
c
c.... output the result
c
        if (numpe > 1) call MPI_BARRIER (MPI_COMM_WORLD, ierr)
        
        if (myrank .eq. master) then
c
c.... results of continuity and momentum 
c
           print 2000, lstep+1, cputme, totres, jtotrs, rmaxdyU,
     &          rmaxdyP,nrsmax,
     &          mproc(1)+1, jresmx, int(statsflow(4)),
     &          int(statsflow(1))
           open (unit=ihist, file=fhist, status='unknown',
     &            access='append',iostat=ioerr)
           if (ioerr.eq.0) then
             write (ihist,2000) lstep+1, cputme, totres, jtotrs, 
     &          rmaxdyU, rmaxdyP, nrsmax,
     &          mproc(1)+1,jresmx,int(statsflow(4)),
     &          int(statsflow(1))
           else
             write(*,*) 'IO_ERROR file: ',fhist,' code: ',ioerr
           endif
           close (ihist)
        endif
        if(numpe>1) call MPI_BARRIER (MPI_COMM_WORLD,ierr)
c
c.... return
c
        return
c
 1000   format(1p,i6,5e13.5)
 2000   format(1p,i6,e10.3,e10.3,2x,'(',i4,')',2x,e10.3,2x,e10.3,
     &       2x,'<',i6,'-',i2,'|',
     &       i4,'>', ' [', i4,' -',i4,']')
 3000   format(1p,i6,e10.3,e10.3,3x,'(',i4,')',3x,'<',i6,'-',i2,'|',
     &       i4,'>', ' [', i4,' -',i4,' -',i4,']')

c
        end

!> This subroutine calculates the statistics of the residual

        subroutine rstaticSclr (res, y, Dy, icomp)
c
        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/extrat.h"
        include "common_blocks/incomp.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/matdat.h"
        include "common_blocks/mio.h"
        include "common_blocks/mioname.h"
        include "common_blocks/newdim.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
C
C     Argument variables
C
      INTEGER             icomp
C
      REAL*8                res
C
C     Local variables
C
      INTEGER             intsec,      ioerr
C
      REAL*8                cputme,      rdy1,        rdy2
      REAL*8                resnrm,      rmaxdyt,     rtmp,     totres
C
        dimension res(nshg)
        dimension rtmp(nshg)
        real*8    y(nshg,ndof),    Dy(nshg), nrm
        integer tmrc
c
c.... compute max delta y
c
        rdy1 = zero
        rdy2 = zero
c
c.... normalize turbulence with molecular viscosity
c        
        nrm = zero
        call sumgat( abs(gami*Delt(itseq)*Dy(:)),1,rdy1)
        call sumgat( abs( y(:,icomp)),1,rdy2)
        rmaxdyT = rdy1/(rdy2+nrm)
c
c.... compute the maximum residual and the corresponding node number
c
        rtmp = zero
        rtmp = rtmp + res**2 ! add temperature also
        call sumgat (rtmp, 1, resnrm)

        if (numpe == 1) nshgt=nshg ! global = this processor

        totres = resnrm / float(nshgt)
        totres = sqrt(totres)

c        if (mod(impl(1),100)/10 .eq. 0) then  !not solving flow
           if (myrank .eq. master) then
c     
c.... get the CPU-time
c
              intsec=TMRC()
              cputme = (intsec - ttim(100))

           print 802, lstep+1, cputme, totres, rmaxdyT,
     &                int(statssclr(1))
           open (unit=ihist, file=fhist, status='unknown',iostat=ioerr)
           if (ioerr.eq.0) then
             write (ihist,802) lstep+1, cputme, totres, 
     &          rmaxdyT,int(statssclr(1))
           else
             write(*,*) 'IO_ERROR file: ',fhist,' code: ',ioerr
           endif
           close(ihist)
           endif
c        else 
c           if (myrank .eq. master) then
c              print 803, totres, rmaxdyT, int(statssclr(1))
c              write(ihist,803) totres, rmaxdyT, int(statssclr(1))
c           endif
c        endif

        return
        
 802    format(1p,i6,e10.3,e10.3,10X,e10.3,31X'[',i6,']')
 803    format(1p,16x,e10.3,10x,e10.3,31X,'[',i10,']')
    
        end

!> This is the 2nd interface routine to the linear equation 
!! solver library that uses the CGP and GMRES methods.
!!
!! input:<BR>
!! @param[in] y(nshg,ndof) Y-variables at n+alpha_f
!! @param[in] ac(nshg,ndof) Primvar. accel. variable n+alpha_m
!! @param[in] yold(nshg,ndof) Y-variables at beginning of step
!! @param[in] acold(nshg,ndof) Primvar. accel. at beginning of step
!! @param[in] x(numnp,nsd) Node coordinates
!! @param[in] iBC(nshg) BC codes
!! @param[in] BC(nshg,ndofBC) BC constraint parameters
!! @param[in] iper(nshg) Periodic nodal information
!!
!! output:<BR>
!! @param[out] res(nshg,nflow) Preconditioned residual
!! @param[out] y(nshg,ndof) Y-variables at n+alpha_f
!! @param[out] ac(nshg,ndof) Primvar. accel. variable n+alpha_m
!!
!! The followings are preliminary steps required to use the
!! solver library.  New way of writing has to be used such as
!!
!!          |  K     G | | du |    | Rmom  |
!!          |          | |    | =  |       |
!!          | G^t    C | | dp |    | Rcon  |
!!
!!          |     E    | | dT | =  | Rtemp |
!!
!! where:<BR>
!!
!! xKebe:<BR> 
!! - K_ab = dRmom_a/du_b    
!! xTe:<BR> 
!! - E_ab = dRtemp_a/dT_b 
!! - G_ab = dRmom_a/dp_b
!! xGoC:
!! - C_ab = dRcon_a/dp_b       
!! - resf = Rmon Rcon       
!! - rest = Rtemp
!!

      subroutine SolFlow(y,          ac,         u,
     &                   yold,       acold,      uold,
     &                   x,          iBC,
     &                   BC,         res,             
     &                   nPermDims,  nTmpDims,  aperm,
     &                   atemp,      iper,       
     &                   ilwork,     shp,        shgl, 
     &                   shpb,       shglb,      rowp,     
     &                   colm,       lhsK,       lhsP, 
     &                   solinc,     rerr,       sumtime,
     &                   svLS_lhs,  svLS_ls,   svLS_nFaces)
      use pointer_data
      use LagrangeMultipliers 
c        
        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "svLS.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/incomp.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/workfc.h"
        include "common_blocks/timdat.h"

C
C     Argument variables
C
      INTEGER            npermdims
      INTEGER             ntmpdims
C
C     Local variables
C
      INTEGER              lesid
C
      REAL*8                rdtmp
C    
      TYPE(svLS_lhsType) svLS_lhs
      TYPE(svLS_lsType) svLS_ls
      
      real*8    y(nshg,ndof),             ac(nshg,ndof),
     &          yold(nshg,ndof),          acold(nshg,ndof),
     &          u(nshg,nsd),              uold(nshg,nsd),
     &          x(numnp,nsd),             BC(nshg,ndofBC),
     &          res(nshg,nflow),
     &          flowDiag(nshg,4),
     &          aperm(nshg,nPermDims),    atemp(nshg,nTmpDims),
     &          sclrDiag(nshg,1),         
     &          lhsK(9,nnz_tot),          lhsP(4,nnz_tot)          
c
      real*8    shp(MAXTOP,maxsh,MAXQPT),  
     &          shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &          shpb(MAXTOP,maxsh,MAXQPT),
     &          shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
      integer   usr(100),                 eqnType,
     &          rowp(nshg*nnz),           colm(nshg+1),
     &          iBC(nshg),                ilwork(nlwork),
     &          iper(nshg) 
c
      real*8    yAlpha(nshg,ndof),        acAlpha(nshg,ndof),
     &          uAlpha(nshg,nsd),         
     &          lesP(nshg,4),             lesQ(nshg,4),
     &          solinc(nshg,ndof)
      
      real*8    rerr(nshg,10),            rtmp(nshg,4)
      REAL*8 sumtime
      INTEGER dof, svLS_nFaces, i, j, k, l
      INTEGER, ALLOCATABLE :: incL(:)
      REAL*8, ALLOCATABLE :: faceRes(:), Res4(:,:), Val4(:,:)
c
c.... *******************>> Element Data Formation <<******************
c
c
c.... set the parameters for flux and surface tension calculations
c
c
      idflx = 0 
      if(idiff >= 1 )  idflx= (nflow-1) * nsd
      if (isurf == 1) idflx=nflow*nsd
c        
c.... compute solution at n+alpha
c
      call itrYAlpha( uold,    yold,    acold,       
     &                u,       y,       ac,            
     &                uAlpha,  yAlpha,  acAlpha)

c
c.... form the LHS matrices, the residual vector (at alpha)
c
      call ElmGMR (uAlpha,    yAlpha,     acAlpha,    x,
     &             shp,       shgl,       iBC,       
     &             BC,        shpb,       shglb,
     &             res,       iper,       ilwork,   
     &             rowp,      colm,       lhsK,      
     &             lhsP,      rerr)


      IF (svLSFlag .EQ. 1) THEN

c####################################################################
!     Here calling svLS

      ALLOCATE(faceRes(svLS_nFaces), incL(svLS_nFaces))
      CALL AddElmpvsQForsvLS(faceRes, svLS_nFaces)

      incL = 1
      dof = 4
      IF (.NOT.ALLOCATED(Res4)) THEN
         ALLOCATE (Res4(dof,nshg), Val4(dof*dof,nnz_tot))
      END IF

      DO i=1, nshg
         Res4(1:dof,i) = res(i,1:dof)
      END DO

      DO i=1, nnz_tot
         Val4(1:3,i)   = lhsK(1:3,i)
         Val4(5:7,i)   = lhsK(4:6,i)
         Val4(9:11,i)  = lhsK(7:9,i)
         Val4(13:15,i) = lhsP(1:3,i)
         Val4(16,i)    = lhsP(4,i)
      END DO

      !Val4(4:12:4,:) = -lhsP(1:3,:)^t
      DO i=1, nshg
         Do j=colm(i), colm(i+1) - 1
            k = rowp(j)
            DO l=colm(k), colm(k+1) - 1
               IF (rowp(l) .EQ. i) THEN
                  Val4(4:12:4,l) = -lhsP(1:3,j)
                  EXIT
               END IF
            END DO
         END DO
      END DO
      CALL svLS_SOLVE(svLS_lhs, svLS_ls, dof, Res4, Val4, incL, 
     2   faceRes)
      
      DO i=1, nshg
         solinc(i,1:dof) = Res4(1:dof,i)
      END DO
 
c####################################################################
      ELSE
c
c.... lesSolve : main matrix solver
c
      lesId   = numeqns(1)
      eqnType = 1
c
c.... setup the linear algebra solver
c
      rtmp = res(:,1:4)
      call usrNew ( usr,        eqnType,          aperm,
     &              atemp,      rtmp,             solinc,          
     &              flowDiag,   sclrDiag,         lesP,   
     &              lesQ,       iBC,              BC,
     &              iper,       ilwork,           numpe,
     &              nshg,       nshl,             nPermDims,  
     &              nTmpDims,   rowp,             colm,     
     &              lhsK,       lhsP,             rdtmp,      
     &              nnz_tot )
c
c.... solve linear system
c
      call myfLesSolve ( lesId, usr )
      call getSol ( usr, solinc )

      if (numpe > 1) then
         call commu ( solinc, ilwork, nflow, 'out')
      endif

      if(Lagrange .gt. zero) then
         call CalcNANBLagrange(colm, rowp, solinc(:,1:3))
         call LagMultiplyMatrix(solinc, 0, nsrflistLagrange,
     &      numLagrangeSrfs)  
         Lagincr(:,1:3) = (- resL(:,1:3) - AddLag(:,1:3) )
     &      /ScaleFactor(1,1)/alfi/gami/two
      endif

      END IF

      call rstatic (res, y, solinc) ! output flow stats
c     
c.... end
c     
      return
      end

!> This is the 2nd interface routine to the linear equation 
!! solver library.
!!
!! input:<BR>
!! @param[in] y(nshg,ndof) Y-variables at n+alpha_f
!! @param[in] ac(nshg,ndof) Primvar. accel. variable n+alpha_m
!! @param[in] yold(nshg,ndof) Y-variables at beginning of step
!! @param[in] x(numnp,nsd) Node coordinates
!! @param[in] iBC(nshg) BC codes
!! @param[in] BC(nshg,ndofBC) BC constraint parameters
!! @param[in] iper(nshg) Periodic nodal information
!!
!! output:<BR>
!! @param[out] y(nshg,ndof) Y-variables at n+alpha_f
!! @param[out] ac(nshg,ndof) Primvar. accel. variable n+alpha_m
!!
!! The followings are preliminary steps required to use LesLib
!! solver library.  New way of writing has to be used such as
!!
!!          |     E    | | dS | =  | RScal |

      subroutine SolSclr(y,          ac,         u,
     &                   yold,       acold,      uold,
     &                   x,          iBC,
     &                   BC,         nPermDimsS,  nTmpDimsS,  
     &                   apermS,     atempS,     iper,       
     &                   ilwork,     shp,        shgl, 
     &                   shpb,       shglb,      rowp,     
     &                   colm,       lhsS,       solinc)
c
      use pointer_data
        
        include "global.h"
        include "mpif.h"
        include "auxmpi.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/incomp.h"
        include "common_blocks/sclrs.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/workfc.h"

C
C     Argument variables
C
      INTEGER             npermdimss,  ntmpdimss
C
C     Local variables
C
      INTEGER             lesid,       nsolsc
C
      REAL*8                rlhsk,       rlhsp
C   
      real*8    y(nshg,ndof),             ac(nshg,ndof),
     &          yold(nshg,ndof),          acold(nshg,ndof),
     &          u(nshg,nsd),              uold(nshg,nsd),
     &          x(numnp,nsd),             BC(nshg,ndofBC),
     &          res(nshg,1),
     &          flowDiag(nshg,4),
     &          sclrDiag(nshg,1),           lhsS(nnz_tot),
     &          apermS(nshg,nPermDimsS),  atempS(nshg,nTmpDimsS)

c
      real*8    shp(MAXTOP,maxsh,MAXQPT),  
     &          shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &          shpb(MAXTOP,maxsh,MAXQPT),
     &          shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
      integer   usr(100),                 eqnType,
     &          rowp(nshg*nnz),           colm(nshg+1),
     &          iBC(nshg),                ilwork(nlwork),
     &          iper(nshg)
c
      real*8    yAlpha(nshg,ndof),        acAlpha(nshg,ndof),
     &          uAlpha(nshg,nsd),
     &          lesP(nshg,1),               lesQ(nshg,1),
     &          solinc(nshg,1)
      
c     
c.... *******************>> Element Data Formation <<******************
c
c.... compute solution at n+alpha
c
      call itrYAlpha( uold,    yold,    acold, 
     &                u,       y,       ac,  
     &                uAlpha,  yAlpha,  acAlpha)
c
c.... form the LHS matrices, the residual vector (at alpha)
c
      call ElmGMRSclr(yAlpha,acAlpha,    x,
     &             shp,       shgl,       iBC,       
     &             BC,        shpb,       shglb,
     &             res,       iper,       ilwork,   
     &             rowp,      colm,       lhsS   )

c
c.... lesSolve : main matrix solver
c
      lesId   = numeqns(1+nsolt+isclr)
      eqnType = 2
c
c.... setup the linear algebra solver
c
      call usrNew ( usr,        eqnType,          apermS,
     &              atempS,     res,              solinc,          
     &              flowDiag,   sclrDiag,         lesP,   
     &              lesQ,       iBC,              BC,
     &              iper,       ilwork,           numpe,
     &              nshg,       nshl,             nPermDimsS,  
     &              nTmpDimsS,  rowp,             colm,     
     &              rlhsK,      rlhsP,            lhsS,      
     &              nnz_tot )
c
c.... solve linear system
c
      call myfLesSolve ( lesId, usr )
      call getSol ( usr, solinc )

      if (numpe > 1) then
         call commu ( solinc, ilwork, 1, 'out')
      endif
      
      nsolsc=5+isclr
      call rstaticSclr (res, y, solinc, nsolsc) ! output scalar stats
c     
c.... end
c     
      return
      end

!> Create the new statistics arrays
      
      subroutine initStats(x,   iBC,    iper,   ilwork)
c      
      use stats

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/timdat.h"

      real*8  x(numnp,3)
      integer ilwork(nlwork), iper(nshg), iBC(nshg)
      
      if (ipord .eq. 1) then
         stsType      = 1
      else
         stsType      = 2
      endif
      
      stsWriteFreq = 200
      
      nResDims = 11
      nSolDims = 10
      nLhsDims = 19
      
      allocate ( stsVec(nshg,nResDims) )

      if (stsType .eq. 1) then
         allocate ( stsReg(nshg)          )
         allocate ( stsMInv(nshg,6)       )
         allocate ( stsB(nshg,3)          )
         allocate ( stsDInv(nshg,3)       )
         allocate ( stsCInv(nshg,6)       )
      endif

      allocate ( stsPres(nshg)         )
      allocate ( stsPresSqr(nshg)      )
      allocate ( stsVel(nshg,3)        )
      allocate ( stsVelSqr(nshg,6)     )
      allocate ( stsVelReg(nshg,3)     )
      allocate ( stsStress(nshg,6)     )
      
      stsPres    = 0.0
      stsPresSqr = 0.0
      stsVel     = 0.0
      stsVelSqr  = 0.0
      stsVelReg  = 0.0
      stsStress  = 0.0

      step1      = lstep+1
      nTimeStep  = 0
      stsResFlg  = 0

      if (stsType .eq. 1) then
         call elmStatsLhs( x,   iBC,   iper,   ilwork )
         call stsInitLhs(  nshg )
      endif

      return
      end
      
!> Compute the Lhs matrices needed for the conservative projection
!! of the statistics

      subroutine stsInitLhs(nshg)

      use     stats
      integer nshg

      real*8  res(nResDims), reg, det, r0, r1, r2, r3, r4, r5,
     &        d0, d1, d2, c0, c1, c2, c3, c4, c5
      integer i
      
c
c.... build the regularization
c      
      do i = 1, nshg
         res = stsVec(i,:)
         reg = res(1) * res(2) * res(3)
         det = res(1) * (res(2) * res(3) - res(5) * res(5))
     &       + res(4) * (res(5) * res(6) - res(4) * res(3))
     &       + res(6) * (res(4) * res(5) - res(2) * res(6)) 
      
         if ( det .gt. 1.d-10*reg .and. reg .ne. 0 ) then
          stsReg(i) = 0 
         else 
          stsReg(i) = (res(1) + res(2) + res(3)) / 1000. 
         endif
      enddo
      
c
c.... form M and factorize
c
      do i = 1, nshg
         res   = stsVec(i,:)
         reg   = stsReg(i)
         r0    = res(1) + reg
         r1    = res(2) + reg         
         r2    = res(3) + reg         
         r3    = res(4)
         r4    = res(5)
         r5    = res(6)
         
         det   = r0 * (r1 * r2 - r4 * r4)
     &         + r3 * (r4 * r5 - r3 * r2)
     &         + r5 * (r3 * r4 - r1 * r5)
         det   = 1.0/det
         
         stsMInv(i,1) = det * (r1 * r2 - r4 * r4)
         stsMInv(i,2) = det * (r0 * r2 - r5 * r5)
         stsMInv(i,3) = det * (r0 * r1 - r3 * r3)
         stsMInv(i,4) = det * (r4 * r5 - r2 * r3)
         stsMInv(i,5) = det * (r3 * r5 - r0 * r4)
         stsMInv(i,6) = det * (r3 * r4 - r1 * r5)
      enddo

c
c.... form B, DInv and CInv      
c
      do i = 1, nshg
      res          = stsVec(i,:)
      reg          = stsReg(i) 
      r0           = res(1) 
      r1           = res(2) 
      r2           = res(3) 
      r3           = res(4) 
      r4           = res(5) 
      r5           = res(6) 
      d0           = 1. / ( reg + r0 ) 
      d1           = 1. / ( reg + r1 ) 
      d2           = 1. / ( reg + r2 ) 
      stsDInv(i,1) = d0 
      stsDInv(i,2) = d1 
      stsDInv(i,3) = d2 
      stsB(i,1)    = r3 
      stsB(i,2)    = r4 
      stsB(i,3)    = r5 
      c0           = r0 + r1 - r3 * r3 * (d0 + d1) + reg 
      c1           = r1 + r2 - r4 * r4 * (d1 + d2) + reg 
      c2           = r2 + r0 - r5 * r5 * (d2 + d0) + reg 
      c3           = r5      - r3 * r4 * d1 
      c4           = r3      - r4 * r5 * d2 
      c5           = r4      - r5 * r3 * d0 
      det          = c0 * (c1 * c2 - c4 * c4)
     &               + c3 * (c4 * c5 - c3 * c2)
     &               + c5 * (c3 * c4 - c1 * c5) 
      det          = 1. / det 
      stsCInv(i,1) = det * (c1 * c2 - c4 * c4) 
      stsCInv(i,2) = det * (c0 * c2 - c5 * c5) 
      stsCInv(i,3) = det * (c0 * c1 - c3 * c3) 
      stsCInv(i,4) = det * (c4 * c5 - c2 * c3) 
      stsCInv(i,5) = det * (c3 * c5 - c0 * c4) 
      stsCInv(i,6) = det * (c3 * c4 - c1 * c5) 
      enddo
      
      return
      end
               
!> Collect the desired statistics 

      subroutine stsGetStats( y,      yold,   ac,     acold, 
     &                        u,      uold,   x,
     &                        shp,    shgl,   shpb,   shglb,
     &                        iBC,    BC,     iper,   ilwork,
     &                        rowp,   colm,   lhsK,   lhsP )
      
      use     stats

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
C
C     Local variables
      REAL*8                reg   
C   
      real*8  y(nshg,ndof),             yold(nshg,ndof),
     &        ac(nshg,ndof),            acold(nshg,ndof),
     &        u(nshg,nsd),              uold(nshg,nsd),
     &        shp(MAXTOP,maxsh,MAXQPT),  shgl(MAXTOP,nsd,maxsh,MAXQPT),
     &        shpb(MAXTOP,maxsh,MAXQPT),
     &        shglb(MAXTOP,nsd,maxsh,MAXQPT),
     &        BC(nshg,ndofBC),          lhsK(9,nnz_tot),
     &        lhsP(4,nnz_tot),         x(numnp,nsd)

      integer iBC(nshg),                iper(nshg),
     &        ilwork(nlwork),           rowp(nshg*nnz),
     &        colm(nshg+1)
      
      
      real*8  yAlpha(nshg,ndof),      acAlpha(nshg,ndof),
     &        uAlpha(nshg,nsd),
     &        res(nResDims),          MInv(6),
     &        DInv(3),                B(3),
     &        CInv(6)
      
      real*8 u1, u2, u3, r0, r1, r2, r3, r4, r5, t3, t4, t5

      integer i
      
      nTimeStep = nTimeStep + 1
c
c.... compute solution at n+alpha
c
      call itrYAlpha( uold,     yold,     acold,
     &                u,        y,        ac,  
     &                uAlpha,   yAlpha,   acAlpha)
      
c
c.... assemble the residual
c
      if (stsType .eq. 1) then
         call elmStatsRes( yAlpha,   acAlpha,     x,       shp,   shgl, 
     &                     shpb,     shglb,       iBC,     BC, 
     &                     iper,     ilwork,      rowp,    colm,
     &                     lhsK,     lhsP  )

c
c.... compute the statistics
c
         do i = 1, nshg
            res   = stsVec(i,:)
            reg   = stsReg(i)
      
            MInv  = stsMInv(i,:)
            DInv  = stsDInv(i,:)
            B     = stsB(i,:)
            CInv  = stsCInv(i,:)
            
            u1    = yAlpha(i,1)
            u2    = yAlpha(i,2)
            u3    = yAlpha(i,3)
            
            stsPres(i)    = stsPres(i)    +  y(i,4) 
            stsPresSqr(i) = stsPresSqr(i) +  y(i,4)*y(i,4)  

            r0            = res(1) + reg * u1 
            r1            = res(2) + reg * u2 
            r2            = res(3) + reg * u3 
         
            stsVel(i,1)   = stsVel(i,1) 
     &                    + MInv(1) * r0 + MInv(4) * r1 + MInv(6) * r2 
            stsVel(i,2)   = stsVel(i,2)
     &                    + MInv(4) * r0 + MInv(2) * r1 + MInv(5) * r2 
            stsVel(i,3)   = stsVel(i,3)
     &                    + MInv(6) * r0 + MInv(5) * r1 + MInv(3) * r2 
            
            stsVelReg(i,1) = stsVelReg(i,1) + u1 
            stsVelReg(i,2) = stsVelReg(i,2) + u2 
            stsVelReg(i,3) = stsVelReg(i,3) + u3 
            
            r0          = res(1) * u1               + reg * u1 * u1 
            r1          = res(2) * u2               + reg * u2 * u2 
            r2          = res(3) * u3               + reg * u3 * u3 
            r3          = res(1) * u2 + res(2) * u1 + reg * u2 * u1 
            r4          = res(2) * u3 + res(3) * u2 + reg * u3 * u2 
            r5          = res(3) * u1 + res(1) * u3 + reg * u1 * u3 
            r0          = DInv(1) * r0 
            r1          = DInv(2) * r1 
            r2          = DInv(3) * r2 
            r3          = r3 - B(1) * (r0 + r1) 
            r4          = r4 - B(2) * (r1 + r2) 
            r5          = r5 - B(3) * (r2 + r0) 
            t3          = CInv(1) * r3 + CInv(4) * r4 + CInv(6) * r5 
            t4          = CInv(4) * r3 + CInv(2) * r4 + CInv(5) * r5 
            t5          = CInv(6) * r3 + CInv(5) * r4 + CInv(3) * r5 
            
            stsVelSqr(i,1) = stsVelSqr(i,1)  
     &                  + r0 - DInv(1) * (B(1) * t3 + B(3) * t5) 
            stsVelSqr(i,2) = stsVelSqr(i,2)  
     &                  + r1 - DInv(2) * (B(2) * t4 + B(1) * t3) 
            stsVelSqr(i,3) = stsVelSqr(i,3)  
     &                  + r2 - DInv(3) * (B(3) * t5 + B(2) * t4) 

            stsVelSqr(i,4) = stsVelSqr(i,4) + t3 
            stsVelSqr(i,5) = stsVelSqr(i,5) + t4 
            stsVelSqr(i,6) = stsVelSqr(i,6) + t5 

            r0           = res(4) 
            r1           = res(5) 
            r2           = res(6) 
            r3           = res(7) 
            r4           = res(8) 
            r5           = res(9) 
            
            r0          = DInv(1) * r0 
            r1          = DInv(2) * r1 
            r2          = DInv(3) * r2 

            r3          = r3 - B(1) * (r0 + r1) 
            r4          = r4 - B(2) * (r1 + r2) 
            r5          = r5 - B(3) * (r2 + r0) 

            t3          = CInv(1) * r3 + CInv(4) * r4 + CInv(6) * r5 
            t4          = CInv(4) * r3 + CInv(2) * r4 + CInv(5) * r5 
            t5          = CInv(6) * r3 + CInv(5) * r4 + CInv(3) * r5 

            stsStress(i,1) = stsStress(i,1)
     &                  + r0 - DInv(1) * (B(1) * t3 + B(3) * t5) 
            stsStress(i,2) = stsStress(i,2)
     &                  + r1 - DInv(2) * (B(2) * t4 + B(1) * t3) 
            stsStress(i,3) = stsStress(i,3)
     &                  + r2 - DInv(3) * (B(3) * t5 + B(2) * t4) 
            stsStress(i,4) = stsStress(i,4) + t3 
            stsStress(i,5) = stsStress(i,5) + t4 
            stsStress(i,6) = stsStress(i,6) + t5 
         enddo
      else if (stsType .eq. 2) then
         
         call evalAtInterp( yAlpha,     stsVec,         x, 
     &                      nResDims,   nshape)
         
         do i = 1, nshg
            
            u1    = stsVec(i,1)
            u2    = stsVec(i,2)
            u3    = stsVec(i,3)

            stsPres(i)    = stsPres(i)    +  stsVec(i,4) 
            stsPresSqr(i) = stsPresSqr(i) +  stsVec(i,4)*stsVec(i,4)  
            
            stsVel(i,1) = stsVel(i,1) + u1 
            stsVel(i,2) = stsVel(i,2) + u2 
            stsVel(i,3) = stsVel(i,3) + u3 

            stsVelSqr(i,1) = stsVelSqr(i,1) + u1*u1
            stsVelSqr(i,2) = stsVelSqr(i,2) + u2*u2
            stsVelSqr(i,3) = stsVelSqr(i,3) + u3*u3
            stsVelSqr(i,4) = stsVelSqr(i,4) + u1*u2
            stsVelSqr(i,5) = stsVelSqr(i,5) + u2*u3
            stsVelSqr(i,6) = stsVelSqr(i,6) + u3*u1
            
            stsStress(i,1) = stsStress(i,1) + stsVec(i,6)
            stsStress(i,2) = stsStress(i,2) + stsVec(i,7)
            stsStress(i,3) = stsStress(i,3) + stsVec(i,8)
            stsStress(i,4) = stsStress(i,4) + stsVec(i,9)
            stsStress(i,5) = stsStress(i,5) + stsVec(i,10)
            stsStress(i,6) = stsStress(i,6) + stsVec(i,11)

         enddo
      endif
      
      if ( mod(nTimeStep,stsWriteFreq) .eq. 0 .or. 
     &     nTimeStep .eq. nstep(itseq) ) then
         call stsWriteStats()
      endif
      
      return
      end
         
!> Collect the desired statistics 

      subroutine stsWriteStats()
      
      use     stats

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"


      integer      iofile, step2, itmp1, itmp,iofile2
      character*30 fname,  fmt1
      character*5  cname
      character*1  dash
      real*8       outvec(nshg,19)
c
c.... open the output file
c
      iofile = 39
      step2 = lstep+1  ! current time step
      itmp  = 1
      itmp1 = 1
      if (step1 .gt. 0) itmp  = int(log10(float(step1)))+1
      if (step2 .gt. 0) itmp1 = int(log10(float(step2)))+1
      dash = '-'
      write (fmt1,
     &     "('(''stats.'',i',i1,',a1,i',i1,',1x)')")
     &     itmp,itmp1
      write (fname,fmt1) step1,dash,step2
      fname = trim(fname) // cname(myrank+1)
      open ( unit = iofile, file = fname, status = 'unknown',
     &       form = 'unformatted')
c
c.... write the statistics
c
      outvec(:,1)     = stsPres(:)
      outvec(:,2:4)   = stsVel(:,:)
c      outvec(:,2:4)   = stsVelReg(:,:)
      outvec(:,5)     = zero   ! later will be temperature
      outvec(:,6)     = stsPresSqr(:)
      outvec(:,7:12)  = stsVelSqr(:,:)
      outvec(:,13)    = zero   ! later wil be tempSqr
      outvec(:,14:19) = stsStress(:,:)
      
      write (iofile) numnp, nshg, nTimeStep
      write (iofile) outvec(1:nshg,:)
      close (iofile)

      iofile2 = 40

 111  format(1p,3e24.16)
 112  format(1p, e24.16)
 113  format(1p,6e24.16)
      
      return
      end

!> This routine computes and assembles the data corresponding to the
!! boundary elements for the temperature equation

      subroutine AsBSclr (y,       x,       shpb,    shglb,
     &                   ienb,    materb,  iBCB,    BCB,
     &                   res)
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"

c........................Declarations....................................
       INTEGER ienb,materb,iBCB
       REAL*8 shglb,shpb,x,y,BCB,res,rl,sgn,yl,xlb      
c................................................................      
 
        dimension y(nshg,ndofl),           x(numnp,nsd),
     &            shpb(nshl,*),
     &            shglb(nsd,nshl,*),         
     &            ienb(npro,nshl),         materb(npro),
     &            iBCB(npro,ndiBCB),       BCB(npro,nshlb,ndBCB),
     &            res(nshg)         
c
        dimension yl(npro,nshl,ndofl),     xlb(npro,nenl,nsd),
     &            rl(npro,nshl),     sgn(npro,nshl)
        real*8 dwl(npro,nshl)
c
c.... get the matrix of mode signs for the hierarchic basis functions
c
        if (ipord .gt. 1) then
           call getsgn(ienb,sgn)
        endif
c
c.... gather the variables
c
        call localy(y,      yl,     ienb,   ndofl,  'gather  ')
        call localx(x,      xlb,    ienb,   nsd,    'gather  ')
c
c.... get the boundary element residuals
c
        rl  = zero

        call e3bSclr  (yl,      iBCB,    BCB,     shpb,    shglb,
     &                 xlb,     rl,      sgn,     dwl)
c
c.... assemble the residual and the modified residual
c
        call local (res,    rl,     ienb,   1,  'scatter ')
c     
c.... end
c
        return
        end

!> This routine computes and assembles data required for an Augmented
!! Lagrangian Method. 

      subroutine AsBPNABI ( x,       shpb,
     &                   ienb,  iBCB)
        use pvsQbi
        use LagrangeMultipliers ! brings in face radius and center 
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
c..............................Declaration.............................
       INTEGER ienb,iBCB,iel,iface,ipt3,k,lnode,n,nodlcl,i,m
       REAL*8 x,shpb,bnorm,dxdxib,rl,sgn,shdrv,shglb,shpfun,temp,temp1,
     &temp2,temp3,v1,v2
       REAL*8 WdetJb,xlb,rl2
c.......................................................................
c
        dimension xlb(npro,nenl,nsd),    bnorm(npro,nsd),
     &            rl(npro,nshl,nsd),     WdetJb(npro),
     &            rl2(npro,nshl,nsd,3)

        dimension x(numnp,nsd),
     &            shpb(nshl,ngaussb),      shglb(nsd,nshl,ngaussb),
     &            ienb(npro,nshl),
     &            iBCB(npro,ndiBCB)

        dimension lnode(27),               sgn(npro,nshl),
     &            shpfun(npro,nshl),        shdrv(npro,nsd,nshl)

c
        dimension dxdxib(npro,nsd,nsd),      temp(npro),
     &            temp1(npro),               temp2(npro),
     &            temp3(npro),
     &            v1(npro,nsd),              v2(npro,nsd)
c
        real*8    intpxlb(npro,3,nsd),    intpdistance(npro,3),
     &            intpprofile(npro,3)
        real*8    tmpLagInplaneVectors(3,3,0:MAXSURF)
        real*8    tmpLagProfileArea(0:MAXSURF)
        real*8    tmpProfileDelta(0:MAXSURF)
        real*8    Inplane1, Inplane2, Inplane3, InplaneNorm
        integer   count
c
c.... get the matrix of mode signs for the hierarchic basis functions
c
        if (ipord .gt. 1) then
           call getsgn(ienb,sgn)
        endif
c
c.... gather the variables
c
        call localx(x,      xlb,    ienb,   nsd,    'gather  ')
c
c....   calculate quadrature points
c        
        intpxlb = zero
        intpdistance = zero
        intpprofile = zero
        if (lcsyst.ne.6) then 
           do intp=1, 3 ! use 3 quadrature points
              do n=1, 3  
                 intpxlb(:,intp,:) = intpxlb(:,intp,:)
     &              +xlb(:,n,:)*Qptb(1,n,intp)
              enddo
           enddo
        else
           do intp=1, 3 ! use 3 quadrature points
              do n=1, 2  
                 intpxlb(:,intp,:) = intpxlb(:,intp,:)
     &              +xlb(:,n,:)*Qptb(1,n,intp)
              enddo
              intpxlb(:,intp,:) = intpxlb(:,intp,:)
     &           +xlb(:,5,:)*Qptb(1,3,intp)              
           enddo
        endif
c
c....   calculate profile functions at quadrature points
c        
        do k=1, numLagrangeSrfs
           do intp = 1, 3
              do iel=1, npro
                 if (iBCB(iel,2) .eq. nsrflistLagrange(k)) then
                    intpdistance(iel,intp)=sqrt(
     &                (intpxlb(iel,intp,1)-LagCenter(1,k))**2+
     &                (intpxlb(iel,intp,2)-LagCenter(2,k))**2+
     &                (intpxlb(iel,intp,3)-LagCenter(3,k))**2)
     &                /LagRadius(k)
                    intpprofile(iel,intp)=
     &                (ProfileOrder(k)+2)/ProfileOrder(k)*
     &                (1-intpdistance(iel,intp)**ProfileOrder(k))
                 endif
              enddo
           enddo
         enddo  

c
c.... get the boundary element residuals
c
        rl  = zero
        rl2 = zero
        tmpLagProfileArea = zero
        tmpProfileDelta = zero     
        tmpLagInplaneVectors = zero
c
c.... compute the nodes which lie on the boundary (hierarchic)
c
        call getbnodes(lnode)
c
c.... loop through the integration points
c
        ngaussb = nintb(lcsyst)
c        
        do intp = 1, ngaussb
c
c.... get the hierarchic shape functions at this int point
c
           shglb=zero  ! protect debugger 
           call getshpb(shpb,        shglb,        sgn, 
     &              shpfun,       shdrv)

c
c.... compute the normal to the boundary. This is achieved by taking
c     the cross product of two vectors in the plane of the 2-d 
c     boundary face.
c
           if(lcsyst.ne.6) then
              ipt3=3
           else
              ipt3=5
           endif
           v1 = xlb(:,2,:) - xlb(:,1,:)
           v2 = xlb(:,ipt3,:) - xlb(:,1,:)
c
c.....The following are done in order to correct temp1..3  
c     based on the results from compressible code.  This is done only 
c     for wedges, depending on the boundary face.(tri or quad)  
c        
           if (lcsyst .eq. 1) then
              temp1 = v1(:,2) * v2(:,3) - v2(:,2) * v1(:,3)
              temp2 = v2(:,1) * v1(:,3) - v1(:,1) * v2(:,3)
              temp3 = v1(:,1) * v2(:,2) - v2(:,1) * v1(:,2)
           else 
              temp1 = - v1(:,2) * v2(:,3) + v2(:,2) * v1(:,3)
              temp2 = - v2(:,1) * v1(:,3) + v1(:,1) * v2(:,3)
              temp3 = - v1(:,1) * v2(:,2) + v2(:,1) * v1(:,2)
           endif
c     
           temp       = one / sqrt ( temp1**2 + temp2**2 + temp3**2 )
           bnorm(:,1) = temp1 * temp
           bnorm(:,2) = temp2 * temp
           bnorm(:,3) = temp3 * temp
c 
           if (lcsyst .eq. 3) then
              WdetJb     = (1 - Qwtb(lcsyst,intp)) / (four*temp)
           elseif (lcsyst .eq. 4) then
              WdetJb     = Qwtb(lcsyst,intp) / temp
           else
              WdetJb     = Qwtb(lcsyst,intp) / (four*temp)
           endif
c
           do iel=1,npro
              count = 0
              do k=1, numLagrangeSrfs
                 if (iBCB(iel,2) .eq. nsrflistLagrange(k)) then
                    iface = iBCB(iel,2)
                    ndsurf(ienb(iel,1:nshlb))=iface   
                    count = count+1
                 endif
              enddo
              if (count .eq. 0) then
                 bnorm(iel,:) = zero  ! we want zeros where we are not integrating
                 WdetJb(iel) = zero  ! we want zeros where we are not integrating
              endif              
           enddo
c
c   Calculate two orthonormal in-plane vectors
c   |bnorm(iel,1)  bnorm(iel,2)  bnorm(iel,3) |
c   |v1(iel,1)     v1(iel,2)     v1(iel,3)    | 
c   x1 component: -v1(iel,2)*bnorm(iel,3)+v1(iel,3)*bnorm(iel,2)
c   x2 component: -v1(iel,3)*bnorm(iel,1)+v1(iel,1)*bnorm(iel,3)
c   x3 component: -v1(iel,1)*bnorm(iel,2)+v1(iel,2)*bnorm(iel,1)
c
           do k=1, numLagrangeSrfs
              do iel=1,npro
                 if (iBCB(iel,2) .eq. nsrflistLagrange(k)) then
                    tmpLagInplaneVectors(1:3,1,k)=bnorm(iel,1:3)
                    tmpLagInplaneVectors(1:3,2,k)=v1(iel,1:3)
     &                 /sqrt(v1(iel,1)**2+v1(iel,2)**2+v1(iel,3)**2)
                    Inplane1=-v1(iel,2)*bnorm(iel,3)
     &                 +v1(iel,3)*bnorm(iel,2)
                    Inplane2=-v1(iel,3)*bnorm(iel,1)
     &                 +v1(iel,1)*bnorm(iel,3)
                    Inplane3=-v1(iel,1)*bnorm(iel,2)
     &                 +v1(iel,2)*bnorm(iel,1)
                    InplaneNorm=one
     &                 /sqrt(Inplane1**2+Inplane2**2+Inplane3**2)
                    tmpLagInplaneVectors(1,3,k)=Inplane1*InplaneNorm
                    tmpLagInplaneVectors(2,3,k)=Inplane2*InplaneNorm
                    tmpLagInplaneVectors(3,3,k)=Inplane3*InplaneNorm
                    exit
                 endif
              enddo
           enddo              
c
c  Now lets calculate Integral N_(a:e)^i n_i ProfileFunction  d Gamma
c
c
           do n = 1, nshlb
              nodlcl = lnode(n)
              rl(:,nodlcl,1) = rl(:,nodlcl,1) + shpfun(:,nodlcl) 
     &           * bnorm(:,1)*intpprofile(:,intp)*WdetJb(:)
              rl(:,nodlcl,2) = rl(:,nodlcl,2) + shpfun(:,nodlcl) 
     &           * bnorm(:,2)*intpprofile(:,intp)*WdetJb(:)
              rl(:,nodlcl,3) = rl(:,nodlcl,3) + shpfun(:,nodlcl) 
     &           * bnorm(:,3)*intpprofile(:,intp)*WdetJb(:)
           enddo
c
c  Now lets calculate Integral N_(a:e)^i n_i N_(b:e)^i n_i d Gamma
c
c
           do k=1, numLagrangeSrfs
              do n = 1, nshlb
                 nodlcl = lnode(n)
                 do m=1, nsd                 
                    rl2(:,nodlcl,m,1)=rl2(:,nodlcl,m,1)+
     &                 shpfun(:,nodlcl)*shpfun(:,nodlcl)*WdetJb(:)
     &                 *tmpLagInplaneVectors(m,1,k)
     &                 *tmpLagInplaneVectors(m,1,k)
                    rl2(:,nodlcl,m,2)=rl2(:,nodlcl,m,2)+
     &                 shpfun(:,nodlcl)*shpfun(:,nodlcl)*WdetJb(:)
     &                 *tmpLagInplaneVectors(m,2,k)
     &                 *tmpLagInplaneVectors(m,2,k)
                    rl2(:,nodlcl,m,3)=rl2(:,nodlcl,m,3)+
     &                 shpfun(:,nodlcl)*shpfun(:,nodlcl)*WdetJb(:)
     &                 *tmpLagInplaneVectors(m,3,k)
     &                 *tmpLagInplaneVectors(m,3,k)
                 enddo
              enddo
           enddo
           
           do k=1, numLagrangeSrfs
              do iel=1,npro
                 if (iBCB(iel,2) .eq. nsrflistLagrange(k)) then
                    tmpLagProfileArea(k)=tmpLagProfileArea(k)+
     &                 intpprofile(iel,intp)*WdetJb(iel)
                    tmpProfileDelta(k)=tmpProfileDelta(k)+
     &                 intpprofile(iel,intp)**2*WdetJb(iel)
                 endif
              enddo
           enddo
        enddo  ! quadrature point loop
c
c.... assemble the PNABI vector
c
        call local (PNABI,    rl,     ienb,   3,  'scatter ')
c
c.... assemble the NANBIJ vector
c      
        do i=1, 3
           call local (NANBIJ(:,:,i),rl2(:,:,:,i),ienb,3,'scatter ')
        enddo
        
        do k=1, numLagrangeSrfs
           LagProfileArea(k)=LagProfileArea(k)+tmpLagProfileArea(k)
           ProfileDelta(k)=ProfileDelta(k)+tmpProfileDelta(k)
           InplaneNorm=sqrt(LagInplaneVectors(1,1,k)**2+
     &        LagInplaneVectors(2,1,k)**2+LagInplaneVectors(3,1,k)**2)
           if (InplaneNorm .eq. zero) then
              LagInplaneVectors(:,:,k)=tmpLagInplaneVectors(:,:,k)
           endif
        enddo   
c                 
        return
        end

!> This routine computes and assembles the data corresponding to the
!! interior elements.

        subroutine AsIGMRSclr(y,       ac,      x,       
     &                     shp,     shgl,    ien,     
     &                     res,     qres,    xSebe, xmudmi )


        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
 
c......................Declaration.....................................
C     Argument variables
C
      INTEGER             ien
C
      REAL*8                ac,          qres,        res,         shgl
      REAL*8                shp,         x,           y
C
C     Local variables
C
      REAL*8                sgn
c......................................................................
        dimension y(nshg,ndofl), ac(nshg,ndofl),x(numnp,nsd),
     &            shp(nshl,ngauss), shgl(nsd,nshl,ngauss),
     &            ien(npro,nshl),res(nshg),
     &            qres(nshg,nsd),sgn(npro,nshl)

c
        real*8    yl(npro,nshl,ndofl),        acl(npro,nshl,ndofl),
     &            xl(npro,nenl,nsd),         
     &            rl(npro,nshl),              ql(npro,nshl,nsd),
     &            dwl(npro,nenl)            
c        
        real*8    xSebe(npro,nshl,nshl),      xmudmi(npro,ngauss) 
c
c.... gather the variables
c
c.... get the matrix of mode signs for the hierarchic basis functions. 
c
        if (ipord .gt. 1) then
           call getsgn(ien,sgn)
        endif
        
        call localy(y,      yl,     ien,    ndofl,  'gather  ')
        call localy(ac,    acl,     ien,    ndofl,  'gather  ')
        call localx(x,      xl,     ien,    nsd,    'gather  ')
        call local (qres,   ql,     ien,    nsd,    'gather  ')
c
c.... zero the matrices if they are being recalculated
c
        if (lhs. eq. 1)  then
           xSebe = zero
        endif   
c
c.... get the element residuals, LHS matrix, and preconditioner
c
      rl = zero
      call e3Sclr  (yl,      acl,     shp,
     &              shgl,    xl,      dwl,
     &              rl,      ql,      xSebe,   
     &              sgn, xmudmi)
c
c.... assemble the residual
c
        call local (res,    rl,     ien,    1,  'scatter ')
c
c.... end
c
        return
        end

      subroutine initSponge( y,x)
      
      use     specialBC
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/matdat.h"
        include "common_blocks/mmatpar.h"
        include "common_blocks/outpar.h"
        
C     Local variables
C
      INTEGER             id
C
      REAL*8                bfz,         ptarget,     rad,         radc
      REAL*8                radsqr,      radst,       radsts,      rslc
      REAL*8                rsteep,      ttarget
      REAL*8                utarget,     vcl,       we
      REAL*8                zstart,      zval
C
      real*8   y(nshg,nflow), x(numnp,3)
      allocate (ytarget(nshg,nflow))  
      
      if(matflg(5,1).eq.5) then
         write(*,*) 'calculating IC sponge'
         ytarget = y
      else
         write(*,*) 'calculating Analytic sponge'

c
c OLD style sponge pushed onto target.  You need to be sure that your
c solver.inp entries for start and stop of sponge match as well as the
c growth rates
c
      vcl=datmat(1,5,1)         ! velocity on centerline
      rslc=datmat(2,5,1)        ! shear layer center radius
      bfz=datmat(3,5,1)
      we=3.0*29./682.
      rsteep=3.0
      zstart=30.0
      radst=10.0
      radsts=radst*radst
      do id=1,numnp
         radsqr=x(id,2)**2+x(id,1)**2
c         if((x(id,3).gt. zstart) .or. (radsqr.gt.radsts))  then
            rad=sqrt(radsqr)
            radc=max(rad,radst)
            zval=max(x(id,3),zstart)
            utarget=(tanh(rsteep*(rslc-rad))+one)/two*
     &                    (vcl-we) + we
            Ttarget  = press/(ro*Rgas)
            ptarget= press
            ytarget(id,1) = zero
            ytarget(id,2) = zero
            ytarget(id,3) = utarget
            ytarget(id,4) = ptarget
            ytarget(id,5) = Ttarget            
c         endif
      enddo
      endif
      return
      end

!> Initialize:time varying boundary condition

      subroutine initBCt( x,lgmapping, iBC, BC )

      use     specialBC

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/workfc.h"
        include "common_blocks/inpdat.h"
        include "mpif.h"

      real*8   x(numnp,nsd), BC(nshg,ndofBC)
      integer  iBC(numnp),lgmapping(numnp)
      integer  i,ierr
      character*20 fileno

      ntv=0
      nptsmax=0
      ic=0

      if (BCTFlag.eq.0) then
         if(myrank.eq.master) then
           write(*,*) ''
           write(*,'(A)') 'Opening bct.dat'
         endif
      else if (BCTFlag .eq. 1) then
         if(myrank.eq.master) then
           write(*,*) ''
           write(*,'(A)') 'Opening bct.vtp'
         endif
      endif

      if (numpe > 1) call MPI_BARRIER (MPI_COMM_WORLD, ierr)

      if(any(ibits(iBC,3,3).eq.7)) then
        if (BCTFlag .eq. 0) then

          do i=1,BCTFileNumber
            if(BCTFileNumber.eq.1) then
              call getnumfrombctdat('bct.dat',ntv_temp,nptsmax_temp)
            else
              write (fileno,'(i1)') i
              call getnumfrombctdat('bct'//trim(fileno)//'.dat',
     &                ntv_temp,nptsmax_temp)
            endif
            ntv=ntv+ntv_temp
            if(nptsmax_temp.gt.nptsmax) then
              nptsmax=nptsmax_temp
            endif
          enddo

          allocate (nBCt(numnp))
          allocate (numBCt(ntv))
          allocate (BCt(ntv,nptsmax,4))

          do i=1,BCTFileNumber
            if(BCTFileNumber.eq.1) then
              call initBCtfromDat('bct.dat', x,lgmapping, iBC, BC )
            else
              write (fileno,'(i1)') i
              call initBCtfromDat('bct'//trim(fileno)//'.dat', x,
     &           lgmapping, iBC, BC )
            endif
          enddo

#if(VER_USE_VTK == 1)
        else if (BCTFlag .eq. 1) then

          do i=1,BCTFileNumber
            if(BCTFileNumber.eq.1) then
              call getnumfrombctvtp('bct.vtp'//CHAR(0), ntv_temp,
     &                                nptsmax_temp)
            else
              write (fileno,'(i1)') i
              call getnumfrombctvtp('bct'//trim(fileno)//'.vtp'//
     &                              CHAR(0),ntv_temp,nptsmax_temp)
            endif
            ntv=ntv+ntv_temp
            if(nptsmax_temp.gt.nptsmax) then
              nptsmax=nptsmax_temp
            endif
          enddo

          allocate (nBCt(numnp))
          allocate (numBCt(ntv))
          allocate (BCt(ntv,nptsmax,4))

          do i=1,BCTFileNumber
            if(BCTFileNumber.eq.1) then
              call getnumfrombctvtp('bct.vtp'//CHAR(0), ntv_temp,
     &                                          nptsmax_temp)
              call initBCtfromVTP('bct.vtp', x, lgmapping, iBC, BC )
            else
              write (fileno,'(i1)') i
              call getnumfrombctvtp('bct'//trim(fileno)//'.vtp'//
     &                              CHAR(0),ntv_temp,nptsmax_temp)
              call initBCtfromVTP('bct'//trim(fileno)//'.vtp', x,
     &                               lgmapping, iBC, BC )
            endif
          enddo
#endif
        endif
        BCt(:,:,4)=BCt(:,:,4)*bcttimescale
      endif

      itvn=ic

      write(*,'(A,I3,A,I5,A)') ' Process rank(',myrank,') has ',
     & ic,' bct nodes.'

      if (numpe > 1) call MPI_BARRIER (MPI_COMM_WORLD, ierr)

      return
      end


      subroutine getnumfrombctdat(filename,ntv_temp,nptsmax_temp)
         include "global.h"

         character(len=*) filename
         character*80 card
         integer ntv_temp,nptsmax_temp

         open(unit=567, file=filename,ACTION='READ',STATUS='old')
         read (567,'(a80)') card
         read (card,*) ntv_temp, nptsmax_temp

         close(567)
         return
      end


      subroutine initBCtfromDat(filename, x, lgmapping, iBC, BC )

      use     specialBC

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/workfc.h"
        include "common_blocks/inpdat.h"

C     Local variables
C
      INTEGER             i,                     j,           k
      INTEGER             n,           ntpts,    ierr
C
      REAL*8               x1,          x2
      REAL*8                x3
C
      character(len=*) filename
      real*8   x(numnp,nsd), BC(nshg,ndofBC), rj1,rj2,rj3,rj4,distd,epsd
      integer  iBC(numnp),lgmapping(numnp)
      character*80 card
      real*8 distds
      real*8 dd
      integer global_node_id
!      integer ioerr
c
c  This one should be used for boundary layer meshes where bct.dat must
c  be given to greater precision than is currently being generated.
c
c      epsd=1.0d-12    ! this is distance SQUARED to save square root

c      epsd=1.0d-8              ! this is distance SQUARED to save square root
      epsd=1.0d-6              ! new distance to avoid problems when using mm

         open(unit=567, file=filename,ACTION='READ',STATUS='old')
         read (567,'(a80)') card
         read (card,*) ntv_temp, nptsmax_temp
         do k=1,ntv_temp

            if(BCTMatchingFlag.eq.0) then
              read(567,*) x1,x2,x3,ntpts
            else if (BCTMatchingFlag.eq.1) then
              read(567,*) x1,x2,x3,ntpts,global_node_id
            endif

            do i=1,numnp
               if(ibits(ibc(i),3,3) .eq.7) then
c
c Find the point on the boundary (if it is on this processor)
c that matches this point using coordinates or global node id
c
                  if(BCTMatchingFlag.eq.0) then
                      dd= distds(x1,x2,x3,x(i,1),x(i,2),x(i,3))
                      if(dd.lt.epsd) then
                         ic=ic+1
                         nBCt(ic)=i ! the pointer to this point
                         numBCt(ic)=ntpts ! the number of time series
                         do j=1,ntpts
c                        read(567,*) BCt(ic,j,4),(BCt(ic,j,n),n=1,3)
                            read(567,*) (BCt(ic,j,n),n=1,4)
                         enddo
                         exit
                      endif
                  else if (BCTMatchingFlag.eq.1) then
                      if(lgmapping(i).eq.global_node_id) then
                         ic=ic+1
                         nBCt(ic)=i ! the pointer to this point
                         numBCt(ic)=ntpts ! the number of time series
                         do j=1,ntpts
                            read(567,*) (BCt(ic,j,n),n=1,4)
                         enddo
                         exit
                      endif
                  endif
               endif
            enddo
            if(i.eq.numnp+1) then
c
c  if we get here the point was not found.  It must be on another
c  processor so we read past this record and move on
c
               do j=1,ntpts
                  read(567,*) rj1,rj2,rj3,rj4
               enddo
            endif
         enddo                  ! end of the loop over ntv

      close(567)

      return
      end

#if(VER_USE_VTK == 1)
      subroutine initBCtfromVTP(filename, x, lgmapping, iBC, BC )

      use     specialBC

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/workfc.h"
        include "common_blocks/inpdat.h"

C     Local variables
C
      INTEGER             i,                     j,           k
      INTEGER             n,           ntpts,   ierr
C
      character(len=*) filename
      real*8   x(numnp,nsd), BC(nshg,ndofBC),distd,epsd
      integer  iBC(numnp),lgmapping(numnp)
      real*8 distds
      real*8 dd

      real*8  x1(ntv_temp), x2(ntv_temp),x3(ntv_temp)
      real*8  v1(nptsmax_temp,ntv_temp),v2(nptsmax_temp,ntv_temp)
      real*8  v3(nptsmax_temp,ntv_temp)
!      real*8  v1(nptsmax_temp*ntv_temp),v2(nptsmax_temp*ntv_temp)
!      real*8  v3(nptsmax_temp*ntv_temp)
      real*8  ts(nptsmax_temp)
      integer global_node_id(ntv_temp)

      call getdatafrombctvtp(filename//CHAR(0),x1,x2,x3,v1,v2,v3,
     &                                        ts,global_node_id)
      ntpts=nptsmax_temp
c
c  This one should be used for boundary layer meshes where bct.dat must
c  be given to greater precision than is currently being generated.
c
c      epsd=1.0d-12    ! this is distance SQUARED to save square root

c      epsd=1.0d-8              ! this is distance SQUARED to save square root
      epsd=1.0d-6              ! new distance to avoid problems when using mm

         do k=1,ntv_temp
            do i=1,numnp
               if(ibits(ibc(i),3,3) .eq.7) then
c
c Find the point on the boundary (if it is on this processor)
c that matches this point using coordinates or global node id
c
                  if(BCTMatchingFlag.eq.0) then
                      dd= distds(x1(k),x2(k),x3(k),x(i,1),x(i,2),x(i,3))
                      if(dd.lt.epsd) then
                         ic=ic+1
                         nBCt(ic)=i ! the pointer to this point
                         numBCt(ic)=ntpts ! the number of time series
                         do j=1,ntpts
                            BCt(ic,j,1)=v1(j,k);
                            BCt(ic,j,2)=v2(j,k);
                            BCt(ic,j,3)=v3(j,k);
                            BCt(ic,j,4)=ts(j);
                         enddo
                         exit
                      endif
                  else if (BCTMatchingFlag.eq.1) then
                      if(lgmapping(i).eq.global_node_id(k)) then
                         ic=ic+1
                         nBCt(ic)=i ! the pointer to this point
                         numBCt(ic)=ntpts ! the number of time series
                         do j=1,ntpts
                            BCt(ic,j,1)=v1(j,k);
                            BCt(ic,j,2)=v2(j,k);
                            BCt(ic,j,3)=v3(j,k);
                            BCt(ic,j,4)=ts(j);
                         enddo
                         exit
                      endif
                  endif
               endif
            enddo
         enddo                  ! end of the loop over ntv

      return
      end
#endif

      subroutine BCint(timel,shp,shgl,shpb,shglb,x,BC,iBC)

      use     specialBC ! brings in itvn,nbct, bct, numbct, nptsmax

        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/genpar.h"
        include "common_blocks/nomodule.h"

C     Local variables
C
      REAL*8      wr
C
      real*8   BC(nshg,ndofBC), timel,t
      real*8   x(numnp,nsd),   
     &         shp(MAXTOP,maxsh,MAXQPT),
     &         shgl(MAXTOP,nsd,maxsh,MAXQPT),
     &         shpb(MAXTOP,maxsh,MAXQPT),
     &         shglb(MAXTOP,nsd,maxsh,MAXQPT)

      integer  iBC(numnp),nlast,i,j,nper 

      do i =1,itvn ! itvn is the number of varying nodes on this proc 

         nlast=numBCt(i)     ! number of time series to interpolate from
         nper=timel/BCt(i,nlast,4)! number of periods completed to shift off


         t=timel-nper*BCt(i,nlast,4)  ! now time in periodic domain

         do j=2,nlast   !loop to find the interval that we are in

            if(BCt(i,j,4).gt.t) then  ! this is upper bound, j-1 is lower

               wr=(t-BCt(i,j-1,4))/(BCt(i,j,4)-BCt(i,j-1,4))
               BC(nbct(i),3:5)= BCt(i,j-1,1:3)*(one-wr) 
     &                        + BCt(i,j,1:3)*wr
               exit

            endif
         enddo
      enddo
      return
      end

      function distds(x1,y1,z1,x2,y2,z2)
      real*8 distds 
      real*8 x1,y1,z1,x2,y2,z2,x,y,z
      x=x1-x2
      y=y1-y2
      z=z1-z2
      distds=x*x+y*y+z*z
      return
      end

!> Initialize the impedance boundary condition:
!! read the data in initImpt
!! interpolate the data to match the process time step in Impint

      subroutine initImpt()
      
      use convolImpFlow
      include "global.h"
      include "common_blocks/nomodule.h"

C     Local variables
C
      INTEGER             j,           k,           n
      
      open(unit=817, file='impt.dat',status='old')
         read (817,*) nptsImpmax
         allocate (numImpt(numImpSrfs))  
         allocate (ValueImpt(nptsImpmax,2,numImpSrfs))
         ValueImpt=0
         do k=1,numImpSrfs
            read (817,*) numDataImp
            numImpt(k) = numDataImp
            do j=1,numDataImp
               read(817,*) (ValueImpt(j,n,k),n=1,2) ! n=1 time, 2 value
            enddo
         enddo
      close(817)
      
      allocate (ValueListImp(ntimeptpT+1,numImpSrfs))
      ValueListImp(ntimeptpT+1,:) = ValueImpt(1,2,:) !Z(time=0), last entry
      ValueListImp(1,:) = ValueImpt(1,2,:) !Z(time=0)=Z(time=T)
      return
      end
      
      
      
      subroutine Impint(ctime,jstep)
      
      use convolImpFlow

        include "global.h"
        include "common_blocks/nomodule.h"

C     Local variables
C
      REAL*8          wr
C
      real*8 ctime, ptime
      integer nlast, nper, k, j , jstep
      
         
      do k =1,numImpSrfs
         nlast=numImpt(k)     ! number of time series to interpolate from
         nper=ctime/ValueImpt(nlast,1,k)!number of periods completed to shift off
         ptime = ctime-nper*ValueImpt(nlast,1,k)  ! now time in periodic domain
            
         do j=2,nlast   !loop to find the interval that we are in

            if(ValueImpt(j,1,k).gt.ptime) then  ! this is upper bound, j-1 is lower
               wr=(ptime-ValueImpt(j-1,1,k))
     &             / ( ValueImpt(j,1,k)-ValueImpt(j-1,1,k) )
               ValueListImp(jstep,k)= ValueImpt(j-1,2,k)*(one-wr) 
     &                        + ValueImpt(j,2,k)*wr
               exit
            endif

         enddo
      enddo
      return
      end


!> Initialize the RCR boundary condition:
!! read the data in initRCRt
!! interpolate the data to match the process time step in RCRint

      subroutine initRCRt()
      
      use convolRCRFlow

      include "global.h"
      include "common_blocks/timdat.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/inpdat.h"

C     Local variables
C
      INTEGER             j,           k,           n

C
      open(unit=818, file='rcrt.dat',status='old')
         read (818,*) nptsRCRmax
         allocate (numRCRt(numRCRSrfs))  
         allocate (RCRArea(numRCRSrfs))  
         allocate (ValuePdist(nptsRCRmax,2,numRCRSrfs))
         allocate (ValueListRCR(3,numRCRSrfs))
         RCRArea = zero
         ValuePdist=0
         ValueListRCR=0
         do k=1,numRCRSrfs
            read (818,*) numDataRCR
            numRCRt(k) = numDataRCR
            do j=1,3
               read(818,*) ValueListRCR(j,k) ! reads Rp,C,Rd
            enddo
            do j=1,numDataRCR
               read(818,*) (ValuePdist(j,n,k),n=1,2) ! n=1 time, 2 value
            enddo
         enddo
      close(818)

      allocate (dtRCR(numRCRSrfs))
      if (lstep .eq. 0) then
         nptsRCR = 0
         allocate (QHistRCR(nstep(1)+1,numRCRSrfs)) !for flow history
         allocate (PHistRCR(nstep(1)+1,numRCRSrfs)) !for flow history
         allocate (RCRConvCoef(nstep(1)+2,numRCRSrfs)) !for convolution coeff
         QHistRCR = zero
         PHistRCR = zero
      elseif (lstep .gt. 0) then   
         nptsRCR = lstep            
         allocate (QHistRCR(lstep+nstep(1)+1,numRCRSrfs))
         allocate (RCRConvCoef(lstep+nstep(1)+2,numRCRSrfs)) !for convolution coeff
         allocate (PHistRCR(lstep+nstep(1)+1,numRCRSrfs))
         PHistRCR = zero
         QHistRCR = zero
         call ReadDataFile(QHistRCR(1:lstep+1,:),lstep+1,numRCRSrfs,
     &      'QHistRCR.dat',870)
         call ReadDataFile(PHistRCR(1:lstep+1,:),lstep+1,numRCRSrfs,
     &      'PHistRCR.dat',871)
      endif
      
      return
      end
           
      
      subroutine RCRint(ctime,Pdist)
      
      use convolRCRFlow ! brings numRCRSrfs, ValuePdist

      include "global.h"
      include "common_blocks/nomodule.h"

C     Local variables
C
      REAL*8             wr
C
      real*8  ctime, ptime
      integer nlast, nper, k, j
      real*8  Pdist(0:MAXSURF)      
         
      do k =1,numRCRSrfs
         nlast=numRCRt(k)     ! number of time series to interpolate from
         nper=ctime/ValuePdist(nlast,1,k)!number of periods completed to shift off
         ptime = ctime-nper*ValuePdist(nlast,1,k)  ! now time in periodic domain
            
         do j=2,nlast   !loop to find the interval that we are in

            if(ValuePdist(j,1,k).gt.ptime) then  ! this is upper bound, j-1 is lower
               wr=(ptime-ValuePdist(j-1,1,k))
     &             / ( ValuePdist(j,1,k)-ValuePdist(j-1,1,k) )
               Pdist(k)= ValuePdist(j-1,2,k)*(one-wr) 
     &                        + ValuePdist(j,2,k)*wr
               exit
            endif

         enddo
      enddo
      return
      end
      
!> Read data for Lagrange multipliers: read input data in initLagrange
!! This data is required to generate profile functions

      subroutine initLagrange()
      
      use LagrangeMultipliers 

      include "global.h"
      include "common_blocks/nomodule.h"
      include "common_blocks/timdat.h"
      include "common_blocks/inpdat.h"

C     Local variables
C
      INTEGER k
      INTEGER n
      LOGICAL ierr
C
      integer NumOfData
      
      allocate(LagCenter(3,numLagrangeSrfs))
      allocate(LagInplaneVectors(3,3,numLagrangeSrfs))
      allocate(LagRadius(numLagrangeSrfs))
      allocate(LagProfileArea(numLagrangeSrfs))
      allocate(Lagold(numLagrangeSrfs,3))
      allocate(Lag(numLagrangeSrfs,3))
      allocate(Lagincr(numLagrangeSrfs,3))
      allocate(Lagalpha(numLagrangeSrfs,3))
      allocate(ProfileOrder(numLagrangeSrfs))
      allocate(ProfileDelta(numLagrangeSrfs))
      allocate(Penalty(numLagrangeSrfs,3))
      allocate(PenaltyCoeff(numLagrangeSrfs,3))
      allocate(ScaleFactor(numLagrangeSrfs,3))
      allocate(AddLag(numLagrangeSrfs,3))
      allocate(LagMeanFlow(numLagrangeSrfs))
      NumOfData = numLagrangeSrfs*3
      allocate(LagHist(lstep+nstep(1)+1, NumOfData))
      allocate(LagErrorHist(lstep+nstep(1)+1, NumOfData))
      LagCenter = zero
      LagInplaneVectors = zero
      LagRadius = zero
      LagProfileArea = zero
      Lagold = zero
      Lag = zero
      Lagincr = zero
      Lagalpha = zero
      ProfileOrder = 0
      LagSwitch = 0
      ProfileDelta = zero
      Penalty = zero
      PenaltyCoeff = zero
      ScaleFactor = zero
      AddLag = zero
      LagMeanFlow = zero
      LagHist = zero
      LagErrorHist = zero
      open(unit=800, file='LagrangeData.dat', status='old')
      do k=1, numLagrangeSrfs
         read(800,*)
         read(800,*) (LagCenter(n,k), n=1,3)  !Center of a constrained surface
         read(800,*)
         read(800,*) LagRadius(k)        !Surface radius
         read(800,*)
         read(800,*) ProfileOrder(k)      !Profile order
         read(800,*)
         read(800,*) LagMeanFlow(k)      !Mean flow
         read(800,*)
         read(800,*) (Lagold(k,n), n=1,3) !Initial Lagrange Multipliers 
         read(800,*)
         read(800,*) (PenaltyCoeff(k,n), n=1,3) !Penalty numbers
         read(800,*)
         read(800,*) (ScaleFactor(k,n), n=1,3) !Scaling factors
      enddo
      close(800)

c     CHECK IF THE LAGRANGE MULTIPLIER FILE EXISTS - DES
      INQUIRE(FILE='LagrangeMultipliers.dat', EXIST=ierr)
      if (lstep .gt. zero .AND. ierr) then
         call ReadDataFile(LagHist(1:lstep+1,:),lstep+1,NumOfData,
     &      'LagrangeMultipliers.dat',801)
         call ReadDataFile(LagErrorHist(1:lstep+1,:),lstep+1,NumOfData,
     &      'LagrangeErrors.dat',802)
      endif
      
      return
      end         

!> Returns in pold the history dependent part of the pressure in the
!! impedance/flow rate convolution for the impedance, RCR, COR

      subroutine pHist(pressHist,QHist,betas,nTimePoint,nSrfs)

      include "global.h"
C     Local variables
C
      INTEGER             j,           k
C
      integer  nTimePoint,nSrfs
      real*8   pressHist(0:MAXSURF)
      real*8   QHist(nTimePoint+1,nSrfs),betas(nTimePoint+2,nSrfs)
      !don't need here betas(ntimePoint+2)
      !but pb of array passing if cut at nTimePoint+1
      pressHist=zero
      do k=1,nSrfs
        do j=1,nTimePoint+1
            pressHist(k) = pressHist(k) + QHist(j,k)*betas(j,k)
        enddo
      enddo
      return
      end

!> This subroutine reads a data file and copies to a data array

      subroutine ReadDataFile(DataFile,nrows,ncolms,Filename,UnitNumber)

      include "global.h"
C     Local variables
C
      INTEGER             i,           j
C
      character(len=*) Filename
      real*8    DataFile(nrows,ncolms)
      integer   nrows, ncolms, UnitNumber
      
      open(unit=UnitNumber, file=Filename, status='old')
         read(UnitNumber,*) 
         do i=1, nrows
            read(UnitNumber,*) (DataFile(i,j), j=1, ncolms)
         enddo
      close(UnitNumber)
   
      return
      end

!> This routine computes the LHS mass matrix, the RHS residual 
!! vector, and the preconditioning matrix, for use with the GMRES
!! solver.

      subroutine ElmGMRSclr (y,         ac,        x,     
     &                       shp,       shgl,      iBC,
     &                       BC,        shpb,      shglb,
     &                       res,       iper,      ilwork,
     &                       rowp,      colm,      lhsS    )
c
        use pointer_data
        use local_mass
c
        include "global.h"
        include "mpif.h"
        include "common_blocks/aerfrc.h"
        include "common_blocks/blkdat.h"
        include "common_blocks/conpar.h"
        include "common_blocks/elmpar.h"
        include "common_blocks/fronts.h"
        include "common_blocks/genpar.h"
        include "common_blocks/inpdat.h"
        include "common_blocks/intpt.h"
        include "common_blocks/propar.h"
        include "common_blocks/shpdat.h"
        include "common_blocks/timdat.h"
        include "common_blocks/workfc.h"
        include "common_blocks/sclrs.h"
C
C     Argument variables
C
      INTEGER             ibc,         iper
C
      REAL*8                ac,          bc,          res,         shgl
      REAL*8                shglb,       shp,         shpb,        x
      REAL*8                y
C
C     Local variables
C
      INTEGER             iblk,         iel
C
      REAL*8                qres,        rmass
      REAL*8                spmass
C
        dimension y(nshg,ndof),         ac(nshg,ndof),
     &            x(numnp,nsd),         iBC(nshg),           
     &            BC(nshg,ndofBC),      res(nshg),
     &            iper(nshg)
c
        dimension shp(MAXTOP,maxsh,MAXQPT),  
     &            shgl(MAXTOP,nsd,maxsh,MAXQPT), 
     &            shpb(MAXTOP,maxsh,MAXQPT),
     &            shglb(MAXTOP,nsd,maxsh,MAXQPT) 
c
        dimension qres(nshg,nsd),     rmass(nshg)
c
        integer ilwork(nlwork), rowp(nshg*nnz),   colm(nshg+1)

        real*8 lhsS(nnz_tot)

        real*8, allocatable, dimension(:,:,:) :: xSebe
c
c.... set up the timer
c

CAD        call timer ('Elm_Form')
c
c.... -------------------->   diffusive flux   <--------------------
c
        ires   = 1

        if (idiff==1 .or. idiff==3) then ! global reconstruction of qdiff
c
c loop over element blocks for the global reconstruction
c of the diffusive flux vector, q, and lumped mass matrix, rmass
c
           qres = zero
           rmass = zero
        
           do iblk = 1, nelblk
              iel    = lcblk(1,iblk)
              lcsyst = lcblk(3,iblk)
              nenl   = lcblk(5,iblk) ! no. of vertices per element
              nshl   = lcblk(10,iblk)
              mattyp = lcblk(7,iblk)
              ndofl  = lcblk(8,iblk)
              npro   = lcblk(1,iblk+1) - iel 
              
              ngauss = nint(lcsyst)
c     
c.... compute and assemble diffusive flux vector residual, qres,
c     and lumped mass matrix, rmass

              call AsIqSclr (y,                   x,                       
     &                       shp(lcsyst,1:nshl,:), 
     &                       shgl(lcsyst,:,1:nshl,:),
     &                       mien(iblk)%p,     qres,                   
     &                       rmass )
       
           enddo
       
c
c.... form the diffusive flux approximation
c
           call qpbcSclr ( rmass, qres, iBC, iper, ilwork )       
c
        endif 
c
c.... -------------------->   interior elements   <--------------------
c
        res    = zero
        spmass = zero

        if (lhs .eq. 1) then
           lhsS   = zero
        endif

        if ((impl(1)/10) .eq. 0) then   ! no flow solve so flxID was not zeroed
           flxID = zero
        endif
c
c.... loop over the element-blocks
c
        do iblk = 1, nelblk
          iblock = iblk         ! used in local mass inverse (p>2)
          iel    = lcblk(1,iblk)
          lcsyst = lcblk(3,iblk)
          nenl   = lcblk(5,iblk) ! no. of vertices per element
          nshl   = lcblk(10,iblk)
          ndofl  = lcblk(8,iblk)
          npro   = lcblk(1,iblk+1) - iel

          ngauss = nint(lcsyst)
c
c.... allocate the element matrices
c
          allocate ( xSebe(npro,nshl,nshl) )
c
c.... compute and assemble the residual and tangent matrix
c
          call AsIGMRSclr(y,                   ac,
     &                 x,
     &                 shp(lcsyst,1:nshl,:), 
     &                 shgl(lcsyst,:,1:nshl,:),
     &                 mien(iblk)%p,        res,
     &                 qres,                xSebe, mxmudmi(iblk)%p )
c
c.... satisfy the BC's on the implicit LHS
c     
          if (impl(1) .ne. 9 .and. lhs .eq. 1) then
             call fillsparseSclr (mien(iblk)%p, 
     &                 xSebe,             lhsS,
     &                 rowp,              colm)
          endif

          deallocate ( xSebe )
c
c.... end of interior element loop
c
       enddo

c
c.... add in lumped mass contributions if needed
c
       if((flmpr.ne.0).or.(flmpl.ne.0)) then
          call lmassaddSclr(ac(:,isclr), res,rowp,colm,lhsS,gmass)
       endif

       have_local_mass = 1
c
c
c  call DtN routine which updates the flux to be consistent with the
c  current solution values.  We will put the result in the last slot of
c  BC (we added a space in input.f).  That way we can localize this
c  value to the boundary elements.  This is important to keep from calling
c  the DtN evaluator more than once per node (it can be very expensive).
c
         if(idtn.eq.1)  call DtN(iBC,BC,y)
c
c.... -------------------->   boundary elements   <--------------------
c
c
c.... loop over the boundary elements
c
        do iblk = 1, nelblb
c
c.... set up the parameters
c
          iel    = lcblkb(1,iblk)
          lcsyst = lcblkb(3,iblk)
          nenl   = lcblkb(5,iblk)  ! no. of vertices per element
          nenbl  = lcblkb(6,iblk)  ! no. of vertices per bdry. face
          nshl   = lcblkb(9,iblk)
          nshlb  = lcblkb(10,iblk)
          ndofl  = lcblkb(8,iblk)
          npro   = lcblkb(1,iblk+1) - iel

          if(lcsyst.eq.3) lcsyst=nenbl
          if(lcsyst.eq.3 .or. lcsyst.eq.4) then
             ngaussb = nintb(lcsyst)
          else
             ngaussb = nintb(lcsyst)
          endif
c
c localize the dtn boundary condition
c

          if(idtn.eq.1)   call dtnl(   iBC, BC, mienb(iblk)%p,
     &              miBCB(iblk)%p,  mBCB(iblk)%p)

c
c.... compute and assemble the residuals corresponding to the 
c     boundary integral
c
          call AsBSclr (y,                       x,
     &                  shpb(lcsyst,1:nshl,:),
     &                  shglb(lcsyst,:,1:nshl,:),
     &                  mienb(iblk)%p,           mmatb(iblk)%p,
     &                  miBCB(iblk)%p,           mBCB(iblk)%p,
     &                  res)
c
c.... end of boundary element loop
c
        enddo
c
c
c.... -------------------->   communications <-------------------------
c

      if (numpe > 1) then
        call commu (res  , ilwork, 1  , 'in ')
      endif

c
c.... ---------------------->   post processing  <----------------------
c
c.... satisfy the BCs on the residual
c
      call bc3ResSclr (iBC,  res,  iper, ilwork)
c
c.... return
c
CAD      call timer ('Back    ')
      return
      end

        
c
c....routine to compute and return the flow rates for coupled surfaces of a given type
c        
      subroutine GetFlowQ (qsurf,y,srfIdList,numSrfs)
        
      use pvsQbi  ! brings in NABI
c
      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
C
C     Local variables
C
      INTEGER             i,           ierr,        j,           k
      INTEGER              npars

C
      real*8  y(nshg,3)
      real*8  qsurf(0:MAXSURF),qsurfProc(0:MAXSURF)
      integer numSrfs, srfIdList(0:MAXSURF)

c note we only need the first three entries (u) from y

      IF (numSrfs .EQ. 0) RETURN

      qsurfProc=zero
      do i = 1,nshg
         do k = 1,numSrfs
            if (srfIdList(k).eq.ndsurf(i)) then
               do j = 1,3              
                  qsurfProc(k) = qsurfProc(k) + NABI(i,j)*y(i,j)
               enddo
            endif      
         enddo       
      enddo
c      
c     at this point, each qsurf has its "nodes" contributions to Q
c     accumulated into qsurf. Note, because NABI is on processor this
c     will NOT be Q for the surface yet
c
c.... reduce integrated Q for each surface, push on qsurf
c
       npars=MAXSURF+1
       call MPI_ALLREDUCE (qsurfProc, qsurf, npars,
     &        MPI_DOUBLE_PRECISION,MPI_SUM, MPI_COMM_WORLD,ierr) 
c
c.... return
c
      return
      end 
c
c.... routine to compute and return the flow rates multiplied by a profile function
c.... for constrained surfaces
c        
      subroutine GetProfileFlowQ (qsurf, y, srfIdList, numSrfs)
        
      use pvsQbi  ! brings in PNABI, ndsurf
c
      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
C
C     Local variables
C
      INTEGER             ierr
      INTEGER              npars

C
      real*8  y(nshg,3)
      real*8  qsurf(0:MAXSURF), qsurfProc(0:MAXSURF)
      integer numSrfs, irankCoupled, srfIdList(0:MAXSURF)
      integer i, j, k
c
c.... clear the vectors 
c
      qsurfProc = zero
      do i = 1,nshg      
         if(numSrfs .gt. zero) then
            do k = 1, numSrfs
               irankCoupled = 0
               if (srfIdList(k) .eq. ndsurf(i)) then
                  irankCoupled=k
                  do j = 1, 3   
                     qsurfProc(irankCoupled) = qsurfProc(irankCoupled)
     &                  +PNABI(i,j)*y(i,j)
                  enddo
               endif      
            enddo       
         endif      
      enddo
c      
c     at this point, each qsurf has its "nodes" contributions to Q
c     accumulated into qsurf. Note, because PNABI is on processor this
c     will NOT be Q for the surface yet
c
c.... reduce integrated Q for each surface, push on qsurf
c
      npars=MAXSURF+1
      call MPI_ALLREDUCE (qsurfProc, qsurf, npars,
     &        MPI_DOUBLE_PRECISION,MPI_SUM, MPI_COMM_WORLD,ierr)
  
c
c.... return
c
      return
      end

!> Routine for computing inner products of velocity components for constrained surfaces.
!! Inner product is computed by calling GetInnerProduct after this routine.

      subroutine CalcNANBLagrange(col, row, y)
c

      use LagrangeMultipliers
      use pvsQbi 
 
      include "global.h"
      include "common_blocks/conpar.h"
      include "common_blocks/nomodule.h"
c
      integer col(nshg+1),    row(nnz_tot)
      real*8    y(nshg,3)
c
      real*8 tmp1, tmp2, tmp3
      integer p,  i, j, k, n, m
c
c.... clear the vector
c
      if (LagSwitch .gt. 0) then 
         NANBLagrange(4:6,:,:) = zero
      else
         NANBLagrange = zero
      endif
c
c....calculate NANBLagrange
c
      do i = 1, nshg
         do n=1, 3
            do p=1, numLagrangeSrfs
               tmp1 = 0
               tmp2 = 0
               tmp3 = 0
               if (nsrflistLagrange(p).eq.ndsurf(i)) then 
                  do k = col(i), col(i+1)-1
                     j = row(k)
c
                     tmp1 = tmp1
     1                  +lhsLagL(1,k,n)*y(j,1)
     2                  +lhsLagL(4,k,n)*y(j,2)
     3                  +lhsLagL(7,k,n)*y(j,3)
                     tmp2 = tmp2
     1                  +lhsLagL(2,k,n)*y(j,1)
     2                  +lhsLagL(5,k,n)*y(j,2)
     3                  +lhsLagL(8,k,n)*y(j,3)
                     tmp3 = tmp3
     1                  +lhsLagL(3,k,n)*y(j,1)
     2                  +lhsLagL(6,k,n)*y(j,2)
     3                  +lhsLagL(9,k,n)*y(j,3)
c
                  enddo
                  if (LagSwitch .gt. 0) then 
                     m = n+3
                     NANBLagrange(m,i,1)=NANBLagrange(m,i,1)+tmp1
                     NANBLagrange(m,i,2)=NANBLagrange(m,i,2)+tmp2
                     NANBLagrange(m,i,3)=NANBLagrange(m,i,3)+tmp3
                  else
                     m=n
                     NANBLagrange(m,i,1)=NANBLagrange(m,i,1)+tmp1
                     NANBLagrange(m,i,2)=NANBLagrange(m,i,2)+tmp2
                     NANBLagrange(m,i,3)=NANBLagrange(m,i,3)+tmp3
                  endif
               endif
            enddo
         enddo
      enddo
c
      return 
      end
                        
!> Routine to compute inner products for constrained surfaces.
!! CalcNANBLagrange should be called first
        
      subroutine GetInnerProduct (qsurf, y, srfIdList, numSrfs)
c        
      use LagrangeMultipliers ! brings in NANBLagrange
      use pvsQbi  ! brings in ndsurf
c
      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
c
C
      INTEGER             npars,	ierr

C
      real*8  y(nshg,3)
      real*8  qsurf(0:MAXSURF,3), qsurfProc(0:MAXSURF,3)
      integer numSrfs, irankCoupled, srfIdList(0:MAXSURF)
      integer i, j, k, n
c
c.... clear the vector 
c
      qsurfProc = zero
      if(numSrfs.gt.zero) then
         do i = 1, nshg
            do n=1, 3
               do k = 1, numSrfs      
                  if (srfIdList(k) .eq. ndsurf(i)) then
                     do j=1, 3
                        qsurfProc(k,n)=qsurfProc(k,n)
     &                     +NANBLagrange(n,i,j)*y(i,j)
                     enddo
                  endif
               enddo      
            enddo       
         enddo      
      endif
c      
c     at this point, each qsurf has its "nodes" contributions to Q
c     accumulated into qsurf. Note, because NABI is on processor this
c     will NOT be Q for the surface yet
c
c.... reduce integrated Q for each surface, push on qsurf
c
      do n=1, 3
         npars=MAXSURF+1
         call MPI_ALLREDUCE (qsurfProc(:,n), qsurf(:,n), npars,
     &        MPI_DOUBLE_PRECISION,MPI_SUM, MPI_COMM_WORLD,ierr)
      enddo  
c
c.... return
c
      return
      end      

!> Routine to multiply 1/mu * L transpose matrix for Lagrange Multipliers

      subroutine LagMultiplyMatrixTranspose(srfIDList, numSrfs)     

      use pvsQbi  ! brings in NABI
      use LagrangeMultipliers !brings in the current part of coef for Lagrange Multipliers
c
      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
      include "common_blocks/inpdat.h"
      include "common_blocks/timdat.h"
C
C     Local variables
C
      INTEGER             i,           k

C
      real*8  DiagonalDelta,          DiagonalDeltaSurf
      integer  srfIDList(0:MAXSURF),  numSrfs 
     
      DiagonalDelta = -two*alfi*gami*Delt(1)
      LagAPproduct = zero

      do i=1, nshg
         do k = 1, numSrfs
            DiagonalDeltaSurf = zero
            DiagonalDeltaSurf = DiagonalDelta*LagMeanFlow(k)
            if (srfIDList(k).eq.ndsurf(i)) then 
               LagAPproduct(i,1:3)=LagAPproduct(i,1:3)+DiagonalDeltaSurf
     &            *((NANBLagrange(1,i,1:3)-PQLagrange(k,1)*NABI(i,1:3)
     &            -QLagrange(k,1)*PNABI(i,1:3)/LagProfileArea(k)
     &            +QLagrange(k,1)*NABI(i,1:3)*ProfileDelta(k))
     &            *AddLag(k,1)+NANBLagrange(2,i,1:3)*AddLag(k,2)
     &            +NANBLagrange(3,i,1:3)*AddLag(k,3) )
            endif
         enddo
      enddo  
     
      return
      end

!> Routine to multiply L matrix for Lagrange Multipliers

      subroutine LagMultiplyMatrix (Dy, CaseNumber, srfIDList, numSrfs) 

      use pvsQbi  ! brings in NABI
      use LagrangeMultipliers !brings in the current part of coef for Lagrange Multipliers
c
      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
      include "common_blocks/inpdat.h"
      include "common_blocks/timdat.h"
c
C
C     Local variables
C
      INTEGER              k
C
      real*8  Dy(nshg,3),    DiagonalDeltaSurf
      real*8  DiagonalDelta, ProcAddLag(0:MAXSURF,3)
      integer CaseNumber, srfIDList(0:MAXSURF), numSrfs 

      DiagonalDelta = -two*alfi*gami*Delt(1)
      ProcAddLag = zero
      if (CaseNumber .eq. zero) then
         call GetFlowQ(ProcAddLag(:,1), Dy(:,1:3), srfIDList, numSrfs)  
         QLagrange(1:numSrfs,2)=ProcAddLag(1:numSrfs,1)
         ProcAddLag = zero
         call GetProfileFlowQ(ProcAddLag(:,1), Dy(:,1:3), srfIDList,
     &      numSrfs)    
         PQLagrange(1:numSrfs,2)=ProcAddLag(1:numSrfs,1)
     &      /LagProfileArea(1:numSrfs) 
         ProcAddLag = zero
         call GetInnerProduct(ProcAddLag, Dy(:,1:3), srfIDList, numSrfs)
         IPLagrange(1:numSrfs,4:6)=ProcAddLag(1:numSrfs,1:3)  
      endif
            
      do k = 1, numSrfs
         DiagonalDeltaSurf = zero
         DiagonalDeltaSurf = DiagonalDelta * LagMeanFlow(k)
         AddLag(k,1)=DiagonalDeltaSurf*
     &      (IPLagrange(k,4)-PQLagrange(k,1)*QLagrange(k,2)
     &      -QLagrange(k,1)*PQLagrange(k,2)
     &      +QLagrange(k,1)*QLagrange(k,2)*ProfileDelta(k))
         AddLag(k,2)=DiagonalDeltaSurf*IPLagrange(k,5)
         AddLag(k,3)=DiagonalDeltaSurf*IPLagrange(k,6)
      enddo  
  
      return
      end

c
c...initialize the coefficients for the impedance convolution
c
      subroutine CalcImpConvCoef (numISrfs, numTpoints)

      use convolImpFlow !uses flow history and impedance for convolution
      
      include "global.h" 
      include "common_blocks/timdat.h" !for alfi
C
C     Local variables
C
      INTEGER              j
C      
      integer numISrfs, numTpoints      

      allocate (ConvCoef(numTpoints+2,3)) !same time discret. for all imp. BC
      do j=1,numTpoints+2
         ConvCoef(j,:)=0.5/numTpoints !dt/2 divided by period T=N*dt
         ConvCoef(j,1)=ConvCoef(j,1)*(1.0-alfi)*(1.0-alfi)
         ConvCoef(j,2)=ConvCoef(j,2)*(1.0+2*alfi*(1.0-alfi))
         ConvCoef(j,3)=ConvCoef(j,3)*alfi*alfi
      enddo
      ConvCoef(1,2)=zero
      ConvCoef(1,3)=zero
      ConvCoef(2,3)=zero
      ConvCoef(numTpoints+1,1)=zero
      ConvCoef(numTpoints+2,2)=zero
      ConvCoef(numTpoints+2,1)=zero  
c
c...calculate the coefficients for the impedance convolution
c 
      allocate (ImpConvCoef(numTpoints+2,numISrfs))

c..try easiest convolution Q and Z constant per time step
      do j=3,numTpoints+1
         ImpConvCoef(j,:) = ValueListImp(j-1,:)/numTpoints
      enddo
      ImpConvCoef(1,:) =zero
      ImpConvCoef(2,:) =zero
      ImpConvCoef(numTpoints+2,:) = 
     &           ValueListImp(numTpoints+1,:)/numTpoints
c compensate for yalpha passed not y in Elmgmr()
      ImpConvCoef(numTpoints+1,:)= ImpConvCoef(numTpoints+1,:)
     &                  - ImpConvCoef(numTpoints+2,:)*(1.0-alfi)/alfi 
      ImpConvCoef(numTpoints+2,:)= ImpConvCoef(numTpoints+2,:)/alfi 
      return
      end

c
c...calculate the time varying coefficients for the RCR convolution
c
      subroutine CalcRCRConvCoef (stepn, numSrfs)

      use convolRCRFlow !brings in ValueListRCR, dtRCR
      
      include "global.h" 
      include "common_blocks/timdat.h" !brings alfi
C
C     Local variables
C
      INTEGER             j
C      
      integer numSrfs, stepn    

      RCRConvCoef = zero
      if (stepn .eq. 0) then
        RCRConvCoef(1,:) = ValueListRCR(1,:)*(1.0-alfi) +
     &   ValueListRCR(3,:)*(-alfi + 1.0 + 1/dtRCR(:) 
     &     - exp(-alfi*dtRCR(:))*(1 + 1/dtRCR(:)))
        RCRConvCoef(2,:) = ValueListRCR(1,:)*alfi 
     &     + ValueListRCR(3,:)
     &     *(alfi - 1/dtRCR(:) + exp(-alfi*dtRCR(:))/dtRCR(:))
      endif
      if (stepn .ge. 1) then
        RCRConvCoef(1,:) =-ValueListRCR(3,:)*exp(-dtRCR(:)*(stepn+alfi))
     &        *(1 + (1 - exp(dtRCR(:)))/dtRCR(:))
        RCRConvCoef(stepn+1,:) = ValueListRCR(1,:)*(1-alfi) 
     &     - ValueListRCR(3,:)*(alfi - 1 - 1/dtRCR(:) 
     &     + exp(-alfi*dtRCR(:))/dtRCR(:)*(2 - exp(-dtRCR(:))))
        RCRConvCoef(stepn+2,:) = ValueListRCR(1,:)*alfi 
     &     + ValueListRCR(3,:)
     &     *(alfi - 1/dtRCR(:) + exp(-alfi*dtRCR(:))/dtRCR(:))
      endif
      if (stepn .ge. 2) then
        do j=2,stepn
         RCRConvCoef(j,:) = ValueListRCR(3,:)/dtRCR(:)*
     &        exp(-dtRCR(:)*(stepn + alfi + 2 - j))*
     &        (1 - exp(dtRCR(:)))**2
        enddo
      endif

c compensate for yalpha passed not y in Elmgmr()
      RCRConvCoef(stepn+1,:)= RCRConvCoef(stepn+1,:)
     &                  - RCRConvCoef(stepn+2,:)*(1.0-alfi)/alfi 
      RCRConvCoef(stepn+2,:)= RCRConvCoef(stepn+2,:)/alfi 

      return
      end

c
c...calculate the time dependent H operator for the RCR convolution
c
      subroutine CalcHopRCR (timestepRCR, stepn, numSrfs)

      use convolRCRFlow !brings in HopRCR, dtRCR

      include "global.h"
      include "mpif.h" !needed?
      include "common_blocks/timdat.h"
C
      integer numSrfs, stepn      
      real*8  PdistCur(0:MAXSURF), timestepRCR
      
      HopRCR=zero
      call RCRint(timestepRCR*(stepn + alfi),PdistCur)
      HopRCR(1:numSrfs) = RCRic(1:numSrfs) 
     &     *exp(-dtRCR(1:numSrfs)*(stepn + alfi)) + PdistCur(1:numSrfs)
      return
      end

c
c.... This subroutine writes FlowHist.dat and PressHist.dat files
c
      subroutine UpdRCR(y, srfIDList, numSrfs)

      use convolRCRFlow 

        include "global.h"
        include "common_blocks/outpar.h"
        include "common_blocks/workfc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
C     
      real*8   y(nshg, ndof), NewP(0:MAXSURF)
      integer  srfIDList(0:MAXSURF),  numSrfs
      
      call integrScalar(NewP,y(:,4),srfIdList,numSrfs)
         PHistRCR(lstep+1,1:numSrfs)=NewP(1:numSrfs)/RCRArea(1:numSrfs)
      if ((mod(lstep, ntout) .eq. 0).and.
     &   (myrank .eq. zero)) then
         call OutputDataFile(QHistRCR(1:lstep+1,:),lstep+1,numSrfs,
     &      'QHistRCR.dat',870)
         call OutputDataFile(PHistRCR(1:lstep+1,:),lstep+1,numSrfs,
     &      'PHistRCR.dat',871)
      endif 

      return
      end


c 
c ... calculate initial conditions for the CalcSurfaces
c      
      subroutine calcCalcic(y,srfIdList,numSrfs)
      
      use calcFlowPressure
c
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
C      
      integer   srfIdList(0:MAXSURF), numSrfs, irankCoupled
      real*8    y(nshg,4)   !need velocity and pressure
      real*8    Qini(0:MAXSURF) !initial flow rate
      real*8    PdistIni(0:MAXSURF)!initial distal pressure
      real*8    Pini(0:MAXSURF),CoupleArea(0:MAXSURF) ! initial pressure
      real*8    VelOnly(nshg,3), POnly(nshg)
c
      POnly(:)= one ! one to get area
      call integrScalar(CoupleArea,POnly,srfIdList,numSrfs) !get surf area
      CalcArea(1:numSrfs) = CoupleArea(1:numSrfs)
      VelOnly(:,1:3)=y(:,1:3)
      call GetFlowQ(Qini,VelOnly,srfIdList,numSrfs) !get initial flow
      FlowHist(lstep+1,1:numSrfs)=Qini(1:numSrfs) !initialize QHistRCR
      POnly(:)=y(:,4) ! pressure
      call integrScalar(Pini,POnly,srfIdList,numSrfs) !get initial pressure integral
      Pini(1:numSrfs) = Pini(1:numSrfs)/CalcArea(1:numSrfs)
      PressHist(lstep+1,1:numSrfs)=Pini(1:numSrfs)
     
      return
      end
c 
c ... initialize the influence of the initial conditions for the RCR BC
c    
      subroutine calcRCRic(y,srfIdList,numSrfs)
      
      use convolRCRFlow    !brings RCRic, ValueListRCR, ValuePdist

        include "global.h" !needed?
        include "mpif.h" !needed?
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
C
      integer   srfIdList(0:MAXSURF), numSrfs, irankCoupled
      real*8    y(nshg,4)   !need velocity and pressure
      real*8    Qini(0:MAXSURF) !initial flow rate
      real*8    PdistIni(0:MAXSURF)!initial distal pressure
      real*8    Pini(0:MAXSURF),CoupleArea(0:MAXSURF) ! initial pressure
      real*8    VelOnly(nshg,3), POnly(nshg)

      allocate (RCRic(0:MAXSURF))
c      call RCRint(lstep,PdistIni) !get initial distal P 
      call RCRint(time,PdistIni) !get initial distal P 
      POnly(:)= one ! one to get area
      call integrScalar(CoupleArea,POnly,srfIdList,numSrfs) !get surf area
      RCRArea(1:numSrfs) = CoupleArea(1:numSrfs)
      if (lstep .eq. zero) then
         VelOnly(:,1:3)=y(:,1:3)
         call GetFlowQ(Qini,VelOnly,srfIdList,numSrfs) !get initial flow
         QHistRCR(1,1:numSrfs)=Qini(1:numSrfs) !initialize QHistRCR
         POnly(:)=y(:,4) ! pressure
         call integrScalar(Pini,POnly,srfIdList,numSrfs) !get initial pressure integral
         Pini(1:numSrfs) = Pini(1:numSrfs)/RCRArea(1:numSrfs)
         PHistRCR(1,1:numSrfs)=Pini(1:numSrfs)
         RCRic(1:numSrfs) = Pini(1:numSrfs) 
     &          - ValueListRCR(1,:)*Qini(1:numSrfs)-PdistIni(1:numSrfs)
      elseif (lstep .gt. zero) then
          RCRic(1:numSrfs) = PHistRCR(1,1:numSrfs) 
     &     -ValueListRCR(1,1:numSrfs)*QHistRCR(1,1:numSrfs)
     &     -PdistIni(1:numSrfs)
      endif
      
      return
      end


c
c.... This subroutine writes FlowHist.dat and PressHist.dat files
c
      subroutine Updcalc(y, srfIDList, numSrfs)

      use calcFlowPressure

        include "global.h"
        include "common_blocks/outpar.h"
        include "common_blocks/workfc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
C
      real*8   y(nshg, ndof), NewP(0:MAXSURF), NewQ(0:MAXSURF)
      integer  srfIDList(0:MAXSURF),  numSrfs

      call GetFlowQ(NewQ, y(:,1:3), srfIDList,numSrfs) !new flow at time n+1
      FlowHist(lstep+1,1:numSrfs) = NewQ(1:numSrfs)
      call integrScalar(NewP, y(:,4), srfIDList, numSrfs)
      PressHist(lstep+1,1:numSrfs) = NewP(1:numSrfs)/CalcArea(1:numSrfs)
      if ((mod(lstep, ntout) .eq. 0).and.
     &   (myrank .eq. zero)) then
c            CALL AppendDataFile(NewQ(1:numSrfs), srfIDList(1:numSrfs), 
c     2         numSrfs, 'FlowHist.dat', 1004)
c            CALL AppendDataFile(NewP(1:numSrfs), srfIDList(1:numSrfs), 
c     2         numSrfs, 'PressHist.dat', 1005)
         call OutputDataFile(FlowHist(1:lstep+1,:),lstep+1,numSrfs,
     &      'FlowHist.dat',1004)
         call OutputDataFile(PressHist(1:lstep+1,:),lstep+1,numSrfs,
     &      'PressHist.dat',1005)
      endif 

      return
      end

c
c.... This subroutine writes Lagrange Multipliers and errors in 
c.... LagrangeMultipliers.dat and LagrangeErrors.dat
c
      subroutine UpdateLagrangeCoef(y, col, row, srfIDList, numSrfs)

      use LagrangeMultipliers

        include "global.h"
        include "common_blocks/nomodule.h"
        include "common_blocks/outpar.h"
        include "common_blocks/workfc.h"
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      INTEGER            k
C      
      real*8   y(nshg, ndof)
      integer  col(nshg+1),           row(nnz_tot)
      integer  srfIDList(0:MAXSURF),  numSrfs, NumOfData
      real*8   Integral(0:MAXSURF),   InnerProduct(0:MAXSURF,3)
 
      Integral = zero     
      InnerProduct = zero   
      call GetFlowQ(Integral, y(:,1:3), srfIDList, numSrfs)  
      QLagrange(1:numSrfs,1)=Integral(1:numSrfs)
      Integral = zero
      call GetProfileFlowQ(Integral, y(:,1:3), srfIDList, numSrfs) 
      PQLagrange(1:numSrfs,1)=Integral(1:numSrfs)
     &   /LagProfileArea(1:numSrfs) 
      Integral = zero
      LagSwitch = 0 
      call CalcNANBLagrange(col, row, y(:,1:3))
      call GetInnerProduct(InnerProduct, y(:,1:3), srfIDList, numSrfs)
      IPLagrange(1:numSrfs,1:3)=InnerProduct(1:numSrfs,1:3)
      do k=1, numSrfs
         NumOfData = (k-1)*3+1
         LagErrorHist(lstep+1,NumOfData)=abs(IPLagrange(k,1)
     &      -two*QLagrange(k,1)*PQLagrange(k,1)
     &      +QLagrange(k,1)**2*ProfileDelta(k))
         LagErrorHist(lstep+1,NumOfData+1)=abs(IPLagrange(k,2))
         LagErrorHist(lstep+1,NumOfData+2)=abs(IPLagrange(k,3))
            LagErrorHist(lstep+1,NumOfData:NumOfData+2)=
     &      LagErrorHist(lstep+1,NumOfData:NumOfData+2)
     &      *LagMeanFlow(k)
         LagHist(lstep+1,NumOfData:NumOfData+2)=Lag(k,1:3)
      enddo    

      if ((mod(lstep, ntout) .eq. 0).and.
     &      (myrank .eq. zero)) then
         NumOfData = numLagrangeSrfs*3
         call OutputDataFile(LagHist(1:lstep+1,:),lstep+1, NumOfData,
     &      'LagrangeMultipliers.dat',801)
         call OutputDataFile(LagErrorHist(1:lstep+1,:),lstep+1,
     &      NumOfData,'LagrangeErrors.dat',802)
      endif
c
      return
      end  
c
c.... this function calculates an initial condition of a constrained surface
c
      subroutine calcLagrangeic(srfIDList, numSrfs)
c      
      use LagrangeMultipliers
      
      include "global.h" 
      include "common_blocks/timdat.h"
      include "common_blocks/conpar.h"
C
C     Local variables
C
      INTEGER             k
C
      integer  srfIDList(0:MAXSURF),  numSrfs,   NumOfData 
      
      LagSwitch = 0 
      allocate(lhsLagL(9,nnz_tot,3))
      allocate(resL(numSrfs,3))
      allocate(LagAPproduct(nshg,3))
      lhsLagL = zero
      resL = zero   
      LagAPproduct = zero
      call MergeLagrangeParameters(srfIDList, numSrfs)
      ProfileDelta(1:numSrfs)=ProfileDelta(1:numSrfs)
     &   /LagProfileArea(1:numSrfs)/LagProfileArea(1:numSrfs)
      do k=1, numSrfs
         LagMeanFlow(k)=two*LagProfileArea(k)/LagMeanFlow(k)
     &      /LagMeanFlow(k)
         if (lstep .eq. zero) then
            LagHist(1,(k-1)*3+1:(k-1)*3+3)=Lagold(k,1:3)
         elseif (lstep .gt. zero) then
            Lagold(k,1:3)=LagHist(lstep+1,(k-1)*3+1:(k-1)*3+3)
         endif
      enddo

      return 
      end

c
c.... this function calculates an initial condition of a constrained surface
c
      subroutine calcLagrangeErroric(y,  col,  row,  srfIDList, numSrfs)
c      
      use LagrangeMultipliers
      
        include "global.h"
        include "common_blocks/conpar.h"
        include "common_blocks/timdat.h"
C
C     Local variables
C
      INTEGER             k
C
      real*8   y(nshg, ndof)
      integer  srfIDList(0:MAXSURF),  numSrfs,   NumOfData 
      integer  col(nshg+1),           row(nnz_tot)
      real*8   Integral(0:MAXSURF),   InnerProduct(0:MAXSURF,3)
      
      Integral = zero     
      InnerProduct = zero   
      call GetFlowQ(Integral, y(:,1:3), srfIDList, numSrfs)  
      QLagrange(1:numSrfs,1)=Integral(1:numSrfs)
      Integral = zero
      call GetProfileFlowQ(Integral, y(:,1:3), srfIDList, numSrfs) 
      PQLagrange(1:numSrfs,1)=Integral(1:numSrfs)
     &   /LagProfileArea(1:numSrfs) 
      Integral = zero
      call CalcNANBLagrange(col, row, y(:,1:3))
      call GetInnerProduct(InnerProduct, y(:,1:3), srfIDList, numSrfs)
      IPLagrange(1:numSrfs,1:3)=InnerProduct(1:numSrfs,1:3)
      do k=1, numSrfs
         NumOfData = (k-1)*3+1
         LagErrorHist(lstep+1,NumOfData)=abs(IPLagrange(k,1)
     &      -two*QLagrange(k,1)*PQLagrange(k,1)
     &      +QLagrange(k,1)**2*ProfileDelta(k))
         LagErrorHist(lstep+1,NumOfData+1)=abs(IPLagrange(k,2))
         LagErrorHist(lstep+1,NumOfData+2)=abs(IPLagrange(k,3))
         LagErrorHist(lstep+1,NumOfData:NumOfData+2)=
     &      LagErrorHist(lstep+1,NumOfData:NumOfData+2)
     &      *LagMeanFlow(k)
      enddo

      return 
      end

c
c.... this function calculates an area and plane vectors of a constrained surface
c
      subroutine MergeLagrangeParameters(srfIDList, numSrfs)
c      
      use LagrangeMultipliers
c      
      include "global.h" 
      include "mpif.h"   
C
C     Local variables
C
      INTEGER             i,           ierr,        j,           k
C           
      integer  srfIDList(0:MAXSURF),  numSrfs 
      real*8   VectMag(3), Inplane1, Inplane2, Inplane3, InplaneNorm
      real*8, allocatable, dimension (:) :: TotalArea
      real*8, allocatable, dimension (:,:,:) :: InplaneVectors
c      
      allocate(TotalArea(numSrfs))
      allocate(InplaneVectors(3,3,numSrfs))
      TotalArea = zero
      InplaneVectors = zero
      call MPI_ALLREDUCE (LagProfileArea, TotalArea, numSrfs,
     &        MPI_DOUBLE_PRECISION,MPI_SUM, MPI_COMM_WORLD,ierr)  
      LagProfileArea(1:numSrfs)=TotalArea(1:numSrfs)
      TotalArea = zero
      call MPI_ALLREDUCE (ProfileDelta, TotalArea, numSrfs,
     &        MPI_DOUBLE_PRECISION,MPI_SUM, MPI_COMM_WORLD,ierr)  
      ProfileDelta(1:numSrfs)=TotalArea(1:numSrfs)
      InplaneVectors = zero
      
      do i=1,3
         do j=1,3
            call MPI_ALLREDUCE(LagInplaneVectors(i,j,:),
     &         InplaneVectors(i,j,:), numSrfs,  
     &         MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD,ierr)
         enddo
      enddo
      LagInplaneVectors = InplaneVectors
      do k=1, numSrfs
         do i=1,3
            VectMag(i)=sqrt(LagInplaneVectors(1,i,k)**2+
     &         LagInplaneVectors(2,i,k)**2+LagInplaneVectors(3,i,k)**2)
         enddo
         if ( VectMag(1) .gt. zero .and. VectMag(2) .gt. zero) then
            LagInplaneVectors(1:3,1,k) = LagInplaneVectors(1:3,1,k)
     &         /VectMag(1)
            LagInplaneVectors(1:3,2,k)=LagInplaneVectors(1:3,2,k)
     &         /VectMag(2)
            Inplane1=-LagInplaneVectors(2,2,k)*LagInplaneVectors(3,1,k)
     &         +LagInplaneVectors(2,1,k)*LagInplaneVectors(3,2,k)
            Inplane2=-LagInplaneVectors(1,1,k)*LagInplaneVectors(3,2,k)
     &         +LagInplaneVectors(1,2,k)*LagInplaneVectors(3,1,k)
            Inplane3=-LagInplaneVectors(1,2,k)*LagInplaneVectors(2,1,k)
     &         +LagInplaneVectors(1,1,k)*LagInplaneVectors(2,2,k)
            InplaneNorm=one/sqrt(Inplane1**2+Inplane2**2+Inplane3**2)
            LagInplaneVectors(1,3,k)=Inplane1*InplaneNorm
            LagInplaneVectors(2,3,k)=Inplane2*InplaneNorm
            LagInplaneVectors(3,3,k)=Inplane3*InplaneNorm
         endif
      enddo
c            
      return 
      end      
c  
c.........function that integrates a scalar over a boundary
c
      subroutine integrScalar(scalInt,scal,srfIdList,numSrfs)

      use pvsQbi !brings ndsurf, NASC

      include "global.h"
      include "mpif.h"
      include "common_blocks/conpar.h"
C
C     Local variables
C
      INTEGER             ierr
      INTEGER             npars
C 
      integer   srfIdList(0:MAXSURF), numSrfs, i, k
      real*8    scal(nshg), scalInt(0:MAXSURF), scalIntProc(0:MAXSURF)

      IF (numSrfs .EQ. 0) RETURN

      scalIntProc = zero
      do i = 1,nshg
         do k = 1,numSrfs
            if (srfIdList(k).eq.ndsurf(i)) then
               scalIntProc(k) = scalIntProc(k) + NASC(i)*scal(i)
            endif
         enddo
      enddo
c      
c     at this point, each scalint has its "nodes" contributions to the scalar
c     accumulated into scalIntProc. Note, because NASC is on processor this
c     will NOT be the scalar for the surface yet
c
c.... reduce integrated scalar for each surface, push on scalInt
c
        npars=MAXSURF+1
       call MPI_ALLREDUCE (scalIntProc, scalInt, npars,
     &        MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD,ierr)  
   
      return
      end

!> Function that outputs an input data array

      subroutine OutputDataFile(DataFile, nrows, ncolms, Filename,
     &   UnitNumber)

      include "global.h"
C
C     Local variables
C
      INTEGER             i,           n
C      
      character(len=*) Filename
      real*8    DataFile(nrows,ncolms)
      integer   nrows, ncolms, UnitNumber
      integer ioerr
      
      open(unit=UnitNumber, file=Filename,status='replace',iostat=ioerr)

      if (ioerr.eq.0) then
         write(UnitNumber,*) nrows
         do i=1, nrows
            write(UnitNumber,*) (DataFile(i,n),n=1, ncolms)
         enddo
      else
         write(*,*) 'IO_ERROR file: ',Filename,' code: ',ioerr
      endif

      close(UnitNumber)
   
      return
      end
