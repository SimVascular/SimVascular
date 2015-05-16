c     UC Copyright Notice
c
c     This software is Copyright (c) 2014-2015 The Regents of the 
c     University of California. All Rights Reserved.
c
c     Permission to copy and modify this software and its documentation
c     for educational, research and non-profit purposes, without fee, 
c     and without a written agreement is hereby granted, provided that
c     the above copyright notice, this paragraph and the following three
c     paragraphs appear in all copies.
c
c     Permission to make commercial use of this software may be obtained
c     by contacting:
c
c     Technology Transfer Office
c     9500 Gilman Drive, Mail Code 0910
c     University of California
c     La Jolla, CA 92093-0910
c     (858) 534-5815
c     invent@ucsd.edu
c
c     This software program and documentation are copyrighted by The
c     Regents of the University of California. The software program and
c     documentation are supplied "as is", without any accompanying
c     services from The Regents. The Regents does not warrant that the
c     operation of the program will be uninterrupted or error-free. The
c     end-user understands that the program was developed for research
c     purposes and is advised not to rely exclusively on the program for
c     any reason.
c
c     IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY 
c     PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL 
c     DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS 
c     SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF 
c     CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
c     THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY 
c     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
c     OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE 
c     SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE 
c     UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO PROVIDE 
c     MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

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

!> This routine computes and assembles the data corresponding to the
!! boundary elements.

      subroutine AsBNABI ( x,       shpb,
     &                   ienb,  iBCB)

       use pvsQbi
       
       include "global.h"
       include "common_blocks/conpar.h"
       include "common_blocks/elmpar.h"
       include "common_blocks/genpar.h"
       include "common_blocks/intpt.h"
       include "common_blocks/nomodule.h"
       include "common_blocks/propar.h"
       include "common_blocks/shpdat.h"
c..............................Declaration...............................................

       INTEGER ienb,iBCB,iel,iface,ipt3,k,lnode,n,nodlcl
       REAL*8 x,shpb,bnorm,dxdxib,rl,sgn,shdrv,shglb,shpfun,temp,temp1,
     &        temp2,temp3,v1,v2
       REAL*8 WdetJb,xlb

c........................................................................................

#if (VER_CLOSEDLOOP == 1)
        INTEGER i
        LOGICAL flagDirichlet
#endif

        dimension xlb(npro,nenl,nsd),    bnorm(npro,nsd),
     &            rl(npro,nshl,nsd),     WdetJb(npro)

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
c.... get the boundary element residuals
c
        rl  = zero
c
c.... compute the nodes which lie on the boundary (hierarchic)
c
        call getbnodes(lnode)
c
c.... loop through the integration points
c
        ngaussb = nintb(lcsyst)

        
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

#if (VER_CLOSEDLOOP == 1)
           DO iel=1,npro
              bnorm(iel,1:3)=bnorm(iel,1:3)*WdetJb(iel)
              IF (.NOT.btest(iBCB(iel,1),1)) THEN
                 flagDirichlet = .TRUE.
                 DO i=1, numDirichletSrfs
                    IF (iBCB(iel,2) .EQ. nsrflistDirichlet(i)) THEN
                       flagDirichlet = .FALSE.
                    END IF
                 END DO
                 IF (flagDirichlet) THEN
                    bnorm(iel,:) = zero
                 END IF
              END IF
           END DO
#else
           do iel=1,npro
              if (btest(iBCB(iel,1),1)) then 
                 bnorm(iel,1:3)=bnorm(iel,1:3)*WdetJb(iel)
              else
                 bnorm(iel,:) = zero  ! we want zeros where we are not integrating
              endif
           enddo
#endif


c
c  Now lets calculate Integral N_(a:e)^i n_i d Gamma
c
c
           do n = 1, nshlb
              nodlcl = lnode(n)
         rl(:,nodlcl,1) = rl(:,nodlcl,1) + shpfun(:,nodlcl) * bnorm(:,1)
         rl(:,nodlcl,2) = rl(:,nodlcl,2) + shpfun(:,nodlcl) * bnorm(:,2)
         rl(:,nodlcl,3) = rl(:,nodlcl,3) + shpfun(:,nodlcl) * bnorm(:,3) 
         
           enddo

        enddo  ! quadrature point loop
c
c.... assemble the NABI vector
c
        call local (NABI,    rl,     ienb,   3,  'scatter ')
c
c     push the surf number which we have associated with boundary
C     elements up to the global level in the array ndsurf
c
        do iel=1,npro
           if (iBCB(iel,2) .ne. 0) then
              iface = iBCB(iel,2)
              ndsurf(ienb(iel,1:nshlb))=iface   
           endif
           
           if (numVisFluxSrfs .gt. zero) then
              do k=1, numVisFluxSrfs
                 if (iBCB(iel,2) .eq. nsrflistVisFlux(k)) then
                    iBCB(iel,1) = 6
                 endif
              enddo
           endif
        enddo
c     
c.... end
c
        return
        end

