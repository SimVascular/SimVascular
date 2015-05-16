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

c----------------------------------------------------------------------
c
c.... common /elmpar/   : element parameters
c
c lelCat        : element category (P1, Q1, P2, Q2, etc.)
c lcsyst        : element coordinate system
c iorder        : element order (=k for Pk and Qk)
c nenb          : number of element nodes per boundary sides
c maxsh         : total number integration points
c maxshb        : total number integration points of boundary elements
c nelblk        : number of element blocks
c nelblb        : number of boundary element blocks
c ndofl         : number of degrees of freedom (for current block)
c nsymdl        : number of d.o.f for symm. storage (for current block)
c nenl          : number of element nodes (for current block)
c nfacel        : number of element faces (for current block)
c nenbl         : number of boundary element nodes
c intind        : integration data index
c nintg         : number of integration points
c mattyp        : material type ( = 0 for fluid; = 1 for solid )
c
c----------------------------------------------------------------------
c       \\Common Block variables for "elmpar"
        INTEGER lelCat,lcsyst,iorder,nenb,nelblk,nelblb,ndofl
        INTEGER nsymdl,nenl,nfacel,nenbl,intind, mattyp
        common /elmpar/ lelCat, lcsyst, iorder, nenb,
     &                  nelblk, nelblb, ndofl,  nsymdl, nenl, nfacel,
     &                  nenbl,  intind, mattyp
