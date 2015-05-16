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
c.... common /genpar/   : control parameters
c
c E3nsd         : NSD .eq. 3 flag; 0. for 2D, 1. for 3D
c I3nsd         : NSD .eq. 3 flag; 0  for 2D, 1  for 3D
c nsymdf        : number of d.o.f.'s in symm. storage (= ndof*(ndof+1)/2)
c ndofBC        : dimension size of the boundary condition array BC
c ndiBCB        : dimension size of the boundary condition array iBCB
c ndBCB         : dimension size of the boundary condition array BCB
c Jactyp        : Jacobian type flag
c jump          : jump term computation flag
c ires          : residual type computation flag
c iprec         : block-diagonal preconditioner flag
c iprev         : ypl array allocation flag
c ibound        : boundary element flag
c idiff         : diffusive flux vector flag
c                 ( = 0 not used; = 1 global reconstruction )
c itau          : type of tau to be used
c
c----------------------------------------------------------------------
c       \\Common Block variables for "genpar"
c
        INTEGER EntropyPressure,I3nsd,nsymdf, ndofBC, ndiBCB, ndBCB
        INTEGER Jactyp,jump,ires,iprec,iprev,ibound,idiff,lhs,itau
        INTEGER ipord, ipred,lstres,iepstm,ibksiz, iabc, isurf,idflx
        REAL*8 E3nsd,dtsfct, taucfct, Bo
        common /genpar/ E3nsd,  I3nsd,  nsymdf, ndofBC, ndiBCB, ndBCB,
     &                  Jactyp, jump,   ires,   iprec,  iprev,  ibound,
     &                  idiff,  lhs,    itau,   ipord,  ipred,  lstres,
     &                  iepstm, dtsfct, taucfct, ibksiz, iabc, isurf,
     &                  idflx,  Bo,     EntropyPressure
