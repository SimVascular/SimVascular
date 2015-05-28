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

c      \\Common Block variables for "no module"

       REAL*8 bcttimescale,ValueListResist(0:MAXSURF),rhovw,thicknessvw,
     & evw,rnuvw
       REAL*8 rshearconstantvw, betai,rescontrol,ResCriteria,
     & backflowstabcoef
       INTEGER icardio, itvn, ipvsq, numResistSrfs,
     & nsrflistResist(0:MAXSURF)
       INTEGER numImpSrfs, nsrflistImp(0:MAXSURF),impfile,numRCRSrfs,
     & nsrflistRCR(0:MAXSURF)
       INTEGER icorfile,numCORSrfs,nsrflistCOR(0:MAXSURF)
       INTEGER ircrfile,numVisFluxSrfs, nsrflistVisFlux(0:MAXSURF),
     & numCalcSrfs, nsrflistCalc(0:MAXSURF)
       INTEGER Lagrange, numLagrangeSrfs,nsrflistLagrange(0:MAXSURF),
     & iLagfile
       INTEGER MinNumIter,ideformwall, ivarwallprop
       INTEGER applyWallDeformation
       INTEGER iwallmassfactor,iwallstiffactor,
     & nProps
       INTEGER iGenInitialization,iGenFromFile
       INTEGER numNeumannSrfs,nsrflistNeumann(0:MAXSURF)
       INTEGER numDirichletSrfs,nsrflistDirichlet(0:MAXSURF)
       INTEGER numNormalSrfs,nsrflistNormal(0:MAXSURF)

        common /nomodule/ bcttimescale,ValueListResist,
     &            rhovw, thicknessvw, evw, rnuvw,
     &            rshearconstantvw, betai,rescontrol,ResCriteria,
     &            backFlowStabCoef, icardio, itvn, ipvsq,
     &            numResistSrfs, nsrflistResist,
c                 ADDED FOR CONSISTENCY - CLOSED LOOP
     &            numNeumannSrfs,nsrflistNeumann,
     &            iGenInitialization,iGenFromFile,
     &            numDirichletSrfs,nsrflistDirichlet,
c                 ===================================
     &            numImpSrfs, nsrflistImp,impfile,
     &            numRCRSrfs, nsrflistRCR,ircrfile,
     &            numCORSrfs,nsrflistCOR,icorfile,
     &            numVisFluxSrfs, nsrflistVisFlux,
     &            numCalcSrfs, nsrflistCalc,
c                 ADDED FOR CONSISTENCY - CLOSED LOOP
     &            numNormalSrfs,nsrflistNormal,
c                 ===================================
     &            Lagrange, numLagrangeSrfs,
     &            nsrflistLagrange,iLagfile,
     &            MinNumIter,
     &            ideformwall, applyWallDeformation,
c                 ADDED FOR CONSISTENCY - VARWALL
     &            ivarwallprop, 
c                 ===============================
     &            iwallmassfactor,
     &            iwallstiffactor, nProps
