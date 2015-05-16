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
c----------------------------------------------------------------------
c
c.... common /matpar/   : material constants
c
c pr            : Prandtl number
c Planck        : Planck's constant
c Stefan        : Stefan's constant (for radiation)
c Nh            : Avogadro's number
c Rh            : universal gas constant
c Rgas          : specific gas constant
c gamma         : specific heat ratio
c gamma1        : gamma - 1
c s0            : reference specific entropy
c const         : special constant
c xN2           : mole fraction of diatomic nitrogen
c xO2           : mole fraction of diatomic oxygen
c yN2           : mole fraction of diatomic nitrogen
c yO2           : mole fraction of diatomic oxygen
c Msh  (5)      : molar mass of species
c cpsh (5)      : molar heat at constant pressure of species
c s0sh (5)      : molar reference entropy of species
c h0sh (5)      : molar heat of formation of species
c Rs   (5)      : specific gas constant of species
c cps  (5)      : specific heat at constant pressure of species
c cvs  (5)      : specific heat at constant volume of species
c h0s  (5)      : specific heat of formation of species
c Trot (5)      : characteristic rotational temperature of species
c sigs (5)      : symmetry factor of species
c Tvib (5)      : characteristic vibrational temperature of species
c g0s  (5)      : ground degeneracy of electronic energy
c dofs (5)      : degrees of freedom of species
c ithm          : thermodynamic property flag
c
c----------------------------------------------------------------------
c
        REAL*8 Nh, Msh(5),pr,Planck, Stefan, Rh, Rgas, gamma,  gamma1, s0
        REAL*8 const,xN2,xO2,yN2,yO2,cpsh(5),s0sh(5),h0sh(5),Rs(5), cps(5)
        REAL*8 cvs(5), h0s(5), Trot(5),sigs(5),Tvib(5),g0s(5), dofs(5)
        INTEGER ithm
        common /mmatpar/ pr,     Planck, Stefan, Nh,     Rh,     Rgas,
     &                  gamma,  gamma1, s0,     const,  xN2,    xO2,
     &                  yN2,    yO2,    Msh, cpsh,s0sh,h0sh,
     &                  Rs,  cps, cvs, h0s, Trot,sigs,
     &                  Tvib,g0s, dofs,ithm
