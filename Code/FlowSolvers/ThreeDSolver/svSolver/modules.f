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

!> Module for time averaged statistics (conservative projection).

      module stats
      
      integer nResDims 
      integer nSolDims 
      integer nLhsDims 
      integer nTimeStep 
      integer stsResFlg
      integer stsCompFreq
      integer stsWriteFreq 
      integer stsResetFreq 
      integer step1
      integer stsType
      
      real*8, allocatable :: stsVec(:,:)
      
      real*8, allocatable :: stsReg(:)
      real*8, allocatable :: stsMInv(:,:)
      real*8, allocatable :: stsB(:,:)
      real*8, allocatable :: stsDInv(:,:)
      real*8, allocatable :: stsCInv(:,:)
      
      real*8, allocatable :: stsPres(:)
      real*8, allocatable :: stsPresSqr(:) 
      real*8, allocatable :: stsVel(:,:)
      real*8, allocatable :: stsVelSqr(:,:)
      real*8, allocatable :: stsVelReg(:,:)
      real*8, allocatable :: stsStress(:,:)

      end module

!> Module readarrays ("Red Arrays") -- contains the arrays that
!! are read in from binary files but not immediately blocked 
!! through pointers.

      module readarrays
      
      real*8, allocatable :: point2x(:,:)
      integer, allocatable :: l2g(:)
      real*8, allocatable :: qold(:,:)
      real*8, allocatable :: uold(:,:)
#if(VER_VARWALL == 1)
c     variable wall thickness and Young Mod for each node, global,
      real*8, allocatable :: wallpropg(:,:)
#endif
      real*8, allocatable :: acold(:,:)
      integer, allocatable :: iBCtmp(:)
      real*8, allocatable :: BCinp(:,:)

      integer, allocatable :: point2ilwork(:)
      integer, allocatable :: nBC(:)
      integer, allocatable :: point2iper(:)
     
      end module

!> Natural pressure boundary condition can be calculated with p, the pressure,
!! related (in some prescribed manner) to Q, the flow rate, through the same 
!! boundary.  To do this efficiently requires us to precompute the integral 
!! of N_A over the boundary for each node A and store it in a vector of length
!! nshg (a bit wasteful since only the nodes on the boundary will be non zero
!! in this vector but it is probably slower to index it than to multiply and 
!! add the extra zeros....check later).

      module pvsQbi

      real*8, allocatable ::  NABI(:,:)
      real*8, allocatable ::  NASC(:)
      real*8, allocatable ::  PNABI(:,:)
      real*8, allocatable ::  NANBIJ(:,:,:)
      integer, allocatable :: ndsurf(:)
      
      end module

      module pointer_data
c
c.... maximum number of blocks
c
      INTEGER        MAXBLK2
         parameter ( MAXBLK2 = 5000 ) ! Note compiler was complaining 
c                                       because MAXBLK in common.h be careful
c    					to chang both places
c
c.... data type definitions
c
         type r1d
           real*8, pointer :: p(:)
         end type
c
         type r2d
           real*8, pointer :: p(:,:)
         end type
c
         type r3d
           real*8, pointer :: p(:,:,:)
         end type
c
         type i1d
           integer, pointer :: p(:)
         end type
c
         type i2d
           integer, pointer :: p(:,:)
         end type
c
         type i3d
           integer, pointer :: p(:,:,:)
         end type
c
c.... pointer declarations
c
         type (i1d), dimension(MAXBLK2) ::  mmat  
         type (i1d), dimension(MAXBLK2) ::  mmatb
         type (i2d), dimension(MAXBLK2) ::  mien
         type (i2d), dimension(MAXBLK2) ::  mienb  
         type (i2d), dimension(MAXBLK2) ::  miBCB
         type (r2d), dimension(MAXBLK2) ::  mxmudmi
#if(VER_VARWALL == 1)
         type (r2d), dimension(MAXBLK2) ::  wallpropelem
#endif
         type (r3d), dimension(MAXBLK2) ::  mBCB
c
         real*8, allocatable :: gmass(:)
       end module

c
c
c
      module periodicity       
      real*8, allocatable :: rcount(:)
      end module

c
c
c

      module local_mass
c
      INTEGER   MAXBLK2     
      parameter (MAXBLK2 = 5000)
c      
      integer :: iblock = 0
      integer :: have_local_mass = 0
c      
      type r2d
      real*8, pointer :: p(:,:,:)
      end type
c      
      type (r2d), dimension(MAXBLK2) :: lmassinv
c      
      end module

c
c
c
      module dtnmod
      integer, allocatable :: ifeature(:)
      end module

!> This module conveys temporal BC data.  Below functions read in the data
!! and interpolate it to the current time level. 

      module specialBC

      real*8, allocatable ::  BCt(:,:,:)
      real*8, allocatable ::  acs(:,:)
      real*8, allocatable ::  spamp(:)
      real*8, allocatable ::  ytarget(:,:)
      integer, allocatable :: nBCt(:) 
      integer, allocatable :: numBCt(:)
     
      integer ntv,nptsmax
      integer ntv_temp,nptsmax_temp
      integer ic

      end module
  
!> This module conveys flow rate history for the different impedance outlets
!! over one period. Below functions read in the data and store it for the
!! current time level. 

      module convolImpFlow

      real*8, allocatable ::  QHistImp(:,:)
      real*8, allocatable ::  ValueImpt(:,:,:)
      real*8, allocatable ::  ValueListImp(:,:)
      real*8, allocatable ::  ConvCoef(:,:)
      real*8, allocatable ::  ImpConvCoef(:,:) 
      real*8, allocatable ::  poldImp(:)
      integer ntimeptpT
      integer numDataImp
      integer, allocatable :: nImpt(:)
      integer, allocatable :: numImpt(:)
      integer nptsImpmax
      real*8, allocatable ::  QHistTry(:,:)
      real*8, allocatable ::  QHistTryF(:,:) !filter
      integer cutfreq !filter
      end module

!> This module conveys the parameters for the different RCR outlets.
!! Below functions read in the inputs (proximal resistance, capacitance, 
!! distal resistance and distal pressure) and store it for the
!! current time level. 

      module convolRCRFlow

      real*8, allocatable ::  ValueListRCR(:,:)
      real*8, allocatable ::  ValuePdist(:,:,:) !inputs
      real*8, allocatable ::  QHistRCR(:,:)
      real*8, allocatable ::  PHistRCR(:,:)
      real*8, allocatable ::  HopRCR(:) !calc
      real*8, allocatable ::  RCRConvCoef(:,:)
      real*8, allocatable ::  poldRCR(:) !calc
      real*8, allocatable ::  dtRCR(:)
      real*8, allocatable ::  RCRArea(:) !scaled timestep: deltat/RdC
      real*8, allocatable ::  RCRic(:) !(P(0)-RQ(0)-Pd(0))
      integer nptsRCRmax
      integer numDataRCR
      integer nptsRCR !to read inputs
      integer, allocatable :: numRCRt(:) !to read inputs
      end module

!> This module conveys the parameters to save flow and pressure history.

      module calcFlowPressure

      real*8, allocatable  :: FlowHist(:,:)         
      real*8, allocatable  :: PressHist(:,:)
      real*8, allocatable  :: CalcArea(:)
      end module

!> This module conveys the parameters to save residuals.

      module ResidualControl 

      real*8   controlResidual
      integer  CurrentIter
      end module

!> This module conveys parameters for Lagrange multipliers. Below 
!! function reads in the inputs (LagCenter, LagRadius and ProfileOrder).
!! Defined variables are used to construct LHS and RHS of the solver. 

      MODULE LagrangeMultipliers

      real*8, allocatable  :: QLagrange(:,:)     
      real*8, allocatable  :: PQLagrange(:,:)
      real*8, allocatable  :: NANBLagrange(:,:,:)  
      real*8, allocatable  :: IPLagrange(:,:)   
      real*8, allocatable  :: Lag(:,:)             
      real*8, allocatable  :: Lagold(:,:)
      real*8, allocatable  :: Lagincr(:,:)        
      real*8, allocatable  :: Lagalpha(:,:)
      real*8, allocatable  :: LagCenter(:,:)      
      real*8, allocatable  :: LagRadius(:)
      real*8, allocatable  :: ProfileDelta(:)      
      real*8, allocatable  :: LagProfileArea(:)
      real*8, allocatable  :: loclhsLag(:,:,:,:,:) 
      real*8, allocatable  :: lhsLagL(:,:,:)
      real*8, allocatable  :: LagErrorHist(:,:)   
      real*8, allocatable  :: LagHist(:,:)
      real*8, allocatable  :: PenaltyCoeff(:,:)    
      real*8, allocatable  :: Penalty(:,:)
      real*8, allocatable  :: ScaleFactor(:,:)   
      real*8, allocatable  :: AddLag(:,:)
      real*8, allocatable  :: LagAPproduct(:,:)   
      real*8, allocatable  :: resL(:,:)
      real*8, allocatable  :: LagInplaneVectors(:,:,:)
      real*8, allocatable  :: LagMeanFlow(:)
      integer, allocatable :: ProfileOrder(:) 
      integer LagSwitch  
      end module

