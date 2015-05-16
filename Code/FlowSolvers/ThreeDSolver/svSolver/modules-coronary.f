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

#include "cvFlowsolverOptions.h"

#if (VER_CORONARY == 1)

!> This module conveys the parameters for the different Coronary outlets.
!! Below functions read in the inputs (coronary resistances, capacitances,
!! and left ventricular pressure) and store it for the current time level.

      module convolCORFlow

      real*8, allocatable  :: ValuePlvist(:,:,:),    PlvHistCOR(:,:)
      real*8, allocatable  :: QHistCOR(:,:),         HopCOR(:) 
      real*8, allocatable  :: poldCOR(:),            plvoldCOR(:)
      real*8, allocatable  :: dQinidT(:),            dPinidT(:)
      real*8, allocatable  :: CORArea(:),            PHistCOR(:,:)
c      complex, allocatable :: COR(:,:),              dtCOR(:,:) 
c      complex, allocatable :: CORic(:,:),            ValueListCOR(:,:) 
c      complex, allocatable :: CoefCOR(:,:),          DetCOR(:) 

c     TEST DES - 28JAN2014
      real*8, allocatable :: COR(:,:),              dtCOR(:,:) 
      real*8, allocatable :: CORic(:,:),            ValueListCOR(:,:) 
      real*8, allocatable :: CoefCOR(:,:),          DetCOR(:) 

      real*8, allocatable  :: CORConvCoef(:,:),      CORPlvConvCoef(:,:)
c      real*8, allocatable  :: CORScaleFactor(:)
      integer, allocatable :: numCORt(:)
      integer nptsCORmax, numDataCOR, nptsCOR
      end module
#endif
