# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

ifeq ($(CLUSTER), x64_cygwin)
    ITK_SRC_DIR	= $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/itk-4.7.1
    ITK_BIN_DIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/itk-4.7.1
    ITK_BUILD_DIR = $(OPEN_SOFTWARE_BUILDS_TOPLEVEL)/itk-4.7.1
#    ITK_DEFS = -D ITK_IO_FACTORY_REGISTER_MANAGER -D MSVC
    ITK_DEFS = -D MSVC
    ITK_INCLUDE_DIR_BASE = $(ITK_BIN_DIR)/include/ITK-4.7
    ITK_LIBDIRS = $(ITK_BIN_DIR)/lib
    ITK_BIN_LIBS = $(ITK_BIN_DIR)/bin

    ITK_INCDIRS = \
      -I$(ITK_INCLUDE_DIR_BASE) \
      -I$(ITK_INCLUDE_DIR_BASE)/itkfdstream \
      -I$(ITK_INCLUDE_DIR_BASE)/itkhdf5 \
      -I$(ITK_INCLUDE_DIR_BASE)/itkjpeg \
      -I$(ITK_INCLUDE_DIR_BASE)/itkpng \
      -I$(ITK_INCLUDE_DIR_BASE)/itkzlib \
      -I$(ITK_INCLUDE_DIR_BASE)/vnl \
      -I$(ITK_BUILD_DIR)/Modules/IO/ImageBase \
      -I$(ITK_BUILD_DIR)/Examples/ITKIOFactoryRegistration

   ITK_LIBS =    $(LIBPATH_COMPILER_FLAG)$(ITK_LIBDIRS) \
$(LIBFLAG)ITKBiasCorrection-4.7.lib     $(LIBFLAG)ITKIOJPEG-4.7.lib                    $(LIBFLAG)ITKOptimizers-4.7.lib \
$(LIBFLAG)ITKBioCell-4.7.lib            $(LIBFLAG)ITKIOLSM-4.7.lib                     $(LIBFLAG)ITKPath-4.7.lib \
$(LIBFLAG)ITKCommon-4.7.lib             $(LIBFLAG)ITKIOMesh-4.7.lib                    $(LIBFLAG)itkpng-4.7.lib \
$(LIBFLAG)ITKDICOMParser-4.7.lib        $(LIBFLAG)ITKIOMeta-4.7.lib                    $(LIBFLAG)ITKPolynomials-4.7.lib \
$(LIBFLAG)itkdouble-conversion-4.7.lib  $(LIBFLAG)ITKIOMRC-4.7.lib                     $(LIBFLAG)ITKQuadEdgeMesh-4.7.lib \
$(LIBFLAG)ITKEXPAT-4.7.lib              $(LIBFLAG)ITKIONIFTI-4.7.lib                   $(LIBFLAG)ITKSpatialObjects-4.7.lib \
$(LIBFLAG)ITKFEM-4.7.lib                $(LIBFLAG)ITKIONRRD-4.7.lib                    $(LIBFLAG)ITKStatistics-4.7.lib \
$(LIBFLAG)ITKIOPNG-4.7.lib                     $(LIBFLAG)itksys-4.7.lib \
$(LIBFLAG)ITKIOSiemens-4.7.lib                 \
$(LIBFLAG)ITKIOSpatialObjects-4.7.lib          \
$(LIBFLAG)ITKIOStimulate-4.7.lib               \
$(LIBFLAG)ITKIOTIFF-4.7.lib                    $(LIBFLAG)itktiff-4.7.lib \
$(LIBFLAG)ITKIOTransformBase-4.7.lib           $(LIBFLAG)itkv3p_lsqr-4.7.lib \
$(LIBFLAG)ITKIOTransformHDF5-4.7.lib           $(LIBFLAG)itkv3p_netlib-4.7.lib \
$(LIBFLAG)ITKIOTransformInsightLegacy-4.7.lib  $(LIBFLAG)itkvcl-4.7.lib \
$(LIBFLAG)ITKgiftiio-4.7.lib            $(LIBFLAG)ITKIOTransformMatlab-4.7.lib         $(LIBFLAG)ITKVideoCore-4.7.lib \
$(LIBFLAG)itkhdf5_cpp-4.7.lib           $(LIBFLAG)ITKIOVTK-4.7.lib                     $(LIBFLAG)ITKVideoIO-4.7.lib \
$(LIBFLAG)itkhdf5-4.7.lib               $(LIBFLAG)ITKIOXML-4.7.lib                     $(LIBFLAG)itkvnl_algo-4.7.lib \
$(LIBFLAG)ITKIOBioRad-4.7.lib           $(LIBFLAG)itkjpeg-4.7.lib                      $(LIBFLAG)itkvnl-4.7.lib \
$(LIBFLAG)ITKIOBMP-4.7.lib              $(LIBFLAG)ITKKLMRegionGrowing-4.7.lib          $(LIBFLAG)ITKVNLInstantiation-4.7.lib \
$(LIBFLAG)ITKIOCSV-4.7.lib              $(LIBFLAG)ITKLabelMap-4.7.lib                  $(LIBFLAG)ITKVTK-4.7.lib \
$(LIBFLAG)ITKIOGDCM-4.7.lib             $(LIBFLAG)ITKMesh-4.7.lib                      $(LIBFLAG)ITKVtkGlue-4.7.lib \
$(LIBFLAG)ITKIOGE-4.7.lib               $(LIBFLAG)ITKMetaIO-4.7.lib                    $(LIBFLAG)ITKWatersheds-4.7.lib \
$(LIBFLAG)ITKIOGIPL-4.7.lib             $(LIBFLAG)itkNetlibSlatec-4.7.lib              $(LIBFLAG)itkzlib-4.7.lib \
$(LIBFLAG)ITKIOHDF5-4.7.lib             $(LIBFLAG)ITKniftiio-4.7.lib                   $(LIBFLAG)ITKznz-4.7.lib \
$(LIBFLAG)ITKIOImageBase-4.7.lib        $(LIBFLAG)ITKNrrdIO-4.7.lib \
$(LIBFLAG)ITKIOIPL-4.7.lib              $(LIBFLAG)itkopenjpeg-4.7.lib

endif

# only needed if you are using itk gdcm
#

#$(LIBFLAG)itkgdcmCommon-4.7.lib
#$(LIBFLAG)itkgdcmDICT-4.7.lib
#$(LIBFLAG)itkgdcmDSED-4.7.lib
#$(LIBFLAG)itkgdcmIOD-4.7.lib
#$(LIBFLAG)itkgdcmjpeg12-4.7.lib
#$(LIBFLAG)itkgdcmjpeg16-4.7.lib
#$(LIBFLAG)itkgdcmjpeg8-4.7.lib
#$(LIBFLAG)itkgdcmMSFF-4.7.lib

#-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/DataStructureAndEncodingDefinition \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/MessageExchangeDefinition \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/InformationObjectDefinition \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/Common \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/DataDictionary \
-I$(ITK_SRC_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/MediaStorageAndFileFormat \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/GDCM/src/gdcm/Source/Common \
-I$(ITK_BIN_DIR)/Modules/ThirdParty/GDCM \
