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

ifeq ($(CLUSTER), x64_linux)
    ITK_SRC_DIR	= $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/itk-4.7.1
    ITK_BIN_DIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/itk-4.7.1
    ITK_BUILD_DIR = $(OPEN_SOFTWARE_BUILDS_TOPLEVEL)/itk-4.7.1
#    ITK_DEFS = -D ITK_IO_FACTORY_REGISTER_MANAGER -D ITK_USE_PTHREADS
    ITK_DEFS = -D ITK_USE_PTHREADS
    ITK_INCLUDE_DIR_BASE = $(ITK_BIN_DIR)/include/ITK-4.7
    ITK_LIBDIRS = $(ITK_BIN_DIR)/lib
    ITK_BIN_LIBS = $(ITK_BIN_DIR)/lib

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
$(LIBFLAG)ITKBiasCorrection-4.7$(LIBLINKEXT)     $(LIBFLAG)ITKIOJPEG-4.7$(LIBLINKEXT)                    $(LIBFLAG)ITKOptimizers-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKBioCell-4.7$(LIBLINKEXT)            $(LIBFLAG)ITKIOLSM-4.7$(LIBLINKEXT)                     $(LIBFLAG)ITKPath-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKCommon-4.7$(LIBLINKEXT)             $(LIBFLAG)ITKIOMesh-4.7$(LIBLINKEXT)                    $(LIBFLAG)itkpng-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKDICOMParser-4.7$(LIBLINKEXT)        $(LIBFLAG)ITKIOMeta-4.7$(LIBLINKEXT)                    $(LIBFLAG)ITKPolynomials-4.7$(LIBLINKEXT) \
$(LIBFLAG)itkdouble-conversion-4.7$(LIBLINKEXT)  $(LIBFLAG)ITKIOMRC-4.7$(LIBLINKEXT)                     $(LIBFLAG)ITKQuadEdgeMesh-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKEXPAT-4.7$(LIBLINKEXT)              $(LIBFLAG)ITKIONIFTI-4.7$(LIBLINKEXT)                   $(LIBFLAG)ITKSpatialObjects-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKFEM-4.7$(LIBLINKEXT)                $(LIBFLAG)ITKIONRRD-4.7$(LIBLINKEXT)                    $(LIBFLAG)ITKStatistics-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOPNG-4.7$(LIBLINKEXT)                     $(LIBFLAG)itksys-4.7$(LIBLINKEXT)        \
$(LIBFLAG)ITKIOSiemens-4.7$(LIBLINKEXT)                 \
$(LIBFLAG)ITKIOSpatialObjects-4.7$(LIBLINKEXT)          \
$(LIBFLAG)ITKIOStimulate-4.7$(LIBLINKEXT)               \
$(LIBFLAG)ITKIOTIFF-4.7$(LIBLINKEXT)                    $(LIBFLAG)itktiff-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOTransformBase-4.7$(LIBLINKEXT)           $(LIBFLAG)itkv3p_lsqr-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOTransformHDF5-4.7$(LIBLINKEXT)           $(LIBFLAG)itkv3p_netlib-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOTransformInsightLegacy-4.7$(LIBLINKEXT)  $(LIBFLAG)itkvcl-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKgiftiio-4.7$(LIBLINKEXT)            $(LIBFLAG)ITKIOTransformMatlab-4.7$(LIBLINKEXT)         $(LIBFLAG)ITKVideoCore-4.7$(LIBLINKEXT) \
           $(LIBFLAG)ITKIOVTK-4.7$(LIBLINKEXT)                     $(LIBFLAG)ITKVideoIO-4.7$(LIBLINKEXT) \
           $(LIBFLAG)ITKIOXML-4.7$(LIBLINKEXT)                     $(LIBFLAG)itkvnl_algo-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOBioRad-4.7$(LIBLINKEXT)           $(LIBFLAG)itkjpeg-4.7$(LIBLINKEXT)                      $(LIBFLAG)itkvnl-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOBMP-4.7$(LIBLINKEXT)              $(LIBFLAG)ITKKLMRegionGrowing-4.7$(LIBLINKEXT)          $(LIBFLAG)ITKVNLInstantiation-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOCSV-4.7$(LIBLINKEXT)              $(LIBFLAG)ITKLabelMap-4.7$(LIBLINKEXT)                  $(LIBFLAG)ITKVTK-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOGDCM-4.7$(LIBLINKEXT)             $(LIBFLAG)ITKMesh-4.7$(LIBLINKEXT)                      $(LIBFLAG)ITKVtkGlue-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOGE-4.7$(LIBLINKEXT)               $(LIBFLAG)ITKMetaIO-4.7$(LIBLINKEXT)                    $(LIBFLAG)ITKWatersheds-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOGIPL-4.7$(LIBLINKEXT)             $(LIBFLAG)itkNetlibSlatec-4.7$(LIBLINKEXT)              $(LIBFLAG)itkzlib-4.7$(LIBLINKEXT) \
        $(LIBFLAG)ITKniftiio-4.7$(LIBLINKEXT)                   $(LIBFLAG)ITKznz-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOImageBase-4.7$(LIBLINKEXT)        $(LIBFLAG)ITKNrrdIO-4.7$(LIBLINKEXT) \
$(LIBFLAG)ITKIOIPL-4.7$(LIBLINKEXT)              \
$(LIBFLAG)itkhdf5-4.7$(LIBLINKEXT) $(LIBFLAG)itkhdf5_cpp-4.7$(LIBLINKEXT) $(LIBFLAG)ITKIOHDF5-4.7$(LIBLINKEXT) $(LIBFLAG)itkhdf5-4.7$(LIBLINKEXT) \
$(ITK_SYS_LIBS)
endif

#

# conflicts with vtkhdf5 library itkhdf5-4.7.lib

