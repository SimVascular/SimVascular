# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved. 
#
# Portions of the code Copyright (c) 2009-2011 Open Source Medical
# Software Corporation, University of California, San Diego.
#
# Portions Copyright (c) 2000-2007, Stanford University, 
#    Rensselaer Polytechnic Institute, Kenneth E. Jansen, 
#    Charles A. Taylor (see SimVascular Acknowledgements file 
#    for additional contributors to the source code).
#
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions 
# are met:
#
# Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer. 
# Redistributions in binary form must reproduce the above copyright 
# notice, this list of conditions and the following disclaimer in the 
# documentation and/or other materials provided with the distribution. 
# Neither the name of the Stanford University or Rensselaer Polytechnic
# Institute nor the names of its contributors may be used to endorse or
# promote products derived from this software without specific prior 
# written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
# AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
# THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(MESHSIM_INCDIR) $(PARASOLID_INCDIR) $(SOLVERIO_INCDIR) \
           $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) $(ZLIB_INCDIR) -DCV_STATIC_LINK

FFLAGS   = $(GLOBAL_FFLAGS)

HDRS	= MeshSimInternal_phAdapt.h \
          cvAdaptCore.h cvAdaptHelp.h

CXXSRCS = cvAdaptCore.cxx main.cxx \
	  cvAdaptHelpers.cxx cvAdaptSF.cxx

FSRCS   = pythag.f tred2EISPACK.f tql2.f lubksb.f ludcmp.f

SRCS	= $(CXXSRCS) $(FSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS    = $(addprefix $(BUILD_DIR)/,$(CXXSRCS:.cxx=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(CSRCS:.c=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(FSRCS:.f=.$(OBJECTEXT)))

TARGET_EXE_FULL = $(TOP)/Bin/adaptor-$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION).exe
TARGET_EXE = $(TOP)/Bin/adaptor.exe

#intentionally link against static library
ifeq ($(MAKE_WITH_PARASOLID),1)
  PARASOLID_LIB_FOR_LINKING = $(SVLIBFLAG)_lib_simvascular_parasolid$(LIBLINKEXT)
endif

static: directories $(TARGET_EXE_FULL)

directories:
	-mkdir -p $(BUILD_DIR)

$(TARGET_EXE_FULL): $(OBJS)
	for fn in $(TARGET_EXE); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE)).pdb; do /bin/rm -f $$fn; done
	for fn in $(TARGET_EXE_FULL); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE_FULL)).pdb; do /bin/rm -f $$fn; done
	$(LINK_EXE)$(TARGET_EXE_FULL) $(GLOBAL_LFLAGS) \
             $(OBJS) \
             $(PARASOLID_LIB_FOR_LINKING) \
             $(SVLIBFLAG)_lib_simvascular_polydatasolid$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_solverio$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_sysgeom$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_solid$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_vmtk$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_repository$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_polydatasolid$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_utils$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_globals$(LIBLINKEXT) \
             $(MESHSIM_LIBS) \
             $(PARASOLID_LIBS) $(ZLIB_LIBS) \
	     $(TCLTK_LIBS) $(VTK_LIBS) $(ZLIB_LIBS) \
             $(F90_LIBS) $(CXX_LIBS) \
             $(PARASOLID_LIBS) $(MESHSIM_LIBS)
	if [ -e $(notdir $(TARGET_EXE_FULL)).pdb ];then mv $(basename $(TARGET_EXE_FULL)).pdb $(TOP)/Bin;fi
	ln -s $(notdir $(TARGET_EXE_FULL)) $(TARGET_EXE)

ifndef NO_DEPEND
-include $(DEPS)
endif

clean:
	for fn in $(BUILD_DIR); do /bin/rm -f -r $$fn;done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done

veryclean: clean
	if [ -e obj ];then /bin/rm -f -r obj;fi


