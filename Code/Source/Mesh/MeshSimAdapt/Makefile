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

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT)) $(FSRCS:.f=.$(OBJECTEXT))

TARGET_EXE = adaptor.exe

# needed to use globals in dll
ifneq ($(CLUSTER),x64_linux)
  ifneq ($(MAKE_STATIC_BUILD),1)
	LFLAGS += $(TOP)/Lib/lib_simvascular_globals.$(STATICEXT)
  endif
endif

#intentionally link against static library
ifeq ($(MAKE_WITH_PARASOLID),1)
  ifeq ($(MAKE_WITH_PARASOLID_SHARED),1)
    PARASOLID_LIB_FOR_LINKING = $(TOP)/Lib/lib_lib_simvascular_parasolid.$(STATICEXT)
  else
    PARASOLID_LIB_FOR_LINKING = $(TOP)/Lib/lib_lib_simvascular_parasolid.$(STATICEXT)
  endif
endif

static: $(TOP)/Bin/$(TARGET_EXE)

ifeq ($(CLUSTER),x64_linux)
$(TOP)/Bin/$(TARGET_EXE): $(OBJS)
	$(LINK_EXE)$(TARGET_EXE) $(GLOBAL_LFLAGS) $(LFLAGS) \
             $(OBJS) \
             $(TOP)/Lib/lib_lib_simvascular_solverio.$(STATICEXT) \
             $(PARASOLID_LIB_FOR_LINKING) \
             $(LFLAGS) -l_lib_simvascular_globals \
                       -l_lib_simvascular_solid \
                       -l_lib_simvascular_utils \
                       -l_lib_simvascular_vmtk \
             $(MESHSIM_LIBS) \
             $(PARASOLID_LIBS) $(ZLIB_LIBS) $(VMTK_LIBS) \
	     $(VTK_LIBS) $(ZLIB_LIBS) $(GLIB_LIBS) $(GTS_LIBS) $(TCLTK_LIBS) \
             $(F90_LIBS) $(CXX_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	mv $(TARGET_EXE)* $(TOP)/Bin
else
$(TOP)/Bin/$(TARGET_EXE): $(OBJS)
	$(LINK_EXE)$(TARGET_EXE) $(GLOBAL_LFLAGS) $(LFLAGS) \
             $(OBJS) \
             $(TOP)/Lib/lib_lib_simvascular_solverio.$(STATICEXT) \
             $(TOP)/Lib/lib_lib_simvascular_vmtk.$(STATICEXT) \
             $(PARASOLID_LIB_FOR_LINKING) \
             $(MESHSIM_LIBS) \
             $(PARASOLID_LIBS) $(ZLIB_LIBS) \
	     $(TCLTK_LIBS) $(VTK_LIBS) $(ZLIB_LIBS) \
             $(F90_LIBS) $(CXX_LIBS) \
             $(PARASOLID_LIBS) $(MESHSIM_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	mv $(TARGET_EXE)* $(TOP)/Bin
endif

ifndef NO_DEPEND
-include $(DEPS)
endif

clean: veryclean

veryclean: clean
	for fn in *.$(OBJECTEXT); do /bin/rm -f $$fn; done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in *.mod; do /bin/rm -f $$fn;done
	for fn in *.d; do /bin/rm -f $$fn; done
	for fn in *.pdb; do /bin/rm -f $$fn; done
	for fn in *.exp; do /bin/rm -f $$fn; done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done

