# Hey emacs, this is a -*- makefile -*-

# /*=========================================================================
# *
# * Copyright (c) 2014-2015 The Regents of the University of California.
# * All Rights Reserved. 
# *
# * Permission is hereby granted, free of charge, to any person obtaining
# * a copy of this software and associated documentation files (the
# * "Software"), to deal in the Software without restriction, including 
# * without limitation the rights to use, copy, modify, merge, publish, 
# * distribute, sublicense, and/or sell copies of the Software, and to
# * permit persons to whom the Software is furnished to do so, subject
# * to the following conditions:
# * 
# * The above copyright notice and this permission notice shall be included 
# * in all copies or substantial portions of the Software.
# * 
# * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# *
# *=========================================================================*/

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(TETGEN_INCDIR) $(SOLVERIO_INCDIR) \
           $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) $(ZLIB_INCDIR) -DCV_STATIC_LINK

FFLAGS   = $(GLOBAL_FFLAGS)

HDRS	= cvTetAdaptCore.h AdaptHelpers.h

CXXSRCS = cvTetAdaptCore.cxx main.cxx AdaptHelpers.cxx

FSRCS   = pythag.f tred2EISPACK.f tql2.f

SRCS	= $(CXXSRCS) $(FSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

OBJS	= $(CXXSRCS:.cxx=.$(OBJECTEXT)) $(FSRCS:.f=.$(OBJECTEXT))

TARGET_EXE = tetadaptor.exe

# needed to use globals in dll
ifneq ($(CLUSTER),x64_linux)
  ifneq ($(MAKE_STATIC_BUILD),1)
	LFLAGS += $(TOP)/Lib/lib_simvascular_globals.$(STATICEXT)
  endif
endif

static: $(TOP)/Bin/$(TARGET_EXE)

ifeq ($(CLUSTER),x64_linux)
$(TOP)/Bin/$(TARGET_EXE): $(OBJS)
	$(LINK_EXE)$(TARGET_EXE) $(GLOBAL_LFLAGS) $(LFLAGS) \
             $(OBJS) \
             $(TOP)/Lib/lib_lib_simvascular_solverio.$(STATICEXT) \
             $(LFLAGS) -l_lib_simvascular_globals -l_lib_simvascular_solid -l_lib_simvascular_utils \
             $(TETGEN_LIBS) \
             $(ZLIB_LIBS) \
	     $(VTK_LIBS) $(ZLIB_LIBS) $(GLIB_LIBS) $(GTS_LIBS) $(TCLTK_LIBS) \
             $(F90_LIBS) $(CXX_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	mv $(TARGET_EXE)* $(TOP)/Bin
else
$(TOP)/Bin/$(TARGET_EXE): $(OBJS)
	$(LINK_EXE)$(TARGET_EXE) $(GLOBAL_LFLAGS) $(LFLAGS) \
             $(OBJS) \
             $(TOP)/Lib/lib_lib_simvascular_solverio.$(STATICEXT) \
             $(TETGEN_LIBS) $(ZLIB_LIBS) $(GLIB_LIBS) $(GTS_LIBS) \
	     $(TCLTK_LIBS) $(VTK_LIBS) $(ZLIB_LIBS) \
             $(F90_LIBS) $(CXX_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	mv $(TARGET_EXE)* $(TOP)/Bin
endif

ifndef NO_DEPEND
-include $(DEPS)
endif

clean: veryclean

veryclean:
	for fn in *.$(OBJECTEXT); do /bin/rm -f $$fn; done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in *.mod; do /bin/rm -f $$fn;done
	for fn in *.d; do /bin/rm -f $$fn; done
	for fn in *.pdb; do /bin/rm -f $$fn; done
	for fn in *.exp; do /bin/rm -f $$fn; done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done

