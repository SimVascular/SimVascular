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

OBJS    = $(addprefix $(BUILD_DIR)/,$(CXXSRCS:.cxx=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(CSRCS:.c=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(FSRCS:.f=.$(OBJECTEXT)))

TARGET_EXE_FULL = $(TOP)/Bin/tetadaptor-$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION).exe
TARGET_EXE = $(TOP)/Bin/tetadaptor.exe

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
             $(SVLIBFLAG)_lib_simvascular_polydatasolid$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_solverio$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_sysgeom$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_solid$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_vmtk$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_repository$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_polydatasolid$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_utils$(LIBLINKEXT) \
             $(SVLIBFLAG)_lib_simvascular_globals$(LIBLINKEXT) \
             $(TETGEN_LIBS) \
             $(ZLIB_LIBS) \
	     $(TCLTK_LIBS) $(VTK_LIBS) $(ZLIB_LIBS) \
             $(F90_LIBS) $(CXX_LIBS)
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

