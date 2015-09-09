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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(SOLVERIO_INCDIR) $(THREEDSOLVER_INCDIR) $(VTK_INCDIRS) $(NSPCG_INCDIR) $(ZLIB_INCDIR)
CCFLAGS  = $(GLOBAL_CXXFLAGS) $(SPARSE_INCDIR) $(ZLIB_INCDIR) $(THREEDSOLVER_INCDIR)
FFLAGS   = $(GLOBAL_FFLAGS) $(NSPCG_INCDIR) $(THREEDSOLVER_INCDIR)

HDRS	= cmd.h

CSRCS   = directsolve.c

#CSRCS   += missing_mingw_and_CL.c


CXXSRCS = cmd.cxx displacements.cxx helpers.cxx supre-cmds.cxx supre.cxx \
	  presolver-vtk-cmds.cxx

#if(VER_VARWALL == 1)
  CXXSRCS += Laplace_Solve.cxx Transient_Laplace_Solve.cxx \
	         displacements_var_prop.cxx
#endif

FSRCS   = stannspcg.f

# not sure what the differences are between gmresfortran.f and rmresfortran.f90
#if(VER_VARWALL == 1)
 FSRCS += gmresfortran.f 
#endif


SRCS	= $(CSRCS) $(CXXSRCS)

DEPS	= $(CSRCS:.c=.d) $(CXXSRCS:.cxx=.d)

OBJS    = $(addprefix $(BUILD_DIR)/,$(CXXSRCS:.cxx=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(CSRCS:.c=.$(OBJECTEXT))) \
          $(addprefix $(BUILD_DIR)/,$(FSRCS:.f=.$(OBJECTEXT)))

TARGET_EXE_FULL = $(TOP)/Bin/presolver-$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION).exe
TARGET_EXE = $(TOP)/Bin/presolver.exe

static: directories createOptionFile $(TARGET_EXE_FULL) 

directories:
	-mkdir -p $(BUILD_DIR)

$(TARGET_EXE_FULL): $(OBJS)
	for fn in $(TARGET_EXE); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE)).pdb; do /bin/rm -f $$fn; done
	for fn in $(TARGET_EXE_FULL); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE_FULL)).pdb; do /bin/rm -f $$fn; done
	$(LINK_EXE)$(TARGET_EXE_FULL) $(GLOBAL_LFLAGS) \
             $(OBJS) $(METIS_LIBS)  \
             $(SPARSE_LIBS) $(NSPCG_LIBS) $(ZLIB_LIBS) $(VTK_LIBS) \
             $(SVLIBFLAG)_lib_simvascular_solverio$(LIBLINKEXT) \
             $(F90_LIBS) $(CXX_LIBS)
	if [ -e $(notdir $(TARGET_EXE_FULL)).pdb ];then mv $(basename $(TARGET_EXE_FULL)).pdb $(TOP)/Bin;fi
	ln -s $(notdir $(TARGET_EXE_FULL)) $(TARGET_EXE)

ifndef NO_DEPEND
-include $(DEPS)
endif

createOptionFile:
	@echo "#define VER_CORONARY $(FLOWSOLVER_VERSION_CORONARY_ACTIVATE)" > $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPre/cvFlowsolverOptions.h
	@echo "#define VER_CLOSEDLOOP $(FLOWSOLVER_VERSION_CLOSEDLOOP_ACTIVATE)" >> $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPre/cvFlowsolverOptions.h
	@echo "#define VER_VARWALL $(FLOWSOLVER_VERSION_VARWALL_ACTIVATE)" >> $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPre/cvFlowsolverOptions.h
	@echo "#define VER_USE_VTK $(FLOWSOLVER_VERSION_USE_VTK_ACTIVATE)" >> $(TOP)/../Code/FlowSolvers/ThreeDSolver/svPre/cvFlowsolverOptions.h

clean:
	for fn in $(BUILD_DIR); do /bin/rm -f -r $$fn;done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	/bin/rm -f cvFlowsolverOptions.h

veryclean: clean
	if [ -e obj ];then /bin/rm -f -r obj;fi

