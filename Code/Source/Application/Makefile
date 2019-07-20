# Hey emacs, this is a -*- makefile -*-

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

TOP=../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
           $(ZLIB_INCDIR) $(TETGEN_INCDIR) \
           $(ITK_INCDIRS) $(OPENCASCADE_INCDIR) $(OPENCASCADE_DEFS) $(SV_MMG_INCDIR) \
           $(PYTHON_INCDIR) $(SV_GDCM_INCDIR) $(QT_INCDIRS) $(QT_DEFS) \
           $(MITK_INCDIRS) $(MITK_DEFS)

HDRS	= 

CXXSRCS = main.cxx SimVascular_Init.cxx 

ifeq ($(SV_USE_TCL),1)
  HDRS += svTcl_AppInit.h
  CXXSRCS += svTcl_AppInit.cxx
endif

ifeq ($(SV_USE_WIN32_REGISTRY),1)
  HDRS += sv_use_win32_registry.h
  CXXSRCS += sv_use_win32_registry.cxx
endif

ifeq ($(SV_USE_PYTHON),1)
   HDRS    += PythonShell_Init.h
   CXXSRCS += PythonShell_Init.cxx
#  CXXSRCS += SimVascular_Init_py.cxx
#  SV_LINK_WITH_INTERPRETER_LIBS = 1
endif

# special linker flags
ifeq ($(CLUSTER), x64_cygwin)
  LINK_EXE_LFLAGS = $(GLOBAL_LFLAGS)
endif
ifeq ($(CLUSTER), x64_linux)
  LINK_EXE_LFLAGS = $(GLOBAL_LFLAGS)
  #ignore undefined symbols in shared libs
  LINK_EXE_LFLAGS += -Wl,--unresolved-symbols=ignore-in-shared-libs
endif
ifeq ($(CLUSTER), x64_macosx)
  LINK_EXE_LFLAGS = $(GLOBAL_LFLAGS)
endif

# sv libs
LINK_EXE_LFLAGS += $(SVLIBFLAG)$(SV_LIB_SEGMENTATION_NAME)$(LIBLINKEXT)

# mitk libs only needed to link on win32
ifeq ($(SV_USE_MITK),1)
  ifeq ($(SV_USE_SV4_GUI),1)
    LINK_EXE_LFLAGS += $(SVLIBFLAG)$(SV_PLUGIN_APPLICATION_NAME)$(LIBLINKEXT)
  endif
  ifeq ($(CLUSTER), x64_cygwin)
    LINK_EXE_LFLAGS += $(MITK_LIBS)
  endif
endif

# add SV globals
LINK_EXE_LFLAGS += $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

# vtk libs
LINK_EXE_LFLAGS += $(LIBPATH_COMPILER_FLAG)$(VTK_LIBDIRS) \
	  $(LIBFLAG)vtkCommonCore-$(VTK_MAJOR_VERSION).$(VTK_MINOR_VERSION)$(LIBLINKEXT) \
          $(LIBFLAG)vtksys-$(VTK_MAJOR_VERSION).$(VTK_MINOR_VERSION)$(LIBLINKEXT)

# qt libs
ifeq ($(SV_USE_QT),1)
ifeq ($(CLUSTER), x64_cygwin)
  LINK_EXE_LFLAGS += $(QT_LIBS)
endif
ifeq ($(CLUSTER), x64_linux)
  LINK_EXE_LFLAGS += $(LIBPATH_COMPILER_FLAG)$(QT_LIBDIRS) \
	$(SVLIBFLAG)Qt5Sql$(LIBLINKEXT) \
	$(SVLIBFLAG)Qt5Qml$(LIBLINKEXT) \
	$(SVLIBFLAG)Qt5Core$(LIBLINKEXT)
endif
ifeq ($(CLUSTER), x64_macosx)
  LINK_EXE_LFLAGS += $(QT_LIBS)
endif
endif

ifeq ($(SV_USE_TCL),1)
  LINK_EXE_LFLAGS += $(TCLTK_LIBS)
endif

ifeq ($(SV_USE_PYTHON),1)
  LINK_EXE_LFLAGS += $(PYTHON_LIB)
endif

LINK_EXE_LFLAGS += $(CXX_LIBS)

# system libs needed by vtk
LINK_EXE_LFLAGS += $(VTK_SYS_LIBS)

SRCS	= $(CXXSRCS) $(foreach name,$(COPIED_CXXSRCS),$(lastword $(subst /, ,$(name))))

DEPS	= $(SRCS:.cxx=.d)

OBJS    = $(addprefix $(BUILD_DIR)/,$(SRCS:.cxx=.$(OBJECTEXT)))

TARGET_EXE_FULL = $(TOP)/Bin/simvascular-$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION).exe
TARGET_EXE = $(TOP)/Bin/simvascular.exe

shared: pre directories $(TARGET_EXE_FULL)

static: pre directories $(TARGET_EXE_FULL)

# only for static case
$(BUILD_DIR)/sv-cpp-resources.$(OBJECTEXT):
	-rm -Rf ./static_cpp_resources
	-mkdir -p ./static_cpp_resources
	-cp -Rf ../Modules/Model/resource ./static_cpp_resources/svModel
	-cp -Rf ../Modules/Path/resource ./static_cpp_resources/svPath
	-cp -Rf ../Modules/Segmentation/resource ./static_cpp_resources/svSegmentation
	-cd ./static_cpp_resources;zip -r res_0.zip svModel/ svPath/ svSegmentation/
ifeq ($(CLUSTER), x64_cygwin)
	-echo "#define US_RESOURCE      200" >> ./static_cpp_resources/sv-cpp-resources.rc
	-echo "#define IDR_US_RESOURCE  101" >> ./static_cpp_resources/sv-cpp-resources.rc
	-echo "IDR_US_RESOURCE US_RESOURCE res_0.zip" >> ./static_cpp_resources/sv-cpp-resources.rc
	-cd ./static_cpp_resources;rc /fo sv-cpp-resources.res sv-cpp-resources.rc
	-cd ./static_cpp_resources;cvtres /MACHINE:X64 /OUT:../$(BUILD_DIR)/sv-cpp-resources.obj sv-cpp-resources.res
endif
ifeq ($(CLUSTER), x64_linux)
	-cd ./static_cpp_resources;ld -o ../$(BUILD_DIR)/sv-cpp-resources.o -r -b binary res_0.zip
	-cd ./static_cpp_resources;objcopy --rename-section .data=.rodata,alloc,load,readonly,data,contents ../$(BUILD_DIR)/sv-cpp-resources.o ../$(BUILD_DIR)/sv-cpp-resources.o
endif
ifeq ($(CLUSTER), x64_macosx)
	-cp usCMakeResourceDependencies.cxx ./static_cpp_resources
	-cd ./static_cpp_resources;${CXX} ${GLOBAL_CXXFLAGS} -c usCMakeResourceDependencies.cxx -o stub.o
	-cd ./static_cpp_resources;ld -r -sectcreate __TEXT us_resources res_0.zip stub.o -o ../$(BUILD_DIR)/sv-cpp-resources.o
endif

directories:
	-mkdir -p $(BUILD_DIR)

$(TARGET_EXE_FULL): $(OBJS)
	for fn in $(TARGET_EXE); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE)).pdb; do /bin/rm -f $$fn; done
	for fn in $(TARGET_EXE_FULL); do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE_FULL)).pdb; do /bin/rm -f $$fn; done
	$(LINK_EXE)$(TARGET_EXE_FULL) $(OBJS) \
        $(LINK_EXE_LFLAGS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
ifeq ($(CLUSTER),x64_cygwin)
ifneq ($(SV_STATIC_BUILD),1)
        # we need to check and see if we should be still rebasing
        # the tcltk and vtk dlls
	editbin /REBASE:BASEFILE $(TARGET_EXE_FULL) ../../../BuildWithMake/Lib/$(CLUSTER)/$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION)*.$(SOEXT)
endif
endif
  # had to remove vtk libdirs for mine to compile on mac
	#-install_name_tool -add_rpath $(VTK_LIBDIRS) $(TARGET_EXE_FULL)
ifeq ($(CLUSTER),x64_macosx)
	-install_name_tool -add_rpath $(TCLTK_SO_PATH) $(TARGET_EXE_FULL)
	-install_name_tool -add_rpath $(VTK_TCL_LIB_PATH) $(TARGET_EXE_FULL)
	-install_name_tool -add_rpath $(VTK_SO_PATH) $(TARGET_EXE_FULL)
	-install_name_tool -add_rpath $(VTK_BINDIR) $(TARGET_EXE_FULL)
ifeq ($(SV_USE_GDCM),1)
	-install_name_tool -add_rpath $(SV_GDCM_SO_PATH) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_HDF5),1)
	-install_name_tool -add_rpath $(SV_HDF5_SO_PATH) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_ITK),1)
	-install_name_tool -add_rpath $(ITK_BIN_LIBS) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_OPENCASCADE),1)
	-install_name_tool -add_rpath $(SV_OPENCASCADE_SO_PATH) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_QT),1)
	-install_name_tool -add_rpath $(QT_SO_PATH) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_MITK),1)
	-install_name_tool -add_rpath $(SV_MITK_SO_PATH) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_FREETYPE),1)
	-install_name_tool -add_rpath $(SV_FREETYPE_SO_PATH) $(TARGET_EXE_FULL)
endif
ifeq ($(SV_USE_PYTHON),1)
	-install_name_tool -add_rpath $(SV_PYTHON_SO_PATH) $(TARGET_EXE_FULL)
endif
endif
	if [ -e $(notdir $(TARGET_EXE_FULL)).pdb ];then mv $(basename $(TARGET_EXE_FULL)).pdb $(TOP)/Bin;fi
	ln -s $(notdir $(TARGET_EXE_FULL)) $(TARGET_EXE)

pre:
	$(foreach name,$(COPIED_CXXSRCS),$(shell cp -f $(name) .))

ifndef NO_DEPEND
-include $(DEPS)
endif

clean:
	for fn in $(BUILD_DIR); do /bin/rm -f -r $$fn;done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in $(TARGET_EXE)*; do /bin/rm -f $$fn; done
	for fn in $(TARGET_EXE_FULL)*; do /bin/rm -f $$fn; done
	for fn in $(basename $(TARGET_EXE_FULL)).*;do /bin/rm -f $$fn; done
	for fn in $(notdir $(COPIED_CXXSRCS)); do /bin/rm -f $$fn;done
	for fn in coffbase.txt; do /bin/rm -f $$fn;done

veryclean: clean
	if [ -e static_cpp_resources ];then /bin/rm -f -r static_cpp_resources;fi
	if [ -e obj ];then /bin/rm -f -r obj;fi


