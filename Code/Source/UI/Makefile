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
           $(ZLIB_INCDIR) $(PARASOLID_INCDIR) $(MESHSIM_INCDIR) $(TETGEN_INCDIR) \
           $(ITK_INCDIRS) $(OPENCASCADE_INCDIR) $(OPENCASCADE_DEFS) $(SV_MMG_INCDIR) \
           $(PYTHON_INCDIR) $(SV_GDCM_INCDIR) $(QT_INCDIRS) $(QT_DEFS) \
           $(MITK_INCDIRS) $(MITK_DEFS)

HDRS	=

CXXSRCS = main.cxx SimVascular_Init.cxx

ifneq ($(CLUSTER),x64_cygwin)
  ifeq ($(SV_USE_QT),1)
    HDRS    += qttclnotifier.h
    CXXSRCS += qttclnotifier.cxx moc_qttclnotifier.cxx
  endif
endif

ifeq ($(SV_STATIC_BUILD),1)

  COPIED_CXXSRCS += ../Common/Geometry/cv_geom_init.cxx \
                    ../PostProcessing/cv_post_init.cxx \
                    ../ImageProcessing/cv_image_init.cxx \
                    ../Common/Repository/cv_repos_init.cxx \
                    ../Model/SolidModel/cv_solid_init.cxx \
                    ../Legacy/LevelSet/cv_LsetCore_init.cxx \
                    ../Legacy/LevelSet/cv_LsetV_init.cxx \
                    ../Legacy/LevelSet/cv_Lset_init.cxx \
                    ../Common/Utils/cv_get_tcl_interp_init.cxx \
                    ../Mesh/MeshObject/cv_mesh_init.cxx \
                    ../Model/PolyDataSolidModel/cv_polydatasolid_init.cxx \
                    ../Common/Utils/cv_math_init.cxx \

  ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
	COPIED_CXXSRCS += ../Mesh/AdaptObject/cv_adapt_init.cxx
  else
    ifeq ($(SV_USE_MESHSIM_ADAPTOR),1)
	  COPIED_CXXSRCS += ../Mesh/AdaptObject/cv_adapt_init.cxx
    endif
  endif

  ifeq ($(SV_USE_MESHSIM),1)
    ifneq ($(SV_USE_MESHSIM_SHARED),1)
	COPIED_CXXSRCS += ../Mesh/MeshSimMeshObject/cv_meshsim_mesh_init.cxx
    endif
  endif
  ifeq ($(SV_USE_MESHSIM_ADAPTOR),1)
    ifneq ($(SV_USE_MESHSIM_SHARED),1)
	COPIED_CXXSRCS += ../Mesh/MeshSimAdapt/cv_meshsim_adapt_init.cxx
    endif
  endif
  ifeq ($(SV_USE_TETGEN),1)
	COPIED_CXXSRCS += ../Mesh/TetGenMeshObject/cv_tetgen_mesh_init.cxx
  endif
  ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
	COPIED_CXXSRCS += ../Mesh/TetGenAdapt/cv_tetgen_adapt_init.cxx
  endif
  ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
    ifneq ($(SV_USE_MESHSIM_DISCRETE_MODEL_SHARED),1)
	COPIED_CXXSRCS += ../Model/MeshSimDiscreteSolidModel/cv_discrete_init.cxx
    endif
  endif
  ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
    ifneq ($(SV_USE_MESHSIM_SOLID_MODEL_SHARED),1)
	COPIED_CXXSRCS += ../Model/MeshSimSolidModel/cv_meshsimsolid_init.cxx
    endif
  endif
  ifeq ($(SV_USE_PARASOLID),1)
    ifneq ($(SV_USE_PARASOLID_SHARED),1)
	COPIED_CXXSRCS += ../../Licensed/ParasolidSolidModel/cv_parasolid_solid_init.cxx
    endif
  endif
  ifeq ($(SV_USE_OPENCASCADE),1)
    ifneq ($(SV_USE_OPENCASCADE_SHARED),1)
	COPIED_CXXSRCS += ../Model/OCCTSolidModel/cv_occt_init.cxx
    endif
  endif
  ifeq ($(SV_USE_MMG),1)
    ifneq ($(SV_USE_MMG_SHARED),1)
	COPIED_CXXSRCS += ../Mesh/MMGMeshUtils/cv_mmg_mesh_init.cxx
    endif
  endif
  ifeq ($(SV_USE_ITK),1)
	COPIED_CXXSRCS += ../Segmentation/ITK/cv_ITKLset_init.cxx
	COPIED_CXXSRCS += ../Segmentation/ITK/cv_ITKUtils_init.cxx
	COPIED_CXXSRCS += ../Segmentation/ITK/cv_ITKLset2d_init.cxx
	COPIED_CXXSRCS += ../Segmentation/ITK/cv_ITKLset3d_init.cxx
  endif

  ifeq ($(SV_USE_SOLVERIO),1)
	LFLAGS += $(SOLVERIO_LIB)
  endif

  ifeq ($(SV_USE_MESHSIM),1)
    ifneq ($(SV_USE_MESHSIM_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_MESH_NAME)$(LIBLINKEXT)
    endif
  endif
  ifeq ($(SV_USE_MESHSIM_ADAPTOR),1)
    ifneq ($(SV_USE_MESHSIM_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_ADAPTOR_NAME)$(LIBLINKEXT)
        LFLAGS += $(SVLIBFLAG)$(SV_LIB_ADAPTOR_NAME)$(LIBLINKEXT)
    endif
  endif
  ifeq ($(SV_USE_TETGEN),1)
    ifneq ($(SV_USE_SHARED),1)
      LFLAGS += $(SVLIBFLAG)$(SV_LIB_TETGEN_MESH_NAME)$(LIBLINKEXT)
   endif
  endif
  ifeq ($(SV_USE_TETGEN_ADAPTOR),1)
    ifneq ($(SV_USE_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_TETGEN_ADAPTOR_NAME)$(LIBLINKEXT)
        LFLAGS += $(SVLIBFLAG)$(SV_LIB_ADAPTOR_NAME)$(LIBLINKEXT)
    endif
  endif
  ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
    ifneq ($(SV_USE_MESHSIM_DISCRETE_MODEL_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_DISCRETE_SOLID_NAME)$(LIBLINKEXT)
    endif
  endif
  ifeq ($(SV_USE_MESHSIM_SOLID_MODEL),1)
    ifneq ($(SV_USE_MESHSIM_SOLID_MODEL_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_MESHSIM_SOLID_NAME)$(LIBLINKEXT)
    endif
  endif
  ifeq ($(SV_USE_PARASOLID),1)
    ifneq ($(SV_USE_PARASOLID_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_PARASOLID_SOLID_NAME)$(LIBLINKEXT)
    endif
  endif
  ifeq ($(SV_USE_OPENCASCADE),1)
    ifneq ($(SV_USE_OPENCASCADE_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
    endif
  endif

  ifeq ($(SV_USE_VMTK),1)
	LFLAGS += $(VMTK_LIBS)
  endif

  ifeq ($(SV_USE_ITK),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT)
  endif

  ifeq ($(SV_USE_SOLVERIO),1)
	LFLAGS += $(SOLVERIO_LIB)
  endif

  ifeq ($(SV_USE_MMG),1)
    ifneq ($(SV_USE_MMG_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_MMG_MESH_NAME)$(LIBLINKEXT)
    endif
  endif

  ifeq ($(SV_USE_PYTHON),1)
    ifneq ($(SV_USE_PYTHON_SHARED),1)
	LFLAGS += $(SVLIBFLAG)$(SV_LIB_PYTHON_INTERP_NAME)$(LIBLINKEXT)
    endif
  endif

endif

LFLAGS += $(SVLIBFLAG)$(SV_LIB_MESH_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_QT_GUI),1)
LFLAGS +=  \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_MODEL_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PROJECTMANAGEMENT_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_QTWIDGETS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_MODELING_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_PATHPLANNING_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_PROJECTDATANODES_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_PROJECTMANAGER_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_PLUGIN_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)
#  ifneq ($(SV_USE_SHARED),1)
#    LFLAGS += $(SVLIBFLAG)_simvascular_qtcode$(LIBLINKEXT)
#  endif

# if we want to modify apputil, uncomment here!
#LFLAGS += $(SVLIBFLAG)_simvascular_qtgui_module_apputil$(LIBLINKEXT) \

endif

LFLAGS +=  $(SVLIBFLAG)$(SV_LIB_VTKSVCOMMON_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_VTKSVFILTERS_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_VTKSVGEOMETRY_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_VTKSVBOOLEAN_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_VTKSVNURBS_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_VTKSVPARAMETERIZATION_NAME)$(LIBLINKEXT)

# need to explicitly add in globals for executable
ifeq ($(SV_USE_GLOBALS_SHARED),1)
  LFLAGS += $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)
else
  LFLAGS += $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)
endif

SRCS	= $(CXXSRCS) $(foreach name,$(COPIED_CXXSRCS),$(lastword $(subst /, ,$(name))))

DEPS	= $(SRCS:.cxx=.d)

OBJS    = $(addprefix $(BUILD_DIR)/,$(SRCS:.cxx=.$(OBJECTEXT)))

### nate: why did I include this, for static libs maybe?
#ifeq ($(SV_USE_MITK),1)
#OBJS    += \
#../Modules/Common/$(BUILD_DIR)/us_init.$(OBJECTEXT) \
#../Modules/Model/$(BUILD_DIR)/us_init.$(OBJECTEXT) \
#../Modules/Path/$(BUILD_DIR)/us_init.$(OBJECTEXT) \
#../Modules/ProjectManagement/$(BUILD_DIR)/us_init.$(OBJECTEXT) \
#../Modules/QtAppBase/$(BUILD_DIR)/us_init.$(OBJECTEXT) \
#../Modules/QtWidgets/$(BUILD_DIR)/us_init.$(OBJECTEXT) \
#../Modules/Segmentation/$(BUILD_DIR)/us_init.$(OBJECTEXT)
#endif

# only for static case?
#ifeq ($(SV_USE_MITK),1)
#   OBJS += $(BUILD_DIR)/sv-cpp-resources.$(OBJECTEXT)
#endif

TARGET_EXE_FULL = $(TOP)/Bin/simvascular-$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION).exe
TARGET_EXE = $(TOP)/Bin/simvascular.exe

shared: pre directories $(TARGET_EXE_FULL)

static: pre directories $(TARGET_EXE_FULL)

moc:
	$(foreach name,$(HDRS),$(shell $(QT_MOC_PARSER) $(QT_DEFS) $(QT_MOC_INCDIRS) $(EXTRA_MOC_INCDIRS) $(name) -o moc_$(basename $(notdir $(name))).cxx))

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
	$(LINK_EXE)$(TARGET_EXE_FULL) $(LFLAGS) $(OBJS) \
        $(LFLAGS) \
        $(VMTK_LIBS) $(VTK_LIBS) $(ITK_LIBS) $(MITK_LIBS) $(PARASOLID_LIBS) $(MESHSIM_LIBS) $(TETGEN_LIBS) $(ITK_LIBS) \
        $(ZLIB_LIBS) $(MPICH_LIBS) $(OPENCASCADE_LIBS) $(TCLTK_LIBS) $(CXX_LIBS) \
        $(PYTHON_LIB) $(SV_GDCM_LIBS) $(QT_LIBS) $(SV_FREETYPE_LIBS) $(SV_MMG_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
ifeq ($(CLUSTER),x64_cygwin)
ifneq ($(SV_STATIC_BUILD),1)
        # can no longer rebase parasolid dlls
        # we need to check and see if we should be still rebasing
        # the tcltk and vtk dlls
	editbin /REBASE:BASEFILE $(TARGET_EXE_FULL) ../../../BuildWithMake/Lib/$(CLUSTER)/$(CXX_COMPILER_VERSION)-$(FORTRAN_COMPILER_VERSION)*.$(SOEXT)
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
	for fn in moc_*.cxx; do /bin/rm -f $$fn;done
	for fn in coffbase.txt; do /bin/rm -f $$fn;done

veryclean: clean
	if [ -e static_cpp_resources ];then /bin/rm -f -r static_cpp_resources;fi
	if [ -e obj ];then /bin/rm -f -r obj;fi


