# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
# 
# All rights reserved. 
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
# 
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

TOP=../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
           $(ZLIB_INCDIR) $(PARASOLID_INCDIR) $(MESHSIM_INCDIR) $(TETGEN_INCDIR) \
           $(ITK_INCDIRS) $(MYRLM_INCDIR) -DCV_STATIC_LINK

HDRS	= 

CXXSRCS = main.cxx gdscInit.cxx cv_VTK_init.cxx

ifeq ($(MAKE_STATIC_BUILD),1)  

  COPIED_CXXSRCS += ../Common/Geometry/cv_geom_init.cxx \
                    ../PostProcessing/cv_post_init.cxx \
                    ../ImageProcessing/cv_image_init.cxx \
                    ../Common/Repository/cv_repos_init.cxx \
                    ../Model/SolidModel/cv_solid_init.cxx \
                    ../Legacy/LevelSet/cv_LsetCore_init.cxx \
                    ../Legacy/LevelSet/cv_LsetV_init.cxx \
                    ../Common/Utils/cv_get_tcl_interp_init.cxx \
                    ../Mesh/MeshObject/cv_mesh_init.cxx \
                    ../Model/PolyDataSolidModel/cv_polydatasolid_init.cxx \
                    ../Common/Utils/cv_math_init.cxx

  ifeq ($(MAKE_WITH_MESHSIM),1)
    ifneq ($(MAKE_WITH_MESHSIM_SHARED),1)
	COPIED_CXXSRCS += ../Mesh/MeshSimMeshObject/cv_meshsim_mesh_init.cxx
    endif
  endif
  ifeq ($(MAKE_WITH_TETGEN),1)
	COPIED_CXXSRCS += ../Mesh/TetGenMeshObject/cv_tetgen_mesh_init.cxx
  endif
  ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
    ifneq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL_SHARED),1)
	COPIED_CXXSRCS += ../Model/MeshSimDiscreteSolidModel/cv_discrete_init.cxx
    endif
  endif
  ifeq ($(MAKE_WITH_PARASOLID),1)
    ifneq ($(MAKE_WITH_PARASOLID_SHARED),1)
	COPIED_CXXSRCS += ../../Licensed/ParasolidSolidModel/cv_parasolid_solid_init.cxx
    endif
  endif
  ifeq ($(MAKE_WITH_ITK),1)
	COPIED_CXXSRCS += ../Segmentation/ITK/Util/cv_ITKUtils_init.cxx
	COPIED_CXXSRCS += ../Segmentation/ITK/cv_ITKLset2d_init.cxx
	COPIED_CXXSRCS += ../Segmentation/ITK/cv_ITKLset3d_init.cxx
  endif

  LINKEXPLICITLIBS =      ../../../BuildWithMake/Lib/lib_lib_simvascular_post.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_image.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_repository.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_lset.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_solid.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_mesh.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_sysgeom.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_utils.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_lset.$(STATICEXT)  \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_image.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_polydatasolid.$(STATICEXT) \
                          ../../../BuildWithMake/Lib/lib_lib_simvascular_globals.$(STATICEXT)

  ifeq ($(MAKE_WITH_SOLVERIO),1)
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_solverio.$(STATICEXT)
  endif
  ifeq ($(MAKE_WITH_MESHSIM),1)
    ifeq ($(MAKE_WITH_MESHSIM_SHARED),1)
#	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_simvascular_meshsim_mesh.$(STATICEXT)
    else
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_meshsim_mesh.$(STATICEXT)
    endif
  endif
  ifeq ($(MAKE_WITH_TETGEN),1)
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_tetgen_mesh.$(STATICEXT)
  endif
  ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL_SHARED),1)
#	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_simvascular_meshsim_discrete.$(STATICEXT)
    else
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_meshsim_discrete.$(STATICEXT)
    endif
  endif
  ifeq ($(MAKE_WITH_PARASOLID),1)
    ifeq ($(MAKE_WITH_PARASOLID_SHARED),1)
#	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_simvascular_parasolid.$(STATICEXT)
    else
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_parasolid.$(STATICEXT)
    endif
  endif

  ifeq ($(MAKE_WITH_VMTK),1)
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_vmtk.$(STATICEXT)
  endif

  ifeq ($(MAKE_WITH_ITK),1)
	LINKEXPLICITLIBS += ../../../BuildWithMake/Lib/lib_lib_simvascular_itkutils.$(STATICEXT) \
                            ../../../BuildWithMake/Lib/lib_lib_simvascular_cvitk.$(STATICEXT)
  endif

endif

SRCS	= $(CXXSRCS) $(COPIED_CXXSRCS)

DEPS	= $(SRCS:.cxx=.d)

OBJS	= $(SRCS:.cxx=.$(OBJECTEXT))

TARGET_EXE = simvascular.exe

# here we use the trick that the objects are actually compiled in the
# local directory

#LINKOBJS = $(OBJS)

LINKOBJS = $(foreach name,$(OBJS),$(lastword $(subst /, ,$(name))))

dynamic: pre $(TOP)/Bin/$(TARGET_EXE)

static: pre $(TOP)/Bin/$(TARGET_EXE)

ifeq ($(CLUSTER),x64_linux) 
$(TOP)/Bin/$(TARGET_EXE): $(LINKOBJS)
	$(LINK_EXE)$(TARGET_EXE) $(LFLAGS) $(LINKOBJS) \
        $(LINKEXPLICITLIBS) \
        $(VMTK_LIBS) $(VTK_LIBS) $(ITK_LIBS) $(PARASOLID_LIBS) $(MESHSIM_LIBS) $(TETGEN_LIBS) $(ITK_LIBS) \
        $(ZLIB_LIBS) $(GLIB_LIBS) $(GTS_LIBS) $(TCLTK_LIBS) $(CXX_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	mv $(TARGET_EXE)* $(TOP)/Bin
else
$(TOP)/Bin/$(TARGET_EXE): $(LINKOBJS)
	$(LINK_EXE)$(TARGET_EXE) $(LFLAGS) $(LINKOBJS) \
        $(LINKEXPLICITLIBS) \
        $(VMTK_LIBS) $(VTK_LIBS) $(ITK_LIBS) $(PARASOLID_LIBS) $(MESHSIM_LIBS) $(TETGEN_LIBS) $(ITK_LIBS) \
        $(ZLIB_LIBS) $(GLIB_LIBS) $(GTS_LIBS) $(TCLTK_LIBS) $(CXX_LIBS)
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
ifneq ($(MAKE_STATIC_BUILD),1)
        # can no longer rebase parasolid dlls
        # we need to check and see if we should be still rebasing 
        # the tcltk and vtk dlls
	editbin /REBASE:BASEFILE $(TARGET_EXE) ../../../BuildWithMake/Lib/*.$(SOEXT) \
          $(TCLTK_DLLS) $(VTK_DLLS)
endif
	mv $(TARGET_EXE)* $(TOP)/Bin
endif

pre:
	$(foreach name,$(SRCS),$(shell cp -f $(name) .))

ifndef NO_DEPEND
-include $(DEPS)
endif

clean:
	for fn in *.$(OBJECTEXT); do /bin/rm -f $$fn; done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in *.mod; do /bin/rm -f $$fn;done

veryclean: clean
	for fn in *.d; do /bin/rm -f $$fn; done
	for fn in *.pdb; do /bin/rm -f $$fn; done
	for fn in *.exp; do /bin/rm -f $$fn; done
	for fn in $(TOP)/Bin/$(TARGET_EXE)*; do /bin/rm -f $$fn; done
	for fn in *.lib; do /bin/rm -f $$fn; done
	for fn in coffbase.txt; do /bin/rm -f $$fn; done
	for fn in $(notdir $(COPIED_CXXSRCS)); do /bin/rm -f $$fn;done


