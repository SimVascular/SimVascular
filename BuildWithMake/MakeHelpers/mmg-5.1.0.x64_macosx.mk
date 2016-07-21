ifeq ($(CLUSTER), x64_macosx)
    SV_MMG_DEFS   =
    SV_MMG_TOP    = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/mmg-5.1.0
    SV_MMG_INCDIR = -I $(SV_MMG_TOP)/include
    SV_MMG_LIBS   = $(LIBPATH_COMPILER_FLAG)$(SV_MMG_TOP)/lib \
                     $(LIBFLAG)mmg$(LIBLINKEXT) $(LIBFLAG)mmgs$(LIBLINKEXT)
    SV_MMG_DLLS   =  $(SV_MMG_TOP)/lib
    SV_MMG_SO_PATH = $(SV_MMG_TOP)/lib
endif
