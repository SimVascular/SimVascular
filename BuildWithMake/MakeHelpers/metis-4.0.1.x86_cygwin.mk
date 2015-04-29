ifeq ($(CLUSTER), x86_cygwin)
#    METIS_INCDIR  = -I $(OPEN_SOFTWARE_SOURCES_TOPLEVEL)/metis-5.0.2/include
#    METIS_LIBS    = -LIBPATH:$(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/metis-5.0.2/libmetis/RelWithDebInfo metis.lib
    METIS_INCDIR  = -I $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/metis-4.0.1/
    METIS_LIBS    = -LIBPATH:$(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/metis-4.0.1/ libmetis.lib
endif
