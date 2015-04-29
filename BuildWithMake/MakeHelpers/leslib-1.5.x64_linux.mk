ifeq ($(CLUSTER), x64_linux)
    LESLIB_DEFS   = -DACUSIM_LINUX
    LESLIB_TOP    = $(LICENSED_SOFTWARE_TOPLEVEL)/leslib-1.5/x64_linux
    LESLIB_INCDIR = -I $(LESLIB_TOP)
    LESLIB_LIBS   = -L$(LESLIB_TOP) -lles
    #LESLIB_LIBS   = -L$(LESLIB_TOP)/shared -lles -llapack
endif
