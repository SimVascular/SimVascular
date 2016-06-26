ifeq ($(CLUSTER), x64_linux)
    PYTHON_INCDIR = -I/usr/include/python2.7
    PYTHON_LIBDIR = 
    PYTHON_LIB    = $(PYTHON_LIBDIR) -lpython2.7
    PYTHON_SITE_PACKAGES = /usr/lib/python2.7/dist-packages
endif
