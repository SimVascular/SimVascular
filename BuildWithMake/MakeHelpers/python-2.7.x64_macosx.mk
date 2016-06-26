ifeq ($(CLUSTER),x64_macosx)
    PYTHON_TOP_DIR = /opt/local/Library/Frameworks/Python.framework/Versions/2.7
    PYTHON_INCDIR = -I$(PYTHON_TOP_DIR)/include/python2.7
    PYTHON_LIBDIR = 
    PYTHON_LIB    = $(PYTHON_TOP_DIR)/lib/libpython2.7.dylib
    PYTHON_SITE_PACKAGES = $(PYTHON_TOP_DIR)/lib/python2.7/site-packages
endif
