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

OBJ_DIR ?= $(BUILD_DIR)

OBJS    ?= $(addprefix $(OBJ_DIR)/,$(CXXSRCS:.cxx=.$(OBJECTEXT))) \
           $(addprefix $(OBJ_DIR)/,$(CSRCS:.c=.$(OBJECTEXT))) \
           $(addprefix $(OBJ_DIR)/,$(FSRCS:.f=.$(OBJECTEXT)))

DLLOBJS  ?= $(addprefix $(OBJ_DIR)/,$(DLLSRCS:.cxx=.$(OBJECTEXT))) $(OBJS)
DLLOBJS2 ?= $(addprefix $(OBJ_DIR)/,$(DLLSRCS2:.cxx=.$(OBJECTEXT)))
DLLOBJS3 ?= $(addprefix $(OBJ_DIR)/,$(DLLSRCS3:.cxx=.$(OBJECTEXT)))

SRCS	= $(CXXSRCS)

DEPS	= $(CXXSRCS:.cxx=.d)

TARGET_LIB = $(TOP)/Lib/$(LIB_BUILD_DIR)/lib_$(TARGET_LIB_NAME).$(STATICEXT)
TARGET_SHARED = $(TOP)/Lib/$(LIB_BUILD_DIR)/lib_$(TARGET_LIB_NAME).$(SOEXT)

ifdef TARGET_LIB_NAME2
  TARGET_LIB2 = $(TOP)/Lib/$(LIB_BUILD_DIR)/lib_$(TARGET_LIB_NAME2).$(STATICEXT)
  TARGET_SHARED2 = $(TOP)/Lib/$(LIB_BUILD_DIR)/lib_$(TARGET_LIB_NAME2).$(SOEXT)
endif

ifdef TARGET_LIB_NAME3
  TARGET_LIB3 = $(TOP)/Lib/$(LIB_BUILD_DIR)/lib_$(TARGET_LIB_NAME3).$(STATICEXT)
  TARGET_SHARED3 = $(TOP)/Lib/$(LIB_BUILD_DIR)/lib_$(TARGET_LIB_NAME3).$(SOEXT)
endif

ifneq ($(TARGET_LIB_NAME),simvascular_globals)
  DLLLIBS += $(SVLIBFLAG)_simvascular_globals$(LIBLINKEXT)
  DLLLIBS2 += $(SVLIBFLAG)_simvascular_globals$(LIBLINKEXT)
  DLLLIBS3 += $(SVLIBFLAG)_simvascular_globals$(LIBLINKEXT)
endif

all:	lib

directories:
	mkdir -p $(BUILD_DIR)
	mkdir -p $(TOP)/Lib/$(LIB_BUILD_DIR)

lib:	directories $(TARGET_LIB)

shared: directories $(TARGET_SHARED) $(TARGET_SHARED2) $(TARGET_SHARED3)

$(TARGET_LIB):	$(OBJS)
	for fn in $(TARGET_LIB); do /bin/rm -f $$fn; done 
	$(AR)$(TARGET_LIB) $(OBJS)

ifeq ($(CLUSTER),x64_linux) 
$(TARGET_SHARED):	$(DLLOBJS)
	for fn in $(TARGET_SHARED); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	$(SHAR) $(SHARED_LFLAGS) $(TARGET_SHARED)  \
             $(DLLOBJS) $(LFLAGS) $(DLLLIBS)
ifdef SV_COPY_DLL_TO_BIN_PLUGINS
	mkdir -p $(TOP)/Bin/plugins
	cp -f $(TARGET_SHARED) $(TOP)/Bin/plugins
endif
endif
ifeq ($(CLUSTER),x64_macosx) 
$(TARGET_SHARED):	$(DLLOBJS)
	for fn in $(TARGET_SHARED); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	$(SHAR) $(SHARED_LFLAGS) $(TARGET_SHARED)  \
             $(DLLOBJS) $(LFLAGS) $(DLLLIBS)
ifdef SV_COPY_DLL_TO_BIN_PLUGINS
	mkdir -p $(TOP)/Bin/plugins
	cp -f $(TARGET_SHARED) $(TOP)/Bin/plugins
endif
endif
ifeq ($(CLUSTER),x64_cygwin)
$(TARGET_SHARED):	$(DLLOBJS)
	for fn in $(TARGET_SHARED); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.exp); do /bin/rm -f $$fn; done
ifneq ($(CXX_COMPILER_VERSION),mingw-gcc)
	for fn in $(TARGET_SHARED:.$(SOEXT)=.pdb); do /bin/rm -f $$fn; done
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS) /out:"$(TARGET_SHARED)" \
             /pdb:"$(TARGET_SHARED:.$(SOEXT)=.pdb)" \
             $(DLLOBJS) $(LFLAGS)
else
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS) /out:"$(TARGET_SHARED)" \
             /pdb:"$(TARGET_SHARED:.$(SOEXT)=.pdb)" \
             $(DLLOBJS) $(LFLAGS)
#	$(LIBCMD) /out:"$(TARGET_SHARED:.$(SOEXT)=.lib)" $(DLLOBJS)
endif
ifdef SV_COPY_DLL_TO_BIN_PLUGINS
	mkdir -p $(TOP)/Bin/plugins
	cp -f $(TARGET_SHARED) $(TOP)/Bin/plugins
	cp -f $(TARGET_SHARED:.$(SOEXT)=.pdb) $(TOP)/Bin/plugins
endif
endif

ifeq ($(CLUSTER),x64_linux) 
$(TOP)/Lib/$(TARGET_SHARED2):	$(DLLOBJS2)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED2)              \
             $(DLLOBJS2) $(LFLAGS) $(DLLLIBS2)
	for fn in $(TOP)/Lib/$(TARGET_SHARED2); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED2); do /bin/mv -f $$fn $(TOP)/Lib; done
endif
ifeq ($(CLUSTER),x64_macosx) 
$(TOP)/Lib/$(TARGET_SHARED2):	$(DLLOBJS2)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED2)              \
             $(DLLOBJS2) $(LFLAGS) $(DLLLIBS2)
	for fn in $(TOP)/Lib/$(TARGET_SHARED2); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED2); do /bin/mv -f $$fn $(TOP)/Lib; done
endif
ifeq ($(CLUSTER),x64_cygwin)
$(TARGET_SHARED2):	$(DLLOBJS2)
	for fn in $(TARGET_SHARED2); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.exp); do /bin/rm -f $$fn; done
ifneq ($(CXX_COMPILER_VERSION),mingw-gcc)
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.pdb); do /bin/rm -f $$fn; done
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS2) /out:"$(TARGET_SHARED2)" \
             /pdb:"$(TARGET_SHARED2:.$(SOEXT)=.pdb)" \
             $(DLLOBJS2) $(LFLAGS)
else
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS2) /out:"$(TARGET_SHARED2)" \
             /pdb:"$(TARGET_SHARED2:.$(SOEXT)=.pdb)" \
             $(DLLOBJS2) $(LFLAGS)
	$(LIBCMD) /out:"$(TARGET_SHARED2:.$(SOEXT)=.lib)" $(DLLOBJS2)
endif
endif
ifeq ($(CLUSTER),x64_linux) 
$(TOP)/Lib/$(TARGET_SHARED3):	$(DLLOBJS3)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED3)             \
             $(DLLOBJS3) $(LFLAGS) $(DLLLIBS3)
	for fn in $(TOP)/Lib/$(TARGET_SHARED3); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED3); do /bin/mv -f $$fn $(TOP)/Lib; done
endif
ifeq ($(CLUSTER),x64_macosx) 
$(TOP)/Lib/$(TARGET_SHARED3):	$(DLLOBJS3)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED3)             \
             $(DLLOBJS3) $(LFLAGS) $(DLLLIBS3)
	for fn in $(TOP)/Lib/$(TARGET_SHARED3); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED3); do /bin/mv -f $$fn $(TOP)/Lib; done
endif
ifeq ($(CLUSTER),x64_cygwin)
$(TARGET_SHARED3):	$(DLLOBJS3)
	for fn in $(TARGET_SHARED3); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.exp); do /bin/rm -f $$fn; done
ifneq ($(CXX_COMPILER_VERSION),mingw-gcc)
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.pdb); do /bin/rm -f $$fn; done
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS3) /out:"$(TARGET_SHARED3)" \
             /pdb:"$(TARGET_SHARED3:.$(SOEXT)=.pdb)" \
             $(DLLOBJS3) $(LFLAGS)
else
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS3) /out:"$(TARGET_SHARED3)" \
             /pdb:"$(TARGET_SHARED3:.$(SOEXT)=.pdb)" \
             $(DLLOBJS3) $(LFLAGS)
	$(LIBCMD) /out:"$(TARGET_SHARED3:.$(SOEXT)=.lib)" $(DLLOBJS3)
endif
endif

ifndef NO_DEPEND
-include $(DEPS)
endif

moc:
	$(foreach name,$(HDRS),$(shell $(QT_MOC_PARSER) $(QT_DEFS) $(QT_MOC_INCDIRS) $(EXTRA_MOC_INCDIRS) $(name) -o moc_$(basename $(notdir $(name))).cxx))

rcc:
	$(foreach name,$(RCFILES),$(shell $(QT_RCC_CMD) $(name) --name $(basename $(notdir $(name))) -o rcc_$(basename $(notdir $(name))).cxx))

ui:
	$(foreach name,$(UIFILES),$(shell $(QT_UIC_CMD) $(name) -o ui_$(basename $(notdir $(name))).h))

create_exports_h:
	echo "#ifndef $(MODULE_NAME_ALL_CAPS)_EXPORT_H" > $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#define $(MODULE_NAME_ALL_CAPS)_EXPORT_H" >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo ""  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#include \"SimVascular.h\""  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo ""  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#ifdef $(MODULE_NAME_ALL_CAPS)_STATIC_DEFINE"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#  define $(MODULE_NAME_ALL_CAPS)_EXPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#  define $(MODULE_NAME_ALL_CAPS)_NO_EXPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#else"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#  ifndef $(MODULE_NAME_ALL_CAPS)_EXPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#    ifdef $(MODULE_NAME)_EXPORTS"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "       /* We are building this library */"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#      define $(MODULE_NAME_ALL_CAPS)_EXPORT SV_DLL_EXPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#    else"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "       /* We are using this library */"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#      define $(MODULE_NAME_ALL_CAPS)_EXPORT SV_DLL_IMPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#    endif"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#  endif"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo ""  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#  ifndef $(MODULE_NAME_ALL_CAPS)_NO_EXPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#    define $(MODULE_NAME_ALL_CAPS)_NO_EXPORT"  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#  endif" >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#endif" >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo ""  >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h
	echo "#endif" >> $(TOP)/../Code/Source/Include/Make/$(MODULE_NAME)Exports.h


clean:
	for fn in $(BUILD_DIR); do /bin/rm -f -r $$fn;done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in *_wrap.cxx*; do /bin/rm -f $$fn; done
	for fn in moc_*.cxx; do /bin/rm -f $$fn; done
	for fn in ui_*.h; do /bin/rm -f $$fn; done
	for fn in rcc_*.cxx; do /bin/rm -f $$fn; done
	if [ -e us_init.cxx ];then /bin/rm -f us_init.cxx;fi
	for fn in $(TOP)/Lib/$(TARGET_LIB); do /bin/rm -f $$fn; done
	if [ -n "$(TARGET_SHARED)" ];then for fn in $(TARGET_SHARED:.$(SOEXT)=.*); do /bin/rm -f $$fn; done;fi
	if [ -n "$(TARGET_SHARED2)" ];then for fn in $(TARGET_SHARED2:.$(SOEXT)=.*); do /bin/rm -f $$fn; done;fi
	if [ -n "$(TARGET_SHARED3)" ];then for fn in $(TARGET_SHARED3:.$(SOEXT)=.*); do /bin/rm -f $$fn; done;fi
	if [ -n "$(TOP)/Bin/plugins/lib_$(TARGET_LIB_NAME).$(SOEXT)" ];then for fn in $(TOP)/Bin/plugins/lib_$(TARGET_LIB_NAME).*; do /bin/rm -f $$fn; done;fi

veryclean: clean
	if [ -e obj ];then /bin/rm -f -r obj;fi
