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

all:	lib

lib:	$(TOP)/Lib/$(TARGET_LIB)

shared: $(TOP)/Lib/$(TARGET_SHARED) $(TOP)/Lib/$(TARGET_SHARED2) $(TOP)/Lib/$(TARGET_SHARED3)

$(TOP)/Lib/$(TARGET_LIB):	$(OBJS)
	$(AR) $(TARGET_LIB) $(OBJS)
	for fn in $(TOP)/Lib/$(TARGET_LIB); do /bin/rm -f $$fn; done 
	for fn in $(TARGET_LIB); do /bin/mv -f $$fn  $(TOP)/Lib/$(TARGET_LIB); done

ifeq ($(CLUSTER),x64_linux) 
$(TOP)/Lib/$(TARGET_SHARED):	$(DLLOBJS)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED)  \
             $(DLLOBJS) $(LFLAGS) $(DLLLIBS)
	for fn in $(TOP)/Lib/$(TARGET_SHARED); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED); do /bin/mv -f $$fn $(TOP)/Lib; done
#	$(LINK_EXE) dummy.exe $(LFLAGS) \
             $(DLLOBJS) $(CXX_LIBS) $(VTK_LIBS) $(PARASOLID_LIBS) $(MESHSIM_LIBS) $(ZLIB_LIBS) -l_lib_simvascular_sysgeom -l_lib_simvascular_solid -l_lib_simvascular_repository -l_lib_simvascular_utils -l_simvascular_globals
#	$(LINK_EXE) dummy.exe $(LFLAGS) \
             $(DLLOBJS) $(LFLAGS) $(DLLLIBS)
else
$(TOP)/Lib/$(TARGET_SHARED):	$(DLLOBJS)
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS) /out:"$(TARGETDIR)/$(TARGET_SHARED)" \
             /pdb:"$(TARGETDIR)/$(TARGET_SHARED:.$(SOEXT)=.pdb)" \
             $(DLLOBJS) $(LFLAGS)
	for fn in $(TOP)/Lib/$(TARGET_SHARED); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED:.$(SOEXT)=.exp); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED:.$(SOEXT)=.pdb); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED:.$(SOEXT)=.$(SOEXT).manifest); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED); do /bin/mv -f $$fn $(TOP)/Lib; done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.$(STATICEXT)); do /bin/mv -f $$fn $(TOP)/Lib; done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.exp); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.pdb); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
	for fn in $(TARGET_SHARED:.$(SOEXT)=.$(SOEXT).manifest); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
endif

ifeq ($(CLUSTER),x64_linux) 
$(TOP)/Lib/$(TARGET_SHARED2):	$(DLLOBJS2)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED2)              \
             $(DLLOBJS2) $(LFLAGS) $(DLLLIBS2)
	for fn in $(TOP)/Lib/$(TARGET_SHARED2); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED2); do /bin/mv -f $$fn $(TOP)/Lib; done
else
$(TOP)/Lib/$(TARGET_SHARED2):	$(DLLOBJS2)
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS2) /out:"$(TARGETDIR)/$(TARGET_SHARED2)" \
             /pdb:"$(TARGETDIR)/$(TARGET_SHARED2:.$(SOEXT)=.pdb)" \
             $(DLLOBJS2) $(LFLAGS)
	for fn in $(TOP)/Lib/$(TARGET_SHARED2); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED2:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED2:.$(SOEXT)=.exp); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED2:.$(SOEXT)=.pdb); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED2:.$(SOEXT)=.$(SOEXT).manifest); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED2); do /bin/mv -f $$fn $(TOP)/Lib; done
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.$(STATICEXT)); do /bin/mv -f $$fn $(TOP)/Lib; done
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.exp); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.pdb); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
	for fn in $(TARGET_SHARED2:.$(SOEXT)=.$(SOEXT).manifest); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
endif

ifeq ($(CLUSTER),x64_linux) 
$(TOP)/Lib/$(TARGET_SHARED3):	$(DLLOBJS3)
	$(SHAR) $(SHARED_LFLAGS) $(TARGETDIR)/$(TARGET_SHARED3)             \
             $(DLLOBJS3) $(LFLAGS) $(DLLLIBS3)
	for fn in $(TOP)/Lib/$(TARGET_SHARED3); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED3); do /bin/mv -f $$fn $(TOP)/Lib; done
else
$(TOP)/Lib/$(TARGET_SHARED3):	$(DLLOBJS3)
	$(SHAR) $(SHARED_LFLAGS) $(DLLLIBS3) /out:"$(TARGETDIR)/$(TARGET_SHARED3)" \
             /pdb:"$(TARGETDIR)/$(TARGET_SHARED3:.$(SOEXT)=.pdb)" \
             $(DLLOBJS3) $(LFLAGS)
	for fn in $(TOP)/Lib/$(TARGET_SHARED3); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED3:.$(SOEXT)=.$(STATICEXT)); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED3:.$(SOEXT)=.exp); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED3:.$(SOEXT)=.pdb); do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_SHARED3:.$(SOEXT)=.$(SOEXT).manifest); do /bin/rm -f $$fn; done
	for fn in $(TARGET_SHARED3); do /bin/mv -f $$fn $(TOP)/Lib; done
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.$(STATICEXT)); do /bin/mv -f $$fn $(TOP)/Lib; done
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.exp); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.pdb); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
	for fn in $(TARGET_SHARED3:.$(SOEXT)=.$(SOEXT).manifest); do (if [ -e $$fn ];then /bin/mv -f $$fn $(TOP)/Lib;fi); done
endif

ifndef NO_DEPEND
-include $(DEPS)
endif

clean: veryclean

veryclean:
	for fn in *.$(OBJECTEXT); do /bin/rm -f $$fn; done
	for fn in *~; do /bin/rm -f $$fn;done
	for fn in *.d; do /bin/rm -f $$fn; done
	for fn in *.pdb; do /bin/rm -f $$fn; done
	for fn in *.exp; do /bin/rm -f $$fn; done
	for fn in *_wrap.cxx*; do /bin/rm -f $$fn; done
	for fn in $(TOP)/Lib/$(TARGET_LIB); do /bin/rm -f $$fn; done
	if [ -n "$(TARGET_SHARED)" ];then for fn in $(TOP)/Lib/$(TARGET_SHARED:.$(SOEXT)=.*); do /bin/rm -f $$fn; done;fi
	if [ -n "$(TARGET_SHARED2)" ];then for fn in $(TOP)/Lib/$(TARGET_SHARED2:.$(SOEXT)=.*); do /bin/rm -f $$fn; done;fi
	if [ -n "$(TARGET_SHARED3)" ];then for fn in $(TOP)/Lib/$(TARGET_SHARED3:.$(SOEXT)=.*); do /bin/rm -f $$fn; done;fi
