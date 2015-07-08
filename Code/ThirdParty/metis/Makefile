# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2015 Open Source Medical Software Corporation,
#                    University of California, San Diego.
# 
# All rights reserved. 
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

METIS_SUBDIRS = simvascular_metis/Lib

lib:	createHeaderFile libsubdirs

shared:

libsubdirs:
	@for i in ${METIS_SUBDIRS}; do ( \
	  cd $$i; \
	  $(MAKE)) ; done

clean:
	for i in ${METIS_SUBDIRS}; do ( \
	  cd $$i; \
	  $(MAKE) clean ) ; done

veryclean: clean
	rm -f simvascular_metis.h
	rm -f metis_defs.h
	rm -f metis_macros.h
	rm -f metis.h
	rm -f metis_proto.h
	rm -f metis_rename.h
	rm -f metis_struct.h
	for i in ${METIS_SUBDIRS}; do ( \
	  cd $$i; \
	  $(MAKE) veryclean ) ; done

createHeaderFile:
	rm -f simvascular_metis.h
	rm -f metis_defs.h
	rm -f metis_macros.h
	rm -f metis.h
	rm -f metis_proto.h
	rm -f metis_rename.h
	rm -f metis_struct.h
	@echo "#ifndef __simvascular_metis_h" > simvascular_metis.h
	@echo "#define __simvascular_metis_h" >> simvascular_metis.h
	@echo "#include <metis.h>" >> simvascular_metis.h
	@echo "#endif" >> simvascular_metis.h
	cp -f simvascular_metis/Lib/defs.h metis_defs.h
	cp -f simvascular_metis/Lib/macros.h metis_macros.h
	cp -f simvascular_metis/Lib/metis-wrapper.h metis.h
	cp -f simvascular_metis/Lib/proto.h metis_proto.h
	cp -f simvascular_metis/Lib/rename.h metis_rename.h
	cp -f simvascular_metis/Lib/struct.h metis_struct.h
