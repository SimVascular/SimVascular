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

SolverIO_SUBDIRS = simvascular_solverio

lib:	createHeaderFile libsubdirs

shared:

libsubdirs:
	@for i in ${SolverIO_SUBDIRS}; do ( \
	  cd $$i; \
	  $(MAKE)) ; done

clean:
	for i in ${SolverIO_SUBDIRS}; do ( \
	  cd $$i; \
	  $(MAKE) clean ) ; done

veryclean: clean
	rm -f simvascular_solverio.h
	for i in ${SolverIO_SUBDIRS}; do ( \
	  cd $$i; \
	  $(MAKE) veryclean ) ; done

createHeaderFile:
	rm -f simvascular_solverio.h
	@echo "#ifndef __simvascular_solverio_h" > simvascular_solverio.h
	@echo "#define __simvascular_solverio_h" >> simvascular_solverio.h
	@echo "#include <simvascular_solverio/cvSolverIO.h>" >> simvascular_solverio.h
	@echo "#endif" >> simvascular_solverio.h


