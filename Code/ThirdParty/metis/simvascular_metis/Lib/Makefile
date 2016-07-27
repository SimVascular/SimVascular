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

TOP=../../../../../BuildWithMake

include $(TOP)/include.mk

CCFLAGS = $(GLOBAL_CCFLAGS) -I .

HDRS	= defs.h macros.h metis.h \
          proto.h rename.h struct.h

CSRCS	= coarsen.c fm.c initpart.c match.c ccgraph.c memory.c \
       pmetis.c pqueue.c refine.c util.c timing.c debug.c \
       bucketsort.c graph.c stat.c kmetis.c kwayrefine.c \
       kwayfm.c balance.c ometis.c srefine.c sfm.c separator.c \
       mincover.c mmd.c mesh.c meshpart.c frename.c fortran.c \
       myqsort.c compress.c parmetis.c estmem.c \
       mpmetis.c mcoarsen.c mmatch.c minitpart.c mbalance.c \
       mrefine.c mutil.c mfm.c mkmetis.c mkwayrefine.c mkwayfmh.c \
       mrefine2.c minitpart2.c mbalance2.c mfm2.c \
       kvmetis.c kwayvolrefine.c kwayvolfm.c subdomains.c

TARGET_LIB_NAME = simvascular_thirdparty_metis

include $(TOP)/targetlib.mk


