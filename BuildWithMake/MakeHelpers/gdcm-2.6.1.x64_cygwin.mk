# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

ifeq ($(CLUSTER), x64_cygwin)
    SV_GDCM_DEFS   =
    SV_GDCM_TOP    = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/gdcm-2.6.1
    SV_GDCM_INCDIR = -I $(SV_GDCM_TOP)/include/gdcm-2.6
    SV_GDCM_LIBS   = $(LIBPATH_COMPILER_FLAG)$(SV_GDCM_TOP)/lib \
                     $(LIBFLAG)gdcmCommon.lib \
                     $(LIBFLAG)gdcmDICT.lib    \
                     $(LIBFLAG)gdcmDSED.lib    \
                     $(LIBFLAG)gdcmIOD.lib     \
                     $(LIBFLAG)gdcmjpeg12.lib  \
                     $(LIBFLAG)gdcmjpeg16.lib  \
                     $(LIBFLAG)gdcmjpeg8.lib   \
                     $(LIBFLAG)gdcmMSFF.lib
    SV_GDCM_DLLS   =  $(SV_GDCM_TOP)/bin
    SV_GDCM_SO_PATH = $(SV_GDCM_TOP)/bin
#    SV_GDCM_LIBS  += $(LIBFLAG)Advapi32.lib $(LIBFLAG)Ws2_32.lib
endif
