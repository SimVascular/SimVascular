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

HDF5_MAJOR_VERSION=1
HDF5_MINOR_VERSION=10
HDF5_PATCH_VERSION=1
HDF5_VERSION=$(HDF5_MAJOR_VERSION).$(HDF5_MINOR_VERSION).$(HDF5_PATCH_VERSION)

SV_HDF5_DEFS   =
SV_HDF5_TOP    = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/hdf5-$(HDF5_VERSION)
SV_HDF5_INCDIR = -I $(SV_HDF5_TOP)/include
SV_HDF5_LIBS   = $(LIBPATH_COMPILER_FLAG)$(SV_HDF5_TOP)/lib \
  $(LIBFLAG)hdf5$(LIBLINKEXT) \
  $(LIBFLAG)hdf5_cpp$(LIBLINKEXT) \
  $(LIBFLAG)hdf5_hl$(LIBLINKEXT) \
  $(LIBFLAG)hdf5_hl_cpp$(LIBLINKEXT) \
  $(LIBFLAG)hdf5_tools$(LIBLINKEXT)
SV_HDF5_DLLS   =  $(SV_HDF5_TOP)/lib
SV_HDF5_SO_PATH = $(SV_HDF5_TOP)/lib
