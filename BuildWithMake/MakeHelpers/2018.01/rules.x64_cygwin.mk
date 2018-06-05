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

$(BUILD_DIR)/%.d: %.cxx
	$(SHELL) -ec '$(CXXDEP) $(CXXFLAGS) $< \
	      | sed '\''s/\($*\)\.o[ :]*/\1.obj $@ : /g'\'' > $@; \
	      [ -s $@ ] || rm -f $@'

$(BUILD_DIR)/%.d: %.c
	$(SHELL) -ec '$(CCDEP) $(CCFLAGS) $< \
	      | sed '\''s/\($*\)\.o[ :]*/\1.obj $@ : /g'\'' > $@; \
	      [ -s $@ ] || rm -f $@'

$(BUILD_MPI_DIR)/%.d: %.cxx
	$(SHELL) -ec '$(CXXDEP) $(CXXFLAGS) $< \
	      | sed '\''s/\($*\)\.o[ :]*/\1.obj $@ : /g'\'' > $@; \
	      [ -s $@ ] || rm -f $@'

$(BUILD_MPI_DIR)/%.d: %.c
	$(SHELL) -ec '$(CCDEP) $(CCFLAGS) $< \
	      | sed '\''s/\($*\)\.o[ :]*/\1.obj $@ : /g'\'' > $@; \
	      [ -s $@ ] || rm -f $@'

ifeq ($(CXX_COMPILER_VERSION),mingw-gcc)
$(BUILD_DIR)/%.obj: %.cxx
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) -c $< -o $@
$(BUILD_DIR)/%.obj: %.c
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) -c $< -o $@
$(BUILD_MPI_DIR)/%.obj: %.cxx
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) -c $< -o $@
$(BUILD_MPI_DIR)/%.obj: %.c
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) -c $< -o $@
else
$(BUILD_DIR)/%.obj: %.cxx
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) /Fd:$(BUILD_DIR)/vc.pdb -c $< /Fo: $@
$(BUILD_DIR)/%.obj: %.c
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) /Fd:$(BUILD_DIR)/vc.pdb -c $< /Fo: $@
$(BUILD_MPI_DIR)/%.obj: %.cxx
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) /Fd:$(BUILD_MPI_DIR)/vc.pdb -c $< /Fo: $@
$(BUILD_MPI_DIR)/%.obj: %.c
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) /Fd:$(BUILD_MPI_DIR)/vc.pdb -c $< /Fo: $@
endif

ifeq ($(FORTRAN_COMPILER_VERSION),mingw-gfortran)
$(BUILD_DIR)/%.obj: %.f
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< -o $@
$(BUILD_MPI_DIR)/%.obj: %.f
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< -o $@
else
$(BUILD_DIR)/%.obj: %.f
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_DIR)/vc.pdb /module:$(BUILD_DIR) /Fo$@
$(BUILD_DIR)/%.obj: %.f90
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_DIR)/vc.pdb /module:$(BUILD_DIR) /Fo$@
$(BUILD_MPI_DIR)/%.obj: %.f
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_MPI_DIR)/vc.pdb /module:$(BUILD_MPI_DIR) /Fo$@
$(BUILD_MPI_DIR)/%.obj: %.f90
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_MPI_DIR)/vc.pdb /module:$(BUILD_MPI_DIR) /Fo$@
endif

endif
