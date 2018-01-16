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

ifeq ($(CLUSTER), x64_linux)
$(BUILD_DIR)/%.d: %.cxx
	$(SHELL) -ec '$(CXXDEP) $(CXXFLAGS) $< \
              | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; \
              [ -s $@ ] || rm -f $@'

$(BUILD_DIR)/%.d: %.c
	$(SHELL) -ec '$(CCDEP) $(CCFLAGS) $< \
              | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; \
              [ -s $@ ] || rm -f $@'

$(BUILD_MPI_DIR)/%.d: %.cxx
	$(SHELL) -ec '$(CXXDEP) $(CXXFLAGS) $< \
              | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; \
              [ -s $@ ] || rm -f $@'

$(BUILD_MPI_DIR)/%.d: %.c
	$(SHELL) -ec '$(CCDEP) $(CCFLAGS) $< \
              | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; \
              [ -s $@ ] || rm -f $@'

$(BUILD_DIR)/%.o: %.cxx
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.c
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CC) $(CCFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.f
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.f90
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.cxx
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.c
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(CC) $(CCFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.f
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.f90
	$(SV_QUIET_FLAG)mkdir -p $(dir $@)
	$(SV_QUIET_FLAG)echo "  building src ($<)"
	$(SV_QUIET_FLAG)$(F90) $(FFLAGS) -c $< -o $@

endif
