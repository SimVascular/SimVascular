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
	$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) -c $< -o $@
$(BUILD_DIR)/%.obj: %.c
	$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) -c $< -o $@
$(BUILD_MPI_DIR)/%.obj: %.cxx
	mkdir -p $(dir $@)
	$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) -c $< -o $@
$(BUILD_MPI_DIR)/%.obj: %.c
	mkdir -p $(dir $@)
	$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) -c $< -o $@
else
$(BUILD_DIR)/%.obj: %.cxx
	$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) /Fd:$(BUILD_DIR)/vc.pdb -c $< /Fo: $@
$(BUILD_DIR)/%.obj: %.c
	$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) /Fd:$(BUILD_DIR)/vc.pdb -c $< /Fo: $@
$(BUILD_MPI_DIR)/%.obj: %.cxx
	mkdir -p $(dir $@)
	$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) /Fd:$(BUILD_MPI_DIR)/vc.pdb -c $< /Fo: $@
$(BUILD_MPI_DIR)/%.obj: %.c
	mkdir -p $(dir $@)
	$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) /Fd:$(BUILD_MPI_DIR)/vc.pdb -c $< /Fo: $@
endif

ifeq ($(FORTRAN_COMPILER_VERSION),mingw-gfortran)
$(BUILD_DIR)/%.obj: %.f
	$(F90) $(FFLAGS) -c $< -o $@
$(BUILD_MPI_DIR)/%.obj: %.f
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< -o $@
else
$(BUILD_DIR)/%.obj: %.f
	$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_DIR)/vc.pdb /module:$(BUILD_DIR) /Fo$@
$(BUILD_DIR)/%.obj: %.f90
	$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_DIR)/vc.pdb /module:$(BUILD_DIR) /Fo$@
$(BUILD_MPI_DIR)/%.obj: %.f
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_MPI_DIR)/vc.pdb /module:$(BUILD_MPI_DIR) /Fo$@
$(BUILD_MPI_DIR)/%.obj: %.f90
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< /Fd:$(BUILD_MPI_DIR)/vc.pdb /module:$(BUILD_MPI_DIR) /Fo$@
endif

endif
