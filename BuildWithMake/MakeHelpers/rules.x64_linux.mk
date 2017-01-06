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
