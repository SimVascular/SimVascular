ifeq ($(CLUSTER), x64_macosx)
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
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.f
	$(F90) $(FFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.f90
	$(F90) $(FFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.cxx
	mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CCFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.f
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< -o $@

$(BUILD_MPI_DIR)/%.o: %.f90
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< -o $@

endif
