ifeq ($(CLUSTER), x86_cygwin)
%.d: %.cxx
	$(SHELL) -ec '$(CXXDEP) $(CXXFLAGS) $< \
	      | sed '\''s/\($*\)\.o[ :]*/\1.obj $@ : /g'\'' > $@; \
	      [ -s $@ ] || rm -f $@'

%.d: %.c
	$(SHELL) -ec '$(CCDEP) $(CCFLAGS) $< \
	      | sed '\''s/\($*\)\.o[ :]*/\1.obj $@ : /g'\'' > $@; \
	      [ -s $@ ] || rm -f $@'

%.obj: %.cxx
	$(CXX) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CXXFLAGS) -c $<

%.obj: %.c
	$(CC) $(DEBUG_FLAGS) $(OPT_FLAGS) $(CCFLAGS) -c $<

%.obj: %.f
	$(F90) $(DEBUG_FLAGS) $(OPT_FLAGS) $(FFLAGS) -c $<
endif
