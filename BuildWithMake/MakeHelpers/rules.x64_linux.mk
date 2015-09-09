ifeq ($(CLUSTER), x64_linux)
%.d: %.cxx
	$(SHELL) -ec '$(CXXDEP) $(CXXFLAGS) $< \
              | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; \
              [ -s $@ ] || rm -f $@'

%.d: %.c
	$(SHELL) -ec '$(CCDEP) $(CCFLAGS) $< \
              | sed '\''s/\($*\)\.o[ :]*/\1.o $@ : /g'\'' > $@; \
              [ -s $@ ] || rm -f $@'

%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $<

%.o: %.c
	$(CC) $(CCFLAGS) -c $<

%.o: %.f
	$(F90) $(FFLAGS) -c $<

%.o: %.f90
	$(F90) $(FFLAGS) -c $<

endif
