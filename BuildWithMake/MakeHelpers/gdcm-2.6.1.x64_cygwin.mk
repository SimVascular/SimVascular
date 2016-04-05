ifeq ($(CLUSTER), x64_cygwin)
    SV_GDCM_DEFS   =
    SV_GDCM_TOP    = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/gdcm-2.6.1
    SV_GDCM_INCDIR = -I $(SV_GDCM_TOP)/Source/DataStructureAndEncodingDefinition \
                     -I $(SV_GDCM_TOP)/Source/MessageExchangeDefinition \
                     -I $(SV_GDCM_TOP)/Source/InformationObjectDefinition \
                     -I $(SV_GDCM_TOP)/Source/Common \
                     -I $(SV_GDCM_TOP)/Source/DataDictionary \
                     -I $(SV_GDCM_TOP)/Source/MediaStorageAndFileFormat \
                     -I $(SV_GDCM_TOP)/Source/Common \
                     -I $(SV_GDCM_TOP)/GDCM
    SV_GDCM_LIBS   = $(LIBPATH_COMPILER_FLAG)$(SV_GDCM_TOP)/bin/RelWithDebInfo \
                     $(LIBFLAG)gdcmCommon.lib \
                     $(LIBFLAG)gdcmDICT.lib    \
                     $(LIBFLAG)gdcmDSED.lib    \
                     $(LIBFLAG)gdcmIOD.lib     \
                     $(LIBFLAG)gdcmjpeg12.lib  \
                     $(LIBFLAG)gdcmjpeg16.lib  \
                     $(LIBFLAG)gdcmjpeg8.lib   \
                     $(LIBFLAG)gdcmMSFF.lib
#    SV_GDCM_LIBS  += $(LIBFLAG)Advapi32.lib $(LIBFLAG)Ws2_32.lib
endif
