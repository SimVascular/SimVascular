ifeq ($(CLUSTER), x64_macosx)
    SV_GDCM_DEFS   =
    SV_GDCM_TOP    = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/gdcm-2.6.1
    SV_GDCM_INCDIR = -I $(SV_GDCM_TOP)/include/gdcm-2.6
    SV_GDCM_LIBS   = $(LIBPATH_COMPILER_FLAG)$(SV_GDCM_TOP)/lib \
                     $(LIBFLAG)gdcmCommon$(LIBLINKEXT) \
                     $(LIBFLAG)gdcmDICT$(LIBLINKEXT)    \
                     $(LIBFLAG)gdcmDSED$(LIBLINKEXT)    \
                     $(LIBFLAG)gdcmIOD$(LIBLINKEXT)    \
                     $(LIBFLAG)gdcmjpeg12$(LIBLINKEXT)  \
                     $(LIBFLAG)gdcmjpeg16$(LIBLINKEXT)  \
                     $(LIBFLAG)gdcmjpeg8$(LIBLINKEXT)   \
                     $(LIBFLAG)gdcmMSFF$(LIBLINKEXT) \
                     $(LIBFLAG)gdcmopenjpeg$(LIBLINKEXT) \
                     $(LIBFLAG)gdcmexpat$(LIBLINKEXT) \
                     $(LIBFLAG)gdcmuuid$(LIBLINKEXT) \
                     $(LIBFLAG)gdcmcharls$(LIBLINKEXT) \
                     $(LIBFLAG)gdcmzlib$(LIBLINKEXT)
    SV_GDCM_DLLS   =  $(SV_GDCM_TOP)/lib
    SV_GDCM_SO_PATH = $(SV_GDCM_TOP)/lib
endif
