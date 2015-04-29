
set(TKCXIMAGE_NEEDED_LIBS Tkcximage)

#if(SIMVASCULAR_USE_SV_EXTERN)
set(TKCXIMAGE_PATH_PREFIX "tkcximage-0.98.9/tcltk-8.5.11/bin" CACHE TYPE PATH)
	if(LINUX)		
		message(WARNING "Tkcximage is not currently supported on Linux systems")
		set(TKCXIMAGE_FULL_PATH "${OpenLibs_Bin_Directory}/${TKCXIMAGE_PATH_PREFIX}")
	endif()
	
	if(IS64 AND (WIN32 OR CYGWIN))
		set(TKCXIMAGE_FULL_PATH "${OpenLibs_Bin_Directory}/${TKCXIMAGE_PATH_PREFIX}")
		
	endif()

	if((WIN32 OR CYGWIN) AND NOT IS64)
		set(TKCXIMAGE_FULL_PATH "${OpenLibs_Bin_Directory}/${TKCXIMAGE_PATH_PREFIX}")
	endif()
#endif()

set(TKCXIMAGE_LIB_DIR "${TKCXIMAGE_FULL_PATH}")
set(TKCXIMAGE_INCLUDE_DIR "${TKCXIMAGE_FULL_PATH}")

GENLIBS(TKCXIMAGE_LIBRARY "${TKCXIMAGE_NEEDED_LIBS}" "TKCXIMAGE" "${TKCXIMAGE_LIB_DIR}")

link_directories(${TKCXIMAGE_LIB_DIR})
include_directories(${TKCXIMAGE_INCLUDE_DIR})
