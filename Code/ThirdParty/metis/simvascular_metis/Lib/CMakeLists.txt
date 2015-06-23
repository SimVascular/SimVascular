
include_directories(./)

set(CSRCS coarsen.c fm.c initpart.c match.c ccgraph.c memory.c 
       pmetis.c pqueue.c refine.c util.c timing.c debug.c 
       bucketsort.c graph.c stat.c kmetis.c kwayrefine.c 
       kwayfm.c balance.c ometis.c srefine.c sfm.c separator.c 
       mincover.c mmd.c mesh.c meshpart.c frename.c fortran.c 
       myqsort.c compress.c parmetis.c estmem.c 
       mpmetis.c mcoarsen.c mmatch.c minitpart.c mbalance.c 
       mrefine.c mutil.c mfm.c mkmetis.c mkwayrefine.c mkwayfmh.c 
       mrefine2.c minitpart2.c mbalance2.c mfm2.c 
       kvmetis.c kwayvolrefine.c kwayvolfm.c subdomains.c)
message("METIS_LIBRARY_NAME ${METIS_LIBRARY_NAME}")
add_library(${METIS_LIBRARY_NAME} STATIC ${CSRCS})
# install(TARGETS ${METIS_LIBRARY} 
#     RUNTIME DESTINATION ${METIS_INSTALL_DIR}
#     LIBRARY DESTINATION ${METIS_INSTALL_DIR}
#     ARCHIVE DESTINATION ${METIS_INSTALL_DIR}
#     )


# file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/temp)
# set(TEMP_DIR ${CMAKE_CURRENT_SOURCE_DIR}/temp)
# set(HDRS)
# #copy and rename files
# FILE(READ "metis.h" METIS_H_TEMP)

# set(filename "macros.h")
# install(FILES ${filename} DESTINATION ${METIS_INSTALL_DIR} RENAME metis_${filename})
# STRING(REGEX REPLACE ${filename} metis_${filename} METIS_H_TEMP ${METIS_H_TEMP})

# set(filename "defs.h")
# file(COPY ${filename} DESTINATION ${TEMP_DIR})
# file(RENAME ${TEMP_DIR}/${filename} 
#      ${TEMP_DIR}/metis_${filename} )
# STRING(REGEX REPLACE ${filename} metis_${filename} METIS_H_TEMP ${METIS_H_TEMP})

# set(filename "proto.h")
# install(FILES ${filename} DESTINATION ${METIS_INSTALL_DIR} RENAME metis_${filename})
# STRING(REGEX REPLACE ${filename} metis_${filename} METIS_H_TEMP ${METIS_H_TEMP})

# set(filename "rename.h")
# install(FILES ${filename} DESTINATION ${METIS_INSTALL_DIR} RENAME metis_${filename})
# STRING(REGEX REPLACE ${filename} metis_${filename} METIS_H_TEMP ${METIS_H_TEMP})

# set(filename "struct.h")
# install(FILES ${filename} DESTINATION ${METIS_INSTALL_DIR} RENAME metis_${filename})
# STRING(REGEX REPLACE ${filename} metis_${filename} METIS_H_TEMP ${METIS_H_TEMP})

# set(filename ${TEMP_DIR}/metis.h)
# FILE(WRITE ${filename} ${METIS_H_TEMP})
# install(FILES ${filename} DESTINATION ${METIS_INSTALL_DIR})
