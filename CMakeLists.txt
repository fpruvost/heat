cmake_minimum_required (VERSION 2.8)
project (PatcHeat)
enable_language(Fortran) 
enable_testing()

# add library Heat
add_subdirectory(lib)

# try to find MPI on the system
find_package(MPI)

# compiled with MPI for parallel version
if(MPI_FOUND)
    message(STATUS "MPI found; compiling both the parallel and sequential versions")
	# add include path
	include_directories(${MPI_INCLUDE_PATH})
	# add the parallel executable
	set(HEAT_PAR_SOURCES heat_par.f90 mat_utils.f90)
	add_executable(patcHeatPar ${HEAT_PAR_SOURCES})
	# link patcHeatPar executable with Heat and MPI libs
	target_link_libraries(patcHeatPar Heat ${MPI_Fortran_LIBRARIES})
	# Define the test suite
    add_test(patcHeatPar4 ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 4 ./patcHeatPar 10 10 200 2 2 0) 
	# install process
    install(TARGETS patcHeatPar DESTINATION .)
else(MPI_FOUND)
	message(WARNING "MPI not found; only the sequential version will be built")
endif(MPI_FOUND)

# add the sequential executable
set(HEAT_SOURCES heat_seq.f90 mat_utils.f90)
add_executable(patcHeatSeq ${HEAT_SOURCES})
# link patcHeatSeq executable with Heat lib
target_link_libraries(patcHeatSeq Heat)
# install process
install(TARGETS patcHeatSeq DESTINATION .)

# Define the test suite
add_test(patcHeatSeqUsage ./patcHeatSeq)
set_tests_properties (patcHeatSeqUsage PROPERTIES PASS_REGULAR_EXPRESSION "Usage*")
add_test(patcHeatSeq10Err ./patcHeatSeq 10 10 0 0)
set_tests_properties (patcHeatSeq10Err PROPERTIES PASS_REGULAR_EXPRESSION "1.118")

# Support for building a package
set(CPACK_PACKAGE_NAME patcHeat)
set(CPACK_SOURCE_IGNORE_FILES "/build/" "/.*\\\\.swp$")
set(CPACK_SOURCE_GENERATOR "TGZ")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "patcHeat")
include (CPack)
