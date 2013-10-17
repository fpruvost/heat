cmake_minimum_required (VERSION 2.8)
project (PatcHeat)
enable_language(Fortran) 

# add the executable
set(HEAT_SOURCES heat_seq.f90 heat.f90 mat_utils.f90)
add_executable(patcHeatSeq ${HEAT_SOURCES})
