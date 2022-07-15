cmake_minimum_required(VERSION 3.0.0)
project(binfor VERSION 0.1.0 LANGUAGES C Fortran)

# check fotran C interoparability
include(FortranCInterface)
FortranCInterface_VERIFY()

add_executable(binfor)
target_sources(binfor PUBLIC 
    main.f90
    binary_format/binff.f90 
    binary_format/binff.c 
    binary_format/binf.c
)

target_compile_options(binfor PUBLIC -fpic -Wall -fsanitize=address)
target_link_libraries(binfor PUBLIC -fsanitize=address)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fimplicit-none")  # -std=f2018

set_target_properties(binfor PROPERTIES
    C_STANDARD 90                     # 90 99 11 17 23
    Fortran_PREPROCESS ON
    RUNTIME_OUTPUT_DIRECTORY ../bin
)



# include(CTest)
# enable_testing()

# set(CPACK_PROJECT_NAME ${PROJECT_NAME})
# set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
# include(CPack)
