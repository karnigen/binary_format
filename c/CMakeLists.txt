cmake_minimum_required(VERSION 3.0.0)
project(binf VERSION 0.1.0 LANGUAGES C CXX)


add_executable(binf)
target_sources(binf PUBLIC  main.c binary_format/binf.c)
target_compile_options(binf PUBLIC -fpic -Wall -fsanitize=address)
target_link_libraries(binf PUBLIC -fsanitize=address)
set_target_properties(binf PROPERTIES
    C_STANDARD 90                     # 90 99 11 17 23
    RUNTIME_OUTPUT_DIRECTORY ../bin
)

# find_package(Armadillo)
# find_package(OpenMP)

add_executable(binfpp)
target_sources(binfpp PUBLIC  main.cpp binary_format/binf.c)
target_compile_options(binfpp PUBLIC -fpic -Wall -fsanitize=address)
set_target_properties(binfpp PROPERTIES
    CXX_STANDARD 11                     # 98 11 14 17 20 23
    RUNTIME_OUTPUT_DIRECTORY ../bin
)
target_link_libraries(binfpp PUBLIC 
    -fsanitize=address
    # fmt
    # OpenMP::OpenMP_CXX
    # ${ARMADILLO_LIBRARIES}
)


# include(CTest)
# enable_testing()

# set(CPACK_PROJECT_NAME ${PROJECT_NAME})
# set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
# include(CPack)
