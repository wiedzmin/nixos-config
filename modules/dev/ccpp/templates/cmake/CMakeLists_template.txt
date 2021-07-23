cmake_minimum_required(VERSION 3.9)
#TODO: provide @-delimited placeholders with actual values

target_compile_features(app cxx_std_11)

project(@SOMETHING@)

file(GLOB TARGET_SRC "CMAKE_CURRENT_SOURCE_DIR/*.cpp")

target_include_directories(@LIB@ PUBLIC ...)

add_executable(@SOMETHING@ ${TARGET_SRC})

target_link_libraries (@SOMETHING@ @LIBS@)
