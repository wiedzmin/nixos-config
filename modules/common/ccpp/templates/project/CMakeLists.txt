cmake_minimum_required(VERSION 3.9)

project(@SOMETHING@) #TODO: provide actual project name

set(SOURCES @SOMETHING@.cpp)

add_executable(@SOMETHING@ ${SOURCES})
target_link_libraries (@SOMETHING@ @LIB@)
