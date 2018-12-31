cmake_minimum_required(VERSION 3.5.0)
project(__PROJECT-NAME__)

STRING(TOLOWER "${CMAKE_BUILD_TYPE}" CMAKE_BUILD_TYPE_LOWER)

if(CMAKE_BUILD_TYPE_LOWER STREQUAL "debug")
  message("Building Debug target.")
elseif(CMAKE_BUILD_TYPE_LOWER STREQUAL "release")
  message("Building Release target.")
else()
  message("Build Type is None or not set.")
endif()

include_directories(include)

# Debug 
# ------------------------------

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG
  ${CMAKE_SOURCE_DIR}/lib
  CACHE PATH
  "Single directory for all libraries.")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG
  ${CMAKE_SOURCE_DIR}/bin
  CACHE PATH
  "Single directory for all executables.")

# Release
# ------------------------------
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
  ${CMAKE_BINARY_DIR}/lib
  CACHE PATH
  "Single directory for all libraries.")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
  ${CMAKE_BINARY_DIR}/bin
  CACHE PATH
  "Single directory for all executables.")

# Build
# ------------------------------
set(SOURCES src/main.cpp)

add_executable(__TARGET_NAME__ ${SOURCES})

