cmake_minimum_required(VERSION 3.10)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)

project(OpenGLCubeDemo)

add_executable(main
    main.cpp
    shader.cpp)

find_package(OpenGL REQUIRED COMPONENTS OpenGL)
find_package(GLUT REQUIRED)
find_package(GLEW REQUIRED)

add_dependencies(main
    OpenGL::OpenGL)
include_directories(
    ${GLUT_INCLUDE_DIRS})
target_link_libraries(main
    OpenGL::OpenGL
    GLU
    ${GLUT_LIBRARY}
    GLEW::GLEW)
