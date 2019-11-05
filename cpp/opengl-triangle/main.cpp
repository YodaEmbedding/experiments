#include <cstdlib>

#include <GL/glew.h>
#if __has_include(<GL/freeglut.h>)
#include <GL/freeglut.h>
// #include <GL/freeglut_ext.h>
#else
#include <GL/glut.h>
#endif

#include "shader.h"

// clang-format off
const GLfloat g_vertex_buffer_data[] = {
    -1.0f, -1.0f, 0.0f,
    1.0f, -1.0f, 0.0f,
    0.0f,  1.0f, 0.0f,
};
// clang-format on

class Graphics {
public:
    Graphics() {}

    void init() {
        glClearColor(0.0f, 0.0f, 0.4f, 0.0f);

        glGenVertexArrays(1, &vertexArrayID);
        glBindVertexArray(vertexArrayID);

        programID = LoadShaders("vshader.glsl", "fshader.glsl");

        glGenBuffers(1, &vertex_buffer);
        glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
        glBufferData(
            GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data,
            GL_STATIC_DRAW);
    }

    void dispose() {
        glDeleteBuffers(1, &vertex_buffer);
        glDeleteVertexArrays(1, &vertexArrayID);
        glDeleteProgram(programID);
    }

    void display() {
        glClear(GL_COLOR_BUFFER_BIT);
        glUseProgram(programID);

        GLuint attribute;
        attribute = glGetAttribLocation(programID, "vertexPosition_modelspace");
        glEnableVertexAttribArray(attribute);
        glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
        glVertexAttribPointer(
            attribute, // attribute
            3,         // size
            GL_FLOAT,  // type
            GL_FALSE,  // normalized?
            0,         // stride
            (void*)0   // array buffer offset
        );
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glDisableVertexAttribArray(attribute);

        glutSwapBuffers();
    }

private:
    GLuint vertexArrayID;
    GLuint programID;
    GLuint vertex_buffer;
};

Graphics graphics;

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(512, 512);
    glutInitContextVersion(3, 2);
    glutInitContextProfile(GLUT_CORE_PROFILE);
    glutCreateWindow("floating");
    glutDisplayFunc([]() { graphics.display(); });

    glewInit();
    graphics.init();

    glutMainLoop();

    graphics.dispose();
    return 0;
}
