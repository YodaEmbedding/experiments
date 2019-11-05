#include <cstdlib>

#include <GL/glew.h>
#if __has_include(<GL/freeglut.h>)
#include <GL/freeglut.h>
// #include <GL/freeglut_ext.h>
#else
#include <GL/glut.h>
#endif
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "shader.h"

const int numCubeVertices = 12 * 3;

// clang-format off
const GLfloat cubeVertexData[numCubeVertices * 3] = {
    // Triangle 1
    -1.0f, -1.0f, -1.0f,
    -1.0f, -1.0f, 1.0f,
    -1.0f, 1.0f,  1.0f,
    // Triangle 2
    1.0f,  1.0f,  -1.0f,
    -1.0f, -1.0f, -1.0f,
    -1.0f, 1.0f,  -1.0f,
    // Triangle 3
    1.0f,  -1.0f, 1.0f,
    -1.0f, -1.0f, -1.0f,
    1.0f,  -1.0f, -1.0f,
    // Triangle 4
    1.0f,  1.0f,  -1.0f,
    1.0f,  -1.0f, -1.0f,
    -1.0f, -1.0f, -1.0f,
    // Triangle 5
    -1.0f, -1.0f, -1.0f,
    -1.0f, 1.0f,  1.0f,
    -1.0f, 1.0f,  -1.0f,
    // Triangle 6
    1.0f,  -1.0f, 1.0f,
    -1.0f, -1.0f, 1.0f,
    -1.0f, -1.0f, -1.0f,
    // Triangle 7
    -1.0f, 1.0f,  1.0f,
    -1.0f, -1.0f, 1.0f,
    1.0f,  -1.0f, 1.0f,
    // Triangle 8
    1.0f,  1.0f,  1.0f,
    1.0f,  -1.0f, -1.0f,
    1.0f,  1.0f,  -1.0f,
    // Triangle 9
    1.0f,  -1.0f, -1.0f,
    1.0f,  1.0f,  1.0f,
    1.0f,  -1.0f, 1.0f,
    // Triangle 10
    1.0f,  1.0f,  1.0f,
    1.0f,  1.0f,  -1.0f,
    -1.0f, 1.0f,  -1.0f,
    // Triangle 11
    1.0f,  1.0f,  1.0f,
    -1.0f, 1.0f,  -1.0f,
    -1.0f, 1.0f,  1.0f,
    // Triangle 12
    1.0f,  1.0f,  1.0f,
    -1.0f, 1.0f,  1.0f,
    1.0f,  -1.0f, 1.0f
};
// clang-format on

// clang-format off
const GLfloat cubeColorData[numCubeVertices * 3] = {
    0.583f,  0.771f,  0.014f,
    0.609f,  0.115f,  0.436f,
    0.327f,  0.483f,  0.844f,
    0.822f,  0.569f,  0.201f,
    0.435f,  0.602f,  0.223f,
    0.310f,  0.747f,  0.185f,
    0.597f,  0.770f,  0.761f,
    0.559f,  0.436f,  0.730f,
    0.359f,  0.583f,  0.152f,
    0.483f,  0.596f,  0.789f,
    0.559f,  0.861f,  0.639f,
    0.195f,  0.548f,  0.859f,
    0.014f,  0.184f,  0.576f,
    0.771f,  0.328f,  0.970f,
    0.406f,  0.615f,  0.116f,
    0.676f,  0.977f,  0.133f,
    0.971f,  0.572f,  0.833f,
    0.140f,  0.616f,  0.489f,
    0.997f,  0.513f,  0.064f,
    0.945f,  0.719f,  0.592f,
    0.543f,  0.021f,  0.978f,
    0.279f,  0.317f,  0.505f,
    0.167f,  0.620f,  0.077f,
    0.347f,  0.857f,  0.137f,
    0.055f,  0.953f,  0.042f,
    0.714f,  0.505f,  0.345f,
    0.783f,  0.290f,  0.734f,
    0.722f,  0.645f,  0.174f,
    0.302f,  0.455f,  0.848f,
    0.225f,  0.587f,  0.040f,
    0.517f,  0.713f,  0.338f,
    0.053f,  0.959f,  0.120f,
    0.393f,  0.621f,  0.362f,
    0.673f,  0.211f,  0.457f,
    0.820f,  0.883f,  0.371f,
    0.982f,  0.099f,  0.879f
};
// clang-format on

class Graphics {
public:
    Graphics() {}

    void init() {
        glClearColor(0.0f, 0.0f, 0.4f, 0.0f);

        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_LESS);

        glGenVertexArrays(1, &vertexArrayID);
        glBindVertexArray(vertexArrayID);

        programID = LoadShaders("vshader.glsl", "fshader.glsl");
        mvpID = glGetUniformLocation(programID, "MVP");
        vertexPosition_modelspace
            = glGetAttribLocation(programID, "vertexPosition_modelspace");
        vertexColor = glGetAttribLocation(programID, "vertexColor");

        glm::mat4 projection
            = glm::perspective(glm::radians(45.0f), 1.0f, 0.1f, 100.0f);
        glm::mat4 view = glm::lookAt(
            glm::vec3(-10, 10, 10), // camera location
            glm::vec3(0, 0, 0),     // center point (to look towards)
            glm::vec3(0, 1, 0)      // up vector
        );
        vp = projection * view;

        glGenBuffers(1, &vertexBuffer);
        glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
        glBufferData(
            GL_ARRAY_BUFFER, sizeof(cubeVertexData), cubeVertexData,
            GL_STATIC_DRAW);

        glGenBuffers(1, &colorBuffer);
        glBindBuffer(GL_ARRAY_BUFFER, colorBuffer);
        glBufferData(
            GL_ARRAY_BUFFER, sizeof(cubeColorData), cubeColorData,
            GL_STATIC_DRAW);
    }

    void dispose() {
        glDeleteBuffers(1, &vertexBuffer);
        glDeleteVertexArrays(1, &vertexArrayID);
        glDeleteProgram(programID);
    }

    void display() {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glUseProgram(programID);
        setupCube();
        drawCube(glm::mat4(1.0f));
        drawCube(glm::translate(glm::mat4(1.0f), glm::vec3(4.0f, 0.0f, 0.0f)));
        teardownCube();
        glutSwapBuffers();
    }

private:
    GLuint vertexArrayID;
    GLuint mvpID;
    GLuint programID;
    GLuint vertexBuffer;
    GLuint colorBuffer;
    GLuint vertexPosition_modelspace;
    GLuint vertexColor;
    glm::mat4 vp;

    void drawCube(glm::mat4 model) {
        glm::mat4 mvp = vp * model;
        glUniformMatrix4fv(mvpID, 1, GL_FALSE, &mvp[0][0]);
        glDrawArrays(GL_TRIANGLES, 0, 3 * 12);
    }

    void setupCube() {
        glEnableVertexAttribArray(vertexPosition_modelspace);
        glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
        glVertexAttribPointer(
            vertexPosition_modelspace, // attribute
            3,                         // size
            GL_FLOAT,                  // type
            GL_FALSE,                  // normalized?
            0,                         // stride
            (void*)0                   // array buffer offset
        );

        glEnableVertexAttribArray(vertexColor);
        glBindBuffer(GL_ARRAY_BUFFER, colorBuffer);
        glVertexAttribPointer(
            vertexColor, // attribute
            3,           // size
            GL_FLOAT,    // type
            GL_FALSE,    // normalized?
            0,           // stride
            (void*)0     // array buffer offset
        );
    }

    void teardownCube() {
        glDisableVertexAttribArray(vertexPosition_modelspace);
        glDisableVertexAttribArray(vertexColor);
    }
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
