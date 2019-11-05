#version 330 core

in vec3 vertexPosition_modelspace;
in vec3 vertexColor;
out vec3 fragmentColor;
uniform mat4 MVP;

void main() {
	gl_Position = MVP * vec4(vertexPosition_modelspace, 1);
	fragmentColor = vertexColor;
}
