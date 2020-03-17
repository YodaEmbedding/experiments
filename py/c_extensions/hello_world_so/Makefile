default: run

.PHONY: run
run: hello_world.so
	python hello_world.py

hello_world.so:
	gcc -shared -Wl,-soname,hello_world -o hello_world.so -fPIC hello_world.c
