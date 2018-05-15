all: run

run: clean test
	./test

clean:
	rm -f test

test:
	g++ -Wall -Wextra -pedantic -lopencv_core test.cpp -o test
	#g++ -Wall -Wextra -pedantic -lopencv_core -lopencv_imgproc -lopencv_highgui test.cpp -o test
