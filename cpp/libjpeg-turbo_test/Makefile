default: run

.PHONY: clean
clean:
	rm main.o

.PHONY: run
run: main.o
	./main.o "out.jpg" "out2.jpg"

main.o: main.cpp
	g++ main.cpp -std=c++17 -lturbojpeg -o main.o
