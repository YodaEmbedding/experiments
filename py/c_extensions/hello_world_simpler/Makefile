default: run

.PHONY: clean
clean:
	rm -rf build
	rm -rf *.so

.PHONY: run
run:
	python setup.py build_ext --inplace
	python main.py
