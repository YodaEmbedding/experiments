PYTHON=python3
PIP=pip3

.PHONY: default clean run

default: run

clean:
	rm -rf env .install

env:
	virtualenv -p $(PYTHON) --system-site-packages env

.install: env setup.py hello.c
	( \
		source env/bin/activate; \
		$(PIP) install .; \
	)
	touch .install

run: .install
	( \
		source env/bin/activate; \
		$(PYTHON) hello_example.py; \
	)
