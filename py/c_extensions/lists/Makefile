PYTHON=python3
PIP=pip3

.PHONY: default clean run

default: run

clean:
	rm -rf env .install

env:
	virtualenv -p $(PYTHON) --system-site-packages env

.install: env setup.py lists.c
	( \
		source env/bin/activate; \
		$(PIP) install .; \
	)
	touch .install

run: .install
	( \
		source env/bin/activate; \
		$(PYTHON) lists_example.py; \
	)
