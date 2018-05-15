GHDL=ghdl
GHDLFLAGS=--std=08
MODULES=\
	hello_world \
	experiment
	#file.o \
	#file_tb \

.PHONY: run clean

run: $(MODULES)
	./hello_world
	./experiment
	# ./*_tb --vcd=*_tb.vcd

clean:
	rm -f *.o $(MODULES) work*.cf e*.lst

#experiment: two_port.o experiment.o
#    $(GHDL) -e $(GHDLFLAGS) $@

#experiment.o: experiment.vhd
#    $(GHDL) -a $(GHDLFLAGS) $<

#two_port.o: two_port.vhd
#    $(GHDL) -a $(GHDLFLAGS) $<

%: %.o
	$(GHDL) -e $(GHDLFLAGS) $@

%.o: %.vhd
	$(GHDL) -a $(GHDLFLAGS) $<

