
module NotBit(x, y);
	input x;
	output y;
	// wire s;

	y <= not x;
endmodule

module NotByte(xs, ys);
	input[7:0] xs;
	output[7:0] ys;

	for ?
	ys


	for loop?

	// and (out, a, b);
endmodule

module DLatch(clk, D, Q);
	input D, clk;
	output Q;
	reg Q;    // TODO why??? Makes Q a register

	always @(D or clk)
		if (clk) Q = D;
endmodule

always @(posedge clk)
	...

always @(select A or B)

