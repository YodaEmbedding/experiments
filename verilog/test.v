
// TODO difference between =, <=, and assign?
// =      "blocking"
// <=     "non-blocking"
// assign "concurrent" (no always or sensitivity list required)

module AdderStage(a, b, cin, sum, cout);
	input a, b, cin;
	output sum, cout;

	// reg sum, cout;

	always @ (a or b or cin) begin
		sum <= a ^ b ^ cin;
		cout <= (a & b) | (a & cin) | (b & cin);
	end
endmodule

module adder8(a, b, cin, sum, cout);
	input[7:0] a, b;
	input cin;
	output[7:0] sum;
	output cout;
	wire[8:0] carry;

	carry[0] <= cin;

	// always? not needed?

	for (i=0; i < 8; i++)
		AdderStage(a[i], b[i], carry[i], sum[i], carry[i + 1]);

	cout <= carry[8];
endmodule

module NotBit(x, y);
	input x;
	output y;
	// wire s;

	// posedge clk?
	y <= not x;
endmodule

module NotByte(xs, ys);
	input[7:0] xs;
	output[7:0] ys;

	// posedge clk?
	for (i = 0; i < 8; i = i + 1)
		NotBit(x[i], y[i]);
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


.notation?

