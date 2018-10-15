module top(
    input wire clk,
    input wire reset,
    output wire [31:0] fib
);

fibonacci fib_inst (
    .clk(clk),
    .reset(reset),
    .fib(fib));

endmodule
