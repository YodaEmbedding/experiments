module fibonacci(
    input wire clk,
    input wire reset,
    output wire [31:0] fib
);

reg [31:0] a = 0;
reg [31:0] b = 1;

always @ (posedge clk) begin
    if (reset) begin
        a <= 0;
        b <= 1;
    end else begin
        a <= b;
        b <= a + b;
    end
end

assign fib = b;

endmodule
