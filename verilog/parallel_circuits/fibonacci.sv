module fibonacci(
    input wire clk,
    output reg [31:0] a,
    output reg [31:0] b
);
    always @ (posedge clk) begin
        a <= b;
        b <= a + b;
    end
endmodule
