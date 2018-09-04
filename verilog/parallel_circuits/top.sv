module top(
    input wire clk,
    input wire reset,
    output reg [31:0] fib
);
    always @ (posedge clk) begin
        a <= b;
        b <= a + b;
    end

    // reset? 
    always @ 
endmodule
