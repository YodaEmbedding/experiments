module fibonacci(
    input wire clk,
    input wire [31:0] a_in,
    input wire [31:0] b_in,
    output reg [31:0] a_out,
    output reg [31:0] b_out   // TODO should these be registers? or wires?
);
    always @ (posedge clk) begin
        a_out <= b_in;
        b_out <= a_in + b_in;
    end
endmodule
