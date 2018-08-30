module simulation_time;

reg clk = 0,
    reset = 0;

initial begin
    $monitor ("time: %2g   reset: %b    clk: %b", $time, reset, clk);
    #5 reset = 1;
    #5 $finish;
end

always
    #1 clk = !clk;

endmodule
