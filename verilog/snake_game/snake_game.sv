module snake_game;

reg clk = 0,
    reset = 0;

// format_grid loop through and assign ' ' or 'x' using case?

task simple(byte b);
    $display ("%B", b);
endtask

typedef struct packed {
    int x;
    int y;
} Vector2;

typedef struct packed {
    Vector2 pos;
    Vector2 lame;
} Player;

// Player player = {{32'sd0, 32'sd0}, {32'sd0, 32'sd0}};
Vector2 player_pos = {32'sd0, 32'sd0};

// reg [0:7] grid [0:7];
// byte grid [8] = '{0, 0, 0, 0, 0, 0, 0, 0};
byte grid [0:7];

initial begin
    // $monitor ("time: %2g   reset: %b    clk: %b", $time, reset, clk);
    player_pos.x = 4;
    $display ("%0d", $signed(player_pos.x));

    // while loop?

    // #5 reset = 1;
    // #5 $finish;
end

always @(posedge clk)
begin
    // display_grid(grid);
    // simple(3);
    // $display ("%B", grid[0]);
    grid[2] = 14;

    foreach (grid[i]) $display ("%B", grid[i]);
end

// always
//     #1 clk = !clk;

endmodule
