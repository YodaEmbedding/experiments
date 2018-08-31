module snake_game;

reg clk = 0,
    reset = 0;

// format_grid loop through and assign ' ' or 'x' using case?

task simple(byte b);
    $display ("%B", b);
endtask

// input? const? ref?
// task display_grid(byte grid [8]);
task display_grid(byte grid []);
// task display_grid;
    // input byte grid[8];
// begin
    // grid[0] = 4;
    foreach (grid[i]) $display ("%B", grid[i]);
    // foreach (grid[i]) grid[i] = 1 << i;
    // foreach (grid[i]) $display("Address = %0d, Data = %grid", i, grid[i]);
// end
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
byte grid[];

initial begin
    $monitor ("time: %2g   reset: %b    clk: %b", $time, reset, clk);
    $display ("%0d", $signed(player_pos.x));

    // while loop?
    grid = new [8];

    #5 reset = 1;
    #5 $finish;
end

always @(posedge clk)
    display_grid(grid);
    // simple(3);
    // $display ("%B", grid[0]);

always
    #1 clk = !clk;

endmodule
