library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;
use work.all;

entity experiment is
end experiment;

architecture behaviour of experiment is
    signal always_false : std_logic := '0';
    signal always_true : std_logic := '1';
    signal out_nand : std_logic;
begin
    yay_nands: entity work.two_port(my_nand) port map(
        a => always_true,
        b => always_false,
        y => out_nand
    );

    process(out_nand)
        variable l : line;
        variable y_tmp : std_logic;
    begin
        y_tmp := out_nand;
        --write (l, String'("Hello world!"));
        --write (l, std_logic'image(out_nand));
        write (l, std_logic'image(y_tmp));
        writeline (output, l);
        --wait;
    end process;
end behaviour;


-- TODO:
-- Testbench
-- Lab 1 (basic things first?)

