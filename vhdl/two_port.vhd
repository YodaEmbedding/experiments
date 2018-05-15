library ieee;
use ieee.std_logic_1164.all;

entity two_port is
    port(
        a: in std_logic;
        b: in std_logic;
        y: out std_logic
    );
end;

architecture my_nand of two_port is
begin
    y <= not (a and b);
end;

