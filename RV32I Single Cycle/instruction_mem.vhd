---------------------------------------------------------------------------------
-- A 1KB instruction memory module having :
-- Synchronous Write / Asynchronous Read
-- 32-bit word read and write (non word-aligned reads/writes not required)
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity instruction_BRAM is
    port (  i_clk : in std_logic;
            i_write_en : in std_logic;
            i_addr : in std_logic_vector(7 downto 0);
            i_data_in : in std_logic_vector(31 downto 0);
            o_data_out : out std_logic_vector(31 downto 0)
    );
end entity instruction_BRAM;

architecture beh of instruction_BRAM is
    type RAM_type is array(0 to 255) of std_logic_vector(31 downto 0);
    signal RAM : RAM_type;
    
begin
    proc_name: process(i_clk)
    begin
        --------------------------------------------------------------------------------
        -- All writes are synchronous with input clock
        --------------------------------------------------------------------------------
        if rising_edge(i_clk) then
            if i_write_en = '1' then
                RAM(to_integer(unsigned(i_addr))) <= i_data_in;
            end if;
        end if;
    end process proc_name;
    o_data_out <= RAM(to_integer(i_addr));
end architecture beh;