---------------------------------------------------------------------------------
-- A 1KB instruction ROM having :
-- 32-bit word synchronous Read
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

entity instruction_BROM is
    port (  i_clk : in std_logic;
            i_rst : in std_logic;
            i_addr : in std_logic_vector(7 downto 0);
            o_data_out : out std_logic_vector(31 downto 0)
    );
end entity instruction_BROM;

architecture beh of instruction_BROM is
    type ROM_type is array(0 to 255) of std_logic_vector(31 downto 0);
    signal num_instructs : integer := 18;
    
    impure function init_ROM (rom_file : in string)  return ROM_type is 
        FILE romfile : text open read_mode is rom_file;
        variable instruct : line;
        variable rom : rom_type;
        variable temp_bv : bit_vector(31 downto 0);
    begin
            for i in 0 to num_instructs - 1 loop
                readline(romfile, instruct);
                read(instruct, temp_bv);
                rom(i) := to_stdlogicvector(temp_bv);
            end loop;
            return rom;
    end function;
    
    signal ROM : ROM_type := init_ROM("code_translated.txt");
    signal s_addr : unsigned(7 downto 0);
    
begin
    read_proc: process(i_clk)
    begin
        --------------------------------------------------------------------------------
        -- All reads are synchronous with input clock
        --------------------------------------------------------------------------------
        if rising_edge(i_clk) then
            if i_rst = '1' then 
                s_addr <= (others => '0');
            else
                s_addr <= unsigned(i_addr);
            end if;
        end if;
    end process read_proc;
    o_data_out <= ROM(to_integer(s_addr));
end architecture beh;