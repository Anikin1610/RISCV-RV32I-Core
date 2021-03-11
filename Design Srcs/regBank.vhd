--------------------------------------------------------------------------------
-- 32 x 32 Register bank with :
-- Asynchronous Reads and Synchronous Write
-- 2 data read ports & 1 data write port
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity reg32 is
    port (  i_clk : in std_logic;         -- Input active-high clock signal
            i_rst : in std_logic;         -- Input active-high reset signal
            i_write_en : in std_logic;  -- Input active-high write enable signal
            i_write_data : in std_logic_vector(31 downto 0);    -- 32-bit data input for writes
            i_read_addr_1, i_read_addr_2, i_write_addr : in std_logic_vector(4 downto 0);   -- 3 5-bit address ports : 1 for write and 2 for read
            o_read_data_1, o_read_data_2 : out std_logic_vector(31 downto 0));              -- 2 32-bit data read ports
end entity reg32;   

architecture reg_beh of reg32 is
    type reg_type is array (0 to 31) of std_logic_vector(31 downto 0);      -- Register bank type for 32 x 32 register
    signal reg_file : reg_type;                  -- Instance of register bank
begin
    --------------------------------------------------------------------------------
    -- Combinational (asynchronous logic) for data reads
    --------------------------------------------------------------------------------
    read_proc: process(i_read_addr_1, i_read_addr_2, reg_file)
    begin
        o_read_data_1 <= reg_file(to_integer(unsigned(i_read_addr_1)));
        o_read_data_2 <= reg_file(to_integer(unsigned(i_read_addr_2)));
    end process read_proc;

    --------------------------------------------------------------------------------
    -- Synchronous logic for data writes
    --------------------------------------------------------------------------------
    write_proc: process(i_clk, i_rst)
    begin
        --------------------------------------------------------------------------------
        --  If rst is active, initialize all register locations to 0x00000 (asynchronous reset)
        --------------------------------------------------------------------------------
        if i_rst = '1' then
            reg_file <= (others => x"00000000");
        elsif rising_edge(i_clk) then
            --------------------------------------------------------------------------------
            --  Prevent writes to the zero register at address 0
            --------------------------------------------------------------------------------
            if i_write_en = '1' and i_write_addr /= "00000" then
                reg_file(to_integer(unsigned(i_write_addr))) <= i_write_data;
            end if;
        end if;
    end process write_proc; 
end architecture reg_beh;