----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    14:22:51 03/08/2021 
-- Design Name: 
-- Module Name:    data_RAM - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity data_RAM is
    Port ( i_clk : in  STD_LOGIC;
           i_rst : in  STD_LOGIC;
           i_cs : in  STD_LOGIC;
           i_write_en : in  STD_LOGIC;
           i_rw_addr : in  STD_LOGIC_VECTOR (4 downto 0);
           i_write_data : in  STD_LOGIC_VECTOR (31 downto 0);
           o_read_data : out  STD_LOGIC_VECTOR (31 downto 0));
end data_RAM;

architecture Behavioral of data_RAM is
    type mem_type is array(0 to 2**5 - 1) of std_logic_vector(31 downto 0);
  signal RAM : mem_type;
begin
    write_proc: process(i_clk, i_rst)
    begin
        if i_rst = '1' then
            RAM <= (others => x"00000000");
        elsif rising_edge(i_clk) then
            if i_write_en = '1' and i_cs = '1' then
               RAM(to_integer(unsigned(i_rw_addr))) <= i_write_data;
            end if;
        end if;
    end process write_proc;
    o_read_data <= RAM(to_integer(unsigned(i_rw_addr))) when i_cs = '1' else (others => 'Z');
end Behavioral;

