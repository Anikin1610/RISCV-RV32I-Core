--------------------------------------------------------------------------------
-- Top module used to interface the data path with the data memory and other
-- memory mapped IO
--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top_module is
    Port ( i_clk : in  STD_LOGIC;
           i_rst : in  STD_LOGIC;
           o_led : out  STD_LOGIC_VECTOR (7 downto 0));
end top_module;

architecture Behavioral of top_module is
    signal s_data_bus_in : std_logic_vector(31 downto 0);
    signal s_addr_bus : std_logic_vector(31 downto 0);
    signal s_data_bus_out : std_logic_vector(31 downto 0);
    signal s_wr_en : std_logic;
    signal s_data_mem_cs : std_logic;
begin
    Inst_CPU_single_cycle: entity work.CPU_single_cycle PORT MAP(
		i_clk => i_clk,
		i_rst => i_rst,
		i_data_bus_in => s_data_bus_in,
		o_addr_bus => s_addr_bus,
		o_wr_en => s_wr_en,
		o_data_bus_out => s_data_bus_out
	);
      
    s_data_mem_cs <= '1' when s_addr_bus(31 downto 7) = std_logic_vector(to_unsigned(0, 25)) else '0';
    --------------------------------------------------------------------------------
    -- Data memory instantiation
    --------------------------------------------------------------------------------
    mem_data_inst : entity work.data_RAM
        port map (  i_clk => i_clk,
                    i_rst => i_rst,
                    i_cs => s_data_mem_cs,
                    i_write_en => s_wr_en,
                    i_rw_addr => s_addr_bus(6 downto 2),
                    i_write_data => s_data_bus_out,
                    o_read_data => s_data_bus_in);
    
    --------------------------------------------------------------------------------
    -- Used to display result of program using the LEDs on the prototyping board
    --------------------------------------------------------------------------------
    process(i_clk)
    begin
        if rising_edge(i_clk) then
            if s_addr_bus = x"00000080" and s_wr_en = '1' then
                o_led <= s_data_bus_out(7 downto 0);
            end if;
        end if;
    end process;

end Behavioral;

