---------------------------------------------------------------------------------
-- A 1KB memory module having an integrated memory controller with :
-- Synchronous Write / Asynchronous Read
-- 32-bit word read and write (non word-aligned reads and writes also possible)
-- 16-bit half-word reads and writes
-- 8-bit reads and writes
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity data_mem is
  generic (ADDR_WIDTH : integer := 7);
  port (
    clk : in std_logic;           -- Input clock signal
    rst : in std_logic;           -- Input active-high reset signal
    i_write_en : in std_logic;    -- Input active-high write enable signa
    i_write_hword : in std_logic; -- Input active-high half-word write enable
    i_write_byte : in std_logic;  -- Input active-high byte write enable
    i_read_hword : in std_logic;  -- Input active-high half-word read enable
    i_read_byte : in std_logic;   -- Input active-high byte read enable 
    i_write_data : in std_logic_vector(31 downto 0);  -- 32-bit data input for writes 
    i_rw_addr : in std_logic_vector(ADDR_WIDTH - 1 downto 0);      -- 10-bit address for both reads and writes
    o_read_data : out std_logic_vector(31 downto 0);  -- 32-bit data output for reads
    o_read_invalid : out std_logic;                   -- Output invalid read signal
    o_write_invalid : out std_logic);                 -- Output invalid write signal
end entity data_mem;

architecture beh of data_mem is
  type mem_type is array(0 to 2**ADDR_WIDTH - 1) of std_logic_vector(7 downto 0);
  signal mem1K_module : mem_type;
begin
  --------------------------------------------------------------------------------
  -- Combinational (asynchronous) logic for memory reads
  --------------------------------------------------------------------------------
  mem_read_proc: process(i_rw_addr, i_read_hword, i_read_byte, mem1K_module)
  begin
    --------------------------------------------------------------------------------
    --  Check whether the accessed word/half-word address is out of bounds 
    --------------------------------------------------------------------------------
    if (to_integer(unsigned(i_rw_addr)) = 2**ADDR_WIDTH - 1 and i_read_byte = '0') or ((to_integer(unsigned(i_rw_addr)) = 2**ADDR_WIDTH - 2 or to_integer(unsigned(i_rw_addr)) = 2**ADDR_WIDTH - 3) and i_read_hword  = '0' and i_read_byte = '0') then
      o_read_invalid <= '1';
      o_read_data <= (others => 'X');
    else
      --------------------------------------------------------------------------------
        --  When read byte signal is asserted
      --------------------------------------------------------------------------------
      if i_read_byte = '1' and i_read_hword = '0' then
        o_read_data(7 downto 0) <= mem1K_module(to_integer(unsigned(i_rw_addr)));
        o_read_data(31 downto 8) <= (others => 'X');
        o_read_invalid <= '0';
      --------------------------------------------------------------------------------
        --  When read half-word signal is asserted
      --------------------------------------------------------------------------------
      elsif i_read_byte = '0' and i_read_hword = '1' then
        o_read_data(7 downto 0) <= mem1K_module(to_integer(unsigned(i_rw_addr)));
        o_read_data(15 downto 8) <= mem1K_module(to_integer(unsigned(i_rw_addr)) + 1);
        o_read_data(31 downto 16) <= (others => 'X');
        o_read_invalid <= '0';
      --------------------------------------------------------------------------------
      --  When read byte and write half-word signal are both deasserted (read word)
      --------------------------------------------------------------------------------
      elsif i_read_byte = '0' and i_read_hword = '0' then
        o_read_data(7 downto 0) <= mem1K_module(to_integer(unsigned(i_rw_addr)));
        o_read_data(15 downto 8) <= mem1K_module(to_integer(unsigned(i_rw_addr)) + 1);
        o_read_data(23 downto 16) <= mem1K_module(to_integer(unsigned(i_rw_addr)) + 2);
        o_read_data(31 downto 24) <= mem1K_module(to_integer(unsigned(i_rw_addr)) + 3);
        o_read_invalid <= '0';
      else
        o_read_data <= (others => 'X');
        o_read_invalid <= '1';
      end if;
    end if; 
  end process mem_read_proc;

  --------------------------------------------------------------------------------
  -- Synchronous logic for memory writes
  --------------------------------------------------------------------------------
  mem_write_proc: process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        mem1K_module <= (others => x"00");
      elsif i_write_en = '1' then
        --------------------------------------------------------------------------------
        --  Check whether the accessed word/half-word address is out of bounds 
        --------------------------------------------------------------------------------
        if (to_integer(unsigned(i_rw_addr)) = 2**ADDR_WIDTH - 1 and i_write_byte = '0') or (to_integer(unsigned(i_rw_addr)) = 2**ADDR_WIDTH - 2 and to_integer(unsigned(i_rw_addr)) = 2**ADDR_WIDTH - 3 and i_write_hword = '0' and i_write_byte = '0') then
          o_write_invalid <= '1';
        --------------------------------------------------------------------------------
        --  When write byte signal is asserted
        --------------------------------------------------------------------------------
        elsif i_write_byte = '1' and i_write_hword = '0' then
          mem1K_module(to_integer(unsigned(i_rw_addr))) <= i_write_data(7 downto 0);
          o_write_invalid <= '0';
        --------------------------------------------------------------------------------
        --  When write half-word signal is asserted
        --------------------------------------------------------------------------------
        elsif i_write_byte = '0' and i_write_hword = '1' then
          mem1K_module(to_integer(unsigned(i_rw_addr))) <= i_write_data(7 downto 0);
          mem1K_module(to_integer(unsigned(i_rw_addr)) + 1) <= i_write_data(15 downto 8);
          o_write_invalid <= '0';
        --------------------------------------------------------------------------------
        --  When write byte and write half-word signal are both deasserted (write word)
        --------------------------------------------------------------------------------
        elsif i_write_byte = '0' and i_write_hword = '0' then
          mem1K_module(to_integer(unsigned(i_rw_addr))) <= i_write_data(7 downto 0);
          mem1K_module(to_integer(unsigned(i_rw_addr)) + 1) <= i_write_data(15 downto 8);
          mem1K_module(to_integer(unsigned(i_rw_addr)) + 2) <= i_write_data(23 downto 16);
          mem1K_module(to_integer(unsigned(i_rw_addr)) + 3) <= i_write_data(31 downto 24);
          o_write_invalid <= '0';
        else
          o_write_invalid <= '1';
        end if;
      end if;
    end if;
  end process mem_write_proc;
end architecture beh;