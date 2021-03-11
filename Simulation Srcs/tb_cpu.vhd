--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   07:28:45 03/09/2021
-- Design Name:   
-- Module Name:   C:/Users/Administrator/Desktop/ISE_projects/RISCV/tb_cpu.vhd
-- Project Name:  RISCV
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: CPU_single_cycle
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY tb_cpu IS
END tb_cpu;
 
ARCHITECTURE behavior OF tb_cpu IS  

   --Inputs
   signal clk : std_logic := '0';
   signal rst : std_logic := '0';

 	--Outputs
   signal o_led : std_logic_vector(7 downto 0);

   -- Clock period definitions
   constant clk_period : time := 10 ns;
    
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: entity work.top_module PORT MAP (
          i_clk => clk,
          i_rst => rst,
          o_led => o_led
        );

   -- Clock process definitions
   clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      rst <= '1';
      wait for 20 ns;	
      rst <= '0';
      wait;
   end process;

END;
