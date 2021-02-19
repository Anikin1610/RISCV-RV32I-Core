library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

entity tb_CPU is
end entity tb_CPU;

architecture tb_beh of tb_CPU is
    signal num_instructs : integer := 34;
    type RAM_type is array(0 to num_instructs - 1) of std_logic_vector(31 downto 0);

    impure function init_RAM_bin return RAM_type is
        file instructions : text open read_mode is "code_translated.txt";
        variable instruct_line : line;
        variable ram_content : ram_type;
    begin
        for i in 0 to num_instructs - 1 loop
            readline(instructions, instruct_line);
            bread(instruct_line, ram_content(i));
        end loop;
        return ram_content;
    end function;
    signal i : integer := 0;  
    signal flash_data : RAM_type := init_RAM_bin;
    
    signal start_up, rst_init : std_logic := '1';
    signal tb_clk, tb_rst, tb_flash_enable, tb_instruct_overflow : std_logic;
    signal tb_flashData_ip : std_logic_vector(31 downto 0);
    signal tb_flashAddr_ip : unsigned(9 downto 0) := (others => '0');
    
begin
    DUT : entity work.CPU_single_cycle
            port map (  clk => tb_clk,
                        rst => tb_rst,
                        i_flashData => tb_flashData_ip,
                        i_flashAddr => std_logic_vector(tb_flashAddr_ip),
                        i_flash_en => tb_flash_enable,
                        o_instruct_overflow => tb_instruct_overflow);

    clk_proc : process
    begin
        tb_clk <= '0';
        wait for 0.5 ms;
        tb_clk <= '1';
        wait for 0.5 ms;
    end process clk_proc;

    start_up_proc : process(tb_clk)
    begin
        if rising_edge(tb_clk) then
            if start_up = '1' then
                if rst_init = '1' then 
                    tb_rst <= '1';
                    rst_init <= '0';
                else
                    tb_rst <= '0';
                    if i < num_instructs then
                        tb_flash_enable <= '1';
                        tb_flashData_ip <= flash_data(i);
                        i <= i + 1;
                        if i = 0 then 
                            tb_flashAddr_ip <= (others => '0');
                        else
                            tb_flashAddr_ip <= tb_flashAddr_ip + 4;
                        end if;
                    else
                        tb_flash_enable <= '0';
                        start_up <= '0';  
                    end if;
                end if;
            end if;
        end if;        
    end process start_up_proc;
end architecture tb_beh;