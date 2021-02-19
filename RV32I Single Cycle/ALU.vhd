--------------------------------------------------------------------------------
-- Arithemetic Logic Unit with 2 32-bit data inputs, 1 32-bit data output, 
-- 4-bit Operation select lines, a zero flag and sign flag. 
-- Supports the following operations :
-- Addition
-- Subtraction
-- Logical bitwise AND
-- Logical bitwise OR
-- Logical bitwise XOR
-- Logical bitwise NOR
-- Shift left logical
-- Shift right logical/arithemetic
-- Set on less than - Signed and Unsigned
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ALU is
    port (  i_data_1, i_data_2 : in std_logic_vector(31 downto 0);      -- 32-bit input data
            i_oper_sel : in std_logic_vector(3 downto 0);               -- 4-bit operation select line
            o_data : out std_logic_vector(31 downto 0);                 -- 32-bit output data
            o_zero : out std_logic;                                     -- Output zero flag 
            o_sign : out std_logic);                                    -- Output sign flag
end entity ALU;

architecture beh_ALU of ALU is
    signal s_ALUin1_us, s_ALUin2_us, s_ALUout_us : unsigned(31 downto 0);
    signal s_ALUin1_signed, s_ALUin2_signed : signed(31 downto 0);
    signal shamt : integer;
    signal s_ALU_slt, s_ALU_sltu : unsigned(31 downto 0) := (others => '0'); 
begin
    s_ALUin1_us <= unsigned(i_data_1);              --  Type conversion of input data 1 to unsigned for behavourial simulation
    s_ALUin2_us <= unsigned(i_data_2);              --  Type conversion of input data 2 to unsigned for behavourial simulation
    s_ALUin1_signed <= signed(i_data_1);            --  Type conversion of input data 1 to signed for behavourial simulation
    s_ALUin2_signed <= signed(i_data_2);            --  Type conversion of input data 2 to signed for behavourial simulation
    o_data <= std_logic_vector(s_ALUout_us);        --  Connect multiplexer output to output of ALU                            
    o_sign <= std_logic(s_ALUout_us(31));           --  Sign flag is connected to the 32nd bit of the data output
    shamt <= to_integer(s_ALUin2_us(4 downto 0));   --  Shift amount (lower 5 bits) for behavioral simulation of shift operations 

    --------------------------------------------------------------------------------
    --  Multiplexer logic to select the desired 32-bit output based on the 
    --  4-bit operation select lines :
    --  0000 -> Pass i_data_2 to output
    --  0001 -> Logical bitwise AND
    --  0010 -> Logical bitwise OR
    --  0011 -> Logical bitwise XOR
    --  0100 -> Logical bitwise NOR
    --  0101 -> Addition
    --  0110 -> Subtraction
    --  0111 -> Set on less than
    --  1000 -> Set on less than (Unsigned inputs)
    --  1001 -> Shift left logical
    --  1010 -> Shift right logical
    --  1011 -> Shift right arithemetic
    --------------------------------------------------------------------------------

    ALU_op_proc: process(i_oper_sel, s_ALUin1_us, s_ALUin2_us, s_ALUin1_signed, s_ALUin2_signed, shamt)
    begin
        case i_oper_sel is
            when "0000" => 
                s_ALUout_us <= s_ALUin2_us;
            when "0001" => 
                s_ALUout_us <= s_ALUin1_us and s_ALUin2_us;         --  Logical bitwise AND operation of the two data inputs
            when "0010" => 
                s_ALUout_us <= s_ALUin1_us or s_ALUin2_us;          --  Logical bitwise OR operation of the two data inputs
            when "0011" =>
                s_ALUout_us <= s_ALUin1_us xor s_ALUin2_us;         --  Logical bitwise XOR operation of the two data inputs
            when "0101" => 
                s_ALUout_us <= s_ALUin1_us + s_ALUin2_us;           --  Addition operation of the two data inputs
            when "0110" => 
                s_ALUout_us <= s_ALUin1_us - s_ALUin2_us;           --  Subtraction operation of the two data inputs
            
            --------------------------------------------------------------------------------
            -- Set on less than :
            -- o_data = 0x00000001 if i_data_1 < i_data_2 
            -- else o_data = 0x00000000
            --------------------------------------------------------------------------------    
            when "0111" => 
                if s_ALUin1_signed < s_ALUin2_signed then
                    s_ALUout_us <= x"00000001";  
                else
                    s_ALUout_us <= x"00000000";
                end if;
                
            --------------------------------------------------------------------------------
            -- Set on less than logic for unsigned inputs:
            -- o_data = 0x00000001 if i_data_1 < i_data_2 
            -- else o_data = 0x00000000
            --------------------------------------------------------------------------------    
            when "1000" => 
                if s_ALUin1_us < s_ALUin2_us then
                    s_ALUout_us <= x"00000001";  
                else
                    s_ALUout_us <= x"00000000";
                end if;
            
            --------------------------------------------------------------------------------
            -- Shift Left Logical
            -- i_data_1 -> Data to be shifted
            -- i_data_2 -> Shift amount
            --------------------------------------------------------------------------------
            when "1001" => 
                s_ALUout_us <= shift_left(s_ALUin1_us, shamt);
                
            --------------------------------------------------------------------------------
            -- Shift Right Logical
            -- i_data_1 -> Data to be shifted
            -- i_data_2 -> Shift amount
            --------------------------------------------------------------------------------
            when "1010" => 
                s_ALUout_us <= shift_right(s_ALUin1_us, shamt);
                
            --------------------------------------------------------------------------------
            -- Shift Right Arithemetic
            -- i_data_1 -> Data to be shifted
            -- i_data_2 -> Shift amount
            --------------------------------------------------------------------------------
            when "1011" => 
                s_ALUout_us <= unsigned(shift_right(s_ALUin1_signed, shamt));
            when others => 
                s_ALUout_us <= (others => 'X');                     --  INVALID OPERATION
        end case;
    end process ALU_op_proc;

    --------------------------------------------------------------------------------
    --  Combinational logic for the outptu zero flag
    --  o_zero = 1 when o_data = 0x00000000
    --  else o_zero = 0
    --------------------------------------------------------------------------------
    ALU_flag_proc: process(s_ALUout_us)
    begin
        if s_ALUout_us = to_unsigned(0, 32) then
            o_zero <= '1';
        else
            o_zero <= '0';
        end if;
    end process ALU_flag_proc; 
end architecture beh_ALU;