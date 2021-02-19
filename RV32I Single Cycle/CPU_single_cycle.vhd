--------------------------------------------------------------------------------
-- Top module for the single cycle RV32I core which consists of a
-- instruction memory, 32x32 register bank, ALU, control unit and data memory.
-- ecall, ebreak and fence instructions are not supported. 
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity CPU_single_cycle is
    port (  clk   : in std_logic;                           -- Input clock signal
            rst : in std_logic;                             -- Input reset signal
            i_flashData : in std_logic_vector(31 downto 0); -- 32-bit instruction flash data input
            i_flashAddr : in std_logic_vector(9 downto 0);  -- 10-bit instruction flash address input
            i_flash_en : in std_logic;                      -- Input flash enable signal
            o_mem_data : out std_logic_vector(31 downto 0));           -- Output instruction write overflow flag
end entity;

architecture cpu_rtl of CPU_single_cycle is

    signal s_instruction : std_logic_vector(31 downto 0);                           --  Instruction from the instruction memory
    alias a_funct7 : std_logic_vector(6 downto 0) is s_instruction(31 downto 25);   --  funct7 field of instruction
    alias a_rs2 : std_logic_vector(4 downto 0) is s_instruction(24 downto 20);      --  rs2 field of instruction
    alias a_rs1 : std_logic_vector(4 downto 0) is s_instruction(19 downto 15);      --  rs1 field of instruction
    alias a_funct3 : std_logic_vector(2 downto 0) is s_instruction(14 downto 12);   --  funct3 field of instruction
    alias a_rd : std_logic_vector(4 downto 0) is s_instruction(11 downto 7);        --  rd field of instruction
    alias a_OpCode : std_logic_vector(6 downto 0) is s_instruction(6 downto 0);     --  opcode field of instruction
    alias a_imm_I : std_logic_vector(11 downto 0) is s_instruction(31 downto 20);   --  I type immediate field
    alias a_imm_U : std_logic_vector(19 downto 0) is s_instruction(31 downto 12);   --  U type immediate field
    alias a_shamt : std_logic_vector(4 downto 0) is s_instruction(24 downto 20);    --  Shamt field 
    signal a_imm_S : std_logic_vector(11 downto 0);                                 --  S type immediate field 
    signal s_branch_offset : std_logic_vector(11 downto 0);                         --  B type immediate field
    signal s_jump_offset : std_logic_vector(19 downto 0);                           --  J type immediate field
    signal s_shamt_zeroExtn : std_logic_vector(31 downto 0);                        --  32-bit zero extended shamt field

    signal s_reg_read_data_1, s_reg_read_data_2 : std_logic_vector(31 downto 0);    --  Internal signals for register data outputs 
    signal s_reg_write_assert : std_logic;                                          --  Internal signal for register data write enable

    signal s_data_read_invalid, s_data_write_invalid, s_instruct_read_invalid, s_sign_extn_sel, s_ALUdata1_sel, s_ALUdata2_sel, s_shamt_sel, s_regWrite_en, s_data_write_en, s_data_write_hword_en, s_data_write_byte_en, s_data_read_sign_extn_sel, s_data_read_hword_sel, s_data_read_byte_sel, s_PCjump_sel, s_PCbranch_sel, s_PC_stall : std_logic; --  Internal signals for the outputs of control unit
    signal s_regWrite_src_sel, s_imm_type_sel : std_logic_vector(1 downto 0);       --  Internal signals for the 2 bit outputs of the control unit
    
    signal s_ALU_oper_sel : std_logic_vector(3 downto 0);                           --  Internal signal for the ALU operation select output from control unit
    signal s_ALU_data_op : std_logic_vector(31 downto 0);                           --  Internal signal for the 32-bit ALU data output
    signal s_ALU_zero_op, s_ALU_sign_op : std_logic;                                --  Internal signal for the zero and sign flag from ALU

    signal s_mem_data_read_op : std_logic_vector(31 downto 0);                      --  Internal signal for 32-bit output from data memory

    signal s_imm_I_signExtn, s_imm_S_signExtn, s_imm_B_signExtn, s_imm_U_signExtn, s_imm_J_signExtn : std_logic_vector(31 downto 0);    --  Internal signals for the 32-bit sign extended immediates
    signal s_imm_mux : std_logic_vector(31 downto 0);                               --  32-bit 4x1 MUX for selecting immediate type
    signal s_imm_shamt_mux : std_logic_vector(31 downto 0);                         --  32-bit 2x1 MUX for selecting immediate or shamt field  
    

    signal s_PC : unsigned(31 downto 0);                                            --  32-bit Program counter
    signal s_next_instruct, s_next_instruct_if_branch, s_next_instruct_if_jump : unsigned(31 downto 0);                                 --  Internal signals for the next address, branch address and jump address 
    signal s_inst_reg_addr_mux : unsigned(31 downto 0);                             --  32-bit 2x1 MUX used for flashing instruction memory without changing the value of PC  

    signal s_ALUdata1_mux : std_logic_vector(31 downto 0);                          --  32-bit 2x1 MUX used for selecting the data input 1 to ALU
    signal s_ALUdata2_mux : std_logic_vector(31 downto 0);                          --  32-bit 2x1 MUX used for selecting the data input 2 to ALU  

    signal s_data_read_sign_extn : std_logic_vector(31 downto 0);                   --  Internal signal used for the sign extended memory data output (for hword and byte reads)
    signal s_regWrite_data_mux : std_logic_vector(31 downto 0);                     --  32-bit 4x1 MUX used for selecting the register write data source

begin
    
    o_mem_data <= s_data_read_sign_extn;
    
    --------------------------------------------------------------------------------
    -- Instruction memory instantiation
    --------------------------------------------------------------------------------
    mem_instruct_inst : entity work.instruction_BRAM
        port map (  i_clk => clk,
                    i_write_en => i_flash_en,
                    i_data_in => i_flashData,
                    i_addr => std_logic_vector(s_inst_reg_addr_mux(7 downto 0)),
                    o_data_out => s_instruction);

    --------------------------------------------------------------------------------
    -- Register bank instantiation
    --------------------------------------------------------------------------------
    reg32_inst : entity work.reg32
        port map (  clk => clk,
                    rst => rst,
                    i_write_en => s_reg_write_assert,
                    i_write_data => s_regWrite_data_mux,
                    i_read_addr_1 => a_rs1,
                    i_read_addr_2 => a_rs2,
                    i_write_addr => a_rd,
                    o_read_data_1 => s_reg_read_data_1,
                    o_read_data_2 => s_reg_read_data_2);

    --------------------------------------------------------------------------------
    -- Control unit instantiation
    --------------------------------------------------------------------------------
    control_unit_inst : entity work.control_unit
        port map (  i_opcode => a_OpCode,
                    i_funct3 => a_funct3,
                    i_funct7 => a_funct7,
                    i_ALUzero => s_ALU_zero_op,
                    i_ALUsign => s_ALU_sign_op,
                    i_data_read_invalid => s_data_read_invalid,
                    i_data_write_invalid => s_data_write_invalid,
                    i_instruct_read_invalid => s_instruct_read_invalid,
                    o_ALUdata1_sel => s_ALUdata1_sel,
                    o_ALUdata2_sel => s_ALUdata2_sel,
                    o_imm_type_sel => s_imm_type_sel,
                    o_shamt_sel => s_shamt_sel,
                    o_ALU_oper_sel => s_ALU_oper_sel,
                    o_data_write_en => s_data_write_en,
                    o_data_write_hword_sel => s_data_write_hword_en,
                    o_data_write_byte_sel => s_data_write_byte_en,
                    o_data_read_sign_extn_sel => s_data_read_sign_extn_sel,
                    o_data_read_hword_sel => s_data_read_hword_sel,
                    o_data_read_byte_sel => s_data_read_byte_sel,
                    o_regWrite_src_sel => s_regWrite_src_sel,
                    o_regWrite_en => s_regWrite_en,
                    o_PCjump_sel => s_PCjump_sel,
                    o_PCbranch_sel => s_PCbranch_sel,
                    o_PC_stall => s_PC_stall);

    --------------------------------------------------------------------------------
    -- ALU instantiation
    --------------------------------------------------------------------------------
    ALU_inst : entity work.ALU
        port map (  i_data_1 => s_ALUdata1_mux,
                    i_data_2 => s_ALUdata2_mux,
                    i_oper_sel => s_ALU_oper_sel,
                    o_data => s_ALU_data_op,
                    o_zero => s_ALU_zero_op,
                    o_sign => s_ALU_sign_op);

    --------------------------------------------------------------------------------
    -- Data memory instantiation
    --------------------------------------------------------------------------------
    mem_data_inst : entity work.data_mem
        port map (  clk => clk,
                    rst => rst,
                    i_write_en => s_data_write_en,
                    i_write_hword => s_data_write_hword_en,
                    i_write_byte => s_data_write_byte_en,
                    i_read_hword => s_data_read_hword_sel,
                    i_read_byte => s_data_read_byte_sel,
                    i_write_data => s_reg_read_data_2,
                    i_rw_addr => s_ALU_data_op(6 downto 0),
                    o_read_data => s_mem_data_read_op,
                    o_read_invalid => s_data_read_invalid,
                    o_write_invalid => s_data_write_invalid);

    s_branch_offset <= s_instruction(31)  & s_instruction(7) & s_instruction(30 downto 25) & s_instruction(11 downto 8);    --  B type immediate
    s_jump_offset <= s_instruction(31) & s_instruction(19 downto 12) & s_instruction(20) & s_instruction(30 downto 21);     --  J type immediate
    a_imm_S <= s_instruction(31 downto 25) & s_instruction(11 downto 7);                                                    --  S type immediate                                                  

    --------------------------------------------------------------------------------
    -- Zero extend the shamt field to 32 bits
    --------------------------------------------------------------------------------
    s_shamt_zeroExtn(31 downto 5) <= (others => '0'); 
    s_shamt_zeroExtn(4 downto 0) <= a_shamt;

    --------------------------------------------------------------------------------
    -- Sign extend the immediates to 32-bits
    --------------------------------------------------------------------------------
    s_imm_I_signExtn(31 downto 12) <= (others => a_imm_I(11));
    s_imm_I_signExtn(11 downto 0) <= a_imm_I;

    s_imm_S_signExtn(31 downto 12) <= (others => a_imm_S(11));
    s_imm_S_signExtn(11 downto 0) <= a_imm_S;

    s_imm_B_signExtn(31 downto 12) <= (others => s_branch_offset(11));
    s_imm_B_signExtn(11 downto 0) <= s_branch_offset;

    s_imm_J_signExtn(31 downto 21) <= (others => s_jump_offset(19));
    s_imm_J_signExtn(20 downto 0) <= s_jump_offset & '0';

    --------------------------------------------------------------------------------
    -- Zero extend the last 12 bits to U type immediate
    --------------------------------------------------------------------------------
    s_imm_U_signExtn(31 downto 12) <= a_imm_U;
    s_imm_U_signExtn(11 downto 0) <= (others => '0');

    
    --------------------------------------------------------------------------------
    -- MUX process used for selecting the address input to instruction memory
    -- When flashing - Address comes from i_flashAddr ports
    -- Else - Address comes from the program counter
    --------------------------------------------------------------------------------
    instAddr_flash_mux_proc: process(s_PC, i_flashAddr, i_flash_en)
    begin
        if i_flash_en = '0' then
            s_inst_reg_addr_mux <= s_PC;
        else
            s_inst_reg_addr_mux(9 downto 0) <= unsigned(i_flashAddr);
            s_inst_reg_addr_mux(31 downto 10) <= (others => '0');
        end if;
    end process instAddr_flash_mux_proc;    
    
    --------------------------------------------------------------------------------
    -- MUX process used for selecting the type of immediate to be used as
    -- input to ALU
    --------------------------------------------------------------------------------
    imm_mux_proc: process(s_imm_I_signExtn, s_imm_S_signExtn, s_imm_U_signExtn, s_imm_J_signExtn, s_imm_type_sel)
    begin
        case s_imm_type_sel is
            when "00" => 
                s_imm_mux <= s_imm_I_signExtn;
            when "01" => 
                s_imm_mux <= s_imm_S_signExtn;
            when "10" => 
                s_imm_mux <= s_imm_U_signExtn;
            when others => 
                s_imm_mux <= s_imm_J_signExtn;
        end case;
    end process imm_mux_proc;

    --------------------------------------------------------------------------------
    -- MUX process used for selecting between the shamt field and immediate field
    --------------------------------------------------------------------------------
    imm_shamt_mux_proc: process(s_imm_mux, s_shamt_zeroExtn, s_shamt_sel)
    begin
        if s_shamt_sel = '0' then
            s_imm_shamt_mux <= s_imm_mux;
        else
            s_imm_shamt_mux <= s_shamt_zeroExtn;
        end if;  
    end process imm_shamt_mux_proc;

    --------------------------------------------------------------------------------
    -- MUX process for selecting the data input 1 to ALU
    -- When select line is 1 : Input to ALU will be the PC
    -- Else : Input to ALU will come from register output port 1
    --------------------------------------------------------------------------------
    ALUdata1_mux_proc: process(s_reg_read_data_1, s_PC, s_ALUdata1_sel)
    begin      
        if s_ALUdata1_sel = '0'  then
            s_ALUdata1_mux <= s_reg_read_data_1;
        else
            s_ALUdata1_mux <= std_logic_vector(s_PC);
        end if;
    end process ALUdata1_mux_proc;

    --------------------------------------------------------------------------------
    -- MUX process for selecting the data input 2 to ALU
    -- When select line is 1 : Input to ALU will from register output port 1
    -- Else : Input to ALU will come from the output of s_imm_shamt_mux
    --------------------------------------------------------------------------------
    ALUdata2_mux_proc: process(s_reg_read_data_2, s_imm_shamt_mux, s_ALUdata2_sel)
    begin      
        if s_ALUdata2_sel = '0'  then
            s_ALUdata2_mux <= s_reg_read_data_2;
        else
            s_ALUdata2_mux <= s_imm_shamt_mux;
        end if;
    end process ALUdata2_mux_proc;

    --------------------------------------------------------------------------------
    -- Combinational logic for asserting the register write enable
    -- For the register write enable to be active :
    -- regWrite_en from control unit must be active
    -- PC_stall from control unit must be inactive 
    --------------------------------------------------------------------------------
    reg_write_assert_proc: process(s_regWrite_en, s_PC_stall)
    begin
        if s_regWrite_en = '0' then 
            s_reg_write_assert <= '0';
        else
            s_reg_write_assert <= not s_PC_stall;
        end if;
    end process reg_write_assert_proc;

    --------------------------------------------------------------------------------
    -- Process to perform sign extension for hal-word and byte reads from
    -- data memory.
    --------------------------------------------------------------------------------
    mem_data_read_sign_set_proc: process(s_mem_data_read_op, s_data_read_sign_extn_sel, s_data_read_hword_sel, s_data_read_byte_sel)
    begin
        if s_data_read_hword_sel = '0' and s_data_read_byte_sel = '0' then
            s_data_read_sign_extn <= s_mem_data_read_op;
        elsif s_data_read_hword_sel = '1' and s_data_read_byte_sel = '0' then
            if s_data_read_sign_extn_sel = '0' then
                s_data_read_sign_extn(31 downto 16) <= (others => '0');
                s_data_read_sign_extn(15 downto 0) <= s_mem_data_read_op(15 downto 0);
            else
                s_data_read_sign_extn(31 downto 16) <= (others => s_mem_data_read_op(15));
                s_data_read_sign_extn(15 downto 0) <= s_mem_data_read_op(15 downto 0);
            end if;
        elsif s_data_read_hword_sel = '0' and s_data_read_byte_sel = '1' then
            if s_data_read_sign_extn_sel = '0' then
                s_data_read_sign_extn(31 downto 8) <= (others => '0');
                s_data_read_sign_extn(7 downto 0) <= s_mem_data_read_op(7 downto 0);
            else
                s_data_read_sign_extn(31 downto 8) <= (others => s_mem_data_read_op(7));
                s_data_read_sign_extn(7 downto 0) <= s_mem_data_read_op(7 downto 0);
            end if;
        else
            s_data_read_sign_extn <= (others => '1');
        end if;
    end process mem_data_read_sign_set_proc;

    --------------------------------------------------------------------------------
    -- MUX process to selecting the data to be written to the register between the :
    -- ALU data output, data memory output, zero extended U type immediate and 
    -- PC + 4
    --------------------------------------------------------------------------------
    regWrite_src_mux_proc: process(s_ALU_data_op, s_data_read_sign_extn, s_regWrite_src_sel, s_imm_U_signExtn, s_next_instruct)
    begin
        if s_regWrite_src_sel = "00" then
            s_regWrite_data_mux <= s_ALU_data_op;
        elsif s_regWrite_src_sel = "01" then
            s_regWrite_data_mux <= s_data_read_sign_extn;
        elsif s_regWrite_src_sel = "10" then
            s_regWrite_data_mux <= s_imm_U_signExtn;
        elsif s_regWrite_src_sel = "11" then
            s_regWrite_data_mux <= std_logic_vector(s_next_instruct);
        else
            s_regWrite_data_mux <= (others => 'X');
        end if;
    end process regWrite_src_mux_proc;

    --------------------------------------------------------------------------------
    -- Multiplexer to choose the address of next instruction to be executed.
    -- If PCbranch_sel = 1, then branch using B-type immediate
    -- If PCjump_sel = 1, then change address to the output of ALU
    -- Else increment PC value by 4 (1 word = 4 bytes)
    --------------------------------------------------------------------------------
    next_instruct_proc: process(s_PC_stall, s_PCbranch_sel, s_PCjump_sel, s_PC, s_imm_B_signExtn, s_ALU_data_op)
    begin
        if s_PCbranch_sel = '1' then
            s_next_instruct <= s_PC + unsigned(s_imm_B_signExtn(30 downto 0) & '0');
        elsif s_PCjump_sel = '1' then     
            s_next_instruct <= unsigned(s_ALU_data_op);
        else
            s_next_instruct <= s_PC + 4;; 
        end if;
    end process next_instruct_proc;
    
    --------------------------------------------------------------------------------
    -- Process to update the value of PC 
    --------------------------------------------------------------------------------
    PC_update_proc: process(clk, rst)
    begin
        if rst = '1' then
            s_PC <= to_unsigned(0, 32);
        elsif rising_edge(clk) then
            if i_flash_en = '1' or s_PC_stall = '1' then
                s_PC <= s_PC;
            else
                s_PC <= s_next_instruct; 
            end if;
        end if;
    end process PC_update_proc;
end architecture;