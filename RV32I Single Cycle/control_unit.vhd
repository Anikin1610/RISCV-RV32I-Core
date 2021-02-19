--------------------------------------------------------------------------------
-- Control Unit for the Single cycle implementation of the RV32I core 
-- Currently unsupported instructions :
-- ecall, ebreak, fence
--------------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity control_unit is
  port (
    i_opcode : in std_logic_vector(6 downto 0);         -- Input 7-bit OPCODE 
    i_funct3 : in std_logic_vector(2 downto 0);         -- Input 3-bit funct3
    i_funct7 : in std_logic_vector(6 downto 0);         -- Input 7-bit funct7
    i_ALUzero : in std_logic;                           -- Input zero flag from ALU
    i_ALUsign : in std_logic;                           -- Input sign flag from ALU
    i_data_read_invalid : in std_logic;                 -- Input read invalid flag from data memory
    i_data_write_invalid : in std_logic;                -- Input write invalid flag from data memory
    i_instruct_read_invalid : in std_logic;             -- Input read invalid flag from instruction memory
    o_ALUdata1_sel : out std_logic;                     -- Output ALU data input 1 select line (0 => from Register, 1 => from PC)
    o_ALUdata2_sel : out std_logic;                     -- Output ALU data input 2 select line (0 => from Register, 1 => from immediate)
    o_imm_type_sel : out std_logic_vector(1 downto 0);  -- Output Immediate type select line (00 => I type immediate,
                                                        --                                    01 => S type immediate,
                                                        --                                    10 => U type immediate)
    o_shamt_sel : out std_logic;                        -- Output Shamt select line (0 => Use immediate, 1 => Use Shamt field)
    o_ALU_oper_sel : out std_logic_vector(3 downto 0);  -- Output 4-bit Operation select line for ALU
    o_data_write_en : out std_logic;                    -- Output data memory write enable 
    o_data_write_hword_sel : out std_logic;             -- Output data memory write half-word signal
    o_data_write_byte_sel : out std_logic;              -- Output data memory write byte signal
    o_data_read_sign_extn_sel : out std_logic;          -- Output data memory sign extend for half-word and byte reads (0 => Zero extend, 1 => Sign extend) 
    o_data_read_hword_sel : out std_logic;              -- Output data memory read half-word signal
    o_data_read_byte_sel : out std_logic;               -- Output data memory read byte signal
    o_regWrite_src_sel : out std_logic_vector(1 downto 0);  -- Output register write data source select line (00 => Write ALU output to register,
                                                            --                                                01 => Write data memory output to register,
                                                            --                                                10 => Write sign extended U-type immediate to register, 
                                                            --                                                11 => Write address of next instruction to register) 
    o_regWrite_en : out std_logic;                      -- Output register write enable
    o_PCjump_sel : out std_logic;                       -- Output PC jump select line (0 => No jump, 1 => jump to address)
    o_PCbranch_sel : out std_logic;                     -- Output PC branch select line (0 => Don't branch, 1 => Branch to PC + sign extended B-type immediate)
    o_PC_stall : out std_logic);                        -- Output Stall PC signal
end entity control_unit;

architecture control_beh of control_unit is
  signal s_data_read_en, s_data_write_en : std_logic;   -- Internal signals to keep track of reads and writes to data memory 
  signal s_force_stall : std_logic;                     -- Internal signal to force the PC update to be stalled
begin

  o_data_write_en <= s_data_write_en;

  --------------------------------------------------------------------------------
  -- Process to assert o_PC_stall and stop updation of PC and execution of
  -- further instructions under the following cases :
  -- Data memory read is invalid,
  -- Data memory write is invalid,
  -- Instruction memory read is invalid,
  -- Instruction is invalid/not supported.
  --------------------------------------------------------------------------------
  pc_stall_proc: process(s_data_read_en, i_data_read_invalid,i_data_write_invalid, i_instruct_read_invalid, s_data_write_en, s_force_stall)
  begin
    if (s_data_read_en = '1' and i_data_read_invalid ='1') or (s_data_write_en = '1' and i_data_write_invalid ='1') or i_instruct_read_invalid = '1' or s_force_stall = '1' then
      o_PC_stall <= '1';
    else
      o_PC_stall <= '0';
    end if;
  end process pc_stall_proc;

  --------------------------------------------------------------------------------
  -- Combinational Process for decoding the instruction based on :
  -- opcode, funct3 and funct7 fields from the instruction and
  -- i_ALUzero and i_ALUsign for branch instructions.
  --------------------------------------------------------------------------------
  opcode_decode : process (i_opcode, i_funct3, i_funct7, i_ALUzero, i_ALUsign)
  begin

    --------------------------------------------------------------------------------
    -- R-type instructions (OPCODE = 0110011) operate on two data from the register 
    -- bank and the result is stored back into the register. 
    --------------------------------------------------------------------------------
    if i_opcode = "0110011" then
      o_imm_type_sel <= "11";                             --  No immediates are involved in R-type instruction
      o_ALUdata1_sel <= '0';                              --  Data input 1 to ALU is from register 
      o_ALUdata2_sel <= '0';                              --  Data input 2 to ALU is from register
      o_shamt_sel <= '0';                                 --  No immediates are involved in R-type instruction
      s_data_write_en <= '0';                             --  No data is written to memory
      o_data_write_hword_sel <= '0';                      --  No data is written to memory
      o_data_write_byte_sel <= '0';                       --  No data is written to memory
      s_data_read_en <= '0';                              --  No data is read from memory
      o_data_read_hword_sel <= '0';                       --  No data is read from memory
      o_data_read_byte_sel <= '0';                        --  No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   --  No data is read from memory
      o_regWrite_src_sel <= "00";                         --  ALU output is written to register
      o_regWrite_en <= '1';                               --  Data is written back into register
      o_PCjump_sel <= '0';                                --  PC is incremented by 4. No jumps are involved
      o_PCbranch_sel <= '0';                              --  PC is incremented by 4. No branches are involved
      
      
      case i_funct3 is
        when "000" =>
          if i_funct7 = "0000000" then                    -- ADDITION OPERATION
            o_ALU_oper_sel <= "0101";
            s_force_stall <= '0';
          elsif i_funct7 = "0100000" then                 -- SUBTRACTION OPERATION
            o_ALU_oper_sel <= "0110";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";                     
            s_force_stall <= '1';
          end if;
        when "001" =>                                     
          if i_funct7 = "0000000" then                    -- SHIFT LEFT LOGICAL INSTRUCTION
            o_ALU_oper_sel <= "1001";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when "010" =>                                     -- SET ON LESS THAN INSTRUCTION
          if i_funct7 = "0000000" then
            o_ALU_oper_sel <= "0111";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION                     
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when "011" => 
          if i_funct7 = "0000000" then                    -- SET ON LESS THAN (unsigned data inputs) INSTRUCTION
            o_ALU_oper_sel <= "1000";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION                       
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when "100" =>     
          if i_funct7 = "0000000" then                    -- LOGICAL BITWISE XOR INSTRUCTION
            o_ALU_oper_sel <= "0011";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when "101" =>
          if i_funct7 = "0000000" then                    -- SHIFT RIGHT LOGICAL INSTRUCTION
            o_ALU_oper_sel <= "1010";
            s_force_stall <= '0';
          elsif i_funct7 = "0100000" then                 -- SHIFT RIGHT ARITHEMETIC INSTRUCTION
            o_ALU_oper_sel <= "1011";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when "110" =>
          if i_funct7 = "0000000" then                    -- LOGICAL BITWISE OR INSTRUCTION
            o_ALU_oper_sel <= "0010";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when "111" =>
          if i_funct7 = "0000000" then                    -- LOGICAL BITWISE AND INSTRUCTION
            o_ALU_oper_sel <= "0001";
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            s_force_stall <= '1';
          end if;
        when others =>                                    -- INVALID INSTRUCTION
          o_ALU_oper_sel <= "1111";
          s_force_stall <= '1';
      end case;
    
    --------------------------------------------------------------------------------
    -- i-type instructions (OPCODE = 0010011) operate on ONE data from the register 
    -- bank, 1 sign/zero extended immediate value and the result is stored 
    -- back into the register bank. 
    --------------------------------------------------------------------------------
    elsif i_opcode = "0010011" then
      o_imm_type_sel <= "00";                             -- I-type instructions operate on I-type immediates
      o_ALUdata1_sel <= '0';                              -- Data input 1 to ALU is from register bank
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is sign/zero extended immediate
      s_data_write_en <= '0';                             -- No data is written to memory
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory
      o_regWrite_src_sel <= "00";                         -- ALU output is written to register
      o_regWrite_en <= '1';                               -- Data is written back into register
      o_PCjump_sel <= '0';                                -- PC is incremented by 4. No jumps are involved                        
      o_PCbranch_sel <= '0';                              -- PC is incremented by 4. No branches are involved

      case i_funct3 is
        when "000" =>                                     -- IMMEDIATE ADDITION INSTRUCTION
          o_ALU_oper_sel <= "0101"; 
          o_shamt_sel <= '0';                             -- Shamt field is not used 
          s_force_stall <= '0';
          
        when "001" =>                                     -- IMMEDIATE SHIFT LEFT LOGICAL
          if i_funct7(6 downto 1) = "000000" then
            o_ALU_oper_sel <= "1001";
            o_shamt_sel <= '1';                           -- Shamt field is used as immediate
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            o_shamt_sel <= '0';
            s_force_stall <= '1';
          end if;

        when "010" =>                                     -- IMMEDIATE SET ON LESS THAN 
          o_ALU_oper_sel <= "0111";
          o_shamt_sel <= '0';                             -- Shamt field is not used
          s_force_stall <= '0';

        when "011" =>                                     -- IMMEDIATE SET ON LESS THAN (unsigned data)
          o_ALU_oper_sel <= "1000";
          o_shamt_sel <= '0';                             -- Shamt field is not used
          s_force_stall <= '0';

        when "100" =>                                     -- IMMEDIATE LOGICAL BITWISE XOR
          o_ALU_oper_sel <= "0011";
          o_shamt_sel <= '0';                             -- Shamt field is not used
          s_force_stall <= '0';

        when "101" =>
          if i_funct7(6 downto 1) = "000000" then         -- IMMEDIATE SHIFT RIGHT LOGICAL
            o_ALU_oper_sel <= "1010";
            o_shamt_sel <= '1';                           -- Shamt field is used
            s_force_stall <= '0';
          elsif i_funct7(6 downto 1) = "010000" then      -- IMMEDIATE SHIFT RIGHT ARITHEMETIC
            o_ALU_oper_sel <= "1011";
            o_shamt_sel <= '1';                           -- Shamt field is not used
            s_force_stall <= '0';
          else                                            -- INVALID INSTRUCTION
            o_ALU_oper_sel <= "1111";
            o_shamt_sel <= '0';
            s_force_stall <= '1';
          end if;

        when "110" =>                                     -- IMMEDIATE LOGICAL BITWISE OR
          o_ALU_oper_sel <= "0010";
          o_shamt_sel <= '0';                             -- Shamt field is not used
          s_force_stall <= '0';
        
        when "111" =>                                     -- IMMEDIATE LOGICAL BITWISE AND
          o_ALU_oper_sel <= "0001";
          o_shamt_sel <= '0';                             -- Shamt field is not used
          s_force_stall <= '0';
        
        when others =>                                    -- INVALID INSTRUCTION
          o_ALU_oper_sel <= "1111";
          o_shamt_sel <= '0';          
          s_force_stall <= '1';
      end case;
    
    --------------------------------------------------------------------------------
    -- Load instructions (OPCODE = 0000011) generates address by adding an immediate
    -- offset and contents of a register (rs1). Data to be stored comes from the 
    -- second register output port (rs2).
    -- Immediates are encoded as I-type  
    --------------------------------------------------------------------------------
    elsif i_opcode = "0000011" then 
      o_imm_type_sel <= "00";                             -- Load instructions operate on I-type immediates                
      o_ALUdata1_sel <= '0';                              -- Data input 1 to ALU is from register bank
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is sign extended immediate
      o_shamt_sel <= '0';                                 -- Shamt field is not used
      o_ALU_oper_sel <= "0101";                           -- ALU operation done is addition
      s_data_write_en <= '0';                             -- No data is written to memory
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '1';                              -- Data is read from memory
      o_regWrite_en <= '1';                               -- Data is written to register
      o_regWrite_src_sel <= "01";                         -- Memory output is written to register
      o_PCjump_sel <= '0';                                -- PC is incremented by 4. No jumps are involved
      o_PCbranch_sel <= '0';                              -- PC is incremented by 4. No branches are involved
      
      if i_funct3 = "000" then                            -- LOAD BYTE INSTRUCTION
        o_data_read_sign_extn_sel <= '1';                 -- Memory output is sign extended
        o_data_read_hword_sel <= '0';                     -- Byte of data is read
        o_data_read_byte_sel <= '1';                      -- Byte of data is read
        s_force_stall <= '0'; 
      
      elsif i_funct3 = "001" then                         -- LOAD HALF-WORD INSTRUCTION
        o_data_read_sign_extn_sel <= '1';                 -- Memory output is sign extended
        o_data_read_hword_sel <= '1';                     -- Half-word of data is read
        o_data_read_byte_sel <= '0';                      -- Half-word of data is read
        s_force_stall <= '0';

      elsif i_funct3 = "010" then                         -- LOAD WORD INSTRUCTION
        o_data_read_sign_extn_sel <= '0';                 -- Memory output is not extended
        o_data_read_hword_sel <= '0';                     -- Word of data is read
        o_data_read_byte_sel <= '0';                      -- Word of data is read
        s_force_stall <= '0';

      elsif i_funct3 = "100" then                         -- LOAD BYTE UNSIGNED INSTRUCTION
        o_data_read_sign_extn_sel <= '0';                 -- Memory output is zero extended
        o_data_read_hword_sel <= '0';                     -- Byte of data is read
        o_data_read_byte_sel <= '1';                      -- Byte of data is read
        s_force_stall <= '0';

      elsif i_funct3 = "101" then                         -- LOAD HALF-WORD UNSIGNED INSTRUCTION
        o_data_read_sign_extn_sel <= '0';                 -- Memory output is zero extended
        o_data_read_hword_sel <= '1';                     -- Half-word of data is read
        o_data_read_byte_sel <= '0';                      -- Half-word of data is read
        s_force_stall <= '0';
      
      else                                                -- INVALID INSTRUCTION
        o_data_read_sign_extn_sel <= '0';
        o_data_read_hword_sel <= '0';
        o_data_read_byte_sel <= '0';
        s_force_stall <= '1';
      end if;

    --------------------------------------------------------------------------------
    -- Store instructions (OPCODE = 0100011) generates address by adding an immediate
    -- offset and contents of a register (rs1). Data is stored in register at 
    -- address rd
    -- Immediates are encoded as S-type  
    --------------------------------------------------------------------------------
    elsif i_opcode = "0100011" then 
      o_imm_type_sel <= "01";                             -- Store instructions operate on S-type immediates
      o_ALUdata1_sel <= '0';                              -- Data input 1 to ALU is from register bank
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is zero extended immediate
      o_shamt_sel <= '0';                                 -- Shamt field is not used
      o_ALU_oper_sel <= "0101";                           -- ALU operation is addition
      s_data_write_en <= '1';                             -- Data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory 
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory  
      o_regWrite_src_sel <= "00";                         -- No data is written to register
      o_regWrite_en <= '0';                               -- No data is written to regiser
      o_PCjump_sel <= '0';                                -- PC is incremented by 4. No jumps are involved 
      o_PCbranch_sel <= '0';                              -- PC is incremented by 4. No branches are involved

      if i_funct3 = "000" then                            -- STORE BYTE INSTRUCTION
        o_data_write_hword_sel <= '0';                    -- Byte of data is written
        o_data_write_byte_sel <= '1';                     -- Byte of data is written
        s_force_stall <= '0';

      elsif i_funct3 = "001" then                         -- STORE HALF-WORD INSTRUCTION
        o_data_write_hword_sel <= '1';                    -- Hald-word of data is written
        o_data_write_byte_sel <= '0';                     -- Hald-word of data is written
        s_force_stall <= '0';
      
      elsif i_funct3 = "010" then                         -- STORE WORD INSTRUCTION
        o_data_write_hword_sel <= '0';                    -- Word of data is written
        o_data_write_byte_sel <= '0';                     -- Word of data is written
        s_force_stall <= '0';
      
      else                                                -- INVALID INSTRUCTION
        o_data_write_hword_sel <= '0';
        o_data_write_byte_sel <= '0';
        s_force_stall <= '1';
      end if;

    --------------------------------------------------------------------------------
    -- Branch instructions (OPCODE = 1100011) compare contents of 2 registers
    -- (rs1 and rs2) and update the content of PC accordingly.  
    --------------------------------------------------------------------------------
    elsif i_opcode = "1100011" then --Branch Instructions
      o_imm_type_sel <= "11";                             -- Immediates are used only during the updation of PC  
      o_ALUdata1_sel <= '0';                              -- Data input 1 to ALU is from register
      o_ALUdata2_sel <= '0';                              -- Data input 2 to ALU is from register
      o_shamt_sel <= '0';                                 -- Shamt field in not used
      s_data_write_en <= '0';                             -- No data is written to memory 
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory
      o_regWrite_src_sel <= "00";                         -- No data is written to register
      o_regWrite_en <= '0';                               -- No data is written to register
      o_PCjump_sel <= '0';                                -- PC value changes based on branch condition. No jumps are involved

      case i_funct3 is
        when "000" =>                                     -- BRANCH ON EQUAL INSTRUCTION 
          o_ALU_oper_sel <= "0110";                       -- ALU operation done is subtraction
          if i_ALUzero = '1' then                         -- If zero flag is high (i.e. the two inputs to ALU are equal)
            o_PCbranch_sel <= '1';                        -- Branching occurs
          else
            o_PCbranch_sel <= '0';                        -- Else PC is incremented by 4
          end if;
          s_force_stall <= '0';

        when "001" =>                                     -- BRANCH ON NOT EQUAL INSTRUCTION
          o_ALU_oper_sel <= "0110";                       -- ALU operation done is subtraction
          if i_ALUzero = '0' then                         -- If zero flag is low (i.e. the two inputs to ALU are not equal)
            o_PCbranch_sel <= '1';                        -- Branching occurs
          else
            o_PCbranch_sel <= '0';                        -- Else PC is incremented by 4
          end if;
          s_force_stall <= '0';

        when "100" =>                                     -- BRANCH ON LESS THAN INSTRUCTION
          o_ALU_oper_sel <= "0111";                       -- ALU operation done is "set on less than"
          if i_ALUzero = '0' then                         -- If zero flag is low (i.e. contents of rs1 < rs2)
            o_PCbranch_sel <= '1';                        -- Branching occurs
          else
            o_PCbranch_sel <= '0';                        -- Else PC is incremented by 4
          end if;
          s_force_stall <= '0';

        when "101" =>                                     -- BRANCH ON GREATER THAN OR EQUAL INSTRUCTION
          o_ALU_oper_sel <= "0111";                       -- ALU operation done is "set on less than"
          if i_ALUzero = '1' then                         -- If zero flag is high (i.e. contents of rs1 >= rs2)
            o_PCbranch_sel <= '1';                        -- Branching occurs
          else
            o_PCbranch_sel <= '0';                        -- Else PC is incremented by 4
          end if;
          s_force_stall <= '0';

        when "110" =>                                     -- BRANCH ON LESS THAN INSTRUCTION (unsigned comparion)
          o_ALU_oper_sel <= "1000";                       -- ALU operation done is "set on less than using unsigned comparison"
          if i_ALUzero = '0' then                         -- If zero flag is low (i.e. contents of rs1 < rs2)
            o_PCbranch_sel <= '1';                        -- Branching occurs
          else
            o_PCbranch_sel <= '0';                        -- Else PC is incremented by 4
          end if;
          s_force_stall <= '0';

        when "111" =>                                     -- BRANCH ON GREATER THAN OR EQUAL INSTRUCTION (unsigned comparison)                       
          o_ALU_oper_sel <= "1000";                       -- ALU operation done is "set on less than using unsigned comparison"
          if i_ALUzero = '1' then                         -- If zero flag is high (i.e. contents of rs1 >= rs2)
            o_PCbranch_sel <= '1';                        -- Branching occurs
          else
            o_PCbranch_sel <= '0';                        -- Else PC is incremented by 4
          end if;
          s_force_stall <= '0';

        when others =>                                    -- INVALID INSTRUCTION
          o_ALU_oper_sel <= "1111";
          o_PCbranch_sel <= '0';
          s_force_stall <= '1';
        end case;

    --------------------------------------------------------------------------------
    -- Jump and link register instruction (OPCODE = 1100111) forms the jump address
    -- by addition of a 32-bit register rs1 and a sign extended immediate.
    -- The address of next instruction is stored in the register location rd.
    -- I-type encoding is used for immediate
    --------------------------------------------------------------------------------
    elsif i_opcode = "1100111" then 
      o_imm_type_sel <= "00";                             -- JALR instruction operate on I-type immediates
      o_ALUdata1_sel <= '0';                              -- Data input 1 to ALU is from register bank
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is signed extended immediate
      o_shamt_sel <= '0';                                 -- Shamt field is not used
      o_ALU_oper_sel <= "0101";                           -- ALU is operation is addition 
      s_data_write_en <= '0';                             -- No data is written to memory 
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory  
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory
      o_regWrite_src_sel <= "11";                         -- Next instruction (PC + 4) is written to register 
      o_regWrite_en <= '1';                               -- Data is written to register 
      o_PCjump_sel <= '1';                                -- PC value changes to the address of next instruction given by jump instruction
      o_PCbranch_sel <= '0';                              -- PC value changes to the address of next instruction given by jump instruction. 
                                                          -- Branching is not involved.
      s_force_stall <= '0';
    
    --------------------------------------------------------------------------------
    -- Jump and link instruction (OPCODE = 1101111) forms the jump address
    -- by addition of PC and a sign extended immediate.
    -- The address of next instruction is stored in the register location rd.
    -- J-type encoding is used for immediate
    --------------------------------------------------------------------------------
    elsif i_opcode = "1101111" then 
      o_imm_type_sel <= "11";                             -- JAL instruction operates on J-type immediates 
      o_ALUdata1_sel <= '1';                              -- Data input 1 to ALU is from PC
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is sign extended immediate
      o_shamt_sel <= '0';                                 -- Shamt field is not used
      o_ALU_oper_sel <= "0101";                           -- ALU operation is addition
      s_data_write_en <= '0';                             -- No data is written to memory
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory
      o_regWrite_src_sel <= "11";                         -- Next instruction (PC + 4) is written to register
      o_regWrite_en <= '1';                               -- Data is written to register
      o_PCjump_sel <= '1';                                -- PC value changes to the address of next instruction given by jump instruction
      o_PCbranch_sel <= '0';                              -- PC value changes to the address of next instruction given by jump instruction
                                                          -- Branching is not involved.
      s_force_stall <= '0';
      
    --------------------------------------------------------------------------------
    -- Load upper immediate instruction (OPCODE = 0110111) writes a 
    -- 20-bit immediate to the first 20 bits of register rd and the rest
    -- with zeros.
    -- U-type encoding is used for immediate
    --------------------------------------------------------------------------------                                                      
    elsif i_opcode = "0110111" then 
      o_imm_type_sel <= "10";                             -- LUI instruction operate on U-type immediate                   
      o_ALUdata1_sel <= '0';                              -- Data input 1 to ALU not necessary (Don't care)
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is from immediate
      o_shamt_sel <= '0';                                 -- Shamt field is not used
      o_ALU_oper_sel <= "0000";                           -- ALU doesn't perform any operation (Pass data input 2 to output)
      s_data_write_en <= '0';                             -- No data is written to memory 
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory
      o_regWrite_src_sel <= "10";                         -- ALU output is written to register
      o_regWrite_en <= '1';                               -- Data is written to register
      o_PCjump_sel <= '0';                                -- PC is incremented by 4. No jumps are involved
      o_PCbranch_sel <= '0';                              -- PC is incremented by 4. No branches are involved
      s_force_stall <= '0';

    --------------------------------------------------------------------------------
    -- Add upper immediate to PC instruction (OPCODE = 0010111) adds a 
    -- 20-bit immediate left shifted by 12 bits to the current value of PC and
    -- stores in register rd
    -- U-type encoding is used for immediate
    -------------------------------------------------------------------------------- 
    elsif i_opcode = "0010111" then 
      o_imm_type_sel <= "10";                             -- AUIPC instruction operate on U-type immediate
      o_ALUdata1_sel <= '1';                              -- Data input 1 to ALU is from PC
      o_ALUdata2_sel <= '1';                              -- Data input 2 to ALU is from immediate
      o_shamt_sel <= '0';                                 -- Shamt field is not used
      o_ALU_oper_sel <= "0101";                           -- ALU operation is addition   
      s_data_write_en <= '0';                             -- No data is written to memory 
      o_data_write_hword_sel <= '0';                      -- No data is written to memory
      o_data_write_byte_sel <= '0';                       -- No data is written to memory
      s_data_read_en <= '0';                              -- No data is read from memory
      o_data_read_hword_sel <= '0';                       -- No data is read from memory
      o_data_read_byte_sel <= '0';                        -- No data is read from memory
      o_data_read_sign_extn_sel <= '0';                   -- No data is read from memory
      o_regWrite_src_sel <= "00";                         -- ALU output is written to register
      o_regWrite_en <= '1';                               -- Data is written to register
      o_PCjump_sel <= '0';                                -- PC is incremented by 4. No jumps are involved
      o_PCbranch_sel <= '0';                              -- PC is incremented by 4. No branches are involved
      s_force_stall <= '0';
      
    else                                                  -- INVALID OPCODE
      o_imm_type_sel <= "11";
      o_ALUdata1_sel <= '0';
      o_ALUdata2_sel <= '0';
      o_shamt_sel <= '0';
      o_ALU_oper_sel <= "1111";
      s_data_write_en <= '0';
      o_data_write_hword_sel <= '0';
      o_data_write_byte_sel <= '0';
      s_data_read_en <= '0';
      o_data_read_hword_sel <= '0';
      o_data_read_byte_sel <= '0';
      o_data_read_sign_extn_sel <= '0';
      o_regWrite_src_sel <= "00";
      o_regWrite_en <= '0';
      o_PCjump_sel <= '0';
      o_PCbranch_sel <= '0';
      s_force_stall <= '1';
    end if;
  end process opcode_decode;
end architecture control_beh;