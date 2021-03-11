# 32-Bit RV32I-like Single Cycle RISC Core

This is a 32-Bit single cycle core which is based off of RISCV's RV32I ISA.

The core is completely synthesizable and was tested on a Spartan-6 FPGA.

## HARDWARE REQUIREMENTS FOR TOP MODULE (Using area optimization mode):
  Resource | BRAM | LUTs | Slice Registers | LUT FF pairs 
--- | --- | --- | --- |--- 
Amount Used | 1 | 1698 | 2048 | 2539 | 

Currently the core doesn't support byte and half-word(16-Bit) addressing due to the additional hardware complexity inccured during synthesis.

## Directory Structure :
  * /Design Srcs - Contains all the required HDL files for the core. Additionally it also consists of a "code_translated.txt" file which contains the binary instructions used for initializing the Instruction ROM.
  * /Simulation Srcs - Contains a simple testbench for simulating the execution of the program defined in the "code_translated.txt" file.
