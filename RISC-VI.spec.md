
*Work In Progress*

# The RISC VI ISA Specification

Editor(s): June R. `microsynthera@pm.me`

Contributor(s): June R.

This document is released under the Creative Commons Attribution 4.0 International License.

This document is a derivative of "The RISC-V Instruction Set Manual, Volume I: User-Level ISA, Document Version 20191213”, Editors Andrew Waterman and Krste Asanovi ́c, RISC-V Foundation, December 2019."

## Introduction

RISC-VI (pronounced "risk-six") is an experimental instruction set architecture (ISA) designed for balanced ternary computing. Our goals in defining RISC-VI include:

- A completely open ISA that is freely available to academia and industry.
- Fun.

The name RISC-VI was chosen because there are six trits in a tryte.

## RVI18I Base Integer Instruction Set

### Programmer's Model

The value of a trit will be represented by the characters: "+" for positive, "0" for zero, and "-" for negative. Sequences of trits will be interpreted as big endian unless noted otherwise. Balanced ternary numbers will be prefixed with "0t" for clarity. For example, `0t0-+` represents the balanced ternary number with a positive 9-trit, negative 3-trit, and zero 1-trit, i.e., 6_10_.

For RVI24I, the 27 registers are each 18 trits wide, i.e., XLEN=18. Register `0t---` is hardwired with all trits equal to zero. General purpose registers `0t0--` to `0t0++` hold values that various instructions interpret as a collection of three-valued logic values, or as balanced ternary integers.

There is one additional register: the program counter "pc" (register `0t+++`) holds the address of the current instruction. The first instruction is addressed `0t------------------`. Subsequent instructions increment the program counter by one.

### Base Instruction Formats

Each instruction is stored across an 18-trit word.

legend:
`[another_label(number_of_trits): range_higher~range_lower]+[label(number_of_trits): range_higher~range_lower]`
- **label** is the name of the section of the instruction format
- **number_of_trits** is the number of trits in this section
- **range_lower** is the lower bound of the section, inclusive
- **range_higher** is the higher bound of the section, inclusive

#### R-type instruction (register-register instructions)
`[funct7(3): 17~15]+[rs2(3): 14~12]+[rs1(3): 11~9]+[funct3(3): 8~6]+[rd(3): 5~3]+[opcode(3): 2~0]`

#### I-type instruction (register-immediate instructions)
`[imm(6): 17~12]+[rs1(3): 11~9]+[funct3(3): 8~6]+[rd(3): 5~3]+[opcode(3): 2~0]`

### Immediate Encoding Variants

...

### Integer Computational Instructions

Most integer computational instructions operate on XLEN trits of values held in the integer register file. Integer computational instructions are either encoded as register-immediate operations using the I-type format or as register-register operations using the R-type format. The destination is register *rd* for both register-immediate and regitster-register instructions. No integer computational instructions cause arithmetic exceptions.

### Control Transfer Instructions

...

### Load and Store Instructions

...

### Memory Ordering Instructions

...

### Environment Call and Breakpoints

...

### HINT Instructions

...

## "M" Standard Extension for Integer Multiplication and Division

...

### Multiplication Operations

...

### Division Operations

...


## ISA Extension Naming Conventions

...

## History and Acknowledgements

...