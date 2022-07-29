
*Work In Progress*

# The RISC VI ISA Specification

Editor: June R. `microsynthera@pm.me`

Contributor(s): June R.

This document is released under the Creative Commons Attribution 4.0 International License. 

This document is a derivative of "The RISC-V Instruction Set Manual, Volume I: User-Level ISA, Document Version 20191213”, Editors Andrew Waterman and Krste Asanovi ́c, RISC-V Foundation, December 2019."

## Introduction

RISC-VI (pronounced "risk-six") is an experimental instruction set architecture (ISA) designed for balanced ternary computing. Our goals in defining RISC-VI include:

- A completely open ISA that is freely available to academia and industry.
- Fun.

The name RISC-VI was chosen because there are six trits in a tryte. 

### RISC-VI Hardware Platform Terminology

...

### RISC-VI Software Execution Environments and Harts

...

### RISC-VI ISA Overview

...

### Memory

...

### Base Instruction-Length Encoding

...

### Exceptions, Traps, and Interrupts

...

### Unspecified Behaviors and Values

...

## RVI24I Base Integer Instruction Set

### Programmer's Model

The value of a trit will be represented by the characters: "+" for positive, "0" for zero, and "-" for negative. Balanced ternary numbers will be prefixed with "0t" for clarity. For example, `0t+-0` represents the balanced ternary number with a positive 9-trit, negative 3-trit, and zero 1-trit, i.e., 6~10~. 

For RVI24I, the 27 registers are each 24 trits wide, i.e., XLEN=24. Register `0t000` is hardwired with all trits equal to zero. General purpose registers `0t--0` to `0t00-` and `0t00+` to `0t+++` hold values that various instructions interpret as a collection of three-valued logic values, or as balanced ternary integers.

There is one additional register: the program counter "pc" (register `0t---`) holds the address of the current instruction. The first instruction is addressed `0t------------------------`. Subsequent instructions increment the program counter by one. 

### Base Instruction Formats

...

### Immediate Encoding Variants

...

### Integer Computational Instructions

...

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