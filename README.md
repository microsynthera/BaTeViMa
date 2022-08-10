# BaTeViMa
A Balanced Ternary Virtual Machine

*Under construction... Come back later.*

## TODO

- [ ] ability to define multiple fixed length bt integer types (neatly)
- [ ] data type containing registers, memory, i/o, etc. (adjustable also?)
- [X] trit-level multiplication
- [ ] trit-level quotRem/divMod
- [ ] trit-level slicing
- [ ] trit-level radix
- [ ] 18-trits word data type
- - [X] Num instance
- - [X] Trits instance (custom Bits-like typeclass)
- - [ ] Integral instance
- - [ ] Real instance
- - [ ] Ix instance
- [X] three-valued logic
- [ ] ternary-coded characters (unicode)
- - [ ] reqs: { #2b57, #2af3, #2add, #2600~26ff (256), #1f411 }
- [ ] TCB/BCT
- [ ] modified RISC-V ISA spec, "RISC-VI"
- - [ ] Base 24-trit Integer Instruction Set (modified RV32I, "RVI24I")
- - [ ] Integer Multiplication/Division Extension "M" (modified RISC-V extention "M")
- [ ] emulate RVI24IM as defined in the spec
- [ ] RVI23IM assembly language compiler
- [ ] gui for emulated RVI23IM machine
- - [ ] display register and processor status ("blinkenlights")
