# jit codegen conventions

## register mapping

in order to get the jit assembler running quickl;y

## codegen conventions

ALL ssa functions are expected to be callable using the System V ABI as its
calling convention. this is because it is easy to implement and makes interop
with zig incredibly simple.

inlined functions or functions created by the compiler can use whatever
convention desired, as long as it doesn't break System V compatibility.

### quick overview

- available registers
    - caller must preserve: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
    - callee must preserve: rbx, rsp, rbp, r12, r13, r14, r15
- function calls
    - the stack must be 16-byte aligned before a call is performed.
    - generally, first 6 parameters go in: rdi, rsi, rdx, rcx, r8, r9
    - arguments greater than 8 and up to 16 bytes use two registers
    - extra arguments, and arguments greater than 16 bytes must be pushed
      to the stack
    - floats are placed in numbered xmm registers
    - values are returned in rax, and rdx can be used for up to 16 bytes


### references
- [the osdev wiki](https://wiki.osdev.org/System_V_ABI) provides a succinct and
  useful high-level description of System V and a TON of helpful links.
    - [this pdf](https://www.uclibc.org/docs/psABI-x86_64.pdf) is linked from
      osdev and provides an incredibly detailed overview