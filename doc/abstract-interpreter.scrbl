#lang scribble/text

@title{Design of an Abstract Interpreter}

The ideal is to turn the crank on the Abstracting Abstract Machines (AAM) recipe to derive an abstract interpreter from a concrete interpreter.
We will be able to follow the recipe in spirit, but the TEAL virtual machine is defined in significantly different terms than a CESK machine.

The most salient issue is that the TEAL VM is a stack machine (whose stack is effectively unbounded) interpreting instructions whereas a CESK machine is more like a register machine with an effectively unbounded number of registers.
This distinction is important for several reasons.
First, variables names, when present, provide quantities with a stable reference across evaluation steps.
A stack position has no such quality.

It is tempting to try to recover names.
Without recovering other high-level control features, the names would exist in a global scope and would be meaningless---to determine that a stack argument is a reference to a prior name, one would have to find a way to correlate stack positions across steps anyway.

The alternative that we will use is simple (and a kind of name recovery, I suppose):
each stack position paired with a program counter is a name.
Within a basic block, the stack operations of each instruction can be used to determine how the names are used and change.

Names alone produce a kind of 0CFA.
Possible contexts are:
- last k jump positions
- the set of all jump positions and which way they went (intriguing)

Can store-allocate symbolic expressions.
Need to figure out the semantics when joining.
For TEALv3 and prior (before backward jumps), we won't need to join.

