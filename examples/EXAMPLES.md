# Examples

This directory contains some examples of how to put together pieces of the framework to accomplish various tasks.

## Parser

`parser.rkt` demonstrates how to invoke the TEAL parser.

## Disassembler

`disassemble.rkt` demonstrates how to construct a TEAL disassembler to convert bytecode to a custom format.

The `read-prefix` function reads the leading version `varuint` from a bytecode stream and returns `#false` if a valid `varuint` was not present and a pair of the read version and the remainder of the stream otherwise.

Using this remainder (named `bytecode`), it defines a `read-byte` mixin which defines monadic operations `unit` and `>>=` and a monadic operation `read-byte` which reads a byte from the stream.
These monadic operations thread the current position in the stream.

This mixin is `mix`ed in with the provided `instruction-read/version` to obtain an instruction reader, accessed with the key `'read-instruction`.

The disassmbler uses this reader, named `read-instruction`, to read the instructions in the stream into a map keyed by their position in the stream.

## Analyzer

`analyzer.rkt` uses the disassembler from the previous example to obtain the logic signature version and assembly map (in the function `analyze`).
Given these, it calls `make->` which derives a TEAL virtual machine stepper specialized to them.

`make->` composes the appropriate virtual machine mixin (`vm/version`), instruction mixin (`instruction-name/version` and `instruction-version/version`), monad mixins (`monad+-extras` and `monad-extras`) and analyzer specific mixins defined explicitly.
These mixins each handle a different aspect of the virtual machine.
In order of definition, they handle:

- monadic operations, including `panic` and the `return` instruction;
- virtual machine register manipulation;
- instruction read-at-PC and PC check;
- stack operations;
- integer and byte constant block management;
- logical operations;
- transaction and global field access;
- scratchspace management;
- arithmetic operations;
- extended precision arithmetic operations;
- byte string operations;
- control flow;
- inner transaction operations; and
- mode (`LogicSig` or `Application`) monitoring.

The machine is characterized by each of these components, but in particular the initial monad definitions which specify how the machine state is propagated across execution steps and how to fork execution.

The results of the analysis are the set of seen execution states and final states.
The `analyze-program` function receives these results and cleans them up for inspection.