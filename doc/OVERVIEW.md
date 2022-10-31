# Jade: A TEAL analysis framework

Jade comprises functional units to ingest, manipulate, and scrutinize TEAL programs.
It is designed to be flexible enough to support myriad TEAL program analysis tools---such as linters, type checkers, static analyzers, and symbolic execution engines---with a minimum of code duplication.

In addition to providing this framework, Jade includes an analyzer out of the box which performs *unconstrained property analysis*.
This analysis determines the extent to which the input program constrains generally-sensitive transaction fields, such as the `OnCompletion` property.

The `jade` shell script is self-documenting and will provide instructions if invoked without arguments.

## Installation

Jade is written in [Racket](https://racket-lang.org).

If you have Racket installed, you can run the unconstrained property analysis at the top level of the repository by invoking `./jade`.

Alternatively, if you have Docker installed, you can run the Docker file at the top level of the repository.

## Functional Units

Jade includes a parser out of the box which produces an assembly object.
An assembly structure indicates the TEAL version extracted from the preamble and the assembler directives.
Labels, pragmas, and pseudoinstructions (`pushint` and `pushbytes`) are preserved by the parser.

The assembly parser is implemented in `parse.rkt` and exports the following:

1. `parse` parses a assembly string to an assembly structure.
2. `parse-varuint` parses a varuint string (using any notation accepted in assembly) to a Racket number value.
3. `parse-bytes` parses a bytes string (using any notation accepted in assembly) to a Racket bytes value.

The bytecode disassember is implemented in `disassemble.rkt` and exports `disassemble`, which disassembles a Racket bytes value into an assembly structure.

The `assembly/control.rkt` exports the `control-flow-graph` function which produces a control-flow graph of the given assembly structure.

## Constructing Tools that Operate on TEAL Programs

The Jade framework is implemented in superset of a subset of Racket which does not include observable mutation and makes regular use of higher-order functions.
It is organized using Racket's module system and three minor extensions to Racket: pure object prototype wrappers, records, and sum types.

### Pure Object Prototype Wrappers

Defined in `static/object.rkt`, pure object prototype wrappers can be seen as interface fragments which allow one to assemble pure objects.

A literal prototype wrapper form is tagged with `inc`, for *increment*, as it increments an object in which it is integrated.
The literal
```
(inc ()
     [the-answer
      42])
```
denotes a prototype wrapper which responds to the message `the-answer` with the value 42.

After the keyword `inc` is a list of message names on which the wrapper relies (i.e. assumes exist) but does not itself define.
For example, the literal
```
(inc (a-sum b-sum)
     [c-sum
      (+ a-sum b-sum)])
```
assumes that the message names `a-sum` and `b-sum` exist.

The function `mix` composes wrappers so that
```
(mix (inc ()
          [the-answer
	   42])
     (inc ()
          [the-question
	   "What is the answer to life, the universe, and everything?"]))
```
denotes the wrapper which responds to both the message `the-answer` and the message `the-question`.
Wrappers are first-class, so they can be bound, as in
```
(define the-protocol
  (mix (inc ()
            [the-answer
  	     42])
       (inc ()
            [the-question
	     "What is the answer to life, the universe, and everything?"])))

```

Two wrappers may include definitions for the same message.
If these wrappers are `mix`ed, the earlier argument to `mix` overrides that later.
For instance, the expression
```
(mix (inc ()
          [the-answer
	   43])
     the-protocol)
```
denotes the wrapper which behaves like `the-protocol` except returns 43 in response to `the-answer` message.
An overriding definition can appeal to an overridden definition by quoting the name and qualifying it with `super`.
For example, the expression
```
(mix (inc ()
          [the-answer
	   (+ 1 (super 'the-answer)])
     the-protocol)
```
denotes the same wrapper as just previous but achieves its behavior by deferring to (and modifying) a later wrapper.

A prototype wrapper is not an object, but can be `fix`ed to be an object.
The expression `(fix the-protocol)` denotes the object that responds to `the-question` and `the-answer` messages.

Wrappers provide open recursion so the expression
```
(inc (the-real-answer)
     [the-answer
      the-real-answer])
```
denotes the wrapper in which the result of the `the-answer` message is the result of the *earliest* `the-real-answer` in the wrapper chain.
(Notice how `the-real-answer` is present in the list of assumed message names.)

## The Virtual Machine

The virtual machine (VM) is itself defined in terms of pure objects constructed using `inc`, `mix`, and `fix`.
In particular, an instance of the virtual machine is an object obtained by `fix`ing the result of `vm/version` `mix`ed with prototype wrappers which implement supporting messages.

Two critical supporting messages of the VM prototype wrapper are `unit` (typically called `return`) and `>>=` (pronounced "bind"), the two monadic operations.
The VM prototype wrapper makes no assumptions about the particular monad used, which affords the framework a great amount of flexibility.

Applying the `vm-pseudo` wrapper on top of the result of `vm/version` modifies the VM to operate on `PseudoInstruction`s (regular instructions plus `pushint` and `pushbytes`).
