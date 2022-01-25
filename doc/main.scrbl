#lang scribble/base

Jade is a static analysis tool for TEAL bytecode.

Currently, Jade supports a single analysis, @secref{unconstrained-parameter-analysis}.
We are currently working to enhance Jade with multiple analyses.

We intend for most of the analyses Jade features to be @deftech{fully-automatic}
meaning that the analysis requires only a specification of its execution environment (e.g. via command-line arguments) to succeed.
@secref{unconstrained-parameter-analysis} is @reftech{fully-automatic} in this sense.
(We also intend for Jade to feature some @deftech{semi-automatic} analyses which can establish deeper properties about DApps but require expert guidance to succeed.)

@section[#:tag "installation"]{Installation}

Jade is written in @link{Racket} and uses the @link{Z3} SMT solver.

In a clone of the @link{Jade repository}, you can invoke Jade using XXX.

Alternatively, if you have Docker installed, you can run the Docker file present at the repository top level.

@section[#:tag "unconstrained-parameter-analysis"]{Unconstrained Parameter Analysis}

Each execution of a transaction takes place within an environment comprising the current state of the blockchain and a transaction group.
Each transaction in the group has a number of parameters which can be set freely.

The environment , comprised of global properties, and a transaction group The Algorand Virtual Machine allows transactions to specify effect

The effects of execution of a transaction is Many transaction 

Jade is a fully-automatic static analysis tool for TEAL bytecode.

Currently, Jade supports TEALv3 bytecode and ensures that TEAL programs abide by the following constraints:

First, each successful execution of the program must constrain the @something{OnCompletion} property appropriately. [what does this mean?]

Second, each successful execution of the program must constrain the @something{RekeyTo} property to be the zero address.
If the program permits other addresses, then it [may be, in some cases, check about transaction groups] is possible to construct a transaction which changes the account authorization [verbiage?] key.
If an attacker is able to coax a user to submit a transaction to the Algorand network with @something{RekeyTo} set to an attacker-chosen key, then the attacker can assume control of the account.
[talk about the forms an attacker could take]

Currently, the tool ingests TEALv3 bytecode.
By default, the tool interprets all constants within the program literally.
Thus, the default behavior demonstrates that a particular program abides by the constraints.
However, many such TEALv3 programs are generated from a template by replacing in template variables with concrete values.
To demonstrate that any program generated from the template abides by the properties, the user of the tool may specify constants within the program which should not be treated literally but which instead stand for template variables.
So specified, the tool reports whether the program abides by the constraints @emph{for any legal choice of concrete constant values}.

how to acquire the tool
(download a Dockerized executable)

how to run the tool
(running on the command line)

what the tool does
determines what/how parameters are constrained, including globals.
- ways to characterize the constraints on a parameter:
  - not at all
  - to a particular value
  - in the satisfaction of a formula (which has to do with other constraints)
  - no value at all can exist which constrains it

how to use the tool
- sensible defaults
- document the kinds of vulnerabilities that may exist when particular fields are unconstrained

@section{Typechecking}

A stack value can be either a @vm{uint64} or a vm{bytes}, represented in the Go VM as a @go{uint} and @go{[]byte}, respectively.
The Go VM also implements the notion of a stack value as a record with fields each for the @vm{uint64} and @vm{bytes} values.
A @go{StackValue} is considered to hold a @vm{bytes} value if its @go{[]byte} field is not @go{nil} and to hold a @vm{uint64} value otherwise.
That is, the Go VM uses an ad-hoc encoding of a record to represent a sum type, a fragile representation which can easily fall out of consistency.

In the implementation of several instructions, the Go VM assumes---but does not check---that the interpretation of a particular stack value as a @vm{uint64} is valid.
I have not verified whether it is possible to provoke the VM to set the @go{[]byte} field of a stack value that previously encoded a @vm{uint64}.
If it is not possible, then only the most implicit and subtle coding protocol and practice prevents it.
If so, then it is possible to write a TEAL program which produces a @vm{uint64} stack value, interprets it as @vm{bytes}, and then accesses and uses its old @vm{uint64} value.
Again, I have not verified whether such behavior, if possible, can be converted into something that subverts expectations, but it seems there is no reason to permit it and also that a simple static check (a type check, in particular) could rule it out.



#lang scribble/base

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



#lang scribble/base

@title{Unconstrained Parameter Analysis}

@; describe what it is
At the outset of TEAL execution, the execution environment is entirely unconstrained in the sense that, theoretically, execution could succeed no matter what values it comprised.
However, many TEAL programs are intended to succeed only when certain parameters meet certain constraints.
These contraints could be strict, such as limiting the parameter to a single static value, or loose, such as accepting values that satisfy a particular formula.
Which values are intended to be allowed is ultimately DApp-specific, but there are many parameters that in most cases should be limited in particular known ways, and it makes sense to verify whether the TEAL program in fact carries that limitation out.


A TEAL program executes in the context of three things:
an execution mode, either @tech{Signature} or @tech{Application};
a set of global values; and
a transaction group.
The transaction group comprises up to 16 transactions and each transaction has its own transaction parameters.
Some of these parameters are given meaning by execution, such as the arguments.
Others, however, specify effects surrounding the execution itself.
For instance, the @txnfield{RekeyTo} field specifies an address which is allowed to sign transactions on behalf of the current sender.

Let's go through each and determine whether it should be constrained.
(I probably should just have conservative defaults but let users specify which parameters should be constrained.)
@(table 0 @txnfield{Sender} @snippet{no}
        1 @txnfield{Fee} @snippet{maybe}
        2 @txnfield{FirstValid} @snippet{probably, because it may be intended only later than some point}
        3 @txnfield{FirstValidTime} @snippet{bad}
        4 @txnfield{LastValid} @snippet{yes}
        5 @txnfield{Note} @snippet{probably not}
        6 @txnfield{Lease} @snippet{probably not}
        7 @txnfield{Receiver} @snippet{no, because, as is the case with many of these, the execution should be parameterized over the value}
        8 @txnfield{Amount} @snippet{probably}
        9 @txnfield{CloseRemainderTo} @snippet{}
        10 @txnfield{} @snippet{}
        11 @txnfield{} @snippet{}
        12 @txnfield{} @snippet{}
        13 @txnfield{} @snippet{}
        )


The unconstrained property (not parameter) analysis identifies
properties which the contract allows to vary and, in some cases,
the conditions under which it does so.

It is achieved by a series of increasingly sophisticated, precise, 
and costly analyses.

Consider this cheap analysis for the fields OnCompletion and 
RekeyTo in particular.
The analyzer interprets all arithmetic/logic instructions
symbolically, so that the stack contains symbolic expressions.
When a branch is encountered, the symbolic expression serving 
as the guard is scanned for an occurrence of OnCompletion or 
RekeyTo.
If one is found, the symbolic expression is interpreted---
matched against one of several simple known forms---to refine
the abstract value of the field in the machine state.

For example, when an ApprovalProgram runs, the abstract value 
of the OnCompletion property is { 0, 1, 2, 4, 5 }.
Suppose the program contains the sequence

txn OnCompletion
push 1
==
txn OnCompletion
push 2
==
||
txn OnCompletion
push 4
==
||
txn OnCompletion
push 5
||
bz good
err
good:
...

Upon encountering the `bz` instruction, the symbolic expression is

(∨ (∨ (∨ (= OnCompletion 1)
         (= OnCompletion 2))
      (= OnCompletion 4))
   (= OnCompletion 5))

Regardless of whether OnCompletion or RekeyTo appears in the expression,
the analyzer proceeds to decide whether it can be zero and nonzero, first
by processing negations, conjunctions, and disjunctions.

To decide whether it can be nonzero is to `assume` it.
When `assume` encounters a disjunction, it splits the world, assuming in
turn each side of the disjunction.
Recurring, the analyzer will split into four parts.
To `assume` (= OnCompletion x) is to constrain the OnCompletion abstract
value to be its current value intersected with { x }.
If the result is the empty set, then the assumption is false.
If the result is not the empty set, then the assumption can be satisfied
with the non-empty set as the newly-assumed value.
Each of these four cases leads to the contract executing `err`, 
an unsuccessful execution of the program.

To decide whether it can be zero is to `refute` it, or to `assume` its
negation.
`assume` implements that `(¬ (∨ A B))` ≡ `(∧ (¬ A) (¬ B))` which,
recurring, yields

(∧ (∧ (∧ (¬ (= OnCompletion 1))
         (¬ (= OnCompletion 2)))
      (¬ (= OnCompletion 4)))
   (¬ (= OnCompletion 5)))

A conjunction is handled by processing each side in turn, on the same path.
The interpretation of `(¬ (= OnCompletion 1))` is to subtract the set { x }
from the current value of OnCompletion, i.e., intersect it with the complement
of { x }.
If the result is the empty set, then it cannot be refuted, and the value cannot
be zero.
In this example, the initial value of { 0, 1, 2, 4, 5 } becomes { 0, 2, 4, 5 },
{ 0, 4, 5 }, { 0, 5 }, and then { 0 }, before continuing at label `good`.

The OnCompletion interpretation is dispatched only once the negations, 
conjunctions, and disjunctions have been handled.
If the term does not contain the `OnCompletion` property (or another of interest),
then it can be ignored.
The result is a less-precise analysis, but not a less-safe analysis as ignoring
those terms allows the analysis to explore only more of the execution space.

The stack is not the only place that the `OnCompletion` property could conceivably
be stored.
It could also be stored in storage or even global or local storage.
Thus, these features should be soundly approximated.

The suite of programs from scratch will include strange ways of handling these
properties, including each of these.

If an expression contains `OnCompletion` but it isn't recognized by the interpreter,
the analysis fails (and fortunately can report on exactly what it choked).
Alternatively, this term could be stored in a path condition and reported at the
end.

By not storing any terms which do not contain properties of interest in the path 
condition, the path condition is very sparsely populated and therefore doesn't
increase the state space much.
Only the path condition and the values of the properties of interest increase
the state space, so this should be a very brisk analysis.


I made a record type which I've used to represent both datatypes and typeclasses.
I made a sumtype type which gathers together a set of records, defining them if necessary.
A given record can belong to more than one sumtype, but this freedom hasn't proved useful yet.

One question is how to do typeclasses for various definitions.
For example, I could put a #:deriving clause in sumtype definitions which dispatched on all of the constituents and did a show over them.
(I perhaps could record the show implementation statically so that I could defer, for each variant for instance, to the record-specific typeclass instance.)

records can contain sumtypes, but don't depend on them the ways sumtypes depend on records, so it might be worth working it out for records first.
From a record-info static type, one can derive a function to show the datatype in a canonical way:
(match-lambda
  [(<rec-name> <rec-field> ...)
   (pretty-print `(<rec-name> [<rec-field> ,(show <rec-field>)] ...))])

But which `show` is called for each field?
The types, if not the implementations, of each field need to be declared statically so that the static information can keep track of it.

If the `show` of a record type is defined manually, the appropriate `show` for each field can be explicitly given.

Maybe a typeclass should be its own datatype derived from records?



show
read

(record recname [name type])

(record underway [values (Listof Value)] [state State])

(define-sumtype Result
  (underway [values (Listof Value)] [state State])
  (failure [message String])
  (finish [code Number]))

Number entails
a runtime predicate
any number of typeclasses

(define-instance Show Number
  [show (λ (n) (print n))])
