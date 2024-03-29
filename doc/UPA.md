# Unconstrained Property Analysis

Execution of a TEAL program occurs within an environment of property--value associations.
These properties can be queried by the running program and several affect the execution or post-execution action taken by the ledger.

For any given contract, many values that would be associated with a particular property are undesirable.
The contract should query such properties and guard execution from undesirable values, thus constraining the values such properties can take in a successful execution.

The *Unconstrained Property Analysis* (UPA) determines whether and how a given contract constrains particularly sensitive properties.

UPA works by defining an abstract machine which reproduces the behavior of the Algorand client.
This abstract machines serves as the ground truth of execution, and is expressed in a formal framework that is particularly responsive to formal manipulation.

UPA is conceptually derived from such a machine by following the *Abstracting Abstract Machines* methodology for producing static analyses.

Users can provide ledger data to constrain the initial execution environment, such as initialization parameters, global program state, etc.
Many important properties, such as user input, remain unconstrained in the initial execution environment which is crucial for UPA to make a sufficiently broad conclusion about program behavior.
UPA refines the initial execution environment by executing the TEAL program within it.
Typically, many points of execution control within a single execution will depend on unknown values (unknown Because the environment is not fully-specified).
In these cases, UPA conservatively explores each path that branches from that point.
By practicing this policy at each such point, UPA explores all feasible execution paths within the program, with respect to the initial execution environment.

Internally, UPA can be instantiated as a path-sensitive or path-insensitive analysis.
When path-sensitive, UPA respects the path that was taken to arrive at the current execution point, which reflects the environment constraints necessary to traverse such a path to the current execution point.
When path-insensitive, UPA records only the constraints necessary for execution to be at the current point on any path.
The latter is typically much more scalable than the former, though the former is typically much more precise than the latter.
Both are conservative, however, and consider all possible execution paths.

## The `OnCompletion` Property

We will consider the `OnCompletion` property as a concrete example.

The `OnCompletion` property determines what action the client takes with respect to the ledger at the conclusion of TEAL program execution.
These actions include doing nothing, managing registration with the target program, and modifying or deleting the program from the ledger.
If the TEAL program does not otherwise protect itself, a user can arbitrarily disrupt or hijack the DApps supported by the program by executing a transaction with a properly chosen `OnCompletion` property.

Consider this cheap analysis for the fields `OnCompletion` and 
`RekeyTo` in particular.
The analyzer interprets all arithmetic/logic instructions
symbolically, so that the stack contains symbolic expressions.
When a branch is encountered, the symbolic expression serving 
as the guard is scanned for an occurrence of OnCompletion or 
RekeyTo.
If one is found, the symbolic expression is interpreted---
matched against one of several simple known forms---to refine
the abstract value of the field in the machine state.

For example, when an ApprovalProgram runs, the abstract value 
of the OnCompletion property is `{ 0, 1, 2, 4, 5 }`.
Suppose the program contains the sequence

```
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
```

Upon encountering the `bz` instruction, the symbolic expression is

`OnCompletion = 1 or OnCompletion = 2 or OnCompletion = 4 or OnCompletion = 5`


Regardless of whether `OnCompletion` or `RekeyTo` appears in the expression,
the analyzer proceeds to decide whether it can be zero and nonzero, first
by processing negations, conjunctions, and disjunctions.

To decide whether it can be nonzero is to `assume` it.
When `assume` encounters a disjunction, it splits the world, assuming in
turn each side of the disjunction.
Recurring, the analyzer will split into four parts.
To `assume` `OnCompletion = X` is to constrain the `OnCompletion` abstract
value to be its current value intersected with `{ X }`.
If the result is the empty set, then the assumption is false.
If the result is not the empty set, then the assumption can be satisfied
with the non-empty set as the newly-assumed value.
Each of these four cases leads to the contract executing `err`, 
an unsuccessful execution of the program.

To decide whether it can be zero is to `refute` it, or to `assume` its
negation.
`assume` implements that `not (A and B) = not A or not B` which,
recurring, yields

`not OnCompletion = 1 and not OnCompletion = 2 and not OnCompletion = 4 and not OnCompletion = 5`


A conjunction is handled by processing each side in turn, on the same path.
The interpretation of `not OnCompletion = X` is to subtract the set `{ X }`
from the current value of `OnCompletion`, i.e., intersect it with the complement
of `{ X }`.
If the result is the empty set, then it cannot be refuted, and the value cannot
be zero.
In this example, the initial value of `{ 0, 1, 2, 4, 5 }` becomes `{ 0, 2, 4, 5 }`, `{ 0, 4, 5 }`, `{ 0, 5 }`, and then `{ 0 }`, before continuing at label `good`.

The `OnCompletion` interpretation is dispatched only once the negations, 
conjunctions, and disjunctions have been handled.
If the term does not contain the `OnCompletion` property (or another of interest),
then it can be ignored.
The result is a less-precise analysis, but not a less-safe analysis as ignoring
those terms allows the analysis to explore only more of the execution space.

The stack is not the only place that the `OnCompletion` property could conceivably be stored.
It could also be stored in storage or even global or local storage.
Thus, these features should be (and are) soundly approximated.
