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
