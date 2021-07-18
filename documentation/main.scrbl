#lang scribble/base

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
