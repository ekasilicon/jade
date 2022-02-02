#lang scribble/base
@(require scribble/manual)

@title{Jade: A TEAL static analysis tool}

Jade is a static analysis tool for TEAL bytecode.

Jade performs @seclink["unconstrained-property-analysis"]{unconstrained property analysis} to determine which transaction properties the TEAL program allows to freely vary.
Over time, Jade will accrue more (and more in-depth) analyses.

Jade is @deftech{fully-automatic}, meaning that a user can analyze a target program simply by invoking Jade on it.
Users can refine the execution environment (via command-line arguments) that Jade models to obtain a more precise analysis.
In the future, Jade will feature some @deftech{semi-automatic} analysis which, with the help of expert guidance, can establish deeper properties about target programs.

@section[#:tag "installation"]{Installation}

Jade is written in @hyperlink["https://racket-lang.org"]{Racket}.

In a clone of the @link{Jade repository}, you can invoke Jade using XXX.

Alternatively, if you have Docker installed, you can run the Docker file present at the repository top level.

@section[#:tag "unconstrained-property-analysis"]{Unconstrained Property Analysis}

@(define txnfield tt)

A TEAL program executes in a transaction environment.
The values of properties within this environment can (and typically do) affect program execution and post-execution actions taken by the virtual machine.
Unless a program protects itself, a malicious user can craft an execution environment which can devastate the function of the DApp a particular smart contract supports, including the protection it offers to user resources.

Jade verifies that a program protects itself in all cases from a particular set of general attacks.
For example, Jade verifies that a program constrains the @txnfield{OnCompletion} property, which dictates the contract-global action to be taken post-execution (including replacement or deletion of the executed contract).
