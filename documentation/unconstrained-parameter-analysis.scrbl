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
