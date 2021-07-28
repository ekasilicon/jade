#lang scribble/text

The concrete interpreter can implement subroutines with a call stack.
The @instr{callsub} instruction reads the offset, pushes the PC (which has been modified by the read operation), and jumps by the offset.
The @instr{retsub} instruction pops the top address from the call stack and goes to it.
(The @instr{callsub} instruction is in terms of an offset; the @instr{retsub} instruction is in terms of an absolute address.)

``The call stack is separate from the data stack.
Only @instr{callsub} and @instr{retsub} manipulate it.''
Accordingly, which data stack values which serve as arguments to the subroutine call (if any) is implicit.
We will have to look at some real-world code patterns to see what it looks like.
My concern is that a recursive subroutine might add arguments each call, only to systematically pop them off at the end (being careful to swap as necessary to preserve the result) and so each invocation will have a different size of stack.

What I want instead is to evaluate a subroutine call with a sort of virtual stack with indeterminate extent.
When the stack is accessed, I instantiate the invocation with the actual callers' stacks, up to the point that it was accessed.
(The data flow might influence the stack activity, so I don't just evaluate with a virtual stack the whole way through.)


     push 10
     callsub fact
     return
fact:
	dup
	push 0
	==
	bz body
	pop
	push 1
	retsub
body:
	dup
	push 1
	-
	callsub fact
	*
	retsub

when fact is entered, we have stacks [10], [9,10], [8,9,10], ... , [0,1,...,10]
even if I abstract the value domain, I still have differently-sized stacks
what I want is to enter fact like this:
stack is a logic variable
dup
dup assumes that the stack has form
x::stack'
and continues with stack
x::x::stack'
(since we're not considering the value of the operand, we can treat it parametrically)
push 0 pushes an actual literal
0::x::x::stack'
== actually considers the value, so we instantiate
we save this point so that any caller of fact is instantiated here and picks up
0::num::num::stack'
we end up with two threads
0::num::stack'
and
1::num::stack'
(maybe we can have a lazy boolean that doesn't split until it's discriminated)
(and unify because we now know num is 0)
bz
num::stack'
pop
stack'
push 1
1::stack'
other branch
num::stack'
dup
num::num::stack'
push 1
1::num::num::stack'
-
num::num::stack'
callsub fact
1::num::stack'
num::stack'
other return
num::num::stack'
num::stack'