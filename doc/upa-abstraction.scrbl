#lang scribble/base

@title{UPA Abstraction}

an abstract interpretation depends on the choice of a particular abstraction which characterizes the analysis generally.
this abstraction significantly influences the speed and precision of the analysis too, so choosing a suitable one is important.

the question here is which abstraction to use for @secref["upa"].
I have two possibilities in mind:

first, an explicit model of transaction properties as a record.
each field begins unconstrained and is refined as execution imposes constraint on it.
for instance, the @field{RekeyTo} field begins as an arbitrary bytes value.
it may pass through a branch on @expression{RekeyTo == ZeroAddress} in which case we refine its value in each branch and continue execution.
the refinement may express a proposition it satisfies explicitly, such as phi(x) = x == ZeroAddress, or it may simply reflect that proposition in its value, unifying it with ZeroAddress.
not all propositions can be reflected, however, such as phi(x) = x != ZeroAddress which is accrued on the other branch.

the second possibility is a set of constraints over fields.
+ many transaction fields are actually derived
+ can express logical relationships between fields
In this framework, the approximation of the analysis seems directly tied to the power of the prover.
If a particular set of constraints actually entails a condition but the prover can't determine it, then we will arrive at an inconsistent set of constraints.
inconsistency doesn't seem to be fatal to the soundness of the analysis, but instead the precision, as it opens the door for anything to happen.
however, if the prover isn't powerful enough to prove A and therefore admits ~A, it may be that nothing helps it prove A later so that it can't get a juxtaposition of A and ~A.
[there is something to work out here, because this argument implies that a weaker prover is more precise, which can't be true.]
a set of constraints must also be finitized somehow.