# jade

jade is a static analysis platform and suite for TEAL programs.

jade currently provides an initial analysis for TEALv3, the *Unconstrained Parameter Analysis*, which checks whether the program sensibly constrains the `OnCompletion` and `RekeyTo` transaction fields.

##Installation

jade's only dependency is an installation of [Racket](https://download.racket-lang.org), version 8.2 or greater.
There is no obvious reason that earlier versions (within the last few years) should not work; please let us know if you run into problems with a specific version or platform.

The `Dockerfile` in the repository is configured to obtain Racket and run jade.

##Roadmap

We are working to enhance jade in the following four ways:

1. Improve the *Unconstrained Parameter Analysis* to support all transaction parameters.

2. Introduce additional generic analyses to the suite---generic in the sense that they apply to programs generally and do not verify program-specific behaviors.

3. Enhance the analyses to work on programs written in TEALv4 and TEALv5.

4. Improve the analyzer API to make it easier to build on the platform.

