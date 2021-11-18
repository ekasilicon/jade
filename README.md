# jade

jade is a static analysis platform and suite for TEAL programs.

jade currently provides an initial analysis for TEALv3, the *Unconstrained Parameter Analysis*, which checks whether the program sensibly constrains the `OnCompletion` and `RekeyTo` transaction fields.

## Installation

jade's only dependency is an installation of [Racket](https://download.racket-lang.org), version 8.2 or greater.
There is no obvious reason that earlier versions (within the last few years) should not work; please let us know if you run into problems with a specific version or platform.

The `Dockerfile` in the repository is configured to obtain Racket and run jade.

## Usage

jade is designed to be self-documenting insofar as possible.
Running jade without any arguments will print instructions.

In brief, jade expects TEALv3 program bytecode on standard input, either as raw bytes or within a JSON package produced by the Algorand API v2 (as implemented by the AlgoExplorer, for example).
jade will analyze the program and print a report which includes its disassembly and a result summary of the *Unconstrained Parameter Analysis*.

jade allows you to specify symbolic names for hard-coded constants so that the program can be treated as a template.
This functionality allows one to apply analysis results to all programs instantiated from that template, as opposed to a single, specific program.

## Roadmap

We are working to enhance jade in the following four ways:

1. Improve the *Unconstrained Parameter Analysis* to support all transaction parameters and support TEAL assembly as input.

2. Introduce additional generic analyses to the suite---generic in the sense that they apply to programs generally and do not verify program-specific behaviors.

3. Enhance the analyses to work on programs written in TEALv4 and TEALv5.

4. Improve the analyzer API to make it easier to build on the platform.

