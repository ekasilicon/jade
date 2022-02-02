# jade

jade is a static analysis platform and suite for TEAL programs.

jade currently provides an initial analysis for TEALv3, the *Unconstrained Parameter Analysis*, which checks whether the program sensibly constrains the `OnCompletion` and `RekeyTo` transaction fields.

## Installation

jade's only dependency is an installation of [Racket](https://download.racket-lang.org), version 8.2 or greater.
There is no obvious reason that earlier versions (within the last few years) should not work; please let us know if you run into problems with a specific version or platform.

The `Dockerfile` in the repository is configured to obtain Racket and run jade.
The included script [build.sh](build.sh) uses Docker to build jade and [run.sh](run.sh) runs the newly-built jade, passing arguments from the command line.

## Usage

jade is designed to be self-documenting insofar as possible.
Running jade without any arguments will print instructions.

In brief, jade ingests
TEALv3 program bytecode,
TEALv3 program assembly, or
a JSON package produced by the Algorand Indexer v2 (as implemented by [AlgoExplorer](https://algoexplorer.io), for example)
and analyzes it.
After analysis, jade prints a report which includes program disassembly and a result summary of the *Unconstrained Property Analysis*.

jade allows you to specify symbolic names for hard-coded constants so that the program can be treated as a template.
This functionality allows one to apply analysis results to all programs instantiated from that template, as opposed to a single, specific program.

###

Example:

```
curl https://algoindexer.algoexplorerapi.io/v2/applications/<application-id> | jq -cM .application | ./jade --json-package
```

## Roadmap

We are working to enhance jade in the following four ways:

1. Improve the *Unconstrained Parameter Analysis* to support all transaction parameters.

2. Introduce additional generic analyses to the suite---generic in the sense that they apply to programs generally and do not verify program-specific behaviors.

3. Enhance the analyses to work on programs written in TEALv4 and TEALv5.

4. Improve the analyzer API to make it easier to build on the platform.

