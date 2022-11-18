# jade

jade is a TEAL program analyzer built on an extensible semantic framework so that new analyzers, visualizers, and other tools can be rapidly constructed.

jade currently provides

- a typechecker for TEAL version <= 8, and
- the *Unconstrained Parameter Analysis* for TEAL version <= 3, which checks whether the program sensibly constrains the `OnCompletion` and `RekeyTo` transaction fields.

The semantic framework has comprehensive support for TEALv8 and lower.

## Installation

jade's only dependency is an installation of [Racket](https://download.racket-lang.org), version 8.2 or greater.
There is no obvious reason that earlier versions (within the last few years) should not work; please let us know if you run into problems with a specific version or platform.

Jade can be run using `./jade`, after installing the appropriate dependencies as follows:

```bash
raco pkg install --deps search-auto compiler-lib
```

The `Dockerfile` in the repository is configured to obtain Racket and run jade.
The included script [build.sh](build.sh) uses Docker to build jade and [run.sh](run.sh) runs the newly-built jade, passing arguments from the command line.


## Usage

jade is designed to be self-documenting insofar as possible.
Running jade without any arguments will print instructions.

In brief, jade ingests
TEALv8 program bytecode,
TEALv8 program assembly, or
a JSON package produced by the Algorand Indexer v2 (as implemented by [AlgoExplorer](https://algoexplorer.io), for example)
and analyzes it.
After analysis, jade prints a report which includes program disassembly and a result summary of the *Unconstrained Property Analysis*.

jade allows you to specify symbolic names for hard-coded constants so that the program can be treated as a template.
This functionality allows one to apply analysis results to all programs instantiated from that template, as opposed to a single, specific program.

### Example

To download and analyze the JSON package of an application with `<application-id>`, use

```
curl https://algoindexer.algoexplorerapi.io/v2/applications/<application-id> | ./jade --json-package
```

By default, jade will print the disassembled code for inspection and run the typechecker.
Options to suppress printing of the disassembled code and running the typechecker, as well as an option to invoke UPA, are available and documented.
Use `./jade --help` to see usage instructions and details about the available analyses.

