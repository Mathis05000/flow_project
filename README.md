### Mathis Eynaud
### Ishaac Ourahou

# Final Project of host matching problem.

### Template of input file 

```bash
%% hosts
%% h name , capacity [list of constraints (what they have at home) ]

h Julie , 8 dogs cats
h Wissal , 3 stairs
h Thomas , 5 smoke dogs
...

%% guests
%% g name , [list of constraints (what they can't stand) ]
g Ishaac , dogs
g Mathis , smoke
g Marie , stairs cats
g Tom , dogs cats smoke
...
```

### Template of output file

```bash
Ishaac dors chez Wissal
Mathis dors chez Julie
Marie dors chez Thomas
Tom dors chez Wissal
...
```
To use, you should install the *OCaml* extension in VSCode. Other extensions might work as well but make sure there is only one installed.
Then open VSCode in the root directory of this repository (command line: `code path/to/ocaml-maxflow-project`).

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - automatic indentation on file save


A makefile provides some useful commands:
 - `make build` to compile. This creates an ftest.native executable
 - `make demo` to run the `ftest` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts
 
### To execute the program

```bash
make build
```

```bash
./ftest.native in/template out/result 
```

where file in/template is your input file and file out/result is your output file

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).

