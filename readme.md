# trnsys-syntax

AST (Abstract Syntax Tree) for TRNSYS input files. 

Currently, it only supports the TRNSYS 18 syntax.

## What is this?
This is a package that provides an AST for TRNSYS input files, allowing you to parse, 
manipulate, and analyze TRNSYS input files programmatically. 
It is designed to be used as rust or python package and can be extended to 
support additional features such as validation, output generation, and more.

# Roadmap
- [x] TRNSYS 18 Read Support
- [x] Tests
- [ ] Validation of references and expressions
- [ ] Output Generation & Create from Zero
- [ ] Benchmarks
- [ ] Python Binding

# Known Issues
- Constants are not fully supported as parameter, e.g. for `NAN_CHECK`