# trnsys-syntax

AST (Abstract Syntax Tree) for TRNSYS input files. 

Currently, it only supports the TRNSYS 18 syntax.

# Roadmap
- [x] TRNSYS 18 Read Support
- [x] Tests
- [ ] Validation of references and expressions
- [ ] Output Generation & Create from Zero
- [ ] Benchmarks
- [ ] Python Binding

# Known Issues
- Constants are not fully supported as parameter, e.g. for `NAN_CHECK`