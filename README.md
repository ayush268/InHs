# InHs

Interpreter in Haskell.

Assignment-2: Principles of Programming Languages (CS350A), IITK

### AST Specification

| __Oz Syntax__ | __Problem Statement__ | __InHs Specification__ |
|-------------|------------|------------|
| skip | [nop] | Types.Skip |
| skip skip | [[nop] [nop]] | Types.Multiple [Types.Skip, Types.Skip] |
| local \<X\> in \<s\> end | [var ident(x) s] | Types.Var {Types.Identifier Types.Statement} |
