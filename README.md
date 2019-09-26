# InHs

Interpreter in Haskell.

Assignment-2: Principles of Programming Languages (CS350A), IITK

### AST Specification

| __Oz Syntax__ | __Problem Statement__ | __InHs Specification__ |
|-------------|------------|------------|
| skip | [nop] | Types.Skip |
| skip skip | [[nop] [nop]] | Types.Multiple [Types.Skip, Types.Skip] |
| local \<X\> in \<s\> end | [var ident(x) s] | Types.Var { Types.Identifier Types.Statement } |
| x = \<v\> | [bind ident(x) literal(v)] | Types.BindValue { Types.Identifier Types.ValuesRead } |
| x = y | [bind ident(x) ident(v)] | Types.BindIdent { Types.Identifier Types.Identifier } |
| if \<x\> then \<s\>1 else \<s\>2 end | [conditional ident(x) s1 s2] | Types.Conditional { Types.Identifier Types.Statement Types.Statement } |
| case \<x\> of \<p1\> then \<s\>1 else \<s\>2 end | [match ident(x) p1 s1 s2] | Types.Match { Types.Identifier Types.Value Types.Statement Types.Statement } |
| {F X1 ... Xn} | [apply ident(f) ident(x1) ... ident(xn)] | Types.Apply { Types.Identifier [Types.Identifier] } |

### Build Instructions
