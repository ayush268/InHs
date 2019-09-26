# InHs

An implementation of Oz Interpreter in Haskell.

Assignment-2: Principles of Programming Languages (CS350A), IITK

## Build Instructions
The interpreter is a developed using [Stack](https://docs.haskellstack.org/en/stable/README/).

``` bash
 > make setup       # Builds the project, installs binaries appropriately
```

The project also has a comprehensive [test-suite](./test), covering all the cases
as described in the problem statement. To run the tests:

``` bash
 > make test
```
This also catches the exception for the intentionally failing tests. i.e
unification failure, record arity mismatch, variable out of scope etc.

## AST Specification
The interpreter requires code of Oz in AST format. Here is a brief specification
of various Types and Typeclasses used.

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


## List of Tests

|__Test Description__|__Result__|
|----|----|
| Identifier binding succeeds (both unbound) | Passed | 
| Identifier binding succeeds (s1 -> bound; s2 -> unbound) | Passed | 
| Identifier binding succeeds (s1 -> unbound; s2 -> bound) | Passed | 
| Identifier binding fails; Variable out of scope | Passed |
| Bound unification fails | Passed | 
| Bound unification succeeds | Passed | 
| Binding record to a value succeeds | Passed | 
| Binding two records fails (different arity) | Passed | 
| Binding two records succeeds (same arity) | Passed | 
| Collecting free variables from closures | Passed |
| Assign procedure value along with closure | Passed | 


## Project Structure

```
├── app
│   └── Main.hs
├── bin
│   └── InHs-exe
├── Setup.hs
├── src
│   ├── Execution.hs
│   ├── Helpers.hs
│   ├── SingleAssignmentStore.hs
│   └── Types.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs

```

* `Execution.hs`: Main program. Calls `executeStack` which in turn defines operations
of the abstract machine depending on the type of statement.

* `Helpers.hs` : Set of helper functions to convert `ValuesRead` type to `Value` type
and retrieve free variables from a procedure value.

* `SingleAssignmentStore.hs` : Implements the code for SAS types and relevant unification
for literals, records and values.

* `Types.hs` : Defines all the types and typeclasses used in the project. Brief intro
is given below.


## Type Specification
The types have been defined so as to distinguish various statements of the kernel
language, and also to allow converting between user input and storage form in SAS.

* **Statement** Type

``` Haskell
data Statement = Skip
                 | Multiple {stmts :: [Statement]}
                 | Var {dest :: Identifier,
                        stmt :: Statement}
                 | BindIdent {dest :: Identifier,
                              src  :: Identifier}
                 | BindValue {dest  :: Identifier,
                              value :: ValuesRead}
                 | Conditional {src     :: Identifier,
                                fststmt :: Statement,
                                sndstmt :: Statement}
                 | Match {src     :: Identifier,
                          pattern :: Value,
                          fststmt :: Statement,
                          sndstmt :: Statement}
                 | Apply {func       :: Identifier,
                          parameters :: [Identifier]} deriving (Eq, Show, Read)

```

* **Value** and **ValuesRead** Type (Literal, Record, Closures)

``` Haskell
data ValuesRead = Lit {val :: Literal}
                  | Proc {params :: [Identifier],
                          pStmt   :: Statement}
                  | Record {label  :: Literal,
                            values :: ReadFeatureMap} deriving (Eq, Show, Read)

data Value = Liter {litVal :: Literal}
             | Closure {procParameters :: [Identifier],
                        procStmt       :: Statement,
                        procEnv        :: EnvironmentMap}
             | Rec {recLabel  :: Literal,
                    recValues :: FeatureMap} deriving (Show, Read)
```
`ValuesRead` type describes how the values are read from IO, while `Value` type
is how data is stored in SingleAssignmentStore. The conversion is done using
`convertValuesReadToValue` function described in `Helpers.hs`.
* **Literals**: Store the input literals as it is.
* **Records**: Map features to the corresponding values in SingleAssignmentStore
* **Closures**: Segregate procedure arguments, procedure statement and free variables
