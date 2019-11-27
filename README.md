# InHs

An implementation of Oz Interpreter in Haskell.

This Interpreter was initially developed as part of an [Assignment 2](https://www.cse.iitk.ac.in/users/satyadev/au19/hw2.pdf) and [Assignment 3](https://www.cse.iitk.ac.in/users/satyadev/au19/hw3.html) for the Course - Principles of Programming Languages ([CS350A](https://cse.iitk.ac.in/pages/CS350.html)), IITK

---

## How to Use?

The interpreter is a developed using [Stack](https://docs.haskellstack.org/en/stable/README/) (This is the only dependency to build the project).

``` bash
 > make             # Builds the project, installs binaries appropriately
```

The project also has a comprehensive [test-suite](./test), covering all the cases as described in the problem statement.

To run the tests:

``` bash
 > make test
```
This also catches the exception for the intentionally failing tests such as *unification failure*, *record arity mismatch*, *variable out of scope* etc.



Please refer to [Type Specification](#type-specification) and [Examples](#examples) sections to get an idea of how to give the binary an input program to execute.

---

## Type Specification

The types have been defined so as to distinguish various statements of the kernel language, and also to allow converting between user input and storage form in Single Assignment Store.

* **Statement** Type (Set of valid statements which can be part of a program)

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
                          pattern :: ValueRead,
                          fststmt :: Statement,
                          sndstmt :: Statement}
                 | Apply {func       :: Identifier,
                          parameters :: [Identifier]}
                 | Thread {stmt :: Statement}
                 | ByNeed {dest :: Identifier,
                           value :: ValuesRead} deriving (Eq, Show, Read)
```

* **StackState** Type (Ready, Suspended, Completed), for Concurreny

``` Haskell
data StackState = Ready | Suspended | Completed deriving (Eq, Ord, Show, Read)
```

* **Value** Type (Literal, Record, Closures)

``` Haskell
data Value = Liter {litVal :: Literal}
             | Closure {procParameters :: [Identifier],
                        procStmt       :: Statement,
                        procEnv        :: EnvironmentMap}
             | Rec {recLabel  :: Literal,
                    recValues :: FeatureMap} deriving (Show, Read)
```
`Value` type describes how values (data) is stored in the Single Assignment Store, the SAS can only contain data of types Literals (integers), Records or Closures (procedures).

- **ValuesRead** Type (Literal, Record, Closures), **Expressions** and **Operators**.
``` Haskell
data ValuesRead = Expr {expr :: Expression}
                  | Proc {params :: [Identifier],
                          pStmt   :: Statement}
                  | Record {label  :: Literal,
                            values :: ReadFeatureMap} deriving (Eq, Show, Read)

data Expression = Lit {val :: Literal}
                  | Variable {expVar :: Identifier}
                  | Exp {operator :: Operator,
                         leftOperand :: Expression,
                         rightOperand :: Expression} deriving (Eq, Show, Read)

data Operator = Add | Sub | Mult | Div deriving (Eq, Show, Read)
```

`ValuesRead` type describes how the values are read from IO. The conversion is done using
`convertValuesReadToValue` function described in `Helpers.hs`.

* **Expressions**: Evaluated and converted to value literals to store in SAS.
* **Records**: Map features to the corresponding values in Single Assignment Store.
* **Closures**: Segregate procedure arguments, procedure statement and free variables.

---

## Examples

- Lazy Program (ByNeed Statement which creates a trigger for a variable and it is activated on encountering a suspended statement), this corresponds to *Positive TestCase 22*.

``` Haskell
Var {dest = "x", stmt = Var {dest = "y", stmt = Multiple {stmts = [ByNeed {dest = "x", value = Proc {params = ["a"], pStmt = BindValue {dest = "a", value = Expr {expr = Lit {val = 0}}}}},Conditional {src = "x", fststmt = BindValue {dest = "y", value = Expr {expr = Lit {val = 1}}}, sndstmt = BindValue {dest = "y", value = Expr {expr = Lit {val = 2}}}}]}}}
```

- Concurrent Program (Thread Statement which binds the variable *alice* for Conditional suspendable statements), this corresponds to *Positive TestCase 19*.

``` Haskell
Var {dest = "alice", stmt = Var {dest = "bob", stmt = Var {dest = "charles", stmt = Multiple {stmts = [Thread {stmt = BindIdent {dest = "charles", src = "alice"}},BindValue {dest = "charles", value = Expr {expr = Lit {val = 0}}},Conditional {src = "alice", fststmt = BindIdent {dest = "bob", src = "alice"}, sndstmt = BindValue {dest = "bob", value = Expr {expr = Lit {val = 100}}}}]}}}}
```

- Evaluating Expressions (Multiplication of 2 Variables, namely *alice*, *bob* and stored in a 3rd variable *charles*), this corresponds to *Positive TestCase 17*.

``` Haskell
Var {dest = "alice", stmt = Var {dest = "bob", stmt = Var {dest = "charles", stmt = Multiple {stmts = [BindValue {dest = "alice", value = Expr {expr = Lit {val = 12}}}, BindValue {dest = "bob", value = Expr {expr = Lit {val = 27}}}, BindValue {dest = "charles", value = Expr {expr = Exp {operator = Mult, leftOperand = Variable {expVar = "alice"}, rightOperand = Variable {expVar = "bob"}}}}]}}}}
```

- Conditional Statement (Taking True branch for non-zero literal value stored in *alice*), this corresponds to *Positive TestCase 10*.

``` Haskell
Var {dest = "bob", stmt = Var {dest = "alice", stmt = Multiple {stmts = [BindValue {dest = "alice", value = Expr {expr = Lit {val = 1}}}, Conditional {src = "alice", fststmt = BindIdent {dest = "alice", src = "bob"}, sndstmt = BindValue {dest = "bob", value = Expr {expr = Lit {val = 100}}}}]}}}
```

- Value binding, binding to a Procedure (with free variables), this corresponds to *Positive TestCase 8*.

``` Haskell
Var {dest = "charles", stmt = Var {dest = "bob", stmt = Var {dest = "alice", stmt = BindValue {dest = "charles", value = Proc {params = ["bob"], pStmt = BindIdent {dest = "alice", src = "bob"}}}}}}
```

- Value binding, binding to a Record, this corresponds to *Positive TestCase 6*.

``` Haskell
Var {dest = "charles", stmt = Var {dest = "bob", stmt = Var {dest = "alice", stmt = BindValue {dest = "alice", value = Record {label = 12, values = fromList [(1,"bob"),(2,"charles")]}}}}}
```

- More examples can be found in the file [Spec.hs](./test/Spec.hs).

---

## Summary of Tests

We have divided the test-suite into 2 classes, namely **Positive** and **Negative** (Should raise an Exception) test cases.
### Positive Test Cases

|S No|__Test Description__|__Result__|
|----|----|----|
|1|Variable declarations (multiple combinations)|Passed|
| 2 | Identifier binding succeeds (both unbound) | Passed |
| 3 | Identifier binding succeeds (one variable unbound and one bound) | Passed |
| 4 | Unification of 3 variables | Passed |
| 5 | Identifier binding succeeds (both bound to equal values)     | Passed |
| 6 | Value binding, binding to a Record | Passed |
| 7 | Value binding, binding to a Procedure (w/o free variables) | Passed |
| 8 | Value binding, binding to a Procedure (with free variables) | Passed |
| 9 | Binding two records succeeds (label, arity, features match, unification succeeds) | Passed |
| 10 | Conditional Statement (Taking True branch for non-zero literal) | Passed |
| 11 | Conditional Statement (Taking False branch for zero literal) | Passed |
| 12 | Match (Case) Statement, case of Pattern match | Passed |
| 13 | Match (Case) Statement, case of Pattern mis-match (else part) | Passed |
| 14 | Procedure Call (Apply Statement) (w/o free variables) | Passed |
| 15 | Procedure Call (Apply Statement) (with free variables) | Passed |
| 16 | Evaluating Expressions (Addition of a Variable and a literal) | Passed |
| 17 | Evaluating Expressions (Multiplication of 2 Variables) | Passed |
| 18 | Thread Statement (without Suspend Case) | Passed |
| 19 | Thread Statement (with Suspension of Conditional Statement) | Passed |
| 20 | Multiple Threads (Depending on one another, suspension goes back and forth) | Passed |
| 21 | ByNeed Statement (trigger activated by a variable being bound) | Passed |
| 22 | ByNeed Statement (trigger activated by a suspendable statement) | Passed |
| 23 | ByNeed Statement (trigger NOT activated) | Passed |
| 24 | Eager Factorial Function (Recursion) | Passed |
| 25 | Lazy Factorial Function (Recursion with Laziness) | Passed |

### Negative Test Cases (Raising Exceptions)
|S No|__Test Description__|__Result__|
|----|----|----|
|1|Identifier binding fails, Variable out of scope|Passed|
|2|Unification fails (due to unequal values of bound variables)|Passed|
|3|Unification fails (Arity mis-match for 2 records)|Passed|
|4|Conditional expression variable is not a literal|Passed|
|5|Pattern in match (case) statement is not a record|Passed|
|6|Invalid Procedure Call (type of variable is not closure)|Passed|
|7|Invalid Procedure Call (Arity of Call and stored Closure don't match)|Passed|
|8|Evaluating Expressions Fails (one operand not bound to a value)|Passed|
|9|Single suspendable Statement Failure Case|Passed|
|10|Multiple Statements Suspended on each other (Conditional, Match and Apply)|Passed|
|11|ByNeed Statement trigger activated and Unification error|Passed|

---

## Developer's Section

The interpreter requires input in AST format. Here is a brief specification of how each statement of Oz syntax is represented in our type specification.

| __Oz Syntax__ | __Problem Statement__ | __InHs Specification__ |
|-------------|------------|------------|
| skip | [nop] | Skip |
| skip skip | [ [nop] [nop] ] | Multiple { stmts = [Skip, Skip] } |
| local \<X\> in \<s\> end | [var ident(x) s] | Var { dest = "x", stmt = `Statement` } |
| x = \<v\> | [bind ident(x) literal(v)] | BindValue { dest = "x", value = `ValuesRead` } |
| x = y | [bind ident(x) ident(v)] | BindIdent { dest = "x", src = "y" } |
| if \<x\> then \<s\>1 else \<s\>2 end | [conditional ident(x) s1 s2] | Conditional { src = "x", fststmt = `Statement`, sndstmt = `Statement` } |
| case \<x\> of \<p1\> then \<s\>1 else \<s\>2 end | [match ident(x) p1 s1 s2] | Match { src = "x", pattern = `Record`, fststmt = `Statement`, sndstmt = `Statement` } |
| {F X1 ... Xn} | [apply ident(f) ident(x1) ... ident(xn)] | Apply { func = "F", parameters = ["X1", ... , "Xn"] } |
| thread \<s\> end | [Thread s] | Thread { stmt = `Statement` } |
| ByNeed \<p\> \<x\> end | [ByNeed p x] | ByNeed { dest = "x", value = `ValuesRead` } |

The Specification for each of the Records and Value types can be found in [Examples](#examples) section.

#### Project Structure

```
├── app
│   └── Main.hs
├── LICENSE
├── Makefile
├── package.yaml
├── README.md
├── Setup.hs
├── src
│   ├── ExecuteProgram.hs
│   ├── Execution.hs
│   ├── Helpers.hs
│   ├── SingleAssignmentStore.hs
│   ├── TriggerStore.hs
│   └── Types.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs
```

* `ExecuteProgram.hs`: Main program. Calls `threadScheduler` which picks a READY stack and calls `executeStack` which defines operations of the abstract machine depending on the type of statement.
* `Execution.hs`: All the functions related to Threading, Context Switch and Execution Semantics of each statement and stack are defined here, they in turn call functions from different modules.
* `Helpers.hs` : Set of helper functions to convert `ValuesRead` type to `Value` type or retrieve values from SAS or free variables from a procedure value and many more.
* `SingleAssignmentStore.hs` : This is where the MAGIC happens, it implements the code for SAS and UNIFICATION Algorithm for literals, records and values.
* `TriggerStore.hs`: This is the where Laziness feature comes from, `ByNeed` statement creates a trigger in this store and it is activated when the variable is really needed.
* `Types.hs` : Defines all the types and typeclasses used in the project. Brief intro
is given in the [Type Specification](#type-specification) section.

---

