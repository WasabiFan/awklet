# awklet: a little awk

<img align="right" caption="auklet" width="200" src="https://upload.wikimedia.org/wikipedia/commons/0/00/Leastauklet6.jpg">

awklet is an implementation of the core awk language in pure Rust. It was primarily designed to be
an exercise for myself, and as such it uses no pre-made parsing or lexing libraries; the entirety of
the interpreter is implemented from scratch. That being said, it may be of use to others, and
exposes individual interfaces for lexing, parsing and evaluation.

awklet is thoroughly covered by unit tests and a handful of integration tests.

_To the right: an auklet, one species of auk. Source: Wikipedia._

## Design

awklet is split into three parts: the lexer, parser and evaluator.

### Lexer

Maps an input string to a stream of tokens. It is entirely context free, and rather naive in its
structure. This is largely appropriate for the awk language, with minor exceptions. Notably, there
are existing shortcomings in its ability to discern division from regex literals. This functionality
is implemented as a rather ugly context-dependent hack in the gawk grammar, and I deemed it not
sufficiently important to sacrifice simplicity in awklet.

### Parser

Ingests a token stream to construct an AST. The bulk of this component is a standard
recursive-descent parser, however groups of tokens in expression position are extracted together and
evaluated by a separate operator precedence parser.

The overarching recursive parser is rooted at `src/parser/mod.rs`, while the operator precedence
parser lives in `src/parser/expression_ops.rs`.

### Evaluator

Owns a provided AST, and exposes interfaces for feeding records into that AST's defined rules.
Structurally, the evaluator manages record-level operations and selects actions to execute, while
it defers to the internal `ExecutionEngine` for executing individual statements and managing the
global closure. Unlike the lexer and top-level parser components, the evaluator is stateful and
internally-mutable. The current record, along with active variables, are members of the closure.

## Features

### Included features
- Default, `BEGIN`, `END`, expression, and range patterns.
  - Range patters are still to-do at the evaluator level. Regex are not currently implemented.
- `print`
- Variables and all three core types (numeric, string, and "numeric string")
- `FS`, `OFS`, `RS`, `ORS`, `NF`, `NR`
- Correct behavior of changing fields, and their interop with `NF` and `ORS`
- Most of the language's operators, minus gawk or mawk extensions

### Noteworthy omissions
- There is currently no frontend CLI.
- Most of the standard library functions (all functions aside from `length`)
    - Parser support is present, and implementing these functions would be straightforward.
- Live user input, e.g. `getline`
- User-defined functions
- Control flow constructs, e.g. `if`, `for`
- Piping or IO redirection
- Arrays