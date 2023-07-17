# simala
## SImple MAths LAnguage (pronounced like similar, but with more 'a's)
simala is a simple computer language intended to be used as a desktop
calculator. It offers several nicenesses, like out of order execution,
expression based syntax, and various levels of logging. This makes the
calculator simple to use, makes the code re-usable, and explains each step
when evaluating the code.

```simala
// A simple simala script which calculates the area of a square
area = width height;
width = 3;
height = 4;

= area;
```

The language provides most things that you need to do maths. The out of order
execution is done via setting goals and then each identifier is a reference to
another expression. This is then used to construct some DAG like stucture that
allows us to generate a correct order of execution if there isn't any cyclic
dependencies. You can use functions to explicitly handle data passing if you
do end up with recursive data.

### Features

Simala has many neat features. it's not a programming language, meaning we can
have a lot more features that would usually be damaging to the language.

features list (non-exhaustive)
- Out of order execution
  This makes easier to write code, since you can write your goal and gradually
  fill it out until you have a solution without needing to order code.
- two types of multiplying (high-precedence `a b`, low-precedence `a * b`)
  This allows for you to write things in a more idiomatic way.
  you can write code like `2 pi / T` which is the same as `(2 * pi) / T`.
- simple grammar
  the grammer is simple and very petternistic. Combined with the tight set of
  syntactic elements, this language is incredibly easy to pick up.
- Simple Type System
  The type system supports Numbers(f64) and functions(`* -> *`). Functions use
  tuple as a way to describe their parameters, but the language doesn't have 
  tuples.

## POA (Plan of Action)
- [X] lexer
  - [X] Idents, Numbers, Comments
  - [X] Operators (`+-*/^`)
  - [X] Parens
  - [X] Definition (`=`)
- [ ] parser
    - [ ] Top Level
        - [ ] Definition
            - [ ] non-func
            - [ ] func
        - [ ] Goal
    - [ ] Expression
        - [ ] Atom   (idents, numbers, func application)
        - [ ] Tight  (`^` and `(...)`)
        - [ ] Factor (space mult)
        - [ ] Term   (`*/`)
        - [ ] Phrase (`+-`)
- [ ] checker
    - [ ] detachment (*1)
    - [ ] basic type checking
    - [ ] existance checking
    - [ ] dependency-ordering
- [ ] runtime
    - [ ] interpreter
    - [ ] GC (not needed until closures) 
    - [ ] closures
*1: Currently both the lexer and parser do not make new strings for tokens or
    the AST. They rely on the entire file being in memory. Detachment is the
    process of taking the required strings out of our AST, detaching it from
    the file.
## Future
I've looked into higher precision floating point libraries (it's a calculator
language, people'll want them.), And i've found the `qd` crate which offers a
`Quad` which is 256-bit (making it an octuple precision float, assuming single
is 32-bit). I As of `2023-07-15` I'm planning on using `f64` because it's just
plenty.
