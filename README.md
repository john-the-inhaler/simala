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
- Out of order execution:
  This makes easier to write code, since you can write your goal and gradually
  fill it out until you have a solution without needing to order code.
- two types of multiplying (high-precedence `a b`, low-precedence `a * b`):
  This allows for you to write things in a more idiomatic way.
  you can write code like `2 pi / T` which is the same as `(2 * pi) / T`.
- simple grammar:
  the grammer is simple and very petternistic. Combined with the tight set of
  syntactic elements, this language is incredibly easy to pick up.
- Simple Type System:
  The type system supports Numbers(f64) and functions(`* -> *`). Functions use
  tuple as a way to describe their parameters, but the language doesn't have 
  tuples.
- Type Deduction:
  You won't have to write a single type annotation, the interpreter will
  either generate the correct instructions or will inform you of the error.

hopeful features list (semi-joking)
- Transpilation to various languages:
  For when you need your calculator to run fast (C++), or you need rock hard
  safety (rust), Or you really need that paycheck (Python).


## POA (Plan of Action)
- [ ] lexer
  - [X] Idents, Numbers, Comments
  - [ ] Operators (`+-*/^!¬?`)
  - [ ] Parens (`()`,`[]`,`{}`)
  - [X] Definition (`=`)
  - [ ] seperators (`,:`)
- [ ] parser
    - [ ] Top Level
        - [ ] Definition
            - [X] non-func
            - [ ] func
        - [ ] Goal
    - [ ] Expression
        - [ ] Atom   (idents, numbers, func application)
        - [X] Tight  (`^` and `(...)`)  NOTE: i added (`"-"<tight>` and
                      `"+"<tight>`)
        - [X] Factor (space mult)
        - [X] Term   (`*/`)
        - [X] Phrase (`+-`)
- [ ] checker
    - [ ] detachment (*1)
    - [ ] basic type checking (*4)
    - [ ] existance checking
    - [ ] dependency analysis (*3)
        - [ ] dependency-graph generation (*2)
        - [ ] Acyclic check
        - [ ] dependency aware reorder
- [ ] runtime
    - [ ] interpreter
    - [ ] GC (not needed until closures) 
    - [ ] closures
- [ ] EXTRA
    - [ ] location reporting covering start + end
    - [ ] error messages
    
### notes
1) Currently both the lexer and parser do not make new strings for tokens or
    the AST. They rely on the entire file being in memory. Detachment is the
    process of taking the required strings out of our AST, detaching it from
    the file.
2) The Dependency graph will be implemented as an associativity table or map
    (`Map<Node, Vec<Node>>` sort of thing). This is for simplicity, as graph
    traversal is a pain in the ass.
3) The Entirety of the dependency stuff will have to happen before any of the
    type checking since we want the code in order. We're not doing inference,
    we're doing simple deduction and type-set fuckery.
4) This will use similar type checking to what is used in Python type-checkers
    but with less going on. Basically just typeclasses/traits but hard-wired.
    This is to have the type-checker to be more flexible without excessive
    complexity.
## Future
I've looked into higher precision floating point libraries (it's a calculator
language, people'll want them.), And i've found the `qd` crate which offers a
`Quad` which is 256-bit (making it an octuple precision float, assuming single
is 32-bit). I As of `2023-07-15` I'm planning on using `f64` because it's just
plenty.
