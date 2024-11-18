# postscript-interpreter
A interpreter for the Adobe PostScript language written in Haskell.

### Run
`stack run` to run a PostScript REPL.
`stack test` to run the unit tests, integration tests, and snapshot tests.

## Features
- [x] Arithmetic operators
- [x] Stack manipulation operators
- [x] Dictionary operators
- [x] Control operators
- [x] String operators
- [x] Boolean operators
- [x] Switch between Static and Dynamic scoping

## Implementation
PostScript is a stack based language, meaning all operations reference a stack of operands. Because of this, the interpreter can be implemented without any Abstract Syntax Trees. It really consists of only three steps:
1. Lexical analysis: Tokenize the input, check for errors in defining strings and procedures (unmatched brackets or parentheses)
2. Semantic Analysis: Evaluate an operation using the current stack, throw an error on unknown operations
3. Syntax Analysis: Determine if the operation has valid grammar (ie add takes two integers)

Variables are defined on a "dictionary stack" and operands are placed on an "operand stack". The dictionary stack is a stack of dictionaries, where each dictionary is a map of variable names to their values. The operand stack is a stack of operands, where each operand is a value or a reference to a variable in the dictionary stack.

Note that semantic analysis takes place before syntax analysis because of the stack based nature of postscript-- operations come before operands (ie `1 2 add`)

## Dynamic vs Static Scoping
This interpreter has the ability to switch between dynamic and static scoping. Do this by passing the compiler flag USE_STATIC_SCOPING in `stack.yaml`. Dynamic scoping is enabled by default as it is the default in PostScript.

The static scoping implementation stays true to PostScripts stack based nature. Every new scope (some procedure `{...}`) will trigger a new dictionary to be created by merging all the dictionaries in the stack (a closure). Then, any modifications done to this dictionary will not be reflected in the parent scope, giving us static scoping.

See `test/sample/scoping.ps.in`:
```postscript
/outervar 1 def
/func {
    /outervar 2 def
    outervar
} def

func
outervar
```

### Dynamic Scoping Output
<img width="502" alt="Screenshot 2024-11-16 at 6 56 01 PM" src="https://github.com/user-attachments/assets/2fbb369b-8dca-4fb8-ba37-3a227d2143c8">

### Static Scoping Output
<img width="502" alt="Screenshot 2024-11-16 at 6 57 50 PM" src="https://github.com/user-attachments/assets/8e44b66e-6450-466b-b3e5-79fae8aa22f0">



## Testing
This project uses `hunit` for unit, integration and snapshot testing.

Run `stack test` to run all tests. The snapshot tests are located in `test/snapshots/` and are used to test the interpreter against a set of PostScript programs. The output of the interpreter is compared to the expected output in the snapshot file.
