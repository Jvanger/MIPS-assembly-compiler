# Bach Compiler

## Overview
This project implements a compiler for the Bach programming language, which compiles Bach source code into MIPS assembly language. The compiler performs lexical analysis, parsing, name analysis, type checking, and code generation for Bach programs.

## Bach Language Features
Bach is a simple programming language with the following features:
- Integer and boolean primitive types
- Local and global variables
- Functions with parameters and return values
- Control structures (if, if-else, while)
- Input/output operations
- Arithmetic and logical operations
- Struct support for complex data types

## Project Components
The compiler is organized into several components:

1. **Scanner**: Performs lexical analysis to convert input text into tokens
2. **Parser**: Constructs an Abstract Syntax Tree (AST) from tokens
3. **Name Analyzer**: Resolves variable and function names and constructs symbol tables
4. **Type Checker**: Ensures type correctness in expressions and statements
5. **Code Generator**: Transforms the AST into MIPS assembly code

## Implementation Details

### Abstract Syntax Tree (AST)
The compiler uses an object-oriented approach to represent Bach programs as Abstract Syntax Trees. The AST classes include:
- ProgramNode, DeclListNode, StmtListNode, etc. for program structure
- TypeNode subclasses (IntegerNode, BooleanNode, VoidNode, etc.)
- ExpNode subclasses for expressions (arithmetic, logical, function calls)
- StmtNode subclasses for statements (assignments, control flow, I/O)

### Symbol Tables
Symbol tables track variable and function declarations, their types, and their storage locations. They enforce scope rules and detect name errors such as duplicate declarations or undeclared identifiers.

### Code Generation
The code generator transforms the validated AST into MIPS assembly code:
- Implements function calling conventions
- Manages stack frames for local variables and parameters
- Handles arithmetic and logical operations
- Implements control structures with conditional branching
- Provides I/O through system calls

## Building and Running

### Prerequisites
- Java Development Kit (JDK)
- MIPS simulator (e.g., SPIM or MARS)

### Compilation
```bash
# Compile the compiler
javac *.java

# Run the compiler on a Bach program
java Compiler myprogram.bach
```

### Running Generated Code
```bash
# Run the generated assembly code
spim -file myprogram.s
```

## Example Bach Program

```
integer minus[integer x, integer y] [
  integer m.
  m = x - y.
  return m.
]

void main[] [
  integer a.
  integer b.
  integer c.
  a = 7.
  b = a.
  c = a + b.
  disp <- (a * b).
  disp <- (c + a).
  c = c * 2.
  disp <- (c).
  b = minus(c, a).
  disp <- (b).
]
```

Expected output:
```
49
21
28
21
```

## Troubleshooting

### Common Errors
- **Unaligned address error**: Typically indicates a problem with stack management, function calls, or assignment operations.
- **Arithmetic overflow**: May require using MIPS `mult`/`div` with `mflo` instead of pseudo-instructions.
- **Function calling issues**: Check parameter passing and stack cleanup in function calls.

### Debugging Tips
- Use code comments in the generated assembly to track operations
- Add verbose output to show stack and register values
- Trace execution step-by-step in the SPIM simulator

## Project Structure
- `ast.java`: Abstract Syntax Tree classes
- `Codegen.java`: Code generation utilities
- `Compiler.java`: Main compiler driver
- `ErrMsg.java`: Error message handling
- `Sym.java`: Symbol table implementation
- (Other lexical analysis and parsing files)

## Acknowledgments
This compiler was developed as part of a compiler construction course, building on concepts from modern compiler design and implementation.
