# Monkey Language Interpreter

A **Rust** implementation of the Monkey programming language interpreter, following the "Writing An Interpreter In Go" book by Thorsten Ball.

## Overview

Monkey is a toy programming language interpreter written in Rust. It features a lexer, parser, and evaluator that can process Monkey language code. The project serves as an educational exercise to understand how programming language interpreters work.

There is a **Go** implementation of the Monkey language interpreter [here](https://github.com/rand0m42195/monkey-go).

## Features

### ✅ Implemented
- **Lexer**: Tokenizes Monkey language source code into tokens
- **Parser**: Complete Pratt parser with precedence handling
- **AST Generation**: Full Abstract Syntax Tree construction
- **REPL**: Interactive Read-Eval-Print Loop with customizable I/O
- **Comprehensive Testing**: Unit tests for lexer and parser functionality

### Supported Language Features
- **Expressions**:
  - Integer literals (`123`)
  - Boolean literals (`true`, `false`)
  - Identifiers (`x`, `myVar`)
  - Prefix expressions (`-5`, `!true`)
  - Infix expressions (`5 + 3`, `x == y`, `a < b`)
  - Grouped expressions (`(1 + 2) * 3`)
  - Function literals (`fn(x, y) { x + y }`)
  - Function calls (`add(1, 2)`)
  - If expressions (`if (x < y) { x } else { y }`)

- **Statements**:
  - Let statements (`let x = 5;`)
  - Return statements (`return 42;`)
  - Expression statements (`x + y;`)
  - Block statements (`{ let x = 1; x + 2; }`)

- **Operators**:
  - Arithmetic: `+`, `-`, `*`, `/`
  - Comparison: `==`, `!=`, `<`, `>`
  - Logical: `!`
  - Assignment: `=`

- **Control Flow**:
  - If/else expressions with proper precedence
  - Function definitions and calls
  - Block scoping

## Project Structure

```
src/
├── lexer/           # Lexical analysis
│   ├── lexer.rs     # Main lexer implementation
│   └── mod.rs       # Module exports
├── token/           # Token definitions
│   ├── token.rs     # Token and TokenType definitions
│   └── mod.rs       # Module exports
├── parser/          # Syntax analysis
│   ├── parser.rs    # Pratt parser implementation
│   └── mod.rs       # Module exports
├── ast/             # Abstract Syntax Tree
│   ├── ast.rs       # AST node definitions
│   └── mod.rs       # Module exports
├── repl/            # Read-Eval-Print Loop
│   ├── repl.rs      # REPL implementation
│   └── mod.rs       # Module exports
├── lib.rs           # Library root
└── main.rs          # Binary entry point
```

## Getting Started

### Prerequisites
- Rust 1.70+ (stable)
- Cargo

### Building and Running

```bash
# Clone the repository
git clone <repository-url>
cd rmonkey-language

# Build the project
cargo build

# Run the REPL
cargo run

# Run tests
cargo test
```

### Example Usage

```bash
$ cargo run
Hello! This is the Monkey programming language!
Feel free to type in commands
>> let x = 5;
let x = 5;
>> let y = 10;
let y = 10;
>> x + y * 2;
(x + (y * 2))
>> fn add(a, b) { a + b; }
fn(a, b) {
(a + b)
}
>> add(5, 3);
add(5, 3)
>> if (x > y) { x } else { y };
if ((x > y)) {
x
} else {
y
}
```

## Development Roadmap

### Phase 1: Core Language Features ✅ COMPLETED
- [x] **Parser**: Implement Pratt parser for AST generation
  - [x] Expression parsing (arithmetic, comparison, logical)
  - [x] Statement parsing (let, return, expression statements)
  - [x] Block statement parsing
  - [x] Function declaration parsing
- [x] **AST Nodes**: Define Abstract Syntax Tree node types
  - [x] Expression nodes (identifiers, literals, infix, prefix, call, if, function)
  - [x] Statement nodes (let, return, expression, block)
  - [x] Program root node

### Phase 2: Evaluation Engine
- [ ] **Evaluator**: Implement expression and statement evaluation
  - [ ] Integer arithmetic operations
  - [ ] Boolean operations and comparisons
  - [ ] Variable binding and lookup
  - [ ] Function definition and calling
  - [ ] Conditional evaluation (if/else)
- [ ] **Environment**: Variable scope management
  - [ ] Local and global variable storage
  - [ ] Function closure support

### Phase 3: Advanced Features
- [ ] **Built-in Functions**: Implement standard library
  - [ ] `len()` for string and array length
  - [ ] `first()`, `last()`, `rest()` for array operations
  - [ ] `push()` for array manipulation
  - [ ] `puts()` for output
- [ ] **Data Types**: Extended type system
  - [ ] String literals and operations
  - [ ] Array literals and indexing
  - [ ] Hash/Map data structures
- [ ] **Error Handling**: Comprehensive error reporting
  - [ ] Syntax error messages with line/column info
  - [ ] Runtime error handling
  - [ ] Type error checking

### Phase 4: Language Enhancements
- [ ] **Control Flow**: Advanced control structures
  - [ ] `while` loops
  - [ ] `for` loops
  - [ ] `break` and `continue` statements
- [ ] **Functions**: Advanced function features
  - [ ] Higher-order functions
  - [ ] Closures with proper variable capture
  - [ ] Recursive function calls
- [ ] **Modules**: Code organization
  - [ ] Import/export system
  - [ ] Module namespace management

### Phase 5: Tooling and Optimization
- [ ] **Performance**: Interpreter optimization
  - [ ] Bytecode compilation
  - [ ] Virtual machine implementation
  - [ ] Garbage collection
- [ ] **Developer Experience**: Enhanced tooling
  - [ ] Syntax highlighting
  - [ ] Debugger support
  - [ ] Language server protocol (LSP)
- [ ] **Documentation**: Comprehensive docs
  - [ ] Language specification
  - [ ] API documentation
  - [ ] Tutorial and examples

## Testing

The project includes comprehensive test suites:

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test module
cargo test lexer::tests
cargo test parser::tests
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Learning Resources

This project is based on the excellent book "Writing An Interpreter In Go" by Thorsten Ball. The original Go implementation can be found at [monkey-lang](https://github.com/thorstenball/monkey-lang).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Thorsten Ball for the original Monkey language design and implementation
- The Rust community for excellent tooling and documentation
- Contributors and testers who help improve the project
