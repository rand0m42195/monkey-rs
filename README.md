# Monkey Language Interpreter

A **Rust** implementation of the Monkey programming language interpreter, following the "Writing An Interpreter In Go" book by Thorsten Ball.

## Overview

Monkey is a toy programming language interpreter written in Rust. It features a lexer, parser, and evaluator that can process Monkey language code. The project serves as an educational exercise to understand how programming language interpreters work.

There is a **Go** implementation of the Monkey language interpreter [here](https://github.com/rand0m42195/monkey-go).

## Features

### ✅ Implemented
- **Lexer**: Tokenizes Monkey language source code into tokens
- **REPL**: Interactive Read-Eval-Print Loop with customizable I/O
- **Token Types**: Support for identifiers, integers, operators, keywords, and delimiters
- **Comprehensive Testing**: Unit tests for lexer functionality

### Supported Token Types
- **Identifiers**: `let`, `fn`, `if`, `else`, `return`, `true`, `false`
- **Literals**: Integer numbers, identifiers
- **Operators**: `+`, `-`, `*`, `/`, `==`, `!=`, `<`, `>`, `!`
- **Delimiters**: `(`, `)`, `{`, `}`, `[`, `]`, `;`, `,`, `:`

## Project Structure

```
src/
├── lexer/           # Lexical analysis
│   ├── lexer.rs     # Main lexer implementation
│   └── mod.rs       # Module exports
├── token/           # Token definitions
│   ├── token.rs     # Token and TokenType definitions
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
Token { typ: LET, literal: "let" }
Token { typ: IDENT, literal: "x" }
Token { typ: ASSIGN, literal: "=" }
Token { typ: INT, literal: "5" }
Token { typ: SEMICOLON, literal: ";" }
Token { typ: EOF, literal: "" }
```

## Development Roadmap

### Phase 1: Core Language Features
- [ ] **Parser**: Implement recursive descent parser for AST generation
  - [ ] Expression parsing (arithmetic, comparison, logical)
  - [ ] Statement parsing (let, return, expression statements)
  - [ ] Block statement parsing
  - [ ] Function declaration parsing
- [ ] **AST Nodes**: Define Abstract Syntax Tree node types
  - [ ] Expression nodes (identifiers, literals, infix, prefix, call)
  - [ ] Statement nodes (let, return, expression, block)
  - [ ] Program root node

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
