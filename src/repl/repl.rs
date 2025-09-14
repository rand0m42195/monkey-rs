use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{BufRead, Write};

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    writeln!(writer, "Hello! This is the Monkey programming language!").unwrap();
    writeln!(writer, "Feel free to type in commands").unwrap();

    loop {
        write!(writer, ">> ").unwrap();
        writer.flush().unwrap();

        let mut input = String::new();
        match reader.read_line(&mut input) {
            Ok(0) => break, // EOF
            Ok(_) => {
                let input = input.trim();
                if input.is_empty() {
                    continue;
                }

                // Create lexer and tokenize the input
                let mut lexer = Lexer::new(input);

                // Print all tokens
                loop {
                    let token = lexer.next_token();
                    writeln!(writer, "{:?}", token).unwrap();

                    if token.typ() == TokenType::EOF {
                        break;
                    }
                }
            }
            Err(error) => {
                writeln!(writer, "Error reading input: {}", error).unwrap();
                break;
            }
        }
    }
}
