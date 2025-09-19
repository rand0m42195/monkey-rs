use crate::{eval, lexer::Lexer, object, parser::Parser};
use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc;

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    writeln!(writer, "Hello! This is the Monkey programming language!").unwrap();
    writeln!(writer, "Feel free to type in commands").unwrap();

    // initial global environment
    let env = Rc::new(RefCell::new(object::Environment::new(None)));
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
                let lexer = Lexer::new(input);
                let mut parser = Parser::new(lexer);
                let program = parser.parse_program();
                if let Err(err) = program {
                    writeln!(writer, "parse error: {}", err).unwrap();
                    continue;
                }

                let res = eval::eval(program.unwrap(), env.clone()).unwrap();

                writeln!(writer, "{}", res).unwrap();
                writer.flush().unwrap();
            }
            Err(error) => {
                writeln!(writer, "Error reading input: {}", error).unwrap();
                break;
            }
        }
    }
}
