use crate::token::{Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let mut tok = Token::new(TokenType::ILLEGAL, "".to_string());

        self.skip_whitespace();

        match self.ch {
            '=' => {
                let ch = self.peek_char();
                tok = if ch == '=' {
                    self.read_char();
                    Token::new(TokenType::EQ, "==".to_string())
                } else {
                    Token::new(TokenType::ASSIGN, "=".to_string())
                };
            }
            ';' => tok = Token::new(TokenType::SEMICOLON, ";".to_string()),
            ',' => tok = Token::new(TokenType::COMMA, ",".to_string()),
            '+' => tok = Token::new(TokenType::PLUS, "+".to_string()),
            '-' => tok = Token::new(TokenType::MINUS, "-".to_string()),
            '!' => {
                let ch = self.peek_char();
                tok = if ch == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_string())
                } else {
                    Token::new(TokenType::BANG, "!".to_string())
                }
            }
            '*' => tok = Token::new(TokenType::ASTERISK, "*".to_string()),
            '/' => tok = Token::new(TokenType::SLASH, "/".to_string()),
            '<' => tok = Token::new(TokenType::LT, "<".to_string()),
            '>' => tok = Token::new(TokenType::GT, ">".to_string()),
            '(' => tok = Token::new(TokenType::LPAREN, "(".to_string()),
            ')' => tok = Token::new(TokenType::RPAREN, ")".to_string()),
            '{' => tok = Token::new(TokenType::LBRACE, "{".to_string()),
            '}' => tok = Token::new(TokenType::RBRACE, "}".to_string()),
            '[' => tok = Token::new(TokenType::LBRACKET, "[".to_string()),
            ']' => tok = Token::new(TokenType::RBRACKET, "]".to_string()),
            ':' => tok = Token::new(TokenType::COLON, ":".to_string()),
            '"' => {
                tok = {
                    let s = self.read_string();
                    Token::new(TokenType::STRING, s)
                }
            }
            '\0' => tok = Token::new(TokenType::EOF, "".to_string()),
            _c if Self::is_letter(self.ch) => {
                let identifier = self.read_identifier();
                tok = Token::new(Token::lookup_ident(&identifier), identifier);
                return tok;
            }
            _d if Self::is_digit(self.ch) => {
                let number = self.read_number();
                tok = Token::new(TokenType::INT, number);
                return tok;
            }
            _ => {}
        }
        self.read_char();
        tok
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while Self::is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn is_digit(ch: char) -> bool {
        ch >= '0' && ch <= '9'
    }

    fn is_letter(ch: char) -> bool {
        (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    fn read_string(&mut self) -> String {
        self.read_char();
        let start = self.position;
        while self.ch != '\0' && self.ch != '"' {
            self.read_char();
        }

        self.input[start..self.position].iter().collect()
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_next_token() {
        let input = "=+(){}";
        let expected_tokens = vec![
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];

        let mut lexer = Lexer::new(input);
        for expected_token in expected_tokens {
            let tok = lexer.next_token();
            assert_eq!(tok, expected_token);
        }
    }

    #[test]
    fn test_next_token_2() {
        // dbg!("test_next_token2");
        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
return true;
} else {
return false;
}

10 == 10;
10 != 9;

"foobar"
"foo bar"
"#;
        let expected_tokens = vec![
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::FUNCTION, "fn".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::IDENT, "x".to_string()),
            Token::new(TokenType::PLUS, "+".to_string()),
            Token::new(TokenType::IDENT, "y".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::LET, "let".to_string()),
            Token::new(TokenType::IDENT, "result".to_string()),
            Token::new(TokenType::ASSIGN, "=".to_string()),
            Token::new(TokenType::IDENT, "add".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::IDENT, "five".to_string()),
            Token::new(TokenType::COMMA, ",".to_string()),
            Token::new(TokenType::IDENT, "ten".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::BANG, "!".to_string()),
            Token::new(TokenType::MINUS, "-".to_string()),
            Token::new(TokenType::SLASH, "/".to_string()),
            Token::new(TokenType::ASTERISK, "*".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::GT, ">".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::IF, "if".to_string()),
            Token::new(TokenType::LPAREN, "(".to_string()),
            Token::new(TokenType::INT, "5".to_string()),
            Token::new(TokenType::LT, "<".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::RPAREN, ")".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::TRUE, "true".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::ELSE, "else".to_string()),
            Token::new(TokenType::LBRACE, "{".to_string()),
            Token::new(TokenType::RETURN, "return".to_string()),
            Token::new(TokenType::FALSE, "false".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::RBRACE, "}".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::EQ, "==".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::INT, "10".to_string()),
            Token::new(TokenType::NotEq, "!=".to_string()),
            Token::new(TokenType::INT, "9".to_string()),
            Token::new(TokenType::SEMICOLON, ";".to_string()),
            Token::new(TokenType::STRING, "foobar".to_string()),
            Token::new(TokenType::STRING, "foo bar".to_string()),
            Token::new(TokenType::EOF, "".to_string()),
        ];
        let mut lexer = Lexer::new(input);
        for expected_token in expected_tokens {
            let tok = lexer.next_token();
            assert_eq!(tok, expected_token);
        }
    }
}
