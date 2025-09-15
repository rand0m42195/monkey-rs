use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN,   // '='
    PLUS,     // '+'
    MINUS,    // '-'
    ASTERISK, // '*'
    SLASH,    // '/'

    EQ,    // '='
    NotEq, // '!='
    LT,    // '<'
    GT,    // '>'

    COMMA,     // ','
    COLON,     // ':'
    SEMICOLON, // ';'

    LPAREN,   // '('
    RPAREN,   // ')'
    LBRACKET, // '['
    RBRACKET, // ']'
    LBRACE,   // '{'
    RBRACE,   // '}'
    BANG,     // '!'

    FUNCTION, // "fn"
    LET,      // 'let'
    TRUE,     // 'true'
    FALSE,    // 'false'
    IF,       // 'if'
    ELSE,     // 'else'
    RETURN,   // 'return'

              // ...
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    typ: TokenType,
    literal: String,
}

impl Token {
    pub fn new(typ: TokenType, literal: String) -> Self {
        Self { typ, literal }
    }

    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }

    pub fn typ(&self) -> TokenType {
        self.typ
    }

    pub fn literal(&self) -> &String {
        &self.literal
    }
}
