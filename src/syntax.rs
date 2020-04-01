use crate::error;
use crate::String;
use crate::intern;
use crate::print_cursor;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    LPARENS,
    RPARENS,
    LBRACE,
    RBRACE,
    COLON,
    COMMA,
    SEMICOLON,
    STAR,
    ARROW,
    ASSIGN,
    FN,
    LET,
    RETURN,
    NAME,
    INTEGER,
    EOF,
}
pub use Token::*;

#[derive(Debug, Clone)]
pub enum Type {
    Name(String),
    Pointer(Box<Type>),
    Func(Box<FuncType>),
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub params: Vec<String>,
    pub ty: FuncType,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Let(String, Option<Type>, Expr),
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Integer(String),
    Name(String),
}

pub struct Parser<'a> {
    pub text: &'a str,
    pub start: usize,
    pub end: usize,
    pub token: Token,
}

impl<'a> Parser<'a> {
    pub fn next(&mut self) {
        self.skip_space();
        self.start = self.end;
        let remaining = self.text[self.end..].as_bytes();
        let (c, d) = match remaining.len() {
            0 => {
                self.token = EOF;
                return;
            }
            1 => (remaining[0] as char, '\0'),
            _ => (remaining[0] as char, remaining[1] as char),
        };

        let (token, n) = match c {
            '-' if d == '>' => (ARROW, 2),
            '(' => (LPARENS, 1),
            ')' => (RPARENS, 1),
            '{' => (LBRACE, 1),
            '}' => (RBRACE, 1),
            '=' => (ASSIGN, 1),
            ',' => (COMMA, 1),
            ';' => (SEMICOLON, 1),
            '*' => (STAR, 1),
            ':' => (COLON, 1),
            _ if c.is_ascii_alphabetic() || c == '_' => {
                let mut n = 0;
                for c in remaining {
                    if !c.is_ascii_alphanumeric() && *c != b'_' {
                        break;
                    }
                    n += 1;
                }
                let token = match &remaining[..n] {
                    b"fn" => FN,
                    b"let" => LET,
                    b"return" => RETURN,
                    _ => NAME,
                };
                (token, n)
            }
            _ if c.is_ascii_digit() => {
                let mut n = 0;
                for c in remaining {
                    if !c.is_ascii_digit() {
                        break;
                    }
                    n += 1;
                }
                (INTEGER, n)
            }
            _ => {
                print_cursor(self.text, self.start, self.start + 1);
                println!("unexpected character {:?}", c);
                error();
            }
        };
        self.token = token;
        self.end += n;
    }

    fn skip_space(&mut self) {
        for b in self.text[self.end..].bytes() {
            if !b.is_ascii_whitespace() {
                break;
            }
            self.end += 1;
        }
    }

    pub fn parse_func(&mut self) -> Func {
        self.parse(FN);

        let name = self.token_string();
        self.parse(NAME);

        let mut params = vec![];
        let param_types = self.parse_list(|p| {
            let name = p.token_string();
            p.parse(NAME);
            p.parse(COLON);
            let ty = p.parse_type();
            params.push(name);
            ty
        }, LPARENS, RPARENS, COMMA);

        self.parse(ARROW);
        let ret = self.parse_type();
        let ty = FuncType {
            params: param_types,
            ret: ret.into(),
        };

        let body = self.parse_block();

        Func {
            name: name,
            params: params,
            ty: ty,
            body: body,
        }
    }

    fn parse_block(&mut self) -> Block {
        let stmts = self.parse_list(|p| {
            p.parse_stmt()
        }, LBRACE, RBRACE, SEMICOLON);

        Block { stmts }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.token {
            LET => {
                self.next();

                let name = self.token_string();
                self.parse(NAME);

                let ty = match self.token {
                    COLON => {
                        self.next();
                        Some(self.parse_type())
                    }
                    _ => None,
                };

                self.parse(ASSIGN);
                let e = self.parse_expr();

                Stmt::Let(name, ty, e)
            }
            RETURN => {
                self.next();
                let e = self.parse_expr();
                Stmt::Return(e)
            }
            _ => {
                print_cursor(self.text, self.start, self.end);
                println!("expected statement");
                error();
            }
        }
    }

    fn parse_expr(&mut self) -> Expr {
        match self.token {
            INTEGER => {
                let s = self.token_string();
                self.next();
                Expr::Integer(s)
            }
            NAME => {
                let name = self.token_string();
                self.next();
                Expr::Name(name)
            }
            _ => {
                print_cursor(self.text, self.start, self.end);
                println!("expected expression");
                error();
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.token {
            NAME => {
                let name = self.token_string();
                self.next();
                Type::Name(name)
            }
            STAR => {
                self.next();
                let ty = self.parse_type();
                Type::Pointer(ty.into())
            }
            _ => {
                print_cursor(self.text, self.start, self.end);
                println!("expected type");
                error();
            }
        }
    }

    fn parse_list<T>(
        &mut self,
        mut parse_elem: impl FnMut(&mut Self) -> T,
        start: Token,
        end: Token,
        separator: Token
    ) -> Vec<T> {
        self.parse(start);
        let mut list = vec![];
        while self.token != end {
            let elem = parse_elem(self);
            list.push(elem);
            if self.token != separator {
                break;
            }
            self.next();
        }
        self.parse(end);
        list
    }

    fn token_string(&self) -> String {
        intern(&self.text[self.start..self.end])
    }

    fn parse(&mut self, token: Token) {
        if self.token == token {
            self.next();
            return;
        }
        print_cursor(self.text, self.start, self.end);
        println!("expected {:?}, got {:?}", token, self.token);
        error();
    }
}
