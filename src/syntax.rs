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
    FLOAT,
    STRING,
    PLUS,
    MINUS,
    ELLIPSIS,
    DOT,
    TYPE,
    STRUCT,
    EOF,
}
pub use Token::*;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Name(String),
    Pointer(Box<Type>),
    Func(Box<FuncType>),
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: Vec<Type>,
    pub ret: Type,
    pub var_args: bool,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<String>,
    pub ty: FuncType,
}

#[derive(Debug)]
pub struct FuncBody {
    // FIXME use something else to associate with FuncDecl?
    pub id: usize,
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
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Unit,
    Integer(String),
    Float(String),
    Name(String),
    Binary(Token, Box<Expr>, Box<Expr>),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
    Struct(Vec<(String, Expr)>),
    Field(Box<Expr>, String),
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
        let text = self.text[self.end..].as_bytes();
        let (c, d, e) = match text.len() {
            0 => {
                self.token = EOF;
                return;
            }
            1 => (text[0] as char, '\0', '\0'),
            2 => (text[0] as char, text[1] as char, '\0'),
            _ => (text[0] as char, text[1] as char, text[2] as char),
        };

        let (token, n) = match c {
            '-' if d == '>' => (ARROW, 2),
            '-' => (MINUS, 1),
            '+' => (PLUS, 1),
            '(' => (LPARENS, 1),
            ')' => (RPARENS, 1),
            '{' => (LBRACE, 1),
            '}' => (RBRACE, 1),
            '=' => (ASSIGN, 1),
            ',' => (COMMA, 1),
            ';' => (SEMICOLON, 1),
            '*' => (STAR, 1),
            ':' => (COLON, 1),
            '.' if d == '.' && e == '.' => (ELLIPSIS, 3),
            '.' => (DOT, 1),
            '"' => {
                let mut escaped = false;
                let mut n = 1;
                for b in &text[1..] {
                    if !escaped && *b == b'"' {
                        break;
                    }
                    escaped = !escaped && *b == b'\\';
                    n += 1;
                }
                if text.get(n) != Some(&b'"') {
                    print_cursor(self.text, self.start, self.start + 1);
                    println!("unterminated string literal");
                    error();
                }
                n += 1;
                (STRING, n)
            }
            _ if c.is_ascii_alphabetic() || c == '_' => {
                let mut n = 0;
                for c in text {
                    if !c.is_ascii_alphanumeric() && *c != b'_' {
                        break;
                    }
                    n += 1;
                }
                // keywords
                let token = match &text[..n] {
                    b"fn" => FN,
                    b"let" => LET,
                    b"return" => RETURN,
                    b"type" => TYPE,
                    b"struct" => STRUCT,
                    _ => NAME,
                };
                (token, n)
            }
            _ if c.is_ascii_digit() => {
                let mut t = INTEGER;
                let mut n = 0;
                for c in text {
                    match (t, c) {
                        (INTEGER, b'.') => {
                            t = FLOAT;
                        }
                        (_, c) if c.is_ascii_digit() => {}
                        _ => break,
                    }
                    n += 1;
                }
                (t, n)
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

    pub fn parse_struct_type(&mut self) -> StructType {
        self.parse(TYPE);
        let name = self.token_string();
        self.parse(NAME);
        self.parse(STRUCT);
        self.parse(LBRACE);
        let mut fields = vec![];
        while self.token != RBRACE {
            let name = self.token_string();
            self.parse(NAME);
            self.parse(COLON);
            let ty = self.parse_type();
            fields.push((name, ty));
            if self.token != COMMA {
                break;
            }
            self.next();
        }
        self.parse(RBRACE);

        StructType {
            name: name,
            fields: fields,
        }
    }

    pub fn parse_func_decl(&mut self) -> FuncDecl {
        self.parse(FN);

        let name = self.token_string();
        self.parse(NAME);

        let mut var_args = false;
        let mut params = vec![];
        let mut param_types = vec![];
        self.parse(LPARENS);
        while self.token != RPARENS {
            if self.token == ELLIPSIS {
                self.next();
                var_args = true;
                break;
            }

            let name = self.token_string();
            self.parse(NAME);
            self.parse(COLON);
            let ty = self.parse_type();
            params.push(name);
            param_types.push(ty);

            if self.token != COMMA {
                break;
            }
            self.next();
        }
        self.parse(RPARENS);

        let ret = match self.token {
            ARROW => {
                self.next();
                self.parse_type()
            }
            _ => Type::Unit,
        };

        let ty = FuncType {
            params: param_types,
            ret: ret.into(),
            var_args: var_args,
        };

        FuncDecl {
            name: name,
            params: params,
            ty: ty,
        }
    }

    pub fn parse_block(&mut self) -> Block {
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
                let e = match self.token {
                    SEMICOLON => Expr::Unit,
                    _ => self.parse_expr(),
                };
                Stmt::Return(e)
            }
            _ => {
                let e = self.parse_expr();
                Stmt::Expr(e)
            }
        }
    }

    fn parse_binary(&mut self, mut lhs: Expr, min_precedence: i32) -> Expr {
        fn precedence(op: Token) -> i32 {
            match op {
                PLUS | MINUS => 20,
                _ => -1,
            }
        }

        while precedence(self.token) >= min_precedence {
            let op = self.token;
            let i = precedence(op);
            self.next();
            let mut rhs = self.parse_call();
            while precedence(self.token) > i {
                let j = precedence(self.token);
                rhs = self.parse_binary(rhs, j);
            }

            lhs = Expr::Binary(op, lhs.into(), rhs.into());
        }

        lhs
    }

    fn parse_expr(&mut self) -> Expr {
        let lhs = self.parse_call();
        self.parse_binary(lhs, 0)
    }

    fn parse_field(&mut self) -> Expr {
        let mut e = self.parse_atom();
        while self.token == DOT {
            self.next();
            let field_name = self.token_string();
            self.parse(NAME);
            e = Expr::Field(e.into(), field_name);
        }
        e
    }

    fn parse_call(&mut self) -> Expr {
        let mut x = self.parse_field();
        while self.token == LPARENS {
            self.next();

            let mut args = vec![];
            while self.token != RPARENS {
                let arg = self.parse_expr();
                args.push(arg);

                if self.token != COMMA {
                    break;
                }
                self.next();
            }
            self.parse(RPARENS);

            x = Expr::Call(x.into(), args);
        }
        x
    }

    fn parse_atom(&mut self) -> Expr {
        match self.token {
            LBRACE => {
                self.next();
                let mut fields = vec![];
                while self.token != RBRACE {
                    let name = self.token_string();
                    self.parse(NAME);
                    self.parse(COLON);
                    let e = self.parse_expr();
                    fields.push((name, e));

                    if self.token != COMMA {
                        break;
                    }
                    self.next();
                }
                self.parse(RBRACE);
                Expr::Struct(fields)
            }
            LPARENS => {
                self.next();
                self.parse(RPARENS);
                Expr::Unit
            }
            STRING => {
                let s = self.token_string();
                self.next();
                Expr::String(s)
            }
            FLOAT => {
                let s = self.token_string();
                self.next();
                Expr::Float(s)
            }
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
