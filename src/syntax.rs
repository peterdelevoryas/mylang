use crate::error;
use crate::intern;
use crate::print_cursor;
use crate::String;

#[derive(Debug)]
pub struct Module {
    pub const_decls: Vec<ConstDecl>,
    pub type_decls: Vec<TypeDecl>,
    pub func_decls: Vec<FuncDecl>,
    pub func_bodys: Vec<FuncBody>,
}

pub fn parse(text: &str) -> Module {
    let mut p = Parser {
        text: text,
        start: 0,
        end: 0,
        token: EOF,
    };
    p.next();
    let mut type_decls = vec![];
    let mut func_decls = vec![];
    let mut func_bodys = vec![];
    let mut const_decls = vec![];
    while p.token != EOF {
        match p.token {
            CONST => {
                let const_decl = p.parse_const_decl();
                const_decls.push(const_decl);
            }
            TYPE => {
                let type_decl = p.parse_type_decl();
                type_decls.push(type_decl);
            }
            FN => {
                let decl = p.parse_func_decl();
                let id = func_decls.len();
                func_decls.push(decl);
                if p.token == SEMICOLON {
                    p.next();
                    continue;
                }

                let body = p.parse_block();
                let body = FuncBody { id: id, body: body };
                func_bodys.push(body);
            }
            _ => {
                print_cursor(p.text, p.start, p.start + 1);
                println!("expected type or function");
                error();
            }
        }
    }
    Module {
        const_decls: const_decls,
        type_decls: type_decls,
        func_decls: func_decls,
        func_bodys: func_bodys,
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token {
    BREAK,
    CONTINUE,
    NULL,
    CONST,
    SIZEOF,
    PLUSEQ,
    MINUSEQ,
    AMPERSAND,
    FOR,
    WHILE,
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    IF,
    LBRACKET,
    RBRACKET,
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
    AS,
    LET,
    RETURN,
    NAME,
    INTEGER,
    FLOAT,
    STRING,
    CHAR,
    PLUS,
    MINUS,
    ELLIPSIS,
    DOT,
    TYPE,
    STRUCT,
    SLASH,
    TRUE,
    FALSE,
    EOF,
}
pub use Token::*;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Name(String),
    Pointer(Box<Type>),
    Func(Box<FuncType>),
    Array(u32, Box<Type>),
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: String,
    pub kind: TypeDeclKind,
}

#[derive(Debug, Clone)]
pub enum TypeDeclKind {
    Struct(Vec<(String, Type)>),
    Alias(Type),
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
pub struct ConstDecl {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expr,
}

#[derive(Debug)]
pub struct FuncBody {
    // FIXME use something else to associate with FuncDecl?
    pub id: usize,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(String, Option<Type>, Option<Expr>),
    Return(Expr),
    Expr(Expr),
    If(Expr, Block),
    While(Expr, Block),
    Assign(Expr, Expr),
    For(Box<Stmt>, Expr, Box<Stmt>, Block),
    OpAssign(Token, Expr, Expr),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: (u16, u16),
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Unit,
    Integer(String),
    Float(String),
    Name(String),
    Unary(Token, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
    Struct(Vec<(String, Expr)>),
    Array(Vec<Expr>),
    Field(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Cast(Box<Expr>, Type),
    Bool(bool),
    Sizeof(Type),
    Char(u8),
    Null,
}

pub struct Parser<'a> {
    pub text: &'a str,
    pub start: usize,
    pub end: usize,
    pub token: Token,
}

impl<'a> Parser<'a> {
    pub fn next(&mut self) {
        loop {
            let end = self.end;
            self.skip_space();
            self.skip_comments();
            if self.end == end {
                break;
            }
        }
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
            '-' if d == '-' => (MINUSEQ, 2),
            '-' => (MINUS, 1),
            '&' => (AMPERSAND, 1),
            '+' if d == '=' => (PLUSEQ, 2),
            '+' => (PLUS, 1),
            '(' => (LPARENS, 1),
            ')' => (RPARENS, 1),
            '{' => (LBRACE, 1),
            '}' => (RBRACE, 1),
            '[' => (LBRACKET, 1),
            ']' => (RBRACKET, 1),
            '/' => (SLASH, 1),
            '=' if d == '=' => (EQ, 2),
            '!' if d == '=' => (NE, 2),
            '<' if d == '=' => (LE, 2),
            '>' if d == '=' => (GE, 2),
            '<' => (LT, 1),
            '>' => (GT, 1),
            '=' => (ASSIGN, 1),
            ',' => (COMMA, 1),
            ';' => (SEMICOLON, 1),
            '*' => (STAR, 1),
            ':' => (COLON, 1),
            '.' if d == '.' && e == '.' => (ELLIPSIS, 3),
            '.' => (DOT, 1),
            '"' | '\'' => {
                let quote = c as u8;
                let mut escaped = false;
                let mut n = 1;
                for &b in &text[1..] {
                    if !escaped && b == quote {
                        break;
                    }
                    escaped = !escaped && b == b'\\';
                    n += 1;
                }
                if text.get(n) != Some(&quote) {
                    print_cursor(self.text, self.start, self.start + 1);
                    println!("unterminated literal");
                    error();
                }
                n += 1;
                let t = match quote {
                    b'"' => STRING,
                    b'\'' => CHAR,
                    _ => unreachable!(),
                };
                (t, n)
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
                    b"break" => BREAK,
                    b"continue" => CONTINUE,
                    b"null" => NULL,
                    b"const" => CONST,
                    b"sizeof" => SIZEOF,
                    b"for" => FOR,
                    b"while" => WHILE,
                    b"if" => IF,
                    b"fn" => FN,
                    b"as" => AS,
                    b"let" => LET,
                    b"return" => RETURN,
                    b"type" => TYPE,
                    b"struct" => STRUCT,
                    b"true" => TRUE,
                    b"false" => FALSE,
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

    fn skip_comments(&mut self) {
        if !self.text[self.end..].starts_with("//") {
            return;
        }
        self.end += 2;
        for b in self.text[self.end..].bytes() {
            self.end += 1;
            if b == b'\n' {
                break;
            }
        }
    }

    pub fn parse_const_decl(&mut self) -> ConstDecl {
        self.parse(CONST);
        let name = self.token_string();
        self.parse(NAME);
        let ty = match self.token {
            COLON => {
                self.next();
                let ty = self.parse_type();
                Some(ty)
            }
            _ => None,
        };
        self.parse(ASSIGN);
        let value = self.parse_expr();
        self.parse(SEMICOLON);
        ConstDecl { name, ty, value }
    }

    pub fn parse_type_decl(&mut self) -> TypeDecl {
        self.parse(TYPE);
        let name = self.token_string();
        self.parse(NAME);
        let kind = match self.token {
            STRUCT => {
                self.next();
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
                TypeDeclKind::Struct(fields)
            }
            ASSIGN => {
                self.next();
                let ty = self.parse_type();
                self.parse(SEMICOLON);
                TypeDeclKind::Alias(ty)
            }
            _ => {
                print_cursor(self.text, self.start, self.end);
                println!("expected struct or alias type declaration");
                error();
            }
        };
        TypeDecl { name, kind }
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
        self.parse(LBRACE);
        let mut stmts = vec![];
        while self.token != RBRACE {
            let stmt = self.parse_stmt();
            stmts.push(stmt);
            if self.token == SEMICOLON {
                self.next();
            }
        }
        self.parse(RBRACE);

        Block { stmts }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.token {
            BREAK => {
                self.next();
                Stmt::Break
            }
            CONTINUE => {
                self.next();
                Stmt::Continue
            }
            FOR => {
                self.next();
                let init = self.parse_stmt();
                self.parse(SEMICOLON);
                let cond = self.parse_expr();
                self.parse(SEMICOLON);
                let post = self.parse_stmt();
                let body = self.parse_block();
                Stmt::For(init.into(), cond, post.into(), body)
            }
            WHILE => {
                self.next();
                let cond = self.parse_expr();
                let body = self.parse_block();
                Stmt::While(cond, body)
            }
            IF => {
                self.next();
                let cond = self.parse_expr();
                let body = self.parse_block();
                Stmt::If(cond, body)
            }
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

                let e = match self.token {
                    ASSIGN => {
                        self.next();
                        let e = self.parse_expr();
                        Some(e)
                    }
                    _ => None,
                };

                Stmt::Let(name, ty, e)
            }
            RETURN => {
                self.next();
                let e = match self.token {
                    SEMICOLON => Expr {
                        kind: ExprKind::Unit,
                        span: (self.start as u16, self.end as u16),
                    },
                    _ => self.parse_expr(),
                };
                Stmt::Return(e)
            }
            _ => {
                let e = self.parse_expr();
                let stmt = match self.token {
                    ASSIGN => {
                        self.next();
                        let x = self.parse_expr();
                        Stmt::Assign(e, x)
                    }
                    PLUSEQ | MINUSEQ => {
                        let op = match self.token {
                            PLUSEQ => PLUS,
                            MINUSEQ => MINUS,
                            _ => unreachable!(),
                        };
                        self.next();
                        let x = self.parse_expr();
                        Stmt::OpAssign(op, e.into(), x.into())
                    }
                    _ => Stmt::Expr(e),
                };
                stmt
            }
        }
    }

    fn parse_binary(&mut self, mut lhs: Expr, min_precedence: i32) -> Expr {
        fn precedence(op: Token) -> i32 {
            match op {
                LT | GT | LE | GE | EQ | NE => 10,
                PLUS | MINUS => 20,
                STAR | SLASH => 30,
                _ => -1,
            }
        }

        let start = self.start;
        while precedence(self.token) >= min_precedence {
            let op = self.token;
            let i = precedence(op);
            self.next();
            let mut rhs = self.parse_unary();
            while precedence(self.token) > i {
                let j = precedence(self.token);
                rhs = self.parse_binary(rhs, j);
            }

            lhs = Expr {
                kind: ExprKind::Binary(op, lhs.into(), rhs.into()),
                span: (start as u16, self.end as u16),
            };
        }

        lhs
    }

    fn parse_expr(&mut self) -> Expr {
        let lhs = self.parse_unary();
        self.parse_binary(lhs, 0)
    }

    fn parse_as(&mut self) -> Expr {
        let start = self.start;
        let mut x = self.parse_call();
        while self.token == AS {
            self.next();
            let ty = self.parse_type();
            x = Expr {
                kind: ExprKind::Cast(x.into(), ty),
                span: (start as u16, self.end as u16),
            };
        }
        x
    }

    fn parse_unary(&mut self) -> Expr {
        let start = self.start as u16;
        match self.token {
            STAR | AMPERSAND => {
                let op = self.token;
                self.next();
                let e = self.parse_unary();
                Expr {
                    kind: ExprKind::Unary(op, e.into()),
                    span: (start as u16, self.end as u16),
                }
            }
            _ => self.parse_as(),
        }
    }

    fn parse_index(&mut self) -> Expr {
        let start = self.start;
        let mut x = self.parse_field();
        while self.token == LBRACKET {
            self.next();
            let i = self.parse_expr();
            self.parse(RBRACKET);

            x = Expr {
                kind: ExprKind::Index(x.into(), i.into()),
                span: (start as u16, self.end as u16),
            };
        }
        x
    }

    fn parse_field(&mut self) -> Expr {
        let start = self.start;
        let mut e = self.parse_atom();
        while self.token == DOT {
            self.next();
            let field_name = self.token_string();
            self.parse(NAME);
            e = Expr {
                kind: ExprKind::Field(e.into(), field_name),
                span: (start as u16, self.end as u16),
            };
        }
        e
    }

    fn parse_call(&mut self) -> Expr {
        let start = self.start;
        let mut x = self.parse_index();
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

            x = Expr {
                kind: ExprKind::Call(x.into(), args),
                span: (start as u16, self.end as u16),
            };
        }
        x
    }

    fn parse_atom(&mut self) -> Expr {
        let start = self.start;
        let kind = match self.token {
            CHAR => {
                let s = self.token_string();
                let s = s.as_bytes();
                self.next();
                let c = match s[1] {
                    b'\\' => match s[2] {
                        b'n' => b'\n',
                        b't' => b'\t',
                        b'r' => b'\r',
                        c => c,
                    },
                    c => c,
                };
                ExprKind::Char(c)
            }
            NULL => {
                self.next();
                ExprKind::Null
            }
            SIZEOF => {
                self.next();
                self.parse(LPARENS);
                let ty = self.parse_type();
                self.parse(RPARENS);
                ExprKind::Sizeof(ty)
            }
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
                ExprKind::Struct(fields)
            }
            LBRACKET => {
                self.next();
                let mut elems = vec![];
                while self.token != RBRACKET {
                    let e = self.parse_expr();
                    elems.push(e);
                    if self.token != COMMA {
                        break;
                    }
                    self.next();
                }
                self.parse(RBRACKET);
                ExprKind::Array(elems)
            }
            LPARENS => {
                self.next();
                let e = match self.token {
                    RPARENS => None,
                    _ => Some(self.parse_expr()),
                };
                self.parse(RPARENS);

                match e {
                    Some(e) => e.kind,
                    None => ExprKind::Unit
                }
            }
            TRUE => {
                self.next();
                ExprKind::Bool(true)
            }
            FALSE => {
                self.next();
                ExprKind::Bool(false)
            }
            STRING => {
                let s = self.token_string();
                self.next();
                ExprKind::String(s)
            }
            FLOAT => {
                let s = self.token_string();
                self.next();
                ExprKind::Float(s)
            }
            INTEGER => {
                let s = self.token_string();
                self.next();
                ExprKind::Integer(s)
            }
            NAME => {
                let name = self.token_string();
                self.next();
                ExprKind::Name(name)
            }
            _ => {
                print_cursor(self.text, self.start, self.end);
                println!("expected expression");
                error();
            }
        };
        let span = (start as u16, self.end as u16);
        Expr { kind, span }
    }

    fn parse_type(&mut self) -> Type {
        match self.token {
            FN => {
                self.next();
                self.parse(LPARENS);
                let mut var_args = false;
                let mut params = vec![];
                while self.token != RPARENS {
                    if self.token == ELLIPSIS {
                        self.next();
                        var_args = true;
                        break;
                    }
                    let ty = self.parse_type();
                    params.push(ty);
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

                let ty = FuncType { params, ret, var_args };
                Type::Func(ty.into())
            }
            LBRACKET => {
                self.next();
                let s = self.token_string();
                let i = self.start;
                let j = self.end;
                self.parse(INTEGER);
                let n: u32 = match s.parse() {
                    Ok(n) => n,
                    Err(e) => {
                        print_cursor(self.text, i, j);
                        println!("unable to parse array element count: {}", e);
                        error();
                    }
                };
                self.parse(RBRACKET);
                let elem_ty = self.parse_type();

                Type::Array(n, elem_ty.into())
            }
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
