use crate::error;
use crate::intern;
use crate::syntax;
use crate::String;

#[derive(Debug, Default)]
pub struct TypeIntern {
    pub types: Vec<Type>,
}

impl TypeIntern {
    fn intern(&mut self, ty: Type) -> TypeId {
        for (i, interned) in self.types.iter().enumerate() {
            if ty == *interned {
                return i;
            }
        }
        let i = self.types.len();
        self.types.push(ty);
        i
    }

    fn get(&self, i: TypeId) -> &Type {
        &self.types[i]
    }

    fn auto_deref(&self, i: TypeId) -> &Type {
        let mut ty = self.get(i);
        while let Type::Pointer(i) = ty {
            ty = self.get(*i);
        }
        ty
    }
}

#[derive(Debug, Default)]
pub struct NameTable {
    names: Vec<(String, Def)>,
}

#[derive(Debug, Copy, Clone)]
enum Def {
    Func(FuncId),
    Type(TypeId),
    Param(ParamId),
    Local(LocalId),
}

impl NameTable {
    fn def(&mut self, name: String, def: Def) {
        self.names.push((name, def));
    }

    fn get(&self, name: String) -> Option<Def> {
        for &(other, def) in self.names.iter().rev() {
            if other == name {
                return Some(def);
            }
        }
        None
    }

    fn enter_scope(&self) -> usize {
        self.names.len()
    }

    fn exit_scope(&mut self, scope: usize) {
        while scope < self.names.len() {
            self.names.pop();
        }
    }
}

pub fn build(
    struct_types: &[syntax::StructType],
    func_decls: &[syntax::FuncDecl],
    func_bodys: &[syntax::FuncBody],
) -> (Vec<FuncDecl>, Vec<FuncBody>, Vec<Type>) {
    let mut b = ModuleBuilder::default();

    b.add_type("i8", Type::I8);
    b.add_type("i16", Type::I16);
    b.add_type("i32", Type::I32);
    b.add_type("i64", Type::I64);
    b.add_type("f32", Type::F32);
    b.add_type("f64", Type::F64);
    b.add_type("bool", Type::Bool);

    for struct_type in struct_types {
        b.add_struct_type(struct_type);
    }

    for decl in func_decls {
        b.add_func_decl(decl);
    }

    let mut bodys = vec![];
    for func in func_bodys {
        let b = FuncBuilder {
            module: &mut b,
            body: FuncBody {
                id: func.id,
                locals: vec![],
                // FIXME This block is unnecessary.
                body: Block {
                    stmts: vec![],
                },
            },
        };
        let body = b.build_body(&func.body);
        bodys.push(body);
    }

    (b.func_decls, bodys, b.types.types)
}

struct FuncBuilder<'a> {
    module: &'a mut ModuleBuilder,
    body: FuncBody,
}

impl<'a> FuncBuilder<'a> {
    fn build_body(mut self, block: &syntax::Block) -> FuncBody {
        let scope = self.module.names.enter_scope();
        let decl = &self.module.func_decls[self.body.id];
        for (i, name) in decl.params.iter().enumerate() {
            self.module.names.def(*name, Def::Param(i));
        }
        self.body.body = self.build_block(block);
        self.module.names.exit_scope(scope);

        self.body
    }

    fn build_block(&mut self, block: &syntax::Block) -> Block {
        let scope = self.module.names.enter_scope();
        let mut block2 = Block {
            stmts: vec![],
        };
        for stmt in &block.stmts {
            let stmt = self.build_stmt(stmt);
            block2.stmts.push(stmt);
        }
        self.module.names.exit_scope(scope);
        block2
    }

    fn build_stmt(&mut self, stmt: &syntax::Stmt) -> Stmt {
        match stmt {
            syntax::Stmt::For(init, cond, post, body) => {
                let scope = self.module.names.enter_scope();
                let init = self.build_stmt(init);
                let bool = self.module.types.intern(Type::Bool);
                let cond = self.build_expr(cond, Some(bool));
                let post = self.build_stmt(post);
                let body = self.build_block(body);
                self.module.names.exit_scope(scope);
                Stmt::For(init.into(), cond, post.into(), body)
            }
            syntax::Stmt::Assign(x, y) => {
                let x = self.build_expr(x, None);
                let y = self.build_expr(y, Some(x.ty));
                Stmt::Assign(x, y)
            }
            syntax::Stmt::While(cond, body) => {
                let bool = self.module.types.intern(Type::Bool);
                let cond = self.build_expr(cond, Some(bool));
                let body = self.build_block(body);
                Stmt::While(cond, body)
            }
            syntax::Stmt::If(cond, body) => {
                let bool = self.module.types.intern(Type::Bool);
                let cond = self.build_expr(cond, Some(bool));
                let body = self.build_block(body);
                Stmt::If(cond, body)
            }
            syntax::Stmt::Let(name, ty, e) => {
                let ty = ty.as_ref().map(|ty| self.module.build_type(ty));
                let e = self.build_expr(e, ty);
                let i = self.body.locals.len();
                let x = Expr {
                    kind: ExprKind::Local(i),
                    ty: e.ty,
                };
                self.body.locals.push(x.ty);
                self.module.names.def(*name, Def::Local(i));
                Stmt::Assign(x, e)
            }
            syntax::Stmt::Return(e) => {
                let ret = self.module.func_decls[self.body.id].ty.ret;
                let e = self.build_expr(e, Some(ret));
                Stmt::Return(e)
            }
            syntax::Stmt::Expr(e) => {
                let e = self.build_expr(e, None);
                Stmt::Expr(e)
            }
        }
    }

    fn build_expr(&mut self, e: &syntax::Expr, env: Option<TypeId>) -> Expr {
        let (kind, ty) = match e {
            syntax::Expr::Unary(op, e) => match op {
                syntax::AMPERSAND => {
                    let env = match env {
                        Some(ty) => match self.module.types.get(ty) {
                            Type::Pointer(ty) => Some(*ty),
                            _ => panic!("address-of expression should have pointer type"),
                        }
                        None => None,
                    };
                    let e = self.build_expr(e, env);
                    let ty = self.module.types.intern(Type::Pointer(e.ty));
                    (ExprKind::Unary(Unop::AddressOf, e.into()), ty)
                }
                syntax::STAR => {
                    let env = match env {
                        Some(ty) => Some(self.module.types.intern(Type::Pointer(ty))),
                        None => None,
                    };
                    let e = self.build_expr(e, env);
                    let ty = match self.module.types.get(e.ty) {
                        Type::Pointer(ty) => *ty,
                        _ => panic!("expected pointer type in deref"),
                    };
                    (ExprKind::Unary(Unop::Deref, e.into()), ty)
                }
                op => unimplemented!("unary operator {:?}", op),
            }
            syntax::Expr::Bool(b) => {
                let bool = self.module.types.intern(Type::Bool);
                (ExprKind::Bool(*b), bool)
            }
            syntax::Expr::Cast(e, ty) => {
                let e = self.build_expr(e, None);
                let ty = self.module.build_type(ty);
                (ExprKind::Cast(e.into(), ty), ty)
            }
            syntax::Expr::Field(e, field_name) => {
                let e = self.build_expr(e, None);
                let sty = match self.module.types.auto_deref(e.ty) {
                    Type::Struct(sty) => sty,
                    _ => panic!(),
                };
                if let Some(i) = sty.field_index(*field_name) {
                    let ty = sty.fields[i].1;
                    (ExprKind::Field(e.into(), i as u32), ty)
                } else {
                    // Try to find a function to use as a method call.
                    let mut func_id = None;
                    for (i, func) in self.module.func_decls.iter().enumerate() {
                        if func.name != *field_name {
                            continue;
                        }
                        if func.ty.params.len() == 0 {
                            continue;
                        }
                        let first_ty = func.ty.params[0];
                        if first_ty == e.ty {
                            func_id = Some(i);
                            break;
                        }
                    }
                    if let Some(i) = func_id {
                        let mut fnty = self.module.func_decls[i].ty.clone();
                        fnty.params.remove(0);
                        let fnty = self.module.types.intern(Type::Func(fnty));
                        (ExprKind::MethodCall(i, e.into()), fnty)
                    } else {
                        panic!("unable to find {:?} on {:?}", field_name, sty.name);
                    }
                }
            }
            syntax::Expr::Struct(fields) => {
                let ty = match env {
                    None => panic!(),
                    Some(ty) => self.module.types.get(ty),
                };
                let sty = match ty {
                    Type::Struct(ty) => ty.clone(),
                    _ => panic!(),
                };
                let mut fields2 = vec![];
                for (name, e) in fields {
                    let field_index = sty.field_index(*name).unwrap();
                    let field_type = sty.fields[field_index].1;
                    let e = self.build_expr(e, Some(field_type));
                    fields2.push((field_index as u32, e));
                }
                (
                    ExprKind::Struct(fields2),
                    self.module.types.intern(Type::Struct(sty)),
                )
            }
            syntax::Expr::Unit => (ExprKind::Unit, self.module.types.intern(Type::Unit)),
            syntax::Expr::Call(func, args) => {
                let mut func = self.build_expr(func, None);
                let mut args2 = vec![];
                func = match func.kind {
                    ExprKind::MethodCall(i, arg) => {
                        args2.push(*arg);
                        let fnty = self.module.func_decls[i].ty.clone();
                        let fnty = self.module.types.intern(Type::Func(fnty));
                        Expr {
                            kind: ExprKind::Func(i),
                            ty: fnty,
                        }
                    }
                    _ => func,
                };

                // Infer function type.
                let fnty = match self.module.types.get(func.ty) {
                    Type::Func(fnty) => fnty.clone(),
                    _ => panic!(),
                };
                // Verify return type matches environment type.
                if let Some(ret) = env {
                    if fnty.ret != ret {
                        panic!("return type doesn't match");
                    }
                }
                // Account for method call arg
                let params = &fnty.params[args2.len()..];
                // Verify number of call args count.
                if fnty.var_args {
                    if args.len() < params.len() {
                        println!(
                            "var args function requires {} params, got {}",
                            params.len(),
                            args.len()
                        );
                        error();
                    }
                } else {
                    if args.len() != params.len() {
                        println!(
                            "function has {} params, but {} args were supplied",
                            params.len(),
                            args.len()
                        );
                        error();
                    }
                }
                // Verify arg types match inferred function type.
                for (arg, &ty) in args.iter().zip(params) {
                    let arg = self.build_expr(arg, Some(ty));
                    args2.push(arg);
                }
                // Var args params don't get type checked.
                for arg in &args[params.len()..] {
                    let arg = self.build_expr(arg, None);
                    args2.push(arg);
                }
                let args = args2;

                (ExprKind::Call(func.into(), args), fnty.ret)
            }
            syntax::Expr::String(s) => {
                let i8 = self.module.types.intern(Type::I8);
                let ptr_i8 = self.module.types.intern(Type::Pointer(i8));
                (ExprKind::String(*s), ptr_i8)
            }
            syntax::Expr::Binary(op, x, y) => {
                let op = match op {
                    syntax::PLUS => Binop::Add,
                    syntax::MINUS => Binop::Sub,
                    syntax::STAR => Binop::Mul,
                    syntax::SLASH => Binop::Div,
                    syntax::LT => Binop::Cmp(Predicate::Lt),
                    syntax::GT => Binop::Cmp(Predicate::Gt),
                    syntax::LE => Binop::Cmp(Predicate::Le),
                    syntax::GE => Binop::Cmp(Predicate::Ge),
                    syntax::EQ => Binop::Cmp(Predicate::Eq),
                    syntax::NE => Binop::Cmp(Predicate::Ne),
                    _ => panic!(),
                };
                let x = self.build_expr(x, None);
                let y_ty = match self.module.types.get(x.ty) {
                    Type::Pointer(_) => self.module.types.intern(Type::I32),
                    _ => x.ty,
                };
                let y = self.build_expr(y, Some(y_ty));
                let ty = match op {
                    Binop::Cmp(_) => self.module.types.intern(Type::Bool),
                    _ => x.ty,
                };
                (ExprKind::Binary(op, x.into(), y.into()), ty)
            }
            syntax::Expr::Float(s) => {
                let f32 = self.module.types.intern(Type::F32);
                let ty = match env {
                    Some(ty) => match self.module.types.get(ty) {
                        Type::F32 | Type::F64 => ty,
                        _ => f32,
                    },
                    None => f32,
                };
                (ExprKind::Float(*s), ty)
            }
            syntax::Expr::Integer(s) => {
                let ty = match env {
                    None => self.module.types.intern(Type::I32),
                    Some(ty) => match self.module.types.get(ty) {
                        Type::I8 | Type::I16 | Type::I32 | Type::I64 => ty,
                        _ => self.module.types.intern(Type::I32),
                    },
                };
                (ExprKind::Integer(*s), ty)
            }
            syntax::Expr::Name(name) => match self.module.names.get(*name) {
                None => {
                    println!("undefined symbol {:?}", name);
                    error();
                }
                Some(def) => match def {
                    Def::Func(i) => {
                        let ty = self.module.types.intern({
                            let ty = &self.module.func_decls[i].ty;
                            let ty = ty.clone();
                            Type::Func(ty)
                        });
                        (ExprKind::Func(i), ty)
                    }
                    Def::Local(i) => (ExprKind::Local(i), self.body.locals[i]),
                    Def::Param(i) => {
                        let ty = self.module.func_decls[self.body.id].ty.params[i];
                        (ExprKind::Param(i), ty)
                    }
                    _ => unimplemented!(),
                },
            },
        };
        if let Some(env) = env {
            if ty != env {
                println!(
                    "expected {:?}, got {:?}",
                    self.module.types.get(env),
                    self.module.types.get(ty)
                );
                error();
            }
        }
        Expr { kind, ty }
    }
}

#[derive(Default)]
struct ModuleBuilder {
    names: NameTable,
    types: TypeIntern,
    func_decls: Vec<FuncDecl>,
}

impl ModuleBuilder {
    fn add_type(&mut self, name: &str, ty: Type) {
        let name = intern(name);
        let i = self.types.intern(ty);
        self.names.def(name, Def::Type(i));
    }

    fn add_struct_type(&mut self, struct_type: &syntax::StructType) {
        let mut fields = vec![];
        for (name, ty) in &struct_type.fields {
            let ty = self.build_type(ty);
            fields.push((*name, ty));
        }
        let ty = StructType {
            name: struct_type.name,
            fields,
        };
        let ty = self.types.intern(Type::Struct(ty));
        self.names.def(struct_type.name, Def::Type(ty));
    }

    fn add_func_decl(&mut self, func: &syntax::FuncDecl) {
        let i = self.func_decls.len();
        self.names.def(func.name, Def::Func(i));

        let func_type = self.build_func_type(&func.ty);
        let func_decl = FuncDecl {
            name: func.name,
            ty: func_type,
            params: func.params.clone(),
        };
        self.func_decls.push(func_decl);
    }

    fn build_type(&mut self, ty: &syntax::Type) -> TypeId {
        match ty {
            syntax::Type::Name(name) => match self.names.get(*name) {
                Some(Def::Type(i)) => i,
                x => panic!("building type: name def = {:?}", x),
            },
            syntax::Type::Pointer(ty) => {
                let ty = self.build_type(ty);
                self.types.intern(Type::Pointer(ty))
            }
            syntax::Type::Func(_) => unimplemented!(),
            syntax::Type::Unit => self.types.intern(Type::Unit),
        }
    }

    fn build_func_type(&mut self, ty: &syntax::FuncType) -> FuncType {
        let mut params = vec![];
        for ty in &ty.params {
            let ty = self.build_type(ty);
            params.push(ty);
        }
        let ret = self.build_type(&ty.ret);
        let var_args = ty.var_args;
        FuncType {
            params,
            ret,
            var_args,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Pointer(TypeId),
    Func(FuncType),
    Struct(StructType),
    Unit,
    Bool,
}

impl Type {
    pub fn kind(&self) -> TypeKind {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => TypeKind::Int,
            Type::F32 | Type::F64 => TypeKind::Float,
            Type::Struct(_) => TypeKind::Struct,
            Type::Unit => TypeKind::Unit,
            Type::Bool => TypeKind::Bool,
            Type::Func(_) => TypeKind::Func,
            Type::Pointer(_) => TypeKind::Pointer,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TypeKind {
    Int,
    Float,
    Pointer,
    Struct,
    Unit,
    Bool,
    Func,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<(String, TypeId)>,
}

impl StructType {
    fn field_index(&self, field_name: String) -> Option<usize> {
        for (i, &(name, _)) in self.fields.iter().enumerate() {
            if name == field_name {
                return Some(i);
            }
        }
        None
    }
}

pub type TypeId = usize;
pub type ParamId = usize;
pub type FuncId = usize;
pub type LocalId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
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
    pub id: FuncId,
    pub locals: Vec<TypeId>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Assign(Expr, Expr),
    Return(Expr),
    Expr(Expr),
    If(Expr, Block),
    While(Expr, Block),
    For(Box<Stmt>, Expr, Box<Stmt>, Block),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TypeId,
}

#[derive(Debug, Copy, Clone)]
pub enum Predicate {
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}

#[derive(Debug, Copy, Clone)]
pub enum Unop {
    AddressOf,
    Deref,
}

#[derive(Debug, Copy, Clone)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Cmp(Predicate),
}

#[derive(Debug)]
pub enum ExprKind {
    Unit,
    Integer(String),
    Float(String),
    Param(ParamId),
    Func(FuncId),
    Local(LocalId),
    Unary(Unop, Box<Expr>),
    Binary(Binop, Box<Expr>, Box<Expr>),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
    Struct(Vec<(u32, Expr)>),
    Field(Box<Expr>, u32),
    MethodCall(FuncId, Box<Expr>),
    Cast(Box<Expr>, TypeId),
    Bool(bool),
}
