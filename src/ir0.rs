use crate::error;
use crate::String;
use crate::intern;
use crate::print_cursor;
use crate::syntax;

#[derive(Debug, Default)]
pub struct TypeIntern {
    pub types: Vec<Type>,
}

impl TypeIntern {
    fn intern(&mut self, ty: Type) -> TypeId {
        for (i, interned) in self.types.iter().enumerate() {
            if ty == *interned {
                return i
            }
        }
        let i = self.types.len();
        self.types.push(ty);
        i
    }

    fn get(&self, i: TypeId) -> &Type {
        &self.types[i]
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
        for &(other, def) in &self.names {
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

pub fn build(func_decls: &[syntax::FuncDecl], func_bodys: &[syntax::FuncBody]) -> (Vec<FuncDecl>, Vec<FuncBody>, Vec<Type>) {
    let mut b = ModuleBuilder::default();

    b.add_type("i8", Type::I8);
    b.add_type("i32", Type::I32);

    for decl in func_decls {
        b.add_func_decl(decl);
    }

    let mut bodys = vec![];
    for func in func_bodys {
        let body = FuncBody {
            id: func.id,
            locals: vec![],
            stmts: vec![],
        };
        let b = FuncBuilder {
            module: &mut b,
            body: body,
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
        self.build_block(block);
        self.module.names.exit_scope(scope);

        self.body
    }

    fn build_block(&mut self, block: &syntax::Block) {
        let scope = self.module.names.enter_scope();
        for stmt in &block.stmts {
            self.build_stmt(stmt);
        }
        self.module.names.exit_scope(scope);
    }

    fn build_stmt(&mut self, stmt: &syntax::Stmt) {
        let stmt = match stmt {
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
        };
        self.body.stmts.push(stmt);
    }

    fn build_expr(&mut self, e: &syntax::Expr, env: Option<TypeId>) -> Expr {
        let (kind, ty) = match e {
            syntax::Expr::Call(func, args) => {
                // Infer function type.
                let func = self.build_expr(func, None);
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
                // Verify number of call args count.
                if fnty.var_args {
                    if args.len() < fnty.params.len() {
                        println!("var args function requires {} params, got {}",
                            fnty.params.len(), args.len());
                        error();
                    }
                } else {
                    if args.len() != fnty.params.len() {
                        println!("function has {} params, but {} args were supplied",
                            fnty.params.len(), args.len());
                        error();
                    }
                }
                // Verify arg types match inferred function type.
                let mut args2 = vec![];
                for (arg, &ty) in args.iter().zip(&fnty.params) {
                    let arg = self.build_expr(arg, Some(ty));
                    args2.push(arg);
                }
                // Var args params don't get type checked.
                for arg in &args[fnty.params.len()..] {
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
                let x = self.build_expr(x, None);
                let y = self.build_expr(y, Some(x.ty));
                let op = match op {
                    syntax::PLUS => Binop::Add,
                    syntax::MINUS => Binop::Sub,
                    _ => panic!(),
                };
                let ty = x.ty;
                (ExprKind::Binary(op, x.into(), y.into()), ty)
            }
            syntax::Expr::Integer(s) => {
                let ty = match env {
                    None => self.module.types.intern(Type::I32),
                    Some(ty) => match self.module.types.get(ty) {
                        Type::I8 | Type::I32 => ty,
                        _ => self.module.types.intern(Type::I32),
                    }
                };
                (ExprKind::Integer(*s), ty)
            },
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
                    Def::Local(i) => {
                        (ExprKind::Local(i), self.body.locals[i])
                    }
                    _ => unimplemented!(),
                },
            }
        };
        if let Some(env) = env {
            if ty != env {
                println!("expected {:?}, got {:?}",
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
                _ => panic!(),
            }
            syntax::Type::Pointer(ty) => {
                let ty = self.build_type(ty);
                self.types.intern(Type::Pointer(ty))
            }
            syntax::Type::Func(ty) => {
                unimplemented!()
            }
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
        FuncType { params, ret, var_args }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I32,
    Pointer(TypeId),
    Func(FuncType),
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
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Assign(Expr, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: TypeId,
}

#[derive(Debug, Copy, Clone)]
pub enum Binop {
    Add,
    Sub,
}

#[derive(Debug)]
pub enum ExprKind {
    Integer(String),
    Param(ParamId),
    Func(FuncId),
    Type(TypeId),
    Local(LocalId),
    Binary(Binop, Box<Expr>, Box<Expr>),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
}
