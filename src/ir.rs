use crate::error;
use crate::intern;
use crate::print_cursor;
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
    Const(ConstId),
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

pub struct Module2 {
    pub func_decls: Vec<FuncDecl>,
    pub func_bodys: Vec<FuncBody>,
    pub types: Vec<Type>,
    pub consts: Vec<Const>,
}

#[derive(Debug)]
pub struct Const {
    pub name: String,
    pub expr: Expr,
}

pub fn build(module: &syntax::Module) -> Module2 {
    let mut b = ModuleBuilder::default();

    b.add_type("i8", Type::I8);
    b.add_type("i16", Type::I16);
    b.add_type("i32", Type::I32);
    b.add_type("i64", Type::I64);
    b.add_type("f32", Type::F32);
    b.add_type("f64", Type::F64);
    b.add_type("bool", Type::Bool);

    for type_decl in &module.type_decls {
        b.add_type_decl(type_decl);
    }

    for const_decl in &module.const_decls {
        let i = b.add_const_decl(const_decl);
        b.names.def(const_decl.name, Def::Const(i));
    }

    for decl in &module.func_decls {
        b.add_func_decl(decl);
    }

    let mut bodys = vec![];
    for func in &module.func_bodys {
        let b = FuncBuilder {
            text: module.text,
            module: &mut b,
            body: FuncBody {
                id: func.id,
                locals: vec![],
                // FIXME This block is unnecessary.
                body: Block { stmts: vec![] },
            },
        };
        let body = b.build_body(&func.body);
        bodys.push(body);
    }

    Module2 {
        func_decls: b.func_decls,
        func_bodys: bodys,
        types: b.types.types,
        consts: b.consts,
    }
}

struct FuncBuilder<'a> {
    text: &'a str,
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
        let mut block2 = Block { stmts: vec![] };
        for stmt in &block.stmts {
            let stmts = self.build_stmt(stmt);
            for stmt in stmts {
                block2.stmts.push(stmt);
            }
        }
        self.module.names.exit_scope(scope);
        block2
    }

    fn build_match_expr(&mut self, pat: &syntax::Pattern, rhs: Expr) -> Option<Expr> {
        let cond = match pat {
            syntax::Pattern::Name(_) => return None,
            syntax::Pattern::Tuple(elems) => unimplemented!(),
            &syntax::Pattern::EnumVariant(name, ref elems) => {
                let variant_index = match self.module.types.get(rhs.ty) {
                    Type::Enum(ety) => ety.variant(name).unwrap().0,
                    _ => panic!(),
                };
                let pattern_tag = Expr {
                    kind: ExprKind::EnumVariant(variant_index as u32),
                    ty: self.module.types.intern(Type::I8),
                };
                let tag = Expr {
                    kind: ExprKind::EnumTag(rhs.into()),
                    ty: self.module.types.intern(Type::I8),
                };
                Expr {
                    kind: ExprKind::Binary(
                        Binop::Cmp(Predicate::Eq),
                        pattern_tag.into(),
                        tag.into(),
                    ),
                    ty: self.module.types.intern(Type::Bool),
                }
            }
        };
        Some(cond)
    }

    fn build_stmt(&mut self, stmt: &syntax::Stmt) -> Vec<Stmt> {
        let stmt = match stmt {
            syntax::Stmt::IfLet(pat, expr, body) => {
                let expr = self.build_expr(expr, None);
                let scope = self.module.names.enter_scope();
                let tmp_id = self.new_local(expr.ty);
                let tmp = Expr {
                    kind: ExprKind::Local(tmp_id),
                    ty: expr.ty,
                };
                let tmp_init = Stmt::Assign(tmp.clone(), expr.clone().into());
                let mut ret = vec![tmp_init];
                let cond = self.build_match_expr(pat, tmp.clone()).unwrap();
                let mut stmts = self.build_pattern(pat, expr.ty, Some(tmp));
                let body = self.build_block(body);
                for stmt in body.stmts {
                    stmts.push(stmt);
                }
                let body = Block { stmts };
                self.module.names.exit_scope(scope);
                ret.push(Stmt::If(cond, body));
                return ret;
            }
            syntax::Stmt::Break => Stmt::Break,
            syntax::Stmt::Continue => Stmt::Continue,
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
            syntax::Stmt::OpAssign(op, x, y) => {
                let x2 = Box::new((*x).clone());
                let y2 = Box::new((*y).clone());
                let rhs = syntax::Expr {
                    kind: syntax::ExprKind::Binary(*op, x2, y2),
                    span: (x.span.0, y.span.1),
                };
                let lhs = self.build_expr(x, None);
                let rhs = self.build_expr(&rhs, Some(lhs.ty));
                Stmt::Assign(lhs, rhs)
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
            syntax::Stmt::Let(pattern, ty, e) => {
                let ty = match ty {
                    Some(ty) => Some(self.module.build_type(ty)),
                    None => None,
                };
                let e = match e {
                    Some(e) => Some(self.build_expr(e, ty)),
                    None => None,
                };
                let ty = match (&e, ty) {
                    (_, Some(ty)) => ty,
                    (Some(e), _) => e.ty,
                    _ => panic!("let must have type or expression"),
                };

                return self.build_pattern(pattern, ty, e);
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
        vec![stmt]
    }

    fn build_tuple_pattern(
        &mut self,
        pats: &[syntax::Pattern],
        tys: &[TypeId],
        rhs: Expr,
    ) -> Vec<Stmt> {
        let mut assigns = vec![];
        assert_eq!(pats.len(), tys.len());
        for (i, (elem, &ty)) in pats.iter().zip(tys).enumerate() {
            let i = i as u32;
            let field = Expr {
                kind: ExprKind::Field(rhs.clone().into(), i),
                ty: ty,
            };
            let stmts = self.build_pattern(elem, ty, Some(field));
            for stmt in stmts {
                assigns.push(stmt);
            }
        }
        assigns
    }

    fn build_enum_pattern(
        &mut self,
        variant: u32,
        pats: &[syntax::Pattern],
        tys: &[TypeId],
        rhs: Expr,
    ) -> Vec<Stmt> {
        let mut assigns = vec![];
        assert_eq!(pats.len(), tys.len());
        for (i, (elem, &ty)) in pats.iter().zip(tys).enumerate() {
            let i = i as u32;
            let field = Expr {
                kind: ExprKind::EnumField(rhs.clone().into(), variant, i),
                ty: ty,
            };
            let stmts = self.build_pattern(elem, ty, Some(field));
            for stmt in stmts {
                assigns.push(stmt);
            }
        }
        assigns
    }

    fn build_pattern(&mut self, lhs: &syntax::Pattern, ty: TypeId, rhs: Option<Expr>) -> Vec<Stmt> {
        let local_id = self.new_local(ty);
        let local = Expr {
            kind: ExprKind::Local(local_id),
            ty: ty,
        };
        let mut assigns = vec![];
        if let Some(e) = rhs {
            assigns.push(Stmt::Assign(local.clone(), e));
        }
        match lhs {
            &syntax::Pattern::EnumVariant(name, ref elems) => {
                let (i, elem_tys) = match self.module.types.get(ty) {
                    Type::Enum(ety) => {
                        let (i, variant) = ety.variant(name).unwrap();
                        (i, variant.args.clone())
                    }
                    _ => panic!(),
                };
                let stmts = self.build_enum_pattern(i, elems, &elem_tys, local);
                for stmt in stmts {
                    assigns.push(stmt);
                }
            }
            &syntax::Pattern::Name(name) => {
                self.module.names.def(name, Def::Local(local_id));
            }
            syntax::Pattern::Tuple(elems) => {
                // Unwrap type as tuple, get element types.
                let elem_tys = match self.module.types.get(ty) {
                    Type::Tuple(elem_tys) => elem_tys.clone(),
                    _ => panic!("tuple pattern doesn't have tuple type"),
                };
                let stmts = self.build_tuple_pattern(elems, &elem_tys, local);
                for stmt in stmts {
                    assigns.push(stmt);
                }
            }
        }
        assigns
    }

    fn new_local(&mut self, ty: TypeId) -> LocalId {
        let i = self.body.locals.len();
        self.body.locals.push(ty);
        i
    }

    fn build_expr(&mut self, e: &syntax::Expr, env: Option<TypeId>) -> Expr {
        let x = self.infer_expr(e, env);
        if let Some(env) = env {
            if x.ty != env {
                let start = e.span.0 as usize;
                let end = e.span.1 as usize;
                print_cursor(self.text, start, end);
                println!(
                    "expected {:?}, got {:?}",
                    self.module.types.get(env),
                    self.module.types.get(x.ty)
                );
                error();
            }
        }
        x
    }

    fn infer_expr(&mut self, e: &syntax::Expr, env: Option<TypeId>) -> Expr {
        let (kind, ty) = match &e.kind {
            &syntax::ExprKind::TupleField(ref tuple, i) => {
                let tuple = self.build_expr(tuple, None);
                let elem_ty = match self.module.types.get(tuple.ty) {
                    Type::Tuple(elem_tys) => elem_tys[i as usize],
                    ty => {
                        let start = e.span.0 as usize;
                        let end = e.span.1 as usize;
                        print_cursor(self.text, start, end);
                        println!("expected tuple, got {:?}", ty);
                        error();
                    }
                };
                (ExprKind::Field(tuple.into(), i), elem_ty)
            }
            syntax::ExprKind::Tuple(elems) => {
                let env: Vec<TypeId> = match env {
                    Some(ty) => match self.module.types.get(ty) {
                        Type::Tuple(elems) => elems.clone(),
                        _ => vec![],
                    },
                    None => vec![],
                };
                let mut xelems = vec![];
                let mut elem_tys = vec![];
                for (i, elem) in elems.iter().enumerate() {
                    let env = env.get(i).map(|&ty| ty);
                    let elem = self.build_expr(elem, env);
                    elem_tys.push(elem.ty);
                    xelems.push(elem);
                }
                let tuple_ty = Type::Tuple(elem_tys);
                let tuple_ty = self.module.types.intern(tuple_ty);
                (ExprKind::Tuple(xelems), tuple_ty)
            }
            &syntax::ExprKind::Char(c) => {
                let i8 = self.module.types.intern(Type::I8);
                (ExprKind::Char(c), i8)
            }
            syntax::ExprKind::Null => {
                let ty = match env {
                    Some(ty) => ty,
                    None => panic!("cannot infer type of null"),
                };
                match self.module.types.get(ty) {
                    Type::Pointer(_) => {}
                    ty => panic!("expected {:?}, got null", ty),
                };
                (ExprKind::Null, ty)
            }
            syntax::ExprKind::Sizeof(ty) => {
                let ty = self.module.build_type(ty);
                let i64 = self.module.types.intern(Type::I64);
                (ExprKind::Sizeof(ty), i64)
            }
            syntax::ExprKind::Index(p, i) => {
                let p = self.build_expr(p, None);
                let i = self.build_expr(i, None);

                let ty = match self.module.types.get(p.ty) {
                    &Type::Pointer(ty) => ty,
                    &Type::Array(elem_ty, _) => elem_ty,
                    _ => panic!(),
                };
                (ExprKind::Index(p.into(), i.into()), ty)
            }
            syntax::ExprKind::Unary(op, e) => match op {
                syntax::AMPERSAND => {
                    let env = match env {
                        Some(ty) => match self.module.types.get(ty) {
                            Type::Pointer(ty) => Some(*ty),
                            _ => panic!("address-of expression should have pointer type"),
                        },
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
            },
            syntax::ExprKind::Bool(b) => {
                let bool = self.module.types.intern(Type::Bool);
                (ExprKind::Bool(*b), bool)
            }
            syntax::ExprKind::Cast(e, ty) => {
                let e = self.build_expr(e, None);
                let ty = self.module.build_type(ty);
                (ExprKind::Cast(e.into(), ty), ty)
            }
            &syntax::ExprKind::Field(ref e, field_name) => {
                let e = self.build_expr(e, None);
                match self.module.types.auto_deref(e.ty) {
                    Type::Struct(sty) => {
                        let i = match sty.field_index(field_name) {
                            Some(i) => i,
                            None => {
                                panic!("field {:?} not found on struct {:?}", field_name, sty.name)
                            }
                        };
                        let ty = sty.fields[i].1;
                        (ExprKind::Field(e.into(), i as u32), ty)
                    }
                    Type::Enum(ety) => {
                        let mut i = None;
                        for (j, variant) in ety.variants.iter().enumerate() {
                            if field_name == variant.name {
                                i = Some(j);
                                break;
                            }
                        }
                        let i = match i {
                            Some(i) => i,
                            None => panic!(
                                "enum variant {:?} not found on enum type {:?}",
                                field_name, ety.name
                            ),
                        };
                        let i = i as u32;
                        (ExprKind::EnumVariant(i), e.ty)
                    }
                    _ => panic!(),
                }
            }
            syntax::ExprKind::Struct(fields) => {
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
            syntax::ExprKind::Array(elems) => {
                let (elem_ty, n) = match env {
                    None => (None, None),
                    Some(ty) => match self.module.types.get(ty) {
                        &Type::Array(elem_ty, n) => (Some(elem_ty), Some(n)),
                        ty => panic!("expected {:?}, got {:?}", ty, e),
                    },
                };
                if let Some(n) = n {
                    let n = n as usize;
                    if elems.len() != n {
                        panic!("expected array with {:?} elems, got {:?}", n, elems.len());
                    }
                }
                let mut elems2 = vec![];
                for e in elems {
                    let e = self.build_expr(e, elem_ty);
                    elems2.push(e);
                }
                let elem_ty = match elem_ty {
                    Some(ty) => ty,
                    None => match elems2.first() {
                        Some(e) => e.ty,
                        None => panic!("cannot infer type of empty array"),
                    },
                };
                let array_ty = Type::Array(elem_ty, elems2.len() as u32);
                let array_ty = self.module.types.intern(array_ty);
                (ExprKind::Array(elems2), array_ty)
            }
            syntax::ExprKind::Unit => (ExprKind::Unit, self.module.types.intern(Type::Unit)),
            syntax::ExprKind::Call(func, args) => {
                let func = self.build_expr(func, None);
                if let ExprKind::EnumVariant(i) = func.kind {
                    let variant = match self.module.types.get(func.ty) {
                        Type::Enum(ety) => ety.variants[i as usize].clone(),
                        _ => panic!(),
                    };
                    let mut xargs = vec![];
                    if args.len() != variant.args.len() {
                        let start = e.span.0 as usize;
                        let end = e.span.1 as usize;
                        print_cursor(self.text, start, end);
                        println!(
                            "enum variant {:?} has {:?} args, got {:?}",
                            variant.name,
                            variant.args.len(),
                            args.len()
                        );
                        error();
                    }
                    for (arg, &ty) in args.iter().zip(&variant.args) {
                        let arg = self.build_expr(arg, Some(ty));
                        xargs.push(arg);
                    }
                    let kind = ExprKind::EnumCall(i, xargs);
                    let ty = func.ty;
                    return Expr { kind, ty };
                }

                let mut args2 = vec![];

                // Infer function type.
                let fnty = match self.module.types.get(func.ty) {
                    Type::Func(fnty) => fnty.clone(),
                    &Type::Pointer(ty) => match self.module.types.get(ty) {
                        Type::Func(fnty) => fnty.clone(),
                        _ => panic!(),
                    },
                    _ => panic!(),
                };
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
            syntax::ExprKind::String(s) => {
                let i8 = self.module.types.intern(Type::I8);
                let ptr_i8 = self.module.types.intern(Type::Pointer(i8));
                (ExprKind::String(*s), ptr_i8)
            }
            syntax::ExprKind::Binary(op, x, y) => {
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
                    syntax::AND => Binop::And,
                    syntax::AMPERSAND => Binop::And,
                    syntax::LSHIFT => Binop::Shl,
                    syntax::RSHIFT => Binop::Shr,
                    _ => panic!(),
                };
                let x = self.build_expr(x, None);
                let i8 = self.module.types.intern(Type::I8);
                let i32 = self.module.types.intern(Type::I32);
                let x_ty = self.module.types.get(x.ty);
                let y_ty = match (x_ty.scalar_kind(), op) {
                    (ScalarKind::Pointer, Binop::Add) => i32,
                    (ScalarKind::Pointer, Binop::Sub) => x.ty,
                    (ScalarKind::Pointer, Binop::Cmp(_)) => x.ty,
                    (ScalarKind::Pointer, op) => panic!("pointer not allowed in {:?} expr", op),
                    (ScalarKind::Int, Binop::Shl) => i8,
                    _ => x.ty,
                };
                let y = self.build_expr(y, Some(y_ty));
                let ty = match op {
                    Binop::Cmp(_) => self.module.types.intern(Type::Bool),
                    _ => x.ty,
                };
                (ExprKind::Binary(op, x.into(), y.into()), ty)
            }
            syntax::ExprKind::Float(s) => {
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
            syntax::ExprKind::Integer(s) => {
                let ty = match env {
                    None => self.module.types.intern(Type::I32),
                    Some(ty) => match self.module.types.get(ty) {
                        Type::I8 | Type::I16 | Type::I32 | Type::I64 => ty,
                        _ => self.module.types.intern(Type::I32),
                    },
                };
                (ExprKind::Integer(*s), ty)
            }
            syntax::ExprKind::Name(name) => match self.module.names.get(*name) {
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
                    Def::Type(id) => (ExprKind::Type(id), id),
                    Def::Const(i) => {
                        let ty = self.module.consts[i].expr.ty;
                        (ExprKind::Const(i), ty)
                    }
                },
            },
        };
        Expr { kind, ty }
    }
}

#[derive(Default)]
struct ModuleBuilder {
    names: NameTable,
    types: TypeIntern,
    consts: Vec<Const>,
    func_decls: Vec<FuncDecl>,
}

impl ModuleBuilder {
    fn add_type(&mut self, name: &str, ty: Type) {
        let name = intern(name);
        let i = self.types.intern(ty);
        self.names.def(name, Def::Type(i));
    }

    fn add_const_decl(&mut self, const_decl: &syntax::ConstDecl) -> ConstId {
        let ty = match &const_decl.ty {
            Some(ty) => Some(self.build_type(ty)),
            None => None,
        };
        let expr = self.check_const_expr(&const_decl.value, ty);
        let c = Const {
            name: const_decl.name,
            expr: expr,
        };
        let id = self.consts.len();
        self.consts.push(c);
        id
    }

    fn check_const_expr(&mut self, e: &syntax::Expr, ty: Option<TypeId>) -> Expr {
        let e = self.infer_const_expr(e, ty);
        if let Some(ty) = ty {
            if e.ty != ty {
                let expected = self.types.get(ty);
                let got = self.types.get(e.ty);
                println!("expected {:?}, got {:?}", expected, got);
                error();
            }
        }
        e
    }

    fn infer_const_expr(&mut self, e: &syntax::Expr, ty: Option<TypeId>) -> Expr {
        match &e.kind {
            syntax::ExprKind::Integer(s) => {
                let kind = ExprKind::Integer(*s);
                let ty = match ty {
                    None => self.types.intern(Type::I32),
                    Some(ty) => match self.types.get(ty) {
                        Type::I8 | Type::I16 | Type::I32 | Type::I64 => ty,
                        _ => self.types.intern(Type::I32),
                    },
                };
                Expr { kind, ty }
            }
            e => panic!("infer const expr {:?}", e),
        }
    }

    fn add_type_decl(&mut self, type_decl: &syntax::TypeDecl) {
        let ty = match &type_decl.kind {
            syntax::TypeDeclKind::Enum(variants) => {
                let mut xvariants = vec![];
                for variant in variants {
                    let mut args = vec![];
                    for arg in &variant.args {
                        let arg = self.build_type(arg);
                        args.push(arg);
                    }
                    let variant = EnumVariant {
                        name: variant.name,
                        args: args,
                    };
                    xvariants.push(variant);
                }
                let ty = EnumType {
                    name: type_decl.name,
                    variants: xvariants,
                };
                self.types.intern(Type::Enum(ty))
            }
            syntax::TypeDeclKind::Struct(fields) => {
                let mut fields2 = vec![];
                for (name, ty) in fields {
                    let ty = self.build_type(ty);
                    fields2.push((*name, ty));
                }
                let sty = StructType {
                    name: type_decl.name,
                    fields: fields2,
                };
                self.types.intern(Type::Struct(sty))
            }
            syntax::TypeDeclKind::Alias(ty) => self.build_type(ty),
        };
        self.names.def(type_decl.name, Def::Type(ty));
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
            syntax::Type::Tuple(elem_tys) => {
                let mut xelem_tys = vec![];
                for elem_ty in elem_tys {
                    let elem_ty = self.build_type(elem_ty);
                    xelem_tys.push(elem_ty);
                }
                let tuple = Type::Tuple(xelem_tys);
                self.types.intern(tuple)
            }
            syntax::Type::Name(name) => match self.names.get(*name) {
                Some(Def::Type(i)) => i,
                x => panic!("building type: {:?} def = {:?}", name, x),
            },
            syntax::Type::Pointer(ty) => {
                let ty = self.build_type(ty);
                self.types.intern(Type::Pointer(ty))
            }
            syntax::Type::Func(func) => {
                let mut params = vec![];
                for param in &func.params {
                    let param = self.build_type(param);
                    params.push(param);
                }
                let ret = self.build_type(&func.ret);
                let var_args = func.var_args;
                let func = FuncType {
                    params,
                    ret,
                    var_args,
                };
                let func = Type::Func(func);
                self.types.intern(func)
            }
            syntax::Type::Unit => self.types.intern(Type::Unit),
            syntax::Type::Array(n, elem_ty) => {
                let elem_ty = self.build_type(elem_ty);
                let array_ty = Type::Array(elem_ty, *n);
                self.types.intern(array_ty)
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
    Tuple(Vec<TypeId>),
    Array(TypeId, u32),
    Enum(EnumType),
    Unit,
    Bool,
}

impl Type {
    pub fn kind(&self) -> TypeKind {
        match self {
            Type::Unit => TypeKind::Unit,
            Type::I8 => TypeKind::Scalar,
            Type::I16 => TypeKind::Scalar,
            Type::I32 => TypeKind::Scalar,
            Type::I64 => TypeKind::Scalar,
            Type::F32 => TypeKind::Scalar,
            Type::F64 => TypeKind::Scalar,
            Type::Bool => TypeKind::Scalar,
            Type::Func(_) => TypeKind::Scalar,
            Type::Pointer(_) => TypeKind::Scalar,
            Type::Struct(_) => TypeKind::Aggregate,
            Type::Array(_, _) => TypeKind::Aggregate,
            Type::Tuple(_) => TypeKind::Aggregate,
            Type::Enum(_) => TypeKind::Aggregate,
        }
    }

    pub fn scalar_kind(&self) -> ScalarKind {
        match self {
            Type::Unit => panic!(),
            Type::I8 => ScalarKind::Int,
            Type::I16 => ScalarKind::Int,
            Type::I32 => ScalarKind::Int,
            Type::I64 => ScalarKind::Int,
            Type::Bool => ScalarKind::Int,
            Type::F32 => ScalarKind::Float,
            Type::F64 => ScalarKind::Float,
            Type::Func(_) => ScalarKind::Pointer,
            Type::Pointer(_) => ScalarKind::Pointer,
            Type::Struct(_) => panic!(),
            Type::Array(_, _) => panic!(),
            Type::Tuple(_) => panic!(),
            Type::Enum(_) => panic!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ScalarKind {
    Float,
    Int,
    Pointer,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeKind {
    Aggregate,
    Unit,
    Scalar,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumType {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub args: Vec<TypeId>,
}

impl EnumType {
    pub fn variant(&self, name: String) -> Option<(u32, &EnumVariant)> {
        for (i, variant) in self.variants.iter().enumerate() {
            if variant.name == name {
                let i = i as u32;
                return Some((i, variant));
            }
        }
        None
    }
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
pub type ConstId = usize;

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
    For(Vec<Stmt>, Expr, Vec<Stmt>, Block),
    Break,
    Continue,
}

#[derive(Debug, Clone)]
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
    And,
    Add,
    Sub,
    Mul,
    Div,
    Shl,
    Shr,
    Cmp(Predicate),
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Null,
    Unit,
    Integer(String),
    Float(String),
    Const(ConstId),
    Param(ParamId),
    Func(FuncId),
    Local(LocalId),
    Type(TypeId),
    Unary(Unop, Box<Expr>),
    Binary(Binop, Box<Expr>, Box<Expr>),
    String(String),
    Call(Box<Expr>, Vec<Expr>),
    Struct(Vec<(u32, Expr)>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Field(Box<Expr>, u32),
    Index(Box<Expr>, Box<Expr>),
    Cast(Box<Expr>, TypeId),
    Bool(bool),
    Char(u8),
    Sizeof(TypeId),
    EnumVariant(u32),
    EnumCall(u32, Vec<Expr>),
    // target, enum variant, field index
    EnumField(Box<Expr>, u32, u32),
    // Read enum tag from expr
    EnumTag(Box<Expr>),
}

pub fn print(module: &Module2) {
    for (i, ty) in module.types.iter().enumerate() {
        println!("type {:?} = {:?}", i, ty);
    }
    for func_decl in &module.func_decls {
        println!("{:?}", func_decl);
    }
    for func_body in &module.func_bodys {
        println!("{:?}", func_body.locals);
        for stmt in &func_body.body.stmts {
            println!("{:?}", stmt);
        }
    }
}
