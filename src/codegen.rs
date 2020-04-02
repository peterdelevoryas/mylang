use crate::error;
use crate::ir0::*;
use llvm_sys::*;
use std::ffi::CStr;
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr;

macro_rules! cstr {
    ($s:expr) => ({
        concat!($s, "\0").as_ptr() as *const i8
    })
}

pub unsafe fn emit_object(func_decls: &[FuncDecl], func_bodys: &[FuncBody], types: &[Type]) {
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
    let module = LLVMModuleCreateWithName(cstr!("a"));

    let triple = LLVMGetDefaultTargetTriple();
    let mut target = MaybeUninit::uninit().assume_init();
    let mut err = MaybeUninit::uninit().assume_init();
    if LLVMGetTargetFromTriple(triple, &mut target, &mut err) != 0 {
        let err = CStr::from_ptr(err);
        println!("error getting llvm target: {:?}", err);
        error();
    }

    let cpu = cstr!("generic");
    let features = cstr!("");
    let machine = LLVMCreateTargetMachine(
        target,
        triple,
        cpu,
        features,
        LLVMCodeGenOptLevel_LLVMCodeGenLevelNone,
        LLVMRelocMode_LLVMRelocDefault,
        LLVMCodeModel_LLVMCodeModelDefault,
    );
    let layout = LLVMCreateTargetDataLayout(machine);
    LLVMSetModuleDataLayout(module, layout);
    LLVMSetTarget(module, triple);

    let b = LLVMCreateBuilder();
    let type_bld = &TypeBuilder::new(types);

    let mut llfuncs = vec![];
    for func_decl in func_decls {
        let lltype = type_bld.func_type(&func_decl.ty);
        let mut name = func_decl.name.deref().to_string();
        name.push('\0');
        let name = name.as_ptr() as *const i8;
        let llfunc = LLVMAddFunction(module, name, lltype);
        llfuncs.push(llfunc);
    }
    let llfuncs = &llfuncs;

    for func_body in func_bodys {
        let func_decl = &func_decls[func_body.id];
        build_func_body(b, type_bld, llfuncs, func_decl, func_body);
    }

    LLVMDumpModule(module);
    let mut msg = ptr::null_mut();
    LLVMVerifyModule(
        module,
        LLVMVerifierFailureAction_LLVMAbortProcessAction,
        &mut msg,
    );
    if !msg.is_null() {
        let msg = CStr::from_ptr(msg);
        println!("verify message: {:?}", msg.to_str().unwrap());
    }
    let mut msg = ptr::null_mut();
    if LLVMTargetMachineEmitToFile(
        machine,
        module,
        cstr!("a.o") as *mut i8,
        LLVMCodeGenFileType_LLVMObjectFile,
        &mut msg,
    ) != 0
    {
        let msg = CStr::from_ptr(msg);
        println!("error emitting object file: {:?}", msg);
        error();
    }
}

struct TypeBuilder<'a> {
    lltypes: Vec<LLVMTypeRef>,
    types: &'a [Type],
}

impl<'a> TypeBuilder<'a> {
    unsafe fn new(types: &'a [Type]) -> Self {
        let mut b = TypeBuilder {
            lltypes: vec![],
            types: types,
        };
        for type_id in 0..types.len() {
            let lltype = b.build_type(type_id);
            b.lltypes.push(lltype);
        }
        b
    }

    unsafe fn build_type(&self, ty: TypeId) -> LLVMTypeRef {
        match self.irtype(ty) {
            Type::Bool => LLVMInt1Type(),
            Type::I8 => LLVMInt8Type(),
            Type::I16 => LLVMInt16Type(),
            Type::I32 => LLVMInt32Type(),
            Type::I64 => LLVMInt64Type(),
            Type::F32 => LLVMFloatType(),
            Type::F64 => LLVMDoubleType(),
            Type::Pointer(ty) => {
                let lltype = self.build_type(*ty);
                LLVMPointerType(lltype, 0)
            }
            Type::Func(ty) => self.func_type(ty),
            Type::Unit => LLVMVoidType(),
            Type::Struct(ty) => {
                let mut elem_types = vec![];
                for (_, ty) in &ty.fields {
                    let ty = self.build_type(*ty);
                    elem_types.push(ty);
                }
                LLVMStructType(elem_types.as_mut_ptr(), elem_types.len() as u32, 0)
            }
        }
    }

    fn irtype(&self, ty: TypeId) -> &'a Type {
        &self.types[ty]
    }

    fn lltype(&self, ty: TypeId) -> LLVMTypeRef {
        self.lltypes[ty]
    }

    unsafe fn func_type(&self, func: &FuncType) -> LLVMTypeRef {
        let mut params = vec![];
        for &ty in &func.params {
            let ty = match self.irtype(ty) {
                Type::Struct(_) => {
                    let sty = self.lltype(ty);
                    LLVMPointerType(sty, 0)
                }
                Type::Unit => continue,
                _ => self.lltype(ty),
            };
            params.push(ty);
        }

        let ret = match self.irtype(func.ret) {
            Type::Struct(_) => {
                let ret = self.lltype(func.ret);
                let sret = LLVMPointerType(ret, 0);
                params.push(sret);
                LLVMVoidType()
            }
            Type::Unit => LLVMVoidType(),
            _ => self.lltype(func.ret),
        };
        let var_args = if func.var_args { 1 } else { 0 };

        LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, var_args)
    }
}

unsafe fn build_func_body(
    b: LLVMBuilderRef,
    type_bld: &TypeBuilder,
    llfuncs: &[LLVMValueRef],
    func: &FuncDecl,
    body: &FuncBody,
) {
    let llfunc = llfuncs[body.id];
    let entry = LLVMAppendBasicBlock(llfunc, cstr!("entry"));
    LLVMPositionBuilderAtEnd(b, entry);

    let sret = match type_bld.irtype(func.ty.ret) {
        Type::Struct(_) => Some(LLVMGetLastParam(llfunc)),
        _ => None,
    };

    let mut locals = vec![];
    for &ty in &body.locals {
        let lltype = type_bld.lltype(ty);
        let p = LLVMBuildAlloca(b, lltype, cstr!(""));
        locals.push(p);
    }
    let locals = &locals;

    let mut b = StmtBuilder {
        bld: b,
        tybld: type_bld,

        llfuncs: llfuncs,
        llfunc: llfunc,
        locals: locals,
        sret: sret,
        block: entry,
    };
    b.build_block(&body.body);

    let term = LLVMGetBasicBlockTerminator(b.block);
    if term.is_null() {
        LLVMBuildRetVoid(b.bld);
    }
}

struct StmtBuilder<'a> {
    bld: LLVMBuilderRef,
    tybld: &'a TypeBuilder<'a>,
    llfuncs: &'a [LLVMValueRef],

    llfunc: LLVMValueRef,
    locals: &'a [LLVMValueRef],
    sret: Option<LLVMValueRef>,

    block: LLVMBasicBlockRef,
}

#[derive(Debug, Copy, Clone)]
enum Value {
    Unit,
    Scalar(LLVMValueRef),
    Struct(LLVMValueRef),
}

impl<'a> StmtBuilder<'a> {
    unsafe fn build_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.build_stmt(stmt);
        }
    }

    unsafe fn build_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::For(init, cond, post, body) => {
                self.build_stmt(init);
                let head = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                let then = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                let tail = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                let done = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                LLVMBuildBr(self.bld, head);
                LLVMPositionBuilderAtEnd(self.bld, head);
                self.block = head;
                let cond = self.build_scalar(cond);
                LLVMBuildCondBr(self.bld, cond, then, done);
                LLVMPositionBuilderAtEnd(self.bld, then);
                self.block = then;
                self.build_block(body);
                LLVMBuildBr(self.bld, tail);
                LLVMPositionBuilderAtEnd(self.bld, tail);
                self.block = tail;
                self.build_stmt(post);
                LLVMBuildBr(self.bld, head);
                LLVMPositionBuilderAtEnd(self.bld, done);
                self.block = done;
            }
            Stmt::While(cond, body) => {
                let head = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                let then = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                let done = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                LLVMBuildBr(self.bld, head);
                LLVMPositionBuilderAtEnd(self.bld, head);
                self.block = head;
                let cond = self.build_scalar(cond);
                LLVMBuildCondBr(self.bld, cond, then, done);
                LLVMPositionBuilderAtEnd(self.bld, then);
                self.block = then;
                self.build_block(body);
                LLVMBuildBr(self.bld, head);
                LLVMPositionBuilderAtEnd(self.bld, done);
                self.block = done;
            }
            Stmt::If(cond, body) => {
                let cond = self.build_scalar(cond);
                let then = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                let done = LLVMAppendBasicBlock(self.llfunc, cstr!(""));
                LLVMBuildCondBr(self.bld, cond, then, done);
                LLVMPositionBuilderAtEnd(self.bld, then);
                self.block = then;
                self.build_block(body);
                LLVMBuildBr(self.bld, done);
                LLVMPositionBuilderAtEnd(self.bld, done);
                self.block = done;
            }
            Stmt::Assign(x, y) => {
                let p = self.build_place(x);
                let _ = self.build_expr(y, Some(p));
            }
            Stmt::Return(x) => {
                let v = self.build_expr(x, self.sret);
                match v {
                    Value::Unit => LLVMBuildRetVoid(self.bld),
                    Value::Struct(_) => LLVMBuildRetVoid(self.bld),
                    Value::Scalar(v) => LLVMBuildRet(self.bld, v),
                };
            }
            Stmt::Expr(x) => {
                let _ = self.build_expr(x, None);
            }
        }
    }

    unsafe fn build_place(&mut self, e: &Expr) -> LLVMValueRef {
        match &e.kind {
            ExprKind::Local(i) => self.locals[*i],
            ExprKind::Field(x, i) => {
                let p = self.build_place(x);
                let sty = self.tybld.lltype(x.ty);
                LLVMBuildStructGEP2(self.bld, sty, p, *i, cstr!(""))
            }
            ExprKind::Param(i) => {
                let ty = self.tybld.lltype(e.ty);
                let kind = LLVMGetTypeKind(ty);
                assert_eq!(kind, LLVMTypeKind_LLVMStructTypeKind);
                LLVMGetParam(self.llfunc, *i as u32)
            }
            k => unimplemented!("build place {:?}", k),
        }
    }

    unsafe fn build_expr(&mut self, e: &Expr, dst: Option<LLVMValueRef>) -> Value {
        match self.tybld.irtype(e.ty) {
            Type::Unit => {
                self.build_unit(e);
                Value::Unit
            }
            Type::Struct(_) => {
                let p = match dst {
                    Some(p) => p,
                    None => {
                        let sty = self.tybld.lltype(e.ty);
                        LLVMBuildAlloca(self.bld, sty, cstr!(""))
                    }
                };
                self.build_struct(e, p);
                Value::Struct(p)
            }
            _ => {
                let v = self.build_scalar(e);
                if let Some(dst) = dst {
                    LLVMBuildStore(self.bld, v, dst);
                }
                Value::Scalar(v)
            }
        }
    }

    unsafe fn build_call(&mut self, func: &Expr, args: &[Expr], sret: Option<LLVMValueRef>) -> LLVMValueRef {
        let fnty = self.tybld.lltype(func.ty);
        let func = self.build_scalar(func);
        let mut args2 = vec![];
        for arg in args {
            let arg = self.build_expr(arg, None);
            let arg = match arg {
                Value::Unit => continue,
                Value::Struct(p) => p,
                Value::Scalar(v) => v,
            };
            args2.push(arg);
        }
        if let Some(sret) = sret {
            args2.push(sret);
        }
        LLVMBuildCall2(self.bld, fnty, func, args2.as_mut_ptr(), args2.len() as u32, cstr!(""))
    }

    unsafe fn build_unit(&mut self, e: &Expr) {
        match &e.kind {
            ExprKind::Unit => {}
            ExprKind::Call(func, args) => {
                let _ = self.build_call(func, args, None);
            }
            _ => panic!("expected (), got {:?}", e),
        }
    }

    unsafe fn build_struct(&mut self, e: &Expr, dst: LLVMValueRef) {
        match &e.kind {
            ExprKind::Struct(fields) => {
                let sty = self.tybld.lltype(e.ty);
                for (i, e) in fields {
                    let i = *i as u32;
                    let dst = LLVMBuildStructGEP2(self.bld, sty, dst, i, cstr!(""));
                    let _ = self.build_expr(e, Some(dst));
                }
            }
            ExprKind::Call(func, args) => {
                let _ = self.build_call(func, args, Some(dst));
            }
            ExprKind::Param(i) => {
                let i = *i as u32;
                let param = LLVMGetParam(self.llfunc, i);
                self.copy(e.ty, param, dst);
            }
            ExprKind::Unary(Unop::Deref, p) => {
                let p = self.build_scalar(p);
                self.copy(e.ty, p, dst);
            }
            _ => {
                let p = self.build_place(e);
                self.copy(e.ty, p, dst);
            }
        }
    }

    unsafe fn copy(&mut self, ty: TypeId, src: LLVMValueRef, dst: LLVMValueRef) {
        match self.tybld.irtype(ty) {
            Type::Unit => {}
            Type::Struct(sty) => {
                let llsty = self.tybld.lltype(ty);
                for (i, &(_, field_ty)) in sty.fields.iter().enumerate() {
                    let i = i as u32;
                    let src = LLVMBuildStructGEP2(self.bld, llsty, src, i, cstr!(""));
                    let dst = LLVMBuildStructGEP2(self.bld, llsty, dst, i, cstr!(""));
                    self.copy(field_ty, src, dst);
                }
            }
            _ => {
                let lltype = self.tybld.lltype(ty);
                let v = LLVMBuildLoad2(self.bld, lltype, src, cstr!(""));
                LLVMBuildStore(self.bld, v, dst);
            }
        }
    }

    unsafe fn build_scalar(&mut self, e: &Expr) -> LLVMValueRef {
        match &e.kind {
            ExprKind::Field(_, _) => {
                let p = self.build_place(e);
                let field_type = self.tybld.lltype(e.ty);
                LLVMBuildLoad2(self.bld, field_type, p, cstr!(""))
            }
            ExprKind::Float(s) => {
                let lltype = self.tybld.lltype(e.ty);
                let ptr = s.as_ptr() as *const i8;
                let len = s.len() as u32;
                LLVMConstRealOfStringAndSize(lltype, ptr, len)
            }
            ExprKind::Integer(s) => {
                let lltype = self.tybld.lltype(e.ty);
                let ptr = s.as_ptr() as *const i8;
                let len = s.len() as u32;
                let radix = 10;
                LLVMConstIntOfStringAndSize(lltype, ptr, len, radix)
            }
            ExprKind::Local(i) => {
                let lltype = self.tybld.lltype(e.ty);
                let p = self.locals[*i];
                LLVMBuildLoad2(self.bld, lltype, p, cstr!(""))
            }
            ExprKind::Param(i) => {
                LLVMGetParam(self.llfunc, *i as u32)
            }
            ExprKind::Func(i) => {
                self.llfuncs[*i]
            }
            ExprKind::Binary(op, x, y) => {
                let irty = self.tybld.irtype(x.ty);
                let float = match irty {
                    Type::I8 | Type::I16 | Type::I32 | Type::I64 => false,
                    Type::F32 | Type::F64 => true,
                    x => panic!("unexpected type {:?}", x),
                };
                let x = self.build_scalar(x);
                let y = self.build_scalar(y);
                use Predicate::*;
                match (op, float) {
                    (Binop::Add, false) => LLVMBuildAdd(self.bld, x, y, cstr!("")),
                    (Binop::Sub, false) => LLVMBuildSub(self.bld, x, y, cstr!("")),
                    (Binop::Mul, false) => LLVMBuildMul(self.bld, x, y, cstr!("")),
                    (Binop::Div, false) => LLVMBuildSDiv(self.bld, x, y, cstr!("")),

                    (Binop::Add, true) => LLVMBuildFAdd(self.bld, x, y, cstr!("")),
                    (Binop::Sub, true) => LLVMBuildFSub(self.bld, x, y, cstr!("")),
                    (Binop::Mul, true) => LLVMBuildFMul(self.bld, x, y, cstr!("")),
                    (Binop::Div, true) => LLVMBuildFDiv(self.bld, x, y, cstr!("")),

                    (Binop::Cmp(pred), float) => {
                        let pred = match (pred, float) {
                            (Eq, true) => LLVMRealPredicate_LLVMRealOEQ,
                            (Ne, true) => LLVMRealPredicate_LLVMRealONE,
                            (Ge, true) => LLVMRealPredicate_LLVMRealOGE,
                            (Le, true) => LLVMRealPredicate_LLVMRealOLE,
                            (Gt, true) => LLVMRealPredicate_LLVMRealOGT,
                            (Lt, true) => LLVMRealPredicate_LLVMRealOLT,

                            (Eq, false) => LLVMIntPredicate_LLVMIntEQ,
                            (Ne, false) => LLVMIntPredicate_LLVMIntNE,
                            (Ge, false) => LLVMIntPredicate_LLVMIntSGE,
                            (Le, false) => LLVMIntPredicate_LLVMIntSLE,
                            (Gt, false) => LLVMIntPredicate_LLVMIntSGT,
                            (Lt, false) => LLVMIntPredicate_LLVMIntSLT,
                        };
                        let cmp = if float { LLVMBuildFCmp } else { LLVMBuildICmp };
                        cmp(self.bld, pred, x, y, cstr!(""))
                    }
                }
            }
            ExprKind::String(s) => {
                let mut s = unescape(s);
                s.push('\0');
                let ptr = s.as_ptr() as *const i8;
                LLVMBuildGlobalStringPtr(self.bld, ptr, cstr!(""))
            }
            ExprKind::Call(func, args) => {
                self.build_call(func, args, None)
            }
            ExprKind::Cast(e, ty) => {
                let dst_ty = self.tybld.irtype(*ty);
                let src_ty = self.tybld.irtype(e.ty);
                let dst_llty = self.tybld.lltype(*ty);
                let v = self.build_scalar(e);
                match (src_ty, dst_ty) {
                    (Type::I8, Type::I8) | (Type::I16, Type::I16) |
                    (Type::I32, Type::I32) | (Type::I64, Type::I64) |
                    (Type::F32, Type::F32) | (Type::F64, Type::F64) => v,

                    (Type::I8, Type::I16) | (Type::I8, Type::I32) |
                    (Type::I8, Type::I64) | (Type::I16, Type::I32) |
                    (Type::I16, Type::I64) | (Type::I32, Type::I64) => {
                        LLVMBuildSExt(self.bld, v, dst_llty, cstr!(""))
                    }

                    (Type::I64, Type::I32) | (Type::I64, Type::I16) |
                    (Type::I64, Type::I8) | (Type::I32, Type::I16) |
                    (Type::I32, Type::I8) | (Type::I16, Type::I8) => {
                        LLVMBuildTrunc(self.bld, v, dst_llty, cstr!(""))
                    }

                    (Type::F32, Type::F64) => LLVMBuildFPExt(self.bld, v, dst_llty, cstr!("")),
                    (Type::F64, Type::F32) => LLVMBuildFPTrunc(self.bld, v, dst_llty, cstr!("")),

                    _ => panic!(),
                }
            }
            ExprKind::Bool(true) => LLVMConstInt(LLVMInt1Type(), 1, 0),
            ExprKind::Bool(false) => LLVMConstInt(LLVMInt1Type(), 0, 0),
            ExprKind::Unary(Unop::AddressOf, e) => {
                self.build_place(e)
            }
            ExprKind::Unary(Unop::Deref, p) => {
                let lltype = self.tybld.lltype(e.ty);
                let p = self.build_scalar(p);
                LLVMBuildLoad2(self.bld, lltype, p, cstr!(""))
            }
            _ => panic!("expected scalar, got {:?}", e),
        }
    }
}

fn unescape(s: &str) -> String {
    let s = &s[1..s.len() - 1];
    let mut x = String::with_capacity(s.len());
    let mut backslash = false;
    for c in s.chars() {
        let escaped = backslash;
        backslash = false;
        let c = match c {
            '\\' if !escaped => {
                backslash = true;
                continue;
            }
            'n' if escaped => '\n',
            't' if escaped => '\t',
            '\\' if escaped => '\\',
            _ => c,
        };
        x.push(c);
    }
    x
}
