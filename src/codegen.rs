use crate::ir0::*;
use crate::error;
use llvm_sys::*;
use std::ffi::CStr;
use std::mem::MaybeUninit;
use std::ptr;
use std::ops::Deref;

pub unsafe fn emit_object(
    func_decls: &[FuncDecl], 
    func_bodys: &[FuncBody],
    types: &[Type]
) {
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
    let module = LLVMModuleCreateWithName("a\0".as_ptr() as *const i8);

    let triple = LLVMGetDefaultTargetTriple();
    let mut target = MaybeUninit::uninit().assume_init();
    let mut err = MaybeUninit::uninit().assume_init();
    if LLVMGetTargetFromTriple(triple, &mut target, &mut err) != 0 {
        let err = CStr::from_ptr(err);
        println!("error getting llvm target: {:?}", err);
        error();
    }

    let cpu = "generic\0".as_ptr() as *const i8;
    let features = "\0".as_ptr() as *const i8;
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

    let mut llfuncs = vec![];
    for func_decl in func_decls {
        let lltype = build_func_type(b, types, &func_decl.ty);
        let mut name = func_decl.name.deref().to_string();
        name.push('\0');
        let name = name.as_ptr() as *const i8;
        let llfunc = LLVMAddFunction(module, name, lltype);
        llfuncs.push(llfunc);
    }
    let llfuncs = &llfuncs;
    for func_body in func_bodys {
        build_func_body(b, func_decls, types, llfuncs, func_body);
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
        "a.o\0".as_ptr() as *mut i8,
        LLVMCodeGenFileType_LLVMObjectFile,
        &mut msg,
    ) != 0
    {
        let msg = CStr::from_ptr(msg);
        println!("error emitting object file: {:?}", msg);
        error();
    }
}

unsafe fn build_type(b: LLVMBuilderRef, types: &[Type], ty: TypeId) -> LLVMTypeRef {
    match &types[ty] {
        Type::I8 => LLVMInt8Type(),
        Type::I32 => LLVMInt32Type(),
        Type::Pointer(ty) => {
            let lltype = build_type(b, types, *ty);
            LLVMPointerType(lltype, 0)
        }
        Type::Func(ty) => build_func_type(b, types, ty),
        Type::Unit => LLVMVoidType(),
        Type::Struct(ty) => {
            let mut elem_types = vec![];
            for (_, ty) in &ty.fields {
                let ty = build_type(b, types, *ty);
                elem_types.push(ty);
            }
            LLVMStructType(elem_types.as_mut_ptr(), elem_types.len() as u32, 0)
        }
    }
}

unsafe fn build_func_type(b: LLVMBuilderRef, types: &[Type], ty: &FuncType) -> LLVMTypeRef {
    let mut params = vec![];
    for ty in &ty.params {
        let lltype = build_type(b, types, *ty);
        params.push(lltype);
    }
    let ret = build_type(b, types, ty.ret);
    let var_args = if ty.var_args { 1 } else { 0 };
    LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, var_args)
}

unsafe fn build_func_body(
    b: LLVMBuilderRef,
    funcs: &[FuncDecl],
    types: &[Type],
    llfuncs: &[LLVMValueRef],
    body: &FuncBody,
) {
    let func = &funcs[body.id];
    let llfunc = llfuncs[body.id];
    let entry = "entry\0".as_ptr() as *const i8;
    let entry = LLVMAppendBasicBlock(llfunc, entry);
    LLVMPositionBuilderAtEnd(b, entry);

    let mut locals = vec![];
    for ty in &body.locals {
        let lltype = build_type(b, types, *ty);
        let name = "\0".as_ptr() as *const i8;
        let p = LLVMBuildAlloca(b, lltype, name);
        locals.push(p);
    }
    let locals = &locals;

    for stmt in &body.stmts {
        build_stmt(b, funcs, types, llfuncs, llfunc, locals, stmt);
    }
}

unsafe fn build_stmt(
    b: LLVMBuilderRef,
    funcs: &[FuncDecl],
    types: &[Type],
    llfuncs: &[LLVMValueRef],
    llfunc: LLVMValueRef,
    locals: &[LLVMValueRef],
    stmt: &Stmt,
) {
    println!("building stmt {:?}", stmt);
    match stmt {
        Stmt::Assign(x, y) => {
            let p = build_place(b, funcs, types, llfuncs, llfunc, locals, x);
            build_value_into(b, funcs, types, llfuncs, llfunc, locals, y, p);
        }
        Stmt::Return(x) => {
            let v = build_value(b, funcs, types, llfuncs, llfunc, locals, x);
            match &types[x.ty] {
                Type::Unit => LLVMBuildRetVoid(b),
                _ => LLVMBuildRet(b, v),
            };
        }
        Stmt::Expr(x) => {
            let tmp = build_alloca(b, types, x.ty);
            build_value_into(b, funcs, types, llfuncs, llfunc, locals, x, tmp);
        }
    }
}

unsafe fn build_alloca(b: LLVMBuilderRef, types: &[Type], ty: TypeId) -> LLVMValueRef {
    match &types[ty] {
        Type::Unit => LLVMGetUndef(LLVMVoidType()),
        _ => {
            let lltype = build_type(b, types, ty);
            LLVMBuildAlloca(b, lltype, "\0".as_ptr() as *const i8)
        }
    }
}

unsafe fn build_value_into(
    b: LLVMBuilderRef,
    funcs: &[FuncDecl],
    types: &[Type],
    llfuncs: &[LLVMValueRef],
    llfunc: LLVMValueRef,
    locals: &[LLVMValueRef],
    e: &Expr,
    dst: LLVMValueRef
) {
    match &types[e.ty] {
        Type::Unit => {
            let _ = build_value(b, funcs, types, llfuncs, llfunc, locals, e);
        }
        Type::Struct(sty) => match &e.kind {
            ExprKind::Struct(fields) => {
                let llsty = build_type(b, types, e.ty);
                for (i, e) in fields {
                    let dst = LLVMBuildStructGEP2(b, llsty, dst, *i as u32, "\0".as_ptr() as *const i8);
                    build_value_into(b, funcs, types, llfuncs, llfunc, locals, e, dst);
                }
            }
            _ => {
                let src = build_place(b, funcs, types, llfuncs, llfunc, locals, e);
                copy(b, types, e.ty, src, dst);
            }
        },
        _ => {
            let v = build_value(b, funcs, types, llfuncs, llfunc, locals, e);
            LLVMBuildStore(b, v, dst);
        }
    }
}

unsafe fn copy(b: LLVMBuilderRef, types: &[Type], ty: TypeId, src: LLVMValueRef, dst: LLVMValueRef) {
    match &types[ty] {
        Type::Struct(sty) => {
            let llsty = build_type(b, types, ty);
            for (i, &(_, ty)) in sty.fields.iter().enumerate() {
                let i = i as u32;
                let src = LLVMBuildStructGEP2(b, llsty, src, i, "\0".as_ptr() as *const i8);
                let dst = LLVMBuildStructGEP2(b, llsty, dst, i, "\0".as_ptr() as *const i8);
                copy(b, types, ty, src, dst);
            }
        }
        Type::Unit => {}
        _ => {
            let lltype = build_type(b, types, ty);
            let v = LLVMBuildLoad2(b, lltype, src, "\0".as_ptr() as *const i8);
            LLVMBuildStore(b, v, dst);
        }
    }
}

unsafe fn build_value(
    b: LLVMBuilderRef,
    funcs: &[FuncDecl],
    types: &[Type],
    llfuncs: &[LLVMValueRef],
    llfunc: LLVMValueRef,
    locals: &[LLVMValueRef],
    expr: &Expr
) -> LLVMValueRef {
    match &expr.kind {
        ExprKind::Field(_, _) => {
            let p = build_place(b, funcs, types, llfuncs, llfunc, locals, expr);
            let field_type = build_type(b, types, expr.ty);
            LLVMBuildLoad2(b, field_type, p, "\0".as_ptr() as *const i8)
        }
        ExprKind::Struct(fields) => panic!(),
        ExprKind::Unit => {
            LLVMGetUndef(LLVMVoidType())
        }
        ExprKind::Type(_) => unimplemented!(),
        ExprKind::Integer(s) => {
            let lltype = build_type(b, types, expr.ty);
            let i: i64 = match s.parse() {
                Err(e) => {
                    println!("unable to parse {:?} as integer: {}", s, e);
                    error();
                }
                Ok(i) => i,
            };
            LLVMConstInt(lltype, i as u64, 0)
        }
        ExprKind::Local(i) => {
            let lltype = build_type(b, types, expr.ty);
            LLVMBuildLoad2(b, lltype, locals[*i], "\0".as_ptr() as *const i8)
        }
        ExprKind::Binary(op, x, y) => {
            let x = build_value(b, funcs, types, llfuncs, llfunc, locals, x);
            let y = build_value(b, funcs, types, llfuncs, llfunc, locals, y);
            let name = "\0".as_ptr() as *const i8;
            match op {
                Binop::Add => LLVMBuildAdd(b, x, y, name),
                Binop::Sub => LLVMBuildSub(b, x, y, name),
            }
        }
        ExprKind::String(s) => {
            let mut s = unescape(s);
            s.push('\0');
            let s = s.as_ptr() as *const i8;
            let name = "\0".as_ptr() as *const i8;
            LLVMBuildGlobalStringPtr(b, s, name)
        }
        ExprKind::Call(func, args) => {
            let fnty = match &types[func.ty] {
                Type::Func(fnty) => build_func_type(b, types, fnty),
                _ => panic!(),
            };
            let func = build_value(b, funcs, types, llfuncs, llfunc, locals, func);
            let mut args2 = vec![];
            for arg in args {
                let arg = build_value(b, funcs, types, llfuncs, llfunc, locals, arg);
                args2.push(arg);
            }
            let mut args = args2;
            let name = "\0".as_ptr() as *const i8;
            LLVMBuildCall2(b, fnty, func, args.as_mut_ptr(), args.len() as u32, name)
        }
        ExprKind::Func(i) => llfuncs[*i],
        ExprKind::Param(i) => {
            LLVMGetParam(llfunc, *i as u32)
        }
    }
}

unsafe fn build_place(
    b: LLVMBuilderRef,
    funcs: &[FuncDecl],
    types: &[Type],
    llfuncs: &[LLVMValueRef],
    llfunc: LLVMValueRef,
    locals: &[LLVMValueRef],
    expr: &Expr
) -> LLVMValueRef {
    match &expr.kind {
        ExprKind::Local(i) => locals[*i],
        ExprKind::Field(x, i) => {
            let sty = build_type(b, types, x.ty);
            let p = build_place(b, funcs, types, llfuncs, llfunc, locals, x);
            LLVMBuildStructGEP2(b, sty, p, *i, "\0".as_ptr() as *const i8)
        }
        k => unimplemented!("{:?}", k),
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
                continue
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
