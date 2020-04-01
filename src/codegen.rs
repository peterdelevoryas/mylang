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
    }
}

unsafe fn build_func_type(b: LLVMBuilderRef, types: &[Type], ty: &FuncType) -> LLVMTypeRef {
    let mut params = vec![];
    for ty in &ty.params {
        let lltype = build_type(b, types, *ty);
        params.push(lltype);
    }
    let ret = build_type(b, types, ty.ret);
    LLVMFunctionType(ret, params.as_mut_ptr(), params.len() as u32, 0)
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
    match stmt {
        Stmt::Assign(x, y) => {
            let v = build_value(b, funcs, types, llfuncs, llfunc, locals, y);
            let p = build_place(b, funcs, types, llfuncs, llfunc, locals, x);
            LLVMBuildStore(b, v, p);
        }
        Stmt::Return(x) => {
            let v = build_value(b, funcs, types, llfuncs, llfunc, locals, x);
            LLVMBuildRet(b, v);
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
        _ => unimplemented!(),
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
        k => unimplemented!("{:?}", k),
    }
}
