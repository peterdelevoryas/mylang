use crate::error;
use crate::intern;
use crate::print_cursor;
use crate::syntax;
use crate::String;
use clang::diagnostic::Severity;
use clang::{Clang, Entity, EntityKind, Index, TypeKind, Unsaved};
use std::convert::TryFrom;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::string::String as StdString;

#[derive(Clone, Copy)]
enum CIntType {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Clone)]
enum CType {
    Void,
    Bool,
    Int(CIntType),
    Float32,
    Float64,
    Pointer(Box<CType>),
    Array(u32, Box<CType>),
    Function(Box<CFuncType>),
    Record(usize),
}

#[derive(Clone)]
struct CFuncType {
    params: Vec<CType>,
    ret: Box<CType>,
    var_args: bool,
}

#[derive(Clone)]
struct CFunction {
    ty: CFuncType,
}

#[derive(Clone)]
struct CConstant {
    ty: CIntType,
    value_text: StdString,
}

struct CRecord {
    original_name: Option<StdString>,
    fields: Option<Vec<(StdString, CType)>>,
}

struct CImporter<'a> {
    text: &'a str,
    functions: HashMap<StdString, Result<CFunction, StdString>>,
    types: HashMap<StdString, Result<CType, StdString>>,
    constants: HashMap<StdString, Result<CConstant, StdString>>,
    records: Vec<CRecord>,
    record_ids: HashMap<StdString, usize>,
    record_in_progress: HashSet<usize>,
    generated_type_decls: Vec<syntax::TypeDecl>,
    generated_const_decls: Vec<syntax::ConstDecl>,
    generated_func_decls: Vec<syntax::FuncDecl>,
    type_names: HashMap<StdString, String>,
    const_names: HashMap<StdString, String>,
    func_names: HashMap<StdString, String>,
    record_names: HashMap<usize, String>,
}

pub fn rewrite(module: &mut syntax::Module<'_>) {
    if !module.import_libc {
        return;
    }
    if !module_uses_libc(module) {
        return;
    }

    let mut importer = CImporter::new(module.text);
    importer.rewrite_module(module);

    module.type_decls.extend(importer.generated_type_decls);
    module.const_decls.extend(importer.generated_const_decls);
    module.func_decls.extend(importer.generated_func_decls);
}

impl<'a> CImporter<'a> {
    fn new(text: &'a str) -> Self {
        const LIBC_HEADERS: &[&str] = &[
            "stddef.h",
            "stdio.h",
            "stdlib.h",
            "stdint.h",
            "stdbool.h",
            "string.h",
            "strings.h",
            "unistd.h",
            "errno.h",
            "ctype.h",
            "math.h",
            "time.h",
            "dirent.h",
            "fcntl.h",
            "sys/types.h",
            "sys/stat.h",
        ];
        let mut headers = vec![];
        let mut seen = HashSet::new();
        for &header in LIBC_HEADERS {
            let header = header.to_string();
            if seen.insert(header.clone()) {
                headers.push(header);
            }
        }

        let source = headers
            .iter()
            .map(|header| format!("#include \"{}\"\n", header))
            .collect::<StdString>();
        let path = PathBuf::from("ono_c_imports.h");
        let unsaved = Unsaved::new(&path, &source);

        let clang = match Clang::new() {
            Ok(clang) => clang,
            Err(err) => {
                println!("unable to initialize libclang for libc namespace: {}", err);
                error();
            }
        };
        let index = Index::new(&clang, false, false);
        let mut parser = index.parser(&path);
        parser.arguments(&["-xc", "-std=gnu11"]);
        parser.detailed_preprocessing_record(true);
        parser.skip_function_bodies(true);
        parser.unsaved(&[unsaved]);
        let tu = match parser.parse() {
            Ok(tu) => tu,
            Err(err) => {
                println!("unable to parse builtin libc headers: {:?}", err);
                error();
            }
        };

        let diagnostics = tu.get_diagnostics();
        let mut failed = false;
        for diagnostic in &diagnostics {
            let severity = diagnostic.get_severity();
            if severity < Severity::Error {
                continue;
            }
            failed = true;
            println!("builtin libc error: {}", diagnostic.get_text());
        }
        if failed {
            error();
        }

        let mut importer = CImporter {
            text,
            functions: HashMap::new(),
            types: HashMap::new(),
            constants: HashMap::new(),
            records: vec![],
            record_ids: HashMap::new(),
            record_in_progress: HashSet::new(),
            generated_type_decls: vec![],
            generated_const_decls: vec![],
            generated_func_decls: vec![],
            type_names: HashMap::new(),
            const_names: HashMap::new(),
            func_names: HashMap::new(),
            record_names: HashMap::new(),
        };

        for entity in tu.get_entity().get_children() {
            importer.index_entity(entity);
        }

        importer
    }

    fn rewrite_module(&mut self, module: &mut syntax::Module<'_>) {
        for decl in &mut module.const_decls {
            if let Some(ty) = &mut decl.ty {
                self.rewrite_type(ty);
            }
            self.rewrite_expr(&mut decl.value);
        }

        for decl in &mut module.type_decls {
            self.rewrite_type_decl(decl);
        }

        for decl in &mut module.func_decls {
            self.rewrite_func_decl(decl);
        }

        for body in &mut module.func_bodys {
            self.rewrite_block(&mut body.body);
        }
    }

    fn rewrite_type_decl(&mut self, decl: &mut syntax::TypeDecl) {
        match &mut decl.kind {
            syntax::TypeDeclKind::Struct(fields) => {
                for (_, ty) in fields {
                    self.rewrite_type(ty);
                }
            }
            syntax::TypeDeclKind::Enum(variants) => {
                for variant in variants {
                    for arg in &mut variant.args {
                        self.rewrite_type(arg);
                    }
                }
            }
            syntax::TypeDeclKind::Alias(ty) => self.rewrite_type(ty),
        }
    }

    fn rewrite_func_decl(&mut self, decl: &mut syntax::FuncDecl) {
        for ty in &mut decl.ty.params {
            self.rewrite_type(ty);
        }
        self.rewrite_type(&mut decl.ty.ret);
    }

    fn rewrite_block(&mut self, block: &mut syntax::Block) {
        for stmt in &mut block.stmts {
            self.rewrite_stmt(stmt);
        }
    }

    fn rewrite_stmt(&mut self, stmt: &mut syntax::Stmt) {
        match stmt {
            syntax::Stmt::Let(pattern, ty, expr) => {
                self.rewrite_pattern(pattern);
                if let Some(ty) = ty {
                    self.rewrite_type(ty);
                }
                if let Some(expr) = expr {
                    self.rewrite_expr(expr);
                }
            }
            syntax::Stmt::Return(expr) | syntax::Stmt::Expr(expr) => self.rewrite_expr(expr),
            syntax::Stmt::If(expr, block) | syntax::Stmt::While(expr, block) => {
                self.rewrite_expr(expr);
                self.rewrite_block(block);
            }
            syntax::Stmt::Assign(lhs, rhs) | syntax::Stmt::OpAssign(_, lhs, rhs) => {
                self.rewrite_expr(lhs);
                self.rewrite_expr(rhs);
            }
            syntax::Stmt::IfLet(pattern, expr, block) => {
                self.rewrite_pattern(pattern);
                self.rewrite_expr(expr);
                self.rewrite_block(block);
            }
            syntax::Stmt::For(init, cond, post, block) => {
                self.rewrite_stmt(init);
                self.rewrite_expr(cond);
                self.rewrite_stmt(post);
                self.rewrite_block(block);
            }
            syntax::Stmt::Break | syntax::Stmt::Continue => {}
        }
    }

    fn rewrite_pattern(&mut self, pattern: &mut syntax::Pattern) {
        match pattern {
            syntax::Pattern::Name(_) => {}
            syntax::Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.rewrite_pattern(pattern);
                }
            }
            syntax::Pattern::EnumVariant(_, patterns) => {
                for pattern in patterns {
                    self.rewrite_pattern(pattern);
                }
            }
        }
    }

    fn rewrite_expr(&mut self, expr: &mut syntax::Expr) {
        match &mut expr.kind {
            syntax::ExprKind::Unary(_, expr)
            | syntax::ExprKind::PostInc(expr)
            | syntax::ExprKind::TupleField(expr, _) => {
                self.rewrite_expr(expr);
            }
            syntax::ExprKind::Binary(_, lhs, rhs)
            | syntax::ExprKind::Index(lhs, rhs) => {
                self.rewrite_expr(lhs);
                self.rewrite_expr(rhs);
            }
            syntax::ExprKind::Call(func, args) => {
                self.rewrite_expr(func);
                for arg in args {
                    self.rewrite_expr(arg);
                }
            }
            syntax::ExprKind::Struct(fields) => {
                for (_, expr) in fields {
                    self.rewrite_expr(expr);
                }
            }
            syntax::ExprKind::Array(exprs) | syntax::ExprKind::Tuple(exprs) => {
                for expr in exprs {
                    self.rewrite_expr(expr);
                }
            }
            syntax::ExprKind::Field(base, field) => {
                if let syntax::ExprKind::Name(namespace) = base.kind {
                    if namespace == intern("libc") {
                        let name = *field;
                        let replacement = self.resolve_libc_value(name, expr.span);
                        expr.kind = syntax::ExprKind::Name(replacement);
                        return;
                    }
                }
                self.rewrite_expr(base);
            }
            syntax::ExprKind::Cast(expr, ty) => {
                self.rewrite_expr(expr);
                self.rewrite_type(ty);
            }
            syntax::ExprKind::Sizeof(ty) => self.rewrite_type(ty),
            syntax::ExprKind::Unit
            | syntax::ExprKind::Integer(_)
            | syntax::ExprKind::Float(_)
            | syntax::ExprKind::Name(_)
            | syntax::ExprKind::String(_)
            | syntax::ExprKind::Bool(_)
            | syntax::ExprKind::Char(_)
            | syntax::ExprKind::Null => {}
        }
    }

    fn rewrite_type(&mut self, ty: &mut syntax::Type) {
        match ty {
            syntax::Type::Unit | syntax::Type::Name(_) => {}
            syntax::Type::Path(namespace, name) => {
                if *namespace != intern("libc") {
                    println!(
                        "unsupported qualified type {:?}.{:?}; only builtin libc.* names are supported",
                        namespace,
                        name
                    );
                    error();
                }
                *ty = syntax::Type::Name(self.ensure_type(*name));
            }
            syntax::Type::Pointer(elem) => self.rewrite_type(elem),
            syntax::Type::Func(func) => {
                for param in &mut func.params {
                    self.rewrite_type(param);
                }
                self.rewrite_type(&mut func.ret);
            }
            syntax::Type::Array(_, elem) => self.rewrite_type(elem),
            syntax::Type::Tuple(elems) => {
                for elem in elems {
                    self.rewrite_type(elem);
                }
            }
        }
    }

    fn resolve_libc_value(&mut self, name: String, span: (u16, u16)) -> String {
        if self.functions.contains_key(&name.to_string()) {
            return self.ensure_function(name, span);
        }
        if self.constants.contains_key(&name.to_string()) {
            return self.ensure_constant(name, span);
        }
        if self.types.contains_key(&name.to_string()) {
            self.expr_error(span, &format!("libc.{name} is a type; use it in a type position"));
        }
        self.expr_error(span, &format!("libc does not contain symbol `{name}`"));
    }

    fn ensure_function(&mut self, name: String, span: (u16, u16)) -> String {
        if let Some(&generated) = self.func_names.get(&name.to_string()) {
            return generated;
        }
        let function = match self.functions.get(&name.to_string()) {
            Some(Ok(function)) => function.clone(),
            Some(Err(message)) => self.expr_error(span, message),
            None => self.expr_error(span, &format!("missing libc function `{name}`")),
        };
        let generated = intern(&format!("__ono_libc_func_{}", sanitize(name)));
        let params = function
            .ty
            .params
            .iter()
            .enumerate()
            .map(|(i, _)| intern(&format!("arg{i}")))
            .collect::<Vec<_>>();
        let param_types = function
            .ty
            .params
            .iter()
            .map(|ty| self.syntax_type(ty))
            .collect::<Vec<_>>();
        let ret = self.syntax_type(&function.ty.ret);
        let decl = syntax::FuncDecl {
            name: generated,
            link_name: Some(name),
            params,
            ty: syntax::FuncType {
                params: param_types,
                ret,
                var_args: function.ty.var_args,
            },
        };
        self.generated_func_decls.push(decl);
        self.func_names.insert(name.to_string(), generated);
        generated
    }

    fn ensure_constant(&mut self, name: String, span: (u16, u16)) -> String {
        if let Some(&generated) = self.const_names.get(&name.to_string()) {
            return generated;
        }
        let constant = match self.constants.get(&name.to_string()) {
            Some(Ok(constant)) => constant.clone(),
            Some(Err(message)) => self.expr_error(span, message),
            None => self.expr_error(span, &format!("missing libc constant `{name}`")),
        };
        let generated = intern(&format!("__ono_libc_const_{}", sanitize(name)));
        let ty = int_syntax_type(constant.ty);
        let value = syntax::Expr {
            kind: syntax::ExprKind::Integer(intern(&constant.value_text)),
            span: (0, 0),
        };
        let decl = syntax::ConstDecl {
            name: generated,
            ty: Some(ty),
            value,
        };
        self.generated_const_decls.push(decl);
        self.const_names.insert(name.to_string(), generated);
        generated
    }

    fn ensure_type(&mut self, name: String) -> String {
        if let Some(&generated) = self.type_names.get(&name.to_string()) {
            return generated;
        }
        let ty = match self.types.get(&name.to_string()) {
            Some(Ok(ty)) => ty.clone(),
            Some(Err(message)) => {
                println!("{}", message);
                error();
            }
            None => {
                println!("missing libc type `{name}`");
                error();
            }
        };

        let generated = intern(&format!("__ono_libc_type_{}", sanitize(name)));
        let alias = syntax::TypeDecl {
            name: generated,
            kind: syntax::TypeDeclKind::Alias(self.syntax_type(&ty)),
        };
        self.generated_type_decls.push(alias);
        self.type_names.insert(name.to_string(), generated);
        generated
    }

    fn ensure_record(&mut self, id: usize) -> String {
        if let Some(&generated) = self.record_names.get(&id) {
            return generated;
        }
        let suffix = match &self.records[id].original_name {
            Some(name) => sanitize_str(name),
            None => format!("anon_{:x}", hash_u64(&id)),
        };
        let generated = intern(&format!("__ono_libc_record_{suffix}"));
        self.record_names.insert(id, generated);

        let fields = match self.records[id].fields.clone() {
            Some(fields) => fields
                .into_iter()
                .map(|(name, ty)| (intern(&name), self.syntax_type(&ty)))
                .collect::<Vec<_>>(),
            None => vec![],
        };
        let decl = syntax::TypeDecl {
            name: generated,
            kind: syntax::TypeDeclKind::Struct(fields),
        };
        self.generated_type_decls.push(decl);
        generated
    }

    fn syntax_type(&mut self, ty: &CType) -> syntax::Type {
        match ty {
            CType::Void => syntax::Type::Unit,
            CType::Bool => syntax::Type::Name(intern("bool")),
            CType::Int(int_ty) => int_syntax_type(*int_ty),
            CType::Float32 => syntax::Type::Name(intern("f32")),
            CType::Float64 => syntax::Type::Name(intern("f64")),
            CType::Pointer(elem) => {
                let elem = match elem.as_ref() {
                    CType::Void => syntax::Type::Name(intern("i8")),
                    elem => self.syntax_type(elem),
                };
                syntax::Type::Pointer(Box::new(elem))
            }
            CType::Array(n, elem) => syntax::Type::Array(*n, Box::new(self.syntax_type(elem))),
            CType::Function(func) => syntax::Type::Func(
                syntax::FuncType {
                    params: func.params.iter().map(|param| self.syntax_type(param)).collect(),
                    ret: self.syntax_type(&func.ret),
                    var_args: func.var_args,
                }
                .into(),
            ),
            CType::Record(id) => syntax::Type::Name(self.ensure_record(*id)),
        }
    }

    fn index_entity<'tu>(&mut self, entity: Entity<'tu>) {
        match entity.get_kind() {
            EntityKind::FunctionDecl => self.index_function(entity),
            EntityKind::TypedefDecl => self.index_typedef(entity),
            EntityKind::StructDecl => self.index_record_decl(entity),
            EntityKind::UnionDecl => self.index_union_decl(entity),
            EntityKind::EnumDecl => self.index_enum_decl(entity),
            EntityKind::MacroDefinition => self.index_macro(entity),
            _ => {}
        }
    }

    fn index_function<'tu>(&mut self, entity: Entity<'tu>) {
        let Some(name) = entity.get_name() else {
            return;
        };
        if name.starts_with("__builtin_") || entity.is_inline_function() {
            return;
        }
        let result = (|| {
            let ret = match entity.get_result_type() {
                Some(ty) => self.import_type(ty)?,
                None => CType::Void,
            };
            let params = match entity.get_arguments() {
                Some(args) => {
                    let mut params = vec![];
                    for arg in args {
                        let ty = match arg.get_type() {
                            Some(ty) => ty,
                            None => return Err(format!("unable to read parameter type for `{name}`")),
                        };
                        params.push(self.import_type(ty)?);
                    }
                    params
                }
                None => vec![],
            };
            Ok(CFunction {
                ty: CFuncType {
                    params,
                    ret: Box::new(ret),
                    var_args: entity.is_variadic(),
                },
            })
        })();
        remember(&mut self.functions, name, result);
    }

    fn index_typedef<'tu>(&mut self, entity: Entity<'tu>) {
        let Some(name) = entity.get_name() else {
            return;
        };
        let result = match entity.get_typedef_underlying_type() {
            Some(ty) => self.import_type(ty),
            None => Err(format!("unable to resolve typedef `{name}`")),
        };
        remember(&mut self.types, name, result);
    }

    fn index_record_decl<'tu>(&mut self, entity: Entity<'tu>) {
        let Some(name) = entity.get_name() else {
            let _ = self.import_record(entity);
            return;
        };
        let result = self.import_record(entity).map(CType::Record);
        remember(&mut self.types, name, result);
    }

    fn index_union_decl<'tu>(&mut self, entity: Entity<'tu>) {
        let Some(name) = entity.get_name() else {
            return;
        };
        remember(
            &mut self.types,
            name,
            Err("C unions are not supported yet".into()),
        );
    }

    fn index_enum_decl<'tu>(&mut self, entity: Entity<'tu>) {
        let underlying = match entity.get_enum_underlying_type() {
            Some(ty) => self.import_int_type(ty),
            None => Err("unable to resolve enum underlying type".into()),
        };

        if let Some(name) = entity.get_name() {
            remember(
                &mut self.types,
                name,
                underlying.clone().map(CType::Int),
            );
        }

        for child in entity.get_children() {
            if child.get_kind() != EntityKind::EnumConstantDecl {
                continue;
            }
            let Some(name) = child.get_name() else {
                continue;
            };
            let result = (|| {
                let int_ty = underlying.clone()?;
                let (signed, unsigned) = match child.get_enum_constant_value() {
                    Some(value) => value,
                    None => return Err(format!("unable to evaluate enum constant `{name}`")),
                };
                let value_text = signed_int_text(int_ty, signed, unsigned)?;
                Ok(CConstant { ty: int_ty, value_text })
            })();
            remember(&mut self.constants, name, result);
        }
    }

    fn index_macro<'tu>(&mut self, entity: Entity<'tu>) {
        if entity.is_builtin_macro() || entity.is_function_like_macro() {
            return;
        }
        let Some(name) = entity.get_name() else {
            return;
        };
        let result = self.import_macro(entity);
        remember(&mut self.constants, name, result);
    }

    fn import_macro<'tu>(&self, entity: Entity<'tu>) -> Result<CConstant, StdString> {
        let Some(name) = entity.get_name() else {
            return Err("macro has no name".into());
        };
        let range = match entity.get_range() {
            Some(range) => range,
            None => return Err(format!("unable to tokenize macro `{name}`")),
        };
        let mut tokens = range
            .tokenize()
            .into_iter()
            .map(|token| token.get_spelling())
            .collect::<Vec<_>>();

        if tokens.first().map(|token| token.as_str()) == Some("#") {
            tokens.remove(0);
        }
        if tokens.first().map(|token| token.as_str()) == Some("define") {
            tokens.remove(0);
        }
        if tokens.first().map(|token| token.as_str()) == Some(name.as_str()) {
            tokens.remove(0);
        }

        strip_outer_parens(&mut tokens);
        let value = parse_macro_integer_tokens(&tokens)
            .ok_or_else(|| format!("unsupported macro `{name}`; only simple integer macros are supported"))?;
        let ty = fit_int_type(value)
            .ok_or_else(|| format!("macro `{name}` does not fit into Ono integer types"))?;
        Ok(CConstant {
            ty,
            value_text: value.to_string(),
        })
    }

    fn import_record<'tu>(&mut self, entity: Entity<'tu>) -> Result<usize, StdString> {
        if entity.get_kind() == EntityKind::UnionDecl {
            return Err("C unions are not supported yet".into());
        }

        let key = entity_key(entity);
        if let Some(&id) = self.record_ids.get(&key) {
            if self.records[id].fields.is_none() && entity.is_definition() {
                if self.record_in_progress.contains(&id) {
                    return Ok(id);
                }
                self.record_in_progress.insert(id);
                let fields = self.import_record_fields(entity);
                self.record_in_progress.remove(&id);
                if let Ok(fields) = fields {
                    self.records[id].fields = Some(fields);
                }
                if self.records[id].original_name.is_none() {
                    self.records[id].original_name = entity.get_name();
                }
            }
            return Ok(id);
        }

        let id = self.records.len();
        self.record_ids.insert(key, id);
        self.records.push(CRecord {
            original_name: entity.get_name(),
            fields: None,
        });

        if entity.is_definition() {
            self.record_in_progress.insert(id);
            let fields = self.import_record_fields(entity);
            self.record_in_progress.remove(&id);
            if let Ok(fields) = fields {
                self.records[id].fields = Some(fields);
            }
        }

        Ok(id)
    }

    fn import_record_fields<'tu>(
        &mut self,
        entity: Entity<'tu>,
    ) -> Result<Vec<(StdString, CType)>, StdString> {
        let mut fields = vec![];
        for child in entity.get_children() {
            if child.get_kind() != EntityKind::FieldDecl {
                continue;
            }
            if child.is_bit_field() {
                return Err("bit-fields are not supported yet".into());
            }
            let name = child
                .get_name()
                .ok_or_else(|| "anonymous struct fields are not supported yet".to_string())?;
            let ty = child
                .get_type()
                .ok_or_else(|| format!("unable to read field type for `{name}`"))?;
            let ty = self.import_type(ty)?;
            fields.push((name, ty));
        }
        Ok(fields)
    }

    fn import_type<'tu>(&mut self, ty: clang::Type<'tu>) -> Result<CType, StdString> {
        let ty = desugar_type(ty);
        match ty.get_kind() {
            TypeKind::Void => Ok(CType::Void),
            TypeKind::Bool => Ok(CType::Bool),
            TypeKind::CharS
            | TypeKind::CharU
            | TypeKind::SChar
            | TypeKind::UChar
            | TypeKind::WChar
            | TypeKind::Char16
            | TypeKind::Char32
            | TypeKind::Short
            | TypeKind::UShort
            | TypeKind::Int
            | TypeKind::UInt
            | TypeKind::Long
            | TypeKind::ULong
            | TypeKind::LongLong
            | TypeKind::ULongLong
            | TypeKind::Int128
            | TypeKind::UInt128 => self.import_int_type(ty).map(CType::Int),
            TypeKind::Float => Ok(CType::Float32),
            TypeKind::Double => Ok(CType::Float64),
            TypeKind::LongDouble => Err("long double is not supported yet".into()),
            TypeKind::Pointer => {
                let pointee = ty
                    .get_pointee_type()
                    .ok_or_else(|| format!("unable to read pointee type for `{}`", ty.get_display_name()))?;
                let pointee = desugar_type(pointee);
                if pointee.get_kind() == TypeKind::Void {
                    return Ok(CType::Pointer(Box::new(CType::Int(CIntType::I8))));
                }
                Ok(CType::Pointer(Box::new(self.import_type(pointee)?)))
            }
            TypeKind::FunctionPrototype | TypeKind::FunctionNoPrototype => {
                let params = ty
                    .get_argument_types()
                    .unwrap_or_default()
                    .into_iter()
                    .map(|arg| self.import_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret = match ty.get_result_type() {
                    Some(ret) => self.import_type(ret)?,
                    None => CType::Void,
                };
                Ok(CType::Function(Box::new(CFuncType {
                    params,
                    ret: Box::new(ret),
                    var_args: ty.is_variadic(),
                })))
            }
            TypeKind::ConstantArray => {
                let n = ty
                    .get_size()
                    .ok_or_else(|| format!("unable to determine array length for `{}`", ty.get_display_name()))?;
                let n = match u32::try_from(n) {
                    Ok(n) => n,
                    Err(_) => return Err(format!("array `{}` is too large for Ono", ty.get_display_name())),
                };
                let elem = ty
                    .get_element_type()
                    .ok_or_else(|| format!("unable to determine array element type for `{}`", ty.get_display_name()))?;
                Ok(CType::Array(n, Box::new(self.import_type(elem)?)))
            }
            TypeKind::IncompleteArray
            | TypeKind::VariableArray
            | TypeKind::DependentSizedArray => {
                Err(format!("incomplete array type `{}` is not supported", ty.get_display_name()))
            }
            TypeKind::Record => {
                let decl = ty
                    .get_declaration()
                    .ok_or_else(|| format!("unable to resolve record type `{}`", ty.get_display_name()))?;
                Ok(CType::Record(self.import_record(decl)?))
            }
            TypeKind::Enum => self.import_enum_type(ty).map(CType::Int),
            _ => Err(format!("unsupported C type `{}`", ty.get_display_name())),
        }
    }

    fn import_enum_type<'tu>(&mut self, ty: clang::Type<'tu>) -> Result<CIntType, StdString> {
        let decl = ty
            .get_declaration()
            .ok_or_else(|| format!("unable to resolve enum type `{}`", ty.get_display_name()))?;
        let underlying = decl
            .get_enum_underlying_type()
            .ok_or_else(|| format!("unable to read enum underlying type for `{}`", ty.get_display_name()))?;
        self.import_int_type(underlying)
    }

    fn import_int_type<'tu>(&self, ty: clang::Type<'tu>) -> Result<CIntType, StdString> {
        let size = ty
            .get_sizeof()
            .map_err(|_| format!("unable to determine size of `{}`", ty.get_display_name()))?;
        match size {
            1 => Ok(CIntType::I8),
            2 => Ok(CIntType::I16),
            4 => Ok(CIntType::I32),
            8 => Ok(CIntType::I64),
            _ => Err(format!(
                "unsupported integer width {} for `{}`",
                size * 8,
                ty.get_display_name()
            )),
        }
    }

    fn expr_error(&self, span: (u16, u16), message: &str) -> ! {
        let start = span.0 as usize;
        let end = span.1 as usize;
        if start < end {
            print_cursor(self.text, start, end);
        }
        println!("{}", message);
        error();
    }
}

fn remember<T>(
    map: &mut HashMap<StdString, Result<T, StdString>>,
    name: StdString,
    result: Result<T, StdString>,
) {
    let replace = match map.get(&name) {
        None => true,
        Some(Ok(_)) => false,
        Some(Err(_)) => result.is_ok(),
    };
    if replace {
        map.insert(name, result);
    }
}

fn module_uses_libc(module: &syntax::Module<'_>) -> bool {
    for decl in &module.const_decls {
        if decl.ty.as_ref().is_some_and(type_uses_libc) || expr_uses_libc(&decl.value) {
            return true;
        }
    }
    for decl in &module.type_decls {
        if type_decl_uses_libc(decl) {
            return true;
        }
    }
    for decl in &module.func_decls {
        if func_decl_uses_libc(decl) {
            return true;
        }
    }
    for body in &module.func_bodys {
        if block_uses_libc(&body.body) {
            return true;
        }
    }
    false
}

fn type_decl_uses_libc(decl: &syntax::TypeDecl) -> bool {
    match &decl.kind {
        syntax::TypeDeclKind::Struct(fields) => fields.iter().any(|(_, ty)| type_uses_libc(ty)),
        syntax::TypeDeclKind::Enum(variants) => variants
            .iter()
            .any(|variant| variant.args.iter().any(type_uses_libc)),
        syntax::TypeDeclKind::Alias(ty) => type_uses_libc(ty),
    }
}

fn func_decl_uses_libc(decl: &syntax::FuncDecl) -> bool {
    decl.ty.params.iter().any(type_uses_libc) || type_uses_libc(&decl.ty.ret)
}

fn block_uses_libc(block: &syntax::Block) -> bool {
    block.stmts.iter().any(stmt_uses_libc)
}

fn stmt_uses_libc(stmt: &syntax::Stmt) -> bool {
    match stmt {
        syntax::Stmt::Let(_, ty, expr) => ty.as_ref().is_some_and(type_uses_libc)
            || expr.as_ref().is_some_and(expr_uses_libc),
        syntax::Stmt::Return(expr) | syntax::Stmt::Expr(expr) => expr_uses_libc(expr),
        syntax::Stmt::If(expr, block) | syntax::Stmt::While(expr, block) => {
            expr_uses_libc(expr) || block_uses_libc(block)
        }
        syntax::Stmt::Assign(lhs, rhs) | syntax::Stmt::OpAssign(_, lhs, rhs) => {
            expr_uses_libc(lhs) || expr_uses_libc(rhs)
        }
        syntax::Stmt::IfLet(_, expr, block) => expr_uses_libc(expr) || block_uses_libc(block),
        syntax::Stmt::For(init, cond, post, block) => {
            stmt_uses_libc(init) || expr_uses_libc(cond) || stmt_uses_libc(post) || block_uses_libc(block)
        }
        syntax::Stmt::Break | syntax::Stmt::Continue => false,
    }
}

fn expr_uses_libc(expr: &syntax::Expr) -> bool {
    match &expr.kind {
        syntax::ExprKind::Unit
        | syntax::ExprKind::Integer(_)
        | syntax::ExprKind::Float(_)
        | syntax::ExprKind::Name(_)
        | syntax::ExprKind::String(_)
        | syntax::ExprKind::Bool(_)
        | syntax::ExprKind::Char(_)
        | syntax::ExprKind::Null => false,
        syntax::ExprKind::Unary(_, expr)
        | syntax::ExprKind::PostInc(expr)
        | syntax::ExprKind::TupleField(expr, _) => expr_uses_libc(expr),
        syntax::ExprKind::Binary(_, lhs, rhs) | syntax::ExprKind::Index(lhs, rhs) => {
            expr_uses_libc(lhs) || expr_uses_libc(rhs)
        }
        syntax::ExprKind::Call(func, args) => expr_uses_libc(func) || args.iter().any(expr_uses_libc),
        syntax::ExprKind::Struct(fields) => fields.iter().any(|(_, expr)| expr_uses_libc(expr)),
        syntax::ExprKind::Array(exprs) | syntax::ExprKind::Tuple(exprs) => exprs.iter().any(expr_uses_libc),
        syntax::ExprKind::Field(base, _) => {
            matches!(base.kind, syntax::ExprKind::Name(name) if name == intern("libc")) || expr_uses_libc(base)
        }
        syntax::ExprKind::Cast(expr, ty) => expr_uses_libc(expr) || type_uses_libc(ty),
        syntax::ExprKind::Sizeof(ty) => type_uses_libc(ty),
    }
}

fn type_uses_libc(ty: &syntax::Type) -> bool {
    match ty {
        syntax::Type::Unit | syntax::Type::Name(_) => false,
        syntax::Type::Path(namespace, _) => *namespace == intern("libc"),
        syntax::Type::Pointer(elem) => type_uses_libc(elem),
        syntax::Type::Func(func) => {
            func.params.iter().any(type_uses_libc) || type_uses_libc(&func.ret)
        }
        syntax::Type::Array(_, elem) => type_uses_libc(elem),
        syntax::Type::Tuple(elems) => elems.iter().any(type_uses_libc),
    }
}

fn int_syntax_type(ty: CIntType) -> syntax::Type {
    let name = match ty {
        CIntType::I8 => "i8",
        CIntType::I16 => "i16",
        CIntType::I32 => "i32",
        CIntType::I64 => "i64",
    };
    syntax::Type::Name(intern(name))
}

fn entity_key<'tu>(entity: Entity<'tu>) -> StdString {
    entity
        .get_usr()
        .map(|usr| usr.0)
        .or_else(|| entity.get_name())
        .unwrap_or_else(|| format!("{:?}", entity.get_location()))
}

fn desugar_type<'tu>(mut ty: clang::Type<'tu>) -> clang::Type<'tu> {
    loop {
        match ty.get_kind() {
            TypeKind::Typedef | TypeKind::Unexposed => {
                let next = ty.get_canonical_type();
                if next.get_kind() == ty.get_kind() {
                    return ty;
                }
                ty = next;
            }
            TypeKind::Elaborated => match ty.get_elaborated_type() {
                Some(next) => ty = next,
                None => ty = ty.get_canonical_type(),
            },
            _ => return ty,
        }
    }
}

fn sanitize(name: String) -> StdString {
    sanitize_str(&name.to_string())
}

fn sanitize_str(name: &str) -> StdString {
    let mut out = StdString::with_capacity(name.len());
    for c in name.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() {
        "anon".to_string()
    } else {
        out
    }
}

fn hash_u64<T: Hash>(value: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn signed_int_text(ty: CIntType, signed: i64, unsigned: u64) -> Result<StdString, StdString> {
    let signed = signed as i128;
    let unsigned = unsigned as i128;
    let (min, max) = signed_range(ty);
    if min <= signed && signed <= max {
        return Ok(signed.to_string());
    }
    if 0 <= unsigned && unsigned <= max {
        return Ok(unsigned.to_string());
    }
    Err("enum constant does not fit imported integer type".into())
}

fn signed_range(ty: CIntType) -> (i128, i128) {
    match ty {
        CIntType::I8 => (i8::MIN as i128, i8::MAX as i128),
        CIntType::I16 => (i16::MIN as i128, i16::MAX as i128),
        CIntType::I32 => (i32::MIN as i128, i32::MAX as i128),
        CIntType::I64 => (i64::MIN as i128, i64::MAX as i128),
    }
}

fn fit_int_type(value: i128) -> Option<CIntType> {
    if (i8::MIN as i128..=i8::MAX as i128).contains(&value) {
        return Some(CIntType::I8);
    }
    if (i16::MIN as i128..=i16::MAX as i128).contains(&value) {
        return Some(CIntType::I16);
    }
    if (i32::MIN as i128..=i32::MAX as i128).contains(&value) {
        return Some(CIntType::I32);
    }
    if (i64::MIN as i128..=i64::MAX as i128).contains(&value) {
        return Some(CIntType::I64);
    }
    None
}

fn strip_outer_parens(tokens: &mut Vec<StdString>) {
    loop {
        if tokens.len() < 2 {
            return;
        }
        if tokens.first().map(|token| token.as_str()) != Some("(") {
            return;
        }
        if tokens.last().map(|token| token.as_str()) != Some(")") {
            return;
        }
        let mut depth = 0i32;
        let mut wrapped = true;
        for (i, token) in tokens.iter().enumerate() {
            match token.as_str() {
                "(" => depth += 1,
                ")" => depth -= 1,
                _ => {}
            }
            if depth == 0 && i + 1 != tokens.len() {
                wrapped = false;
                break;
            }
        }
        if !wrapped {
            return;
        }
        tokens.remove(0);
        tokens.pop();
    }
}

fn parse_macro_integer_tokens(tokens: &[StdString]) -> Option<i128> {
    match tokens {
        [value] => parse_c_integer_literal(value),
        [sign, value] if sign == "-" || sign == "+" => {
            let value = parse_c_integer_literal(value)?;
            if sign == "-" {
                Some(-value)
            } else {
                Some(value)
            }
        }
        _ => None,
    }
}

fn parse_c_integer_literal(token: &str) -> Option<i128> {
    let end = token.trim_end_matches(|c: char| matches!(c, 'u' | 'U' | 'l' | 'L' | 'z' | 'Z'));
    if end.is_empty() {
        return None;
    }
    let (radix, digits) = if let Some(rest) = end.strip_prefix("0x").or_else(|| end.strip_prefix("0X")) {
        (16, rest)
    } else if end.len() > 1 && end.starts_with('0') {
        (8, &end[1..])
    } else {
        (10, end)
    };
    i128::from_str_radix(digits, radix).ok()
}
