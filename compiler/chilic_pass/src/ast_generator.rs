use chilic_ast::{
    ast::{Ast, ModuleInfo, Use, Visibility},
    path::resolve_relative_path,
};
use chilic_error::DiagnosticResult;
use chilic_parse::Parser;
use chilic_span::Span;
use chilic_token::{lexer::Lexer, TokenKind};
use codespan_reporting::files::SimpleFiles;
use common::{
    compiler_info::{self, IntrinsticModuleInfo},
    Stopwatch,
};
use std::{collections::HashSet, path::PathBuf};
use unindent::unindent;
use ustr::{ustr, UstrSet};

pub struct AstGenerator<'a> {
    pub files: &'a mut SimpleFiles<String, String>,
    pub root_dir: String,
    pub root_file_id: usize,
    pub already_parsed_modules: UstrSet,
}

impl<'a> AstGenerator<'a> {
    pub fn new(
        files: &'a mut SimpleFiles<String, String>,
        root_dir: String,
    ) -> Self {
        Self {
            files,
            root_dir,
            root_file_id: 0,
            already_parsed_modules: Default::default(),
        }
    }

    pub fn start(&mut self, file_path: String) -> DiagnosticResult<Vec<Ast>> {
        let mut asts: Vec<Ast> = vec![];

        let root_file_path = resolve_relative_path(
            &file_path,
            &common::builtin::root_module(),
            None,
        )?;

        let root_module_info = ModuleInfo::new(
            common::builtin::root_module(),
            ustr(&root_file_path),
        );

        self.add_source_file(&mut asts, root_module_info, true)?;

        Ok(asts)
    }

    fn add_source_file(
        &mut self,
        asts: &mut Vec<Ast>,
        module_info: ModuleInfo,
        is_root: bool,
    ) -> DiagnosticResult<()> {
        if !self.already_parsed_modules.insert(module_info.name) {
            return Ok(());
        }

        let source = std::fs::read_to_string(module_info.file_path.as_str())
            .expect(&format!("failed to read `{}`", module_info.file_path));

        let file_id = self
            .files
            .add(module_info.file_path.to_string(), unindent(&source));

        if is_root {
            self.root_file_id = file_id;
        }

        let sw = Stopwatch::start_new("tokenize");
        let tokens = Lexer::new(file_id, &source).scan()?;
        // println!(
        //     "{:?}",
        //     tokens
        //         .iter()
        //         .map(|t| t.kind.lexeme())
        //         .collect::<Vec<&str>>()
        // );
        sw.print();

        if tokens.is_empty() || tokens.first().unwrap().is(TokenKind::Eof) {
            return Ok(());
        }

        let sw = Stopwatch::start_new("parse");

        let mut parser = Parser::new(
            tokens,
            module_info,
            self.root_dir.clone(),
            PathBuf::from(module_info.file_path.as_str())
                .parent()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
        );

        let mut parse_result = parser.parse()?;

        // implicitly add `std` to every file we parse
        add_intrinsic_std_use(&mut parse_result.ast, &mut parse_result.uses);

        for used_module in parse_result.uses.iter() {
            // TODO: This is a workaround until the parser becomes recoverable
            self.add_source_file(asts, *used_module, false)?;
        }

        sw.print();

        asts.push(parse_result.ast);

        Ok(())
    }
}

fn add_intrinsic_std_use(ast: &mut Ast, uses: &mut HashSet<ModuleInfo>) {
    add_intrinsic_module(ast, uses, compiler_info::std_module_info())
}

fn add_intrinsic_module(
    ast: &mut Ast,
    uses: &mut HashSet<ModuleInfo>,
    intrinsic_module_info: IntrinsticModuleInfo,
) {
    let intrinsic_module_info = ModuleInfo::new(
        intrinsic_module_info.name,
        intrinsic_module_info.file_path,
    );

    ast.uses.push(Use {
        module_info: intrinsic_module_info,
        alias: intrinsic_module_info.name,
        use_path: vec![],
        visibility: Visibility::Private,
        span: Span::unknown(),
    });

    uses.insert(intrinsic_module_info);
}
