use std::path::PathBuf;

use chilic_error::DiagnosticResult;
use chilic_ir::{
    entity::Visibility,
    item::{Item, ItemKind, Items},
    module::ModuleInfo,
    path::resolve_relative_path,
    use_decl::UseDecl,
};
use chilic_parse::{Parser, ParserResult};
use chilic_span::Span;
use chilic_token::{lexer::Lexer, TokenType};
use codespan_reporting::files::SimpleFiles;
use common::{
    compiler_info::{self, IntrinsticModuleInfo},
    Stopwatch,
};
use ustr::{ustr, UstrSet};

pub struct IrGen<'a> {
    pub files: &'a mut SimpleFiles<String, String>,
    pub root_dir: String,
    pub root_file_id: usize,
    pub already_parsed_modules: UstrSet,
}

impl<'a> IrGen<'a> {
    pub fn gen(&mut self, file_path: String) -> DiagnosticResult<Items> {
        let mut items = vec![];

        let file_path = resolve_relative_path(&file_path, "", None)?;
        self.add_root_source_file(&mut items, file_path)?;

        Ok(items)
    }

    fn add_root_source_file(
        &mut self,
        all_items: &mut Items,
        file_path: String,
    ) -> DiagnosticResult<()> {
        self.add_source_file_internal(
            all_items,
            ModuleInfo::new(common::builtin::root_module(), ustr(&file_path)),
            true,
        )
    }

    fn add_source_file(
        &mut self,
        all_items: &mut Items,
        module_info: ModuleInfo,
    ) -> DiagnosticResult<()> {
        self.add_source_file_internal(all_items, module_info, false)
    }

    fn add_source_file_internal(
        &mut self,
        all_items: &mut Items,
        module_info: ModuleInfo,
        is_root: bool,
    ) -> DiagnosticResult<()> {
        if !self.already_parsed_modules.insert(module_info.name) {
            return Ok(());
        }

        let source = std::fs::read_to_string(module_info.file_path.as_str())
            .expect(&format!("failed to read `{}`", module_info.file_path));

        let file_id = self.files.add(
            module_info.file_path.to_string(),
            unindent::unindent(&source),
        );

        if is_root {
            self.root_file_id = file_id;
        }

        let sw = Stopwatch::start_new("tokenize");
        let tokens = Lexer::new(file_id, &source).scan()?;
        // println!(
        //     "{:?}",
        //     tokens
        //         .iter()
        //         .map(|t| t.token_type.lexeme())
        //         .collect::<Vec<&str>>()
        // );
        sw.print();

        if tokens.is_empty() || tokens.first().unwrap().is(TokenType::Eof) {
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

        let ParserResult {
            mut items,
            mut uses,
        } = parser.parse()?;

        // implicitly add `std` to every file we parse
        // TODO: if `std` is already added, don't add it!
        add_intrinsic_std_use(&mut items, &mut uses, module_info);

        for used_module in uses {
            self.add_source_file(all_items, used_module)?;
        }

        sw.print();

        all_items.extend(items);

        Ok(())
    }
}

fn add_intrinsic_std_use(
    items: &mut Items,
    uses: &mut Vec<ModuleInfo>,
    module_info: ModuleInfo,
) {
    add_intrinsic_module(
        items,
        uses,
        module_info,
        compiler_info::std_module_info(),
    )
}

fn add_intrinsic_module(
    items: &mut Items,
    uses: &mut Vec<ModuleInfo>,
    module_info: ModuleInfo,
    intrinsic_module_info: IntrinsticModuleInfo,
) {
    let intrinsic_module_info = ModuleInfo::new(
        intrinsic_module_info.name,
        intrinsic_module_info.file_path,
    );

    items.push(Item::new(
        module_info,
        ItemKind::UseDecl(UseDecl {
            module_info: intrinsic_module_info,
            alias: intrinsic_module_info.name,
            use_path: vec![],
            visibility: Visibility::Private,
            span: Span::empty(),
        }),
        Span::empty(),
    ));

    uses.push(intrinsic_module_info);
}
