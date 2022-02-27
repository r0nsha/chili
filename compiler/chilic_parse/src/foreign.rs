use crate::{func::ParseProtoKind, *};
use chilic_ast::{
    ast::{Entity, EntityKind, Expr, ExprKind, ForeignLibrary, Visibility},
    pattern::{Pattern, SymbolPattern},
};
use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_span::To;
use ustr::{ustr, Ustr};

impl Parser {
    pub(crate) fn parse_foreign_block(
        &mut self,
    ) -> DiagnosticResult<Vec<Entity>> {
        let lib_name = self.parse_lib_name()?;

        expect!(self, OpenCurly, "{")?;

        let entities = parse_delimited_list!(
            self,
            CloseCurly,
            Semicolon,
            {
                let visibility = if eat!(self, Pub) {
                    Visibility::Public
                } else {
                    Visibility::Private
                };

                expect!(self, Let, "let")?;

                self.parse_foreign_entity(lib_name, visibility)?
            },
            "; or }"
        );

        Ok(entities)
    }

    pub(crate) fn parse_foreign_single(
        &mut self,
        visibility: Visibility,
    ) -> DiagnosticResult<Entity> {
        let lib_name = self.parse_lib_name()?;
        let entity = self.parse_foreign_entity(lib_name, visibility)?;
        Ok(entity)
    }

    fn parse_foreign_entity(
        &mut self,
        lib_name: Ustr,
        visibility: Visibility,
    ) -> DiagnosticResult<Entity> {
        let id = expect!(self, Id(_), "identifier")?.clone();

        let pattern = Pattern::Single(SymbolPattern {
            symbol: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span,
            ignore: false,
        });

        let entity = if eat!(self, Eq) {
            expect!(self, Fn, "fn")?;

            let proto_start_span = self.previous().span;
            let mut proto =
                self.parse_fn_proto(id.symbol(), ParseProtoKind::Value)?;
            proto.lib_name = Some(lib_name);

            Entity::new(
                visibility,
                EntityKind::Value,
                pattern,
                None,
                Some(Expr::new(
                    ExprKind::FnType(proto),
                    proto_start_span.to(self.previous_span()),
                )),
                Some(lib_name),
            )
        } else {
            let ty_expr = self.parse_ty()?;

            Entity::new(
                visibility,
                EntityKind::Value,
                pattern,
                Some(ty_expr),
                None,
                Some(lib_name),
            )
        };

        Ok(entity)
    }

    fn parse_lib_name(&mut self) -> DiagnosticResult<Ustr> {
        expect!(self, OpenParen, "(")?;

        let lib_token = expect!(self, Str(_), "str")?;
        let lib = lib_token.symbol();

        let lib = if lib.ends_with(".lib") {
            lib.clone().to_string()
        } else {
            format!("{}.lib", lib)
        };

        let lib = ustr(&lib);

        expect!(self, CloseParen, ")")?;

        self.foreign_libraries.insert(ForeignLibrary::from_str(
            &lib,
            self.module_info.file_path,
            lib_token.span,
        )?);

        Ok(lib)
    }
}
