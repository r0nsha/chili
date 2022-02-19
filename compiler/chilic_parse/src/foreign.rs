use chilic_error::{DiagnosticResult, SyntaxError};
use chilic_ir::{
    entity::{Entity, EntityKind, Visibility},
    expr::{Expr, ExprKind},
    pattern::{Pattern, SymbolPattern},
};
use chilic_span::Span;
use ustr::{ustr, Ustr};

use crate::{func::ParseProtoKind, *};

impl Parser {
    pub(crate) fn parse_foreign_block(
        &mut self,
    ) -> DiagnosticResult<Vec<Entity>> {
        let lib_name = self.parse_lib_name()?;

        require!(self, OpenCurly, "{")?;

        let mut entitys = vec![];

        while !match_token!(self, CloseCurly) {
            let visibility = if match_token!(self, Pub) {
                Visibility::Public
            } else {
                Visibility::Private
            };

            require!(self, Let, "let")?;

            entitys.push(self.parse_foreign_entity(lib_name, visibility)?);

            if match_token!(self, Semicolon) {
                continue;
            } else if match_token!(self, CloseCurly) {
                break;
            } else {
                return Err(SyntaxError::expected(
                    self.span_ref(),
                    "newline, ; or }",
                ));
            }
        }

        Ok(entitys)
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
        let id = require!(self, Id(_), "identifier")?.clone();

        let pattern = Pattern::Single(SymbolPattern {
            symbol: id.symbol(),
            alias: None,
            is_mutable: false,
            span: id.span.clone(),
            ignore: false,
        });

        let entity = if match_token!(self, Eq) {
            require!(self, Fn, "fn")?;

            let proto_start_span = self.previous().span.clone();
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
                    Span::merge(&proto_start_span, self.previous_span_ref()),
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
        require!(self, OpenParen, "(")?;

        let lib_token = require!(self, Str(_), "str")?;
        let lib_name = lib_token.symbol();

        let lib_name = if lib_name.ends_with(".lib") {
            lib_name.clone().to_string()
        } else {
            format!("{}.lib", lib_name)
        };
        let lib_name = ustr(&lib_name);

        require!(self, CloseParen, ")")?;

        Ok(lib_name)
    }
}
