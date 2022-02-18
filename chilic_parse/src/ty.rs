use crate::{func::ParseProtoKind, *};
use chilic_error::SyntaxError;
use chilic_ir::expr::{Expr, ExprKind, StructType, StructTypeField};
use chilic_span::Span;
use chilic_token::TokenType::*;
use chilic_ty::StructTyKind;

impl Parser {
    pub(super) fn parse_decl_ty(
        &mut self,
        decl_name: Ustr,
    ) -> DiagnosticResult<Expr> {
        self.skip_redundant_tokens();
        self.decl_name_frames.push(decl_name);
        let ty = self.parse_ty()?;
        self.decl_name_frames.pop();
        Ok(ty)
    }

    pub(super) fn parse_ty(&mut self) -> DiagnosticResult<Expr> {
        if self.match_id() {
            let token = self.previous();
            let symbol = token.symbol();
            let kind = if symbol == "Self" {
                ExprKind::SelfType
            } else {
                ExprKind::Id {
                    symbol,
                    is_mutable: false,
                    entity_span: Span::empty(),
                }
            };

            Ok(Expr::new(kind, token.span.clone()))
        } else if self.match_one(Placeholder) {
            Ok(Expr::new(ExprKind::PlaceholderType, self.previous_span()))
        } else if self.match_one(Star) {
            let start_span = self.previous().span.clone();
            let is_mutable = self.match_one(Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::PointerType(Box::new(ty), is_mutable),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        } else if self.match_one(Bang) {
            Ok(Expr::new(ExprKind::NeverType, self.previous().span.clone()))
        } else if self.match_one(OpenParen) {
            if self.match_one(CloseParen) {
                Ok(Expr::new(ExprKind::UnitType, self.previous().span.clone()))
            } else {
                self.parse_struct_or_tuple_ty()
            }
        } else if self.match_one(OpenBracket) {
            self.parse_array_type()
        } else if self.match_one(Fn) {
            self.parse_fn_ty()
        } else if self.match_one(Union) {
            self.parse_struct_union_ty()
        } else {
            Err(SyntaxError::expected(self.span_ref(), "a type"))
        }
    }

    fn parse_array_type(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();

        if self.match_one(Star) {
            // multi-pointer type

            let is_mutable = self.match_one(Mut);

            self.consume(CloseBracket)?;

            let inner = self.parse_ty()?;

            let ty = Expr::new(
                ExprKind::MultiPointerType(Box::new(inner), is_mutable),
                Span::merge(&start_span, self.previous_span_ref()),
            );

            Ok(ty)
        } else if self.match_one(CloseBracket) {
            // slice type

            let is_mutable = self.match_one(Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::SliceType(Box::new(ty), is_mutable),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        } else {
            // array type or sized array literal

            let size = self.parse_expr()?;
            self.consume(CloseBracket)?;
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::ArrayType(Box::new(ty), Box::new(size)),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        }
    }

    fn parse_struct_or_tuple_ty(&mut self) -> DiagnosticResult<Expr> {
        if self.match_id() {
            if self.match_one(Colon) {
                self.revert(2);
                self.parse_struct_ty()
            } else {
                self.revert(1);
                self.parse_tuple_ty()
            }
        } else {
            self.parse_tuple_ty()
        }
    }

    fn parse_tuple_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();

        let mut tys = vec![];

        while !self.match_one(CloseParen) && !self.is_end() {
            tys.push(self.parse_ty()?);

            if self.match_one(Comma) {
                continue;
            } else if self.match_one(CloseParen) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or )"));
            }
        }

        Ok(Expr::new(
            ExprKind::TupleLiteral(tys),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    fn parse_struct_union_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let name = self.get_decl_name();

        self.consume(OpenParen)?;

        let fields = self.parse_struct_ty_fields()?;

        Ok(Expr::new(
            ExprKind::StructType(StructType {
                name,
                qualified_name: name,
                fields,
                kind: StructTyKind::Union,
            }),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    fn parse_struct_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let name = self.get_decl_name();

        let fields = self.parse_struct_ty_fields()?;

        Ok(Expr::new(
            ExprKind::StructType(StructType {
                name,
                qualified_name: name,
                fields,
                kind: StructTyKind::Struct,
            }),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }

    fn parse_struct_ty_fields(
        &mut self,
    ) -> DiagnosticResult<Vec<StructTypeField>> {
        let mut fields = vec![];

        while !self.match_one(CloseParen) && !self.is_end() {
            let id = self.consume_id()?.clone();
            let name = id.symbol();

            self.consume(Colon)?;

            let ty = self.parse_ty()?;

            fields.push(StructTypeField {
                name,
                ty: ty.clone(),
                span: id.span.clone(),
            });

            if self.match_one(Comma) {
                continue;
            } else if self.match_one(CloseParen) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or )"));
            }
        }

        Ok(fields)
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let name = self.get_decl_name();
        let proto = self.parse_fn_proto(name, ParseProtoKind::Type)?;

        Ok(Expr::new(
            ExprKind::FnType(proto),
            Span::merge(&start_span, self.previous_span_ref()),
        ))
    }
}
