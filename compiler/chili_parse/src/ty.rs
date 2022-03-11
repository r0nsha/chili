use crate::{func::ParseProtoKind, *};
use chili_ast::ast::{Expr, ExprKind, StructType, StructTypeField};
use chili_ast::ty::StructTyKind;
use chili_error::SyntaxError;
use chili_span::{Span, To};
use chili_token::TokenKind::*;

const SELF_SYMBOL: &str = "Self";

impl<'w> Parser<'w> {
    pub(super) fn parse_decl_ty(&mut self, decl_name: Ustr) -> DiagnosticResult<Expr> {
        self.skip_redundant_tokens();
        self.decl_name_frames.push(decl_name);
        let ty = self.parse_ty()?;
        self.decl_name_frames.pop();
        Ok(ty)
    }

    pub(super) fn parse_ty(&mut self) -> DiagnosticResult<Expr> {
        if eat!(self, Id(_)) {
            let token = self.previous();
            let symbol = token.symbol();
            let kind = if symbol == SELF_SYMBOL {
                ExprKind::SelfType
            } else {
                ExprKind::Id {
                    symbol,
                    is_mutable: false,
                    binding_span: Span::unknown(),
                    binding_info_idx: Default::default(),
                }
            };

            Ok(Expr::new(kind, token.span))
        } else if eat!(self, Placeholder) {
            Ok(Expr::new(ExprKind::PlaceholderType, self.previous_span()))
        } else if eat!(self, Star) {
            let start_span = self.previous().span;
            let is_mutable = eat!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::PointerType(Box::new(ty), is_mutable),
                start_span.to(self.previous_span()),
            ))
        } else if eat!(self, Bang) {
            Ok(Expr::new(ExprKind::NeverType, self.previous().span))
        } else if eat!(self, OpenParen) {
            if eat!(self, CloseParen) {
                Ok(Expr::new(ExprKind::UnitType, self.previous().span))
            } else {
                self.parse_tuple_ty()
            }
        } else if eat!(self, OpenCurly) {
            self.parse_struct_ty()
        } else if eat!(self, OpenBracket) {
            self.parse_array_type()
        } else if eat!(self, Fn) {
            self.parse_fn_ty()
        } else if eat!(self, Union) {
            self.parse_struct_union_ty()
        } else {
            Err(SyntaxError::expected(self.span(), "a type"))
        }
    }

    fn parse_array_type(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        if eat!(self, Star) {
            // multi-pointer type

            let is_mutable = eat!(self, Mut);

            expect!(self, CloseBracket, "]")?;

            let inner = self.parse_ty()?;

            let ty = Expr::new(
                ExprKind::MultiPointerType(Box::new(inner), is_mutable),
                start_span.to(self.previous_span()),
            );

            Ok(ty)
        } else if eat!(self, CloseBracket) {
            // slice type

            let is_mutable = eat!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::SliceType(Box::new(ty), is_mutable),
                start_span.to(self.previous_span()),
            ))
        } else {
            // array type or sized array literal

            let size = self.parse_expr()?;
            expect!(self, CloseBracket, "]")?;
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::ArrayType(Box::new(ty), Box::new(size)),
                start_span.to(self.previous_span()),
            ))
        }
    }

    fn parse_tuple_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        let tys = parse_delimited_list!(self, CloseParen, Comma, self.parse_ty()?, ", or )");

        Ok(Expr::new(
            ExprKind::TupleLiteral(tys),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_struct_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;
        let name = self.get_decl_name();

        let fields = self.parse_struct_ty_fields()?;

        Ok(Expr::new(
            ExprKind::StructType(StructType {
                name,
                fields,
                kind: StructTyKind::Struct,
                binding_info_idx: Default::default(),
            }),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_struct_ty_fields(&mut self) -> DiagnosticResult<Vec<StructTypeField>> {
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id = expect!(self, Id(_), "identifier")?.clone();
                let name = id.symbol();

                expect!(self, Colon, ":")?;

                let ty = self.parse_ty()?;

                StructTypeField {
                    name,
                    ty: ty.clone(),
                    span: id.span,
                }
            },
            ", or }"
        );

        Ok(fields)
    }

    fn parse_struct_union_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;
        let name = self.get_decl_name();

        expect!(self, OpenParen, "(")?;

        let fields = self.parse_struct_ty_fields()?;

        Ok(Expr::new(
            ExprKind::StructType(StructType {
                name,
                fields,
                kind: StructTyKind::Union,
                binding_info_idx: Default::default(),
            }),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;
        let name = self.get_decl_name();
        let proto = self.parse_fn_proto(name, ParseProtoKind::Type)?;

        Ok(Expr::new(
            ExprKind::FnType(proto),
            start_span.to(self.previous_span()),
        ))
    }
}
