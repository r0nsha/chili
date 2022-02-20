use crate::{func::ParseProtoKind, *};
use chilic_ast::expr::{Expr, ExprKind, StructType, StructTypeField};
use chilic_error::SyntaxError;
use chilic_span::{Merge, Span};
use chilic_token::TokenType::*;
use chilic_ty::StructTyKind;

const SELF_SYMBOL: &str = "Self";

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
        if match_token!(self, Id(_)) {
            let token = self.previous();
            let symbol = token.symbol();
            let kind = if symbol == SELF_SYMBOL {
                ExprKind::SelfType
            } else {
                ExprKind::Id {
                    symbol,
                    is_mutable: false,
                    entity_span: Span::unknown(),
                }
            };

            Ok(Expr::new(kind, token.span))
        } else if match_token!(self, Placeholder) {
            Ok(Expr::new(ExprKind::PlaceholderType, self.previous_span()))
        } else if match_token!(self, Star) {
            let start_span = self.previous().span;
            let is_mutable = match_token!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::PointerType(Box::new(ty), is_mutable),
                start_span.merge(self.previous_span()),
            ))
        } else if match_token!(self, Bang) {
            Ok(Expr::new(ExprKind::NeverType, self.previous().span))
        } else if match_token!(self, OpenParen) {
            if match_token!(self, CloseParen) {
                Ok(Expr::new(ExprKind::UnitType, self.previous().span))
            } else {
                self.parse_tuple_ty()
            }
        } else if match_token!(self, OpenCurly) {
            self.parse_struct_ty()
        } else if match_token!(self, OpenBracket) {
            self.parse_array_type()
        } else if match_token!(self, Fn) {
            self.parse_fn_ty()
        } else if match_token!(self, Union) {
            self.parse_struct_union_ty()
        } else {
            Err(SyntaxError::expected(self.span(), "a type"))
        }
    }

    fn parse_array_type(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        if match_token!(self, Star) {
            // multi-pointer type

            let is_mutable = match_token!(self, Mut);

            require!(self, CloseBracket, "]")?;

            let inner = self.parse_ty()?;

            let ty = Expr::new(
                ExprKind::MultiPointerType(Box::new(inner), is_mutable),
                start_span.merge(self.previous_span()),
            );

            Ok(ty)
        } else if match_token!(self, CloseBracket) {
            // slice type

            let is_mutable = match_token!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::SliceType(Box::new(ty), is_mutable),
                start_span.merge(self.previous_span()),
            ))
        } else {
            // array type or sized array literal

            let size = self.parse_expr()?;
            require!(self, CloseBracket, "]")?;
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::ArrayType(Box::new(ty), Box::new(size)),
                start_span.merge(self.previous_span()),
            ))
        }
    }

    fn parse_tuple_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;

        let tys = parse_delimited_list!(
            self,
            CloseParen,
            Comma,
            self.parse_ty()?,
            ", or )"
        );

        Ok(Expr::new(
            ExprKind::TupleLiteral(tys),
            start_span.merge(self.previous_span()),
        ))
    }

    fn parse_struct_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;
        let name = self.get_decl_name();

        let fields = self.parse_struct_ty_fields()?;

        Ok(Expr::new(
            ExprKind::StructType(StructType {
                name,
                qualified_name: name,
                fields,
                kind: StructTyKind::Struct,
            }),
            start_span.merge(self.previous_span()),
        ))
    }

    fn parse_struct_ty_fields(
        &mut self,
    ) -> DiagnosticResult<Vec<StructTypeField>> {
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id = require!(self, Id(_), "identifier")?.clone();
                let name = id.symbol();

                require!(self, Colon, ":")?;

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

        require!(self, OpenParen, "(")?;

        let fields = self.parse_struct_ty_fields()?;

        Ok(Expr::new(
            ExprKind::StructType(StructType {
                name,
                qualified_name: name,
                fields,
                kind: StructTyKind::Union,
            }),
            start_span.merge(self.previous_span()),
        ))
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span;
        let name = self.get_decl_name();
        let proto = self.parse_fn_proto(name, ParseProtoKind::Type)?;

        Ok(Expr::new(
            ExprKind::FnType(proto),
            start_span.merge(self.previous_span()),
        ))
    }
}
