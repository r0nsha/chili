use crate::{func::ParseProtoKind, *};
use chilic_error::SyntaxError;
use chilic_ir::expr::{Expr, ExprKind, StructType, StructTypeField};
use chilic_span::Span;
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
        if mat!(self, Id(_)) {
            let token = self.previous();
            let symbol = token.symbol();
            let kind = if symbol == SELF_SYMBOL {
                ExprKind::SelfType
            } else {
                ExprKind::Id {
                    symbol,
                    is_mutable: false,
                    entity_span: Span::empty(),
                }
            };

            Ok(Expr::new(kind, token.span.clone()))
        } else if mat!(self, Placeholder) {
            Ok(Expr::new(ExprKind::PlaceholderType, self.previous_span()))
        } else if mat!(self, Star) {
            let start_span = self.previous().span.clone();
            let is_mutable = mat!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::PointerType(Box::new(ty), is_mutable),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        } else if mat!(self, Bang) {
            Ok(Expr::new(ExprKind::NeverType, self.previous().span.clone()))
        } else if mat!(self, OpenParen) {
            if mat!(self, CloseParen) {
                Ok(Expr::new(ExprKind::UnitType, self.previous().span.clone()))
            } else {
                self.parse_tuple_ty()
            }
        } else if mat!(self, OpenCurly) {
            self.parse_struct_ty()
        } else if mat!(self, OpenBracket) {
            self.parse_array_type()
        } else if mat!(self, Fn) {
            self.parse_fn_ty()
        } else if mat!(self, Union) {
            self.parse_struct_union_ty()
        } else {
            Err(SyntaxError::expected(self.span_ref(), "a type"))
        }
    }

    fn parse_array_type(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();

        if mat!(self, Star) {
            // multi-pointer type

            let is_mutable = mat!(self, Mut);

            req!(self, CloseBracket, "]")?;

            let inner = self.parse_ty()?;

            let ty = Expr::new(
                ExprKind::MultiPointerType(Box::new(inner), is_mutable),
                Span::merge(&start_span, self.previous_span_ref()),
            );

            Ok(ty)
        } else if mat!(self, CloseBracket) {
            // slice type

            let is_mutable = mat!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::SliceType(Box::new(ty), is_mutable),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        } else {
            // array type or sized array literal

            let size = self.parse_expr()?;
            req!(self, CloseBracket, "]")?;
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::ArrayType(Box::new(ty), Box::new(size)),
                Span::merge(&start_span, self.previous_span_ref()),
            ))
        }
    }

    fn parse_tuple_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();

        let mut tys = vec![];

        while !mat!(self, CloseParen) && !self.is_end() {
            tys.push(self.parse_ty()?);

            if mat!(self, Comma) {
                continue;
            } else if mat!(self, CloseParen) {
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

        while !mat!(self, CloseCurly) && !self.is_end() {
            let id = req!(self, Id(_), "identifier")?.clone();
            let name = id.symbol();

            req!(self, Colon, ":")?;

            let ty = self.parse_ty()?;

            fields.push(StructTypeField {
                name,
                ty: ty.clone(),
                span: id.span.clone(),
            });

            if mat!(self, Comma) {
                continue;
            } else if mat!(self, CloseCurly) {
                break;
            } else {
                return Err(SyntaxError::expected(self.span_ref(), ", or }"));
            }
        }

        Ok(fields)
    }

    fn parse_struct_union_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous().span.clone();
        let name = self.get_decl_name();

        req!(self, OpenParen, "(")?;

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
