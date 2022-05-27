use crate::*;
use chili_ast::{
    ast::{self, Expr, ExprKind, StructType, StructTypeField},
    ty::StructTyKind,
};
use chili_error::SyntaxError;
use chili_span::To;
use chili_token::TokenKind::*;

const SELF_SYMBOL: &str = "Self";

impl<'p> Parser<'p> {
    pub(super) fn parse_decl_ty(&mut self, decl_name: Ustr) -> DiagnosticResult<Expr> {
        self.skip_trailing_semicolons();
        self.decl_name_frames.push(decl_name);
        let ty = self.parse_ty()?;
        self.decl_name_frames.pop();
        Ok(ty)
    }

    pub(super) fn parse_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.span();

        if eat!(self, Ident(_)) {
            let token = self.previous();
            let symbol = token.symbol();
            let kind = if symbol == SELF_SYMBOL {
                ExprKind::SelfType
            } else {
                ExprKind::Ident(ast::Ident {
                    symbol,
                    binding_info_id: Default::default(),
                })
            };

            Ok(Expr::new(kind, token.span))
        } else if eat!(self, Placeholder) {
            Ok(Expr::new(ExprKind::Placeholder, self.previous_span()))
        } else if eat!(self, Star) {
            let start_span = self.previous_span();
            let is_mutable = eat!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::PointerType(ast::ExprAndMut {
                    inner: Box::new(ty),
                    is_mutable,
                }),
                start_span.to(self.previous_span()),
            ))
        } else if eat!(self, OpenParen) {
            self.parse_tuple_ty()
        } else if eat!(self, OpenBracket) {
            self.parse_array_type()
        } else if eat!(self, Fn) {
            self.parse_fn_ty()
        } else {
            Err(SyntaxError::expected(self.span(), "a type"))
        }
    }

    fn parse_array_type(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous_span();

        if eat!(self, Star) {
            // multi-pointer type

            let is_mutable = eat!(self, Mut);

            require!(self, CloseBracket, "]")?;

            let inner = self.parse_ty()?;

            let ty = Expr::new(
                ExprKind::MultiPointerType(ast::ExprAndMut {
                    inner: Box::new(inner),
                    is_mutable,
                }),
                start_span.to(self.previous_span()),
            );

            Ok(ty)
        } else if eat!(self, CloseBracket) {
            // slice type

            let is_mutable = eat!(self, Mut);
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::SliceType(ast::ExprAndMut {
                    inner: Box::new(ty),
                    is_mutable,
                }),
                start_span.to(self.previous_span()),
            ))
        } else {
            // array type or sized array literal

            let size = self.parse_expr()?;
            require!(self, CloseBracket, "]")?;
            let ty = self.parse_ty()?;

            Ok(Expr::new(
                ExprKind::ArrayType(ast::ArrayType {
                    inner: Box::new(ty),
                    size: Box::new(size),
                }),
                start_span.to(self.previous_span()),
            ))
        }
    }

    fn parse_tuple_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous_span();

        let elements = parse_delimited_list!(self, CloseParen, Comma, self.parse_ty()?, ", or )");

        Ok(Expr::new(
            ExprKind::TupleLiteral(ast::TupleLiteral { elements }),
            start_span.to(self.previous_span()),
        ))
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<Expr> {
        let start_span = self.previous_span();
        let name = self.get_decl_name();
        let sig = self.parse_fn_sig(name)?;

        Ok(Expr::new(
            ExprKind::FnType(sig),
            start_span.to(self.previous_span()),
        ))
    }
}
