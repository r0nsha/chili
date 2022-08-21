use super::*;
use crate::{
    ast,
    error::{
        diagnostic::{Diagnostic, Label},
        *,
    },
    span::Span,
    token::TokenKind::*,
    types::StructTypeKind,
};
use std::vec;

impl Parser {
    pub fn parse_statement(&mut self) -> DiagnosticResult<Ast> {
        let attrs = if is!(self, At) { self.parse_attrs()? } else { vec![] };
        let has_attrs = !attrs.is_empty();

        let parse_binding_result = self.try_parse_any_binding(attrs, ast::Visibility::Private, false)?;

        match parse_binding_result {
            Some(binding) => Ok(Ast::Binding(binding?)),
            None => {
                if !has_attrs {
                    self.parse_expression(true, false)
                } else {
                    Err(Diagnostic::error()
                        .with_message(format!("expected a binding, got `{}`", self.peek().kind.lexeme()))
                        .with_label(Label::primary(self.span(), "unexpected token")))
                }
            }
        }
    }

    pub fn parse_expression(&mut self, allow_assignments: bool, allow_newlines: bool) -> DiagnosticResult<Ast> {
        self.with_res(Restrictions::empty(), |p| {
            p.parse_expression_inner(allow_assignments, allow_newlines)
        })
    }

    pub fn parse_expression_res(
        &mut self,
        restrictions: Restrictions,
        allow_assignments: bool,
        allow_newlines: bool,
    ) -> DiagnosticResult<Ast> {
        self.with_res(restrictions, |p| {
            p.parse_expression_inner(allow_assignments, allow_newlines)
        })
    }

    fn parse_expression_inner(&mut self, allow_assignments: bool, allow_newlines: bool) -> DiagnosticResult<Ast> {
        let mut expr_stack: Vec<Ast> = vec![];
        let mut op_stack: Vec<ast::BinaryOp> = vec![];
        let mut last_precedence = 1000000;

        expr_stack.push(self.parse_operand()?);

        loop {
            if allow_newlines {
                if self.eof() {
                    break;
                }
                self.skip_newlines();
            } else if self.eol() {
                break;
            }

            let op = match self.parse_operator() {
                Some(op) => {
                    if op.is_assignment() && !allow_assignments {
                        break;
                    }

                    self.bump();

                    op
                }
                None => break,
            };

            let precedence = op.precedence();

            self.skip_newlines();

            let rhs = self.parse_operand()?;

            while precedence <= last_precedence && expr_stack.len() > 1 {
                let rhs = expr_stack.pop().unwrap();
                let op = op_stack.pop().unwrap();

                last_precedence = op.precedence();

                if last_precedence < precedence {
                    expr_stack.push(rhs);
                    op_stack.push(op);
                    break;
                }

                let lhs = expr_stack.pop().unwrap();

                let span = lhs.span().to(rhs.span());

                expr_stack.push(Ast::Binary(ast::Binary {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                    span,
                }));
            }

            op_stack.push(op);
            expr_stack.push(rhs);

            last_precedence = precedence;
        }

        while expr_stack.len() > 1 {
            let rhs = expr_stack.pop().unwrap();
            let op = op_stack.pop().unwrap();
            let lhs = expr_stack.pop().unwrap();

            let span = lhs.span().to(rhs.span());

            expr_stack.push(Ast::Binary(ast::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span,
            }));
        }

        Ok(expr_stack.into_iter().next().unwrap())
    }

    pub fn parse_if(&mut self) -> DiagnosticResult<Ast> {
        let token = self.previous();
        let span = token.span;

        self.skip_newlines();

        let condition = self.parse_expression_res(self.restrictions | Restrictions::NO_STRUCT_LITERAL, false, true)?;

        self.skip_newlines();

        let then = self.parse_block_expr()?;

        let last_index = self.current;

        self.skip_newlines();

        let otherwise = if eat!(self, Else) {
            let expr = if eat!(self, If) {
                self.parse_if()?
            } else {
                self.parse_block_expr()?
            };

            Some(Box::new(expr))
        } else {
            self.current = last_index;
            None
        };

        Ok(Ast::If(ast::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise,
            span: span.to(self.previous_span()),
        }))
    }

    pub fn parse_block(&mut self) -> DiagnosticResult<ast::Block> {
        let start_span = require!(self, OpenCurly, "{")?.span;

        self.skip_newlines();

        let mut statements = vec![];

        while !self.eof() {
            if eat!(self, CloseCurly) {
                return Ok(ast::Block {
                    statements,
                    span: start_span.to(self.previous_span()),
                });
            } else if eat!(self, Semicolon | Newline) {
                continue;
            } else {
                statements.push(self.parse_statement()?);

                // TODO: Recovery
                // let statement = self.parse_statement().unwrap_or_else(|diag| {
                //     let span = self.previous_span();

                //     self.cache.lock().diagnostics.push(diag);
                //     self.skip_until_recovery_point();

                //     ast::Ast::Error(ast::Empty { span })
                // });

                // statements.push(statement);
            }
        }

        Err(SyntaxError::expected(self.span(), "}"))

        // TODO: Recovery
        // self.cache
        //     .lock()
        //     .diagnostics
        //     .push(SyntaxError::expected(self.span(), "}"));

        // self.skip_until_recovery_point();

        // Ok(ast::Block {
        //     statements,
        //     yields: true,
        //     span: start_span.to(self.previous_span()),
        // })
    }

    pub fn parse_block_expr(&mut self) -> DiagnosticResult<Ast> {
        Ok(Ast::Block(self.parse_block()?))
    }

    pub fn parse_operator(&mut self) -> Option<ast::BinaryOp> {
        match &self.peek().kind {
            Plus => Some(ast::BinaryOp::Add),
            Minus => Some(ast::BinaryOp::Sub),
            Star => Some(ast::BinaryOp::Mul),
            FwSlash => Some(ast::BinaryOp::Div),
            Percent => Some(ast::BinaryOp::Rem),
            EqEq => Some(ast::BinaryOp::Eq),
            BangEq => Some(ast::BinaryOp::Ne),
            Lt => Some(ast::BinaryOp::Lt),
            LtEq => Some(ast::BinaryOp::Le),
            Gt => Some(ast::BinaryOp::Gt),
            GtEq => Some(ast::BinaryOp::Ge),
            AmpAmp => Some(ast::BinaryOp::And),
            BarBar => Some(ast::BinaryOp::Or),
            LtLt => Some(ast::BinaryOp::Shl),
            GtGt => Some(ast::BinaryOp::Shr),
            Amp => Some(ast::BinaryOp::BitAnd),
            Bar => Some(ast::BinaryOp::BitOr),
            Caret => Some(ast::BinaryOp::BitXor),
            Eq => Some(ast::BinaryOp::Assign),
            PlusEq => Some(ast::BinaryOp::AddAssign),
            MinusEq => Some(ast::BinaryOp::SubAssign),
            StarEq => Some(ast::BinaryOp::MulAssign),
            FwSlashEq => Some(ast::BinaryOp::DivAssign),
            PercentEq => Some(ast::BinaryOp::RemAssign),
            AmpAmpEq => Some(ast::BinaryOp::AndAssign),
            BarBarEq => Some(ast::BinaryOp::OrAssign),
            LtLtEq => Some(ast::BinaryOp::ShlAssign),
            GtGtEq => Some(ast::BinaryOp::ShrAssign),
            AmpEq => Some(ast::BinaryOp::BitAndAssign),
            BarEq => Some(ast::BinaryOp::BitOrAssign),
            CaretEq => Some(ast::BinaryOp::BitXorAssign),
            _ => None,
        }
    }

    pub fn parse_operand(&mut self) -> DiagnosticResult<Ast> {
        self.skip_newlines();
        let expr = self.parse_operand_base()?;
        self.parse_operand_postfix_operator(expr)
    }

    pub fn parse_operand_base(&mut self) -> DiagnosticResult<Ast> {
        if eat!(self, Ident(_)) {
            // TODO: `Self` should be a keyword
            const SYM_SELF: &str = "Self";

            let token = self.previous().clone();
            let name = token.name();

            if name == SYM_SELF {
                Ok(Ast::SelfType(ast::Empty { span: token.span }))
            } else if eat!(self, Bang) {
                self.parse_builtin(name, token.span)
            } else {
                Ok(Ast::Ident(ast::Ident { name, span: token.span }))
            }
        } else if eat!(self, Placeholder) {
            Ok(Ast::Placeholder(ast::Empty {
                span: self.previous_span(),
            }))
        } else if eat!(self, Amp) {
            let start_span = self.previous_span();

            let op = ast::UnaryOp::Ref(eat!(self, Mut));
            let value = self.parse_operand()?;

            Ok(Ast::Unary(ast::Unary {
                op,
                value: Box::new(value),
                span: start_span.to(self.previous_span()),
            }))
        } else if eat!(self, Minus) {
            let start_span = self.previous_span();

            let value = self.parse_operand()?;

            Ok(Ast::Unary(ast::Unary {
                op: ast::UnaryOp::Neg,
                value: Box::new(value),
                span: start_span.to(self.previous_span()),
            }))
        } else if eat!(self, Plus) {
            let start_span = self.previous_span();

            let value = self.parse_operand()?;

            Ok(Ast::Unary(ast::Unary {
                op: ast::UnaryOp::Plus,
                value: Box::new(value),
                span: start_span.to(self.previous_span()),
            }))
        } else if eat!(self, Bang) {
            let start_span = self.previous_span();

            let value = self.parse_operand()?;

            Ok(Ast::Unary(ast::Unary {
                op: ast::UnaryOp::Not,
                value: Box::new(value),
                span: start_span.to(self.previous_span()),
            }))
        } else if eat!(self, Import) {
            self.parse_import()
        } else if is!(self, Static) {
            Ok(Ast::StaticEval(self.parse_static_eval()?))
        } else if eat!(self, Star) {
            let start_span = self.previous_span();
            let is_mutable = eat!(self, Mut);

            let expr = self.parse_operand()?;

            Ok(Ast::PointerType(ast::PointerType {
                inner: Box::new(expr),
                is_mutable,
                span: start_span.to(self.previous_span()),
            }))
        } else if eat!(self, If) {
            self.parse_if()
        } else if eat!(self, While) {
            self.parse_while()
        } else if eat!(self, For) {
            self.parse_for()
        } else if is!(self, OpenCurly) {
            self.parse_struct_literal_or_parse_block_expr()
        } else if eat!(self, OpenBracket) {
            self.parse_array_type_or_literal()
        } else if eat!(self, Break | Continue | Return) {
            self.parse_terminator()
        } else if eat!(self, Nil | True | False | Int(_) | Float(_) | Str(_) | Char(_)) {
            self.parse_literal()
        } else if eat!(self, OpenParen) {
            let start_span = self.previous_span();

            if eat!(self, CloseParen) {
                Ok(Ast::TupleLiteral(ast::TupleLiteral {
                    elements: vec![],
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                let mut expr = self.parse_expression(false, true)?;

                if eat!(self, Comma) {
                    self.parse_tuple_literal(expr, start_span)
                } else {
                    require!(self, CloseParen, ")")?;

                    expr.span().range().start -= 1;
                    *expr.span_mut() = Span::to(&expr.span(), self.previous_span());

                    Ok(expr)
                }
            }
        } else if eat!(self, Fn) {
            self.parse_function(None, false)
        } else if eat!(self, Struct) {
            self.parse_struct_type()
        } else if eat!(self, Union) {
            self.parse_struct_union_type()
        } else {
            Err(SyntaxError::expected(
                self.span(),
                &format!("an expression, got `{}`", self.peek().kind.lexeme()),
            ))
        }
    }

    fn parse_array_type_or_literal(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        if eat!(self, CloseBracket) {
            if self.peek().kind.is_expr_start() {
                // []T
                let inner = self.parse_expression(false, true)?;

                Ok(Ast::SliceType(ast::SliceType {
                    inner: Box::new(inner),
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                // [] - empty array literal
                Ok(Ast::ArrayLiteral(ast::ArrayLiteral {
                    kind: ast::ArrayLiteralKind::List(vec![]),
                    span: start_span.to(self.previous_span()),
                }))
            }
        } else {
            let array_literal = self.parse_array_literal(start_span)?;

            match array_literal {
                Ast::ArrayLiteral(ast::ArrayLiteral {
                    kind: ast::ArrayLiteralKind::List(elements),
                    ..
                }) if elements.len() == 1 && self.peek().kind.is_expr_start() => {
                    let size = &elements[0];

                    let inner = self.parse_expression(false, true)?;

                    Ok(Ast::ArrayType(ast::ArrayType {
                        inner: Box::new(inner),
                        size: Box::new(size.clone()),
                        span: start_span.to(self.previous_span()),
                    }))
                }
                _ => Ok(array_literal),
            }
        }
    }

    fn parse_struct_type(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        let kind = if eat!(self, OpenParen) {
            const SYM_PACKED: &str = "packed";

            let token = require!(self, Ident(_), SYM_PACKED)?;

            if token.name() != SYM_PACKED {
                return Err(SyntaxError::expected(token.span, "packed"));
            }

            require!(self, CloseParen, ")")?;

            StructTypeKind::PackedStruct
        } else {
            StructTypeKind::Struct
        };

        require!(self, OpenCurly, "{")?;

        let fields = self.parse_struct_type_fields()?;

        Ok(Ast::StructType(ast::StructType {
            name: ustr(""),
            fields,
            kind,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_struct_union_type(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        require!(self, OpenCurly, "{")?;

        let fields = self.parse_struct_type_fields()?;

        Ok(Ast::StructType(ast::StructType {
            name: ustr(""),
            fields,
            kind: StructTypeKind::Union,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_struct_type_fields(&mut self) -> DiagnosticResult<Vec<ast::StructTypeField>> {
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Semicolon | Newline,
            {
                let id = require!(self, Ident(_), "an identifier")?;
                let name = id.name();

                self.skip_newlines();

                require!(self, Colon, ":")?;

                self.skip_newlines();

                let mut ty = self.parse_expression(false, false)?;
                Self::assign_expr_name_if_needed(&mut ty, name);

                ast::StructTypeField {
                    name,
                    ty,
                    span: id.span,
                }
            },
            "new line, ; or }"
        );

        Ok(fields)
    }

    fn parse_builtin(&mut self, name: Ustr, start_span: Span) -> DiagnosticResult<Ast> {
        require!(self, OpenParen, "(")?;

        let kind = match name.as_str() {
            "size_of" => ast::BuiltinKind::SizeOf(Box::new(self.parse_expression(false, true)?)),
            "align_of" => ast::BuiltinKind::AlignOf(Box::new(self.parse_expression(false, true)?)),
            name => {
                return Err(Diagnostic::error()
                    .with_message(format!("unknown builtin function `{}`", name))
                    .with_label(Label::primary(start_span, "")))
            }
        };

        require!(self, CloseParen, ")")?;

        Ok(Ast::Builtin(ast::Builtin {
            kind,
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_while(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        self.skip_newlines();

        let condition = self.parse_expression_res(self.restrictions | Restrictions::NO_STRUCT_LITERAL, false, true)?;

        self.skip_newlines();

        let block = self.parse_block()?;

        Ok(Ast::While(ast::While {
            condition: Box::new(condition),
            block,
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_for(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        let iter_ident = require!(self, Ident(_), "an identifier")?;

        self.skip_newlines();

        let iter_index_ident = if eat!(self, Comma) {
            Some(require!(self, Ident(_), "an identifier")?)
        } else {
            None
        };

        require!(self, In, "in")?;

        self.skip_newlines();

        let iter_start = self.parse_expression_res(self.restrictions | Restrictions::NO_STRUCT_LITERAL, false, true)?;

        let iterator = if eat!(self, DotDot) {
            self.skip_newlines();

            let iter_end =
                self.parse_expression_res(self.restrictions | Restrictions::NO_STRUCT_LITERAL, false, true)?;

            ast::ForIter::Range(Box::new(iter_start), Box::new(iter_end))
        } else {
            ast::ForIter::Value(Box::new(iter_start))
        };

        self.skip_newlines();

        let block = self.parse_block()?;

        Ok(Ast::For(ast::For {
            iter_binding: ast::NameAndSpan::new(iter_ident.name(), iter_ident.span),
            index_binding: iter_index_ident.map(|ident| ast::NameAndSpan::new(ident.name(), ident.span)),
            iterator,
            block,
            span: start_span.to(self.previous_span()),
        }))
    }

    pub fn parse_terminator(&mut self) -> DiagnosticResult<Ast> {
        let token = self.previous();
        let span = token.span;

        match token.kind {
            Break => Ok(Ast::Break(ast::Empty { span })),
            Continue => Ok(Ast::Continue(ast::Empty { span })),
            Return => {
                self.skip_newlines();

                let expr = match self.peek().kind {
                    Newline | CloseCurly | Eof => None,
                    _ => Some(Box::new(self.parse_expression(false, true)?)),
                };

                Ok(Ast::Return(ast::Return {
                    expr,
                    span: span.to(self.previous_span()),
                }))
            }
            _ => panic!("got an invalid terminator"),
        }
    }

    pub fn parse_static_eval(&mut self) -> DiagnosticResult<ast::StaticEval> {
        let start_span = self.previous_span();

        require!(self, Static, "static")?;

        self.skip_newlines();

        let expr = self.parse_block_expr()?;

        Ok(ast::StaticEval {
            expr: Box::new(expr),
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn assign_expr_name_if_needed(ast: &mut Ast, name: Ustr) {
        match ast {
            Ast::Function(function) => function.sig.name = Some(name),
            Ast::StructType(struct_type) => struct_type.name = name,
            Ast::FunctionType(sig) => sig.name = Some(name),
            _ => (),
        }
    }
}
