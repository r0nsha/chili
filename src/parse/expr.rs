use super::*;
use crate::{
    ast::{self, Ast, BinaryOp, Block, BuiltinKind, ForIter, NameAndSpan, UnaryOp, Visibility},
    error::{
        diagnostic::{Diagnostic, Label},
        *,
    },
    span::{Span, To},
    token::TokenKind::*,
    types::StructTypeKind,
};
use std::vec;
use ustr::ustr;

macro_rules! parse_binary {
    ($parser:expr, pattern = $(|) ? $($pattern : pat_param) | +, next_fn = $next:expr) => {{
        let mut expr = $next($parser)?;
        let start_span = expr.span();

        while eat!($parser, $( $pattern )|+) {
            let op: BinaryOp = $parser.previous().kind.into();
            let rhs = $next($parser)?;
            let span = start_span.to($parser.previous_span());

            expr = Ast::Binary(ast::Binary {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                    span,
                }
            );
        }

        Ok(expr)
    }};
}

impl Parser {
    pub fn parse_expr(&mut self) -> DiagnosticResult<Ast> {
        self.with_res(Restrictions::empty(), |p| p.parse_expr_inner(ustr("")))
    }

    pub fn parse_expr_with_res(&mut self, restrictions: Restrictions) -> DiagnosticResult<Ast> {
        self.with_res(restrictions, |p| p.parse_expr_inner(ustr("")))
    }

    pub fn parse_decl_expr(&mut self, decl_name: Ustr) -> DiagnosticResult<Ast> {
        self.with_res(Restrictions::empty(), |p| p.parse_expr_inner(decl_name))
    }

    fn parse_expr_inner(&mut self, decl_name: Ustr) -> DiagnosticResult<Ast> {
        let is_stmt = self.restrictions.contains(Restrictions::STMT_EXPR);

        self.decl_name_frames.push(decl_name);

        let expr = if is_stmt {
            if eat!(self, Let) {
                let start_span = self.previous_span();

                let binding = if eat!(self, Extern) {
                    self.parse_extern(Visibility::Private, start_span)?
                } else {
                    self.parse_binding(Visibility::Private)?
                };

                Ok(Ast::Binding(binding))
            } else {
                self.parse_logic_or()
            }
        } else {
            self.parse_logic_or()
        }?;

        let expr = self.parse_postfix_expr(expr)?;

        self.decl_name_frames.pop();

        Ok(expr)
    }

    pub fn parse_if(&mut self) -> DiagnosticResult<Ast> {
        let token = self.previous();
        let span = token.span;

        let condition = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        require!(self, OpenCurly, "{")?;
        let then = self.parse_block_expr()?;

        let otherwise = if eat!(self, Else) {
            let expr = if eat!(self, If) {
                self.parse_if()?
            } else {
                require!(self, OpenCurly, "{")?;
                self.parse_block_expr()?
            };

            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Ast::If(ast::If {
            condition: Box::new(condition),
            then: Box::new(then),
            otherwise,
            span: span.to(self.previous_span()),
        }))
    }

    pub fn parse_block(&mut self) -> DiagnosticResult<Block> {
        let start_span = self.previous_span();

        let mut exprs = vec![];
        let mut yields = false;

        while !eat!(self, CloseCurly) && !self.is_end() {
            self.skip_semicolons();

            let expr = self
                .parse_expr_with_res(Restrictions::STMT_EXPR)
                .unwrap_or_else(|diag| {
                    let span = self.previous_span();

                    self.cache.lock().diagnostics.push(diag);
                    self.skip_until_recovery_point();

                    ast::Ast::Error(ast::Empty { span })
                });

            exprs.push(expr);

            if eat!(self, Semicolon) {
                yields = false;
                continue;
            } else if eat!(self, CloseCurly) {
                yields = true;
                break;
            } else if exprs.last().map_or(false, ast_doesnt_require_semicolon) {
                continue;
            } else {
                let span = Parser::get_missing_delimiter_span(self.previous_span());
                self.cache
                    .lock()
                    .diagnostics
                    .push(SyntaxError::expected(span, "; or }"));
                self.skip_until_recovery_point();
            }
        }

        Ok(Block {
            statements: exprs,
            yields,
            span: start_span.to(self.previous_span()),
        })
    }

    pub fn parse_block_expr(&mut self) -> DiagnosticResult<Ast> {
        Ok(Ast::Block(self.parse_block()?))
    }

    pub fn parse_logic_or(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = BarBar, next_fn = Parser::parse_logic_and)
    }

    pub fn parse_logic_and(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = AmpAmp, next_fn = Parser::parse_comparison)
    }

    pub fn parse_comparison(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(
            self,
            pattern = BangEq | EqEq | Gt | GtEq | Lt | LtEq,
            next_fn = Parser::parse_bitwise_or
        )
    }

    pub fn parse_bitwise_or(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = Bar, next_fn = Parser::parse_bitwise_xor)
    }

    pub fn parse_bitwise_xor(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = Caret, next_fn = Parser::parse_bitwise_and)
    }

    pub fn parse_bitwise_and(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = Amp, next_fn = Parser::parse_bitshift)
    }

    pub fn parse_bitshift(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = LtLt | GtGt, next_fn = Parser::parse_term)
    }

    pub fn parse_term(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(self, pattern = Minus | Plus, next_fn = Parser::parse_factor)
    }

    pub fn parse_factor(&mut self) -> DiagnosticResult<Ast> {
        parse_binary!(
            self,
            pattern = Star | FwSlash | Percent,
            next_fn = Parser::parse_unary
        )
    }

    pub fn parse_unary(&mut self) -> DiagnosticResult<Ast> {
        if eat!(self, Amp | AmpAmp | Bang | Minus | Plus) {
            let start_span = self.previous_span();
            let token = self.previous().kind;

            let op = match token {
                Amp => UnaryOp::Ref(eat!(self, Mut)),
                Star => UnaryOp::Deref,
                Minus => UnaryOp::Neg,
                Plus => UnaryOp::Plus,
                Bang => UnaryOp::Not,
                t => panic!("{} is not a unary op", t),
            };

            let lhs = self.parse_unary()?;

            let span = start_span.to(self.previous_span());

            let expr = Ast::Unary(ast::Unary {
                op,
                value: Box::new(lhs),
                span,
            });

            Ok(expr)
        } else {
            self.parse_primary()
        }
    }

    pub fn parse_primary(&mut self) -> DiagnosticResult<Ast> {
        let expr = if eat!(self, Ident(_)) {
            const SELF_SYMBOL: &str = "Self";

            let token = self.previous().clone();
            let symbol = token.name();

            if symbol == SELF_SYMBOL {
                Ast::SelfType(ast::Empty { span: token.span })
            } else if eat!(self, Bang) {
                self.parse_builtin(symbol, token.span)?
            } else {
                Ast::Ident(ast::Ident {
                    name: symbol,
                    span: token.span,
                })
            }
        } else if eat!(self, Placeholder) {
            Ast::Placeholder(ast::Empty {
                span: self.previous_span(),
            })
        } else if eat!(self, Star) {
            let start_span = self.previous_span();
            let is_mutable = eat!(self, Mut);
            let expr = self.parse_expr()?;

            Ast::PointerType(ast::ExprAndMut {
                inner: Box::new(expr),
                is_mutable,
                span: start_span.to(self.previous_span()),
            })
        } else if eat!(self, If) {
            self.parse_if()?
        } else if eat!(self, While) {
            self.parse_while()?
        } else if eat!(self, For) {
            self.parse_for()?
        } else if eat!(self, OpenCurly) {
            self.parse_block_expr()?
        } else if eat!(self, OpenBracket) {
            self.parse_array_type_or_literal()?
        } else if eat!(self, Dot) {
            let start_span = self.previous_span();

            if eat!(self, OpenCurly) {
                // anonymous struct literal
                self.parse_struct_literal(None, start_span)?
            } else {
                return Err(SyntaxError::expected(
                    self.span(),
                    &format!("[ or {{, got `{}`", self.peek().lexeme),
                ));
            }
        } else if eat!(self, Break | Continue | Return) {
            self.parse_terminator()?
        } else if eat!(
            self,
            Nil | True | False | Int(_) | Float(_) | Str(_) | Char(_)
        ) {
            self.parse_literal()?
        } else if eat!(self, OpenParen) {
            let start_span = self.previous_span();

            if eat!(self, CloseParen) {
                Ast::TupleLiteral(ast::TupleLiteral {
                    elements: vec![],
                    span: start_span.to(self.previous_span()),
                })
            } else {
                let mut expr = self.parse_expr()?;

                if eat!(self, Comma) {
                    self.parse_tuple_literal(expr, start_span)?
                } else {
                    require!(self, CloseParen, ")")?;

                    expr.span().range().start -= 1;
                    *expr.span_mut() = Span::to(&expr.span(), self.previous_span());

                    expr
                }
            }
        } else if eat!(self, Fn) {
            let name = self.get_decl_name();
            self.parse_function(name, None)?
        } else if eat!(self, Struct) {
            self.parse_struct_type()?
        } else if eat!(self, Union) {
            self.parse_struct_union_type()?
        } else {
            return Err(SyntaxError::expected(
                self.span(),
                &format!("an expression, got `{}`", self.peek().lexeme),
            ));
        };

        self.parse_postfix_expr(expr)
    }

    fn parse_array_type_or_literal(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();

        if eat!(self, Star) {
            if eat!(self, Mut) {
                // [*mut]T
                require!(self, CloseBracket, "]")?;

                let inner = self.parse_expr()?;

                Ok(Ast::MultiPointerType(ast::ExprAndMut {
                    inner: Box::new(inner),
                    is_mutable: true,
                    span: start_span.to(self.previous_span()),
                }))
            } else if eat!(self, CloseBracket) {
                // [*]T
                let inner = self.parse_expr()?;

                Ok(Ast::MultiPointerType(ast::ExprAndMut {
                    inner: Box::new(inner),
                    is_mutable: false,
                    span: start_span.to(self.previous_span()),
                }))
            } else {
                // [...array literal]
                self.revert(1);
                self.parse_array_literal(start_span)
            }
        } else if eat!(self, CloseBracket) {
            // Note (Ron): syntax for mut slices will probably be removed once we have unsized types

            if eat!(self, Mut) {
                // []mut T
                let inner = self.parse_expr()?;

                Ok(Ast::SliceType(ast::ExprAndMut {
                    inner: Box::new(inner),
                    is_mutable: true,
                    span: start_span.to(self.previous_span()),
                }))
            } else if self.peek().kind.is_expr_start() {
                // []T
                let inner = self.parse_expr()?;

                Ok(Ast::SliceType(ast::ExprAndMut {
                    inner: Box::new(inner),
                    is_mutable: false,
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

                    let inner = self.parse_expr()?;

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
        let name = self.get_decl_name();

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
            name,
            fields,
            kind,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_struct_union_type(&mut self) -> DiagnosticResult<Ast> {
        let start_span = self.previous_span();
        let name = self.get_decl_name();

        require!(self, OpenCurly, "{")?;

        let fields = self.parse_struct_type_fields()?;

        Ok(Ast::StructType(ast::StructType {
            name,
            fields,
            kind: StructTypeKind::Union,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_struct_type_fields(&mut self) -> DiagnosticResult<Vec<ast::StructTypeField>> {
        let fields = parse_delimited_list!(
            self,
            CloseCurly,
            Comma,
            {
                let id = require!(self, Ident(_), "an identifier")?;
                let name = id.name();

                require!(self, Colon, ":")?;

                let ty = self.parse_expr()?;

                ast::StructTypeField {
                    name,
                    ty,
                    span: id.span,
                }
            },
            ", or }"
        );

        Ok(fields)
    }

    fn parse_builtin(&mut self, symbol: Ustr, start_span: Span) -> DiagnosticResult<Ast> {
        require!(self, OpenParen, "(")?;

        let kind = match symbol.as_str() {
            "import" => self.parse_builtin_import()?,
            "size_of" => BuiltinKind::SizeOf(Box::new(self.parse_expr()?)),
            "align_of" => BuiltinKind::AlignOf(Box::new(self.parse_expr()?)),
            "panic" => BuiltinKind::Panic(if is!(self, CloseParen) {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            }),
            "run" => BuiltinKind::Run(Box::new(self.parse_expr()?)),
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

        let condition = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        require!(self, OpenCurly, "{")?;
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

        let iter_index_ident = if eat!(self, Comma) {
            Some(require!(self, Ident(_), "an identifier")?)
        } else {
            None
        };

        require!(self, In, "in")?;

        let iter_start = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;

        let iterator = if eat!(self, DotDot) {
            let iter_end = self.parse_expr_with_res(Restrictions::NO_STRUCT_LITERAL)?;
            ForIter::Range(Box::new(iter_start), Box::new(iter_end))
        } else {
            ForIter::Value(Box::new(iter_start))
        };

        require!(self, OpenCurly, "{")?;
        let block = self.parse_block()?;

        Ok(Ast::For(ast::For {
            iter_binding: NameAndSpan::new(iter_ident.name(), iter_ident.span),
            index_binding: iter_index_ident.map(|ident| NameAndSpan::new(ident.name(), ident.span)),
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
                let expr = if !self.peek().kind.is_expr_start() && is!(self, Semicolon) {
                    None
                } else {
                    let expr = self.parse_expr()?;
                    Some(Box::new(expr))
                };

                Ok(Ast::Return(ast::Return {
                    expr,
                    span: span.to(self.previous_span()),
                }))
            }
            _ => panic!("got an invalid terminator"),
        }
    }
}

#[inline(always)]
fn ast_doesnt_require_semicolon(ast: &ast::Ast) -> bool {
    match ast {
        ast::Ast::For(_) | ast::Ast::While(_) | ast::Ast::If(_) | ast::Ast::Block(_) => true,
        _ => false,
    }
}
