mod binding;
mod expr;
mod function;
mod import;
mod literal;
mod pattern;
mod postfix_expr;
mod top_level;

use crate::{
    ast::{self, Ast, ExternLibrary},
    error::{diagnostic::Diagnostic, DiagnosticResult, Diagnostics, SyntaxError},
    span::{EndPosition, Position, Span, To},
    token::{lexer::Lexer, Token, TokenKind::*},
    workspace::{ModuleInfo, PartialModuleInfo},
};
use bitflags::bitflags;
use parking_lot::{Mutex, MutexGuard};
use std::{
    collections::HashSet,
    fmt::Debug,
    path::PathBuf,
    sync::{mpsc::Sender, Arc},
};
use threadpool::ThreadPool;
use unindent::unindent;
use ustr::{ustr, Ustr};

bitflags! {
    pub struct Restrictions : u8 {
        const STMT_EXPR = 1 << 0;
        const NO_STRUCT_LITERAL = 1 << 1;
    }
}

macro_rules! is {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        if $parser.is_end() {
            false
        } else {
            match &$parser.peek().kind {
                $( $pattern )|+ => true,
                _ => false
            }
        }
    };
}

macro_rules! eat {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +) => {
        if is!($parser, $( $pattern )|+) {
            $parser.bump();
            true
        } else {
            false
        }
    };
}

macro_rules! require {
    ($parser:expr, $(|) ? $($pattern : pat_param) | +, $msg:expr) => {
        if is!($parser, $( $pattern )|+) {
            Ok($parser.bump().clone())
        } else {
            Err(SyntaxError::expected($parser.span(), $msg))
        }
    };
}

macro_rules! parse_delimited_list {
    ($parser:expr, $close_delim:pat, $sep:pat, $parse:expr, $msg:expr) => {{
        let mut items = vec![];

        while !eat!($parser, $close_delim) && !$parser.is_end() {
            items.push($parse);

            if eat!($parser, $sep) {
                continue;
            } else if eat!($parser, $close_delim) {
                break;
            } else {
                let span = Parser::get_missing_delimiter_span($parser.previous_span());
                return Err(SyntaxError::expected(span, $msg));
            }
        }

        items
    }};
}

pub(super) use eat;
pub(super) use is;
pub(super) use parse_delimited_list;
pub(super) use require;

pub fn spawn_parser(
    thread_pool: ThreadPool,
    tx: Sender<Box<ParserResult>>,
    cache: Arc<Mutex<ParserCache>>,
    partial_module_info: PartialModuleInfo,
) {
    thread_pool.clone().execute(move || {
        Parser::new(thread_pool, tx, Arc::clone(&cache), partial_module_info).parse();
    });
}

pub struct Parser {
    pub cache: Arc<Mutex<ParserCache>>,
    thread_pool: ThreadPool,
    tx: Sender<Box<ParserResult>>,
    tokens: Vec<Token>,
    current: usize,
    module_info: ModuleInfo,
    decl_name_frames: Vec<Ustr>,
    restrictions: Restrictions,
}

#[derive(Debug)]
pub struct ParserCache {
    pub root_file: PathBuf,
    pub root_dir: PathBuf,
    pub std_dir: PathBuf,
    pub include_paths: Vec<PathBuf>,
    pub diagnostics: Diagnostics,
    pub parsed_files: HashSet<Ustr>,
    pub total_lines: u32,
}

pub type ParserCacheGuard<'a> = MutexGuard<'a, ParserCache>;

pub enum ParserResult {
    NewAst(ast::Module),
    AlreadyParsed,
    Failed(Diagnostic),
}

impl Parser {
    pub fn new(
        thread_pool: ThreadPool,
        tx: Sender<Box<ParserResult>>,
        cache: Arc<Mutex<ParserCache>>,
        partial_module_info: PartialModuleInfo,
    ) -> Self {
        Self {
            cache,
            thread_pool,
            tx,
            tokens: vec![],
            current: 0,
            module_info: partial_module_info.into(),
            decl_name_frames: Default::default(),
            restrictions: Restrictions::empty(),
        }
    }

    pub fn parse(mut self) {
        let result = self.parse_inner();
        self.tx.send(Box::new(result)).unwrap();
    }

    fn parse_inner(&mut self) -> ParserResult {
        let (file_id, source) = {
            let mut cache = self.cache.lock();

            if !cache.parsed_files.insert(self.module_info.file_path) {
                return ParserResult::AlreadyParsed;
            } else {
                let source = std::fs::read_to_string(self.module_info.file_path.as_str())
                    .unwrap_or_else(|_| panic!("failed to read `{}`", self.module_info.file_path));

                cache.total_lines += source.lines().count() as u32;

                let file_id = cache
                    .diagnostics
                    .add_file(self.module_info.file_path.to_string(), unindent(&source));

                (file_id, source)
            }
        };

        self.module_info.file_id = file_id;

        match Lexer::new(file_id, &source).scan() {
            Ok(tokens) => {
                self.tokens = tokens;
                self.parse_all_top_level(file_id)
            }
            Err(diag) => ParserResult::Failed(diag),
        }
    }

    pub fn with_res<T>(&mut self, restrictions: Restrictions, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.restrictions;
        self.restrictions = restrictions;
        let res = f(self);
        self.restrictions = old;
        res
    }

    pub fn get_decl_name(&self) -> Ustr {
        if !self.decl_name_frames.is_empty() {
            *self.decl_name_frames.last().unwrap()
        } else {
            ustr("")
        }
    }

    #[inline]
    pub fn bump(&mut self) -> &Token {
        if !self.is_end() {
            self.current += 1;
        }
        self.previous()
    }

    #[allow(unused)]
    #[inline]
    pub fn revert(&mut self, count: usize) {
        self.current -= count;
    }

    #[inline]
    pub fn is_end(&self) -> bool {
        self.peek().kind == Eof
    }

    #[inline]
    pub fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    #[inline]
    pub fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.peek().span
    }

    #[inline]
    pub fn previous_span(&self) -> Span {
        self.previous().span
    }

    #[inline]
    pub fn skip_semicolons(&mut self) {
        while is!(self, Semicolon) {
            self.bump();
        }
    }

    #[inline]
    pub fn skip_until_recovery_point(&mut self) {
        while !is!(self, Semicolon) && !self.is_end() {
            self.bump();
        }
    }

    #[inline]
    pub fn get_missing_delimiter_span(after_span: Span) -> Span {
        let start_pos = Position {
            index: after_span.end.index,
            line: after_span.start.line,
            column: after_span.start.column,
        };

        let end_pos = EndPosition {
            index: after_span.end.index,
        };

        after_span.with_start(start_pos).with_end(end_pos)
    }
}
pub(super) trait Recover<T> {
    fn recover(self, parser: &mut Parser) -> T;
}

impl Recover<Ast> for DiagnosticResult<Ast> {
    fn recover(self, parser: &mut Parser) -> Ast {
        match self {
            Ok(expr) => expr,
            Err(_) => {
                let start_span = parser.previous_span();
                parser.skip_until_recovery_point();
                ast::Ast::Error(ast::Empty {
                    span: start_span.to(parser.previous_span()),
                })
            }
        }
    }
}

impl Recover<()> for DiagnosticResult<()> {
    fn recover(self, parser: &mut Parser) -> () {
        match self {
            Ok(()) => (),
            Err(_) => parser.skip_until_recovery_point(),
        }
    }
}
