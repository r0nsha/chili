mod attrs;
mod binding;
mod expr;
mod function;
mod import;
mod literal;
mod pat;
mod postfix;
mod top_level;

use crate::{
    ast::{self, Ast},
    common::id_cache::IdCache,
    error::{diagnostic::Diagnostic, DiagnosticResult, Diagnostics, SyntaxError},
    span::{FileId, Span},
    token::{lexer::Lexer, Token, TokenKind::*},
    workspace::{library::Library, ModuleId, ModuleInfo, ModulePath},
};
use bitflags::bitflags;
use parking_lot::Mutex;
use std::{
    collections::HashSet,
    fmt::Debug,
    path::PathBuf,
    sync::{mpsc::Sender, Arc},
};
use threadpool::ThreadPool;
use unindent::unindent;
use ustr::{ustr, Ustr, UstrMap};

bitflags! {
    pub struct Restrictions : u8 {
        const NO_STRUCT_LITERAL = 1 << 0;
        const NO_CAST = 1 << 1;
    }
}

macro_rules! is {
    ($parser:expr, $(|) ? $($pat : pat_param) | +) => {
        if $parser.eof() {
            false
        } else {
            match &$parser.peek().kind {
                $( $pat )|+ => true,
                _ => false
            }
        }
    };
}

macro_rules! eat {
    ($parser:expr, $(|) ? $($pat : pat_param) | +) => {
        if is!($parser, $( $pat )|+) {
            $parser.bump();
            true
        } else {
            false
        }
    };
}

macro_rules! require {
    ($parser:expr, $(|) ? $($pat : pat_param) | +, $msg:expr) => {
        if is!($parser, $( $pat )|+) {
            Ok($parser.bump().clone())
        } else {
            Err(SyntaxError::expected($parser.span(), $msg))
        }
    };
}

macro_rules! parse_delimited_list {
    ($parser:expr, $close_delim:pat, $($sep : pat_param) | +, $parse:expr, $msg:expr) => {{
        let mut items = vec![];

        while !eat!($parser, $close_delim) && !$parser.eof() {
            $parser.skip_newlines();
            items.push($parse);

            if eat!($parser, $( $sep )|+) {
                $parser.skip_newlines();
                continue;
            } else if eat!($parser, $close_delim) {
                break;
            } else {
                let span = $parser.previous_span().after();
                return Err(SyntaxError::expected(span, &format!("{}, got {}", $msg, $parser.peek().kind.lexeme())));
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
    module_path: ModulePath,
    parent: Option<ModuleId>,
) {
    thread_pool.clone().execute(move || {
        let mut module_info = ModuleInfo {
            id: ModuleId::unknown(),
            name: module_path.name(),
            qualified_name: ustr(&module_path.qualified_name()),
            file_path: ustr(module_path.path().to_str().unwrap()),
            file_id: FileId::MAX,
            library_id: module_path.library().id,
            parent,
        };

        let mut cache_lock = cache.lock();
        let id = cache_lock.module_infos.insert_with_id(module_info);
        module_info.id = id;
        drop(cache_lock);

        Parser::new(thread_pool, tx, Arc::clone(&cache), module_path, module_info).parse();
    });
}

pub struct Parser {
    pub cache: Arc<Mutex<ParserCache>>,
    thread_pool: ThreadPool,
    tx: Sender<Box<ParserResult>>,
    tokens: Vec<Token>,
    current: usize,
    module_info: ModuleInfo,
    module_path: ModulePath,
    restrictions: Restrictions,
}

#[derive(Debug)]
pub struct ParserCache {
    pub module_infos: IdCache<ModuleId, ModuleInfo>,
    pub libraries: UstrMap<Library>,
    pub include_paths: Vec<PathBuf>,
    pub diagnostics: Diagnostics,
    pub parsed_files: HashSet<Ustr>,
    pub total_lines: u32,
}

pub enum ParserResult {
    NewModule(ast::Module),
    AlreadyParsed,
    ParserFailed,
    LexerFailed(ast::Module, Diagnostic),
}

impl Parser {
    pub fn new(
        thread_pool: ThreadPool,
        tx: Sender<Box<ParserResult>>,
        cache: Arc<Mutex<ParserCache>>,
        module_path: ModulePath,
        module_info: ModuleInfo,
    ) -> Self {
        Self {
            cache,
            thread_pool,
            tx,
            tokens: vec![],
            current: 0,
            module_info,
            module_path,
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
                match std::fs::read_to_string(self.module_info.file_path.as_str()) {
                    Ok(source) => {
                        cache.total_lines += source.lines().count() as u32;

                        let file_id = cache
                            .diagnostics
                            .add_file(self.module_info.file_path.to_string(), unindent(&source));

                        cache.module_infos.get_mut(self.module_info.id).unwrap().file_id = file_id;
                        self.module_info.file_id = file_id;

                        (file_id, source)
                    }
                    Err(_) => return ParserResult::ParserFailed,
                }
            }
        };

        match Lexer::new(file_id, &source).scan() {
            Ok(tokens) => {
                self.tokens = tokens;
                self.parse_module(file_id)
            }
            Err(diag) => ParserResult::LexerFailed(ast::Module::new(file_id, self.module_info), diag),
        }
    }

    pub fn with_res<T>(&mut self, restrictions: Restrictions, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.restrictions;
        self.restrictions = restrictions;
        let res = f(self);
        self.restrictions = old;
        res
    }

    #[inline]
    pub fn bump(&mut self) -> &Token {
        if !self.eof() {
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
    pub fn eof(&self) -> bool {
        matches!(self.peek().kind, Eof)
    }

    #[inline]
    pub fn eol(&self) -> bool {
        matches!(self.peek().kind, Newline | Eof)
    }

    #[inline]
    pub fn peek(&self) -> &Token {
        self.peek_offset(0)
    }

    #[inline]
    pub fn peek_offset(&self, offset: usize) -> &Token {
        self.tokens.get(self.current + offset).unwrap()
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
    #[allow(unused)]
    pub fn skip_until_recovery_point(&mut self) {
        while !is!(self, Semicolon | Newline) && !self.eof() {
            self.bump();
        }
    }

    pub fn skip_newlines(&mut self) {
        while is!(self, Newline) {
            self.bump();
        }
    }

    pub fn require_ident(&mut self) -> DiagnosticResult<Token> {
        require!(self, Ident(_), "an identifier")
    }
}
