use crate::diagnostic::{Diagnostic, DiagnosticKind};
use chili_span::FileId;
use codespan_reporting::{
    diagnostic::Severity,
    files::SimpleFiles,
    term::{
        emit,
        termcolor::{ColorChoice, StandardStream, StandardStreamLock},
        Chars, Config, DisplayStyle, Styles,
    },
};

pub struct DiagnosticEmitter {
    writer: StandardStream,
    config: Config,
}

impl DiagnosticEmitter {
    fn new() -> Self {
        Self {
            writer: StandardStream::stderr(ColorChoice::Always),
            config: Config {
                display_style: DisplayStyle::Rich,
                tab_width: 4,
                styles: Styles::default(),
                chars: Chars::ascii(),
                start_context_lines: 3,
                end_context_lines: 1,
            },
        }
    }

    pub fn emit_one(&self, files: &SimpleFiles<String, String>, diagnostic: Diagnostic) {
        self.emit(&mut self.writer.lock(), files, diagnostic)
    }

    pub fn emit_many(&self, files: &SimpleFiles<String, String>, diagnostics: Vec<Diagnostic>) {
        let writer = &mut self.writer.lock();
        diagnostics
            .into_iter()
            .for_each(|diagnostic| self.emit(writer, files, diagnostic))
    }

    fn emit<'a>(
        &self,
        writer_lock: &mut StandardStreamLock<'a>,
        files: &SimpleFiles<String, String>,
        diagnostic: Diagnostic,
    ) {
        emit(writer_lock, &self.config, files, &diagnostic.into()).unwrap();
    }
}

type CodespanDiagnostic = codespan_reporting::diagnostic::Diagnostic<FileId>;

impl Into<Severity> for DiagnosticKind {
    fn into(self) -> Severity {
        match self {
            DiagnosticKind::Err => Severity::Error,
        }
    }
}

impl Into<CodespanDiagnostic> for Diagnostic {
    fn into(self) -> CodespanDiagnostic {
        CodespanDiagnostic::new(self.kind.into())
            .with_code(self.code)
            .with_message(self.message.unwrap_or(Default::default()))
            .with_labels(
                self.labels
                    .into_iter()
                    .map(|l| {
                        codespan_reporting::diagnostic::Label::primary(
                            l.span.file_id,
                            l.span.range(),
                        )
                        .with_message(l.message)
                    })
                    .collect(),
            )
            .with_notes(self.notes)
    }
}
