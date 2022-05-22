use crate::diagnostic::{Diagnostic, DiagnosticKind, LabelKind};
use chili_span::FileId;
use codespan_reporting::{
    diagnostic::{LabelStyle, Severity},
    files::SimpleFiles,
    term::{
        emit,
        termcolor::{ColorChoice, StandardStream, StandardStreamLock},
        Chars, Config, DisplayStyle,
    },
};

pub struct DiagnosticEmitter {
    writer: StandardStream,
    config: Config,
}

impl DiagnosticEmitter {
    pub fn new(color_mode: ColorMode) -> Self {
        Self {
            writer: StandardStream::stderr(color_mode.into()),
            config: Config {
                display_style: DisplayStyle::Rich,
                tab_width: 4,
                chars: Chars::ascii(),
                ..Default::default()
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

impl From<Diagnostic> for CodespanDiagnostic {
    fn from(val: Diagnostic) -> Self {
        CodespanDiagnostic::new(val.kind.into())
            .with_message(val.message.unwrap_or_default())
            .with_labels(
                val.labels
                    .into_iter()
                    .map(|l| {
                        codespan_reporting::diagnostic::Label::new(
                            l.kind.into(),
                            l.span.file_id,
                            l.span.range(),
                        )
                        .with_message(l.message)
                    })
                    .collect(),
            )
            .with_notes(val.notes)
    }
}

impl From<DiagnosticKind> for Severity {
    fn from(val: DiagnosticKind) -> Self {
        match val {
            DiagnosticKind::Error => Severity::Error,
        }
    }
}

impl From<LabelKind> for LabelStyle {
    fn from(val: LabelKind) -> Self {
        match val {
            LabelKind::Primary => LabelStyle::Primary,
            LabelKind::Secondary => LabelStyle::Secondary,
        }
    }
}

impl From<ColorMode> for ColorChoice {
    fn from(mode: ColorMode) -> Self {
        match mode {
            ColorMode::Always => ColorChoice::Auto,
            ColorMode::Never => ColorChoice::Never,
        }
    }
}

pub enum ColorMode {
    Always,
    Never,
}
