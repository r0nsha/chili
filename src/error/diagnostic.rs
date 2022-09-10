use crate::span::Span;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub kind: LabelKind,
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone, Copy)]
pub enum LabelKind {
    Primary,
    Secondary,
}

impl Label {
    fn new(kind: LabelKind, span: Span, message: impl ToString) -> Self {
        Self {
            kind,
            span,
            message: message.to_string(),
        }
    }

    pub fn primary(span: Span, message: impl ToString) -> Self {
        Label::new(LabelKind::Primary, span, message)
    }

    pub fn secondary(span: Span, message: impl ToString) -> Self {
        Label::new(LabelKind::Secondary, span, message)
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub message: Option<String>,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(severity: DiagnosticSeverity) -> Self {
        Self {
            severity,
            message: None,
            labels: vec![],
            notes: vec![],
        }
    }

    pub fn error() -> Self {
        Self::new(DiagnosticSeverity::Error)
    }

    pub fn warning() -> Self {
        Self::new(DiagnosticSeverity::Warning)
    }

    pub fn set_message(&mut self, message: impl ToString) {
        self.message = Some(message.to_string());
    }

    pub fn with_message(mut self, message: impl ToString) -> Self {
        self.set_message(message);
        self
    }

    pub fn add_label(&mut self, label: Label) {
        self.labels.push(label);
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.add_label(label);
        self
    }

    pub fn maybe_with_label(mut self, label: Option<Label>) -> Self {
        if let Some(label) = label {
            self.add_label(label);
        }
        self
    }

    pub fn add_note(&mut self, note: impl ToString) {
        self.notes.push(note.to_string());
    }

    pub fn with_note(mut self, note: impl ToString) -> Self {
        self.add_note(note);
        self
    }
}
