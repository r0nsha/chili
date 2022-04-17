use chili_span::Span;

pub type DiagnosticCode = String;

#[derive(Debug, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
}

pub struct Label {
    pub kind: LabelKind,
    pub span: Span,
    pub message: String,
}

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

pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: Option<String>,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error() -> Self {
        Self {
            kind: DiagnosticKind::Error,
            message: None,
            labels: vec![],
            notes: vec![],
        }
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

    pub fn add_note(&mut self, note: impl ToString) {
        self.notes.push(note.to_string());
    }

    pub fn with_note(mut self, note: impl ToString) -> Self {
        self.add_note(note);
        self
    }
}
