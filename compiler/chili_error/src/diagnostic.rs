use chili_span::Span;

pub type DiagnosticCode = String;

pub enum DiagnosticKind {
    Err,
}

pub struct Label {
    pub message: String,
    pub span: Span,
}

pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub code: DiagnosticCode,
    pub message: Option<String>,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn err(code: DiagnosticCode) -> Self {
        Self {
            kind: DiagnosticKind::Err,
            code,
            message: None,
            labels: vec![],
            notes: vec![],
        }
    }

    pub fn set_message(&mut self, message: String) {
        self.message = Some(message);
    }

    pub fn with_message(mut self, message: String) -> Self {
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

    pub fn add_note(&mut self, note: String) {
        self.notes.push(note);
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.add_note(note);
        self
    }
}
