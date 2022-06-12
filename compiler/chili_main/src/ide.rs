use chili_ast::workspace::Workspace;
use chili_span::Span;
use serde::{Deserialize, Serialize};

pub(crate) fn do_ide_check(workspace: &Workspace) {
    println!("{:#?}", workspace.diagnostics.items())
    // if let Some(b) = workspace.binding_infos.iter().find(|b| b.symbol == "main") {
    // let span = IdeSpan::from(b.span);
    // println!("{{\"start\": {}, \"end\": {}}}", span.start, span.end);
    // let span = serde_json::to_string(&span).unwrap();
    // println!("asdadsa");
    // }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
struct IdeSpan {
    start: usize,
    end: usize,
}

impl From<Span> for IdeSpan {
    fn from(s: Span) -> Self {
        IdeSpan {
            start: s.start.index,
            end: s.end.index,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct IdeDiagnostic {
    severity: IdeDiagnosticSeverity,
    span: IdeSpan,
    message: String,
    source: String,
}

#[derive(Serialize, Deserialize, Clone)]
enum IdeDiagnosticSeverity {
    Error,
    Warning,
}
