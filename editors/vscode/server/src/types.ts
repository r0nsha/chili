import { InlayHint } from "vscode-languageserver";
import { Range, TextDocument } from "vscode-languageserver-textdocument";

export interface ChiliTextDocument extends TextDocument {
  chiliInlayHints?: InlayHint[];
}

export interface Span {
  file: string;
  start: number;
  end: number;
}

export function spanToRange(textDocument: TextDocument, span: Span): Range {
  return {
    start: textDocument.positionAt(span.start),
    end: textDocument.positionAt(span.end),
  };
}

export type LspDiagnosticSeverity = "Error" | "Warning";

export type LspObject =
  | {
      type: "Diagnostic";
      severity: LspDiagnosticSeverity;
      span: Span;
      message: string;
    }
  | {
      type: "Hint";
      span: Span;
      type_name: string;
      kind: "Binding" | "ReturnType" | "ImplicitParam";
    };

export interface HoverInfo {
  contents: string;
}
