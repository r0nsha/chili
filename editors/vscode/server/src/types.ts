import { InlayHint } from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";

export interface ChiliTextDocument extends TextDocument {
  chiliInlayHints?: InlayHint[];
}

export interface Span {
  start: number;
  end: number;
}

export interface LspDiagnostic {
  severity: LspDiagnosticSeverity;
  span: Span;
  message: string;
  source: string;
}

export enum LspDiagnosticSeverity {
  Error,
}

export type LspObject = {
  type: "diagnostic";
  diagnostic: LspDiagnostic;
};

export interface HoverInfo {
  contents: string;
}
