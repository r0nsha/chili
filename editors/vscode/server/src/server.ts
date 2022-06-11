/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import * as fs from "fs";
import * as tmp from "tmp";
import * as path from "path";
import * as util from "node:util";

import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult,
} from "vscode-languageserver/node";

import { TextDocument } from "vscode-languageserver-textdocument";

interface ChiliTextDocument extends TextDocument {
  chiliInlayHints?: InlayHint[];
}

import {
  Position,
  InlayHint,
  InlayHintParams,
  InlayHintLabelPart,
  InlayHintKind,
} from "vscode-languageserver-protocol";

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

import { TextEncoder } from "node:util";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const exec = util.promisify(require("node:child_process").exec);

const tmpFile = tmp.fileSync();

// The example settings
interface ExampleSettings {
  maxNumberOfProblems: number;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 };
let globalSettings: ExampleSettings = defaultSettings;

// Cache the settings of all open documents
const documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onInitialize(({ capabilities }: InitializeParams) => {
  // Does the client support the `workspace/configuration` request?
  // If not, we fall back using global settings.
  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );

  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );

  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument &&
    capabilities.textDocument.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
    },
  };

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
      },
    };
  }

  console.log("Chili language server initialized");

  return result;
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    // Register for all configuration changes.
    connection.client.register(
      DidChangeConfigurationNotification.type,
      undefined
    );
  }
  if (hasWorkspaceFolderCapability) {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log("Workspace folder change event received.");
    });
  }
});

connection.onDidChangeConfiguration((change) => {
  if (hasConfigurationCapability) {
    // Reset all cached document settings
    documentSettings.clear();
  } else {
    globalSettings = <ExampleSettings>(
      (change.settings.languageServerExample || defaultSettings)
    );
  }

  // Revalidate all open text documents
  documents.all().forEach(validateTextDocument);
});

documents.onDidChangeContent(
  (() => {
    const throttledValidateTextDocument = throttle(validateTextDocument, 500);
    return (change) => {
      throttledValidateTextDocument(change.document);
    };
  })()
);

async function validateTextDocument(
  textDocument: ChiliTextDocument
): Promise<void> {
  console.time("validateTextDocument");

  if (!hasDiagnosticRelatedInformationCapability) {
    console.error(
      "Trying to validate a document with no diagnostic capability"
    );
    return;
  }

  // // In this simple example we get the settings for every validate run.
  // const settings = await getDocumentSettings(textDocument.uri);

  // The validator creates diagnostics for all uppercase words length 2 and more
  // const text = textDocument.getText();

  // const lineBreaks = findLineBreaks(text);

  // textDocument.jaktInlayHints = [];

  // const stdout = await runCompiler(
  //   text,
  //   "-c -H -j" + includeFlagForPath(textDocument.uri)
  // );

  // const diagnostics: Diagnostic[] = [];

  // // FIXME: We use this to deduplicate type hints given by the compiler.
  // //        It'd be nicer if it didn't give duplicate hints in the first place.
  // const seenTypeHintPositions = new Set();

  // const lines = stdout.split("\n").filter((l) => l.length > 0);
  // for (const line of lines) {
  //   // console.log(line);
  //   try {
  //     const obj = JSON.parse(line);

  //     // HACK: Ignore everything that isn't about file ID #1 here, since that's always the current editing buffer.
  //     if (obj.file_id != 1) {
  //       continue;
  //     }
  //     if (obj.type == "diagnostic") {
  //       let severity: DiagnosticSeverity = DiagnosticSeverity.Error;

  //       switch (obj.severity) {
  //         case "Information":
  //           severity = DiagnosticSeverity.Information;
  //           break;
  //         case "Hint":
  //           severity = DiagnosticSeverity.Hint;
  //           break;
  //         case "Warning":
  //           severity = DiagnosticSeverity.Warning;
  //           break;
  //         case "Error":
  //           severity = DiagnosticSeverity.Error;
  //           break;
  //       }

  //       const position_start = convertSpan(obj.span.start, lineBreaks);
  //       const position_end = convertSpan(obj.span.end, lineBreaks);

  //       const diagnostic: Diagnostic = {
  //         severity,
  //         range: {
  //           start: position_start,
  //           end: position_end,
  //         },
  //         message: obj.message,
  //         source: textDocument.uri,
  //       };

  //       // console.log(diagnostic);

  //       diagnostics.push(diagnostic);
  //     } else if (obj.type == "hint") {
  //       if (!seenTypeHintPositions.has(obj.position)) {
  //         seenTypeHintPositions.add(obj.position);
  //         const position = convertSpan(obj.position, lineBreaks);
  //         const hint_string = ": " + obj.typename;
  //         const hint = InlayHint.create(
  //           position,
  //           [InlayHintLabelPart.create(hint_string)],
  //           InlayHintKind.Type
  //         );

  //         textDocument.jaktInlayHints.push(hint);
  //       }
  //     }
  //   } catch (e) {
  //     console.error(e);
  //   }
  // }

  // // Send the computed diagnostics to VSCode.
  // connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
  console.timeEnd("validateTextDocument");
}

async function runCompiler(text: string, flags: string): Promise<string> {
  try {
    fs.writeFileSync(tmpFile.name, text);
  } catch (error) {
    console.log(error);
  }

  let stdout: string;
  try {
    const output = await exec(`chili check ${tmpFile.name} ${flags}`);
    // console.log(output);
    stdout = output.stdout;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
  } catch (e: any) {
    stdout = e.stdout;
    if (e.signal != null) {
      console.log("compile failed: ");
      console.log(e);
    }
  }

  return stdout;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function throttle(fn: (...args: any) => void, delay: number) {
  let shouldWait = false;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  let waitingArgs: any | null;
  const timeoutFunc = () => {
    if (waitingArgs == null) {
      shouldWait = false;
    } else {
      fn(...waitingArgs);
      waitingArgs = null;
      setTimeout(timeoutFunc, delay);
    }
  };

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return (...args: any) => {
    if (shouldWait) {
      waitingArgs = args;
      return;
    }

    fn(...args);
    shouldWait = true;

    setTimeout(timeoutFunc, delay);
  };
}
