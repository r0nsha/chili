import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  TextDocumentSyncKind,
  InitializeResult,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import {
  ChiliTextDocument,
  HoverInfo,
  LspDiagnosticSeverity,
  LspObject,
  Span,
  spanToRange,
} from "./types";
import {
  convertPosition,
  convertSpan,
  findLineBreaks,
  includeFlagForPath,
  runCompiler,
  throttle,
  tmpFile,
} from "./util";
import * as fs from "fs";

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

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
      hoverProvider: true,
      definitionProvider: true,
      typeDefinitionProvider: true,
      // completionProvider: {
      // 	resolveProvider: false,
      // 	triggerCharacters: ['.']
      // },
      // inlayHintProvider: {
      // 	resolveProvider: false
      // },
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

  // if (hasWorkspaceFolderCapability) {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  // connection.workspace.onDidChangeWorkspaceFolders((_event) => {
  // connection.console.log("Workspace folder change event received.");
  // });
  // }
});

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings);
  }
  let result = documentSettings.get(resource);
  if (!result) {
    result = connection.workspace.getConfiguration({
      scopeUri: resource,
      section: "chiliLanguageServer",
    });
    documentSettings.set(resource, result);
  }
  return result;
}

// Only keep settings for open documents
documents.onDidClose((e) => {
  documentSettings.delete(e.document.uri);
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

  // The validator creates diagnostics for all uppercase words length 2 and more
  const text = textDocument.getText();

  // const lineBreaks = findLineBreaks(text);

  textDocument.chiliInlayHints = [];

  const diagnostics: Diagnostic[] = [];

  // const seenTypeHintPositions = new Set();

  const stdout = await runCompiler(
    text,
    "--diagnostics " + includeFlagForPath(textDocument.uri)
  );

  const lines = stdout.split("\n").filter((l) => l.length > 0);

  for (const line of lines) {
    // console.log(line);
    try {
      const objects: LspObject[] = JSON.parse(line);
      // console.log(objects);

      for (const object of objects) {
        if (object.type == "diagnostic") {
          const diagnostic = object.diagnostic;

          let severity = DiagnosticSeverity.Error;
          switch (diagnostic.severity) {
            case LspDiagnosticSeverity.Error:
              severity = DiagnosticSeverity.Error;
              break;
          }

          const uri = "file://" + diagnostic.source;
          // console.log({  uri });

          const document =
            diagnostic.source == tmpFile.name
              ? textDocument
              : documents.get(uri);

          if (!document) {
            console.error(`couldn't open text document: ${uri}`);
            continue;
          }

          const range = spanToRange(document, diagnostic.span);

          diagnostics.push({
            severity,
            range,
            message: diagnostic.message,
            source: document.uri.toString(),
          });
        }
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
        //         textDocument.chiliInlayHints.push(hint);
        //       }
        //     }
      }
    } catch (e) {
      console.error(e);
    }
  }

  // Send the computed diagnostics to VSCode.
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });

  console.timeEnd("validateTextDocument");
}

connection.onHover(async (request) => {
  console.time("onHover");

  const document = documents.get(request.textDocument.uri);

  const text = document?.getText();

  if (typeof text == "string") {
    // console.log("request: ");
    // console.log(request);

    const offset = convertPosition(request.position, text);
    // console.log("offset: " + index);

    const stdout = await runCompiler(
      text,
      "--hover-info " + offset + includeFlagForPath(request.textDocument.uri)
    );
    // console.log("got: ", stdout);

    const lines = stdout.split("\n").filter((l) => l.length > 0);
    for (const line of lines) {
      // console.log("hovering");

      const hoverInfo: HoverInfo | null = JSON.parse(line);
      // console.log(object);

      if (hoverInfo) {
        // FIXME: Figure out how to import `vscode` package in server.ts without
        // getting runtime import errors to remove this deprication warning.
        const contents = {
          value: hoverInfo.contents,
          language: "chili",
        };

        console.timeEnd("onHover");

        return { contents };
      }
    }
  }

  console.timeEnd("onHover");

  return null;
});

const goToDefinition: Parameters<typeof connection.onDefinition>[0] = async (
  request
) => {
  const document = documents.get(request.textDocument.uri);

  if (!document) {
    return null;
  }

  const text = document.getText();

  const offset = document.offsetAt(request.position);

  // console.log("request: ");
  // console.log(request);
  // console.log("offset: " + convertPosition(request.position, text));

  const stdout = await runCompiler(
    text,
    "--goto-def " + offset + includeFlagForPath(request.textDocument.uri)
  );
  // console.log("got: ", stdout);

  const lines = stdout.split("\n").filter((l) => l.length > 0);

  for (const line of lines) {
    const span: Span | null = JSON.parse(line);
    // console.log(span);

    if (span) {
      const uri =
        span.file == tmpFile.name
          ? request.textDocument.uri
          : "file://" + span.file;

      const fileContents = fs.readFileSync(span.file).toString();

      const lineBreaks = findLineBreaks(fileContents);

      const start = convertSpan(span.start, lineBreaks);
      const end = convertSpan(span.end, lineBreaks);

      // console.log(`going to definition: ${uri} | ${start}..${end}`);

      return {
        uri,
        range: { start, end },
      };

      // const document = documents.get(uri);

      // if (document) {
      //   const range = spanToRange(document, span);

      //   return {
      //     uri,
      //     range,
      //   };
      // }
    }
  }
};

connection.onDefinition(goToDefinition);
connection.onTypeDefinition(goToDefinition);

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
