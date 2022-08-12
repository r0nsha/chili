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
  TextDocumentChangeEvent,
  InlayHintParams,
  InlayHint,
  InlayHintLabelPart,
  InlayHintKind,
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
  libRootFlagForPath,
  runCompiler,
  throttle,
} from "./util";
import * as fs from "fs";
import * as tmp from "tmp";

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
      inlayHintProvider: {
        resolveProvider: false,
      },
      // completionProvider: {
      // 	resolveProvider: false,
      // 	triggerCharacters: ['.']
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
  documents
    .all()
    .forEach((textDocument, index) =>
      validateTextDocument(`changeConfiguration_${index}`, textDocument)
    );
});

const THROTTLE_MS = 500;

const createThrottledDocumentChangeEventHandler = (name: string) => {
  const throttledValidateTextDocument = throttle(
    validateTextDocument,
    THROTTLE_MS
  );

  return <T>(e: TextDocumentChangeEvent<T>) => {
    throttledValidateTextDocument(name, e.document);
  };
};

const tmpFiles: { [uri: string]: tmp.FileResult } = {};

documents.onDidChangeContent(
  createThrottledDocumentChangeEventHandler("changeContent")
);
// documents.onDidSave(createThrottledDocumentChangeEventHandler("save"));

documents.onDidOpen((e) => {
  tmpFiles[e.document.uri] = tmp.fileSync({
    template: "chili_vscode_tmp_XXXXXX.chl",
  });
});

documents.onDidClose((e) => {
  delete tmpFiles[e.document.uri];
  documentSettings.delete(e.document.uri);
});

async function validateTextDocument(
  name: string,
  textDocument: ChiliTextDocument
): Promise<void> {
  console.time(`validateTextDocument_${name}`);

  if (!hasDiagnosticRelatedInformationCapability) {
    console.error(
      "Trying to validate a document with no diagnostic capability"
    );
    return;
  }

  textDocument.chiliInlayHints = [];

  const tmpFile = tmpFiles[textDocument.uri];
  const diagnostics: Diagnostic[] = [];
  const seenTypeHintPositions: Set<Span> = new Set();

  const stdout = await runCompiler(
    tmpFile,
    textDocument.getText(),
    "--diagnostics " + libRootFlagForPath(textDocument.uri)
  );

  const lines = stdout.split("\n").filter((l) => l.length > 0);

  for (const line of lines) {
    // console.log(line);
    try {
      const objects: LspObject[] = JSON.parse(line);
      // console.log(objects);

      for (const object of objects) {
        const file = object.span.file;
        const objectTextDocument: ChiliTextDocument | undefined =
          file == tmpFile.name ? textDocument : documents.get(file);

        if (!objectTextDocument) {
          continue;
        }

        if (object.type == "Diagnostic") {
          let severity = DiagnosticSeverity.Error;
          switch (object.severity) {
            case LspDiagnosticSeverity.Error:
              severity = DiagnosticSeverity.Error;
              break;
          }

          const range = spanToRange(objectTextDocument, object.span);

          diagnostics.push({
            severity,
            range,
            message: object.message,
            source: file,
          });
        } else if (object.type == "Hint") {
          const file = object.span.file;

          if (file != tmpFile.name || seenTypeHintPositions.has(object.span)) {
            continue;
          }

          seenTypeHintPositions.add(object.span);

          const position = objectTextDocument.positionAt(object.span.end);

          let hintString: string;

          switch (object.kind) {
            case "Binding":
              hintString = `: ${object.type_name}`;
              break;
            case "ReturnType":
              hintString = ` -> ${object.type_name}`;
              break;
            case "ImplicitParam":
              hintString = `(${object.type_name})`;
              break;
          }

          const inlayHint = InlayHint.create(
            position,
            [InlayHintLabelPart.create(hintString)],
            InlayHintKind.Type
          );

          objectTextDocument.chiliInlayHints.push(inlayHint);
        }
      }
    } catch (e) {
      console.error(e);
    }
  }

  // Send the computed diagnostics to VSCode.
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });

  console.timeEnd(`validateTextDocument_${name}`);
}

connection.onHover(async (request) => {
  console.time("onHover");

  const textDocument = documents.get(request.textDocument.uri);

  if (!textDocument) {
    return null;
  }

  const text = textDocument.getText();

  // console.log("request: ");
  // console.log(request);

  const tmpFile = tmpFiles[textDocument.uri];
  const offset = convertPosition(request.position, text);
  // console.log("offset: " + index);

  const stdout = await runCompiler(
    tmpFile,
    text,
    "--hover-info " + offset + libRootFlagForPath(request.textDocument.uri)
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

  console.timeEnd("onHover");

  return null;
});

const goToDefinition: Parameters<typeof connection.onDefinition>[0] = async (
  request
) => {
  const textDocument = documents.get(request.textDocument.uri);

  if (!textDocument) {
    return null;
  }

  const text = textDocument.getText();

  const tmpFile = tmpFiles[textDocument.uri];
  const offset = textDocument.offsetAt(request.position);

  // console.log("request: ");
  // console.log(request);
  // console.log("offset: " + convertPosition(request.position, text));

  const stdout = await runCompiler(
    tmpFile,
    text,
    "--goto-def " + offset + libRootFlagForPath(request.textDocument.uri)
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

connection.languages.inlayHint.on((params: InlayHintParams) => {
  const document = documents.get(params.textDocument.uri) as ChiliTextDocument;
  return document.chiliInlayHints;
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
