import * as fs from "fs";
import * as path from "path";
import * as tmp from "tmp";
import { Position } from "vscode-languageserver-protocol";
import { TextEncoder, promisify } from "node:util";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const exec = promisify(require("node:child_process").exec);

export function lowerBoundBinarySearch(arr: number[], num: number): number {
  let low = 0;
  let mid = 0;
  let high = arr.length - 1;

  if (num >= arr[high]) return high;

  while (low < high) {
    // Bitshift to avoid floating point division
    mid = (low + high) >> 1;

    if (arr[mid] < num) {
      low = mid + 1;
    } else {
      high = mid;
    }
  }

  return low - 1;
}

export function convertSpan(
  utf8_offset: number,
  lineBreaks: Array<number>
): Position {
  const lineBreakIndex = lowerBoundBinarySearch(lineBreaks, utf8_offset);

  const start_of_line_offset =
    lineBreakIndex == -1 ? 0 : lineBreaks[lineBreakIndex] + 1;
  const character = utf8_offset - start_of_line_offset;

  return { line: lineBreakIndex + 1, character };
}

export function convertPosition(position: Position, text: string): number {
  let line = 0;
  let character = 0;
  const buffer = new TextEncoder().encode(text);

  let i = 0;
  while (i < text.length) {
    if (line == position.line && character == position.character) {
      return i;
    }

    if (buffer.at(i) == 0x0a) {
      line++;
      character = 0;
    } else {
      character++;
    }

    i++;
  }

  return i;
}

export function findLineBreaks(utf16_text: string): Array<number> {
  const utf8_text = new TextEncoder().encode(utf16_text);
  const lineBreaks: Array<number> = [];

  for (let i = 0; i < utf8_text.length; ++i) {
    if (utf8_text[i] == 0x0a) {
      lineBreaks.push(i);
    }
  }

  return lineBreaks;
}

export async function runCompiler(
  tmpFile: tmp.FileResult,
  text: string,
  flags: string
): Promise<string> {
  try {
    fs.writeFileSync(tmpFile.name, text);
  } catch (error) {
    console.log(error);
  }

  let stdout = "";

  try {
    const output = await exec(`chili check ${tmpFile.name} ${flags}`);
    // console.log(output);
    if (output.stderr != null && output.stderr != "") {
      console.error(output.stderr);
    } else {
      stdout = output.stdout;
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
  } catch (e: any) {
    console.log(e);
    stdout = e.stdout ?? "";
  }

  return stdout;
}

export function includeFlagForPath(file_path: string): string {
  const protocol_end = file_path.indexOf("://");
  if (protocol_end == -1) return " --include-paths " + file_path;
  // Not protocol.length + 3, include the last '/'
  return " --include-paths " + path.dirname(file_path.slice(protocol_end + 2));
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function throttle(fn: (...args: any) => void, delay: number) {
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
