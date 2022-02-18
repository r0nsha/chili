use chilic_span::Span;
use std::ops::Range;

pub enum UnescapeError {
    InvalidEscapeSequence(Span),
}

pub fn unescape(s: &str, start_span: Span) -> Result<String, UnescapeError> {
    let mut chars = s.chars();
    let mut s = String::new();

    let mut processed = 0;
    while let Some(c) = chars.next() {
        processed += 1;

        if c != '\\' {
            s.push(c);
        } else {
            let c = match chars.next() {
                Some(c) => c,
                None => break,
            };

            match c {
                'b' => s.push('\u{0008}'),
                'f' => s.push('\u{000C}'),
                'n' => s.push('\n'),
                'r' => s.push('\r'),
                't' => s.push('\t'),
                '0' => s.push('\0'),
                '\'' => s.push('\''),
                '\"' => s.push('\"'),
                '\\' => s.push('\\'),
                _ => {
                    return Err(UnescapeError::InvalidEscapeSequence(Span::new(
                        Range {
                            start: start_span.range.start + processed,
                            end: start_span.range.start + processed + 1,
                        },
                        start_span.file_id,
                    )))
                }
            };
        }
    }

    Ok(s)
}
