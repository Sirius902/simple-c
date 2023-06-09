use unicode_width::UnicodeWidthChar;

use super::{Lexeme, Location, Token, TokenStream};

pub struct Iterator<'a> {
    pub(crate) stream: Option<TokenStream<'a>>,
    leftover_lexeme: Option<Lexeme<'a>>,
}

impl<'a> Iterator<'a> {
    pub const fn new(stream: TokenStream<'a>) -> Self {
        Self {
            stream: Some(stream),
            leftover_lexeme: None,
        }
    }
}

impl<'a> std::iter::Iterator for Iterator<'a> {
    type Item = Result<Lexeme<'a>, Error<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(lexeme) = self.leftover_lexeme.take() {
            if lexeme.0 == Token::Eof {
                self.stream = None;
            }
            return Some(Ok(lexeme));
        }

        let Some(stream) = self.stream.as_mut() else { return None; };
        let lexeme = stream.next();

        if let Some(Lexeme(Token::Unknown, value, location)) = lexeme {
            let stream_input = stream.input;
            let err_start = stream.index + 1 - value.len();
            let line_start = stream.current_line_index;
            let mut err_end = err_start + value.len();

            loop {
                let next = stream.next();
                match next {
                    Some(Lexeme(Token::Unknown, value, ahead_loc)) => {
                        if ahead_loc.line <= location.line {
                            err_end += value.len();
                            continue;
                        }
                        break;
                    }
                    Some(Lexeme(Token::Eof, _, _)) => {
                        self.stream = None;
                    }
                    _ => (),
                }

                self.leftover_lexeme = next;
                break;
            }

            return Some(Err(Error {
                line_to_eof: &stream_input[line_start..],
                err_start: err_start - line_start,
                err_end: err_end - line_start,
                location,
            }));
        }

        lexeme.map(Ok)
    }
}

#[derive(Debug, thiserror::Error)]
pub struct Error<'a> {
    line_to_eof: &'a str,
    err_start: usize,
    err_end: usize,
    location: Location,
}

impl std::fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const CONTEXT_PARTS: &[&str] = &["", ":", ": "];

        let line_end = self
            .line_to_eof
            .chars()
            .take_while(|&c| c != '\r' && c != '\n')
            .map(char::len_utf8)
            .sum();

        let line = &self.line_to_eof[..line_end];

        let location_len = {
            let num_digits = |n: usize| if n == 0 { 1 } else { n.ilog10() as usize + 1 };
            num_digits(self.location.line) + num_digits(self.location.column)
        };

        let context_len = CONTEXT_PARTS.iter().map(|s| s.len()).sum::<usize>() + location_len;

        writeln!(f, "error: syntax")?;
        writeln!(
            f,
            "{}{}{}{}{}{}",
            CONTEXT_PARTS[0],
            self.location.line,
            CONTEXT_PARTS[1],
            self.location.column,
            CONTEXT_PARTS[2],
            line,
        )?;

        for _ in 0..context_len {
            write!(f, " ")?;
        }

        for (i, width) in line
            .char_indices()
            .map(|(i, c)| (i + c.len_utf8(), UnicodeWidthChar::width(c)))
        {
            let (c, width) = if i == self.err_start {
                ("^", width.unwrap_or(1))
            } else if i >= self.err_start {
                if i >= self.err_end {
                    break;
                }
                ("~", width.unwrap_or(1))
            } else {
                (" ", 1)
            };

            for _ in 0..width {
                write!(f, "{}", c)?;
            }
        }

        Ok(())
    }
}
