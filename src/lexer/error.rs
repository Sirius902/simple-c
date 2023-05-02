use super::{Lexeme, Location, Token, TokenStream};

pub struct ErrorIterator<'a> {
    pub(crate) stream: Option<TokenStream<'a>>,
}

impl<'a> Iterator for ErrorIterator<'a> {
    type Item = Result<Lexeme<'a>, Error<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(stream) = self.stream.as_mut() else { return None; };
        let lexeme = stream.next();

        if let Some(Lexeme(Token::Unknown, value, location)) = lexeme {
            let err_start = stream.index + 1 - value.len();
            let line_start = stream.current_line_index;
            let mut err_end = err_start + value.len();

            while let Some(Lexeme(Token::Unknown, value, ahead_loc)) = stream.next() {
                if ahead_loc.line <= location.line {
                    err_end += value.len();
                } else {
                    break;
                }
            }

            let input = stream.input;
            self.stream = None;

            return Some(Err(Error {
                line_to_eof: &input[line_start..],
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
        let line_end = self
            .line_to_eof
            .chars()
            .take_while(|&c| c != '\r' && c != '\n')
            .map(|c| c.len_utf8())
            .sum::<usize>();

        let line = &self.line_to_eof[..line_end];

        let context_before = "line: ";
        let context_after = ": ";
        let line_num_size = if self.location.line == 0 {
            1
        } else {
            self.location.line.ilog10() + 1
        };
        let context_len = context_before.len() + line_num_size as usize + context_after.len();

        writeln!(f, "error: syntax")?;
        writeln!(
            f,
            "{}{}{}{}",
            context_before, self.location.line, context_after, line
        )?;

        for _ in 0..context_len {
            write!(f, " ")?;
        }

        for i in line.char_indices().map(|(i, c)| i + c.len_utf8()) {
            let c = if i == self.err_start {
                "^"
            } else if i >= self.err_start {
                if i >= self.err_end {
                    break;
                }
                "~"
            } else {
                " "
            };

            write!(f, "{}", c)?;
        }

        writeln!(f)?;

        for _ in 0..context_len - 1 + self.err_start {
            write!(f, " ")?;
        }

        writeln!(f, "{}", self.location.column)
    }
}
