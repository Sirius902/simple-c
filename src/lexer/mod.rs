use once_cell::sync::Lazy;
use regex::{Regex, RegexBuilder};

mod error;

static TOKEN_REGEX: Lazy<Regex> = Lazy::new(|| {
    RegexBuilder::new(
        &TOKENS
            .iter()
            .fold(String::new(), |mut acc, &(token, pattern, _)| {
                if !acc.is_empty() {
                    acc.push('|');
                }
                acc.push_str(&format!("^(?P<t{}>{})", token as usize, pattern));
                acc
            }),
    )
    .dot_matches_new_line(true)
    .build()
    .unwrap()
});

pub struct TokenStream<'a> {
    input: &'a str,
    index: usize,
    current_line_index: usize,
    location: Location,
    reached_eof: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            index: 0,
            current_line_index: 0,
            location: Location::default(),
            reached_eof: false,
        }
    }

    fn next(&mut self) -> Option<Lexeme<'a>> {
        let token_regex = &*TOKEN_REGEX;
        loop {
            if self.index == self.input.len() {
                return if self.reached_eof {
                    None
                } else {
                    self.reached_eof = true;
                    Some(Lexeme(Token::Eof, "", self.location))
                };
            }

            let Some((i, capture)) = token_regex
                .captures(&self.input[self.index..])
                .and_then(|captures| {
                    captures
                        .iter()
                        .skip(1)
                        .enumerate()
                        .find(|&(_, capture)| capture.is_some())
                        .and_then(|(i, capture)| capture.map(|c| (i, c)))
                }) else { unreachable!() };

            let lexeme = {
                let (token, _, token_action) = &TOKENS[i];
                token_action(Lexeme(*token, capture.as_str(), self.location))
            };

            self.index += capture.len();

            if let Lexeme(Token::Ignore, value, _) = &lexeme {
                match *value {
                    "\r\n" | "\r" | "\n" => {
                        self.location.next_line();
                        self.current_line_index = self.index;
                    }
                    _ => self.location.column += capture.len(),
                }
                continue;
            }

            self.location.column += match lexeme.0 {
                Token::Unknown => 1,
                _ => capture.len(),
            };

            return Some(lexeme);
        }
    }

    pub const fn into_err_iter(self) -> error::Iterator<'a> {
        error::Iterator { stream: Some(self) }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Lexeme<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

pub const fn identity(l: Lexeme) -> Lexeme {
    l
}

pub fn refine_keywords(l: Lexeme) -> Lexeme {
    match l {
        Lexeme(Token::Id, value, _) => match value {
            "int" | "float" => l.map_token(|_| Token::Type),
            "if" => l.map_token(|_| Token::If),
            "else" => l.map_token(|_| Token::Else),
            "for" => l.map_token(|_| Token::For),
            _ => l,
        },
        _ => l,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub struct TokenError(Location);

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{} syntax error", self.0.line, self.0.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lexeme<'a>(Token, &'a str, Location);

impl Lexeme<'_> {
    pub fn map_token(mut self, f: impl Fn(Token) -> Token) -> Self {
        self.0 = f(self.0);
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    line: usize,
    column: usize,
}

impl Location {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 1;
    }
}

impl Default for Location {
    fn default() -> Self {
        Self::new(1, 1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Id,
    Num,
    Eq,
    Plus,
    Sub,
    Mult,
    Div,
    Assign,
    Lt,
    Semi,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Ignore,
    Type,
    If,
    Else,
    For,
    Eof,
    Unknown,
}

pub type TokenAction = fn(Lexeme) -> Lexeme;

pub const TOKENS: &[(Token, &str, TokenAction)] = &[
    (Token::Id, r"[a-zA-Z][a-zA-Z0-9]*", refine_keywords),
    (Token::Num, r"[0-9]*\.?[0-9]+", identity),
    (Token::Eq, r"==", identity),
    (Token::Plus, r"\+", identity),
    (Token::Sub, r"-", identity),
    (Token::Mult, r"\*", identity),
    (Token::Div, r"\/", identity),
    (Token::Assign, r"=", identity),
    (Token::Lt, r"<", identity),
    (Token::Semi, r";", identity),
    (Token::LParen, r"\(", identity),
    (Token::RParen, r"\)", identity),
    (Token::LBrace, r"\{", identity),
    (Token::RBrace, r"\}", identity),
    (Token::Ignore, r"\r\n|[ \t\r\n]", identity),
    (Token::Unknown, r".", identity),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn is_longest_match() {
        assert_eq!(
            TokenStream::new("==").next(),
            Some(Lexeme(Token::Eq, "==", Location::new(1, 1)))
        );
    }

    #[test]
    pub fn matches_from_start() {
        assert_eq!(
            TokenStream::new("\0<").into_iter().collect::<Vec<_>>(),
            vec![
                Lexeme(Token::Unknown, "\0", Location::new(1, 1)),
                Lexeme(Token::Lt, "<", Location::new(1, 2)),
                Lexeme(Token::Eof, "", Location::new(1, 3)),
            ]
        );
    }

    #[test]
    pub fn iter_success() {
        let lexemes = TokenStream::new("49r14.63 chicken if (6}")
            .into_iter()
            .collect::<Vec<_>>();

        assert_eq!(
            lexemes,
            vec![
                Lexeme(Token::Num, "49", Location::new(1, 1)),
                Lexeme(Token::Id, "r14", Location::new(1, 3)),
                Lexeme(Token::Num, ".63", Location::new(1, 6)),
                Lexeme(Token::Id, "chicken", Location::new(1, 10)),
                Lexeme(Token::If, "if", Location::new(1, 18)),
                Lexeme(Token::LParen, "(", Location::new(1, 21)),
                Lexeme(Token::Num, "6", Location::new(1, 22)),
                Lexeme(Token::RBrace, "}", Location::new(1, 23)),
                Lexeme(Token::Eof, "", Location::new(1, 24)),
            ]
        );
    }

    #[test]
    pub fn iter_error() {
        let lexemes = TokenStream::new("a\0→\0\0\0")
            .into_iter()
            .collect::<Vec<_>>();

        assert_eq!(
            lexemes,
            vec![
                Lexeme(Token::Id, "a", Location::new(1, 1)),
                Lexeme(Token::Unknown, "\0", Location::new(1, 2)),
                Lexeme(Token::Unknown, "→", Location::new(1, 3)),
                Lexeme(Token::Unknown, "\0", Location::new(1, 4)),
                Lexeme(Token::Unknown, "\0", Location::new(1, 5)),
                Lexeme(Token::Unknown, "\0", Location::new(1, 6)),
                Lexeme(Token::Eof, "", Location::new(1, 7)),
            ]
        );
    }

    #[test]
    pub fn eof() {
        assert_eq!(
            {
                let mut s = TokenStream::new("");
                (s.next(), s.next())
            },
            (Some(Lexeme(Token::Eof, "", Location::new(1, 1))), None)
        );
        assert_eq!(
            {
                let mut s = TokenStream::new("a");
                (s.next(), s.next(), s.next())
            },
            (
                Some(Lexeme(Token::Id, "a", Location::new(1, 1))),
                Some(Lexeme(Token::Eof, "", Location::new(1, 2))),
                None
            )
        );
    }

    #[test]
    pub fn location_works() {
        assert_eq!(
            TokenStream::new("a;b").into_iter().collect::<Vec<_>>(),
            vec![
                Lexeme(Token::Id, "a", Location::new(1, 1)),
                Lexeme(Token::Semi, ";", Location::new(1, 2)),
                Lexeme(Token::Id, "b", Location::new(1, 3)),
                Lexeme(Token::Eof, "", Location::new(1, 4)),
            ]
        );

        assert_eq!(
            TokenStream::new(" a\nb c\rd\r\ne\r\n\r\nf ")
                .into_iter()
                .collect::<Vec<_>>(),
            vec![
                Lexeme(Token::Id, "a", Location::new(1, 2)),
                Lexeme(Token::Id, "b", Location::new(2, 1)),
                Lexeme(Token::Id, "c", Location::new(2, 3)),
                Lexeme(Token::Id, "d", Location::new(3, 1)),
                Lexeme(Token::Id, "e", Location::new(4, 1)),
                Lexeme(Token::Id, "f", Location::new(6, 1)),
                Lexeme(Token::Eof, "", Location::new(6, 3)),
            ]
        );
    }

    #[test]
    pub fn large_program() {
        let input = "float x = 4.0;\n\
            x = x + 1;\n\
            if (x == 5) {\n\
                for (int i = 0; i < 5; i = i + 1) {}\n\
            } else {\n\
                int a = 1 + 3 * x * 2 / 6 - 6.0;\n\
            }\n\
            x = x - 1;";

        let lexemes = TokenStream::new(input).into_iter().collect::<Vec<_>>();

        assert_eq!(
            lexemes,
            vec![
                Lexeme(Token::Type, "float", Location::new(1, 1)),
                Lexeme(Token::Id, "x", Location::new(1, 7)),
                Lexeme(Token::Assign, "=", Location::new(1, 9)),
                Lexeme(Token::Num, "4.0", Location::new(1, 11)),
                Lexeme(Token::Semi, ";", Location::new(1, 14)),
                Lexeme(Token::Id, "x", Location::new(2, 1)),
                Lexeme(Token::Assign, "=", Location::new(2, 3)),
                Lexeme(Token::Id, "x", Location::new(2, 5)),
                Lexeme(Token::Plus, "+", Location::new(2, 7)),
                Lexeme(Token::Num, "1", Location::new(2, 9)),
                Lexeme(Token::Semi, ";", Location::new(2, 10)),
                Lexeme(Token::If, "if", Location::new(3, 1)),
                Lexeme(Token::LParen, "(", Location::new(3, 4)),
                Lexeme(Token::Id, "x", Location::new(3, 5)),
                Lexeme(Token::Eq, "==", Location::new(3, 7)),
                Lexeme(Token::Num, "5", Location::new(3, 10)),
                Lexeme(Token::RParen, ")", Location::new(3, 11)),
                Lexeme(Token::LBrace, "{", Location::new(3, 13)),
                Lexeme(Token::For, "for", Location::new(4, 1)),
                Lexeme(Token::LParen, "(", Location::new(4, 5)),
                Lexeme(Token::Type, "int", Location::new(4, 6)),
                Lexeme(Token::Id, "i", Location::new(4, 10)),
                Lexeme(Token::Assign, "=", Location::new(4, 12)),
                Lexeme(Token::Num, "0", Location::new(4, 14)),
                Lexeme(Token::Semi, ";", Location::new(4, 15)),
                Lexeme(Token::Id, "i", Location::new(4, 17)),
                Lexeme(Token::Lt, "<", Location::new(4, 19)),
                Lexeme(Token::Num, "5", Location::new(4, 21)),
                Lexeme(Token::Semi, ";", Location::new(4, 22)),
                Lexeme(Token::Id, "i", Location::new(4, 24)),
                Lexeme(Token::Assign, "=", Location::new(4, 26)),
                Lexeme(Token::Id, "i", Location::new(4, 28)),
                Lexeme(Token::Plus, "+", Location::new(4, 30)),
                Lexeme(Token::Num, "1", Location::new(4, 32)),
                Lexeme(Token::RParen, ")", Location::new(4, 33)),
                Lexeme(Token::LBrace, "{", Location::new(4, 35)),
                Lexeme(Token::RBrace, "}", Location::new(4, 36)),
                Lexeme(Token::RBrace, "}", Location::new(5, 1)),
                Lexeme(Token::Else, "else", Location::new(5, 3)),
                Lexeme(Token::LBrace, "{", Location::new(5, 8)),
                Lexeme(Token::Type, "int", Location::new(6, 1)),
                Lexeme(Token::Id, "a", Location::new(6, 5)),
                Lexeme(Token::Assign, "=", Location::new(6, 7)),
                Lexeme(Token::Num, "1", Location::new(6, 9)),
                Lexeme(Token::Plus, "+", Location::new(6, 11)),
                Lexeme(Token::Num, "3", Location::new(6, 13)),
                Lexeme(Token::Mult, "*", Location::new(6, 15)),
                Lexeme(Token::Id, "x", Location::new(6, 17)),
                Lexeme(Token::Mult, "*", Location::new(6, 19)),
                Lexeme(Token::Num, "2", Location::new(6, 21)),
                Lexeme(Token::Div, "/", Location::new(6, 23)),
                Lexeme(Token::Num, "6", Location::new(6, 25)),
                Lexeme(Token::Sub, "-", Location::new(6, 27)),
                Lexeme(Token::Num, "6.0", Location::new(6, 29)),
                Lexeme(Token::Semi, ";", Location::new(6, 32)),
                Lexeme(Token::RBrace, "}", Location::new(7, 1)),
                Lexeme(Token::Id, "x", Location::new(8, 1)),
                Lexeme(Token::Assign, "=", Location::new(8, 3)),
                Lexeme(Token::Id, "x", Location::new(8, 5)),
                Lexeme(Token::Sub, "-", Location::new(8, 7)),
                Lexeme(Token::Num, "1", Location::new(8, 9)),
                Lexeme(Token::Semi, ";", Location::new(8, 10)),
                Lexeme(Token::Eof, "", Location::new(8, 11)),
            ]
        );
    }
}
