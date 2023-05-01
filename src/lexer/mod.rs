use regex::Regex;

pub struct TokenStream<'a> {
    input: &'a str,
    index: usize,
    re: Regex,
    location: Location,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        let re = Regex::new(
            &TOKENS
                .iter()
                .fold(String::new(), |mut acc, &(token, pattern, _)| {
                    if !acc.is_empty() {
                        acc.push('|');
                    }
                    acc.push_str(&format!("(?P<t{}>{})", token as usize, pattern));
                    acc
                }),
        )
        .unwrap();

        Self {
            input,
            index: 0,
            re,
            location: Location::default(),
        }
    }

    pub fn token(&mut self) -> Option<Result<Lexeme<'a>, TokenError>> {
        loop {
            if self.index == self.input.len() {
                return None;
            }

            let Some((i, capture)) = self.re
                .captures_at(self.input, self.index)
                .and_then(|captures| {
                    captures
                        .iter()
                        .skip(1)
                        .enumerate()
                        .find(|&(_, capture)| capture.is_some())
                        .and_then(|(i, capture)| capture.map(|c| (i, c)))
                }) else { break; };

            let lexeme = {
                let (token, _, token_action) = &TOKENS[i];
                token_action(Lexeme(*token, capture.as_str(), self.location))
            };

            self.index += capture.len();

            if let Lexeme(Token::Ignore, value, _) = &lexeme {
                for c in value.chars() {
                    match c {
                        '\r' | '\n' => self.location.next_line(),
                        _ => self.location.column += 1,
                    }
                }
                continue;
            }

            self.location.column += capture.len();
            return Some(Ok(lexeme));
        }

        Some(Err(TokenError))
    }
}

pub struct TokenIterator<'a> {
    stream: TokenStream<'a>,
    errored: bool,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Lexeme<'a>, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.errored {
            None
        } else {
            let lexeme = self.stream.token();
            self.errored = matches!(lexeme, Some(Err(_)));
            lexeme
        }
    }
}

impl<'a> IntoIterator for TokenStream<'a> {
    type Item = Result<Lexeme<'a>, TokenError>;
    type IntoIter = TokenIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TokenIterator {
            stream: self,
            errored: false,
        }
    }
}

pub fn identity(l: Lexeme) -> Lexeme {
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
pub struct TokenError;

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenError")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lexeme<'a>(Token, &'a str, Location);

impl<'a> Lexeme<'a> {
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
    pub fn new(line: usize, column: usize) -> Self {
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
    (Token::Ignore, r"[ \t\r\n]", identity),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn is_longest_match() {
        assert_eq!(
            TokenStream::new("==").token(),
            Some(Ok(Lexeme(Token::Eq, "==", Location::new(1, 1))))
        );
    }

    #[test]
    pub fn matches_from_start() {
        assert_eq!(TokenStream::new("\0<").token(), Some(Err(TokenError)));
    }

    #[test]
    pub fn iter_success() {
        let lexemes = TokenStream::new("49r14.63 chicken if (6}")
            .into_iter()
            .collect::<Result<Vec<_>, _>>();

        assert_eq!(
            lexemes,
            Ok(vec![
                Lexeme(Token::Num, "49", Location::new(1, 1)),
                Lexeme(Token::Id, "r14", Location::new(1, 3)),
                Lexeme(Token::Num, ".63", Location::new(1, 6)),
                Lexeme(Token::Id, "chicken", Location::new(1, 10)),
                Lexeme(Token::If, "if", Location::new(1, 18)),
                Lexeme(Token::LParen, "(", Location::new(1, 21)),
                Lexeme(Token::Num, "6", Location::new(1, 22)),
                Lexeme(Token::RBrace, "}", Location::new(1, 23)),
            ])
        );
    }

    #[test]
    pub fn iter_error() {
        let lexemes = TokenStream::new("\0\0\0\0\0")
            .into_iter()
            .collect::<Vec<_>>();

        assert_eq!(lexemes, vec![Err(TokenError)]);
    }

    #[test]
    pub fn eof() {
        assert_eq!(TokenStream::new("").token(), None);
    }
}
