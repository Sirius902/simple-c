use super::Location;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Context<'a> {
    line: &'a str,
    location: Location,
}
