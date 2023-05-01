use simple_c::lexer::TokenStream;

pub fn main() {
    let stream = TokenStream::new(r"49r14.63 chicken if (6}");

    for lexeme in stream.into_iter() {
        println!("{:?}", lexeme);
    }
}
