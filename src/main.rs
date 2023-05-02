use simple_c::lexer::TokenStream;

pub fn main() {
    let stream = TokenStream::new(r"49r14.63 chicken if (6}");

    for res in stream.into_iter() {
        match res {
            Ok(lexeme) => println!("{:?}", lexeme),
            Err(e) => println!("{}", e),
        }
    }
}
