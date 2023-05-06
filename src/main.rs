use simple_c::lexer::TokenStream;

pub fn main() {
    let stream = TokenStream::new(r#"49r14.63 c\hicken if (6}"#);

    for res in stream.into_err_iter() {
        match res {
            Ok(lexeme) => println!("{:?}", lexeme),
            Err(e) => print!("\n{}\n\n", e),
        }
    }
}
