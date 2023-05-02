use std::io::Read;

use simple_c::lexer::TokenStream;

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut stdin = std::io::stdin();
    let input = {
        let mut s = String::new();
        let _ = stdin.read_to_string(&mut s)?;
        s
    };

    let stream = TokenStream::new(&input);

    for res in stream.into_err_iter() {
        match res {
            Ok(lexeme) => println!("{:?}", lexeme),
            Err(e) => println!("{}", e),
        }
    }

    Ok(())
}
