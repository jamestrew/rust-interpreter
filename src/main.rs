use std::io::{self, BufRead, BufReader, Stdin, Stdout, Write};

use rust_interpreter::lexer::Lexer;

const PROMPT: &str = ">> ";

fn main() -> io::Result<()> {
    println!("Time to monkey around");
    repl(io::stdin(), io::stdout())
}

fn repl(stdin: Stdin, stdout: Stdout) -> io::Result<()> {
    let mut reader = BufReader::new(stdin);
    let mut line = String::new();

    loop {
        print!("{}", PROMPT);
        stdout.lock().flush().unwrap();
        reader.read_line(&mut line).unwrap();

        let lexer = Lexer::new(&line);
        for token in lexer {
            println!("{:?}", token);
        }

        line.clear();
    }
}
