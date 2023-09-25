use std::io::{self, BufRead, BufReader, Stdin, Stdout, Write};

use rust_interpreter::{eval, new_env};

const PROMPT: &str = ">> ";

fn main() -> io::Result<()> {
    println!("Time to monkey around");
    repl(io::stdin(), io::stdout())
}

fn repl(stdin: Stdin, stdout: Stdout) -> io::Result<()> {
    let mut reader = BufReader::new(stdin);
    let mut line = String::new();

    let env = new_env(None);

    loop {
        print!("{}", PROMPT);
        stdout.lock().flush().unwrap();
        reader.read_line(&mut line).unwrap();

        if let Some(s) = eval(&line, &env) {
            println!("{}", s)
        }

        line.clear();
    }
}
