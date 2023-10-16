use super::*;
use crate::errors::MonkeyError;

struct T<'a> {
    token: TokenKind,
    slice: &'a str,
}

impl<'a> std::fmt::Debug for T<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(token: '{}', slice: '{}')", self.token, self.slice)
    }
}

enum K<'a> {
    Token(T<'a>),
    Error(MonkeyError),
}

impl<'a> std::fmt::Debug for K<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(t) => write!(f, "Token{:?}", t),
            Self::Error(err) => write!(f, "Error({})", err),
        }
    }
}

fn lex(input: &str) -> Vec<Result<Token>> {
    let lexer = Lexer::new(input);
    lexer.collect()
}

fn debug_print(input: &str, token_results: Vec<Result<Token>>) -> Vec<K> {
    let rows = input.lines().collect::<Vec<_>>();

    let mut ret = Vec::new();
    for result in token_results {
        match result {
            Ok(token) => {
                println!("{:?}", token);
                let row_num = rows[token.row_num()];
                let slice = &row_num[token.span()];
                ret.push(K::Token(T {
                    token: token.take(),
                    slice,
                }));
            }
            Err(err) => ret.push(K::Error(err.take())),
        }
    }
    ret
}

macro_rules! snapshot {
    ($name:tt, $input:expr) => {
        #[test]
        fn $name() {
            let tokens = lex($input);
            insta::with_settings!({
                description => $input,
            }, {
                insta::assert_debug_snapshot!(debug_print($input, tokens));
            })
        }
    };
}

snapshot!(basic, "=");
snapshot!(symbols, "=+(){},;");
snapshot!(assignments1, "let five = 5;");
snapshot!(assignments2, "let ten = 10;");
snapshot!(
    assignments3,
    r"let add = fn(x, y) {
        x + y;
    };"
);
snapshot!(assignments4, "let result = add(five, ten);");

snapshot!(operators, "!-/*5;");
snapshot!(equality1, "5 < 10 > 5;");
snapshot!(equality2, "10 == 10;");
snapshot!(equality3, "10 != 9;");
snapshot!(
    conditional,
    r"if (5 < 10) {
        return true;
    } else {
        return false;
    }"
);

snapshot!(string_literal, r#""hello world";"#);
snapshot!(arrays, "[1, 2]");
snapshot!(dictionary, r#"{ "foo": "bar" };"#);
