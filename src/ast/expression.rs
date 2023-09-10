use std::fmt::{Debug, Display};
use std::rc::Rc;

use super::Node;
use crate::lexer::Token;
use crate::parser::*;

#[derive(PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Primative(Primative),
    StringLiteral(Rc<str>),
    Prefix(Prefix),
    Infix(Infix),
}

impl Node for Expression {}

impl Expression {
    pub fn parse(parser: &mut Parser, precedence: Precedence) -> anyhow::Result<Self> {
        let token = parser.current_token()?;
        let mut expr = match token {
            Token::Int(_) | Token::True | Token::False => {
                Expression::Primative(Primative::parse(parser)?)
            }
            Token::Str(s) => Expression::StringLiteral(s.clone()),
            Token::Identifier(_) => Expression::Identifier(Identifier::parse(parser)?),
            Token::Minus | Token::Bang => Expression::Prefix(Prefix::parse(parser)?),
            _ => todo!("Expression::parse for {:?}", token),
        };

        while precedence < parser.peek_precedence()? {
            expr = Expression::Infix(Infix::parse(parser, expr)?);
        }
        parser.swallow_semicolons();
        Ok(expr)
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(val) => write!(f, "{:?}", val),
            Expression::Primative(val) => write!(f, "{:?}", val),
            Expression::StringLiteral(val) => write!(f, "StringLiteral({:?})", val),
            Expression::Prefix(val) => write!(f, "{:?}", val),
            Expression::Infix(val) => write!(f, "{:?}", val),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expression::Identifier(val) => val.to_string(),
            Expression::Primative(val) => val.to_string(),
            Expression::StringLiteral(val) => format!("\"{}\"", val),
            Expression::Prefix(val) => val.to_string(),
            Expression::Infix(val) => val.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(PartialEq, Clone)]
pub struct Identifier {
    token: Token,
    value: Rc<str>,
}

impl Node for Identifier {}

impl Identifier {
    pub fn parse(parser: &Parser) -> anyhow::Result<Self> {
        Self::try_from(parser.current_token()?)
    }
}

impl<'a> TryFrom<&'a Token> for Identifier {
    type Error = anyhow::Error;

    fn try_from(token: &'a Token) -> anyhow::Result<Self> {
        if let Token::Identifier(ident) = token {
            Ok(Self {
                token: token.clone(),
                value: ident.clone(),
            })
        } else {
            Err(anyhow::anyhow!(
                "Identifier can only be created from an Identifier token"
            ))
        }
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Identifier({:?})", self.value)
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub enum Primative {
    Int(i64),
    Bool(bool),
}

impl Node for Primative {}

impl Primative {
    pub fn parse(parser: &Parser) -> anyhow::Result<Self> {
        let token = parser.current_token()?;
        match token {
            Token::Int(val) => Ok(Self::Int(val.parse::<i64>()?)),
            Token::True => Ok(Self::Bool(true)),
            Token::False => Ok(Self::Bool(false)),
            _ => unreachable!("Primative parse unexpected {:?}", token),
        }
    }
}

impl Debug for Primative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(val) => write!(f, "Int({val})"),
            Self::Bool(val) => write!(f, "Bool({val})"),
        }
    }
}

impl Display for Primative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primative::Int(val) => write!(f, "{}", val),
            Primative::Bool(val) => write!(f, "{}", val),
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Prefix {
    token: Token,
    right: Box<Expression>,
}

impl Node for Prefix {}

impl Prefix {
    pub fn parse(parser: &mut Parser) -> anyhow::Result<Self> {
        let operator = parser.current_token()?.clone();
        parser.next_token();
        let right = Expression::parse(parser, Precedence::Prefix)?;
        Ok(Self {
            token: operator,
            right: Box::new(right),
        })
    }
    fn operator_str(&self) -> &'static str {
        match self.token {
            Token::Minus => "-",
            Token::Bang => "!",
            _ => unreachable!("only Bang and Minus are allowed prefixes"),
        }
    }
}

impl Debug for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Prefix")
            .field("token", &self.operator_str())
            .field("right", &self.right)
            .finish()
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator_str(), self.right)
    }
}

#[derive(PartialEq, Clone)]
pub struct Infix {
    token: Token,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl Node for Infix {}

impl Infix {
    fn operator_str(&self) -> &'static str {
        match self.token {
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Asterisk => "*",
            Token::ForwardSlash => "/",
            Token::Equal => "==",
            Token::NotEqual => "!=",
            Token::LT => "<",
            Token::GT => ">",
            _ => unreachable!("unallowed infix operator {:?}", self.token),
        }
    }

    fn parse(parser: &mut Parser, left: Expression) -> anyhow::Result<Self> {
        parser.next_token();
        let op_token = parser.current_token()?.clone();
        let op_precedence = parser.current_precedence()?;
        parser.next_token();
        let right = Expression::parse(parser, op_precedence)?;
        Ok(Self {
            token: op_token,
            left: Box::new(left),
            right: Box::new(right),
        })
    }
}

impl Debug for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Infix")
            .field("token", &self.operator_str())
            .field("left", &self.left)
            .field("right", &self.right)
            .finish()
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator_str(), self.right)
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Statement;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse(input: &str) -> Vec<Statement> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_programe().expect("valid program");
        assert!(!program.statements.is_empty());
        program.statements
    }

    #[test]
    fn swallow_extra_semicolons() {
        let statements = parse("foobar;;;;");
        let first = statements[0].to_string();
        assert_eq!(first, "foobar;");
    }

    macro_rules! test {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let statements = parse($input);
                let first = statements[0].to_string();
                assert_eq!($input, first);
            }
        };
    }

    macro_rules! snapshot {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let statements = parse($input);
                let first = &statements[0];
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_display_snapshot!(first);
                })
            }
        };
    }

    macro_rules! snapshot_debug {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let statements = parse($input);
                let first = &statements[0];
                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_debug_snapshot!(first);
                })
            }
        };
    }

    test!(identifier, "foobar;");
    test!(integer_literal, "1;");
    test!(boolean_literal, "true;");
    test!(string_literal, "\"hello world\";");

    snapshot_debug!(prefix_expression_1, "-5;");
    snapshot_debug!(prefix_expression_2, "!foobar;");

    snapshot_debug!(infix_expr_1, "5 + 6;");
    snapshot_debug!(infix_expr_2, "5 - 6;");
    snapshot_debug!(infix_expr_3, "5 * 6;");
    snapshot_debug!(infix_expr_4, "5 / 6;");
    snapshot_debug!(infix_expr_5, "true == true;");
    snapshot_debug!(infix_expr_6, "true != false;");
    snapshot_debug!(infix_expr_7, "5 < 6");
    snapshot_debug!(infix_expr_8, "7 > 6");
    snapshot_debug!(infix_expr_9, "\"foo\" != \"bar\"");

    snapshot!(operator_precedence_1, "-a * b");
    snapshot!(operator_precedence_2, "!-a");
    snapshot!(operator_precedence_3, "a + b + c");
    snapshot!(operator_precedence_4, "a + b - c");
    snapshot!(operator_precedence_5, "a * b * c");
    snapshot!(operator_precedence_6, "a * b / c");
    snapshot!(operator_precedence_7, "a + b / c");
    snapshot!(operator_precedence_8, "a + b * c + d / e - f");
    snapshot!(operator_precedence_9, "5 > 4 == 3 < 4");
    snapshot!(operator_precedence_10, "5 < 4 != 3 > 4");
    snapshot!(operator_precedence_11, "3 + 4 * 5 == 3 * 1 + 4 * 5");
    snapshot!(operator_precedence_12, "true");
    snapshot!(operator_precedence_13, "false");
    snapshot!(operator_precedence_14, "3 > 5 == false");
    snapshot!(operator_precedence_15, "3 < 5 == true");
    // snapshot!(operator_precedence_16, "1 + (2 + 3) + 4");
    // snapshot!(operator_precedence_17, "(5 + 5) * 2");
    // snapshot!(operator_precedence_18, "2 / (5 + 5)");
    // snapshot!(operator_precedence_19, "-(5 + 5)");
    // snapshot!(operator_precedence_20, "!(true == true)");
    // snapshot!(operator_precedence_21, "a + add(b * c) + d");
    // snapshot!(
    //     operator_precedence_22,
    //     "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
    // );
    // snapshot!(operator_precedence_23, "add(a + b + c * d / f + g)");
    // snapshot!(operator_precedence_24, "a * [1, 2, 3, 4][b * c] * d");
    // snapshot!(operator_precedence_25, "add(a * b[2], b[1], 2 * [1, 2][1])");
}
