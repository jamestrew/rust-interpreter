use std::rc::Rc;

use super::environment::Environment;
use super::object::*;
use super::ObjResult;
use crate::ast::Identifier;

fn len(args: &[Rc<Object>]) -> ObjResult {
    check_arg_length(args, 1, "len")?;
    match args[0].as_ref() {
        Object::String(val) => Ok(Rc::new(Object::Int(val.len() as i64))),
        obj => Err(anyhow::anyhow!(
            "object of type {} has no len()",
            obj.type_str()
        )),
    }
}

fn puts(args: &[Rc<Object>]) -> ObjResult {
    for arg in args {
        match arg.as_ref() {
            Object::Int(val) => print!("{}", val),
            Object::Bool(val) => print!("{}", val),
            Object::String(val) => print!("{}", val),
            Object::Nil => print!("nil"),
            obj => print!("{:?}", std::ptr::addr_of!(obj)),
        };
    }
    println!();
    Ok(Rc::new(EMPTY))
}

fn check_arg_length(args: &[Rc<Object>], count: usize, fn_name: &str) -> anyhow::Result<()> {
    if args.len() != count {
        return Err(anyhow::anyhow!(
            "{} takes exactly {} argument ({} given)",
            fn_name,
            count,
            args.len()
        ));
    }
    Ok(())
}

#[derive(Debug)]
pub enum Builtin {
    Len,
    Puts,
}

impl Builtin {
    pub fn register(env: &mut Environment) {
        env.set(&Identifier::new("len"), Rc::new(Object::Builtin(Self::Len)));
        env.set(
            &Identifier::new("puts"),
            Rc::new(Object::Builtin(Self::Puts)),
        );
    }

    pub fn execute(&self, args: &[Rc<Object>]) -> ObjResult {
        match self {
            Builtin::Len => len(args),
            Builtin::Puts => puts(args),
        }
    }
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fn_name = match self {
            Builtin::Len => "len",
            Builtin::Puts => "puts",
        };
        write!(f, "<built-in function {}>", fn_name)
    }
}
