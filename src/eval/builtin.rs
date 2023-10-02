use std::rc::Rc;

use super::environment::Environment;
use super::object::*;
use super::ObjResult;

fn len(args: &[Rc<Object>]) -> ObjResult {
    check_arg_length(args, 1, "len")?;
    match args[0].as_ref() {
        Object::String(val) => Ok(Rc::new(Object::Int(val.len() as i64))),
        Object::Array(val) => Ok(Rc::new(Object::Int(val.len() as i64))),
        Object::Hash(val) => Ok(Rc::new(Object::Int(val.len() as i64))),
        obj => Err(anyhow::anyhow!(
            "object of type {} has no len()",
            obj.type_str()
        )),
    }
}

fn puts(args: &[Rc<Object>]) -> ObjResult {
    for arg in args {
        match arg.as_ref() {
            Object::Int(_) => println!("{}", arg),
            Object::Bool(_) => println!("{}", arg),
            Object::String(_) => println!("{}", arg),
            Object::Array(_) => println!("{}", arg),
            Object::Hash(_) => println!("{}", arg),
            Object::Nil => println!("nil"),
            obj => println!("{:?} - {}", std::ptr::addr_of!(obj), obj.type_str()),
        };
    }
    Ok(Rc::new(EMPTY))
}

fn first(args: &[Rc<Object>]) -> ObjResult {
    check_arg_length(args, 1, "first")?;
    if let Object::Array(arr) = args[0].as_ref() {
        return Ok(Rc::clone(arr.first().unwrap_or(&Rc::new(NIL))));
    }

    Err(not_callable("first", args[0].as_ref()))
}

fn last(args: &[Rc<Object>]) -> ObjResult {
    if let Object::Array(arr) = args[0].as_ref() {
        return Ok(Rc::clone(arr.last().unwrap_or(&Rc::new(NIL))));
    }

    Err(not_callable("last", args[0].as_ref()))
}

fn rest(args: &[Rc<Object>]) -> ObjResult {
    check_arg_length(args, 1, "rest")?;
    if let Object::Array(arr) = args[0].as_ref() {
        if arr.is_empty() {
            return Ok(Rc::new(NIL));
        }
        return Ok(Rc::new(Object::Array(arr[1..arr.len()].to_owned())));
    }

    Err(not_callable("rest", args[0].as_ref()))
}

fn push(args: &[Rc<Object>]) -> ObjResult {
    check_arg_length(args, 2, "push")?;
    if let Object::Array(arr) = args[0].as_ref() {
        let push_val = Rc::clone(&args[1]);
        if arr.is_empty() {
            return Ok(Rc::new(Object::Array(vec![push_val])));
        }
        let mut arr = arr.to_vec();
        arr.push(push_val);
        return Ok(Rc::new(Object::Array(arr)));
    }

    Err(not_callable("push", args[0].as_ref()))
}

fn check_arg_length(args: &[Rc<Object>], count: usize, fn_name: &str) -> anyhow::Result<()> {
    if args.len() != count {
        return Err(anyhow::anyhow!(
            "{} takes exactly {} argument(s) ({} given)",
            fn_name,
            count,
            args.len()
        ));
    }
    Ok(())
}

fn not_callable(fn_name: &str, obj: &Object) -> anyhow::Error {
    anyhow::anyhow!("'{}' not callable on {}", fn_name, obj.type_str())
}

#[derive(Debug)]
pub enum Builtin {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

impl Builtin {
    pub fn register(env: &mut Environment) {
        let mut reg = |f: Self| env.set(Rc::from(f.as_str()), Rc::new(Object::Builtin(f)));

        reg(Self::Len);
        reg(Self::Puts);
        reg(Self::First);
        reg(Self::Last);
        reg(Self::Rest);
        reg(Self::Push);
    }

    pub fn execute(&self, args: &[Rc<Object>]) -> ObjResult {
        match self {
            Builtin::Len => len(args),
            Builtin::Puts => puts(args),
            Builtin::First => first(args),
            Builtin::Last => last(args),
            Builtin::Rest => rest(args),
            Builtin::Push => push(args),
        }
    }

    fn as_str(&self) -> &'static str {
        match self {
            Builtin::Len => "len",
            Builtin::Puts => "puts",
            Builtin::First => "first",
            Builtin::Last => "last",
            Builtin::Rest => "rest",
            Builtin::Push => "push",
        }
    }
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<built-in function {}>", self.as_str())
    }
}
