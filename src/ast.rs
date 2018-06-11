#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Constant(Const),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Func(String, Vec<Expr>),
    Let(Decl, Box<Expr>),
    BinOp(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int(i32),
    Bool(bool),
}

impl ToString for Const {
    fn to_string(&self) -> String {
        match *self {
            Const::Int(ref n) => n.to_string(),
            Const::Bool(ref b) => b.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Var(String, Box<Expr>),
    Func(String, Vec<String>, Box<Expr>),
}

impl ToString for Decl {
    fn to_string(&self) -> String {
        match *self {
            Decl::Var(ref x, ref e) => format!("{} := {}", x, e.to_string()),
            Decl::Func(ref f, ref ops, ref e) => {
                let ops_str: Vec<String> = ops.iter().map(|op| op.to_string()).collect();
                format!("{}({}) := {}", f, ops_str.join(","), e.to_string())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Plus,
    Lt,
}

impl ToString for Op {
    fn to_string(&self) -> String {
        match *self {
            Op::Plus => "+".to_string(),
            Op::Lt => "<".to_string(),
        }
    }
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        use Expr::*;
        match *self {
            Var(ref x) => x.to_string(),
            Constant(ref c) => c.to_string(),
            If(ref e_cond, ref e_then, ref e_else) => format!(
                "if {} then {} else {}",
                e_cond.to_string(),
                e_then.to_string(),
                e_else.to_string()
            ),
            Func(ref f, ref ops) => {
                let ops_str: Vec<String> = ops.iter().map(|op| op.to_string()).collect();
                format!("{}({})", f, ops_str.join(","))
            }
            Let(ref decl, ref e) => format!("let {} in {}", decl.to_string(), e.to_string()),
            BinOp(ref op, ref e_left, ref e_right) => format!(
                "{} {} {}",
                e_left.to_string(),
                op.to_string(),
                e_right.to_string()
            ),
        }
    }
}
