use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Var(String),
    Constant(Const),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Func(String, Vec<Expr>),
    Let(Decl, Box<Expr>),
    BinOp(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
enum Const {
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
enum Decl {
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
enum Op {
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

#[derive(Debug, Clone, PartialEq)]
enum BaseType {
    Int,
    Bool,
}

impl ToString for BaseType {
    fn to_string(&self) -> String {
        match *self {
            BaseType::Int => "Int".to_string(),
            BaseType::Bool => "Bool".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Var(String),
    Base(BaseType),
    Func(Vec<Type>, Box<Type>),
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match *self {
            Type::Var(ref a) => a.to_string(),
            Type::Base(ref basetype) => basetype.to_string(),
            Type::Func(ref optypes, ref valtype) => {
                let optypes_string: Vec<String> = optypes.iter().map(|t| t.to_string()).collect();
                format!("({})->{}", optypes_string.join(","), valtype.to_string())
            }
        }
    }
}

type TypeEnv = HashMap<String, Type>;

type Equality = (Type, Type);
type Constraints = VecDeque<Equality>;

fn merge(c_dst: &mut Constraints, mut c_src: Constraints) {
    while !c_src.is_empty() {
        let c = c_src.pop_front().unwrap();
        c_dst.push_back(c);
    }
}

fn op2type(op: &Op) -> Type {
    match *op {
        Op::Plus => Type::Func(
            vec![Type::Base(BaseType::Int), Type::Base(BaseType::Int)], // (Int,Int) -> Int
            Box::new(Type::Base(BaseType::Int)),
        ),
        Op::Lt => Type::Func(
            vec![Type::Base(BaseType::Int), Type::Base(BaseType::Int)], // (Int,Int) -> Bool
            Box::new(Type::Base(BaseType::Bool)),
        ),
    }
}

struct TypeInf {
    cnt: usize,
}

impl TypeInf {
    pub fn new() -> Self {
        TypeInf { cnt: 0 }
    }

    fn get_type_variable(&mut self) -> Type {
        let a = format!("a{}", self.cnt);
        self.cnt = self.cnt + 1;
        Type::Var(a)
    }
    pub fn type_inf(&mut self, tenv: &TypeEnv, e: &Expr) -> (Type, Constraints) {
        use Expr::*;
        match *e {
            Var(ref x) => match tenv.get(x) {
                Some(t) => (t.clone(), Constraints::new()),
                None => {
                    panic!("error");
                }
            },
            Constant(ref constant) => match *constant {
                Const::Int(_) => (Type::Base(BaseType::Int), Constraints::new()),
                Const::Bool(_) => (Type::Base(BaseType::Bool), Constraints::new()),
            },
            BinOp(ref op, ref e_left, ref e_right) => {
                let (t_left, c_left) = self.type_inf(tenv, e_left);
                let (t_right, c_right) = self.type_inf(tenv, e_right);
                let mut c = Constraints::new();
                merge(&mut c, c_left);
                merge(&mut c, c_right);

                let alpha = self.get_type_variable();
                c.push_back((
                    op2type(&op),
                    Type::Func(vec![t_left, t_right], Box::new(alpha.clone())),
                ));
                (alpha, c)
            }
            If(ref e_cond, ref e_then, ref e_else) => {
                let (t_cond, c_cond) = self.type_inf(tenv, e_cond);
                let (t_then, c_then) = self.type_inf(tenv, e_then);
                let (t_else, c_else) = self.type_inf(tenv, e_else);
                let mut c = Constraints::new();
                merge(&mut c, c_cond);
                merge(&mut c, c_then);
                merge(&mut c, c_else);
                c.push_back((t_cond, Type::Base(BaseType::Bool)));
                c.push_back((t_then.clone(), t_else));
                (t_then, c)
            }
            Func(ref f, ref ops) => {
                let tcs: Vec<(Type, Constraints)> =
                    ops.iter().map(|e| self.type_inf(tenv, e)).collect();
                let type_f = tenv.get(f).expect("function not found");
                let mut cf = Constraints::new();
                let mut types: Vec<Type> = Vec::new();
                for (t, c) in tcs {
                    types.push(t);
                    merge(&mut cf, c);
                }
                let alpha = self.get_type_variable();
                cf.push_back((Type::Func(types, Box::new(alpha.clone())), type_f.clone()));
                (alpha, cf)
            }
            Let(ref decl, ref e) => {
                let (delta, c1) = self.type_inf_decl(tenv, decl);
                let mut new_tenv = tenv.clone();
                new_tenv.insert(delta.0, delta.1);
                let (t, c2) = self.type_inf(&new_tenv, e);
                let mut c = Constraints::new();
                merge(&mut c, c1);
                merge(&mut c, c2);
                (t, c)
            }
        }
    }

    fn type_inf_decl(&mut self, tenv: &TypeEnv, decl: &Decl) -> ((String, Type), Constraints) {
        match *decl {
            Decl::Var(ref x, ref e) => {
                let (t, c) = self.type_inf(tenv, e);
                ((x.to_string(), t), c)
            }
            Decl::Func(ref f, ref ops, ref e) => {
                let mut new_tenv = tenv.clone();
                let mut ops_types: Vec<Type> = Vec::new();
                for x in ops {
                    let alpha = self.get_type_variable();
                    ops_types.push(alpha.clone());
                    new_tenv.insert(x.to_string(), alpha);
                }
                let alpha = self.get_type_variable();
                let type_f = Type::Func(ops_types, Box::new(alpha.clone()));
                new_tenv.insert(f.to_string(), type_f.clone());

                let (t, mut c) = self.type_inf(&new_tenv, e);

                c.push_back((t, alpha));
                ((f.to_string(), type_f), c)
            }
        }
    }
}

type VarType = (String, Type);
type Unifier = Vec<VarType>;

impl Type {
    fn unify(&self, u: &VarType) -> Self {
        use Type::*;
        match *self {
            Var(ref s) => if *s == u.0 {
                u.1.clone()
            } else {
                self.clone()
            },
            Base(_) => self.clone(),
            Func(ref ops, ref v) => {
                let unified_ops: Vec<Type> = ops.iter().map(|op| op.unify(u)).collect();
                let unified_v = v.unify(u);
                Func(unified_ops, Box::new(unified_v))
            }
        }
    }
    fn appear(&self, x: &String) -> bool {
        use Type::*;
        match *self {
            Var(ref s) => s == x,
            Base(_) => false,
            Func(ref ops, ref v) => {
                for t in ops {
                    if t.appear(x) {
                        return true;
                    }
                }
                v.appear(x)
            }
        }
    }
}

fn unify(constraints: &mut Constraints, u: &VarType) -> Constraints {
    let mut unified_constraints = Constraints::new();
    while !constraints.is_empty() {
        let constraint = constraints.pop_front().unwrap();
        unified_constraints.push_back((constraint.0.unify(&u), constraint.1.unify(&u)));
    }
    return unified_constraints;
}

fn calculate_mgu(constraints: &Constraints) -> Option<Unifier> {
    use Type::*;
    let mut constraints = constraints.clone();
    let mut unifier = Unifier::new();
    while !constraints.is_empty() {
        let constraint = constraints.pop_front().unwrap();
        match constraint {
            (Var(x), t) | (t, Var(x)) => {
                if t.appear(&x) {
                    return None;
                } else {
                    let u = (x, t);
                    constraints = unify(&mut constraints, &u);
                    unifier.push(u);
                }
            }
            (Func(ops1, v1), Func(ops2, v2)) => {
                if ops1.len() == ops2.len() {
                    for e in ops1.iter().map(|t| t.clone()).zip(ops2) {
                        constraints.push_back(e);
                    }
                    constraints.push_back((*v1, *v2));
                } else {
                    return None;
                }
            }
            _ => {
                if constraint.0 != constraint.1 {
                    return None;
                }
            }
        }
    }
    Some(unifier)
}

fn main() {
    // let x := 15 in let f(y,z) := y+z in if 0 < x then x else f(10,x)
    let expr_x = Expr::Var("x".to_string());
    let expr_y = Expr::Var("y".to_string());
    let expr_z = Expr::Var("z".to_string());
    let expr_15 = Expr::Constant(Const::Int(15));
    let expr_10 = Expr::Constant(Const::Int(10));
    let expr_0 = Expr::Constant(Const::Int(0));

    let expr_f = Expr::Func("f".to_string(), vec![expr_10.clone(), expr_x.clone()]);

    let expr_cmp = Expr::BinOp(Op::Lt, Box::new(expr_0.clone()), Box::new(expr_x.clone()));
    let expr_plus = Expr::BinOp(Op::Plus, Box::new(expr_y.clone()), Box::new(expr_z.clone()));
    let expr_then = expr_x.clone();
    let expr_else = expr_f;
    let expr_if = Expr::If(Box::new(expr_cmp), Box::new(expr_then), Box::new(expr_else));
    let decl_f = Decl::Func(
        "f".to_string(),
        vec!["y".to_string(), "z".to_string()],
        Box::new(expr_plus),
    );
    let expr_let1 = Expr::Let(decl_f, Box::new(expr_if));
    let decl_x = Decl::Var("x".to_string(), Box::new(expr_15));
    let expr = Expr::Let(decl_x, Box::new(expr_let1));
    println!("{}", expr.to_string());
    let mut tf = TypeInf::new();
    let tenv = TypeEnv::new();
    let (t, constraints) = tf.type_inf(&tenv, &expr);
    let unifier = calculate_mgu(&constraints).expect("error");
    let mut well_typed = t;
    for u in unifier {
        well_typed = well_typed.unify(&u);
    }
    println!("{}", well_typed.to_string());
}
