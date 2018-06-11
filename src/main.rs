mod ast;
mod typing;

use ast::Expr;
use ast::Decl;
use ast::Const;
use ast::Op;

use typing::TypeInf;
use typing::TypeEnv;

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
    let unifier = typing::calculate_mgu(&constraints).expect("error");
    let mut well_typed = t;
    for u in unifier {
        well_typed = well_typed.unify(&u);
    }
    println!("{}", well_typed.to_string());
}
