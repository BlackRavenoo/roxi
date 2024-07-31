use crate::expr::{Expr, Literal, Visitor};


pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(expr: &Expr) {
        println!("{}", AstPrinter::visit(expr))
    }
}

fn parenthesize(str: &str, exprs: &[&Expr]) -> String {
    let mut string = format!("({}", str);

    for expr in exprs {
        string.push(' ');
        string.push_str(&AstPrinter::visit(expr));
    }

    string.push(')');
    string
}

impl Visitor<String> for AstPrinter {
    fn visit(expr: &Expr) -> String {
        match expr {
            Expr::Binary { left, operator, right } => parenthesize(operator.kind.as_str(), &[left, right]),
            Expr::Grouping { expression } => parenthesize("group", &[expression]),
            Expr::Literal(value) => match value {
                Literal::Number(num) => {
                    if num.fract() == 0.0 {
                        format!("{}.0", num.floor())
                    } else {
                        num.to_string()
                    }
                },
                Literal::String(str) => str.to_string(),
                Literal::Bool(bool) => bool.to_string(),
                Literal::Nil => "nil".to_string(),
            },
            Expr::Unary { operator, right } => parenthesize(operator.kind.as_str(), &[right]),
            Expr::Ternary { condition, then_branch, else_branch } => parenthesize("ternary", &[condition, then_branch, else_branch]),
            Expr::Variable { name } => todo!(),
        }
    }
}