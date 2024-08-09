use crate::{expr::{Expr, Literal, Visitor}, stmt::Stmt};


pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => println!("{}", AstPrinter::visit(expr)),
            Stmt::Print(expr) => println!("print ({})", AstPrinter::visit(expr)),
            Stmt::Return { value, .. } => println!("return {}", AstPrinter::visit(value)),
            Stmt::Var { name, .. } => println!("var {} = ", name),
            Stmt::Block { statements } => {println!("block "); let _ = statements.iter().for_each(|stmt| AstPrinter::print(stmt));},
            Stmt::Function { name, body, .. } => {println!("func {} ", name); let _ = body.iter().for_each(|stmt| AstPrinter::print(stmt));},
            Stmt::If { condition, then_branch, else_branch } => println!("if {}", AstPrinter::visit(condition)),
            Stmt::While { body, .. } => {println!("while "); AstPrinter::print(&body);},
            Stmt::Break { line } => todo!(),
        }
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
            Expr::Binary { values, operator } => parenthesize(operator.kind.as_str(), &[&values.0, &values.1]),
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
            Expr::Ternary { exprs } => parenthesize("ternary", &[&exprs.0, &exprs.1, &exprs.2]),
            Expr::Variable { name, .. } => format!("var {}", name),
            Expr::Assign { .. } => "".to_owned(),
            Expr::Logical { .. } => todo!(),
            Expr::Call { exprs, .. } => parenthesize("call ", &exprs.iter().collect::<Vec<_>>()),
        }
    }
}