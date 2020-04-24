use regex::Regex;
use std::fmt::Formatter;
use std::str::FromStr;
use crate::Expr::BinaryExpr;

#[macro_use]
extern crate lazy_static;

#[derive(Debug)]
enum Expr {
    Variable(String),
    Constant(f64),
    BinaryExpr{a: Box<Expr>, op: BinaryOp, b: Box<Expr>},
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BinaryOp {
    Add, Sub, Mul
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => '+',
            BinaryOp::Sub => '-',
            BinaryOp::Mul => '*',
        })
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Variable(v) => write!(f, "{}", v),
            Expr::Constant(c) => write!(f, "{}", c),
            Expr::BinaryExpr{a,b, op} => {
                match op {
                    BinaryOp::Add | BinaryOp::Sub => {
                        write!(f, "{} {} {}", a, op, b)
                    }
                    BinaryOp::Mul => match **a {
                        Expr::BinaryExpr { .. } => match **b {
                            Expr::BinaryExpr { .. } => write!(f, "({}) {} ({})", a, op, b),
                            _ => write!(f, "({}) {} {}", a, op, b)
                        },
                        _ => match **b {
                            Expr::BinaryExpr { .. } => write!(f, "{} {} ({})", a, op, b),
                            _ => write!(f, "{} {} {}", a, op, b)
                        },
                    }
                }
            }
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        return match self {
            Expr::Variable(name) => {
                match other {
                    Expr::Variable(other_name) =>  { name == other_name }
                    _ => false
                }
            }
            Expr::Constant(constant) => {
                match other {
                    Expr::Constant(other_constant) => { constant == other_constant }
                    _ => false
                }
            }
            Expr::BinaryExpr{a,b,op} => {
                match other {
                    Expr::BinaryExpr{
                        a: other_a,
                        b: other_b, op:other_op } => {
                        *op == *other_op && a == other_a && b == other_b
                    }
                    _ => false
                }
            }
        }
    }
}

fn constant(c:f64) -> Box<Expr> {
    Box::new(Expr::Constant(c))
}

fn binary_expr(a: Box<Expr>, b: Box<Expr>, op:BinaryOp) -> Box<Expr> {
    Box::new(BinaryExpr {a,b,op})
}

fn product(a: Box<Expr>, b: Box<Expr>) -> Box<Expr> {
    Box::new(BinaryExpr {a,b,op:BinaryOp::Mul})
}

fn sum(a: Box<Expr>, b: Box<Expr>) -> Box<Expr> {
    Box::new(BinaryExpr {a,b,op:BinaryOp::Add})
}

impl Eq for Expr {}

impl Expr {
    fn copy(&self) -> Box<Expr> {
        Box::new( match self {
            Expr::Variable(string) => Expr::Variable(string.clone()),
            Expr::Constant(c) => Expr::Constant(*c),
            Expr::BinaryExpr{a, b, op} => {
                Expr::BinaryExpr {a: a.copy(), b: b.copy(), op: op.clone()}
            }
        })
    }

    fn derivative(&self, x: &Box<Expr>) -> Box<Expr> {
        if let Expr::Variable(x_str) = &**x {
            match self {
                Expr::Variable(self_str) => {
                    if self_str == x_str {
                        constant(1.0)
                    } else {
                        constant(0.0)
                    }
                }
                Expr::Constant(_) => {
                    constant(0.0)
                },
                Expr::BinaryExpr { a, b, op } => {
                    match op {
                        BinaryOp::Add | BinaryOp::Sub => {
                            binary_expr(a.derivative(&x),b.derivative(&x),op.clone())
                        }
                        BinaryOp::Mul => { sum(
                                product(a.copy(), b.derivative(&x)),
                                product(a.derivative(&x), b.copy()))
                        }
                    }
                }
            }
        } else {
            panic!("x is not a variable")
        }
    }

    fn simplify_leaves(&self) -> Box<Expr> {
        match self {
            Expr::BinaryExpr{a,b,op} => {
                binary_expr(a.simplify(), b.simplify(), *op)
            }
            _ => self.copy()
        }
    }

    fn simplify(&self) -> Box<Expr> {
        let simplified_leaves = self.simplify_leaves();
        match *simplified_leaves {
            Expr::BinaryExpr {a,b,op} => {
                if let Expr::Constant(a) = *a {
                    if let Expr::Constant(b) = *b {
                        match op {
                            BinaryOp::Add => constant(a + b),
                            BinaryOp::Sub => constant(a - b),
                            BinaryOp::Mul => constant(a * b)
                        }
                    } else if a == 0.0 {
                        match op {
                            BinaryOp::Add => b.simplify(),
                            BinaryOp::Sub => self.simplify_leaves(),
                            BinaryOp::Mul => constant(0.0)
                        }
                    } else if a == 1.0 {
                        match op {
                            BinaryOp::Mul => b.simplify(),
                            _ => self.simplify_leaves()
                        }
                    } else {
                        self.simplify_leaves()
                    }
                } else if let Expr::Constant(b) = *b {
                    if b == 0.0 {
                        match op {
                            BinaryOp::Add | BinaryOp::Sub => a.copy(),
                            BinaryOp::Mul => constant(0.0)
                        }
                    } else if b == 1.0 {
                        match op {
                            BinaryOp::Mul => a.copy(),
                            _ => self.simplify_leaves()
                        }
                    } else {
                        self.simplify_leaves()
                    }
                } else if op == BinaryOp::Add && a == b {
                    product(constant(2.0), b)
                } else {
                    self.simplify_leaves()
                }
            }
            _ => self.simplify_leaves()
        }
    }
}

fn parse(expr: &str) -> Box<Expr> {
    lazy_static! {
        static ref PLUS_OR_MINUS: Regex = Regex::new(r"\+|-").unwrap();
        static ref VARIABLE: Regex = Regex::new("[a-zA-Z_]+").unwrap();
        static ref NUMBER: Regex = Regex::new(r"\d+").unwrap();
    }
    if let Some(match_) = PLUS_OR_MINUS.find(expr) {
        let i = match_.start();
        let str_before = expr[..i].trim();
        let str_after = expr[i + 1..].trim();
        if str_before.len() == 0 || str_after.len() == 0 {
            panic!("invalid expression: {}", expr)
        } else {
            let op;
            match &expr[i..=i] {
                "+" => op = BinaryOp::Add,
                "-" => op = BinaryOp::Sub,
                _ => panic!("internal error")
            };
            return Box::new(Expr::BinaryExpr {
                a: parse(str_before),
                b: parse(str_after), op })
        }
    } else if let Some(i) = expr.find("*") {
        let str_before = expr[..i].trim();
        let str_after = expr[i + 1..].trim();
        if str_before.len() == 0 || str_after.len() == 0 {
            panic!("invalid expression: {}", expr)
        } else {
            return Box::new(Expr::BinaryExpr {
                a: parse(str_before),
                b: parse(str_after), op: BinaryOp::Mul })
        }
    } else if let Some(match_) = VARIABLE.find(expr) {
        return Box::new(Expr::Variable(String::from(match_.as_str())));
    } else if let Some(match_) = NUMBER.find(expr) {
        return constant(f64::from_str(match_.as_str()).unwrap());
    }
    panic!("invalid expression: {}, no operators, variables, or numbers found", expr);
}

fn main() {
    let x = Box::new(Expr::Variable(String::from("x")));
    let y = Box::new(Expr::Variable(String::from("y")));
    let expr = Expr::BinaryExpr{a: x.copy(), b: y.copy(), op: BinaryOp::Add};
    println!("{}", expr);
    println!("{}", expr.derivative(&x));
    println!("{}", expr.derivative(&x).simplify());
    println!();
    println!("{}", parse("a*x+y*y+555"));
    println!("{}", parse("a*x+y*y+555").derivative(&x));
    println!("{}", parse("a*x+y*y+555").derivative(&x).simplify());
    println!();
    println!("{}", parse("a*x+y*y+555"));
    println!("{}", parse("a*x+y*y+555").derivative(&y));
    println!("{}", parse("a*x+y*y+555").derivative(&y).simplify());
    println!();
    println!("{}", parse("x*x*x"));
    println!("{}", parse("x*x*x").derivative(&x));
    println!("{}", parse("x*x*x").derivative(&x).simplify());
}