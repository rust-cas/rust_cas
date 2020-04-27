use crate::AssociativeOp::{Addition, Multiplication};
use crate::Expr::{AssociativeExpr, Constant, UnaryExpr, Variable};
use crate::UnaryOp::Minus;
use regex::Regex;
use std::fmt::Formatter;
use std::str::FromStr;

#[macro_use]
extern crate lazy_static;

#[derive(Debug)]
enum Expr {
    Variable(String),
    Constant(f64),
    UnaryExpr {
        op: UnaryOp,
        a: Box<Expr>,
    },
    AssociativeExpr {
        op: AssociativeOp,
        args: Vec<Box<Expr>>,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum UnaryOp {
    Minus,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum AssociativeOp {
    Addition,
    Multiplication,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Minus => '-',
            }
        )
    }
}

impl std::fmt::Display for AssociativeOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Addition => '+',
                Multiplication => '*',
            }
        )
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable(name) => write!(f, "{}", name),
            Constant(c) => write!(f, "{}", c),
            UnaryExpr { op, a } => write!(f, "{} ({})", op, a),
            AssociativeExpr { op, args } => match op {
                Addition => {
                    let arg_strings = args.iter().map(|e| e.to_string()).collect::<Vec<String>>();
                    write!(f, "{}", &arg_strings[..].join(" + "))
                }
                Multiplication => {
                    let arg_strings = args
                        .iter()
                        .map(|e| match **e {
                            Variable { .. } | Constant { .. } | UnaryExpr { .. } => e.to_string(),
                            AssociativeExpr { .. } => format!("({})", e),
                        })
                        .collect::<Vec<String>>();
                    write!(f, "{}", &arg_strings[..].join(" * "))
                }
            },
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        return match self {
            Variable(name) => match other {
                Variable(other_name) => name == other_name,
                _ => false,
            },
            Constant(c) => match other {
                Constant(other_c) => c == other_c,
                _ => false,
            },
            UnaryExpr { a, op } => match other {
                UnaryExpr {
                    a: other_a,
                    op: other_op,
                } => *op == *other_op && *a == *other_a,
                _ => false,
            },
            AssociativeExpr { op, args } => match other {
                AssociativeExpr {
                    op: other_op,
                    args: other_args,
                } => *op == *other_op && args == other_args,
                _ => false,
            },
        };
    }
}

fn variable(name: &str) -> Box<Expr> {
    Box::new(Variable(String::from(name)))
}

fn constant(c: f64) -> Box<Expr> {
    Box::new(Constant(c))
}

fn associative_expr(op: AssociativeOp, args: Vec<Box<Expr>>) -> Box<Expr> {
    Box::new(AssociativeExpr { op, args })
}

fn unary_expr(op: UnaryOp, a: Box<Expr>) -> Box<Expr> {
    Box::new(UnaryExpr { a, op })
}

// todo check if it is possible to create a variadic version of this function using macro's
#[allow(dead_code)]
fn product(a: Box<Expr>, b: Box<Expr>) -> Box<Expr> {
    Box::new(AssociativeExpr {
        op: Multiplication,
        args: vec![a, b],
    })
}

fn product_skeleton(size: usize) -> Box<Expr> {
    Box::new(AssociativeExpr {
        op: Multiplication,
        args: Vec::with_capacity(size),
    })
}

#[allow(dead_code)]
fn sum(a: Box<Expr>, b: Box<Expr>) -> Box<Expr> {
    Box::new(AssociativeExpr {
        op: Addition,
        args: vec![a, b],
    })
}

fn sum_skeleton(size: usize) -> Box<Expr> {
    Box::new(AssociativeExpr {
        op: Addition,
        args: Vec::with_capacity(size),
    })
}

impl Eq for Expr {}

impl Expr {
    // todo refactor expr as a trait
    fn get_args(&mut self) -> Option<&mut Vec<Box<Expr>>> {
        match self {
            AssociativeExpr { op: _, args } => Some(args),
            _ => None,
        }
    }

    fn copy(&self) -> Box<Expr> {
        Box::new(match self {
            Variable(name) => Variable(name.clone()),
            Constant(c) => Constant(*c),
            UnaryExpr { a, op } => UnaryExpr {
                a: a.copy(),
                op: op.clone(),
            },
            AssociativeExpr { op, args } => AssociativeExpr {
                op: op.clone(),
                args: args.iter().map(|arg| arg.copy()).collect(),
            },
        })
    }

    fn derivative(&self, x: &Box<Expr>) -> Box<Expr> {
        if let Variable(x_str) = &**x {
            match self {
                Variable(self_str) => {
                    if self_str == x_str {
                        constant(1.0)
                    } else {
                        constant(0.0)
                    }
                }
                Constant(_) => constant(0.0),
                UnaryExpr { a, op } => match *op {
                    UnaryOp::Minus => Box::new(Expr::UnaryExpr {
                        op: op.clone(),
                        a: a.derivative(&x),
                    }),
                },
                AssociativeExpr { op, args } => match op {
                    Addition => {
                        associative_expr(Addition, args.iter().map(|e| e.derivative(&x)).collect())
                    }
                    Multiplication => {
                        // let a = &args[0];
                        // let b = &args[1];
                        // sum(
                        //     product_of_2(a.copy(), b.derivative(&x)),
                        //     product_of_2(a.derivative(&x), b.copy()),
                        // );
                        let mut derivative = sum_skeleton(args.len());
                        let terms = derivative.get_args().unwrap();
                        for i in 0..args.len() {
                            terms.push(product_skeleton(args.len()));
                            let factors = terms[i].get_args().unwrap();
                            for j in 0..args.len() {
                                factors.push(match j == i {
                                    true => args[j].derivative(&x),
                                    false => args[j].copy(),
                                });
                            }
                        }
                        derivative
                    }
                },
            }
        } else {
            panic!("x is not a variable")
        }
    }

    fn simplify_leaves(&self) -> Box<Expr> {
        match self {
            AssociativeExpr { op, args } => {
                associative_expr(op.clone(), args.iter().map(|e| e.simplify()).collect())
            }
            UnaryExpr { op, a } => unary_expr(op.clone(), a.simplify()),
            _ => self.copy(),
        }
    }

    fn simplify(&self) -> Box<Expr> {
        let simplified_leaves = self.simplify_leaves();
        let simplified = match *simplified_leaves {
            AssociativeExpr { op, args } => {
                if op == Multiplication && args.contains(&constant(0.0)) {
                    constant(0.0)
                } else {
                    let mut units_removed = Vec::with_capacity(args.len());
                    for arg in args {
                        match *arg {
                            Constant(c) => {
                                if c != match op {
                                    Addition => 0.0,
                                    Multiplication => 1.0
                                } {
                                    units_removed.push(arg)
                                }
                            }
                            _ => {units_removed.push(arg)}
                        }
                    }
                    match units_removed.len() {
                        0 => match op {
                            Addition => constant(0.0),
                            Multiplication => constant(1.0)
                        }
                        1 => units_removed[0].copy(),
                        _ => associative_expr(op, units_removed)
                    }

                }
            }
            _ => self.copy(),
        };
        simplified
    }
}

fn parse(expr: &str) -> Box<Expr> {
    lazy_static! {
        static ref VARIABLE: Regex = Regex::new("[a-zA-Z_]+").unwrap();
        static ref NUMBER: Regex = Regex::new(r"\d+").unwrap();
    }
    return Box::new(if let Some(i) = expr.find('+') {
        let str_before = expr[..i].trim();
        let str_after = expr[i + 1..].trim();
        if str_before.len() == 0 || str_after.len() == 0 {
            panic!("invalid expression: {}", expr)
        } else {
            AssociativeExpr {
                op: Addition,
                args: vec![parse(str_before), parse(str_after)],
            }
        }
    } else if let Some(i) = expr.find('-') {
        let str_before = expr[..i].trim();
        let str_after = expr[i + 1..].trim();
        if str_before.len() == 0 || str_after.len() == 0 {
            panic!("invalid expression: {}", expr)
        } else {
            AssociativeExpr {
                op: Addition,
                args: vec![parse(str_before), unary_expr(Minus, parse(str_after))],
            }
        }
    } else if let Some(i) = expr.find("*") {
        let str_before = expr[..i].trim();
        let str_after = expr[i + 1..].trim();
        if str_before.len() == 0 || str_after.len() == 0 {
            panic!("invalid expression: {}", expr)
        } else {
            AssociativeExpr {
                op: Multiplication,
                args: vec![parse(str_before), parse(str_after)],
            }
        }
    } else if let Some(match_) = VARIABLE.find(expr) {
        Variable(String::from(match_.as_str()))
    } else if let Some(match_) = NUMBER.find(expr) {
        Constant(f64::from_str(match_.as_str()).unwrap())
    } else {
        panic!(
            "invalid expression: {}, no operators, variables, or numbers found",
            expr
        )
    });
}

fn main() {
    let x = variable("x");

    println!();
    println!("{}", parse("a*x+y*y+555"));
    println!("{}", parse("a*x+y*y+555").derivative(&x));
    println!("{}", parse("a*x+y*y+555").derivative(&x).simplify());
    println!("{:?}", parse("a*x+y*y+555").derivative(&x).simplify());
    // println!();
    // println!("{}", parse("a*x+y*y+555"));
    // println!("{}", parse("a*x+y*y+555").derivative(&y));
    // println!("{}", parse("a*x+y*y+555").derivative(&y).simplify());
    // println!();
    // println!("{}", parse("x*x*x"));
    // println!("{}", parse("x*x*x").derivative(&x));
    // println!("{}", parse("x*x*x").derivative(&x).simplify());

    let expr = parse("x*x*x");
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(&x));
    println!("{}", expr.derivative(&x).simplify());

    let n = 3;
    let mut expr = product_skeleton(n);
    let factors = expr.get_args().unwrap();
    for _ in 0..n {
        factors.push(variable("x"));
    }
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(&x));
    println!("{}", expr.derivative(&x).simplify());
}
