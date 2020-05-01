use crate::AssociativeOp::{Addition, Multiplication};
use crate::Expr::{AssociativeExpr, Constant, Power, UnaryExpr, Variable};
use crate::UnaryOp::Minus;
use regex::Regex;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::str::FromStr;

macro_rules! debug {
    ($e:expr) => {
        println!("{} = {}", stringify!($e), $e);
    };
}

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
    Power {
        base: Box<Expr>,
        exp: Box<Expr>,
    },
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        let simplified = *self.simplify();
        let other_simplified = *other.simplify();
        match simplified {
            Variable(name) => match other_simplified {
                Variable(other_name) => name == other_name,
                _ => false,
            },
            Constant(c) => match other_simplified {
                Constant(other_c) => c == other_c,
                _ => false,
            },
            UnaryExpr { op, a } => match other_simplified {
                UnaryExpr {
                    op: other_op,
                    a: other_a,
                } => op == other_op && a.simplify() == other_a.simplify(),
                _ => false,
            },
            AssociativeExpr { op, args } => match other_simplified {
                AssociativeExpr {
                    op: other_op,
                    args: other_args,
                } => op == other_op && sort(&args) == sort(&other_args),
                _ => false,
            },
            Power { base, exp } => match other {
                Power {
                    base: other_base,
                    exp: other_exp,
                } => base == *other_base && exp == *other_exp,
                _ => false,
            },
        }
    }
}

fn sort(v: &Vec<Box<Expr>>) -> Vec<Box<Expr>> {
    let mut sorted = Vec::with_capacity(v.len());
    for expr in v {
        sorted.push(expr.copy());
    }
    sorted.sort();
    sorted
}

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match self {
            Variable(name) => match other {
                Variable(other_name) => name.cmp(other_name),
                _ => Ordering::Less,
            },
            Constant(c) => match other {
                Variable(_) => Ordering::Greater,
                Constant(other_c) => {
                    if c < other_c {
                        Ordering::Less
                    } else if c == other_c {
                        Ordering::Equal
                    } else {
                        Ordering::Greater
                    }
                }
                _ => Ordering::Less,
            },
            UnaryExpr { op, a } => match other {
                Variable(_) | Constant(_) => Ordering::Greater,
                UnaryExpr {
                    op: other_op,
                    a: other_a,
                } => match op.cmp(&other_op) {
                    Ordering::Equal => a.cmp(&other_a),
                    _ => op.cmp(&other_op),
                },
                _ => Ordering::Less,
            },
            AssociativeExpr { op, args } => match other {
                Variable(_) | Constant(_) | UnaryExpr { .. } => Ordering::Greater,
                AssociativeExpr {
                    op: other_op,
                    args: other_args,
                } => match op.cmp(&other_op) {
                    Ordering::Equal => sort(args).cmp(&sort(other_args)),
                    _ => op.cmp(&other_op),
                },
                _ => Ordering::Less,
            },
            Power { base, exp } => match other {
                Variable(_) | Constant(_) | UnaryExpr { .. } | AssociativeExpr { .. } => {
                    Ordering::Greater
                }
                Power {
                    base: other_base,
                    exp: other_exp,
                } => {
                    let base_cmp = base.cmp(&other_base);
                    if base_cmp != Ordering::Equal {
                        return Some(base_cmp);
                    }
                    exp.cmp(&other_exp)
                }
            },
        })
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Eq for Expr {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
enum UnaryOp {
    Minus,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
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
                            Variable { .. } | Constant { .. } | UnaryExpr { .. } | Power { .. } => {
                                e.to_string()
                            }
                            AssociativeExpr { .. } => format!("({})", e),
                        })
                        .collect::<Vec<String>>();
                    write!(f, "{}", &arg_strings[..].join("·"))
                }
            },
            Power { base, exp } => {
                match **base {
                    Variable(_) | Constant(_) => write!(f, "{}^", base)?,
                    UnaryExpr { .. } | AssociativeExpr { .. } | Power { .. } => {
                        write!(f, "({})^", base)?
                    }
                }
                match **exp {
                    Variable(_) | Constant(_) => write!(f, "{}", exp),
                    UnaryExpr { .. } | AssociativeExpr { .. } | Power { .. } => {
                        write!(f, "({})", exp)
                    }
                }
            }
        }
    }
}

fn variable(name: &str) -> Box<Expr> {
    Box::new(Variable(String::from(name)))
}

fn constant(c: f64) -> Box<Expr> {
    if !c.is_nan() {
        Box::new(Constant(c))
    } else {
        panic!("encountered a NaN (not a number)")
    }
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
        args: vec![a.copy(), b.copy()],
    })
}

fn sum_skeleton(size: usize) -> Box<Expr> {
    Box::new(AssociativeExpr {
        op: Addition,
        args: Vec::with_capacity(size),
    })
}

fn power(base: Box<Expr>, exp: Box<Expr>) -> Box<Expr> {
    Box::new(Power {
        base: base.copy(),
        exp: exp.copy(),
    })
}

impl Expr {
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
            Power { base, exp } => Power {
                base: base.copy(),
                exp: exp.copy(),
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
                UnaryExpr { a, op } => match op {
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
                Power { base, exp } => match **exp {
                    Constant(n) => {
                        if n == 1.0 {
                            constant(1.0)
                        } else {
                            product(constant(n), power(base.copy(), constant(n - 1.0)))
                        }
                    }
                    _ => panic!("derivative cannot handle variable exponents yet"),
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
            _ => self.copy()
        }
    }

    // fn expand(&self) -> Box<Expr> {
    //     let mut modified = false;
    //     match *self {
    //
    //         AssociativeExpr { op, args} => {
    //             match op {
    //                 Multiplication => {
    //                     mi
    //                     *self
    //                 }
    //                 _ => *self
    //             }
    //         }
    //         _ => self
    //     }
    // }

    // fn group(&self) -> Box<Expr> {
    //     // group equal expressions
    //     // for example, turn x+x into 2*x, or x*x into x^2
    //     match self {
    //         AssociativeExpr { op, args } => {
    //             let args = sort(&args);
    //             let mut grouped = Vec::with_capacity(args.len());
    //             let mut i = 0;
    //             while i < args.len() {
    //                 let mut k = i;
    //                 let current = &args[i];
    //                 loop {
    //                     k += 1;
    //                     if k >= args.len() - 1 || *current != args[k] {
    //                         break;
    //                     }
    //                 }
    //                 if k < args.len() && *current == args[k] {
    //                     k += 1
    //                 }
    //                 if k - i == 1 {
    //                     grouped.push(args[i].copy());
    //                 } else {
    //                     let repeated = args[i].copy();
    //                     let times = constant((k - i) as f64);
    //                     grouped.push(match op {
    //                         Addition => product(times, repeated),
    //                         Multiplication => power(repeated, times),
    //                     });
    //                 }
    //                 i = k;
    //             }
    //             match grouped.len() {
    //                 1 => grouped[0].copy(),
    //                 _ => associative_expr(*op, grouped),
    //             }
    //         }
    //         _ => self.copy(),
    //     }
    // }

    fn simplify(&self) -> Box<Expr> {
        let simplified_leaves = self.simplify_leaves();
        let simplified = match *simplified_leaves {
            AssociativeExpr { op, args } => {
                if op == Multiplication && args.contains(&constant(0.0)) {
                    constant(0.0)
                } else {
                    let mut count = args.len();
                    for arg in &args {
                        match **arg {
                            AssociativeExpr {
                                op: ref child_op,
                                args: ref child_args,
                            } => {
                                if child_op == &op {
                                    count -= 1;
                                    for child_arg in child_args {
                                        count += match **child_arg {
                                            AssociativeExpr { .. } => 1,
                                            UnaryExpr { .. } => 1,
                                            Power { .. } => 1,
                                            Variable(_) => 1,
                                            Constant(_) => 0,
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    let mut non_constants = Vec::with_capacity(count);
                    let unit = match op {
                        Addition => 0.0,
                        Multiplication => 1.0,
                    };
                    let mut accumulated_constant = unit;
                    for arg in args {
                        match *arg.copy() {
                            AssociativeExpr {
                                op: child_op,
                                args: child_args,
                            } => {
                                if child_op == op {
                                    for child_arg in child_args {
                                        match *child_arg {
                                            AssociativeExpr { .. }
                                            | UnaryExpr { .. }
                                            | Power { .. }
                                            | Variable(_) => non_constants.push(child_arg),
                                            Constant(c) => match op {
                                                Addition => accumulated_constant += c,
                                                Multiplication => accumulated_constant *= c,
                                            },
                                        }
                                    }
                                } else {
                                    non_constants.push(arg.copy());
                                }
                            }
                            Constant(c) => match op {
                                Addition => accumulated_constant += c,
                                Multiplication => accumulated_constant *= c,
                            },
                            _ => non_constants.push(arg.copy()),
                        }
                    }
                    // could make the code above more efficient by using sort sooner

                    match non_constants.len() {
                        0 => constant(accumulated_constant),
                        1 => {
                            if accumulated_constant == unit {
                                non_constants[0].copy()
                            } else {
                                match op {
                                    Addition => {
                                        non_constants.push(constant(accumulated_constant));
                                    }
                                    Multiplication => {
                                        non_constants.insert(0, constant(accumulated_constant));
                                    }
                                }
                                associative_expr(op, non_constants)
                            }
                        }
                        _ => associative_expr(op, non_constants),
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

    let expr = parse("x*x*x");
    debug!(expr);
    debug!(expr.derivative(&x));
    debug!(expr.derivative(&x).simplify());

    let expr = parse("x*x*x");
    println!();
    debug!(expr);
    debug!(expr.simplify());

    let expr = parse("1+3*5*x");
    println!();
    debug!(expr);
    debug!(expr.simplify());
    debug!(expr.derivative(&x).simplify());

    let n = 3;
    let mut expr = product_skeleton(n);
    let factors = expr.get_args().unwrap();
    for _ in 0..n {
        factors.push(variable("x"));
    }
    expr = product(x.copy(), expr.copy());
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(&x));
    println!("{}", expr.derivative(&x).simplify());

    let expr = product(product(product(x.copy(), x.copy()), x.copy()), x.copy());
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(&x));
    println!("{}", expr.derivative(&x).simplify());

    println!();
    println!("{}", expr.simplify());
    println!("{}", expr.simplify().derivative(&x));
    println!("{}", expr.simplify().derivative(&x).simplify());

    let expr = power(x.copy(), constant(5.0));
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(&x));
    println!("{}", expr.derivative(&x).simplify());
}
