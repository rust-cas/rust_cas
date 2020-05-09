use itertools::Itertools;
use regex::Regex;

use std::cmp::Ordering;
use std::fmt::Formatter;
use std::fmt::Write;
use std::str::FromStr;

use crate::Expr::{Constant, Power, Product, Sum, UnaryExpr, Variable};
use crate::UnaryOp::{Exp, Minus};

macro_rules! display {
    ($e:expr) => {
        println!("{} = {}", stringify!($e), $e);
    };
}

macro_rules! debug {
    ($e:expr) => {
        println!("{} = {:?}", stringify!($e), $e);
    };
}

#[macro_use]
extern crate lazy_static;

enum Expr {
    Variable(String),
    Constant(f64),
    UnaryExpr { op: UnaryOp, a: Box<Expr> },
    Sum(Vec<Expr>),
    Product(Vec<Expr>),
    Power { base: Box<Expr>, exp: Box<Expr> },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
enum UnaryOp {
    Minus,
    Exp,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        // todo remove these simplifications of make the optional
        let simplified = self.clone().simplify();
        let other_simplified = other.clone().simplify();
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
                } => op == other_op && a == other_a,
                _ => false,
            },
            Product(args) => match other_simplified {
                Product(other_args) => sort(&args) == sort(&other_args),
                _ => false,
            },
            Sum(args) => match other_simplified {
                Sum(other_args) => sort(&args) == sort(&other_args),
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

fn sort(v: &Vec<Expr>) -> Vec<Expr> {
    let mut sorted = v.clone();
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
            Sum(args) => match other {
                Variable(_) | Constant(_) | UnaryExpr { .. } => Ordering::Greater,
                Sum(other_args) => sort(args).cmp(&sort(other_args)),
                _ => Ordering::Less,
            },
            Product(args) => match other {
                Variable(_) | Constant(_) | UnaryExpr { .. } | Sum(_) => Ordering::Greater,
                Product(other_args) => sort(args).cmp(&sort(other_args)),
                _ => Ordering::Less,
            },
            Power { base, exp } => match other {
                Variable(_) | Constant(_) | UnaryExpr { .. } | Sum(_) | Product(_) => {
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

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Minus => "-",
                Exp => "exp",
            }
        )
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable(name) => write!(f, "{}", name),
            Constant(c) => write!(f, "{}", c),
            UnaryExpr { op, a } => match op {
                Minus => write!(f, "- ({:?})", a),
                _ => write!(f, "{}({:?})", op, a),
            },
            Sum(args) => write!(f, "sum({:})", &debug_args(args)),
            Product(args) => write!(f, "product({:})", &debug_args(args)),
            Power { base, exp } => write!(f, "({:?})^({:?})", base, exp),
        }
    }
}

fn debug_args(args: &Vec<Expr>) -> String {
    let arg_strings = args
        .iter()
        .map(|e| {
            let mut s = String::new();
            write!(s, "{:?}", e).unwrap();
            s
        })
        .collect::<Vec<String>>();
    arg_strings[..].join(", ")
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable(name) => write!(f, "{}", name),
            Constant(c) => write!(f, "{}", c),
            UnaryExpr { op, a } => match op {
                Minus => write!(f, "- ({})", a),
                _ => write!(f, "{}({})", op, a),
            },
            Sum(args) => {
                let arg_strings = args.iter().map(|e| e.to_string()).collect::<Vec<String>>();
                write!(f, "{}", &arg_strings[..].join(" + "))
            }
            Product(args) => {
                let arg_strings = args
                    .iter()
                    .map(|e| match *e {
                        Variable { .. }
                        | Constant { .. }
                        | UnaryExpr { .. }
                        | Product(_)
                        | Power { .. } => e.to_string(),
                        Sum(_) => format!("({})", e),
                    })
                    .collect::<Vec<String>>();
                write!(f, "{}", &arg_strings[..].join("·"))
            }
            Power { base, exp } => {
                match **base {
                    Variable(_) | Constant(_) => write!(f, "{}^", base)?,
                    UnaryExpr { .. } | Sum(_) | Product(_) | Power { .. } => {
                        write!(f, "({})^", base)?
                    }
                }
                match **exp {
                    Variable(_) | Constant(_) => write!(f, "{}", exp),
                    UnaryExpr { .. } | Sum(_) | Product(_) | Power { .. } => write!(f, "({})", exp),
                }
            }
        }
    }
}

fn variable(name: &str) -> Expr {
    Variable(String::from(name))
}

fn constant(c: f64) -> Expr {
    if !c.is_nan() {
        Constant(c)
    } else {
        panic!("encountered a NaN (not a number)")
    }
}

fn unary_expr(op: UnaryOp, a: Expr) -> Expr {
    UnaryExpr { op, a: Box::new(a) }
}

fn product_skeleton(size: usize) -> Expr {
    Product(Vec::with_capacity(size))
}

fn sum_skeleton(size: usize) -> Expr {
    Sum(Vec::with_capacity(size))
}

fn power(base: Expr, exp: Expr) -> Expr {
    Power {
        base: Box::new(base.clone()),
        exp: Box::new(exp.clone()),
    }
}

fn factorial(n: u64) -> u64 {
    (1..=n).product()
}

fn multi_nom_coeff(v: &Vec<i32>) -> u64 {
    let mut result = factorial(v.iter().sum::<i32>() as u64);
    for e in v {
        result /= factorial(*e as u64);
    }
    result
}

fn expand_all(args: &Vec<Expr>) -> Vec<Expr> {
    args.iter().map(|e| e.clone().full_expand()).collect()
}

fn simplify_all(args: &Vec<Expr>) -> Vec<Expr> {
    args.iter().map(|e| e.clone().simplify()).collect()
}

impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Variable(ref name) => Variable(name.clone()),
            Constant(c) => Constant(*c),
            UnaryExpr { ref a, ref op } => UnaryExpr {
                a: (*a).clone(),
                op: *op,
            },
            Sum(args) => Sum(args.clone()),
            Product(args) => Product(args.clone()),
            Power { ref base, ref exp } => Power {
                base: (*base).clone(),
                exp: (*exp).clone(),
            },
        }
    }
}

fn new_length_product(args: &Vec<Expr>) -> usize {
    let mut new_length = args.len();
    for arg in args {
        match *arg {
            Product(ref child_args) => {
                new_length -= 1;
                for child_arg in child_args {
                    new_length += match *child_arg {
                        Constant(_) => 0,
                        _ => 1,
                    }
                }
            }
            _ => {}
        }
    }
    new_length
}

fn new_length_sum(args: &Vec<Expr>) -> usize {
    let mut new_length = args.len();
    for arg in args {
        match *arg {
            Sum(ref child_args) => {
                new_length -= 1;
                for child_arg in child_args {
                    new_length += match *child_arg {
                        Constant(_) => 0,
                        _ => 1,
                    }
                }
            }
            _ => {}
        }
    }
    new_length
}

impl Expr {
    const ZERO: Expr = Constant(0.0);
    const ONE: Expr = Constant(1.0);

    fn is_expandable(&self) -> bool {
        match *self {
            Product(ref args) => args.iter().any(|arg| arg.is_addition()),
            //Power { ref base, .. } => base.is_addition() || base.is_expandable(),
            _ => false,
        }
    }

    fn expand_leaves(self) -> Expr {
        match self {
            Sum(args) => Sum(expand_all(&args)),
            Product(args) => Product(expand_all(&args)),
            _ => self,
        }
    }

    fn full_expand(self) -> Expr {
        let expr = self.expand_leaves();
        expr.expand()
    }

    fn expand(self) -> Expr {
        if let Product(ref args) = self {
            if !self.is_expandable() {
                return self;
            }
            let unexpandable_factors: Vec<&Expr> =
                args.iter().filter(|arg| !arg.is_addition()).collect();
            let expanded_args = args
                .iter()
                .filter_map(|arg| {
                    if let Sum(ref child_args) = arg {
                        Some(child_args.clone())
                    } else {
                        None
                    }
                })
                .multi_cartesian_product();
            let expanded_args = expanded_args
                .map(|mut e: Vec<Expr>| {
                    e.reserve(unexpandable_factors.len());
                    for expr in &unexpandable_factors {
                        e.push((**expr).clone())
                    }
                    Product(e)
                })
                .collect();
            Sum(expanded_args)
        } else {
            self
        }
    }

    fn is_addition(&self) -> bool {
        if let Sum(_) = *self {
            true
        } else {
            false
        }
    }

    fn get_args(&mut self) -> Option<&mut Vec<Expr>> {
        match self {
            Sum(args) => Some(args),
            Product(args) => Some(args),
            _ => None,
        }
    }

    // fn clone(&self) -> Expr {
    //     match self {
    //         Variable(name) => Variable(name.clone()),
    //         Constant(c) => Constant(*c),
    //         UnaryExpr { a, op } => UnaryExpr {
    //             a: (*a).clone(),
    //             op: *op,
    //         },
    //         Product(args) => Product( args: args.clone() },
    //         Sum(args) => Sum( args: args.clone() },
    //         Power { base, exp } => Power {
    //             base: (*base).clone(),
    //             exp: (*exp).clone(),
    //         },
    //     }
    // }

    fn derivative(&self, x: &Expr) -> Expr {
        if let Variable(x_str) = &*x {
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
                    Minus => unary_expr(Minus, a.derivative(x)),
                    Exp => {
                        if **a == *x {
                            self.clone()
                        } else {
                            Product(vec![self.clone(), a.derivative(x)])
                        }
                    }
                },
                Sum(args) => {
                    // todo filter out constants
                    Sum(args.iter().map(|e| e.derivative(&x)).collect())
                }
                Product(args) => {
                    let mut derivative = sum_skeleton(args.len());
                    let terms = derivative.get_args().unwrap();
                    for i in 0..args.len() {
                        terms.push(product_skeleton(args.len()));
                        let factors = terms[i].get_args().unwrap();
                        for j in 0..args.len() {
                            factors.push(match j == i {
                                true => args[j].derivative(&x),
                                false => args[j].clone(),
                            });
                        }
                    }
                    derivative
                }
                Power { base, exp } => match **exp {
                    Constant(n) => {
                        if n == 0.0 {
                            Expr::ONE
                        } else {
                            let mut args =
                                vec![constant(n), power((**base).clone(), constant(n - 1.0))];
                            if **base != *x {
                                args.push((**base).derivative(&x));
                            }
                            Product(args)
                        }
                    }
                    _ => panic!("derivative can currently only handle constant exponents yet"),
                },
            }
        } else {
            panic!("x is not a variable")
        }
    }

    fn group(self) -> Expr {
        let grouped = self.clone().group_once();
        if grouped != self {
            grouped.group()
        } else {
            grouped
        }
    }

    fn group_once(self) -> Expr {
        // group equal expressions
        // for example, turn x+x into 2*x, or x*x into x^2
        match self {
            Sum(args) => {
                let args = sort(&args);
                let mut grouped = Vec::with_capacity(args.len());
                let mut i = 0;
                while i < args.len() {
                    let mut k = i;
                    let current = &args[i];
                    loop {
                        k += 1;
                        if k >= args.len() - 1 || *current != args[k] {
                            break;
                        }
                    }
                    if k < args.len() && *current == args[k] {
                        k += 1
                    }
                    if k - i == 1 {
                        // if the current argument is not repeated
                        grouped.push(args[i].clone());
                    } else {
                        let repeated = args[i].clone();
                        let times = Constant((k - i) as f64);
                        grouped.push(match repeated {
                            Product(ref repeated_args) => {
                                // if the repeated expression is a multiplication,
                                // we just insert times into repeated_args
                                let mut repeated_args = repeated_args.clone();
                                repeated_args.insert(0, times);
                                Product(repeated_args)
                            }
                            _ => Product(vec![times, repeated]),
                        });
                    }
                    i = k;
                }
                match grouped.len() {
                    1 => grouped[0].clone(),
                    _ => Sum(grouped),
                }
            }
            Product(args) => {
                let args = sort(&args);
                let mut grouped = Vec::with_capacity(args.len());
                let mut i = 0;
                while i < args.len() {
                    let mut k = i;
                    let current = &args[i];
                    loop {
                        k += 1;
                        if k >= args.len() - 1 || *current != args[k] {
                            break;
                        }
                    }
                    if k < args.len() && *current == args[k] {
                        k += 1
                    }
                    if k - i == 1 {
                        // if the current argument is not repeated
                        grouped.push(args[i].clone());
                    } else {
                        let repeated = args[i].clone();
                        let times = Constant((k - i) as f64);
                        grouped.push(power(repeated, times));
                    }
                    i = k;
                }
                match grouped.len() {
                    1 => grouped[0].clone(),
                    _ => Product(grouped),
                }
            }
            _ => self,
        }
    }

    fn simplify(self) -> Expr {
        let simplified = match self {
            Product(args) => {
                let args = simplify_all(&args);
                if args.contains(&Expr::ZERO) {
                    return Expr::ZERO;
                }
                let mut new_args = Vec::with_capacity(new_length_product(&args));
                let mut constant = 1.0;
                for arg in args {
                    match arg {
                        Product(child_args) => {
                            for child_arg in child_args {
                                match child_arg {
                                    Product(_)
                                    | Sum(_)
                                    | UnaryExpr { .. }
                                    | Power { .. }
                                    | Variable(_) => new_args.push(child_arg.clone()),
                                    Constant(c) => constant *= c,
                                }
                            }
                        }
                        _ => new_args.push(arg.clone()),
                    }
                }
                match new_args.len() {
                    0 => Constant(constant),
                    _ => {
                        if new_args.len() == 1 && constant == 1.0 {
                            new_args.pop().unwrap()
                        } else {
                            if constant != 1.0 {
                                new_args.insert(0, Constant(constant));
                            }
                            Product(new_args)
                        }
                    }
                }
            }
            Sum(args) => {
                let args = simplify_all(&args);
                let mut new_args = Vec::with_capacity(new_length_sum(&args));
                let mut constant = 0.0;
                for arg in args {
                    match arg {
                        Sum(child_args) => {
                            for child_arg in child_args {
                                match child_arg {
                                    Product(_)
                                    | Sum(_)
                                    | UnaryExpr { .. }
                                    | Power { .. }
                                    | Variable(_) => new_args.push(child_arg.clone()),
                                    Constant(c) => constant += c,
                                }
                            }
                        }
                        _ => new_args.push(arg.clone()),
                    }
                }
                match new_args.len() {
                    0 => Constant(constant),
                    _ => {
                        if new_args.len() == 1 && constant == 0.0 {
                            new_args.pop().unwrap()
                        } else {
                            if constant != 0.0 {
                                new_args.push(Constant(constant));
                            }
                            Sum(new_args)
                        }
                    }
                }
            }
            UnaryExpr { op, a } => unary_expr(op, a.simplify()),
            Power { base, exp } => power(base.simplify(), exp.simplify()),
            _ => self,
        };
        simplified
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Token {
    Constant(Number),
    Variable(String),
    Multiplication,
    Addition,
    LParen,
    RParen,
}

#[derive(Debug)]
struct Number(f64); // we define this so that we can derive PartialEq for Token
                    // in effect, we assert here that the contents of Number is not NaN.

impl std::cmp::PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl std::cmp::Eq for Number {}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Constant(Number(c)) => write!(f, "{}", c),
            Token::Variable(name) => write!(f, "{}", name),
            Token::Multiplication => write!(f, "*"),
            Token::Addition => write!(f, "+"),
            Token::RParen => write!(f, "("),
            Token::LParen => write!(f, ")"),
        }
    }
}

#[allow(dead_code)]
fn display_vec_token(vec_token: &Vec<Token>) {
    let arg_strings = vec_token
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>();
    println!("[{}]", &arg_strings[..].join(", "))
}

fn parse(expr: &str) -> Expr {
    let original_expr = expr;
    lazy_static! {
        static ref STARTS_WITH_NUMBER: Regex = get_starts_with_float_regex();
        static ref STARTS_WITH_OPERATOR: Regex = Regex::new(r"^[\+\*-/]").unwrap();
        static ref STARTS_WITH_VARIABLE: Regex = Regex::new("^[a-zA-Z_]+").unwrap();
    }
    let expr: String = expr.chars().filter(|c| !c.is_whitespace()).collect();
    let mut expr = &expr[..];
    // we parse the expression using a shunting yard algorithm. See
    // https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    let mut output = Vec::with_capacity(expr.len());
    // In a valid expression, the number of operators is less than half the number of characters
    // Proof:
    // suppose the expression contains only the +,-,*,/ operators. One of the operators needs
    // has two inputs that are either two variables, two constants, or one of each.
    // The other operators need at most one other variable or constant. Hence there should be
    // at least one more non-operator (i.e. a variable or a constant) than there are operators.
    // Hence if there are only +,-,*,/ operators, the number of operators is less than half the
    // number of characters.
    // Suppose there is another operator. Unary operators such as ln(), sin() or cos(), use at
    // least for characters. Hence the number ratio of operators to characters will remain less
    // than one half, even if operators other than +,-,*,/ are used.
    let mut operators = Vec::with_capacity(expr.len() / 2);
    while expr != "" {
        let token = if let Some(match_) = STARTS_WITH_NUMBER.find(expr) {
            expr = &expr[match_.end()..];
            Token::Constant(Number(f64::from_str(match_.as_str()).unwrap()))
        } else if let Some(match_) = STARTS_WITH_OPERATOR.find(expr) {
            expr = &expr[match_.end()..];
            match match_.as_str() {
                "*" => Token::Multiplication,
                "+" => Token::Addition,
                _ => panic!("unsupported token: {}", match_.as_str()),
            }
        } else if let Some(match_) = STARTS_WITH_VARIABLE.find(expr) {
            expr = &expr[match_.end()..];
            Token::Variable(match_.as_str().to_string())
        } else if expr.starts_with('(') {
            expr = &expr[1..];
            Token::LParen
        } else if expr.starts_with(')') {
            expr = &expr[1..];
            Token::RParen
        } else {
            panic!(
                "The expression is not valid. No operator, variable, or constant found at {}",
                expr
            );
        };
        match token {
            Token::Constant(_) | Token::Variable(_) => output.push(token),
            Token::Addition => {
                while let Some(token) = operators.last() {
                    if *token != Token::LParen {
                        output.push(operators.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operators.push(Token::Addition);
            }
            Token::Multiplication => {
                while let Some(token) = operators.last() {
                    if *token == Token::Multiplication {
                        output.push(operators.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operators.push(Token::Multiplication)
            }
            Token::LParen => operators.push(token),
            Token::RParen => {
                let mut found_matching_lparen = false;
                while let Some(token) = operators.last() {
                    if *token != Token::LParen {
                        output.push(operators.pop().unwrap())
                    } else {
                        operators.pop();
                        found_matching_lparen = true;
                        break;
                    }
                }
                if !found_matching_lparen {
                    panic!(
                        "The expression is not valid. \
                            The right parenthesis before {} in {} can not be matched",
                        expr, original_expr
                    );
                }
            }
        }
    }
    while let Some(token) = operators.last() {
        if *token != Token::LParen {
            output.push(operators.pop().unwrap())
        } else {
            panic!(
                "The expression is not valid. \
                A left parenthesis ( '(' ) was found that cannot be matched."
            )
        }
    }
    let mut stack = Vec::with_capacity(output.len());
    for token in output {
        match token {
            Token::Constant(Number(c)) => stack.push(Constant(c)),
            Token::Variable(name) => stack.push(Variable(name)),
            Token::Multiplication => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(Product(vec![op2, op1]));
            }
            Token::Addition => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(Sum(vec![op2, op1]));
            }
            Token::LParen | Token::RParen => {}
        }
    }
    stack.pop().unwrap()
}

/// ```
/// assert!(false)
/// ```
//     debug!(regex.find("+3.3e3+   ").unwrap().as_str());
//     debug!(regex.find("3.3e+3+   ").unwrap().as_str());
//     debug!(regex.find("+3.3e+3+   ").unwrap().as_str());
//     debug!(regex.find(".3e3+    ").unwrap().as_str());
//     debug!(regex.find("3e3+     ").unwrap().as_str());
//     debug!(regex.find("3.3+     ").unwrap().as_str());
//     debug!(regex.find(".3+      ").unwrap().as_str());
//     debug!(regex.find("3+       ").unwrap().as_str());

fn get_starts_with_float_regex() -> Regex {
    let regex = "\
         ^ [\\+-]? \\d+\\.\\d* [Ee] [\\+-]? \\d+ \
        |^ [\\+-]? \\.\\d+     [Ee] [\\+-]? \\d+ \
        |^ [\\+-]? \\d+        [Ee] [\\+-]? \\d+ \
        |^ [\\+-]? \\d+\\.\\d*                   \
        |^ [\\+-]? \\.\\d+                       \
        |^ [\\+-]? \\d+                          ";
    let regex: String = regex.chars().filter(|c| !c.is_whitespace()).collect();
    Regex::new(&regex).unwrap()
}

fn main() {
    let x = &variable("x");

    println!();
    display!(parse("x*x*x"));
    display!(parse("x*x*x").simplify());
    display!(parse("x*x*x").derivative(x));
    display!(parse("x*x*x").derivative(x).simplify());

    println!();
    display!(parse("1+3*5*x"));
    display!(parse("1+3*5*x").simplify());
    display!(parse("1+3*5*x").derivative(x).simplify());

    let expr = Product(vec![x.clone(), x.clone(), x.clone(), x.clone()]);
    println!();
    display!(expr);
    display!(expr.derivative(&x));
    display!(expr.derivative(&x).simplify());
    display!(expr.derivative(&x).simplify().group());

    let expr = Product(vec![
        Product(vec![Product(vec![x.clone(), x.clone()]), x.clone()]),
        x.clone(),
    ]);
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(x));
    println!("{}", expr.derivative(x).simplify());
    display!(expr
        .derivative(&x)
        .simplify()
        .full_expand()
        .full_expand()
        .simplify()
        .group());

    let expr = power(x.clone(), constant(5.0));
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(x));
    println!("{}", expr.derivative(x).simplify());

    let expr = Product(vec![Constant(2.0), parse("x+y")]);
    let expanded = expr.clone().expand();
    println!();
    display!(expr);
    display!(expanded);

    let expr = Product(vec![constant(2.0), parse("x+y"), parse("x+y")]);
    let expanded = expr.clone().expand();
    println!();
    display!(expr);
    display!(expanded);
    display!(expanded.simplify());

    let expr = unary_expr(Exp, parse("5.1e-1*x*x"));
    println!();
    display!(expr);
    display!(expr.derivative(x));
    display!(expr.derivative(x).simplify());

    debug!(parse("3.4*x*y+z"));
    debug!(parse("(5+x+t)*(x+y)*z"));
    debug!(parse("((x+y)*(x+y)+x)*(x+y)"));
    display!(multi_nom_coeff(&vec![2, 2]));

    display!(parse("x*(x+x) + y"));
    display!(parse("(x + x)*x + x*x").full_expand());
    display!(parse("x*x").expand());
}
