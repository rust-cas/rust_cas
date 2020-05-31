use itertools::Itertools;
use regex::Regex;

use std::cmp::Ordering;
use std::fmt::Formatter;
use std::fmt::Write;
use std::str::FromStr;

use crate::Expr::*;
use crate::FuncName::*;

// TODO: support parsing ^ for exponentiation

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
    E,
    Function(FuncName, Box<Expr>),
    Sum(Vec<Expr>),
    Product(Vec<Expr>),
    Power { base: Box<Expr>, exp: Box<Expr> },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
enum FuncName {
    Sin,
    Cos,
    Log,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        // todo remove these simplifications or make the optional
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
            E => match other_simplified {
                E => true,
                _ => false,
            },
            Function(func, arg) => match other_simplified {
                Function(other_func, other_arg) => func == other_func && arg == other_arg,
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
            E => match other {
                Variable(_) | Constant(_) => Ordering::Greater,
                E => Ordering::Equal,
                _ => Ordering::Less,
            },
            Function(func, arg) => match other {
                Variable(_) | Constant(_) | E => Ordering::Greater,
                Function(other_func, other_arg) => match func.cmp(&other_func) {
                    Ordering::Equal => arg.cmp(&other_arg),
                    _ => func.cmp(&other_func),
                },
                _ => Ordering::Less,
            },
            Sum(args) => match other {
                Variable(_) | Constant(_) | E | Function(_, _) => Ordering::Greater,
                Sum(other_args) => sort(args).cmp(&sort(other_args)),
                _ => Ordering::Less,
            },
            Product(args) => match other {
                Variable(_) | Constant(_) | E | Function(_, _) | Sum(_) => Ordering::Greater,
                Product(other_args) => sort(args).cmp(&sort(other_args)),
                _ => Ordering::Less,
            },
            Power { base, exp } => match other {
                Variable(_) | Constant(_) | E | Function(_, _) | Sum(_) | Product(_) => {
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

impl std::fmt::Display for FuncName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Sin => "sin",
                Cos => "cos",
                Log => "log",
            }
        )
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable(name) => write!(f, "{}", name),
            Constant(c) => write!(f, "{}", c),
            E => write!(f, "e"),
            Function(func, arg) => write!(f, "{}({:?})", func, arg),
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
            E => write!(f, "e"),
            Function(func, arg) => write!(f, "{}({})", func, arg),
            Sum(args) => {
                let arg_strings = args.iter().map(|e| e.to_string()).collect::<Vec<String>>();
                write!(f, "{}", &arg_strings[..].join(" + "))
            }
            Product(args) => {
                let arg_strings = args
                    .iter()
                    .map(|e| match *e {
                        Variable(_)
                        | Constant(_)
                        | E
                        | Function(_, _)
                        | Product(_)
                        | Power { .. } => e.to_string(),
                        Sum(_) => format!("({})", e),
                    })
                    .collect::<Vec<String>>();
                write!(f, "{}", &arg_strings[..].join("·"))
            }
            Power { base, exp } => {
                match **base {
                    Variable(_) | Constant(_) | E => write!(f, "{}^", base)?,
                    Function(_, _) | Sum(_) | Product(_) | Power { .. } => {
                        write!(f, "({})^", base)?
                    }
                }
                match **exp {
                    Variable(_) | Constant(_) | E => write!(f, "{}", exp),
                    Function(_, _) | Sum(_) | Product(_) | Power { .. } => write!(f, "({})", exp),
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
            E => E,
            Function(func, ref arg) => Function(*func, arg.clone()),
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
    const MINUS_ONE: Expr = Constant(-1.0);

    fn is_constant(&self) -> bool {
        match *self {
            Constant(_) | E => true,
            _ => false,
        }
    }

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

    fn derivative(&self, x: &Expr) -> Expr {
        let der = self.derivative_wo_simplification(x);
        der.simplify()
    }

    fn derivative_wo_simplification(&self, x: &Expr) -> Expr {
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
                E => constant(0.0),
                Function(func, arg) => {
                    let derivative = match func {
                        Sin => Function(Cos, arg.clone()),
                        Cos => Product(vec![Expr::MINUS_ONE, Function(Sin, arg.clone())]),
                        Log => Power {
                            base: arg.clone(),
                            exp: Box::new(Expr::MINUS_ONE),
                        },
                    };
                    if **arg != *x {
                        Product(vec![derivative, arg.derivative_wo_simplification(x)])
                    } else {
                        derivative
                    }
                }
                Sum(args) => Sum(args
                    .iter()
                    .filter_map(|e| {
                        if !e.is_constant() {
                            Some(e.derivative_wo_simplification(&x))
                        } else {
                            None
                        }
                    })
                    .collect()),
                Product(args) => {
                    let mut derivative = sum_skeleton(args.len());
                    let terms = derivative.get_args().unwrap();
                    for i in 0..args.len() {
                        terms.push(product_skeleton(args.len()));
                        let factors = terms[i].get_args().unwrap();
                        for j in 0..args.len() {
                            factors.push(match j == i {
                                true => args[j].derivative_wo_simplification(&x),
                                false => args[j].clone(),
                            });
                        }
                    }
                    derivative
                }
                Power { base, exp } => {
                    if let Constant(n) = **exp {
                        if n == 0.0 {
                            Expr::ONE
                        } else {
                            let mut args =
                                vec![constant(n), power((**base).clone(), constant(n - 1.0))];
                            if **base != *x {
                                args.push((**base).derivative_wo_simplification(&x));
                            }
                            Product(args)
                        }
                    } else if **base == E {
                        if **exp == *x {
                            self.clone()
                        } else {
                            Product(vec![self.clone(), exp.derivative(x)])
                        }
                    } else {
                        //Variable(name) => if exp == x {},
                        panic!("unimplemented")
                    }
                }
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
                                    | Function(_, _)
                                    | Power { .. }
                                    | E
                                    | Variable(_) => new_args.push(child_arg.clone()),
                                    Constant(c) => constant *= c,
                                }
                            }
                        }
                        Constant(c) => constant *= c,
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
                                    | Function(_, _)
                                    | Power { .. }
                                    | E
                                    | Variable(_) => new_args.push(child_arg.clone()),
                                    Constant(c) => constant += c,
                                }
                            }
                        }
                        Constant(c) => constant += c,
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
            Function(func, arg) => Function(func, Box::new(arg.simplify())),
            Power { base, exp } => power(base.simplify(), exp.simplify()),
            _ => self,
        };
        simplified
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Token<'e> {
    Constant(Number),
    Variable(String),
    Function(&'e str),
    Multiplication,
    Division,
    Addition,
    Subtraction,
    LParen,
    RParen,
}

#[derive(Debug, Clone)]
struct Number(f64); // we define this so that we can derive PartialEq for Token
                    // in effect, we assert here (by implementing std::cmd::PartialEq for Number)
                    // that the contents of Number is not NaN.

impl std::cmp::PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl std::cmp::Eq for Number {}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Constant(Number(c)) => write!(f, "{}", c),
            Token::Variable(name) => write!(f, "{}", name),
            Token::Function(name) => write!(f, "{}", name),
            Token::Multiplication => write!(f, "*"),
            Token::Division => write!(f, "/"),
            Token::Addition => write!(f, "+"),
            Token::Subtraction => write!(f, "-"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
        }
    }
}

impl Token<'_> {
    fn is_function(&self) -> bool {
        match self {
            Token::Function(_) => true,
            _ => false,
        }
    }
    fn is_variable(&self) -> bool {
        match self {
            Token::Variable(_) => true,
            _ => false,
        }
    }
    fn is_constant(&self) -> bool {
        match self {
            Token::Constant(_) => true,
            _ => false,
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
        static ref STARTS_WITH_FUNCTION: Regex = get_starts_with_function_regex();
        static ref STARTS_WITH_VARIABLE: Regex =
            Regex::new("^[a-zA-Z_]+\\[\\d+\\]|^[a-zA-Z_]+").unwrap();
    }
    let expr: String = expr.chars().filter(|c| !c.is_whitespace()).collect();
    let mut expr = &expr[..];
    // we parse the expression using a shunting yard algorithm. See
    // https://en.wikipedia.org/wiki/Shunting-yard_algorithm
    let mut output: Vec<Token> = Vec::with_capacity(expr.len());
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
    let mut option_last_token: Option<Token> = None;
    while expr != "" {
        let mut option_token = None;
        if expr.starts_with('+') || expr.starts_with('-') {
            let parse_as_operator = if let Some(last) = option_last_token {
                last.is_constant() || last.is_variable() || last == Token::RParen
            } else {
                true
            };
            if parse_as_operator {
                match expr.chars().next().unwrap() {
                    '+' => option_token = Some(Token::Addition),
                    '-' => option_token = Some(Token::Subtraction),
                    _ => panic!("internal error"),
                }
            }
        };
        let token = if let Some(unwrapped_token) = option_token {
            expr = &expr[1..];
            unwrapped_token
        } else if let Some(match_) = STARTS_WITH_NUMBER.find(expr) {
            expr = &expr[match_.end()..];
            Token::Constant(Number(f64::from_str(match_.as_str()).unwrap()))
        } else if let Some(match_) = STARTS_WITH_FUNCTION.find(expr) {
            expr = &expr[match_.end()..];
            Token::Function(match_.as_str())
        } else if let Some(match_) = STARTS_WITH_VARIABLE.find(expr) {
            expr = &expr[match_.end()..];
            Token::Variable(match_.as_str().to_string())
        } else if expr.starts_with('*') {
            expr = &expr[1..];
            Token::Multiplication
        } else if expr.starts_with('/') {
            expr = &expr[1..];
            Token::Division
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
        option_last_token = Some(token.clone());
        // display!(token);
        // display!(expr);
        // debug!(operators);
        // debug!(output);
        // println!();
        match token {
            Token::Constant(_) | Token::Variable(_) => output.push(token),
            Token::Addition | Token::Subtraction => {
                while let Some(operator) = operators.last() {
                    if *operator != Token::LParen {
                        output.push(operators.pop().unwrap())
                    } else {
                        break;
                    }
                }
                operators.push(token);
            }
            Token::Multiplication | Token::Division => {
                // todo: figure out the associativity of operators
                while let Some(operator) = operators.last() {
                    if *operator == Token::Multiplication
                        || *operator == Token::Division
                        || operator.is_function()
                    {
                        output.push(operators.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operators.push(token)
            }
            Token::Function(_) => {
                while let Some(operator) = operators.last() {
                    if operator.is_function() {
                        output.push(operators.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operators.push(token)
            }
            Token::LParen => operators.push(token),
            Token::RParen => {
                let mut found_matching_lparen = false;
                while let Some(operator) = operators.last() {
                    if *operator != Token::LParen {
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

    while let Some(operator) = operators.last() {
        if *operator != Token::LParen {
            output.push(operators.pop().unwrap())
        } else {
            panic!(
                "The expression is not valid. \
                A left parenthesis ( '(' ) was found that cannot be matched."
            )
        }
    }
    //debug!(output);
    let mut stack = Vec::with_capacity(output.len());
    for token in output {
        //  debug!(stack);
        match token {
            Token::Constant(Number(c)) => stack.push(Constant(c)),
            Token::Variable(name) => stack.push(Variable(name)),
            Token::LParen | Token::RParen => {}
            Token::Function(name) => {
                let arg = Box::new(stack.pop().unwrap());
                stack.push(match &name[..] {
                    "exp" => Power {
                        base: Box::new(E),
                        exp: arg,
                    },
                    "log" => Function(Log, arg),
                    "sin" => Function(Sin, arg),
                    "cos" => Function(Cos, arg),
                    "tan" => {
                        let sin = Function(Sin, arg.clone());
                        let cos = Function(Cos, arg);
                        Product(vec![
                            sin,
                            Power {
                                base: Box::new(cos),
                                exp: Box::new(Expr::MINUS_ONE),
                            },
                        ])
                    }
                    _ => panic!("internal error: unknown function {}", name),
                });
            }
            Token::Addition | Token::Subtraction => {
                let op1 = stack.pop().unwrap();
                let op1= if token == Token::Subtraction {
                    Product(vec![Expr::MINUS_ONE, op1])
                } else {
                    op1
                };
                let op2 = stack.pop().unwrap_or(Expr::ZERO);
                if let Sum(mut op2_args) = op2 {
                    if let Sum(mut op1_args) = op1 {
                        op2_args.append(&mut op1_args);
                        stack.push(Sum(op2_args))
                    } else {
                        op2_args.push(op1);
                        stack.push(Sum(op2_args))
                    }
                } else {
                    if let Sum(mut op1_args) = op1 {
                        op1_args.insert(0, op2);
                        stack.push(Sum(op1_args))
                    } else {
                        stack.push(Sum(vec![op2, op1]))
                    }
                }
            }
            Token::Multiplication | Token::Division => {
                let op1 = stack.pop().unwrap();
                let op1 = if token == Token::Division {
                    Power {
                        base: Box::new(op1),
                        exp: Box::new(Expr::MINUS_ONE),
                    }
                } else {
                    op1
                };
                let op2 = stack.pop().unwrap();
                if let Product(mut op2_args) = op2 {
                    if let Product(mut op1_args) = op1 {
                        op2_args.append(&mut op1_args);
                        stack.push(Product(op2_args))
                    } else {
                        op2_args.push(op1);
                        stack.push(Product(op2_args))
                    }
                } else {
                    if let Product(mut op1_args) = op1 {
                        op1_args.insert(0, op2);
                        stack.push(Product(op1_args))
                    } else {
                        stack.push(Product(vec![op2, op1]))
                    }
                }
            }
        }
    }
    // debug!(stack);
    if stack.len() != 1 {
        debug!(stack);
        panic!(
            "Internal Error, Stack should contain one element, but contains {} elements",
            stack.len()
        );
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

fn get_starts_with_function_regex() -> Regex {
    Regex::new("^sin|^cos|^tan|^exp|^log").unwrap()
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
    println!("{}", expr.derivative_wo_simplification(x));
    println!("{}", expr.derivative_wo_simplification(x).simplify());
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
    println!("{}", expr.derivative_wo_simplification(x));
    println!("{}", expr.derivative_wo_simplification(x).simplify());

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

    let expr = Power {
        base: Box::new(E),
        exp: Box::new(parse("5.1e-1*x*x")),
    };
    println!();
    display!(expr);
    display!(expr.derivative(x));
    display!(expr.derivative(x).simplify());

    debug!(parse("3.4*x*y+z"));
    debug!(parse("(5+x+t)*(x+y)*z"));
    debug!(parse("((x+y)*(x+y)+x)"));
    debug!(parse("-x+y"));
    display!(multi_nom_coeff(&vec![2, 2]));

    display!(parse("x*(x+x) + y"));
    display!(parse("(x + x)*x + x*x").full_expand());

    display!(parse("1-3*5*x"));
    display!(parse("x/(y*a*b*c)"));
    display!(parse("x/(y*a*b*c)").derivative(&parse("y")));
    display!(parse("x/(y*a*b*c)").derivative(&parse("y")).simplify());
    display!(parse("0+x"));
    display!(parse("0+x").simplify());

    println!();
    //    display!(parse("y[0]+exp(1/exp(y[1])+x)*y[11]"));
    //    display!(parse("1/exp(x)"));
    //display!(parse("y[0]*(-1.965927E+4)+y[11]*(2.1E+1/4.0E+2)"));
    let expr = "y[0]*(-1.965927E+4)+y[11]*(2.1E+1/4.0E+2)-y[96]*(y[144]*5.3175E+1+y[145]*5.3175E+1+y[146]*5.3175E+1+y[147]*5.3175E+1+y[148]*5.3175E+1+y[149]*5.3175E+1+y[150]*5.3175E+1+y[151]*5.3175E+1+y[152]*5.3175E+1-8.343294077331707E+1)+y[6]*exp(y[153]*(-2.145E-2))*7.308510546875-y[0]*y[152]*2.493";

    display!(parse(expr));
    display!(parse(expr).simplify());
    display!(parse("y[0]*(-1.965927E+4)+y[11]*(2.1E+1/4.0E+2)-y[96]*(y[144]*5.3175E+1+y[145]*5.3175E+1+1-8.343294077331707E+1)+y[6]*exp(y[153]*(-2.145E-2))*7.308510546875-y[0]*y[152]*2.493"));
    //display!(parse(
    //    "y[0]*(-1.965927E+4)+y[11]*(2.1E+1/4.0E+2)-y[96]*(y[144]*5.3175E+1+y[145]*5.3175E+1)"
    //));
    display!(parse("x+y*(exp(y)+1)+y"));
    //display!(parse("--x")); // panics
    display!(parse("--1"));
    //display!(parse("--(1)")); // panics
    //display!(parse("(0)--(1)")); // panics
    display!(parse("(0)--1"));
    display!(parse("sin(x)"));
    display!(parse("sin(x)").derivative(x));
    display!(parse("sin(x)").derivative(x).derivative(x));
    display!(parse("sin(x)").derivative(x).derivative(x).derivative(x));
    display!(parse("sin(x)")
        .derivative(x)
        .derivative(x)
        .derivative(x)
        .derivative(x));

    display!(parse("sin(sin(x))").derivative(x));
    display!(parse("sin(exp(x))").derivative(x));
    display!(parse("exp(exp(x))").derivative(x));
    display!(parse("exp(sin(x))").derivative(x));

    display!(parse("log(sin(x))"));
    display!(parse("log(sin(x))").derivative(x));
    display!(parse("tan(x)"));
    display!(parse("tan(x)").derivative(x));
    display!(parse("tan(x)").derivative(x).simplify().group());
    debug!(parse("tan(x)").derivative(x).group());
    display!(parse("sin(x)/(cos(x))*sin(x)").group());
}
