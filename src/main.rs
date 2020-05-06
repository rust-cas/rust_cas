use itertools::Itertools;
use regex::Regex;

use std::cmp::Ordering;
use std::fmt::Formatter;
use std::fmt::Write;
use std::str::FromStr;

use crate::AssociativeOp::{Addition, Multiplication};
use crate::Expr::{AssociativeExpr, Constant, Power, UnaryExpr, Variable};
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

macro_rules! product {
    ( $( $x:expr ),* ) => {
        AssociativeExpr{op: Multiplication, args: vec![ $($x,)* ]}
    };
}

macro_rules! sum  {
    ( $( $x:expr ),* ) => {
        AssociativeExpr{op: Addition, args: vec![ $($x,)* ]}
    };
}

#[macro_use]
extern crate lazy_static;

enum Expr {
    Variable(String),
    Constant(f64),
    UnaryExpr { op: UnaryOp, a: Box<Expr> },
    AssociativeExpr { op: AssociativeOp, args: Vec<Expr> },
    Power { base: Box<Expr>, exp: Box<Expr> },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
enum UnaryOp {
    Minus,
    Exp,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
enum AssociativeOp {
    Addition,
    Multiplication,
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        let simplified = self.simplify();
        let other_simplified = other.simplify();
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

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable(name) => write!(f, "{}", name),
            Constant(c) => write!(f, "{}", c),
            UnaryExpr { op, a } => match op {
                Minus => write!(f, "- ({:?})", a),
                _ => write!(f, "{}({:?})", op, a),
            },
            AssociativeExpr { op, args } => {
                let arg_strings = args.iter().map(|e| {
                    let mut s = String::new();
                    write!(s, "{:?}", e).unwrap();
                    s
                }).collect::<Vec<String>>();
                match op {
                    Addition => {
                        write!(f, "sum({:})", &arg_strings[..].join(", "))
                    }
                    Multiplication => {
                        write!(f, "prod({:})", &arg_strings[..].join(", "))
                    }
                }
            },
            Power { base, exp } => {
                write!(f, "({:?})^({:?})", base, exp)
            }
        }
    }
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
            AssociativeExpr { op, args } => match op {
                Addition => {
                    let arg_strings = args.iter().map(|e| e.to_string()).collect::<Vec<String>>();
                    write!(f, "{}", &arg_strings[..].join(" + "))
                }
                Multiplication => {
                    let arg_strings = args
                        .iter()
                        .map(|e| match *e {
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

fn associative_expr(op: AssociativeOp, args: Vec<Expr>) -> Expr {
    AssociativeExpr { op, args }
}

fn unary_expr(op: UnaryOp, a: Expr) -> Expr {
    UnaryExpr { op, a: Box::new(a) }
}

fn product_skeleton(size: usize) -> Expr {
    AssociativeExpr {
        op: Multiplication,
        args: Vec::with_capacity(size),
    }
}

fn sum_skeleton(size: usize) -> Expr {
    AssociativeExpr {
        op: Addition,
        args: Vec::with_capacity(size),
    }
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

impl Clone for Expr {
    fn clone(&self) -> Self {
        match *self {
            Variable(ref name) => Variable(name.clone()),
            Constant(c) => Constant(c),
            UnaryExpr { ref a, ref op } => UnaryExpr {
                a: (*a).clone(),
                op: *op,
            },
            AssociativeExpr { ref op, ref args } => AssociativeExpr {
                op: *op,
                args: args.iter().map(|arg| arg.clone()).collect(),
            },
            Power { ref base, ref exp } => Power {
                base: (*base).clone(),
                exp: (*exp).clone(),
            },
        }
    }
}

impl Expr {

    fn is_expandable(&self) -> bool {
        match *self {
            AssociativeExpr { ref op, ref args } => {
                op == &Multiplication && args.iter().any(|arg| arg.is_addition())
            }
            //Power { ref base, .. } => base.is_addition() || base.is_expandable(),
            _ => false,
        }
    }

    fn expand_leaves(self) -> Expr {
        match self {
            AssociativeExpr { op, args } => AssociativeExpr {
                op,
                args: args.iter().map(|e| {
                    e.clone().full_expand()
                }).collect(),
            },
            _ => self,
        }
    }

    #[allow(dead_code)]
    fn full_expand_clone(&self) -> Expr {
        self.clone().full_expand()
    }

    fn full_expand(self) -> Expr {
        let expr = self.expand_leaves();
        expr.expand()
    }

    fn expand(self) -> Expr {
        if let AssociativeExpr { ref op, ref args } = self {
            if *op == Multiplication {
                if ! self.is_expandable() {
                    return self
                }
                let unexpandable_factors: Vec<&Expr> =
                    args.iter().filter(|arg| !arg.is_addition()).collect();
                let expanded_args = args
                    .iter()
                    .filter_map(|arg| {
                        if let AssociativeExpr {
                            op: ref child_op,
                            args: ref child_args,
                        } = arg
                        {
                            if *child_op == Addition {
                                Some(child_args.clone())
                            } else {
                                None
                            }
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
                        associative_expr(Multiplication, e)
                    })
                    .collect();
                associative_expr(Addition, expanded_args)
            } else {
                self
            }
        } else {
            self
        }
    }

    fn is_addition(&self) -> bool {
        if let AssociativeExpr { ref op, .. } = *self {
            *op == Addition
        } else {
            false
        }
    }

    fn get_args(&mut self) -> Option<&mut Vec<Expr>> {
        match self {
            AssociativeExpr { op: _, args } => Some(args),
            _ => None,
        }
    }

    fn clone(&self) -> Expr {
        match self {
            Variable(name) => Variable(name.clone()),
            Constant(c) => Constant(*c),
            UnaryExpr { a, op } => UnaryExpr {
                a: (*a).clone(),
                op: *op,
            },
            AssociativeExpr { op, args } => AssociativeExpr {
                op: *op,
                args: args.iter().map(|arg| arg.clone()).collect(),
            },
            Power { base, exp } => Power {
                base: (*base).clone(),
                exp: (*exp).clone(),
            },
        }
    }

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
                            product!(self.clone(), a.derivative(&x))
                        }
                    }
                },
                AssociativeExpr { op, args } => match op {
                    Addition => {
                        // todo filter out constants
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
                                    false => args[j].clone(),
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
                            if **base == *x {
                                product!(constant(n), power((**base).clone(), constant(n - 1.0)))
                            } else {
                                product!(
                                    constant(n),
                                    power((**base).clone(), constant(n - 1.0)),
                                    (**base).derivative(&x)
                                )
                            }
                        }
                    }
                    _ => panic!("derivative can currently only handle constant exponents yet"),
                },
            }
        } else {
            panic!("x is not a variable")
        }
    }

    fn simplify_leaves(&self) -> Expr {
        match self {
            AssociativeExpr { op, args } => {
                associative_expr(*op, args.iter().map(|e| e.simplify()).collect())
            }
            UnaryExpr { op, a } => unary_expr(*op, a.simplify()),
            Power { base, exp } => power(base.simplify(), exp.simplify()),
            _ => self.clone(),
        }
    }

    fn group(self) -> Expr {
        let grouped = self.clone().group_once();
        if  grouped != self {
            grouped.group()
        } else {
            grouped
        }
    }

    fn group_once(self) -> Expr {
        // group equal expressions
        // for example, turn x+x into 2*x, or x*x into x^2
        match self {
            AssociativeExpr { op, args } => {
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
                        grouped.push(args[i].clone());
                    } else {
                        let repeated = args[i].clone();
                        let times = Constant((k - i) as f64);
                        grouped.push(match op {
                            Addition => match repeated {
                                AssociativeExpr {
                                    op: repeated_op,
                                    args: ref repeated_args,
                                } => {
                                    // if the repeated expression is a multiplication,
                                    // we just insert repeated into repeated_args
                                    if repeated_op == Multiplication {
                                        let mut repeated_args = repeated_args.clone();
                                        repeated_args.insert(0, times);
                                        AssociativeExpr {
                                            op: Multiplication,
                                            args: repeated_args,
                                        }
                                    } else {
                                        product!(times, repeated)
                                    }
                                }
                                _ => product!(times, repeated),
                            },
                            Multiplication => power(repeated, times),
                        });
                    }
                    i = k;
                }
                match grouped.len() {
                    1 => grouped[0].clone(),
                    _ => associative_expr(op, grouped),
                }
            }
            _ => self
        }
    }

    fn simplify(&self) -> Expr {
        let simplified_leaves = self.simplify_leaves();
        let simplified = match simplified_leaves {
            AssociativeExpr { op, args } => {
                if op == Multiplication && args.contains(&constant(0.0)) {
                    constant(0.0)
                } else {
                    let mut count = args.len();
                    for arg in &args {
                        match *arg {
                            AssociativeExpr {
                                op: ref child_op,
                                args: ref child_args,
                            } => {
                                if child_op == &op {
                                    count -= 1;
                                    for child_arg in child_args {
                                        count += match *child_arg {
                                            Constant(_) => 0,
                                            _ => 1,
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
                        match arg {
                            AssociativeExpr {
                                op: ref child_op,
                                args: ref child_args,
                            } => {
                                if *child_op == op {
                                    for child_arg in child_args {
                                        match *child_arg {
                                            AssociativeExpr { .. }
                                            | UnaryExpr { .. }
                                            | Power { .. }
                                            | Variable(_) => non_constants.push(child_arg.clone()),
                                            Constant(c) => match op {
                                                Addition => accumulated_constant += c,
                                                Multiplication => accumulated_constant *= c,
                                            },
                                        }
                                    }
                                } else {
                                    non_constants.push(arg.clone());
                                }
                            }
                            Constant(c) => match op {
                                Addition => accumulated_constant += c,
                                Multiplication => accumulated_constant *= c,
                            },
                            _ => non_constants.push(arg.clone()),
                        }
                    }
                    // could make the code above more efficient by using sort sooner
                    match non_constants.len() {
                        0 => constant(accumulated_constant),
                        _ => {
                            if non_constants.len() == 1 && accumulated_constant == unit {
                                non_constants[0].clone()
                            } else {
                                if accumulated_constant != unit {
                                    match op {
                                        Addition => {
                                            non_constants.push(constant(accumulated_constant));
                                        }
                                        Multiplication => {
                                            non_constants.insert(0, constant(accumulated_constant));
                                        }
                                    }
                                }
                                associative_expr(op, non_constants)
                            }
                        }
                    }
                }
            }
            _ => self.clone(),
        };
        simplified
    }
}

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd)]
enum TokenType {
    ConstantT,
    VariableT,
    MultiplicationT,
    AdditionT,
    RParen,
    LParen,
    None,
}
use crate::TokenType::{AdditionT, ConstantT, LParen, MultiplicationT, RParen, VariableT};

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd)]
struct Token<'e>(TokenType, &'e str);

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Token(_, token) = self;
        write!(f, "{}", token)
    }
}

#[allow(dead_code)]
fn display_vec_token(vec_token: &Vec<Token<'_>>) {
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
        let mut token_type = TokenType::None;
        let mut token = "";
        if let Some(match_) = STARTS_WITH_NUMBER.find(expr) {
            token_type = ConstantT;
            token = match_.as_str();
            expr = &expr[match_.end()..];
        } else if let Some(match_) = STARTS_WITH_OPERATOR.find(expr) {
            token = match_.as_str();
            token_type = match token {
                "*" => MultiplicationT,
                "+" => AdditionT,
                _ => panic!("unsupported token: {}", token),
            };
            expr = &expr[match_.end()..];
        } else if let Some(match_) = STARTS_WITH_VARIABLE.find(expr) {
            token_type = VariableT;
            token = match_.as_str();
            expr = &expr[match_.end()..];
        } else if expr.starts_with('(') {
            token_type = LParen;
            token = "(";
            expr = &expr[1..];
        } else if expr.starts_with(')') {
            token_type = RParen;
            token = ")";
            expr = &expr[1..];
        }
        match token_type {
            ConstantT => output.push(Token(ConstantT, token)),
            VariableT => output.push(Token(VariableT, token)),
            AdditionT => {
                while let Some(Token(token_type, _)) = operators.last() {
                    if *token_type != LParen {
                        output.push(operators.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operators.push(Token(AdditionT, token))
            }
            MultiplicationT => {
                while let Some(Token(token_type, _)) = operators.last() {
                    if *token_type == MultiplicationT {
                        output.push(operators.pop().unwrap());
                    } else {
                        break;
                    }
                }
                operators.push(Token(MultiplicationT, token))
            }
            LParen => operators.push(Token(LParen, token)),
            RParen => {
                let mut found_matching_lparen = false;
                while let Some(Token(token_type, _)) = operators.last() {
                    if *token_type != LParen {
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
            TokenType::None => {
                panic!(
                    "The expression is not valid. \
                        No operator, variable, or constant found at {}",
                    expr
                );
            }
        }
    }
    while let Some(Token(token_type, _)) = operators.last() {
        if *token_type != LParen {
            output.push(operators.pop().unwrap())
        } else {
            panic!(
                "The expression is not valid. \
                A left parenthesis ( '(' ) was found that cannot be matched."
            )
        }
    }
    let mut stack = Vec::with_capacity(output.len());
    for i in 0..output.len() {
        let Token(token_type, token) = &output[i];
        match *token_type {
            ConstantT => {
                if let Ok(c) = f64::from_str(token) {
                    stack.push(Constant(c));
                } else {
                    println!("cannot parse float from '{}'", token);
                    panic!();
                }
            }
            VariableT => stack.push(variable(token.clone())),
            MultiplicationT => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(product!(op2, op1));
            }
            AdditionT => {
                let op1 = stack.pop().unwrap();
                let op2 = stack.pop().unwrap();
                stack.push(sum!(op2, op1));
            }
            LParen | RParen | TokenType::None => {}
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

fn get_float_regex() -> Regex {
    let regex = "\
          [\\+-]? \\d+\\.\\d* [Ee] [\\+-]? \\d+ \
        | [\\+-]? \\.\\d+     [Ee] [\\+-]? \\d+ \
        | [\\+-]? \\d+        [Ee] [\\+-]? \\d+ \
        | [\\+-]? \\d+\\.\\d*                   \
        | [\\+-]? \\.\\d+                       \
        | [\\+-]? \\d+                          ";
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

    let expr = product!(x.clone(), x.clone(), x.clone(), x.clone());
    println!();
    display!(expr);
    display!(expr.derivative(&x));
    display!(expr.derivative(&x).simplify());
    display!(expr.derivative(&x).simplify().group());

    let expr = product!(
        product!(product!(x.clone(), x.clone()), x.clone()),
        x.clone()
    );
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(x));
    println!("{}", expr.derivative(x).simplify());
    display!(expr.derivative(&x).simplify().full_expand().full_expand().simplify().group());

    println!();
    println!("{}", expr.simplify());
    println!("{}", expr.simplify().derivative(x));
    println!("{}", expr.simplify().derivative(x).simplify());

    let expr = power(x.clone(), constant(5.0));
    println!();
    println!("{}", expr);
    println!("{}", expr.derivative(x));
    println!("{}", expr.derivative(x).simplify());

    let expr = product!(constant(2.0), parse("x+y"));
    let expanded = expr.clone().expand();
    println!();
    display!(expr);
    display!(expanded);

    let expr = product!(constant(2.0), parse("x+y"), parse("x+y"));
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
