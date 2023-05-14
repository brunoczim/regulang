use nom_grapheme_clusters::{
    span::{Spanned, Symbol},
    Span,
};
use regex::Regex;

#[derive(Debug, Clone)]
pub struct Test {
    pub regex: Regex,
}

#[derive(Debug, Clone)]
pub struct Substitution {
    pub regex: Regex,
    pub substitute: Symbol<String>,
    pub is_global: bool,
}

#[derive(Debug, Clone)]
pub struct Sequence {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct Conjunction {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct Disjunction {
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct Negation {
    pub target: Expression,
}

#[derive(Debug, Clone)]
pub struct Condition {
    pub condition: Expression,
    pub then: Expression,
    pub else_: Expression,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<Symbol<Binding>>,
    pub sub_expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub identifier: Symbol<Identifier>,
    pub definition: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Test(Box<Symbol<Test>>),
    Substitution(Box<Symbol<Substitution>>),
    Sequence(Box<Symbol<Sequence>>),
    Conjunction(Box<Symbol<Conjunction>>),
    Disjunction(Box<Symbol<Disjunction>>),
    Negation(Box<Symbol<Negation>>),
    Condition(Box<Symbol<Condition>>),
    Identifier(Box<Symbol<Identifier>>),
    Let(Box<Symbol<Let>>),
}

impl Spanned for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Test(expr) => expr.span.clone(),
            Expression::Substitution(expr) => expr.span.clone(),
            Expression::Sequence(expr) => expr.span.clone(),
            Expression::Conjunction(expr) => expr.span.clone(),
            Expression::Disjunction(expr) => expr.span.clone(),
            Expression::Negation(expr) => expr.span.clone(),
            Expression::Condition(expr) => expr.span.clone(),
            Expression::Identifier(expr) => expr.span.clone(),
            Expression::Let(expr) => expr.span.clone(),
        }
    }
}
