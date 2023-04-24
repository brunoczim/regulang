use nom_grapheme_clusters::Span;
use regex::Regex;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Test {
    pub span: Option<Span>,
    pub regex: Regex,
}

#[derive(Debug, Clone)]
pub struct Replacement {
    pub span: Option<Span>,
    pub regex: Regex,
    pub replacement: String,
    pub is_global: bool,
}

#[derive(Debug, Clone)]
pub struct Sequence {
    pub span: Option<Span>,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct Conjunction {
    pub span: Option<Span>,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct Disjunction {
    pub span: Option<Span>,
    pub left: Expression,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub struct Negation {
    pub span: Option<Span>,
    pub target: Expression,
}

#[derive(Debug, Clone)]
pub struct Condition {
    pub span: Option<Span>,
    pub then: Expression,
    pub else_: Expression,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Option<Span>,
    pub name: Arc<str>,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub span: Option<Span>,
    pub bindings: Vec<Binding>,
    pub sub_expr: Expression,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub span: Option<Span>,
    pub identifier: Identifier,
    pub definition: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Test(Box<Test>),
    Replacement(Box<Replacement>),
    Sequence(Box<Sequence>),
    Conjunction(Box<Conjunction>),
    Disjunction(Box<Disjunction>),
    Negation(Box<Negation>),
    Condition(Box<Condition>),
    Identifier(Box<Identifier>),
    Let(Box<Let>),
}

impl Expression {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Test(expr) => expr.span.clone(),
            Self::Replacement(expr) => expr.span.clone(),
            Self::Sequence(expr) => expr.span.clone(),
            Self::Conjunction(expr) => expr.span.clone(),
            Self::Disjunction(expr) => expr.span.clone(),
            Self::Negation(expr) => expr.span.clone(),
            Self::Condition(expr) => expr.span.clone(),
            Self::Identifier(expr) => expr.span.clone(),
            Self::Let(expr) => expr.span.clone(),
        }
    }
}
