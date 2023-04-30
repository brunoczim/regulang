use nom_grapheme_clusters::span::Symbol;
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
    pub left: Symbol<Expression>,
    pub right: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub struct Conjunction {
    pub left: Symbol<Expression>,
    pub right: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub struct Disjunction {
    pub left: Symbol<Expression>,
    pub right: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub struct Negation {
    pub target: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub struct Condition {
    pub condition: Symbol<Expression>,
    pub then: Symbol<Expression>,
    pub else_: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub bindings: Vec<Symbol<Binding>>,
    pub sub_expr: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub identifier: Symbol<Identifier>,
    pub definition: Symbol<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Test(Box<Test>),
    Substitution(Box<Substitution>),
    Sequence(Box<Sequence>),
    Conjunction(Box<Conjunction>),
    Disjunction(Box<Disjunction>),
    Negation(Box<Negation>),
    Condition(Box<Condition>),
    Identifier(Box<Identifier>),
    Let(Box<Let>),
}
