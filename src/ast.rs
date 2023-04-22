use regex::Regex;
use std::mem;

macro_rules! impl_nested {
    ($type:ident) => {
        impl $type {
            pub fn new() -> Self {
                Self::default()
            }

            pub fn push(&mut self, mut expression: Expression) {
                if let Expression::$type(other) = &mut expression {
                    if self.sub_exprs.len() == 0 {
                        mem::swap(self, other);
                    } else {
                        self.sub_exprs.append(&mut other.sub_exprs);
                    }
                    return;
                }
                self.sub_exprs.push(expression);
            }

            pub fn sub_exprs(&self) -> &[Expression] {
                &self.sub_exprs[..]
            }

            pub fn into_sub_exprs(self) -> Vec<Expression> {
                self.sub_exprs
            }
        }

        impl Extend<Expression> for $type {
            fn extend<I>(&mut self, iterable: I)
            where
                I: IntoIterator<Item = Expression>,
            {
                let iterator = iterable.into_iter();
                let (low, _) = iterator.size_hint();
                self.sub_exprs.reserve(low);
                for sub_expr in iterator {
                    self.push(sub_expr);
                }
            }
        }

        impl FromIterator<Expression> for $type {
            fn from_iter<I>(iterable: I) -> Self
            where
                I: IntoIterator<Item = Expression>,
            {
                let mut this = Self::new();
                this.extend(iterable);
                this
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct Test {
    pub regex: Regex,
}

#[derive(Debug, Clone)]
pub struct Replacement {
    pub regex: Regex,
    pub replacement: String,
    pub is_global: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Sequence {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Sequence }

#[derive(Debug, Clone, Default)]
pub struct Conjunction {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Conjunction }

#[derive(Debug, Clone, Default)]
pub struct Disjunction {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Disjunction }

#[derive(Debug, Clone, Default)]
pub struct Negation {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Negation }

#[derive(Debug, Clone)]
pub enum Expression {
    Test(Test),
    Replacement(Replacement),
    Sequence(Sequence),
    Conjunction(Conjunction),
    Disjunction(Disjunction),
    Negation(Negation),
}
