use regex::Regex;
use std::{borrow::Cow, mem};

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
pub struct TooMuchNesting;

#[derive(Debug)]
pub struct StackGuard {
    maximum_nesting: usize,
}

impl StackGuard {
    pub fn new(available_calls: usize) -> Self {
        Self { maximum_nesting: available_calls }
    }

    pub fn nest(&self) -> Result<Self, TooMuchNesting> {
        match self.maximum_nesting.checked_sub(1) {
            Some(count) => Ok(Self { maximum_nesting: count }),
            None => Err(TooMuchNesting),
        }
    }
}

pub trait Evaluable {
    fn eval(
        &self,
        input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting>;
}

#[derive(Debug, Clone)]
pub struct Test {
    pub regex: Regex,
}

impl Evaluable for Test {
    fn eval(
        &self,
        input: String,
        _guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        Ok((self.regex.is_match(&input), input))
    }
}

#[derive(Debug, Clone)]
pub struct Replacement {
    pub regex: Regex,
    pub replacement: String,
    pub is_global: bool,
}

impl Evaluable for Replacement {
    fn eval(
        &self,
        input: String,
        _guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let replaced = if self.is_global {
            self.regex.replace_all(&input, &self.replacement)
        } else {
            self.regex.replace(&input, &self.replacement)
        };
        match replaced {
            Cow::Borrowed(_) => Ok((false, input)),
            Cow::Owned(output) => Ok((true, output)),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Sequence {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Sequence }

impl Evaluable for Sequence {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = true;
        for sub_expr in &self.sub_exprs {
            let (status, output) = sub_expr.eval(input, guard.nest()?)?;
            last_status = status;
            input = output;
        }
        Ok((last_status, input))
    }
}

#[derive(Debug, Clone, Default)]
pub struct Conjunction {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Conjunction }

impl Evaluable for Conjunction {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = true;
        let mut sub_exprs = self.sub_exprs.iter();
        loop {
            if !last_status {
                break;
            }
            let sub_expr = match sub_exprs.next() {
                Some(sub_expr) => sub_expr,
                None => break,
            };
            let (status, output) = sub_expr.eval(input, guard.nest()?)?;
            last_status = status;
            input = output;
        }
        Ok((last_status, input))
    }
}

#[derive(Debug, Clone, Default)]
pub struct Disjunction {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Disjunction }

impl Evaluable for Disjunction {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = false;
        let mut sub_exprs = self.sub_exprs.iter();
        loop {
            if last_status {
                break;
            }
            let sub_expr = match sub_exprs.next() {
                Some(sub_expr) => sub_expr,
                None => break,
            };
            let (status, output) = sub_expr.eval(input, guard.nest()?)?;
            last_status = status;
            input = output;
        }
        Ok((last_status, input))
    }
}

#[derive(Debug, Clone, Default)]
pub struct Negation {
    sub_exprs: Vec<Expression>,
}

impl_nested! { Negation }

impl Evaluable for Negation {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = true;
        for sub_expr in &self.sub_exprs {
            let (status, output) = sub_expr.eval(input, guard.nest()?)?;
            last_status = status;
            input = output;
        }
        Ok((!last_status, input))
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Test(Test),
    Replacement(Replacement),
    Sequence(Sequence),
    Conjunction(Conjunction),
    Disjunction(Disjunction),
    Negation(Negation),
}

impl Evaluable for Expression {
    fn eval(
        &self,
        input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        match self {
            Self::Test(test) => test.eval(input, guard),
            Self::Replacement(replacement) => replacement.eval(input, guard),
            Self::Sequence(sequence) => sequence.eval(input, guard),
            Self::Conjunction(conjunction) => conjunction.eval(input, guard),
            Self::Disjunction(disjunction) => disjunction.eval(input, guard),
            Self::Negation(negation) => negation.eval(input, guard),
        }
    }
}
