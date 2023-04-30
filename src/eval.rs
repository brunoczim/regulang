use crate::ast::{
    Conjunction,
    Disjunction,
    Expression,
    Negation,
    Sequence,
    Substitution,
    Test,
};
use std::borrow::Cow;

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

impl Evaluable for Test {
    fn eval(
        &self,
        input: String,
        _guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        Ok((self.regex.is_match(&input), input))
    }
}

impl Evaluable for Substitution {
    fn eval(
        &self,
        input: String,
        _guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let replaced = if self.is_global {
            self.regex.replace_all(&input, &self.substitute)
        } else {
            self.regex.replace(&input, &self.substitute)
        };
        match replaced {
            Cow::Borrowed(_) => Ok((false, input)),
            Cow::Owned(output) => Ok((true, output)),
        }
    }
}

impl Evaluable for Sequence {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = true;
        for sub_expr in self.sub_exprs() {
            let (status, output) = sub_expr.eval(input, guard.nest()?)?;
            last_status = status;
            input = output;
        }
        Ok((last_status, input))
    }
}

impl Evaluable for Conjunction {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = true;
        let mut sub_exprs = self.sub_exprs().iter();
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

impl Evaluable for Disjunction {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = false;
        let mut sub_exprs = self.sub_exprs().iter();
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

impl Evaluable for Negation {
    fn eval(
        &self,
        mut input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        let mut last_status = true;
        for sub_expr in self.sub_exprs() {
            let (status, output) = sub_expr.eval(input, guard.nest()?)?;
            last_status = status;
            input = output;
        }
        Ok((!last_status, input))
    }
}

impl Evaluable for Expression {
    fn eval(
        &self,
        input: String,
        guard: StackGuard,
    ) -> Result<(bool, String), TooMuchNesting> {
        match self {
            Self::Test(test) => test.eval(input, guard),
            Self::Substitution(replacement) => replacement.eval(input, guard),
            Self::Sequence(sequence) => sequence.eval(input, guard),
            Self::Conjunction(conjunction) => conjunction.eval(input, guard),
            Self::Disjunction(disjunction) => disjunction.eval(input, guard),
            Self::Negation(negation) => negation.eval(input, guard),
        }
    }
}
