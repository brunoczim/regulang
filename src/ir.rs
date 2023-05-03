use regex::Regex;
use std::sync::Arc;

pub type Label = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Test(Regex),
    Replace(Regex, String),
    ReplaceGlobal(Regex, String),
    IfTrue(Label),
    IfFalse(Label),
    Jump(Label),
    Save,
    Restore,
    Discard,
}

#[derive(Debug, Clone)]
pub struct OutOfBoundsLabel {
    pub label: Label,
}

#[derive(Debug, Clone)]
pub struct Program {
    instructions: Arc<[Instruction]>,
}

impl Program {
    pub fn new<I>(instructions: I) -> Self
    where
        I: IntoIterator<Item = Instruction>,
    {
        Self { instructions: instructions.into_iter().collect() }
    }

    pub fn get(&self, label: Label) -> Result<&Instruction, OutOfBoundsLabel> {
        self.instructions.get(label).ok_or(OutOfBoundsLabel { label })
    }
}
