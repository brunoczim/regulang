use regex::Regex;
use std::sync::Arc;

pub type Label = usize;

#[derive(Debug, Clone)]
pub enum Instruction<L = Label> {
    Test(Regex),
    Replace(Regex, String),
    ReplaceGlobal(Regex, String),
    JumpTrue(L),
    JumpFalse(L),
    Jump(L),
    SetTrue,
    SetFalse,
    Save,
    Restore,
    Discard,
}

impl<L> Instruction<L> {
    pub fn map_labels<F, M>(self, mapper: F) -> Instruction<M>
    where
        F: FnOnce(L) -> M,
    {
        match self {
            Instruction::Test(regex) => Instruction::Test(regex),
            Instruction::Replace(regex, repl) => {
                Instruction::Replace(regex, repl)
            },
            Instruction::ReplaceGlobal(regex, repl) => {
                Instruction::Replace(regex, repl)
            },
            Instruction::JumpTrue(label) => {
                Instruction::JumpTrue(mapper(label))
            },
            Instruction::JumpFalse(label) => {
                Instruction::JumpFalse(mapper(label))
            },
            Instruction::Jump(label) => Instruction::Jump(mapper(label)),
            Instruction::SetTrue => Instruction::SetTrue,
            Instruction::SetFalse => Instruction::SetFalse,
            Instruction::Save => Instruction::Save,
            Instruction::Restore => Instruction::Restore,
            Instruction::Discard => Instruction::Discard,
        }
    }

    pub fn visit_labels<F, T>(&mut self, visitor: F) -> Option<T>
    where
        F: FnOnce(&mut L) -> T,
    {
        match self {
            Instruction::JumpTrue(label)
            | Instruction::JumpFalse(label)
            | Instruction::Jump(label) => Some(visitor(label)),
            _ => None,
        }
    }
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
