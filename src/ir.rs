use regex::Regex;
use std::sync::Arc;

pub type Label = usize;

#[derive(Debug, Clone)]
pub enum Instruction<L = Label> {
    Test(Regex),
    Replace(Regex, String),
    ReplaceGlobal(Regex, String),
    IfTrue(L),
    IfFalse(L),
    Jump(L),
    Save,
    Restore,
    Discard,
}

impl<L> Instruction<L> {
    pub fn clone_or_as_ref<F, M>(&self) -> Instruction<&L> {
        match self {
            Instruction::Test(regex) => Instruction::Test(regex.clone()),
            Instruction::Replace(regex, repl) => {
                Instruction::Replace(regex.clone(), repl.clone())
            },
            Instruction::ReplaceGlobal(regex, repl) => {
                Instruction::Replace(regex.clone(), repl.clone())
            },
            Instruction::IfTrue(label) => Instruction::IfTrue(label),
            Instruction::IfFalse(label) => Instruction::IfFalse(label),
            Instruction::Jump(label) => Instruction::Jump(label),
            Instruction::Save => Instruction::Save,
            Instruction::Restore => Instruction::Restore,
            Instruction::Discard => Instruction::Discard,
        }
    }

    pub fn clone_or_as_mut<F, M>(&mut self) -> Instruction<&mut L> {
        match self {
            Instruction::Test(regex) => Instruction::Test(regex.clone()),
            Instruction::Replace(regex, repl) => {
                Instruction::Replace(regex.clone(), repl.clone())
            },
            Instruction::ReplaceGlobal(regex, repl) => {
                Instruction::Replace(regex.clone(), repl.clone())
            },
            Instruction::IfTrue(label) => Instruction::IfTrue(label),
            Instruction::IfFalse(label) => Instruction::IfFalse(label),
            Instruction::Jump(label) => Instruction::Jump(label),
            Instruction::Save => Instruction::Save,
            Instruction::Restore => Instruction::Restore,
            Instruction::Discard => Instruction::Discard,
        }
    }

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
            Instruction::IfTrue(label) => Instruction::IfTrue(mapper(label)),
            Instruction::IfFalse(label) => Instruction::IfFalse(mapper(label)),
            Instruction::Jump(label) => Instruction::Jump(mapper(label)),
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
            Instruction::IfTrue(label)
            | Instruction::IfFalse(label)
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
