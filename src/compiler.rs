use crate::{
    ast::Identifier,
    error::ResultExt,
    ir::{Instruction, Label, Program},
};
use nom_grapheme_clusters::{
    span::{Spanned, Symbol},
    Span,
};

#[derive(Debug, Clone)]
pub enum InternalError {
    UndefinedInternal(InternalLabel),
}

#[derive(Debug, Clone)]
pub enum Error {
    UndefinedIdent(Symbol<Identifier>),
    DuplicatedIdent {
        duplicated: Symbol<Identifier>,
        first: Symbol<Identifier>,
    },
}

impl Spanned for Error {
    fn span(&self) -> Span {
        match self {
            Error::UndefinedIdent(ident) => ident.span.clone(),
            Error::DuplicatedIdent { duplicated, first: _ } => {
                duplicated.span.clone()
            },
        }
    }
}

pub type ErrorList = crate::error::ErrorList<Error>;

pub type Result<T> = std::result::Result<T, ErrorList>;

pub type InternalLabel = u128;

#[derive(Debug, Clone)]
pub enum UnlinkedLabel {
    External(Symbol<Identifier>),
    Internal(InternalLabel),
}

#[derive(Debug, Clone)]
pub enum LinkLabelKind {
    Linked(Label),
    Unlinked(UnlinkedLabel),
}

#[derive(Debug, Clone)]
pub struct LinkLabel {
    kind: LinkLabelKind,
}

impl LinkLabel {
    pub fn new_unlinked(unlinked_label: UnlinkedLabel) -> Self {
        Self { kind: LinkLabelKind::Unlinked(unlinked_label) }
    }

    pub fn kind(&self) -> &LinkLabelKind {
        &self.kind
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    instructions: Vec<Instruction<LinkLabel>>,
    internal_counter: InternalLabel,
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl Module {
    pub fn new() -> Self {
        Self { instructions: Vec::new(), internal_counter: 0 }
    }

    pub fn create_internal(&mut self) -> InternalLabel {
        let internal = self.internal_counter;
        self.internal_counter += 1;
        internal
    }

    pub fn link_external(
        &mut self,
        identifier: &Symbol<Identifier>,
        linked: Label,
    ) {
        for instruction in &mut self.instructions {
            instruction.visit_labels(|label| {
                use LinkLabelKind::*;
                use UnlinkedLabel::*;

                if let Unlinked(External(target)) = &mut label.kind {
                    if target.data.name == identifier.data.name {
                        label.kind = Linked(linked);
                    }
                }
            });
        }
    }

    pub fn link_internal(&mut self, internal: InternalLabel, linked: Label) {
        for instruction in &mut self.instructions {
            instruction.visit_labels(|label| {
                use LinkLabelKind::*;
                use UnlinkedLabel::*;

                if let Unlinked(Internal(target)) = &mut label.kind {
                    if *target == internal {
                        label.kind = Linked(linked);
                    }
                }
            });
        }
    }

    pub fn end(&self) -> Label {
        self.instructions.len()
    }

    pub fn push_instruction(
        &mut self,
        instruction: Instruction<LinkLabel>,
    ) -> Label {
        let label = self.end();
        self.instructions.push(instruction);
        label
    }

    pub fn merge(&mut self, mut other: Self) -> &mut Self {
        for instruction in &mut other.instructions {
            use LinkLabelKind::*;
            use UnlinkedLabel::*;

            instruction.visit_labels(|link_label| match &mut link_label.kind {
                Linked(label) => *label += self.end(),
                Unlinked(External(_)) => (),
                Unlinked(Internal(internal)) => {
                    *internal += self.internal_counter
                },
            });
        }

        self.instructions.append(&mut other.instructions);
        self.internal_counter += other.internal_counter;
        self
    }

    pub fn into_program(self) -> Result<Program> {
        self.try_into_program().expect("internal compiler error")
    }

    pub fn try_into_program(
        self,
    ) -> std::result::Result<Result<Program>, InternalError> {
        let mut result = Ok(Vec::with_capacity(self.instructions.len()));
        for instruction in self.instructions {
            use LinkLabelKind::*;
            use UnlinkedLabel::*;

            match instruction {
                Instruction::Test(regex) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Test(regex));
                    }
                },
                Instruction::Replace(regex, repl) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Replace(regex, repl));
                    }
                },
                Instruction::ReplaceGlobal(regex, repl) => {
                    if let Ok(instructions) = &mut result {
                        instructions
                            .push(Instruction::ReplaceGlobal(regex, repl));
                    }
                },
                Instruction::Jump(LinkLabel { kind: Linked(label) }) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Jump(label));
                    }
                },
                Instruction::IfTrue(LinkLabel { kind: Linked(label) }) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::IfTrue(label));
                    }
                },
                Instruction::IfFalse(LinkLabel { kind: Linked(label) }) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::IfFalse(label));
                    }
                },
                Instruction::Save => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Save);
                    }
                },
                Instruction::Restore => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Restore);
                    }
                },
                Instruction::Discard => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Discard);
                    }
                },

                Instruction::Jump(LinkLabel {
                    kind: Unlinked(Internal(internal)),
                })
                | Instruction::IfTrue(LinkLabel {
                    kind: Unlinked(Internal(internal)),
                })
                | Instruction::IfFalse(LinkLabel {
                    kind: Unlinked(Internal(internal)),
                }) => Err(InternalError::UndefinedInternal(internal))?,

                Instruction::Jump(LinkLabel {
                    kind: Unlinked(External(identifier)),
                })
                | Instruction::IfTrue(LinkLabel {
                    kind: Unlinked(External(identifier)),
                })
                | Instruction::IfFalse(LinkLabel {
                    kind: Unlinked(External(identifier)),
                }) => {
                    result.raise_error(Error::UndefinedIdent(identifier));
                },
            }
        }

        Ok(result.map(Program::new))
    }
}

pub trait Compile {
    fn compile_to_module(&self) -> Module;

    fn try_compile(
        &self,
    ) -> std::result::Result<Result<Program>, InternalError> {
        self.compile_to_module().try_into_program()
    }

    fn compile(&self) -> Result<Program> {
        self.compile_to_module().into_program()
    }
}
