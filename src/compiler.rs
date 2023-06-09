use crate::{
    ast::{
        Condition,
        Conjunction,
        Disjunction,
        Expression,
        Identifier,
        Let,
        Negation,
        Sequence,
        Substitution,
        Test,
    },
    error_list::ResultExt,
    ir::{Instruction, Label, Program},
};
use core::fmt;
use nom_grapheme_clusters::{
    span::{Spanned, Symbol},
    Span,
};
use std::collections::{hash_map, HashMap};

#[derive(Debug, Clone)]
pub enum InternalError {
    UndefinedInternal(InternalLabel),
}

impl fmt::Display for InternalError {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UndefinedInternal(label) => write!(
                fmtr,
                "undefined internal label {} has not been linked",
                label
            ),
        }
    }
}

impl std::error::Error for InternalError {}

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

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        write!(fmtr, "link error: ")?;
        match self {
            Self::UndefinedIdent(ident) => {
                write!(fmtr, "undefined identifier {}", ident.data.name)
            },
            Self::DuplicatedIdent { duplicated, first } => {
                write!(
                    fmtr,
                    "duplicated identifier {} (first defined {})",
                    duplicated.data.name,
                    first.span.start()
                )
            },
        }?;
        write!(fmtr, " in {}", self.span().start())?;
        Ok(())
    }
}

impl std::error::Error for Error {}

pub type ErrorList = crate::error_list::ErrorList<Error>;

pub type Result<T> = std::result::Result<T, ErrorList>;

pub type InternalLabel = u128;

#[derive(Debug, Clone)]
pub enum LinkLabel {
    Linked(Label),
    External(Symbol<Identifier>),
    Internal(InternalLabel),
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
                if let LinkLabel::External(target) = label {
                    if target.data.name == identifier.data.name {
                        *label = LinkLabel::Linked(linked);
                    }
                }
            });
        }
    }

    pub fn link_internal(&mut self, internal: InternalLabel, linked: Label) {
        for instruction in &mut self.instructions {
            instruction.visit_labels(|label| {
                if let LinkLabel::Internal(target) = label {
                    if *target == internal {
                        *label = LinkLabel::Linked(linked);
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

    pub fn append(&mut self, mut other: Self) -> &mut Self {
        for instruction in &mut other.instructions {
            instruction.visit_labels(|link_label| match link_label {
                LinkLabel::Linked(label) => *label += self.end(),
                LinkLabel::External(_) => (),
                LinkLabel::Internal(internal) => {
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
                Instruction::Jump(LinkLabel::Linked(label)) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::Jump(label));
                    }
                },
                Instruction::JumpTrue(LinkLabel::Linked(label)) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::JumpTrue(label));
                    }
                },
                Instruction::JumpFalse(LinkLabel::Linked(label)) => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::JumpFalse(label));
                    }
                },
                Instruction::SetTrue => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::SetTrue);
                    }
                },
                Instruction::SetFalse => {
                    if let Ok(instructions) = &mut result {
                        instructions.push(Instruction::SetFalse);
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
                Instruction::Jump(LinkLabel::Internal(internal))
                | Instruction::JumpTrue(LinkLabel::Internal(internal))
                | Instruction::JumpFalse(LinkLabel::Internal(internal)) => {
                    Err(InternalError::UndefinedInternal(internal))?
                },
                Instruction::Jump(LinkLabel::External(identifier))
                | Instruction::JumpTrue(LinkLabel::External(identifier))
                | Instruction::JumpFalse(LinkLabel::External(identifier)) => {
                    result.raise_error(Error::UndefinedIdent(identifier));
                },
            }
        }

        Ok(result.map(Program::new))
    }
}

pub trait Compile {
    fn compile_to_module(&self) -> Result<Module>;

    fn try_compile(
        &self,
    ) -> std::result::Result<Result<Program>, InternalError> {
        match self.compile_to_module() {
            Ok(module) => module.try_into_program(),
            Err(errors) => Ok(Err(errors)),
        }
    }

    fn compile(&self) -> Result<Program> {
        self.compile_to_module()?.into_program()
    }
}

impl Compile for Symbol<Test> {
    fn compile_to_module(&self) -> Result<Module> {
        let mut module = Module::new();
        module.push_instruction(Instruction::Test(self.data.regex.clone()));
        Ok(module)
    }
}

impl Compile for Symbol<Substitution> {
    fn compile_to_module(&self) -> Result<Module> {
        let mut module = Module::new();
        let variant = if self.data.is_global {
            Instruction::ReplaceGlobal
        } else {
            Instruction::Replace
        };
        module.push_instruction(variant(
            self.data.regex.clone(),
            self.data.substitute.data.clone(),
        ));
        Ok(module)
    }
}

impl Compile for Symbol<Sequence> {
    fn compile_to_module(&self) -> Result<Module> {
        self.data
            .left
            .compile_to_module()
            .merge_and_zip(self.data.right.compile_to_module())
            .map(|(mut left, right)| {
                left.append(right);
                left
            })
    }
}

impl Compile for Symbol<Conjunction> {
    fn compile_to_module(&self) -> Result<Module> {
        self.data
            .left
            .compile_to_module()
            .merge_and_zip(self.data.right.compile_to_module())
            .map(|(left, right)| {
                let mut module = Module::new();
                let internal_label0 = module.create_internal();
                let internal_label1 = module.create_internal();

                module.push_instruction(Instruction::Save);
                module.append(left);
                module.push_instruction(Instruction::JumpFalse(
                    LinkLabel::Internal(internal_label0),
                ));
                module.append(right);
                module.push_instruction(Instruction::Discard);
                module.push_instruction(Instruction::Jump(
                    LinkLabel::Internal(internal_label1),
                ));
                let label0 = module.end();
                module.push_instruction(Instruction::Restore);
                let label1 = module.end();

                module.link_internal(internal_label0, label0);
                module.link_internal(internal_label1, label1);

                module
            })
    }
}

impl Compile for Symbol<Disjunction> {
    fn compile_to_module(&self) -> Result<Module> {
        self.data
            .left
            .compile_to_module()
            .merge_and_zip(self.data.right.compile_to_module())
            .map(|(left, right)| {
                let mut module = Module::new();
                let internal_label0 = module.create_internal();
                let internal_label1 = module.create_internal();

                module.push_instruction(Instruction::Save);
                module.append(left);
                module.push_instruction(Instruction::JumpTrue(
                    LinkLabel::Internal(internal_label0),
                ));
                module.push_instruction(Instruction::Restore);
                module.append(right);
                module.push_instruction(Instruction::Jump(
                    LinkLabel::Internal(internal_label1),
                ));
                let label0 = module.end();
                module.push_instruction(Instruction::Discard);
                let label1 = module.end();

                module.link_internal(internal_label0, label0);
                module.link_internal(internal_label1, label1);

                module
            })
    }
}

impl Compile for Symbol<Negation> {
    fn compile_to_module(&self) -> Result<Module> {
        self.data.target.compile_to_module().map(|inner| {
            let mut module = Module::new();
            let internal_label0 = module.create_internal();
            let internal_label1 = module.create_internal();

            module.push_instruction(Instruction::Save);
            module.append(inner);
            module.push_instruction(Instruction::JumpTrue(
                LinkLabel::Internal(internal_label0),
            ));
            module.push_instruction(Instruction::Discard);
            module.push_instruction(Instruction::SetTrue);
            module.push_instruction(Instruction::Jump(LinkLabel::Internal(
                internal_label1,
            )));
            let label0 = module.end();
            module.push_instruction(Instruction::Restore);
            module.push_instruction(Instruction::SetFalse);
            module.push_instruction(Instruction::Jump(LinkLabel::Internal(
                internal_label1,
            )));
            let label1 = module.end();

            module.link_internal(internal_label0, label0);
            module.link_internal(internal_label1, label1);

            module
        })
    }
}

impl Compile for Symbol<Condition> {
    fn compile_to_module(&self) -> Result<Module> {
        self.data
            .condition
            .compile_to_module()
            .merge_and_zip(self.data.then.compile_to_module())
            .merge_and_zip(self.data.else_.compile_to_module())
            .map(|((cond, then), else_)| {
                let mut module = Module::new();
                let internal_label0 = module.create_internal();
                let internal_label1 = module.create_internal();

                module.append(cond);
                module.push_instruction(Instruction::JumpTrue(
                    LinkLabel::Internal(internal_label0),
                ));
                module.append(then);
                module.push_instruction(Instruction::JumpTrue(
                    LinkLabel::Internal(internal_label1),
                ));
                let label0 = module.end();
                module.append(else_);
                let label1 = module.end();

                module.link_internal(internal_label0, label0);
                module.link_internal(internal_label1, label1);

                module
            })
    }
}

impl Compile for Symbol<Identifier> {
    fn compile_to_module(&self) -> Result<Module> {
        let mut module = Module::new();
        module.push_instruction(Instruction::Save);
        module.push_instruction(Instruction::Jump(LinkLabel::External(
            self.clone(),
        )));
        Ok(module)
    }
}

impl Compile for Symbol<Let> {
    fn compile_to_module(&self) -> Result<Module> {
        let mut result = self.data.sub_expr.compile_to_module();
        let mut env = HashMap::new();

        for binding in &self.data.bindings {
            match env.entry(binding.data.identifier.data.name.clone()) {
                hash_map::Entry::Vacant(entry) => {
                    let label = match &result {
                        Ok(module) => module.end(),
                        Err(_) => 0,
                    };
                    entry.insert((binding.data.identifier.span.clone(), label));
                    let subresult = binding.data.definition.compile_to_module();
                    result = result.merge_and_zip(subresult).map(
                        |(mut left, right)| {
                            left.append(right);
                            left
                        },
                    );
                },

                hash_map::Entry::Occupied(entry) => {
                    result.raise_error(Error::DuplicatedIdent {
                        duplicated: Symbol {
                            span: entry.get().0.clone(),
                            data: binding.data.identifier.data.clone(),
                        },
                        first: binding.data.identifier.clone(),
                    })
                },
            }
        }

        if let Ok(module) = &mut result {
            for (name, (span, label)) in env {
                module.link_external(
                    &Symbol { span, data: Identifier { name } },
                    label,
                );
            }
        }

        result
    }
}

impl Compile for Expression {
    fn compile_to_module(&self) -> Result<Module> {
        match self {
            Expression::Test(expr) => expr.compile_to_module(),
            Expression::Substitution(expr) => expr.compile_to_module(),
            Expression::Sequence(expr) => expr.compile_to_module(),
            Expression::Conjunction(expr) => expr.compile_to_module(),
            Expression::Disjunction(expr) => expr.compile_to_module(),
            Expression::Negation(expr) => expr.compile_to_module(),
            Expression::Condition(expr) => expr.compile_to_module(),
            Expression::Identifier(expr) => expr.compile_to_module(),
            Expression::Let(expr) => expr.compile_to_module(),
        }
    }
}
