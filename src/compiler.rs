use crate::{
    ast::{Identifier, Substitution, Test},
    ir::{Instruction, Label, Program},
};
use nom_grapheme_clusters::{span::Symbol, Span};
use std::collections::{hash_map, HashMap};

pub type Result<T> = ::std::result::Result<T, ErrorList>;

#[derive(Debug, Clone)]
pub enum Error {
    UndefinedIdent(Symbol<Identifier>),
    DuplicatedIdent { first: Symbol<Identifier>, given: Symbol<Identifier> },
}

#[derive(Debug, Clone, Default)]
pub struct ErrorList {
    pub errors: Vec<Error>,
}

#[derive(Debug)]
pub struct Environment {
    counter: Label,
    map: HashMap<String, Vec<Label>>,
}

impl Environment {
    fn new() -> Self {
        Self { counter: 0, map: HashMap::new() }
    }

    pub fn get(&self, ident: &Symbol<Identifier>) -> Result<Label> {
        match self.map.get(&ident.data.name) {
            Some(stack) => Ok(stack[0]),
            None => Err(ErrorList {
                errors: vec![Error::UndefinedIdent(ident.clone())],
            }),
        }
    }

    pub fn scope(&mut self) -> Scope {
        Scope { env: self, idents: HashMap::new() }
    }
}

#[derive(Debug)]
pub struct Scope<'env> {
    env: &'env mut Environment,
    idents: HashMap<String, Span>,
}

impl<'env> Scope<'env> {
    pub fn env(&self) -> &Environment {
        &self.env
    }

    pub fn env_mut(&mut self) -> &mut Environment {
        &mut self.env
    }

    pub fn declare(&mut self, ident: Symbol<Identifier>) -> Result<()> {
        match self.idents.entry(ident.data.name) {
            hash_map::Entry::Occupied(entry) => Err(ErrorList {
                errors: vec![Error::DuplicatedIdent {
                    first: Symbol {
                        span: entry.get().clone(),
                        data: Identifier { name: entry.key().clone() },
                    },
                    given: Symbol {
                        span: ident.span,
                        data: Identifier { name: entry.key().clone() },
                    },
                }],
            }),
            hash_map::Entry::Vacant(entry) => {
                let label = self.env.counter;
                self.env.counter += 1;
                self.env
                    .map
                    .entry(entry.key().clone())
                    .or_default()
                    .push(label);
                entry.insert(ident.span);
                Ok(())
            },
        }
    }

    fn drop_idents(&mut self) {
        for (name, _) in self.idents.drain() {
            match self.env.map.entry(name) {
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().pop();
                    if entry.get().is_empty() {
                        entry.remove();
                    }
                },
                hash_map::Entry::Vacant(_) => panic!("inconsistent scope"),
            }
        }
    }
}

impl<'env> Drop for Scope<'env> {
    fn drop(&mut self) {
        self.drop_idents();
    }
}

pub trait Compile {
    fn compile_internal(
        &self,
        env: &mut Environment,
    ) -> Result<Vec<Instruction>>;

    fn compile(&self) -> Result<Program> {
        let mut env = Environment::new();
        self.compile_internal(&mut env).map(Program::new)
    }
}

impl Compile for Test {
    fn compile_internal(
        &self,
        _env: &mut Environment,
    ) -> Result<Vec<Instruction>> {
        Ok(vec![Instruction::Test(self.regex.clone())])
    }
}

impl Compile for Substitution {
    fn compile_internal(
        &self,
        _env: &mut Environment,
    ) -> Result<Vec<Instruction>> {
        let variant = if self.is_global {
            Instruction::ReplaceGlobal
        } else {
            Instruction::Replace
        };
        Ok(vec![variant(self.regex.clone(), self.substitute.data.clone())])
    }
}

impl Compile for Identifier {
    fn compile_internal(
        &self,
        _env: &mut Environment,
    ) -> Result<Vec<Instruction>> {
        todo!()
    }
}
