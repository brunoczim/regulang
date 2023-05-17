use crate::{compiler, interpreter, parser};
use core::fmt;
use std::io;

#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Parse(parser::ErrorList),
    ParseNom(nom::Err<parser::ErrorList>),
    Compile(compiler::ErrorList),
    CompileInternal(compiler::InternalError),
    Interpret(interpreter::Error),
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self::IO(error)
    }
}

impl From<parser::ErrorList> for Error {
    fn from(errors: parser::ErrorList) -> Self {
        Self::Parse(errors)
    }
}

impl From<nom::Err<parser::ErrorList>> for Error {
    fn from(errors: nom::Err<parser::ErrorList>) -> Self {
        Self::ParseNom(errors)
    }
}

impl From<compiler::ErrorList> for Error {
    fn from(errors: compiler::ErrorList) -> Self {
        Self::Compile(errors)
    }
}

impl From<compiler::InternalError> for Error {
    fn from(error: compiler::InternalError) -> Self {
        Self::CompileInternal(error)
    }
}

impl From<interpreter::Error> for Error {
    fn from(error: interpreter::Error) -> Self {
        Self::Interpret(error)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IO(error) => fmt::Display::fmt(error, fmtr),
            Self::Parse(errors) => fmt::Display::fmt(errors, fmtr),
            Self::ParseNom(errors) => fmt::Display::fmt(errors, fmtr),
            Self::Compile(errors) => fmt::Display::fmt(errors, fmtr),
            Self::CompileInternal(error) => fmt::Display::fmt(error, fmtr),
            Self::Interpret(error) => fmt::Display::fmt(error, fmtr),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::IO(error) => Some(error),
            Self::Parse(errors) => Some(errors),
            Self::ParseNom(errors) => Some(errors),
            Self::Compile(errors) => Some(errors),
            Self::CompileInternal(error) => Some(error),
            Self::Interpret(error) => Some(error),
        }
    }
}
