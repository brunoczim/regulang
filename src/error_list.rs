use core::fmt;
use nom_grapheme_clusters::span::Spanned;
use std::{slice, vec};

#[derive(Debug)]
pub struct ErrorList<E> {
    errors: Vec<E>,
}

impl<E> ErrorList<E>
where
    E: Spanned,
{
    pub fn new(error: E) -> Self {
        Self { errors: vec![error] }
    }

    pub fn insert(&mut self, error: E) {
        let span = error.span();
        let index = self.errors.partition_point(|stored| stored.span() <= span);
        self.errors.insert(index, error);
    }

    pub fn merge(&mut self, other: &mut Self) {
        self.errors.reserve(other.len());
        for error in other.errors.drain(..) {
            self.insert(error);
        }
    }
}

impl<E> ErrorList<E> {
    pub fn first(&self) -> &E {
        &self.errors[0]
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn iter(&self) -> Iter<E> {
        Iter { inner: self.errors.iter() }
    }
}

impl<E> fmt::Display for ErrorList<E>
where
    E: fmt::Display,
{
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        for error in self {
            writeln!(fmtr, "{}", error)?;
        }
        Ok(())
    }
}

impl<E> std::error::Error for ErrorList<E>
where
    E: std::error::Error + 'static,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(self.first())
    }
}

impl<'this, E> IntoIterator for &'this ErrorList<E> {
    type Item = &'this E;
    type IntoIter = Iter<'this, E>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<E> IntoIterator for ErrorList<E> {
    type Item = E;
    type IntoIter = IntoIter<E>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { inner: self.errors.into_iter() }
    }
}

#[derive(Debug)]
pub struct Iter<'list, E> {
    inner: slice::Iter<'list, E>,
}

impl<'list, E> Iterator for Iter<'list, E> {
    type Item = &'list E;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[derive(Debug)]
pub struct IntoIter<E> {
    inner: vec::IntoIter<E>,
}

impl<E> Iterator for IntoIter<E> {
    type Item = E;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub trait ResultExt<T, E> {
    fn raise_error(&mut self, error: E);

    fn merge_and_zip<U>(
        self,
        other: Result<U, ErrorList<E>>,
    ) -> Result<(T, U), ErrorList<E>>;
}

impl<T, E> ResultExt<T, E> for Result<T, ErrorList<E>>
where
    E: Spanned,
{
    fn raise_error(&mut self, error: E) {
        match self {
            Ok(_) => *self = Err(ErrorList::new(error)),
            Err(errors) => errors.insert(error),
        }
    }

    fn merge_and_zip<U>(
        self,
        other: Result<U, ErrorList<E>>,
    ) -> Result<(T, U), ErrorList<E>> {
        match (self, other) {
            (Ok(left), Ok(right)) => Ok((left, right)),
            (Err(mut left), Err(mut right)) => Err({
                left.merge(&mut right);
                left
            }),
            (Err(errors), Ok(_)) | (Ok(_), Err(errors)) => Err(errors),
        }
    }
}
