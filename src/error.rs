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
}

impl<E> ErrorList<E> {
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn iter(&self) -> Iter<E> {
        Iter { inner: self.errors.iter() }
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
}
