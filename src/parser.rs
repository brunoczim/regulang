use crate::ast::{Expression, Test};
use nom::{branch::alt, combinator::map, sequence::tuple, IResult, Parser};
use nom_grapheme_clusters::{
    parse::symbol,
    span::Spanned,
    LocatedSegment,
    Span,
};

#[derive(Debug)]
pub enum ParseError {
    UnknownFlag(LocatedSegment),
    DuplicateFlag(LocatedSegment),
    Nom(nom::error::Error<Span>),
}

impl Spanned for ParseError {
    fn span(&self) -> Span {
        match self {
            Self::UnknownFlag(segment) => segment.span(),
            Self::DuplicateFlag(segment) => segment.span(),
            Self::Nom(nom) => nom.input.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ParseErrorList {
    errors: Vec<ParseError>,
}

impl ParseErrorList {
    pub fn new(error: ParseError) -> Self {
        Self { errors: vec![error] }
    }

    pub fn insert(&mut self, error: ParseError) {
        let span = error.span();
        let index = self.errors.partition_point(|stored| stored.span() <= span);
        self.errors.insert(index, error);
    }
}

impl nom::error::ParseError<Span> for ParseErrorList {
    fn from_error_kind(input: Span, kind: nom::error::ErrorKind) -> Self {
        Self::new(ParseError::Nom(nom::error::Error { input, code: kind }))
    }

    fn append(
        input: Span,
        kind: nom::error::ErrorKind,
        mut other: Self,
    ) -> Self {
        other.insert(ParseError::Nom(nom::error::Error { input, code: kind }));
        other
    }
}

pub fn parse_test(input: Span) -> IResult<Span, Test, ParseErrorList> {
    symbol(tuple(()));
}

fn wrap_expr<P, F, T>(
    parser: P,
    mut function: F,
) -> impl FnMut(Span) -> IResult<Span, Expression, ParseErrorList>
where
    P: Parser<Span, T, ParseErrorList>,
    F: FnMut(Box<T>) -> Expression,
{
    map(parser, move |variant| function(Box::new(variant)))
}

pub fn parse_expr(input: Span) -> IResult<Span, Expression, ParseErrorList> {
    alt((wrap_expr(parse_test, Expression::Test),))(input)
}
