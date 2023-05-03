use crate::ast::{
    Binding,
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
};
use nom::{
    branch::alt,
    combinator::{eof, map, map_res, not, value},
    multi::{fold_many0, many0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
    Parser,
};
use nom_grapheme_clusters::{
    parse::{
        alpha0,
        alphanumeric0,
        any_segment,
        segment,
        symbol,
        whitespace0,
        whitespace1,
        Tag,
    },
    span::{Spanned, Symbol},
    LocatedSegment,
    Span,
};
use regex::{Regex, RegexBuilder};
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Flag {
    I,
    M,
    S,
    U,
    X,
    G,
}

pub type Result<T> = IResult<Span, T, ErrorList>;

#[derive(Debug)]
pub enum Error {
    UnknowwnFlag(Symbol<Flag>),
    DuplicateFlag(Symbol<Flag>),
    Regex(Symbol<regex::Error>),
    Nom(nom::error::Error<Span>),
}

impl Spanned for Error {
    fn span(&self) -> Span {
        match self {
            Self::UnknowwnFlag(segment) => segment.span(),
            Self::DuplicateFlag(segment) => segment.span(),
            Self::Regex(symbol) => symbol.span.clone(),
            Self::Nom(nom) => nom.input.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ErrorList {
    errors: Vec<Error>,
}

impl ErrorList {
    pub fn new(error: Error) -> Self {
        Self { errors: vec![error] }
    }

    pub fn insert(&mut self, error: Error) {
        let span = error.span();
        let index = self.errors.partition_point(|stored| stored.span() <= span);
        self.errors.insert(index, error);
    }
}

impl nom::error::ParseError<Span> for ErrorList {
    fn from_error_kind(input: Span, kind: nom::error::ErrorKind) -> Self {
        Self::new(Error::Nom(nom::error::Error { input, code: kind }))
    }

    fn append(
        input: Span,
        kind: nom::error::ErrorKind,
        mut other: Self,
    ) -> Self {
        other.insert(Error::Nom(nom::error::Error { input, code: kind }));
        other
    }
}

impl nom::error::FromExternalError<Span, Self> for ErrorList {
    fn from_external_error(
        _input: Span,
        _kind: nom::error::ErrorKind,
        e: Self,
    ) -> Self {
        e
    }
}

fn parse_flag(input: Span) -> Result<Flag> {
    alt((
        value(Flag::I, segment("i")),
        value(Flag::M, segment("m")),
        value(Flag::S, segment("s")),
        value(Flag::U, segment("u")),
        value(Flag::X, segment("x")),
        value(Flag::G, segment("g")),
    ))(input)
}

fn parse_flags(input: Span) -> Result<HashSet<Symbol<Flag>>> {
    let parse_seq = terminated(many0(symbol(parse_flag)), not(alpha0));
    let mut parse = map_res(parse_seq, |flags| {
        let mut visited = HashSet::new();
        let mut result = Ok(());
        for flag in flags {
            if !visited.insert(flag.clone()) {
                let error = Error::DuplicateFlag(flag);
                match &mut result {
                    Ok(()) => result = Err(ErrorList::new(error)),
                    Err(error_list) => error_list.insert(error),
                }
            }
        }
        result.map(|_| visited)
    });
    parse(input)
}

fn build_regex(
    code: Span,
    mut flags: HashSet<Symbol<Flag>>,
) -> ::std::result::Result<(Regex, HashSet<Symbol<Flag>>), ErrorList> {
    let mut builder = RegexBuilder::new(code.as_str());
    if flags.remove(&Flag::I) {
        builder.case_insensitive(true);
    }
    if flags.remove(&Flag::M) {
        builder.multi_line(true);
    }
    if flags.remove(&Flag::S) {
        builder.dot_matches_new_line(true);
    }
    if flags.remove(&Flag::U) {
        builder.swap_greed(true);
    }
    if flags.remove(&Flag::X) {
        builder.ignore_whitespace(true);
    }
    match builder.build() {
        Ok(regex) => Ok((regex, flags)),
        Err(error) => Err(ErrorList::new(Error::Regex(Symbol {
            span: code,
            data: error,
        }))),
    }
}

fn parse_operator<'slice, 'seg>(
    tok: Tag<'slice, 'seg>,
) -> impl FnMut(Span) -> Result<()> + 'slice + 'seg
where
    'slice: 'seg,
{
    map(
        tuple((
            tok,
            alt((
                map(eof, |_| ()),
                map(whitespace0, |_| ()),
                not(segment("!")),
                not(segment(",")),
                not(segment(";")),
                not(segment("|")),
                not(segment("&")),
                not(segment("/")),
                not(segment("?")),
                not(segment("=")),
                not(segment(">")),
            )),
        )),
        |_| (),
    )
}

fn parse_leftmost_expr(input: Span) -> Result<Symbol<Expression>> {
    let mut parse = alt((
        wrap_expr(parse_test, Expression::Test),
        wrap_expr(parse_substitution, Expression::Substitution),
        wrap_expr(parse_ident, Expression::Identifier),
        wrap_expr(parse_negation, Expression::Negation),
        wrap_expr(parse_let, Expression::Let),
    ));
    parse(input)
}

fn parse_binop<'slice, 'seg, F, T>(
    tok: Tag<'slice, 'seg>,
    mut make_op: F,
) -> impl FnMut(Span) -> Result<Symbol<T>> + 'slice + 'seg
where
    'slice: 'seg,
    T: 'slice + 'seg,
    F: FnMut(Symbol<Expression>, Symbol<Expression>) -> T + 'slice + 'seg,
{
    let parse_tuple =
        tuple((parse_leftmost_expr, parse_operator(tok), parse_expr));
    let parse = map(parse_tuple, move |(left, _, right)| make_op(left, right));
    symbol(parse)
}

fn wrap_expr<P, F, T>(
    parser: P,
    mut function: F,
) -> impl FnMut(Span) -> Result<Symbol<Expression>>
where
    P: Parser<Span, Symbol<T>, ErrorList>,
    F: FnMut(Box<T>) -> Expression,
{
    map(parser, move |symbol| symbol.map(|variant| function(Box::new(variant))))
}

pub fn parse_test(input: Span) -> Result<Symbol<Test>> {
    let parse_regex_char =
        alt((preceded(segment("\\"), segment("/")), any_segment));
    let parse_regex_chars =
        symbol(fold_many0(parse_regex_char, || (), |_, _| ()));
    let parse_pattern_code =
        delimited(segment("/"), parse_regex_chars, segment("/"));
    let parse_as_tuple = tuple((parse_pattern_code, parse_flags));
    let parse_regex = map_res(parse_as_tuple, |(pattern, flags)| {
        build_regex(pattern.span, flags)
    });
    let parse = map_res(parse_regex, |(regex, flags)| {
        let mut result = Ok(Test { regex });
        for flag in flags {
            let error = Error::UnknowwnFlag(flag);
            match &mut result {
                Ok(_) => result = Err(ErrorList::new(error)),
                Err(errors) => errors.insert(error),
            }
        }
        result
    });
    symbol(parse)(input)
}

pub fn parse_substitution(input: Span) -> Result<Symbol<Substitution>> {
    let parse_char =
        || alt((preceded(segment("\\"), segment("/")), any_segment));
    let parse_regex_chars = symbol(fold_many0(parse_char(), || (), |_, _| ()));
    let parse_substitute_chars =
        symbol(fold_many0(parse_char(), String::new, |mut buf, segment| {
            buf.push_str(segment.as_str());
            buf
        }));

    let parse_pattern_code =
        delimited(Tag(&["s", "/"]), parse_regex_chars, segment("/"));
    let parse_substitute = terminated(
        parse_substitute_chars,
        alt((map(segment("/"), |segment: LocatedSegment| segment.span()), eof)),
    );

    let parse_as_tuple =
        tuple((parse_pattern_code, parse_substitute, parse_flags));
    let parse_regex =
        map_res(parse_as_tuple, |(pattern, substitute, flags)| {
            build_regex(pattern.span, flags)
                .map(|(regex, flags)| (regex, substitute, flags))
        });
    let parse = map_res(parse_regex, |(regex, substitute, mut flags)| {
        let mut result = Ok(Substitution {
            regex,
            substitute,
            is_global: flags.remove(&Flag::G),
        });
        for flag in flags {
            let error = Error::UnknowwnFlag(flag);
            match &mut result {
                Ok(_) => result = Err(ErrorList::new(error)),
                Err(errors) => errors.insert(error),
            }
        }
        result
    });
    symbol(parse)(input)
}

pub fn parse_ident(input: Span) -> Result<Symbol<Identifier>> {
    let parse_symbol = symbol(tuple((
        alt((alpha0::<Span, _>, Tag(&["_"]), Tag(&["-"]))),
        many0(alt((alphanumeric0, Tag(&["_"]), Tag(&["-"])))),
    )));
    let mut parse = map(parse_symbol, |symbol| Symbol {
        data: Identifier { name: symbol.span.as_str().to_owned() },
        span: symbol.span,
    });
    parse(input)
}

pub fn parse_binding(input: Span) -> Result<Symbol<Binding>> {
    let parse_tuple = delimited(
        whitespace0,
        symbol(tuple((
            parse_ident,
            delimited(
                tuple((whitespace0, segment("="), whitespace0)),
                parse_expr,
                tuple((whitespace0, segment(";"))),
            ),
        ))),
        whitespace0,
    );

    let mut parser = map(parse_tuple, |symbol| {
        symbol
            .map(|(identifier, definition)| Binding { identifier, definition })
    });
    parser(input)
}

pub fn parse_let(input: Span) -> Result<Symbol<Let>> {
    let parse_let_tok = terminated::<_, _, _, ErrorList, _, _>(
        Tag(&["l", "e", "t"]),
        whitespace1,
    );
    let parse_bindings = many0(parse_binding);
    let parse_in_tok = terminated(Tag(&["i", "n"]), whitespace1);
    let parse_tuple = tuple((
        preceded(parse_let_tok, parse_bindings),
        preceded(parse_in_tok, parse_expr),
    ));
    let mut parse = symbol(map(parse_tuple, |(bindings, sub_expr)| Let {
        bindings,
        sub_expr,
    }));
    parse(input)
}

pub fn parse_negation(input: Span) -> Result<Symbol<Negation>> {
    let parse =
        map(preceded(parse_operator(Tag(&["!"])), parse_expr), |target| {
            Negation { target }
        });
    symbol(parse)(input)
}

pub fn parse_sequence(input: Span) -> Result<Symbol<Sequence>> {
    parse_binop(Tag(&[","]), |left, right| Sequence { left, right })(input)
}

pub fn parse_disjunction(input: Span) -> Result<Symbol<Disjunction>> {
    parse_binop(Tag(&["|"]), |left, right| Disjunction { left, right })(input)
}

pub fn parse_conjunction(input: Span) -> Result<Symbol<Conjunction>> {
    parse_binop(Tag(&["&"]), |left, right| Conjunction { left, right })(input)
}

pub fn parse_condition(input: Span) -> Result<Symbol<Condition>> {
    let parse_tuple = tuple((
        preceded(parse_operator(Tag(&["?"])), parse_expr),
        preceded(parse_operator(Tag(&["=", ">"])), parse_expr),
        preceded(parse_operator(Tag(&["!", ">"])), parse_expr),
    ));

    symbol(map(parse_tuple, |(condition, then, else_)| Condition {
        condition,
        then,
        else_,
    }))(input)
}

pub fn parse_expr(input: Span) -> Result<Symbol<Expression>> {
    let mut parse = alt((
        parse_leftmost_expr,
        wrap_expr(parse_sequence, Expression::Sequence),
        wrap_expr(parse_disjunction, Expression::Disjunction),
        wrap_expr(parse_conjunction, Expression::Conjunction),
        wrap_expr(parse_condition, Expression::Condition),
        delimited(segment("("), parse_expr, segment(")")),
    ));
    parse(input)
}
