use crate::{
    ast::{
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
    },
    error_list::ResultExt,
};
use core::fmt;
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

#[derive(Debug, Clone, Copy)]
pub struct Config {
    stack_limit: Option<u32>,
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

impl Config {
    pub fn new() -> Self {
        Self { stack_limit: None }
    }

    pub fn limit_stack(&mut self, sub_expr_count: u32) -> &mut Self {
        self.stack_limit = Some(sub_expr_count);
        self
    }

    pub fn unlimit_stack(&mut self) -> &mut Self {
        self.stack_limit = None;
        self
    }

    pub fn enter(self) -> impl FnMut(Span) -> Result<Self> {
        move |input| match self.stack_limit {
            Some(0) => Err(nom::Err::Failure(ErrorList::new(
                Error::TooMuchRecursion(input),
            ))),
            Some(available) => {
                Ok((input, Self { stack_limit: Some(available - 1), ..self }))
            },
            None => Ok((input, self)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Flag {
    I,
    M,
    S,
    U,
    X,
    G,
}

impl fmt::Display for Flag {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        write!(fmtr, "{}", match self {
            Self::I => "i",
            Self::M => "m",
            Self::S => "s",
            Self::U => "U",
            Self::X => "x",
            Self::G => "g",
        })
    }
}

pub type ErrorList = crate::error_list::ErrorList<Error>;

pub type Result<T> = IResult<Span, T, ErrorList>;

#[derive(Debug)]
pub enum Error {
    TooMuchRecursion(Span),
    UnknownFlag(Symbol<Flag>),
    DuplicateFlag(Symbol<Flag>),
    Regex(Symbol<regex::Error>),
    Nom(nom::error::Error<Span>),
}

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        write!(fmtr, "syntax error: ")?;
        match self {
            Self::TooMuchRecursion(_) => {
                write!(fmtr, "too much syntatic nesting")
            },
            Self::UnknownFlag(flag) => {
                write!(fmtr, "unknown flag {}", flag.data)
            },
            Self::DuplicateFlag(flag) => {
                write!(fmtr, "duplicate flag {}", flag.data)
            },
            Self::Regex(error) => write!(fmtr, "regex error, {}", error.data),
            Self::Nom(error) => write!(fmtr, "{}", error),
        }?;
        write!(fmtr, " in {}", self.span().start())?;
        Ok(())
    }
}

impl Spanned for Error {
    fn span(&self) -> Span {
        match self {
            Self::TooMuchRecursion(span) => span.clone(),
            Self::UnknownFlag(segment) => segment.span(),
            Self::DuplicateFlag(segment) => segment.span(),
            Self::Regex(symbol) => symbol.span.clone(),
            Self::Nom(nom) => nom.input.clone(),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Regex(error) => Some(&error.data),
            Self::Nom(error) => Some(error),
            _ => None,
        }
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
                result.raise_error(Error::DuplicateFlag(flag));
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

fn parse_leftmost_expr(
    config: Config,
) -> impl FnMut(Span) -> Result<Expression> {
    move |input| {
        let mut parser = alt((
            wrap_expr(parse_test, Expression::Test),
            wrap_expr(parse_substitution, Expression::Substitution),
            wrap_expr(parse_ident, Expression::Identifier),
            wrap_expr(parse_negation(config), Expression::Negation),
            wrap_expr(parse_let(config), Expression::Let),
        ));
        parser(input)
    }
}

fn parse_binop<'tag, F, T>(
    config: Config,
    tok: Tag<'tag, 'tag>,
    mut make_op: F,
) -> impl FnMut(Span) -> Result<Symbol<T>> + 'tag
where
    T: 'tag,
    F: FnMut(Expression, Expression) -> T + 'tag,
{
    let parse_tuple = tuple((
        parse_leftmost_expr(config),
        parse_operator(tok),
        parse_expr(config),
    ));
    let parse = map(parse_tuple, move |(left, _, right)| make_op(left, right));
    symbol(parse)
}

fn wrap_expr<P, F, T>(
    parser: P,
    mut function: F,
) -> impl FnMut(Span) -> Result<Expression>
where
    P: Parser<Span, Symbol<T>, ErrorList>,
    F: FnMut(Box<Symbol<T>>) -> Expression,
{
    map(parser, move |symbol| function(Box::new(symbol)))
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
            result.raise_error(Error::UnknownFlag(flag));
        }
        result
    });
    symbol(parse)(input)
}

pub fn parse_substitution(input: Span) -> Result<Symbol<Substitution>> {
    let parse_char = || {
        alt((
            preceded(segment("\\"), alt((segment("/"), segment("\\")))),
            any_segment,
        ))
    };
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
    let mut parse = map_res(parse_regex, |(regex, substitute, mut flags)| {
        let mut result = Ok(Substitution {
            regex,
            substitute,
            is_global: flags.remove(&Flag::G),
        });
        for flag in flags {
            result.raise_error(Error::UnknownFlag(flag))
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

pub fn parse_binding(
    config: Config,
) -> impl FnMut(Span) -> Result<Symbol<Binding>> {
    let parse_tuple = delimited(
        whitespace0,
        symbol(tuple((
            parse_ident,
            delimited(
                tuple((whitespace0, segment("="), whitespace0)),
                parse_expr(config),
                tuple((whitespace0, segment(";"))),
            ),
        ))),
        whitespace0,
    );

    map(parse_tuple, |symbol| {
        symbol
            .map(|(identifier, definition)| Binding { identifier, definition })
    })
}

pub fn parse_let(config: Config) -> impl FnMut(Span) -> Result<Symbol<Let>> {
    let parse_let_tok = terminated::<_, _, _, ErrorList, _, _>(
        Tag(&["l", "e", "t"]),
        whitespace1,
    );
    let parse_bindings = many0(parse_binding(config));
    let parse_in_tok = terminated(Tag(&["i", "n"]), whitespace1);
    let parse_tuple = tuple((
        preceded(parse_let_tok, parse_bindings),
        preceded(parse_in_tok, parse_expr(config)),
    ));
    symbol(map(parse_tuple, |(bindings, sub_expr)| Let { bindings, sub_expr }))
}

pub fn parse_negation(
    config: Config,
) -> impl FnMut(Span) -> Result<Symbol<Negation>> {
    let parse = map(
        preceded(parse_operator(Tag(&["!"])), parse_expr(config)),
        |target| Negation { target },
    );
    symbol(parse)
}

pub fn parse_sequence(
    config: Config,
) -> impl FnMut(Span) -> Result<Symbol<Sequence>> {
    parse_binop(config, Tag(&[","]), |left, right| Sequence { left, right })
}

pub fn parse_disjunction(
    config: Config,
) -> impl FnMut(Span) -> Result<Symbol<Disjunction>> {
    parse_binop(config, Tag(&["|"]), |left, right| Disjunction { left, right })
}

pub fn parse_conjunction(
    config: Config,
) -> impl FnMut(Span) -> Result<Symbol<Conjunction>> {
    parse_binop(config, Tag(&["&"]), |left, right| Conjunction { left, right })
}

pub fn parse_condition(
    config: Config,
) -> impl FnMut(Span) -> Result<Symbol<Condition>> {
    let parse_tuple = tuple((
        preceded(parse_operator(Tag(&["?"])), parse_expr(config)),
        preceded(parse_operator(Tag(&["=", ">"])), parse_expr(config)),
        preceded(parse_operator(Tag(&["!", ">"])), parse_expr(config)),
    ));

    let parse = map(parse_tuple, |(condition, then, else_)| Condition {
        condition,
        then,
        else_,
    });

    symbol(parse)
}

pub fn parse_expr(config: Config) -> impl FnMut(Span) -> Result<Expression> {
    move |input| {
        let (input, config) = config.enter()(input)?;
        let mut parser = alt((
            parse_leftmost_expr(config),
            wrap_expr(parse_sequence(config), Expression::Sequence),
            wrap_expr(parse_disjunction(config), Expression::Disjunction),
            wrap_expr(parse_conjunction(config), Expression::Conjunction),
            wrap_expr(parse_condition(config), Expression::Condition),
            delimited(segment("("), parse_expr(config), segment(")")),
        ));
        parser(input)
    }
}
