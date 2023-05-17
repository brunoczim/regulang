pub mod error_list;
pub mod parser;
pub mod ast;
pub mod compiler;
pub mod ir;
pub mod interpreter;
mod error;

use compiler::Compile;
pub use error::Error;
use interpreter::Interpreter;
use ir::Program;
use nom_grapheme_clusters::Source;

pub type Result<T> = std::result::Result<T, Error>;

pub fn compile(
    parse_config: parser::Config,
    prog_name: &str,
    program: &str,
) -> Result<Program> {
    let span = Source::new(prog_name, program).full_span();
    let (_, expr) = parser::parse_expr(parse_config)(span)?;
    Ok(expr.try_compile()??)
}

pub fn eval(
    parse_config: parser::Config,
    interpreter_config: interpreter::Config,
    prog_name: &str,
    program: &str,
    input: &str,
) -> Result<(String, bool)> {
    let program = compile(parse_config, prog_name, program)?;
    let mut interpreter = Interpreter::new(program);
    interpreter.set_curr_data(input.to_owned());
    interpreter.run(interpreter_config)?;
    Ok((interpreter.curr_data().to_owned(), interpreter.flag()))
}
