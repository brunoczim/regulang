use crate::ir::{Instruction, Label, OutOfBoundsLabel, Program};
use core::fmt;
use std::borrow::Cow;

#[derive(Debug, Clone, Copy)]
pub struct Config {
    step_limit: Option<u32>,
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

impl Config {
    pub fn new() -> Self {
        Self { step_limit: None }
    }

    pub fn limit_steps(&mut self, sub_expr_count: u32) -> &mut Self {
        self.step_limit = Some(sub_expr_count);
        self
    }

    pub fn unlimit_steps(&mut self) -> &mut Self {
        self.step_limit = None;
        self
    }

    pub fn step(&mut self) -> Result<()> {
        match &mut self.step_limit {
            Some(0) => Err(Error::StepsExhausted),
            Some(available) => {
                *available -= 1;
                Ok(())
            },
            None => Ok(()),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Error {
    OutOfBoundsLabel(OutOfBoundsLabel),
    StepsExhausted,
    EmptyStack,
}

impl fmt::Display for Error {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::OutOfBoundsLabel(error) => write!(fmtr, "{}", error),
            Self::StepsExhausted => write!(fmtr, "execution steps exhausted"),
            Self::EmptyStack => write!(fmtr, "empty stack"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::OutOfBoundsLabel(error) => Some(error),
            _ => None,
        }
    }
}

impl From<OutOfBoundsLabel> for Error {
    fn from(error: OutOfBoundsLabel) -> Self {
        Self::OutOfBoundsLabel(error)
    }
}

#[derive(Debug, Clone)]
struct StackFrame {
    pc: Label,
    data: String,
    flag: bool,
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    program: Program,
    curr_stack_frame: StackFrame,
    prev_stack_frames: Vec<StackFrame>,
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            curr_stack_frame: StackFrame {
                pc: 0,
                data: String::new(),
                flag: false,
            },
            prev_stack_frames: Vec::new(),
        }
    }

    pub fn program(&self) -> Program {
        self.program.clone()
    }

    pub fn start(&mut self, input: String) {
        self.prev_stack_frames.clear();
        self.curr_stack_frame.pc = 0;
        self.curr_stack_frame.flag = false;
        self.set_curr_data(input);
    }

    pub fn set_curr_data(&mut self, data: String) {
        self.curr_stack_frame.data = data;
    }

    pub fn curr_data(&self) -> &str {
        &self.curr_stack_frame.data
    }

    pub fn flag(&self) -> bool {
        self.curr_stack_frame.flag
    }

    pub fn pc(&self) -> Label {
        self.curr_stack_frame.pc
    }

    pub fn step_next(&mut self) -> Result<bool> {
        let pc = self.curr_stack_frame.pc;
        self.curr_stack_frame.pc += 1;
        match self.program.get(pc)? {
            Instruction::Test(regex) => {
                self.curr_stack_frame.flag =
                    regex.is_match(&self.curr_stack_frame.data);
            },
            Instruction::Replace(regex, replacement) => {
                let replaced =
                    regex.replace(&self.curr_stack_frame.data, replacement);
                self.curr_stack_frame.flag =
                    matches!(&replaced, Cow::Borrowed(_));
                self.curr_stack_frame.data = replaced.into_owned();
            },
            Instruction::ReplaceGlobal(regex, replacement) => {
                let replaced =
                    regex.replace_all(&self.curr_stack_frame.data, replacement);
                self.curr_stack_frame.flag =
                    matches!(&replaced, Cow::Borrowed(_));
                self.curr_stack_frame.data = replaced.into_owned();
            },
            Instruction::Jump(label) => self.curr_stack_frame.pc = *label,
            Instruction::JumpTrue(label) => {
                if self.curr_stack_frame.flag {
                    self.curr_stack_frame.pc = *label;
                }
            },
            Instruction::JumpFalse(label) => {
                if !self.curr_stack_frame.flag {
                    self.curr_stack_frame.pc = *label;
                }
            },
            Instruction::SetTrue => self.curr_stack_frame.flag = true,
            Instruction::SetFalse => self.curr_stack_frame.flag = false,
            Instruction::Save => {
                let stack_frame = self.curr_stack_frame.clone();
                self.prev_stack_frames.push(stack_frame);
            },
            Instruction::Restore => {
                let stack_frame =
                    self.prev_stack_frames.pop().ok_or(Error::EmptyStack)?;
                self.curr_stack_frame = stack_frame;
            },
            Instruction::Discard => {
                self.prev_stack_frames.pop().ok_or(Error::EmptyStack)?;
            },
        };
        Ok(true)
    }

    pub fn run(&mut self, mut config: Config) -> Result<()> {
        loop {
            config.step()?;
            if self.step_next()? {
                break Ok(());
            }
        }
    }
}
