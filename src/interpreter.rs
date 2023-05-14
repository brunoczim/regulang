use crate::ir::{Instruction, Label, OutOfBoundsLabel, Program};
use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Error {
    OutOfBoundsLabel(OutOfBoundsLabel),
    EmptyStack,
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

    pub fn step_next(&mut self) -> Result<bool, Error> {
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
}
