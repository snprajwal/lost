use log::{debug, trace};

use crate::{
    chunk::{Chunk, Heap, Object, Op, Value},
    error::{runtime_error, Error, ErrorMsg},
};

#[derive(Debug, Default)]
pub struct Vm {
    pc: usize,
    stack: Vec<Value>,
    heap: Heap,
}

impl Vm {
    pub fn exec(&mut self, chunk: &Chunk) -> Result<(), Error> {
        // Reset the program counter
        self.pc = 0;
        while self.pc < chunk.ops.len() {
            trace!("STACK: {:?}", self.stack);
            debug!("{}", chunk.decode(self.pc).0);
            if let Ok(op) = chunk.ops[self.pc].try_into() {
                match op {
                    Op::Return => {
                        self.stack
                            .pop()
                            .map(|v| {
                                println!("{v}");
                            })
                            .expect("cannot pop empty stack");
                        return Ok(());
                    }
                    Op::Constant => {
                        self.pc += 1;
                        self.stack
                            .push(chunk.constants[chunk.ops[self.pc] as usize].clone());
                    }
                    Op::Object => {
                        self.pc += 1;
                        self.heap
                            .push(chunk.objects[chunk.ops[self.pc] as usize].clone());
                        self.stack.push(Value::Object(self.heap.len() - 1));
                    }
                    Op::Pop => {
                        self.stack.pop();
                    }
                    Op::Not => {
                        if let Some(v) = self.stack.pop() {
                            self.stack.push(Value::Boolean(!bool::from(v)));
                        }
                    }
                    Op::Negate => {
                        if let Some(Value::Number(n)) = self.stack.last_mut() {
                            *n = -*n;
                        }
                    }
                    Op::Null => self.stack.push(Value::Null),
                    Op::True => self.stack.push(Value::Boolean(true)),
                    Op::False => self.stack.push(Value::Boolean(false)),
                    Op::Equal => {
                        let Some(right) = self.stack.pop() else {
                            return Err(runtime_error(ErrorMsg::EmptyStack, op));
                        };
                        let Some(left) = self.stack.pop() else {
                            return Err(runtime_error(ErrorMsg::EmptyStack, op));
                        };
                        self.stack.push(Value::Boolean(self.is_eq(&left, &right)))
                    }
                    Op::Add => {
                        let Some(right) = self.stack.pop() else {
                            return Err(runtime_error(ErrorMsg::EmptyStack, op));
                        };
                        let Some(left) = self.stack.pop() else {
                            return Err(runtime_error(ErrorMsg::EmptyStack, op));
                        };
                        match (&left, &right) {
                            (Value::Object(l), Value::Object(r)) => {
                                if let Object::Str(ref left_str) = self.heap[*l] {
                                    let Object::Str(ref right_str) = self.heap[*r] else {
                                        return Err(runtime_error(
                                            ErrorMsg::ExpectedStr,
                                            self.heap[*r].clone(),
                                        ));
                                    };
                                    self.heap.push(Object::Str(left_str.clone() + right_str));
                                    self.stack.push(Value::Object(self.heap.len() - 1));
                                }
                            }
                            (Value::Number(left_num), Value::Number(right_num)) => {
                                self.stack.push(Value::Number(left_num + right_num))
                            }
                            (Value::Number(_), _) => {
                                return Err(runtime_error(ErrorMsg::ExpectedNum, right))
                            }
                            _ => return Err(runtime_error(ErrorMsg::ExpectedNumOrStr, left)),
                        }
                    }
                    _ => {
                        let Some(right) = self.stack.pop() else {
                            return Err(runtime_error(ErrorMsg::EmptyStack, op));
                        };
                        let Some(left) = self.stack.pop() else {
                            return Err(runtime_error(ErrorMsg::EmptyStack, op));
                        };

                        let Value::Number(left_num) = left else {
                            return Err(runtime_error(ErrorMsg::ExpectedNum, left));
                        };
                        let Value::Number(right_num) = right else {
                            return Err(runtime_error(ErrorMsg::ExpectedNum, right));
                        };

                        self.stack.push(match op {
                            Op::Sub => Value::Number(left_num - right_num),
                            Op::Mul => Value::Number(left_num * right_num),
                            Op::Mod => Value::Number(left_num % right_num),
                            Op::Div => Value::Number(left_num / right_num),
                            Op::Greater => Value::Boolean(left_num > right_num),
                            Op::Less => Value::Boolean(left_num < right_num),
                            _ => unreachable!("non-numeric operations cannot reach this case"),
                        });
                    }
                }
            };
            self.pc += 1;
        }
        Ok(())
    }

    fn is_eq(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(m), Value::Number(n)) => m == n,
            (Value::Object(m), Value::Object(n)) => m == n || self.heap[*m] == self.heap[*n],
            _ => bool::from(left).eq(&right.into()),
        }
    }
}
