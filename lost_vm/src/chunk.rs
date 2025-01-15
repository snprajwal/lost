use std::fmt::Display;

use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum Op {
    Constant,
    Object,
    Return,
    Pop,
    Not,
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    True,
    False,
    Null,
    Equal,
    Greater,
    Less,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{self:?}").to_uppercase())
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Object(HeapId),
    Boolean(bool),
    Null,
}

impl From<&Value> for bool {
    fn from(value: &Value) -> Self {
        match value {
            Value::Null => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        bool::from(&value)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Self::Number(n) => n.to_string(),
            Self::Boolean(b) => b.to_string(),
            Self::Object(id) => format!("HeapId({id})"),
            Self::Null => "null".to_string(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
    Str(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Str(s) => s,
        })
    }
}

pub type HeapId = usize;

pub type Heap = Vec<Object>;

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub objects: Vec<Object>,
    pub ops: Vec<u8>,
}

impl Chunk {
    pub fn add_constant(&mut self, v: Value) {
        if self.constants.len() > u8::MAX.into() {
            panic!("too many constants in chunk")
        }
        self.add_op(Op::Constant);
        self.constants.push(v);
        self.add_op((self.constants.len() - 1) as u8);
    }

    pub fn add_object(&mut self, o: Object) {
        if self.constants.len() > u8::MAX.into() {
            panic!("too many objects in chunk")
        }
        self.add_op(Op::Object);
        self.objects.push(o);
        self.add_op((self.objects.len() - 1) as u8);
    }

    pub fn add_op<T: Into<u8>>(&mut self, op: T) {
        self.ops.push(op.into());
    }

    /// Decodes the instruction at the given offset and pretty prints it with some metadata
    pub fn decode(&self, offset: usize) -> (String, usize) {
        let mut s = format!("{offset:0>4} ");

        if let Ok(op) = self.ops[offset].try_into() {
            match op {
                Op::Constant => {
                    s.push_str(&format!(
                        "{op} {:0>4} '{}'",
                        offset + 1,
                        self.constants
                            .get(self.ops[offset + 1] as usize)
                            .expect("invalid constant pool offset")
                    ));
                    return (s, offset + 2);
                }
                _ => s.push_str(&format!("{op}")),
            }
        } else {
            s.push_str(&format!("{} invalid opcode", self.ops[offset]));
        }
        (s, offset + 1)
    }
}

pub fn decode_chunk(chunk: &Chunk, source: &str) {
    println!("=== {source} ===");
    let mut offset = 0;
    while offset < chunk.ops.len() {
        let (s, new_offset) = chunk.decode(offset);
        println!("{s}");
        offset = new_offset;
    }
}
